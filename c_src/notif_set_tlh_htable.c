#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include "notif_set.h"
#include "optics_htable/htable.h"

struct Notif_set {
    // a two-level hash table where the value of each
    // key points to another (level2) hash table
    struct htable lev1; // directory watch descriptors
    size_t len;
};

struct Notif_set_itr {
    struct Notif_set *tab;
    struct htable_bucket *itr1, *itr2;
};

struct Notif_set *notif_set_new()
{
    struct Notif_set *tlh = calloc(1, sizeof(*tlh));
    CHECK_SUCCESS(tlh);
    htable_init(&tlh->lev1);
    tlh->len = 0;
    return tlh;

fn_fail:
    return NULL;
}

void notif_set_destroy(struct Notif_set *tab)
{
    struct htable_bucket *itr;
    // destroy each level2 table
    for(itr = htable_next(&tab->lev1, NULL); itr; itr = htable_next(&tab->lev1, itr)) {
        htable_reset((struct htable*) itr->value);
        free((struct htable*) itr->value);
    }
    htable_reset(&tab->lev1);
    free(tab);
}

bool notif_set_insert(struct Notif_set *tab, const struct Message *msg, int *err_flag)
{
    // IMPORTANT: Each htable owns its keys, but the
    //            values are owned by the user.
    *err_flag = 0;
    struct Key *key_int = NULL;
    struct Key *key_str = NULL;
    bool new_key_int_added = false;

    key_int = htable_key_int(msg->wd);
    CHECK_SUCCESS(key_int);
    struct htable_ret get_ret = htable_get(&tab->lev1, key_int);
    struct htable *lev2_tab = (struct htable*) get_ret.value;
    if(!get_ret.ok) {
        // We have a new directory wd; insert to level1
        // First create a level2 table for this wd (the value)
        lev2_tab = calloc(1, sizeof(*lev2_tab));
        CHECK_SUCCESS(lev2_tab);
        htable_init(lev2_tab);
        CHECK_SUCCESS(!htable_put(&tab->lev1, key_int, (uint64_t) lev2_tab).err_flag);
        new_key_int_added = true; //necessary for propser cleanup on error
    }
    else {
        htable_key_free(key_int); //because did not put it in the htable
    }

    // add the filename to the level2 hash table of this wd
    key_str = htable_key_str(msg->name);
    CHECK_SUCCESS(key_str);
    struct htable_ret put_ret = htable_put(lev2_tab, key_str, 0);
    CHECK_SUCCESS(!put_ret.err_flag);
    if(!put_ret.ok) { //filename is already in the set
        htable_key_free(key_str); //because did not put it in the htable
        return false;
    }

    tab->len++;
    return true;

fn_fail:
    if(new_key_int_added) {
        htable_del(&tab->lev1, key_int);
    }
    else {
        htable_key_free(key_int);
    }
    htable_key_free(key_str);
    *err_flag = 1;
    return false;
}

bool notif_set_find(const struct Notif_set *tab, const struct Message *msg, int *err_flag)
{
    *err_flag = 0;
    struct Key *key = htable_key_int(msg->wd);
    CHECK_SUCCESS(key);
    struct htable_ret get_ret = htable_get(&tab->lev1, key);
    htable_key_free(key);
    if(!get_ret.ok) {
        return false;
    }
    key = htable_key_str(msg->name);
    CHECK_SUCCESS(key);
    get_ret = htable_get((struct htable*) get_ret.value, key);
    htable_key_free(key);
    if(!get_ret.ok) {
        return false;
    }

    return true;

fn_fail:
    *err_flag = 1;
    return false;
}

struct Notif_set_itr *notif_set_itr_init(struct Notif_set *tab)
{
    struct Notif_set_itr *itr = calloc(1, sizeof(*itr));
    CHECK_SUCCESS(itr);
    if(tab != NULL) {
        itr->tab = tab;
        itr->itr1 = htable_next(&tab->lev1, NULL);
    }

    return itr;

fn_fail:
    return NULL;
}

void notif_set_itr_destroy(struct Notif_set_itr *itr)
{
    free(itr);
}

bool notif_set_itr_next(struct Notif_set_itr *itr, struct Message *msg)
{
    if(itr->itr1 == NULL) {
        return false;
    }
    // Advance the second-level iterator first
    if((itr->itr2 = htable_next((struct htable *) itr->itr1->value, itr->itr2))) {
        assert(itr->itr1->key->n == sizeof(uint64_t));
        msg->wd = *((uint64_t*) itr->itr1->key->bytes); //Really don't like this
        memcpy(msg->name, itr->itr2->key->bytes, itr->itr2->key->n);
        return true;
    }
    // If second level is exhausted, advance the iterator in the first level
    if((itr->itr1 = htable_next(&itr->tab->lev1, itr->itr1))) {
        // Call recursively to iterate in the second level
        return notif_set_itr_next(itr, msg);
    }

    return false;
}

bool notif_set_itr_remove(const struct Notif_set_itr *itr)
{
    // First, attempt to remove from the second-level table
    if(itr->itr2) {
        if(htable_key_free((struct Key*) itr->itr2->key)) {
            itr->itr2->key = NULL;
            ((struct htable*)(itr->itr1->value))->len--;
            // We could destroy the level2 htable and remove
            // the corresponding entry in the level1 table if
            // the level2 table becomes empty. But, let's do it
            // in a lazy fashion and leave it to the destroy function.
            itr->tab->len--;
            return true;
        }
    }

    // If itr2 is NULL, then itr1 must be NULL too, otherwise
    // we have some kind of a dangling iterator. Of course, I'm
    // assuming that the iterator is only progressed by _next function.
    assert(itr->itr1 == NULL);
    return false;
}

void notif_set_fprint(FILE *fp, const struct Notif_set *tab)
{
    struct htable_bucket *itr1, *itr2;
    int entry_idx = 0;

    for(itr1 = htable_next(&tab->lev1, NULL);
        itr1;
        itr1 = htable_next(&tab->lev1, itr1)) {
        for(itr2 = htable_next((struct htable*) itr1->value, NULL);
            itr2;
            itr2 = htable_next((struct htable*) itr1->value, itr2)) {
            fprintf(fp, "Entry %d: WD = %d, name = %s\n", entry_idx,
                    *((int*) itr1->key->bytes),
                    itr2->key->bytes); //no cast; bytes is char[]
            entry_idx++;
        }
    }

    if(entry_idx == 0) {
        fprintf(fp, "Table empty!\n");
    }
}

void notif_set_print(const struct Notif_set *tab)
{
    return notif_set_fprint(stdout, tab);
}

size_t notif_set_get_length(const struct Notif_set *tab)
{
    return tab->len;
}

bool notif_set_del(struct Notif_set *tab, const struct Message *msg, int *err_flag)
{
    *err_flag = 0;
    struct Key *key = htable_key_int(msg->wd);
    CHECK_SUCCESS(key);
    struct htable_ret get_ret = htable_get(&tab->lev1, key);
    htable_key_free(key);
    if(!get_ret.ok) {
        return false;
    }
    key = htable_key_str(msg->name);
    CHECK_SUCCESS(key);
    if(htable_del((struct htable*) get_ret.value, key).ok) {
        tab->len--;
        // We could destroy the level2 htable and remove
        // the corresponding entry in the level1 table if
        // the level2 table becomes empty. But, let's do it
        // in a lazy fashion and leave it to the destroy function.
        htable_key_free(key);
        return true;
    }

    htable_key_free(key);
    return false;

fn_fail:
    *err_flag = 1;
    return false;
}
