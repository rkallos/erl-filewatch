/* htable.c
   Rémi Attab (remi.attab@gmail.com), 10 Mar 2016
   FreeBSD-style copyright and disclaimer apply

   Modified for use in erl_filewatch by Seyed Mirsadeghi
   (hessam.mirsadeghi@gmail.com), 5 Sep. 2017
*/

#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include "htable.h"

extern inline uint64_t htable_hash(const struct Key *key);

// -----------------------------------------------------------------------------
// hash
// -----------------------------------------------------------------------------

// FNV-1a hash implementation: http://isthe.com/chongo/tech/comp/fnv/
inline uint64_t htable_hash(const struct Key *key)
{
    uint64_t hash = 0xcbf29ce484222325;
    for (size_t i = 0; i < key->n; ++i)
        hash = (hash ^ key->bytes[i]) * 0x100000001b3;

    return hash;
}


// -----------------------------------------------------------------------------
// config
// -----------------------------------------------------------------------------

enum { probe_window = 8 };


// -----------------------------------------------------------------------------
// basics
// -----------------------------------------------------------------------------

void htable_reset(struct htable *ht)
{
    if (ht->table) {
        for (size_t i = 0; i < ht->cap; ++i) {
            if (ht->table[i].key) free((struct Key *) ht->table[i].key);
        }
        free(ht->table);
    }
    *ht = (struct htable) {0};
}

static bool table_put(
        struct htable_bucket *table, size_t cap,
        const struct Key *key, uint64_t value)
{
    uint64_t hash = htable_hash(key);

    for (size_t i = 0; i < probe_window; ++i) {
        struct htable_bucket *bucket = &table[(hash + i) % cap];
        if (bucket->key) continue;

        bucket->key = key;
        bucket->value = value;
        return true;
    }

    return false;
}

static bool htable_resize(struct htable *ht, size_t cap)
{
    if (cap <= ht->cap) return true;

    size_t new_cap = ht->cap ? ht->cap : 1;
    while (new_cap < cap) new_cap *= 2;

    struct htable_bucket *new_table = calloc(new_cap, sizeof(*new_table));
    if(new_table == NULL) {
        return false;
    }

    for (size_t i = 0; i < ht->cap; ++i) {
        struct htable_bucket *bucket = &ht->table[i];
        if (!bucket->key) continue;

        if (!table_put(new_table, new_cap, bucket->key, bucket->value)) {
            free(new_table);
            return htable_resize(ht, new_cap * 2);
        }
    }

    free(ht->table);
    ht->cap = new_cap;
    ht->table = new_table;
    return true;
}

bool htable_reserve(struct htable *ht, size_t items)
{
    return htable_resize(ht, items * 4);
}

struct Key *htable_key_int(uint64_t i)
{
    struct Key *key = malloc(sizeof(*key) + sizeof(i));
    if(key == NULL) {
        return NULL;
    }
    key->n = sizeof(i);
    memcpy(key->bytes, &i, sizeof(i));
    return key;
}

struct Key *htable_key_str(const char *str)
{
    size_t len = strnlen(str, NAME_MAX);
    if(len < NAME_MAX) {
        len++; // to count for the null character
    }
    struct Key *key = malloc(sizeof(*key) + len);
    if(key == NULL) {
        return NULL;
    }
    key->n = len;
    memcpy(key->bytes, str, len);
    return key;
}

bool htable_key_free(struct Key *key)
{
    if(key == NULL) {
        return false;
    }

    free(key);
    return true;
}


// -----------------------------------------------------------------------------
// ops
// -----------------------------------------------------------------------------

struct htable_ret htable_get(const struct htable *ht, const struct Key *key)
{
    if(ht->len == 0) {
        return (struct htable_ret) { .ok = false };
    }

    uint64_t hash = htable_hash(key);

    for (size_t i = 0; i < probe_window; ++i) {
        struct htable_bucket *bucket = &ht->table[(hash + i) % ht->cap];

        if (!bucket->key) continue;
        if (bucket->key->n != key->n) continue;
        if (memcmp(bucket->key->bytes, key->bytes, key->n) != 0) continue;

        return (struct htable_ret) { .ok = true, .value = bucket->value };
    }

    return (struct htable_ret) { .ok = false };
}

struct htable_ret htable_put(struct htable *ht, const struct Key *key, uint64_t value)
{
    uint64_t hash = htable_hash(key);
    if(!htable_resize(ht, probe_window)) {
        return (struct htable_ret) { .ok = false, .err_flag = 1 };
    }

    struct htable_bucket *empty = NULL;

    for (size_t i = 0; i < probe_window; ++i) {
        struct htable_bucket *bucket = &ht->table[(hash + i) % ht->cap];

        if (bucket->key) {
            if (bucket->key->n != key->n) continue;
            if (memcmp(bucket->key->bytes, key->bytes, key->n) != 0 ) continue;
            return (struct htable_ret) { .ok = false,
                                         .value = bucket->value,
                                         .err_flag = 0
                                       };
        }

        if (!empty) empty = bucket;
    }

    if (empty) {
        ht->len++;
        empty->key = key;
        empty->value = value;
        return (struct htable_ret) { .ok = true, .err_flag = 0 };
    }

    if(!htable_resize(ht, ht->cap * 2)) {
        return (struct htable_ret) { .ok = false, .err_flag = 1 };
    }
    return htable_put(ht, key, value);
}

struct htable_ret htable_del(struct htable *ht, const struct Key *key)
{
    if(ht->len == 0) {
        return (struct htable_ret) { .ok = false };
    }

    uint64_t hash = htable_hash(key);

    for (size_t i = 0; i < probe_window; ++i) {
        struct htable_bucket *bucket = &ht->table[(hash + i) % ht->cap];

        if (!bucket->key) continue;
        if (bucket->key->n != key->n) continue;
        if (memcmp(bucket->key->bytes, key->bytes, key->n) != 0) continue;

        ht->len--;
        free((struct Key *) bucket->key);
        bucket->key = NULL;
        return (struct htable_ret) { .ok = true, .value = bucket->value };
    }

    return (struct htable_ret) { .ok = false };
}


struct htable_bucket * htable_next(
        const struct htable *ht, struct htable_bucket *bucket)
{
    if (!ht->table) return NULL;

    size_t i = 0;
    if (bucket) i = (bucket - ht->table) + 1;

    for (; i < ht->cap; ++i) {
        bucket = &ht->table[i];
        if (bucket->key) return bucket;
    }

    return NULL;
}

size_t htable_size(const struct htable *ht)
{
    return ht->len;
}

void htable_init(struct htable *ht)
{
    *ht = (struct htable) { 0 };
}
