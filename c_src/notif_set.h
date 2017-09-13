#pragma once
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#define CHECK_SUCCESS(RET_VAL) do { if(!(RET_VAL)) goto fn_fail; } while(0)

/* forward declarations */
struct Notif_set;
struct Notif_set_itr;
struct Message {
    uint64_t wd;
    size_t len;
    char *name;
};

struct Notif_set *notif_set_new();
void notif_set_destroy(struct Notif_set *tab);
bool notif_set_insert(struct Notif_set *tab, const struct Message *msg, int *err_flag);
bool notif_set_del(struct Notif_set *tab, const struct Message *msg, int *err_flag);
bool notif_set_find(const struct Notif_set *tab, const struct Message *msg, int *err_flag);
void notif_set_print(const struct Notif_set *tab);
void notif_set_fprint(FILE *fp, const struct Notif_set *tab);
struct Notif_set_itr *notif_set_itr_init(struct Notif_set *tab);
void notif_set_itr_destroy(struct Notif_set_itr *itr);
bool notif_set_itr_next(struct Notif_set_itr *itr, struct Message *msg);
bool notif_set_itr_remove(const struct Notif_set_itr *itr);
size_t notif_set_get_length(const struct Notif_set *tab);
