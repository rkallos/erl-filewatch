/* htable.h
   Rémi Attab (remi.attab@gmail.com), 10 Mar 2016
   FreeBSD-style copyright and disclaimer apply

   Modified for use in erl_filewatch by Seyed Mirsadeghi
   (hessam.mirsadeghi@gmail.com), 5 Sep. 2017
*/

#pragma once

#include <string.h>
#include <stdint.h>
#include <stdbool.h>

// -----------------------------------------------------------------------------
// struct
// -----------------------------------------------------------------------------

struct Key
{
    size_t n;
    char bytes[];
};

struct htable_bucket
{
    const struct Key *key;
    uint64_t value;
};

struct htable
{
    size_t len;
    size_t cap;
    struct htable_bucket *table;
};


struct htable_ret
{
    bool ok;
    uint64_t value;
    bool err_flag;
};

struct htable_itr
{
    struct htable *ht;
    struct htable_bucket *bucket;
    bool is_valid;
};

// -----------------------------------------------------------------------------
// basics
// -----------------------------------------------------------------------------

void htable_reset(struct htable *);
bool htable_reserve(struct htable *, size_t items);
struct Key *htable_key_int(uint64_t i);
struct Key *htable_key_str(const char *str);
bool htable_key_free(struct Key *key);


// -----------------------------------------------------------------------------
// ops
// -----------------------------------------------------------------------------

struct htable_ret htable_get(const struct htable *, const struct Key *key);
struct htable_ret htable_put(struct htable *, const struct Key *key, uint64_t value);
struct htable_ret htable_del(struct htable *, const struct Key *key);
struct htable_bucket * htable_next(const struct htable *, struct htable_bucket *bucket);
size_t htable_size(const struct htable *ht);
void htable_init(struct htable *ht);
