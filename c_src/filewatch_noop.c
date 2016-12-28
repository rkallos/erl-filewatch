#include <ei.h>
#include <erl_driver.h>

#include "macrology.h"

static ErlDrvSSizeT call(
    ErlDrvData UNUSED,
    unsigned int UNUSED,
    char *UNUSED, ErlDrvSizeT UNUSED,
    char **rbuf, ErlDrvSizeT rlen,
    unsigned int *UNUSED)
{
    // Encode the terms first with a NULL buffer, which safely increments index
    // to determine the required buffer length. On the second iteration, either
    // encode to *rbuf if it is big enough, or to a new allocation of the right
    // size.
    char *out = NULL;
    int index = 0;
    do {
        if (index) {
            out = ((ErlDrvSizeT)index <= rlen) ? *rbuf : driver_alloc(index);
            if (!out) goto fail_alloc;
            index = 0;
        }

        if (ei_encode_version(out, &index) < 0) goto fail_encode;
        if (ei_encode_tuple_header(out, &index, 2) < 0) goto fail_encode;
        if (ei_encode_atom(out, &index, "ok") < 0) goto fail_encode;
        if (ei_encode_long(out, &index, 0) < 0) goto fail_encode;
    } while (!out);

    *rbuf = out;
    return index;

  fail_encode:
    if (out && out != *rbuf) driver_free(out);

  fail_alloc:
    return -1;
}

static ErlDrvEntry driver_entry = {
    .driver_name = "filewatch",
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
    .call = call
};

DRIVER_INIT(filewatch) { return &driver_entry; }
