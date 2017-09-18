#include "macrology.h"

#include <alloca.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <sys/inotify.h>
#include <unistd.h>
#include <limits.h>

#include <ei.h>
#include <erl_driver.h>
#include "notif_set.h"

enum {
    EVENT_TUPLE_LENGTH = 11,
    PORT_CALL_ARG = 1,
    WATCH_FLAGS = IN_MOVED_TO | IN_CLOSE_WRITE | IN_DELETE,
    BUFFER_TAIL_SIZE = 3,
    BUFFER_TUPLE_SIZE = 2
};

/*
  A struct inotify_event's relevant data is copied to an array of
  struct Message. This fixes a bug where messages sent back to Erlang
  had incorrect filenames.
*/
struct instance {
    ErlDrvPort port;
    int fd;
    unsigned long cooldown_ms;
    bool is_timer_set;
    struct Notif_set *notif_set;
};

// argv = ["filewatch", CooldownMs]
static bool get_arguments(char *command, unsigned long *cooldown_ms)
{
    if (!command || !cooldown_ms) return false;

    command = strchr(command, ' ');
    if (!command) return false;

    command += strspn(command, " ");
    if (!*command) return false;

    char *start = command, *end;
    *cooldown_ms = strtoul(start, &end, 10);
    if (start == end)
        return false;
    return true;
}

static ErlDrvData start(ErlDrvPort port, char *command)
{
    unsigned long cooldown_ms;
    if (!get_arguments(command, &cooldown_ms))
        return ERL_DRV_ERROR_BADARG;

    struct instance *self = driver_alloc(sizeof(*self));
    if (!self) return ERL_DRV_ERROR_GENERAL;
    *self = (struct instance){0};
    self->port = port;
    self->cooldown_ms = cooldown_ms;
    self->is_timer_set = false;

    self->notif_set = notif_set_new();
    if(self->notif_set == NULL) {
        driver_free(self);
        return ERL_DRV_ERROR_GENERAL;
    }

    self->fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (self->fd < 0) {
        notif_set_destroy(self->notif_set);
        driver_free(self);
        return ERL_DRV_ERROR_ERRNO;
    }

    int error = driver_select(
        self->port,
        (ErlDrvEvent)(intptr_t)self->fd,
        ERL_DRV_READ | ERL_DRV_USE,
        1
    );
    if (error) {
        close(self->fd);
        notif_set_destroy(self->notif_set);
        driver_free(self);
        return ERL_DRV_ERROR_GENERAL;
    }

    return (ErlDrvData)self;
}

static void stop_select(ErlDrvEvent event, void *UNUSED)
{
    close((intptr_t)event);
}

static void stop(ErlDrvData self_)
{
    struct instance *self = (struct instance *)self_;
    driver_select(self->port, (ErlDrvEvent)(intptr_t)self->fd, ERL_DRV_USE, 0);
    notif_set_destroy(self->notif_set);
    driver_free(self);
}

static ErlDrvSSizeT call(
    ErlDrvData self_,
    unsigned int operation,
    char *buf, ErlDrvSizeT UNUSED,
    char **rbuf, ErlDrvSizeT rlen,
    unsigned int *UNUSED)
{
    if (operation != PORT_CALL_ARG) goto fail_op_check;

    struct instance *self = (struct instance *)self_;

    int index = 0;
    if (ei_decode_version(buf, &index, NULL) < 0) goto fail_decode;

    int type;
    int path_len;
    if (ei_get_type(buf, &index, &type, &path_len) < 0) goto fail_decode;

    assert(path_len >= 0);

    char *path = alloca(path_len+1);
    if (ei_decode_string(buf, &index, path) < 0) goto fail_decode;

    int error = 0;
    char *error_str = NULL;
    int wd = inotify_add_watch(self->fd, path, WATCH_FLAGS);
    if (wd < 0) {
        error = errno;
        error_str = strerror(error);
    }

    // Encode the terms first with a NULL buffer, which safely increments index
    // to determine the required buffer length. On the second iteration, either
    // encode to *rbuf if it is big enough, or to a new allocation of the right
    // size.
    char *out = NULL;
    index = 0;
    do {
        if (index) {
            out = ((ErlDrvSizeT)index <= rlen) ? *rbuf : driver_alloc(index);
            if (!out) goto fail_alloc;
            index = 0;
        }

        if (ei_encode_version(out, &index) < 0) goto fail_encode;
        if (ei_encode_tuple_header(out, &index, 2) < 0) goto fail_encode;
        if (error) {
            if (ei_encode_atom(out, &index, "error") < 0) goto fail_encode;
            if (ei_encode_tuple_header(out, &index, 3) < 0) goto fail_encode;
            if (ei_encode_string(out, &index, path) < 0) goto fail_encode;
            if (ei_encode_long(out, &index, error) < 0) goto fail_encode;
            if (ei_encode_string(out, &index, error_str) < 0) goto fail_encode;
        } else {
            if (ei_encode_atom(out, &index, "ok") < 0) goto fail_encode;
            if (ei_encode_long(out, &index, wd) < 0) goto fail_encode;
        }
    } while (!out);

    *rbuf = out;
    return index;

  fail_encode:
    if (out && out != *rbuf) driver_free(out);

  fail_alloc:
  fail_decode:
  fail_op_check:
    return -1;
}

/* Return type to Erlang:
   {port(), [{watch_descriptor(), filename:basename()}]}
*/
static void send_output(ErlDrvData self_)
{
    struct instance *self = (struct instance *)self_;

    size_t len = notif_set_get_length(self->notif_set);

    size_t pos = 0;
    ErlDrvTermData buf[(len * EVENT_TUPLE_LENGTH) + BUFFER_TAIL_SIZE];

    buf[pos++] = ERL_DRV_PORT;
    buf[pos++] = driver_mk_port(self->port);

    struct Message msg[len];
    for(size_t i = 0; i < len; i++) {
        msg[i].name = driver_alloc(NAME_MAX);
    }
    int num_entries_removed = 0;
    struct Notif_set_itr *itr = notif_set_itr_init(self->notif_set);
    if(itr == NULL) {
        stop(self_);
    }
    while(notif_set_itr_next(itr, &msg[num_entries_removed])) {
        buf[pos++] = ERL_DRV_INT;
        buf[pos++] = msg[num_entries_removed].wd;
        buf[pos++] = ERL_DRV_STRING;
        buf[pos++] = (ErlDrvTermData) msg[num_entries_removed].name;
        buf[pos++] = strnlen(msg[num_entries_removed].name, NAME_MAX);
        buf[pos++] = ERL_DRV_TUPLE;
        buf[pos++] = BUFFER_TUPLE_SIZE;

        notif_set_itr_remove(itr);
        num_entries_removed++;
    }
    notif_set_itr_destroy(itr);

    /* Append values designating Erlang type */
    buf[pos++] = ERL_DRV_NIL;
    buf[pos++] = ERL_DRV_LIST;
    buf[pos++] = num_entries_removed + 1; // +1 for nil
    buf[pos++] = ERL_DRV_TUPLE;
    buf[pos++] = BUFFER_TUPLE_SIZE;

    //Don't need to free any memory associated with notif_set
    //entries; they are freed automatically as they get removed.

    erl_drv_output_term(driver_mk_port(self->port), buf, pos);

    for(size_t i = 0; i < len; i++) {
        driver_free(msg[i].name);
    }

    self->is_timer_set = false;
}

static void serialize_event(struct instance *self, const struct inotify_event *evt) {
    //Insert into notif_set to keep track of unique {wd, name} tuples.
    int err_flag;
    notif_set_insert(self->notif_set,
                     &(struct Message) {.wd = evt->wd, .name = (char *) evt->name},
                     &err_flag);
    if(err_flag) {
        stop((ErlDrvData) self);
    }
}

#define aligned(x) __attribute((aligned(x)))
#define alignof(x) __alignof(x)

static void ready_input(ErlDrvData self_, ErlDrvEvent fd_)
{
    struct instance *self = (struct instance *)self_;
    int fd = (intptr_t)fd_;

    char buf[4096] aligned(alignof(struct inotify_event));
    const struct inotify_event *event;
    ssize_t len;

    while ((len = read(fd, buf, sizeof(buf))) > 0) {

        const char *ptr;
        for (ptr = buf; ptr < buf + len; ptr += sizeof(struct inotify_event) + event->len) {
            event = (const struct inotify_event *)ptr;
            serialize_event(self, event);
        }

        if (!self->is_timer_set) {
            self->is_timer_set = true;
            driver_set_timer(self->port, self->cooldown_ms);
        }

        // The kernel always checks if there is enough room in the buffer for
        // an entire event, so there should be nothing left over.
        assert(ptr - buf == len);
    }

    if (len < 0 && errno != EAGAIN) {
        driver_failure_posix(self->port, errno);
    }
}

static ErlDrvEntry driver_entry = {
    .driver_name = "filewatch",
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
    .start = start,
    .stop_select = stop_select,
    .timeout = send_output,
    .stop = stop,
    .call = call,
    .ready_input = ready_input
};

DRIVER_INIT(filewatch) { return &driver_entry; }
