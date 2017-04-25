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

enum {
    MAX_EVENTS = 12,
    EVENT_TUPLE_LENGTH = 11,
    PORT_CALL_ARG = 1,
    WATCH_FLAGS = IN_MOVED_TO | IN_CLOSE_WRITE,
    BUFFER_TAIL_SIZE = 3,
    BUFFER_TUPLE_SIZE = 2
};

/*
  A struct inotify_event's relevant data is copied to an array of
  struct message. This fixes a bug where messages sent back to Erlang
  had incorrect filenames.
*/
struct message {
    int wd;
    size_t len;
    char name[NAME_MAX];
};

struct instance {
    ErlDrvPort port;
    int fd;
    size_t n_events;
    unsigned long cooldown_ms;
    struct message msgs[MAX_EVENTS];
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

    self->fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (self->fd < 0) {
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

    size_t pos = 0;
    ErlDrvTermData buf[(MAX_EVENTS * EVENT_TUPLE_LENGTH) + BUFFER_TAIL_SIZE];

    buf[pos++] = ERL_DRV_PORT;
    buf[pos++] = driver_mk_port(self->port);

    struct message *msg;
    for(uint i = 0; i < self->n_events; ++i) {
        msg = &(self->msgs[i]);
        buf[pos++] = ERL_DRV_INT;
        buf[pos++] = msg->wd;
        buf[pos++] = ERL_DRV_STRING;
        buf[pos++] = (ErlDrvTermData) msg->name;
        buf[pos++] = strnlen(msg->name, NAME_MAX);
        buf[pos++] = ERL_DRV_TUPLE;
        buf[pos++] = BUFFER_TUPLE_SIZE;
    }

    /* Append values designating Erlang type */
    buf[pos++] = ERL_DRV_NIL;
    buf[pos++] = ERL_DRV_LIST;
    buf[pos++] = self->n_events + 1; // +1 for nil
    buf[pos++] = ERL_DRV_TUPLE;
    buf[pos++] = BUFFER_TUPLE_SIZE;

    self->n_events = 0;

    erl_drv_output_term(driver_mk_port(self->port), buf, pos);
}

static void serialize_event(struct instance *self, const struct inotify_event *evt) {
    struct message *msg = &(self->msgs[self->n_events++]);
    msg->wd = evt->wd;
    msg->len = strnlen(evt->name, NAME_MAX);
    strncpy(msg->name, evt->name, NAME_MAX);
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
            if (self->n_events >= MAX_EVENTS) {
                send_output(self_);
            }
            event = (const struct inotify_event *)ptr;
            serialize_event(self, event);
        }

        if (++self->n_events < MAX_EVENTS) {
            driver_set_timer(self->port, self->cooldown_ms);
        } else {
            send_output(self_);
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
