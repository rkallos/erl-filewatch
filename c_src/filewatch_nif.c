#include <errno.h>
#include <limits.h>
#include <string.h>
#include <sys/inotify.h>
#include <unistd.h>

#include <erl_nif.h>

typedef struct {
    int fd;
} Instance;

static ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
add_watch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1)
        return enif_make_badarg(env);

    char path[NAME_MAX];
    if (enif_get_string(env, argv[0], path, NAME_MAX, ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    Instance* self = (Instance*) enif_priv_data(env);

    int wd = inotify_add_watch(self->fd, path, IN_MODIFY | IN_MOVED_TO | IN_CREATE);
    if (wd < 0) {
        ERL_NIF_TERM error_str;
        error_str = enif_make_string(env, strerror(errno), ERL_NIF_LATIN1);
        return enif_make_tuple2(env, mk_atom(env, "error"), error_str);
    }

    return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_int(env, wd));
}

static ERL_NIF_TERM
get_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0)
        return enif_make_badarg(env);

    Instance* self = (Instance*) enif_priv_data(env);
    int fd = self->fd;

    return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_int(env, fd));
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {

    Instance* self = (Instance*) enif_alloc(sizeof(Instance));
    self->fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (self->fd == -1) {
        ERL_NIF_TERM error_str;
        error_str = enif_make_string(env, strerror(errno), ERL_NIF_LATIN1);
        return enif_make_tuple2(env, mk_atom(env, "error"), error_str);
    }

    *priv = self;
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv_data) {
    Instance* self = (Instance*) priv_data;
    close(self->fd);
    enif_free(self);
}

static ErlNifFunc nif_funcs[] = {
    {"add_watch", 1, add_watch},
    {"get_fd", 0, get_fd}
};

ERL_NIF_INIT(filewatch_nif, nif_funcs, &load, NULL, NULL, &unload)
