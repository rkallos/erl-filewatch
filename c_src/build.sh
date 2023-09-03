#!/usr/bin/env sh

set -eu

erlang_eval() {
    erl -noshell -eval "io:format(\"~s\", [$1]), halt()." -s init stop
}

ERL_ROOT=${ERL_ROOT:-$(erlang_eval 'code:root_dir()')}
ERL_INCLUDE_DIR=${ERL_INCLUDE_DIR:-${ERL_ROOT}/usr/include/}
ERL_LIB_DIR=${ERL_LIB_DIR:-${ERL_ROOT}/usr/lib}
ERTS_INCLUDE_DIR=${ERTS_INCLUDE_DIR:-${ERL_ROOT}/erts-$(erlang_eval 'erlang:system_info(version)')/include}

CC=${CC:-cc}
DEFAULT_CFLAGS="-O3 -ggdb -Wall -Wextra"
CFLAGS="-fPIC -I${ERTS_INCLUDE_DIR} -I${ERL_INCLUDE_DIR} -std=gnu11 ${CFLAGS:-$DEFAULT_CFLAGS}"
LDFLAGS="-L${ERL_LIB_DIR} -lei ${LDFLAGS:-}"

OS="$(uname -s)"

SRC=./c_src
case "$OS" in
    Linux)
        IMPLEMENTATION=filewatch_inotify
        SRCFILES="$SRC/$IMPLEMENTATION.c $SRC/notif_set_tlh_htable.c $SRC/optics_htable/htable.c"
        ;;
    Darwin)
        LDFLAGS="$LDFLAGS -flat_namespace -undefined suppress"
        IMPLEMENTATION=filewatch_noop
        SRCFILES="$SRC/$IMPLEMENTATION.c"
        ;;
    *)
        IMPLEMENTATION=filewatch_noop
        ;;
esac

if [ "$IMPLEMENTATION" = "filewatch_noop" ]; then
    echo "warning: using filewatch_noop implementation"
fi

TARGET=${TARGET:-./priv/filewatch.so}

mkdir -p priv

up_to_date_p() {
    if [ ! -e "$TARGET" ]; then exit 1; fi
    for i in $SRC/*.c; do
        if [ "$TARGET" -ot "$i" ]; then exit 1; fi
    done
    exit 0
}

if (up_to_date_p); then exit 0; fi

exec "$CC" $CFLAGS -shared -o "$TARGET" $SRCFILES $LDFLAGS
