#!/usr/bin/env sh
#
# Evaluate which function is fastest on this machine right now, and
# build it into a shared object.  This is just a shell script rather
# than a Makefile since there's not much point avoiding rebuilds here.

set -eu

erlang_eval() {
    erl -noshell -s init stop -eval "io:format(\"~s\", [$1]), halt()."
}

ERL_ROOT=${ERL_ROOT:-$(erlang_eval 'code:root_dir()')}
ERL_INCLUDE_DIR=${ERL_INCLUDE_DIR:-${ERL_ROOT}/usr/include/}
ERL_LIB_DIR=${ERL_LIB_DIR:-${ERL_ROOT}/usr/lib}
ERTS_INCLUDE_DIR=${ERTS_INCLUDE_DIR:-${ERL_ROOT}/erts-$(erlang_eval 'erlang:system_info(version)')/include}

CC=${CC:-cc}
DEFAULT_CFLAGS="-O3 -march=native -mtune=native -ggdb -Wall -Wextra -Wno-missing-field-initializers"
CFLAGS="-fPIC -I${ERTS_INCLUDE_DIR} -I${ERL_INCLUDE_DIR} -std=gnu11 ${CFLAGS:-$DEFAULT_CFLAGS}"
LDFLAGS="-L${ERL_LIB_DIR} -lei ${LDFLAGS:-}"

OS="$(uname -s)"

case "$OS" in
    Linux)
        IMPLEMENTATION=filewatch_inotify
        ;;
    Darwin)
        LDFLAGS="$LDFLAGS -flat_namespace -undefined suppress"
        IMPLEMENTATION=filewatch_noop
        ;;
    *)
        IMPLEMENTATION=filewatch_noop
        ;;
esac

if [ "$IMPLEMENTATION" = "filewatch_noop" ]; then
    echo "warning: using filewatch_noop implementation"
fi

TARGET=${TARGET:-./priv/filewatch.so}
SRC=./c_src

mkdir -p priv

up_to_date_p() {
    if [ ! -e "$TARGET" ]; then exit 1; fi
    for i in $SRC/*.c; do
        if [ "$TARGET" -ot "$i" ]; then exit 1; fi
    done
    exit 0
}

if (up_to_date_p); then exit 0; fi

exec "$CC" $CFLAGS -shared -o "$TARGET" $SRC/$IMPLEMENTATION.c $LDFLAGS
