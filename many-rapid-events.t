#!/usr/bin/env bats
# -*- shell-script -*-
repeatedly_toggle_watched_file() {
    local toggle=$1
    wait_for_changes() {
        xargs -0 -- erl -pa $(./rebar3 path -s :) -noshell -eval <<EOF
{ok,P} = filewatch:start(self(),[{"$toggle",ok}]),
io:format("start~n", []),
State = receive {filewatch,P,ok} -> "ok" after 4000 -> "timeout" end,
filewatch:stop(P),
io:format("~s~n", [State]),
halt(0).
EOF
    }
    while read s; do
        case "$s" in
            start) for i in $(seq $2); do touch $toggle; rm $toggle; done;;
            ok) exit 0;;
            *) exit 1;;
        esac
    done < <(wait_for_changes)
    exit 2
}
@test "a few quick events on one file do not crash" {
    run repeatedly_toggle_watched_file "$BATS_TMPDIR/toggle" 5
    [ "$status" -eq 0 ]
}
@test "many quick events on one file do not crash" {
    run repeatedly_toggle_watched_file "$BATS_TMPDIR/toggle" 1000
    [ "$status" -eq 0 ]
}
