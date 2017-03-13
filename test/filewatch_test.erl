-module(filewatch_test).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_TIMEOUT, 4000).
-define(ONE_FILE, ["testfile"]).

-define(
   fixture(Files, Fun, NEvents, Expect),
       Dir = create_sandbox(?FUNCTION_NAME),
       Pairs = [{filename:join(Dir, File), File} || File <- Files],
       {ok, P} = filewatch:start(self(), Pairs),
       [start_sender(Path, Fun, NEvents) || {Path, _} <- Pairs],
       State = collect_events(Files),
       filewatch:stop(P),
       rm_rf(Dir),
       ?assert(State =:= Expect)
  ).

one_file_one_touch_test() ->
    ?fixture(?ONE_FILE, fun touch/1, 1, false).

one_file_one_rm_test() ->
    ?fixture(?ONE_FILE, fun rm/1, 1, false).

one_file_one_touch_rm_test() ->
    ?fixture(?ONE_FILE, fun touch_rm/1, 1, true).

one_file_one_replace_test() ->
    ?fixture(?ONE_FILE, fun replace/1, 1, true).

one_file_few_touch_rm_test() ->
    ?fixture(?ONE_FILE, fun touch_rm/1, 5, true).

one_file_few_replace_test() ->
    ?fixture(?ONE_FILE, fun replace/1, 5, true).

one_file_many_touch_rm_test() ->
    ?fixture(?ONE_FILE, fun touch_rm/1, 1000, true).

one_file_many_replace_test() ->
    ?fixture(?ONE_FILE, fun replace/1, 1000, true).

many_files_few_touch_rm_test() ->
    ?fixture(many_file_names(1 bsl 3), fun touch_rm/1, 5, true).

many_files_many_touch_rm_test() ->
    ?fixture(many_file_names(1 bsl 3), fun touch_rm/1, 1000, true).

long_named_file_test() ->
    ?fixture([long_file_name()], fun touch_rm/1, 1000, true).

%% Helper functions

test_dir(Name) ->
    filename:join(".tests", Name).

create_sandbox(Name) ->
    Dir = test_dir(Name),
    ok = rm(Dir),
    ok = filelib:ensure_dir(Dir ++ "/"),
    Dir.

rm(File) ->
    case filelib:is_dir(File) of
        true -> rm_rf(File);
        false ->
            case filelib:is_file(File) of
                true -> file:delete(File);
                false -> ok
            end
    end.

rm_rf(Dir) ->
    Paths = filelib:wildcard(Dir ++ "/**"),
    {Dirs, Files} = lists:partition(fun filelib:is_dir/1, Paths),
    ok = lists:foreach(fun file:delete/1, Files),
    ok = lists:foreach(fun rm_rf/1, Dirs),
    file:del_dir(Dir).

start_sender(Path, Fun, N) ->
    spawn(fun () -> send(Path, Fun, N) end).

send(_Path, _Fun, 0) -> ok;
send(Path, Fun, N) ->
    Fun(Path),
    send(Path, Fun, N-1).

touch_rm(Path) ->
    touch(Path),
    rm(Path).

touch(Path) ->
    os:cmd("touch " ++ Path).

replace(Path) ->
    rm(Path),
    touch(Path).

collect_events(Files) ->
    collect_events_loop(ordsets:from_list(Files)).

collect_events_loop([]) -> true;
collect_events_loop(Files) ->
    receive
        {filewatch, _P, File} ->
            NewFiles = ordsets:subtract(Files, [File]),
            collect_events_loop(NewFiles)
    after
        4500 -> false
    end.

long_file_name() ->
    string:chars($a, 255).

many_file_names(0) -> [];
many_file_names(N) -> ["testfile" ++ integer_to_list(N) | many_file_names(N-1)].
