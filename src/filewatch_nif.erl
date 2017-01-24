-module(filewatch_nif).
-on_load(init/0).

-export([init/0]).

-define(APPNAME, filewatch_nif).
-define(LIBNAME, filewatch_nif).

%% API exports
-export([add_watch/1, add_watches/1, get_fd/0]).

%%====================================================================
%% API functions
%%====================================================================

add_watches(Lst) ->
    {Paths, Terms} = lists:unzip(Lst),
    Wds = [add_watch(Path) || Path <- Paths],
    lists:zip(Wds, Terms).

add_watch({_Path, _Term}) ->
    erlang:nif_error(filewatch_not_loaded).

get_fd() ->
    erlang:nif_error(filewatch_not_loaded).

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).
