-module(filewatch).
-export([start/1,
         start/2,
         stop/1]).

-type watch_pair() :: {file:name_all(), term()}.
-export_type([watch_pair/0]).

-define(DRIVER_NAME, filewatch).
-define(COOLDOWN_MS, application:get_env(filewatch, cooldown_ms, 3000)).
-define(CALL_ARG, 1).

-type handle() :: pid().
-type watch_descriptor() :: integer().
-type directory() :: file:filename().

-record(state,
        {pid :: pid(),
         port :: port(),
         watch_map :: #{
           {watch_descriptor(), file:filename()} => any(),
            directory()                          => watch_descriptor()
         }
        }).

%% public

-spec start([filewatch:watch_pair()]) -> {ok, handle()} | {error, _}.

start(Pairs) ->
    start(self(), Pairs).

-spec start(pid(), [filewatch:watch_pair()]) -> {ok, handle()} | {error, _}.

start(Dest, Pairs) ->
    Pid = spawn_link(fun () -> init(Dest, Pairs) end),
    {ok, Pid}.

-spec stop(handle()) -> ok | {error, _}.

stop(Handle) ->
    Handle ! terminate,
    ok.

%% private

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.


load() ->
    case erl_ddll:load_driver(priv_dir(), ?DRIVER_NAME) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} ->
            error_logger:error_msg("~p: error loading driver: ~p",
                                   [?DRIVER_NAME,
                                    erl_ddll:format_error(Message)])
    end.


init(Pid, Pairs) ->
    ok = load(),
    Arg = io_lib:format("~p ~p", [?DRIVER_NAME, ?COOLDOWN_MS]),
    Port = open_port({spawn_driver, Arg}, [in]),
    WatchMap = create_watch_map(Pairs, Port, maps:new()),
    [cast(Pid, Term) || {_, Term} <- Pairs],
    watch(#state{pid = Pid, port = Port, watch_map = WatchMap}).


create_watch_map([], _Port, Map) -> Map;
create_watch_map([{Path, Term} | Rest], Port, Map) ->
    Dir = filename:dirname(Path),
    NewMap = case filelib:is_dir(Dir) of
                 true ->
                     File = filename:basename(Path),
                     add_to_watch_map(Dir, File, Term, Port, Map);
                 false ->
                     error_logger:error_msg("~p: Path ~p is not a directory",
                                            [?DRIVER_NAME, Dir]),
                     Map
             end,
    create_watch_map(Rest, Port, NewMap).


add_to_watch_map(Dir, File, Term, Port, Map) ->
    {WatchDescriptor, NewMap} =
        case maps:find(Dir, Map) of
            error -> {ok, WD} = add_watch(Dir, Port),
                     {WD, maps:put(Dir, WD, Map)};
            {ok, WD} -> {WD, Map}
        end,
    maps:put({WatchDescriptor, File}, Term, NewMap).


add_watch(Dir, Port) ->
    erlang:port_call(Port, ?CALL_ARG, Dir).


-spec watch(#state{}) -> ok.
watch(S = #state{port = Port}) ->
    receive
        {Port, Msgs} ->
            handle_events(S, Msgs),
            watch(S);
        terminate ->
            ok;
        Msg ->
            error_logger:error_msg("~p: Unexpected message received by watcher process: ~p",
                                   [?DRIVER_NAME, Msg]),
            ok
    end.


handle_events(_S, []) -> ok;
handle_events(S = #state{}, [Msg | Msgs]) ->
    handle_event(S, Msg),
    handle_events(S, Msgs).


handle_event(#state{pid = Pid, watch_map = Map}, {Wd, Name}) ->
    case maps:get({Wd, Name}, Map, undefined) of
        undefined -> ok;
        Term -> cast(Pid, Term)
    end.


cast(Pid, Term) ->
    Pid ! {?DRIVER_NAME, self(), Term}.
