-module(filewatch).
-export([start/2, stop/1]).

-type handle() :: pid().

-record(state,
        {pid :: pid(),
         port :: port(),
         map :: #{ {integer(), file:filename()} => any(),
                   file:filename() => integer()}
        }).


priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                          "priv");
        Dir -> Dir
    end.


%% If you put this in on_load, it will immediately get unloaded when that child dies.
load() ->
    case erl_ddll:load_driver(priv_dir(), "filewatch") of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} ->
            error_logger:error_msg("filewatch: error loading driver: ~p",
                                   [erl_ddll:format_error(Message)])
    end.


-spec start(pid(), [{file:name_all(), term()}]) -> {ok, handle()} | {error, _}.

start(Self, Pairs) ->
    %% CooldownMs = case application:get_env(cooldown) of
    %%                  {ok, Cd} -> Cd;
    %%                  undefined -> 3000
    %%              end,
    Pid = spawn_link(fun () -> init(Self, Pairs) end),
    {ok, Pid}.

-spec stop(handle()) -> ok | {error, _}.

stop(Handle) ->
    Handle ! terminate,
    ok.

init(Pid, Pairs) ->
    ok = load(),
    Port = open_port({spawn_driver, "filewatch "}, [in]),
    WatchMap = create_watch_map(Pairs, Port, maps:new()),
    watch(#state{pid=Pid, port=Port, map=WatchMap}).


create_watch_map([], _Port, Map) -> Map;
create_watch_map([{Path, Term} | Rest], Port, Map) ->
    Dir = filename:dirname(Path),
    File = filename:basename(Path),
    Wd = case maps:get(Dir, Map, undefined) of
             undefined ->
                 {ok, Descriptor} = add_watch(Dir, Port),
                 Descriptor;
             D -> D
         end,
    Result = maps:put({Wd, File}, Term, Map),
    create_watch_map(Rest, Port, Result).


add_watch(Dir, Port) ->
    erlang:port_call(Port, 1337, Dir).


-spec watch(#state{}) -> ok.
watch(S=#state{port = Port}) ->
    receive
        {Port, Msgs} ->
            handle_events(S, Msgs),
            watch(S);
        terminate ->
            ok;
        _ ->
            exit(unexpected_message)
    end.


handle_events(_S, []) -> ok;
handle_events(S=#state{}, [Msg | Msgs]) ->
    handle_event(S, Msg),
    handle_events(S, Msgs).


handle_event(#state{pid = Pid, map = Map}, {Wd, "*"}) ->
    case maps:get({Wd, "*"}, Map, undefined) of
        undefined -> ok;
        Term -> msg(Pid, Term)
    end;
handle_event(State = #state{pid = Pid, map = Map}, {Wd, Name}) ->
    case maps:get({Wd, Name}, Map, undefined) of
        undefined -> handle_event(State, {Wd, "*"});
        Term -> msg(Pid, Term)
    end.


msg(Pid, Term) ->
    Pid ! {filewatch, self(), Term}.
