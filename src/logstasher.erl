-module(logstasher).

-behaviour(gen_server).

%% API
-export([send/1]).

%% Supervisor API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Xref ignores
-ignore_xref([start_link/0]).

%%==============================================================================
%% API
%%==============================================================================

-spec send(binary()) -> ok.
send(Data) ->
    gen_server:call(?MODULE, {send, Data}).

%%==============================================================================
%% Supervisor API
%%==============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

-spec init(term()) -> {ok, maps:map()} | {stop, maps:map()}.
init(_) ->
    Transport = application:get_env(?MODULE, transport, udp),
    Host = application:get_env(?MODULE, host, undefined),
    Port = application:get_env(?MODULE, port, undefined),
    Opts = #{transport => Transport, host => Host, port => Port},
    State = Opts#{socket => connect(Opts)},io:format("State ~p~n", [State]),
    {ok, State}.

-spec handle_call({'send',binary()}, any(), maps:map()) -> {reply, ok, maps:map()}.
handle_call({send, Data}, _, State) ->
    ok = send(Data, State),
    {reply, ok, State}.

-spec handle_cast(term(), maps:map()) -> {noreply, maps:map()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec terminate(term(), maps:map()) -> ok.
terminate(_, #{transport := tcp, socket := Socket}) ->
    gen_tcp:close(Socket);
terminate(_, #{transport := udp, socket := Socket}) ->
    gen_udp:close(Socket).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec connect(maps:map()) -> gen_udp:socket() | gen_tcp:socket() | undefined.
connect(#{transport := tcp, host := Host, port := Port}) ->
    Opts = [binary, {active, false}, {keepalive, true}],
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Socket} ->
            Socket;
        {error, _} ->
            undefined
    end;
connect(#{transport := udp}) ->
    Opts = [binary],
    case gen_udp:open(0, Opts) of
        {ok, Socket} ->
            Socket;
        {error, _} ->
            undefined
    end.

-spec send(binary(), maps:map()) -> ok.
send(Data, #{transport := tcp, socket := Socket}) ->
    ok = gen_tcp:send(Socket, Data);
send(Data, #{transport := udp, socket := Socket, host := Host, port := Port}) ->
    ok = gen_udp:send(Socket, Host, Port, Data).
