-module(logstasher).

-behaviour(gen_server).

%% API
-export([send/1, send_message/2]).

%% Supervisor API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Xref ignores
-ignore_xref([start_link/0, send_message/2]).

%% Default transport, port and host for logstash
-define(LOGSTASH_TRANSPORT, udp).
-define(LOGSTASH_PORT, 5000).
-define(LOGSTASH_HOST, "localhost").

-define(TCP_CONNECT_TIMEOUT, 5000).

%%==============================================================================
%% API
%%==============================================================================


%% @doc Send a custom message with fields to logstash. The fields must be valid
%% input for the json encoder.
-spec send_message(Message :: binary(), Fields :: map()) -> ok | {error, atom()}.
send_message(Message, Fields) when is_map(Fields) ->
    T = erlang:system_time(microsecond),
    Timestamp = list_to_binary(calendar:system_time_to_rfc3339(T, [{unit, microsecond}, {offset, "Z"}])),
    Msg = #{
        '@timestamp' => Timestamp,
        fields => Fields,
        message => unicode:characters_to_binary(Message)
    },
    send(jsx:encode(Msg)).

%% @doc Send an encoded JSON message to logstash.
-spec send(Data :: binary()) -> ok | {error, atom()}.
send(Data) when is_binary(Data) ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        Pid -> gen_server:call(Pid, {send, Data})
    end.

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
    Transport = application:get_env(?MODULE, transport, ?LOGSTASH_TRANSPORT),
    Host = application:get_env(?MODULE, host, ?LOGSTASH_HOST),
    Port = application:get_env(?MODULE, port, ?LOGSTASH_PORT),
    Opts = #{transport => Transport, host => Host, port => Port},
    State = Opts#{socket => connect(Opts)},
    {ok, State}.

-spec handle_call({send, binary()}, any(), maps:map()) ->
    {reply, ok | {error, atom() | {timeout, binary()}}, maps:map()}.
handle_call({send, Data}, _, State) ->
    Result = maybe_send(Data, State),
    {reply, Result, State}.

-spec handle_cast(term(), maps:map()) -> {noreply, maps:map()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec terminate(term(), maps:map()) -> ok.
terminate(_, #{transport := tcp, socket := Socket}) ->
    gen_tcp:close(Socket);
terminate(_, #{transport := udp, socket := Socket}) ->
    gen_udp:close(Socket);
terminate(_, #{transport := console}) ->
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec connect(maps:map()) -> gen_udp:socket() | gen_tcp:socket() | undefined.
connect(#{transport := tcp, host := Host, port := Port}) ->
    Opts = [binary, {active, false}, {keepalive, true}],
    case gen_tcp:connect(Host, Port, Opts, ?TCP_CONNECT_TIMEOUT) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            io:format("logstasher: error opening tcp socket (~p)~n", [Reason]),
            undefined
    end;
connect(#{transport := udp}) ->
    Opts = [binary],
    case gen_udp:open(0, Opts) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            io:format("logstasher: error opening udp socket (~p)~n", [Reason]),
            undefined
    end;
connect(#{transport := console}) ->
    undefined.

-spec maybe_send(binary(), maps:map()) -> ok | {error, atom()}.
maybe_send(Data, #{transport := console} = State) ->
    send(Data, State);
maybe_send(Data, #{socket := undefined} = State) ->
    maybe_send(Data, State#{socket => connect(State)});
maybe_send(Data, State) ->
    case send(Data, State) of
        ok -> ok;
        {error, closed} -> maybe_send(Data, State#{socket => undefined});
        {error, _} = Error -> Error
    end.

-spec send(binary(), maps:map()) -> ok | {error, atom()}.
send(Data, #{transport := console}) ->
    io:put_chars([ Data, "\n"]);
send(_Data, #{socket := undefined}) ->
    {error, closed};
send(Data, #{transport := tcp, socket := Socket}) ->
    gen_tcp:send(Socket, Data);
send(Data, #{transport := udp, socket := Socket, host := Host, port := Port}) ->
    gen_udp:send(Socket, Host, Port, Data).
