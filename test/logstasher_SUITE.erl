-module(logstasher_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("kernel/include/logger.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([logstasher_udp/1, logstasher_tcp/1, logstasher_message/1]).

-spec all() -> [ct_suite:ct_test_def(), ...].
all() ->
    [{group, logstasher}].

-spec groups() -> [ct_suite:ct_group_def(), ...].
groups() ->
    [{logstasher, [sequence], [
        logstasher_udp,
        logstasher_tcp,
        logstasher_message
    ]}].

-spec init_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) ->
                           ct_suite:ct_config() | {fail, term()} | {skip, term()}.
init_per_testcase(_Name, Config) ->
    ok = logger:add_handler(logstash, logstasher_h, #{level => info}),
    ok = logger:update_primary_config(#{level => all}),
    Config.

-spec end_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) ->
                          term() | {fail, term()} | {save_config, ct_suite:ct_config()}.
end_per_testcase(_Name, _Config) ->
    ok = application:stop(logstasher),
    ok = logger:remove_handler(logstash),
    ok.

-spec logstasher_udp(ct_suite:ct_config()) -> ok | no_return().
logstasher_udp(_Config) ->
    Transport = udp,
    Host = {127, 0, 0, 1},
    Port = 8080,
    ErrorMsg = "test UDP error log message",

    ok = application:set_env(logstasher, transport, Transport),
    ok = application:set_env(logstasher, host , Host),
    ok = application:set_env(logstasher, port, Port),

    {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
    ok = inet:setopts(Socket, [{active, once}]),

    {ok, Started} = application:ensure_all_started(logstasher),
    ok = ct:pal("Started: ~p", [Started]),

    ok = ?LOG_ERROR(ErrorMsg),

    #{<<"message">> := Msg} = receive
        {udp, Socket, Host, _, Bin} ->
            jsx:decode(Bin, [return_maps])
        after 500 ->
            timeout
    end,

    ok = gen_udp:close(Socket),

    ?assertEqual(list_to_binary(ErrorMsg), Msg).

-spec logstasher_tcp(ct_suite:ct_config()) -> ok | no_return().
logstasher_tcp(_Config) ->
    Transport = tcp,
    Host = "localhost",
    Port = 8181,
    ErrorMsg = "test TCP error log message",
    Opts = [binary, {active, false}, {keepalive, true}, {reuseaddr, true}],
    Pid = self(),

    ok = application:set_env(logstasher, transport, Transport),
    ok = application:set_env(logstasher, host , Host),
    ok = application:set_env(logstasher, port, Port),

    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),

    spawn(fun() ->
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        inet:setopts(Socket, [{active, once}]),
        receive
            {tcp, Socket, Bin} ->
                ok = gen_tcp:close(Socket),
                Pid ! jsx:decode(Bin, [return_maps])
            after 500 ->
                ok = gen_tcp:close(Socket)
        end
    end),

    {ok, Started} = application:ensure_all_started(logstasher),
    ok = ct:pal("Started: ~p", [Started]),

    ok = ?LOG_ERROR(ErrorMsg),

    Msg  = receive
        #{<<"message">> := Data} ->
            Data
        after 500 ->
            timeout
    end,

    ?assertEqual(list_to_binary(ErrorMsg), Msg).


-spec logstasher_message(ct_suite:ct_config()) -> ok | no_return().
logstasher_message(_Config) ->
    {ok, _Started} = application:ensure_all_started(logstasher),
    #{
        message := <<"Hello">>,
        fields := Fields1
    } = logstasher_h:log_data(#{
        level => info,
        msg => {report, #{ msg => <<"Hello">> }},
        meta => #{ time => 0 }
    }),
    true = maps:is_key(msg, Fields1),
    #{
        message := <<"Hello">>,
        fields := Fields2
    } = logstasher_h:log_data(#{
        level => info,
        msg => {report, #{ text => <<"Hello">> }},
        meta => #{ time => 0 }
    }),
    false = maps:is_key(text, Fields2),
    ok.
