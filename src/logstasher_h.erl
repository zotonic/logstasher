-module(logstasher_h).

%% logger callbacks
-export([log/2]).

%% Xref ignores
-ignore_xref([log/2]).

%%==============================================================================
%% API
%%==============================================================================

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := info, meta := #{error_logger := #{type := progress}}}, _Config) ->
    % Ignore supervisor progress reports
    ok;
log(#{level := Level, msg := EventData, meta := Meta}, _Config) ->
    {Msg, MsgFields} = format_msg(EventData),
    Fields = [{severity, Level}] ++ safe_meta(Meta) ++ MsgFields,
    Data = #{fields => Fields, '@timestamp' => format_timestamp(Meta), message => Msg},
    _ = logstasher:send(jsx:encode(Data)),
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec format_msg(Data) -> {binary(), [{binary() | atom(), jsx:json_term()}]} when
    Data ::  {io:format(), [term()]}
           | {report, logger:report()}
           | {string, unicode:chardata()}.
format_msg({string, Message}) ->
    {unicode:characters_to_binary(Message), []};
format_msg({report, Report}) when is_map(Report) ->
    format_msg({report, maps:to_list(Report)});
format_msg({report, Report}) when is_list(Report) ->
    {proplists:get_value(msg, Report, null), safe_fields(Report)};
format_msg({Format, Params}) ->
    {unicode:characters_to_binary(io_lib:format(Format, Params)), []}.

-spec format_timestamp(logger:metadata()) -> binary().
format_timestamp(#{time := Ts}) ->
    list_to_binary(calendar:system_time_to_rfc3339(Ts, [{unit, microsecond}, {offset, "Z"}])).

-spec safe_meta(logger:metadata()) -> [{binary() | atom(), jsx:json_term()}].
safe_meta(Meta) ->
    safe_fields(maps:to_list(Meta)).

-spec safe_fields([{term(), term()}]) -> [{binary() | atom(), jsx:json_term()}].
safe_fields(Terms) ->
    lists:map(fun safe_field/1, Terms).

-spec safe_field({atom() | binary() | atom(), term()}) -> {atom() | binary(), jsx:json_term()}.
safe_field({Key, Value}) when is_atom(Key); is_binary(Key) ->
    {Key, safe_value(Value)};
safe_field({Key, Value}) when is_list(Key) ->
    safe_field({list_to_binary(Key), Value}).

-spec safe_value(term()) -> jsx:json_term().
safe_value(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
safe_value(List) when is_list(List) ->
    case io_lib:char_list(List) of
        true ->
            list_to_binary(List);
        false ->
            lists:map(fun safe_value/1, List)
    end;
safe_value(undefined) ->
    null;
safe_value(Val) when is_binary(Val); is_atom(Val); is_integer(Val) ->
    Val;
safe_value(Val) ->
    unicode:characters_to_binary(io_lib:format("~p", [Val])).
