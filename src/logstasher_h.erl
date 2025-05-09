-module(logstasher_h).

%% logger callbacks
-export([log/2]).

%% Testing
-export([log_data/1]).

%% Xref ignores
-ignore_xref([log/2, log_data/1]).

%% Truncate binary values beyond this size.
-define(LOG_BINARY_SIZE, 2000).

%%==============================================================================
%% API
%%==============================================================================

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := info, meta := #{error_logger := #{type := progress}}}, _Config) ->
    % Ignore supervisor progress reports
    ok;
log(LogEvent, _Config) ->
    try
        Data = log_data(LogEvent),
        _ = logstasher:send(jsx:encode(Data)),
        ok
    catch
        _:_ ->
            % Ignore crashes on unexpected data, as that would remove the log
            % handler from the logger and stop logging.
            ok
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

log_data(#{level := Level, msg := EventData, meta := Meta}) ->
    {Msg, MsgFields} = format_msg(EventData),
    {Msg1, MsgFields1} = maybe_extract_message(Msg, MsgFields),
    Fields = maps:merge(safe_meta(Meta), MsgFields1),
    Fields1 = Fields#{ severity => Level },
    #{
        fields => Fields1,
        '@timestamp' => format_timestamp(Meta),
        message => Msg1
    }.

%% @doc If there is no message, try to extract the 'text' fields from the message fields
%% and use that as the message.
maybe_extract_message(null, #{ text := Text } = MsgFields) when is_binary(Text) ->
    {Text, maps:remove(text, MsgFields)};
maybe_extract_message(Msg, MsgFields) ->
    {Msg, MsgFields}.


-spec format_msg(Data) -> {Message, #{ Key => Value } } when
    Data :: {io:format(), [ term() ]}
          | {report, logger:report()}
          | {string, unicode:chardata()},
    Message :: binary() | null,
    Key :: binary() | atom(),
    Value :: jsx:json_term().
format_msg({string, Message}) ->
    {unicode:characters_to_binary(Message), #{}};
format_msg({report, Report}) when is_map(Report) ->
    {maps:get(msg, Report, null), safe_fields(Report)};
format_msg({report, Report}) when is_list(Report) ->
    format_msg({report, maps:from_list(Report)});
format_msg({"Error in process ~p on node ~p with exit value:~n~p~n", [_, _, {undef, Undef}]}) ->
    format_undef(Undef);
format_msg({Format, Params}) when is_list(Format), is_list(Params) ->
    {unicode:characters_to_binary(io_lib:format(Format, Params)), #{}};
format_msg(Other) ->
    {unicode:characters_to_binary(io_lib:format("~p", [ Other ])), #{}}.

format_undef([ {Module, Function, Args, _} | _ ] = Stack) when is_list(Args) ->
    Arity = length(Args),
    Message = io_lib:format("Undefined function ~p:~p/~p", [Module, Function, Arity]),
    Report = #{
        result => error,
        reason => undef,
        module => Module,
        function => Function,
        args => Args,
        stack => Stack
    },
    {unicode:characters_to_binary(Message), safe_fields(Report)}.

-spec format_timestamp(logger:metadata()) -> binary().
format_timestamp(#{time := Ts}) ->
    list_to_binary(calendar:system_time_to_rfc3339(Ts, [{unit, microsecond}, {offset, "Z"}])).

-spec safe_meta(logger:metadata()) -> #{ Key => Term } when
    Key :: binary() | atom(),
    Term :: jsx:json_term().
safe_meta(Meta) ->
    safe_fields(Meta).

-spec safe_fields(map()) -> map().
safe_fields(Terms) ->
    maps:fold(
        fun(K, V, Acc) ->
            {K1, V1} = safe_field(K, V),
            Acc#{ K1 => V1 }
        end,
        #{},
        Terms).

-spec safe_field(atom() | binary() | string(), term()) -> {atom() | binary(), jsx:json_term()}.
safe_field(stack, Stack) when is_list(Stack) ->
    {stack, safe_stack(Stack)};
safe_field(file, Filename) when is_list(Filename) ->
    {file, unicode:characters_to_binary(Filename)};
safe_field(Key, Value) when is_atom(Key); is_binary(Key) ->
    {Key, safe_value(Value)};
safe_field(Key, Value) when is_list(Key) ->
    safe_field(unicode:characters_to_binary(Key), Value).

safe_stack(Stack) ->
    lists:map(fun safe_stack_entry/1, Stack).

safe_stack_entry({Mod, Fun, Args, _}) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    Arity = length(Args),
    Function = io_lib:format("~p:~p/~p", [Mod, Fun, Arity]),
    #{
        function => unicode:characters_to_binary(Function)
    };
safe_stack_entry({Mod, Fun, Arity, Loc}) when is_atom(Mod), is_atom(Fun), is_integer(Arity) ->
    Function = io_lib:format("~p:~p/~p", [ Mod, Fun, Arity ]),
    #{
        function => unicode:characters_to_binary(Function),
        at => unicode:characters_to_binary([stack_file(Loc), $:, integer_to_binary(stack_line(Loc))])
    };
safe_stack_entry(Entry) ->
    safe_value(Entry).

stack_file(Loc) when is_list(Loc) -> proplists:get_value(file, Loc, "");
stack_file({File, _}) -> File;
stack_file({File, _, _}) -> File;
stack_file(_) -> "".

stack_line([ {_, _} | _ ] = Loc) -> proplists:get_value(line, Loc, "");
stack_line({_, Line}) -> Line;
stack_line({_, Line, _}) -> Line;
stack_line(_) -> 0.

-spec safe_value(term()) -> jsx:json_term().
safe_value(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
safe_value([]) ->
    [];
safe_value(List) when is_list(List) ->
    case is_proplist(List) of
        true -> safe_value(map_from_proplist(List));
        false ->
            case is_ascii_list(List) of
                true -> unicode:characters_to_binary(List);
                false -> lists:map(fun safe_value/1, List)
            end
    end;
safe_value(Map) when is_map(Map) ->
    safe_fields(Map);
safe_value(undefined) ->
    null;
safe_value(Val) when is_atom(Val); is_number(Val) ->
    Val;
safe_value(Val) when is_binary(Val) ->
    maybe_truncate(Val);
safe_value(Val) ->
    maybe_truncate(unicode:characters_to_binary(io_lib:format("~p", [Val]))).

% Map a proplists to a map
map_from_proplist(L) ->
    lists:foldl(
        fun
            ({K,V}, Acc) -> Acc#{ K => V };
            (K, Acc) -> Acc#{ K => true }
        end,
        #{},
        L).

% If something is a proplist, then we will display it as a map.
is_proplist([]) -> true;
is_proplist([ {K, _} | T ]) when is_atom(K); is_binary(K) -> is_proplist(T);
is_proplist([ K | T ]) when is_atom(K) -> is_proplist(T);
is_proplist(_) -> false.

% Simple ASCII character string, typically SQL statements, filenames or literal texts.
is_ascii_list([]) -> true;
is_ascii_list([ C | T ]) when C >= 32, C =< 127 -> is_ascii_list(T);
is_ascii_list([ C | T ]) when C =:= $\n; C =:= $\t -> is_ascii_list(T);
is_ascii_list(_) -> false.

maybe_truncate(<<Truncated:?LOG_BINARY_SIZE/binary, _/binary>>) ->
    Truncated;
maybe_truncate(Bin) ->
    Bin.
