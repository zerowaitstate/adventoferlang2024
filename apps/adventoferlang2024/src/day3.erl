%%%-----------------------------------
%% @doc day1 logic
%%%-----------------------------------

-module(day3).

-export([part1/0, part2/0]).

load_file(Fn) -> 
    case file:read_file(Fn) of
        {ok, Binary} ->
            binary_to_list(Binary)
    end.

puzzle_input() ->
    Fn = "day3.txt",
    AbsFn = filename:absname_join(code:priv_dir(adventoferlang2024), Fn),
    load_file(AbsFn).

op_regex() ->
    {ok, RE} = re:compile("(mul|do|don't)\\(([\\d,]*)\\)"),
    RE.

mul_op(Args, State) ->
    case State of
        {enabled, Acc} ->
            Lexemes = string:lexemes(Args, ","),
            Parsed_Lexemes = lists:map(fun(L) -> list_to_integer(L) end, Lexemes),
            Result = lists:nth(1,Parsed_Lexemes) * lists:nth(2,Parsed_Lexemes),
            {enabled, Acc + Result};
        _Else ->
            State
    end.

do_op(_Args, State) ->
    {_, Acc} = State,
    {enabled, Acc}.

dont_op(_Args, State) ->
    {_, Acc} = State,
    {disabled, Acc}.

part2_handle_match(M, State) ->
    Opcode = lists:nth(2,M),
    Args = lists:nth(3,M),
    case Opcode of
        "mul" ->
            mul_op(Args, State);
        "do" ->
            do_op(Args, State);
        "don't" ->
            dont_op(Args, State)
    end.

part1_handle_match(M, State) ->
    Opcode = lists:nth(2,M),
    Args = lists:nth(3,M),
    case Opcode of
        "mul" ->
            mul_op(Args, State);
        _Else ->
            State
    end.

part1() ->
    S = puzzle_input(),
    {match, Matches} = re:run(S, op_regex(), [global, {capture, all, list}]),
    State = lists:foldl(fun(X,AccIn) -> part1_handle_match(X,AccIn) end, {enabled, 0}, Matches),
    {_, Acc} = State,
    Acc.

%%%-----------------------------------
%% part 2 stuff
%%%-----------------------------------

part2() ->
    S = puzzle_input(),
    {match, Matches} = re:run(S, op_regex(), [global, {capture, all, list}]),
    State = lists:foldl(fun(X,AccIn) -> part2_handle_match(X,AccIn) end, {enabled, 0}, Matches),
    {_, Acc} = State,
    Acc.
