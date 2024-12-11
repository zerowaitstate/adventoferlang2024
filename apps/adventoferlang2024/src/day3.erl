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


%%%-----------------------------------
%% part 1 stuff
%%%-----------------------------------

part1() ->
    S = puzzle_input(),
    {ok, RE} = re:compile("(mul)\\((\\d+),(\\d+)\\)"),
    {match, Matches} = re:run(S, RE, [global, {capture, all, list}]),
    Mults = lists:map(fun(M) -> list_to_integer(lists:nth(3, M)) * list_to_integer(lists:nth(4,M)) end, Matches),
    lists:sum(Mults).

%%%-----------------------------------
%% part 2 stuff
%%%-----------------------------------

part2() -> ok.
