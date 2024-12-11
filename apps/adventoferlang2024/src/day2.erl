%%%-----------------------------------
%% @doc day1 logic
%%%-----------------------------------

-module(day2).

-export([part1/0, part2/0, summarize/2]).

load_file(Fn) ->
    {ok, Device} = file:open(Fn, [read]),
    try load_file_loop(Device)
        after file:close(load_file_loop)
    end.

load_file_loop(IoDevice) ->
    case io:get_line(IoDevice, "") of
        eof -> [];
        Data ->
            Report = lists:map(fun(X)->list_to_integer(X) end, string:lexemes(string:trim(Data), " ")),
            [Report|load_file_loop(IoDevice)]
    end.

puzzle_input() ->
    Fn = "day2.txt",
    AbsFn = filename:absname_join(code:priv_dir(adventoferlang2024), Fn),
    load_file(AbsFn).

%%%------------------------------
%% part 1 stuff
%%%------------------------------

summarize([FirstTerm|[]], Misses) -> {ok, FirstTerm, none, Misses, 1};
summarize([FirstTerm|Rest], Misses) ->
    Result = summarize(Rest, Misses),
    case Result of
        {ok, SecondTerm, none, Rem, Levels} when (FirstTerm < SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, inc, Rem, Levels + 1};
        {ok, SecondTerm, none, Rem, Levels} when (FirstTerm > SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, dec, Rem, Levels + 1};
        {ok, SecondTerm, inc, Rem, Levels} when (FirstTerm < SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, inc, Rem, Levels + 1};
        {ok, SecondTerm, dec, Rem, Levels} when (FirstTerm > SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, dec, Rem, Levels + 1};
        {ok, SecondTerm, Dir, Rem, Levels} when Rem > 0 ->
            {ok, SecondTerm, Dir, Rem - 1, Levels + 1};
        _Else ->
            {failed}
    end.

part1() ->
    Results = lists:filter(fun(X) -> element(1,summarize(X, 0)) == ok end, puzzle_input()),
    length(Results).

%%%------------------------------
%% part 2 stuff
%%%------------------------------

part2() ->
    Results = lists:filter(fun(X) -> element(1,summarize(X, 1)) == ok end, puzzle_input()),
    length(Results).
