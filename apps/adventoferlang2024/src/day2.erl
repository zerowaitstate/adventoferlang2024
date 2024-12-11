%%%-----------------------------------
%% @doc day1 logic
%%%-----------------------------------

-module(day2).

-export([part1/0, part2/0]).

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

summarize([FirstTerm|[]]) -> {ok, FirstTerm, none};
summarize([FirstTerm|Rest]) ->
    case summarize(Rest) of
        {ok, SecondTerm, none} when (FirstTerm < SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, inc};
        {ok, SecondTerm, none} when (FirstTerm > SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, dec};
        {ok, SecondTerm, inc} when (FirstTerm < SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, inc};
        {ok, SecondTerm, dec} when (FirstTerm > SecondTerm) and (abs(FirstTerm - SecondTerm) =< 3) ->
            {ok, FirstTerm, dec};
        {failed, SecondTerm, Dir} ->
            {failed, SecondTerm, Dir};
        {ok, SecondTerm, Dir} ->
            {failed, SecondTerm, Dir}
    end.

part1() ->
    Results = lists:filter(fun(X) -> {Res, _, _} = summarize(X), Res == ok end, puzzle_input()),
    length(Results).

%%%------------------------------
%% part 2 stuff
%%%------------------------------

part2() ->
    ok.
