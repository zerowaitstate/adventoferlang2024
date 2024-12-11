%%%-----------------------------------
%% @doc day1 logic
%%%-----------------------------------

-module(day1).

-export([part1/0, part2/0]).

load_file(Fn) ->
    {ok, Device} = file:open(Fn, [read]),
    try load_file_loop(Device)
        after file:close(load_file_loop)
    end.

load_file_loop(IoDevice) ->
    case io:fread(IoDevice, "", "~d ~d") of
        eof -> [];
        {ok, [First|[Second]]} -> [{First,Second}|load_file_loop(IoDevice)]
    end.

puzzle_input() ->
    Fn = "day1.txt",
    AbsFn = filename:absname_join(code:priv_dir(adventoferlang2024), Fn),
    load_file(AbsFn).

%%%-----------------------------------
%% part 1 stuff
%%%-----------------------------------

sort_diff_sum(TwoTupleList) ->
    {L1, L2} = lists:unzip(TwoTupleList),
    L1_Sorted = lists:sort(L1),
    L2_Sorted = lists:sort(L2),
    lists:sum(lists:zipwith(fun(X,Y) -> abs(X-Y) end, L1_Sorted, L2_Sorted)).

part1() ->
    sort_diff_sum(puzzle_input()).

%%%-----------------------------------
%% part 2 stuff
%%%-----------------------------------

get_freq_product(FreqTable, E) ->
    Freq = case ets:lookup(FreqTable, E) of
        [] -> 0;
        [{_Key, Val}] -> Val
    end,
    E * Freq.

part2() ->
    FreqT = ets:new(freq, [set]),
    Input = puzzle_input(),
    {L1, L2} = lists:unzip(Input),
    lists:foreach(fun(E) -> ets:update_counter(FreqT, E, 1, {0, 0}) end, L2),
    Total = lists:sum(lists:map(fun(E) -> get_freq_product(FreqT, E) end, L1)),
    ets:delete(FreqT),
    Total.
