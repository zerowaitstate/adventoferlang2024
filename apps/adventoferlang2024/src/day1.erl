%%%-----------------------------------
%% @doc day1 logic
%%%-----------------------------------

-module(day1).

-export([part1/0]).

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

process_input(TwoTupleList) ->
    {L1, L2} = lists:unzip(TwoTupleList),
    L1_Sorted = lists:sort(L1),
    L2_Sorted = lists:sort(L2),
    lists:sum(lists:zipwith(fun(X,Y) -> abs(X-Y) end, L1_Sorted, L2_Sorted)).

part1() ->
    Fn = "day1.txt",
    Data = load_file(filename:absname_join(code:priv_dir(adventoferlang2024), Fn)),
    process_input(Data).
