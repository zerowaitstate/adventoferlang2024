%%%-----------------------------------
%% @doc day4 logic
%%%-----------------------------------

-module(day4).

-export([part1/0, part2/0]).

-record(topology, {dim_x, dim_y, fields}).
-record(step, {x, y, char}).

print_topo(Topo) -> print_topo(Topo, 0).

print_topo(Topo, Pos) ->
    if
        Pos < Topo#topology.dim_x * Topo#topology.dim_y ->
            io:format("~s\n", [binary:part(Topo#topology.fields, Pos, Topo#topology.dim_x)]),
            print_topo(Topo, Pos + Topo#topology.dim_x);
        true -> ok
    end.

linear_patterns() ->
    [
        [
            #step{x=0, y=0, char='X'},
            #step{x=1, y=0, char='M'},
            #step{x=2, y=0, char='A'},
            #step{x=3, y=0, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=-1, y=0, char='M'},
            #step{x=-2, y=0, char='A'},
            #step{x=-3, y=0, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=0, y=1, char='M'},
            #step{x=0, y=2, char='A'},
            #step{x=0, y=3, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=0, y=-1, char='M'},
            #step{x=0, y=-2, char='A'},
            #step{x=0, y=-3, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=1, y=1, char='M'},
            #step{x=2, y=2, char='A'},
            #step{x=3, y=3, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=-1, y=-1, char='M'},
            #step{x=-2, y=-2, char='A'},
            #step{x=-3, y=-3, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=1, y=-1, char='M'},
            #step{x=2, y=-2, char='A'},
            #step{x=3, y=-3, char='S'}
        ],
        [
            #step{x=0, y=0, char='X'},
            #step{x=-1, y=1, char='M'},
            #step{x=-2, y=2, char='A'},
            #step{x=-3, y=3, char='S'}
        ]
    ].

x_patterns() ->
    [
        [
            #step{x=0, y=0, char='M'},
            #step{x=2, y=0, char='S'},
            #step{x=1, y=1, char='A'},
            #step{x=0, y=2, char='M'},
            #step{x=2, y=2, char='S'}
        ],
        [
            #step{x=0, y=0, char='M'},
            #step{x=2, y=0, char='S'},
            #step{x=1, y=1, char='A'},
            #step{x=0, y=2, char='M'},
            #step{x=2, y=2, char='S'}
        ],
        [
            #step{x=0, y=0, char='M'},
            #step{x=2, y=0, char='S'},
            #step{x=1, y=1, char='A'},
            #step{x=0, y=2, char='M'},
            #step{x=2, y=2, char='S'}
        ],
        [
            #step{x=0, y=0, char='M'},
            #step{x=2, y=0, char='S'},
            #step{x=1, y=1, char='A'},
            #step{x=0, y=2, char='M'},
            #step{x=2, y=2, char='S'}
        ],
    ].

sample() ->
    S = <<"MMMSXXMASMMSAMXMSMSAAMXSXMAAMMMSAMASMSMXXMASAMXAMMXXAMMXXAMASMSMSASXSSSAXAMASAAAMAMMMXMMMMMXMXAXMASX">>,
    #topology{dim_x=10, dim_y=10, fields=S}.

part1() ->
    T = sample(),
    print_topo(T).    

part2() -> ok.
