-module(montecarlo).
-export([calc/1 ).

calc(Tally) ->
calc(Tally, 0, 0).
calc(0, Matches, Done) -> 4 * Matches / Done;
calc ( Tally - 1,
    if X*X + Y*Y < 1 ->
        Matches + 1;
    true ->
        Matches
    end,
    Done + 1
).



