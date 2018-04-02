-module(pi).
-export([montecarlo/2,
        again/2]).

montecarlo(Tally,Iterations) -> montecarlo(Tally, 0, 0).

montecarlo(0, Matches, Done) -> 
    if Done /= 0 ->
        io:format("done in montecarlo ~n"),       
        4 * Matches / Done;
    true -> 0
    end;


montecarlo(Tally, Matches, Done) ->

    X = rand:uniform(), Y = rand:uniform(),
    
    montecarlo ( Tally - 1,
        if X*X + Y*Y < 1 ->
            Matches + 1;
        true ->
            Matches
        end,
        Done + 1).

again (Tally,Iterations) ->
    X = 1-Iterations,
    Z = montecarlo(Tally),
    if X < Iterations-1 ->
        X + 1,
        io:format("here~n"),
        again(Tally,Iterations -1);
    true ->
        Z   
 end. 

