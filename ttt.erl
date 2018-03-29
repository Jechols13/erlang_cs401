-module(ttt).
-export([
    new_game/0,
    play_with/1
]).

new_game() ->
    io:format("Welcome! Let's play some Tic Tac Toe!~n"),
    io:format("Tell your oponent to play with: ~w.~n", [node()]),
    io:format("Waiting on opponent...~n"),

    receive
        {join, Partner_PID} ->
            io:format("PID ~w has joined the game!~n", [Partner_PID]),
            Partner_PID ! confirm,
            io:format("Sent partner confirmation.~n")
    end.

play_with(Partner) ->
    io:format("Atempting to join game with ~w~n", [Partner]),
    case net_adm:ping(Partner) of 
        pong ->
            io:format("Partner found!");
        pangs ->
            io:format("ERROR: Partner not found! Exiting..."),
            halt()
    end,
    Partner ! {join, self()},

    receive
        confirm ->
            io:format("Partner confirmation received!~n")
    end.