-module(ttt).
-export([
    new_game/0,
    play_with/1
]).

new_game() ->
    io:format("Welcome! Let's play some Tic Tac Toe!~n"),
    io:format("Please enter name."),
    io:format("If you use spaces or capital letters, tell partner to surround your name with single quotes.)~n"),
    {ok, [Name]} = io:fread("Name: ", "~s"),
    register(list_to_atom(Name), self()),
    io:format("Welcome, ~w~nWaiting on opponent...~n", [Name]),

    receive
        {join, Partner_PID} ->
            io:format("PID ~w has joined the game!~n", [Partner_PID]),
            Partner_PID ! confirm,
            io:format("Sent partner confirmation.~n")
    end.

play_with(Partner) ->
    io:format("Atempting to join game with ~w~n", [Partner]),
    case lists:member(Partner, registered()) of 
        true ->
            io:format("Partner found!");
        false ->
            io:format("ERROR: Partner not found! Exiting..."),
            halt()
    end,
    Partner ! {join, self()},

    receive
        confirm ->
            io:format("Partner confirmation received!~n")
    end.