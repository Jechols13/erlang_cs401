-module(t3).
-export([
	new_game/0,
	play_with/1,
	wait_opponent/0,
	connect_opponent/1,
	tell/1
]).


new_board() ->
	maps:new().

draw_board(Board) ->
	List =[ 
		maps:get(a1, Board, "_"), maps:get(a2, Board, "_"), maps:get(a3, Board, "_"),
		maps:get(b1, Board, "_"), maps:get(b2, Board, "_"), maps:get(b3, Board, "_"),
		maps:get(c1, Board, "_"), maps:get(c2, Board, "_"), maps:get(c3, Board, "_")
	],
	io:format("~s ~s ~s~n~s ~s ~s~n~s ~s ~s~n", List).

% validates that Coodinate is a valid move and places a new token.
% move is also communicated to opponent's client
% coordinate is in form of a1, a2, a3, b1, b2, b3, c1, c2, c3
% when winning position is reached or no move can be made
place_token(Board, MySymbol) ->
	io:format("Board coordinates:~na1 a2 a3~nb1 b2 b3~nc1 c2 c3~n~n"),
	draw_board(Board),
	{ok, [PosStr|_]} = io:fread("Postion: ", "~s"),
	PosAtm = list_to_atom(PosStr),
	case lists:member(PosAtm, [a1, a2, a3, b1, b2, b3, c1, c2, c3]) of
		true ->
			case maps:get(PosAtm, Board, "_") of
				"_" ->
					io:format("Move accepted.~n"),
					New_Board = maps:put(PosAtm, MySymbol, Board),
					{New_Board, PosAtm};
				_ ->
					io:format("Invalid move, someone is already there.~n"),
					place_token(Board, MySymbol)
			end;
		false ->
			io:format("Invalid move, input is not valid location.~n"),
			place_token(Board, MySymbol)
	end.
	
check_winner(Board) ->
	case {Board, maps:size(Board)} of
		% all spots are filled, tie
		{_                           , 9} -> tie;

		% horizontal wins
		{#{a1 := N, a2 := N, a3 := N}, _} -> win;
		{#{b1 := N, b2 := N, b3 := N}, _} -> win;
		{#{c1 := N, c2 := N, c3 := N}, _} -> win;

		% vertical wins
		{#{a1 := N, b1 := N, c1 := N}, _} -> win;
		{#{a2 := N, b2 := N, c2 := N}, _} -> win;
		{#{a3 := N, b3 := N, c3 := N}, _} -> win;

		% diagonal wins
		{#{a1 := N, b2 := N, c3 := N}, _} -> win;
		{#{a3 := N, b2 := N, c1 := N}, _} -> win;

		% continue
		{_                           , _} -> continue
	end.

% This function runs in the background (from the spawn calls of wait/connect_opponent)
msg_center(OponentPID, MySymbol) ->
	receive
		%% send message to the opponent
		{sendmsg, Msg} ->
			OponentPID ! {msg, Msg},
			msg_center(OponentPID, MySymbol);
		
		%% receieved a message from the opponent
		{msg, Msg} ->
			io:format("Message from opponent: ~p~n", [Msg]),
			msg_center(OponentPID, MySymbol);
		%% ------------------------------------------
		
		%% begin game, opponent says you go first
		{start, Board} ->
			{New_Board, New_Position} = place_token(Board, MySymbol),
			OponentPID ! {continue, New_Board, New_Position},
			msg_center(OponentPID, MySymbol);
		
		%% game is still on, opponent sends new board and their latest position
		{continue, Board, Position} ->
			io:format("---------------------~nYour opponent has placed their latest token at: ~w~n", [Position]),
			{New_Board, New_Position} = place_token(Board, MySymbol),
			case check_winner(New_Board) of
				win ->
					io:format("You win!~n"),
					OponentPID ! {stop, lose, New_Board};
			   %lose ->  this is not possible, losses are communicated via msg_center
				tie ->
					io:format("Tie!~n"),
					OponentPID ! {stop, tie, New_Board};
				continue ->
					OponentPID ! {continue, New_Board, New_Position},
					msg_center(OponentPID, MySymbol)
			end;
		%% -----------------------------------------

		%% game is over, opponent says we tied
		{stop, tie, Board} ->
			io:format("Tie!~n"),
			draw_board(Board),
			io:format("Game Over.~n");
		%% -----------------------------------------

		%% game is over, opponent says we lost
		{stop, lose, Board} ->
			io:format("You lose!~n"),
			draw_board(Board),
			io:format("Game Over.~n")
		%% -----------------------------------------
	end.


%PlayerX waiting for PlayerY
wait_opponent() ->
	io:format("You will be X~n"),
	receive
		{connect, PlayerY_PID} ->
			io:format("Another player joined.~n", []),
			PlayerY_PID ! {gamestart, self() },
			% don't need to see with the new rand API vs old deprecated random API
			R = rand:uniform(),
		
			case R > 0.5 of
				true ->
					%current player starts
					io:format("You will start first~n", []),
					{Board, Position} = place_token(new_board(), "X"),
					PlayerY_PID ! {continue, Board, Position};

				false ->
					%the other player starts
					PlayerY_PID ! {start, new_board()}
			end,
			%wait_msg(x, o, Board, PlayerY_PID, Turn)			
			msg_center(PlayerY_PID, "X")
	end.


%PlayerY trying to connect to X
connect_opponent(XNode) ->
	io:format("You will be O~n"),
	{player, XNode} ! {connect, self()},
	receive
		{gamestart, PlayerX_PID} ->
			io:format("Connection successful.~n", []),
			msg_center(PlayerX_PID, "O")
	end.
	
	
%sends the message to the same node: wait_msg
%The node will then forward the message to the opponent
tell(Message) ->
	{player, node()} ! {sendmsg, Message},
	io:format("Your message will be sent after the opponent makes their move.").
	
%starts a new game node and waits for an opponent
new_game() ->
	register(player, spawn(t3, wait_opponent, [])).
	
%connects to another Erlang node identified by Opponent and starts a new game.
play_with(XNode) ->
	register(player, spawn(t3, connect_opponent, [XNode])).