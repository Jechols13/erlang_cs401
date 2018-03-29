-module(test).
-export([newgame/0, playwith/1, stop/0, create_empty_board/0, wait_opponent/0, connect_opponent/1, tell/1]).
% -export([update_board/3, create_empty_board/0, check/1]).

wait_msg(YourSym, TheirSym, Board, OpponentPID, Turn) ->
    receive
	
	
%This is called by the owner node
%The process then forwards the message to the opponent
	{sendmsg, Msg} ->
		UpdatedBoard = Board,
		OpponentPID ! {message, Msg};

%A new message has arrived from the opponent
	{message, Msg} ->
		UpdatedBoard = Board,
		io:format("Msg: ~w~n", [Msg])
		end,
		wait_msg(YourSym, TheirSym, UpdatedBoard, OpponentPID, Turn).

		
create_empty_board() ->
	{'_', '_', '_',
	 '_', '_', '_',
	 '_', '_', '_'}.
	 
						%PlayerX waiting for PlayerY
wait_opponent() ->
	receive
	{connect, PlayerY_PID} ->
		io:format("Another player joined.~n", []),
		PlayerY_PID ! {gamestart, self() }, random:seed(now()),
		R = random:uniform(), %better to have a seed for random number
		io:format("Random = ~w~n", [R]),
		Board = create_empty_board(),
		
	if
	R > 0.5 ->
						%current player starts
			io:format("You will start first~n", []), Turn = self();
		true ->
						%the other player starts
			PlayerY_PID ! {message, 'You will start first.~n'}, Turn = PlayerY_PID
		end,
		wait_msg(x, o, Board, PlayerY_PID, Turn)
			
	end.
		
						%PlayerY trying to connect to X
connect_opponent(XNode) ->
	{player, XNode} ! {connect, self()},
	receive
	{gamestart, PlayerX_PID} ->
		io:format("Connection successful.~n", []),
		Board = create_empty_board(),
		wait_msg(o, x, Board, PlayerX_PID, self())
	end.
	
	
						%sends the message to the same node: wait_msg
						%The node will then forward the message to the opponent
tell(Message) ->
	{player, node()} ! {sendmsg, Message}.
	
						%starts a new game node and waits for an opponent
newgame() ->
	register(player, spawn(test, connect_opponent, [])).
	
						%connects to another Erlang node identified by Opponent and starts a new game.
playwith(XNode) ->
	register(player, spawn(test, connect_opponent, [XNode])).
	
stop() ->
	player!{self(), reqstop},
	unregister(player).