-module(test).
-export([newgame/0,playwith/1,stop/0,create_empty_board/0,wait_opponent/0,connect_opponent/1,tell/1]).


wait_msg(YourSym,HisSym,Board,OpponentPID,Turn) -> receive

%this is called by the owner node.
%this process then forwards the message to the opponent
    {send_msg,Msg} ->
        UpdatedBoard = Board,
        OpponentPID !{message,Msg};
%A new message has arrived from the opponent
    {mesage,Msg} ->
        UpdatedBoard = Board,
        io:format("msg : ~w~n",[Msg])
        end,
        wait_msg(YourSym,HisSym,UpdatedBoard,OpponentPID,Turn).

        create_empty_board() ->
        {'_', '_', '_',
        '_', '_', '_',
        '_', '_', '_'}.

        %PlayerX waiting for PlayerY

    wait_opponent() ->
    receive
    {connect, PlayerY_PID} ->
    io:format("Another Player joined. ~n",[]),
    PlayerY_PID ! {gamestart, self()}, randio:seed(now()),
    R = rand:uniform(), %better to have seed for random number
    io:format("Random= ~w~n",[R]),
    Board = create_empty_board(),
        if R > 0.5 ->
            io:format("You will start first ~n", []), Turn = self();
            true ->
                PlayerY_PID ! {message, "You will start first. ~n"}, Turn = PlayerY_PID end,
                wait_msg(x,o,Board,PlayerY_PID,Turn)
                end.

%PlayerY connect to PlayerX
    connect_opponent(XNode) ->
        {player,XNode} ! {connect,self()},
        receive
        {gamestart,PlayerX_PID} ->
        io:format("Connection successful. ~n", []),
        Board = create_empty_board(),
        wait_msg(o,x,Board,PlayerX_PID,self()) end.
%send the message to same node: wait_msg
%the node will then forward the message to the opponent

tell(Message) ->
    {player,node()} ! {sendmsg, Message}.
    
%start a new game

newgame() ->
    register(player, spawn(test,wait_opponent,[])).

playwith(XNode) ->
    register(player, spawn(test,connect_opponent,[XNode])).

    stop() ->
    player!{self(),reqstop},
    unregister(player).