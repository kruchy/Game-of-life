-module(game).
%-export([set/3,get/2,count/2,count2/2,checkCell/3,]).
-compile(export_all).	
-define(DEAD_TO_ALIVE,[3]).
-define(KEEP_ALIVE,[2,3]).
%-import(view,start).
-record(board,{area,width,height}).

checkXY(W,H,X,Y) when (X >0 ) and (X < W-1) and (Y < H-1) and (Y >0)-> true;
checkXY(_,_,_,_) -> false.

getA(Board,X,Y) -> array:get(Y,array:get(X,Board#board.area)).

getFromCenter(Board,X,Y) -> case checkXY(Board#board.width,Board#board.height,X,Y) of
								true-> getA(Board,X,Y);
								false -> none
							end.





set( X, Y, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).



startGame(_,_,Alive) when length(Alive) =:= 0 -> throw('empty board');
startGame(W,H,Alive)->
	Board = createBoard(W,H),
	initBoard(Board),
	Frame = view:initFrame(W,H),
	case (lists:member(frame,registered())) of
		true -> ok;
		false -> register(frame,spawn(view,loop,[Frame,W,H,0]))
	end,
	setAlive(Board,Alive),
	frame ! {draw},
	spawn(?MODULE,next,[Board]).

setAlive(_,[]) -> ok;
setAlive(Board, [{X,Y}]) -> getA(Board,X,Y)! {set,alive}; 
setAlive(Board,[{X,Y}|Rest]) -> getA(Board,X,Y) ! {set,alive}, setAlive(Board,Rest). 

next(Board)->
	receive 
		after 500 -> nextIteration(Board)
	end,
	%frame ! {draw},
	next(Board).

sendNext(Pid) -> Pid ! {next}.

nextIteration(Board) ->
	[ [ sendNext(getA(Board,X,Y)) || Y <- lists:seq(0,Board#board.height-1) ] || X <- lists:seq(0,Board#board.width-1)].


writeSmt(W,H) -> [ [ io:format("~p ~p~n",[X,Y]) || Y <- lists:seq(0,H-1) ] || X <- lists:seq(0,W-1)].


createBoard(Width,Height)->
	#board{area = createArea(Width,Height),width = Width, height = Height}.

createArea(Width,Height) when Height < 3 , Width < 3 -> throw('invalid size');
createArea(Width,Height) -> 
	Table = [[ create_cell(X,Y) || Y <- lists:seq(0,Height-1)] || X <- lists:seq(0,Width - 1)],
	array:map(fun(_,Value) -> array:from_list(Value) end,array:from_list(Table)).

drawBoard(Board)->
	New = [array:to_list(L) || L <- array:to_list(Board) ],
	lists:foreach(fun(A) -> io:format("~p~n",[A]) end,New).

glider()-> [{5,4},{3,5},{5,5},{4,6},{5,6}].


initBoard(Board) -> 
	[[ 
		getA(Board,X,Y) ! {init,
			[getFromCenter(Board,X-1,Y-1),
			getFromCenter(Board,X-1,Y),
			getFromCenter(Board,X-1,Y+1),
			getFromCenter(Board,X,Y-1),
			getFromCenter(Board,X,Y+1),
			getFromCenter(Board,X+1,Y-1),
			getFromCenter(Board,X+1,Y),
			getFromCenter(Board,X+1,Y+1)
			]} || 
			Y <- lists:seq(0,Board#board.height-1) ] || X <- lists:seq(0,Board#board.width-1)].


create_cell(X,Y) ->
	spawn_link(?MODULE,cell,[X,Y,dead,[],0,0]).

cell(X,Y,State,Neighbors,Living,Received) ->
	receive
		{init,NewN} -> cell(X,Y,State,NewN,0,0);
		
		{set,NewS} -> frame ! {update,X,Y,NewS} ,cell(X,Y,NewS,Neighbors,Living,Received);

		{state,dead} -> case Received  of		
							7 ->  NewS = determineState(State,Living), 
									 frame ! {update,X,Y,NewS},
									 cell(X,Y,NewS,Neighbors,0,0);
							_ -> cell(X,Y,State,Neighbors,Living,Received + 1)
						end;
		
		{state,alive} -> case Received  of
							7 ->  NewS = determineState(State,Living+1),
									 frame ! {update,X,Y,NewS}, 
									 cell(X,Y,NewS,Neighbors,0,0);
							_ -> cell(X,Y,State,Neighbors,Living + 1,Received + 1)
						end;
		{debug} -> io:format("~p ~p ~p ~p ~p ~p~n",[X,Y,State,Neighbors,Living,Received]),
		cell(X,Y,State,Neighbors,Living,Received); 

		{next} -> 
					sendState(Neighbors,State),
					cell(X,Y,State,Neighbors,Living,Received)
	end.
 
determineState(alive,Living) -> 
	case (lists:member(Living,?KEEP_ALIVE)) of
		true  -> alive  ;
		false -> dead
	end;

determineState(dead,Living) -> 
	case (lists:member(Living,?DEAD_TO_ALIVE)) of
		true  -> alive; 
		false -> dead
	end. 

sendState(Neighbors,State) -> lists:foreach(fun(Pid) -> sendHelper(Pid,State) end,Neighbors).
sendHelper(none,_) -> ok;
sendHelper(Pid,State) -> Pid ! {state,State}.