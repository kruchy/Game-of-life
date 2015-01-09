-module(game).
%-export([set/3,get/2,count/2,count2/2,checkCell/3,]).
-compile(export_all).	
-define(DEAD_TO_ALIVE,[3]).
-define(KEEP_ALIVE,[2,3]).
%-import(view,start).

checkXY(W,H,X,Y) when (X >0 ) and (X < W) and (Y < H) and (Y >0)-> true;
checkXY(_,_,_,_) -> false.

getA(Board,X,Y) -> array:get(Y,array:get(X,Board)).

getFromCenter(Board,X,Y) -> case checkXY(array:size(Board),array:size(array:get(0,Board)),X,Y) of
								true-> getA(Board,X,Y);
								false -> []
							end.


set( X, Y, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).



startGame(W,H)->
	Board = createBoard(W,H),
	initBoard(Board,W,H),
	view:start(W,H),
	spawn(?MODULE,next,[Board,W,H]).


next(Board,W,H)->
	receive 
		after 100 -> nextIteration(Board,W,H)
	end,
	next(Board,W,H).

nextIteration(Board,Height,Width) ->
	[ [getA(Board,X,Y) ! {next} || Y <- lists:seq(1,Height-2) ] || X <- lists:seq(1,Width-2)].

createColumn(_,0) -> [];
createColumn(X,Height) -> [create_cell(X,Height) | createColumn(X,Height-1)].

createBoard(Width,Height) when Height < 3 orelse Width < 3 -> throw('invalid size');
createBoard(Width,Height) -> 
	Table = [[ create_cell(X,Y) || Y <- lists:seq(0,Height-1)] || X <- lists:seq(0,Width - 1)],
	array:map(fun(_,Value) -> array:from_list(Value) end,array:from_list(Table)).

drawBoard(Board)->
	New = [array:to_list(L) || L <- array:to_list(Board) ],
	lists:foreach(fun(A) -> io:fwrite("~p~n",[A]) end,New).


initBoard(Board,Width,Height) -> 
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
			Y <- lists:seq(0,Height-1) ] || X <- lists:seq(0,Width-1)].


create_cell(X,Y) ->
	spawn(?MODULE,cell,[X,Y,dead,[],0,0]).

cell(X,Y,State,Neighbors,Living,Received) ->
	receive
		{init,NewN} -> cell(X,Y,State,NewN,0,0);
		{state,dead} -> case Received =:= length(Neighbors) of		
							true -> %io:fwrite("dead ~p ~p ~p ~n",[X,Y,Neighbors]),
							cell(X,Y,determineState(State,Living),Neighbors,0,0);
							false -> cell(X,Y,State,Neighbors,Living,Received + 1)
						end;
		
		{state,alive} -> case Received =:= length(Neighbors) of
							true -> cell(X,Y,determineState(State,Living + 1),Neighbors,0,0);
							false -> io:fwrite("alive ~p ~p ~n",[Received,Living]),
							cell(X,Y,State,Neighbors,Living + 1,Received + 1)
						end;
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

 sendState([],_) -> ok;
 sendState([First|Rest],State) -> io:fwrite("~p ~p ~p ~n",[State,First,Rest]), First ! {state,State},sendState(Rest,State).



