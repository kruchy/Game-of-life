-module(view).
-compile(export_all).
% Author : Krzysztof Misiak


% Initiates view board with given size.
initBoard(Width,Height) ->
	 array:new( [{size, Width}, {default, array:new( [{size, Height},{default,'o'}] )}] ).



	
%printing the board on the screen
drawBoard(Board)->
	New = [array:to_list(L) || L <- array:to_list(Board) ],
	lists:foreach(fun(A) -> printTable(A) end,New),
	io:format("~n").
printTable([]) -> io:format("|~n",[]);
printTable([Head|Tail]) -> case (Head == 'x') of
								true -> io:format("~s",['o']);
								false -> io:format(" ",[])
							end, printTable(Tail).


set( X, Y, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).


%main loop, prints the board if received every message from every cell process
loop(Board,Width,Height,Received) when Width*Height =:= Received ->
	drawBoard(Board),
	loop(Board,Width,Height,0);

loop(Board,Width,Height,Received) ->
	receive 
	{update,X,Y,State} -> case State of
								alive -> NewBoard = set(X,Y,x,Board), loop(NewBoard,Width,Height,Received+1); 
								dead  -> NewBoard = set(X,Y,o,Board), loop(NewBoard,Width,Height,Received+1) 
						end;
	{draw} 				-> drawBoard(Board), loop(Board,Width,Height,Received) 
	end.

