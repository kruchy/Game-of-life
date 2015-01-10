-module(view).
-compile(export_all).



initFrame(Width,Height) ->
	 array:new( [{size, Width}, {default, array:new( [{size, Height},{default,o}] )}] ).

start(Width,Height)->
	spawn(?MODULE,initFrame,[Width,Height]).

%set(X,Y,Value,Frame) ->
%	array:set(Y,Value,array:get(X,Frame)).
drawBoard(Board)->
	New = [array:to_list(L) || L <- array:to_list(Board) ],
	lists:foreach(fun(A) -> io:format("~p~n",[A]) end,New).

set( X, Y, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).

loop(Frame,Width,Height,Received) when Width*Height == Received ->
	drawBoard(Frame),
	loop(Frame,Width,Height,0);

loop(Frame,Width,Height,Received) ->
	receive 
	{update,X,Y,State} -> case State of
								alive -> NewFrame = set(X,Y,x,Frame), loop(NewFrame,Width,Height,Received); 
								dead  -> NewFrame = set(X,Y,o,Frame), loop(NewFrame,Width,Height,Received) 
						end;
	{draw} 				-> drawBoard(Frame), loop(Frame,Width,Height,Received) 
	end.