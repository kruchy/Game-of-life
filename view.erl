-module(view).
-compile(export_all).



initFrame(Width,Height) ->
	 array:new( [{size, Width}, {default, array:new( [{size, Height},{default,o}] )}] ).

start(Width,Height)->
	register(frame,spawn(?MODULE,initFrame,[Width,Height])).

set(X,Y,Value,Frame) ->
	array:set(Y,Value,array:get(X,Frame)).


loop(Frame,Width,Height,Received) when Width*Height == Received ->
	New = [array:to_list(L) || L <- array:to_list(Frame) ],
	lists:foreach(fun(A) -> io:fwrite("~p~n",[A]) end,New),
	loop(Frame,Width,Height,0);

loop(Frame,Width,Height,Received) ->
	receive 
	{update,X,Y,State} -> case State of
								alive -> NewFrame = set(X,Y,x,Frame), loop(NewFrame,Width,Height,Received); 
								dead  -> NewFrame = set(X,Y,x,Frame), loop(NewFrame,Width,Height,Received) 
						end
	end.