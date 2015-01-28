-module(com).
-compile(export_all).
%% Author : Krzysztof Misiak



% main function

start(Width,Height,_) when Width < 3 , Height < 3 -> throw(invalid_size);
start(_,_,Alive) when length(Alive) =:= 0 -> throw(empty_board);

start(Width, % first dimension
	  Height, % second dimension
	  Alive % list of alive cells
	  ) ->
	case (lists:member(com,registered())) of
		true -> ok;
		false -> register(com,spawn_link(view,loop,[view:initBoard(Width,Height),Width,Height,0]))
	end,
	spawn_link(game,startGame,[Width,Height,Alive]).
	

%%main loop, receives messages from the model and passes them to the view module

loop() ->
	receive 
	{update,X,Y,State} -> view ! {update,X,Y,State},loop(); 
	{draw} 				-> view ! {draw} , loop() 
	end.

