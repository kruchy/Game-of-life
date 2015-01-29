-module(game).

-compile(export_all).

-define(DEAD_TO_ALIVE, [3]).

-define(KEEP_ALIVE, [2, 3]).

-record(board,
	{area, width,
	 height}).%% Author : Krzysztof Misiak

% Demonstration purposes only if board is bigger than 5x5

glider() -> [{4, 3}, {2, 4}, {4, 4}, {3, 5}, {4, 5}].

lightWeight() ->
    [{6, 6}, {7, 6}, {4, 7}, {5, 7}, {7, 7}, {8, 7}, {4, 8},
     {5, 8}, {6, 8}, {7, 8}, {5, 9}, {6, 9}].

checkXY(W, H, X, Y)
    when (X > 0) and (X < W - 1) and (Y < H - 1) and
	   (Y > 0) ->
    true;
checkXY(_, _, _, _) -> false.

getA(Board, X, Y) ->
    array:get(Y, array:get(X, Board#board.area)).

%Function to take only from the center of the board, without the edges
getFromCenter(Board, X, Y) ->
    case checkXY(Board#board.width, Board#board.height, X,
		 Y)
	of
      true -> getA(Board, X, Y);
      false -> none
    end.

%main function, starts the loop
startGame(Width, Height, Alive) ->
    Board = createBoard(Width, Height),
    initBoard(Board),
    setAlive(Board, Alive),
    com ! {draw},
    spawn(?MODULE, next, [Board]).

setAlive(_, []) -> ok;
setAlive(Board, [{X, Y}]) ->
    getA(Board, X, Y) ! {set, alive};
setAlive(Board, [{X, Y} | Rest]) ->
    getA(Board, X, Y) ! {set, alive}, setAlive(Board, Rest).

%main loop, after 500 miliseconds send message to start next iteration
next(Board) ->
    receive  after 500 -> nextIteration(Board) end,
    next(Board).

sendNext(Pid) -> Pid ! {next}.

nextIteration(Board) ->
    [[sendNext(getA(Board, X, Y))
      || Y <- lists:seq(0, Board#board.height - 1)]
     || X <- lists:seq(0, Board#board.width - 1)].

%creates the record with given dimensions
createBoard(Width, Height) ->
    #board{area = createArea(Width, Height), width = Width,
	   height = Height}.

%creates the array
createArea(Width, Height) when Height < 3, Width < 3 ->
    throw('invalid size');
createArea(Width, Height) ->
    Table = [[create_cell(X, Y)
	      || Y <- lists:seq(0, Height - 1)]
	     || X <- lists:seq(0, Width - 1)],
    array:map(fun (_, Value) -> array:from_list(Value) end,
	      array:from_list(Table)).

% send to every cell message with new neighbors
initBoard(Board) ->
    [[getA(Board, X, Y) !
	{init,
	 [getFromCenter(Board, X - 1, Y - 1),
	  getFromCenter(Board, X - 1, Y),
	  getFromCenter(Board, X - 1, Y + 1),
	  getFromCenter(Board, X, Y - 1),
	  getFromCenter(Board, X, Y + 1),
	  getFromCenter(Board, X + 1, Y - 1),
	  getFromCenter(Board, X + 1, Y),
	  getFromCenter(Board, X + 1, Y + 1)]}
      || Y <- lists:seq(0, Board#board.height - 1)]
     || X <- lists:seq(0, Board#board.width - 1)].

% spawns new cell at given location
create_cell(X, Y) ->
    spawn_link(?MODULE, cell, [X, Y, dead, [], 0, 0]).

%main cell function, receives messages from every neighbor and sends them state,
%when received from every neighbor sends message to communicator to update state to draw
cell(X, Y, State, Neighbors, Living, Received) ->
    receive
      {init, NewN} -> cell(X, Y, State, NewN, 0, 0);
      {set, NewS} ->
	  com ! {update, X, Y, NewS},
	  cell(X, Y, NewS, Neighbors, Living, Received);
      {state, dead} ->
	  case Received of
	    7 ->
		NewS = determineState(State, Living),
		com ! {update, X, Y, NewS},
		cell(X, Y, NewS, Neighbors, 0, 0);
	    _ -> cell(X, Y, State, Neighbors, Living, Received + 1)
	  end;
      {state, alive} ->
	  case Received of
	    7 ->
		NewS = determineState(State, Living + 1),
		com ! {update, X, Y, NewS},
		cell(X, Y, NewS, Neighbors, 0, 0);
	    _ ->
		cell(X, Y, State, Neighbors, Living + 1, Received + 1)
	  end;
      {debug} ->
	  io:format("~p ~p ~p ~p ~p ~p~n",
		    [X, Y, State, Neighbors, Living, Received]),
	  cell(X, Y, State, Neighbors, Living, Received);
      {next} ->
	  sendState(Neighbors, State),
	  cell(X, Y, State, Neighbors, Living, Received)
    end.

neighborsLength(List) -> neighborsLength(List, 0).

neighborsLength([], Length) -> Length;
neighborsLength([[] | Rest], Length) ->
    neighborsLength(Rest, Length);
neighborsLength([[_ | Rest]], Length) ->
    neighborsLength(Rest, Length + 1).

determineState(alive, Living) ->
    case lists:member(Living, ?KEEP_ALIVE) of
      true -> alive;
      false -> dead
    end;
determineState(dead, Living) ->
    case lists:member(Living, ?DEAD_TO_ALIVE) of
      true -> alive;
      false -> dead
    end.

sendState(Neighbors, State) ->
    lists:foreach(fun (Pid) -> sendHelper(Pid, State) end,
		  Neighbors).

sendHelper(none, _) -> ok;
sendHelper(Pid, State) -> Pid ! {state, State}.
