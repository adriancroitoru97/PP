:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(state([X, X, X, X, X, X, X, X, X], x, _, Avl)) :-
    empty_board(X),
    positions(Avl).

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards(state(State, _, _, _), [Nw, N, Ne, W, C, E, Sw, S, Se]) :-
    nth0(0, State, Nw), nth0(1, State, N), nth0(2, State, Ne),
    nth0(3, State, W), nth0(4, State, C), nth0(5, State, E),
    nth0(6, State, Sw), nth0(7, State, S), nth0(8, State, Se).



% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(State, UPos, Board) :- getBoards(State, Boards),
                                positions(X), nth0(Index, X, UPos),
                                nth0(Index, Boards, Board).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getUBoard(state([Nw, N, Ne, W, C, E, Sw, S, Se], _, _, _), [Nw1, N1, Ne1, W1, C1, E1, Sw1, S1, Se1]) :-
    getBoardResult(Nw, Nw1), getBoardResult(N, N1), getBoardResult(Ne, Ne1),
    getBoardResult(W, W1), getBoardResult(C, C1), getBoardResult(E, E1),
    getBoardResult(Sw, Sw1), getBoardResult(S, S1), getBoardResult(Se, Se1).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(State, UPos, Pos, Cell) :- getBoard(State, UPos, Board),
                                  getPos(Board, Pos, Cell).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(X), nth0(Index, X, Pos),
                            nth0(Index, Board, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(state(_, x, _, _), x).
getNextPlayer(state(_, 0, _, _), 0).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
predicate(X) :-
   X == ''.

filter(_,[],[]).
filter([P1 | P2], [First | Rest], [First | Tail]) :-
   predicate(P1), filter(P2, Rest, Tail).
filter([_ | P2], [_ | Rest], Result) :-
   filter(P2, Rest, Result).

getNextAvailableBoards(S, Boards) :- initialState(S), positions(Boards).
getNextAvailableBoards(state(S, _, L, _), Boards) :-
    getUBoard(state(S, _, _, _), UBoard),
    (getPos(UBoard, L, x); getPos(UBoard, L, 0); getPos(UBoard, L, r)),
    positions(Y), filter(UBoard, Y, Boards).
getNextAvailableBoards(state(_, _, L, _), Boards) :-
    Boards = [L].



% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, P) :- player_wins(P, Board).
getBoardResult(Board, P) :- countOccurances(Board, 0, A), countOccurances(Board, x, B), 9 is A + B, P = r.
getBoardResult(_, '').
    

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
countOccurances([], _, 0).
countOccurances([H | T], H, N) :- countOccurances(T, H, N1), N is N1 + 1.
countOccurances([_ | T], H1, N) :- countOccurances(T, H1, N).

countAll([], _, 0).
countAll([H | T], X, N) :- countOccurances(H, X, N1), countAll(T, X, N2), N is N2 + N1.

nextTurn(Xes, Zes, P) :- Xes > Zes, P = 0; P = x.

buildState([Nw, N, Ne, W, C, E, Sw, S, Se], L, state(State, P, L, NextAvl)) :-
    nth0(0, State, Nw), nth0(1, State, N), nth0(2, State, Ne),
    nth0(3, State, W), nth0(4, State, C), nth0(5, State, E),
    nth0(6, State, Sw), nth0(7, State, S), nth0(8, State, Se),
    countAll([Nw, N, Ne, W, C, E, Sw, S, Se], x, Xes),
    countAll([Nw, N, Ne, W, C, E, Sw, S, Se], 0, Zes),
    nextTurn(Xes, Zes, P),
    getNextAvailableBoards(state(State, P, L, _), NextAvl).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(state(_, _, _, [X]), X).
    % :- getBoard(S, Avl, Board), getBoardResult(Board, '').
validMove(state(S, _, _, Avl), (UPos, Pos)) :-
    member(UPos, Avl), getBoard(S, UPos, Board),
    getBoardResult(Board, ''), getPos(Board, Pos, '').
validMove(_, _) :- false.

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(_, _, _) :- false.

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(_, _) :- false.

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last(_, _) :- false.
