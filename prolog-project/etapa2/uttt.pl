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

filter(_,[],[]) :- !.
filter([P1 | P2], [First | Rest], [First | Tail]) :-
   predicate(P1), filter(P2, Rest, Tail), !.
filter([_ | P2], [_ | Rest], Result) :-
   filter(P2, Rest, Result).

getNextAvailableBoards(S, Boards) :- initialState(S), positions(Boards).
getNextAvailableBoards(state(S, _, L, _), Boards) :-
    getUBoard(state(S, _, _, _), UBoard),
    (getPos(UBoard, L, x); getPos(UBoard, L, 0); getPos(UBoard, L, r)),
    positions(Y), filter(UBoard, Y, Boards), !.
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
getBoardResult(Board, P) :- player_wins(P, Board),!.
getBoardResult(Board, P) :- countOccurances(Board, 0, A), countOccurances(Board, x, B), 9 is A + B, P = r, !.
getBoardResult(_, '').
    

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
countOccurances([], _, 0).
countOccurances([H | T], H, N) :- countOccurances(T, H, N1), N is N1 + 1, !.
countOccurances([_ | T], H1, N) :- countOccurances(T, H1, N).

countAll([], _, 0).
countAll([H | T], X, N) :- countOccurances(H, X, N1), countAll(T, X, N2), N is N2 + N1.

nextTurn(Xes, Zes, P) :-
    Xes > Zes, P = 0, !;
    P = x.

buildState([Nw, N, Ne, W, C, E, Sw, S, Se], L, state(State, P, L, NextAvl)) :-
    length(State, 9),
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
gameNotFinished(S) :- getUBoard(S, UB), getBoardResult(UB, R), R \= x, R \= 0, R \= r.

validMove(state(S, P, L, Avl), Pos) :-
    gameNotFinished(state(S, P, L, Avl)),
    length(Avl, 1),
    nth0(0, Avl, Board),
    getPos(state(S, P, L, Avl), Board, Pos, '').

validMove(state(S, P, L, Avl), (UPos, Pos)) :-
    gameNotFinished(state(S, P, L, Avl)),
    member(UPos, Avl),
    getBoard(state(S, P, L, Avl), UPos, Board), getBoardResult(Board, ''),
    getPos(Board, Pos, '').

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.

replaceElementAtIndex(Index, List, New, Result) :-
    nth0(Index, List, _, X),
    nth0(Index, Result, New, X).

makeMove(state(S, P, L, Avl), Move, NewState) :-
    validMove(state(S, P, L, Avl), Move),
    positions(PS), nth0(Index, PS, Move), nth0(UIndex, PS, L),  % indecsi Board si Uboard mutare
    nth0(UIndex, S, Board),                                     % Board-ul pe care se muta
    replaceElementAtIndex(Index, Board, P, NTemp),              % P face mutarea pe Board-ul respectiv, salvat in NTemp acum
    replaceElementAtIndex(UIndex, S, NTemp, NL),                % Se inlocuieste in vechiul State doar noul joc modificat, NTemp
    buildState(NL, Move, NewState).

makeMove(state(S, P, L, Avl), (UPos, Pos), NewState) :-
    validMove(state(S, P, L, Avl), (UPos, Pos)),
    positions(PS), nth0(Index, PS, Pos), nth0(UIndex, PS, UPos),
    nth0(UIndex, S, Board),
    replaceElementAtIndex(Index, Board, P, NTemp),
    replaceElementAtIndex(UIndex, S, NTemp, NL),
    buildState(NL, Pos, NewState).


% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(state(S, P, L, Avl), Pos) :-
    length(Avl, 1),
    nth0(0, Avl, UPos),
    getBoard(state(S, P, L, Avl), UPos, Board),       % available board
    nth0(Index, Board, ''), !, positions(PS), nth0(Index, PS, Pos).

dummy_first(state(S, P, L, Avl), (UPos, Pos)) :-
    nth0(0, Avl, UPos),
    getBoard(state(S, P, L, Avl), UPos, Board),       % first available board
    nth0(Index, Board, ''), !, positions(PS), nth0(Index, PS, Pos).

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last(state(S, P, L, Avl), Pos) :-
    length(Avl, 1),
    nth0(0, Avl, UPos),
    getBoard(state(S, P, L, Avl), UPos, Board),       % available board
    reverse(Board, RevBoard),                         % reverse available board
    nth0(RevIndex, RevBoard, ''), !, positions(PS), Index is 8 - RevIndex, nth0(Index, PS, Pos).

dummy_last(state(S, P, L, Avl), (UPos, Pos)) :-
    length(Avl, AvlLen),
    AvlLen2 is AvlLen - 1,
    nth0(AvlLen2, Avl, UPos),
    getBoard(state(S, P, L, Avl), UPos, Board),       % available board
    reverse(Board, RevBoard),                         % reverse available board
    nth0(RevIndex, RevBoard, ''), !, positions(PS), Index is 8 - RevIndex, nth0(Index, PS, Pos).


% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.
getOpponent(x, 0) :- !.
getOpponent(0, x).

movePriority(Player, Board, Move, 0) :-
    positions(PS), nth0(Index, PS, Move),
    replaceElementAtIndex(Index, Board, Player, NTemp),
    player_wins(Player, NTemp), !.

movePriority(Player, Board, Move, 1) :-
    positions(PS), nth0(Index, PS, Move),
    getOpponent(Player, Opponent),
    replaceElementAtIndex(Index, Board, Opponent, NTemp),
    player_wins(Opponent, NTemp), !.

movePriority(_, Board, Move, 2) :-
    empty_board(Board),
    (Move = ne, !; Move = nw, !; Move = se, !; Move = sw, !), !.

movePriority(Player, Board, Move, 3) :-
    countOccurances(Board, Player, Occs), Occs = 0,
    getOpponent(Player, Opponent),
    nth0(4, Board, Opponent),               % opponent is in center
    (Move = ne, !; Move = nw, !; Move = se, !; Move = sw, !), !.

movePriority(Player, Board, Move, 3) :-
    countOccurances(Board, Player, Occs), Occs = 0,
    getOpponent(Player, Opponent),
    \+ nth0(4, Board, Opponent),
    Move = c, !.

movePriority(Player, Board, Move, 4) :-
    positions(PS), nth0(Index, PS, Move),
    replaceElementAtIndex(Index, Board, Player, TempBoard),

    filter(TempBoard, PS, AvlMoves),
    movePriority(Player, TempBoard, NMove, 0), 
    member(NMove, AvlMoves), !.

movePriority(_, _, Move, 5) :-
    (Move = ne, !;
    Move = nw, !;
    Move = se, !;
    Move = sw, !), !.

movePriority(_, _, _, 6).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.

myMapBIM(_, _, [], []) :- !.
myMapBIM(Player, Board, [HA | TA], [(Priority, HA) | Pairs]) :-
    movePriority(Player, Board, HA, Priority),
    myMapBIM(Player, Board, TA, Pairs).
    

bestIndividualMoves(Player, Board, Moves) :-
    positions(PS), filter(Board, PS, AvlMoves),
    myMapBIM(Player, Board, AvlMoves, Pairs),
    sortMoves(Pairs, Moves).

% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.

bestValid(state(S, P, L, Avl), [Pos | _], Pos) :- validMove(state(S, P, L, Avl), (L, Pos)), !.
bestValid(state(S, P, L, Avl), [_ | TBMoves], Pos) :- bestValid(state(S, P, L, Avl), TBMoves, Pos).

narrowGreedy(state(S, P, L, Avl), Pos) :-
    length(Avl, 1),
    nth0(0, Avl, UPos),
    getBoard(state(S, P, L, Avl), UPos, Board),
    bestIndividualMoves(P, Board, BMoves),
    bestValid(state(S, P, L, Avl), BMoves, Pos), !.

narrowGreedy(state(S, P, L, Avl), (UPos, Pos)) :-
    getUBoard(state(S, P, L, Avl), UBoard),
    bestIndividualMoves(P, UBoard, UBMoves),
    nth0(0, UBMoves, UPos),
    getBoard(state(S, P, L, Avl), UPos, Board),
    bestIndividualMoves(P, Board, [Pos | _]).

% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.

globalPriority(state(S, P, L, Avl), (UPos, Pos), 0) :-
    % validMove(state(S, P, L, Avl), (UPos, Pos)),
    makeMove(state(S, P, L, Avl), (UPos, Pos), NewState),
    getUBoard(NewState, UB),
    player_wins(P, UB), !.

globalPriority(state(S, P, L, Avl), (UPos, Pos), 1) :-
    % validMove(state(S, P, L, Avl), (UPos, Pos)),
    getBoard(state(S, P, L, Avl), Pos, Board),
    getOpponent(P, Opponent),
    getBoardResult(Board, X), X \= 0, X \= x, X \= r,
    countOccurances(Board, Opponent, Occs), Occs == 0, 

    \+ (makeMove(state(S, P, L, Avl), (UPos, Pos), state(S2, _, L2, Avl2)),
    makeMove(state(S2, P, L2, Avl2), (Pos, _), NewState2),
    getBoard(NewState2, Pos, B),
    player_wins(P, B)), !.

globalPriority(state(S, P, L, Avl), (_, Pos), 2) :-
    % validMove(state(S, P, L, Avl), (UPos, Pos)),
    getBoard(state(S, P, L, Avl), Pos, Board),
    getOpponent(P, Opponent),
    getBoardResult(Board, X), X \= 0, X \= x, X \= r,
    countOccurances(Board, Opponent, Occs), Occs == 1, !.

globalPriority(state(S, P, L, Avl), (_, Pos), 3) :-
    % validMove(state(S, P, L, Avl), (UPos, Pos)),
    getBoard(state(S, P, L, Avl), Pos, Board),
    getOpponent(P, Opponent),
    getBoardResult(Board, X), X \= 0, X \= x, X \= r,
    countOccurances(Board, Opponent, Occs), Occs > 1,

    makeMove(state(S, P, L, Avl), (Pos, _), NewState),
    getUBoard(NewState, UB), getOpponent(P, Opponent),
    (\+ player_wins(Opponent, UB)),
    
    makeMove(NewState, (Pos, _), NewState2),
    getUBoard(NewState2, UB2),
    (\+ player_wins(P, UB2)), !.

globalPriority(state(S, P, L, Avl), (_, Pos), 5) :-
    makeMove(state(S, P, L, Avl), (Pos, X), NewState),
    getBoard(NewState, X, B),
    player_wins(P, B), !.
    
globalPriority(state(S, P, L, Avl), (_, Pos), 6) :-
    makeMove(state(S, P, L, Avl), (Pos, X), NewState),
    getBoard(NewState, X, B), getOpponent(P, Opponent),
    player_wins(Opponent, B),

    makeMove(NewState, (X, Y), NewState2),
    getBoard(NewState2, Y, B2),
    (player_wins(P, B2), !; getBoardResult(B2, Res), Res \= ''), !.

globalPriority(state(S, P, L, Avl), (_, Pos), 7) :-
    makeMove(state(S, P, L, Avl), (Pos, X), NewState),
    getBoard(NewState, X, B), getOpponent(P, Opponent),
    player_wins(Opponent, B),

    makeMove(NewState, (X, Y), NewState2),
    getBoard(NewState2, Y, B2),
    \+ player_wins(P, B2), !.

globalPriority(state(S, P, L, Avl), (_, Pos), 8) :-
    getBoard(state(S, P, L, Avl), Pos, B),
    getBoardResult(B, Res), Res \= '', !.

globalPriority(state(S, P, L, Avl), (_, Pos), 9) :-
    makeMove(state(S, P, L, Avl), (Pos, _), NewState),
    getUBoard(NewState, UB), getOpponent(P, Opponent),
    player_wins(Opponent, UB), !.

globalPriority(_, _, 4).


myMapBM1(_, _, [], []) :- !.
myMapBM1(state(S, P, L, Avl), UPos, [HA | TA], [(Priority, (UPos, HA)) | Pairs]) :-
    globalPriority(state(S, P, L, Avl), (UPos, HA), Priority),
    myMapBM1(state(S, P, L, Avl), UPos, TA, Pairs).

onlySecondOfPairs([], []) :- !.
onlySecondOfPairs([(_, Snd) | T], [Snd | Res]) :- onlySecondOfPairs(T, Res).

bestMoves(state(S, P, L, Avl), Moves) :-
    length(Avl, 1), nth0(0, Avl, UPos),
    getBoard(state(S, P, L, Avl), UPos, UBoard),
    bestIndividualMoves(P, UBoard, AvlMoves),
    myMapBM1(state(S, P, L, Avl), UPos, AvlMoves, Pairs),
    sortMoves(Pairs, MovesPairs),
    onlySecondOfPairs(MovesPairs, Moves), !.


bestMoves(state(S, P, L, Avl), Moves) :-
    getUBoard(state(S, P, L, Avl), UB),
    bestIndividualMoves(P, UB, AvlMoves),
    bM2Helper(state(S, P, L, Avl), AvlMoves, Pairs),
    sortMoves(Pairs, Moves).

bM2Helper(_, [], []) :- !.
bM2Helper(state(S, P, L, Avl), [HAvl | TAvl], Y) :-
    getBoard(state(S, P, L, Avl), HAvl, Board),
    bestIndividualMoves(P, Board, AvlMoves),
    myMapBM1(state(S, P, L, Avl), HAvl, AvlMoves, Pairs),
    append(Pairs, Z, Y),
    bM2Helper(state(S, P, L, Avl), TAvl, Z).

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(State, Move) :-
    bestMoves(State, Moves), nth0(0, Moves, Move), !.
