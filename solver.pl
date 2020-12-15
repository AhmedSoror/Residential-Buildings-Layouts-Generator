:-use_module(library(clpfd)).

% -----------------------------------------------------------
% Conventions:
%   -Cartesian coordinates
%   - X & width ==> horizontal , Y & legth ==> vertical       
%   -(x, y) represents top left corner of the floor || room || ... etc
% -----------------------------------------------------------

%%% floorwidth, floorlength, [n,e,w,s] (open-area..etc), app count
%%% 1 apartment type: [#,[#,[(type,minarea,w,l,assigned)]]]
%%% soft cons leave for now
%%% global const ,, ,,  ,,
getAppartmentsNTimes(0,_,[]).
getAppartmentsNTimes(N,Types,R):-
    N#>0,
    getRooms(Types,R1),
    N1 #= N-1,
    getAppartmentsNTimes(N1,Types,R2),
    append([R1],R2,R).

getRooms([],[]).
getRooms([H|T],R):-
    R1=[H,[_,_,_,_]],
    getRooms(T,R2),
    append([R1],R2,R).
    
getAppartment([],[]).
getAppartment([H|T],R):-
    H=[Num,[_,Types]],
    getAppartmentsNTimes(Num,Types,R1),
    getAppartment(T,R2),
    append(R1,R2,R).


% ---------------------------------- Disjoint / Adj ----------------------------------
% check if two rooms are adjacent but not overlapping
adjacent([_, O1], [_, O2]):-
    O1 = [X1, Y1, W1, H1|_],
    O2 = [X2, Y2, W2, H2|_],
    
    % there exist an x that belongs to rect 1 and rect 2
    X1#=< X,
    X#=< X1+W1,
    X2#=< X,
    X#=< X2+W2,
    % there exist a y that belongs to rect 1 and rect 2
    Y1#=< Y,
    Y#=< Y1+H1,
    Y2#=< Y,
    Y#=< Y2+H2,
    % the two rectangles are not overlapping 
    % disjoint([X1,Y1,W1,H1],[X2,Y2,W2,H2]).
    disjoint2([rect(X1,W1, Y1,H1), rect(X2,W2,Y2,H2)]).



% ---------------------------------- Apartment Constraints ----------------------------------
% -------------------
% checks that every room has at least on adjacent room in the apartment 
%  input: apartment => [rooms]
consistentRooms(A):-
    consistentRoomsHelper(A, A).

% helper takes rooms one by one and compares against the rest to find adjacent room (a room is not adjacent to itself)
consistentRoomsHelper([], _).
consistentRoomsHelper([H|T], L):-
    % check if every room has adj 
    % hasAdj(H, L),
    % consistentRoomsHelper(T, L).
    
    % check if there is a room adj to this one
    belongsTo(R, L), 
    adjacent(H, R),
    consistentRoomsHelper(T, L).
    
hasAdj(R, [H|_]):-
    adjacent(R, H).
hasAdj(R, [_|T]):-
    hasAdj(R, T).

belongsTo(R, [R|_]).
belongsTo(R, [_|T]):-
    belongsTo(R, T).
% -------------------

% ---------------------------------- Floor Constraints ----------------------------------
% each apartment contains rooms belonging to the apartment
consistentApartments([]).
consistentApartments([H|T]):-
    consistentRooms(H),
    consistentApartments(T).

% apartments don't overlap 

% --------------------------------------------------------------------------------------------

%% output: [[(type,x,y,w,l),..],apartment2 ...,stairs,elev,hallwayslist]
% (x, y) are cartesian coordinates where x is the the horizontal axis, y is the vertical one. (0,0) represents the top left corner of the floor
solve(F,A,R):-
    F=[Width,Length,[North,East,South,West]],
    A=[[2,[5,[R1,R2,R3,R4,R5]]]],
    NUM_AP=2,
    %length(R, NUM_AP),
    getAppartment(A,R).


    


