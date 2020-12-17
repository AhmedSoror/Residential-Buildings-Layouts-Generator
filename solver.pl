:-use_module(library(clpfd)).

% -----------------------------------------------------------
% Conventions:
%   -Cartesian coordinates
%   - X & width ==> horizontal , Y & legth ==> vertical       
%   -(x, y) represents top left corner of the floor || room || ... etc
% -----------------------------------------------------------

%%% floorwidth, floorlength, [n,e,w,s] (open-area..etc), app count
%%% 1 apartment type: [#,[#,[[type,minarea,w,h,assigned]]]]
%  Room = [ [type,minarea,w,h,assigned] , [X, W, Y, H]  ]
%%% soft cons leave for now
%%% global const ,, ,,  ,,
getAppartmentsNTimes(0,_,[]).
getAppartmentsNTimes(N,Types,R):-
    N#>0,
    getRooms(Types,R1,HallwaysCount),
    N1 #= N-1,
    getAppartmentsNTimes(N1,Types,R2),
    append([[HallwaysCount|R1]],R2,R).
  

getRooms([],[],0).
getRooms([H|T],R,HallwaysCount):-
    R1=[H,[_,_,_,_]],
    H=[hallway|_],
    getRooms(T,R2,Count2),
    HallwaysCount#= 1+Count2,
    append([R1],R2,R).

getRooms([H|T],R,HallwaysCount):-
    R1=[H,[_,_,_,_]],
    H=[RoomType|_],
    RoomType\=hallway,
    getRooms(T,R2,Count2),
    HallwaysCount#= Count2,
    append([R1],R2,R).
    
getAppartments([],[]).
getAppartments([H|T],R):-
    H=[Num,[_,Types]],
    getAppartmentsNTimes(Num,Types,R1),
    getAppartments(T,R2),
    append(R1,R2,R).

% -------------------

getRects([],[], [], []).
getRects([[_|Apartment1]|T], R, VarsX, VarsY):-
    getRectRooms(Apartment1, R1, VarsX1, VarsY1),
    getRects(T, R2, VarsX2, VarsY2),
    append(R1, R2, R),
    append(VarsX1, VarsX2, VarsX),
    append(VarsY1, VarsY2, VarsY).


getRectRooms([],[], [], []).
getRectRooms([Room1|T],R, VarsX, VarsY):-
    Room1=[[_,MinArea,W1,H1,_] ,[X1, W1, Y1, H1|_]],
    R1 = rect(X1, W1, Y1, H1),
    X2 #= X1+W1,
    X2#>X1,
    Y2 #= Y1+H1,
    Y2#>Y1,
    Area #= W1 * H1,
    Area #>= MinArea,
    CoordinatesX=[X1, X2],
    CoordinatesY=[Y1, Y2],
    getRectRooms(T,R2, VarsX2, VarsY2),

    append(CoordinatesX, VarsX2, VarsX),
    append(CoordinatesY, VarsY2, VarsY),    
    append([R1],R2,R).
% ---------------------------------- Disjoint / Adj ----------------------------------
% check if two rooms are adjacent but not overlapping
adjacent([_,[X1,W1,Y1,H1]],[_,[X2,W2,Y2,H2]]):-
    X2#=X1+W1 #==> (Y1#< Y2+H2 #/\ Y2#< Y1+H1), 
    X2#=X1-W2 #==> (Y1#< Y2+H2 #/\ Y2#< Y1+H1),
    Y2#=Y1+H1 #==> (X1#< X2+W2 #/\ X2#< X1+W1),
    Y2#=Y1-H2 #==> (X1#< X2+W2 #/\ X2#< X1+W1),
    X1#=X2 #==> Y1#\=Y2,
    Y1#=Y2 #==> X1#\=X2,
    % there exist an x that belongs to rect 1 and rect 2
    X1#=< X,
    X#=< X1+W1,
    X2#=< X,
    X#=< X2+W2,
    % there exist a y that belongs to rect 1 and rect 2
    Y1#=< Y,
    Y#=< Y1+H1,
    Y2#=< Y,
    Y#=< Y2+H2.



% ---------------------------------- Apartment Constraints ----------------------------------
% getHallway(Ap,Hallway):-
%     Hallway = [[hallway|_],_],
%     belongsTo(Hallway, Ap).

getHallways([],R,R).
getHallways([H|T],Acc,R):-
    H=[[hallway|_],_],
    append(Acc,[H],Acc1),
    getHallways(T,Acc1,R).
getHallways([H|T],Acc,R):-
    H\=[[hallway|_],_],
    getHallways(T,Acc,R).
    
% -------------------
% checks that every room has at least on adjacent room in the apartment 
%  input: apartment => [rooms]
consistentRooms([HallwaysCount|A]):-
    HallwaysCount#>1,
    getHallways(A,[],Hallways),
    consistentRoomsHelper(HallwaysCount,A, Hallways).

consistentRooms([1|A]):-
    delete(A, [[hallway|_],_], AwithoutHallways),
    getHallways(A,[],Hallways),
    consistentRoomsHelper(1,AwithoutHallways, Hallways).

% helper takes rooms one by one and compares against the rest to find adjacent room (a room is not adjacent to itself)
consistentRoomsHelper(_,[], _).

consistentRoomsHelper(HallwaysCount,[H|T], L):-
    % check if every room has adj 
    % check if there is a room adj to this one
    belongsTo(Hallway,L),
    print(Hallway),nl,
    print(H),nl,
    adjacent(H, Hallway),
    % print("COMAPRING"+ H +"  "+ Hallway +" ===>"+F),nl,
    consistentRoomsHelper(HallwaysCount,T, L).
    
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
    statistics(runtime, [Start|_]),
    F=[Width,Height,[North,East,South,West]],
    % A=[[2,[5,[R1,R2,R3]]]],
    %length(R, NUM_AP),

    getAppartments(A,R),
    % print(R),
    getRects(R, Rects, VarsX, VarsY),
    % constraints: 
    % domain
    VarsX ins 0.. Width,
    VarsY ins 0.. Height,
    % consistent Apartments where rooms in the same apartment are adjacent
    consistentApartments(R),
    % non overlapping
    disjoint2(Rects),
    append(VarsX, VarsY, Vars),
    labeling([], Vars),
    statistics(runtime, [Stop|_]),
    Runtime is Stop - Start,
    print("Runtime"+Runtime).
    