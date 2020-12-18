:-use_module(library(clpfd)).

% -----------------------------------------------------------
% Conventions:
%   -Cartesian coordinates
%   - X & width ==> horizontal , Y & legth ==> vertical       
%   -(x, y) represents top left corner of the floor || room || ... etc
% -----------------------------------------------------------

%%% floorwidth, floorlength, [n,w,s,e] (open-area..etc), app count     0(closed),1(open),2(landscape)
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

%----------------------------------- sunRoomConstraint(Sides,R),-----------------------------
sunRoomConstraintHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]]):-
    X2 #= X+W,
    Y2 #= Y+H,
    (X#=0 #/\ West#>0) #\/ (Y#=0 #/\ North#>0) #\/ (X2#=FloorWidth #/\ East#>0) #\/ (Y2#=FloorHeight #/\ South#>0).

diningRoomConstraintHelper(DiningRoom,Kitchens):-
    belongsTo(Kitchen,Kitchens),
    adjacent(DiningRoom,Kitchen).

bathkitchenRoomConstraintHelper(H,Ducts):-          %H is either kitchen or bathroom
    belongsTo(Duct,Ducts),
    adjacent(H,Duct).
    

roomConstraint(_,[],_).


roomConstraint(Floor,[H|T],Appartment):-
    H=[[diningroom|_],_],
    getKitchens(Appartment,[],Kitchens),
    diningRoomConstraintHelper(H,Kitchens),
    roomConstraint(Floor,T,Appartment).

roomConstraint(Floor,[H|T],Appartment):-
    H=[[bathroom,_,_,_,Assigned],_],
    Assigned = none,
    getDucts(Appartment,[],Ducts),
    bathkitchenRoomConstraintHelper(H,Ducts),
    roomConstraint(Floor,T,Appartment).

roomConstraint(Floor,[H|T],Appartment):-
    H=[[bathroom,_,_,_,Assigned],_],
    Assigned \= none,
    getAssigned(Assigned,Appartment,[],[AssignedRoom]),
    adjacent(H,AssignedRoom),
    % check for ducts
    getDucts(Appartment,[],Ducts),
    bathkitchenRoomConstraintHelper(H,Ducts),
    roomConstraint(Floor,T,Appartment).

roomConstraint(Floor,[H|T],Appartment):-
    H=[[kitchen|_],_],
    getDucts(Appartment,[],Ducts),
    bathkitchenRoomConstraintHelper(H,Ducts),
    roomConstraint(Floor,T,Appartment).

roomConstraint(Floor,[H|T],Appartment):-
    H=[[sunroom|_],_],
    sunRoomConstraintHelper(Floor,H),
    roomConstraint(Floor,T,Appartment).

roomConstraint(Floor,[H|T],Appartment):-
    H=[[_,_,_,_,Assigned],_],
    Assigned \= none,
    getAssigned(Assigned,Appartment,[],[AssignedRoom]),
    adjacent(H,AssignedRoom),
    roomConstraint(Floor,T,Appartment).

roomConstraint(Floor,[H|T],Appartment):-
    H\=[[sunroom|_],_],
    roomConstraint(Floor,T,Appartment).


% ---------------------------------- Appartment Constraints ----------------------------------
% getHallway(Ap,Hallway):-
%     Hallway = [[hallway|_],_],
%     belongsTo(Hallway, Ap).

getAssigned(_,[],R,R).
getAssigned(Assigned,[H|T],Acc,R):-
    H=[[Assigned|_],_],
    append(Acc,[H],Acc1),
    getAssigned(Assigned,[],Acc1,R).
getAssigned(Assigned,[H|T],Acc,R):-
    H\=[[Assigned|_],_],
    getAssigned(Assigned,T,Acc,R).

getKitchens([],R,R).
getKitchens([H|T],Acc,R):-
    H=[[kitchen|_],_],
    append(Acc,[H],Acc1),
    getKitchens(T,Acc1,R).
getKitchens([H|T],Acc,R):-
    H\=[[kitchen|_],_],
    getKitchens(T,Acc,R).
    
getDucts([],R,R).
getDucts([H|T],Acc,R):-
    H=[[duct|_],_],
    append(Acc,[H],Acc1),
    getDucts(T,Acc1,R).
getDucts([H|T],Acc,R):-
    H\=[[duct|_],_],
    getDucts(T,Acc,R).


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
    delete(A,[[duct|_],_],AWithoutDucts),
    getHallways(AWithoutDucts,[],Hallways),
    consistentRoomsHelper(HallwaysCount,AWithoutDucts, Hallways).

consistentRooms([1|A]):-
    delete(A,[[duct|_],_],AWithoutDucts),
    delete(AWithoutDucts, [[hallway|_],_], AwithoutHallways),
    getHallways(A,[],Hallways),
    consistentRoomsHelper(1,AwithoutHallways, Hallways).

% helper takes rooms one by one and compares against the rest to find adjacent room (a room is not adjacent to itself)
consistentRoomsHelper(_,[], _).

consistentRoomsHelper(HallwaysCount,[H|T], L):-
    % check if there is a room adj to this one
    belongsTo(Hallway,L),
    adjacent(H, Hallway),
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
consistentApartments(_,[]).
consistentApartments(Floor,[H|T]):-
    consistentRooms(H),
    roomConstraint(Floor,H,H),
    consistentApartments(Floor,T).

% apartments don't overlap 

% --------------------------------------------------------------------------------------------

%% output: [[(type,x,y,w,l),..],apartment2 ...,stairs,elev,hallwayslist]
% (x, y) are cartesian coordinates where x is the the horizontal axis, y is the vertical one. (0,0) represents the top left corner of the floor
solve(F,A,R):-
    statistics(runtime, [Start|_]),
    F=[Width,Height,Sides],
    % A=[[2,[5,[R1,R2,R3]]]],
    %length(R, NUM_AP),

    getAppartments(A,R),
    getRects(R, Rects, VarsX, VarsY),
    % constraints: 
    %SunRoooms
    % sunRoomConstraint(F,R),

    % domain
    VarsX ins 0.. Width,
    VarsY ins 0.. Height,
    % consistent Apartments where rooms in the same apartment are adjacent
    consistentApartments(F,R),
    % non overlapping
    disjoint2(Rects),
    append(VarsX, VarsY, Vars),
    labeling([], Vars),
    statistics(runtime, [Stop|_]),
    Runtime is Stop - Start,
    print("Runtime"+Runtime).
    