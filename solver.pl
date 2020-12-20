:- use_module(library(clpfd)).
:- use_module(library(clpr)).

% -----------------------------------------------------------
% Conventions:
%   -Cartesian coordinates
%   - X & width ==> horizontal , Y & legth ==> vertical       
%   -(x, y) represents top left corner of the floor || room || ... etc
% -----------------------------------------------------------

%%% floorwidth, floorlength, [n,w,s,e] (open-area..etc), app count     0(closed),1(open),2(landscape)
%%% 1 apartment type: [#,[#,[[type,minarea,w,h,assigned]]]]
%%% Corridors: number(1,2,3..)
%  Room = [ [type,minarea,w,h,assigned] , [X, W, Y, H]  ]
%%% soft cons leave for now
%%% global const ,, ,,  ,,
% -----------------------------------------------------------
% -----------------------------------------------------------

% getAppartments(Apartments types, List of all apartments) 
getAppartments([],[]).
getAppartments([H|T],R):-
    H=[Num,[_,Types]],
    getAppartmentsNTimes(Num,Types,R1),
    getAppartments(T,R2),
    append(R1,R2,R).
% -------------------

% getAppartmentsNTimes(N,Apartment, Result) 
getAppartmentsNTimes(0,_,[]).
getAppartmentsNTimes(N,ApartmentType,R):-
    N#>0,
    getRooms(ApartmentType,R1,HallwaysCount),
    N1 #= N-1,
    getAppartmentsNTimes(N1,ApartmentType,R2),
    append([[HallwaysCount|R1]],R2,R).
% -------------------

%getRooms(Rooms list, result list after modifications, Hallways count).
getRooms([],[],0).
getRooms([H|T],R,HallwaysCount):-
    H=[hallway,_,Width,Height,_],
    R1=[H,[_,Width,_,Height]],
    getRooms(T,R2,Count2),
    HallwaysCount#= 1+Count2,
    append([R1],R2,R).

getRooms([H|T],R,HallwaysCount):-
    H=[RoomType,_,Width,Height,_],
    R1=[H,[_,Width,_,Height]],
    RoomType\=hallway,
    getRooms(T,R2,Count2),
    HallwaysCount#= Count2,
    append([R1],R2,R).

% -------------------
% getRects([[HallwaysCount|Apartment1]|T], Rects List, VarsX, VarsY, Total UsedFloorArea)
getRects([],[], [], [], 0,[]).
getRects([[_|Apartment1]|T], R, VarsX, VarsY, UsedFloorArea,Vars):-
    getRectRooms(Apartment1, R1, VarsX1, VarsY1, Apartment1Area,Vars1),
    getRects(T, R2, VarsX2, VarsY2, UsedFloorArea2,Vars2),
    append(R1, R2, R),
    append(VarsX1, VarsX2, VarsX),
    append(VarsY1, VarsY2, VarsY),
    append(Vars1,Vars2,Vars),
    UsedFloorArea #= Apartment1Area+UsedFloorArea2.
% -------------------
% getRectsRooms for one apartment
getRectRooms([],[], [], [], 0,[]).
getRectRooms([Room1|T],R, VarsX, VarsY, TotalApartmentArea,Vars):-
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
    getRectRooms(T,R2, VarsX2, VarsY2, TotalApartmentArea2,Vars1),
    % append([X1,Y1,X2,Y2],Vars1,Vars),
    append([Y1,X1,Y2,X2],Vars1,Vars),
    append(CoordinatesX, VarsX2, VarsX),
    append(CoordinatesY, VarsY2, VarsY),    
    append([R1],R2,R),
    TotalApartmentArea #= Area+TotalApartmentArea2.
% -------------------

% -------------------- Getters predicates --------------------
getCorridors(0,[]).
getCorridors(N,R):-
    N>=1,
    Corridor=[[corridor,1,_,_,none],[_,_,_,_]],
    N1 is N-1,
    getCorridors(N1,R1),
    R=[Corridor|R1].

getAssigned(_,[],R,R).
getAssigned(Assigned,[H|_],Acc,R):-
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

hasAdj(R, [H|_]):-
    adjacent(R, H).
hasAdj(R, [_|T]):-
    hasAdj(R, T).

belongsTo(R, [R|_]).
belongsTo(R, [_|T]):-
    belongsTo(R, T).
%------------------------- Constraints --------------------------------------

% -------------------- Rooms Constraints --------------------
% check if two rooms are adjacent
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

% ---------- Soft constraints
% daylightConstraintHelper(Floor info, Room, Reward)
daylightConstraintHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]], Reward):-
    X2 #= X+W,
    Y2 #= Y+H,
    (X#=0 #/\ West#>0) #\/ (Y#=0 #/\ North#>0) #\/ (X2#=FloorWidth #/\ East#>0) #\/ (Y2#=FloorHeight #/\ South#>0),
    Reward #=5.

daylightConstraintHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]], Reward):-
    X2 #= X+W,
    Y2 #= Y+H,
    % negate previous condition by pushing negation
    (X#\=0 #\/ West#=0) #/\ (Y#\=0 #\/ North#=0) #/\ (X2#\=FloorWidth #\/ East#=0) #/\ (Y2#\=FloorHeight #\/ South#=0),
    Reward #= 0.


% -------- sunRoomConstraint(Floor,R)
sunRoomConstraintHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]]):-
    X2 #= X+W,
    Y2 #= Y+H,
    (X#=0 #/\ West#>0) #\/ (Y#=0 #/\ North#>0) #\/ (X2#=FloorWidth #/\ East#>0) #\/ (Y2#=FloorHeight #/\ South#>0).
% -----------------------

diningRoomConstraintHelper(DiningRoom,Kitchens):-
    belongsTo(Kitchen,Kitchens),
    adjacent(DiningRoom,Kitchen).
% -----------------------

bathkitchenRoomConstraintHelper(H,Ducts):-          %H is either kitchen or bathroom
    belongsTo(Duct,Ducts),
    adjacent(H,Duct).


% -----------------------
% apply all constraints on rooms
% roomConstraint(Floor,Room,Appartment)

roomConstraint(_,[],_).
% dining room ==> adj to kitchen
roomConstraint(Floor,[H|T],Appartment):-
    H=[[diningroom|_],_],
    getKitchens(Appartment,[],Kitchens),
    diningRoomConstraintHelper(H,Kitchens),
    roomConstraint(Floor,T,Appartment).

% unassigned bathroom ==> adj to duct
roomConstraint(Floor,[H|T],Appartment):-
    H=[[bathroom,_,_,_,Assigned],_],
    Assigned = none,
    getDucts(Appartment,[],Ducts),
    bathkitchenRoomConstraintHelper(H,Ducts),
    roomConstraint(Floor,T,Appartment).

% assigned bathroom ==> adj duct & adj to assigned room
roomConstraint(Floor,[H|T],Appartment):-
    H=[[bathroom,_,_,_,Assigned],_],
    Assigned \= none,
    getAssigned(Assigned,Appartment,[],[AssignedRoom]),
    adjacent(H,AssignedRoom),
    % check for ducts
    getDucts(Appartment,[],Ducts),
    bathkitchenRoomConstraintHelper(H,Ducts),
    roomConstraint(Floor,T,Appartment).

% room assigned to another one ==> adj to assigned room
roomConstraint(Floor,[H|T],Appartment):-
    H=[[_,_,_,_,Assigned],_],
    Assigned \= none,
    getAssigned(Assigned,Appartment,[],[AssignedRoom]),
    adjacent(H,AssignedRoom),
    roomConstraint(Floor,T,Appartment).

% kitchen ==> adj to duct
roomConstraint(Floor,[H|T],Appartment):-
    H=[[kitchen|_],_],
    getDucts(Appartment,[],Ducts),
    bathkitchenRoomConstraintHelper(H,Ducts),
    roomConstraint(Floor,T,Appartment).

% sunroom ==> lies on landscape or openview side
roomConstraint(Floor,[H|T],Appartment):-
    H=[[sunroom|_],_],
    sunRoomConstraintHelper(Floor,H),
    roomConstraint(Floor,T,Appartment).

% else
roomConstraint(Floor,[H|T],Appartment):-
    H\=[[diningroom|_],_],
    H\=[[bathroom,_,_,_,_],_],
    H\=[[dressingroom,_,_,_,_],_],
    H\=[[kitchen|_],_],
    H\=[[sunroom|_],_],
    (H=[[_,_,_,_,none],_];H=1),
    roomConstraint(Floor,T,Appartment).

daylightConstraint(_,[],0).
daylightConstraint(Floor,[Room|RoomsRest], Reward):-
    daylightConstraintHelper(Floor,Room, Reward1),
    daylightConstraint(Floor,RoomsRest, Reward2),
    Reward#= Reward1+Reward2.

softContraints(Floor, Apartment, TotalReward):-
    daylightConstraint(Floor, H, DayLightReward),
    TotalReward #= DayLightReward.

% -------------------
% checks that every room is adjacent to a hallway in the apartment 
consistentRooms([HallwaysCount|A]):-
    HallwaysCount#>1,
    delete(A,[[duct|_],_],AWithoutDucts),
    getHallways(AWithoutDucts,[],Hallways),
    consistentRoomsHelper(AWithoutDucts, Hallways).

consistentRooms([1|A]):-
    delete(A,[[duct|_],_],AWithoutDucts),
    delete(AWithoutDucts, [[hallway|_],_], AwithoutHallways),
    getHallways(A,[],Hallways),
    consistentRoomsHelper(AwithoutHallways, Hallways).


consistentRoomsHelper([], _).
consistentRoomsHelper([H|T], L):-
    belongsTo(Hallway,L),
    adjacent(H, Hallway),
    consistentRoomsHelper(T, L).
    

% -------------------
floorConstraint(Ap,Corridors):-
    getHallways(Ap,[],Hallways),
    belongsTo(Hallway,Hallways),
    belongsTo(Corridor,Corridors),
    adjacent(Hallway,Corridor).
% ---------------------------------- Floor Constraints ----------------------------------
% each apartment contains rooms belonging to the apartment
consistentApartments(_,[],_,0).
consistentApartments(Floor,[H|T],Corridors, SoftConstraintsReward):-
    consistentRooms(H),
    roomConstraint(Floor,H,H),
    %making sure that each ap is adjacent to a corridor
    floorConstraint(H,Corridors),
    % apply soft constraints to each apartment
    softContraints(Floor, Apartment, Reward1),

    consistentApartments(Floor,T,Corridors, Reward2),

    SoftConstraintsReward#= Reward1+Reward2.

% ------------------------------------- oPTIONAL ----------------------------------------
optionalConstraints(Floor,A,Corridors,Stairs,Min,Max,[LandScape,EqualDistanceToElev,Symmetry,GoldenRatio]):-
    
    
    landScapeView(LandScape,Floor,A),
    equalDistancesToElev(EqualDistanceToElev,A,Corridors,Stairs,Min,Max),
    goldenRatio(GoldenRatio,A).

landScapeView(_,_,[]).
landScapeView(0,_,_).
landScapeView(1,Floor,[A|T]):-
    write(LandScape),nl,
    % landScapeViewHelper(Floor,A),
    belongsTo(Room,A),
    landScapeViewHelper(Floor,Room),
    landScapeView(1,Floor,T).
landScapeViewHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]]):-
    X2 #= X+W,
    Y2 #= Y+H,
    (X#=0 #/\ West#>1) #\/ (Y#=0 #/\ North#>1) #\/ (X2#=FloorWidth #/\ East#>1) #\/ (Y2#=FloorHeight #/\ South#>1).

distance([_,[X1,W1,Y1,H1]],[_,[X2,W2,Y2,H2]],Distance):-
    X1new#=X1+(W1 div 2),
    Y1new#=Y1+(H1 div 2),
    X2new#=X2+(W2 div 2),
    Y2new#=Y2+(H2 div 2),
    Distance= (X1new-X2new)^2+(Y1new-Y2new)^2.

equalDistancesToElev(_,[],_,_,_,_).
equalDistancesToElev(0,_,_,_,_,_).
equalDistancesToElev(1,[A|T],Corridors,Stairs,Min,Max):-
    belongsTo(Hallway,A),
    belongsTo(Corridor,Corridors),
    adjacent(Hallway,Corridor),
    distance(Hallway,Stairs,D),
    D#=<Max,
    D#>=Min,
    equalDistancesToElev(1,T,Corridors,Stairs,Min,Max).

goldenRatioHelper([]).
goldenRatioHelper([[[duct|_],[_,W,_,H]]|T]):-
    goldenRatioHelper(T).

goldenRatioHelper([[[hallway|_],[_,W,_,H]]|T]):-
    goldenRatioHelper(T).

goldenRatioHelper([[[Type|_],[_,W,_,H]]|T]):-
    Type \= duct,
    Type \= hallway,
    H#>W,
    {(W+H)/H >= 1.6},
    {(W+H)/H =< 1.7},!,
    goldenRatioHelper(T).
goldenRatioHelper([[[Type|_],[_,W,_,H]]|T]):-
    Type \= duct,
    Type \= hallway,
    W#>H,
    {(W+H)/W >= 1.6},
    {(W+H)/W =< 1.7},!,
    goldenRatioHelper(T).
goldenRatio(0,_).
goldenRatio(_,[]).
goldenRatio(1,[[_|A]|T]):-
    goldenRatioHelper(A),
    goldenRatio(1,T).
% --------------------------------------------------------------------------------------------
%% output: [[(type,x,y,w,l),..],apartment2 ...,stairs,elev,hallwayslist]
% (x, y) are cartesian coordinates where x is the the horizontal axis, y is the vertical one. (0,0) represents the top left corner of the floor

solve(F,A,CorridorsCount,OptionalConstraints,R):-
    % write("F"),
    Min=0,
    Max=36,
    statistics(runtime, [Start|_]),
    F=[Width,Height,Sides],
    %stairs and elevator
    Stairs=[[stairselev,1,1,1,none],[StairsX,1,StairsY,1]],

    % A=[[2,[5,[R1,R2,R3]]]],
    %length(R, NUM_AP),
    getCorridors(CorridorsCount,CorridorsTmp),
    belongsTo(Corridor,CorridorsTmp),
    adjacent(Corridor,Stairs),
    Corridors=[CorridorsCount,Stairs|CorridorsTmp],
    getAppartments(A,R),
    % print(R),nl,nl,
    % print([Corridors]),
    getRects([Corridors],CorridorRects,CorridorsX,CorridorsY,_,_),
    getRects(R, Rects, VarsX, VarsY, TotalUsedArea,Vs),
    % constraints: 
    % domain
    CorridorsX ins 0..Width,
    CorridorsY ins 0..Height,
    VarsX ins 0.. Width,
    VarsY ins 0.. Height,
    
    % apply constraints on the floor apartments
    append(Rects,CorridorRects,FloorRects),
    % non overlapping
    disjoint2(FloorRects),
    consistentApartments(F,R,Corridors, SoftConstraintsReward),
    optionalConstraints(F,R,CorridorsTmp,Stairs,Min,Max,OptionalConstraints),
   
    append(VarsX, VarsY, Vars1),
    append(CorridorsX,CorridorsY,Vars2),
    append(Vars1,Vars2,Vars),
    labeling([ffc,up,bisect,max(TotalUsedArea, SoftConstraintsReward)], Vs),
    labeling([ffc],Vars2),
    statistics(runtime, [Stop|_]),
    Runtime is Stop - Start,
    print("Runtime "+Runtime),nl,
    print("Corridors: "+Corridors),nl.


% ff,up,bisect  2797
% ffc,up,bisect 3364
% ff,down,enum 3773
% ffc,down,step 3761
% ff, down, bisect 3549
% ffc,down,bisect 3987
    