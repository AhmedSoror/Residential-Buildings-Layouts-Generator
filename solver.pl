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

% getAppartments(Apartments types, soft constraints per type , List of all apartments , List of all soft constraints options) 
getAppartments([],_,[],[]).
getAppartments([H|T],[SoftConstraintOptionPerType| ConstraintsTail],R, SoftConstraintOptions):-
    H=[Num,[_,Types]],
    getAppartmentsNTimes(Num,Types,R1),
    getConstraintsNTimes(Num,SoftConstraintOptionPerType,SoftConstraintOptions1),

    getAppartments(T,ConstraintsTail, R2, SoftConstraintOptions2),
    
    append(R1,R2,R),
    append(SoftConstraintOptions1,SoftConstraintOptions2,SoftConstraintOptions).

% -------------------

% getAppartmentsNTimes(N,Apartment, Result) 
getAppartmentsNTimes(0,_,[]).
getAppartmentsNTimes(N,ApartmentType,R):-
    N#>0,
    getRooms(ApartmentType,R1,HallwaysCount),
    N1 #= N-1,
    getAppartmentsNTimes(N1,ApartmentType,R2),!,
    append([[HallwaysCount|R1]],R2,R).


% getConstraintsNTimes(N,Soft Constraint per apartment type, Result) 
getConstraintsNTimes(0,_,[]).
getConstraintsNTimes(N,Constraint,R):-
    N#>0,
    N1 #= N-1,
    getConstraintsNTimes(N1,Constraint,R2),!,
    append([Constraint],R2,R).
% -------------------
% getRooms return list of rooms in the apartment after adding the output part(x, w, y, h) that represents the top left corner position of the room 
% and adds hallways count as well
%getRooms(Rooms list, result list after modifications, Hallways count).
getRooms([],[],0).
getRooms([H|T],R,HallwaysCount):-
    H=[hallway,_,Width,Height,_],
    R1=[H,[_,Width,_,Height]],
    getRooms(T,R2,Count2),
    HallwaysCount#= 1+Count2,
    append([R1],R2,R),!.

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
    append([Y1,X1,Y2,X2],Vars1,Vars),
    append(CoordinatesX, VarsX2, VarsX),
    append(CoordinatesY, VarsY2, VarsY),    
    append([R1],R2,R),
    TotalApartmentArea #= Area+TotalApartmentArea2.
% -------------------

% -------------------- Getters predicates --------------------
generateCorridors(N,[]):-
    N#=0.       % so that it works if the user did not specify the number
generateCorridors(N,R):-
    N#>=1,
    Corridor=[[corridor,1,_,_,none],[_,_,_,_]],
    N1 #= N-1,
    generateCorridors(N1,R1),
    R=[Corridor|R1].

generateDucts(N,[]):-
    N#=0.   % so that it works if the user did not specify the number
generateDucts(N,R):-
    N#>=1,
    Duct=[[duct,1,_,_,none],[_,_,_,_]],
    N1 #= N-1,
    generateDucts(N1,R1),
    R=[Duct|R1].

getAssigned(_,[],R,R).
getAssigned(Assigned,[H|_],Acc,R):-
    H=[[Assigned|_],_],
    append(Acc,[H],Acc1),
    % once the assigned room is found call the base case
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


getRoom(Room, [Apartment|T], Result):-
    getRoomFromApartment(Room, Apartment, Result), !.
getRoom(Room, [_|T], Result):-
    getRoom(Room, T, Result), !.

getRoomFromApartment(Room, [H|T], H):-
    H=[[Room|_],_].
getRoomFromApartment(Room, [H|T], R):-
    H\=[[Room|_],_],
    getRoomFromApartment(Room, T, R).

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
    % X coordinate of one room = x+width of the other => Y value should be in the range of y+height
    X2#=X1+W1 #==> (Y1#< Y2+H2 #/\ Y2#< Y1+H1), 
    X2#=X1-W2 #==> (Y1#< Y2+H2 #/\ Y2#< Y1+H1),
    % Y coordinate of one room = y+height of the other => X value should be in the range of x+width
    Y2#=Y1+H1 #==> (X1#< X2+W2 #/\ X2#< X1+W1),
    Y2#=Y1-H2 #==> (X1#< X2+W2 #/\ X2#< X1+W1),
    %  not the same room
    X1#=X2 #==> Y1#\=Y2,
    Y1#=Y2 #==> X1#\=X2,
    % there exist an x that belongs to room 1 and room 2
    X1#=< X,
    X#=< X1+W1,
    X2#=< X,
    X#=< X2+W2,
    % there exist a y that belongs to room 1 and room 2
    Y1#=< Y,
    Y#=< Y1+H1,
    Y2#=< Y,
    Y#=< Y2+H2.

% ---------- Soft constraints
% daylightConstraintHelper(Floor info, Room, Cost)
% if the room is either duct or hallway add no cost
daylightConstraintHelper(Floor,[[duct|_],_], 0).
daylightConstraintHelper(Floor,[[hallway|_],_], 0).
daylightConstraintHelper(Floor,Room, Cost):-
    Room \= [[hallway|_],_],
    Room \= [[duct|_],_],
    sunRoomConstraintHelperCost(Floor,Room,Cost).

isBedroom([[Type|_],_]):-
    sub_string(Type,0,7,_,bedroom).
notBedroom(X):-
    \+isBedroom(X).
getBedrooms(A,ABedrooms):-
    exclude(notBedroom, A, ABedrooms).

% get distance between room X - which is a bedroom- and every room in the list and consider it to be the cost
bedroomsDistanceHelper(_,[],0).
bedroomsDistanceHelper(X,[Room|Rest],Cost):-
    distance(X,Room,Distance),
    bedroomsDistanceHelper(X,Rest,Cost1),
    Cost#=Distance+Cost1.
% get the cost of distance between bedrooms with each other
% bedroomsAsCloseAsPossible(BedroomsList,Cost)
bedroomsAsCloseAsPossible([],0).
bedroomsAsCloseAsPossible([Room|Rest],Cost):-
    bedroomsDistanceHelper(Room,Rest,Cost1),
    bedroomsAsCloseAsPossible(Rest,Cost2),
    Cost #= Cost1+Cost2.

bedroomsAsClose(_,0, 0).
bedroomsAsClose(A,NearBedroomsOption, Cost):-
    NearBedroomsOption #=1,
    getBedrooms(A,ABedrooms),
    bedroomsAsCloseAsPossible(ABedrooms,Cost).  

% -------- sunRoomConstraint(Floor,R)
% make sure that the room with x, y lies on either open area or landscape view
sunRoomConstraintHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]]):-
    X2 #= X+W,
    Y2 #= Y+H,
    (X#=0 #/\ West#>0) #\/ (Y#=0 #/\ North#>0) #\/ (X2#=FloorWidth #/\ East#>0) #\/ (Y2#=FloorHeight #/\ South#>0).

% apply cost if it is not on open area nor landscape view
sunRoomConstraintHelperCost([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]],Cost):-
    X2 #= X+W,
    Y2 #= Y+H,
    ((X#=0 #/\ West#>0) #\/ (Y#=0 #/\ North#>0) #\/ (X2#=FloorWidth #/\ East#>0) #\/ (Y2#=FloorHeight #/\ South#>0))#<==>Cost.
% -----------------------

%there exist a kitchen adjacent to the dining room 
diningRoomConstraintHelper(DiningRoom,Kitchens):-
    belongsTo(Kitchen,Kitchens),
    adjacent(DiningRoom,Kitchen).
% -----------------------
% there exist a duct in the Ducts list that is adjacent to the kitchen or bathroom
bathkitchenRoomConstraintHelper(H,Ducts):-          %H is either kitchen or bathroom
    belongsTo(Duct,Ducts),
    adjacent(H,Duct).

% -----------------------
% apply all constraints on rooms
% roomConstraint(Floor,Room,Appartment)

roomConstraint(_,[],_, _).
% dining room ==> adj to kitchen
roomConstraint(Floor,[H|T],Appartment,DuctsList):-
    H=[[diningroom|_],_],
    getKitchens(Appartment,[],Kitchens),
    diningRoomConstraintHelper(H,Kitchens),
    roomConstraint(Floor,T,Appartment,DuctsList).

% unassigned bathroom ==> adj to duct
roomConstraint(Floor,[H|T],Appartment, DuctsList):-
    H=[[bathroom,_,_,_,Assigned],_],
    Assigned = none,
    bathkitchenRoomConstraintHelper(H,DuctsList),
    roomConstraint(Floor,T,Appartment,DuctsList).

% assigned bathroom ==> adj duct & adj to assigned room
roomConstraint(Floor,[H|T],Appartment, DuctsList):-
    H=[[bathroom,_,_,_,Assigned],_],
    Assigned \= none,
    getAssigned(Assigned,Appartment,[],[AssignedRoom]),
    adjacent(H,AssignedRoom),
    bathkitchenRoomConstraintHelper(H,DuctsList),
    roomConstraint(Floor,T,Appartment,DuctsList).

% room assigned to another one ==> adj to assigned room
roomConstraint(Floor,[H|T],Appartment,DuctsList):-
    H=[[_,_,_,_,Assigned],_],
    Assigned \= none,
    getAssigned(Assigned,Appartment,[],[AssignedRoom]),
    adjacent(H,AssignedRoom),
    roomConstraint(Floor,T,Appartment,DuctsList).

% kitchen ==> adj to duct
roomConstraint(Floor,[H|T],Appartment, DuctsList):-
    H=[[kitchen|_],_],
    bathkitchenRoomConstraintHelper(H,DuctsList),
    roomConstraint(Floor,T,Appartment,DuctsList).

% sunroom ==> lies on landscape or openview side
roomConstraint(Floor,[H|T],Appartment,DuctsList):-
    H=[[sunroom|_],_],
    sunRoomConstraintHelper(Floor,H),
    roomConstraint(Floor,T,Appartment,DuctsList).

% else
roomConstraint(Floor,[H|T],Appartment,DuctsList):-
    H\=[[diningroom|_],_],
    H\=[[bathroom,_,_,_,_],_],
    H\=[[dressingroom,_,_,_,_],_],
    H\=[[kitchen|_],_],
    H\=[[sunroom|_],_],
    (H=[[_,_,_,_,none],_];H#>=0),
    roomConstraint(Floor,T,Appartment,DuctsList).

% all rooms are exposed to day light
daylightConstraint(_,_,0,0).
daylightConstraint(_,[],_,0).
daylightConstraint(Floor,[Room|RoomsRest], RoomsDayLightOption, Cost):-
    RoomsDayLightOption#=1,
    daylightConstraintHelper(Floor,Room, Cost1),
    daylightConstraint(Floor,RoomsRest,RoomsDayLightOption, Cost2),
    Cost#= Cost1+Cost2.

% distance between two rooms in apartment is either greater or less than some value
roomsDistanceConstraintApartment( _,0,0).
roomsDistanceConstraintApartment(_,[],0).
roomsDistanceConstraintApartment(Apartment, DistanceBetweenRoomsOption, Cost):-
    DistanceBetweenRoomsOption = [Room1, Room2, IsGreater, DistanceLimit],
    IsGreater#=0,
    getRoomFromApartment(Room1, Apartment, R1),
    getRoomFromApartment(Room2, Apartment, R2),
    distance(R1,R2,Distance),
    % if the constraint is not applied, AppliedCost should be 1 otherwise it's 0
    (Distance#>DistanceLimit)#<==> AppliedCost,
    Cost #=AppliedCost *100.
roomsDistanceConstraintApartment(Apartment, DistanceBetweenRoomsOption, Cost):-
    DistanceBetweenRoomsOption = [Room1, Room2, IsGreater, DistanceLimit],
    IsGreater#=1,
    getRoomFromApartment(Room1, Apartment, R1),
    getRoomFromApartment(Room2, Apartment, R2),
    distance(R1,R2,Distance),
    % if the constraint is not applied, AppliedCost should be 1 otherwise it's 0
    (Distance#<DistanceLimit)#<==> AppliedCost,
    Cost #=AppliedCost *100.

% bathroom is easily accessible especially from living room
getDistanceWithAllRooms(_,[],0).
getDistanceWithAllRooms(Bathroom,[Room| RoomsRest],BathroomCloseCost):-
    Room \= [[livingroom, _, _, _, none]|_],
    distance(Bathroom,Room,BathLivingDistance),
    BathroomCloseCost1 = BathLivingDistance,
    getDistanceWithAllRooms(Bathroom,RoomsRest,BathroomCloseCost2),
    BathroomCloseCost = BathroomCloseCost1+ BathroomCloseCost2.
getDistanceWithAllRooms(Bathroom,[LivingRoom| RoomsRest],BathroomCloseCost):-
    LivingRoom = [[livingroom, _, _, _, none]|_],
    distance(Bathroom,LivingRoom,BathLivingDistance),
    BathroomCloseCost1 = BathLivingDistance*50,
    getDistanceWithAllRooms(Bathroom,RoomsRest,BathroomCloseCost2),
    BathroomCloseCost = BathroomCloseCost1+ BathroomCloseCost2.


easyAccessBathroom(_, 0, 0).
easyAccessBathroom(Apartment, EasilyAccessibleBathroomOption, BathroomCloseCost):-
    EasilyAccessibleBathroomOption #=1,
    Bathroom=[[bathroom, _, _, _, none]|_],                 % define main bathroom
    getRoomFromApartment(bathroom, Apartment, Bathroom),    % get bathroom that is a main bathroom
    getDistanceWithAllRooms(Bathroom,Apartment,BathroomCloseCost),!.
    
easyAccessBathroom(Apartment, EasilyAccessibleBathroomOption, 0):-
    EasilyAccessibleBathroomOption #=1,
    Bathroom=[[bathroom, _, _, _, none]|_],                 % define main bathroom
    \+getRoomFromApartment(bathroom, Apartment, Bathroom).    % if there are no bathroom 


% apply all soft constraints on the apartment
softContraints(Floor, [_|Apartment], ApartmentSoftConstraintOptions, DayLightCost, RoomsDistanceCost, BedroomsCloseCost, BathroomCloseCost):-
    ApartmentSoftConstraintOptions = [RoomsDayLightOption, DistanceBetweenRoomsOption, NearBedroomsOption, EasilyAccessibleBathroomOption],
    daylightConstraint(Floor, Apartment, RoomsDayLightOption, DayLightCost),
    roomsDistanceConstraintApartment(Apartment, DistanceBetweenRoomsOption, RoomsDistanceCost),
    bedroomsAsClose(Apartment,NearBedroomsOption, BedroomsCloseCost),
    easyAccessBathroom(Apartment, EasilyAccessibleBathroomOption, BathroomCloseCost).
    
% -------------------
% if there are more than 1 hallway in the apartment, get all hallways and call the consistentRoomsHelper, which will ensure that every room is adjacent to a hallway
% including hallways themselves
consistentRooms([HallwaysCount|A]):-
    HallwaysCount#>1,
    getHallways(A,[],Hallways),
    consistentRoomsHelper(A, Hallways).
% if there is only 1 hallway in the apartment, remove it from the apartment list and call the consistentRoomsHelper, which will ensure that every room is 
% adjacent to a hallway.
consistentRooms([1|A]):-
    delete(A, [[hallway|_],_], AwithoutHallways),
    getHallways(A,[],Hallways),
    consistentRoomsHelper(AwithoutHallways, Hallways).

consistentRoomsHelper([], _).
consistentRoomsHelper([H|T], L):-
    belongsTo(Hallway,L),
    adjacent(H, Hallway),
    consistentRoomsHelper(T, L).
    

% ---------------------------------- Floor Constraints ----------------------------------
% the apartment is accessible by one of the corridors in the floor
floorConstraint(Ap,Corridors):-
    getHallways(Ap,[],Hallways),
    belongsTo(Hallway,Hallways),
    belongsTo(Corridor,Corridors),
    adjacent(Hallway,Corridor).

% -------------------
% each apartment contains rooms belonging to the apartment
consistentApartments(_,[],_,_,_,0,0,0,0).
consistentApartments(Floor,[H|T],[ApartmentSoftConstraintOptions|SoftContraintsTail],Corridors, DuctsList, DayLightCost, RoomsDistanceCost, BedroomsCloseCost, BathroomCloseCost):-
    consistentRooms(H),
    roomConstraint(Floor,H,H,DuctsList),
    %making sure that each ap is adjacent to a corridor
    floorConstraint(H,Corridors),
    % apply soft constraints to each apartment
    softContraints(Floor, H, ApartmentSoftConstraintOptions, DayLightCost1, RoomsDistanceCost1, BedroomsCloseCost1, BathroomCloseCost1),
    consistentApartments(Floor, T, SoftContraintsTail, Corridors, DuctsList, DayLightCost2, RoomsDistanceCost2, BedroomsCloseCost2, BathroomCloseCost2),
    DayLightCost #= DayLightCost1 + DayLightCost2,
    RoomsDistanceCost = RoomsDistanceCost1 + RoomsDistanceCost2,
    BedroomsCloseCost #= BedroomsCloseCost1 + BedroomsCloseCost2,
    BathroomCloseCost #= BathroomCloseCost1+ BathroomCloseCost2.

% ------------------------------------- OPTIONAL ----------------------------------------
optionalGlobalConstraints(Floor,A,Corridors,Stairs,Min,Max,[LandScape,EqualDistanceToElev,Symmetry,GoldenRatio]):-
    landScapeView(LandScape,Floor,A),
    equalDistancesToElev(EqualDistanceToElev,A,Corridors,Stairs,Min,Max),
    goldenRatio(GoldenRatio,A).

% there exist a room in the apartment that has a landscape view
landScapeView(_,_,[]).
landScapeView(0,_,_).
landScapeView(1,Floor,[A|T]):-
    belongsTo(Room,A),
    landScapeViewHelper(Floor,Room),
    landScapeView(1,Floor,T).
landScapeViewHelper([FloorWidth,FloorHeight,[North,West,South,East]],[_,[X,W,Y,H]]):-
    X2 #= X+W,
    Y2 #= Y+H,
    (X#=0 #/\ West#>1) #\/ (Y#=0 #/\ North#>1) #\/ (X2#=FloorWidth #/\ East#>1) #\/ (Y2#=FloorHeight #/\ South#>1).

% distance is calculated using the midpoint of the two rooms
distance([_,[X1,W1,Y1,H1]],[_,[X2,W2,Y2,H2]],Distance):-
    X1new#=X1+(W1 div 2),
    Y1new#=Y1+(H1 div 2),
    X2new#=X2+(W2 div 2),
    Y2new#=Y2+(H2 div 2),
    Distance#= (X1new-X2new)^2+(Y1new-Y2new)^2.

% distance of all apartments to the elevator should be in the same range. Range conspet is used to add some tolerance to the system instead of making the distance
% exactly the same through all apartments. If necessary, the min& max value descriping the range can have the same value to make distance exactly equal.
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

% apply golden ratio constraint
% skip if the room is either duct or a hallway
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
solve(F,A,CorridorsCount,DuctsCount,SoftConstraintOptionsList,OptionalGlobalConstraints,Result):-
    Min=0,
    Max=36,
    statistics(runtime, [Start|_]),
    F=[Width,Height,Sides],
    % create stairs, corridors and ducts
    %stairs and elevator
    Stairs=[[stairselev,1,1,1,none],[StairsX,1,StairsY,1]],
    generateCorridors(CorridorsCount,CorridorsList),
    
    % constraints: 
    % stairs-elevator unit is accessible
    belongsTo(Corridor,CorridorsList),
    adjacent(Corridor,Stairs),
    % getAppartments([H|T],[SoftConstraintOptionPerType| ConstraintsTail],R, SoftConstraintOptions)
    getAppartments(A,SoftConstraintOptionsList,R,SoftConstraintOptions ),
    length(R, ApartmentsCount),
    
    Corridors=[CorridorsCount,Stairs|CorridorsList],
    getRects([Corridors],CorridorRects,CorridorsX,CorridorsY,_,_),
    
    DuctsCount in 0 .. ApartmentsCount,
    generateDucts(DuctsCount,DuctsList),
    Ducts=[DuctsCount|DuctsList],
    getRects([Ducts],DuctRects,DuctsX,DuctsY,_,_),
    
    getRects(R, Rects, VarsX, VarsY, TotalUsedArea,Vs),
    
    % domain
    CorridorsX ins 0..Width,
    CorridorsY ins 0..Height,

    DuctsX ins 0..Width,
    DuctsY ins 0..Height,

    VarsX ins 0.. Width,
    VarsY ins 0.. Height,

    % apply constraints on the floor apartments
    append(CorridorRects,DuctRects,CorrDuctsRects),
    append(Rects,CorrDuctsRects,FloorRects),
    
    % non overlapping
    disjoint2(FloorRects),
    consistentApartments(F,R,SoftConstraintOptions, CorridorsList, DuctsList, DayLightCost, RoomsDistanceCost, BedroomsCloseCost, BathroomCloseCost),
    optionalGlobalConstraints(F,R,CorridorsList,Stairs,Min,Max,OptionalGlobalConstraints),
    
    append(VarsX, VarsY, Vars1),
    append(CorridorsX,CorridorsY,Vars2),
    append(DuctsX,DuctsY,VarsDucts),
    
    append(Vars2,VarsDucts,VarsCorrDucts),
    append(Vars1,VarsCorrDucts,Vars),
    
    labeling([ffc,up,bisect,max(TotalUsedArea),min(DuctsCount),min(DayLightCost),min(RoomsDistanceCost) ,min(BedroomsCloseCost), min(BathroomCloseCost)], Vs),
    labeling([ffc],VarsCorrDucts),
    statistics(runtime, [Stop|_]),
    append(Corridors, DuctsList, CorrDucts),
    append(R, [CorrDucts],Result),
    Runtime is Stop - Start,
    print("Runtime "+Runtime),nl,
    print("Corridors: "+Corridors),nl,
    print("Duct: "+DuctsList),nl.


