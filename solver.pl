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

% -------------------
% getRects([[HallwaysCount|Apartment1]|T], Rects List, VarsX, VarsY, Total UsedFloorArea)
getRects([],[], [], [], 0).
getRects([[_|Apartment1]|T], R, VarsX, VarsY, UsedFloorArea):-
    getRectRooms(Apartment1, R1, VarsX1, VarsY1, Apartment1Area),
    getRects(T, R2, VarsX2, VarsY2, UsedFloorArea2),
    append(R1, R2, R),
    append(VarsX1, VarsX2, VarsX),
    append(VarsY1, VarsY2, VarsY),
    UsedFloorArea #= Apartment1Area+UsedFloorArea2.
% -------------------
% getRectsRooms for one apartment
getRectRooms([],[], [], [], 0).
getRectRooms([Room1|T],R, VarsX, VarsY, TotalApartmentArea):-
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
    getRectRooms(T,R2, VarsX2, VarsY2, TotalApartmentArea2),

    append(CoordinatesX, VarsX2, VarsX),
    append(CoordinatesY, VarsY2, VarsY),    
    append([R1],R2,R),
    TotalApartmentArea #= Area+TotalApartmentArea2.
% -------------------

% -------------------- Getters predicates --------------------
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

% ---------------------------------- Floor Constraints ----------------------------------
% each apartment contains rooms belonging to the apartment
consistentApartments(_,[]).
consistentApartments(Floor,[H|T]):-
    consistentRooms(H),
    roomConstraint(Floor,H,H),
    consistentApartments(Floor,T).


% --------------------------------------------------------------------------------------------
%% output: [[(type,x,y,w,l),..],apartment2 ...,stairs,elev,hallwayslist]
% (x, y) are cartesian coordinates where x is the the horizontal axis, y is the vertical one. (0,0) represents the top left corner of the floor

solve(F,A,R):-
    statistics(runtime, [Start|_]),
    F=[Width,Height,Sides],
    % A=[[2,[5,[R1,R2,R3]]]],
    %length(R, NUM_AP),

    getAppartments(A,R),
    getRects(R, Rects, VarsX, VarsY, TotalUsedArea),
    % constraints: 

    % domain
    VarsX ins 0.. Width,
    VarsY ins 0.. Height,
   
    % apply constraints on the floor apartments
    consistentApartments(F,R),
   
    % non overlapping
    disjoint2(Rects),
   
    append(VarsX, VarsY, Vars),
    labeling([ffc,up,bisect,max(TotalUsedArea)], Vars),
   
    statistics(runtime, [Stop|_]),
    Runtime is Stop - Start,
    print("Runtime"+Runtime).


% ff,up,bisect  2797
% ffc,up,bisect 3364
% ff,down,enum 3773
% ffc,down,step 3761
% ff, down, bisect 3549
% ffc,down,bisect 3987
    