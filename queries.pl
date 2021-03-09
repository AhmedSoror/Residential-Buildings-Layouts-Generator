% ---------------------------------------------------------- Input Format  ----------------------------------------------------------
solve([FloorWidth,FloorHeight,[North,West,South,East]],
      [[SameTypeCount,[RoomsCount,[[bedroom1,MinArea,Width,Height,Assigned],[hallway,1,_,_,none]]]], Apartment2 ],
      CorridorsCount,DuctsCount,
      OptionalSoftConstraints,
      OptionalGlobalConstraints,
      Result).

OptionalSoftConstraints = [ApartmentSoftConstraintOptions|SoftContraintsTail].
ApartmentSoftConstraintOptions = [RoomsDayLight, DistanceBetweenRooms, NearBedrooms, EasilyAccessibleBathroom].
OptionalGlobalConstraints = [LandscapeView, EqualDistanceToElev, FloorSymmetry, GoldenRatio].   % 1 constraint is on, 0 is off
DistanceBetweenRooms = [Room1, Room2, IsGreater, DistanceLimit].
DistanceBetweenRooms = [bedroom1, bathroom, 1, 10].    % dist > 10
DistanceBetweenRooms = [bedroom1, bathroom, 0, 10].    % dits < 10


% ------------------------------------------------------------------------------------------------------------------------------------
% ----------------------------------------------------------  output format ----------------------------------------------------------
R = [
  [1, [[bedroom1, 3, 1, 3, none], [0, 1, 0, 3]], 
      [[bedroom2, 3, 1, 3, none], [1, 1, 0, 3]], 
      [[kitchen, 3, 1, 3, none], [2, 1, 0, 3]], 
      [[hallway, 3, 4, 1, none], [0, 4, 3, 1]]], 
  [1, [[stairselev, 1, 1, 1, none], [3, 1, 1, 1]], 
      [[corridor, 1, 1, 1, none], [3, 1, 2, 1]],
      [[duct, 1, 1, 1, none], [3, 1, 0, 1]]]
].
% ------------------------------------------------------------------------------------------------------------------------------------

% ----------------------------------------------------------  Queries example ----------------------------------------------------------

% one apartment
solve([4,4,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[bedroom2,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,0,0,0]],[0,0,0,0],R).
% sun room
solve([4,4,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[sunroom,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,0,0,0]], [0,0,0,0],R).
% dinning room
solve([4,4,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[diningroom,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,0,0,0]], [0,0,0,0],R).
%all rooms
solve([7,7,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[sunroom,3,1,_,none],[diningroom,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,0,0,0]], [0,0,0,0],R).

% all apartments should have landscape view
False:
solve([7,7,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[sunroom,3,1,_,none],[diningroom,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,0,0,0]], [1,0,0,0],R).

True:
% 74428 msec
solve([7,7,[0,0,0,2]],[[1,[5,[[bedroom1,3,1,_,none],[sunroom,3,1,_,none],[diningroom,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,0,0,0]], [1,0,0,0],R).

% -------------------- two apartments --------------------

% 10 min  ==> 2.6 min
solve([4,4,[0,0,0,1]],
      [[1,[5,[[bedroom1,1,1,1,none],[bathroom,1,1,1,none],[hallway,1,_,_,none]]]], 
      [1,[5,[[bathroom,1,1,1,none],[bedroom2,1,1,1,none],[hallway,1,_,_,none]]]]],
      1,1,
      [[0,0,0,0], [0,0,0,0]], 
      [0,0,0,0],
      R).
% ---------------------
%  distance between rooms constraint
solve([4,4,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[bedroom2,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,[bedroom1, kitchen, 1, 2],0,0]],[0,0,0,0], R).
solve([4,4,[0,0,0,1]],[[1,[5,[[bedroom1,3,1,_,none],[bedroom2,3,1,_,none],[kitchen,3,1,_,none],[hallway,3,_,1,none]]]]],1,1,[[0,[bedroom1, kitchen, 0, 2],0,0]],[0,0,0,0], R).


% ------------------------------------------------------------------------------------------------------------------------------------
% ------------------------------------------------------------------------------------------------------------------------------------
% ------------------------------------------------------------------------------------------------------------------------------------