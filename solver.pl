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

% disjoint(+Rectangle, +Rectangle)
disjoint([X1,Y1,W1,L1],[X2,Y2,W2,L2]):-
    %Rect A
    % (XA1, YA1) ....................
    % ...............................
    % .................... (XA2, YA2)
    %Rect B
    % (XB1, YB1) ....................
    % ...............................
    % .................... (XB2, YB2)


    XA1 #= X1,
    XA2 #= X1+W1,
    YA1 #= Y1,
    YA2 #= Y1+L1,
    
    XB1 #= X2,
    XB2 #= X2+W2,
    YB1 #= Y2,
    YB2 #= Y2+L2,

    XB1 #>= XA2 #\/ XA1 #>= XB2 #\/
    YB1 #>= YA2 #\/ YA1 #>= YB2.

% check if two rooms are adjacent but not overlapping
adjacent([_, O1], [_, O2]):-
    O1 = [X1, Y1, W1, L1|_],
    O2 = [X2, Y2, W2, L2|_],
    
    % there exist an x that belongs to rect 1 and rect 2
    X1#=< X,
    X#=< X1+W1,
    X2#=< X,
    X#=< X2+W2,
    % there exist a y that belongs to rect 1 and rect 2
    Y1#=< Y,
    Y#=< Y1+L1,
    Y2#=< Y,
    Y#=< Y2+L2,
    % the two rectangles are not overlapping 
    disjoint([X1,Y1,W1,L1],[X2,Y2,W2,L2]).


 


%% output: [[(type,x,y,w,l),..],apartment2 ...,stairs,elev,hallwayslist]
% (x, y) are cartesian coordinates where x is the the horizontal axis, y is the vertical one. (0,0) represents the top left corner of the floor
solve(F,A,R):-
    F=[Width,Length,[North,East,South,West]],
    A=[[2,[5,[R1,R2,R3,R4,R5]]]],
    NUM_AP=2,
    %length(R, NUM_AP),
    getAppartment(A,R).


    


