:-use_module(library(clpfd)).


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


adjacent([_, O1], [_, O2]):-
    O1 = [X1, Y1, W1, L1|_],
    O2 = [X2, Y2, _, _|_],
    X1#=<X2,
    Y2#=Y1+W1,
    X2#=<X1 + L1.

adjacent([_, O1], [_, O2]):-
    O1 = [X1, Y1, _, _|_],
    O2 = [X2, Y2, _, L2|_],
    X2#=<X1,
    Y2#=Y1+W1,
    X1#=<X2 + L2.

adjacent([_, O1], [_, O2]):-
    O1 = [X1, Y1, W1, L1|_],
    O2 = [X2, Y2, _, _|_],
    Y1#=<Y2,
    X1=X2,
    Y2#=<Y1 + W1.

adjacent([_, O1], [_, O2]):-
    O1 = [X1, Y1, _, _|_],
    O2 = [X2, Y2, W2, _|_],
    Y2#=<Y1,
    X1=X2,
    Y1#=<Y2 + W2.


%% output: [[(type,x,y,w,l),..],apartment2 ...,stairs,elev,hallwayslist]
solve(F,A,R):-
    F=[Width,Length,[North,East,South,West]],
    A=[[2,[5,[R1,R2,R3,R4,R5]]]],
    NUM_AP=2,
    %length(R, NUM_AP),
    getAppartment(A,R).


    


