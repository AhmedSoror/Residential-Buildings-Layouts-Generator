:-use_module(library(clpfd)).

a(A,B):-
    A#\=3 #==> B.

b(A,B):-
    B#=A.

adjacent2([_,[X1,W1,Y1,H1]],[_,[X2,W2,Y2,H2]]):-
    X2#=X1+W1 #==> Y#>Y1 #/\ Y#<Y1+H1  #/\ Y#>Y2 #/\ Y#<Y2+H2, 
    X2#=X1-W2 #==> Y#>Y1 #/\ Y#<Y1+H1  #/\ Y#>Y2 #/\ Y#<Y2+H2,
    Y2#=Y1+H1 #==> X#>X1 #/\ X#<X1+W1  #/\ X#>X2 #/\ X#<X2+W2,
    Y2#=Y1-H2 #==> X#>X1 #/\ X#<X1+W1  #/\ X#>X2 #/\ X#<X2+W2.


adjacent([_,[X1,W1,Y1,H1]],[_,[X2,W2,Y2,H2]]):-
    X2#=X1+W1 #==> Y1#< Y2+H2 #/\ Y2#< Y1+H1, 
    X2#=X1-W2 #==> Y1#< Y2+H2 #/\ Y2#< Y1+H1,
    Y2#=Y1+H1 #==> X1#< X2+W2 #/\ X2#< X1+W1,
    Y2#=Y1-H2 #==> X1#< X2+W2 #/\ X2#< X1+W1,
    X1#=X2 #==> Y1#\=Y2,
    Y1#=Y2 #==> X1#\=X2.

getHallway([],R,R).
getHallway([H|T],Acc,R):-
    H=[[hallway|_],_],
    append(Acc,[H],Acc1),
    getHallway(T,Acc1,R).
getHallway([H|T],Acc,R):-
    H\=[[hallway|_],_],
    getHallway(T,Acc,R).
    