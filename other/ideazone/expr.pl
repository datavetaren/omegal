start(E,X,Y) :- expr(E,X,Y).

expr(E,X,Y) :- expr(E1,X,Y1), Y1 = ['+'|Y2], term(T,Y2,Y), E = E1+T.
expr(T,X,Y) :- term(T,X,Y).

term(T,X,Y) :- term(T1,X,Y1), Y1 = ['*'|Y2], factor(F,Y2,Y), T = T1*F.
term(F,X,Y) :- factor(F,X,Y).

factor(F,X,Y) :- X = ['('|Y1], expr(E,Y1,Y2), Y2 = [')'|Y], F = E.
factor(F,X,Y) :- X = [N|Y], F = num(N), number(N).
