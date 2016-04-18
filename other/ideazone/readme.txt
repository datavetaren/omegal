?- [lalr].

?- lalr('grammar.pl').
...
Writing to file out.pl

?- [out].
... bla bla a lot of warnings ...

% Forward parsing:
?- start_0(X, [a,a,b,b,b],[]).
X = prog([a, a, a, b, b]).

?-

% Backward parsing:
?- start_0(prog([a,a,b,b,b]), T, []).
T = [a, a, a, b, b].

?-
