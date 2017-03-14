main :-
    read_clauses('grammar.pl', Clauses),
    get_clause(Clauses, start(X,T,T1) :- Body),
    pretty_program(Clauses).

is_non_epsilon_clause(Clauses, Head :- Body) :-
    commas_to_list(Body, Goals),
    is_non_epsilon_goals(Goals, Clauses).

is_non_epsilon_clause([Goal|Goals], _) :- is_unification(Goal),
is_epsilon_clause([Goal|Goals], Clauses) :-
    (is_epsilon_clause(

% ------------------------------------------------
% is_unification(+Term)
%  True if Term is a unification operation
% ------------------------------------------------
is_unification(_ = _).

% ------------------------------------------------
%
%

% ------------------------------------------------
% commas_to_list(+Commas, -List)
%  Given a comma based term (a, (b, (c, d))),
%  convert into a list: [a, b, c, d]
% ------------------------------------------------
commas_to_list((A,B),[A|C]) :- !, commas_to_list(B,C).
commas_to_list(X,[X]).

% ------------------------------------------------
% list_to_commas(+List, -Commas)
%  Given a list [a,b,c,d]
%  convert into a comma terms: (a, (b, (c, d))),
% ------------------------------------------------
list_to_commas(L,C) :- foldl(L,',',C).

list_to_semi(L,C) :- foldl(L,';',C).

foldl([A|B],F,G) :- foldl(B,F,C), G =.. [F,A,C].
foldl([B], _, B).

% ------------------------------------------------
% read_clauses(+File, -Clauses)
%  Read clauses from the File and put them into
%  a list (standard term representation.)
% ------------------------------------------------
read_clauses(File,Clauses) :-
    open(File,read,F),
    read_clauses_stream(F,Clauses1),
    filter_clauses(Clauses1, Clauses),
    close(F).

% ------------------------------------------------
% filter_clauses(+Clauses, -Clauses)
%  Remove anything that is not a clause.
%  (When reading in file we get the atom
%   end_of_file)
% ------------------------------------------------
filter_clauses([], []) :- !.
filter_clauses([(Head:-Body)|Cs], [(Head:-Body)|Cs1]) :-
    !, filter_clauses(Cs,Cs1).
filter_clauses([_|Cs], Cs1) :-
    filter_clauses(Cs,Cs1).

% ------------------------------------------------
% read_clauses_stream(+F, -Clauses)
%  Read Prolog clauses from the stream F and return
%  the Clauses as a list of terms (standard
%  representation.)
% ------------------------------------------------
read_clauses_stream(F,[]) :-
    at_end_of_stream(F), !.
read_clauses_stream(F, [C|Cs]) :-
    read(F,C), read_clauses_stream(F, Cs).

% ------------------------------------------------
% print_file(+FileName)
%  Print the file as text.
% ------------------------------------------------
print_file(FileName) :-
    open(FileName, read, Stream),
    print_file1(Stream),
    close(Stream).

print_file1(Stream) :-
    \+ at_end_of_stream(Stream),
    get_char(Stream, Char),
    put_char(Char),
    print_file1(Stream).
print_file1(_).

% ------------------------------------------------
% pretty_program(+Clauses)
%  A list of clauses are printed line by line
%  using pretty/1
% ------------------------------------------------
pretty_program([]).
pretty_program([C|Cs]) :-
    pretty(C), nl,
    pretty_program(Cs).

% ------------------------------------------------
% pretty(Term, S)
%  It is so ugly to see all these _G1234
%  uninstantiated variables when printing. Let us
%  replace them with more readable stuff when
%  printing.
% ------------------------------------------------
pretty(Term) :- current_output(S), pretty(Term, S).

pretty(Term, S) :- pretty(Term, Term1, S), write(S, Term1).
pretty(Term, Term1, S) :- copy_term(Term, Term0), pretty1(Term0, [], _, Term1, S).
pretty1(V, EnvIn, EnvOut, Found, _) :- 
    var(V), !,
    length(EnvIn,N),
    get_name(N, Name),
    V = Name,
    EnvOut = [Name|EnvIn],
    Found = Name.
pretty1(Term, EnvIn, EnvOut, Term1, S) :-
    Term =.. Xs,
    (Xs = [_] -> pretty1_leaf(Term, EnvIn, EnvOut, Term1, S) ;
     pretty1_list(Xs, EnvIn, EnvOut, Ys, S),
     Term1 =.. Ys).

pretty1_list([], Env, Env, [], _).
pretty1_list([X|Xs], EnvIn, EnvOut, [Y|Ys], S) :-
    pretty1(X, EnvIn, EnvOut1, Y, S),
    pretty1_list(Xs, EnvOut1, EnvOut, Ys, S).

pretty1_leaf(Term, Env, Env, Term, _).

get_name(0,'A') :- !.
get_name(N,Name) :-
    get_name1(N,Xs), reverse(Xs,Ys), atom_to_chars(Name,Ys).
get_name1(N,Xs) :-
    (N > 0 ->
     X is 65+mod(N,26), N1 is div(N,26), get_name1(N1,Xs1), Xs = [X|Xs1]
    ;Xs = []).
     
