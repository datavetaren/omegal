main :-
    read_clauses('grammar.pl', Clauses),
    get_clauses(start(X,T,T1), Clauses, Match),
    pretty_program(Match),
    interpret([start(X,T,T1)], Clauses, Result0),
    simplify(Result0, Result1),
    pull(Result1, Result),
    pretty(Result), nl.


% -----------------------------------------------------
%  interpret(+Goals,+Clauses,-Result)
%   Interpret Goals using the defined Clauses. Result is
%   a statement with conjunctions and disjunctions which
%   holds the result of the execution, where we never
%   visit the same goal twice (to prevent recursion.)
% -----------------------------------------------------

interpret(Goals,Clauses,Result) :-
    interpret0(Goals,Clauses,[],Result).

interpret0([],_Clauses, _Stack, true).
interpret0([Goal|Goals], Clauses, Stack, Result) :-
%    write('Goal:'), nl, pretty(Goal), nl,
%    write('Stack:'), nl, write(Stack), nl,
    % If Goal already on stack, then skip it.
    (\+ member(Goal, Stack) ->
     interpret_goal(Goal, Clauses, Stack, Result0),
     interpret0(Goals, Clauses, Stack, Result1),
     Result = (Result0, Result1)
   ; interpret0(Goals, Clauses, Stack, Result)).

interpret_goal(Goal, _Clauses, _Stack, Result) :-
    is_unification(Goal), !, Result = Goal.
interpret_goal(Goal, Clauses, Stack, Result) :-
    get_clauses(Goal, Clauses, Match),
    interpret_match(Match, Goal, Clauses, [Goal|Stack], Result).

interpret_match([], _Goal, _Clauses, _Stack, false).
interpret_match([M|Match], Goal, Clauses, Stack, (Result ; Result1)) :-
    interpret_match_one(M, Goal, Clauses, Stack, Result),
    interpret_match(Match, Goal, Clauses, Stack, Result1).

interpret_match_one(Head :- Body, Goal, Clauses, Stack, Result) :-
    Head =.. [H|ArgsHead],
    Goal =.. [H|ArgsGoal],
    append(Result1, [Result0], Result2),
    interpret_match_unify(ArgsHead, ArgsGoal, Result1),
    commas_to_list(Body, Goals),
    interpret0(Goals, Clauses, Stack, Result0),
    list_to_commas(Result2, Result).

interpret_match_unify([], [], []).
interpret_match_unify([A|As], [B|Bs], Result) :-
    A = B,
    interpret_match_unify(As, Bs, Result).

% ------------------------------------------------
%  simplify(+Prog,-Result)
%   Remove true from conjunctions
%   Remove false from disjunctions
% ------------------------------------------------
simplify(A,B) :- \+ var(A), simplify_nonvar(A,B), !.
simplify(A,B) :-
    !,
    (var(A) -> B = A
   ; A =.. [Functor|AArgs],
     simplify_list(AArgs, BArgs),
     B =.. [Functor|BArgs]
     ).

simplify_nonvar((A,true),B) :- simplify(A,B).
simplify_nonvar((true,A),B) :- simplify(A,B).
simplify_nonvar((A;false),B) :- simplify(A,B).
simplify_nonvar((false;A),B) :- simplify(A,B).

simplify_list([],[]).
simplify_list([A|As],[B|Bs]) :-
    simplify(A,B),
    simplify_list(As,Bs).

% -----------------------------------------------------
% pull(A,B)
%  Given (A ; B), C
%  Rewrite it to (A, C ; B, C)
%  This enables further optimizations
% -----------------------------------------------------

pull(((A ; B), C), ((A, C1) ; (B, C1))) :-
    !, pull(C, C1).
pull((A,B), (A1,B1)) :-
    !,
    pull(A, A1),
    pull(B, B1).
pull(A,A).

% ------------------------------------------------
% is_unification(+Term)
%  True if Term is a unification operation
% ------------------------------------------------
is_unification(_ = _).

% ------------------------------------------------
% get_clauses(+Goal, +Clauses, -Match)
%  Get clauses from Clauses that match Goal.
%  The result is in Match.
% ------------------------------------------------
get_clauses(Goal, Clauses, Match) :-
    get_clauses0(Clauses, Goal, Match).

get_clauses0([], _Goal, []).
get_clauses0([(Head :- Body) | Clauses], Goal, [MatchGoal|Match]) :-
    \+ Head \= Goal, !,
    copy_term(Head:-Body, MatchGoal),
    get_clauses0(Clauses, Goal, Match).
get_clauses0([_Clause|Clauses], Goal, Match) :-
    get_clauses0(Clauses, Goal, Match).

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
     
