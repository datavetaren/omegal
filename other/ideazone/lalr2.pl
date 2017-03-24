main :-
    read_clauses('grammar.pl', Clauses),
    get_clauses(start(X,T,T1), Clauses, Match),
    pretty_program(Match),
    Source = [start(X,T,T1)],
    first(Source,Clauses,[T],Bindings),
    pretty_program(Bindings), nl, fail.


% ------------------------------------------------------
%  Construct LALR(1) state machine
%
%  State
%  state(Number,Seen,Follow,Lookahead)
%



init_env(Env,Clauses) :-
    InitItem = item('$start', [],[start(X,T,[])],[]),
    Env = env([state(0,InitItem)],[T],Clauses).

process_state(State, Env) :-
    Env = env(States, GroundVars, Clauses),
    State = state(Number,Seen,Follow,Lookahead),
    Follow = [First|Rest],
    first(Rest,Clauses,GroundVars,Lookahead1),
    append(Lookahead1,Lookahead,NewLookahead),
    

get_kernel(Item, Env, Kernel) :-
    Env = env(States, GroundVars, Clauses),
    Item = item(Head,Seen,Follow,Lookahead),
    Follow = [First|Rest],
    first(Rest,Clauses,GroundVars,NewLookahead0),
    append(Lookahead, NewLookahead0, NewLookahead1),
    sort(NewLookahead1, NewLookahead),
    get_clauses(Clauses, First, Match),
    itemize_clauses(Match, Lookahead, Items),

itemize_clauses([Head :- Body | Clauses], Lookahead, [Item|Items]) :-
    commas_to_list(Body, Goals),
    Item = item(Head,[], Goals, Lookahead)
    

% ------------------------------------------------------
%  first(+Goals,+Clauses,+Vars,Bindings)
%
%  Execute all possible variants Goals using Clauses as
%  the program and Vars as set of variables we are
%  interested in.
% -----------------------------------------------------
first(Goals,Clauses,Vars,Bindings) :-
    term_variables(Goals, AllVars),
    subtract_vars(AllVars, Vars, ExtVars),
    append(ExtVars, [interpret_nr(Goals,Clauses)], ExtList),
    list_to_ext(ExtList, Query),
    setof(Vars, Query, Bindings1),
    term_variables(Bindings1, Bindings1Vars),
    bind_all_vars(Bindings1Vars, '$var'),
    sort(Bindings1, Bindings2).
%    unbind_term(Bindings2, '$var', Bindings).

bind_all_vars([V|Vs],T) :- V = T, bind_all_vars(Vs,T).
bind_all_vars([],_).

unbind_term(Term,ToUnbind,NewTerm) :-
    !,
    (Term == ToUnbind -> true
     ;
     Term =.. [Functor|Args],
     unbind_term_list(Args,ToUnbind,NewArgs),
     NewTerm =.. [Functor|NewArgs]).

unbind_term_list([],_,[]).
unbind_term_list([T|Ts],ToUnbind,[T1|Ts1]) :-
     unbind_term(T,ToUnbind,T1),
     unbind_term_list(Ts,ToUnbind,Ts1).

% ------------------------------------------------------
%  subtract_vars(+Vars,+ToRemove,-Remaining)
%   Remove vars in Vars from ToRemove. The remaining set
%   of vars is in Remaning.
% ------------------------------------------------------
subtract_vars([],_,[]).
subtract_vars([X|Xs],Ys,Zs) :-
    (member_term(Ys, X) -> subtract_vars(Xs,Ys,Zs)
    ;Zs = [X|Zs1], subtract_vars(Xs,Ys,Zs1)).

% ------------------------------------------------------
%  member_term(Xs,X)
%  Like member(X,Xs), but using == when comparing terms.
%  This enables us to compare vars without unification.
% ------------------------------------------------------
member_term([Y|_], X) :-
    X == Y, !.
member_term([_|Ys], X) :-
    !, member_term(Ys,X).

% ------------------------------------------------------

% -----------------------------------------------------
%  interpret_nr(+Goals,+Clauses)
%   Interpret Goals using the defined Clauses _non_recursive_;
%   instead skips goals whenever a recursion would have occurred.
% -----------------------------------------------------

interpret_nr(Goals,Clauses) :-
    interpret_goals(Goals,Clauses,[]).

interpret_goals([],_Clauses, _Stack).
interpret_goals([Goal|Goals], Clauses, Stack) :-
%    write('Goal:'), nl, pretty(Goal), nl,
%    write('Stack:'), nl, write(Stack), nl,
    % If Goal already on stack, then skip it.
    (\+ member(Goal, Stack) ->
     interpret_goal(Goal, Clauses, Stack),
     interpret_goals(Goals, Clauses, Stack)
   ; interpret_goals(Goals, Clauses, Stack)).

interpret_goal(X = Y, _Clauses, _Stack) :-
    X = Y, !.
interpret_goal(Goal, Clauses, Stack) :-
    get_clauses(Goal, Clauses, Match),
    member(Goal :- Body, Match),
    commas_to_list(Body, Goals),
    interpret_goals(Goals, Clauses, [Goal|Stack]).

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
list_to_ext(L,C) :- foldl(L,'^',C).

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
     
