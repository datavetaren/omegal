lalr(File) :-
    get_clauses(File, Clauses),
    rearrange_clauses(Clauses, Clauses1),
    write('-----------------------------------------------------------'), nl,
    print_file(File),
    write('-----------------------------------------------------------'), nl,
    lookahead_unifications([start(A,B,C)], Clauses1, UnificationTree),
    write('Lookahead for: '), pretty(start(A,B,C)), nl,
    pretty((start(A,B,C) :- UnificationTree)),
    nl.

% ------------------------------------------------
% lookahead_unififications(...)
%  This is a combination of FIRST and FOLLOW functions for LALR(1)
%  parsers. It is more precise because we allow a continuation
%  when processing a predicate. This continuation is then used
%  if an epsilon production (i.e. a predicate with no other calls
%  to predicates.) This has the effect that the unification tree
%  returned will only use the continuation on those branches
%  where an epsilon clause is present.
%  (An epsilon clause different from "empty" clause;
%   an empty clause is usually called a fact.)
% ------------------------------------------------

lookahead_unifications(Goals,Clauses,UnificationTree) :-
    lookahead_unifications_goals(Goals,Clauses,UnificationTrees),
    (UnificationTrees = [] -> UnificationTree = true
     ;UnificationTrees = [Tree] -> UnificationTree = Tree
     ;UnificationTree = and(UnificationTrees)).

lookahead_unifications_goals([],_,[]).
lookahead_unifications_goals([G|Goals],Clauses,UnificationTrees) :-
     lookahead_unifications_goal(Clauses,G,[],UnificationTree1),
    (has_transitive_epsilon(Clauses,G) ->
     lookahead_unifications_goals(Goals, Clauses, UnificationTrees1),
     UnificationTrees = [UnificationTree1|UnificationTrees1]
   ; UnificationTrees = [UnificationTree1]
    ).

lookahead_unifications_goal(Clauses,Goal,Visited,UnificationTree) :-
%    write('Check goal '), pretty(Goal), nl,
    % Compute the list of matching clauses for this goal.
    bagof((Goal:-Body), member((Goal:-Body),Clauses), Match),
    % Create a disjunction of the results
    Visited1 = [Goal | Visited],
    lookahead_unifications_clauses(Match,Goal,Clauses,Visited1,UnificationTrees),
    (UnificationTrees = [] -> UnificationTree = false
    ;UnificationTrees = [T] -> UnificationTree = T
    ;UnificationTree = or(UnificationTrees)
    ).

lookahead_unifications_clauses([],_,_,_,[]).
lookahead_unifications_clauses([(Goal:-(Us,Ps))|Match],Goal,Clauses,Visited,UnificationTrees) :-
    % Does this clause have any predicate calls?
    (Ps = [] -> % No, then terminate recursion
       DArg0 = Us
     ; Ps = [P|Ps0],
       % First get the continuation unification tree
       (has_transitive_epsilon(Clauses,P) ->
	% First predicate goal has an epsilon clause, so we continue.
	lookahead_unifications_goals(Ps0,Clauses,UnificationTreesPs0)
	; UnificationTreesPs0 = []
       ),
       % Then get the unification tree for P
       (member(P, Visited) ->
	% P has already been visited, so we terminate
	 UnificationTreeP = true
       ; Visited1 = [P|Visited],
	lookahead_unifications_goal(Clauses,P,Visited1,UnificationTreeP)
       ),
      (UnificationTreeP = true -> UnificationTreesPs = UnificationTreesPs0
       ; UnificationTreesPs = [UnificationTreeP | UnificationTreesPs0]),
       % At this point UnificationTreesPs is the unifications
       % from P and onwards (if needed.) Now we glue in the
       % primary unifications of the clause itself.
       append(Us, UnificationTreesPs, DArg0)
    ),
    (DArg0 = [] -> DArg = false
     ;DArg0 = [D] -> DArg = D
     ;DArg = and(DArg0)),
    UnificationTrees = [DArg|UnificationTrees0],
    lookahead_unifications_clauses(Match,Goal,Clauses,Visited,UnificationTrees0).

% ------------------------------------------------
% has_epsilon(+Clauses,+Head)
%  Is true iff Head has a clause which has no predicate calls
%  (i.e. it is a leaf in the production grammar.)
% ------------------------------------------------
has_epsilon(Clauses,Head) :-
    member((Head:-(_,[])), Clauses), !.

% ------------------------------------------------
% has_transitive_epsilon(+Clauses,+Head)
%  Is true iff the Head can produce epsilon
%  directly or indirectly.
% ------------------------------------------------
has_transitive_epsilon(Clauses,Head) :-
    has_transitive_epsilon1(Clauses,Head,[]).

has_transitive_epsilon1(Clauses,Head,Visited) :-
    \+ member(Head,Visited),
    Visited1 = [Head|Visited],
    (has_epsilon(Clauses,Head) -> true
   ; member((Head:-(_,Ps)), Clauses),
     has_all_transitive_epsilon1(Ps,Clauses,Visited1)
    ).

has_all_transitive_epsilon1([],_,_).
has_all_transitive_epsilon1([P|Ps],Clauses,Visited) :-
    has_transitive_epsilon1(Clauses,P,Visited),
    has_all_transitive_epsilon1(Ps,Clauses,Visited).

% ------------------------------------------------
% rearrange_clause(+Clause, -Clause)
%  Given a clause we rearrange the goals so that
%  goals are partition into two parts
%  1) Primitive unifications 
%  2) All other goals.
% ------------------------------------------------
rearrange_clause((Head:-Body),(Head:-Body1)) :-
    rearrange_body(Body,Body1).

rearrange_body(BodyIn,BodyOut) :-
    commas_to_list(BodyIn,Body0),
    (bagof(X, (member(X, Body0), is_unification(X)), Body1),! ; Body1 = []),
    (bagof(X, (member(X, Body0), \+ is_unification(X)), Body2),!; Body2 = []),
    BodyOut = ','(Body1, Body2).

rearrange_clauses([], []).
rearrange_clauses([C|Cs], [C1|Cs1]) :-
    rearrange_clause(C,C1), rearrange_clauses(Cs,Cs1).

% ------------------------------------------------
% is_unification(+Term)
%  True if Term is a unification operation
% ------------------------------------------------
is_unification(_ = _).

% ------------------------------------------------
% commas_to_list(+Commas, -List)
%  Given a comma based term (a, (b, (c, d))),
%  convert into a list: [a, b, c, d]
% ------------------------------------------------
commas_to_list((A,B),[A|C]) :- !, commas_to_list(B,C).
commas_to_list(X,[X]).

% ------------------------------------------------
% get_clauses(+File, -Clauses)
%  Get the clauses from the File and put them into
%  a list (standard term representation.)
% ------------------------------------------------
get_clauses(File,Clauses) :-
    open(File,read,F),
    read_clauses(F,Clauses1),
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
% read_clauses(+F, -Clauses)
%  Read Prolog clauses from the stream F and return
%  the Clauses as a list of terms (standard
%  representation.)
% ------------------------------------------------
read_clauses(F,[]) :-
    at_end_of_stream(F), !.
read_clauses(F, [C|Cs]) :-
    read(F,C), read_clauses(F, Cs).

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
% pretty(Term)
%  It is so ugly to see all these _G1234
%  uninstantiated variables when printing. Let us
%  replace them with more readable stuff when
%  printing.
% ------------------------------------------------
pretty(Term) :- pretty(Term, Term1), write(Term1).
pretty(Term, Term1) :- copy_term(Term, Term0), pretty1(Term0, [], _, Term1).
pretty1(V, EnvIn, EnvOut, Found) :- 
    var(V), !,
    length(EnvIn,N),
    get_name(N, Name),
    V = Name,
    EnvOut = [Name|EnvIn],
    Found = Name.
pretty1(Term, EnvIn, EnvOut, Term1) :-
    Term =.. Xs,
    (Xs = [_] -> pretty1_leaf(Term, EnvIn, EnvOut, Term1) ;
     pretty1_list(Xs, EnvIn, EnvOut, Ys),
     Term1 =.. Ys).

pretty1_list([], Env, Env, []).
pretty1_list([X|Xs], EnvIn, EnvOut, [Y|Ys]) :-
    pretty1(X, EnvIn, EnvOut1, Y),
    pretty1_list(Xs, EnvOut1, EnvOut, Ys).

pretty1_leaf(Term, Env, Env, Term).

get_name(0,'A') :- !.
get_name(N,Name) :-
    get_name1(N,Xs), reverse(Xs,Ys), atom_to_chars(Name,Ys).
get_name1(N,Xs) :-
    (N > 0 ->
     X is 65+mod(N,26), N1 is div(N,26), get_name1(N1,Xs1), Xs = [X|Xs1]
    ;Xs = []).
     
