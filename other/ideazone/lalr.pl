lalr(File) :-
    get_clauses(File, Clauses),
    rearrange_clauses(Clauses, Clauses1),
    write('-----------------------------------------------------------'), nl,
    print_file(File),
    write('-----------------------------------------------------------'), nl,
    create_env(Clauses1, EnvIn),
    EnvIn = env([(Start :- _)|_], _),
    specialize(Start, true, EnvIn, EnvOut, SpecGoal),
    find_all_spec(EnvOut, SpecGoal, All),
    write('Writing to file out.pl'), nl,
    open('out.pl',write,S),
    pretty_specs(All, EnvOut, S),
    close(S).

% ------------------------------------------------
% pretty_specs(+SpecGoals, +Env)
%  Given a list of specialization goals, print
%  their corresponding clauses.
% ------------------------------------------------
pretty_specs([], _, _).
pretty_specs([SpecGoal | SpecGoals], Env, S) :-
    Env = env(_, Specializations),
    member(spec(_,_,SpecGoal,SpecClauses), Specializations), !,
    pretty_clauses(SpecClauses, S),
    pretty_specs(SpecGoals, Env, S).

pretty_clauses([], _).
pretty_clauses([Head :- (Body1,Body2)|Clauses], S) :-
    append(Body1,Body2,Body0),
    list_to_commas(Body0, Body),
    pretty(Head :- Body, S), write(S, '.'), nl(S),
    pretty_clauses(Clauses, S).

% ------------------------------------------------
% find_all_spec(+Env,+Goal,-All)
%  Recursively traverse all reachable goals starting
%  from Goal. All is a list of all such goals.
% ------------------------------------------------
find_all_spec(Env, Goal, All) :-
    find_all_spec0(Env, Goal, [], All0),
    reverse(All0, All).

find_all_spec0(Env, Goal, In, Out) :-
    Env = env(_, Specs),
    find_all_spec1(Specs, Goal, In, Out).

find_all_spec1(Specs, SpecGoal, In, Out) :-
    % write('check '), pretty(SpecGoal), nl,
    (member(SpecGoal, In) -> In = Out
    ; member(spec(_, _, SpecGoal, SpecClauses), Specs), !,
      In1 = [SpecGoal | In],
      find_all_spec2(SpecClauses, Specs, SpecGoal, In1, Out)
    ).

find_all_spec2([], _, _, Out, Out).
find_all_spec2([(_ :- (_,Goals))|Clauses], Specs, SpecGoal, In, Out) :-
    find_all_spec3(Goals, Specs, In, In1),
    find_all_spec2(Clauses, Specs, SpecGoal, In1, Out).

find_all_spec3([], _, Out, Out).
find_all_spec3([Goal|Goals], Specs, In, Out) :-
     find_all_spec1(Specs, Goal, In, In1),
     find_all_spec3(Goals, Specs, In1, Out).

% ------------------------------------------------
% create_env(+Clauses, -Env)
%  Create a new environment for our specializer.
% ------------------------------------------------
create_env(Clauses, env(Clauses, [])).

% ------------------------------------------------
%  get_starting_context(+Env, -Context)
%   Create a new starting context, the main clause
%   for the initial state of parsing. (The first
%   clause in Env will be defined as a the starting
%   clause.)
% ------------------------------------------------
get_starting_context(true).

% ------------------------------------------------
%  Definition: Specialization context
%
%  We define context(Clause, LookaheadUnifications)
%  to define:
%  S ::= A [*] B C <LookaheadUnifications>
%  Where [*] is represented with a number telling its
%  position (0 = as leading.) LookaheadUnifications tells
%  what unifications may follow if we reach the
%  end of the clause.
% ------------------------------------------------

find_specialization([spec(Goal, Context, SpecGoal, _)|_], Goal, Context, SpecGoal).
find_specialization([_|Specs], Goal, Context, SpecGoal) :-
    find_specialization(Specs, Goal, Context, SpecGoal).

% ------------------------------------------------
%  specialize(+Goal, +Context, +EnvIn, -EnvOut, -Spec
% ------------------------------------------------
specialize(Goal, Context, EnvIn, EnvOut, SpecGoal) :-
    write('specialize '), pretty(Goal), nl,
    EnvIn = env(_, Specializations),
    (find_specialization(Specializations, Goal, Context, SpecGoal) ->
     EnvOut = EnvIn
    ;create_specialization(Goal, Context, EnvIn, EnvOut, Spec),
     Spec = spec(Goal, Context, SpecGoal, _)
    ).

% ------------------------------------------------
%  create_specialization(+Goal, +Context, +EnvIn, -EnvOut, -Spec)
% ------------------------------------------------
create_specialization(Goal, Context, EnvIn, EnvOut, Spec) :-
    write('create_specialization for '), pretty(Goal), nl,
    EnvIn = env(Clauses, Specializations),
    get_specialization_count(Specializations, Goal, Count),
    functor(Goal, GoalName, GoalArity),
    name(GoalName, GoalNameChars),
    name('_', UnderscoreChars),
    name(Count, CountChars),
    append(GoalNameChars, UnderscoreChars, GoalNameChars0),
    append(GoalNameChars0, CountChars, SpecGoalChars),
    name(SpecGoalName, SpecGoalChars),
    functor(SpecGoal, SpecGoalName, GoalArity),
    unify_args(SpecGoal, Goal), 
       write('create_specialization for goal '), pretty(Goal), write(' --> '),
       pretty(SpecGoal), nl,
    bagof((Goal:-Body), member((Goal:-Body),Clauses), Match),
       write('found '), length(Match,N), write(N), write(' matches...'), nl,
    Spec = spec(Goal, Context, SpecGoal, SpecClauses),
    EnvIn1 = env(Clauses, [Spec | Specializations]),
    create_specializations(Match, Goal, SpecGoal, Context, EnvIn1, EnvOut, SpecClauses).

% ------------------------------------------------
%  create_specializations(+Clauses, +Goal, +SpecGoal, +Context, +EnvIn, -EnvOut, -SpecClauses)
% ------------------------------------------------
create_specializations([], _, _, _, Env, Env, []).
create_specializations([(Head :- (BodyUnifications,BodyGoals))|Clauses],
		       Goal, SpecGoal, Context, EnvIn, EnvOut,
		       [(SpecGoal0 :- (BodyUnifications1,SpecGoals))|
			SpecClauses]) :-
    write('create_specializations for '), pretty(Head), nl,
    copy_term(all(Head,BodyUnifications,BodyGoals,Goal,SpecGoal,Context),
	      all(Head0,BodyUnifications0,BodyGoals0,Goal0,SpecGoal0,Context0)),
    Head0 = Goal0,
    unify_args(Head0, SpecGoal0),
    create_specialization_goals(BodyGoals0, Context0, EnvIn, EnvIn1, SpecGoals),
    (SpecGoals = [] -> append(BodyUnifications0, [\+ \+ Context0, !], BodyUnifications1)
    ; append(BodyUnifications0, [!], BodyUnifications1)
    ),
    create_specializations(Clauses, Goal, SpecGoal, Context, EnvIn1, EnvOut,
			   SpecClauses).

% ------------------------------------------------
%  unify_args(Term1,Term2)
%   Ignore leading functor in Term1 and Term2 and unify args
% ------------------------------------------------
unify_args(Term1, Term2) :-
	 Term1 =.. [_|Args1],
	 Term2 =.. [_|Args2],
	 Args1 = Args2.

% ------------------------------------------------
%  create_specialization_goals(+Goals, +Context, +EnvIn, -EnvOut, -SpecGoals)
% ------------------------------------------------
create_specialization_goals([], _, Env, Env, []).
create_specialization_goals([Goal|Goals], Context, EnvIn, EnvOut,[SpecGoal|SpecGoals]) :-
    write('create_specialization_goals for '), pretty(Goal), nl,
    EnvIn = env(Clauses, _),
    % Get lookahead unifications for this point.
    write('get lookahead unifications...'), nl,
    lookahead_unifications(Goals, Clauses, UnificationTree),
    write('   Goals: '), pretty(Goals-UnificationTree), nl,
    write('get lookahead unifications done...'), nl,
    % First see if input UnificationTree needs to be used
    (has_all_transitive_epsilon(Goals, Clauses) ->
       % Check if concatenation is needed (is it already entailed?)
       (Context = ','(UnificationTree, _) ->
	   Context1 = Context
         ; Context1 = ','(UnificationTree, Context),
           write('End can be reached. Concatenate with context.'), nl
       )
     ; Context1 = Context,
       write('No need to use context'), nl
    ),
    specialize(Goal, Context1, EnvIn, EnvIn1, SpecGoal),
    create_specialization_goals(Goals, Context, EnvIn1, EnvOut, SpecGoals).

% ------------------------------------------------
% get_specialization_count(+Specializations, +Head, -Count)
%  Count the number of specializations with Head.
% ------------------------------------------------
get_specialization_count(Specializations, Goal, Count) :-
    findall(spec(Goal,Context,SpecGoal,SpecClauses),
	  member(spec(Goal,Context,SpecGoal,SpecClauses),Specializations),
	  Match),
    length(Match, Count).

% ------------------------------------------------
% lookahead_unifications(Goals, Clauses, UnificationTree)
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
     ;list_to_commas(UnificationTrees, UnificationTree)
     ).

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
    ;list_to_semi(UnificationTrees, UnificationTree)
    ).

lookahead_unifications_clauses([],_,_,_,[]).
lookahead_unifications_clauses([(Head0:-(Us0,Ps0))|Match],Goal0,Clauses,Visited,UnificationTrees) :-
    copy_term(all(Head0,Us0,Ps0,Goal0), all(Head,Us,Ps,Goal)),
    Head = Goal,
    % Does this clause have any predicate calls?
    (Ps = [] -> % No, then terminate recursion
       DArg0 = Us
     ; Ps = [P|Ps1],
       % First get the continuation unification tree
       (has_transitive_epsilon(Clauses,P) ->
	% First predicate goal has an epsilon clause, so we continue.
	lookahead_unifications_goals(Ps1,Clauses,UnificationTreesPs1)
	; UnificationTreesPs1 = []
       ),
       % Then get the unification tree for P
       (member(P, Visited) ->
	% P has already been visited, so we terminate
	 UnificationTreeP = true
       ; Visited1 = [P|Visited],
	lookahead_unifications_goal(Clauses,P,Visited1,UnificationTreeP)
       ),
      (UnificationTreeP = true -> UnificationTreesPs = UnificationTreesPs1
       ; UnificationTreesPs = [UnificationTreeP | UnificationTreesPs1]),
       % At this point UnificationTreesPs is the unifications
       % from P and onwards (if needed.) Now we glue in the
       % primary unifications of the clause itself.
       append(Us, UnificationTreesPs, DArg0)
    ),
    (DArg0 = [] -> DArg = false
     ;DArg0 = [D] -> DArg = D
     ;list_to_semi(DArg0, DArg)),
    UnificationTrees = [DArg|UnificationTrees1],
    lookahead_unifications_clauses(Match,Goal,Clauses,Visited,UnificationTrees1).

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

has_all_transitive_epsilon([],_).
has_all_transitive_epsilon([Goal|Goals],Clauses) :-
    has_transitive_epsilon1(Clauses,Goal,[]),
    has_all_transitive_epsilon(Goals,Clauses).

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
% list_to_commas(+List, -Commas)
%  Given a list [a,b,c,d]
%  convert into a comma terms: (a, (b, (c, d))),
% ------------------------------------------------
list_to_commas(L,C) :- foldl(L,',',C).

list_to_semi(L,C) :- foldl(L,';',C).

foldl([A|B],F,G) :- foldl(B,F,C), G =.. [F,A,C].
foldl([B], _, B).

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
     
