main :-
    (retract(mycount(_)) -> true ; true),
    assert(mycount(1)),
    init_env(Env, 'expr.pl'),
    Env = env([InitItem|_], _Clauses),
    item_closure(InitItem, Env, Closure),
    pretty_program(Closure),
%    first(InitItem,Clauses,Lookahead),
%    pretty_program(Lookahead),
    nl.


main2 :-
    init_env(Env, 'expr.pl'),
    Env = env(_, Clauses),
%    Goals = [
%	X = ground, Y1 = ground, expr(E1,X,Y1), Y1 = ['+'|Y2], expr(E2,Y2,Y), E = E1+E2
%    ],
    Goals = start(_,ground,ground),
    groundness_analysis(Clauses, Goals),
    write('hej '),
    pretty(Goals), nl.

% ------------------------------------------------------
%  Construct LALR(1) state machine
%
%  State
%  state(Number,Seen,Follow,Lookahead)
%



init_env(Env, GrammarFile) :-
    read_clauses(GrammarFile, Clauses),
%    InitItem = item('$start', [],[start(X,T,T1)],[T,T1],[]),

     InitItem = item(factor(F,X,Y), [X =['('|Y1]], [expr(E,Y1,Y2), Y2 = [')'|Y], F = E], [X,Y,Y1,Y2], []),
%    InitItem = item(expr(E,X,Y), [],[expr(E1,X,Y1),Y1=['+'|Y2], expr(E2,Y2,Y), E=E1+E2],[X],[]),
%    InitItem = item(expr(T,X,Y), [],[term(T,X,Y)],[X],[]),

    Env = env([InitItem],Clauses).


% -----------------------------------------------
%   groundness
%
%  Given a Goal goal(X,Y,Z, ...) with a choice of the
%  variables X,Y,Z,... bound to 'ground' we run
%  an abstract interpretation using Clauses.
%  Any variables (X,Y,Z,...) may be bound to 'ground'
%  to indicate that they are always ground.
%
%  This needs to be rewritten using a fixpoint analysis
%  over a lattice...
%

groundness_analysis(Clauses, Goal) :-
    (Goal = [_ | _] ->
     groundness_analysis_goals(Goal, Clauses, [])
   ; groundness_analysis_goals([Goal], Clauses, [])
    ).

groundness_analysis_goals([], _, _).
groundness_analysis_goals([Goal | Goals], Clauses, Visited) :-
%    write('check goal '), pretty(Goal), nl,
    (Goal = (A = B) ->
     (A == ground
      -> term_variables(B, Vars), bind_all_vars(Vars, ground)
      ; B == ground
      -> term_variables(A, Vars), bind_all_vars(Vars, ground)
      ; true
      ),
     Visited1 = Visited
     ;
     functor(Goal, Functor, Arity),
     (member(Functor/Arity, Visited) -> true
      ; Visited1 = [Functor/Arity | Visited],
        (get_clauses(Goal, Clauses, Match) ->
	 groundness_analysis_match(Match, Goal, Clauses, Visited1),
	 write(' match '), pretty_program(Match), nl,
	 groundness_analysis_propagate(Match, Goal)
	 ,write(' result goal '), pretty(Goal), nl
	 ; true))),
    groundness_analysis_goals(Goals, Clauses, Visited1).

groundness_analysis_match([], _, _, _).
groundness_analysis_match([Head :- Body | Match], Goal, Clauses, Visited) :-
     Head = Goal,
     commas_to_list(Body, Goals),
     groundness_analysis_goals(Goals, Clauses, Visited),
     groundness_analysis_match(Match, Goal, Clauses, Visited).

groundness_analysis_propagate(Match, Goal) :-
     groundness_match_heads(Match, Heads),
     groundness_head_args(Heads, Args),
     Goal =.. [_Functor | GoalArgs],
%     write('check args'), nl, pretty(Args), nl, pretty(GoalArgs), nl,
     groundness_analysis_check_args(0, Args, GoalArgs).

groundness_analysis_all_ground([]).
groundness_analysis_all_ground([V|Vs]) :-
     V == ground, groundness_analysis_all_ground(Vs).

groundness_analysis_check_args(N, ArgArgs, [G | GArgs]) :-
    groundness_analysis_extract_arg(ArgArgs, N, Vs),
    (groundness_analysis_all_ground(Vs) -> G = ground ; true),
    N1 is N + 1,
    ArgArgs = [Args | _],
    length(Args, NLimit),
    (N1 < NLimit ->
     groundness_analysis_check_args(N1, ArgArgs, GArgs)
     ; GArgs = []).

groundness_analysis_extract_arg([], _, []).
groundness_analysis_extract_arg([Args|ArgArgs], N, [E|Extracted]) :-
    nth0(N, Args, E),
    groundness_analysis_extract_arg(ArgArgs, N, Extracted).

groundness_match_heads([], []).
groundness_match_heads([Head :- _ | Match], [Head | Heads]) :-
     groundness_match_heads(Match, Heads).

groundness_head_args([], []).
groundness_head_args([Head | Heads], [ArgList | Args]) :-
     Head =.. [_Functor | ArgList],
     groundness_head_args(Heads, Args).

item_closure(Item, Env, Closure) :-
    item_closure(Item, Env, [], Closure).

item_closure(Item, Env, ItemsIn, ItemsOut) :-
%    write('Closure: '), pretty(Item), nl,
    mycount(Cnt),
    (Cnt >= 50 -> xxx ; true),
    retract(mycount(Cnt)),
    Cnt1 is Cnt + 1,
    assert(mycount(Cnt1)),
    Env = env(_, Clauses),
    Item = item(_,_,Follow,GroundVars,_),
    (Follow = [First|_] ->
     first(Item,Clauses,Lookahead),
%     write('Lookahead: '), pretty(Item-Lookahead), nl,
     (get_clauses(First, Clauses, Match) ->
      itemize_clauses(Match, First, GroundVars, Lookahead, Items),
      item_closure_add(Items, Env, ItemsIn, NewItems)
    ; NewItems = []
     ),
     append(ItemsIn, NewItems, ItemsIn0),
     item_closure_list(NewItems, Env, ItemsIn0, ItemsOut0)
   ; ItemsOut0 = ItemsIn
    ),
    item_closure_add([Item], Env, ItemsOut0, NewItems0),
    append(NewItems0, ItemsOut0, ItemsOut).

item_closure_list([], _, Items, Items).
item_closure_list([Item|Items], Env, ItemsIn, ItemsOut) :-
    item_closure(Item, Env, ItemsIn, ItemsOut0),
    item_closure_list(Items, Env, ItemsOut0, ItemsOut).

item_closure_add([], _, _, []).
item_closure_add([Item|Items], Env, ItemsIn, NewItems) :-
    (member_streq(ItemsIn, Item) ->
     item_closure_add(Items, Env, ItemsIn, NewItems)
   ; NewItems = [Item | NewItems0],
     item_closure_add(Items, Env, ItemsIn, NewItems0)
    ).

itemize_clauses([], _, _, _, []).
itemize_clauses([Head :- Body | Clauses], First, GroundVars, Lookahead, [Item|Items]) :-
    commas_to_list(Body, Goals),
    copy_term(First-GroundVars-Lookahead, CopyFirst-CopyGroundVars-CopyLookahead),
    CopyFirst = Head,
    term_variables(Head, HeadVars),
    find_ground_vars(HeadVars, CopyGroundVars, NewGroundVars0),
    scan_ground_vars(Goals, NewGroundVars0, NewGroundVars1),
    sort(NewGroundVars1, NewGroundVars),
    prune_bindings(CopyLookahead, NewGroundVars, NewLookahead),
    Item = item(Head, [], Goals, NewGroundVars, NewLookahead),
    itemize_clauses(Clauses, First, GroundVars, Lookahead, Items).

find_ground_vars([],_,[]).
find_ground_vars([V|Vs],GroundVars,NVs) :-
%    (member_term(GroundVars, V) -> NVs = [V|NVs0] ; NVs = NVs0),
    (true -> NVs = [V|NVs0] ; NVs = NVs0),
    find_ground_vars(Vs,GroundVars,NVs0).

scan_ground_vars([], GroundVars, GroundVars).
scan_ground_vars([X = Y | Goals], GroundVars, NewGroundVars) :-
    !,
    (var(X) -> % , member_term(GroundVars, X) ->
     term_variables(Y, MoreGroundVars)
    ; var(Y) -> % , member_term(GroundVars, Y) ->
      term_variables(X, MoreGroundVars)
    ; MoreGroundVars = []
     ),
    append(GroundVars, MoreGroundVars, GroundVars0),
    scan_ground_vars(Goals, GroundVars0, NewGroundVars0),
    append(MoreGroundVars, NewGroundVars0, NewGroundVars).
scan_ground_vars([_ | Goals], GroundVars, NewGroundVars) :-
    scan_ground_vars(Goals, GroundVars, NewGroundVars).

% ------------------------------------------------------
%  member_streq(List, Term)
%
%  Look for Term in List using structurally_eq.
%

member_streq([T|List], Term) :-
    (structurally_eq(T, Term) -> true
    ;member_streq(List, Term)).

% ------------------------------------------------------
%  structurally_eq(+Term1,+Term2)
%
%  Take copies of Term1 and Term2. Bind each variable
%  to a unique ground term, identified by a counter.
%  If then these term are equal, then they are
%  structurally equal.
%
structurally_eq(Term1, Term2) :-
    term_structure(Term1, TermStructure1),
    term_structure(Term2, TermStructure2),
    TermStructure1 == TermStructure2.

% ------------------------------------------------------
%  term_structure(+Term, -GroundTerm)
%
%  Replace each var in Term with a unique atom.
%  The result is in GroundTerm.
%

term_structure(Term, GroundTerm) :-
    copy_term(Term, TermCopy),
    term_variables(TermCopy, TermVars),
    bind_all_vars_with_count(TermVars,0,'$var',_),
    GroundTerm = TermCopy.


repeated_vars(Term,Repeated) :-
    repeated_vars1(Term,[],_,[],Repeated).

repeated_vars1(Term,FoundIn,FoundOut,RepeatedIn,RepeatedOut) :-
    (var(Term) ->
     (member_term(FoundIn,Term) ->
        RepeatedOut = [Term|RepeatedIn],
        FoundOut = FoundIn
      ; RepeatedOut = RepeatedIn,
        FoundOut = [Term|FoundIn]
     )
     ; atom(Term) -> FoundOut = FoundIn, RepeatedOut = RepeatedIn
     ; Term =.. [_|Args],
       repeated_vars1_list(Args,FoundIn,FoundOut,RepeatedIn,RepeatedOut)
     ).

repeated_vars1_list([],Found,Found,Repeated,Repeated).
repeated_vars1_list([T|Ts],FoundIn,FoundOut,RepeatedIn,RepeatedOut) :-
     repeated_vars1(T,FoundIn,FoundOut0,RepeatedIn,RepeatedOut0),
     repeated_vars1_list(Ts,FoundOut0,FoundOut,RepeatedOut0,RepeatedOut).

bind_all_vars_with_count(Term,CountIn,Tag,CountOut) :-
    (var(Term) ->
     number_codes(CountIn, NumCodes),
     atom_codes(Tag, PrefixCodes),
     append(PrefixCodes,NumCodes,AtomCodes),
     atom_codes(Atom, AtomCodes),
     Term = Atom,
     CountOut is CountIn+1
    ;atom(Term) -> CountOut = CountIn
    ;Term =.. [_|Args],
     bind_all_vars_with_count_list(Args,CountIn,Tag,CountOut)
     ).

bind_all_vars_with_count_list([],Count,_,Count).
bind_all_vars_with_count_list([Term|Terms],CountIn,Tag,CountOut) :-
    bind_all_vars_with_count(Term,CountIn,Tag,CountOut0),
    bind_all_vars_with_count_list(Terms,CountOut0,Tag,CountOut).
    

% ------------------------------------------------------
%  first(+Item,+Clauses,-Bindings)
%
%  Execute all possible variants Goals using Clauses as
%  the program.
% -----------------------------------------------------
first(Item,Clauses,Lookahead) :-
    Item = item(Head,Seen,Follow,GroundVars,ItemLookahead),
    term_variables(Head-Seen-Follow, AllVars),
    subtract_vars(AllVars, GroundVars, ExtVars),
    append(ExtVars, [interpret_nr(Follow,Clauses,Constraints)], ExtList),
    list_to_ext(ExtList, Query),
    setof(GroundVars-Constraints, Query, Result),
    add_binding_vars(Result, GroundVars, Bindings0),
    flatten_bindings(Bindings0, Bindings1),
%    write('first: '), pretty(Bindings1), nl,
    filter_result(Bindings1, Bindings2),
    append(Bindings2, ItemLookahead, Bindings3),
    prune_bindings(Bindings3, GroundVars, Bindings4),
    prune_lookahead(Bindings4, GroundVars, Lookahead),
    write('first: '), pretty(Item-lookahead(Lookahead)), nl.


prune_bindings([], _, []).
prune_bindings([(V=T)-C | Bs], GroundVars, NewBs) :-
    (member_term(GroundVars, V) -> B = (V=T)-C, NewBs = [B|NewBs0]
    ;NewBs = NewBs0),
    prune_bindings(Bs, GroundVars, NewBs0).
prune_bindings([(V=T) | Bs], GroundVars, NewBs) :-
    (member_term(GroundVars, V) -> B = (V=T), NewBs = [B|NewBs0]
    ;NewBs = NewBs0),
    prune_bindings(Bs, GroundVars, NewBs0).

prune_lookahead(LookaheadIn, ProjectVars, LookaheadOut) :-
    copy_term(LookaheadIn-ProjectVars, CopyLookaheadIn-CopyProjectVars),
    bind_all_vars_with_count(CopyProjectVars,0,'$var',_),
    prune_lookahead1(LookaheadIn, CopyLookaheadIn, [], LookaheadOut).

prune_lookahead1([], [], _, []).
prune_lookahead1([Binding|Bindings], [CopyBinding|CopyBindings], Found,
		 LookaheadOut) :-
    bind_all_vars_with_count(CopyBinding,0,'$bind',_),
    (member(CopyBinding, Found) ->
     LookaheadOut = LookaheadOut0, Found0 = Found
    ; LookaheadOut = [Binding|LookaheadOut0], Found0 = [CopyBinding|Found]
    ),
    prune_lookahead1(Bindings, CopyBindings, Found0, LookaheadOut0).

bind_vars(Term, To) :-
    (var(Term) -> Term = To
    ;atom(Term) -> true
    ;Term =.. [_|Args],
     bind_vars_list(Args,To)
     ).

bind_vars_list([],_).
bind_vars_list([T|Ts],To) :-
    bind_vars(T, To),
    bind_vars_list(Ts,To).

unbind_vars(Term, Bound, Unbound, NewTerm) :-
    (atom(Term), nth0(Index, Bound, Term) ->
     nth0(Index, Unbound, NewTerm)
   ; Term =.. [Functor|Args],
     unbind_vars_list(Args,Bound,Unbound,NewArgs),
     NewTerm =.. [Functor|NewArgs]).

unbind_vars_list([], _, _, []).
unbind_vars_list([T|Ts], Bound, Unbound, [NT|NTs]) :-
    unbind_vars(T,Bound,Unbound,NT),
    unbind_vars_list(Ts, Bound, Unbound, NTs).
    

add_binding_vars([], _, []).
add_binding_vars([B-C|Bs], Vars, [Bounded|Bs1]) :-
    add_binding_vars1(B, C, Vars, Bounded),
    add_binding_vars(Bs, Vars, Bs1).

add_binding_vars1([], _, [], []).
add_binding_vars1([B|Bs], C, [V|Vs], [(V=B)-C | Bounded]) :-
    add_binding_vars1(Bs,C,Vs,Bounded).

flatten_bindings([], []).
flatten_bindings([Bindings|Rest], Flattened) :-
    append(Bindings, Flattened0, Flattened),
    flatten_bindings(Rest, Flattened0).

filter_result([], []).
filter_result([(V=Term)-C|List], Result) :-
    prune_term(Term, 1, NewTerm),
    term_variables(NewTerm, V1),
    prune_constraints(C, V1, C0),
    (C0 = [] ->
     (var(NewTerm) ->
       Result = Result0
     ; Result = [V=NewTerm|Result0]
     )
   ; Result = [V=NewTerm-C0|Result0]
    ),
    filter_result(List, Result0).

prune_constraints([], _, []).
prune_constraints([C|Cs], Vars, Constraints) :-
    term_variables(C, CVars),
    subtract_vars(CVars, Vars, Vars0),
    (CVars == Vars0 ->
     prune_constraints(Cs, Vars, Constraints)
   ; Constraints = [C|Constraints0],
     prune_constraints(Cs, Vars, Constraints0)
    ).

prune_term(Term, Depth, NewTerm) :-
    (var(Term) -> NewTerm = Term
   ; atom(Term) -> NewTerm = Term
   ; (Depth = 0 ->
      NewTerm = _
      ; % Non-zero depth
      Depth1 is Depth - 1,
      Term =.. [Functor|Args],
      prune_args(Args, Depth1, NewArgs),
      NewTerm =.. [Functor|NewArgs]
     )
    ).

prune_args([], _, []).
prune_args([Arg|Args], Depth, [NewArg|NewArgs]) :-
    prune_term(Arg, Depth, NewArg),
    prune_args(Args, Depth, NewArgs).

bind_all_vars([V|Vs],T) :- V = T, bind_all_vars(Vs,T).
bind_all_vars([],_).

unbind_term(Term,ToUnbind,NewTerm) :-
    !,
    (Term == ToUnbind -> true
     ;var(Term) -> NewTerm = Term
     ;Term =.. [Functor|Args],
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

interpret_nr(Goals,Clauses,Constraints) :-
    interpret_goals(Goals,Clauses,[],[],Constraints).

interpret_goals([],_Clauses, _Stack, Constraints, Constraints).
interpret_goals([Goal|Goals], Clauses, Stack, Cin, Cout) :-
%    write('Goal:'), nl, pretty(Goal), nl,
%    write('Stack:'), nl, write(Stack), nl,
    % If Goal already on stack, then skip it.
    (\+ member(Goal, Stack) ->
     interpret_goal(Goal, Clauses, Stack, Cin, Cout0),
     interpret_goals(Goals, Clauses, Stack, Cout0, Cout)
   ; interpret_goals(Goals, Clauses, Stack, Cin, Cout)).

interpret_goal(X = Y, _Clauses, _Stack, Constraints, Constraints) :-
    X = Y, !.
interpret_goal(Goal, Clauses, Stack, Cin, Cout) :-
    (get_clauses(Goal, Clauses, Match) ->
     member(Goal :- Body, Match),
     commas_to_list(Body, Goals),
     interpret_goals(Goals, Clauses, [Goal|Stack], Cin, Cout)
     % Goal could not be found. Assume it is external.
   ; append(Cin, [Goal], Cout)
    ).

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
    get_clauses0(Clauses, Goal, Match),
    Match = [_|_].

get_clauses0([], _Goal, []).
get_clauses0([(Head :- Body) | Clauses], Goal, [MatchGoal|Match]) :-
    \+ Head \= Goal, !,
    copy_term(Head:-Body, MatchGoal),
    get_clauses0(Clauses, Goal, Match).
get_clauses0([_Clause|Clauses], Goal, Match) :-
    get_clauses0(Clauses, Goal, Match).


get_clauses_nocopy(Goal, Clauses, Match) :-
    get_clauses_nocopy0(Clauses, Goal, Match),
    Match = [_|_].

get_clauses_nocopy0([], _Goal, []).
get_clauses_nocopy0([(Head :- Body) | Clauses], Goal, [MatchGoal|Match]) :-
    \+ Head \= Goal, !,
    MatchGoal = (Head :- Body),
    get_clauses_nocopy0(Clauses, Goal, Match).
get_clauses_nocopy0([_Clause|Clauses], Goal, Match) :-
    get_clauses_nocopy0(Clauses, Goal, Match).


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
     
