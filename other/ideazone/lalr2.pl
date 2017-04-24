main :-
    (retract(mycount(_)) -> true ; true),
    assert(mycount(1)),
    read_grammar('expr.pl', Env, InitItem),
    InitState = state(1, [InitItem], [], []),
    StatesIn = [InitState],
    build_states(InitState, Env, StatesIn, StatesOut),
    print_states(StatesOut, 100).


main2 :-
    read_grammar('expr.pl', Env, _InitItem),
    InitItem = item(factor(A,B,C), [], [B = ['(' | U], expr(V,U,W), W = [')'|C], A=V], [], [B,C,U,W], []),
    Env = env(_, Clauses, _, _),
    first(InitItem, Clauses, Lookahead),
    pretty(InitItem-Lookahead), nl.

% ------------------------------------------------------
%  read_grammar(+File, -Env, -InitItem)
%
read_grammar(File, Env, InitItem) :-
    init_env(Env1, File),
    Env1 = env([InitItem|Items], Clauses, _, Focus),
    type_env(Clauses, Types),
    partition_clauses(Clauses, Types, Focus, Clauses1),
    pretty_program(Clauses1), nl,
    Env = env([InitItem|Items], Clauses1, Types, Focus).

% ------------------------------------------------------
%  partition_clauses(Clauses, Types, Focus, NewClauses)
%
%  Iterate over clauses and scan their bodies.
%  Partition the body into goals which does not
%  actively "drive" the parser. These become pure
%  actions.
%

partition_clauses([], _, _, []).
partition_clauses([(Head :- Body) | Clauses], Types, Focus, [NewClause | NewClauses]) :-
    commas_to_list(Body, Goals),
    partition_goals(Goals, Types, Focus, FilteredGoals, Actions),
    NewClause = (Head :- (FilteredGoals, Actions)),
    partition_clauses(Clauses, Types, Focus, NewClauses).

partition_goals([], _, _, [], []).
partition_goals([Goal | Goals], Types, Focus, FilteredGoals, Actions) :-
    (goal_is_action(Goal, Types, Focus)
       -> Actions = [Goal | Actions1], FilteredGoals1 = FilteredGoals
        ; Actions = Actions1, FilteredGoals = [Goal | FilteredGoals1]
     ), partition_goals(Goals, Types, Focus, FilteredGoals1, Actions1).
     
goal_is_action(A = B, Types, Focus) :-
    \+ var_has_type(Types, A, Focus),
    \+ var_has_type(Types, B, Focus), !.
goal_is_action(Goal, Types, Focus) :-
    term_variables(Goal, Vars),
    no_focus_vars(Vars, Types, Focus).

no_focus_vars([], _, _).
no_focus_vars([V|Vars], Types, Focus) :-
    \+ var_has_type(Types, V, Focus),
    no_focus_vars(Vars, Types, Focus).

% ------------------------------------------------------
% build_states(State, StatesIn, StatesOut)
%
build_states(State, Env, StatesIn, StatesOut) :-
    State = state(Label,KernelItems,_ClosureItems,Actions),
    % write('State '), write(Label), nl,
    % print_state(State),
    (Label > 500 -> xxx ; true),
    items_closure(KernelItems, Env, ItemClosure),
%    (Label == 1 -> write('ClosedState.'), nl, pretty(ItemClosure), nl ; true),
    ClosedState = state(Label,KernelItems,ItemClosure,Actions),
    state_replace(StatesIn, ClosedState, StatesOut0),
%    write('---- state is -----'), nl,
%    print_states(StatesOut0, 1), nl,
%    (Label < 3 -> write('hejsan-----'), nl, print_states(StatesOut0), write('klart-----'), nl ; true),
    iterate_transitions(ItemClosure, ClosedState, StatesOut0, StatesOut1, NewStates),
%    write('---- state is after iterate -----'), nl,
%    print_states(StatesOut0, 1), nl,
    build_states_list(NewStates, Env, StatesOut1, StatesOut).

build_states_list([], _, States, States).
build_states_list([State | States], Env, StatesIn, StatesOut) :-
    build_states(State, Env, StatesIn, StatesOut0),
    build_states_list(States, Env, StatesOut0, StatesOut).

% ------------------------------------------------------
%  iterate_transitions(+ItemClosure, +FromState, +StatesIn, -StatesOut, -NewStates)
%   Given an item closure and a from state, iterate through the
%   possible transitions and create new states + actions.
%

iterate_transitions([], _FromState, States, States, []) :- !.
iterate_transitions(ItemClosure,
		    state(FromLabel,FromKernelItems,FromClosureItems,FromActions),
		    StatesIn, StatesOut, NewStates) :-
    select_transition(ItemClosure, Next, NewItems, RestItems),
    nextify_token(Next, Nexted),
    (NewItems = [] -> StatesOut = StatesIn, NewStates = [] 
     ;
     (state_find(StatesIn, NewItems, state(ToLabel, _ToKernelItems, _ToClosureItems, _ToActions))
      -> append(FromActions, [shift(Nexted,ToLabel)], NewFromActions),
      state_replace(StatesIn, state(FromLabel, FromKernelItems, FromClosureItems, NewFromActions),
		    StatesOut0), NewStates0 = []
      ; length(StatesIn, N),
      N1 is N + 1,
      copy_term(NewItems, NewItemsCopy),
      NewState = state(N1, NewItemsCopy, [], []),
      append(FromActions, [shift(Nexted,N1)], NewFromActions),
      state_replace(StatesIn, state(FromLabel, FromKernelItems, FromClosureItems, NewFromActions),
		    StatesOut1),
      NewStates0 = [NewState],
      append(StatesOut1, NewStates0, StatesOut0)
     ),
     iterate_transitions(RestItems, state(FromLabel,FromKernelItems,FromClosureItems,NewFromActions), StatesOut0, StatesOut, NewStates1),
     append(NewStates0, NewStates1, NewStates)
     ).

% ------------------------------------------------------
%  nextify_token(+Next, -Nexted)
%
%  If Next is a goal (e.g. foo(A,B,C) we use the Nexted
%  token foo/3 to represent it, otherwise Nexted = Next.

nextify_token(Next, Nexted) :-
     (Next = (_ = _) -> Nexted = Next
    ; functor(Next,F,N),
      Nexted = F/N
      ).

% ------------------------------------------------------
%  select_transition(+ItemClosure, -Next, -NewItems, -RestItems)
%
%  Look at the next "token" of the item closure set
%  to determine the new kernel items. Also return the
%  rest of items.
%

select_transition([Item|ItemClosure], Next, NewItems, RestItems) :-
    Item = item(_Head, _Seen, [Next|_Follow], _Action, _Vars, _Lookahead),
    !,
    select_transition1([Item|ItemClosure], Next, NewItems0, RestItems),
    sort(NewItems0, NewItems).
select_transition([Item|ItemClosure], Next, NewItems, [Item|RestItems]) :-
    !, select_transition(ItemClosure, Next, NewItems, RestItems), !.
select_transition([], _, [], []).

select_transition1([], _, [], []).
select_transition1([Item|ItemClosure], Next, NewItems, RestItems) :-
    Item = item(Head, Seen, [Next1|NewFollow], Action, Vars, Lookahead),
    \+ Next \= Next1, !,
    append(Seen, [Next1], NewSeen),
    NewItems = [item(Head, NewSeen, NewFollow, Action, Vars, Lookahead) | NewItems0],
    select_transition1(ItemClosure, Next, NewItems0, RestItems).
select_transition1([Item|ItemClosure], Next, NewItems, [Item|RestItems]) :-
    select_transition1(ItemClosure, Next, NewItems, RestItems).

% ------------------------------------------------------
%  state_find(+States, +SearchKernelItems, -FoundLabel)
%  True if there is a state for SearchKernelItems. The
%  state label is returned in FoundLabel.
%

state_find([State | States],SearchKernelItems,FoundState) :-
    State = state(_Label, KernelItems, _ClosureItems, _Actions),
    (structurally_eq(KernelItems, SearchKernelItems) ->
     FoundState = State
   ; state_find(States,SearchKernelItems,FoundState)
    ).

% ------------------------------------------------------
%  state_get(+States, +Label, -FoundState)
%  Search through states with the specified label.
%  Return FoundState if found.
%
state_get([state(Label,KernelItems,ClosureItems,Actions)|_], Label, state(Label,KernelItems,ClosureItems,Actions)) :- !.
state_get([_ | States], Label, Found) :-
	   state_get(States, Label, Found).


% ------------------------------------------------------
%  state_replace(+States, +State, -NewStates)
%

state_replace([state(Label,_KernelItems,_ClosureItems,_Actions) | States],
	      state(Label,NewKernelItems,NewClosureItems,NewActions),
	      NewStates) :-
    !,
    NewStates = [state(Label,NewKernelItems,NewClosureItems,NewActions)|States].
state_replace([State | States], ReplaceState, [State | NewStates]) :-
	      state_replace(States, ReplaceState, NewStates).
state_replace([], ReplaceState, [ReplaceState]).


% ------------------------------------------------------
%  print_states(State)
%
print_states(States) :- print_states(States, 1000).

print_states([], _).
print_states([State | States], Lim) :-
    Lim > 0,
    copy_term(State, StateCopy),
    bind_all_vars_with_count(StateCopy,0,'$disp',_),
    print_state(StateCopy),
    Lim1 is Lim - 1,
    print_states(States, Lim1).

% ------------------------------------------------------
%  print_state(State)
%

print_state(state(Label,_KernelItems,ClosureItems,Actions)) :-
    write('--- State '), write(Label), write(' -------------------------'),nl,
%    print_items(KernelItems),
%    write('---closure items---'), nl,
    predsort(item_pred, ClosureItems, SortedItems),
    print_items(SortedItems),
%    write('---closure items done---'), nl,
    print_actions(Actions),
    nl.

%
% This will put (most) kernel items on top
%
item_pred(Delta, Item1, Item2) :-
      Item1 = item(_, Seen1, _, _, _, _),
      Item2 = item(_, Seen2, _, _, _, _),
      length(Seen1, N1),
      length(Seen2, N2),
      (N1 > N2 -> Delta = '<'
     ; N1 < N2 -> Delta = '>'
     ; compare(Delta, Item1, Item2)
       ).

print_actions([]).
print_actions([Action|Actions]) :-
    print_action(Action),
    print_actions(Actions).

print_action(shift(Next,Label)) :-
    write('    shift on '), pretty(Next), write(' and goto '), write(Label),nl.


print_items([]).
print_items([Item | Items]) :-
    print_item(Item), nl,
    print_items(Items).

print_item(item(Head, Seen, Follow, Action, _Vars, Lookahead)) :-
    append(Seen, ['(*)'], Goals1),
    append(Goals1, Follow, Goals2),
    append(Goals2, ['(+)' | Action], Goals3),
    append(Goals3, [lookahead(Lookahead)], Goals),
    list_to_commas(Goals, Body),
    pretty(Head :- Body), !.

% ------------------------------------------------------
%  init_env(?Env, +GrammarFile)
%  Setup Env by loading it with clauses from GrammarFile.
%

init_env(Env, GrammarFile) :-
    read_clauses(GrammarFile, Clauses),
    InitItem = item('$start', [],[start(X,T,T1)],[],[T,T1],[T1=eof]),
    Env = env([InitItem],Clauses,_,ts).

% ------------------------------------------------------
%  item_closure(+Item, +Env, -Closure)
%

items_closure(Items, Env, Closure) :-
    item_closure_list(Items, Env, [], Closure0),
    compact_closure(Closure0, Closure).

compact_closure([], []).
compact_closure([Item|Items], Compact) :-
    Item = item(Head,Seen,Follow,Action,Vars,Lookahead),
    Search = item(Head,Seen,Follow,Action,Vars1,Lookahead1),
    (select(Search, Items, Rest) ->
     % We found another matching item, so merge lookaheads
     MergedItem = item(Head,Seen,Follow,Action,MergedVars,MergedLookaheads),
     append(Vars,Vars1,MergedVars1),
     sort(MergedVars1, MergedVars),
     append(Lookahead,Lookahead1,MergedLookaheads1),
     prune_lookahead(MergedLookaheads1, MergedVars, MergedLookaheads2),
     sort(MergedLookaheads2, MergedLookaheads),
     compact_closure([MergedItem|Rest], Compact)
   ; Compact = [Item | Compact0],
     compact_closure(Items, Compact0)
     ).


head_list([], []).
head_list([item(Head,_,_,_,_,_)|Items], [Head|Heads]) :-
    head_list(Items, Heads).

item_closure(Item, Env, ItemsIn, ItemsOut) :-
    Item = item(_,_,Follow,_,_,_),
%    head_list(ItemsIn, Heads),
%    write('Closure: '), pretty(Head), write(' '), pretty(Heads), nl,
    mycount(Cnt),
%    (Cnt >= 50 -> xxx ; true),
    retract(mycount(Cnt)),
    Cnt1 is Cnt + 1,
    assert(mycount(Cnt1)),
    Env = env(_, Clauses, _, Focus),
    Item = item(_,_,Follow,_,_,_),
    (Follow = [First|_] ->
     first(Item,Clauses,Lookahead),
%     write('Lookahead: '), pretty(Item-Lookahead), nl,
     (get_clauses_with_types(First, Env, Match, Types) ->
      remove_recursive_matches(Match, ItemsIn, Match1),
%      write('----- matching -----'), nl,
%      pretty(Match1), nl,
      itemize_clauses(Match1, Types, Focus, First, Lookahead, Items),
      item_closure_add(Items, Env, ItemsIn, NewItems)
    ; NewItems = []
     ),
     append(ItemsIn, NewItems, ItemsIn0),
     item_closure_list(NewItems, Env, ItemsIn0, ItemsOut0)
   ; ItemsOut0 = ItemsIn
    ),
    item_closure_add([Item], Env, ItemsOut0, NewItems0),
    append(ItemsOut0, NewItems0, ItemsOut).
%    write('item closure done: '), pretty(Head), nl,
%    (Head == '$start' -> pretty(ItemsOut), nl ; true).

has_matching_head([item(Head, _Seen, _Goals, _Action, _Vars, _LookaHead) | _],
		  Head1 :- _Body) :-
    \+ Head \= Head1.
has_matching_head([_ | Items], Clause) :-
    has_matching_head(Items, Clause).

remove_recursive_matches([], _, []).
remove_recursive_matches([Match | Matches], ItemsIn, Matches1) :-
    (has_matching_head(ItemsIn, Match) ->
%     write('Remove recursive match '), pretty(Match), nl,
     remove_recursive_matches(Matches, ItemsIn, Matches1)
   ; Matches1 = [Match | Matches0],
     remove_recursive_matches(Matches, ItemsIn, Matches0)
     ).

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

itemize_clauses([], _, _, _, _, []).
itemize_clauses([Head :- (Goals,Actions) | Clauses], Types, Focus, First, Lookahead,
		[Item|Items]) :-
    First = Head,
    term_variables(First-Goals, AllVars),
    find_focus_vars(AllVars, Types, Focus, FocusVars),
    sort(FocusVars, FocusVars1),
%    write('prune '), pretty(Lookahead-AllVars1), nl,
    prune_bindings(Lookahead, FocusVars1, NewLookahead1),
    sort(NewLookahead1, NewLookahead),
    Item = item(Head, [], Goals, Actions, FocusVars1, NewLookahead),
    itemize_clauses(Clauses, Types, Focus, First, Lookahead, Items).


%itemize_clauses([Head :- Body | Clauses], Types, Focus, First, Lookahead, [Item|Items]) :-
%    commas_to_list(Body, Goals),
%    copy_term(First-Lookahead, CopyFirst-CopyLookahead),
%    CopyFirst = Head,
%    term_variables(CopyFirst-Body, AllVars),
%    find_focus_vars(AllVars, Types, Focus, FocusVars),
%    sort(FocusVars, FocusVars1),
%%    write('prune '), pretty(CopyLookahead-AllVars1), nl,
%    prune_bindings(CopyLookahead, FocusVars1, NewLookahead),
%    Item = item(Head, [], Goals, FocusVars1, NewLookahead),
%    itemize_clauses(Clauses, Types, Focus, First, Lookahead, Items).


var_has_type([V1=T1|Types],V,T) :-
    (V == V1 -> T = T1
     ; var_has_type(Types,V,T)).

find_focus_vars([], _, _, []).
find_focus_vars([V|Vs], Types, Focus, FocusVars) :-
    (var_has_type(Types, V, Focus) ->
     FocusVars = [V | FocusVars0],
     find_focus_vars(Vs, Types, Focus, FocusVars0)
   ; find_focus_vars(Vs, Types, Focus, FocusVars)
     ).

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
    Item = item(Head,Seen,Follow,Action,Vars,ItemLookahead),
    term_variables(Head-Seen-Follow-Action, AllVars),
    subtract_vars(AllVars, Vars, ExtVars),
    append(ExtVars, [interpret_nr(Follow,Clauses,Constraints)], ExtList),
    list_to_ext(ExtList, Query),
    setof(Vars-Constraints, Query, Result),
    add_binding_vars(Result, Vars, Bindings0),
    flatten_bindings(Bindings0, Bindings1),
%    write('first: '), pretty(Bindings1), nl,
    filter_result(Bindings1, Bindings2),
    append(Bindings2, ItemLookahead, Bindings3),
    prune_bindings(Bindings3, Vars, Bindings4),
    prune_lookahead(Bindings4, Vars, Lookahead).
%    write('first: '), pretty(Item-lookahead(Lookahead)), nl.


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
     member((Goal :- (Goals,_Actions)), Match),
     interpret_goals(Goals, Clauses, [Goal|Stack], Cin, Cout)
     % Goal could not be found. Assume it is external.
   ; append(Cin, [Goal], Cout)
    ).

% ------------------------------------------------
%  Type analysis. Identify the type category of
%  each variable for a program given a start goal.
%  The analysis is monovariant and the aim is to
%  identify which variables are related to the
%  token stream and which are related to the
%  parse tree.
% ------------------------------------------------
type_analyze(Clauses, Goal) :-
    (Goal = (_ = _) ->
     type_analyze_unification(Goal)
    ; get_clauses_nocopy(Goal, Clauses, Match) ->
      type_analyze_goal(Clauses, Goal, Match)
    ; % External goal, skipped
      true
    ).

type_analyze_unification(A = B) :-
    ((var(A) ; var(B)) -> A = B
     ; true).

type_analyze_goal(Clauses, Goal, Match) :-
    [Head :- _Body | _] = Match,
    (Goal == Head -> true
   ; unify_heads(Match, Goal),
    type_analyze_match(Match, Clauses)
    ).

type_analyze_match([], _).
type_analyze_match([_ :- Body | Match], Clauses) :-
    commas_to_list(Body, Goals),
    type_analyze_goals(Goals, Clauses),
    type_analyze_match(Match, Clauses).

type_analyze_goals([], _).
type_analyze_goals([Goal | Goals], Clauses) :-
    type_analyze(Clauses, Goal),
    type_analyze_goals(Goals, Clauses).

unify_heads([], _).
unify_heads([Head :- _ | Clauses], Goal) :-
    Head = Goal,
    unify_heads(Clauses, Goal).

type_env(Clauses, Types) :-
    term_variables(Clauses, ClauseVars),
    copy_term(Clauses, Analyzed),
    term_variables(Analyzed, AnalyzedVars),
    type_analyze(Analyzed, start(ps,ts,ts)),
    zip_type_env(ClauseVars, AnalyzedVars, Types0),
    simplify_types(Types0, Types).

zip_type_env([], [], []).
zip_type_env([V|Vs], [A|As], [V=A | Types]) :-
    zip_type_env(Vs,As, Types).

simplify_types([], []).
simplify_types([V=[_|ts] | Types0], [V = ts | Types1]) :-
    !, simplify_types(Types0, Types1).
simplify_types([Type | Types0], [Type | Types1]) :-
    simplify_types(Types0, Types1).

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


get_clauses_with_types(Goal, Env, Match, Types) :-
    Env = env(_, Clauses, _, _),
    get_clauses_with_types0(Clauses, Env, Goal, Match, Types),
    Match = [_|_].

get_clauses_with_types0([], _Env, _Goal, [], []).
get_clauses_with_types0([(Head :- Body) | Clauses],
			Env, Goal, [MatchGoal|Match], Types) :-
    \+ Head \= Goal, !,
    term_variables((Head:-Body), ClauseVars),
    lookup_type_env_vars(ClauseVars, Env, Types0),
    copy_term((Head:-Body)-Types0, MatchGoal-Types1),
    get_clauses_with_types0(Clauses, Env, Goal, Match, Types2),
    append(Types1, Types2, Types).

get_clauses_with_types0([_Clause|Clauses], Env, Goal, Match, Types) :-
    get_clauses_with_types0(Clauses, Env, Goal, Match, Types).

lookup_type_env_vars([], _, []).
lookup_type_env_vars([V|Vs], Env, [V=T | Types]) :-
    lookup_type_env_var(V, Env, T),
    lookup_type_env_vars(Vs, Env, Types).

lookup_type_env_var(V, Env, T) :-
    Env = env(_,_,Types,_),
    lookup_type_env_var1(Types, V, T).

lookup_type_env_var1([(V=T) | _], V1, T) :-
    V == V1, !.
lookup_type_env_var1([_| Types], V1, T) :-
    lookup_type_env_var1(Types, V1, T).
    

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

pretty1_leaf(Term, Env, Env, Term1, _) :-
    (atom(Term) ->
     atom_codes(Term, Codes),
     (Codes = [36, 100, 105, 115,112 | R] ->
       number_codes(N, R),
       get_name(N, Name),
       Term1 = Name
       ; Term1 = Term
      ) ; Term1 = Term).

get_name(0,'A') :- !.
get_name(N,Name) :-
    get_name1(N,Xs), reverse(Xs,Ys), atom_to_chars(Name,Ys).
get_name1(N,Xs) :-
    (N > 0 ->
     X is 65+mod(N,26), N1 is div(N,26), get_name1(N1,Xs1), Xs = [X|Xs1]
    ;Xs = []).
     
