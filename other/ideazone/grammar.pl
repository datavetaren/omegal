start(X,T,T1) :- listA(As,AT,T,T2),
                 listB(Bs,BT,T2,T1),
                 AT = Bs, BT = [], X = prog(As).

listA(As,AT,T,T1) :- T = [a|T2], listA(As1,AT,T2,T1), As = [a | As1].
listA(As,AT,T,T1) :- T = T1, As = AT.

listB(Bs,BT,T,T1) :- T = [b|T2], listB(Bs1,BT,T2,T1), Bs = [b | Bs1].
listB(Bs,BT,T,T1) :- T = T1, Bs = BT.
