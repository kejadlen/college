union([X|Y],Z,W) :- member(X,Z),  union(Y,Z,W).
union([X|Y],Z,[X|W]) :- \+ member(X,Z), union(Y,Z,W).
union([],Z,Z).

max_list([X|Y],Z) :- max(Y,X,Z).

max([],N,N).
max([A|B],C,D) :- A > C, max(B,A,D).
max([A|B],C,D) :- A =< C, max(B,C,D).

max_value([X],X).
max_vaule([A|B],X) :-
  max_value(B,X),
  A =< X.
max_value([A|B],X) :-
  max_value(B,A),
  A > X.

min_list([X|Y],Z) :- min(Y,X,Z).

min([],N,N).
min([A|B],C,D) :- A =< C, min(B,A,D).
min([A|B],C,D) :- A > C, min(B,C,D).

max_min(A,[B,C]) :- max_list(A,B), min_list(A,C).

last_list([],[]).
last_list([N],N).
last_list([_|A],B) :- last_list(A,B).
