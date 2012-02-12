pair(K,V,L) :- [K,V] = L.

cross_product([],[],[]).
cross_product([K_h|K_t],[V_h|V_t],[D_h|D_t]) :- 
  pair(K_h,V_h,D_h),
  cross_product(K_t,V_t,D_t).

all_keys([],[]).
all_keys([[K,_]|D_t],[K_h|K_t]) :-
  K = K_h, all_keys(D_t,K_t).

all_values([],[]).
all_values([[_,V]|D_t],[V_h|V_t]) :-
  V = V_h, all_values(D_t,V_t).

lookup(K,V,[[D_k,D_v]|_]) :- K = D_k, V = D_v.
lookup(K,V,[_|D_t]) :- lookup(K,V,D_t).
