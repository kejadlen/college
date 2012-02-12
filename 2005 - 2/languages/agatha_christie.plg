/*----------------------------------------------------------------------
 *  "Agatha Christie" Problem
 *  Who was the killer?
 *  
 *  Alice, her husband, son, daughter, 
 *  and brother are involved in a murder. 
 *  One of the five killed one of the other
 *  four.
 *  
 *  1. A man and a woman were together in the bar
 *     at the time of the murder. 
 *  2. The victim and the killer were together 
 *     on the beach at the time of the murder. 
 *  3. One of the children was alone at the 
 *     time of the murder. 
 *  4. Alice and her husband were not together 
 *     at the time of the murder. 
 *  5. The victim's twin was innocent. 
 *  6. The killer was younger than the victim.
 *----------------------------------------------------------------------*/

/*----- Facts -----*/
person(alice).
person(husband).
person(son).
person(daughter).
person(brother).

child(son).
child(daughter).

male(husband).
male(son).
male(brother).

female(alice).
female(daughter).

twin(alice,    brother).
twin(brother,  alice).
twin(son,      daughter).
twin(daughter, son).


/*----- Rules -----*/
istwin(X) :- twin(X, _).

older(alice,   son).
older(alice,   daughter).
older(husband, son).
older(husband, daughter).

inbar(M, N) :- person(M), person(N),
               male(M),   female(N).

together(S, T) :- S=alice, T=husband.
together(S, T) :- T=alice, S=husband.

alone(P) :- person(P), child(P).


/*----- Rule Combining Clues -----*/
solution(Killer, Victim, InBarA, InBarB, Alone) :-
    person(Killer), person(Victim),

    /* The victim's twin was innocent. */
        istwin(Victim), \+ twin(Victim, Killer),

    /* The killer was younger than the victim. */
	\+ older(Killer, Victim),
	    /* not the same as "older(Victim, Killer)"! */

    /* Alice and her husband were not together 
       at the time of the murder. */
        \+ together(Killer, Victim), Killer \= Victim,

    /* A man and a woman were together in the 
       at the time of the murder. */
        inbar(InBarA, InBarB),
	InBarA \= Killer, InBarB \= Killer,
	InBarA \= Victim, InBarB \= Victim,

    /* Alice and her husband were not together 
       at the time of the murder. */
        \+ together(InBarA, InBarB),

    /* One of the children was alone at the 
       time of the murder. */
	alone(Alone),
	Alone \= InBarA, Alone \= InBarB,
	Alone \= Killer, Alone \= Victim.


/*----- Goal -----*/
print_solution :-
    /* Find the solution */
        solution(Killer, Victim, InBarA, InBarB, Alone),
    /* Write solution */
	nl,
        write(Killer), write(' killed '), write(Victim), write('.'), nl,
	write(InBarA), write(' and '), write(InBarB),
	write(' were together in the bar.'), nl,
	write(Alone), write(' was alone.'), nl, nl.

?- print_solution.    /* Run it */
