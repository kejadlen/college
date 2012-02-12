value(O,_,N) :-
  integer(O), O = N.
value(a,[N,_,_,_,_],N).
value(b,[_,N,_,_,_],N).
value(c,[_,_,N,_,_],N).
value(d,[_,_,_,N,_],N).
value(e,[_,_,_,_,N],N).

all_ints([N]) :- integer(N).
all_ints([H|T]) :- integer(H), all_ints(T).

/* split(Before,Splitter,After,List) */
split([],L_h,L_t,[L_h|L_t]).
split([H|B_t],S,A,[H|L_t]) :-
  H \= S,
  split(B_t,S,A,L_t).

/* <statement> --> <assignment_stmt> */
statement([H|T],Input,Output) :-
  T = [:=|E], 
  assignment(H,E,Input,Output).
/* <statement> --> <block_stmt> */
statement([H|T],Input,Output) :-
/*  list(H),*/
  statement(H,Input,Temp),
  interpret_block(T,Temp,Output).
/* <statement> --> <if_stmt> */
statement([if|T],Input,Output) :-
  split(Expr,then,Rest,T),
  length(Rest,3),
  split(Then,else,Else,Rest),
  expression(Expr,Input,N),
  (
    (
      N > 0,
      statement(Then,Input,Output),
      statement(Else,Input,_)
    )
  ;
    (
      N =< 0,
      statement(Then,Input,_),
      statement(Else,Input,Output)
    )
  ).
/* <statement> --> <while_stmt> */
statement([while|T],Input,Output) :-
  split(Expr,do,Statement,T),
  length(Statement,1),
  expression(Expr,Input,N),
  (
    (
      N > 0,
      statement(Statement,Input,Temp),
      statement([while|T],Temp,Output)
    )
    ;
    (
      N =< 0,
      statement(Statement,Input,_),
      Output = Input
    )
  ).
/* <statement> --> \[ \] */
statement([],N,N).

/* <assignment_stmt> --> \[ <variable>, :=, <expression> \] */
assignment(a,Expr,[A,B,C,D,E],[N,B,C,D,E]) :-
  expression(Expr,[A,B,C,D,E],N).
assignment(b,Expr,[A,B,C,D,E],[A,N,C,D,E]) :-
  expression(Expr,[A,B,C,D,E],N).
assignment(c,Expr,[A,B,C,D,E],[A,B,N,D,E]) :-
  expression(Expr,[A,B,C,D,E],N).
assignment(d,Expr,[A,B,C,D,E],[A,B,C,N,E]) :-
  expression(Expr,[A,B,C,D,E],N).
assignment(e,Expr,[A,B,C,D,E],[A,B,C,D,N]) :-
  expression(Expr,[A,B,C,D,E],N).

/* <expression> --> <operand> {, ( + | - ), <operand> } */
expression([H|T],Input,N) :-
  value(H,Input,V),
  expression_help(T,Input,V,N).
expression_help([],_,N,N).
expression_help([Operator,Operand|T],Input,Sum,N) :-
  value(Operand,Input,Value),
  (
    (
      Operator = +,
      New_Sum is Sum + Value
    )
  ;
    (
      Operator = -,
      New_Sum is Sum - Value
    )
  ),
  expression_help(T,Input,New_Sum,N).

/* <block_stmt> --> \[ \] */
interpret_block([],N,N).
/* <block_stmt> --> \[ <statement> {, <statement> } \] */
interpret_block([H|T],Input,Output) :-
  statement(H,Input,Temp),
  interpret_block(T,Temp,Output).

/* <program> --> <block_stmt> */
interpret_OSIL3(Block,Input,Output) :-
  length(Input,5),
  all_ints(Input),
  interpret_block(Block,Input,Output).
