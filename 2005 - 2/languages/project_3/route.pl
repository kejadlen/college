/* Handy operators. */
:- op(50,xfy,:).
:- op(100,xfx,is_after).
:- op(100,xfx,is_before).
:- op(100,xfx,plus_45_min_is).

/* Predicates for the last three operators. */
is_after(A:B,C:D) :-
  A*60+B > C*60+D.
is_before(A:B,C:D) :-
  A*60+B < C*60+D.
plus_45_min_is(A:B,C:D) :-
  A*60+B =< 1395,
  (
    ( B < 15, C = A, D = B+45 )
  ;
    ( B >= 15, C = A+1, D = B-15 )
  ).

one_hop(From,To,Departure,Arrival,Day,Route) :-
  route(Route,From,To,R_Departure,R_Arrival,DayList),

  /* Make sure the route runs on the given day. */
  member(Day,DayList),
  
  /* Departure time should be after the desired departure time. */
  (
    Departure = R_Departure
    ;
    \+ Departure = R_Departure,
    R_Departure is_after Departure,
    one_hop(From,To,R_Departure,Arrival,Day,Route)
  ),

  /* Arrival time should be before the desired arrival time. */
  (
    R_Arrival = Arrival
  ;
    \+ R_Arrival = Arrival,
    R_Arrival is_before Arrival,
    one_hop(From,To,Departure,R_Arrival,Day,Route)
  ).

/* plan_route/6 -- main plan_route function. */
plan_route(From,To,Departure,Arrival,Day,Route) :-
  plan_route(From,To,Departure,Arrival,Day,Route,[From]).

/* plan_route/7 -- helper to plan_route/6 */
plan_route(From,To,Departure,Arrival,Day,[L],Visited) :-
  one_hop(From,To,Departure,Arrival,Day,L),
  \+ member(To,Visited).
plan_route(From,To,Departure,Arrival,Day,[L|R],Visited) :-
  /* Find a linking route to the destination and get the arrival time. */
  one_hop(From,Link,Departure,_,Day,L),
  
  /* Don't create any cycles. */
  \+ member(Link,Visited),

  /* Make sure layovers are at least 45 minutes long. */
  route(L,_,_,_,L_Departure,_),
  L_Departure plus_45_min_is N_Departure,

  /* Plan the rest of the route. */
  plan_route(Link,To,N_Departure,Arrival,Day,R,[Link|Visited]).

route( lp1211, columbus, charlotte, 11:10, 12:20, [ mon, tue, wed, fri, sun ] ).
route( lp1212, charlotte, columbus, 13:20, 16:20, [ mon, tue, wed, fri, sun ] ).
route( lp1322, columbus, pittsburgh, 11:30, 12:40, [ tue, thu ] ).
route( lp1323, pittsburgh, columbus, 13:30, 14:40, [ tue, thu ] ).
route( lp1458, roanoke, charlotte, 09:10, 10:00, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1459, charlotte, roanoke, 11:00, 13:50, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1472, columbus, charlotte, 20:30, 21:30, [ mon, wed, thu, sat ] ).
route( lp1473, charlotte, columbus, 16:30, 19:30, [ mon, wed, thu, sat ] ).
route( lp1510, charlotte, roanoke, 08:30, 11:20, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1511, roanoke, charlotte, 12:20, 13:10, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1613, pittsburgh, charlotte, 09:00, 09:40, [ mon, tue, wed, thu, fri, sat ] ).
route( lp1614, charlotte, pittsburgh, 09:10, 11:45, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1620, pittsburgh, roanoke, 07:55, 08:45, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1621, roanoke, pittsburgh, 09:25, 10:15, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1623, roanoke, pittsburgh, 12:45, 13:35, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp1805, charlotte, pittsburgh, 14:45, 17:20, [ mon, tue, wed, thu, fri, sun ] ).
route( lp1806, pittsburgh, charlotte, 16:10, 16:55, [ mon, tue, wed, thu, fri, sun ] ).
route( lp4732, charlotte, raleigh, 09:40, 10:50, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp4733, raleigh, charlotte, 09:40, 10:50, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp4752, charlotte, raleigh, 11:40, 12:50, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp4773, raleigh, charlotte, 13:40, 14:50, [ mon, tue, wed, thu, fri, sat, sun ] ).
route( lp4822, charlotte, raleigh, 18:40, 19:50, [ mon, tue, wed, thu, fri ] ).
route( lp4833, raleigh, charlotte, 19:40, 20:50, [ mon, tue, wed, thu, fri ] ).
