/*
(Q1) Design a naive sorting algorithm which make use
  of two predicates, permute(Xs,Ys), where Ys is a permutation
  of Xs; and ordered(Xs) which ensures that Xs is a sorted
  list of integers. This solution to sorting is
  simple but naive since it has exponential complexity to the size of
  the input list.

permute::(List[Int],List[Int]) -> Bool
ordered::(List[Int]) -> Bool
sorted::(List[Int],List[Int]) -> Bool
   
*/
sorted(Xs,Ys) :- permute(Xs, Ys), ordered(Ys).

permute([],[]).
permute(List, [H|Permutation]):- delete(H, List, Rest), permute(Rest, Permutation).

delete(X, [X|T], T).
delete(X, [H|T], [H|NT]):- delete(X,T,NT).

ordered([]).  /* for empty cases */
ordered([_]).
ordered([X,Y|T]):- X=<Y, ordered([Y|T]).


/*
(Q2)

The Dutch national flag problem is to take a list of elements that are
either red, white, or blue and return a list with all red elements first, followed by all
white elements, and then all blue elements last (the order in which they appear on the
Dutch national flag). We represent the property of being red, white, or blue with
three predicates, red(x), white(x), and blue(x). You may assume that every
element of the input list satisfies exactly one of these three predicates.
An append predicate is provided for your convenience.

append::(List[Int],List[Int],List[Int]) -> Bool
white ::(Int) -> Bool
dutch_flag::(xs:List[Int],rs:List[Int]) -> Bool
  
*/

append([],Y,Y).
append([X|Xs],Y,[X|Rs]):-append(Xs,Y,Rs).
red(1).
white(2).
blue(3).

appendRed(Xs,Ys):- include(red, Xs, Ys).
appendWhite(Xs,Ys):- include(white, Xs, Ys). 
appendBlue(Xs,Ys):- include(blue, Xs, Ys).
appendAll(A,B,C,D):- append(A,B,AB), append(AB,C,D).

dutch(Xs,Ys):- appendRed(Xs,A),
               appendWhite(Xs,B),
               appendBlue(Xs,C),
               appendAll(A,B,C,Ys).

/*
  (Q3) Maze problem
  
  Consider a maze problem that is being constructed from three
  predicates, entry(P) to denote an entry point P of the maze,
  exit(P) to denote an exit point of the maze, and next(x,y) to denote
  an immediate path from point X to point Y.

  (i) Write a solution
    mazepath :: (X:Point,Y:Point,P:List[Point]) -> Bool
  which will find all paths P from point X to point Y.
  Your initial solution may assume that the maze is acyclic,

  (ii) Write a better solution that will ensure that a finite path
  is returned even if there are cycles in the graph.
    mazepath2 :: (Point,PointList[Point]) -> Bool

*/

entry(a).
exit(e).
exit(f).
next(a,b).
next(b,c).
next(b,a). 
next(b,d).
next(c,e).
next(d,f).

move(X, X, P, P).
move(X, Y, V, P):- next(X,Z), move(Z,Y,[Z|V],P).

move2(X, X, P, P).
move2(X, Y, V, P):- next(X,Z), \+ member(Z, V), move2(Z,Y,[Z|V],P).

mazepath(X ,Y, Rs):- move(X, Y, [], Rs).
mazepath2(X ,Y, Rs):- move2(X, Y, [], Rs).



/*
(Q4)
  How Old is Granny?
  
Tom asked his Granny how old she was. Rather than giving him a straight answer, she replied:

"I have 6 children, and there are 4 years between each one and the next. 
I had my first child (your Uncle Peter) when I was 19. Now the youngest one (Your Auntie Jane) 
is 19 herself. That's all I'm telling you!"

granny_age::(Age:Int,Peter_age:Int,Jane_age:Int) -> Bool

Generalise your solution to
  granny_age_N::(Age:Int,Peter_age:Int,Jane_age:Int,N:Int)
where N is the number of children that granny has, and where Peter_age is
age of the oldest child, while Jane_age is the age of the youngest child.
*/

:- use_module(library(clpfd)).

granny_age(Age,Peter_age,Jane_age) :-
        Age - Peter_age #= 19,
        Peter_age - Jane_age #= 5*4,
        Jane_age #= 19.

granny_with_N(Age,Peter_age,Jane_age,N) :-
        Age - Peter_age #= 19,
        Peter_age - Jane_age #= (N-1) * 4,
        Jane_age #= 19.

/* 

A kid goes into a grocery store and buys four items. The cashier charges $7.11. 
The kid pays and is about to leave when the cashier calls the kid back, and says 
"Hold on, I multiplied the four items instead of adding them; I'll try again... 
Gosh, with adding them the price still comes to $7.11"! What were the prices of 
the four items?
   
grocery::(Items:List[Int]) -> Bool
*/

grocery(Vars):-
	Vars=[A,B,C,D],
        Vars ins 1..711,
        A+B+C+D #= 711,
        A*B*C*D #= 711000000,
	label(Vars).





