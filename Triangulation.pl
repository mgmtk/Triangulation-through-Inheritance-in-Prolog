% mod11pa.pl -- CS 430 Spring 2018 Module 12 PA.
%
% Author: Mitchell Mesecher
%

%
% concatenate(L1, L1, T) is true if and only if T is equal to the concatenation
% of lists L1 and L2.
%
concatenate([],X,X). %recursive concatenation
concatenate([X|L1],L2,[X|L3]):- concatenate(L1,L2,L3). 

%
% singletons(P, Q) is true if and only if Q is equivalent to the list obtained
% from P if each item in P is wrapped in "[" and "]" to create a singleton list.
%
% For example, singletons([1,2,3], [[1],[2],[3]]) is true.
%

singletons([],[]). %break P into singletons 
singletons([H|T],[[H]|ET]) :- singletons(T,ET).


%
% prefix_all(I, P, Q) is true if and only if P is a list of lists and Q is the
% list obtained by prepending I to each element in P.
%
% For example, prefix_all(1, [[2,3],[4,5]], [[1,2,3],[1,4,5]]) is true.
%

prefix_all(_,[],[]). %append I to lists in P
prefix_all(I,[H|T],[[I|H]|ET]) :- prefix_all(I,T,ET).

%
% pairs_all(I, P, Q) is true if and only if Q is the list obtained by pairing I
% with each item in P.
%
% For example pairs_all(3, [4,5], [[3,4],[3,5]]) is true.
%
% HINT: Use the 'singletons' and 'prefix_all' predicates!
%

pairs_all(_,[],[]). %append I to all singletons made from list P
pairs_all(I,P,Q) :- singletons(P, T), prefix_all(I, T, Q).


%
% ccw(T, U) is true if and only if T and U are triples containing the same
% points and U is in counter-clockwise orientation.
%
% Specifically, if T = [P1, P2, P3] is already counter-clockwise this predicate
% should be true when U = [P1, P2, P3]. Otherwise, the predicate should be true
% when U = [P1, P3, P2].
%

%check T is equal to U if T is ccw, otherwise check T is equal to U with points switched
ccw([X,Y,Z], [X1,Y1,Z1]) :- is_ccw([X,Y,Z]), is_same_point(X,X1), is_same_point(Y,Y1), is_same_point(Z,Z1).
ccw([X,Y,Z], [X1,Y1,Z1]) :- \+is_ccw([X,Y,Z]), is_same_point(X,X1), is_same_point(Y,Z1), is_same_point(Z,Y1).


%
% ccw_triples(P, Q) is true if and only if Q is the list containing all the
% triples of points in the list P except arranged in ccw orientation.
%


ccw_triples([],[]). %make the head ccw and recursively transform the tail
ccw_triples([H|T], [X|Y]) :- ccw(H,X), ccw_triples(T,Y).



%
% pairs_of_points([H|T], Q) is true if and only if Q is a list containing all of
% the distinct pairs that can be made from the points in the list of points
% [H|T].
%
% HINT: Q is the concatenation of 1) all the pairs made from H and items in T
% and 2) all the pairs made from T.
%

pairs_of_points([],[]). %generate pairs of H and T and concatenate to the tail pairs
pairs_of_points([H|T], Q) :- pairs_all(H, T, X),  pairs_of_points(T, Y), concatenate(X, Y, Q).

%
% triples_of_points([H|T], Q) is true if and only if Q is a list containing all
% of the distinct triples that can be made from the points in the list of points
% [H|T].
%
% HINT: Q is the concatenation of 1) the list obtained by adding H as a prefix
% to all pairs of points from T and 2) all the triples of points in T.
%

triples_of_points([],[]). %generate pairs with H appended then concatenate the recursive tail
triples_of_points([H|T], Q) :- pairs_of_points(T,Z), prefix_all(H, Z, X), triples_of_points(T,Y), concatenate(X,Y,Q).

%
% is_delaunay_triangle(T, P) is true if and only if no point of the point set P
% is in the circle defined by the triple T (which here you may assume is in CCW
% orientation).  This predicate is undefined if P is empty.
%

is_delaunay_triangle(_,[]). %check all H is not in the circle of X
is_delaunay_triangle(X, [H|T]) :- \+is_in_circle(H, X), is_delaunay_triangle(X, T).

%
% delaunay_triangles(T, P, X) is true if and only if X is the subset of
% triangles from T that are Delaunay triangles for the point set P.
%
% HINT: Define this recursively on the list of triangles T.
%

delaunay_triangles([], _, []). %check for delaunay_triangles in P
delaunay_triangles([H|T], P, [H|X]) :- is_delaunay_triangle(H, P), delaunay_triangles(T, P, X).
delaunay_triangles([H|T], P, X) :- \+is_delaunay_triangle(H, P),  delaunay_triangles(T, P, X).
%
% delaunay_triangulation(P, X) is true if and only if X is the list of Delaunay
% triangles for the point list P.
%
% HINT: Create temporary variables to describe all triples from P as well as all
% CCW triples from P. Use the predicates you've already defined above!
%

delaunay_triangulation([],[]). %convert all triples of p to ccw and check if they are delaunay_triangles
delaunay_triangulation(P, X) :- triples_of_points(P,T), ccw_triples(T, Y), delaunay_triangles(Y,P,X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                  PROVIDED PREDICATES -- DO NOT EDIT!                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% det2(A, B, C, D, X) is true when X is the determinant of the matrix
% [[A, B], [C, D]].
det2(A, B, C, D, X) :- X is A*D - B*C.

% det3(A, B, C, D, E, F, G, H, I, X) is true when X is the determinant of the
% matrix [[A, B, C], [D, E, F], [G, H, I]].
det3(A, B, C, D, E, F, G, H, I, X) :- det2(E, F, H, I, X1),
                                      det2(D, F, G, I, X2),
                                      det2(D, E, G, H, X3),
                                      X is A * X1 - B * X2 + C * X3.

% det3(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, X) is true when X is the
% determinant of the matrix [[A, B, C], [D, E, F], [G, H, I]].
det4(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, X) :-
  det3(F, G, H, J, K, L, N, O, P, X1),
  det3(E, G, H, I, K, L, M, O, P, X2),
  det3(E, F, H, I, J, L, M, N, P, X3),
  det3(E, F, G, I, J, K, M, N, O, X4),
  X is A * X1 - B * X2 + C * X3 - D * X4.

% Predicate for checking whether two points are equal.
is_same_point([X,Y],[X1,Y1]) :- X = X1, Y = Y1.

% Predicate to test if three points are in counter-clockwise orientation.
is_ccw([[X1,Y1],[X2,Y2],[X3,Y3]]) :- (X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1) > 0.

% Checks if point [X, Y] is in the circle through points
% [[X1,Y1],[X2,Y2],[X3,Y3]].
is_in_circle([X,Y],[[X1,Y1],[X2,Y2],[X3,Y3]]) :-
  \+ is_same_point([X,Y],[X1,Y1]),
  \+ is_same_point([X,Y],[X2,Y2]),
  \+ is_same_point([X,Y],[X3,Y3]),
  Sqd1 is X1*X1 + Y1*Y1,
  Sqd2 is X2*X2 + Y2*Y2,
  Sqd3 is X3*X3 + Y3*Y3,
  Sqd is X*X + Y*Y,
  det4(X1, Y1, Sqd1, 1, X2, Y2, Sqd2, 1, X3, Y3, Sqd3, 1, X, Y, Sqd, 1, D),
  D > 0.

