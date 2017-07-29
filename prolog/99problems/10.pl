
/* P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.

Example:
?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
*/

/* P9 "Pack" Solution */

pack([],[]).
pack([X|T], [Xs|Ts]) :- group(X, [X|T], Xs, Rs), pack(Rs, Ts).

group(X, [], [], YZ).
group(X, [Y|T], [], [Y|T]) :- X \= Y.
group(X, [X|T], [X|XX], YZ) :- group(X, T, XX, YZ).

/* Kludgy RLE */

encode([],[]).
encode(L,R) :- pack(L,Packed), compress(Packed, R).

compress([],[]).
compress([[Xs|Ts] | T], [[Len,Xs]| S]) :- length(Ts,Lm), Len is Lm+1, compress(T,S).


