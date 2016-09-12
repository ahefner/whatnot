
/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
*/

pack([],[]).
pack([X|T], [Xs|Ts]) :- group(X, [X|T], Xs, Rs), pack(Rs, Ts).

group(X, [], [], YZ).
group(X, [Y|T], [], [Y|T]) :- X \= Y.
group(X, [X|T], [X|XX], YZ) :- group(X, T, XX, YZ).

