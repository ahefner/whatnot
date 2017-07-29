/* 99 Problems page did this with a simpler approach: */

my_flatten(X, [X]) :- not(is_list(X)).
my_flatten([],[]).
my_flatten([X|T], Z) :-
    my_flatten(X, Xf),
    my_flatten(T, Tf),
    append(Xf, Tf, Z).
