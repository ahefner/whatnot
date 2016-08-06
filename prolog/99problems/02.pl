/* P02 (*) Find the last but one element of a list. */

last_but_one(X, [X, _]).
last_but_one(X, [_, Y | R]) :- last_but_one(X, [Y|R]).

