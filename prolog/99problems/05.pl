/* P05 (*) Reverse a list. */

rev(X,Y) :- rev(X,[],Y).

rev([],Y,Y).

rev([X|T], Y, Z) :- rev(T, [X|Y], Z).





