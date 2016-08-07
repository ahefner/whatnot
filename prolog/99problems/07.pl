/* P07 (**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:
?- my_flatten([a, [b, [c, d], e]], X).
X = [a, b, c, d, e]

Hint: Use the predefined predicates is_list/1 and append/3
*/

my_flatten([], []).

my_flatten([X|T], Y) :-
    is_list(X),
    my_flatten(X, Xf),
    my_flatten(T, Tf),
    append(Xf, Tf, Y).

my_flatten([X|T], Y) :-
    not(is_list(X)),
    my_flatten(T, Tf),
    append([X],Tf,Y).
