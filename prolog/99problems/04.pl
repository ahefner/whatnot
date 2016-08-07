/* P04 (*) Find the number of elements of a list. */

my_length([], 0).
my_length([_|T], N) :- my_length(T, Np), N is Np+1.

