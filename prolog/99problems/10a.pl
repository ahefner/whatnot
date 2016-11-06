/* Attempt at cleaner encode without using pack function */

encode([],[]).
encode([X], [[X,1]]).
encode([X,Y|T], [[X,1]|S]) :- X \= Y, encode([Y|T],S).
encode([X,X|T], [[X,N]|S]) :- encode([X|T],[[X,Np]|S]), N is Np+1.
