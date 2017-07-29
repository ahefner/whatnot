
factorial(0,1).

factorial(N,F) :-
    N>0,
    Nprev is N-1,
    factorial(Nprev,Fprev),
    F is N * Fprev.

