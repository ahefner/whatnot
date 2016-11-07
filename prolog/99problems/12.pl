% P12 (**): Decode a run-length compressed list.

% decode(L1,L2) :- L2 is the uncompressed version of the run-length
%    encoded list L1.
%    (list,list) (+,?)

decode([],[]).
decode([X|S], [X|T]) :- atom(X), decode(S,T).
decode([[X,N]|T], [X|S]) :- N > 0, Np is N-1, decode([[X,Np]|T], S).
decode([[X,0]|T], S) :- decode(T,S).

