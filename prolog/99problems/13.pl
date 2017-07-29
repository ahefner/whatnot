

encode_modified([],[]).
encode_modified([X], [[X,1]]).
encode_modified([X,Y|T], [X|S]) :- X \= Y, encode_modified([Y|T],S).
encode_modified([X,X|T], [[X,N]|S]) :- encode_modified([X|T],[[X,Np]|S]), N is Np+1.
encode_modified([X,X|T], [[X,2]|S]) :- encode_modified([X|T],[X|S]).
