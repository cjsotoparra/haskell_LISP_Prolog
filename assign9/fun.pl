read_animal(X) :-
  write('please type animal name:'),
 % nl,
  read(X).


animal(alligator).
animal(tortue).
animal(caribou).
animal(ours).
animal(cheval).
animal(vache).
animal(lapin).


%2.) convert to string
%1.) find two animals
%3.) suffix of animal A1 = same as the prefix of animal A2
%4.) combine strings 
%5.) write it out
%6.) repeat /fail

:- dynamic good/2.
:- dynamic bad/2.

mutate(X):- 
   animal(A), animal(B), A\==B,
   name(A,As),name(B,Bs),
   append(_,Y,As), Y\==[],
   append(Y,Z,Bs),
   append(As,Z,C),
   name(X,C).

mutate:-
    mutate(X),
    write(X),
    nl,
    fail,
    mutate.

%if two lists don't have any elements in common return true, otherwise false

joint(X,Y) :- member(A,X), member(A,Y).

disjoint(X,Y):-
     not((member(Z,X), member(Z,Y))).
  
delete_one(X,[X|Z],Z).
delete_one(X,[V|Z], [V|Y]) :-
            X \== Y,
            delete_one(X,Z,Y).

perm(X, [Z|V]) :-
      delete_one(Z,X,Y),
      perm(Y,V).
perm([],[]).

ordered([_]).
ordered([X,Y|Z]) :- X =< Y, ordered([Y|Z]).

m_sort(X,Y):-
      perm(X,Y),
      ordered(Y).
