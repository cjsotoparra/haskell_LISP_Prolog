%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Christian Soto
% Class: csc 372
% File: fun.pl
% Purpose: This purpose of this program is get familer on how 
%          the langauge prolog works, using predicates,logic,
%          and databases to accpmlish goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: magic
% Parameters: (board(A,B,C,D,E,F,G,H,I)
% Purpose: This purpose of this predicate to return true if 
%          all rows, colums, and diagonals add up to 15
% Example: magic(board(2,7,6,9,5,1,4,3,8)).
%          true.
%          magic(board(2,7,6,9,5,1,4,3,9)).
%          false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

magic(board(A,B,C,D,E,F,G,H,I)) :-
        15 is A+B+C,
        15 is D+E+F,
        15 is G+H+I,
        15 is A+D+G,
        15 is B+E+H,
        15 is C+F+I,
        15 is A+E+I,
        15 is C+E+G.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: abs
% Parameters: (X,Y)
% Purpose: This purpose of this predicate to return the 
%          absoulte value of X.
% Example: abs(0,0).
%          true.
%          abs(-1,1).
%          true.
%          abs(-1,Y).
%          Y = 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abs(X,Y) :-
    X >= 0, Y = X.
abs(X,Y) :-
    X <0, Y is *(X,-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: move_one
% Parameters: (Z, Q)
% Purpose: This purpose of this predicate is to have 1 added
%          to Z and then instatiate Q to that value.
% Example: move_one(5,Q).
%          Q = 6.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move_one(Z,Q) :-
     Q is Z + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: seq
% Parameters: (Z,Y,X)
% Purpose: This purpose of this predicate is to return all
%          numbers from Z to Y inclusive and have X 
%          instantiated for values.
% Example: seq(1,2,X).
%          X = 1;
%          X = 2;
%          false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(Z,Y,Z) :-
    Z =< Y.
seq(Z, Y, X) :-
    Z < Y, move_one(Z,Q), seq(Q, Y, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: knight
% Parameters: (C, R, C2, R2)
% Purpose: This purpose of this predicate is to return all 
%          the places a knight in a chess game jump relative
%          to it's position.
% Example: knight(8,1,R,C).
%          R = 7,
%	   C = 3 ;
%	   R = 6,
%	   C = 2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

knight(C, R, C2, R2):-
     C2 is C - 1,R2 is R + 2,
     withinBoard(C2,R2);

     C2 is C + 1,R2 is R + 2, 
     withinBoard(C2,R2);

     C2 is C + 2, R2 is R + 1,
     withinBoard(C2,R2);

     C2 is C + 2, R2 is R - 1,
     withinBoard(C2,R2);
    
     C2 is C + 1, R2 is R - 2,
     withinBoard(C2,R2);

     C2 is C - 1, R2 is R - 2, 
     withinBoard(C2,R2);

     C2 is C - 2, R2 is R - 1,
     withinBoard(C2,R2);

     C2 is C - 2, R2 is R + 1, 
     withinBoard(C2,R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: withinBoard
% Parameters: (Col, Row)
% Purpose: This purpose of this predicate is to check if the
%          Row and Col are within the board of 8X8 size
% Example: withinBoard(9, 10) ---> false.
%          withinBoard (8,8)  ---> true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

withinBoard(Col,Row):-
            Row < 9, Row > 0, Col < 9, Col > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: sum
% Parameters: (X,Y,R)
% Purpose: This purpose of this predicate is to sum the 
%          Peano axoms.
% Example: sum(zero, zero, R) ---->    R = zero.
%          sum(zero, zero, zero). ---> true.
%          sum(zero, succ(zero), R) --> R = succ(zero).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum(zero, zero, R) :- R = zero, !.
sum(zero, X, R):- R = X, X \= zero.
sum(X, zero, R):- R = X, X \= zero.  
sum(succ(X), Y, R):-
     sum(X, succ(Y), R).

%Database for parts of a bicycle and car

has(bicycle, wheel, 2).
has(bicycle, handlebar, 1).
has(bicycle, brake, 2).
has(wheel, hub, 1).
has(wheel, spoke, 32).
has(bicycle, frame, 1).

has(car, steering_wheel, 1).
has(car, stereo, 1).
has(car, tires, 4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: partof
% Parameters: X, Y
% Purpose: This purpose of this predicate is to see if X is
%          part of Y based on the database about parts.
% Example: partof(wheel, spoke) ---> true.
%          partof(bicycle, spoke) ---> true.
%          partof(car, spoke)   -----> false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partof(X,Y):- has(X,Y,_).
partof(X,Y):- has(X,Z, _),
              partof(Z,Y).


%person(Name,
%	is(IsGender, IsHeight, IsAge, HasEducation),
%	wants(WantsGender, WantsHeightMin-WantsHeightMax,
%			   WantsAgeMin-WantsAgeMax,
%			   WantsEducationMin)).

person(lisa,
       is(female, 170, 30, phd),
       wants(male, 180-190, 30-35, phd)).

person(john,
       is(male, 180, 25, masters),
       wants(female, 150-175, 20-24, high_school)).

person(alice,
       is(female, 165, 22, bachelor),
       wants(male, 175-200, 20-30, bachelor)).

person(bob,
       is(male, 182, 28, bachelor),
       wants(male, 160-190, 20-35, high_school)).

person(charles,
       is(male, 160, 21, high_school),
       wants(male, 180-220, 20-40, high_school)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: between.
% Parameters: A, From-To     
% Purpose: This purpose of this predicate is to see                 
%          if A is between From and TO inclusive for both
% Example: between(20, 20-35) ---> True.
%          between(36, 20-36) ---> False.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        

between(A, From-To):-
        A > From-1, A < To+1. 

%Education facts where high_school <= bachelor <= masters <= phd

edu_less(high_school, bachelor).
edu_less(bachelor, masters).
edu_less(masters, phd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: edu_lessOrEqual
% Parameters: (X,Y).
% Purpose: This purpose of this predicate is to see if 
%          the two education levels are equal or X < Y
% Example: edu_lessOrEqual(high_school, high_school).
%          true.
%          edu_lessOrEqual(masters, bachelor).
%          false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

edu_lessOrEqual(X,X).
edu_lessOrEqual(X, Y):- edu_less(X,Y).
edu_lessOrEqual(X, Y):- edu_less(X,Z),
                        edu_lessOrEqual(Z,Y), Z \= Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: dateable
% Parameters: X,Y
% Purpose: This purpose of this predicate is to see if X can 
%          date Y and Y can date X, false otherwise.
% Example: dateable(john,alice).
%          true.
%          dateable(john,bob).
%          false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dateable(X,Y):-

         person(X, is(S,_,_,_), _), Sex = S,
         person(Y, _, wants(S2, _, _, _)), Sex2 = S2,
         Sex = Sex2,

         person(Y, is(S3,_,_,_), _), Sex3 = S3,
         person(X, _, wants(S4, _, _, _)), Sex4 = S4,
         Sex3 = Sex4,

         person(X, is(_,W,_,_), _), Weight is W,
         person(Y, _, wants(_, W2, _, _)), Weight2 = W2,
         between(Weight, Weight2),

         person(Y, is(_,W3,_,_), _), Weight3 is W3,
         person(X, _, wants(_, W4, _, _)), Weight4 = W4,
         between(Weight3, Weight4),


         person(Y, is(S3,_,_,_), _), Sex3 = S3,
         person(X, _, wants(S4, _, _, _)), Sex4 = S4,
         Sex3 = Sex4,

         person(X, is(_,_,M,_), _), Agex is M,
         person(Y, _, wants(_, _, N, _)), Agey = N,
         between(Agex, Agey),

         person(Y, is(_,_,M2,_), _), Agex2 is M2,
         person(X, _, wants(_, _, N2, _)), Agey2 = N2,
         between(Agex2, Agey2),

         person(X, is(_,_,_,E), _), Edu = E,
         person(Y, _, wants(_, _, _, E2)), Edu2 = E2,
         edu_lessOrEqual(Edu2, Edu),

         person(Y, is(_,_,_,E3), _), Edu3 = E3,
         person(X, _, wants(_, _, _, E4)), Edu4 = E4,
         edu_lessOrEqual(Edu4, Edu3), 

         X \= Y.
