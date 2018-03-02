%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Christian Soto
% Class: csc 372
% File: fun.pl
% Purpose: This purpose of this program is get familer on how
%          the langauge prolog works with lists and databases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: isUpper
% Parameters: H
% Purpose: This purpose of this predicate to return true if
%          H is between 64 and 97 exculsive.
% Example: isUpper(76)
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isUpper(H):-
        H > 64, H < 97.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: toLowerH
% Parameters: (H,R)
% Purpose: This purpose of this predicate to add 32 if H is 
%          an upper case letter otherwise do nothing
% Example: toLowerH(65,R).
%          R = 97.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
toLowerH(H,R):-
       isUpper(H),
       R is H + 32.
toLowerH(L,L):- \+(isUpper(L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: tolower
% Parameters: ([H|T], [R|L])
% Purpose: This purpose of this predicate to turn all uppercase
%          letters into lower case
% Example: toLowerH("HE",R).
%          R = [h,e].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tolower([], []).
tolower([H|T], [R|L]):-
        toLowerH(H,R), tolower(T,L).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: english2l133t
% Parameters: ([H|T], [R|E])
% Purpose: This purpose of this predicate is to take a list of letters 
%          and return a list of translated numbers
% Example: english2l33t("cat",L).
%          L = [60, 52, 55]
%          
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

english2l33t([],[]).
english2l33t([H|T], [R|E]):-
        translate([H],[R]), english2l33t(T,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: l133t
% Parameters: (E,M)
% Purpose: This purpose of this predicate is to take a list of letters
%          and return a list of translated letters
% Example: l33t("cat",L).
%          L = '<47';
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

l33t(E,M):-
     tolower(E,R), english2l33t(R,R2), name(M,R2).


friend(christian,margaret).
friend(christian,jas).
friend(christian,todd).
friend(christian,ji).
friend(christian,geener).

friend(todd,christian).
friend(todd,susan).

friend(susan,todd).

friend(jas,christian).
friend(jas,geener).
friend(jas,clark).

friend(geener,christian).
friend(geener,jas).
friend(geener,ji).

friend(clark,pat).

friend(pat,mike).
friend(pat,clark).

friend(margaret,christian).

friend(ji,christian).
friend(ji,geener).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate Name: connected
% Parameters: (X,Y)
% Purpose: This purpose of this predicate is to return true
%          if 2 points are connected or false otherwise
% Example: connected(a,b).
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connected(X,Y):- friend(X,Y).

path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

travel(A,B,P,[B|P]) :- 
       connected(A,B).
travel(A,B,Visited,Path) :-
       connected(A,C),           
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path).

list_length(Xs,L) :- list_length(Xs,0,L) .

list_length( []     , L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L).

fb(From, To, MaxLength, Result):-
     path(From, To, Result),
     list_length(Result, R2),
     R2 - 2 < MaxLength.

fball(A,B,Max):-
     path(A,B,Result),
     list_length(Result,R2),
     R2 - 2 < Max,
     write(Result). 
