%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Read a sentence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getsentence(Words, In, Out) :-
   get0(C),
   ((C = 46, name(W, In), append(Words,[W],Out), !); 
    (C = 32, name(W, In), append(Words,[W],Words1), getsentence(Words1, [], Out));
    append(In,[C],In1), getsentence(Words, In1,Out)).

getsentence(Words) :-
   getsentence([],[],Words).

% Read sentence and map chars to atoms.
readsentence(S1) :-
   getsentence(R),
   mapwords(R, S),
   strip(S, S1).

% Strip blanks
strip_initial([],[]).
strip_initial([blank|S],R) :-
   !, strip_initial(S,R).
strip_initial(S,S).

strip_final(S,R) :-
   reverse(S,S1),
   strip_initial(S1,S2),
   reverse(S2,R).

strip_inside([],[]).
strip_inside([blank,blank|S],R) :-
   !, strip_inside([blank|S],R).
strip_inside([blank|S],[blank|R]) :-
   !, strip_inside(S,R).
strip_inside([C|S],[C|R]) :-
   strip_inside(S,R).

strip(S,R):-
   strip_initial(S,S1),
   strip_inside(S1,S2),
   strip_final(S2, R).

% Split words into letters.
mapwords([W|WL],S) :-
   name(W, N),
   mapchars(N, S1),
   mapwords(WL, S2),
   append(S1,[blank|S2], S).
mapwords([],[]).

mapchars([C|N], [C1|S]) :-
   mapchar([C], C1),
   mapchars(N, S).
mapchars([],[]).

mapchar("A",a). mapchar("B",b). mapchar("C",c). mapchar("D",d). mapchar("E",e).
mapchar("F",f). mapchar("G",g). mapchar("H",h). mapchar("I",i). mapchar("J",j).
mapchar("K",k). mapchar("L",l). mapchar("M",m). mapchar("N",n). mapchar("O",o).
mapchar("P",p). mapchar("Q",q). mapchar("R",r). mapchar("S",s). mapchar("T",t).
mapchar("U",u). mapchar("V",v). mapchar("W",w). mapchar("X",x). mapchar("Y",y).
mapchar("Z",z).

mapchar("a",a). mapchar("b",b). mapchar("c",c). mapchar("d",d). mapchar("e",e).
mapchar("f",f). mapchar("g",g). mapchar("h",h). mapchar("i",i). mapchar("j",j).
mapchar("k",k). mapchar("l",l). mapchar("m",m). mapchar("n",n). mapchar("o",o).
mapchar("p",p). mapchar("q",q). mapchar("r",r). mapchar("s",s). mapchar("t",t).
mapchar("u",u). mapchar("v",v). mapchar("w",w). mapchar("x",x). mapchar("y",y).
mapchar("z",z).

mapchar("!", excl_mark).  mapchar("?", quest_mark).
mapchar(".", full_stop).  mapchar(",", comma).
mapchar("-", hyphen).     mapchar(";", semicolon).
mapchar("$", dollar).     mapchar("'",apostrophe).

mapchar("0", 0). mapchar("1", 1). mapchar("2", 2). mapchar("3", 3). 
mapchar("4", 4). mapchar("5", 5). mapchar("6", 6). mapchar("7", 7). 
mapchar("8", 8). mapchar("9", 9). 

mapchar(X, X).
