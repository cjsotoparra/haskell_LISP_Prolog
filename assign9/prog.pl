%
%Minh Le, Christian Soto
%Assignment 9
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    getsentece from sentence.pl
%    Ask the user to give input and put that input into a list of words.
%    The input end when the user hit enter.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getsentence(Words, In, Out) :-
get0(C),
((C = 10, name(W, In), append(Words,[W],Out), !);
(C = 32, name(W, In), append(Words,[W],Words1), getsentence(Words1, [], Out));
append(In,[C],In1), getsentence(Words, In1,Out)).

getsentence(Words) :-
getsentence([],[],Words).

%Dynamic database 

:- dynamic object_at/2.
:- dynamic player_at/1.
:- dynamic player_have/2.
:- dynamic player_learn/1.
:- dynamic is_not_playing/1.

%
%    Database
%    fromTo(A, Direction, B) : Direction of A is B, where A and B is a location, Direction is east, west, north or south.
%

fromTo(house, east, grass1).
fromTo(house, west, rock).
fromTo(house, north, chicken_house).
fromTo(house, south, farm).

fromTo(road1, east, house).
fromTo(road1, west, road2).
fromTo(road1, north, river).
fromTo(road1, south, river).

fromTo(chicken_house, east, river).
fromTo(chicken_house, west, rock).
fromTo(chicken_house, north, grass4).
fromTo(chicken_house, south, house).

fromTo(grass4, east, river).
fromTo(grass4, west, rock).
fromTo(grass4, north, grass5).
fromTo(grass4, south, chicken_house).

fromTo(grass5, east, grass3).
fromTo(grass5, west, rock).
fromTo(grass5, north, rock).
fromTo(grass5, south, grass4).

fromTo(farm, east, river).
fromTo(farm, west, rock).
fromTo(farm, north, house).
fromTo(farm, south, rock).

fromTo(grass3, east, jungle).
fromTo(grass3, west, grass5).
fromTo(grass3, north, river).
fromTo(grass3, south, river).

fromTo(grass1, east, apple_tree1).
fromTo(grass1, west, house).
fromTo(grass1, north, river).
fromTo(grass1, south, river).

fromTo(apple_tree1, east, market).
fromTo(apple_tree1, west, grass1).
fromTo(apple_tree1, north, blacksmith).
fromTo(apple_tree1, south, grass2).

fromTo(market, east, river).
fromTo(market, west, apple_tree1).
fromTo(market, north, river).
fromTo(market, south, river).

fromTo(blacksmith, east, river).
fromTo(blacksmith, west, river).
fromTo(blacksmith, north, bandit).
fromTo(blacksmith, south, apple_tree1).

fromTo(bandit, east, river).
fromTo(bandit, west, river).
fromTo(bandit, north, jungle).
fromTo(bandit, south, blacksmith).

fromTo(jungle, east, river).
fromTo(jungle, west, grass3).
fromTo(jungle, north, masterKF).
fromTo(jungle, south, bandit).

fromTo(masterKF, east, river).
fromTo(masterKF, west, river).
fromTo(masterKF, north, river).
fromTo(masterKF, south, jungle).

fromTo(grass2, east, river).
fromTo(grass2, west, river).
fromTo(grass2, north, apple_tree1).
fromTo(grass2, south, cave).

fromTo(cave, east, rock).
fromTo(cave, west, rock).
fromTo(cave, north, grass2).
fromTo(cave, south, monster).

fromTo(monster, east, rock).
fromTo(monster, west, rock).
fromTo(monster, north, cave).
fromTo(monster, south, rock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Database
%    area(A) : The areas inside in game that the user is allowed to travel on.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

area(grass5).
area(grass4).
area(chicken_house).
area(house).
area(farm).
area(grass3).
area(grass1).
area(jungle).
area(masterKF).
area(bandit).
area(blacksmith).
area(apple_tree1).
area(grass2).
area(cave).
area(monster).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Database
%    door(A, B) : These are the doors in which the areas are connected to.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

door(grass5, grass4).
door(grass4, chicken_house).
door(chicken_house, house).
door(house, farm).
door(grass3, grass5).
door(grass1, house).
door(grass1, apple_tree1).
door(masterKF, jungle).
door(jungle, grass3).
door(jungle, bandit).
door(bandit, blacksmith).
door(blacksmith, apple_tree1).
door(apple_tree1, market).
door(apple_tree1, grass2).
door(grass2, cave).
door(cave, monster).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Database
%    location_s(object_at(A), B) : These are the locations in which the objects of the game can be found
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

location_s(object_at(key), blacksmith).
location_s(object_at(kungfu), masterKF).
location_s(object_at(key), chicken_house).
location_s(object_at(crop), farm).
location_s(object_at(lock), cave).
location_s(object_at(egg), market).
location_s(object_at(crop), market).


% Initialize the data in the begin of the game 

% object_at(Item, Place) : Item is at Place.

object_at(key, blacksmith).
object_at(kungfu, masterKF).
object_at(key, chicken_house).
object_at(crop, farm).
object_at(lock, cave).

%player_at(Place): player is curently at this Place
player_at(house).

%player_have (Num, Item): player has Num of Item currently.
player_have(0, key).
player_have(3, egg).
player_have(6, crop).
player_have(20, coin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Database
%    price(Item, Cost): Item is Cost coin each
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
price(key, 10).
price(egg, 5).
price(crop, 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: go
%Aurgment: Direction 
%Purpose:   move the player to a new location based on the given Direction.
%Example: go(west)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
go(Direction) :-
    player_at(Here),
    fromTo(Here, Direction, There),
    \+ There = rock,
    \+ There = river,
    \+ There = cave,
    retract(player_at(Here)),
    assert(player_at(There)),
    write('I am at '),
    write(There),nl,!.
go(Direction):-
    player_at(Here),
    fromTo(Here, Direction, rock),
    write('cannot go to rock area!'), nl,!.
go(Direction):-
    player_at(Here),
    fromTo(Here, Direction, river),
    write('cannot go to river area!'), nl,!.
go(Direction):-
    player_at(Here),
    fromTo(Here, Direction, cave),
    object_at(lock, cave),
    write('cannot go to through. The cave is locked, you need to get a key at the blacksmith!'), nl,!.
go(Direction):-
    player_at(Here),
    fromTo(Here, Direction, cave),
    \+ object_at(lock, cave),
    retract(player_at(Here)),
    assert(player_at(cave)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: look
%Aurgment: none
%Purpose:  give player information around him and
%Example:  look.
%           You are in the house
%	    You can see the following items:
%	    You can go to:
%	      farm
%	      chicken_house
%	      grass1
% 	      true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look :-
    player_at(Place),
    write('You are in the '), write(Place), nl,
    write('You can see the following items:'), nl,
    list_things(Place),
    write('You can go to:'), nl,
    list_connections(Place).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: list_things
%Aurgment: Place
%Purpose:  list every item that is at Place
%Example: list_things(chicken_house).
%	  key
%	  true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_things(Place) :-
    location_s(object_at(X),Place),
    tab(2),
    write(X),
    nl,
    fail.
list_things(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: list_connections
%Aurgment: Place
%Purpose:  list every area that is connected to Place
%Example: list_connections(chicken_house).
%	    house
%	    grass4
%	  true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_connections(Place) :-
    connect(Place, X),
    tab(2),
    write(X),
    nl,
    fail.
list_connections(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: connect
%Aurgment: A, B
%Purpose:  return true is two areas A and B are connected otherwise return false
%Example: connect(house, grass5)
%           false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(X,Y):-
   door(X,Y).
connect(X,Y):-
    door(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: goo
%Aurgment: Place
%Purpose: move the player to a different area on the map
%Example: goo(chicken_house).
%		You are in the chicken_house
%		You can see the following items:
%		  key
%		You can go to:
%		  house
%		  grass4
%		true.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goo(Place):-
    player_at(X),
    connect(X, Place),
    move(Place),
    look, !.
goo(Place):-
    player_at(X), write('You can''t get the '),
    write(Place), write(' from the '), write(X), write('.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: move
%Aurgment: Place
%Purpose: move the player to a different area on the map
%Example: move(grass5).
%	  false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move(Place):-
    retract(player_at(_)),
    asserta(player_at(Place)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: pickUp
%Aurgment: Item
%Purpose: pickUp an Itme from the current area.
%Example: pickUp(key).
%	    Pick key
%   	    true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pickUp(Item):-
    player_at(Here),
    object_at(Item, Here),
    write('Picked up '), write(Item),
    retract(player_have(Num,Item)),
    NewNum is Num+1,
    assert(player_have(NewNum,Item)),
    !.
pickUp(Item):-
    write(Item), write(' is not here! You cannot pick it up!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: learn
%Aurgment: Skill
%Purpose: learn a skill for the player
%Example: learn(kungfu).
%	   learned kung fu.
% 	   true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

learn(Skill):-
    player_at(masterKF),
    object_at(Skill, masterKF),
    assert(player_learn(kungfu)),
    write('You just learned kungFu!'),nl.
learn(Skill):-
    write('Find master of '), write(Skill), write('first if you want to learn!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: buy
%Aurgment: Item
%Purpose: Buy items from the board game
%Example: buy(key).
%          bought key.
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buy(Item):-
    player_at(Here),
    object_at(Item, Here),
    price(Item,Price),
    player_have(CountCoin, coin),
    CountCoin >= Price,
    retract(player_have(CountCoin,coin)),
    NewCountCoint is CountCoin - Price,
    assert(player_have(NewCountCoint,coin)),
    write('Buy '), write(Item), write(' for '), write(Price), write(' coin.'),
    retract(player_have(Num,Item)),
    NewNum is Num+1,
    assert(player_have(NewNum,Item)),
    !.
buy(Item) :-
    write(Item), write('is not here for you to buy!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: sell
%Aurgment: Num, Item
%Purpose: sell items to the market in the game
%Example: sell(1, key)
%          you sold a key
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sell(Num,Item):-
    player_at(market),
    player_have(Total, Item),
    Total >= Num,
    retract(player_have(Total,Item)),
    NewTotal is Total - Num,
    assert(player_have(NewTotal,Item)),
    price(Item, Price),
    Income is Price*Num,
    retract(player_have(TotalCoin,coin)),
    NewTotalCoin is Income+TotalCoin,
    assert(player_have(NewTotalCoin,coin)),
    write('You just sell '), write(Num), write(Item), write(' for'), write(Price), write(' coin!'), nl,
    !.
sell(Num, Item) :-
    player_have(Total, Item),
    Total < Num,
    write('you dont have that much '),write(Item), write('to sell!'), nl.
sell(_, _) :-
    write('you need to go to the market if you want to sell anything!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: openDoor
%Aurgment: none
%Purpose: Be able to open doors in the game
%Example: openDoor.
%          Cave door is now opened!
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
openDoor:-
    player_at(grass2),
    player_have(NumKey, key),
    NumKey > 0,
    retract(object_at(lock, cave)),
    assert(object_at(unlock,cave)),
    write('Cave door is now opened!'), nl, !.
openDoor:-
    player_at(grass2),
    write('You need to get key from blacksmith first!'), nl,!.
openDoor:-
    write('No door to open!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: killMonster
%Aurgment: none.
%Purpose: This predicate is to determine if you can kill the monster
%Example: killMonster
%          You just killed the monster!!
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

killMonster:-
    player_at(monster),
    player_learn(kungfu),
    write('You just killed the monster!!'),nl.
killMonster:-
    player_at(monster),
    write('You dont have kungfu to kill the monster!!'),nl,
    write('You get wounded and pass out.'),nl,
    write('Someone takes you home and escape the monster.'),nl,
    retract(player_at(monster)),
    assert(player_at(house)),
    !.
killMonster:-
    write('There is no monster here!!'),nl,
    write('Go through the cave to see the monster.'),nl,
    write('And dont forget to learn kungfu before fighting the monster!'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: iLearn
%Aurgment: none.
%Purpose: Display the skills that were learned in the game
%Example: iLearn.
%          I learned kung fu.
%          true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iLearn:-
    player_learn(Skill),
    write('I learned '), write(Skill),nl, !.
iLearn:-
    write('I did not learn any Skill!'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: iHave
%Aurgment: none.
%Purpose: Display the items that the player has
%Example: iHave.
%		I have 0 key
%		I have 3 egg
%		I have 6 crop
%		I have 20 coin
%		true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iHave:-
    player_have(Num, Obj),
    write('I have '), write(Num), write(' '), write(Obj),nl,fail.
iHave.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: around
%Aurgment: X
%Purpose: Display the areas around X
%Example: around(house).
%		The east of house is grass1
%		The west of house is rock
%		The north of house is chicken_house
%		The south of house is farm
%		true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

around(X) :-
    fromTo(X,Direction,Place),
    write('The '), write(Direction), write(' of '), write(X), write(' is '), write(Place), nl,fail.
around(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: iamat
%Aurgment: none.
%Purpose: Display the area that the player currently in.	
%Example: iamat.
%         I am at the house
%	  true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iamat:- player_at(X), write('I am at the '), write(X),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: command
%Aurgment: (L)
%Purpose: takes a line and deteremines which command to run.
%Example: command('where am i at').
%         I am at the house.
%	  true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

command(L) :- member(go,L), member(east,L), go(east),!.
command(L) :- member(go,L), member(west,L), go(west),!.
command(L) :- member(go,L), member(north,L), go(north),!.
command(L) :- member(go,L), member(south,L), go(south),!.

command(L) :- member(go,L), member(house,L), goo(house),!.
command(L) :- member(go,L), member(market,L), goo(market), nl, !.
command(L) :- member(go,L), member(farm,L), goo(farm), nl, !.
command(L) :- member(go,L), member(grass1,L), goo(grass1),!.
command(L) :- member(go,L), member(chicken_house,L), goo(chicken_house),!.
command(L) :- member(go,L), member(grass5,L), goo(grass5), nl, !.
command(L) :- member(go,L), member(grass4,L), goo(grass4), nl, !.
command(L) :- member(go,L), member(grass3,L), goo(grass3),!.
command(L) :- member(go,L), member(masterKF,L), goo(masterKF),!.
command(L) :- member(go,L), member(jungle,L), goo(jungle), nl, !.
command(L) :- member(go,L), member(bandit,L), goo(bandit), nl, !.
command(L) :- member(go,L), member(blacksmith,L), goo(blacksmith),!.
command(L) :- member(go,L), member(apple_tree1,L), goo(apple_tree1),!.
command(L) :- member(go,L), member(grass2,L), goo(grass2), nl, !.
command(L) :- member(go,L), member(cave,L), goo(cave), nl, !.
command(L) :- member(go,L), member(monster,L), goo(monster),!.

command(L) :- member(where,L), member(i,L), iamat,!.
command(L) :- member(i,L), member(look,L), member(around,L), look,!.

command(L) :- member(what,L), member(i,L), member(have,L) , iHave,!.
command(L) :- member(what,L), member(i,L), member(learn,L) , iLearn,!.

command(L) :- member(pick,L),member(egg,L), pickUp(egg),!.
command(L) :- member(pick,L),member(crop,L), pickUp(crop),!.

command(L) :- member(learn,L),member(kungfu,L), learn(kungfu),!.

command(L) :- member(buy,L),member(key,L), buy(key),!.

command(L) :- member(sell,L),member(crop,L), member(1,L), sell(1,crop),!.
command(L) :- member(sell,L),member(crop,L), member(2,L), sell(2,crop),!.
command(L) :- member(sell,L),member(crop,L), member(3,L), sell(3,crop),!.
command(L) :- member(sell,L),member(crop,L), member(4,L), sell(4,crop),!.
command(L) :- member(sell,L),member(crop,L), member(5,L), sell(5,crop),!.

command(L) :- member(sell,L),member(egg,L), member(1,L), sell(1,egg),!.
command(L) :- member(sell,L),member(egg,L), member(2,L), sell(2,egg),!.
command(L) :- member(sell,L),member(egg,L), member(3,L), sell(3,egg),!.
command(L) :- member(sell,L),member(egg,L), member(4,L), sell(4,egg),!.
command(L) :- member(sell,L),member(egg,L), member(5,L), sell(5,egg),!.

command(L) :- member(open,L), openDoor,!.
command(L) :- member(use,L), member(key,L), openDoor,!.


command(L) :- member(kill,L),member(monster,L), killMonster,!.
command(L) :- member(fight,L),member(monster,L), killMonster,!.

command(L) :- member(instruction,L), instruction, !.
command(L) :- member(hint,L), instruction, !.

command(L) :- member(quit,L), endGame.
command(L) :- member(end,L), member(game,L), endGame.

command(_) :- nl, write('Dont know what is your command!'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: start
%Aurgment: none.
%Purpose: starts the game
%Example: start.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start:- getsentence(L), nl, command(L), nl, start.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: endGame
%Aurgment: none.
%Purpose: ends the game
%Example: endGame
%         End game!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

endGame:- write('End game!'),nl, halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Predicate: instruction
%Aurgment: none.
%Purpose: Displays the tutorial for the game
%Example: instruction.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instruction:-
    write('This is monster hunter tutorial'), nl,
    write('You are a very weak explorer and have been tasked with'), nl,
    write('defeating a monster.  You must explore through the areas and learn'), nl,
    write('kung fu from the master, or something of that nature.  In order to get to the monster, you '), nl,
    write('must first collect a key. You can move throught the area by typing simple commands.'), nl.
