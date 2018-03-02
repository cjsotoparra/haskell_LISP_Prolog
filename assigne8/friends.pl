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

is_friends(X,Y,L):- friend(X,Y), L = Y.
