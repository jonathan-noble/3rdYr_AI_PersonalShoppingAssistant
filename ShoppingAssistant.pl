%Main
main :- listing(aisle),
       %shortest(fruit ,home, Path, Length),
       start.

%facts
aisle(fruit).
aisle(veggies).
aisle(butcher).
aisle(cosmetic).
aisle(pet).
aisle(home).
aisle(clothes).
aisle(cleaning).
aisle(sale).


isconnectedto(fruit,veggies,1).
isconnectedto(fruit,butcher,3.5).
isconnectedto(veggies,cosmetic,2.5).
isconnectedto(veggies,pet,1).
isconnectedto(butcher,home,2.2).
isconnectedto(cosmetic,home,1.5).
isconnectedto(cosmetic,pet,4).
isconnectedto(pet,clothes,2.5).
isconnectedto(clothes,cleaning,2).
isconnectedto(cleaning,sale,1).


%rules


%state search
connected(X,Y,L) :- isconnectedto(X,Y,L) ; isconnectedto(Y,X,L).

path(A,B,Path,Len) :-
       travel(A,B,[A],Q,Len),
       reverse(Q,Path).

travel(A,B,P,[B|P],L) :-
       connected(A,B,L).
travel(A,B,Visited,Path,L) :-
       connected(A,C,D),
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path,L1),
       L is D+L1.

shortest(A,B,Path,Length) :-
   setof([P,L],path(A,B,P,L),Set),
   Set = [_|_], % fail if empty
   minimal(Set,[Path,Length]).


minimal([F|R],M) :- min(R,F,M).

% minimal path
min([],M,M).
min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min).
min([_|R],M,Min) :- min(R,M,Min).

%guess(Aisle):-
%
/* fruitvegetable.pro
 * vegetable or fruit identification game.
 * begin with ?- start. */

start :- figureout(FruitVeg),
    /*
    retractall(current_room(_)),
    assertz(current_room(garden)),
    print_location,
    get_input,
    */

    write('My guess is that the object is: '),
    write(FruitVeg),
    nl,
    undo.

/* Facts that will be tested */
figureout(tomato)       :- tomato, !.
figureout(broccoli)     :- broccoli, !.
figureout(potato)       :- potato, !.
figureout(avocado)      :- avocado, !.
figureout(cauliflower ) :- cauliflower, !.
figureout(cucumber)     :- cucumber, !.
figureout(squash)       :- squash, !.
figureout(unknown).              /* no diagnosis */

/* object identification rules */
tomato      :- fruit,
             verify(is_red).
broccoli    :- vegetable,
             verify(is_bushy),
             verify(is_green).
potato      :- vegetable,
             verify(is_yellow).
avocado     :- fruit,
             verify(is_green).
cauliflower :- vegetable,
             verify(is_yellow).
cucumber    :- fruit,
             verify(is_green).
squash      :- fruit,
             verify(is_orange).


/* classification rules */
fruit      :- verify(is_from_fruit_aisle),!.
fruit      :- verify(is_from_tree), !.
fruit      :- verify(has_seed), !.
fruit      :- verify(tastes_sweet), !.

vegetable  :- verify(is_from_ground), !.
vegetable  :- verify(is_leafy), !.
vegetable  :- verify(tastes_bland), !.


/* how to ask questions */
ask(Question) :-

    write('Are you looking for something that: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic (yes/1,no/1).


/* verifying something */
verify(S) :-
   (yes(S)-> true ;
   (no(S) -> fail ;
    ask(S) )).


/*undo all yes/no changes */
undo :- retract(yes(_)), fail.
undo :- retract(no(_)), fail.
undo.

