%Start
start :-write('***********************************************'),
       nl,nl,
       write('*********Welcome to Personal shopping**********'),
       nl,
       write('***********************************************'),
       nl,nl,nl,
       write('***********************************************'),
       nl,
       write('Hello I am your personal shopping assistant!'),
       nl,
       write('***********************************************'),
       nl,nl,nl,
       write('***********************************************'),
       nl,
       write('Below are the list of aisle in this grocery.'),
       nl,
       write('***********************************************'),
       nl,nl,nl,

       %no need listing just type the names write
       listing(aisle),
       nl,


       write('***********************************************'),
       nl,
       write('Which area are you at the moment?'),
       nl,
       write('***********************************************'),
       nl,nl,nl,

       print_location(Current),

       nl, nl, nl,

       guess(Destination),
       %once the product is figured out , it can then be located

       nl,nl,
       write('***********************************************'),
       nl,
       write('(Query)Type in: shortest(Your_current_location,Your_Destination,Path,Length)'),
       nl,
       write('to find out the quickest way to go to that product.'),
       nl,
       %shortest way to reach that product is provided below
       connected(X,Y,L),
       path(A,B,Path,Length),
       travel(A,B,P,[B|P],L),
       travel(A,B,Visited,Path,L),
       minimal([F|R],M),
       min([],M,M),
       min([[P,L]|R],[_,M],Min),
       min([_|R],M,Min),
       shortest(Current ,Destination, Path, Length).




/* ask current location */
print_location(Current) :-
    read(Current),
    write(' You are currently in: '),
    write(Current),
    write(' section!').

%:- dynamic current_room/1.


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



guess(Destination) :- figureout(FruitVeg),
         findout(Destination),
    /*
    retractall(current_room(_)),
    assertz(current_room(garden)),
    print_location,
    get_input,
    */

    write('The product you are looking for is: '),
    write(FruitVeg),
    nl,
    write('That product can be found in: '),
    write(Destination),
    %3 variables one for a and b (current loc-  destination)
    %that can be found in (variable)
    %put in function predicate shortest
    %category= aisle
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
figureout(unknown).             /* no diagnosis */

/* Facts for the area the objects are located at */
findout(fruit_area)    :- fruit_area, !.
findout(veggie_area)    :- veggie_area, !.


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


/* area identification rules */
fruit_area   :- fruit.

veggie_area  :- vegetable.


/* classification rules */
fruit      :- verify(tastes_sweet), !.
fruit      :- verify(is_from_tree), !.
fruit      :- verify(has_seed), !.


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

