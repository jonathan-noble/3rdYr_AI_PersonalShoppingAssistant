%Start
start :-
       write('***********************************************'),
       nl,nl,
       write('*********Welcome to AI Personal Shopping! **********'),
       nl,nl,nl,
       write('***********************************************'),
       nl,
       write('Can I get your name please? '),
       read(Name),
       nl,nl,nl,
       write('***********************************************'),
       nl,
       format("Hello ~q! I will be your personal shopping assistant for today.",         Name),
       nl,
       write('***********************************************'),
       nl,nl,nl,
       shop.


shop :-
       write('***********************************************'),
       nl,
       write('Below are the list of areas in this grocery.'),
       nl,
       write('***********************************************'),
       nl,nl,nl,

       %no need listing just type the names write
       listing(area),
       nl,
       write('***********************************************'),
       nl,
       write('Which area are you at the moment?'),
       nl,
       write('***********************************************'),
       nl,
       print_location(Current),
       nl, nl, nl,

       guess(Destination),
       %once the product is figured out , it can then be located
       nl,

       shortest(Current, Destination, Path, Length),
       write('Here is the shortest path you can take: '),
       write(Path),
       nl,
       write('Length: '),
       write(Length),

       nl,nl,
       write('***********************************************'),
       nl,
       write('Need anything else? Type \'shop.\' for more! '),
       nl,
       write('Otherwise, type \'exit.\' to shut me off'),
       nl,
       write('***********************************************').


exit:- write('Goodbye! See you later!'), abort.


/* ask current location */
print_location(Current) :-
    read(Current),
    write('You are currently in: '),
    write(Current),
    write(' section!').

%facts
area(fruities).
area(veggies).
area(butcher).
area(cosmetic).
area(pet).
area(home).
area(clothes).
area(cleaning).
area(sale).


isconnectedto(fruities,veggies,1).
isconnectedto(fruities,butcher,3.5).
isconnectedto(veggies,cosmetic,2.5).
isconnectedto(veggies,pet,1).
isconnectedto(butcher,home,2.2).
isconnectedto(cosmetic,home,1.5).
isconnectedto(cosmetic,pet,4).
isconnectedto(pet,clothes,2.5).
isconnectedto(clothes,cleaning,2).
isconnectedto(cleaning,sale,1).


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



guess(Destination) :- figureout(Grocery),
         findout(Destination),

    write('The product you are looking for may be: '),
    write(Grocery),
    nl,
    write('That product can be found in: '),
    write(Destination),
    nl,
    undo.


/* Facts that will be tested */
figureout(tomato)       :- tomato, !.
figureout(broccoli)     :- broccoli, !.
figureout(potato)       :- potato, !.
figureout(avocado)      :- avocado, !.
figureout(cauliflower)  :- cauliflower, !.
figureout(cucumber)     :- cucumber, !.
figureout(squash)       :- squash, !.


figureout(beef)         :- beef, !.
figureout(crab)         :- crab, !.
figureout(turkey)       :- turkey, !.

figureout(toys)         :- toys, !.
figureout(leash)        :- leash, !.
figureout(treats)       :- treats, !.

figureout(soap)         :- soap,  !.
figureout(spray)        :- spray, !.
figureout(tools)        :- tools, !.

figureout(unknown).             /* no diagnosis */

/* Facts for the area the objects are located at */
findout(fruities)    :- fruities, !.
findout(veggies)   :- veggies, !.

findout(butcher)  :- butcher, !.
findout(pet)      :- pet, !.
findout(cleaning) :- cleaning, !.



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

beef        :- meat,
               verify(is_put_on_burgers).
crab        :- meat,
               verify(is_seafood).
turkey      :- meat,
               verify(is_served_on_thanksgiving).

toys        :- pet,
               verify(is_for_entertainment).
leash       :- pet,
               verify(is_for_walking).
treats      :- pet,
               verify(is_for_snacks).

soap        :- cleaning,
               verify(is_for_handwash).
spray       :- cleaning,
               verify(is_for_kitchen),
               verify(is_for_toilet).
tools       :- cleaning,
               verify(is_for_mopping),
               verify(is_for_dusting).




/* area identification rules */
fruities  :- fruit.
veggies   :- vegetable.

butcher  :- meat.
pet      :- petmalu.
cleaning :- sanitary .


/* classification rules */
fruit      :- verify(tastes_sweet), !.
fruit      :- verify(is_from_tree), !.
fruit      :- verify(has_seed), !.

vegetable  :- verify(is_from_ground), !.
vegetable  :- verify(is_leafy), !.
vegetable  :- verify(tastes_bland), !.


meat       :- verify(is_chunky), !.
meat       :- verify(has_bone), !.
meat       :- verify(has_high_protein), !.

petmalu    :- verify(is_used_by_pets), !.

sanitary   :- verify(is_for_house), !.


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

