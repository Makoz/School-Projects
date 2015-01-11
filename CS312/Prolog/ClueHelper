% Chen Hao (John )Hsu
% Ellis Ho 

:- dynamic weapon/1.       % weapon
:- dynamic nweapon/2.      % weapons that were proved wrong and the player that proved it wrong
:- dynamic weapons/6.      % list of weapons
:- dynamic room/1.         % room
:- dynamic nroom/2.        % rooms that were proved wrong
:- dynamic rooms/9.        % list of rooms
:- dynamic character/1.    % character
:- dynamic ncharacter/2.    % characters that were proved wrong
:- dynamic characters/6.   % list of characters
:- dynamic hand/1.         % hand
:- dynamic suggestions/6.  % suggestions made

% Start the game with listW/6,listR/9,listC/6,hand/3.
% Predicates used for testing
% weapons('knife','candlestick','pistol','rope','bat','ax').
% rooms('kitchen','patio','spa','theatre','livingroom','observatory','hall','guestroom','dinningroom').
% characters('missscarlett','colonelmustard', 'mrswhite','mrgreen','mrspeacock','professorplum').
% weapon('knife').
% weapon('candlestick').
% weapon('pistol').
% weapon('rope').
% weapon('bat').
% weapon('ax').
% room('kitchen').
% room('patio').
% room('spa').
% room('theatre').
% room('livingroom').
% room('observatory').
% room('hall').
% room('guesthouse').
% room('diningroom').
% character('missscarlett').
% character('colonelmustard').
% character('mrswhite').
% character('mrgreen').
% character('mrspeacock').
% character('professorplum').
% hand('knife').
% hand('spa').
% hand('mrgreen').
%

% dummy initialization variables that will never be called
%nroom('fafh','8dhfs').
%ncharacter('1ref','1rw34').
%nweapon('1ed2ee','13e34').

% New school Clue cards
%
% List of cards
% Initially, everything will be a suspected item
% First we will remove the 3 cards in our hand from the list
% As the game proceeds, we will remove cards that are not suspects
% (when we get proven wrong).

% Pretty much this is the same as the shit before, but from here we
% are free to pick what ever items we want.
%
% List of weapons (6 weapons)
% This initializes the cards
listW(A,B,C,D,E,F) :-
	assert(weapon(A)),
	assert(weapon(B)),
	assert(weapon(C)),
	assert(weapon(D)),
	assert(weapon(E)),
	assert(weapon(F)),
	assert(weapons(A,B,C,D,E,F)).

% List of rooms (9 rooms)
listR(A,B,C,D,E,F,G,H,I) :-
	assert(room(A)),
	assert(room(B)),
	assert(room(C)),
	assert(room(D)),
	assert(room(E)),
	assert(room(F)),
	assert(room(G)),
	assert(room(H)),
	assert(room(I)),
	assert(rooms(A,B,C,D,E,F,G,H,I)).

% List of characters (6 characters)
listC(A,B,C,D,E,F) :-
	assert(character(A)),
	assert(character(B)),
	assert(character(C)),
	assert(character(D)),
	assert(character(E)),
	assert(character(F)),
	assert(characters(A,B,C,D,E,F)).


% Cards in hand
% Will have to assert items into hand
% Should have 3 predicates for hand; 1 for each card
%
% hand('card').

showHand:-
	hand(C),
	write(C).

	
addHand(C,P) :- wrongCard(C,P), assert(hand(C)).	
	
% W is the list of all the weapons
% R is the list of all the rooms
% C is the list of all the characters
% Checks to see which items are possible answers and returns a list of
% it. Initially every item is a possible answer. Retracting
% weapons/rooms/characters will change the list
allItems :-
	% This makes the list so that we can use any items instead of just sticking to whats already defined
	weapons(A,B,C,D,E,F),
	W = [A,B,C,D,E,F],

	rooms(A1,B1,C1,D1,E1,F1,G1,H1,I1),
	R = [A1,B1,C1,D1,E1,F1,G1,H1,I1],

	characters(A2,B2,C2,D2,E2,F2),
	K = [A2,B2,C2,D2,E2,F2],

	write('Weapons: '),
	checkWeapons(W,[]),nl,
	write('Rooms: '),
	checkRooms(R,[]),nl,
	write('Characters: '),
	checkCharacters(K,[]).

% Helper function for allItems. Checks the predicates for items and
% returns a list of the ones that can still be an answer.
checkWeapons([],L) :- write(L).
checkWeapons([H|T],L) :- weapon(H) -> checkWeapons(T,[H|L]) ; checkWeapons(T,L).

checkRooms([],L) :- write(L).
checkRooms([H|T],L) :- room(H) -> checkRooms(T,[H|L]) ; checkRooms(T,L).

checkCharacters([],L) :- write(L).
checkCharacters([H|T],L) :- character(H) -> checkCharacters(T,[H|L]) ; checkCharacters(T,L).

% Quick way of retracting/asserting items (because I'm lazy and don't
% want to type out the function). I'm assuming that C will be
% weapon/room/char At the start of the game, we would want to call
% wrongCard to get rid of the cards that can't be answers and put them
% in a diff list incase we need it. As the game proceeds, this function
% will be called when we are proven wrong.
wrongCard(C, P) :- weapon(C) -> retract(weapon(C)), assert(nweapon(C, P));
		   room(C) -> retract(room(C)), assert(nroom(C, P));
                   retract(character(C)),  assert(ncharacter(C, P)).

% List of suggestions made by everyone
% Initially no suggestions exist
% w = weapon
% r = room
% c = character
% pl = player that made the suggestion
% pw = do we know the card?
% pwp = player that proved it wrong,
%
% suggestions('w','r','c','pl','pw','0').
% Could use something like this to output a list of suggestions
%
% List of suggestions used for TESTING:
% suggestions('knife','theatre','mrswhite','1','no', '0').
/*
suggestions('knife','spa','mrgreen','1','no', '0').
suggestions('knife','hall','mrgreen','1','yes', '0').
suggestions('knife','patio','mrgreen','1','no', '0').
suggestions('knife','theatre','mrgreen','1','no', '0').
suggestions('knife','kitchen','mrgreen','1','no', '0').
*/

% suggestions('knife','theatre','colonelmustard','2','yes', '3').
% suggestions('knife','spa','missscarlett','2','yes', '4').
/*
suggestions('knife','hall','mrswhite','2','no', '0')).
suggestions('knife','patio','mrgreen','2','yes', '0')).
suggestions('knife','theatre','mrspeacock','2','no', '0')).
*/
% suggestions('knife','theatre','mrgreen','5','no','1').
addSugg(W,R,C,PL,PW,PWP) :- assert(suggestions(W,R,C,PL,PW,PWP)).

% We can do something like suggestions(WEAPONS,ROOM,CHARACTER, '1', PW)
% where '1' is the player. From this we can quickly find the suggestions
% a player made or all the suggestions that were proven right, etc.
% Doesnt feel like the cleanest way to do things, but it works.
% (You have to press ; to get the next list instead of everything
% printing out at once. Not sure if this is okay or yea...)
% W = weapon
% R = room
% C = char
% Pl = player that made sugg
% PW = proved wrong
% PWP = player that proved it wrong ('0' if was not proved wrong)
%
allS :- suggestions(W,R,C,PL,PW, PWP),
	write('Accused: '), write([W, R, C]), nl,
	write('Made by: Player'), write(PL), nl,
	write('Proved wrong? '), write(PW), nl,
	write('Proved wrong by: '), write(PWP).

% Let's make a list of suggestions proved wrong and a list that was not
% proved wrong. From here, maybe we can learn more about the game and
% get the program to make a winning suggestion/accusation.
%
% This function pretty much returns a list of possible suspects. I dont
% really know how to make it work better yet.
%
provedWrongSugg :-
	suggestions(WEAPON,ROOM,CHAR,PLAYER,PW,PWP),
	checkPW(PW),
	write([WEAPON,ROOM,CHAR,PWP]).

% Was proved wrong.
checkPW('yes').

% maybe we can make a list of provedNotWrongS and everytime we call
% wrongCard to remove a card, we can also remove a suggestion that
% contains that card.





% Returns a list of valid suggestions
giveSugg(X,Y,Z) :- weapon(X), not(member(X,nweapon)), room(Y), not(member(Y,nroom)),character(Z), not(member(Z,ncharacter)), write('Suggest: Weapon: '), write(X), write(', '), write('In Room: '), write(Y), write(', by '), write(Z).

validRoom(Y) :- room(Y), not(member(Y, nroom)).
validWeapon(X) :- weapon(X), not(member(X,nweapon)).
validCharacter(Z) :- character(Z), not(member(Z,ncharacter)).


% Write a function that given what was shown, adds the item into not set and deals+checks with suggestions: TODO add verify suggestions here
shown(X,Y,Z,C,P,F) :- F == '0' -> write('Accuse: '), write(X),write(' '),write(Y),write(' '),write(Z),write(' ');
                      wrongCard(C,P).
		      %checkSugg.

checkSugg1 :-
	suggestions(X1,Y1,Z1,A,'no',C1),
	grabChar(X1,Y1,C1), assert(ncharacter(Z1,C1)),
	retract(suggestions(X1,Y1,Z1,A,'no',C1)),
	assert(suggestions(X1,Y1,Z1,A,'yes',C1)).
checkSugg2 :-
	suggestions(X1,Y1,Z1,A,'no',C1),
	grabRoom(X1,Z1,C1) -> assert(nroom(Y1,C1)),
	retract(suggestions(X1,Y1,Z1,A,'no',C1)),
	assert(suggestions(X1,Y1,Z1,A,'yes',C1)).
checkSugg3 :-
	suggestions(X1,Y1,Z1,A,'no',C1),
	grabWeapon(Y1,Z1,C1) -> assert(nweapon(X1,C1)),
	retract(suggestions(X1,Y1,Z1,A,'no',C1)),
	assert(suggestions(X1,Y1,Z1,A,'yes',C1)).

grabChar(X,Y,C1) :- nweapon(X,K), nroom(Y,J), C1 \= K, C1 \= J.
grabRoom(X,Z,C1) :- nweapon(X,K), ncharacter(Z,L), C1 \= K, C1 \= L.
grabWeapon(Y,Z,C1) :- nroom(Y,J), ncharacter(Z,L), C1 \= J, C1 \= L.


% nroom('theatre','2').
%ncharacter('mrswhite','1').



member(H,[H|T]).
member(X,[H|T]) :- member(X,T).


