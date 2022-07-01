% hatice erk
% 2018400090
% compiling: yes
% complete: yes

% include the knowledge base
:- ['load.pro'].
:- encoding(utf8).

% Calculation for glanian_distance.
gd_calculation([],[],0).
gd_calculation([H1|T1], [H2|T2], Num) :-	
	gd_calculation(T1, T2, Num1),
	((H1 = -1, Num is Num1);
	(Num is Num1+(H1-H2)*(H1-H2))).
	
% predicate 3.1	
% glanian_distance(+Name1, +Name2, -Distance)
glanian_distance(Name1, Name2, Distance) :-
	expects(Name1, _, List1),
	glanian(Name2, _, List2),
	gd_calculation(List1, List2, Num),
	Distance is sqrt(Num), !.
	
% Calculation for weighted_glanian_distance.
wgd_calculation([],[],[],0).
wgd_calculation([H1|T1], [H2|T2], [H3|T3], Num) :-
	wgd_calculation(T1, T2, T3, Num1),
	(
	(H1 = -1, Num is Num1);
	(Temp1 is H1-H2,
	Temp2 is H3*Temp1*Temp1,
	Num is Num1 + Temp2)
	).

% predicate 3.2
% weighted_glanian_distance(+Name1, +Name2, -Distance)
weighted_glanian_distance(Name1, Name2, Distance) :-
	expects(Name1, _,List1),
	weight(Name1, W),
	glanian(Name2, _, List2),
	wgd_calculation(List1, List2, W, Num),
	Distance is sqrt(Num), !.

% To push an item to front of list.
pushFront(Item, List, [Item|List]).

% To remove duplicates.
remove_dups([], []).
remove_dups([H | T], CityList) :- 
	remove_dups(T, List),
	((member(H, List), delete(List, H, L), pushFront(H, L, CityList));
	(pushFront(H, List, CityList))).
	
% To merge lists.
concatenate([], List, List).
concatenate([H1|T1], List2, [H1|TailResult]) :-
    concatenate(T1, List2, TailResult).
	
% To find where he/she lives now.	
find_where_live(Name, Current) :-
	city(X, L1,_), 
	member(Name, L1), 
	Current = X .
	
% predicate 3.3	
% find_possible_cities(+Name, -CityList)
find_possible_cities(Name, CityList) :-
	find_where_live(Name, Current),
	likes(Name, _, Liked),
	concatenate([Current], Liked, CityL), 
	remove_dups(CityL, CityList),!.

% predicate 3.4
% merge_possible_cities(+Name1, +Name2, -MergedCities)
merge_possible_cities(Name1, Name2, MergedCities) :-
	find_possible_cities(Name1, List1),
	find_possible_cities(Name2, List2),
	concatenate(List1, List2, Merged), 
	remove_dups(Merged, MergedCities), !.

% To make list of mutual activities
if_mutual([], _, []).
if_mutual([H1|T1], List2, MutualActivities) :-
	if_mutual(T1, List2, L),
	(
	(member(H1, List2), append([H1], L, MutualActivities));
	(MutualActivities = L)
	).

% predicate 3.5
% find_mutual_activities(+Name1, +Name2, -MutualActivities)
find_mutual_activities(Name1, Name2, MutualActivities) :-
	likes(Name1, List1,_),
	likes(Name2, List2,_),
	if_mutual(List1, List2, MutualActivities), !.

% Avoid to match with herself/himself.
avoid_match(Name, L, List) :-
	(member(Name,L),
	delete(L, Name, List));
	(List = L).
	
% To control gender is okay.
if_genderokay(Expect, TargetList) :-
	findall(X, (glanian(X, G, _),member(G,Expect)), TargetList).
	
% To find glanian distances for a given list.
find_gdistances(_,[],[]).
find_gdistances(Name,[H1|T1], Pairs) :-
	find_gdistances(Name,T1,Pair),
	glanian_distance(Name,H1,D),
	append([D-H1],Pair, Pairs).

% predicate 3.6
% find_possible_targets(+Name, -Distances, -TargetList) 
find_possible_targets(Name, Distances, TargetList) :-
	expects(Name, Expectgenders, _),
	if_genderokay( Expectgenders, TargetList1),
	avoid_match(Name, TargetList1, TargetList2),
	find_gdistances(Name, TargetList2, Pair),
	keysort(Pair, Pairs),
	pairs_keys_values(Pairs, Distances, TargetList),!.

% To find weighted glanian distances for a given list.
find_wgdistances(_,[],[]).
find_wgdistances(Name, [H1|T1], Pairs):-
	find_wgdistances(Name,T1,Pair),
	weighted_glanian_distance(Name,H1,D),
	append([D-H1],Pair, Pairs).

% predicate 3.7
% find_weighted_targets(+Name, -Distances, -TargetList)
find_weighted_targets(Name, Distances, TargetList) :-
	expects(Name, Expectgenders, _),
	if_genderokay( Expectgenders, TargetList1),
	avoid_match(Name, TargetList1, TargetList2),
	find_wgdistances(Name, TargetList2, Pair),
	keysort(Pair, Pairs),
	pairs_keys_values(Pairs, Distances, TargetList),!.

% To remove old relations from targetlist.
remove_old(L,[],L).
remove_old(L1,[H|T],List) :-
	remove_old(L1, T, L),
	delete(L, H, List).

% To check if they have an old relation.
check_oldrelation(Name, L1, L2) :-
	findall(X, (member(X,L1),(old_relation([X,Name]);old_relation([Name,X]))) , OldRelations),
	remove_old(L1,OldRelations, L2).

% To check features is in limits of Name.
if_limit([],[]).
if_limit([H1|T1], [H2|T2]) :-
	if_limit(T1, T2),
	((H2 == []);
	([Down|X] = H2,
	[Up|_] = X,
	<(H1,Up),
	<(Down,H1))).
	
% To find limits of Name and features of Targets.
check_limit(_,[],[]).
check_limit(Name, [H|T], L2) :-
	check_limit(Name, T, L),
	dislikes(Name,_,_,Limit),
	glanian(H,_,Features),
	((if_limit(Features, Limit),
	append([H], L, L2));
	L2 = L).

% To find number of conflicts in liked activities of someone and disliked activities of another.
notConflict([],_,0).
notConflict([H1|T1], LikedA, Num) :-
	notConflict(T1, LikedA, Num1),
	((member(H1, LikedA),
	Num is Num1+1);(Num is Num1)).

% To find liked or disliked activity lists of given people.
check_activity(_,[],[]).
check_activity(Name, [H|T], L2) :-
	check_activity(Name, T, L), 
	dislikes(Name,DA,_,_), 
	likes(H,LA,_),
	notConflict(DA,LA,Num),
	((Num<3, append([H],L, L2));(L2 = L)).

% Finds and pairs liked activities can done in given city.
date_arrange_in_city_for_an_activity(_,_,_,_,[],[]).
date_arrange_in_city_for_an_activity(Name, Aday, D, City, [Activity|Tail], Pairs):-
	date_arrange_in_city_for_an_activity(Name, Aday, D, City, Tail, Pair),
	dislikes(Name, DA,_,_),
	((member(Activity, DA), Pairs = Pair);
	append([D-Aday-Activity-City], Pair, Pairs)).
	
% Finds and pairs liked activities if done in given city.
date_arrange_in_other_cities_for_liked_activies(_,_,_,_,[],[]).
date_arrange_in_other_cities_for_liked_activies(Name,Aday, D, City, [Activity|Tail], Pairs) :-
	date_arrange_in_other_cities_for_liked_activies(Name, Aday, D, City, Tail, Pair),
	city(City,_,AL),
	((member(Activity, AL),
	append([D-Aday-Activity-City], Pair, Pairs));
	(Pairs = Pair)).
	
% Find cities which Name and Aday can date.
date_arrange_in_city(_,_,_,[],[]).
date_arrange_in_city(Name, Aday, D, [City|T] ,Pairs) :-
	date_arrange_in_city(Name, Aday, D, T, Pair),
	dislikes(Name,_,DC,_),
	( (member(City, DC), Pairs = Pair);
	(city(City,_,AL),
	date_arrange_in_city_for_an_activity(Name, Aday, D, City, AL, P),
	append(P,Pair,Pairs))).

% Find cities which Name and Aday can do a liked activity.
date_arrange_in_other_cities(_,_,_,[],_,[]).
date_arrange_in_other_cities(Name, Aday, D, [City|T], LikedCities, Pairs) :-
	date_arrange_in_other_cities(Name, Aday, D, T, LikedCities, Pair),
	((member(City, LikedCities),Pairs = Pair);
	(dislikes(Name,_,DC,_),
	((member(City, DC),Pairs = Pair);
	likes(Name,LA,_),
	date_arrange_in_other_cities_for_liked_activies(Name, Aday, D, City, LA, P),
	append(P, Pair, Pairs)))
	).

% To check all possibilities.
date_arrange(_,[],[]).
date_arrange(Name,[FirstAday|T], Pairs) :-
	date_arrange(Name,T,Pair),
	weighted_glanian_distance(Name, FirstAday, D),
	
	find_possible_cities(Name, LikedCities),
	date_arrange_in_city(Name, FirstAday, D, LikedCities, P1),
	
	merge_possible_cities(Name, FirstAday, CityList),
	date_arrange_in_other_cities(Name, FirstAday, D, CityList, LikedCities, P2),
	concatenate(P1, P2, P3),
	concatenate(P3, Pair, Pairs).
	
% predicate 3.8
% find_my_best_target(+Name, -Distances, -Activities, -Cities, -TargetList)
find_my_best_target(Name, Distances, Activities, Cities, TargetList) :-
	expects(Name, Expectgenders, _),
	if_genderokay( Expectgenders, L11),
	avoid_match(Name, L11, L1),
	check_oldrelation(Name, L1, L2),
	check_limit(Name, L2, L3),
	check_activity(Name, L3, L4),
	date_arrange(Name, L4, Pair),
	keysort(Pair, Pairs),
	pairs_keys_values(Pairs, DTA, Cities),
	keysort(DTA, DTAS),
	pairs_keys_values(DTAS, DT, Activities),
	keysort(DT, DTS),
	pairs_keys_values(DTS, Distances, TargetList),!.

% Check targetlist if they match with Name or not.
targetlist([],_,[]).
targetlist([T|Tail], Name, List2) :-
	targetlist(Tail, Name, L),
	NameL = [Name],
	check_limit(T,NameL,L1),
	check_activity(T, L1, L2), 
	((expects(T,Gender,_), glanian(Name,G,_), member(G, Gender),((L2==[],List2=L);append([T],L, List2)));
	(List2 = L)).
	
% Finds and pairs liked activities can done in given city for matches.
date_arrange_in_city_for_an_activity_match(_,_,_,_,[],[]).
date_arrange_in_city_for_an_activity_match(Name, Aday, D, City, [Activity|Tail], Pairs):-
	date_arrange_in_city_for_an_activity_match(Name, Aday, D, City, Tail, Pair),
	dislikes(Name, DA1,_,_),
	dislikes(Aday, DA2,_,_),
	((member(Activity, DA1), Pairs = Pair);
	(member(Activity, DA2), Pairs = Pair);
	(append([D-Aday-Activity-City], Pair, Pairs))).
	
% Find cities which Name and Aday can date for matches.
date_arrange_in_city_match(_,_,_,[],[]).
date_arrange_in_city_match(Name, Aday, D, [City|T] ,Pairs) :-
	date_arrange_in_city_match(Name, Aday, D, T, Pair),
	dislikes(Name,_,DC1,_),
	dislikes(Aday,_,DC2,_),
	((member(City, DC1), Pairs = Pair);
	(member(City, DC2), Pairs = Pair);
	(city(City,_,AL),
	date_arrange_in_city_for_an_activity_match(Name, Aday, D, City, AL, P),
	append(P,Pair,Pairs))).	

% Finds and pairs liked activities if done in given city for matches.
date_arrange_in_other_cities_for_liked_activies_match(_,_,_,_,[],[]).
date_arrange_in_other_cities_for_liked_activies_match(Name,Aday, D, City, [Activity|Tail], Pairs) :-
	date_arrange_in_other_cities_for_liked_activies_match(Name, Aday, D, City, Tail, Pair),
	city(City,_,AL),
	((member(Activity, AL),dislikes(Name,DA1,_,_), dislikes(Aday,DA2,_,_),
	((member(Activity, DA1),Pairs = Pair);
	(member(Activity, DA2), Pairs = Pair);
	(append([D-Aday-Activity-City], Pair, Pairs))));
	(Pairs = Pair)).
	
% Find cities which Name and Aday can do a liked activity for matches.
date_arrange_in_other_cities_match(_,_,_,[],_,[]).
date_arrange_in_other_cities_match(Name, Aday, D, [City|T], LikedCities, Pairs) :-
	date_arrange_in_other_cities_match(Name, Aday, D, T, LikedCities, Pair),
	((member(City, LikedCities),Pairs = Pair);
	(dislikes(Name,_,DC1,_), dislikes(Aday,_,DC2,_),
	((member(City, DC1),Pairs = Pair);
	(member(City, DC2), Pairs = Pair);
	(find_possible_cities(Name, LC1),
	((member(City,LC1),
	likes(Aday, LA1,_),
	date_arrange_in_other_cities_for_liked_activies_match(Name, Aday, D, City, LA1, P),
	append(P, Pair, Pairs));		
	(likes(Name, LA2,_), 
	date_arrange_in_other_cities_for_liked_activies_match(Name, Aday, D, City, LA2, P),
	append(P, Pair, Pairs))))))).
	
% To check all possibilities for matches.
date_arrange_match(_,[],[]).
date_arrange_match(Name,[FirstAday|T], Pairs) :-
	date_arrange_match(Name,T,Pair),
	weighted_glanian_distance(Name, FirstAday, D1),
	weighted_glanian_distance(FirstAday, Name, D2),
	D is (D1+D2)/2,
	find_possible_cities(Name,LC1),
	find_possible_cities(FirstAday, LC2),
	intersection(LC1, LC2, LC),
	date_arrange_in_city_match(Name, FirstAday, D, LC, P1),
	merge_possible_cities(Name, FirstAday, CityList),
	date_arrange_in_other_cities_match(Name, FirstAday, D, CityList, LC, P2),
	concatenate(P1, P2, P),
	concatenate(P, Pair, Pairs).
 
% predicate 3.9
% find_my_best_match(+Name, -Distances, -Activities, -Cities, -Targets) 
find_my_best_match(Name, Distance, Activities, Cities, Targets) :-
	expects(Name, Expectgenders,_),
	if_genderokay(Expectgenders, L11),
	avoid_match(Name, L11, L1),
	check_oldrelation(Name, L1, L2),
	check_limit(Name, L2, L3),
	check_activity(Name, L3, L4),
	targetlist(L4, Name, L5),
	date_arrange_match(Name, L5, Pair),
	keysort(Pair, Pairs),
	pairs_keys_values(Pairs, DTA, Cities),
	keysort(DTA, DTAS),
	pairs_keys_values(DTAS, DT, Activities),
	keysort(DT, DTS),
	pairs_keys_values(DTS, Distance, Targets),!.