%%%-------------------------------------------------------------------
%%% @author zero
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. lis 2015 21:06
%%%-------------------------------------------------------------------
-module(abook).
-author("zero").

%% API
-export([createAddressBook/0, addContact/3, getByEmail/2, getByName/2, getBySurname/2,
  addEmail/4, fillRandomly/1, removePerson/2, addContactE/4, addPhone/4, addContactP/4,
  getByPhone/2, removeContact/3, removeEmail/2, getEmails/3, getPhones/3, setCompany/5,
  getByCompany/2, addContactA/5
]).

-record (abook, {list = []}).

-record (person, {name = undefined, surname = undefined, phone = [],
  email = [], company = undefined, job = undefined}).

createAddressBook() ->
  #abook{}.


addContact(N, S, Ab) ->
  case [X || X <- Ab#abook.list, X#person.name == N, X#person.surname == S] of
    [] ->
      Ab#abook{list = [#person{name = N, surname = S} | Ab#abook.list]};
    [_] ->
      {error, "Person already exists."};
    _ ->
      {error, "Unknown error (1)"}
  end.

addContactE(N, S, E, Ab) ->
  Ab#abook{list = [#person{name = N, surname = S, email = E} | Ab#abook.list]}.

addContactP(N, S, P, Ab) ->
  Ab#abook{list = [#person{name = N, surname = S, phone = P} | Ab#abook.list]}.

addContactA(N, S, P, E, Ab) ->
  Ab#abook{list = [#person{name = N, surname = S, phone = P, email = E} | Ab#abook.list]}.

removeContact(N, S, Ab) ->
  NameMatch = fun
                (Per) when Per#person.name == N ->
                  true;
                (_) ->
                  false
              end,
  [Match] = lists:filter(NameMatch, getBySurname(S, Ab)),
  abook:removePerson(Match, Ab).

removeEmail(E, Ab) ->
  [Per] = getByEmail(E, Ab),
  Ab2 = removePerson(Per, Ab),
  PerSet = setEmail(lists:filter(fun(X) -> X /= E end, Per#person.email), Per),
  Ab2#abook{list = [PerSet | Ab2#abook.list]}.

removePhone(P, Ab) ->
  [Per] = getByPhone(P, Ab),
  Ab2 = removePerson(Per, Ab),
  PerSet = setPhone(lists:filter(fun(X) -> X /= P end, Per#person.phone), Per),
  Ab2#abook{list = [PerSet | Ab2#abook.list]}.

getEmails(N, S, Ab) ->
  NameMatch = fun
                (Per) when Per#person.name == N ->
                  true;
                (_) ->
                  false
              end,
  [Match] = lists:filter(NameMatch, getBySurname(S, Ab)),
  Match#person.email.

getPhones(N, S, Ab) ->
  NameMatch = fun
                (Per) when Per#person.name == N ->
                  true;
                (_) ->
                  false
              end,
  [Match] = lists:filter(NameMatch, getBySurname(S, Ab)),
  Match#person.phone.

%----------------------------------------------------------------------------------------

setCompany(N, S, C, J, Ab) ->
  NameMatch = fun
                (Per) when Per#person.name == N ->
                  true;
                (_) ->
                  false
              end,
  [Match] = lists:filter(NameMatch, getBySurname(S, Ab)),
  Ab2 = removePerson(Match, Ab),
  PerSet = Match#person{company = C, job = J},
  Ab2#abook{list = [PerSet | Ab2#abook.list]}.

getByCompany(C, Ab) ->
  [X || X <- Ab#abook.list, X#person.company == C].

%----------------------------------------------------------------------------------------

addEmail(N, S, E, Ab) ->
  case getByEmail(E, Ab) of
    [H | _] ->
      {error, "Email already exists."};
    [] ->
      NameMatch = fun
                    (Per) when Per#person.name == N ->
                      true;
                    (_) ->
                      false
                  end,
      Match = lists:filter(NameMatch, getBySurname(S, Ab)),
      addEmail_(N, S, E, Match, Ab);

    _ ->
      {error, "Unknown error (1)"}
  end.

addEmail_(N, S, E, [MatchPerson], Ab) ->
  Ab2 = removePerson(MatchPerson, Ab),
  PersonSet = setEmail([E | MatchPerson#person.email], MatchPerson),
  Ab2#abook{list = [PersonSet | Ab2#abook.list]};

addEmail_(N, S, E, [], Ab) ->
  addContactE(N, S, [E], Ab);

addEmail_(_, _, _, _, _) ->
  {error, "Unknown error (2)"}.

%----------------------------------------------------------------------------------------

addPhone(N, S, P, Ab) ->
  case getByPhone(P, Ab) of
    [H | _] ->
      {error, "Phone already exists."};
    [] ->
      NameMatch = fun
                    (Per) when Per#person.name == N ->
                      true;
                    (_) ->
                      false
                  end,
      Match = lists:filter(NameMatch, getBySurname(S, Ab)),
      addPhone_(N, S, P, Match, Ab);

    _ ->
      {error, "Unknown error (1)"}
  end.

addPhone_(N, S, Ph, [MatchPerson], Ab) ->
  Ab2 = removePerson(MatchPerson, Ab),
  PersonSet = setPhone([Ph | MatchPerson#person.phone], MatchPerson),
  Ab2#abook{list = [PersonSet | Ab2#abook.list]};

addPhone_(N, S, P, [], Ab) ->
  addContactP(N, S, [P], Ab);

addPhone_(_, _, _, _, _) ->
  {error, "Unknown error (2)"}.

%----------------------------------------------------------------------------------------

fillRandomly(Ab) ->
  Ab1 = addContactE("jan", "dzban", ["jan@k.pl"], Ab),
  Ab2 = addContactE("jan", "kran", ["jankran@kk.pl", "jk@kk.pl"], Ab1),
  Ab3 = addContact("jan", "kuk", Ab2),
  Ab4 = addContact("janusz", "kek", Ab3),
  Ab5 = addContact("janina", "kok", Ab4),
  Ab6 = addContact("janusz", "kok", Ab5),
  Ab7 = addContact("jan", "kok", Ab6).
%---

removePerson(P, Ab) ->
  Ab#abook{list = [X || X <- Ab#abook.list, X /= P]}.

setEmail(E, P) ->
  P#person {email = E}.

setPhone(Ph, P) ->
  P#person {phone = Ph}.

getByEmail(E, Ab) ->
  Match = [X || X <- Ab#abook.list, lists:filter(fun(Y) -> Y==E end, X#person.email)  /= []],
  case Match of
    [H | _] -> Match;
    _ -> []
  end.

getByPhone(P, Ab) ->
  Match = [X || X <- Ab#abook.list, lists:filter(fun(Y) -> Y==P end, X#person.phone)  /= []],
  case Match of
    [H | _] -> Match;
    _ -> []
  end.

getByName(N, Ab) ->
  Match = [X || X <- Ab#abook.list, X#person.name == N],
  case Match of
    [H | _] -> Match;
    _ -> []
  end.

getBySurname(S, Ab) ->
  Match = [X || X <- Ab#abook.list, X#person.surname == S],
  case Match of
    [H | _] -> Match;
    _ -> []
  end.
