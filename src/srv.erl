%%%-------------------------------------------------------------------
%%% @author zero
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. gru 2015 11:15
%%%-------------------------------------------------------------------
-module(srv).
-author("zero").

%% API
-export([start_link/0, init/0, loop/1, get/1, rndstr/0,
  renderDock/0, renderBook/0, createNameList/1, parseAddEmail/1,
  parseAddContact/1]).

rndstr() ->
  "rrr".

start_link() ->
  register(servant, spawn_link(?MODULE, init, [])).

init() ->
  Ab = abook:createAddressBook(),
  Ab1 = abook:addContact("Antonius", "Pius", Ab),
  Ab2 = abook:addContactA("Oktawian", "August",
    ["101101101", "222222222"],
    ["okt@mail.com", "okt2@mail.com"], Ab1),
  Ab3 = abook:addContactA("Marek", "Aureliusz",
    ["101551101", "666222222"],
    ["aur@mail.com"], Ab2),
  Ab4 = abook:addContactA("Flawiusz", "Sewer",
    ["1534101101", "332222222"],
    ["ffft@mail.com", "frfrt2@mail.com"], Ab3),
  Ab5 = abook:addContactA("Lucjusz", "Werus",
    ["1053201101", "99222222"],
    ["luc@mail.com", "wrer2@mail.com"], Ab4),
  Ab6 = abook:addContactA("Romulus", "Augustulus",
    ["8888101101", "231222222"],
    ["n"], Ab5),
  io:format("book: ~w", [Ab6]),
  loop(Ab6).


loop(Ab) ->
  receive
    {get, PID} ->
      PID ! {book, Ab},
      loop(Ab);
    {addC, N, S} ->
      Ab1 = abook:addContact(N, S, Ab),
      case Ab1 of
        {abook, _} -> loop(Ab1);
        _ -> loop(Ab)
      end;
    {addE, N, S, E} ->
      Ab1 = abook:addEmail(N, S, E, Ab),
      case Ab1 of
        {abook, _} -> loop(Ab1);
        _ -> loop(Ab)
      end;
    _ -> loop(Ab)
  end.

get(PID) ->
  servant ! {get, PID}.

addEmail(N, S, E) ->
  servant ! {addE, N, S, E}.

addContact(N, S) ->
  servant ! {addC, N, S}.

createNameList(Al) ->
  io:format("person_list: ~w~n~n~n", [Al]),
  [{tr, [], [
    {td, [], element(2, X)},                          %name
    {td, [], element(3, X)},                          %surname
    {td, [], {
      ul, [], [{li, [], Y} || Y <- element(4, X)]   %phones
    }},
    {td, [], {
      ul, [], [{li, [], Y} || Y <- element(5, X)]   %emails
    }}
  ]} || X <- Al].

parseAddEmail(Dat) ->
  [{"name", Np}, {"surname", Sp}, {"email", Ep} | _] = Dat,
  io:format("addEmail : ~s ~s ~s ~n~n",[Np, Sp, Ep]),
  addEmail(Np, Sp, Ep),
  {page, "/get.yaws"}.

parseAddContact(Dat) ->
  [{"name", Np}, {"surname", Sp} | _] = Dat,
  io:format("addEmail : ~s ~s~n~n",[Np, Sp]),
  addContact(Np, Sp),
  {page, "/get.yaws"}.

renderBook() ->
  servant ! {get, self()},
  receive
    {book, Ab} ->
      {_, Al} = Ab,
      Nl = createNameList(Al),
      io:format("formated list: ~w~n~n~n",[Nl]),
      {ehtml, {'div', [{id, "main"}], [{table, [{id, "book"}],
        [ {tr, [{id, "head"}], [
          {td, [], "Name"},
          {td, [], "Surname"},
          {td, [], "PhoneNumbers"},
          {td, [], "Emails"}
        ]} |Nl]
      }]}};
    _ ->
      error
  end.


renderDock() ->
  AddEmailForm = {form, [{action, "/addE.yaws"}, {method, "post"}], [
    {label, [], "Name"},
    {input, [{type, "text"}, {name, "name"}]}, {br, []},
    {label, [], "Surname"},
    {input, [{type, "text"}, {name, "surname"}]}, {br, []},
    {label, [], "Email"},
    {input, [{type, "text"}, {name, "email"}]}, {br, []},
    {input, [{type, "submit"}, {name, "submit"}]}, {br, []}
  ]},

  AddContactForm = {form, [{action, "/addC.yaws"}, {method, "post"}], [
    {label, [], "Name"},
    {input, [{type, "text"}, {name, "name"}]}, {br, []},
    {label, [], "Surname"},
    {input, [{type, "text"}, {name, "surname"}]}, {br, []},
    {input, [{type, "submit"}, {name, "submit"}]}, {br, []}
  ]},

  {ehtml, {'div', [{id, "dock"}], [
    {h4, [], "Add Contact"}, AddContactForm,
    {h4, [], "Add Email"}, AddEmailForm
  ]}}.




