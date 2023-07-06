-module(ni_joedb).
%% Expected API exports
-export([init_db/0,
         get_all/0,
         get_link/1,
         save_link/1,
         update_link/2,
         delete_link/1,
         new/3,
         id/1, id/2,
         topic/1, topic/2,
         descriptor/1, descriptor/2,
         url/1, url/2
        ]).

%%––––––––––––––––––––––-
%% @doc joeDB
%%––––––––––––––––––––––-
seed() ->
    [[{id, create_id()},
      {topic,"Erlang"},
      {descriptor,"Erlang Programming Language"},
      {url,"http://wwww.erlang.org"}],
     [{id, create_id()},
      {topic,"Nitrogen"},
      {descriptor,"Nitrogen Home Page"},
      {url,"http://nitrogenproject.com/"}]].

create_id() ->
    rand:uniform(999999999999).

get_all() ->
    {ok, Bin} = file:read_file("joedb"),
    binary_to_term(Bin).

put_all(Data) ->
    Bin = term_to_binary(Data),
    file:write_file("joedb", Bin).

init_db() ->
    put_all(seed()).


%% API functions
new(Topic, Descriptor, Url) ->
    [{topic,Topic},
     {descriptor,Descriptor},
     {url,Url}].

id(Weblink) ->
    proplists:get_value(id, Weblink).

topic(Weblink) ->
    proplists:get_value(topic, Weblink).

descriptor(Weblink) ->
    proplists:get_value(descriptor, Weblink).

url(Weblink) ->
    proplists:get_value(url, Weblink).

set_value(Weblink, Key, Value) ->
    Weblink2 = proplists:delete(Key, Weblink),
    [{Key, Value} | Weblink2].

id(Weblink, ID) ->
   set_value(Weblink, id, ID). 

topic(Weblink, Topic) ->
   set_value(Weblink, topic, Topic). 

descriptor(Weblink, Descriptor) ->
   set_value(Weblink, descriptor, Descriptor). 

url(Weblink, Url) ->
   set_value(Weblink, url, Url). 

get_link(ID) ->
    Data = get_all(),
    case [Rec || Rec <- Data, id(Rec) =:= ID] of
        [] -> new;
        [Weblink | _] -> Weblink
    end.

save_link(Weblink) ->
    Data = get_all(),
    Data1 = case id(Weblink) of
        undefined -> insert_link(Weblink, Data);
        _ -> update_link(Weblink, Data)
    end,
    put_all(Data1).

insert_link(Weblink, Data) ->
    Weblink2 = id(Weblink, create_id()),
    [Weblink2|Data].

update_link(NewWeblink, Data) ->
    lists:map(fun(Weblink) ->
        case id(Weblink) == id(NewWeblink) of
            true -> NewWeblink;
            false -> Weblink
        end
    end, Data).

delete_link(ID) ->
    Data = get_all(),
    Data1 = [WL || WL <- Data, id(WL)=/=ID],
    put_all(Data1).
