-module(ni_pgsql).
-export([init_db/0,
         get_all/0,
         get_link/1,
         save_link/1,
         delete_link/1,
         new/3,
         id/1, id/2,
         topic/1, topic/2,
         descriptor/1, descriptor/2,
         url/1, url/2
]).

init_db() ->
    sql_bridge:start().

get_all() ->
    db:maps("select * from link").

get_link(ID) ->
    case db:map("select * from link where linkid=$1", [ID]) of
        not_found -> new;
        Map -> Map
    end.

save_link(Link) ->
    db:save(link, Link).

delete_link(ID) ->
    db:delete(link, ID).

new(Topic, Descriptor, Url) ->
    #{
      linkid=>0,
      topic=>Topic,
      descriptor=>Descriptor,
      url=>Url
    }.

id(#{linkid := ID}) -> ID.
id(W, ID) -> W#{linkid => ID}.
descriptor(#{descriptor := Desc}) -> Desc.
descriptor(W, Desc) -> W#{descriptor => Desc}.
topic(#{topic := Topic}) -> Topic.
topic(W, Topic) -> W#{topic => Topic}.
url(#{url := Url}) -> Url.
url(W, Url) -> W#{url => Url}.

