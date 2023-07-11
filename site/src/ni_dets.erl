-module(ni_dets).
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

-record(weblink, {id, topic, descriptor, url}).

init_db() ->
    open_db(),
    close_db().

open_db() ->
    DB = dets_nindex,
    Opts = [{type, set}, {keypos, #weblink.id}],
    {ok, DB} = dets:open_file(DB, Opts).

close_db() ->
    dets:close(dets_nindex).

get_all() ->
    open_db(),
    Links = dets:match_object(dets_nindex, '_'),
    close_db(),
    Links.

get_link(ID) ->
    open_db(),
    Link = case dets:loopup(dets_nindex, ID) of
        [L] -> L;
        [] -> new
    end,
    close_db(),
    Link.

save_link(Link = #weblink{id=undefined}) ->
    save_link(Link#weblink{id=create_id()});
save_link(Link) ->
    open_db(),
    ok = dets:insert(dets_nindex, Link),
    close_db().

create_id() ->
    rand:uniform(9999999999999999999999).

delete_link(ID) ->
    open_db(),
    dets:delete(dets_nindex, ID),
    close_db().

new(Topic, Descriptor, Url) ->
    #weblink{
       topic=Topic,
       descriptor=Descriptor,
       url=Url
    }.

id(#weblink{id=ID}) -> ID.
id(W, ID) -> W#weblink{id=ID}.

descriptor(#weblink{descriptor=Desc}) -> Desc.
descriptor(W, Desc) -> W#weblink{descriptor=Desc}.

topic(#weblink{topic=Topic}) -> Topic.
topic(W, Topic) -> W#weblink{topic=Topic}.

url(#weblink{url=Url}) -> Url.
url(W, Url) -> W#weblink{url=Url}.
