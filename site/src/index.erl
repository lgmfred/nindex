%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file="./site/templates/bare.html"}.

title() -> "My Web Links".

%% *********************************************
%% Initial State
%% User choice: transition to "add new" form
%% or enter search words and initiate a search
%% *********************************************

body() ->
    [
     #p{text="Add new weblink or search for existing links"},
     #button{text="Add New", click=#redirect{url="/add_edit/new"}},
     #br{},
     #textbox{id=search_words, class=standard},
     #button{id=retrieve, text="Search", postback=search},
     #button{text="Show All", postback=show_all},
     #hr{},
%% *********************************************
%% State 3: Search results displayed here
%% *********************************************
     #panel{id=search_results}
    ].

event(search) ->
    return_search_results();
event(show_all) ->
    show_all();
event({delete, LinkID, Linkwrapperid}) ->
    delete(LinkID, Linkwrapperid).

delete(LinkID, Linkwrapperid) ->
    ni_links:delete_link(LinkID),
    wf:remove(Linkwrapperid).

show_all() ->
    Links = ni_links:get_all(),
    AllBody = draw_links(Links),
    wf:update(search_results, AllBody).

return_search_results() ->
    SearchString = wf:q(search_words),
    Links = ni_search:search(SearchString),
    SearchResultBody = draw_links(Links),
    wf:update(search_results, SearchResultBody).

draw_links(Links) ->
    #panel{id=show_links, body=[
        #p{text="Return search results"},
        [draw_link(Link) || Link <- Links]
    ]}.

draw_link(Weblink) ->
    LinkID = ni_links:id(Weblink),
    Text = ni_links:descriptor(Weblink),
    Url = ni_links:url(Weblink),
    EditUrl = "add_edit/" ++ wf:to_list(LinkID),
    Menuid = wf:temp_id(),
    Linkwrapperid = wf:temp_id(),
    #panel{id=Linkwrapperid, body=[
        #link{
            text=Text,
            click=#toggle{target=Menuid}
        },
        #panel{id=Menuid, style="display: none", body=[
            #link{text="view", url=Url},
            " | ",
            #link{text="edit", url=EditUrl},
            " | ",
            #link{text="delete", postback={delete, LinkID, Linkwrapperid}}
        ]}
    ]}.



