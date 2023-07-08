%% vim: ft=nitrogen
-module(add_edit).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file="./site/templates/bare.html"}.

title() -> "Add New/Edit Web Link".

get_linkid() ->
    case wf:path_info() of
        "new" -> new;
        ID -> wf:to_integer(ID)
    end.

body() ->
    LinkID = get_linkid(),
    Weblink = ni_links:get_link(LinkID),
    form(Weblink).

%% **********************************************
%% State 2:
%% User enters new link info;
%% clicks Save
%% System saves new link info;
%% transistions back to initial state
%% **********************************************

form(new) ->
    form(new, "", "", "");
form(Weblink) ->
    ID = ni_links:id(Weblink),
    Topic = ni_links:topic(Weblink),
    Desc = ni_links:descriptor(Weblink),
    Url = ni_links:url(Weblink),
    form(ID, Topic, Desc, Url).

form(ID, Topic, Desc, Url) ->
    wf:defer(save_link, topic, #validate{validators=[
        #is_required{text="Topic required"}]}),
    wf:defer(save_link, descriptor, #validate{validators=[
        #is_required{text="Descriptive text required"}]}),
    wf:defer(save_link, url, #validate{validators=[
        #is_required{text="URL required"},
        #custom{text="URL must be properly formed",
                function=fun custom_validator/2,
                tag=custom_validator_tag}
    ]}),
    [
     #h3{text="Create or Edit Web Link"},
     #label{text="Topic"},
     #textbox{id=topic, text=Topic},
     #label{text="Descriptive Text"},
     #textbox{id=descriptor, text=Desc},
     #label{text="URL"},
     #textbox{id=url, text=Url},
     #br{},
     #button{
        id=save_link,
        text="Save",
        postback={save, ID}
     },
     #link{style="margin-left: 10px", text="Cancel", url="/"}
    ].

custom_validator(_Tag, Url) ->
    case http_uri:parse(Url) of
        {ok, _} -> true;
        _ -> false
    end.
    
event({save, Linkid}) ->
    save(Linkid).

save(new) ->
    [Topic, Desc, Url] = wf:mq([topic, descriptor, url]),
    Weblink = ni_links:new(Topic, Desc, Url),
    save_and_redirect(Weblink);
save(Linkid) ->
    [Topic, Desc, Url] = wf:mq([topic, descriptor, url]),
    Weblink = ni_links:get_link(Linkid),
    Weblink2 = ni_links:topic(Weblink, Topic),
    Weblink3 = ni_links:descriptor(Weblink2, Desc),
    Weblink4 = ni_links:url(Weblink3, Url),
    save_and_redirect(Weblink4).

save_and_redirect(Weblink) ->
    ni_links:save_link(Weblink),
    wf:redirect("/").

%% *********************************************
%% State 3
%% System displays search results
%% *********************************************


%% *********************************************
%% State 4
%% System displays record
%% User selects view, edit, or delete
%% If view, display web page
%% If edit, transition to edit_add.erl state 6
%% If delete, delete record; transition to
%% State 1
%% *********************************************


