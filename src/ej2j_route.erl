%% Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Stanza routing

-module(ej2j_route).

-export([init/0, add_entry/2, get_entry/2, get_route/3]).

-export_type([route_db/0]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

-include("ej2j.hrl").

-type route_db() :: list().

-spec init() -> route_db().
init() ->
    [].

-spec add_entry(route_db(), tuple()) -> route_db().
add_entry(Routes, {OwnerJID, ForeignJID, ClientSession, ServerSession}) ->
    Route1 = {exmpp_jid:to_list(OwnerJID), ForeignJID, {client, ClientSession}},
    Route2 = {exmpp_jid:to_list(ForeignJID), OwnerJID, {server, ServerSession}},
    Route3 = {exmpp_jid:bare_to_list(OwnerJID), exmpp_jid:bare(ForeignJID), {client, ClientSession}},
    Route4 = {exmpp_jid:bare_to_list(ForeignJID), exmpp_jid:bare(OwnerJID), {server, ServerSession}},
    [Route4|[Route3|[Route2|[Route1|Routes]]]].

-spec get_entry(route_db(), any()) -> any() | false.
get_entry(Routes, JID) ->
    lists:keyfind(exmpp_jid:to_list(JID), 1, Routes).

-spec get_route(route_db(), any(), any()) -> tuple() | false.
get_route(Routes, From, To) when From == To ->
    case get_entry(Routes, From) of	
	{_From, NewJID, Route} -> {Route, NewJID, NewJID};
	false -> 
	    case get_entry(Routes, To) of
		{_To, NewJID, Route} -> {Route, NewJID, NewJID};
		false -> false
	    end
    end;

get_route(Routes, From, To) ->
    case get_entry(Routes, From) of
    	{_From, NewFrom, Route} ->
	    [Node, Domain] = string:tokens(exmpp_jid:node_as_list(To), "%"),
	    Resource = exmpp_jid:resource_as_list(To),
	    NewTo = exmpp_jid:make(Node, Domain, Resource),
	    {Route, NewFrom, NewTo};
	false -> 
	    case get_entry(Routes, To) of
		{_To, NewTo, Route} ->
		    Node = string:join([exmpp_jid:node_as_list(From), exmpp_jid:domain_as_list(From)], "%"),
		    Domain = ?COMPONENT,
		    Resource = exmpp_jid:resource_as_list(From),
		    NewFrom = exmpp_jid:make(Node, Domain, Resource),
		    {Route, NewFrom, NewTo};
		false -> false
	    end
    end.
