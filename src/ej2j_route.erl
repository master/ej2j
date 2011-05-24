%% Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Stanza routing

-module(ej2j_route).

-export([init/0, add/2, get/3, del/2, free/1]).

-export_type([route_db/0]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

-include("ej2j.hrl").

-type route_db() :: ets:tid().

-spec init() -> route_db().
init() ->
    ets:new('route', [bag]).

-spec free(route_db()) -> ok.
free(Routes) ->
    ets:delete(Routes).

-spec add(route_db(), tuple()) -> route_db().
add(Routes, {OwnerJID, ForeignJID, ClientSession, ServerSession}) ->
    Ref = make_ref(),
    ets:insert(Routes, {exmpp_jid:to_list(OwnerJID), ForeignJID, {client, ClientSession}, Ref}),
    ets:insert(Routes, {exmpp_jid:to_list(ForeignJID), OwnerJID, {server, ServerSession}, Ref}),
    ets:insert(Routes, {exmpp_jid:bare_to_list(OwnerJID), exmpp_jid:bare(ForeignJID), {client, ClientSession}, Ref}),
    ets:insert(Routes, {exmpp_jid:bare_to_list(ForeignJID), exmpp_jid:bare(OwnerJID), {server, ServerSession}, Ref}),
    Routes.

-spec get_entry(route_db(), any()) -> list().
get_entry(Routes, Key) when is_list(Key) ->
    ets:lookup(Routes, Key).

-spec del(route_db(), any()) -> route_db().
del(Routes, Key) when is_list(Key) ->
    Refs = lists:flatten(ets:match(Routes, {Key, '_', '_', '$1'})),    
    del_entry(Routes, Refs);
del(Routes, Key) when is_pid(Key) ->
    Refs = lists:flatten(ets:match(Routes, {'_', '_', {client, Key}, '$1'})),
    del_entry(Routes, Refs).

-spec del_entry(route_db(), list()) -> route_db().
del_entry(Routes, [Key|Keys]) when is_reference(Key) ->
    ets:match_delete(Routes, {'_', '_', '_', Key}),
    del_entry(Routes, Keys);
del_entry(Routes, []) ->
    Routes.

-spec make(list(), any(), any(), list(), list(), list()) -> list().
make([Record|Tail], From, To, FromStr, ToStr, Acc) ->
    NewAcc = case Record of
                 {_FromJID, _ToJID, _Route, _Ref} when From == To -> Acc;
                 {FromStr, NewFrom, Route, _Ref} -> 
                     NodeList = exmpp_jid:node_as_list(To),
                     case string:chr(NodeList, $%) of
                         0 ->
                             Acc;
                         _Else  ->
                             [Node, Domain] = string:tokens(NodeList, "%"),
                             Resource = exmpp_jid:resource_as_list(To),
                             NewTo = exmpp_jid:make(Node, Domain, Resource),
                             [{Route, NewFrom, NewTo}|Acc]
                     end;
                 {ToStr, NewTo, Route, _Ref} -> 
                     Node = string:join([exmpp_jid:node_as_list(From), exmpp_jid:domain_as_list(From)], "%"),
                     Domain = ?COMPONENT,
                     Resource = exmpp_jid:resource_as_list(From),
                     NewFrom = exmpp_jid:make(Node, Domain, Resource),
                     [{Route, NewFrom, NewTo}|Acc];
                 _ -> Acc
             end,
    make(Tail, From, To, FromStr, ToStr, NewAcc);
make([], _From, _To, _FromStr, _ToStr, Acc) ->
    lists:reverse(Acc).

-spec get(route_db(), any(), any()) -> list().
get(Routes, From, To) ->
    FromStr = exmpp_jid:to_list(From),
    ToStr = exmpp_jid:to_list(To),
    Records = get_entry(Routes, FromStr) ++ get_entry(Routes, ToStr),
    make(Records, From, To, FromStr, ToStr, []).
