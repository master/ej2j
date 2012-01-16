%% Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Stanza routing

-module(ej2j_route).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([add/4, get/2, del/1, free/0]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("ej2j.hrl").

-record(state, {route_db}).

-type route_db() :: ets:tid().

%% Public API
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks

-spec init([]) -> {ok, #state{}}. %route_db().
init([]) ->
    Routes = ets:new('route', [bag]),
    {ok, #state{route_db=Routes}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}} |
                                             {stop, any(), any(), #state{}}.
handle_call({add, OwnerJID, ForeignJID, ClientSession, ServerSession},
            _From, #state{route_db=Routes} = State) ->
    Ref = make_ref(),
    ets:insert(Routes, {exmpp_jid:to_list(OwnerJID), 
                        ForeignJID, 
                        {client, ClientSession}, 
                        Ref}),
    ets:insert(Routes, {exmpp_jid:to_list(ForeignJID),
                        OwnerJID,
                        {server, ServerSession},
                        Ref}),
    ets:insert(Routes, {exmpp_jid:bare_to_list(OwnerJID),
                        exmpp_jid:bare(ForeignJID), 
                        {client, ClientSession}, 
                        Ref}),
    ets:insert(Routes, {exmpp_jid:bare_to_list(ForeignJID), 
                        exmpp_jid:bare(OwnerJID), 
                        {server, ServerSession}, 
                        Ref}),
    {reply, ok, State};

handle_call(free, _From, #state{route_db=Routes} = State) ->
    ets:delete(Routes),
    NewRoutes = ets:new('route', [bag]),
    {reply, ok, State#state{route_db=NewRoutes}};

handle_call({del, Key}, _From,
            #state{route_db=Routes} = State) when is_list(Key) ->
    Refs = lists:flatten(ets:match(Routes, {Key, '_', '_', '$1'})),    
    del_entry(Routes, Refs),
    {reply, ok, State};

handle_call({del, Key}, _From,
            #state{route_db=Routes} = State) when is_pid(Key) ->
    Refs = lists:flatten(ets:match(Routes, {'_', '_', {client, Key}, '$1'})),
    del_entry(Routes, Refs),
    {reply, ok, State};

handle_call({get, From, To}, _From, #state{route_db=Routes} = State) ->
    FromStr = exmpp_jid:to_list(From),
    ToStr = exmpp_jid:to_list(To),
    Records = get_entry(Routes, FromStr) ++ get_entry(Routes, ToStr),
    Result = make(Records, From, To, FromStr, ToStr, []),
    {reply, Result, State};

handle_call(get_state, _From, #state{route_db=Routes} = State) ->
    {reply, {state, {route_db, Routes}}, State};

handle_call(_Msg, _From, State) ->
    {reply, unexpected, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Msg, State) ->
    {noreply, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% External API functions
-spec free() -> ok.
free() ->
    gen_server:call(?MODULE, free).

-spec add(any(), any(), pid(), pid()) -> ok.
add(OwnerJID, ForeignJID, ClientSession, ServerSession) ->
    gen_server:call(?MODULE,
                    {add, OwnerJID, ForeignJID, ClientSession, ServerSession}).

-spec del(any()) -> ok.
del(Key) ->
    gen_server:call(?MODULE, {del, Key}).

-spec get(any(), any()) -> list().
get(From, To) ->
    gen_server:call(?MODULE, {get, From, To}).

%% Various helpers

-spec get_entry(route_db(), any()) -> list().
get_entry(Routes, Key) when is_list(Key) ->
    ets:lookup(Routes, Key).

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
                     NodeList = case exmpp_jid:node_as_list(To) of
                                    undefined -> "";
                                    Any -> Any
                                end,
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
                     Node = string:join([exmpp_jid:node_as_list(From), 
                                         exmpp_jid:domain_as_list(From)], "%"),
                     Domain = ej2j:get_app_env(component, ?COMPONENT),
                     Resource = exmpp_jid:resource_as_list(From),
                     NewFrom = exmpp_jid:make(Node, Domain, Resource),
                     [{Route, NewFrom, NewTo}|Acc];
                 _ -> Acc
             end,
    make(Tail, From, To, FromStr, ToStr, NewAcc);
make([], _From, _To, _FromStr, _ToStr, Acc) ->
    lists:reverse(Acc).
