%% Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc XMPP Component

-module(ej2j_comp).

-behaviour(gen_server).

-export([start_link/0, stop/0, start_client/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-define(RESTART_DELAY, 1000).
-define(CHECK_INTERVAL, 300000).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("ej2j.hrl").

-record(state, {session :: pid()}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    timer:sleep(?RESTART_DELAY),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

-spec start_client(tuple(), list(), list()) -> {ok, pid()} | {error, any()}.
start_client(OwnerJID, ForeignJID, Creds) ->
    gen_server:call(?MODULE, {start_client, OwnerJID, ForeignJID, Creds}).

-spec get_routes(exmpp_jid:jid(), exmpp_jid:jid()) -> any().
get_routes(FromJID, ToJID) ->
    gen_server:call(?MODULE, {get_routes, FromJID, ToJID}).

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:send_after(0, self(), state),
    {ok, #state{}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}} | 
                                             {stop, any(), any(), #state{}}.
handle_call(stop, _From, State) ->
    exmpp_component:stop(State#state.session),
    {stop, normal, ok, State};
handle_call({start_client, FromJID, ForeignJID, Creds}, _From,
            #state{session = ServerS} = State) ->
    [User, Domain] = string:tokens(ForeignJID, "@"),
    case client_spawn(User, Domain, Creds) of
	{ok, {ToJID, ClientS}} -> 
            ok = ej2j_route:add(FromJID, ToJID, ClientS, ServerS),
            {reply, {ok, ClientS}, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call({get_routes, FromJID, ToJID}, _From, State) ->
    Routes = ej2j_route:get(FromJID, ToJID),
    {reply, Routes, State};
handle_call(_Msg, _From, State) ->
    {reply, unexpected, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(#received_packet{} = Packet, #state{session = Session} = State) ->
    error_logger:info_msg("Packet received: ~p~n", [Packet]),
    spawn_link(fun() -> process_received_packet(Session, Packet) end),
    {noreply, State};
handle_info(#received_packet{packet_type = Type, raw_packet = Packet}, State) ->
    error_logger:warning_msg("Unknown packet received(~p): ~p~n", [Type, Packet]),
    {noreply, State};
handle_info({'EXIT', Server, _}, #state{session = Server} = State) ->
    timer:sleep(?RESTART_DELAY),
    Session = ej2j_helper:component(),
    ej2j_route:free(),
    {noreply, State#state{session = Session}};
handle_info({'EXIT', Pid, _}, State) ->
    ej2j_route:del(Pid),
    {noreply, State};
handle_info(state, #state{session = undefined} = State) ->
    erlang:send_after(?CHECK_INTERVAL, ?MODULE, state),
    Session = ej2j_helper:component(),
    {noreply, State#state{session = Session}};
handle_info(state, State) ->
    erlang:send_after(?CHECK_INTERVAL, ?MODULE, state),
    {noreply, State};
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

%% Process received packet

-spec process_received_packet(any(), #received_packet{}) -> ok.
process_received_packet(Session, 
                        #received_packet{packet_type = 'iq'} = Packet) ->
    #received_packet{type_attr=Type, raw_packet = IQ} = Packet,
    NS = exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)),
    process_iq(Session, Type, NS, IQ);
process_received_packet(Session, 
                        #received_packet{packet_type = 'presence'} = Packet) ->
    #received_packet{raw_packet = Presence} = Packet,
    process_presence(Session, Presence);
process_received_packet(Session, 
                        #received_packet{packet_type = 'message'} = Packet) ->
    #received_packet{raw_packet = Message} = Packet,
    process_message(Session, Message).

%% Process stanza

-spec process_iq(pid(), any(), atom(), #xmlel{}) -> ok.
process_iq(Session, "get", ?NS_DISCO_INFO, IQ) ->
    Result = exmpp_iq:result(IQ, ej2j_helper:disco_info()),
    send_packet(Session, Result);
process_iq(Session, "get", ?NS_DISCO_ITEMS, IQ) ->
    Query = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], []),
    Result = exmpp_iq:result(IQ, Query),
    send_packet(Session, Result);
process_iq(_Session, "result", ?NS_ROSTER, IQ) ->
    Sender = exmpp_stanza:get_sender(IQ),
    Recipient = exmpp_stanza:get_recipient(IQ),
    ok = update_route(Sender, Recipient),
    Component = list_to_binary(ej2j:get_app_env(component, ?COMPONENT)),
    Roster = exmpp_xml:get_element_by_ns(IQ, ?NS_ROSTER),
    Items = lists:map(
              fun(Item) -> 
                      Attr = exmpp_xml:get_attribute(Item, <<"jid">>, <<"">>),
                      JID = exmpp_jid:parse(Attr),
                      NewJID = case exmpp_jid:domain(JID) of
                                   Component -> Attr;
                                   Domain ->
                                       Node = exmpp_jid:node(JID),
                                       <<Node/binary, "%", Domain/binary, "@", Component/binary>>
                               end,
                      NewAttr = exmpp_xml:attribute(<<"jid">>, NewJID),
                      exmpp_xml:set_attribute(Item, NewAttr)
              end, exmpp_xml:get_elements(Roster, 'item')),
    NewRoster = exmpp_xml:set_children(Roster, Items),
    NewIQ = exmpp_xml:set_children(IQ, [NewRoster]),
    process_generic(NewIQ);
process_iq(Session, "get", ?NS_INBAND_REGISTER, IQ) ->
    Result = exmpp_iq:result(IQ, ej2j_helper:inband_register()),
    send_packet(Session, Result);
process_iq(Session, "set", ?NS_INBAND_REGISTER, IQ) ->
    SenderJID = exmpp_jid:parse(exmpp_stanza:get_sender(IQ)),
    try
        Payload = exmpp_iq:get_payload(IQ),
        FormElement = exmpp_xml:get_element(Payload, ?NS_DATA_FORMS, 'x'),
	Form = ej2j_helper:form_parse(FormElement),
	JID = ej2j_helper:form_field(Form, <<"jid">>),
	Password = ej2j_helper:form_field(Form, <<"password">>),
	{ok, UserSession} = start_client(SenderJID, JID, Password),
        Status = exmpp_presence:set_status(exmpp_presence:available(), undefined),
        Roster = exmpp_client_roster:get_roster(),
        send_packet(Session, exmpp_iq:result(IQ)),
        exmpp_session:send_packet(UserSession, Status),
        exmpp_session:send_packet(UserSession, Roster)
    catch
        _Class:_Error ->
	    send_packet(Session, exmpp_iq:error(IQ, forbidden))
    end;
process_iq(_Session, _Type, _NS, IQ) ->
    process_generic(IQ).

-spec process_presence(pid(), #xmlel{}) -> ok.
process_presence(_Session, Presence) ->
    process_generic(Presence).

-spec process_message(pid(), #xmlel{}) -> ok.
process_message(_Session, Message) ->
    process_generic(Message).

-spec process_generic(#xmlel{}) -> ok.
process_generic(Packet) ->
    Sender = exmpp_stanza:get_sender(Packet),
    Recipient = exmpp_stanza:get_recipient(Packet),
    ok = update_route(Sender, Recipient),
    ok = route_packet(Sender, Recipient, Packet).

-spec update_route(binary(), binary()) -> ok.
update_route(Sender, Recipient) ->
    case exmpp_jid:domain(exmpp_jid:parse(Sender)) of
        <<"chat.facebook.com">> ->
            New = binary_to_list(Recipient),
            [Old, _, _] = string:tokens(New, "_"),
            ej2j_route:update(Old, New);
        _Else -> ok
    end.

-spec route_packet(binary() | undefined, binary() | undefined, #xmlel{}) -> ok.
route_packet(Sender, Recipient, Packet) when Sender =/= undefined,
                                              Recipient =/= undefined ->    
    From = exmpp_jid:parse(Sender),
    To = exmpp_jid:parse(Recipient),
    Routes = get_routes(From, To),
    route_packet(Routes, Packet);
route_packet(_Sender, _Recipient, _Packet) -> 
    ok.

-spec route_packet(list(), #xmlel{}) -> ok.
route_packet([{{client, Session}, NewFrom, NewTo}|Tail], Packet) ->
    Tmp = exmpp_stanza:set_sender(Packet, NewFrom),
    NewPacket = exmpp_stanza:set_recipient(Tmp, NewTo),
    exmpp_session:send_packet(Session, NewPacket),
    route_packet(Tail, Packet);
route_packet([{{server, Session}, NewFrom, NewTo}|Tail], Packet) ->
    Tmp = exmpp_stanza:set_sender(Packet, NewFrom),
    NewPacket = exmpp_stanza:set_recipient(Tmp, NewTo),
    send_packet(Session, NewPacket),
    route_packet(Tail, Packet);    
route_packet([], _Packet) ->
    ok.

-spec send_packet(pid(), #xmlel{}) -> ok.
send_packet(Session, El) ->
    exmpp_component:send_packet(Session, El).

-spec client_spawn(list(), list(), list()) -> {ok, {exmpp_jid:jid(), pid()}} |
                                             {error, any()}.
client_spawn(User, Domain, Creds) ->
    try
        FullJID = exmpp_jid:make(User, Domain, random),
        Session = exmpp_session:start_link({1, 0}),
        case Domain of
            "chat.facebook.com" ->
                [Token, AppId] = string:tokens(Creds, ";"),
                exmpp_session:auth_info(Session, FullJID, Token, AppId),
                Method = "X-FACEBOOK-PLATFORM";
            _Else ->
                exmpp_session:auth_info(Session, FullJID, Creds),
                Method = "DIGEST-MD5"
        end,
        Connect = exmpp_session:connect_TCP(Session, Domain, 5222),
        ok = element(1, Connect),
        {ok, FullJID} = exmpp_session:login(Session, Method),
        {ok, {FullJID, Session}}
    catch
        _Class:Error -> {error, Error}
    end.
