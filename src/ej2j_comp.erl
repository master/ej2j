%% Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc XMPP Component

-module(ej2j_comp).

-behaviour(gen_server).

-export([start_link/0, stop/0, start_client/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-define(RESTART_DELAY, 5000).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("ej2j.hrl").

-record(state, {session, db}).

%% Public API

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

-spec start_client(tuple(), list(), list()) -> ok.
start_client(OwnerJID, ForeignJID, Password) ->
    gen_server:call(?MODULE, {start_client, OwnerJID, ForeignJID, Password}).

get_routes(FromJID, ToJID) ->
    gen_server:call(?MODULE, {get_routes, FromJID, ToJID}).

%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    Session = ej2j_helper:component(),
    {ok, #state{session = Session, db = ej2j_route:init()}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}} | {stop, any(), any(), #state{}}.
handle_call(stop, _From, State) ->
    exmpp_component:stop(State#state.session),
    {stop, normal, ok, State};

handle_call({start_client, FromJID, ForeignJID, Password}, _From, #state{db=DB, session=ServerS} = State) ->
    try
	{ToJID, ClientS} = client_spawn(ForeignJID, Password),
	NewDB = ej2j_route:add(DB, {FromJID, ToJID, ClientS, ServerS}),
	{reply, ClientS, State#state{db = NewDB}}
    catch
	_Class:_Error -> {reply, false, State}
    end;

handle_call({get_routes, FromJID, ToJID}, _From, #state{db=DB} = State) ->
    Routes = ej2j_route:get(DB, FromJID, ToJID),
    {reply, Routes, State};

handle_call(get_state, _From, #state{session=ServerS, db=DB} = State) ->
    {reply, {state, {session, ServerS},
                    {db, DB}}, State};

handle_call(_Msg, _From, State) ->
    {reply, unexpected, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(#received_packet{} = Packet, #state{session=S} = State) ->
    spawn_link(fun() -> process_received_packet(S, Packet) end),
    {noreply, State};

handle_info(#received_packet{packet_type=Type, raw_packet=Packet}, State) ->
    error_logger:warning_msg("Unknown packet received(~p): ~p~n", [Type, Packet]),
    {noreply, State};

%% component has died
handle_info({'EXIT', ServerS, _}, #state{db=DB, session=ServerS} = State) ->
    timer:sleep(?RESTART_DELAY),
    NewServerS = ej2j_helper:component(),
    ej2j_route:free(DB),
    NewDB = ej2j_route:init(),
    {noreply, State#state{session=NewServerS, db=NewDB}};

%% one of the user sessions died
handle_info({'EXIT', Pid, _}, #state{db=DB} = State) ->
    NewDB = ej2j_route:del(DB, Pid),
    {noreply, State#state{db=NewDB}};

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
process_received_packet(Session, #received_packet{packet_type = 'iq'} = Packet) ->
    #received_packet{type_attr=Type, raw_packet = IQ} = Packet,
    NS = exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)),
    process_iq(Session, Type, NS, IQ);

process_received_packet(Session, #received_packet{packet_type = 'presence'} = Packet) ->
    #received_packet{raw_packet = Presence} = Packet,
    process_presence(Session, Presence);

process_received_packet(Session, #received_packet{packet_type = 'message'} = Packet) ->
    #received_packet{raw_packet = Message} = Packet,
    process_message(Session, Message).

%% Process stanza

-spec process_iq(pid(), any(), atom(), #xmlel{}) -> ok.
process_iq(Session, "get", ?NS_DISCO_INFO, IQ) ->
    Result = exmpp_iq:result(IQ, ej2j_helper:disco_info()),
    send_packet(Session, Result);

process_iq(Session, "get", ?NS_DISCO_ITEMS, IQ) ->
    Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], [])),
    send_packet(Session, Result);

process_iq(_Session, "result", ?NS_ROSTER, IQ) ->
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
	Form = ej2j_helper:form_parse(exmpp_xml:get_element(exmpp_iq:get_payload(IQ), ?NS_DATA_FORMS, 'x')),
	JID = ej2j_helper:form_field(Form, <<"jid">>),
	Password = ej2j_helper:form_field(Form, <<"password">>),
	UserSession = start_client(SenderJID, JID, Password),
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
    From = exmpp_jid:parse(exmpp_stanza:get_sender(Packet)),
    To = exmpp_jid:parse(exmpp_stanza:get_recipient(Packet)),
    Routes = get_routes(From, To),
    route_packet(Routes, Packet).

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

%% Various helpers

-spec send_packet(pid(), #xmlel{}) -> ok.
send_packet(Session, El) ->
    exmpp_component:send_packet(Session, El).

-spec client_spawn(list(), list()) -> {tuple(), pid()} | false.
client_spawn(JID, Password) ->
    try
        [User, Domain] = string:tokens(JID, "@"),
        FullJID = exmpp_jid:make(User, Domain, random),
        {ok, Session} = auth(sasl_plain, FullJID, Domain, Password),
	{FullJID, Session}
    catch
	_Class:_Error -> false
    end.

-spec auth(term(), #xmlel{}, string(), string()) -> {ok, pid()} | {error, any()}.
auth(sasl_plain, FullJID, Domain, Password) ->
    Session = exmpp_session:start_link({1,0}),
    %% Create a new session with basic auth
    exmpp_session:auth_info(Session, FullJID, Password),
    {ok, _StreamId, _Features} = connect_TCP(Session, Domain, 5222),
    %% Login with defined JID / Auth
    {ok, _JID} = exmpp_session:login(Session, "PLAIN"),
    {ok, Session};

auth(basic_digest, FullJID, Domain, Password) ->
    Session = exmpp_session:start_link(),
    %% Create a new session with basic auth
    exmpp_session:auth_basic_digest(Session, FullJID, Password),
    {ok, _StreamId, _Features} = connect_TCP(Session, Domain, 5222),
    %% Login with defined JID / Auth
    {ok, _JID} = exmpp_session:login(Session),
    {ok, Session};

auth(_AuthType, _FullJid, _Domain, _Password) ->
    {error, "Unknown authentication type"}.


-spec connect_TCP(pid(), string(), port()) -> {ok, any(), #xmlel{}} | {error, any()}.
connect_TCP(Session, Host, Port) ->
    case exmpp_session:connect_TCP(Session, Host, Port) of
        {ok, StreamId} -> {ok, StreamId, empty};
        {ok, StreamId, Features} -> {ok, StreamId, Features};
        Any -> {error, Any}
    end.
