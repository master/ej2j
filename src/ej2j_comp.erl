%% Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc XMPP Component

-module(ej2j_comp).

-behaviour(gen_server).

-export([start_link/0, stop/0, start_client/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

-include("ej2j.hrl").

-record(state, {session, routes}).

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

get_route(FromJID, ToJID) ->
    gen_server:call(?MODULE, {get_route, FromJID, ToJID}).

%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    Session = ej2j_helper:component(),
    {ok, #state{session = Session, routes = ej2j_route:init()}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}} | {stop, any(), any(), #state{}}.
handle_call(stop, _From, State) ->
    exmpp_component:stop(State#state.session),
    {stop, normal, ok, State};

handle_call({start_client, FromJID, ForeignJID, Password}, _From, #state{routes = Routes, session = ServerSession} = State) ->
    try
	{ToJID, ClientSession} = client_spawn(ForeignJID, Password),
	NewRoute = {FromJID, ToJID, ClientSession, ServerSession},
	NewRoutes = ej2j_route:add_entry(Routes, NewRoute),
	NewState = State#state{routes = NewRoutes},
	{reply, ClientSession, NewState}
    catch
	_Class:_Error -> {reply, false, State}
    end;

handle_call({get_route, FromJID, ToJID}, _From, #state{routes = Routes} = State) ->
    try
	Route = ej2j_route:get_route(Routes, FromJID, ToJID),
	{reply, Route, State}
    catch
	_Class:_Error -> {reply, false, State}
    end;	    

handle_call(_Msg, _From, State) ->
    {reply, unexpected, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(#received_packet{} = Packet, #state{session = S} = State) ->
    spawn_link(fun() -> process_received_packet(S, Packet) end),
    {noreply, State};

handle_info(#received_packet{packet_type=Type, raw_packet=Packet}, State) ->
    error_logger:warning_msg("Unknown packet received(~p): ~p~n", [Type, Packet]),
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
    server_send_packet(Session, Result);

process_iq(Session, "get", ?NS_DISCO_ITEMS, IQ) ->
    Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], [])),
    server_send_packet(Session, Result);

process_iq(Session, "get", ?NS_INBAND_REGISTER, IQ) ->
    Result = exmpp_iq:result(IQ, ej2j_helper:inband_register()),
    server_send_packet(Session, Result);

process_iq(Session, "set", ?NS_INBAND_REGISTER, IQ) ->
    SenderJID = exmpp_jid:parse(exmpp_stanza:get_sender(IQ)),
    try
	Form = ej2j_helper:form_parse(exmpp_xml:get_element(exmpp_iq:get_payload(IQ), ?NS_DATA_FORMS, 'x')),
	JID = ej2j_helper:form_field(Form, <<"jid">>),
	Password = ej2j_helper:form_field(Form, <<"password">>),
	UserSession = start_client(SenderJID, JID, Password),
	client_login(UserSession),
	client_send_presence(UserSession),
	server_send_packet(Session, exmpp_iq:result(IQ))
    catch
        _Class:_Error ->
	    server_send_packet(Session, exmpp_iq:error(IQ, forbidden))
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
    try
	From = exmpp_jid:parse(exmpp_stanza:get_sender(Packet)),
	To = exmpp_jid:parse(exmpp_stanza:get_recipient(Packet)),
	{Session, NewFrom, NewTo} = get_route(From, To),
	Tmp = exmpp_stanza:set_sender(Packet, NewFrom),
	NewPacket = exmpp_stanza:set_recipient(Tmp, NewTo),
	send_packet(Session, NewPacket)
    catch
	_Class:_Error -> ok      
    end.

%% Various helpers

-spec send_packet({atom(), pid()}, #xmlel{}) -> ok.
send_packet({client, Session}, El) ->
    client_send_packet(Session, El);
send_packet({server, Session}, El) ->
    server_send_packet(Session, El).

-spec server_send_packet(pid(), #xmlel{}) -> ok.
server_send_packet(Session, El) ->
    exmpp_component:send_packet(Session, El).

-spec client_send_packet(pid(), #xmlel{}) -> ok.
client_send_packet(Session, El) ->
    exmpp_session:send_packet(Session, El).

-spec client_login(pid()) -> ok.
client_login(Session) ->
    exmpp_session:login(Session),
    ok.

-spec client_send_presence(pid()) -> ok.
client_send_presence(Session) ->
    ComponentName = list_to_binary(ej2j:get_app_env(component_name, ?COMPONENT_NAME)),
    client_send_packet(Session, exmpp_presence:set_status(exmpp_presence:available(), ComponentName)),
    ok.

-spec client_spawn(list(), list()) -> {tuple(), pid()} | false.
client_spawn(JID, Password) ->
    try
	[User, Domain] = string:tokens(JID, "@"),
	FullJID = exmpp_jid:make(User, Domain, random),
	Session = exmpp_session:start_link(),
	exmpp_session:auth_info(Session, FullJID, Password),
	exmpp_session:auth_method(Session, digest),
	{ok, _StreamId} = exmpp_session:connect_TCP(Session, Domain, 5222),
	{FullJID, Session}
    catch
	_Class:_Error -> false
    end.
