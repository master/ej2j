%% @author Oleg Smirnov <oleg.smirnov@gmail.com>
%% @doc Helper functions

-module(ej2j_helper).

-export([component/0, disco_info/0, inband_register/0, form_parse/1, form_field/2]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

-include("ej2j.hrl").

%% XEP-0114: Jabber Component Protocol

-spec component() -> pid().
component() ->
    Session = exmpp_component:start_link(),
    exmpp_component:auth(Session, ej2j:get_app_env(component, ?COMPONENT), 
                         ej2j:get_app_env(server_secret, ?SERVER_SECRET)),
    _StreamId = exmpp_component:connect(Session, 
                  ej2j:get_app_env(server_host, ?SERVER_HOST), 
                  ej2j:get_app_env(server_port, ?SERVER_PORT)),
    ok = exmpp_component:handshake(Session),
    Session.

%% XEP-0030: Service Discovery

-spec disco_info() -> #xmlel{}.
disco_info() ->
    Category = exmpp_xml:attribute(<<"category">>, <<"gateway">>),
    Type = exmpp_xml:attribute(<<"type">>, <<"xmpp">>),
    ComponentName = list_to_binary(ej2j:get_app_env(component_name, ?COMPONENT_NAME)),
    Name = exmpp_xml:attribute(<<"name">>, ComponentName),
    Identity = exmpp_xml:element(?NS_DISCO_INFO, 'identity', 
				 [Category, Type, Name], []),
    FeaturesNS = [?NS_DISCO_INFO_s, ?NS_DISCO_ITEMS_s, ?NS_GATEWAY_s,
		  ?NS_INBAND_REGISTER_s, ?NS_SOFT_VERSION_s],
    Features = lists:map(fun(NS) -> exmpp_xml:element(?NS_DISCO_INFO, 'feature', 
						      [exmpp_xml:attribute(<<"var">>, NS)],
						      [])
			 end, FeaturesNS),
    exmpp_xml:element(?NS_DISCO_INFO, 'query', [], [Identity|Features]).

%% XEP-0077: In-Band Registration

-spec inband_register() -> #xmlel{}.
inband_register() ->
    InstructionElement = exmpp_xml:element(?NS_DATA_FORMS, 'instructions',
					   [], [?XMLCDATA(<<?TEXT_FORM>>)]),
    RegisterElement = exmpp_xml:element(?NS_DATA_FORMS, value, 
					[], [?XMLCDATA(?NS_INBAND_REGISTER_s)]),
    FormTypeField = exmpp_xml:element(?NS_DATA_FORMS, 'field', 
				      [?XMLATTR(<<"type">>, <<"hidden">>), 
				       ?XMLATTR(<<"var">>, <<"FORM_TYPE">>)], 
				      [RegisterElement]),
    JIDField = make_field(<<"jid-single">>, <<"jid">>, <<"JID">>, <<"">>),
    PasswordField = make_field(<<"text-private">>, <<"password">>, <<"Password">>, <<>>),
    Form = exmpp_xml:element(?NS_DATA_FORMS, 'x', 
			     [?XMLATTR(<<"type">>, <<"form">>)],
			     [InstructionElement, FormTypeField, JIDField, PasswordField]),
    FormInstructions = exmpp_xml:element(?NS_INBAND_REGISTER, 'instructions',
					 [], [?XMLCDATA(<<?TEXT_REG>>)]),
    exmpp_xml:element(?NS_INBAND_REGISTER, 'query', [], [FormInstructions, Form]).

-spec make_field(binary(), binary(), binary(), binary()) -> #xmlel{}.
make_field(Type, Var, Label, Value) ->
    exmpp_xml:element(?NS_DATA_FORMS, 'field', 
		      [?XMLATTR(<<"type">>, Type), ?XMLATTR(<<"var">>, Var),
		       ?XMLATTR(<<"label">>, Label)],
                      [exmpp_xml:element(?NS_DATA_FORMS, 'value',
					 [], [?XMLCDATA(Value)])]).
-spec form_parse(#xmlel{}) -> list().
form_parse(Form) ->
    Fields = exmpp_xml:get_elements(Form,  ?NS_DATA_FORMS, 'field'),
    lists:map(fun(Field) ->
		      {exmpp_xml:get_attribute_as_binary(Field, <<"var">>, <<>>),
		       exmpp_xml:get_cdata(exmpp_xml:get_element(Field, "value"))}
	      end, Fields).

-spec form_field(list(), binary()) -> list().
form_field(Form, Name) ->
    binary_to_list(element(2, lists:keyfind(Name, 1, Form))).
