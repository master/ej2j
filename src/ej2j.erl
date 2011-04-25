%% @author Oleg Smirnov <oleg.smirnov@gmail.com>                                                      
%% @doc Application

-module(ej2j).

-behaviour(application).

-export([start/2, stop/1, init/1, get_app_env/2]).

-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) -> any().
stop(_State) ->
    ok.

-spec get_app_env(term(), term()) -> term().
get_app_env(Opt, Default) ->
    {ok, App} = application:get_application(),
    case application:get_env(App, Opt) of
        {ok, Val} -> Val;
	_ ->
	    case init:get_argument(Opt) of
		{'ok', [[Val]]} -> Val;
		error       -> Default
	    end
    end.

-spec init(any()) -> {ok, {tuple(), [tuple()]}}.
init([]) ->
    {ok, {{one_for_one, 1, 10}, [{ej2j_comp, {ej2j_comp, start_link, []},
				  permanent, 10, worker, [ej2j_comp]}]}}.
