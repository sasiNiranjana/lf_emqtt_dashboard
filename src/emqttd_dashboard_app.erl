%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc emqttd web dashboard application.
-module(emqttd_dashboard_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_dashboard_sup:start_link(),
    {ok, _ChiId} = supervisor:start_child(Sup, worker_spec(emqttd_dashboard_admin)),

    {ok, Listener} = application:get_env(emqttd_dashboard, listener),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_dashboard, [Listener], 9999),
    open_listener(Listener),
    {ok, Sup}.

stop(_State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_dashboard),
    {ok, {_Proto, Port, _Opts}} = application:get_env(emqttd_dashboard, listener),
    mochiweb:stop_http(Port).

%% open http port
open_listener({_Http, Port, Options}) ->
    MFArgs = {emqttd_dashboard_dispatcher, handle_request, []},
	mochiweb:start_http(Port, Options, MFArgs).

worker_spec(Name) ->
    {Name,
        {Name, start_link, []},
            permanent, 10000, worker, [Name]}.

