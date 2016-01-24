%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2012-2016, Feng Lee <feng@emqtt.io>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc emqttd web dashboard application.
%%%
%%% @author Huang Dan
%%%-----------------------------------------------------------------------------

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
    MFArgs = {emqttd_dashboard, handle_request, []},
	mochiweb:start_http(Port, Options, MFArgs).

worker_spec(Name) ->
    {Name,
        {Name, start_link, []},
            permanent, 10000, worker, [Name]}.

