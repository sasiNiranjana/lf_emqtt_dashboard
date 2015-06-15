%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2012-2015, Feng Lee <feng@emqtt.io>
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
%%% @doc
%%% emqttd web dashboard.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emqttd_dashboard).

-author("Feng Lee <feng@emqtt.io>").

-export([handle_request/1]).

-define(SEPARTOR, $\/).

handle_request(Req) ->
    Path = Req:get(path),
    lager:info("Dashboard file: ~s ~s", [Req:get(method), Path]),
    handle_request(Path, Req).

%%%overview api
%% broker info
handle_request("/api/broker", Req) ->
    Funs = [sysdescr, version, uptime, datetime],
    BrokerInfo = [{Fun, list_to_binary(emqttd_broker:Fun())}|| Fun <- Funs],
    Jsons = mochijson2:encode(BrokerInfo),
    Req:respond({200, [], iolist_to_binary(Jsons)});

handle_request("/api/stats", Req) ->
    Stats = [{Stat, Val} || {Stat, Val} <- emqttd_stats:getstats()],
    Jsons = mochijson2:encode(Stats),
    Req:respond({200, [], iolist_to_binary(Jsons)});

handle_request("/api/ptype", Req) ->
    PortTyeps = emqttd_vm:get_port_types(), 
    Jsons = mochijson2:encode(PortTyeps),
    Req:respond({200, [], iolist_to_binary(Jsons)});

handle_request("/api/memory", Req) ->
    Memory = emqttd_vm:get_memory(), 
    Jsons = mochijson2:encode(Memory),
    Req:respond({200, [], iolist_to_binary(Jsons)});

handle_request("/api/listeners", Req) ->
    Llists = [Listeners || {Listeners , _Port} <- esockd:listeners()],
    Jsons = mochijson2:encode(Llists),
    Req:respond({200, [], iolist_to_binary(Jsons)});

%%clients api
handle_request("/api/clients", Req) ->
    ClientsTab =  emqttd_cm:table(),
    Bodys = [[{mqtt_client,  Tab},
	     {clientId, ClientId}, 
	     {ipaddress, list_to_binary(emqttd_net:ntoa(Ip))}, 
	     {session, CleanSession}] || {Tab, ClientId, _Pid, Ip, _, _, CleanSession, _ }
	    <- emqttd_vm:get_ets_object(ClientsTab)],
 
    Jsons = [mochijson2:encode(Body)|| Body<- Bodys],
    lager:info("Json: ~s", [Jsons]),
    Clients = [iolist_to_binary(Json)|| Json<- Jsons],
    Req:respond({200, [], Clients});


handle_request("/api/" ++ Rest, Req) when length(Rest) > 0 ->
    wm_loop(Req);

handle_request("/" ++ Rest, Req) ->
    mochiweb_request:serve_file(Rest, docroot(), Req).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

build_dispatcher() ->
	[{["api" | Path],  Mod, Args} || {Path, Mod, Args} <-
		lists:append([emqttd_dashboard_dispatcher:dispatcher()])].

wm_loop(Req) ->
    PathAsString = Req:get(path),
    Path = string:tokens(PathAsString, [?SEPARTOR]).
    %%{value, {[_P], M, Args}} = lists:keysearch(Path, 1, build_dispatcher()).
