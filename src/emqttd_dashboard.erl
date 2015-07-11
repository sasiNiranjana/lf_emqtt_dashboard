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
 
-include_lib("stdlib/include/qlc.hrl").

-define(AUTH_CLIENTID_TAB, mqtt_auth_clientid).

handle_request(Req) ->
    Path = Req:get(path),
    lager:info("Dashboard file: ~s ~s", [Req:get(method), Path]),
    handle_request(Path, Req).

handle_request("/api/" ++ Path, Req) when length(Path) > 0 ->
    api(list_to_atom(Path), Req);

handle_request("/" ++ Rest, Req) ->
    mochiweb_request:serve_file(Rest, docroot(), Req).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

%%-----------------------------------overview--------------------------------------

%% broker info
api(broker, Req) ->
    Funs = [sysdescr, version, uptime, datetime],
    BrokerInfo = [{Fun, list_to_binary(emqttd_broker:Fun())}|| Fun <- Funs],
    api_respond(Req, BrokerInfo);
   
api(stats, Req) ->
    Stats = [{Stat, Val} || {Stat, Val} <- emqttd_stats:getstats()],
    api_respond(Req, Stats);
   
api(ptype, Req) ->
    PortTyeps = emqttd_vm:get_port_types(), 
    api_respond(Req, PortTyeps);
   
api(memory, Req) ->
    Memory = emqttd_vm:get_memory(), 
    api_respond(Req, Memory);
    
api(cpu, Req) ->
    Cpu = emqttd_vm:loads(), 
    api_respond(Req, Cpu);
 
api(node, Req) ->
    Nodes = [node()|nodes()],
    NodeInfo = lists:map(fun(Node)-> 
		Memory = rpc:call(Node, emqttd_vm, get_memory, []),
		[{name, Node}, {memory, proplists:get_value(total, Memory)}]
	    end, Nodes),
    api_respond(Req, NodeInfo);
    
api(metrics, Req) ->
    Metrics = [{Metric, Val} || {Metric, Val} <- emqttd_metrics:all()],
    api_respond(Req, Metrics);
   
api(listeners, Req) ->
    Llists = lists:map(fun({{Protocol, Port}, Pid})-> 
		MaxClients = esockd:get_max_clients(Pid),	
	 	CurrentClients = esockd:get_current_clients(Pid),
	 	[{protocol, Protocol},{port, Port},{max_clients, MaxClients},{current_clients, CurrentClients}] 
	      end, esockd:listeners()),
    api_respond(Req, Llists);
    
%%-----------------------------------clients--------------------------------------
%%clients api
api(clients, Req) ->
    F = fun() ->
        Q = qlc:q([E || E <- mnesia:table(?AUTH_CLIENTID_TAB)]),
	qlc:e(Q) 
        end,
    {atomic, Clients} =  mnesia:transaction(F),
    Bodys = [[{mqtt_client,  Tab},
	     {clientId, ClientId}, 
	     {ipaddress, list_to_binary(emqttd_net:ntoa(Ipaddr))}] || {Tab, ClientId, Ipaddr, _Password} <- Clients],
    api_respond(Req, Bodys);
    
%%-----------------------------------session--------------------------------------
%%sessin check api
api(session, Req) ->
    SessionsTab =  emqttd_sm:table(),
    Bodys = [[
	     {clientId, ClientId}, 
	     {session, CleanSession}] || {CleanSession, ClientId, _SessPid, _MRef }
	    <- emqttd_vm:get_ets_object(SessionsTab)],
    api_respond(Req, Bodys);
   
%%-----------------------------------topic--------------------------------------
%%topic api
api(topic, Req) ->
    F = fun() ->
        Q = qlc:q([E || E <- mnesia:table(topic)]),
	qlc:e(Q) 
        end,
    {atomic, TopicLists} =  mnesia:transaction(F),
    Bodys = [[
	     {topic, Topic}, 
	     {node, Node} 
	     ] || {Tab, Topic, Node} <- TopicLists],
 
    api_respond(Req, Bodys);
   
%%-----------------------------------subscribe--------------------------------------
%%subscribe api
api(subscriber, Req) ->
    F = fun() ->
        Q = qlc:q([E || E <- mnesia:table(subscriber)]),
	qlc:e(Q) 
        end,
    {atomic, SubLists} =  mnesia:transaction(F),
    Bodys = [[{mqtt_subscriber,  Tab},
             {topic, Topic}, 
             {qos, Qos}] || {Tab, Topic, Qos, Pid} <- SubLists],

    api_respond(Req, Bodys).
    
api_respond(Req, Bodys) ->
    JsonData = 
    if length(Bodys) == 0 ->
	<<"\[\]">>;
    true ->
        list_to_binary(mochijson2:encode(Bodys) ++ <<10>>)
    end,
    Req:respond({200, [{"Content-Type","application/json"}], JsonData}).
