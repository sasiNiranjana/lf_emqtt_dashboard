%%--------------------------------------------------------------------
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

%% @doc Action for overview api.
-module(emqttd_dashboard_overview).

-include("emqttd_dashboard.hrl").
-include("../../../include/emqttd.hrl").

-import(emqttd_dashboard_util, [rpc/4, kmg/1]).

-export([stats/0, ptype/0, memory/0, cpu/0, 
         nodesinfo/0, metrics/0, listeners/0,
         bnode/0]).

-http_api({"stats",    stats,     []}).
-http_api({"ptype",    ptype,     []}).
-http_api({"memory",   memory,    []}).
-http_api({"cpu",      cpu,       []}).
-http_api({"node",     nodesinfo, []}).
-http_api({"metrics",  metrics,   []}).
-http_api({"listeners",listeners, []}).
-http_api({"bnode",    bnode,     []}).

%%-----------------------------------overview--------------------------------------

%% broker info
stats() ->
    [{Stat, Val} || {Stat, Val} <- emqttd_stats:getstats()].
   
ptype() ->
    emqttd_vm:get_port_types().
   
memory() ->
    emqttd_vm:get_memory().
    
cpu() ->
    emqttd_vm:loads().

nodesinfo() ->
    Nodes = [node()|nodes()],
    lists:map(fun(Node)-> 
			    CpuInfo = [{K, list_to_binary(V)} || {K, V} <- rpc:call(Node, emqttd_vm, loads, [])],
			    Memory = rpc(Node, emqttd_vm, get_memory, []),
			    [{name, Node},
		             {total_memory, kmg(proplists:get_value(allocated, Memory))},
			     {used_memory, kmg(proplists:get_value(used, Memory))},
			     {process_available, rpc(Node, emqttd_vm, get_process_limit, [])},
			     {process_used, length(rpc(Node, emqttd_vm, get_process_list, []))},
			     {max_fds, proplists:get_value(max_fds,rpc(Node, emqttd_vm, get_system_info, [check_io]))}|CpuInfo]
     			 end, Nodes).
    
metrics() ->
    [{Metric, Val} || {Metric, Val} <- emqttd_metrics:all()].
   
listeners() ->
    lists:map(fun({{Protocol, Port}, Pid})-> 
		MaxClients = esockd:get_max_clients(Pid),	
	 	CurrentClients = esockd:get_current_clients(Pid),
	 	[{protocol, Protocol},{port, Port},{max_clients, MaxClients},{current_clients, CurrentClients}] 
	      end, esockd:listeners()).

bnode() ->
    [{node, node()}].


