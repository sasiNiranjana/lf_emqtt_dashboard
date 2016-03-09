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

%% @doc Action for route api.
-module(emqttd_dashboard_route).

-include("emqttd_dashboard.hrl").
-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([execute/0]).

execute() ->
    F = fun() -> qlc:e(qlc:q([E || E <- mnesia:table(route)])) end,
    {atomic, Topics} =  mnesia:transaction(F),
    [[{topic, Topic}, {node, Node}] || #mqtt_route{topic = Topic, node= Node} <- Topics].

