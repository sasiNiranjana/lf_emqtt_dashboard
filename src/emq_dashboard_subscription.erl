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

%% @doc Subscriptions API.
-module(emq_dashboard_subscription).

-include("emq_dashboard.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("stdlib/include/qlc.hrl").

-define(TAB, mqtt_subproperty).

-export([list/3]).

-http_api({"subscriptions", list, [{"client_key", binary},
                                   {"curr_page",  int, 1},
                                   {"page_size",  int, 100}]}).

list(ClientId, PageNo, PageSize) when ?EMPTY_KEY(ClientId) ->
    TotalNum = ets:info(?TAB, size),
    Qh = qlc:q([E || E <- ets:table(?TAB)]),
    emq_dashboard:query_table(Qh, PageNo, PageSize, TotalNum, fun row/1);

list(ClientId, PageNo, PageSize) ->
    Keys = ets:lookup(mqtt_subscription, ClientId),
    Fun = fun() ->lists:map(fun({S, T}) ->
                            [R] = ets:lookup(?TAB, {T, S}), R 
                            end, Keys)
          end,
    emq_dashboard:lookup_table(Fun, PageNo, PageSize, fun row/1).

row({{Topic, ClientId}, [Qos, local]}) ->
    [{clientid, ClientId}, {topic, Topic}, Qos];

row({{Topic, ClientId}, Qos}) ->
    [{clientid, ClientId}, {topic, Topic} | Qos].

