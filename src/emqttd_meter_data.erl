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

%% @doc emqttd meter data collection.
-module(emqttd_meter_data).

-export([init_db/0,
         start/2,
         stop/0,
         get_report/2]).


%% ====================================================================
%% API functions
%% ====================================================================

init_db() ->
    ets:new(bytes_received, [set]),
    ets:new(bytes_sent, [set]),
    ets:new(messages_dropped, [set]),
    ets:new(messages_qos0_received, [set]),
    ets:new(messages_qos0_sent, [set]),
    ets:new(messages_qos1_received, [set]),
    ets:new(messages_qos1_sent, [set]),
    ets:new(messages_qos2_received, [set]),
    ets:new(messages_qos2_sent, [set]),
    ets:new(messages_received, [set]),
    ets:new(messages_retained, [set]),
    ets:new(messages_sent, [set]),
    ets:new(packets_connack, [set]),
    ets:new(packets_connect, [set]),
    ets:new(packets_disconnect, [set]),
    ets:new(packets_pingreq, [set]),
    ets:new(packets_pingresp, [set]),
    ets:new(packets_puback_received, [set]),
    ets:new(packets_puback_sent, [set]),
    ets:new(packets_pubcomp_received, [set]),
    ets:new(packets_pubcomp_sent, [set]),
    ets:new(packets_publish_received, [set]),
    ets:new(packets_publish_sent, [set]),
    ets:new(packets_pubrec_received, [set]),
    ets:new(packets_pubrec_sent, [set]),
    ets:new(packets_pubrel_received, [set]),
    ets:new(packets_pubrel_sent, [set]),
    ets:new(packets_received, [set]),
    ets:new(packets_sent, [set]),
    ets:new(packets_suback, [set]),
    ets:new(packets_subscribe, [set]),
    ets:new(packets_unsuback, [set]),
    ets:new(packets_unsubscribe, [set]),
    {ok}.

start(Interval, Fun) ->
    register(?MODULE, spawn(fun() -> loop(Interval, Fun) end)).

stop() ->
    rpc(?MODULE, stop).

get_report(Metric, Limit) ->
    Result = ets:match(Metric, '$1', Limit),
    case Result of
        {M, _C} -> M;
        '$end_of_table' -> []
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

save(Data) ->
    {{"bytes/received", M1},
     {"bytes/sent", M2},
     {"messages/dropped", M3},
     {"messages/qos0/received", M4},
     {"messages/qos0/sent", M5},
     {"messages/qos1/received", M6},
     {"messages/qos1/sent", M7},
     {"messages/qos2/received", M8},
     {"messages/qos2/sent", M9},
     {"messages/received", M10},
     {"messages/retained", M11},
     {"messages/sent", M12},
     {"packets/connack", M13},
     {"packets/connect", M14},
     {"packets/disconnect", M15},
     {"packets/pingreq", M16},
     {"packets/pingresp", M17},
     {"packets/puback/received", M18},
     {"packets/puback/sent", M19},
     {"packets/pubcomp/received", M20},
     {"packets/pubcomp/sent", M21},
     {"packets/publish/received", M22},
     {"packets/publish/sent", M23},
     {"packets/pubrec/received", M24},
     {"packets/pubrec/sent", M25},
     {"packets/pubrel/received", M26},
     {"packets/pubrel/sent", M27},
     {"packets/received", M28},
     {"packets/sent", M29},
     {"packets/suback", M30},
     {"packets/subscribe", M31},
     {"packets/unsuback", M32},
     {"packets/unsubscribe", M33}} = Data,
    Ts = timestamp(),
    ets:insert(bytes_received, {Ts, M1}),
    ets:insert(bytes_sent, {Ts, M2}),
    ets:insert(messages_dropped, {Ts, M3}),
    ets:insert(messages_qos0_received, {Ts, M4}),
    ets:insert(messages_qos0_sent, {Ts, M5}),
    ets:insert(messages_qos1_received, {Ts, M6}),
    ets:insert(messages_qos1_sent, {Ts, M7}),
    ets:insert(messages_qos2_received, {Ts, M8}),
    ets:insert(messages_qos2_sent, {Ts, M9}),
    ets:insert(messages_received, {Ts, M10}),
    ets:insert(messages_retained, {Ts, M11}),
    ets:insert(messages_sent, {Ts, M12}),
    ets:insert(packets_connack, {Ts, M13}),
    ets:insert(packets_connect, {Ts, M14}),
    ets:insert(packets_disconnect, {Ts, M15}),
    ets:insert(packets_pingreq, {Ts, M16}),
    ets:insert(packets_pingresp, {Ts, M17}),
    ets:insert(packets_puback_received, {Ts, M18}),
    ets:insert(packets_puback_sent, {Ts, M19}),
    ets:insert(packets_pubcomp_received, {Ts, M20}),
    ets:insert(packets_pubcomp_sent, {Ts, M21}),
    ets:insert(packets_publish_received, {Ts, M22}),
    ets:insert(packets_publish_sent, {Ts, M23}),
    ets:insert(packets_pubrec_received, {Ts, M24}),
    ets:insert(packets_pubrec_sent, {Ts, M25}),
    ets:insert(packets_pubrel_received, {Ts, M26}),
    ets:insert(packets_pubrel_sent, {Ts, M27}),
    ets:insert(packets_received, {Ts, M28}),
    ets:insert(packets_sent, {Ts, M29}),
    ets:insert(packets_suback, {Ts, M30}),
    ets:insert(packets_subscribe, {Ts, M31}),
    ets:insert(packets_unsuback, {Ts, M32}),
    ets:insert(packets_unsubscribe, {Ts, M33}),
    {ok, Data}.

rpc(Pid, Request) ->
    Pid ! {self(), Request}.

loop(Interval, Fun) ->
    receive
        {From, stop} ->
            void
        after Interval ->
            save(Fun()),
            loop(Interval, Fun)
    end.

timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).