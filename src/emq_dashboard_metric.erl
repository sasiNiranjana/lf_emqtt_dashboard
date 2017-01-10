%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 Feng Lee <feng@emqtt.io>.
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

-module(emq_dashboard_metric).

-include("emq_dashboard_meter.hrl").

-http_api({"m_chart", m_chart, [{"minutes", int, 60}]}).
-http_api({"metrics_org", metrics_org, [{"minutes", int, 60}]}).
-http_api({"metrics_merge", metrics_merge, [{"minutes", int, 60}]}).

-export([m_chart/1, metrics_merge/1, metrics_org/1]).

m_chart(Minutes) ->
    %Metrics = emq_dashboard_meter:get_report(Minutes),
    %{ok, Metrics}.
    metrics_merge(Minutes).

metrics_org(Minutes) ->
    Data = emq_meter_access:get_data_all(Minutes, ?REPORT_INTERVAL),
    {ok, Data}.

metrics_merge(Minutes) ->
    Data = 
    case Minutes of
        60          -> emq_meter_access:get_data_all(Minutes, ?INTERVAL_1);
        60 * 24     -> emq_meter_access:get_data_all(Minutes, ?INTERVAL_2);
        60 * 24 * 7 -> emq_meter_access:get_data_all(Minutes, ?INTERVAL_3)
    end,
    {ok, Data}.
