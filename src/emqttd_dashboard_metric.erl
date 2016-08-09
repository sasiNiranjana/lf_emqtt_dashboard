-module(emqttd_dashboard_metric).
-include("emqttd_dashboard_meter.hrl").

-http_api({"m_chart", m_chart, [{"minutes", int, 60}]}).
-http_api({"metrics_org", metrics_org, [{"minutes", int, 60}]}).
-http_api({"metrics_merge", metrics_merge, [{"minutes", int, 60}]}).

-export([m_chart/1, metrics_merge/1, metrics_org/1]).

m_chart(Minutes) ->
    %Metrics = emqttd_dashboard_meter:get_report(Minutes),
    %{ok, Metrics}.
    metrics_merge(Minutes).

metrics_org(Minutes) ->
    Data = emqttd_meter_access:get_data_all(Minutes, 5 * 1000),
    {ok, Data}.

metrics_merge(Minutes) ->
    Data = 
    case Minutes of
        60          -> emqttd_meter_access:get_data_all(Minutes, ?INTERVAL_1);
        60 * 24     -> emqttd_meter_access:get_data_all(Minutes, ?INTERVAL_2);
        60 * 24 * 7 -> emqttd_meter_access:get_data_all(Minutes, ?INTERVAL_3)
    end,
    {ok, Data}.