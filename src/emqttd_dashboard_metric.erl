-module(emqttd_dashboard_metric).

-http_api({"m_chart", m_chart, [{"minituts", int, 60}]}).

-export([m_chart/1]).

m_chart(Minutes) ->
    Metrics = emqttd_dashboard_meter:get_report(Minutes),
    {ok, Metrics}.
