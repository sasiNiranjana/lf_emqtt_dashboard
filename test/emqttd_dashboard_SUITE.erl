-module(emqttd_dashboard_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> 
    [{group, overview}
     ].

groups() ->
    [{overview, [sequence], [brokers, stats, ptype, memory, 
                            cpu, nodes, metrics, listeners, bnode]}
    ].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(lager),
    application:set_env(emqttd, conf, filename:join([DataDir, "emqttd.conf"])),
    application:ensure_all_started(emqttd),
    application:set_env(emqttd_dashboard, conf, filename:join([DataDir, "emqttd_dashboard.conf"])),
    application:ensure_all_started(emqttd_dashboard),
    Config.
 
end_per_suite(_Config) ->
    application:stop(emqttd_dashboard),
    application:stop(emqttd),
    application:stop(esockd),
    application:stop(gproc),
    emqttd_mnesia:ensure_stopped().
 
brokers(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/brokers")).

stats(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/stats")).

ptype(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/ptype")).

memory(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/memory")).

cpu(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/cpu")).

nodes(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/nodes")).

metrics(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/metrics")).

listeners(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/listeners")).

bnode(_) ->
    ?_assertEqual(true, connect_dashbaord_(get, "api/bnode")).

connect_dashbaord_(Method, Api) ->
    Url = "http://127.0.0.1:18083/" ++ Api,
    Auth = auth_header_("admin", "public11"),
    case httpc:request(Method, {Url, [Auth]}, [], []) of
      {error, socket_closed_remotely} ->
          false;
      {ok, {{"HTTP/1.1", 200, "OK"}, _, _Return} }  ->
          true;
      {ok, {{"HTTP/1.1", 400, "Bad Request"}, _, []}} ->
          false;
      {ok, {{"HTTP/1.1", 404, "Object Not Found"}, _, []}} ->
          false
    end.

auth_header_(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.
