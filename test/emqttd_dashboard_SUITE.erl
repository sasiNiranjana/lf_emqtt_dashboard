-module(emqttd_dashboard_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

all() -> 
    [{group, overview},
     {group, clients},
     {group, sessions},
     {group, routes}
     ].

groups() ->
    [{overview, [sequence], [brokers, stats, ptype, memory, 
                            cpu, nodes, metrics, listeners, bnode]},
     {clients, [sequence], [clients, clients_query]},
     {sessions, [sequence], [session_query]},
     {routes, [sequence], [route_query]}
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
    ?assert(connect_dashbaord_(get, "api/brokers")).

stats(_) ->
    ?assert(connect_dashbaord_(get, "api/stats")).

ptype(_) ->
    ?assert(connect_dashbaord_(get, "api/ptype")).

memory(_) ->
    ?assert(connect_dashbaord_(get, "api/memory")).

cpu(_) ->
    ?assert(connect_dashbaord_(get, "api/cpu")).

nodes(_) ->
    ?assert(connect_dashbaord_(get, "api/nodes")).

metrics(_) ->
    ?assert(connect_dashbaord_(get, "api/metrics")).

listeners(_) ->
    ?assert(connect_dashbaord_(get, "api/listeners")).

bnode(_) ->
    ?assert(connect_dashbaord_(get, "api/bnode")).

clients(_) ->
    ?assert(connect_dashbaord_(post, "api/clients", "page_size=100&curr_page=1")).
   
clients_query(_) ->
    Sock = client_connect_(<<16,12,0,4,77,81,84,84,4,0,0,90,0,0>>, 4),
    {ok, Entry} = emqttd_dashboard_client:list(<<>>, 1, 100),
    Client = proplists:get_value(result, Entry),
    ClientId = proplists:get_value(clientId, Client), 
    ?assertEqual({ok, Entry}, emqttd_dashboard_client:list(ClientId, 1, 100)),
    gen_tcp:close(Sock).

session_query(_) ->
    Sock = client_connect_(<<16,12,0,4,77,81,84,84,4,0,0,90,0,0>>, 4),
    {ok, Entry} = emqttd_dashboard_session:list(<<>>, 1, 100),
    Session= proplists:get_value(result, Entry),
    ClientId = proplists:get_value(clientId, Session), 
    ?assertEqual({ok, Entry}, emqttd_dashboard_session:list(ClientId, 1, 100)),
    gen_tcp:close(Sock).

route_query(_) ->
    ok = emqttd:subscribe(<<"topic">>),
    timer:sleep(10),
    {ok, Routes} = emqttd_dashboard_route:list(<<>>, 1, 100),
    ?assertEqual({ok, Routes}, emqttd_dashboard_route:list(<<"topic">>, 1, 100)),
    ok = emqttd:unsubscribe(<<"topic">>).


client_connect_(Packet, RecvSize) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 1883, [binary, {packet, raw}, {active, false}]),
    gen_tcp:send(Sock, Packet),
    gen_tcp:recv(Sock, RecvSize, 3000),
    Sock.

connect_dashbaord_(Method, Api) ->
    Url = "http://127.0.0.1:18083/" ++ Api,
    Auth = auth_header_("admin", "public"),
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

connect_dashbaord_(Method, Api, Params) ->
    Url = "http://127.0.0.1:18083/" ++ Api,
    Auth = auth_header_("admin", "public"),
    case httpc:request(Method, {Url, [Auth], ?CONTENT_TYPE, Params}, [], []) of
    {error, socket_closed_remotely} ->
        false;
    {ok, {{"HTTP/1.1", 200, "OK"}, _, _Return} }  ->
        true;
    {ok, {{"HTTP/1.1", 400, "Bad Request"}, _, []}} ->
        false;
    {ok, {{"HTTP/1.1", 404, "Object Not Found"}, _, []}} ->
        false
    end.
