-module(emqttd_dashboard_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

all() -> 
    [{group, overview},
     {group, alarms},
     {group, clients},
     {group, sessions},
     {group, routes},
     {group, subscriptions},
     {group, admins}
     ].

groups() ->
    [{overview, [sequence], [brokers, stats, ptype, memory, 
                            cpu, nodes, metrics, listeners, bnode]},
     {alarms, [sequence], [get_alarms]},
     {clients, [sequence], [clients, clients_query]},
     {sessions, [sequence], [session_query]},
     {routes, [sequence], [route_query]},
     {subscriptions, [sequence], [subscribe_query]},
     {admins, [sequence], [admins_add_delete]}
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
    ?assert(connect_dashbaord_(get, "api/brokers", auth_header_())).

stats(_) ->
    ?assert(connect_dashbaord_(get, "api/stats", auth_header_())).

ptype(_) ->
    ?assert(connect_dashbaord_(get, "api/ptype", auth_header_())).

memory(_) ->
    ?assert(connect_dashbaord_(get, "api/memory", auth_header_())).

cpu(_) ->
    ?assert(connect_dashbaord_(get, "api/cpu", auth_header_())).

nodes(_) ->
    ?assert(connect_dashbaord_(get, "api/nodes", auth_header_())).

metrics(_) ->
    ?assert(connect_dashbaord_(get, "api/metrics", auth_header_())).

listeners(_) ->
    ?assert(connect_dashbaord_(get, "api/listeners", auth_header_())).

bnode(_) ->
    ?assert(connect_dashbaord_(get, "api/bnode", auth_header_())).

get_alarms(_) ->
    AlarmTest = #mqtt_alarm{id = <<"1">>, severity = error, title="alarm title", summary="alarm summary"},
    emqttd_alarm:set_alarm(AlarmTest),
    {ok, [Alarm]} = emqttd_dashboard_alarm:alarms(),
    ?assertEqual(error, proplists:get_value(severity, Alarm)).

clients(_) ->
    ?assert(connect_dashbaord_(post, "api/clients", "page_size=100&curr_page=1", auth_header_())).
   
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

subscribe_query(_) ->
    Sock = client_connect_(<<16,12,0,4,77,81,84,84,4,0,0,90,0,0>>, 4),
    {ok, Entry} = emqttd_dashboard_subscription:list(<<>>, 1, 100),
    Sub = proplists:get_value(result, Entry),
    ClientId = proplists:get_value(clientId, Sub), 
    ?assertEqual({ok, Entry}, emqttd_dashboard_subscription:list(ClientId, 1, 100)),
    gen_tcp:close(Sock).

admins_add_delete(_) ->
    emqttd_dashboard_user:add(<<"username">>, <<"password">>, <<"tag">>),
    emqttd_dashboard_user:add(<<"username1">>, <<"password1">>, <<"tag1">>),
    {ok, Admins} = emqttd_dashboard_user:users(),
    ?assertEqual([[{name, <<"username">>}, {tag, <<"tag">>}],
                  [{name, <<"username1">>}, {tag, <<"tag1">>}],
                  [{name, <<"admin">>}, {tag, <<"administrator">>}]],
                Admins),
    emqttd_dashboard_user:remover(<<"username1">>),
    ?assertEqual({ok, [[{name, <<"username">>}, {tag, <<"tag">>}],
                       [{name, <<"admin">>}, {tag, <<"administrator">>}]]}, 
                emqttd_dashboard_user:users()),
    emqttd_dashboard_user:update(<<"username">>, <<"pwd">>, <<"login">>),
    timer:sleep(10),
    ?assert(connect_dashbaord_(get, "api/brokers", auth_header_("username", "pwd"))),
    emqttd_dashboard_user:remover(<<"username">>),
    ?assertEqual(false, connect_dashbaord_(get, "api/brokers", auth_header_("username", "pwd"))).

client_connect_(Packet, RecvSize) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 1883, [binary, {packet, raw}, {active, false}]),
    gen_tcp:send(Sock, Packet),
    gen_tcp:recv(Sock, RecvSize, 3000),
    Sock.

connect_dashbaord_(Method, Api, Auth) ->
    Url = "http://127.0.0.1:18083/" ++ Api,
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

auth_header_() ->
    auth_header_("admin", "public").

auth_header_(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

connect_dashbaord_(Method, Api, Params, Auth) ->
    Url = "http://127.0.0.1:18083/" ++ Api,
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
