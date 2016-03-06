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

%% @doc emqttd web dashboard.
-module(emqttd_dashboard).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-import(proplists, [get_value/2, get_value/3]).
-export([handle_request/1]).

-define(SEPARTOR, $\/).

-define(KB, 1024).
-define(MB, (1024*1024)).
-define(GB, (1024*1024*1024)).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


handle_request(Req) ->
    case authorized(Req) of
	true ->
		Path = Req:get(path),
    		handle_request(Path, Req);
	false->
		Req:respond({401, [{"WWW-Authenticate", "Basic Realm=\"emqttd dashboad\""}], []})
    end.
   
handle_request("/api/" ++ Path, Req) when length(Path) > 0 ->
    api(list_to_atom(Path), Req);

handle_request("/" ++ Rest, Req) ->
    mochiweb_request:serve_file(Rest, docroot(), Req).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

%%-----------------------------------overview--------------------------------------

%% broker info
api(stats, Req) ->
    Stats = [{Stat, Val} || {Stat, Val} <- emqttd_stats:getstats()],
    api_respond(Req, Stats);
   
api(ptype, Req) ->
    PortTyeps = emqttd_vm:get_port_types(), 
    api_respond(Req, PortTyeps);
   
api(memory, Req) ->
    Memory = emqttd_vm:get_memory(), 
    api_respond(Req, Memory);
    
api(cpu, Req) ->
    Cpu = emqttd_vm:loads(), 
    api_respond(Req, Cpu);

api(node, Req) ->
    Nodes = [node()|nodes()],
    NodeInfo = lists:map(fun(Node)-> 
			    CpuInfo = [{K, list_to_binary(V)} || {K, V} <- rpc:call(Node, emqttd_vm, loads, [])],
			    Memory = rpc(Node, emqttd_vm, get_memory, []),
			    [{name, Node},
		             {total_memory, kmg(proplists:get_value(allocated, Memory))},
			     {used_memory, kmg(proplists:get_value(used, Memory))},
			     {process_available, rpc(Node, emqttd_vm, get_process_limit, [])},
			     {process_used, length(rpc(Node, emqttd_vm, get_process_list, []))},
			     {max_fds, proplists:get_value(max_fds,rpc(Node, emqttd_vm, get_system_info, [check_io]))}|CpuInfo]
     			 end, Nodes),
    api_respond(Req, NodeInfo);
    
api(metrics, Req) ->
    Metrics = [{Metric, Val} || {Metric, Val} <- emqttd_metrics:all()],
    api_respond(Req, Metrics);
   
api(listeners, Req) ->
    Llists = lists:map(fun({{Protocol, Port}, Pid})-> 
		MaxClients = esockd:get_max_clients(Pid),	
	 	CurrentClients = esockd:get_current_clients(Pid),
	 	[{protocol, Protocol},{port, Port},{max_clients, MaxClients},{current_clients, CurrentClients}] 
	      end, esockd:listeners()),
    api_respond(Req, Llists);
    
%%-----------------------------------clients--------------------------------------
%%clients api
api(clients, Req) ->
    ClientQry = Req:parse_post(),
    CurrentPage = int(get_value("curr_page", ClientQry, "1")),
    PageSize = int(get_value("page_size", ClientQry, "10")),
    %Username = get_value("user_key", ClientQry, ""),
    TotalNum = clientcount(mqtt_client), 
    TotalPage = case TotalNum rem PageSize of
		0 -> TotalNum div PageSize;
		_ -> (TotalNum div PageSize) + 1
		    end,
    Result = queryclient(mqtt_client, CurrentPage, TotalPage, PageSize), 
    api_respond(Req, [{currentPage, CurrentPage},{pageSize, PageSize},{totalNum, TotalNum}, {totalPage, TotalPage},{result, Result}]);
    
%%-----------------------------------session--------------------------------------
%%sessin check api
api(sessions, Req) ->
    Records = [emqttd_vm:get_ets_object(Tab) || Tab <- [mqtt_transient_session, mqtt_persistent_session]],
    InfoKeys = [clean_sess, 
                max_inflight,
                inflight_queue,
                message_queue,
                message_dropped,
                awaiting_rel,
                awaiting_ack,
                awaiting_comp,
                created_at],

    Sessions = [ [{clientId, ClientId} | session_table(Session, InfoKeys)]
                    || {{ClientId, _Pid}, Session} <- lists:append(Records)],

    api_respond(Req, Sessions);
   
%%-----------------------------------topic--------------------------------------
%%topic api
api(topics, Req) ->
    F = fun() -> qlc:e(qlc:q([E || E <- mnesia:table(topic)])) end,
    {atomic, Topics} =  mnesia:transaction(F),
    Body = [[{topic, Topic}, {node, Node}] || #mqtt_topic{topic = Topic,
                                                          node = Node} <- Topics],
    api_respond(Req, Body);
   
%%-----------------------------------subscribe--------------------------------------
%%subscribe api
 
api(subscriptions, Req) ->
    Subscriptions =
    case catch mnesia:dirty_all_keys(subscription) of
        {'EXIT', _Error} ->
            [];
        Keys ->
            RowFun = fun(Key) ->
                       Records = ets:lookup(subscription, Key),
                       [{clientId, Key}, {subscriptions, format(subscriptions, Records)}]
                     end,
            [RowFun(Key) || Key <- Keys]
    end,
    api_respond(Req, Subscriptions);

%%-----------------------------------Users--------------------------------------
%%users api
api(users, Req) ->
    F = fun(#mqtt_admin{username = Username, tags = Tags}) ->
            [{name, Username}, {tag, Tags}]
        end,
    api_respond(Req, [F(Admin) || Admin <- emqttd_vm:get_ets_object(mqtt_admin)]);
 
api(update_user, Req) ->
    User = Req:parse_post(),
    Username = proplists:get_value("user_name", User),
    Password = proplists:get_value("password", User),
    Tag = proplists:get_value("tag", User),
    Status = emqttd_dashboard_admin:update_user(bin(Username), bin(Password), bin(Tag)),
    RespondCode = code(Status),
    api_respond(Req, RespondCode);
 
api(remove_user, Req) ->
    User = Req:parse_post(),
    Username = proplists:get_value("user_name", User),
    Status = emqttd_dashboard_admin:remove_user(bin(Username)),
    lager:error("Status =~p", [Status]),
    RespondCode = code(Status),
    api_respond(Req, RespondCode);
 
api(add_user, Req) ->
    User = Req:parse_post(),
    Username = proplists:get_value("user_name", User),
    Password = proplists:get_value("password", User),
    Tag = proplists:get_value("tag", User),
    Status = emqttd_dashboard_admin:add_user(bin(Username), bin(Password), bin(Tag)),
    RespondCode = code(Status),
    api_respond(Req, RespondCode);
 
api(current_user, Req) ->
    "Basic " ++ BasicAuth =  Req:get_header_value("Authorization"),
    {Username, _Password} = user_passwd(BasicAuth),
    api_respond(Req, [{username, Username}]);

api(logout, Req) ->
    Req:respond({401, [{"WWW-Authenticate", "Basic Realm=\"emqttd dashboad\""}], []});

api(bnode, Req) ->
    Node = node(),
    api_respond(Req, [{node, Node}]).

api_respond(Req, Body) ->
    JsonData = if
                   length(Body) == 0 -> <<"\[\]">>;
                   true -> list_to_binary(mochijson2:encode(Body) ++ <<10>>)
               end,
    Req:respond({200, [{"Content-Type","application/json"}], JsonData}).

connected_at_format(Timestamp) ->
    strftime(datetime(emqttd_time:now_to_secs(Timestamp))).

strftime({{Y,M,D}, {H,MM,S}}) ->
    Date = string:join([zeropad(I) || I <- [Y,M,D]], "-"),
    Time = string:join([zeropad(I) || I <- [H, MM, S]], ":"),
    lists:concat([Date, " ", Time]).
	        
datetime(Timestamp) when is_integer(Timestamp) ->
    Universal = calendar:gregorian_seconds_to_datetime(Timestamp +
	calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    calendar:universal_time_to_local_time(Universal).

zeropad(I) when I < 10 ->
    lists:concat(["0", I]);
zeropad(I) ->
    integer_to_list(I).

  
%%------------------------------------------------------------------------------
%% basic authorization
%%------------------------------------------------------------------------------
authorized(Req) ->
    case Req:get_header_value("Authorization") of
	undefined ->
		false;
	"Basic " ++ BasicAuth ->
        {Username, Password} = user_passwd(BasicAuth),
        case emqttd_dashboard_admin:check(bin(Username),  bin(Password)) of
            ok ->
                true;
            {error, Reason} ->
                lager:error("HTTP Auth failure: username=~s, reason=~p", [Username, Reason]),
                false
        end
	end.

user_passwd(BasicAuth) ->
	list_to_tuple(binary:split(base64:decode(BasicAuth), <<":">>)). 

code(ok) -> 1;
code(ignore) -> 2.

session_table(Session, InfoKeys) ->
    CreatedAt = list_to_binary(connected_at_format(proplists:get_value(created_at, Session))),
    NewSession = lists:keyreplace(created_at, 1, Session, {created_at, CreatedAt}),
    [{Key, proplists:get_value(Key, NewSession)} || Key <- InfoKeys].

rpc(Node, M, F, A) ->
   rpc:call(Node, M, F, A).

kmg(Byte) when Byte > ?GB ->
    float(Byte / ?GB, "G");
kmg(Byte) when Byte > ?MB ->
    float(Byte / ?MB, "M");
kmg(Byte) when Byte > ?KB ->
    float(Byte / ?MB, "K");
kmg(Byte) ->
    Byte.

float(F, S) ->
    iolist_to_binary(io_lib:format("~.2f~s", [F, S])).

format(subscriptions, Subscriptions) ->
    list_to_binary(
        string:join([io_lib:format("~s:~w", [Topic, Qos]) ||
                #mqtt_subscription{topic = Topic, qos = Qos} <- Subscriptions], ",")).

bin(S) when is_list(S) -> list_to_binary(S);
bin(A) when is_atom(A) -> bin(atom_to_list(A));
bin(B) when is_binary(B) -> B.

%%%%%%% enhancement mnesia paging%%
%count(TableName) ->
%    case list(TableName) of
%	{error, Msg} -> {error, Msg};
%	{ok, []}     -> 0;
%	{ok, L}      -> length(L)
%    end.
%
%count(TableName, Where) ->
%    case list(TableName, Where) of
%	{error, Msg} -> {error, Msg};
%	{ok, []}     -> 0;
%	{ok, L}      -> length(L)
%    end.
%
%list(Table) ->
%    F = fun() ->
%	qlc:e(qlc:q([X || X <- mnesia:table(Table)]))
%    end,
%    {atomic, L} = mnesia:transaction(F),
%    {ok, L}.
%
%list(Table, Where) ->
%    case query_fun(Table, Where) of 
%	{error, Msg} -> {error, Msg};
%	{ok, F}      -> 
%		{atomic, L} = mnesia:transaction(F),
%    		{ok, L}
%    end.
%
%list(Table, Offset, Limit) ->
%	if is_integer(Offset) and is_integer(Limit)
%		and (Offset > 0) and (Limit >0) ->
%		F = fun()-> 
%			QH = qlc:q([X || X<- mnesia:table(Table)]),
%			Qc = qlc:cursor(QH),
%			case Offset of
%				1 -> skip;
%				_ -> qlc:next_answers(Qc, Offset-1)
%			end,
%			qlc:next_answers(Qc, Limit)
%	end,
%	{atomic, L} = mnesia:transaction(F),
%	{ok ,L};
%	true ->
% 	{error, badarg}	
%	end.
%
%list(Table, Offset, Limit, Where) ->
%    if is_integer(Offset) and is_integer(Limit)
%		and (Offset > 0) and (Limit >0) ->
%	case query_fun(Table, Offset, Limit, Where) of
%		{error, Msg} -> {error, Msg};
%		{ok, F} ->
%			{atomic, L} = mnesia:transaction(F),
%			{ok, L}
%
%	end;
%    true ->
% 	{error, badarg}	
%    end.
%
%query_fun(Table, Where) ->
%    case query_cond(Table, Where) of
%	    {ok, QH} ->
%		    {ok, fun() -> qlc:e(QH) end};
%	    {error, Msg} ->
%		    {error, Msg}
%    end.
%
%query_fun(Table, Offset, Limit, Where) ->
%    case query_cond(Table, Where) of
%	{ok, QH} ->
%		{ok , fun()-> 
%			Qc = qlc:cursor(QH),
%			case Offset of
%				1 -> skip;
%				_ -> qlc:next_answers(Qc, Limit)
%		       	end 
%		      end};
%	{error, Msg} ->
%		{error, Msg}
%    end.
%
%query_cond(mqtt_client, Where) ->
%    case Where of
%	[{username, Username}] ->
%		QH = qlc:q([X || X <- mnesia:table(mqtt_client), match(X#mqtt_client.username, Username)]),
%		{ok, QH};
%	[] ->
%		{error, badarg}
%    end;
%
%query_cond(_, _) ->
%	{error, badarg}.

%match(_DbContent, []) ->
%    true;
%match(DbContent, WContent) ->
%    case string:str(DbContent, WContent) of
%	1 -> true;
%	_ -> false
%    end.

clientcount(Tab) ->
    MScount = ets:fun2ms(fun(#mqtt_client{client_id = ClientId, peername = {IpAddr, Port},
                         username = Username, clean_sess = CleanSess,
                         proto_ver = ProtoVer, keepalive = KeepAlvie,
			 connected_at = ConnectedAt}) -> true end),
    ets:select_count(Tab, MScount).


queryclient(Tab, CurrentPage, TotalPage, PageSize) ->
	if CurrentPage > TotalPage ->
		[];
	true ->
		Obj = ets:match_object(Tab, #mqtt_client{_='_'}, PageSize),
		parser_obj(CurrentPage, Obj)
	end.

parser_obj(_, Obj) when is_atom(Obj), Obj =:= '$end_of_table'->
    [];
parser_obj(1, {Matchs, _C}) ->
    [formatclient(Match)|| Match <- Matchs];
parser_obj(2, {_Matchs, C}) ->
    parser_obj(1, ets:match_object(C));
parser_obj(Times, {_Matchs, C}=Obj) ->
    if is_atom(C), C =:= '$end_of_table' ->
	parser_obj(1, Obj);
    true->
	parser_obj(Times-1, ets:match_object(C))
    end.

int(S) ->
    list_to_integer(S).

%str(A) when is_atom(A) -> atom_to_list(A);
%str(B) when is_binary(B) -> binary_to_list(B);
%str(L) when is_list(L) -> L.
%
formatclient(#mqtt_client{client_id = ClientId, peername = {IpAddr, Port},
                         username = Username, clean_sess = CleanSess,
                         proto_ver = ProtoVer, keepalive = KeepAlvie,
			 connected_at = ConnectedAt})->
	[{clientId, ClientId}, 
       	 {username, Username}, 
 	 {ipaddress, list_to_binary(emqttd_net:ntoa(IpAddr))}, 
	 {port, Port},
         {clean_sess, CleanSess},
         {proto_ver, ProtoVer},
         {keepalive, KeepAlvie},
         {connected_at, list_to_binary(connected_at_format(ConnectedAt))}].


