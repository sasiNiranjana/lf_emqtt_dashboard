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

%% @doc Clients API.
-module(emqttd_dashboard_client).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-http_api({"clients", execute, [{"curr_page", int, "1"},
                                {"page_size", int, "100"},
                                {"client_key", string, ""}]}).

-export([execute/3]).

execute(PageNum, PageSize, _ClientKey) ->
    TotalNum = ets:info(mqtt_client, size),
    {CurrPage, TotalPage} = emqttd_dashboard:paginate(TotalNum, PageNum, PageSize),
    Result = query(mqtt_client, CurrPage, PageSize, TotalPage),
    {ok, [{currentPage, CurrPage},
          {pageSize,    PageSize},
          {totalNum,    TotalNum},
          {totalPage,   TotalPage},
          {result,      Result}]}.

query(_Tab, CurrPage, _PageSize, TotalPage) when CurrPage > TotalPage ->
    [];
query(Tab, CurrPage, PageSize, _TotalPage) ->
    Obj = ets:match_object(Tab, #mqtt_client{_='_'}, PageSize),
    parser_obj(CurrPage, Obj).

parser_obj(_, Obj) when is_atom(Obj),Obj =:= '$end_of_table'->
    [];
parser_obj(1, {Matchs, _C}) ->
    [format_data(Match) || Match <- Matchs];
parser_obj(Times, {_Matchs, C}=Obj) ->
    if is_atom(C), C =:= '$end_of_table' ->
        parser_obj(1, Obj);
    true                              ->
        parser_obj(Times-1, ets:match_object(C))
    end.

format_data(#mqtt_client{client_id = ClientId,
                         peername = {IpAddr, Port},
                         username = Username,
                         clean_sess = CleanSess,
                         proto_ver = ProtoVer,
                         keepalive = KeepAlvie,
                         connected_at = ConnectedAt}) ->
    [{clientId, ClientId},
     {username, Username},
     {ipaddress, list_to_binary(emqttd_net:ntoa(IpAddr))},
     {port, Port},
     {clean_sess, CleanSess},
     {proto_ver, ProtoVer},
     {keepalive, KeepAlvie},
     {connected_at, list_to_binary(emqttd_dashboard:strftime(ConnectedAt))}].

