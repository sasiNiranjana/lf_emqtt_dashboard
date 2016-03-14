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

%% @doc Action for client api.
-module(emqttd_dashboard_client).

-include("emqttd_dashboard.hrl").
-include("../../../include/emqttd.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-import(emqttd_dashboard_util, [connected_at_format/1, currentpage/1, currentpage/2]).
-export([execute/3]).

execute(CurrPage, PageSize, _ClientKey) ->
    %% Count total number.
    TotalNum = count(mqtt_client),
    TotalPage = 
        case TotalNum rem PageSize of
            0 -> TotalNum div PageSize;
            _ -> (TotalNum div PageSize) + 1
        end,
    CurrPage2 = currentpage(CurrPage, TotalPage), 
    Result = query(mqtt_client, CurrPage2, PageSize, TotalPage),
    [{currentPage, CurrPage2},
     {pageSize, PageSize},
     {totalNum, TotalNum},
     {totalPage, TotalPage},
     {result, Result}].

count(Tab) ->
    MScount = ets:fun2ms(fun(#mqtt_client{_='_'})-> true end),
    ets:select_count(Tab, MScount).

query(Tab, CurrPage, PageSize, TotalPage) ->
    if CurrPage > TotalPage ->
        [];
    true                    ->
        Obj = ets:match_object(Tab, #mqtt_client{_='_'}, PageSize),
        parser_obj(CurrPage, Obj)
    end.

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
     {connected_at, list_to_binary(connected_at_format(ConnectedAt))}].


