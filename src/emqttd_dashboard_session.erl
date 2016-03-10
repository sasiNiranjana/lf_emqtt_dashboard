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

%% @doc Action for session api.
-module(emqttd_dashboard_session).

-include("emqttd_dashboard.hrl").
-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-import(emqttd_dashboard_util, [connected_at_format/1]).
-import(proplists, [get_value/2, get_value/3]).

-export([execute/3]).

execute(CurrPage, PageSize, _ClientKey) ->
    %% Count total number.
    TotalNum = count(),
    TotalPage = 
        case TotalNum rem PageSize of
            0 -> TotalNum div PageSize;
            _ -> (TotalNum div PageSize) + 1
        end,
	Result = [querysession(Tab, CurrPage, TotalPage, PageSize)|| Tab <- [mqtt_transient_session, mqtt_persistent_session]],
    [{currentPage, CurrPage},
     {pageSize, PageSize},
     {totalNum, TotalNum},
     {totalPage, TotalPage},
     {result, lists:append(Result)}].

count() ->
    lists:sum([length(ets:match_object(Tab, '$1')) || Tab <-[mqtt_transient_session, mqtt_persistent_session]]).


querysession(Tab, CurrentPage, TotalPage, PageSize) ->
	if CurrentPage > TotalPage ->
		[];
	true ->
		Obj = ets:match_object(Tab, '$1', PageSize),
		parser_session(CurrentPage, Obj)
	end.

parser_session(_, Obj) when is_atom(Obj), Obj =:= '$end_of_table'->
    [];
parser_session(1, {Matchs, _C}) ->
    [session_table(Match)|| Match <- Matchs];
parser_session(Times, {_Matchs, C}=Obj) ->
    if is_atom(C), C =:= '$end_of_table' ->
	parser_session(1, Obj);
    true->
	parser_session(Times-1, ets:match_object(C))
    end.

session_table({{ClientId, _Pid}, Session}) ->
    InfoKeys = [clean_sess, 
             max_inflight,
             inflight_queue,
             message_queue,
             message_dropped,
             awaiting_rel,
             awaiting_ack,
             awaiting_comp,
             created_at],
     [{clientId, ClientId} | [{Key, format(Key, get_value(Key, Session))} || Key <- InfoKeys]].

format(created_at, Val) ->
    list_to_binary(connected_at_format(Val));
format(_, Val) ->
    Val.

