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

%% @doc Session API.
-module(emqttd_dashboard_session).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-import(proplists, [get_value/2]).

-export([execute/3]).

-http_api({"sessions", execute, [{"curr_page", int, "1"},
                                 {"page_size", int, "100"},
                                 {"client_key", string, ""}]}).

execute(PageNum, PageSize, _ClientKey) ->
    %% Count total number.
    TotalNum = count(),
    {CurrPage, TotalPage} = emqttd_dashboard:paginate(TotalNum, PageNum, PageSize),
	Result = [querysession(Tab, CurrPage, TotalPage, PageSize) ||
                Tab <- [mqtt_transient_session, mqtt_persistent_session]],
    {ok, [{currentPage, CurrPage},
          {pageSize, PageSize},
          {totalNum, TotalNum},
          {totalPage, TotalPage},
          {result, lists:append(Result)}]}.

count() ->
    ets:info(mqtt_transient_session, size) + ets:info(mqtt_persistent_session, size).

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
    InfoKeys = [clean_sess, max_inflight, inflight_queue, message_queue,
                message_dropped, awaiting_rel, awaiting_ack, awaiting_comp, created_at],
     [{clientId, ClientId} | [{Key, format(Key, get_value(Key, Session))} || Key <- InfoKeys]].

format(created_at, Val) ->
    list_to_binary(emqttd_dashboard:strftime(Val));
format(_, Val) ->
    Val.

