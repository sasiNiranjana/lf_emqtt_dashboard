%%--------------------------------------------------------------------
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

%% @doc The Module for emqttd_dashboard util.
-module(emqttd_dashboard_util).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-compile(export_all).

-define(KB, 1024).
-define(MB, (1024*1024)).
-define(GB, (1024*1024*1024)).

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

connected_at_format(Timestamp) ->
    strftime(datetime(emqttd_time:now_to_secs(Timestamp))).

strftime({{Y,M,D}, {H,MM,S}}) ->
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, MM, S])).

datetime(Timestamp) when is_integer(Timestamp) ->
    Universal = calendar:gregorian_seconds_to_datetime(Timestamp +
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    calendar:universal_time_to_local_time(Universal).

currentpage(1) ->
        1;
currentpage(CurrPage) ->
        CurrPage.
currentpage(CurrPage, TotalPage)->
    if CurrPage > TotalPage ->
       currentpage(TotalPage);
    true ->
       currentpage(CurrPage)
    end.
 
