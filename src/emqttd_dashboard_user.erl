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

%% @doc Action for user api.
-module(emqttd_dashboard_user).

-include("emqttd_dashboard.hrl").
-include("../../../include/emqttd.hrl").
-export([users/0, update/3, remover/1, add/3]).

%%-----------------------------------Users--------------------------------------
%%users api
users() ->
    F = fun(#mqtt_admin{username = Username, tags = Tags}) ->
            [{name, Username}, {tag, Tags}]
        end,
    [F(Admin) || Admin <- emqttd_vm:get_ets_object(mqtt_admin)].
 
update(Username, Password, Tag) ->
    Status = emqttd_dashboard_admin:update_user(Username, Password, Tag),
    io:format("~p",[Status]),
    code(Status).
 
remover(Username) ->
    Status = emqttd_dashboard_admin:remove_user(Username),
    code(Status).
 
add(Username, Password, Tag) ->
    Status = emqttd_dashboard_admin:add_user(Username, Password, Tag),
    code(Status).
 
code(ok) -> 1;
code(ignore) -> 2.


