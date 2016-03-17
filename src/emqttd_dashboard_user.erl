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

%% @doc User API.
-module(emqttd_dashboard_user).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-import(emqttd_dashboard_admin, [add_user/3, remove_user/1, update_user/3]).

-export([users/0, update/3, remover/1, add/3]).

-http_api({"users", users, []}).

-http_api({"update_user", update,  [{"user_name", binary, ""},
                                    {"password", binary, ""},
                                    {"tags", binary, ""}]}).
-http_api({"remove_user", remover, [{"user_name", binary, ""}]}).

-http_api({"add_user",    add,     [{"user_name", binary, ""},
                                    {"password", binary, ""},
                                    {"tags", binary, ""}]}).

users() ->
    F = fun(#mqtt_admin{username = Username, tags = Tags}) ->
            [{name, Username}, {tag, Tags}]
        end,
    {ok, [F(Admin) || Admin <- ets:tab2list(mqtt_admin)]}.
 
update(Username, Password, Tag) ->
    Status = update_user(Username, Password, Tag),
    {ok, code(Status)}.

remover(<<"admin">>) ->
    {ok, [{status, failure},{reason, list_to_binary("admin cannot be deleted")}]};

remover(Username) ->
    {ok, code(remove_user(Username))}.
 
add(Username, Password, Tag) ->
    {ok, code(add_user(Username, Password, Tag))}.
 
code(ok)              -> [{status, success}];
code({error, Reason}) -> [{status, failure}, {reason, list_to_binary(Reason)}].

