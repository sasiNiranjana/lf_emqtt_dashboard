%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 EMQ Enterprise, Inc. (http://emqtt.io).
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

-module(emq_dashboard_cli2).

-export([register_cli/0, unregister_cli/0]).

-include_lib("emqttd/include/emqttd_cli.hrl").

-export([run/1]).

-import(proplists, [get_value/2]).

-behaviour(clique_handler).

register_cli() ->
    F = fun() -> emqttd_mnesia:running_nodes() end,
    clique:register_node_finder(F),
    clique:register_usage(["admins"], admins_usage()),
    register_cmd().

run([]) ->
    io:format("--------------------------------------------------------------------------------~n"),
    io:format("admins ~ts", [clique_usage:find(["admins"])]),
    io:format("--------------------------------------------------------------------------------~n");

run(Cmd) ->
    clique:run(Cmd).

register_cmd() ->
    admins_add(),
    admins_passwd(),
    admins_del().

unregister_cli() ->
    clique:unregister_usage(["admins"]),
    unregister_cmd().

unregister_cmd() ->
    clique:unregister_command(["admins", "add"]),
    clique:unregister_command(["admins", "passwd"]),
    clique:unregister_command(["admins", "del"]).

admins_add() ->
    Cmd = ["admins", "add"],
    KeySpecs = [{'username', [{typecast, fun(Username) -> list_to_binary(Username) end}]},
                {'password', [{typecast, fun(Password) -> list_to_binary(Password) end}]},
                {'tag',      [{typecast, fun(Tag) -> list_to_binary(Tag) end}]}],
    FlagSpecs = [],
    Callback =
        fun (_, Params, _) ->
            Username = get_value('username', Params),
            Password = get_value('password', Params),
            Tag = get_value('tag', Params),
            Text = case {Username, Password, Tag} of
                {undefined, _, _} ->
                           admins_usage();
                {_, undefined, _} ->
                           admins_usage();
                {_, _, _} ->
                    case emq_dashboard_admin:add_user(Username, Password, Tag) of
                    ok ->
                        io_lib:format(" ~p add successfully~n", [Username]);
                    {error, already_existed} ->
                        io_lib:format("Error:  ~p already existed~n", [Username]);
                    {error, Reason} ->
                        io_lib:format("Error: ~p~n", [Reason])
                   end
                   end,
            [clique_status:text(Text)]
        end,
    clique:register_command(Cmd, KeySpecs, FlagSpecs, Callback).


admins_passwd() ->
    Cmd = ["admins", "passwd"],
    KeySpecs = [{'username', [{typecast, fun(Username) -> list_to_binary(Username) end}]},
                {'password', [{typecast, fun(Password) -> list_to_binary(Password) end}]}],
    FlagSpecs = [],
    Callback =
        fun (_, Params, _) ->
            Username = get_value('username', Params),
            Password = get_value('password', Params),
            Text = case {Username, Password} of
                    {undefined, _} ->
                        admins_usage();
                    {_, undefined} ->
                        admins_usage();
                    {_, _} ->
                        case emq_dashboard_admin:change_password(Username, Password) of
                        ok ->
                            io_lib:format(" ~p password update is successfully~n", [Username]);
                        {error, Reason} ->
                            io_lib:format("Error: ~p~n", [Reason])
                        end
                   end,
            [clique_status:text(Text)]
        end,
    clique:register_command(Cmd, KeySpecs, FlagSpecs, Callback).

admins_del() ->
    Cmd = ["admins", "del"],
    KeySpecs = [{'username', [{typecast, fun(Username) -> list_to_binary(Username) end}]}],
    FlagSpecs = [],
    Callback =
        fun (_, Params, _) ->
            Username = get_value('username', Params),
            Text = case {Username} of
                    {undefined} ->
                        admins_usage();
                    {_} ->
                        case emq_dashboard_admin:remove_user(Username) of
                        ok ->
                            io_lib:format(" ~p deleted successfully~n", [Username]);
                        {error, Reason} ->
                            io_lib:format("Error: ~p~n", [Reason])
                        end
                    end,
            [clique_status:text(Text)]
        end,
    clique:register_command(Cmd, KeySpecs, FlagSpecs, Callback).

admins_usage() ->
    ["\n  admins add username=<Username> password=<Password> tag=<Tags>   Add dashboard user\n",
     "  admins passwd username=<Username> password=<Password>           Reset dashboard user password\n",
     "  admins del username=<Username>                                  Delete dashboard user\n"].
