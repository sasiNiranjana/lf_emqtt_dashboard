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

%% @doc emqttd web dashboard dispatcher.
-module(emqttd_dashboard_dispatcher).

-export([modules/1, build_dispatcher/1]).

-export([dispatcher/0, web_ui/0]).

-behaviour(emqttd_dashboard_extension).

build_dispatcher(Ignore) ->
    [{["api" | Path], Mod, Args} ||
        {Path, Mod, Args} <-
            lists:append([Module:dispatcher() || Module <- modules(Ignore)])].

modules(IgnoreApps) ->
    [Module || {App, Module, Behaviours} <-
                   all_module_attributes(behaviour),
               not lists:member(App, IgnoreApps),
               lists:member(emqttd_dashboard_extension, Behaviours)].

all_module_attributes(Name) ->
    Targets =
        lists:usort(
          lists:append(
            [[{App, Module} || Module <- Modules] ||
                {App, _, _}   <- application:loaded_applications(),
                {ok, Modules} <- [application:get_key(App, modules)]])),
    lists:foldl(
      fun ({App, Module}, Acc) ->
              case lists:append([Atts || {N, Atts} <- module_attributes(Module),
                                         N =:= Name]) of
                  []   -> Acc;
                  Atts -> [{App, Module, Atts} | Acc]
              end
      end, [], Targets).

module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
			lager:warning("Module ~p not found, so not scanned for boot steps. ~n", [Module]),
            %io:format("WARNING: module ~p not found, so not scanned for boot steps.~n", [Module]),
            [];
        {'EXIT', Reason} ->
            exit(Reason);
        V -> V
    end.

%%----------------------------------------------------------------------------

web_ui() -> [{javascript, <<"dispatcher.js">>}].

dispatcher() ->
    [{["overview"],                                                mqttd_dashboard_wm_overview, []},
     {["users"],                                                   mqttd_dashboard_wm_users, []},
     {["users", user],                                             mqttd_dashboard_wm_user, []},
     {["users", user, "permissions"],                              mqttd_dashboard_wm_permissions_user, []}
    ].
