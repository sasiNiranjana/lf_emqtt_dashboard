%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2012-2016, Feng Lee <feng@emqtt.io>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc emqttd web dashboard dispatcher.
%%%
%%% @author Huang Dan
%%%-----------------------------------------------------------------------------
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
