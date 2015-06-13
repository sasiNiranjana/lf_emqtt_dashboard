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

all_module_attributes(behaviour) -> {}.

%%----------------------------------------------------------------------------

web_ui() -> [{javascript, <<"dispatcher.js">>}].

dispatcher() ->
    [{["overview"],                                                mqttd_dashboard_wm_overview, []},
     {["users"],                                                   mqttd_dashboard_wm_users, []},
     {["users", user],                                             mqttd_dashboard_wm_user, []},
     {["users", user, "permissions"],                              mqttd_dashboard_wm_permissions_user, []}
    ].