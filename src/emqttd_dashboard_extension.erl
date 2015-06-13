-module(emqttd_dashboard_extension).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     %% Return a webmachine dispatcher table to integrate
     {dispatcher, 0},

     %% Return a proplist of information for the web UI to integrate
     %% this extension. Currently the proplist should have one key,
     %% 'javascript', the name of a javascript file to load and run.
     {web_ui, 0}
    ];

behaviour_info(_Other) ->
    undefined.