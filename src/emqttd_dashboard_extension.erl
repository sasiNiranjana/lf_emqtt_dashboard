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

%% @doc emqttd web dashboard extension behaviour.
%% @author Huang Dan
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

