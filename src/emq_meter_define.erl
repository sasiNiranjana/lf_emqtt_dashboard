%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 Feng Lee <feng@emqtt.io>.
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

%% @doc Creation and definition of various indicators.

-module(emq_meter_define).

-behaviour(gen_server).

-include("emq_dashboard_meter.hrl").

-define(SERVER, ?MODULE).

-record(state, {metrics_data = []}).

%% api function
-export([start_link/0]).

%% gen_server functions export
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Behaviour callback
%%--------------------------------------------------------------------

init([]) ->
    new_metrics(),
    set_timer(reporter, ?REPORT_INTERVAL),
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reporter, #state{metrics_data = MetsData} = State) ->
    TS = timestamp(),
    Data = emqttd_metrics:all(),
    %% Update metrics values
    Fun = fun(Met) ->
            case proplists:get_value(Met, Data) of
                undefined -> ignore;
                Value     ->
                    V = last_value(MetsData, Met),
                    folsom_metrics:notify({Met, Value - V})
            end
          end,
    lists:foreach(Fun, ?METRICS),

    lists:foreach(fun(Met) ->
                    V = folsom_metrics:get_metric_value(Met),
                    emq_meter_access:save_data(Met, TS, V)
                    %io:format("Metric, ~p ~p ~p~n", [Met, TS, V])
                  end, ?METRICS),

    set_timer(reporter, ?REPORT_INTERVAL),
    {noreply, State#state{metrics_data = Data}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

set_timer(Name, Timeout) ->
    erlang:send_after(Timeout, self(), Name).

new_metrics() ->
    NewMet = fun({Fun, Met}) ->
               erlang:apply(folsom_metrics, Fun, [Met])
             end,
    lists:foreach(NewMet, ?METRICS_DEF).

last_value(MetsData, Met) ->
    case proplists:get_value(Met, MetsData) of
        undefined -> 0;
        Value     -> Value
    end.

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
