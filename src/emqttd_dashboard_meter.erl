%%-------------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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
%%-------------------------------------------------------------------------
-module(emqttd_dashboard_meter).

-behaviour(gen_server).

%% API Exports
-export([start_link/0,
         update_interval/1,
         get_report/2]).

%% gen_server Function Exports
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-define(INTERVAL, 10 * 1000).

-define(Suffix, ".dets").

-define(SERVER, ?MODULE).

-include("emqttd_dashboard_meter.hrl").

-record(meter_state, {interval}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_interval(Val) when is_integer(Val)  ->
    gen_server:cast(?SERVER, {update_interval, Val}).

get_report(Metric, Limit) ->
    gen_server:call(?SERVER, {get_report, Metric, Limit}).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([]) ->
    NewTab = fun(Tab) -> 
                create_table(Tab)
             end,
    lists:foreach(NewTab, ?METRICS),
    erlang:send_after(?INTERVAL, self(), collect_meter),
    case application:get_env(emqttd_dashboard, collect_interval) of
    {ok, Interval} ->
        {ok, #meter_state{interval = Interval}};
    undefined ->
        {ok, #meter_state{interval = ?INTERVAL}}
    end.

handle_call({get_report, Metric, Limit}, _From, State) ->
    Reply = get_metrics(Metric, Limit),
    reply(Reply, State);

handle_call(Req, _From, State) ->
    lager:info("unexpectd info ~p", [Req]),
    reply(unexpectd, State).

handle_cast({update_interval, Interval}, State) ->
    UpdateInterval = Interval * 1000,
    noreply(State#meter_state{interval = UpdateInterval});

handle_cast(Info, State) ->
    lager:info("unexpectd info ~p", [Info]),
    noreply(State). 

handle_info(collect_meter, #meter_state{interval = Interval}=State) ->
    MetricsData = emqttd_metrics:all(),
    save(MetricsData),
    erlang:send_after(Interval, self(), collect_meter),
    noreply(State). 

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    close_table().

reply(Reply, NewState) ->
    {reply, Reply, NewState}.

noreply(NewState) ->
    {noreply, NewState}.

%% ====================================================================
%% Internal functions
%% ====================================================================
create_table(Tab) ->
    Path = filename:join([code:root_dir(), "data", "metrics"]),
    case file:make_dir(Path) of
    ok ->
        ok;
    {error, eexist} ->
        io:format("file is exist~n"),
        ignore
    end,
    FileName = filename_replace(atom_to_list(Tab)),
    File = filename:join(Path, FileName),
    case dets:open_file(Tab, [{file, File}]) of
                 {ok, Tab} ->
                     ok;
                 {error, Reason} ->
                     io:format("reanson:~p~n", [Reason]),
                     exit(edestOpen)
         end.

close_table() ->
    CloseTab = fun(Tab) -> 
                dets:close(Tab)
             end,
    lists:foreach(CloseTab, ?METRICS).
 

save(Data) ->
    Ts = timestamp(),
    Fun = fun(Metric) ->
        case proplists:get_value(Metric, Data) of
            undefined -> ignore;
            V -> dets:insert(Metric, {Ts, V})
        end
        end,
    lists:foreach(Fun, ?METRICS).

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

get_metrics(all, Limit) ->
    get_metrics(?METRICS, Limit);

get_metrics(Metric, Limit) when is_atom(Metric) ->
    Result = dets:match(Metric, '$1', Limit),
    List = case Result of
             {M, _C} -> M;
             '$end_of_table' -> []
           end,
    {Metric, List};

get_metrics(Metrics, Limit) when is_list(Metrics) ->
    [get_metrics(M, Limit) || M <- Metrics].

filename_replace(Src) when is_list(Src) ->
    Des = re:replace(Src, "/", "_", [global, {return, list}]),
    Des ++ ?Suffix.
