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

%% @doc The number of indicators of persistence.

-module(emqttd_meter_access).

-behaviour(gen_server).

-include("emqttd_dashboard_meter.hrl").

-define(Suffix, ".dets").
-define(SERVER, ?MODULE).
-define(GC_INTERVAL, 1000 * 60 * 30).
-define(MAXSIZE, (7 * 60 * 60 * 24 * 1000) div (60 * 1000)).

-record(state, {extent_1 = [], extent_2 = [], extent_3 = []}).

%% gen_server functions export
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API functions
-export([start_link/0, save_data/3, get_data/3, get_data_all/2]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_data_all(Minutes, Interval) ->
    [get_data(M, Minutes, Interval) || M <- ?METRICS].

%% Access to specify minutes metric data recently.
get_data(Met, Minutes, Interval) ->
    Metric = metric_name(Met, Interval),
    
    open_table(Metric),
    TotalNum = dets:info(Metric, size),
    Qh = qlc:sort(dets:table(Metric)),
    End = timestamp(), Start = End - (Minutes * 60),
    Limit = (Minutes * 60 * 1000) div Interval,
    Cursor = qlc:cursor(Qh),
    case TotalNum > Limit of
        true  -> qlc:next_answers(Cursor, TotalNum - Limit);
        false -> ignore
    end,
    Rows = case TotalNum >= 1 of
               true  -> qlc:next_answers(Cursor, TotalNum);
               false -> []
           end,
    qlc:delete_cursor(Cursor),
    close_table(Metric),
    
    L = [[{x, Ts}, {y, V}] || {Ts, V} <- Rows, Ts >= Start],
    case L of
        [[{x, Ts}, {y, _V}]|_T] ->
            if  (Ts - Start) >= 30 * 60 ->
                    {Met, [[{x, Start}, {y, 0}]] ++ L};
                true -> {Met, L}
            end;
        [] -> {Met, [[{x, Start}, {y, 0}], [{x, End}, {y, 0}]]}
    end.

%% Save the Metric data, and do a merge.
save_data(Metric, Ts, Value) ->
    gen_server:cast(?SERVER, {save_data, Ts, Value, Metric}).


%%--------------------------------------------------------------------
%% Behaviour callback
%%--------------------------------------------------------------------

init([]) ->
    %close_table(),
    %open_table(),
    set_timer(metrics_gc, ?GC_INTERVAL),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, unexpect, State }.

handle_cast({save_data, Ts, Value, Metric}, State) ->
    % Save the basic data (the original).
    open_table(Metric),
    dets:insert(Metric, {Ts, Value}),
    close_table(Metric),
    
    % Do a data merge.
    #state{extent_1 = Extent1, extent_2 = Extent2, extent_3 = Extent3} = State,
    State1 = merge_data(Metric, Ts, Value, ?INTERVAL_1, Extent1, State),
    State2 = merge_data(Metric, Ts, Value, ?INTERVAL_2, Extent2, State1),
    State3 = merge_data(Metric, Ts, Value, ?INTERVAL_3, Extent3, State2),
    {noreply, State3};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(metrics_gc, State) ->
    metrics_gc(),
    set_timer(metrics_gc, ?GC_INTERVAL),
    {noreply, State}.

terminate(_Reason, _State) ->
    %close_table(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

metric_name(Met, Interval) ->
    case Interval of
        ?INTERVAL_1 ->
            list_to_atom(atom_to_list(Met) ++ "/1");
        ?INTERVAL_2 ->
            list_to_atom(atom_to_list(Met) ++ "/2");
        ?INTERVAL_3 ->
            list_to_atom(atom_to_list(Met) ++ "/3");
        ?REPORT_INTERVAL -> Met
    end.

merge_data(Met, Ts, Value, Interval, Extent, State) ->
    Metric = metric_name(Met, Interval),
    {[Start, _End], NewState} = 
        case time_extent(Ts, Interval) of
            Extent ->
                {Extent, State};
            Other  ->
                NState = 
                case Interval of
                    ?INTERVAL_1 -> State#state{extent_1 = Other};
                    ?INTERVAL_2 -> State#state{extent_2 = Other};
                    ?INTERVAL_3 -> State#state{extent_3 = Other}
                end,
                {Other, NState}
        end,
    update_met(Metric, Start, Value),
    NewState.

update_met(Met, Key, Value) ->
    open_table(Met),
    case dets:lookup(Met, Key) of
        [] -> dets:insert(Met, {Key, Value});
        _  -> dets:update_counter(Met, Key, {2, Value})
    end,
    close_table(Met).

set_timer(Name, Timeout) ->
    erlang:send_after(Timeout, self(), Name).

open_table(Tab) ->
    Path = filename:join([code:root_dir(), "data", "metrics"]),
    case file:make_dir(Path) of
        ok -> ok;
        {error, eexist} -> ignore
    end,
    FileName = filename_replace(atom_to_list(Tab)),
    File = filename:join(Path, FileName),
    case dets:open_file(Tab, [{file, File}]) of
        {ok, Tab} -> ok;
        {error, _Reason} -> exit(edestOpen)
    end.

close_table(Tab) ->
    dets:close(Tab).

filename_replace(Src) when is_list(Src) ->
    Des = re:replace(Src, "/", "_", [global, {return, list}]),
    Des ++ ?Suffix.

%% @doc To get the data points in the time interval.
time_extent(Ts, Interval) ->
    {{Year, Month, Day}, {Hour, Minite, Second}} = timestamp_to_datetime(Ts),
    case Interval of
        ?INTERVAL_1 -> 
            Begin = datetime_to_timestamp({{Year, Month, Day}, {Hour, 0, 0}}),
            TimePeriods = time_periods(Begin, ?INTERVAL_1 div 1000, 60),
            [H|_T] = [[Start, End] || [Start, End] <- TimePeriods, Ts >= Start, Ts < End],
            H;
        ?INTERVAL_2 -> 
            {{Year, Month, Day}, {Hour, Minite, Second}} = timestamp_to_datetime(Ts),
            Begin = datetime_to_timestamp({{Year, Month, Day}, {Hour, 0, 0}}),
            TimePeriods = time_periods(Begin, ?INTERVAL_2 div 1000, 4),
            [H|_T] = [[Start, End] || [Start, End] <- TimePeriods, Ts >= Start, Ts < End],
            H;
        ?INTERVAL_3 -> 
            {{Year, Month, Day}, {Hour, Minite, Second}} = timestamp_to_datetime(Ts),
            Begin = datetime_to_timestamp({{Year, Month, Day}, {0, 0, 0}}),
            TimePeriods = time_periods(Begin, ?INTERVAL_3 div 1000, 24),
            [H|_T] = [[Start, End] || [Start, End] <- TimePeriods, Ts >= Start, Ts < End],
            H
    end.

time_periods(_Begin, _Interval, 0)   ->
    [];
time_periods(Begin, Interval, Count) ->
    End = Begin + Interval,
    [[Begin, End]] ++ time_periods(End, Interval, Count - 1).

datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

metrics_gc() ->
    Fun =
    fun(Metric) ->
        open_table(Metric),
        Total = dets:info(Metric, size),
        gc_batch(Metric, ?MAXSIZE, Total),
        close_table(Metric)
    end,
    lists:foreach(Fun, ?METRICS_TABS).

gc_batch(_Table, Max, Total)  when Max >= Total ->
    ignore;
gc_batch(Table, Max, Total)  ->
    RowInx = Total - Max,
    Qh = qlc:sort(dets:table(Table)),
    Cursor = qlc:cursor(Qh),
    Rows = qlc:next_answers(Cursor, RowInx),
    qlc:delete_cursor(Cursor),
    lists:foreach(fun({Key, _V}) ->
                    dets:delete(Table, Key)
                  end, Rows).

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
