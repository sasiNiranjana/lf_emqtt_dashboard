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

-module(emqttd_dashboard_meter_gc).

-behaviour(gen_server).

%% Metrics GC  State
-record(gc_state, {collect_interval,
                   gc_table,
                   gc_timer,
                   gc_index}).

%% API Exports
-export([start_link/1,
         named/1]).

%% gen_server Function Exports
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-define(INTERVAL, 5000).

start_link(Table) ->
    gen_server:start_link({local, named(Table)}, 
        ?MODULE, [Table], []).

named(Table) ->
    list_to_atom((atom_to_list(Table) ++ "_gc")).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([Table]) ->
    case application:get_env(emqttd_dashboard, collect_interval) of
    {ok, Interval} ->
        {ok, set_gc_timer(#gc_state{gc_table = Table,
                                collect_interval = Interval})};
    undefined ->
        {ok, set_gc_timer(#gc_state{gc_table = Table,
                                collect_interval = ?INTERVAL})}
    end.

handle_call(_Req, _From, State) ->
    reply(unexpected, State).

handle_cast(_Req, State) ->
    noreply(State). 

handle_info(gc, State) ->
    noreply(set_gc_timer(gc_batch(State)));

handle_info(_Info, State) ->
    noreply(State). 

code_change(_OldVsn, _Extra, State) ->
    {ok, State}.

terminate(_Reason, #gc_state{gc_table = Table}) ->
    dets:close(Table),
    ok.

reply(Reply, NewState) ->
    {reply, Reply, NewState}.

noreply(NewState) ->
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

set_gc_timer(State) ->
    TRef = erlang:send_after(?INTERVAL, self(), gc),
    State#gc_state{gc_timer = TRef}.

gc_batch(#gc_state{gc_table = Table, 
        collect_interval = Interval} = State) ->
%%    Max = 7 * 60 * 60 * 24 * 1000 / Interval,
    Max = 10, 
    Total = dets:info(Table, size),
    gc_batch(Max, Total, State).

gc_batch(Max, Total, State)  when is_integer(Max), 
                                  is_integer(Total),
                                  Max > Total ->
    State;

gc_batch(Max, Total, State)  ->
    Rows = Total - Max,
    gc(Rows, State).

gc(0, State) ->
    State;

gc(Rows, State = #gc_state{gc_table = Table, gc_index = Con}) ->
   Select = case Con of
            undefined ->
                dets:first(Table);
            _ ->
                dets:next(Table, Con)
            end,
    NewCont = case Select of
              '$end_of_table' ->
                undefined;
              Key ->
                remove(Table, Key),
                Key
                end,
    gc(Rows - 1, State#gc_state{gc_index = NewCont}).

remove(Table, Key) ->
    case dets:lookup(Table, key) of
        [] -> ok;
        _Metric -> 
            io:format("remove:~p, ~p ~n", [Table, Key]),
            dets:delete(Table, Key)
    end.
