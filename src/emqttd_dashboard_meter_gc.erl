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

-record(gc_state, {gc_interval,
                   gc_table,
                   gc_timer}).

-export([start_link/1,
         named/1]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-define(INTERVAL, 5000).

start_link(Table) ->
    case gen_server:start_link({local, named(Table)}, 
        ?MODULE, [Table], []) of
    {ok, Pid} -> 
        register(named(Table), Pid),
        {ok, Pid};
    Else ->
        Else
    end.

init(Table) ->
    {ok, set_gc_timer(#gc_state{gc_table = Table,
                                gc_interval = ?INTERVAL})}.

handle_call(_Req, _From, State) ->
    reply(unexpected, State).

handle_cast(_Req, State) ->
    noreply(State). 

handle_info(gc, State) ->
    noreply(State);

handle_info(_Info, State) ->
    noreply(State). 

code_change(_OldVsn, _Extra, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

reply(Reply, NewState) ->
    {reply, Reply, NewState}.

noreply(NewState) ->
    {noreply, NewState}.

named(Table) ->
    list_to_atom(atom_to_list(Table) ++ "_gc").

set_gc_timer(#gc_state{gc_interval = Interval} = State) ->
    TRef = erlang:send_after(Interval, self(), gc),
    State#gc_state{gc_timer = TRef}.
