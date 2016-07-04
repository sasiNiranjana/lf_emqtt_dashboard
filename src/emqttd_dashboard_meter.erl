-module(emqttd_dashboard_meter).

-behaviour(gen_server).

-export([start_link/0,
         update_interval/1,
         get_report/2]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-define(INTERVAL, 10 * 1000).

-define(SERVER, ?MODULE).

-define(METRICS, ['bytes/received',
                  'bytes/sent',
                  'messages/dropped',
                  'messages/qos0/received',
                  'messages/qos0/sent',
                  'messages/qos1/received',
                  'messages/qos1/sent',
                  'messages/qos2/received',
                  'messages/qos2/sent',
                  'messages/received',
                  'messages/retained',
                  'messages/sent',
                  'packets/connack',
                  'packets/connect',
                  'packets/disconnect',
                  'packets/pingreq',
                  'packets/pingresp',
                  'packets/puback/received',
                  'packets/puback/sent',
                  'packets/pubcomp/received',
                  'packets/pubcomp/sent',
                  'packets/publish/received',
                  'packets/publish/sent',
                  'packets/pubrec/received',
                  'packets/pubrec/sent',
                  'packets/pubrel/received',
                  'packets/pubrel/sent',
                  'packets/received',
                  'packets/sent',
                  'packets/suback',
                  'packets/subscribe',
                  'packets/unsuback',
                  'packets/unsubscribe']).

-record(meter_state, {interval}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_interval(Val) when is_integer(Val)  ->
    gen_server:cast(?SERVER, {update_interval, Val}).

get_report(Metric, Limit) ->
    gen_server:call(?SERVER, {get_report, Metric, Limit}).

init([]) ->
    NewTab = fun(Tab) -> 
                ets:new(Tab, [ordered_set, named_table, public]) 
             end,
    lists:foreach(NewTab, ?METRICS),
    erlang:send_after(?INTERVAL, self(), collect_meter),
    case application:get_env(emqttd_dashboard, interval) of
    {ok, Interval} ->
        {ok, #meter_state{interval = Interval}};
    undefined ->
        {ok, #meter_state{interval = ?INTERVAL}}
    end.

handle_call({get_report, Metric, Limit}, _From, State) ->
    Reply = get_metrics(Metric, Limit),
    {reply, Reply, State};

handle_call(Req, _From, State) ->
    lager:info("unexpectd info ~p", [Req]),
    {reply, State}.

handle_cast({update_interval, Interval}, _State) ->
    UpdateInterval = Interval * 1000,
    {noreply, #meter_state{interval = UpdateInterval}};

handle_cast(Info, State) ->
    lager:info("unexpectd info ~p", [Info]),
    {noreply, State}.

handle_info(collect_meter, #meter_state{interval = Interval}=State) ->
    MetricsData = emqttd_metrics:all(),
    save(MetricsData),
    erlang:send_after(Interval, self(), collect_meter),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

save(Data) ->
    Ts = timestamp(),
    Fun = fun(Metric) ->
        case proplists:get_value(Metric, Data) of
            undefined -> ignore;
            V -> ets:insert(Metric, {Ts, V})
        end
        end,
    lists:foreach(Fun, ?METRICS).

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

get_metrics(all, Limit) ->
    get_metrics(?METRICS, Limit);

get_metrics(Metric, Limit) when is_atom(Metric) ->
    Result = ets:match(Metric, '$1', Limit),
    List = case Result of
             {M, _C} -> M;
             '$end_of_table' -> []
           end,
    {Metric, List};

get_metrics(Metrics, Limit) when is_list(Metrics) ->
    [get_metrics(M, Limit) || M <- Metrics].

