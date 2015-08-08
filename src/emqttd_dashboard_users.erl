%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2012-2015, huangdan <wang_yi20081014@163.com>
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
%%% @doc
%%% web dashboard admin authentication with username and password.
%%%
%%% @end
%%%-
-module(emqttd_dashboard_users).

-author('huangdan').

-behaviour(gen_server).

%% API Function Exports
-export([start_link/0]).

%%mqtt_admin api
-export([add_user/1, remove_user/1,
         lookup_user/1, all_users/0,check/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MQTT_ADMIN_TAB, mqtt_admin).

-record(mqtt_admin_user, {username, password, tags}).


-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%=============================================================================
%%% API 
%%%=============================================================================
add_user(User = #mqtt_admin_user{username = _Username, password = _Password}) ->
    gen_server:cast(?MODULE, {add_user, User}).

remove_user(User = #mqtt_admin_user{username = _Username}) ->
    gen_server:cast(?MODULE, {remove_user, User}).

lookup_user(Username) ->
  case ets:lookup(?MQTT_ADMIN_TAB, Username) of
	[User] -> User;
	[] -> undefined
	end.

all_users() ->
    emqttd_vm:get_ets_object(?MQTT_ADMIN_TAB).

check(#mqtt_admin_user{username = undefined}) ->
    {error, "Username undefined"};
check(#mqtt_admin_user{password = undefined}) ->
    {error, "Password undefined"};
check(#mqtt_admin_user{username = Username, password = Password}) ->
	case ets:lookup(?MQTT_ADMIN_TAB, Username) of
        [] -> 
            {error, "Username Not Found"};
        [#mqtt_admin_user{password = <<Salt:4/binary, Hash/binary>>}] ->
            case Hash =:= md5_hash(Salt, Password) of
                true -> ok;
                false -> {error, "Password Not Right"}
            end
	end.
	
%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init([]) ->
    % Create mqtt_admin table
    ets:new(?MQTT_ADMIN_TAB, [set, public, named_table, {keypos, 1},{write_concurrency, true}]),
    % Init mqtt_admin 
    ets:insert(?MQTT_ADMIN_TAB, #mqtt_admin_user{username = admin, password = hash(bin(admin)), tags = administrator}),
    {ok, state}.

handle_call(_Req, _From, State) ->
    {reply, error,  State}.

handle_cast({add_user, User = #mqtt_admin_user{username = _Username, password = _Password}}, State) ->
    ets:insert(?MQTT_ADMIN_TAB, User),
    {noreply, State};

handle_cast({remove_user, User = #mqtt_admin_user{username = Username, password = _Password}}, State) ->
    ets:delete(?MQTT_ADMIN_TAB, Username),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(?MQTT_ADMIN_TAB),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

hash(Password) ->
    SaltBin = salt(),
    <<SaltBin/binary, (md5_hash(SaltBin, Password))/binary>>.

md5_hash(SaltBin, Password) ->
    erlang:md5(<<SaltBin/binary, Password/binary>>).

salt() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Salt = random:uniform(16#ffffffff),
    <<Salt:32>>.

bin(S) when is_list(S) -> list_to_binary(S);
bin(A) when is_atom(A) -> bin(atom_to_list(A));
bin(B) when is_binary(B) -> B.

