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

%% @doc Web emqttd dashboard dispatcher.
-module(emqttd_dashboard_dispatcher).

-import(proplists, [get_value/2, get_value/3]).

-import(emqttd_dashboard_util, [intFun/0, stringFun/0, bin/0]).
-import(emqttd_dashboard_util, [to_json/1]).

-export([handle_request/1]).

handle_request(Req) ->
    case authorized(Req) of
        true  ->
            Path = Req:get(path),
            handle_request(Path, Req);
        false ->
            Req:respond({401,
                         [{"WWW-Authenticate",
                           "Basic Realm=\"emqttd dashboad\""}],
                         []})
    end.

handle_request("/api/current_user", Req)  ->
    "Basic " ++ BasicAuth =  Req:get_header_value("Authorization"),
    {Username, _Password} = user_passwd(BasicAuth),
    to_json([{username, Username}]);

handle_request("/api/logout", Req)  ->
    Req:respond({401, [{"WWW-Authenticate", "Basic Realm=\"emqttd dashboad\""}], []});

handle_request("/api/" ++ Path, Req) when length(Path) > 0 ->
    execude(Path, Req);

handle_request("/" ++ Rest, Req) ->
    mochiweb_request:serve_file(Rest, docroot(), Req).

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

%%------------------------------------------------------------------------------
%% basic authorization
%%------------------------------------------------------------------------------
authorized(Req) ->
    case Req:get_header_value("Authorization") of
        undefined             ->
            false;
        "Basic " ++ BasicAuth ->
            {Username, Password} = user_passwd(BasicAuth),
            case emqttd_dashboard_admin:check(bin(Username), bin(Password)) of
                ok              ->
                    true;
                {error, Reason} ->
                    lager:error("HTTP Auth failure: username=~s, reason=~p",
                                [Username, Reason]),
                    false
            end
    end.

user_passwd(BasicAuth) ->
    list_to_tuple(binary:split(base64:decode(BasicAuth), <<":">>)).

bin(S) when is_list(S) -> list_to_binary(S);
bin(A) when is_atom(A) -> bin(atom_to_list(A));
bin(B) when is_binary(B) -> B.

execude(Path, Req) ->
    WebArgs = Req:parse_post(),
    MFA =[{Module, Fun, Args} || {LocalPath, Module, Fun, Args}<-dispatcher(), LocalPath==Path],
    case MFA of
    [{Module, Function, Args}] ->
        LocalArgs =
        case Args of
            [] ->
                [];
            ListArgs ->
               lists:map(fun({ArgKey, ArgTyepFun, ArgDefault})->
                        case get_value(ArgKey, WebArgs)of
                               undefined ->
                                        ArgTyepFun(ArgDefault);
                                Value ->
                                        ArgTyepFun(Value)
                        end
                       end, ListArgs)
        end, 
        case catch apply(Module, Function, LocalArgs) of
                    {'EXIT', Reason} ->
                            io:format("~p~n",[Reason]),
                            Req:respond({404, [{"Content-Type", "application/json"}],[]});
                    JsonData ->
                        Req:respond({200, [{"Content-Type", "application/json"}], to_json(JsonData)})
            end;
       []->
            Req:respond({404, [{"Content-Type", "application/json"}], []})
    end.
       
%%----------------------------------------------------------------------------

dispatcher() ->
    [{"stats",          emqttd_dashboard_overview, stats, []},
     {"ptype",          emqttd_dashboard_overview, ptype, []},
     {"memory",         emqttd_dashboard_overview, memory, []},
     {"cpu",            emqttd_dashboard_overview, cpu, []},
     {"node",           emqttd_dashboard_overview, nodesinfo, []},
     {"metrics",        emqttd_dashboard_overview,   metrics, []},
     {"listeners",      emqttd_dashboard_overview,   listeners,     []},
     {"bnode",          emqttd_dashboard_overview,    bnode,     []},
     {"clients",        emqttd_dashboard_client,    execute,  [{"curr_page", intFun(), "1"}, {"page_size", intFun(), "10"}, {"client_key", stringFun(), ""}]},
     {"sessions",       emqttd_dashboard_session,    execute, [{"curr_page", intFun(), "1"}, {"page_size", intFun(), "10"}, {"client_key", stringFun(), ""}]},
     {"topics",         emqttd_dashboard_topic,    execute,  []},
     {"subscriptions",  emqttd_dashboard_subscription, execute, []},
     {"users",          emqttd_dashboard_user,     users,  []},
     {"update_user",    emqttd_dashboard_user,     update,      [{"user_name", bin(), "admin"}, {"password", bin(), "public"}, {"tags", bin(), ""}]},
     {"remove_user",    emqttd_dashboard_user,     remover,      [{"user_name", bin(), "admin"}]},
     {"add_user",       emqttd_dashboard_user,     add,    [{"user_name", bin(), "admin"}, {"password", bin(), "public"}, {"tags", bin(), ""}]}
    ].
