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

-module(emqttd_dashboard).

-import(proplists, [get_value/2]).

-export([http_handler/0, handle_request/2]).

-export([paginate/3, strftime/1]).

-define(APP, ?MODULE).

-define(AUTH_HEADER, {"WWW-Authenticate", "Basic Realm=\"emqttd dashboard\""}).

%%--------------------------------------------------------------------
%% HTTP Handler
%%--------------------------------------------------------------------

http_handler() ->
    Env = [{docroot, docroot()}, {api, http_api()}],
    {?MODULE, handle_request, [Env]}.

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

http_api() ->
    {ok, Modules} = application:get_key(?APP, modules),
    lists:append(lists:map(fun http_api/1, Modules)).

http_api(Mod) ->
    [{Name, {Mod, Fun, Args}} || {http_api, [{Name, Fun, Args}]} <- Mod:module_info(attributes)].

find_api(Name, Env) ->
    get_value(Name, get_value(api, Env)).

%%--------------------------------------------------------------------
%% Handle HTTP Request
%%--------------------------------------------------------------------

handle_request(Req, Env) ->
    if_authorized(Req, fun() -> Path = Req:get(path), handle_request(Path, Req, Env) end).

handle_request("/api/current_user", Req, _Env) ->
    "Basic " ++ BasicAuth =  Req:get_header_value("Authorization"),
    {Username, _Password} = user_passwd(BasicAuth),
    json_respond(Req, [{username, bin(Username)}]);

handle_request("/api/logout", Req, _Env)  ->
    Req:respond({401, [?AUTH_HEADER], []});

handle_request("/api/" ++ Name, Req, Env) ->
    Params = Req:parse_post(),
    case find_api(Name, Env)  of
        {Mod, Fun, InitArgs} ->
            Args = lists:map(fun({Arg, Type, Def})->
                                case get_value(Arg, Params)of
                                    undefined -> format(Type, Def);
                                    Value     -> format(Type, Value)
                                end
                             end, InitArgs),
            case catch apply(Mod, Fun, Args) of
                {ok, JsonData} ->
                    json_respond(Req, JsonData);
                {'EXIT', Reason} ->
                    lager:error("Execute API '~s' Error: ~p", [Name, Reason]),
                    Req:respond({404, [{"Content-Type", "application/json"}], []})
            end;
        undefined ->
            Req:respond({404, [{"Content-Type", "application/json"}], []})
    end;
       
handle_request("/" ++ Rest, Req, Env) ->
    mochiweb_request:serve_file(Rest, get_value(docroot, Env), Req).

%%--------------------------------------------------------------------
%% Paging
%%--------------------------------------------------------------------

paginate(TotalNum, PageNum, PageSize) ->
    TotalPage = case TotalNum rem PageSize of
                    0 -> TotalNum div PageSize;
                    _ -> (TotalNum div PageSize) + 1
                end,
    if
        PageNum > TotalPage -> {TotalPage, TotalPage};
        true                -> {PageNum, TotalPage}
    end.

strftime({MegaSecs, Secs, _MicroSecs}) ->
    strftime(datetime(MegaSecs * 1000000 + Secs));

strftime({{Y,M,D}, {H,MM,S}}) ->
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, MM, S])).

datetime(Timestamp) when is_integer(Timestamp) ->
    Universal = calendar:gregorian_seconds_to_datetime(Timestamp +
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    calendar:universal_time_to_local_time(Universal).

%%--------------------------------------------------------------------
%% Basic Authorization
%%--------------------------------------------------------------------

if_authorized(Req, Fun) ->
    case authorized(Req) of
        true  -> Fun();
        false -> Req:respond({401, [?AUTH_HEADER], []})
    end.

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

json_respond(Req, Data) ->
    Req:respond({200, [{"Content-Type", "application/json"}], to_json(Data)}).

to_json([])   -> <<"[]">>;
to_json(Data) -> iolist_to_binary(mochijson2:encode(Data)).

format(string, S) -> S;
format(binary, S) -> list_to_binary(S);
format(int, S)    -> list_to_integer(S).

bin(S) when is_list(S)   -> list_to_binary(S);
bin(A) when is_atom(A)   -> bin(atom_to_list(A));
bin(B) when is_binary(B) -> B.

