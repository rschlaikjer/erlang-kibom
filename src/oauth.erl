-module(oauth).
-compile(export_all).
-define(ACCESS_TOKEN, <<"JdlzWKK8jtWrPIHwoUYjF032dKsg">>).
-include_lib("kibom/include/records.hrl").

-record(oauth_config, {id, secret, redirect}).

get_oauth_config() ->
    {ok, Conf} = application:get_env(kibom, oauth),
    {id, Id} = proplists:lookup(id, Conf),
    {secret, Secret} = proplists:lookup(secret, Conf),
    {redirect, Redirect} = proplists:lookup(redirect, Conf),
    #oauth_config{
        id=Id,
        secret=Secret,
        redirect=Redirect
    }.

digikey_api_url() ->
    {ok, Conf} = application:get_env(kibom, digikey),
    {api_base, ApiBase} = proplists:lookup(api_base, Conf),
    ApiBase.

code_request() ->
    Conf = get_oauth_config(),
    DKeyApi = digikey_api_url(),
    Url = lists:flatten(io_lib:format(
        "~s/v1/oauth2/authorize?response_type=code&client_id=~s&redirect_uri=~s",
        [DKeyApi, Conf#oauth_config.id, Conf#oauth_config.redirect]
    )),
    Url.

token_refresh(Refresh) ->
    Conf = get_oauth_config(),
    DKeyApi = digikey_api_url(),
    Url = lists:flatten(io_lib:format("~s/v1/oauth2/token", [DKeyApi])),
    Headers = [],
    Type = "application/x-www-form-urlencoded",
    Body = uri_string:compose_query([
        {<<"client_id">>, Conf#oauth_config.id},
        {<<"client_secret">>, Conf#oauth_config.secret},
        {<<"refresh_token">>, Refresh},
        {<<"grant_type">>, "refresh_token"}
    ]),
    case httpc:request(post, {Url, Headers, Type, Body}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, RespBody}} ->
            {ok, parse_token_json(jsx:decode(RespBody))};
        Other -> {error, Other}
    end.

token_request(Code) ->
    Conf = get_oauth_config(),
    DKeyApi = digikey_api_url(),
    Url = lists:flatten(io_lib:format("~s/v1/oauth2/token", [DKeyApi])),
    Headers = [],
    Type = "application/x-www-form-urlencoded",
    Body = uri_string:compose_query([
        {<<"code">>, Code},
        {<<"client_id">>, Conf#oauth_config.id},
        {<<"client_secret">>, Conf#oauth_config.secret},
        {<<"redirect_uri">>, Conf#oauth_config.redirect},
        {<<"grant_type">>, "authorization_code"}
    ]),
    case httpc:request(post, {Url, Headers, Type, Body}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, RespBody}} ->
            {ok, parse_token_json(jsx:decode(RespBody))};
        Other -> {error, Other}
    end.


time_add(Time, Seconds) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(Time) + Seconds).

parse_token_json(Json) ->
    ExpiresIn = proplists:get_value(<<"expires_in">>, Json),
    RefreshExpiresIn = proplists:get_value(<<"refresh_token_expires_in">>, Json),
    ExpiresAt = time_add(calendar:universal_time(), ExpiresIn),
    RefreshExpiresAt = time_add(calendar:universal_time(), RefreshExpiresIn),
    #oauth_resp{
       access_token=proplists:get_value(<<"access_token">>, Json),
       refresh_token=proplists:get_value(<<"refresh_token">>, Json),
       expires_at=ExpiresAt,
       refresh_expires_at=RefreshExpiresAt
    }.

get_live_token() ->
    case db:get_live_access_token() of
        {ok, Token} -> Token;
        _ ->
            case db:get_live_refresh_token() of
                {ok, Refresh} ->
                    case token_refresh(Refresh) of
                        {ok, Auth} ->
                            db:store_oauth_token(Auth),
                            Auth#oauth_resp.access_token;
                        {error, _} ->
                            undefined
                    end;
                _ ->
                    undefined
            end
    end.

store_oauth_token(Access, Refresh, Expires, RefreshExpires) ->
    pgapp:equery(
      "insert into oauth
       (expires_at, refresh_token_expires_at, access_token, refresh_token)
       values
       ($1, $2, $3, $4)",
      [Expires, RefreshExpires, Access, Refresh]).
