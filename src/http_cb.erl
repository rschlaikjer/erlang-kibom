-module(http_cb).
-export([handle/2, handle_event/3]).
-compile([{parse_transform, lager_transform}]).
-include_lib("kibom/include/records.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"auth">>], _Req) ->
    Url = oauth:code_request(),
    {307, [{<<"Location">>, Url}], <<"">>};

handle('GET', [<<"oauth">>], Req) ->
    case elli_request:get_arg(<<"code">>, Req) of
        undefined ->
            {400, [], <<"Missing code">>};
        OauthCode ->
            io:format("Code: ~p~n", [OauthCode]),
            TokenResp = oauth:token_request(OauthCode),
            case TokenResp of
                {ok, Token} ->
                    db:store_oauth_token(Token),
                    {200, [], <<"Auth complete">>};
                {error, Resp} ->
                    lager:error("~p", [Resp]),
                    {401, [], <<"Auth error">>}
            end
    end.

handle_event(request_error, Data, _Args) ->
    [_Req, _, Stacktrace] = Data,
    lager:info("Request error: ~p~n", [Stacktrace]);
handle_event(Event, Data, Args) ->
    % lager:info("Event: ~p: Data: ~p, Args: ~p~n", [Event, Data, Args]).
    ok.
