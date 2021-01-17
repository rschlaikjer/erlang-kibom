-module(digikey).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).
-include_lib("kibom/include/records.hrl").

digikey_api_url() ->
    {ok, Conf} = application:get_env(kibom, digikey),
    {api_base, ApiBase} = proplists:lookup(api_base, Conf),
    ApiBase.

digikey_headers(Auth) ->
    {ok, Conf} = application:get_env(kibom, oauth),
    {id, OauthId} = proplists:lookup(id, Conf),
    [
     {"X-DIGIKEY-Client-Id", binary_to_list(OauthId)},
     {"Authorization", lists:flatten(io_lib:format("Bearer ~s", [Auth]))},
     {"X-DIGIKEY-Locale-Site", "US"},
     {"X-DIGIKEY-Locale-Language", "en"},
     {"X-DIGIKEY-Locale-Currency", "USD"},
     {"X-DIGIKEY-Locale-ShipToCountry", "us"},
     {"X-DIGIKEY-Customer-Id", "0"}
    ].

product_details(Auth, PartNumber) ->
    ApiUrl = digikey_api_url(),
    Url = lists:flatten(io_lib:format(
        "~s/Search/v3/Products/~s",
        [ApiUrl, PartNumber]
    )),
    Headers = digikey_headers(Auth),
    {ok, Response}  = httpc:request(get, {Url, Headers}, [], [{body_format, binary}]),
    case Response of
        {{_, 200, _}, _Headers, RespBody} ->
            {ok, jsx:decode(RespBody)};
        Other -> {error, Other}
    end.

parse_product_price(Json) ->
    #dkey_price{
       quantity=proplists:get_value(<<"BreakQuantity">>, Json),
       unit=proplists:get_value(<<"UnitPrice">>, Json),
       total=proplists:get_value(<<"TotalPrice">>, Json)
    }.

update_price_breaks(Auth, PN) ->
    case product_details(Auth, PN) of
        {ok, Json} ->
            update_price_breaks(Json);
        {error, {{_, 404, _}, _, _}} ->
            lager:warning("Failed to find price for '~s'~n", [PN]),
            {error, not_found}
    end.

update_price_breaks(Json) ->
    DigikeyPart = proplists:get_value(<<"DigiKeyPartNumber">>, Json),
    PricingJson = proplists:get_value(<<"StandardPricing">>, Json),
    Pricing = [parse_product_price(P) || P <- PricingJson],
    lager:info("Breaks: ~p~n", [Pricing]),
    db:update_price_breaks(DigikeyPart, Pricing).

col_idx(Name, Cols) -> col_idx(Name, Cols, 1).
col_idx(_Name, [], _Acc) -> not_found;
col_idx(Name, [C|Cols], Acc) ->
    case Name =:= C of
        true -> Acc;
        false -> col_idx(Name, Cols, Acc + 1)
    end.

read_bom(Filename) ->
    Csv = csv:parse_file(Filename),
    Header = hd(Csv),
    QtyCol = col_idx("Qty", Header),
    DigikeyCol = col_idx("DigiKey PN", Header),
    DataLines = tl(Csv),
    [ {list_to_integer(lists:nth(QtyCol, L)), lists:nth(DigikeyCol, L)} || L <- DataLines].

update_breaks_for_bom(Auth, Bom) ->
    lists:foreach(
      fun({_Qty, PN}) -> update_price_breaks(Auth, PN) end,
      Bom
    ).

multiquote_bom(Bom, Qtys) when is_list(Qtys) ->
    lists:foldl(
        fun(Qty, Acc) ->
            maps:put(Qty, quote_bom(Bom, Qty), Acc)
        end,
        #{},
        Qtys
    ).

quote_bom(Bom) ->
    quote_bom(Bom, 1).
quote_bom(Bom, Multiplier) ->
    % Filter out virtual components
    FilteredBom = [V || V <- Bom, element(2, V) =/= "~"],
    lists:foldl(
        fun({Qty, PN}, Acc) ->
            case db:price_for_qty(PN, Qty * Multiplier) of
                undefined ->
                    lager:info("Cannot quote ~s at qty ~p", [PN, Qty * Multiplier]),
                    Acc;
                {ok, Price} -> Acc + (Price * Qty)
            end
        end,
        0,
        FilteredBom
    ).
