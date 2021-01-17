-module(db).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).

-include_lib("kibom/include/records.hrl").

init() ->
    {ok, DbInfo} = application:get_env(kibom, database),
      Password = proplists:get_value(password, DbInfo),
      Username = proplists:get_value(username, DbInfo),
      Host = proplists:get_value(host, DbInfo),
      Database = proplists:get_value(database, DbInfo),
      pgapp:connect([
              {size, 10},
              {host, Host},
              {database, Database},
              {username, Username},
              {password, Password}
          ]
      ),
      ok.

unix_to_gregorian(Time) when is_integer(Time) ->
    calendar:gregorian_seconds_to_datetime(Time + 62167219200).

safe_unix_to_gregorian(Time) when is_integer(Time) ->
    unix_to_gregorian(Time);
safe_unix_to_gregorian(Other) ->
    Other.

get_part_id(DKeyPN) ->
    case pgapp:equery( "select id from parts where digikey_pn = $1", [DKeyPN]) of
        {ok, _Spec, [{V}]} -> {ok, V};
        {ok, _Spec, []} ->
            % Part does not yet exist - create
            case pgapp:equery("insert into parts (digikey_pn) values ($1)
                              returning id", [DKeyPN]) of
            {ok, 1, _Spec, [{Id}]} -> {ok, Id}
            end
    end.

store_oauth_token(Auth) ->
    pgapp:equery(
      "insert into oauth
       (access_token, refresh_token, expires_at, refresh_token_expires_at)
       values
       ($1, $2, $3, $4)",
      [Auth#oauth_resp.access_token,
       Auth#oauth_resp.refresh_token,
       Auth#oauth_resp.expires_at,
       Auth#oauth_resp.refresh_expires_at]).

get_live_access_token() ->
    case pgapp:squery(
        "select access_token
        from oauth
        where expires_at > now()
        order by expires_at desc limit 1") of
        {ok, _Spec, [{V}]} -> {ok, V};
        {ok, _Spec, []} -> undefined
    end.

get_live_refresh_token() ->
    case pgapp:squery(
        "select refresh_token
        from oauth
        where refresh_token_expires_at > now()
        order by refresh_token_expires_at desc limit 1") of
        {ok, _Spec, [{V}]} -> {ok, V};
        {ok, _Spec, []} -> undefined
    end.

update_price_breaks(DKeyPN, Breaks) ->
    % Find the part ID
    {ok, PartId} = get_part_id(DKeyPN),
    % Replace the price break info in a transaction
    pgapp:with_transaction(fun() ->
        % Delete the old price breaks
        pgapp:equery("delete from price_breaks where part_id = $1", [PartId]),
        lists:foreach(fun(Break) ->
            pgapp:equery(
              "insert into price_breaks (part_id, quantity, unit_price)
              values ($1, $2, $3)",
              [PartId,
               Break#dkey_price.quantity,
               Break#dkey_price.unit])
            end,
            Breaks
        )
    end).

price_for_qty(PN, Qty) ->
    case pgapp:equery(
        "select quantity, unit_price from parts, price_breaks
         where parts.digikey_pn = $1
         and parts.id = price_breaks.part_id
         order by price_breaks.quantity asc",
        [PN]) of
        {ok, _Spec, []} -> undefined;
        {ok, _Spec, Rows} ->
            V = lists:foldl(
              fun({BreakQty, BreakPrice}, Acc) ->
                case BreakQty =< Qty of
                    true -> BreakPrice;
                    false -> Acc
                end
              end,
              element(2, hd(Rows)),
              tl(Rows)
            ),
            {ok, V}
    end.

rows_to_proplists(RowSpec, Rows) ->
    ColNames = [element(2, Spec) || Spec <- RowSpec],
    [lists:zip(ColNames, [element(I,Row) || I <- lists:seq(1,tuple_size(Row))])
     || Row <- Rows].
