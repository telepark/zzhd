-module(zzhd_pgsql).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([maybe_pgsql_app/0
        ]).

-include_lib("zzhd.hrl").

-spec maybe_pgsql_app() -> kz_term:proplists().
maybe_pgsql_app() ->
    case kapps_config:get_is_true(<<"zzhd">>, <<"pgsql_pool_enable">>, 'false') of
        'true' ->
            PoolOptions  = [{size, 10}, {max_overflow, 20}],
            PgSqlOptions = [{host, kapps_config:get_string(<<"zzhd">>, <<"pgsql_host">>, <<"localhost">>)}
                           ,{username, kapps_config:get_string(<<"zzhd">>, <<"pgsql_username">>, <<"username">>)}
                           ,{password, kapps_config:get_string(<<"zzhd">>, <<"pgsql_password">>, <<"password">>)}
                           ,{database, kapps_config:get_string(<<"zzhd">>, <<"pgsql_database">>, <<"database">>)}
                           ],
            application:ensure_all_started(pgapp),
            pgapp:connect(?ZZHD_PGSQL_POOL, PoolOptions ++ PgSqlOptions);
        'false' ->
            []
    end.

