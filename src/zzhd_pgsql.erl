-module(zzhd_pgsql).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([maybe_pgsql_app/0
        ,get_informer_id/2
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

-spec get_informer_id(kz_term:ne_binary(), integer()) -> integer().
get_informer_id(AccountId, _LB_Id) ->
    case pgapp:equery('zzhd_pgsql_pool', "SELECT id FROM public.identities where kz_account_id = $1", [AccountId]) of
        {ok,_,[]} ->
            pgapp:equery(?ZZHD_PGSQL_POOL, "INSERT INTO identities (kz_account_id) VALUES($1)", [AccountId]),
            case pgapp:equery('zzhd_pgsql_pool', "SELECT id FROM public.identities where kz_account_id = $1", [AccountId]) of
                {ok,_,[{InformerId}]} -> InformerId;
                _ -> <<"error inserting new InformerId">>
            end;
        {ok,_,[{InformerId}]} -> InformerId
    end.
        
%    Res = pgapp:equery(?ZZHD_PGSQL_POOL, "INSERT INTO public.comments (comment,  kz_account_id) VALUES($1, $2)", [Comment, ConsumerAccountId]),

