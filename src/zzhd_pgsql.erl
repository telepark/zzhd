-module(zzhd_pgsql).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([maybe_pgsql_app/0
        ,get_informer_by_kz_id/1
        ,get_kz_by_informer_id/1
        ,maybe_set_informer_name/2
        ,set_informer_name/2
        ,get_informer_name/1
        ,get_informer_emails/1
        ,get_informer_phonenumbers/1
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

-spec get_informer_by_kz_id(kz_term:ne_binary()) -> integer().
get_informer_by_kz_id(AccountId) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "SELECT id FROM public.identities where kz_account_id = $1", [AccountId]) of
        {ok,_,[]} ->
            pgapp:equery(?ZZHD_PGSQL_POOL, "INSERT INTO identities (kz_account_id) VALUES($1)", [AccountId]),
            case pgapp:equery('zzhd_pgsql_pool', "SELECT id FROM public.identities where kz_account_id = $1", [AccountId]) of
                {ok,_,[{InformerId}]} -> InformerId;
                _ -> <<"error inserting new InformerId">>
            end;
        {ok,_,[{InformerId}]} -> InformerId
    end.

-spec get_kz_by_informer_id(kz_term:ne_binary()|integer()) -> integer().
get_kz_by_informer_id(InformerId) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "SELECT kz_account_id FROM public.identities where id = $1", [kz_term:to_integer(InformerId)]) of
        {ok,_,[{AccountId}]} -> AccountId;
        _ -> 'undefined'
    end.
        
-spec maybe_set_informer_name(kz_term:ne_binary()|integer(), kz_term:ne_binary()) -> any().
maybe_set_informer_name(InformerId, InformerName) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "select informer_name from identities where id = $1", [kz_term:to_integer(InformerId)]) of
        {ok,_,[{IName}]} when is_binary(IName) -> 'ok';
        _ -> set_informer_name(InformerId, InformerName)
    end.

-spec set_informer_name(kz_term:ne_binary()|integer(), kz_term:ne_binary()) -> any().
set_informer_name(InformerId, InformerName) ->
    pgapp:equery(?ZZHD_PGSQL_POOL
                ,"UPDATE identities SET informer_name=$1  WHERE id=$2"
                ,[InformerName, kz_term:to_integer(InformerId)]
                ).

-spec get_informer_name(kz_term:ne_binary()|integer()) -> any().
get_informer_name(InformerId) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "select informer_name from identities where id = $1", [kz_term:to_integer(InformerId)]) of
        {ok,_,[{IName}]} when is_binary(IName) -> IName;
        _ -> 'undefined'
    end.

-spec get_informer_emails(kz_term:ne_binary()|integer()) -> any().
get_informer_emails(InformerId) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "select email_address from email_addresses where informer_id = $1", [kz_term:to_integer(InformerId)]) of
        {ok,_,Emails} -> Emails;
        _ -> 'undefined'
    end.

-spec get_informer_phonenumbers(kz_term:ne_binary()|integer()) -> any().
get_informer_phonenumbers(InformerId) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "select phonenumber from phonenumbers where informer_id = $1", [kz_term:to_integer(InformerId)]) of
        {ok,_,Emails} -> Emails;
        _ -> 'undefined'
    end.

