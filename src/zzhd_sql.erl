-module(zzhd_sql).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([q_raw/2
        ,maybe_mysql_child/0
        ,lbuid_by_uuid/1
        ,account_balance/1
        ,main_agrm_id/1
        ,accounts_table_info/1
        ,account_status/1
        ,account_payments/1
        ,monthly_fees/1
        ,get_accounts_emails/1
        ,curr_month_credit/1
        ,bom_balance/1
        ,calc_curr_month_exp/1
        ,calc_prev_month_exp/1
        ,is_prepaid/1
        ,agreements_table/1
        ,agreements_data/1
        ,addresses_data/1
        ,get_field/3
        ,get_field/4
        ,update_field/4
        ,get_periodic_fees/1
        ,service_cat_uuid/2
        ,accounts_groups/1
	,tariff_types/1
        ,accounts_tariffs_by_type/2
        ,tariff_descr_by_tar_id/1
        ,numbers_by_vg_id/1
        ,ip_addresses_by_vg_id/1
        ,get_docs_list/4
        ,get_calls_list_by_day/5
        ,agreements_creds_by_id/1
        ]).

-include_lib("zzhd.hrl").

-define(LB_MYSQL_POOL, 'lb_mysql').

-spec maybe_mysql_child() -> kz_term:proplists().
maybe_mysql_child() ->
    case kapps_config:get_is_true(<<"zzhd">>, <<"mysql_pool_enable">>, 'false') of
        'true' ->
            PoolOptions  = [{size, 10}, {max_overflow, 20}],
            MySqlOptions = [{host, kapps_config:get_string(<<"zzhd">>, <<"mysql_host">>, <<"localhost">>)}
                           ,{user, kapps_config:get_string(<<"zzhd">>, <<"mysql_user">>, <<"user">>)}
                           ,{password, kapps_config:get_string(<<"zzhd">>, <<"mysql_password">>, <<"password">>)}
                           ,{database, kapps_config:get_string(<<"zzhd">>, <<"mysql_database">>, <<"database">>)}
                           ],
            [mysql_poolboy:child_spec(?LB_MYSQL_POOL, PoolOptions, MySqlOptions)];
        'false' ->
            []
    end.

-spec q_raw(kz_term:ne_binary(),kz_term:proplists()) -> any().
q_raw(Query, Parameters) ->
    mysql_poolboy:query(?LB_MYSQL_POOL, Query, Parameters).

q(Query, Parameters) ->
    {_,_,Result} = mysql_poolboy:query(?LB_MYSQL_POOL, Query, Parameters),
    Result.

-spec lbuid_by_uuid(kz_term:ne_binary()) -> any().
lbuid_by_uuid(AccountId) ->
    case mysql_poolboy:query(?LB_MYSQL_POOL
                            ,<<"select uid from accounts where uuid = ? limit 1">>
                            ,[AccountId])
    of
        {ok,_,[[Uid]]} -> Uid;
        _ -> 'undefined'
    end.

-spec account_balance(kz_term:ne_binary()) -> any().
account_balance(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            case mysql_poolboy:query(?LB_MYSQL_POOL
                                    ,<<"SELECT COALESCE(sum(balance),0) FROM agreements  where uid = ? and agreements.archive = 0">>
                                    ,[UID])
            of
                {ok,_,[[Amount]]} -> Amount;
                _ -> 'undefined'
            end
    end.

-spec main_agrm_id(kz_term:ne_binary()) -> any().
main_agrm_id(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            case mysql_poolboy:query(?LB_MYSQL_POOL
                                    ,<<"SELECT agrm_id from agreements where uid  = ? and oper_id = 1 limit 1">>
                                    ,[UID])
            of
                {ok,_,[[AgrmId]]} -> AgrmId;
                E ->
                    lager:debug("agrm_id for ~p not found: ~p", [AccountId,E]),
                    'undefined'
            end
    end.

-spec accounts_table_info(kz_term:ne_binary()) -> any().
accounts_table_info(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> [];
        UID ->
            case mysql_poolboy:query(?LB_MYSQL_POOL
                                    ,<<"select name,inn,kpp,kont_person,phone,fax,gen_dir_u,gl_buhg_u,email from accounts where uid = ? limit 1">>
                                    ,[UID])
            of
                {ok,_,[[Name,INN,KPP,KontPerson,Phones,Fax,GenDirU,GlBuhgU,Emails]]} ->
                    [{name, Name}
                    ,{inn, INN}
                    ,{kpp, KPP}
                    ,{kont_person, KontPerson}
                    ,{phones, [kz_binary:strip(Phone) || Phone <- binary:split(Phones, [<<",">>,<<";">>], [global])]}
                    ,{fax, Fax}
                    ,{gen_dir_u, GenDirU}
                    ,{gl_buhg_u, GlBuhgU}
                    ,{emails, [kz_binary:strip(Email) || Email <- binary:split(Emails, [<<",">>,<<";">>], [global])]}
                    ];
                _ -> [] 
            end
    end.

-spec account_status(kz_term:ne_binary()) -> any().
account_status(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        undefined -> ['undefined', {{0,1,1},{0,0,0}}];
        UId ->
            case q(<<"SELECT blocked, block_date FROM vgroups where uid = ? and archive = 0 and id = 1 order by blocked asc limit 1">>
                   ,[UId])
            of
                [[StatusID,BlockDate]] -> [StatusID, BlockDate];
                 _  ->
                    case q(<<"SELECT blocked, block_date FROM vgroups where uid = ? and archive = 0 order by blocked desc limit 1">>
                           ,[UId])
                    of
                       [[StatusID,BlockDate]] -> [StatusID, BlockDate];
                       _ -> ['undefined', {{0,1,1},{0,0,0}}]
                    end
            end
    end.

-spec account_payments(kz_term:ne_binary()) -> any().
account_payments(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        undefined -> [];
        UId ->
            q(<<"SELECT amount, left(pay_date,10), comment, status, left(cancel_date,10)
                 FROM payments where agrm_id = (SELECT agrm_id from agreements
                 where uid  = ? and oper_id = 1 limit 1) and status != 2 ORDER BY LEFT( pay_date, 10 ) DESC">>
             ,[UId])
     %%            where uid  = ? and oper_id = 1 limit 1) and status != 2 ORDER BY LEFT( pay_date, 10 ) DESC limit ?">>
     %%        ,[UId, Limit])
    end.

-spec monthly_fees(kz_term:ne_binary()) -> any().
monthly_fees(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        undefined -> [];
        UId ->
            q(<<"SELECT service_categories.descr_full, ROUND(service_categories.above,2), mul, above*mul
                 from service_categories,services,vgroups
                 where service_categories.tar_id = vgroups.tar_id
                       and services.vg_id = vgroups.vg_id
                       and service_categories.serv_cat_idx = services.serv_cat_idx
                       and vgroups.uid = ?
                       and service_categories.rent_period in (1,3)
                       and services.timeto > NOW()">>
             ,[UId])
    end.

-spec get_accounts_emails(kz_term:ne_binary()) -> any().
get_accounts_emails(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> [];
        UID ->
            case mysql_poolboy:query(?LB_MYSQL_POOL
                                    ,<<"select email from accounts where uid = ? limit 1">>
                                    ,[UID])
            of
                {ok,_,[[QueryResult]]} -> binary:split(QueryResult, [<<",">>,<<";">>], [global]);
                _ -> [] 
            end
    end.

-spec curr_month_credit(kz_term:ne_binary()) -> any().
curr_month_credit(AccountId) ->
    case main_agrm_id(AccountId) of
        'undefined' -> 'undefined';
        AgrmId ->
            case mysql_poolboy:query(?LB_MYSQL_POOL
                                    ,<<"SELECT SUM(amount) FROM payments where pay_date >= DATE_FORMAT(NOW() ,'%Y-%m-01') and agrm_id = ?">>
                                    ,[AgrmId]
                                    )
            of
                {ok,_,[['null']]} -> 0.0;
                {ok,_,[[Amount]]} -> Amount;
                _ -> 'undefined'
        end
    end.

-spec bom_balance(kz_term:ne_binary()) -> any().
bom_balance(AccountId) ->
    QStr = <<"SELECT  COALESCE(sum(balances.balance),0) "
            ,"FROM agreements, accounts, balances "
            ,"where agreements.uid = accounts.uid "
            ,"and agreements.agrm_id = balances.agrm_id "
            ,"and accounts.uuid != '' "
            ,"and balances.balance != 0 "
            ,"and balances.date = DATE_FORMAT(NOW() ,'%Y-%m-01') "
            ,"and accounts.uuid = ?"
           >>,
    case mysql_poolboy:query(?LB_MYSQL_POOL, QStr, [AccountId]) of
        {ok,_,[[Uid]]} -> Uid;
        _ -> 'undefined'
    end.

-spec calc_curr_month_exp(kz_term:ne_binary()) -> any().
calc_curr_month_exp(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            {{Year,Month,Day}, _ } = calendar:gregorian_seconds_to_datetime(kz_time:current_tstamp()),
            Today = io_lib:format("~w~2..0w~2..0w",[Year, Month, Day]),
            QueryString = io_lib:format("Select COALESCE(ifnull((SELECT sum(amount) FROM  tel001~s where uid = ~p),0) + ifnull((SELECT sum(amount) FROM  day where Month(timefrom) = Month(Now()) and Year(timefrom) = Year(Now()) and uid = ~p),0) + (Select sum(amount) from charges where agrm_id = (SELECT agrm_id FROM agreements where uid = ~p and oper_id = 1 and archive = 0) and Month(period) = Month(Now()) and Year(period) = Year(Now())),0)",[Today,UID,UID,UID]),
            QueryCheckTableString = io_lib:format("show tables like 'tel001~s'", [Today]),
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryCheckTableString) of
                {ok,_,[]} -> 'undefined';
                _  ->
                    case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString) of
                        {ok,_,[[Amount]]} -> Amount;
                        _ -> 'undefined'
                    end
            end
    end.

-spec calc_prev_month_exp(kz_term:ne_binary()) -> any().
calc_prev_month_exp(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            QueryString = io_lib:format("Select COALESCE(ifnull((SELECT sum(amount) FROM  day where Month(timefrom) = Month(DATE_ADD(Now(), INTERVAL -1 MONTH)) and Year(timefrom) = Year(DATE_ADD(Now(), INTERVAL -1 MONTH)) and uid = ~p),0) + (Select sum(amount) from charges where agrm_id = (SELECT agrm_id FROM agreements where uid = ~p and oper_id = 1 and archive = 0) and Month(period) = Month(DATE_ADD(Now(), INTERVAL -1 MONTH)) and Year(period) = Year(DATE_ADD(Now(), INTERVAL -1 MONTH))),0)",[UID,UID]),
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString) of
                {ok,_,[[Amount]]} -> Amount;
                _ -> 'undefined'
            end
    end.

-spec is_prepaid(kz_term:ne_binary()) -> boolean().
is_prepaid(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'true';
        UID ->
            QueryString = <<"SELECT 1 FROM tarifs, vgroups where tarifs.tar_id = vgroups.tar_id and vgroups.uid = ?  and tarifs.act_block = 2 limit 1">>,
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [UID]) of
                {ok,_,[]} -> 'false';
                _ -> 'true'
            end
    end.

-spec agreements_table(kz_term:ne_binary()) -> kz_term:proplists().
agreements_table(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        undefined -> [];
        UId ->
            q(<<"SELECT agreements.number, agreements.date, accounts.name
                 FROM agreements, accounts
                 where accounts.uid=agreements.oper_id
                       and agreements.uid = ?
                       and agreements.archive = 0">>
             ,[UId])
    end.

-spec agreements_data(kz_term:ne_binary()) -> kz_term:proplists().
agreements_data(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            QueryString = <<"select oper_id,number,date from agreements where uid = ?">>,
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [UID]) of
                {ok,_,Res} -> Res;
                _ -> []
            end
    end.

-spec addresses_data(kz_term:ne_binary()) -> kz_term:proplists().
addresses_data(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            QueryString = <<"select type,address from accounts_addr where uid = ?">>,
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [UID]) of
                {ok,_,Res} -> Res;
                _ -> []
            end
    end.

-spec get_field(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplists().
get_field(K, Table, AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            QueryString = kz_binary:join([<<"select">>, K, <<"from">>, Table, <<"where uid =">>, UID], <<" ">>),
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString) of
                {ok,_,[[Res]]} -> Res;
                _ -> 'undefined'
            end
    end.

-spec get_field(kz_term:ne_binary(), tuple(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplists().
get_field(Field, {K1, V1}, Table, AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            QueryString = kz_binary:join([<<"select">>, Field, <<"from">>, Table, <<"where">>, K1, <<"=">>, V1, <<"and uid =">>, UID], <<" ">>),
            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString) of
                {ok,_,[[Res]]} -> Res;
                _ -> 'undefined'
            end
    end.

-spec update_field(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplists().
update_field(K, V, Table, AccountId) ->
    case lbuid_by_uuid(AccountId) of
        'undefined' -> 'undefined';
        UID ->
            QueryString = kz_binary:join([<<"update">>, Table, <<"set">>, K, <<"= ?">>, <<"where uid =">>, UID], <<" ">>),
            mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [V])
    end.

-spec get_periodic_fees(kz_term:ne_binary()) -> kz_term:proplists().
get_periodic_fees(AccountId) ->
    QueryString = <<"select tar_id,serv_cat_idx,mul,timefrom,timeto from `services` where vg_id in (select vg_id from vgroups,accounts where vgroups.uid = accounts.uid and accounts.uuid = ?)">>,
    case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [AccountId]) of
        {ok,_,Res} when is_list(Res) ->
            [[service_cat_uuid(TarId, ServCatIDX), kz_term:to_integer(Qty), From, To] || [TarId, ServCatIDX, Qty, From, To] <- Res
            ,is_binary(service_cat_uuid(TarId, ServCatIDX))
            ];
        _ -> [] 
    end.

-spec service_cat_uuid(integer(), integer()) -> kz_term:ne_binary().
service_cat_uuid(TarId, ServCatIDX) ->
    QueryString = <<"select uuid from service_categories where uuid != '' and tar_id = ? and serv_cat_idx = ? limit 1">>,
    case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [TarId, ServCatIDX]) of
        {ok,_,[[Res]]} -> Res;
        _ -> [] 
    end.

-spec accounts_groups(kz_term:ne_binary()) -> kz_term:proplists().
accounts_groups(AccountId) ->
    QueryString = <<"select usergroups_staff.group_id from usergroups_staff, usergroups where usergroups.group_id = usergroups_staff.group_id and uid = (Select uid from accounts where uuid = ?)">>,
    case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString, [AccountId]) of
        {ok,_,Res} when is_list(Res) -> Res;
        _ -> [] 
    end.

-spec tariff_types(kz_term:ne_binary()) -> kz_term:proplists().
tariff_types(AccountId) ->
    case lbuid_by_uuid(AccountId) of
        undefined -> [];
        UId ->
            q(<<"select id from vgroups where uid = ? and archive = 0  and blocked != 10 group by id">>
             ,[UId])
    end.

-spec accounts_tariffs_by_type(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplists().
accounts_tariffs_by_type(Type, AccountId) ->
    case lbuid_by_uuid(AccountId) of
        undefined -> [];
        UId ->
            q(<<"select vg_id,tar_id
                 from vgroups
                 where uid = ?
                       and id = ?
                       and archive = 0
                       and blocked != 10">>
             ,[UId, Type])
    end.

-spec tariff_descr_by_tar_id(kz_term:ne_binary()) -> kz_term:proplists().
tariff_descr_by_tar_id(Tar_id) ->
    case q(<<"select descr from tarifs where tar_id = ? limit 1">>, [Tar_id]) of
        [[Descr]] -> Descr;
        [Descr] -> Descr;
        _ -> <<"No description">>
    end.

-spec numbers_by_vg_id(kz_term:ne_binary()) -> kz_term:proplists().
numbers_by_vg_id(Vg_id) ->
    NL = q(<<"SELECT if(substring(phones.number,8)=''
                  ,CONCAT('(812) ', phones.number)
                  ,if(substring(phones.number,11)=''
                     ,CONCAT('(',MID(phones.number,1,3),') ', right(phones.number,7))
                     ,CONCAT('(',MID(phones.number,2,3),') ',RIGHT(phones.number,7))
                     )
                  )
         FROM phones_history, phones
         WHERE vg_id = ?
               and phones.record_id = phones_history.phone_id
               and phones_history.timeto is NULL">>
     ,[Vg_id]),
    [N || [N] <- NL].

-spec ip_addresses_by_vg_id(kz_term:ne_binary()) -> kz_term:proplists().
ip_addresses_by_vg_id(Vg_id) ->
    IPL = q(<<"SELECT concat(INET_NTOA(segment),' / ',INET_NTOA(mask)) FROM staff where vg_id = ?">>, [Vg_id]),
    [IP || [IP] <- IPL].

-spec get_docs_list(kz_term:ne_binary(),kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> kz_term:proplists().
get_docs_list(Year, Month, DocsIds, AccountId) ->
    case lbuid_by_uuid(AccountId) of
        [] -> [];
        UID ->
            QueryString = io_lib:format("SELECT accounts.name, orders.order_id, orders.order_num, orders.period, orders.order_date, if(orders.period >= '2015-12-01',round(orders.curr_summ/1.18,2),round(orders.curr_summ,2)), round(orders.tax_summ,2), if(orders.period >= '2015-12-01',round(orders.curr_summ,2),round(orders.curr_summ,2) + round(orders.tax_summ,2))  FROM orders, accounts where accounts.uid=orders.oper_id and Year(period) = ~s and Month(period) = ~s and agrm_id in (Select agrm_id from agreements where uid = ~s) and doc_id in (~s)", [kz_term:to_binary(Year), kz_term:to_binary(Month), kz_term:to_binary(UID), DocsIds]),

            case mysql_poolboy:query(?LB_MYSQL_POOL, QueryString)
            of
                {ok,_,QueryResult} -> QueryResult;
                _ -> [] 
            end
    end.

-spec get_calls_list_by_day(kz_time:date()
                           ,kz_term:ne_binary()
                           ,kz_term:ne_binary()
                           ,kz_term:ne_binary()
                           ,kz_term:ne_binary()
                           ) -> kz_term:proplists().
get_calls_list_by_day({Year, Month, Day}, Direction, CallsType, MaxCalls, AccountId) ->
    case lbuid_by_uuid(AccountId) of
      [] -> [];
      Uid ->
         QueryCheckTableString = io_lib:format("show tables like 'tel001~w~2..0w~2..0w'", [Year, Month, Day]),
         case q(QueryCheckTableString, []) of
             [] -> [];
             _  ->
                 QueryString = io_lib:format(
                   "select timefrom, numfrom, numto, format(duration_round/60, 0), direction, format(amount, 2), hash
                      from tel001~w~2..0w~2..0w
                      where uid = ~p and direction in (~s) and oper_id in (~s)
                      order by timefrom desc
                      limit ~s"
                  ,[Year
                   ,Month
                   ,Day
                   ,Uid
                   ,Direction
                   ,CallsType
                   ,MaxCalls
                   ]
                 ),
                 q(QueryString, [])
         end
    end.

-spec agreements_creds_by_id(kz_term:integer()) -> kz_term:proplists().
agreements_creds_by_id(Agrm_Id) ->
    q(<<"SELECT accounts.name, agreements.number, agreements.date FROM agreements,
         accounts where accounts.uid=agreements.uid and agreements.agrm_id = ?">>,[Agrm_Id]).

