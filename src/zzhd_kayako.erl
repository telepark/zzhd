-module(zzhd_kayako).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([maybe_kayako_app/0
        ,q_raw/1
        ,q_raw/2
        ,q/1
        ,q/2
        ,q_json/1
        ,get_tickets_by_informer_id/1
        ]).

-include_lib("zzhd.hrl").

-spec maybe_kayako_app() -> any().
maybe_kayako_app() ->
    case kapps_config:get_is_true(<<"zzhd">>, <<"kayako_pool_enable">>, 'false') of
        'true' ->
            MySQLHost = kz_term:to_list(kapps_config:get_string(<<"zzhd">>, <<"kayako_host">>, <<"localhost">>)),
            MySQLDB = kz_term:to_list(kapps_config:get_string(<<"zzhd">>, <<"kayako_database">>, <<"database">>)),
            MySQLUser = kz_term:to_list(kapps_config:get_string(<<"zzhd">>, <<"kayako_user">>, <<"user">>)),
            MySQLPwd = kz_term:to_list(kapps_config:get_string(<<"zzhd">>, <<"kayako_password">>, <<"password">>)),
            application:ensure_all_started('emysql'),
            emysql:add_pool(?ZZHD_KAYAKO_POOL, 10, MySQLUser, MySQLPwd, MySQLHost, 3306, MySQLDB, utf8);
        'false' ->
            'not_enabled'
    end.

-spec q(kz_term:ne_binary()) -> any().
q(Query) ->
    q(Query,[]).
-spec q(kz_term:ne_binary(),list()) -> any().
q(Query,Parameters) ->
    emysql:prepare(zmydb_query,Query),
    {_,_,_,Result,_} = emysql:execute(?ZZHD_KAYAKO_POOL, zmydb_query, Parameters),
    Result.

-spec q_raw(kz_term:ne_binary()) -> any().
q_raw(Query) ->
    q_raw(Query,[]).
-spec q_raw(kz_term:ne_binary(),list()) -> any().
q_raw(Query,Parameters) ->
    emysql:prepare(zmydb_query,Query),
    emysql:execute(?ZZHD_KAYAKO_POOL, zmydb_query, Parameters).

-spec q_json(kz_term:ne_binary()) -> any().
q_json(Query) ->
    Res = q_raw(Query),
    emysql:as_json(Res).

-spec get_tickets_by_informer_id(kz_term:ne_binary()|integer()) -> any().
get_tickets_by_informer_id(InformerId) ->
    Emails = zzhd:stakeholder_emails(InformerId),
    Q = io_lib:format("select * from swtickets where email in (~s)"
                     ,[kz_binary:join([<<"'", B/binary, "'">> || B <- Emails], ", " )]),
    emysql:as_json(q_raw(kz_term:to_binary(Q))).


%% Q4 = io_lib:format("select * from swtickets where email in (~s)", [kz_binary:join([<<"'", B/binary, "'">> || B <- Args0], ", " )]).
%% emysql:as_json(zzhd_kayako:q_raw(kz_term:to_binary(Q4))). 
