-module(cb_zzhds).

-export([init/0
         ,allowed_methods/0,allowed_methods/1
         ,resource_exists/0,resource_exists/1
         ,validate/1,validate/2
         ,content_types_provided/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-include("zzhd.hrl").

-define(ACCOUNT_INFO, <<"account_info">>).
-define(CID_INFO, <<"cid_info">>).
-define(HD_INFO, <<"hd_info">>).
-define(HD_ACCOUNTS, <<"hd_accounts">>).
-define(HD_COMMENTS, <<"hd_comments">>).
-define(ACCOUNT_DOCS, <<"account_docs">>).
-define(ACCOUNT_CDR, <<"account_cdr">>).

-type payload() :: {cowboy_req:req(), cb_context:context()}.
-export_type([payload/0]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.zzhds">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.zzhds">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.zzhds">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.zzhds">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ACCOUNT_INFO) ->
    [?HTTP_GET];
allowed_methods(?CID_INFO) ->
    [?HTTP_GET];
allowed_methods(?HD_ACCOUNTS) ->
    [?HTTP_GET];
allowed_methods(?HD_INFO) ->
    [?HTTP_GET];
allowed_methods(?HD_COMMENTS) ->
    [?HTTP_POST, ?HTTP_GET];
allowed_methods(?ACCOUNT_DOCS) ->
    [?HTTP_POST, ?HTTP_GET];
allowed_methods(?ACCOUNT_CDR) ->
    [?HTTP_POST].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, ?ACCOUNT_DOCS) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            CTPs = ?CONTENT_PROVIDED ++ [{'to_pdf', ?PDF_CONTENT_TYPES}],
            cb_context:add_content_types_provided(Context, CTPs);
        _Verb ->
            Context
    end;
content_types_provided(Context, _) ->
    Context.

-spec get_pdf(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
get_pdf(Period, Filename, Context) ->
    {ok, PDF} = file:read_file(<<"/usr/local/billing/reports/", Period/binary, "/", Filename/binary>>),
    CD = <<"attachment; filename=\"", Filename/binary>>,
    Context1 = cb_context:set_resp_header(Context, <<"content-disposition">>, CD),
    cb_context:set_resp_data(Context1, PDF).

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_zzhd(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(),path_token()) -> cb_context:context().
validate(Context, ?ACCOUNT_INFO) ->
    case zzhd_mysql:lbuid_by_uuid(cb_context:account_id(Context)) of
        'undefined' ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                %        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                        ]);
        _ ->
            validate_account_info(Context, cb_context:req_verb(Context))
    end;
validate(Context, ?HD_ACCOUNTS) ->
    Md5Hash = cb_context:req_value(Context, <<"md5">>),
    case zz_util:get_children_list(<<"d2047d303c22e5399c796a93848dcd9f">>) of
        {'ok', List} ->
            cb_context:setters(Context, [{fun cb_context:set_resp_data/2, [kz_json:get_value(<<"value">>,JObj)|| JObj <- List]}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        {_, _R}=Error ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, []}
                                        ])
    end;
validate(Context, ?CID_INFO) ->
    Number = cb_context:req_value(Context, <<"phone_number">>),
    Num = kz_term:to_binary([Ch || Ch <- kz_term:to_list(Number), Ch >= $0 andalso Ch < $9]),
    case knm_number:lookup_account(Num) of
        {'ok', AccountId, _ExtraOptions} ->
            return_account_info(Context, AccountId);
        {_, _R}=Error ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                        ])
    end;
validate(Context, ?HD_INFO) ->
    lager:info("validate/2  req_data: ~p",[cb_context:req_data(Context)]),
    lager:info("validate/2  req_files: ~p",[cb_context:req_files(Context)]),
    lager:info("validate/2  req_headers: ~p",[cb_context:req_headers(Context)]),
    lager:info("validate/2  req_nouns: ~p",[cb_context:req_nouns(Context)]),
    lager:info("validate/2  req_verb: ~p",[cb_context:req_verb(Context)]),
    lager:info("validate/2  req_id: ~p",[cb_context:req_id(Context)]),
    lager:info("validate/2  req_value: ~p",[cb_context:req_value(Context, <<"consumer_accountId">>)]),
    case cb_context:req_value(Context, <<"consumer_accountId">>) of
        ?MATCH_ACCOUNT_RAW(AccountId) ->
            lager:info("validate/2 HD_INFO Account Matched"),
            return_account_info(Context, AccountId);
        _ ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                        ])
    end;
validate(Context, ?HD_COMMENTS) ->
    validate_hd_comments(Context, cb_context:req_verb(Context));
validate(Context, ?ACCOUNT_DOCS) ->
    validate_account_docs(Context, cb_context:req_verb(Context));
validate(Context, ?ACCOUNT_CDR) ->
    validate_account_cdr(Context, cb_context:req_verb(Context)).

-spec validate_hd_comments(cb_context:context(), http_method()) -> cb_context:context().
validate_hd_comments(Context, ?HTTP_GET) ->
    lager:info("validate_hd_comments validate/2  req_value consumer_accountId: ~p",[cb_context:req_value(Context, <<"consumer_accountId">>)]),
    lager:info("validate_hd_comments validate/2  req_value lb_id: ~p",[cb_context:req_value(Context, <<"lb_id">>)]),
    lager:info("validate_hd_comments validate/2  req_value phone_number: ~p",[cb_context:req_value(Context, <<"phone_number">>)]),
    QS = cb_context:query_string(Context),
    lager:info("validate_hd_comments validate/2 query_string: ~p",[QS]),
    case kz_json:get_first_defined([<<"consumer_accountId">>,<<"lb_id">>,<<"phone_number">>], QS) of
        'undefined' ->
            return_hd_comments_all(Context);
        _ ->
            return_hd_comments_select(Context)
    end;
validate_hd_comments(Context, ?HTTP_POST) ->
    lager:info("validate_hd_comments/2  req_data: ~p",[cb_context:req_data(Context)]),
    ReqData = cb_context:req_data(Context),
    Comment = kz_json:get_value(<<"comment">>, ReqData),
    ConsumerAccountId = kz_json:get_value(<<"consumer_accountId">>, ReqData),
    lager:info("validate_hd_comments/2  Comment: ~p",[Comment]),
    lager:info("validate_hd_comments/2  ConsumerAccountId: ~p",[ConsumerAccountId]),
    Res = pgapp:equery(?ZZHD_PGSQL_POOL, "INSERT INTO public.comments (comment,  kz_account_id) VALUES($1, $2)", [Comment, ConsumerAccountId]),
    lager:info("validate_hd_comments/2 Res: ~p",[Res]),
    cb_context:set_resp_status(Context, 'success').

-spec validate_zzhd(cb_context:context(), http_method()) -> cb_context:context().
validate_zzhd(Context, ?HTTP_PUT) ->
    AccountId = cb_context:account_id(Context),
    case cb_context:is_superduper_admin(Context)
         orelse
         (kz_services:get_reseller_id(AccountId) == cb_context:auth_account_id(Context))
    of
        'true' ->
            ReqData = cb_context:req_data(Context),
            Action = kz_json:get_value(<<"action">>, ReqData),
            validate_zzhd(Context, kz_term:to_binary(Action), AccountId);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

validate_zzhd(Context, <<"lb_to_kazoo_sync">>, AccountId) ->
    zzhd:lb_to_kazoo_sync(AccountId),
    cb_context:set_resp_status(Context, 'success');
validate_zzhd(Context, <<"kazoo_to_lb_sync">>, AccountId) ->
    zzhd:kazoo_to_lb_sync(AccountId),
    cb_context:set_resp_status(Context, 'success');
validate_zzhd(Context, _, _AccountId) ->
    cb_context:add_system_error('forbidden', Context).
    
-spec validate_account_info(cb_context:context(), http_method()) -> cb_context:context().
validate_account_info(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    return_account_info(Context, AccountId).

account_info_jobj(AccountId) ->
    [_StatusID, BlockDate] = zzhd_mysql:account_status(AccountId),
    AccountStatus =
        case zzhd_mysql:account_status(AccountId) of
            [0, BlockDate] ->
                kz_json:from_list([{'status', 'true'},{'block_date', kz_time:iso8601(BlockDate)}]); 
            [_, BlockDate] ->
                kz_json:from_list([{'status', 'false'},{'block_date', kz_time:iso8601(BlockDate)}])
        end, 
    AccountPayments = 
        [ kz_json:from_list([{'amount', Amount}
                            ,{'pay_date', PayDate}
                            ,{'comment', Comment}
                            ,{'status', Status}
                            ,{'cancel_date', CancelDate}
                            ])
            || [Amount, PayDate, Comment, Status, CancelDate] <- zzhd_mysql:account_payments(AccountId)],
    MonthlyFees =
        [ kz_json:from_list([{'fee_name', FeeName}
                            ,{'price', Price}
                            ,{'quantity', Qty}
                            ,{'cost', Cost}
                            ])
            || [FeeName, Price, Qty, Cost] <- zzhd_mysql:monthly_fees(AccountId)],
    PhoneNumbersByTariff =
        [ kz_json:from_list([{'tar_id', TarId}
                            ,{'tar_descr', zzhd_mysql:tariff_descr_by_tar_id(TarId)}
                            ,{'vg_id_numbers', zzhd_mysql:numbers_by_vg_id(VgId)}
                            ])
            || [VgId, TarId] <- zzhd_mysql:accounts_tariffs_by_type(1, AccountId)],
    IPAddressesByTariff =
        [ kz_json:from_list([{'tar_id', TarId}
                            ,{'tar_descr', zzhd_mysql:tariff_descr_by_tar_id(TarId)}
                            ,{'vg_id_ip_addresses', zzhd_mysql:ip_addresses_by_vg_id(VgId)}
                            ])
            || [VgId, TarId] <- zzhd_mysql:accounts_tariffs_by_type(2, AccountId)],
    [[_CompanyName, AgrmNum, {AY, AM, AD}]] =
        zzhd_mysql:agreements_creds_by_id(zzhd_mysql:main_agrm_id(AccountId)),
    kz_json:from_list(
      [{<<"account_balance">>, zzhd_mysql:account_balance(AccountId)}
      ,{<<"main_agrm">>
       ,{[{<<"agrm_num">>, AgrmNum}
        ,{<<"agrm_date">>, <<(kz_term:to_binary(AY))/binary,"-", (kz_date:pad_month(AM))/binary,"-", (kz_date:pad_month(AD))/binary>>}
        ]}
       }
      ,{<<"account_info">>, kz_json:from_list(zzhd_mysql:accounts_table_info(AccountId))}
      ,{<<"kazoo_account_id">>, AccountId}
      ,{<<"lb_id">>, zzhd_mysql:lbuid_by_uuid(AccountId)}
      ,{<<"account_status">>, AccountStatus}
      ,{<<"account_payments">>, AccountPayments}
      ,{<<"monthly_fees">>, MonthlyFees}
      ,{<<"phone_numbers_by_tariff">>, PhoneNumbersByTariff}
      ,{<<"ip_addresses_by_tariff">>, IPAddressesByTariff}
      ]).

return_account_info(Context, AccountId) ->
    JObj = account_info_jobj(AccountId),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, JObj}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).

return_hd_comments_select(Context) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "SELECT * FROM public.comments", []) of
        {ok, Columns, Rows} ->
            Rs = [kz_json:from_list([{<<"comment">>, Comment}
                                    ,{<<"created">>, maybe_correct_datetime(Created)}
                                    ,{<<"modified">>, maybe_correct_datetime(Modified)}
                                    ,{<<"account_id">>, AccountId}
                                    ,{<<"informer_id">>, InformerId}
                                   ])
                    || {Comment, Created, Modified, AccountId, InformerId} <- Rows],
            lager:info("return_hd_comments_select Columns: ~p",[Columns]),
            lager:info("return_hd_comments_select Rows: ~p",[Rows]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                               %%         ,{fun cb_context:set_resp_data/2, kz_json:set_value(<<"rows">>, Rs, kz_json:new())}
                                        ,{fun cb_context:set_resp_data/2, Rs}
                                        ]);
        {error, Error} -> 
            lager:info("return_hd_comments_select Error: ", [Error]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'error'}
                                        ,{fun cb_context:set_resp_data/2, Error}
                                        ])
    end.

return_hd_comments_all(Context) ->
    case pgapp:equery(?ZZHD_PGSQL_POOL, "SELECT * FROM public.comments", []) of
        {ok, Columns, Rows} ->
            Rs = [kz_json:from_list([{<<"comment">>, Comment}
                                    ,{<<"created">>, maybe_correct_datetime(Created)}
                                    ,{<<"modified">>, maybe_correct_datetime(Modified)}
                                    ,{<<"account_id">>, AccountId}
                                    ,{<<"informer_id">>, InformerId}
                                   ])
                    || {Comment, Created, Modified, AccountId, InformerId} <- Rows],
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, Rs}
                                        ]);
        {error, Error} -> 
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'error'}
                                        ,{fun cb_context:set_resp_data/2, Error}
                                        ])
    end.

%maybe_correct_tuple(T) ->
%    L1 = tuple_to_list(T),
%    L2 = [maybe_correct_datetime(Field) || Field <- L1],
%    list_to_tuple(L2).

maybe_correct_datetime({{_, _, _}=Date, {H, M, S}}) ->
    kz_time:gregorian_seconds_to_unix_seconds(calendar:datetime_to_gregorian_seconds({Date,{H,M,kz_term:to_integer(S)}}));
maybe_correct_datetime(Field) ->
    Field.

-spec validate_account_docs(cb_context:context(), http_method()) -> cb_context:context().
validate_account_docs(Context, ?HTTP_GET) ->
    lager:info("IAM validate_account_docs GET"),
    Nouns = cb_context:req_nouns(Context),
    lager:info("validate_account_docs IAMNOUNS: ~p",[Nouns]),
    QS = cb_context:query_string(Context),
    lager:info("validate_account_docs IAMQS: ~p",[QS]),
    case {kz_json:get_value(<<"period">>, QS) ,kz_json:get_value(<<"order_id">>, QS)} of
        {undefined, _} ->
           cb_context:add_system_error('forbidden', Context);
        {_, undefined} ->
           cb_context:add_system_error('forbidden', Context);
        {Period, OrderId} ->
           cb_context:set_resp_status(get_pdf(Period, <<OrderId/binary, ".pdf">>, Context), 'success')
    end;
validate_account_docs(Context, ?HTTP_POST) ->
    AccountId = cb_context:account_id(Context),
    ReqData = cb_context:req_data(Context),
    {Year, Month} =
        case kz_json:get_value(<<"year">>, ReqData) == 'undefined'
               orelse 
             kz_json:get_value(<<"month">>, ReqData) == 'undefined'
        of
            true ->
                {Y, M, _} = erlang:date(),
                {kz_term:to_binary(Y), kz_term:to_binary(M)};
            false ->
                {kz_json:get_value(<<"year">>, ReqData)
                ,kz_json:get_value(<<"month">>, ReqData)}
        end,
    RespJObj = kz_json:from_list(
      [{<<"proformas">>, docs_list_to_json(zzhd_mysql:get_docs_list(Year, Month, <<"1">>, AccountId))}
      ,{<<"acts">>, docs_list_to_json(zzhd_mysql:get_docs_list(Year, Month, <<"2">>, AccountId))}
      ,{<<"vat_invoices">>, docs_list_to_json(zzhd_mysql:get_docs_list(Year, Month, <<"3">>, AccountId))}
      ,{<<"calls_reports_pdf">>, docs_list_to_json(zzhd_mysql:get_docs_list(Year, Month, <<"43">>, AccountId))}
      ,{<<"account_id">>, AccountId}
      ]),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespJObj}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).
docs_list_to_json(DLs) ->
    [ kz_json:from_list(
        [{<<"name">>, Name}
        ,{<<"order_id">>, OrderId}
        ,{<<"order_num">>, OrderNum}
        ,{<<"period">>, <<(kz_term:to_binary(YP))/binary,"-", (kz_date:pad_month(MP))/binary>>}
        ,{<<"order_date">>, <<(kz_term:to_binary(Y))/binary,"-", (kz_date:pad_month(M))/binary,"-", (kz_date:pad_month(D))/binary>>}
        ,{<<"price_netto">>, Netto}
        ,{<<"vat">>, VAT}
        ,{<<"price_brutto">>, Brutto}])
      || [Name, OrderId, OrderNum, {YP,MP,_DP}, {Y,M,D}, Netto, VAT, Brutto] <- DLs ]. 

-spec validate_account_cdr(cb_context:context(), http_method()) -> cb_context:context().
validate_account_cdr(Context, ?HTTP_POST) ->
    AccountId = cb_context:account_id(Context),
    ReqData = cb_context:req_data(Context),
    Date =
        case kz_json:get_value(<<"year">>, ReqData) == 'undefined'
               orelse 
             kz_json:get_value(<<"month">>, ReqData) == 'undefined'
               orelse 
             kz_json:get_value(<<"day">>, ReqData) == 'undefined'
        of
            true ->
                {Y, M, D} = erlang:date();
            false ->
                {kz_json:get_integer_value(<<"year">>, ReqData)
                ,kz_json:get_integer_value(<<"month">>, ReqData)
                ,kz_json:get_integer_value(<<"day">>, ReqData)}
        end,
        Direction = kz_json:get_value(<<"direction">>, ReqData, <<"1">>),
        CallsType = kz_json:get_value(<<"calls_type">>, ReqData, <<"1,2,3,4">>),
        MaxCalls = kz_json:get_value(<<"max_calls">>, ReqData, <<"5000">>),
    RespJObj = kz_json:from_list(
      [{<<"cdrs">>, cdr_to_json(zzhd_mysql:get_calls_list_by_day(Date, Direction, CallsType, MaxCalls, AccountId))}
      ]),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespJObj}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).
 
cdr_to_json(CL) ->
    [ kz_json:from_list(
        [{<<"timefrom">>, kz_time:iso8601(localtime:local_to_local(Timefrom, "Europe/Moscow", "UTC"))}
        ,{<<"numfrom">>, Numfrom}
        ,{<<"numto">>, Numto}
        ,{<<"duration">>, Duration}
        ,{<<"direction">>, Direction}
        ,{<<"amount">>, Amount}
        ,{<<"key">>, Hash}])
      || [Timefrom, Numfrom, Numto, Duration, Direction, Amount, Hash] <- CL ]. 

