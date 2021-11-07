-module(cb_zzhds).

-export([init/0
         ,allowed_methods/0,allowed_methods/1,allowed_methods/2
         ,resource_exists/0,resource_exists/1,resource_exists/2
         ,validate/1,validate/2,validate/3
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-include("zzhd.hrl").

-define(ACCOUNT_INFO, <<"account_info">>).
-define(CID_INFO, <<"cid_info">>).
-define(KZ_INFO, <<"kz_info">>).
-define(INFORMER_INFO, <<"informer_info">>).
-define(INFORMER_NAME, <<"informer_name">>).
-define(INFORMER_NAME_FILL, <<"informer_name_fill">>).
-define(INFORMER_ACCOUNTS, <<"informer_accounts">>).
-define(INFORMER_COMMENTS, <<"informer_comments">>).
-define(INFORMER_TICKETS, <<"informer_tickets">>).
-define(INFORMER_TICKET_MESSAGES, <<"informer_ticket_messages">>).

-type payload() :: {cowboy_req:req(), cb_context:context()}.
-export_type([payload/0]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.zzhds">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.zzhds">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.zzhds">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?ACCOUNT_INFO) ->
    [?HTTP_GET];
allowed_methods(?CID_INFO) ->
    [?HTTP_GET];
allowed_methods(?INFORMER_ACCOUNTS) ->
    [?HTTP_GET];
allowed_methods(?KZ_INFO) ->
    [?HTTP_GET];
allowed_methods(?INFORMER_INFO) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE];
allowed_methods(?INFORMER_COMMENTS) ->
    [?HTTP_POST, ?HTTP_GET];
allowed_methods(?INFORMER_TICKETS) ->
    [?HTTP_GET];
allowed_methods(?INFORMER_TICKET_MESSAGES) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?INFORMER_INFO, ?INFORMER_NAME_FILL) ->
    [?HTTP_POST];
allowed_methods(?INFORMER_COMMENTS, _) ->
    [?HTTP_PUT, ?HTTP_DELETE].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?INFORMER_INFO,_) -> 'true';
resource_exists(?INFORMER_COMMENTS,_) -> 'true'.

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
validate(Context, ?INFORMER_ACCOUNTS) ->
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
            return_all_info(Context, zzhd_pgsql:get_informer_by_kz_id(AccountId), AccountId);
        {_, _R}=Error ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                        ])
    end;
validate(Context, ?INFORMER_INFO) ->
    case cb_context:req_value(Context, <<"informer_id">>) of
        'undefined' ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                        ]);
        InformerId ->
            validate_informer_info(Context, InformerId, cb_context:req_verb(Context))
    end;
validate(Context, ?KZ_INFO) ->
    case cb_context:req_value(Context, <<"consumer_accountId">>) of
        ?MATCH_ACCOUNT_RAW(AccountId) ->
            lager:info("validate/2 KZ_INFO Account Matched"),
            return_all_info(Context, zzhd_pgsql:get_informer_by_kz_id(AccountId), AccountId);
        _ ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                        ])
    end;
validate(Context, ?INFORMER_COMMENTS) ->
    validate_hd_comments(Context, cb_context:req_verb(Context));
validate(Context, ?INFORMER_TICKETS) ->
    validate_hd_tickets(Context, cb_context:req_verb(Context));
validate(Context, ?INFORMER_TICKET_MESSAGES) ->
    validate_hd_ticket_messages(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(),path_token(),path_token()) -> cb_context:context().
validate(Context, ?INFORMER_INFO, ?INFORMER_NAME_FILL) ->
    validate_informer_name_fill(Context, cb_context:req_verb(Context));
validate(Context, ?INFORMER_COMMENTS, CommentId) ->
    validate_hd_comments(Context, CommentId, cb_context:req_verb(Context)).

-spec validate_hd_comments(cb_context:context(), http_method()) -> cb_context:context().
validate_hd_comments(Context, ?HTTP_GET) ->
    lager:info("validate_hd_comments validate/2  req_value informer_id: ~p",[cb_context:req_value(Context, <<"informer_id">>)]),
    QS = cb_context:query_string(Context),
    lager:info("validate_hd_comments validate/2 query_string: ~p",[QS]),
    case kz_json:get_value([<<"informer_id">>], QS) of
        'undefined' ->
            return_hd_comments_all(Context);
        _ ->
            return_hd_comments_select(Context)
    end;
validate_hd_comments(Context, ?HTTP_POST) ->
    lager:info("validate_hd_comments/2  req_data: ~p",[cb_context:req_data(Context)]),
    ReqData = cb_context:req_data(Context),
    CommentHTML = kz_json:get_value(<<"comment_html">>, ReqData),
    CommentTEXT = kz_json:get_value(<<"comment_text">>, ReqData),
    InformerId = kz_json:get_value(<<"informer_id">>, ReqData),
    Res = pgapp:equery(?ZZHD_PGSQL_POOL, "INSERT INTO public.comments (comment_html,comment_text,informer_id) VALUES($1, $2, $3)", [CommentHTML, CommentTEXT, InformerId]),
    lager:info("validate_hd_comments/2 Res: ~p",[Res]),
    cb_context:set_resp_status(Context, 'success').

-spec validate_hd_comments(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
validate_hd_comments(Context, CommentId, ?HTTP_DELETE) ->
    Res = pgapp:equery(?ZZHD_PGSQL_POOL, "DELETE FROM public.comments where comment_id = $1", [kz_term:to_integer(CommentId)]),
    lager:info("validate_hd_comments/3 Res: ~p",[Res]),
    cb_context:set_resp_status(Context, 'success');
validate_hd_comments(Context, CommentId, ?HTTP_PUT) ->
    lager:info("validate_hd_comments/2  req_data: ~p",[cb_context:req_data(Context)]),
    ReqData = cb_context:req_data(Context),
    CommentHTML = kz_json:get_value(<<"comment_html">>, ReqData),
    CommentTEXT = kz_json:get_value(<<"comment_text">>, ReqData),
    Res = pgapp:equery(?ZZHD_PGSQL_POOL, "UPDATE public.comments SET comment_html=$1, comment_text=$2, modified=current_timestamp WHERE comment_id=$3", [CommentHTML, CommentTEXT, kz_term:to_integer(CommentId)]),
    lager:info("validate_hd_comments/2 Res: ~p",[Res]),
    cb_context:set_resp_status(Context, 'success').

-spec validate_hd_tickets(cb_context:context(), http_method()) -> cb_context:context().
validate_hd_tickets(Context, ?HTTP_GET) ->
    lager:info("validate_hd_tickets validate/2  req_value informer_id: ~p",[cb_context:req_value(Context, <<"informer_id">>)]),
    QS = cb_context:query_string(Context),
    lager:info("validate_hd_tickets validate/2 query_string: ~p",[QS]),
    case kz_json:get_value([<<"informer_id">>], QS) of
        'undefined' ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'error'}
                                        ,{fun cb_context:set_resp_data/2, []}
                                        ]);
        _ ->
            return_hd_tickets_select(Context)
    end.

-spec validate_hd_ticket_messages(cb_context:context(), http_method()) -> cb_context:context().
validate_hd_ticket_messages(Context, ?HTTP_GET) ->
    QS = cb_context:query_string(Context),
    lager:info("validate_hd_ticket_messages validate/2 query_string: ~p",[QS]),
    case kz_json:get_value([<<"ticket_id">>], QS) of
        'undefined' ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'error'}
                                        ,{fun cb_context:set_resp_data/2, []}
                                        ]);
        _ ->
            return_hd_ticket_messages_select(Context)
    end.

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
    return_all_info(Context, zzhd_pgsql:get_informer_by_kz_id(AccountId), AccountId).

info_jobj(InformerId, AccountId) ->
    List = props:filter_undefined(
           [{<<"informer_info_jobj">>, informer_info_jobj(InformerId)}
           ,{<<"account_info_jobj">>, account_info_jobj(AccountId)}
           ]),
    kz_json:from_list(List).
    
informer_info_jobj('undefined') -> 'undefined';
informer_info_jobj(InformerId) ->
    List = props:filter_undefined(
           [{<<"informer_name">>, zzhd_pgsql:get_informer_name(InformerId)}
           ,{<<"emails">>, zzhd_pgsql:get_informer_emails(InformerId)}
           ,{<<"phone_numbers">>, zzhd_pgsql:get_informer_phonenumbers(InformerId)}
           ]),
    kz_json:from_list(List).

account_info_jobj('undefined') -> 'undefined';
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
    [[CompanyName, AgrmNum, {AY, AM, AD}]] =
        zzhd_mysql:agreements_creds_by_id(zzhd_mysql:main_agrm_id(AccountId)),
    LB_Id = zzhd_mysql:lbuid_by_uuid(AccountId),
    InformerId = zzhd_pgsql:get_informer_by_kz_id(AccountId),
    zzhd_pgsql:maybe_set_informer_name(InformerId, CompanyName),
    kz_json:from_list(
      [{<<"account_balance">>, zzhd_mysql:account_balance(AccountId)}
      ,{<<"main_agrm">>
       ,{[{<<"agrm_num">>, AgrmNum}
        ,{<<"agrm_date">>, <<(kz_term:to_binary(AY))/binary,"-", (kz_date:pad_month(AM))/binary,"-", (kz_date:pad_month(AD))/binary>>}
        ]}
       }
      ,{<<"account_info">>, kz_json:from_list(zzhd_mysql:accounts_table_info(AccountId))}
      ,{<<"kazoo_account_id">>, AccountId}
      ,{<<"lb_id">>, LB_Id}
      ,{<<"informer_id">>, zzhd_pgsql:get_informer_by_kz_id(AccountId)}
      ,{<<"account_status">>, AccountStatus}
      ,{<<"account_payments">>, AccountPayments}
      ,{<<"monthly_fees">>, MonthlyFees}
      ,{<<"phone_numbers_by_tariff">>, PhoneNumbersByTariff}
      ,{<<"ip_addresses_by_tariff">>, IPAddressesByTariff}
      ]).

return_all_info(Context, InformerId, AccountId) ->
    JObj = info_jobj(InformerId, AccountId),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, JObj}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).

return_hd_comments_select(Context) ->
    QS = cb_context:query_string(Context),
    InformerId = kz_json:get_value([<<"informer_id">>], QS),
    case pgapp:equery(?ZZHD_PGSQL_POOL, "SELECT created,modified,informer_id,comment_id,comment_text,comment_html FROM public.comments where informer_id = $1 order by modified desc", [kz_term:to_integer(InformerId)]) of
        {ok, Columns, Rows} ->
            Rs = [kz_json:from_list([{<<"created">>, maybe_correct_datetime(Created)}
                                    ,{<<"modified">>, maybe_correct_datetime(Modified)}
                                    ,{<<"informer_id">>, InfId}
                                    ,{<<"comment_id">>, CommentId}
                                    ,{<<"comment_text">>, CommentTEXT}
                                    ,{<<"comment_html">>, CommentHTML}
                                   ])
                    || {Created, Modified, InfId, CommentId, CommentTEXT, CommentHTML} <- Rows],
            lager:info("return_hd_comments_select Columns: ~p",[Columns]),
            lager:info("return_hd_comments_select Rows: ~p",[Rows]),
            lager:info("return_hd_comments_select Rs: ~p",[Rs]),
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
    case pgapp:equery(?ZZHD_PGSQL_POOL, "SELECT c.created,c.modified,c.informer_id,c.comment_id,c.comment_text,c.comment_html,i.informer_name FROM comments c, identities i where c.informer_id = i.id order by c.modified desc", []) of
        {ok, Columns, Rows} ->
            Rs = [kz_json:from_list([{<<"created">>, maybe_correct_datetime(Created)}
                                    ,{<<"modified">>, maybe_correct_datetime(Modified)}
                                    ,{<<"informer_id">>, InformerId}
                                    ,{<<"comment_id">>, CommentId}
                                    ,{<<"comment_text">>, CommentTEXT}
                                    ,{<<"comment_html">>, CommentHTML}
                                    ,{<<"informer_name">>, InformerName}
                                   ])
                    || {Created, Modified, InformerId, CommentId, CommentTEXT, CommentHTML, InformerName} <- Rows],
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

return_hd_tickets_select(Context) ->
    QS = cb_context:query_string(Context),
    InformerId = kz_json:get_value([<<"informer_id">>], QS),
    Tickets = zzhd_kayako:get_tickets_by_informer_id(InformerId),
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, [kz_json:from_list(Ticket) || Ticket <- Tickets]}
                                ]).

return_hd_ticket_messages_select(Context) ->
    QS = cb_context:query_string(Context),
    TicketId = kz_json:get_value([<<"ticket_id">>], QS),
    Messages = zzhd_kayako:get_messages_by_ticket_id(TicketId),
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, [kz_json:from_list(Message) || Message <- Messages]}
                                ]).

validate_informer_info(Context, InformerId, ?HTTP_GET) ->
    lager:info("validate/2 INFORMER_INFO InformerId: ~p",[InformerId]),
    return_all_info(Context, InformerId, zzhd_pgsql:get_kz_by_informer_id(InformerId));
validate_informer_info(Context, InformerId, ?HTTP_PUT) ->
    lager:info("validate/2 INFORMER_INFO InformerId: ~p",[InformerId]),
    ReqData = cb_context:req_data(Context),
    case kz_json:get_value(<<"informer_name">>, ReqData) of
        'undefined' -> 'ok';
        InformerName -> 
            Res = zzhd_pgsql:set_informer_name(InformerId, re:replace(InformerName, "[^A-Za-z0-9@.\-_~+]", "", [global, {return, binary}])),
            lager:info("validate/2 INFORMER_INFO Res: ~p",[Res])
    end,
    case kz_json:get_value(<<"informer_phonenumber">>, ReqData) of
        'undefined' -> 'ok';
        InformerPhoneNumber -> 
            ResPN = zzhd_pgsql:set_informer_phonenumber(InformerId, re:replace(InformerPhoneNumber, "[^0-9+]", "", [global, {return, binary}])),
            lager:info("validate/2 INFORMER_INFO ResPN: ~p",[ResPN])
    end,
    case kz_json:get_value(<<"informer_email">>, ReqData) of
        'undefined' -> 'ok';
        InformerEmail -> 
            ResEml = zzhd_pgsql:set_informer_email(InformerId, re:replace(InformerEmail, "[^A-Za-z0-9@.\-_~+]", "", [global, {return, binary}])),
            lager:info("validate/2 INFORMER_INFO Res: ~p",[ResEml])
    end,
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                ]);
validate_informer_info(Context, InformerId, ?HTTP_DELETE) ->
    lager:info("validate/2 INFORMER_INFO InformerId: ~p",[InformerId]),
    ReqData = cb_context:req_data(Context),
    case kz_json:get_value(<<"informer_phonenumber">>, ReqData) of
        'undefined' -> 'ok';
        InformerPhoneNumber -> 
            ResPN = zzhd_pgsql:delete_informer_phonenumber(InformerId, re:replace(InformerPhoneNumber, "[^0-9+]", "", [global, {return, binary}])),
            lager:info("validate/2 INFORMER_INFO ResPN: ~p",[ResPN])
    end,
    case kz_json:get_value(<<"informer_email">>, ReqData) of
        'undefined' -> 'ok';
        InformerEmail -> 
            ResEml = zzhd_pgsql:delete_informer_email(InformerId, re:replace(InformerEmail, "[^A-Za-z0-9@.\-]", "", [global, {return, binary}])),
            lager:info("validate/2 INFORMER_INFO Res: ~p",[ResEml])
    end,
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                ]).

validate_informer_name_fill(Context, ?HTTP_POST) ->
    ReqData = cb_context:req_data(Context),
    InformerId = kz_json:get_value(<<"informer_id">>, ReqData),
    AccountId = zzhd_pgsql:get_kz_by_informer_id(InformerId),
    Props = zzhd_mysql:accounts_table_info(AccountId),
    case props:get_value(name, Props) of
        LBName when is_binary(LBName) ->
            zzhd_pgsql:set_informer_name(InformerId, LBName);
        _ -> 'ok'
    end,
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                ]).

