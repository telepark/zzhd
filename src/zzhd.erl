-module(zzhd).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([stakeholder_emails/1
        ,stakeholder_phonenumbers/1
        ]).

-include_lib("zzhd.hrl").

-spec stakeholder_emails(kz_term:ne_binary()|integer()) -> any().
stakeholder_emails(InformerId) ->
    InformerEmails = zzhd_pgsql:get_informer_emails(InformerId),
    AccountId = zzhd_pgsql:get_kz_by_informer_id(InformerId),
    LBEmails = [re:replace(Email, "[^A-Za-z0-9@.\-_~+]", "", [global, {return, binary}])
                || Email <- zzhd_mysql:get_accounts_emails(AccountId)],
    InformerEmails ++ LBEmails.

-spec stakeholder_phonenumbers(kz_term:ne_binary()|integer()) -> any().
stakeholder_phonenumbers(InformerId) ->
    InformerPhonenumbers = zzhd_pgsql:get_informer_phonenumbers(InformerId),
    AccountId = zzhd_pgsql:get_kz_by_informer_id(InformerId),
    LBPhonenumbers = [re:replace(Phonenumber, "[0-9]", "", [global, {return, binary}])
                || Phonenumber <- zzhd_mysql:get_accounts_phonenumbers(AccountId)],
    InformerPhonenumbers ++ LBPhonenumbers.
