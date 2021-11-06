-ifndef(ZZLB_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(ONBILL_DOC, <<"onbill">>).
-define(ONBILL_DB(ResellerId), <<"onbill-", ResellerId/binary>>).

-define(ZZHD_MYSQL_POOL, 'zzhd_mysql_pool').
-define(ZZHD_KAYAKO_POOL, 'zzhd_kayako_pool').
-define(ZZHD_PGSQL_POOL, 'zzhd_pgsql_pool').

-define(ZZLB_HRL, 'true').
-endif.
