-module( httpd_server_sup ).
-author( "ostrovok@gmail.com" ).
-behaviour( supervisor ).

-export([ start_link/0, start_link/1, init/1 ]).

-include("records.hrl").

-define( MYCONFIG, [
    { keep_alive, true },
    { error_log, "error.log"},
    { security_log, "security.log"},
    { transfer_log, "transfer.log"},
    { erl_script_nocache, true },
    { document_root, "/home/mproxy_app/www" },
    { server_root, "/home/mproxy_app/log" },
    { server_name, "mproxy_app" },
    { bind_address, any },
    { port, 8888 },
    {
        mime_types, [
            { "html", "text/html"},
            { "js", "application/x-javascript"}
        ]
    },
    { modules, [ mod_get, mod_head, mod_log, mod_disk_log, httpd_server ] }
]).

start_link( _ ) -> start_link().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    inets:start( permanent ),
    config_table:init_defaul_data( http_viewer, ?MYCONFIG ),
    MConfig = config_table:type_config( http_viewer ),
    inets:start( httpd, MConfig ),
    {ok, {{ one_for_one, 10, 1}, []}}.

