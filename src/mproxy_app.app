{
    application, mproxy_app,
    [
        { description, "Multi Proxy"},
        { vsn, "1.1"},
        { modules, [ mproxy_app, boss, mproxy_client_socket_sup, mycommon, httpd_server_sup, socket_listener_sup, socket_listener, mproxy_client_socket_lib, protocol_http ]},
        { registered, [ mproxy_app ]},
        { applications, [ kernel, stdlib ]},
        { mod, { mproxy_app, [] }},
        { included_applications, []}
    ]
}.
