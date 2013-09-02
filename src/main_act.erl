-module( main_act ).
-author( "ostrovok@gmail.com" ).

-export([ act/2 ]).

-include("records.hrl").

act( "config",     _MyRequest ) ->
    Res = mline_client_http:view_config(),
    send_result( Res );

% MLINE
act( "client_list",_MyRequest ) ->
    List = mline_client_http:client_list(),
    send_result( List );
act( "send", MyRequest ) ->
    Res = mline_client_http:send_req( MyRequest ),
    send_result( Res );
act( "unsub",       MyRequest ) ->
    Res = mline_client_http:unsub( MyRequest ),
    send_result( Res );
act( "sub",         MyRequest ) ->
    Res = mline_client_http:sub( MyRequest ),
    send_result( Res );

% MPROXY
act( "message", MyRequest ) -> act_message_try( MyRequest );

% FOR ANY CASE
act( "test",       _MyRequest ) -> { 200, "<html><body>Не послать ли нам гонца за бутылочкой винца?</body></html>" };
act( _Act,         _MyRequest ) -> { 404, "<html><body>Act not found</body></html>" }.

send_result( Data ) ->
    Str = io_lib:format("~p", [ Data ]),
    { "text/html", 200, "<html><body><pre>" ++ Str ++ "</pre></body></html>" }.

act_message_try( MyRequest ) ->
    try
        { ok, Pid } = gen_server:start_link( mproxy_client_http, [ ], [] ),
        Res = gen_server:call( Pid, { request, MyRequest }, 20000 ),
        gen_server:cast( Pid, stop ),
        { "text/plain", 200, Res }
    catch
        Class:Error  ->
            d:err("module: ~p, function: json_decode. ~p:~p", [ ?MODULE, Class, Error ]),
            { "text/plain", 404, io_lib:format("module: ~p, function: json_decode. ~p:~p", [ ?MODULE, Class, Error ] )}

    end.
