-module( protocol_simple ).
-author( "ostrovok@gmail.com" ).
-export([ make/1 ]).

make( JSON ) ->
    Host = mc:json_val( <<"host">>, JSON ),
    Port = mc:json_val( <<"port">>, JSON ),
    Body = mc:json_val( <<"body">>, JSON ),
    LastSymbol = last_symbol( mc:json_val( <<"last_symbol">>, JSON ) ),

    case gen_tcp:connect( Host, Port, [ binary, { packet, 0 }, { reuseaddr, true }, { exit_on_close, true }] ) of
        { ok, Socket }    -> send_request( Socket, <<Body/binary, LastSymbol/binary>>  );
        { error, Reason } -> mc:for_send_prepare( {[ Reason ]}, <<"">> )
    end.

send_request( Socket, Body ) ->
    case gen_tcp:send( Socket, Body ) of
        ok ->
            Bin = receive_data(Socket, []),
            mc:for_send_prepare( <<"">>, Bin );
        { error, Reason } -> mc:for_send_prepare( {[ Reason ]}, <<"">> )
    end.

receive_data(Socket, SoFar) ->
    receive
        { tcp, Socket, Bin } ->
            receive_data( Socket, [Bin|SoFar] );
        { tcp_closed, Socket } ->
            list_to_binary(lists:reverse(SoFar))
    end.

last_symbol( <<"null">> ) -> <<"\0">>;
last_symbol( <<"0">> )    -> <<"\0">>;
last_symbol( <<"br">> )   -> <<"\n">>;
last_symbol( <<"br2">> )  -> <<"\n\n">>;
last_symbol( _ )          -> <<"\n">>.