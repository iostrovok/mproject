-module( protocol_ftp ).
-author( "ostrovok@gmail.com" ).

-export([ make/1 ]).

make( JSON ) ->
    Host = mc:json_val( <<"host">>, JSON ),
    Port = mc:check_int( 21, mc:any_to_int(mc:json_val( <<"port">>, JSON ))),
    TimeOut = mc:check_int( 500, mc:any_to_int(mc:json_val( <<"timeout">>, JSON ))),
    Password = mc:json_val( <<"passowrd">>, JSON ),
    Login = mc:json_val( <<"login">>, JSON ),

    inets:start(),
    case inets:start(ftpc, [ {host, erlang:binary_to_list(Host)}, { port, Port }, {timeout, TimeOut} ]) of
        { ok, Pid }       -> login( Pid, Login, Password, JSON );
        { error, Reason } -> mc:for_send_prepare( erlang:list_to_binary(Reason), <<"">> )
    end.

login( Pid, undefined, undefined, JSON ) -> get_file( Pid, JSON );
login( Pid, Login, Password, JSON ) ->
    case ftp:user( Pid, Login, Password ) of
        ok -> get_file( Pid, JSON );
        { error, Reason } -> mc:for_send_prepare( erlang:list_to_binary(Reason), <<"">> )
    end.

get_file( Pid, JSON ) ->
    File = mc:json_val( <<"file">>, JSON ),

    case ftp:recv_bin( Pid, File ) of
        { ok, Bin }       -> mc:for_send_prepare( <<"">>, Bin );
        { error, Reason } -> mc:for_send_prepare( erlang:list_to_binary(Reason), <<"">> )
    end.
