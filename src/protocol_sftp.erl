-module( protocol_sftp ).
-author( "ostrovok@gmail.com" ).
-export([ make/1 ]).

-include("records.hrl").

make( JSON ) ->
    ?D( "++ make START"),
    case ssh:start(temporary) of
        ok -> main( JSON );
        { error, Reason } -> mc:for_send_prepare( {[Reason]}, <<"">> )
    end.


main( JSON ) ->
    ?D( "++ main START"),
    Host = mc:json_val( <<"host">>, JSON ),
    Port = mc:check_int( 22, mc:from_json_to_int(mc:json_val( <<"port">>, JSON ))),
    Password = mc:json_val( <<"passowrd">>, JSON ),
    Login = mc:json_val( <<"login">>, JSON ),
    File = mc:json_val( <<"file">>, JSON ),
    TimeOut = mc:check_int( 500, mc:from_json_to_int(mc:json_val( <<"timeout">>, JSON ))),
    send_stfp_request( Host, Port, File, Login, Password, TimeOut ).

send_stfp_request( Host, Port, File, Login, Password, TimeOut ) ->
    ?D( "send_stfp_request", Host, Port, File, Login, Password, TimeOut),
    case ssh_sftp:start_channel( Host, Port, [{ timeout, TimeOut }, { login, Login }, { passord, Password }] ) of
        { ok, ChannelPid, _Connection } -> read_sftp_file( ChannelPid, File, TimeOut );
        { ok, Pid }                    -> read_sftp_file( Pid, File, TimeOut );
        { error, Reason }              -> mc:for_send_prepare( Reason, <<"">> )
    end.

read_sftp_file( ChannelPid, File, TimeOut )->
    case ssh_sftp:read_file( ChannelPid, File, TimeOut ) of
        { ok, Data }      -> mc:for_send_prepare( <<"">>, Data );
        { error, Reason } -> mc:for_send_prepare( Reason, <<"">> )
    end.
