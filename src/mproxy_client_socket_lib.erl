-module( mproxy_client_socket_lib ).
-author( "ostrovok@gmail.com" ).

-import( md5 ).
-import( mc ).

-export([ main_work/3 ]).

-include("records.hrl").
-record( state, { socket, tail = <<"">>, top_pid, pid_send, min_numder = -1, init_time = 0, id = "" }).

% CALLBACK Functions
main_work( { message, << VerIn:10/binary, "#", ActIn:10/binary, "#", NumIn:10/binary, "#", JSONIn/binary >> }, #state{ id = Id, min_numder = MinNumber } = State, ForLog ) ->
    try
        Num = list_to_integer( binary_to_list(binary:replace( NumIn, <<" ">>, <<"">>, [global])) ),
        ?D( "main_work", "Num", Num ),
        if Num > MinNumber ->
            ?D( "main_work", VerIn, ActIn, NumIn, JSONIn ),
            case JSONIn of
                <<"{}">> -> JSON = {[]}; %
                _ -> JSON = ejson:decode(JSONIn)
            end,
            ?D( "main_work", "JSON", JSON ),
            Act = binary:replace(ActIn, <<" ">>, <<"">>, [global]),
            ?D( "main_work", "act", Act ),
            Ver = binary_to_list(binary:replace(VerIn, <<" ">>, <<"">>, [global])),
            ?D( "main_work", "Ver", Ver ),
            main( Ver, Act, Num, JSON, State, ForLog )
        end
    catch
        Class:Term ->
            %% catch-all for all other unexpected exceptions
            %Trace = erlang:get_stacktrace(),
            ?D("error starting1", Class, Term, erlang:get_stacktrace()),
            Message = mc:any_to_bin( io_lib:format("internal error: ~w:~w", [ Class, Term ])),
            send_data( -1, {[{ <<"error">>, <<"format_message">> }, {<<"type">>, <<"undefined">> },
                { <<"body">>, <<"exeption error">> }, { <<"error_message">>, Message }]},
                {[]}, State, ForLog ),
            logger_sup:error( Id, "common", "Error", ForLog, Message )
    end;

main_work( { message, Line }, State, ForLog ) -> % Hire is bad format message from client
    send_data( -1, {[{ <<"error">>, <<"format_message">> }, {<<"type">>, <<"undefined">> }, { <<"body">>, Line }]}, {[]}, State, ForLog ).

main( _, <<"start_read">>, _Number, _JSON, #state{ id = Id, pid_send = PidSend }, ForLog ) ->
    ?D( "main", "start_read", "PidSend", PidSend ),
    gen_server:cast( PidSend, { start_read }),
    logger_sup:access( Id, "start_read", "Success", ForLog, "send success" );

main( _, <<"clean">>, Number, _JSON, #state{ id = Id, pid_send = PidSend, top_pid = PidParent  }, ForLog ) ->
    ?D( "main", "clean", Number ),
    gen_server:cast( PidSend, { clean }),
    gen_server:call( PidParent, { set_min_numder, Number }),
    logger_sup:access( Id, "clean", "Success", ForLog, "send success" );

main( _, <<"send">>, Number, JSON, #state{ id = Id, min_numder = MinNumber } = State, FolLogIn  ) when MinNumber < Number ->
    ?D( "main", "sock", "start" ),
    SendData = mc:json_val( <<"data">>, JSON ),
    MemKey  = mc:json_val( <<"mem_key">>, JSON ),
    MemTime = mc:check_int( 300, mc:any_to_int(mc:json_val( <<"mem_timeout">>, JSON ))),
    ?D( "main", "sock", "MemTime", MemTime ),

    ProtocolIn = mc:json_val( <<"protocol">>, JSON ),
    ?D( "main", "sock", "Protocol", ProtocolIn ),
    { Module, Protocol, IsProtocol, MessageProtocol } = client_common:init_protocol( ProtocolIn, MemTime ),
    ?D("main. one_worker_in 4", Module, Protocol, IsProtocol, MessageProtocol ),

    FolLog = lists:keymerge(1, FolLogIn, [{ <<"pt:">>, mc:now() } ]),

    case IsProtocol of
        false ->
            Message = mc:join_bin_trunc(" ", [ "Module:", Module, "Number:", Number, "MemKey:", MemKey, "MemTime:", MemTime ]),
            send_data( Number, Message, JSON, State, FolLog ),
            logger_sup:error( Id, "sock", "Error", FolLog, Message );
        true ->
            main_send( Module, Protocol, Number, JSON, SendData, MemKey, MemTime, State, FolLog )
    end;

main( _, <<"send">>, Number, _, #state{ min_numder = MinNumber }, _ ) when MinNumber >= Number -> ok;
main( Ver, Act, Number, JSON, State, ForLog ) -> send_data( Number, {[{ "error", "action" }, {<<"ver">>, Ver }, {<<"act">>, Act }]}, JSON, State, ForLog ).

get_mecached( Key ) when Key == undefined -> error_key;
get_mecached( Key ) -> mcd:get( mainCluster, Key ).
set_mecached( Key, _, _ ) when Key == undefined -> error_key;
set_mecached( Key, Value, TimeOout ) -> mcd:set( mainCluster, Key, Value, 1, TimeOout ).

main_send( Module, Protocol, Number, JSON, SendData, MemKey, MemTimeOout, #state{ id = Id } = State, FolLogIn ) ->
    Time1 = mc:now(),
    case get_mecached( MemKey ) of
        { ok, MemValue } ->
            Time2 = mc:now(),
            FolLogSp = lists:keymerge(1, FolLogIn, [{ <<"br:">>, <<"-1">> }, { <<"t1">>, Time1 }, { <<"t2">>, Time2 } ]),
            send_data( Number, MemValue, JSON, State, FolLogSp ),
            Time3 = mc:now(),
            case client_common:execute_protocol( Module, Protocol, SendData ) of
                { success, Value } ->
                    Time4 = mc:now(),
                    Branch = <<"1">>,
                    set_mecached( MemKey, Value, MemTimeOout );
                _ ->
                    Branch = <<"2">>,
                    Time4 = mc:now()
            end;
        _ ->
            Time2 = mc:now(),
            case client_common:execute_protocol( Module, Protocol, SendData ) of
                { error, Error } ->
                    Time3 = mc:now(),
                    Branch = <<"3">>,
                    FolLogSp = lists:keymerge(1, FolLogIn, [{ <<"br:">>, Branch }, { <<"t1">>, Time1 }, { <<"t2">>, Time2 }, { <<"t3">>, Time3 } ]),
                    send_data( Number, Error, JSON, State, FolLogSp ),
                    Time4 = mc:now();
                { success, Value } ->
                    Time3 = mc:now(),
                    Branch = <<"4">>,
                    FolLogSp = lists:keymerge(1, FolLogIn, [{ <<"br:">>, Branch }, { <<"t1">>, Time1 }, { <<"t2">>, Time2 }, { <<"t3">>, Time3 } ]),
                    send_data( Number, Value, JSON, State, FolLogSp ),
                    Time4 = mc:now(),
                    set_mecached( MemKey, Value, MemTimeOout )
            end
    end,
    FolLog = lists:keymerge(1, FolLogIn, [{ <<"br:">>, Branch }, { <<"t1">>, Time1 }, { <<"t2">>, Time2 }, { <<"t3">>, Time3 }, { <<"t4">>, Time4 }]),
    logger_sup:access( Id, "sock", "Success", FolLog, "send success" ).


%====================================== Addition function
send_data( Number, Result, JSON, #state{ id = Id, pid_send = PidSend }, ForLog ) ->
    ?D(Number, "Result", Result),
    Line  = ejson:encode({[{ <<"number">>, Number }, { <<"request">>, JSON }, { <<"result">>, Result }]}),
    ?D(Number, "send_data", Line),
    gen_server:cast( PidSend, { message, Id, Line, ForLog }).
