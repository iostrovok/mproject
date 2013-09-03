-module( client_common ).
-author( "ostrovok@gmail.com" ).

-export([ init_protocol/2, execute_protocol/3, get_timeout/2, get_timeout/3 ]).
-include("records.hrl").
-define( DEFAULT_TIMEOUT, 500 ).

execute_protocol( Module, Protocol, Data ) ->
	try
        Module:execute_protocol( Protocol, Data )
	catch
        A:B -> { error, io_lib:format("In module ~p error execute request ~p:~p\n", [ Module, A, B ]) }
	end.

transform_protocol( InData ) ->
    ?D("START transform_protocol"),
    case mc:lists_keyfind( [ <<"protocol">>, "protocol" ] , InData ) of
        undefined -> InData;
        { Data } -> Data;
        Data -> Data
    end.

init_protocol( InData, TimeOut ) ->
    ?D("init_protocol"),
    case prepare_data( InData ) of
        { error, Err } ->
            Module = undefined, IsProtocol = false, Protocol = {}, MessageProtocol = lists:flatten(["Init protocol error: ", Err ]);
        { Module, Data } ->
            case init_protocol_try( Module, Data, TimeOut ) of
                { error, MessageProtocol } ->
                    ?D("Error - MessageProtocol", MessageProtocol),
                    IsProtocol = false, Protocol = {};
                {} ->
                    MessageProtocol = list_to_binary( "Module: "++ Module ++" protocol error: Bad protocol data" ),
                    ?D( MessageProtocol ),
                    IsProtocol = false, Protocol = {};
                Protocol ->
                    MessageProtocol = <<"Protocol found success.">>,
                    ?D( MessageProtocol, Protocol ),
                    IsProtocol = true
            end
    end,
    { Module, Protocol, IsProtocol, MessageProtocol }.

prepare_data( InData) ->
    try
        Data = transform_protocol( InData ),
        { get_protocol_file( Data ), Data }
    catch
        _Class:B -> { error, io_lib:format("init protocol error ~p", [ B ]) }
    end.

init_protocol_try( Module, Data, TimeOut ) ->
	try
        mc:load_modul( Module ),
        init_module_protocol( Module, Data, TimeOut )
	catch
        _Class:B ->
            ErStr= io_lib:format("~p", [ B ]),
            { error, <<"init protocol error \"", ErStr/binary, "\" in module: ", Module/binary >> }
	end.

init_module_protocol( Module, Data, TimeOut ) ->
    ?D("init_module_protocol 1--1"),
    case Module:init_protocol( Data, TimeOut ) of
        { success, Protocol } ->
            ?D("init_module_protocol 2--2"),
            Protocol;
        { error, Error } ->
            ?D("init_module_protocol 3--3"),
            { error, mc:join_bin_trunc("", [ "init protocol error \"", Error ,"\" in module: ", Module ])};
        _ ->
            ?D("init_module_protocol 4--4"),
            { error, mc:join_bin_trunc("", [ "init protocol error in module: ", Module ]) }
    end.

get_protocol_file( Data ) ->
    case mc:lists_keyfind([ <<"protocol.type">>, "protocol.type", <<"type">>, "type" ] , Data ) of
        undefined ->  { error, <<"No 'protocol.type' param">> };
        Type ->
            ProtocolBin = mc:any_to_bin( Type ),
            binary_to_atom( <<"protocol_", ProtocolBin/binary>>, latin1 )
    end.

get_timeout( Data, Key ) -> get_timeout( Data, Key, ?DEFAULT_TIMEOUT ).
get_timeout( Data, Key, DefaultTimout ) ->
    case mc:get_from_list( get_prot_keys( Key ), Data ) of
        { _, false, _ }  -> Message = " is default.",  TimeOut = DefaultTimout;
        { Val, true, _ } -> Message = " from request.", TimeOut = mc:check_int( DefaultTimout, mc:any_to_int( Val ))
    end,
    { TimeOut, mc:join_bin_trunc(" ", [ Key, Message ]) }.

get_prot_keys( Key ) -> [ list_to_binary(Key), Key ].
