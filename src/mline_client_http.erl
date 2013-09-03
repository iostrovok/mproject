-module( mline_client_http ).
-author( "ostrovok@gmail.com" ).

-export([ view_config/0 ]).
-export([ sub/1, unsub/1, client_list/0, send_req/1 ]).

-include("records.hrl").

-define( RELOAD_TIME, 10000 ). % By default 10 second
-define( DEFAULT_TIMEOUT, 180000 ). % By default 10 second

view_config() -> config_table:all( ).

send_req( InData ) ->
    case lists:keyfind( "type", 1, InData ) of
        false -> Type = "", MessageType = "type error. ";
        { _, Type } -> MessageType = ""
    end,

    case lists:keyfind( "data", 1, InData ) of
        false -> Data = undefined, MessageSendData = "data found error. ";
        { _, Data } -> MessageSendData = ""
    end,

    Error = MessageType ++ MessageSendData,
    case Error of
        "" ->
                ?D("Type", Type ),
                Sender = clients_table:srget( Type ),
                case is_record(Sender, client_sender) of
                    true ->
                        ?D("SenderClient#client_sender.name", Sender#client_sender.name ),
                        gen_server:cast( Sender#client_sender.name, { request, Data } ),
                        [{ act, "send"}, { result, "success" }, { message, "" }];
                    _ ->
                        [{ act, "send"}, { result, "error" }, { message, "type not found" }, { type, Type }]
                end;
        _ ->
                [{ act, "sub"}, { result, "error" }, { message, Error }, { type, Type }, { data, Data } ]
    end.

unsub( Data ) ->
    case lists:keyfind( "type", 1, Data ) of
        false -> Type = "", MessageType = "type error. ";
        { _, Type } -> MessageType = "type success. "
    end,

    case lists:keyfind( "client_id", 1, Data ) of
        false -> ClientId = "", MessageClientId = "client_id error. ";
        { _, ClientId } -> MessageClientId = "client_id success. "
    end,

    Error = MessageType ++ MessageClientId,
    unsub_in( Type, ClientId, Error ).

unsub_in( Type, ClientId, Error ) when ClientId /= "", Type /= "" ->
    clients_table:delete( Type, ClientId ),
    [{ act, "unsub"}, { result, "success" }, { message, Error }];

unsub_in( Type, ClientId, Error ) -> [{ act, "sub"}, { result, "error" }, { message, Error }, { type, Type }, { client_id, ClientId } ].

client_list() ->
    [{ act, "sub"}, { result, "success" }, { "clients", mc:all_object(clients) }].

% Subscribes offline clienttype
sub( Data ) ->

    ?D("sub - 1", [  Data  ]),
    { Type, IsType, MessageType } = mc:get_from_list([ "type", <<"type">> ], Data ),
    { ClientId, IsClientId, MessageClientId } = mc:get_from_list([ "client_id", <<"client_id">> ], Data ),

    case config_table:rget( mline_client_http, timeout ) of
        undefined -> DefaultLiveTime = ?DEFAULT_TIMEOUT;
        DefaultLiveTime -> ok
    end,

    { LiveTime, MessageLiveTime } = client_common:get_timeout( Data, "live_time", DefaultLiveTime ),
    ?D( "LiveTime", LiveTime ),

    { Module, Protocol, IsProtocol, MessageProtocol } = client_common:init_protocol( Data, LiveTime ),

    %OutMessage = lists:flatten([ MessageType, " ",  MessageClientId, " ", MessageLiveTime, " ", MessageProtocol ]),
    OutMessage = mc:join_bin_trunc(" ", [ MessageType, MessageClientId, MessageLiveTime, MessageProtocol ]),

    ?D( "OutMessage", OutMessage ),

    case IsClientId and IsType and IsProtocol of
        true ->
            sub_in( Type, ClientId, LiveTime, Module, Protocol, OutMessage );
        _ ->
            [{ act, "sub"}, { result, "error" }, { message, OutMessage }, { type, Type }, { client_id, ClientId }, { live_time, LiveTime } ]
    end.

sub_in( Type, ClientId, LiveTime, Module, Protocol, OutMessage ) ->
    Sender = #client_sender {
        name        = clients_table:sckey( Type ),  % Proccess random uniq name
        type        = Type,                         % type subscribe
        pid         = list_to_pid("<0.0.0>")        % Pid for clients
    },

    Client = #client{
        type = Type,
        client_id = ClientId,
        on_off_line = offline,
        live_time = LiveTime,
        pid = list_to_pid("<0.0.0>"),
        protocol = Protocol,
        module = Module,
        created = mc:localtime( string ),
        name =  clients_table:ckey( Type, ClientId )
    },

    case clients_table:srget( Sender ) of
        undefined -> client_sender_worker_sup:new_connection( Sender, Client );
        OldSender ->
            case gen_server:call( OldSender#client_sender.name, { new_client, Client }, ?RELOAD_TIME ) of
                ok -> ok;
                _ ->
                    exit( OldSender#client_sender.pid, bad_reload_time ),
                    client_sender_worker_sup:new_connection( Sender, Client )
            end
    end,
    [{ act, "sub"}, { result, "success" }, { message, OutMessage }].






