-module( socket_send_lib ).
-author( "ostrovok@gmail.com" ).
-behavior(gen_server).

% Just send data to socket. Nothing more.
-export([ start_link/1, start_link/0, stop/0 ]).
-export([ init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3 ]).

-include("records.hrl").
-record( state, { socket, can_send = false, send_list = [] }).
-record( message, { data, start_time = 0, id = "", for_log = [] }).

code_change(_, _, _) -> ok.
terminate( _, _)     -> ok.
stop()               -> ok.

% INIT Functions
start_link( )        -> exit( socket_error ).
start_link( Socket ) -> gen_server:start_link( ?MODULE, [ Socket ], []).
init([ Socket ])     ->
    process_flag( trap_exit, true ),
    { ok, #state{ socket = Socket }}.

% CALLBACK Functions
handle_info({ 'EXIT', _, _ }, State ) ->  {stop, normal, State};
handle_info( Info, State) -> ?D("handle_info", Info, State ), { noreply, State }.


handle_call( _Request, From, State ) -> ?D("handle_call - ERROR, FROM: ", From ), { noreply, State }.

handle_cast( { start_read }, #state{ socket = Socket, send_list = SendList } = State ) ->
    ?D("handle_cast - 0, start_read ", io_lib:format("~p", [State]) ),
    send_data_list( Socket, SendList ),
    { noreply, State#state{ can_send = true, send_list = [] } };
handle_cast( { message, Id, SendData, FolLogIn }, #state{ socket = Socket, can_send = CanSend, send_list = SendList } = State ) ->
    ?D("handle_cast - 1, message", State),
    FolLog = lists:keymerge( 1, FolLogIn, [{ <<"pt:">>,  mc:now() }]),
    case CanSend of
        false ->
            NewSendList = lists:append(SendList, [ #message{ data = SendData, id = Id, for_log = FolLog } ]);
        true ->
            NewSendList = [],
            send_data_list( Socket, SendList ),
            send_data( Socket, #message{ data = SendData, id = Id, for_log = FolLog } )
    end,
    { noreply, State#state{ send_list = NewSendList } };
handle_cast( { clean }, State ) ->
    ?D("handle_cast - 2. clean", io_lib:format("~p", [State])),
    { noreply, State#state{ can_send = false, send_list = [] } };
handle_cast( { stop, _Reason }, State ) ->
    ?D("handle_cast - 2. stop", io_lib:format("~p", [State])),
    {stop, normal, State};
handle_cast( _Request, State ) ->
    ?D("handle_cast - ERROR", io_lib:format("~p", [State])),
    { noreply, State }.

% WORK Funcitons
send_data_list( _, [] ) -> ok;
send_data_list( Socket, [ SendData | SendList ] ) ->
    send_data( Socket, SendData ),
    send_data_list( Socket, SendList ).

send_data( Socket, #message{ data = SendData, id = Id, for_log = FolLog }) ->
	try
        ?D("send_data 1", mc:localtime(string), Socket, SendData ),
        ok = gen_tcp:send( Socket,  mc:any_to_bin( SendData )  ),
        ?D("send_data 2", mc:localtime(string), Socket ),
        ok = gen_tcp:send( Socket, <<"\n">> ),
        ?D("send_data 3", mc:localtime(string)),
        logger_sup:send( Id, "sock", "Success", FolLog, "send success" )
	catch
        Class:Term ->
            Message = mc:any_to_bin( io_lib:format("internal error: ~w:~w", [ Class, Term ])),
            ?D("error send_data 1", Class, Term, erlang:get_stacktrace()),
            ?D("error send_data 2", Message),
            logger_sup:send( Id, "sock", "Error", FolLog, "send error" ),
            logger_sup:error( Id, "sock", "Error", FolLog, Message ),
            exit(socket_error)
	end.
