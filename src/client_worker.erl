-module( client_worker ).
-author( "ostrovok@gmail.com" ).
-behavior(gen_server).

-export([ init/1, start_link/1, start_link/0 ]).
-export([ code_change/3, terminate/2 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2 ]).

-include("records.hrl").
-record( state, { client }).

code_change(_, _, _) -> ok.
terminate( _, #state{ client = Client } = _State  )    ->
    ?D( "terminate", "Client", Client ),
    clients_table:delete_object( Client ).


% INIT Functions
start_link() -> exit(bad_client_records).
start_link( Client ) ->
    gen_server:start_link({ local, Client#client.name }, ?MODULE, [ Client ], []).

init([ InClient ]) ->
    process_flag( trap_exit, true ),
    { ok, #state{ client = init_data( InClient ) }};

init( InClient ) ->
    process_flag( trap_exit, true ),
    { ok, #state{ client = init_data( InClient ) }}.

% CALLBACK Functions
% Gets new clients params
handle_call({ reload, InClient }, _From, State ) ->
    NewState = State#state{ client = init_data( InClient ) },
    { reply, ok, NewState };

handle_call( _, _From, State ) ->
    { noreply, State }.

% Send data to client
handle_cast({ request, Data }, #state{ client = Client } = State ) ->
    ?D("handle_cast", Data, Client ),
    case Client#client.on_off_line of
        online  -> ?D("online"), send_online_data( Data );
        offline -> ?D("offline"), send_offline_data( Client, Data );
        _ -> ?D("error"), error
    end,
    { noreply, State };

handle_cast( _A, State) ->
    { noreply, State }.

handle_info({ 'EXIT', _, Reason }, State ) ->
    { stop, Reason, State };
handle_info( _A, State )                    ->
    { stop, bad_request, State }.


init_data( InClient ) ->
    Client = InClient#client{ pid = self() },
    clients_table:delete( InClient ),
    clients_table:insert( Client ),
    Client.

send_online_data( _Data ) -> ok.
send_offline_data( #client{ live_time = _LiveTime, module = Module,  protocol = Protocol } = _Client, SendData ) ->
    ?D("send_offline_data", "Module", Module ),
    ?D("send_offline_data", "Protocol", Protocol ),
    ?D("send_offline_data", "SendData", SendData ),
	try
        client_common:execute_protocol( Module, Protocol, SendData )
	catch
        A:B ->
            exit(socket_error)
	end.






