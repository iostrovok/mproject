-module( client_sender_worker ).
-author( "ostrovok@gmail.com" ).
-behavior(gen_server).

-export([ init/1, start_link/2 ]).
-export([ code_change/3, terminate/2 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2 ]).

-define( RELOAD_TIME, 10000 ). % By default 10 second

-include("records.hrl").
-record( state, { clients = [], mdata }).

code_change(_, _, _) -> ok.
terminate( _, #state{ mdata = SenderClient } = _State ) ->
    ?D( "terminate", " Client", SenderClient ),
    clients_table:sdelete_object( SenderClient ).

% INIT Functions
start_link( SenderClient, Client ) -> gen_server:start_link({ local, SenderClient#client_sender.name }, ?MODULE, [ SenderClient, Client ], []).

init([ InSenderClient, Client ]) ->
    process_flag( trap_exit, true ),
    SenderClient = init_data( InSenderClient ),
    NewClientList = start_client( Client, [] ),
    { ok, #state{ clients = NewClientList, mdata = SenderClient }}.

% CALLBACK Functions
% Gets new clients params
handle_call({ new_client, Client }, _From, #state{ clients = ClientList } = State ) ->
    NewState = State#state{ clients = start_client( Client, ClientList ) },
    { reply, ok, NewState };

handle_call( _, _From, State ) ->
    { noreply, State }.

% Send data to client
handle_cast({ request, Data }, #state{ clients = ClientList } = State ) ->
    lists:map(
            fun( { Name, _Pid } ) ->
                gen_server:cast( Name, { request, Data } )
            end,
        ClientList ),
    { noreply, State };

handle_cast( _A, State) ->
    { noreply, State }.

handle_info({ 'EXIT', _, Reason }, State ) ->
    { stop, Reason, State };
handle_info( _A, State )                    ->
    { stop, bad_request, State }.


init_data( InSenderClient ) ->
    SenderClient = InSenderClient#client_sender{ pid = self() },
    clients_table:sdelete( InSenderClient ),
    clients_table:sinsert( SenderClient ),
    SenderClient.

start_client( Client, ClientsList ) ->
    case clients_table:rget( Client ) of
        undefined -> OutList = start_client_proccess( Client, ClientsList );
        OldClient ->
            case gen_server:call( OldClient#client.name, { reload, Client }, ?RELOAD_TIME ) of
                ok -> OutList = ClientsList;
                _ ->
                    exit( OldClient#client.pid, bad_reload_time ),
                    OutList = start_client_proccess( Client, ClientsList )
            end
    end,
    OutList.

start_client_proccess( Client, ClientsList ) ->
    case client_worker_sup:new_connection( Client ) of
        error -> lists:keydelete( Client#client.name, #client.name, ClientsList );
        Pid   -> lists:keystore( Client#client.name, #client.name, ClientsList, { Client#client.name, Pid })
    end.
