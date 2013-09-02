-module( socket_listener ).
-author( "ostrovok@gmail.com" ).
-behaviour( gen_server ).

-export([ start/0, start_link/0, start_link/1, start_link/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-define( HOST, {127,0,0,1} ). % By default
-define( PORT, 40005 ).       % By default

-include("records.hrl").

-record( state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                socket_options  % New socket_options
            }).

terminate( _Reason, State )            -> gen_tcp:close( State#state.listener ).
code_change( _OldVsn, State, _Extra ) -> {ok, State}.

start() -> supervisor:start_child( socket_listener_sup, [] ).

start_link()                  -> start_link([]).
start_link([])                -> start_link([ ?HOST,   ?PORT ]);
start_link([ Address ])       -> start_link([ Address, ?PORT ]);
start_link([ Address, Port ]) -> gen_server:start_link( ?MODULE, [ Address, Port ], []).

start_link( Name, [])                                   -> start_link(Name, [ ?HOST,   ?PORT ]);
start_link( Name, [ Address ])                          -> start_link(Name, [ Address, ?PORT ]);
start_link( Name, [ Address, Port ]) when is_atom(Name) -> gen_server:start_link({ local, Name }, ?MODULE, [ Address, Port ], []).

init([ Address, Port ]) ->
    process_flag( trap_exit, true ),
    Opts = [ binary, {packet, 0}, { ip, Address }, { port, Port }, { active, false }, { exit_on_close, true }, { reuseaddr, true }],
    case gen_tcp:listen( Port, Opts ) of
        { ok, Listener } ->
            % Create first accepting process
            { ok, Ref } = prim_inet:async_accept( Listener, -1 ),
            { ok, #state{ listener = Listener, acceptor = Ref, socket_options = Opts }};
        { error, Reason } ->
            { stop, Reason }
    end.

handle_call( _Request, _From, State ) -> { reply, ok, State }.
handle_cast(_Msg, State) -> { noreply, State }.

handle_info({ inet_async, Listener, _OldRef, { ok, CliSocket }}, #state{ listener = Listener } = State ) ->
    try
        true = inet_db:register_socket( CliSocket, inet_tcp ),
        { ok, Opts } =  prim_inet:getopts( Listener, [ active, nodelay, keepalive, delay_send, priority, tos ]),
        ok = prim_inet:setopts( CliSocket, Opts ),
        ok = new_connection( CliSocket ),
        { ok, Ref } = prim_inet:async_accept( Listener, -1 ),
        { noreply, State#state{ listener = Listener, acceptor = Ref }}
    catch
        _What:Why ->
            error_logger:error_msg("Error in async accept: ~p.\n", [ Why ]),
            gen_tcp:close( CliSocket ),
            { stop, Why, State }
    end;

handle_info( _Info, State ) -> { noreply, State }.

new_connection( CliSocket ) ->
    ListenerOpts = {
        mc:random_string(), % Uniq proccess name
        { mproxy_client_socket, start_link, [ CliSocket ] },
        temporary, brutal_kill, worker, [ mproxy_client_socket ]
    },

    case supervisor:start_child( mproxy_client_socket_sup, ListenerOpts ) of
        { ok, Pid }    when is_pid( Pid ) -> gen_tcp:controlling_process( CliSocket, Pid ), ok;
        { ok, Pid, _ } when is_pid( Pid ) -> gen_tcp:controlling_process( CliSocket, Pid ), ok;
        _ -> error
    end.
