-module( mproxy_client_socket ).
-author( "ostrovok@gmail.com" ).
-behavior(gen_server).

-export([ start_link/1, start_link/0 ]).
-export([ init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3 ]).

-include("records.hrl").
-record( state, { socket, tail = <<"">>, top_pid, pid_send, min_numder = -1, init_time = 0, id = "" }).

code_change(_, _, _) -> ok.
terminate( _, _ ) -> ok.

% INIT Functions
start_link() -> exit(bad_socket).
start_link( Socket ) -> gen_server:start_link( ?MODULE, [Socket], []).

init([ Socket ]) ->
    process_flag( trap_exit, true ),
    gen_tcp:controlling_process( Socket, self() ),
    inet:setopts( Socket, [{ active, once }]),

    { ok, PidSend } = gen_server:start_link( socket_send_lib, [ Socket ], [] ),
    { ok, #state{ socket = Socket, top_pid = self(), pid_send = PidSend, min_numder = 0, init_time = mc:now() }}.

% CALLBACK Functions
handle_call({ set_min_numder, Number }, _From, State ) -> { noreply, State#state{ min_numder = Number } };
handle_call( _, _, State ) -> { noreply, State }.
handle_cast( _, State ) -> { noreply, State }.

split_binary( Binary ) ->
    AllList = binary:split( Binary, [ <<"\0">> ], [ trim, global ]),
	case binary:last( Binary ) of
        0 -> T = <<"">>, List = AllList;
		_ -> T = lists:last( AllList ), List = lists:delete(T, AllList)
	end,
	{ List, T }.

handle_info({tcp, Socket, Binary }, #state{ socket = Socket, tail = T } = State) ->
    inet:setopts( Socket, [{ active, once }] ),
	{ List, Tail }  = split_binary( <<T/binary, Binary/binary>> ),

    ForLog = [{ <<"it">>, State#state.init_time }, { <<"st">>, mc:now() } , { <<"id">>, mc:request_id() } ],

    lists:map(fun(E) ->
            ?D( "handle_info", E ),
            ?D( "handle_info", "ForLog", ForLog ),
            spawn(fun() -> apply( mproxy_client_socket_lib, main_work, [{ message, E }, State, ForLog ]) end )
        end, List),
    { noreply, State#state{ tail = Tail }};


handle_info({ tcp_closed, _ },   State ) -> { stop, normal, State };
handle_info({ tcp_error,  _, _}, State ) -> { stop, normal, State };
handle_info({ 'EXIT', _, _  },   State ) -> { stop, normal, State }.


