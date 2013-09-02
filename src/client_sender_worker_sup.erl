-module( client_sender_worker_sup ).
-author( "ostrovok@gmail.com" ).
-behaviour( supervisor ).

-export([start_link/0, init/1, handle_call/3 ]).
-export([ new_connection/2 ]).

-include("records.hrl").

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) -> {ok, {{ one_for_one, 10, 1}, [] }}.
handle_call( _Request, _From, State ) -> { reply, ok, State }.

new_connection( SenderClient, Client ) ->
    Opts = prepare_data ( SenderClient, Client ),
    ?D( "new_sender_connection", "Opts",  Opts ),
    case supervisor:start_child( client_sender_worker_sup, Opts ) of
        { ok, Pid }    when is_pid( Pid ) -> Pid;
        { ok, Pid, _ } when is_pid( Pid ) -> Pid;
        _ -> error
    end.

prepare_data( SenderClient, Client ) ->
    {
        SenderClient#client_sender.name, % Uniq proccess name
        { client_sender_worker, start_link, [ SenderClient, Client ] },
        temporary, brutal_kill, worker, [ client_sender_worker ]
    }.
