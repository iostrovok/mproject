-module( client_worker_sup ).
-author( "ostrovok@gmail.com" ).
-behaviour( supervisor ).

-export([start_link/0, init/1, handle_call/3 ]).
-export([ new_connection/1 ]).

-include("records.hrl").
-import( mc ).

start_link() -> supervisor:start_link({ local, ?MODULE }, ?MODULE, [] ).

init([]) ->
    % Start old client
    OldClients = lists:map(
        fun( { _Key, Client } ) -> mline_client_http:client_data ( Client ) end,
        mc:all_records( clients )
    ),
    ?D( "init", "OldClients", OldClients ),
    {ok, {{ one_for_one, 10, 1}, OldClients }}.

handle_call( _Request, _From, State ) -> { reply, ok, State }.

new_connection( Client ) ->
    Opts = client_data ( Client ),
    case supervisor:start_child( client_worker_sup, Opts ) of
        { ok, Pid }    when is_pid( Pid ) -> Pid;
        { ok, Pid, _ } when is_pid( Pid ) -> Pid;
        _ -> error
    end.

client_data( Client ) ->
    {
        Client#client.name, % Uniq proccess name
        { client_worker, start_link, [ Client ] },
        temporary, brutal_kill, worker, [ client_worker ]
    }.
