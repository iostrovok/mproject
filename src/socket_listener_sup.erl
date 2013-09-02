-module( socket_listener_sup ).
-author( "ostrovok@gmail.com" ).
-behaviour( supervisor ).

-export([ start_link/0, start_link/1, init/1 ]).

-import( socket_listener ).

start_link( _ ) -> start_link().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SocketListener = {
        workerSocketListener,
        { socket_listener, start_link, [] },
        permanent, brutal_kill, worker,
        [ socket_listener ]
    },

    { ok, {{ one_for_one, 10, 1 }, [ SocketListener ]} }.
