-module( mproxy_client_socket_sup ).
-author( "ostrovok@gmail.com" ).
-behaviour( supervisor ).

-include("records.hrl").
-export([start_link/0, init/1, handle_call/3 ]).

start_link() ->  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) -> {ok, {{ one_for_one, 10, 1}, []}}.
handle_call( _, _, State ) -> {reply, ok, State}.
