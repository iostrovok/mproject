-module(mproxy_app).
-author( "ostrovok@gmail.com" ).
-behaviour(application).

-export([ start/2, stop/1 ]).

% appmon:start(). application:start(mproxy_app). mc:all_object(clients).
start( _Type, _StartArgs) -> boss:start_link().
stop( _State ) -> ok.


