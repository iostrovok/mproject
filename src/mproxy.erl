-module(mproxy).
-author( "ostrovok@gmail.com" ).
-behaviour(application).

-export([ start/2, stop/1, ss/0 ]).

-include("records.hrl").

% appmon:start(). application:start(mproxy_app). mc:all_object(clients). mc:all_object(clients_sender).
start( _Type, _StartArgs) -> boss:start_link().
stop( _State ) -> ok.

ss() ->
    ?D( "appmon:start(). mc:all_object(clients). mc:all_object(clients_sender).~nmc:all_object(mconfig).~n"),
    appmon:start(),
    application:start(mproxy_app),
    mc:all_object(clients),
    mc:all_object(clients_sender),
    ok.