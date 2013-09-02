-module( boss ).
-author( "ostrovok@gmail.com" ).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-include("records.hrl").

-define( MEMCACHED_CONFIG, [
    { is_uses, true },
    { servers, [[{ 127, 0, 0, 1 }, 11211 ]] },
    { namespace, "m" }
]).

start_link() ->
    ets:new( clients_sender,    [ set, named_table, public, { keypos, #client_sender.name }, { read_concurrency, true }, { write_concurrency, false }]),
    ets:new( clients,           [ set, named_table, public, { keypos, #client.name }, { read_concurrency, true }, { write_concurrency, false }] ),
    ets:new( messages_type,     [ ordered_set, named_table, public ] ),
    ets:new( protocol_load,     [ ordered_set, named_table, public ] ),
    ets:new( mconfig,           [ bag, named_table, public ] ),
    ?D( "start_link"),
    load_mconfig(),
    config_table:init_defaul_data( memcached, ?MEMCACHED_CONFIG ),
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    % Using or no using memcached
    case config_table:rget( memcached, is_uses ) of
        true ->
            MamCachedList = [{
                mainCluster,
                { mcd_starter, start_link, [ mainCluster, [[{ 127, 0, 0, 1 }, 11211 ]]]},
                permanent,
                infinity,
                supervisor,
                [ mcd_starter ]
            }];
        _A -> MamCachedList = []
    end,

    BossSenderClientWorker = {
        my_client_sender_worker_sup,
        { client_sender_worker_sup, start_link, [] },
        permanent,
        brutal_kill,
        supervisor,
        [ client_sender_worker_sup ]
    },

    BossClientWorker = {
        my_client_worker_sup,
        { client_worker_sup, start_link, [] },
        permanent,
        brutal_kill,
        supervisor,
        [ client_worker_sup ]
    },

    BossHttpd =     {
        bossHttpdServer,
        { httpd_server_sup, start_link, [] },
        permanent,
        brutal_kill,
        supervisor,
        [ httpd_server_sup ]
    },

    BossSocket =     {
        bassSocketListener,
        { socket_listener_sup, start_link, [] },
        permanent,
        brutal_kill,
        supervisor,
        [ socket_listener_sup ]
    },

    MProxClientSocket =     {
        my_mproxy_client_socket_sup,
        { mproxy_client_socket_sup, start_link, [] },
        permanent,
        brutal_kill,
        supervisor,
        [ mproxy_client_socket_sup ]
    },

    %MProxClientHttp =     {
    %    my_mproxy_client_http_sup,
    %    { mproxy_client_http_sup, start_link, [] },
    %    permanent,
    %    brutal_kill,
    %    supervisor,
    %    [ mproxy_client_http_sup ]
    %},

    Logger = {
        myLogger,
        { logger_sup, start_link, [ ]},
		permanent,
        infinity,
        supervisor,
        [ logger_sup ]
    },

    List = lists:append(
            MamCachedList,
            [ Logger, BossSenderClientWorker, BossClientWorker, MProxClientSocket, BossSocket, BossHttpd ]
        ),

    ?D( "init", List ),

    { ok, {{ one_for_one, 10, 1 }, List }}.


load_mconfig() ->
    { ok, Config } = file:consult("../include/mproxy.config"),
    ?D( "init", "Config", Config ),
    lists:map( fun( { Type, Key, Value } ) -> config_table:insert( Type, Key, Value ) end, Config ).

