
-record( client, {
                name = "",          % Proccess random uniq name
                type = "",          % type subscribe
                client_id = "",     %
                on_off_line = "",   % offline or online
                live_time = 6000,   %
                module = "",        % Portocol module
                protocol = "",      % Protocol for offline clients
                created = "",       % DateTime created cliented
                pid                 % Pid for clients
            }).

-record( client_sender, {
                name = "",          % Proccess random uniq name
                type = "",          % type subscribe
                pid                 % Pid for clients
            }).

-record( config, {
                type  = "",         % Proccess random uniq name
                name  = "",         % type subscribe
                value = ""          % Pid for clients
            }).

-record( for_log, {
                type    = "",       % Log typr: error, access, send
                id      = "",       % Uniq id
                mes     = "",       % Add text message
                act     = ""        % Actions type
            }).


% Debug Message
%-ifdef(debug).
    %-define( DF(Format, Args), io:format("~p. ~s. ~w: DEBUG: " ++ Format, [ self(), ?MODULE, ?LINE | Args])).
    %-define( D(E1),                                         io:format(d:debug_fl( 1 ), [ self(), ?MODULE, ?LINE, E1 ])).
    %-define( D(E1, E2),                                     io:format(d:debug_fl( 2 ), [ self(), ?MODULE, ?LINE, E1, E2 ])).
    %-define( D(E1, E2, E3 ),                                io:format(d:debug_fl( 3 ), [ self(), ?MODULE, ?LINE, E1, E2, E3 ])).
    %-define( D(E1, E2, E3, E4 ),                            io:format(d:debug_fl( 4 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4 ])).
    %-define( D(E1, E2, E3, E4, E5 ),                        io:format(d:debug_fl( 5 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4, E5 ])).
    %-define( D(E1, E2, E3, E4, E5, E6 ),                    io:format(d:debug_fl( 6 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4, E5, E6 ])).
    %-define( D(E1, E2, E3, E4, E5, E6, E7 ),                io:format(d:debug_fl( 7 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4, E5, E6, E7 ])).
    %-define( D(E1, E2, E3, E4, E5, E6, E7, E8 ),            io:format(d:debug_fl( 8 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4, E5, E6, E7, E8])).
    %-define( D(E1, E2, E3, E4, E5, E6, E7, E8, E9 ),        io:format(d:debug_fl( 9 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4, E5, E6, E7, E8, E9 ])).
    %-define( D(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10 ),   io:format(d:debug_fl( 10 ), [ self(), ?MODULE, ?LINE, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10 ])).
    %-define( ERW( Class, Error ), io:format("ERROR. ~p module: ~p, line: ~p. ~p:~p~n", [ self(), ?MODULE, ?LINE, Class, Error ])).

%-else.
    -define( DF(Format, Args),                              true).
    -define( D(E),                                          true).
    -define( D(E1, E2),                                     true).
    -define( D(E1, E2, E3 ),                                true).
    -define( D(E1, E2, E3, E4 ),                            true).
    -define( D(E1, E2, E3, E4, E5 ),                        true).
    -define( D(E1, E2, E3, E4, E5, E6 ),                    true).
    -define( D(E1, E2, E3, E4, E5, E6, E7 ),                true).
    -define( D(E1, E2, E3, E4, E5, E6, E7, E8 ),            true).
    -define( D(E1, E2, E3, E4, E5, E6, E7, E8, E9 ),        true).
    -define( D(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10 ),   true).
    -define( ERW( Class, Error ),                           true).
%-endif.


