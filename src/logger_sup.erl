-module( logger_sup ).
-author( "ostrovok@gmail.com" ).
-behaviour( supervisor ).

-export([ start_link/0, start_link/1, init/1, log/1, error/1, error/5, access/5, send/5 ]).
-include("records.hrl").
-import( mc ).

start_link( _ ) -> start_link().
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    LoggerAccess = {
        aLooger,
        { logger_access, start_link, [] },
        permanent, brutal_kill, worker,
        [ logger_access ]
    },

    SendLogger = {
        sLooger,
        { logger_send, start_link, [] },
        permanent, brutal_kill, worker,
        [ logger_send ]
    },

    ErrorLogger = {
        eLooger,
        { logger_error, start_link, [] },
        permanent, brutal_kill, worker,
        [ logger_error ]
    },

    { ok, {{ one_for_one, 10, 1 }, [ LoggerAccess, SendLogger, ErrorLogger ]} }.


log( ForLog ) when is_record( ForLog, for_log ) ->
    case ForLog#for_log.type of
        access -> logger_access:rlog( ForLog );
        send   -> logger_send:rlog( ForLog );
        error  -> logger_error:rlog( ForLog );
        Type   -> ?MODULE:error( io_lib:format( "pid: ~p, Module: ~p, Unknown log type ~p [into record ~p]", [ self(), ?MODULE, Type, ForLog ]))
    end;

log( ForLog ) -> ?MODULE:error( io_lib:format( "pid: ~p, Module: ~p, Unknown log type of record ~p", [ self(), ?MODULE, ForLog ])).

error( Error ) ->
    log(
        #for_log{
            mes     = Error,
            type    = error,
            id      = mc:request_id(),
            act     = error
    }).

com( Type, Id, Act, Result, FolLogIn, Message ) ->
    FolLog2 = lists:keysort( 1, lists:keymerge(1, FolLogIn, [{ <<"ft">>, mc:now() }])),
    FolLog = lists:flatmap(fun({ X, Y1 }) ->
                Y = mc:any_to_bin(Y1), [ <<X/binary, ":", Y/binary>> ]
            end, FolLog2 ),
    Line = mc:join_bin_trunc(";", FolLog),
    log(
        #for_log{
            mes     = io_lib:format( "result:~p ~p ~p", [ Result, Line, Message ]),
            type    = Type,
            id      = Id,
            act     = Act
    }).

error( Id, Act, Result, ForLog, Message ) -> com( error, Id, Act, Result, ForLog, Message  ).
access( Id, Act, Result, ForLog, Message ) -> com( access, Id, Act, Result, ForLog, Message  ).
send( Id, Act, Result, ForLog, Message ) -> com( send, Id, Act, Result, ForLog, Message  ).


