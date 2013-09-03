-export([ init/1, start_link/0 ]).
-export([ code_change/3, terminate/2 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2 ]).
-export([ log/1, log/2, rlog/1 ]).

-include("records.hrl").

-record( file_time, { year, month, day, hour, minute, second }).
-record( state, { dir, file, file_format_minute, file_format_hour, file_format_day, iodev, file_rotate, file_time, file_format }).
-define( MODE, [ append, write ] ).

log( Format, Data ) -> gen_server:cast( ?MODULE, { log, Format, Data }).
log( Data ) -> gen_server:cast( ?MODULE, { log, Data } ).
rlog( ForLog ) -> gen_server:cast( ?MODULE, { rlog, ForLog } ).

% INIT Functions
start_link() -> gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

init([]) ->
    process_flag( trap_exit, true ),
    State = init_file_name(init_params( ?MODULE )),
    NState = open_file( State ),
    { ok, NState }.

code_change(_, _, _) -> ok.
terminate( Reason, State  ) -> logger_sup:error( io_lib:format( "terminate/2 ~p, MODUL: ~p,\n-------------\n State: ~p,\n-------------\n Reason: ~p \n-------------\n\n", [ self(), ?MODULE, State, Reason ])).

% CALLBACK Functions
handle_call({ log, Format, Data }, _From, State ) -> { reply, ok, save( Format, Data, State ) };
handle_call({ log,         Data }, _From, State ) -> { reply, ok, save(         Data, State ) };

handle_call({ rlog,      ForLog }, _From, State ) ->
    case prepare_rlog( ForLog ) of
        { ok, Data } -> NewState = save( Data, State );
        error -> NewState = State
    end,
    { reply, ok, NewState }.

% Send data to client
handle_cast({ log, Format, Data },        State ) -> { noreply,   save( Format, Data, State ) };
handle_cast({ log,         Data },        State ) -> { noreply,   save(         Data, State ) };
handle_cast({ rlog,      ForLog },        State ) ->
    case prepare_rlog( ForLog ) of
        { ok, Data } -> NewState = save( Data, State );
        error -> NewState = State
    end,
    { noreply, NewState }.

handle_info( _A, State ) -> { noreply, State }.

save( Format, Data, State ) ->
    Line = lists:flatten(io_lib:format( Format, Data )),
    save( Line, State).

save( Data, State ) ->
    Line = re:replace( Data, "\n|\s+"," ",[global, { return, binary }]),
    %Line = mc:any_to_bin( Data ),
    NewState = check_file(State),
    case file:write( NewState#state.iodev, <<Line/binary, "\n">> ) of
        ok  -> ok;
        { error, Reason } -> exit({ error, Reason })
    end,
    NewState.

init_params( Log ) ->

    case config_table:rget( Log, dir ) of
        undefined -> Dir = './logs/';
        Dir -> ok
    end,

    case config_table:rget( Log, file_format ) of
        undefined -> Format = "mlog_"++ Log ++".log";
        Format -> ok
    end,

    case config_table:rget( Log, file_format_day ) of
        undefined -> DayFormat = "mlog_"++ Log ++"~4..0B~2..0B~2..0B.log";
        DayFormat -> ok
    end,

    case config_table:rget( Log, file_format_hour ) of
        undefined -> HourFormat = "mlog_"++ Log ++"~4..0B~2..0B~2..0B_~2..0B.log";
        HourFormat -> ok
    end,

    case config_table:rget( Log, file_format_minute ) of
        undefined -> MinuteFormat = "mlog_"++ Log ++"~4..0B~2..0B~2..0B_~2..0B_~2..0B.log";
        MinuteFormat -> ok
    end,

    case config_table:rget( Log, file_rotate ) of
        undefined -> FileRotate = day;
        FileRotate -> ok
    end,

    #state{ dir = Dir, file_format_minute = MinuteFormat, file_format_hour = HourFormat,file_format_day = DayFormat, file_rotate = FileRotate, file_format = Format }.

init_file_name( #state{ dir = Dir, file_format = FileFormat } = State ) ->
    {{ Year, Month, Day }, { Hour, Minute, Second }} = erlang:localtime(),
    FileTime = #file_time{ year = Year, month = Month, day = Day, hour = Hour, minute = Minute, second = Second },
    State#state{ file = mc:any_to_bin( Dir ++ FileFormat ), file_time = FileTime }.

move_file_name( #state{  file_rotate = FileRotate, dir = Dir, file_format_minute = MinuteFormat, file_format_hour = HourFormat, file_format_day = DayFormat }) ->
    {{ Year, Month, Day }, { Hour, Minute, _Second }} = erlang:localtime(),
    case FileRotate of
        day -> File = lists:flatten(io_lib:format( DayFormat, [ Year, Month, Day ]));
        hour -> File = lists:flatten(io_lib:format( HourFormat, [ Year, Month, Day, Hour ]));
        minute -> File = lists:flatten(io_lib:format( MinuteFormat, [ Year, Month, Day, Hour, Minute ]))
    end,
    mc:any_to_bin( Dir ++ File ).

open_file( #state{  file = File } = State ) ->
    case file:open( File, ?MODE) of
        { ok, IoDevice } -> State#state{ iodev = IoDevice };
        { error, Reason } -> exit({ error, Reason })
    end.

check_file( #state{  file_rotate = FileRotate, file_time = FileTime } = State ) ->
    case check_time( FileRotate, FileTime ) of
        false ->
            my_close( State ),
            NewState = open_file(init_file_name( State ));
        true ->
            NewState = State
    end,
    NewState.

check_time( day, #file_time{ year = Y, month = M, day = D } ) ->
    {{ Year, Month, Day }, { _Hour, _Minute, _Second }} = erlang:localtime(),
    ( Y == Year ) and ( M == Month ) and ( D == Day );
check_time( hour, #file_time{ year = Y, month = M, day = D, hour = H } ) ->
    {{ Year, Month, Day }, { Hour, _Minute, _Second }} = erlang:localtime(),
    ( Y == Year ) and ( M == Month ) and ( D == Day ) and ( H == Hour );
check_time( minute, #file_time{ year = Y, month = M, day = D, hour = H, minute = Mi } ) ->
    {{ Year, Month, Day }, { Hour, Minute, _Second }} = erlang:localtime(),
    ( Y == Year ) and ( M == Month ) and ( D == Day ) and ( H == Hour ) and ( Mi == Minute ).

my_close( #state{  file = File, iodev = IoDevice  } = State  ) ->
    MoveFile = move_file_name( State ),
	try
        file:close( IoDevice ),
        file:rename( File, MoveFile )
	catch
        A:B -> logger_sup:error( io_lib:format("clode file ERROR ~p, MODUL: ~p, A: ~p, B: ~p \n", [ self(), ?MODULE, A, B ]))
    end.

prepare_rlog( ForLog ) when is_record( ForLog, for_log ) ->
    Now = io_lib:format("~.6f",[ mc:now() ]),
    Line = mc:join_bin( <<" ">>, lists:map( fun( A ) -> mc:any_to_bin( A ) end,
                            [ mc:localtime(string), Now, ForLog#for_log.act, ForLog#for_log.id, ForLog#for_log.mes]
                    )),
    { ok, Line };

prepare_rlog( ForLog ) ->
    logger_sup:error( io_lib:format( "pid: ~p, Module: ~p, Unknown log type [into record ~p]\n", [ self(), ?MODULE, ForLog ]) ),
    error.

