-module( mproxy_client_http ).
-author( "ostrovok@gmail.com" ).
-behavior(gen_server).

-export([ start_link/0, stop/0 ]).
-export([ init/1, terminate/2, handle_call/3, handle_call/2, handle_cast/2, code_change/3, handle_info/2 ]).
-export([ message/1, one_worker/3 ]).

-include("records.hrl").
-import( mc ).

-define( RELOAD_TIME, 180000 ). % By default 10 second
-record( state, {} ).

code_change(_, _, _) -> ok.
terminate( _, _)     -> ok.
stop()               -> ok.

% INIT Functions
start_link() -> gen_server:start_link( ?MODULE, [], []).
init([]) -> process_flag( trap_exit, true ), { ok, #state{}}.

% CALLBACK Functions
handle_call( { request, MyRequest }, _, State ) -> { reply, message( MyRequest ), State }.
handle_call( { request, MyRequest }, State )    -> { reply, message( MyRequest ), State };
handle_call( _, State )    -> { reply, <<"error data">>, State }.

handle_cast( stop, State)       -> { stop, normal, State };
handle_cast( _, State)          -> { noreply, State }.

handle_info( _, State)          -> { noreply, State }.


get_subreq( List ) when is_list(List) -> List;
get_subreq( List ) when is_tuple(List) -> [ List ];
get_subreq( _ ) -> [].


message( InData ) ->
    DefaultTimout = max( 1, mc:any_to_int( config_table:rget( mproxy_client_http, timeout ))),
    { TimeOut, _MessageTimeOut } = client_common:get_timeout( InData, "timeout", DefaultTimout ),

    {  Data, _IsData, _MessageData } = mc:get_from_list([ "data" ], InData ),

    JSON = mc:json_decode(Data),
    ?D( "message", "JSON", JSON ),
    InList = mc:json_val( <<"data">>, JSON ),
    List = get_subreq( InList ),
    CountReqs = length(List),

    lists:map(
            fun( X ) -> spawn( ?MODULE, one_worker, [ self(), X, DefaultTimout  ] ) end,
        List ),

    mloop( [], CountReqs, TimeOut ).

mloop( Out, Count, TimeOut ) when Count < 1; TimeOut < 1 -> prepare_out( Out );
mloop( Out, Count, InTimeOut ) when Count > 0, InTimeOut >= 1 ->
    StartTime = mc:now(),
    TimeOut = round(InTimeOut),
	receive
		NewOut ->
            NewTimeOut = TimeOut - ( mc:now() - StartTime ),
            NewCount = Count - 1,
            case Out of
                [] -> mloop( [{ NewOut }], NewCount, NewTimeOut );
                Out -> mloop( [{ NewOut } | Out ], NewCount, NewTimeOut )
            end
	after
        TimeOut -> prepare_out([ {[{ <<"error">>, <<"my time out">> }]} | Out ])
	end;
mloop( Out, _Count, _TimeOut ) -> prepare_out( Out ).

prepare_out([ Result ]) -> prepare_out( Result );
prepare_out({ Result }) -> prepare_out( Result );
prepare_out( Result )   -> ejson:encode( Result ).

%A = [{[{<<"error">>, badarg}, { <<"debug">>, <<"one_worker 1">> }]}, {[{<<"error">>, badarg}, {<<"debug">>, <<"one_worker 1">>}]}].
%ejson:encode({ A }).

one_worker( ParentPid, Data, DefaultTimout ) when is_pid(ParentPid), is_list(Data), is_number(DefaultTimout) ->
	try
        one_worker_in( ParentPid, Data, DefaultTimout )
	catch
        Class:Error ->
            d:err("module: ~p, function: one_worker. Error: ~p:~p", [ ?MODULE, Class, Error ]),
            ParentPid ! [{ <<"error">>, Error }, { <<"debug">>, <<"one_worker 1">> }]
	end;

one_worker( ParentPid, { Data }, DefaultTimout )  when is_pid(ParentPid), is_list(Data), is_number(DefaultTimout) ->
    one_worker( ParentPid, Data, DefaultTimout );

one_worker( ParentPid, Data, DefaultTimout ) ->
    d:err("~p:one_worker. Error args: ~p\n", [ ?MODULE, [ ParentPid, Data, DefaultTimout ] ]),
    ParentPid ! [{ <<"error">>, <<"Internal error">> }, { <<"debug">>, <<"one_worker 2">> }].

one_worker_in( ParentPid, Data, DefaultTimout ) ->
    { SendData, _IsData, MessageData } = mc:get_from_list([ <<"data">> ], Data ),
    { TimeOut, MessageTimeOut } = client_common:get_timeout( Data, "timeout", DefaultTimout ),
    { Id, IsId, MessageId } = mc:get_from_list([ <<"id">> ], Data ),
    { Module, Protocol, IsProtocol, MessageProtocol } = client_common:init_protocol( Data, TimeOut ),

    case IsProtocol and IsId of
        false ->
            Error = mc:join_bin_trunc(" ", [ "id:", Id, MessageProtocol, MessageId, MessageTimeOut, MessageData ]),
            ParentPid ! convert_result( mc:any_to_bin(Id), <<"error">>, <<"\"", Error/binary ,"\"">>  );
        true ->
            case client_common:execute_protocol( Module, Protocol, SendData ) of
                { error, Error } -> ParentPid ! convert_result( Id, <<"error">>, Error );
                { success, Mes } -> ParentPid ! convert_result( Id, <<"success">>, Mes )
            end
    end.

convert_result( Id, Error, Mes ) -> [{ <<"id">>, Id }, { <<"error">>, Error }, { <<"body">>, Mes }].


