-module( protocol_http ).
-author( "ostrovok@gmail.com" ).

-export([ make/1, send_request/2, check_protocol/2, init_protocol/2, execute_protocol/2 ]).

-include("records.hrl").

-record( p, {
        http = "http",
        port = 80,
        timeout = 500,
        no_encode = true,
        ssl = false,
        ctype = "",
        host = "",
        path = "",
        how = "GET",
        headers = []
    }).

make( JSON ) ->
    Host = mc:json_val( <<"host">>, JSON ),
    Path = mc:json_val( <<"path">>, JSON ),
    _Params = mc:json_val( <<"params">>, JSON ),
    Url = erlang:binary_to_list(<<"http://", Host/binary, Path/binary>>),
    Type = mc:json_val( <<"type">>, JSON ),
    TimeOutIn = mc:any_to_int(mc:json_val( <<"timeout">>, JSON )),

    if
        TimeOutIn < 1 -> TimeOut = 500;
        TimeOutIn > 0 -> TimeOut = TimeOutIn
    end,

    case send_http_request( Type, Url, TimeOut ) of
        { _, Result } -> convert_http_result( Result );
        _             -> for_send_prepare( <<"failed_connect">>, <<"">>, <<"">>, <<"">> )
    end.

check_protocol( Data, TimeOut ) ->
    { Out, _State } = init_protocol( Data, TimeOut ),
    Out.

bin_to_list( Val ) when is_binary( Val ) -> erlang:binary_to_list( Val );
bin_to_list( Val ) -> Val.

init_protocol( Data, DefTimeOut ) ->
    ?D("init_protocol - 1. ", [ Data, DefTimeOut ]),
    BoolFuncNE = fun( Val, Rec ) -> { true, Rec#p{ no_encode = mc:any_to_bool( Val ) }} end,
    BoolFuncS  = fun( Val, Rec ) -> { true, Rec#p{ ssl = mc:any_to_bool( Val ) }} end,
    SimpleFuncHost = fun( Val, Rec ) ->
        case Val of
            undefined -> { false, "Bad host" };
            _ -> { true, Rec#p{ host = bin_to_list( Val ) }}
        end
    end,
    SimpleFuncPath = fun( Val, Rec ) ->
        case Val of
            undefined -> { false, "Bad path. " };
            _ -> { true, Rec#p{ path = bin_to_list( Val ) }}
        end
    end,
    HowFunc = fun( Val, Rec ) ->
        case Val of
            "POST" -> { true, Rec#p{ how = "POST" }};
            _ -> { true, Rec#p{ how = "GET" }}
        end
    end,
    HeadersFunc = fun( Val, Rec ) ->
        case Val of
            undefined -> { true, Rec#p{ headers = [] }};
            _ ->         { true, Rec#p{ headers = Val }}
        end
    end,

    TimeOutFunc = fun( Val, Rec ) -> { true, Rec#p{ timeout = mc:check_int( DefTimeOut, mc:any_to_int( Val )) }} end,
    PortFunc = fun( Val, Rec ) -> { true, Rec#p{ port = check_port( mc:any_to_int( Val )) }} end,
    CTypeFunc = fun( Val, Rec ) -> { true, Rec#p{ ctype = content_type( Rec#p.how, Val ) }} end,

    ChekList = [
        { "host", SimpleFuncHost  },
        { "path", SimpleFuncPath  },
        { "how", HowFunc  },
        { "timeout", TimeOutFunc  },
        { "no_encode", BoolFuncNE  },
        { "ssl", BoolFuncS  },
        { "port", PortFunc  },
        { "ctype", CTypeFunc },
        { "headers", HeadersFunc }
    ],

    init_protocol_error( #p{}, ChekList, Data, true, "" ).

get_prot_keys( Key ) -> [ list_to_binary("protocol."++ Key), "protocol."++ Key , list_to_binary(Key), Key].

init_protocol_error( State, [ { Key, Func  } | List ], Data, IsError, Mes ) when IsError == true ->
    %?D( "init_protocol_error 1 ", Key ),
    KeyList = get_prot_keys( Key ),
    Val = mc:lists_keyfind( KeyList, Data ),
    case Func( Val, State ) of
        { true, NewRec } ->
            NewMes = mc:join_bin_trunc(<<"">>, [ Mes, <<" Protokol success for key \"">>, lists:nth( 1, KeyList ), <<"\".">> ]),
            init_protocol_error( NewRec, List, Data, true, NewMes );
        { false, Error } ->
            { error, mc:join_bin_trunc("", [ Mes, " Protokol error \"", Error, "\", for key \"", Key, "\"."]) }
    end;

init_protocol_error( State, [], _Data, IsError, _Message ) when IsError == true ->
    { success, State };
init_protocol_error( _State, _L, _Data, IsError, Message ) when IsError /= true ->
    { error, list_to_binary(Message) }.

execute_protocol( State, Data ) when is_record( State, p )  ->
    Url = get_url( "http", State#p.host, State#p.port, State#p.path ),
    if
        State#p.no_encode == false -> SendData = http_uri:encode( Data );
        State#p.no_encode == true  -> SendData = Data
    end,

    inets:start(),
    case msend_http_request( "http", State#p.how, Url, State#p.ctype, State#p.timeout, State#p.headers, SendData ) of
        { _, Result } -> { success, convert_http_result( Result ) };
        _             -> { error, for_send_prepare( <<"failed_connect">>, <<"">>, <<"">>, <<"">> )}
    end;

execute_protocol( _State, _Data ) -> { error, "error protocol data" }.

send_request( Protocol, Data ) ->
    case mc:lists_keyfind([ "type", "protocol.type" ], Protocol ) of
        "https" -> HttpsOrNot = "https";
        _ ->  HttpsOrNot = "http"
    end,

    Host = mc:lists_keyfind(["protocol.host"], Protocol ),
    Path = check_path(mc:lists_keyfind(["protocol.path"], Protocol )),

    Type = mc:lists_keyfind(["protocol.how"], Protocol ),

    CType = content_type( Type, mc:lists_keyfind(["protocol.ctype"], Protocol )),
    NoEncode = mc:any_to_bool( mc:lists_keyfind([ "protocol.no_encode"], Protocol )) ,

    Port = check_port( mc:any_to_int( mc:lists_keyfind([ "protocol.port" ], Protocol ))),

    Url = get_url( HttpsOrNot, Host, Port, Path ),

    TimeOutIn = mc:any_to_int( mc:lists_keyfind( [ "protocol.timeout" ], Protocol )),

    if
        TimeOutIn < 1 -> TimeOut = 500;
        TimeOutIn > 0 -> TimeOut = TimeOutIn
    end,

    if
        NoEncode == false -> SendData = http_uri:encode( Data );
        NoEncode == true  -> SendData = Data
    end,

    inets:start(),
    msend_http_request( HttpsOrNot, Type, Url, CType, TimeOut, [], SendData ).

check_port( P ) when P > 65536; P < 1 -> 80;
check_port( P ) -> P.

check_path( P ) ->
    case lists:sublist( P, 1, 1) of
        "/" -> P;
        _ -> "/" ++ P
    end.

get_url( HttpsOrNot, Host, 80, Path ) -> HttpsOrNot ++ "://" ++ Host ++ Path;
get_url( HttpsOrNot, Host, Port, Path ) -> HttpsOrNot ++ "://" ++ Host ++ ":" ++ integer_to_list( Port ) ++ Path.

msend_http_request( "http", "GET", InUrl, _ContentType, TimeOut, Headers, Data ) ->
    Url = bin_to_list(InUrl) ++ "?" ++ bin_to_list(Data),
    HTTPOptions = [ { timeout, round( TimeOut ) }, { url_encode, false }  ],
    Options = [ { sync, true }, { stream, none }, { body_format, binary  }, { full_result, true }, { headers_as_is, true } ],
    httpc:request( get, { Url, Headers }, HTTPOptions, Options );

msend_http_request( "http", "POST", Url, ContentType, TimeOut, Headers, Body ) ->
    HTTPOptions = [ { timeout, TimeOut }, { url_encode, false } ],
    Options = [ { sync, true }, { stream, none }, { body_format, binary  }, { full_result, true }, { headers_as_is, true } ],
    httpc:request( post, {Url, Headers, ContentType, Body}, HTTPOptions, Options);

msend_http_request( _, _, _Url, _CType, _TimeOut, _Headers, _Body ) -> ok.

%    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, Body}} = R.

send_http_request( Type, Url, TimeOut ) ->
    inets:start(),
    case Type of
        <<"POST">> ->
            httpc:request(post, {
                    Url, [], "application/x-www-form-urlencoded", "hl=en&q=erlang&btnG=Google+Search&meta="
                }, [], []);
        _ ->
            httpc:request(get, {Url, []}, [
                    { timeout, TimeOut }, { url_encode, false }
                ], [])
    end.

convert_http_result( Result ) ->
    case Result of
        { failed_connect, _ }         -> for_send_prepare( <<"failed_connect">>, <<"">>, <<"">>, <<"">> );
        { failed_connect, _, _ }      -> for_send_prepare( <<"failed_connect">>, <<"">>, <<"">>, <<"">> );
        { StatusLine, Headers, Body } -> for_send( StatusLine, Headers, Body );
        { StatusLine, Body }          -> for_send( StatusLine, [], Body );
        _                             -> for_send_prepare( <<"unknow_error">>, <<"">>, <<"">>, <<"">> )
    end.

for_send_prepare( Error, StatusLine, Headers, Body ) ->
    {[
        { <<"error">>, Error },
        { <<"status_line">>, StatusLine },
        { <<"headers">>, Headers },
        { <<"body">>, Body }
    ]}.

for_send( StatusLine, Headers, Body ) -> for_send_prepare( <<"">>, status_line(StatusLine), { headers(Headers) }, mc:any_to_bin( Body ) ).

status_line({ Protocol, Number, Result }) -> [ erlang:list_to_binary(Protocol), Number, erlang:list_to_binary(Result)];
status_line(A) -> A.

headers( Headers ) ->
    lists:map(
        fun({ Key, Val }) ->
            { mc:any_to_bin(Key), mc:any_to_bin(Val)}
        end,
    Headers ).


content_type( "POST", CType ) when CType == ""; CType ==  undefined -> "application/x-www-form-urlencoded";
content_type( "GET",  CType ) when CType == ""; CType ==  undefined -> "";
content_type( _,      CType ) when CType == ""; CType ==  undefined -> "";
content_type( _,      CType ) -> CType.
