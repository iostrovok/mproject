-module( httpd_server ).
-author( "ostrovok@gmail.com" ).

-include("records.hrl").
-include_lib("inets/include/httpd.hrl").
-import( main_act ).
-import( mc ).

-export([ do/1 ]).

do( ModData ) ->
    ?D( "do/1 ~p,~nMODUL: ~p,~nModData: ~p~n", [ self(), ?MODULE, ModData ]),
    ?D( "entity_body", ModData#mod.entity_body, "data", ModData#mod.data, "method", ModData#mod.method ),
    ?D( "absolute_uri", ModData#mod.absolute_uri,"request_line", ModData#mod.request_line,"request_uri", ModData#mod.request_uri),

    InitTime = mc:now(),
    ReqId = mc:request_id(),
    MyRequest = parse_query( ModData ),

    case lists:keyfind( "act", 1, MyRequest) of
        false ->
            ContentType = "text/plain",
            Act = "404_NOT_FOUND",
            StatusCode = 404,
            Body = "Act not found";
        { "act", Act } ->
            { ContentType, StatusCode, Body } = main_act:act( Act, MyRequest )
    end,
    ForLog = [{ <<"it">>, InitTime }, { <<"st">>, InitTime } , { <<"id">>, ReqId }, { <<"act">>, Act } ],
    my_send_replay( ReqId, StatusCode, ContentType, Body, ForLog ).

my_send_replay( ReqId, StatusCode, ContentType, BodyIn, ForLog ) ->
    %Body1 = re:replace(InBody, "\n", "<br />", [{return, list}]),
    %Body2 = re:replace( Body1, "\r", "<br />", [{return, list}]),
    %Body = re:replace( Body2, "~n", "<br />", [{return, list}]),
    %Body = re:replace( InBody, "\n|\s+"," ",[global, { return, list }]),
    %Body = re:replace( InBody, "\s+"," ",[global, { return, list }]),
    io:format("Body: ~p\n", [BodyIn]),
    Body = lists:flatten(BodyIn),
    io:format("Body: ~p\n", [Body]),
    Head = [
        { code, StatusCode },
        { cache_control, "max-age=0"},
        { content_encoding, "utf-8" },
        { content_type, ContentType ++"; charset=utf-8"},
        { content_length, integer_to_list(length(  Body )) }
        %{ content_length, integer_to_list(byte_size( Body )) }
    ],

    io:format("DDDDDD 0\n", []),

    logger_sup:access( ReqId, "http", "Success", ForLog, "send success" ),
    io:format("DDDDDD 1\n", []),
    { proceed, [{ response, { response, Head, Body }}]}.

parse_query( #mod{ request_uri = "/favicon.ico" } ) -> [];
parse_query( #mod{ method = "POST" } = ModData )-> httpd:parse_query( ModData#mod.entity_body );
parse_query( #mod{ method = "GET"  } = ModData )-> httpd:parse_query( mc:cut_head_string( "?", ModData#mod.request_uri ));
parse_query( _ModData  )-> [].





