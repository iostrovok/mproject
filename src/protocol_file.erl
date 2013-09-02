-module( protocol_file ).
-author( "ostrovok@gmail.com" ).

-import( mc ).

-export([ make/1 ]).

make( JSON ) ->
    FileName = mc:json_val( <<"file">>, JSON ),
    case file:read_file(FileName) of
        { ok, Bin }       -> mc:for_send_prepare( <<"">>, Bin );
        { error, Reason } -> mc:for_send_prepare( erlang:list_to_binary(Reason), <<"">> )
    end.
