-module( mycommon ).
-author( "ostrovok@gmail.com" ).

-import( ejson ).

-export([ socket_options/1, file_options/1 ]).
-export([ safe_send/2, json_decode/1, json_val/2, json_prepare/1, number_to_list/1 ]).
-export([ any_to_int/1, any_to_bin/1, any_to_bool/1, check_int/2, temp_file/0, for_send_prepare/2 ]).
-export([ random_string/0, random_string/1, cut_head_string/2 ]).
-export([ all_records/1, all_object_list/1, all_object/1, first_records/2, list_to_term/1 ]).
-export([ lists_keyfind/2 ]).
-export([ load_modul/1 ]).
-export([ now/0, localtime/0, localtime/1 ]).
-export([ join_bin/2, join_bin_trunc/2, list_to_bin/1, request_id/0, get_from_list/2, ceiling/1, floor/1 ]).


-define( TMP_DIR, "/tmp/var" ). % Директория для временных файлов

-include("records.hrl").


request_id() ->
    { MegaSecs, Secs, MicroSecs } = erlang:now(),
    erlang:atom_to_list(node()) ++ pid_to_list(self()) ++"-"++ integer_to_list( 1000000 * MegaSecs + Secs ) ++"-"++ integer_to_list(MicroSecs).

any_to_bool( Val ) when is_binary(Val) -> true;
any_to_bool( Val ) when is_number(Val), Val /= 0 -> true;
any_to_bool( Val ) when is_number(Val), Val == 0 -> false;
any_to_bool( Val ) when is_integer(Val), Val /= 0 -> true;
any_to_bool( Val ) when is_integer(Val), Val == 0 -> false;
any_to_bool( Val ) when is_float(Val)  -> any_to_bool(round(Val));
any_to_bool( Val ) when is_list(Val) -> any_to_bool(list_to_integer(Val));
any_to_bool( Val ) when undefined == Val -> false;
any_to_bool( Val ) when is_boolean( Val ) -> Val;
any_to_bool( _   ) -> true.

any_to_int( Val ) when is_binary(Val) -> list_to_integer(binary_to_list(Val));
any_to_int( Val ) when is_number(Val) -> round(Val);
any_to_int( Val ) when is_float(Val)  -> round(Val);
any_to_int( Val ) when is_list(Val) -> list_to_integer(Val);
any_to_int( Val ) when is_integer(Val) -> Val;
any_to_int( Val ) when undefined == Val -> 0;
any_to_int( Val ) when is_boolean( Val ), Val == false -> 0;
any_to_int( Val ) when is_boolean( Val ), Val == true -> 1;
any_to_int( Val ) -> Val.

any_to_bin( Val ) when Val == ""; Val == <<"">>; Val == []; Val == undefined -> <<"">>;
any_to_bin( Val ) when is_boolean( Val ) -> term_to_binary( Val );
any_to_bin( Val ) when is_float( Val ) -> any_to_bin(io_lib:format("~.6f",[Val]));
any_to_bin( Val ) when is_integer( Val ) -> erlang:integer_to_binary( Val );
%any_to_bin( Val ) when is_list( Val ) -> io_lib:format("~tp", [ Val ]);
any_to_bin( [] )  -> <<"">>;
any_to_bin( Val ) when is_list( Val ) -> list_to_binary( lists:flatten(Val) );
any_to_bin( Val ) when is_atom( Val ) -> term_to_binary( Val );
any_to_bin( Val ) when is_binary( Val ) -> Val;
any_to_bin( Val ) when is_bitstring( Val ) -> <<Val/bitstring>>;
%
%any_to_bin( [ Val | List ] ) when length(List) > 0 ->
%    Val1 = any_to_bin( [ Val ]),
%    Val2 = any_to_bin( List ),
%    <<Val1/binary, Val2/binary>>;
%any_to_bin( [{ Val }]) -> any_to_bin( { Val } );
%any_to_bin( Val ) when is_list( Val ) -> list_to_binary( Val );
%any_to_bin( { Val } ) -> ValIn = any_to_bin( Val ), <<"{", ValIn/binary, "}">>;

any_to_bin( Val ) -> Val.

lists_keyfind( [ Key | KeyList ], List ) ->
    case lists:keyfind( Key, 1, List ) of
        { Key, Val } -> Val;
        _ -> lists_keyfind( KeyList, List )
    end;
lists_keyfind( [], _ ) -> undefined;
lists_keyfind( Key, List ) -> lists_keyfind( [ Key ], List ).

get_from_list( Key, List ) ->
    ViewKey = join_bin_trunc("\", \"",  Key ),
    case lists_keyfind( Key, List ) of
        undefined -> { "", false, join_bin_trunc("", [ "\"",ViewKey, "\" found error."   ])};
        Val       -> { Val, true, join_bin_trunc("", [ "\"", ViewKey, "\" found success." ])}
    end.

for_send_prepare( Error, Body ) ->
    {[ { <<"error">>, Error }, { <<"body">>, Body } ]}.

random_string() -> random_string( 32 ).
random_string( Len ) ->
    Chrs = list_to_tuple( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" ),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

temp_file() -> "".

check_int( Val, undefined ) -> ceiling(Val);
check_int( Val, TimeOutIn ) when TimeOutIn < 1 -> ceiling(Val);
check_int( _Val, TimeOutIn ) -> ceiling(TimeOutIn).

safe_send( Pid, Mes ) when is_pid(Pid) -> Pid ! Mes;
safe_send( _, _ ) -> ok.

socket_options( What ) ->
	if
        What == outer -> [ Data ] = file_options('../include/internal_tcp_options.txt');
        What == inner -> [ Data ] = file_options('../include/internal_tcp_options.txt');
        true          -> Data = [], exit('no_port!')
    end,
	Data.

file_options( File ) ->
    case file:consult( File ) of
        {ok, Terms} -> Terms;
        {error, Reason} -> exit(Reason)
    end.

json_decode( Line ) ->
	try
        ejson:decode( any_to_bin(Line) )
    catch
        Class:Error ->
            d:err("module: ~p, function: json_decode. ~p:~p", [ ?MODULE, Class, Error ]),
            {[]}
	end.

json_val( Key, Tuple ) when is_tuple(Tuple), tuple_size(Tuple) == 2 ->
    case element( 1, Tuple ) of
        Key -> element( 2, Tuple );
        _ -> undefined
    end;
json_val( Key, Tuple ) when is_tuple(Tuple), tuple_size(Tuple) == 1 -> mycommon:json_val( Key, element( 1, Tuple ));
json_val( Key, Tuple ) when is_tuple(Tuple) -> mycommon:json_val( Key, element( 1, Tuple ));
json_val( Key, Tuple ) when is_list(Tuple) ->
    case proplists:get_value(Key, Tuple) of
        { Value } -> Value;
        [ Value ] -> Value;
        Value -> Value
    end;
json_val( _, Tuple ) -> Tuple.

is_string([]) -> true;
is_string([X|T]) -> is_integer(X) andalso X >= 0 andalso is_string(T);
is_string(_) -> false.

json_prepare( Tuple ) when is_tuple(Tuple) -> json_prepare_tuple(Tuple);

json_prepare( List ) when is_list(List) ->
    case is_string(List) of
        true -> list_to_binary( List );
        _    -> lists:map( fun(E) -> json_prepare( E ) end, List )
    end;
    
json_prepare( Value ) -> Value.

json_prepare_tuple( {} ) -> [{}];
json_prepare_tuple( Tuple ) when tuple_size(Tuple) == 1 -> [ json_prepare( element( 1, Tuple ) ) ];
json_prepare_tuple( Tuple ) when tuple_size(Tuple) == 2 -> [{ json_prepare(element( 1, Tuple )), json_prepare(element( 2, Tuple ))  }];
json_prepare_tuple( Tuple ) when tuple_size(Tuple) rem 2 == 1 -> json_prepare(erlang:tuple_to_list(Tuple));
json_prepare_tuple( Tuple ) -> json_prepare_tuple( 1, Tuple, [] ).

json_prepare_tuple( Num, Tuple, List ) when Num == tuple_size(Tuple); Num > tuple_size(Tuple) -> List;
json_prepare_tuple( Num, Tuple, List ) ->
    NextList = List ++ [{ json_prepare(element( Num, Tuple )), json_prepare(element( Num + 1, Tuple ))  }],
    NextNum = Num + 2,
    json_prepare_tuple( NextNum, Tuple, NextList ).


number_to_list(Num) -> number_to_list(Num, []).
number_to_list(0, List) -> List;
number_to_list(Num, List) -> number_to_list(Num div 10, [Num rem 10]++List).

cut_head_string(  Sym, List ) when is_list(Sym), is_list(List) -> t_cut_head_string( Sym, List );
cut_head_string( _Sym, List ) -> List.

t_cut_head_string(  Sym, List ) when length(Sym) < length(List) ->
    case  lists:sublist(List, length(Sym)) of
        Sym -> lists:sublist(List, length(Sym) + 1, length(List));
        _ -> t_cut_head_string( Sym, lists:sublist(List, 2, length(List)))
    end;
t_cut_head_string( _Sym, List ) -> List.


%
% ETS service function
%

all_records(  Table ) -> all_records( Table, [], ets:first( Table ) ).
all_records( _Table, A, '$end_of_table' ) -> A;
all_records(  Table, A, R ) -> all_records( Table, [ R | A ], ets:next(Table, R) ).

all_object( Table ) -> all_object( Table, [], ets:first( Table ) ).
all_object( _Table, A, '$end_of_table' ) -> A;
all_object( Table, A, R ) ->
	Z = ets:lookup(Table, R),
	all_object( Table, lists:append( Z, A ), ets:next(Table, R) ).

all_object_list( In ) -> all_object_list( In, [] ).
all_object_list( [], A ) -> A;
all_object_list( [ In | Next ], A ) ->
	all_object_list( Next, [ all_object( In ) | A ] ).

first_records( Table, Func ) -> first_records( Table, ets:first( Table ), Func ).
first_records( _Table, R, _Func ) when R == '$end_of_table' -> ets:first( rooms );
first_records( Table, R, Func ) ->
	[ Z ] = ets:lookup(Table, R),
	case Func(Z) of
		true -> R;
		_	 -> first_records( Table, ets:next(Table, R), Func )
	end.

list_to_term( String ) ->
    { ok, T, _ } = erl_scan:string( String ++ "." ),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            Term;
        {error, Error} ->
            Error
    end.

load_modul( Module ) ->
    case code:is_loaded( Module ) of
        false ->
            case code:load_file(Module) of
                { error, Error } -> { error, Error };
                _ -> true % Success loaded
            end;
        _ -> true % Already loaded
    end.

now() ->
    { MegaSecs, Secs, MicroSecs } = erlang:now(),
    MegaSecs * 1000000 + Secs + MicroSecs / 1000000.

localtime() ->
    {{ Year, Month, Day }, { Hour, Minute, Second }} = erlang:localtime(),
    [ Year, Month, Day, Hour, Minute, Second ].

localtime( _A ) -> lists:flatten(io_lib:format( "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", mycommon:localtime())).

join_bin( _D,  [] )   -> <<"">>;
join_bin( Del, List ) -> join_bin( Del, List, <<"">> ).

join_bin( _, [], Out ) -> Out;

join_bin( _D,  [ Elm ], Out ) when Out == ""; Out == <<>> -> <<Elm/binary>>;
join_bin( Del, [ Elm ], Out ) -> <<Out/binary, Del/binary, Elm/binary>>;
join_bin( Del, [ Elm | List ], Out ) when Out == ""; Out == <<>> ->
    case Elm of
        <<>>   -> join_bin( Del, List, <<Del/binary>> );
        ""     -> join_bin( Del, List, <<Del/binary>> );
        Elm    -> join_bin( Del, List, <<Elm/binary>> )
    end;
join_bin( Del,  [ Elm | List ], Out ) -> join_bin( Del, List, <<Out/binary, Del/binary, Elm/binary>> ).



join_bin_trunc( _, [] ) -> <<"">>;
join_bin_trunc( Del, [ <<"">> | List ] ) -> join_bin_trunc( any_to_bin(Del), List );
join_bin_trunc( Del, [ [] | List ] )     -> join_bin_trunc( any_to_bin(Del), List );
join_bin_trunc( Del, List )              -> join_bin_trunc( any_to_bin(Del), List, <<"">> ).

join_bin_trunc( Del, [ El | List ], Out ) ->
    ElBin = any_to_bin(El),
    case { ElBin, Out } of
        { <<"">>, <<"">> } -> join_bin_trunc( Del, List, <<"">> );
        { <<"">>, Out }    -> join_bin_trunc( Del, List, Out );
        { ElBin, <<"">> }  -> join_bin_trunc( Del, List, ElBin );
        { ElBin, Out }     -> join_bin_trunc( Del, List, <<Out/binary, Del/binary, ElBin/binary>> )
    end;
join_bin_trunc( _, [], Out ) -> any_to_bin( Out ).

list_to_bin( List ) -> list_to_bin( List, <<"">> ).
list_to_bin([ El | List ], Out ) ->
    ElBin = any_to_bin(El),
    list_to_bin( List, <<Out/binary, ElBin/binary>> );
list_to_bin([], Out ) -> Out.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

