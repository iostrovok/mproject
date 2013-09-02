-module( d ).
-author( "ostrovok@gmail.com" ).

-export([ d/1, d/2, log/1, err/1, err/2, debug_fl/1 ]).

-include("records.hrl").
-define( DEBUG, true ).

d( Format ) -> print( Format, [] ).
d( Format, Data ) -> print( Format, Data ).

print( Format, Data ) ->
    try
        case ?DEBUG of
            true -> io:format( Format, Data );
            _A -> ok
        end
    catch
        Class:Error -> err("module: ~p, function: print. ~p:~p for format: ~p", [ ?MODULE, Class, Error, Format ])
    end.

log( ForLog ) -> logger_sup:log( ForLog ).
err( Error )  -> logger_sup:error( Error ).
err( Format, Data )  -> logger_sup:error( io_lib:format( Format, Data ) ).

debug_fl( Count ) -> debug_fl( Count, "" ).
debug_fl( 0, Out ) -> "~p. ~s. ~w. DEBUG: "++ Out ++"\n";
debug_fl( Count, Out ) -> debug_fl( Count - 1, Out ++ " ~p" ).