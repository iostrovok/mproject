-module( config_table ).
-author( "ostrovok@gmail.com" ).

-export([ rget/2, insert/3, all/0, type_config/1, init_defaul_data/2 ]).

-include("records.hrl").

% ets:match( mconfig, { http_viewer, lisent_port, '$1' } ).
%  ets:match( mconfig, { http_viewer, '_', '_' } ).
% ets:match( mconfig, { http_viewer, '$1', '$2' } )

type_config( Type ) ->
    lists:map (
        fun([ Key, Value ]) -> { Key, Value } end,
        ets:match( mconfig, { Type, '$1', '$2' } )
    ).

all() -> ets:match_object( mconfig, { '_', '_', '_' } ).

rget( Type, Key )    ->
    case ets:match( mconfig, { Type, Key, '$1' } ) of
        [[ A ]] -> A;
        _ -> undefined
    end.

insert( Type, Key, Value ) -> ets:insert( mconfig, { Type, Key, Value } ).

init_defaul_data( Type, Config ) ->
    lists:map(
            fun({ Key, Value }) ->
                case rget( Type, Key ) of
                    undefined -> insert( http_viewer, Key, Value );
                    _ -> ok
                end
            end,
        Config ).

