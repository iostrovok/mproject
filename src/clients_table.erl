-module( clients_table ).
-author( "ostrovok@gmail.com" ).

-export([ ckey/1, ckey/2, rget/1, all_type_clients/1, insert/1, delete/1, delete/2, delete_object/1 ]).
-export([ sckey/1, srget/1, sinsert/1, sdelete/1, sdelete_object/1 ]).

-include("records.hrl").

ckey( Client ) -> ckey( Client#client.type, Client#client.client_id ).
ckey( Type, ClientId ) -> mc:list_to_term( "client_"++ Type ++"_"++ ClientId ) .

rget( Client )    ->
    case ets:lookup( clients, ckey( Client ) ) of
        [ A ] -> A;
        _ -> undefined
    end.

% clients_table:all_type_clients( "table" ).
all_type_clients( Type ) -> ets:match_object( clients, #client{ type = Type, _='_' } ).

insert( Client ) -> ets:insert( clients, Client ).
delete( Client ) -> ets:delete( clients, ckey( Client ) ).
delete( Type, ClientId ) -> ets:delete( clients, ckey( Type, ClientId ) ).
delete_object( Client ) -> ets:delete_object( clients, Client ).

sckey( Sender ) when is_record(Sender, client_sender) -> sckey( Sender#client_sender.type );
sckey( Type ) -> mc:list_to_term( "client_sender_"++ Type ).

srget( Sender ) ->
    case ets:lookup( clients_sender, sckey( Sender ) ) of
        [ A ] -> A;
        _ -> undefined
    end.

sinsert( Sender ) -> ets:insert( clients_sender, Sender ).

sdelete( Sender )  when is_record(Sender, client_sender) -> ets:delete( clients_sender, sckey( Sender ) );
sdelete( Type ) -> ets:delete( clients_sender, sckey( Type ) ).
sdelete_object( Sender ) -> ets:delete_object( clients_sender, Sender ).
