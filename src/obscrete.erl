-module(obscrete).
-export([start/0]).

start() ->
    ok = application:start(sasl),
    ok = application:start(obscrete),
    ok = application:start(pki),
    case config:lookup([player, enabled]) of
        true ->
            ok = application:start(player);
        false ->
            skip
    end.
