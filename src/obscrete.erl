-module(obscrete).
-export([start/0]).

start() ->
    ok = application:start(sasl),
    ok = application:start(obscrete),
    ok = application:start(pki),
    ok = application:start(player).
