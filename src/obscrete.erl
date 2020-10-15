-module(obscrete).
-export([start/0, salt_password/1]).

%% Exported: start

start() ->
    ok = application:start(sasl),
    ok = ssl:start(),
    ok = application:start(obscrete),
    ok = application:start(pki),
    case config:lookup([player, enabled]) of
        true ->
            ok = application:start(player);
        false ->
            skip
    end,
    case config:lookup([simulator, enabled]) of
        true ->
            {ok, _} = application:ensure_all_started(simulator);
        false ->
            skip
    end.

%% Exported: salt_password

salt_password(Password) ->
    io:format("~s\n", [player_password:salt(Password)]),
    erlang:halt(0).
