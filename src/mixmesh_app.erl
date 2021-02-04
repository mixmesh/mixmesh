-module(mixmesh_app).
-behaviour(application).
-export([start/2, stop/1]).

%% Exported: start

start(_Type, StartArgs) ->
    case mixmesh_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% Exported: stop

stop(_State) ->
    ok.
