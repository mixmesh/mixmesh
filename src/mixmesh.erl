-module(mixmesh).
-export([start/0, start_bootstrap/0]).
%%-export([export_public_key/0]).
%%-export([import_public_key/1, import_public_key/2]).
%%-export([list_public_keys/0]).
%%-export([emit_key_pair/0]).
-export([status/0]).

%%-include_lib("keydir/include/keydir_serv.hrl").

%% Exported: start

start() ->
    ensure_all_loaded(),
    ok = application:start(sasl),
    {ok,_} = application:ensure_all_started(ssl),
    {ok,_} = application:ensure_all_started(inets),
    ok = application:start(apptools),
    ok = application:start(tree_db),
    ok = application:start(xbus),
    ok = application:start(rester),
    ok = application:start(mixmesh), %% keydir use config!
    ok = application:start(keydir),
    ok = application:start(jsone),
    ok = application:start(nodis),
    ok = application:start(mail),
    ok = application:start(mpa),
    ok = application:start(elgamal),
    ok = application:start(enacl),
    ok = application:start(pgp),
    case config:lookup([gaia, enabled]) of
        true ->
            ok = application:start(gaia);
        false ->
            skip
    end,
    case config:lookup([player, enabled]) of
        true ->
            ok = application:start(player);
        false ->
            skip
    end,
    case config:lookup([system,hardware], none) of
	none ->
	    skip;
	pimesh ->
	    {ok, _} = application:ensure_all_started(pimesh);
	epxmesh ->
	    {ok, _} = application:ensure_all_started(pimesh)
    end,
    case config:lookup([simulator, enabled], false) of
        true ->
            {ok, _} = application:ensure_all_started(simulator);
        false ->
            skip
    end.

%% load applications needed for config schemas
ensure_all_loaded() ->
    ok = application:load(apptools),
    ok = application:load(keydir),
    ok = application:load(player),
    ok = application:load(mixmesh).
    %% application:load(simulator).

%% called from servator service
%% could basically print any thing
status() ->
    io:format("running\n", []).

%% Exported: start_bootstrap

start_bootstrap() ->
    ok = application:start(sasl),
    ok = application:start(apptools),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(inets),
    ok = application:start(tree_db),
    ok = application:start(xbus),
    ok = application:start(rester),
    ok = application:start(elgamal),
    ok = application:set_env(
           mixmesh, mode, bootstrap, [{persistent, true}]),
    ok = application:start(mixmesh).
