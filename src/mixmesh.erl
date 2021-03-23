-module(mixmesh).
-export([start/0, start_bootstrap/0]).
-export([export_public_key/0]).
-export([import_public_key/1, import_public_key/2]).
-export([list_public_keys/0]).
%%-export([emit_key_pair/0]).
-export([status/0]).

-include_lib("keydir/include/keydir_serv.hrl").

%% Exported: start

start() ->
    ensure_all_loaded(),
    ok = application:start(sasl),
    {ok,_} = application:ensure_all_started(ssl),
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
    ok = application:start(tree_db),
    ok = application:start(xbus),
    ok = application:start(rester),
    ok = application:start(elgamal),
    ok = application:set_env(
           mixmesh, mode, bootstrap, [{persistent, true}]),
    ok = application:start(mixmesh).

%% Utilities

%% FIXME: Remove the utilities below and put them out of the way

%% Exported: list_public_keys

list_public_keys() ->
    ets:foldl(
      fun(#keydir_user{nym=Nym,public_key=Pk}, _Acc) ->
	      MD5 = crypto:hash(md5, elgamal:public_key_to_binary(Pk)),
	      Fs = [tl(integer_to_list(B+16#100,16)) || <<B>> <= MD5],
	      io:format("~16s -- ~s\n", [Nym, string:join(Fs, ":")])
      end, ok, keydir_db).

%% Exported: export_public_key

%% export a public key in a format useful for keydir
export_public_key() ->
    case config:lookup([player, enabled]) of
	true ->
	    Nym = config:lookup([player, username]),
	    case config:lookup([player, routing, 'public-key'], false) of
		false ->
		    false;
		Pk ->
		    KeydirUser =
                        #keydir_user{
                           nym=Nym,
                           password=(<<"">>),
                           email=(<<"">>),
                           public_key=elgamal:binary_to_public_key(Pk)},
		    <<"Keydir:",
		      (base64:encode(term_to_binary(KeydirUser)))/binary>>
	    end;
	false ->
	    false
    end.

%% Exported: import_public_key

import_public_key(Key) ->
    import_public_key(Key, undefined).

import_public_key(<<"Keydir:",KeydirUser64/binary>>, Nym) when
      is_binary(Nym); Nym =:= undefined ->
    KeydirUserBin = base64:decode(KeydirUser64),
    KeydirUser0 = binary_to_term(KeydirUserBin),
    KeydirUser = if Nym =:= undefined -> KeydirUser0;
		 true -> KeydirUser0#keydir_user{nym=Nym}
                 end,
    case keydir_serv:create(KeydirUser) of
	{error, user_already_exists} ->
	    keydir_serv:update(KeydirUser);
	Res ->
	    Res
    end.
