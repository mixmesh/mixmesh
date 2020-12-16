-module(obscrete).
-export([start/0, start_bootstrap/0]).
-export([export_public_key/0]).
-export([import_public_key/1, import_public_key/2]).
-export([list_public_keys/0]).
%%-export([emit_key_pair/0]).
-export([status/0]).

-include_lib("pki/include/pki_serv.hrl").

%% Exported: start

start() ->
    ensure_all_loaded(),
    ok = application:start(sasl),
    {ok,_} = application:ensure_all_started(ssl),
    ok = application:start(apptools),
    ok = application:start(tree_db),
    ok = application:start(xbus),
    ok = application:start(rester),
    ok = application:start(obscrete), %% pki use config!
    ok = application:start(pki),
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
	    true = code:add_path(filename:join([code:lib_dir(obscrete),
						"pimesh", "ebin"])),
	    ok = application:start(pimesh)
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
    ok = application:load(pki),
    ok = application:load(player),
    ok = application:load(obscrete).
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
    ok = application:start(rester),
    ok = application:start(elgamal),
    ok = application:set_env(
           obscrete, mode, bootstrap, [{persistent, true}]),
    ok = application:start(obscrete).

%% Utilities

%% FIXME: Remove the utilities below and put them out of the way

%% Exported: list_public_keys

list_public_keys() ->
    ets:foldl(
      fun(#pki_user{nym=Nym,public_key=Pk}, _Acc) ->
	      MD5 = crypto:hash(md5, elgamal:public_key_to_binary(Pk)),
	      Fs = [tl(integer_to_list(B+16#100,16)) || <<B>> <= MD5],
	      io:format("~16s -- ~s\n", [Nym, string:join(Fs, ":")])
      end, ok, pki_db).

%% Exported: export_public_key

%% export a public key in a format useful for pki
export_public_key() ->
    case config:lookup([player, enabled]) of
	true ->
	    Nym = config:lookup([player, username]),
	    case config:lookup([player, routing, 'public-key'], false) of
		false ->
		    false;
		Pk ->
		    PkiUser = #pki_user{nym=Nym,
					password=(<<"">>),
					email=(<<"">>),
					public_key=elgamal:binary_to_public_key(Pk)},
		    <<"PKI:",
		      (base64:encode(term_to_binary(PkiUser)))/binary>>
	    end;
	false ->
	    false
    end.

%% Exported: import_public_key

import_public_key(Key) ->
    import_public_key(Key, undefined).

import_public_key(<<"PKI:",PkiUser64/binary>>, Nym) when
      is_binary(Nym); Nym =:= undefined ->
    PkiUserBin = base64:decode(PkiUser64),
    PkiUser0 = binary_to_term(PkiUserBin),
    PkiUser = if Nym =:= undefined -> PkiUser0;
		 true -> PkiUser0#pki_user{nym=Nym}
	      end,
    case pki_serv:create(PkiUser) of
	{error, user_already_exists} ->
	    pki_serv:update(PkiUser);
	Res ->
	    Res
    end.
