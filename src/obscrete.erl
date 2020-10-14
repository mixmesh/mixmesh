-module(obscrete).
-export([start/0]).
-export([export_public_key/0,
	 import_public_key/1, import_public_key/2]).
-export([emit_key_pair/0]).

-include_lib("pki/include/pki_serv.hrl").

start() ->
    ok = application:start(sasl),
    ok = application:start(obscrete),
    ok = application:start(pki),
    case config:lookup([player, enabled]) of
        true ->
            ok = application:start(player);
        false ->
            skip
    end,
    case config:lookup([simulator, enabled], false) of
        true ->
            {ok, _} = application:ensure_all_started(simulator);
        false ->
            skip
    end.

%% utility

%% Convenience function - print new keypair in config format
emit_key_pair() ->
    {Pk,Sk} = belgamal:generate_key_pair(),
    io:format("\"public-key\": \"~s\",\n", [Pk]),
    io:format("\"secret-key\": \"~s\",\n", [Sk]).

%% export a public key in a format useful for pki
export_public_key() ->
    case config:lookup([player, enabled]) of
	true ->
	    Name = config:lookup([player, username]),
	    case config:lookup([player, spiridon, 'public-key'], false) of
		false ->
		    false;
		Pk ->
		    PkiUser = #pki_user{name=Name,
					password=(<<"">>),
					email=(<<"">>),
					public_key=Pk},
		    <<"PKI:",
		      (base64:encode(term_to_binary(PkiUser)))/binary>>
	    end;
	false ->
	    false
    end.

import_public_key(Key) ->
    import_public_key(Key, undefined).

import_public_key(<<"PKI:",PkiUser64/binary>>, Name) when
      is_binary(Name); Name =:= undefined ->
    PkiUserBin = base64:decode(PkiUser64),
    PkiUser0 = binary_to_term(PkiUserBin),
    PkiUser = if Name =:= undefined -> PkiUser0;
		 true -> PkiUser0#pki_user{name=Name}
	      end,
    case pki_serv:create(PkiUser) of
	{error, user_already_exists} ->
	    pki_serv:update(PkiUser);
	Res ->
	    Res
    end.
