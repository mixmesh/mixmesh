-module(obscrete_command).
-export([digest_password/1, elgamal_keys/1, pin_salt/0]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("elgamal/include/elgamal.hrl").

%% Exported: digest_password

digest_password([Password]) ->
    io:format(
      "~s\n", [base64:encode(player_crypto:digest_password(Password))]),
    erlang:halt(0).

%% Exported: elgamal_keys

elgamal_keys([Pin, PinSalt, Nym]) ->
    case {length(Pin), lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Pin)} of
        _ when length(Nym) > ?MAX_NYM_SIZE ->
            io:format(standard_error,
                      "Error: A NYM must at most contain ~w characters\n", 
		      [?MAX_NYM_SIZE]),
            erlang:halt(100);
        {PinLen, _} when PinLen /= 6 ->
            io:format(standard_error,
                      "Error: A PIN must contain six digits\n", []),
            erlang:halt(100);
        {_PinLen, false} ->
            io:format(standard_error,
                      "Error: A PIN must only contain digits\n", []),
            erlang:halt(100);
        _ ->
            DecodedPinSalt =
                try
                    base64:decode(PinSalt)
                catch
                    _:_ ->
                        not_base64
                end,
            case DecodedPinSalt of
                not_base64 ->
                    io:format(standard_error,
                              "Error: PIN salt is not BASE64 encoded\n", []),
                    erlang:halt(100);
                _ ->
                    SharedKey =
                        enacl:pwhash(?l2b(Pin), DecodedPinSalt,
                                     enacl:secretbox_KEYBYTES()),
                    {PublicKey, EncryptedSecretKey} =
                        player_crypto:encrypt_new_key_pair(
                          SharedKey, ?l2b(Nym)),
                    io:format("-----BEGIN SECRET KEY-----\n"),
                    io:format("~s\n", [base64:encode(EncryptedSecretKey)]),
                    io:format("-----END SECRET KEY-----\n"),
                    io:format("-----BEGIN PUBLIC KEY-----\n"),
                    io:format("~s\n", [base64:encode(PublicKey)]),
                    io:format("-----END PUBLIC KEY-----\n"),
                    erlang:halt(0)
            end
    end.

%% Exported: pin_salt

pin_salt() ->
    io:format("~s\n", [base64:encode(player_crypto:pin_salt())]),
    erlang:halt(0).
