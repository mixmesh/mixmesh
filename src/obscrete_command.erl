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
    DecodedPinSalt =
        try
            base64:decode(PinSalt)
        catch
            _:_ ->
                not_base64
        end,
    case DecodedPinSalt of
        not_base64 ->
            io:format(standard_error, "Invalid pin salt", []),
            erlang:halt(100);
        _ ->
            case player_crypto:make_key_pair(Pin, DecodedPinSalt, Nym) of
                {ok, PublicKey, EncryptedSecretKey} ->
                    io:format("-----BEGIN SECRET KEY-----\n"),
                    io:format("~s\n", [base64:encode(EncryptedSecretKey)]),
                    io:format("-----END SECRET KEY-----\n"),
                    io:format("-----BEGIN PUBLIC KEY-----\n"),
                    io:format("~s\n", [base64:encode(PublicKey)]),
                    io:format("-----END PUBLIC KEY-----\n"),
                    erlang:halt(0);
                {error, Reason} ->
                    io:format(standard_error, Reason ++ "\n", []),
                    erlang:halt(100)
            end
    end.

%% Exported: pin_salt

pin_salt() ->
    io:format("~s\n", [base64:encode(player_crypto:pin_salt())]),
    erlang:halt(0).
