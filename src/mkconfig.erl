-module(mkconfig).
-export([start/1, start/3, create_player/4]).
-export([ensure_libs/3, return/2]).

%% This script creates the appropriate file structure needed to start
%% Obscrete. You call this command with an obscrete directory and the
%% name of a player as input, e.g. "./mkconfigdir /tmp/obscrete alice".
%% If you do this the following will be created:
%%
%% /tmp/obscrete/global-pki
%% /tmp/obscrete/alice/player/temp/
%% /tmp/obscrete/alice/player/buffer/
%% /tmp/obscrete/alice/player/local-pki/
%% /tmp/obscrete/alice/player/spooler/
%% /tmp/obscrete/alice/player/received-messages/
%% /tmp/obscrete/alice/player/ssl/

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").

-type return_mode() :: command | api.

%% Exported: start

-spec start([string()]) -> no_return().

start([ObscreteDir, SourceCertFilename, Nym]) ->
    GlobalPkiDir = filename:join([ObscreteDir, <<"global-pki">>]),
    true = ensure_libs(command, [GlobalPkiDir], true),
    create_player(ObscreteDir, SourceCertFilename, Nym, command),
    return(command, 0).

-spec start(binary(), binary(), binary()) -> boolean().

start(ObscreteDir, SourceCertFilename, Nym) ->
    GlobalPkiDir = filename:join([ObscreteDir, <<"global-pki">>]),
    true = ensure_libs(api, [GlobalPkiDir], true),
    create_player(?b2l(ObscreteDir), ?b2l(SourceCertFilename), ?b2l(Nym), api).

%% Exported: start

-spec create_player(string(), string(), string(), return_mode()) ->
          no_return() | boolean().

create_player(ObscreteDir, SourceCertFilename, Nym, Mode) ->
    PlayerDir = filename:join([ObscreteDir, Nym, <<"player">>]),
    TempDir = filename:join([PlayerDir, <<"temp">>]),
    BufferDir = filename:join([PlayerDir, <<"buffer">>]),
    LocalPkiDir = filename:join([PlayerDir, <<"local-pki">>]),
    SpoolerDir = filename:join([PlayerDir, <<"spooler">>]),
    ReceivedMessagesDir = filename:join([PlayerDir, <<"received-messages">>]),
    SSLDir = filename:join([PlayerDir, "ssl"]),
    true = ensure_libs(Mode, [TempDir, BufferDir, LocalPkiDir, SpoolerDir,
                              ReceivedMessagesDir, SSLDir],
                       true),
    TargetCertFilename = filename:join([SSLDir, <<"cert.pem">>]),
    format(Mode, "Copies ~s to ~s\n", [SourceCertFilename, TargetCertFilename]),
    case file:copy(SourceCertFilename, TargetCertFilename) of
        {ok, _} ->
            true;
        {error, Reason} ->
            format(Mode, standard_error, "~s: ~s\n",
                   [SourceCertFilename, file:format_error(Reason)]),
            return(Mode, 100)
    end.

%% Exported: ensure_libs

-spec ensure_libs(return_mode(), [binary()], boolean()) -> no_return() | boolean().

ensure_libs(_Mode, [], _Erase) ->
    true;
ensure_libs(Mode, [Dir|Rest], Erase) ->
    format(Mode, "Ensures ~s\n", [Dir]),
    case filelib:ensure_dir(Dir) of
        ok ->
            case file:make_dir(Dir) of
                ok ->
                    ensure_libs(Mode, Rest, Erase);
                {error, eexist} when Erase ->
                    ok = erase_dir(Mode, Dir),
                    ensure_libs(Mode, Rest, Erase);
                {error, eexist} ->
                    ensure_libs(Mode, Rest, Erase);
                {error, Reason} ->
                    format(Mode, standard_error, "~s: ~s\n",
                           [Dir, file:format_error(Reason)]),
                    return(Mode, 100)
            end;
        {error, Reason} ->
            format(Mode, standard_error, "~s: ~s\n",
                   [Dir, file:format_error(Reason)]),
            return(Mode, 200)
    end.

erase_dir(Mode, Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    lists:foreach(
      fun(Filename) ->
              format(Mode, "Deletes ~s\n", [filename:join([Dir, Filename])]),
              case filename:join([Dir, Filename]) of
                  <<"/tmp/obscrete", _/binary>> = AbsoluteFilename ->
                      file:delete(AbsoluteFilename);
                  _ ->
                      yellow
              end
      end, Filenames),
    ok.

format(api, Format, Args) ->
    ?daemon_log_tag_fmt(system, Format, Args);
format(command, Format, Args) ->
    io:format(Format, Args).

format(api, _Device, Format, Args) ->
    ?daemon_log_tag_fmt(system, Format, Args);
format(command, Device, Format, Args) ->
    io:format(Device, Format, Args).

%% Exported: return

-spec return(return_mode(), integer()) -> no_return() | boolean().

return(api, 0) ->
    true;
return(api, _Status) ->
    false;
return(command, Status) ->
    erlang:halt(Status).
