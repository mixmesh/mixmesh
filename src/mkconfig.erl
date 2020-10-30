-module(mkconfig).
-export([start/1, start/3]).
-export([ensure_libs/3, return/2]).

%% This script creates the appropriate file structure needed to start
%% Obscrete. You call this command with an obscrete directory and the
%% name of a player as input, e.g. "./mkconfigdir /tmp/obscrete alice".
%% If you do this the following will be created:
%%
%% /tmp/obscrete/pki/data
%% /tmp/obscrete/alice/player/temp/
%% /tmp/obscrete/alice/player/buffer/
%% /tmp/obscrete/alice/player/pki/data/
%% /tmp/obscrete/alice/player/maildrop/spooler/
%% /tmp/obscrete/alice/player/ssl/
%%
%% As it happens this is the file structure used by the configuration
%% files under ./obscrete/etc/*.conf.

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").

-type mode() :: command | api.

%% Exported: start

-spec start([string()]) -> no_return().

start([ObscreteDir, SourceCertFilename, PlayerName]) ->
    start([ObscreteDir, SourceCertFilename, PlayerName], command).

-spec start(binary(), binary(), binary()) -> boolean().

start(ObscreteDir, SourceCertFilename, PlayerName) ->
    start([?b2l(ObscreteDir), ?b2l(SourceCertFilename), ?b2l(PlayerName)], api).

start([ObscreteDir, SourceCertFilename, PlayerName], Mode) ->
    PkiDataDir = filename:join([ObscreteDir, <<"pki">>, <<"data">>]),
    true = ensure_libs(Mode, [PkiDataDir], true),
    PlayerDir = filename:join([ObscreteDir, PlayerName, <<"player">>]),
    PlayerTempDir = filename:join([PlayerDir, "temp"]),
    PlayerBufferDir = filename:join([PlayerDir, "buffer"]),
    PlayerPkiDataDir = filename:join([PlayerDir, "pki", "data"]),
    PlayerMaildropSpoolerDir =
        filename:join([PlayerDir, "maildrop", "spooler"]),
    PlayerSSLDir = filename:join([PlayerDir, "ssl"]),
    true = ensure_libs(Mode, [PlayerTempDir,
                              PlayerBufferDir,
                              PlayerPkiDataDir,
                              PlayerMaildropSpoolerDir,
                              PlayerSSLDir],
                       true),
    TargetCertFilename = filename:join([PlayerDir, "ssl", "cert.pem"]),
    format(Mode, "Copies ~s to ~s\n", [SourceCertFilename, TargetCertFilename]),
    case file:copy(SourceCertFilename, TargetCertFilename) of
        {ok, _} ->
            return(Mode, 0);
        {error, Reason} ->
            format(Mode, standard_error, "~s: ~s\n",
                   [SourceCertFilename, file:format_error(Reason)]),
            return(Mode, 100)
    end.

%% Exported: ensure_libs

-spec ensure_libs(mode(), [binary()], boolean()) -> no_return() | boolean().

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

-spec return(mode(), integer()) -> no_return() | boolean().

return(api, 0) ->
    true;
return(api, _Status) ->
    false;
return(command, Status) ->
    erlang:halt(Status).
