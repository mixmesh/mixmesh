-module(mkconfig).
-export([start/1, start/3, create_player/4, ensure_libs/3]).

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

-type trace_mode() :: stdout | log.

%% Exported: start

-spec start([string()]) -> no_return().

start([ObscreteDir, SourceCertFilename, Nym]) ->
    GlobalPkiDir = filename:join([ObscreteDir, <<"global-pki">>]),
    try
        true = ensure_libs(stdout, [GlobalPkiDir], true),
        true = create_player(stdout, ObscreteDir, SourceCertFilename, Nym),
        erlang:halt(0)
    catch
        throw:{status, Status} ->
            erlang:halt(Status)
    end.

-spec start(binary(), binary(), binary()) -> boolean().

start(ObscreteDir, SourceCertFilename, Nym) ->
    GlobalPkiDir = filename:join([ObscreteDir, <<"global-pki">>]),
    try
        true = ensure_libs(log, [GlobalPkiDir], true),
        create_player(log, ?b2l(ObscreteDir), ?b2l(SourceCertFilename),
                      ?b2l(Nym))
    catch
        throw:{status, _Status} ->
            false
    end.

%% Exported: create_player

-spec create_player(trace_mode(), string(), string(), string()) -> true.

create_player(TraceMode, ObscreteDir, SourceCertFilename, Nym) ->
    PlayerDir = filename:join([ObscreteDir, Nym, <<"player">>]),
    TempDir = filename:join([PlayerDir, <<"temp">>]),
    BufferDir = filename:join([PlayerDir, <<"buffer">>]),
    LocalPkiDir = filename:join([PlayerDir, <<"local-pki">>]),
    SpoolerDir = filename:join([PlayerDir, <<"spooler">>]),
    ReceivedMessagesDir = filename:join([PlayerDir, <<"received-messages">>]),
    SSLDir = filename:join([PlayerDir, "ssl"]),
    true = ensure_libs(TraceMode,
                       [TempDir, BufferDir, LocalPkiDir, SpoolerDir,
                        ReceivedMessagesDir, SSLDir],
                       true),
    TargetCertFilename = filename:join([SSLDir, <<"cert.pem">>]),
    format(TraceMode, "Copies ~s to ~s\n",
           [SourceCertFilename, TargetCertFilename]),
    case file:copy(SourceCertFilename, TargetCertFilename) of
        {ok, _} ->
            true;
        {error, Reason} ->
            format(TraceMode, standard_error, "~s: ~s\n",
                   [SourceCertFilename, file:format_error(Reason)]),
            throw({status, 100})
    end.

%% Exported: ensure_libs

-spec ensure_libs(trace_mode(), [binary()], boolean()) -> true.

ensure_libs(_TraceMode, [], _Erase) ->
    true;
ensure_libs(TraceMode, [Dir|Rest], Erase) ->
    format(TraceMode, "Ensures ~s\n", [Dir]),
    case filelib:ensure_dir(Dir) of
        ok ->
            case file:make_dir(Dir) of
                ok ->
                    ensure_libs(TraceMode, Rest, Erase);
                {error, eexist} when Erase ->
                    ok = erase_dir(TraceMode, Dir),
                    ensure_libs(TraceMode, Rest, Erase);
                {error, eexist} ->
                    ensure_libs(TraceMode, Rest, Erase);
                {error, Reason} ->
                    format(TraceMode, standard_error, "~s: ~s\n",
                           [Dir, file:format_error(Reason)]),
                    throw({status, 100})
            end;
        {error, Reason} ->
            format(TraceMode, standard_error, "~s: ~s\n",
                   [Dir, file:format_error(Reason)]),
            throw({status, 200})
    end.

erase_dir(TraceMode, Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    lists:foreach(
      fun(Filename) ->
              format(TraceMode,
                     "Deletes ~s\n", [filename:join([Dir, Filename])]),
              case filename:join([Dir, Filename]) of
                  <<"/tmp/obscrete", _/binary>> = AbsoluteFilename ->
                      file:delete(AbsoluteFilename);
                  _ ->
                      yellow
              end
      end, Filenames),
    ok.

format(log, Format, Args) ->
    ?daemon_log_tag_fmt(system, Format, Args);
format(stdout, Format, Args) ->
    io:format(Format, Args).

format(log, _Device, Format, Args) ->
    ?daemon_log_tag_fmt(system, Format, Args);
format(stdout, Device, Format, Args) ->
    io:format(Device, Format, Args).
