-module(mkconfig).
-export([start/1]).
-export([ensure_libs/2, return/1]).

%% This script creates the appropriate file structure needed to start
%% Obscrete. You call this command with an obscrete directory and the
%% name of a player as input, e.g. "./mkconfigdir /tmp/obscrete alice".
%% If you do this the following will be created:
%%
%% /tmp/obscrete/pki/data
%% /tmp/obscrete/alice/player/temp/
%% /tmp/obscrete/alice/player/pki/data/
%% /tmp/obscrete/alice/player/maildrop/spooler/
%%
%% As it happens this is the file structure used by the configuration
%% files under ./obscrete/etc/*.conf.

-spec start([string()]) -> no_return().

start([ObscreteDir, PlayerName]) ->
    PkiDataDir = filename:join([ObscreteDir, <<"pki">>, <<"data">>]),
    ok = ensure_libs([PkiDataDir], true),
    PlayerDir = filename:join([ObscreteDir, PlayerName, <<"player">>]),
    PlayerTempDir = filename:join([PlayerDir, "temp"]),
    PlayerPkiDataDir = filename:join([PlayerDir, "pki", "data"]),
    PlayerMaildropSpoolerDir =
        filename:join([PlayerDir, "maildrop", "spooler"]),
    ensure_libs([PlayerTempDir, PlayerPkiDataDir, PlayerMaildropSpoolerDir],
                true),
    return(0).

ensure_libs([], _Erase) ->
    ok;
ensure_libs([Dir|Rest], Erase) ->
    io:format("Ensure ~s\n", [Dir]),
    case filelib:ensure_dir(Dir) of
        ok ->
            case file:make_dir(Dir) of
                ok ->
                    ensure_libs(Rest, Erase);
                {error, eexist} when Erase ->
                    ok = erase_dir(Dir),
                    ensure_libs(Rest, Erase);
                {error, eexist} ->
                    ensure_libs(Rest, Erase);
                {error, Reason} ->
                    io:format(standard_error, "~s: ~s\n",
                              [Dir, file:format_error(Reason)]),
                    return(100)
            end;
        {error, Reason} ->
            io:format(standard_error, "~s: ~s\n",
                      [Dir, file:format_error(Reason)]),
            return(200)
    end.

erase_dir(Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    lists:foreach(
      fun(Filename) ->
              io:format("Delete ~s\n", [filename:join([Dir, Filename])]),
              file:delete(filename:join([Dir, Filename]))
      end, Filenames),
    ok.

return(Status) ->
    erlang:halt(Status).
