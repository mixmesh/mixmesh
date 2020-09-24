-module(obscrete_log_serv).
-export([start_link/0]).
-export([read_config/0]).

-include_lib("apptools/include/config.hrl").
-include_lib("apptools/include/log_serv.hrl").

%% Exported: start_link

-spec start_link() -> {'ok', pid()} | {'error', log_serv:error_reason()}.

start_link() ->
    case log_serv:start_link(?MODULE, obscrete_config_serv,
                             fun read_config/0) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            io:format(standard_error, log_serv:format_error(Reason), []),
            {error, Reason}
    end.

%%
%% Read log configuration
%%

read_config() ->
    {daemon_log_info(), dbg_log_info(), error_log_info()}.

daemon_log_info() ->
    Enabled = ?config([logs, daemon, enabled]),
    Tty = ?config([logs, daemon, tty]),
    ShowModuleFilters = ?config([logs, daemon, filter, show]),
    HideModuleFilters = ?config([logs, daemon, filter, hide]),
    FileEnabled = ?config([logs, daemon, file, enabled]),
    FilePath = ?config([logs, daemon, file, path]),
    #daemon_log_info{enabled = Enabled,
                     tty = Tty,
                     show_filters = ShowModuleFilters,
                     hide_filters = HideModuleFilters,
                     file = {FileEnabled, FilePath}}.

dbg_log_info() ->
    Enabled = ?config([logs, dbg, enabled]),
    Tty = ?config([logs, dbg, tty]),
    ShowModuleFilters = ?config([logs, dbg, filter, show]),
    HideModuleFilters = ?config([logs, dbg, filter, hide]),
    FileEnabled = ?config([logs, dbg, file, enabled]),
    FilePath = ?config([logs, dbg, file, path]),
    #dbg_log_info{enabled = Enabled,
                  tty = Tty,
                  show_filters = ShowModuleFilters,
                  hide_filters = HideModuleFilters,
                  file = {FileEnabled, FilePath}}.

error_log_info() ->
    Enabled = ?config([logs, error, enabled]),
    Tty = ?config([logs, error, tty]),
    FileEnabled = ?config([logs, error, file, enabled]),
    FilePath = ?config([logs, error, file, path]),
    #error_log_info{enabled = Enabled,
                    tty = Tty,
                    file = {FileEnabled, FilePath}}.
