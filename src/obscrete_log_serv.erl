-module(obscrete_log_serv).
-export([start_link/0]).
-export([is_log_enabled/1]).

-include_lib("apptools/include/log_serv.hrl").
-include("../include/log.hrl").

%% Exported: start_link

-spec start_link() -> {'ok', pid()} | {'error', log_serv:error_reason()}.

start_link() ->
    case log_serv:start_link(?MODULE, obscrete_config_serv,
                             fun read_config/0,
                             fun config_updated/0) of
        {ok, Pid} ->
            ?MODULE = ets:new(?MODULE, [public, named_table]),
            true = config_updated(),
            {ok, Pid};
        {error, Reason} ->
            io:format(standard_error, log_serv:format_error(Reason), []),
            {error, Reason}
    end.

%% Exported: is_log_enabled

is_log_enabled(LogType) ->
    [{LogType, Enabled}] = ets:lookup(?MODULE, LogType),
    Enabled.

%%
%% Configuration
%%

read_config() ->
    {daemon_log_info(), dbg_log_info(), error_log_info()}.

daemon_log_info() ->
    Enabled = config:lookup([logs, daemon, enabled]),
    Tty = config:lookup([logs, daemon, tty]),
    ShowModuleFilters = config:lookup([logs, daemon, filter, show]),
    HideModuleFilters = config:lookup([logs, daemon, filter, hide]),
    FileEnabled = config:lookup([logs, daemon, file, enabled]),
    FilePath = config:lookup([logs, daemon, file, path]),
    #daemon_log_info{enabled = Enabled,
                     tty = Tty,
                     show_filters = ShowModuleFilters,
                     hide_filters = HideModuleFilters,
                     file = {FileEnabled, FilePath}}.

dbg_log_info() ->
    Enabled = config:lookup([logs, dbg, enabled]),
    Tty = config:lookup([logs, dbg, tty]),
    ShowModuleFilters = config:lookup([logs, dbg, filter, show]),
    HideModuleFilters = config:lookup([logs, dbg, filter, hide]),
    FileEnabled = config:lookup([logs, dbg, file, enabled]),
    FilePath = config:lookup([logs, dbg, file, path]),
    #dbg_log_info{enabled = Enabled,
                  tty = Tty,
                  show_filters = ShowModuleFilters,
                  hide_filters = HideModuleFilters,
                  file = {FileEnabled, FilePath}}.

error_log_info() ->
    Enabled = config:lookup([logs, error, enabled]),
    Tty = config:lookup([logs, error, tty]),
    FileEnabled = config:lookup([logs, error, file, enabled]),
    FilePath = config:lookup([logs, error, file, path]),
    #error_log_info{enabled = Enabled,
                    tty = Tty,
                    file = {FileEnabled, FilePath}}.

config_updated() ->
    DaemonEnabled = config:lookup([logs, daemon, enabled]),
    true = ets:insert(?MODULE, {daemon, DaemonEnabled}),
    DbgEnabled = config:lookup([logs, dbg, enabled]),
    true = ets:insert(?MODULE, {dbg, DbgEnabled}),
    ErrorEnabled = config:lookup([logs, error, enabled]),
    ets:insert(?MODULE, {error, ErrorEnabled}).
