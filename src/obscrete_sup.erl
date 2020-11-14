-module(obscrete_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

-include_lib("apptools/include/log_serv.hrl").

%% Exported: start_link

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% Exported: init

init([]) ->
    case application:get_env(obscrete, mode, normal) of
        normal ->
            ConfigJsonServSpec =
                {obscrete_config_serv, {obscrete_config_serv, start_link, []},
                 permanent, brutal_kill, worker, [obscrete_config_serv]},
            LogServSpec =
                {obscrete_log_serv, {obscrete_log_serv, start_link, []},
                 permanent, brutal_kill, worker, [obscrete_log_serv]},
            {ok, {{one_for_one, 3, 10}, [ConfigJsonServSpec, LogServSpec]}};
        bootstrap ->
            ReadLogConfig =
                fun() ->
                        {#daemon_log_info{
                            enabled = true,
                            tty = true,
                            show_filters = [system, rest_bootstrap_server],
                            hide_filters = [],
                            file = {true, <<"/tmp/bootstrap_daemon.log">>}},
                         #dbg_log_info{
                            enabled = true,
                            tty = true,
                            show_filters = [system, rest_bootstrap_server],
                            hide_filters = [],
                            file = {true, <<"/tmp/bootstrap_dbg.log">>}},
                         #error_log_info{
                            enabled = true,
                            tty = true,
                            file = {true, <<"/tmp">>}}}
                end,
            LogServSpec =
                {log_serv, {log_serv, start_link, [ReadLogConfig]},
                 permanent, brutal_kill, worker, [log_serv]},
            RestServSpec =
                {rest_bootstrap_server,
                 {rest_bootstrap_server, start_link, [8444]},
                 permanent, brutal_kill, worker, [rest_bootstrap_server]},
            {ok, {{one_for_one, 3, 10}, [LogServSpec, RestServSpec]}}
    end.
