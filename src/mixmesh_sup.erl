-module(mixmesh_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

-include_lib("apptools/include/log_serv.hrl").

%% Exported: start_link

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% Exported: init

init([]) ->
    case application:get_env(mixmesh, mode, normal) of
        normal ->
            ConfigJsonServSpec =
                {mixmesh_config_serv, {mixmesh_config_serv, start_link, []},
                 permanent, brutal_kill, worker, [mixmesh_config_serv]},
            LogServSpec =
                {mixmesh_log_serv, {mixmesh_log_serv, start_link, []},
                 permanent, brutal_kill, worker, [mixmesh_log_serv]},
            {ok, {{one_for_one, 3, 10}, [ConfigJsonServSpec, LogServSpec]}};
        bootstrap ->
            ReadLogConfig =
                fun() ->
                        {#daemon_log_info{
                            enabled = true,
                            tty = true,
                            show_filters = [system, player_bootstrap_service],
                            hide_filters = [],
                            file = {true, <<"/tmp/bootstrap_daemon.log">>}},
                         #dbg_log_info{
                            enabled = true,
                            tty = true,
                            show_filters = [system, player_bootstrap_service],
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
                {player_bootstrap_service,
                 {player_bootstrap_service, start_link, [443]},
                 permanent, brutal_kill, worker, [player_bootstrap_service]},
            {ok, {{one_for_one, 3, 10}, [LogServSpec, RestServSpec]}}
    end.
