-module(obscrete_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

%% Exported: start_link

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% Exported: init

init([]) ->
    ConfigJsonServChildSpec =
        {obscrete_config_serv, {obscrete_config_serv, start_link, []},
         permanent, brutal_kill, worker, [obscrete_config_serv]},
    LogServChildSpec =
        {obscrete_log_serv, {obscrete_log_serv, start_link, []},
         permanent, brutal_kill, worker, [obscrete_log_serv]},
    {ok, {{one_for_one, 3, 10},
          [ConfigJsonServChildSpec, LogServChildSpec]}}.