%% -*- erlang -*-
{application, obscrete,
 [{description,"The Obscrete top-level application"},
  {vsn, "0.9"},
  {modules, [obscrete_app,
             obscrete_config_serv,
             obscrete_log_serv,
             obscrete_sup]},
  {registered, [obscrete_app, obscrete_sup]},
  {env, []},
  {mod, {obscrete_app, []}},
  {applications, [kernel, stdlib, apptools]}]}.
