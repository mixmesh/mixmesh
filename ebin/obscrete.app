%% -*- erlang -*-
{application, obscrete,
 [{description,"The Obscrete top-level application"},
  {vsn, "1.0"},
  {modules, [obscrete_app,
             obscrete_config_serv,
             obscrete_log_serv,
             obscrete_sup]},
  {registered, [obscrete_app, obscrete_sup]},
  {env, [{prepended_config_schema, [{module, pki_config_schema}]},
         {appended_config_schema, []}]},
  {mod, {obscrete_app, []}},
  {applications, [kernel, stdlib]}]}.