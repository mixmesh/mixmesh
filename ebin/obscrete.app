%% -*- erlang -*-
{application, obscrete,
 [{description,"The Obscrete top-level application"},
  {vsn, "0.9"},
  {modules, [obscrete_app,
             obscrete_config_serv,
             obscrete_log_serv,
             obscrete_sup]},
  {registered, [obscrete_app, obscrete_sup]},
  {env, [{schemas, [pin_schema,
		    obscrete_http_schema,
                    global_pki_server_schema,
                    player_schema,
                    obscrete_control_schema,
                    logs_schema
		   ]}]},
  {mod, {obscrete_app, []}},
  {applications, [kernel, stdlib, apptools]}]}.
