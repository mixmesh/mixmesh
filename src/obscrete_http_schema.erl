-module(obscrete_http_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").
-include_lib("apptools/include/shorthand.hrl").

get() ->
    [{'http-server',
      [{address,
	#json_type{
	   name = ipaddress_port,
	   typical = {{242,45,0,34}, 8443},
	   reloadable = false}},
       {'cert-filename',
	#json_type{
	   name = readable_file,
	   typical = <<"/tmp/cert.pem">>,
	   reloadable = false}},
       {'password',  %% should be stored encrypted via pin!
	#json_type {
	   name = string,  %% db password
	   typical = <<"password">>,
	   reloadable = false}}
      ]}].
