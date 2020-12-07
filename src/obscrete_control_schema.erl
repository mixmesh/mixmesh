-module(obscrete_control_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").

get() ->
    [{'obscrete-control',
      [{listen,
        #json_type{name = ip4_address_port,
                   typical = {{127,0,0,1}, 23765},
                   reloadable = false}}]}].
