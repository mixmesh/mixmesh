-module(system_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").
-include_lib("apptools/include/shorthand.hrl").

get() ->
    [{system,
      [{'initialization-time',
        #json_type{
           name = {integer, 0,  unbounded},
           reloadable = false}},
       {'obscrete-dir',
        #json_type{
           name = writable_directory,
           typical = <<"/var/obscrete">>,
           reloadable = false}},
       {'pin-salt',
        #json_type{
           name = base64,
           typical = <<"xFxxsWkBHF9SWcEQA4pvzg==">>,
           reloadable = false}}]}].
