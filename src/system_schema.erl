-module(system_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").
-include_lib("apptools/include/shorthand.hrl").

get() ->
    [{system,
      [{'conf-revision',
        #json_type{
           name = {integer, 0, unbounded},
           reloadable = false}},
       {'initialization-time',
        #json_type{
           name = {integer, 0, unbounded},
           reloadable = false}},
       {'mixmesh-dir',
        #json_type{
           name = writable_directory,
           typical = <<"/var/mixmesh">>,
           reloadable = false}},
       {'pin-salt',
        #json_type{
           name = base64,
           typical = <<"xFxxsWkBHF9SWcEQA4pvzg==">>,
           reloadable = false}},
       {'hardware',
	#json_type{
	   name = atom,
	   info = "none, epx or pimesh",
	   typical = none,
	   transform =
	       fun(none) -> none;
		  (pimesh) -> pimesh;
		  (epxmesh) -> epxmesh;
		  (_) ->
		       throw(
			 {failed,
			  "Must be one of none, epxmesh or pimesh"})
	       end,
	   reloadable = false}}
       ]}].
