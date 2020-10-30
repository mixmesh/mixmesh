-module(system_schema).
-export([get/0]).

-include_lib("apptools/include/config_schema.hrl").
-include_lib("apptools/include/shorthand.hrl").

get() ->
    [{system,
      [{'obscrete-dir',
        #json_type{
           name = writable_directory,
           typical = <<"/var/obscrete">>,
           reloadable = false}},
       {pin,
        #json_type{
           name = string,
           typical = <<"000000">>,
           convert =
               fun(Pin) when size(Pin) /= 6 ->
                       throw({failed, "pin must contain six digits"});
                  (Pin) ->
                       try
                           _ = ?b2i(Pin),
                           Pin
                       catch _:_ ->
                               throw({failed, "pin must only contain digits"})
                       end
               end,
           reloadable = false}},
       {'pin-salt',
        #json_type{
           name = base64,
           typical = <<"xFxxsWkBHF9SWcEQA4pvzg==">>,
           reloadable = false}}]}].
