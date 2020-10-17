-module(obscrete_config_serv).
-export([start_link/0, stop/0, reload/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/config_schema.hrl").

-define(DEFAULT_CONFIG_FILENAME, <<"/etc/obscrete.conf">>).
-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23313).
-define(OBSCRETE_CONFIG_SCHEMA,
        [{'load-paths',
          [#json_type{name = path,
                      typical = <<"/foo/bar/ebin">>,
                      reloadable = false}]},
         {'obscrete-control',
          [{listen,
            #json_type{name = ipv4address_port,
                       typical = {{127,0,0,1}, 23765},
                       reloadable = false}}]},
         {logs,
          [{daemon,
            [{enabled,
              #json_type{name = bool, typical = true}},
             {filter,
              [{show,
                [#json_type{name = atom, typical = '*'}]},
               {hide,
                [#json_type{name = atom, typical = '*'}]}]},
             {tty,
              #json_type{name = bool, typical = false}},
             {file,
              [{enabled,
                #json_type{name = bool, typical = true}},
               {path,
                #json_type{name = writable_file,
                           typical = <<"daemon.log">>}}]}]},
           {dbg,
            [{enabled,
              #json_type{name = bool, typical = true}},
             {filter,
              [{show,
                [#json_type{name = atom, typical = '*'}]},
               {hide,
                [#json_type{name = atom, typical = '*'}]}]},
             {tty,
              #json_type{name = bool, typical = false}},
             {file,
              [{enabled,
                #json_type{name = bool, typical = true}},
               {path,
                #json_type{name = writable_file,
                           typical = <<"dbg.log">>}}]}]},
           {error,
            [{enabled,
              #json_type{name = bool, typical = true}},
             {tty,
              #json_type{name = bool, typical = true}},
             {file,
              [{enabled,
                #json_type{name = bool, typical = true}},
               {path,
                #json_type{name = writable_directory,
                           typical = <<"./">>}}]}]}]}
        ]).

%% Exported: start_link

-spec start_link() -> {'ok', pid()} |
                      {'error', config_serv:error_reason()}.

start_link() ->
    ConfigFilename = config_filename(),
    ConfigSchema =
        extra_config_schema(prepended_config_schema) ++
        ?OBSCRETE_CONFIG_SCHEMA ++ 
        extra_config_schema(appended_config_schema),
    case config_serv:start_link(?MODULE, ConfigFilename, ConfigSchema,
                                ['obscrete-control', listen],
                                fun config_handler/1) of
        {ok, Pid} ->
            ok = code:add_pathsz(config:lookup(['load-paths'])),
            {ok, Pid};
        {error, Reason} ->
            die("~s: ~s", [ConfigFilename, config_serv:format_error(Reason)]),
            {error, Reason}
    end.

config_filename() ->
    case init:get_argument('-config') of
        {ok, [[ConfigFilename]]} ->
            ConfigFilename;
        error ->
            ?DEFAULT_CONFIG_FILENAME
    end.

extra_config_schema(Parameter) ->
    {ok, ConfigSchema} = application:get_env(obscrete, Parameter),
    load_config_schema(ConfigSchema).

load_config_schema([]) ->
    [];
load_config_schema([{module, Module}|Rest]) ->
    Module:get() ++ load_config_schema(Rest);
load_config_schema([{filename, Filename}|Rest]) ->
    {module, Module} = code:load_abs(Filename),
    Module:get() ++ load_config_schema(Rest).

config_handler(Socket) ->
    receive
        {tcp, Socket, <<"stop">>} ->
            init:stop(),
            ok;
        {tcp, Socket, <<"reload">>} ->
            ?MODULE ! reload,
            ok;
        {tcp_closed, Socket} ->
            ok
    end.

%% Exported: stop

-spec stop() -> no_return().

stop() ->
    ControlAddress = control_address(),
    ControlPort = control_port(),
    case config_serv:tcp_send(ControlAddress, ControlPort, <<"stop">>) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            die(config_serv:format_error(Reason), [])
    end.

%% Exported: reload

-spec reload() -> no_return().

reload() ->
    ConfigFilename = config_filename(),
    ControlAddress = control_address(),
    ControlPort = control_port(),
    case config_serv:tcp_send(ControlAddress, ControlPort, <<"reload">>) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            die("~s: ~s", [ConfigFilename, config_serv:format_error(Reason)])
    end.

control_address() ->
    case init:get_argument('-control-address') of
        error ->
            ?DEFAULT_CONTROL_ADDRESS;
        {ok, [[ControlAddressString]]}->
            case inet_parse:address(ControlAddressString) of
                {ok, ControlAddress} ->
                    ControlAddress;
                {error, _Reason} ->
                    die("Invalid control address: ~s", [ControlAddressString])
            end
    end.

control_port() ->
    case init:get_argument('-control-port') of
        error ->
            ?DEFAULT_CONTROL_PORT;
        {ok, [[ControlPortString]]} ->
            case catch ?l2i(ControlPortString) of
                {'EXIT', _} ->
                    die("Invalid control port number: ~s", [ControlPortString]);
                ControlPort ->
                    ControlPort
            end
    end.

die(Format, Args) ->
    io:format(standard_error, Format ++ "~n", Args),
    %% Allow data on stderr to be printed
    timer:sleep(1000),
    erlang:halt().
