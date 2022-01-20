-module(mixmesh_config_serv).
-export([start_link/0, stop/0, reload/0]).
-export([get_schemas/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/config_schema.hrl").

-define(DEFAULT_CONFIG_FILENAME, <<"/etc/mixmesh.conf">>).
-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23313).

-define(CONF_REVISION, 1).

%%
%% Exported: start_link
%%

-spec start_link() -> {'ok', pid()} |
                      {'error', config_serv:error_reason()}.

start_link() ->
    ConfigFilename = config_filename(),
    case config_serv:start_link(
           ConfigFilename, get_schemas(),
           fun() -> config:lookup(['mixmesh-control', listen]) end,
           fun listener_handler/1,
           fun upgrade_handler/1,
           fun post_process/1) of
        {ok, Pid} ->
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

get_schemas() ->
    {ok, AppSchemas} = application:get_env(mixmesh, app_schemas),
    get_schemas(AppSchemas).

get_schemas([]) ->
    [];
get_schemas([{App, SchemaModule}|Rest]) ->
    [{App, SchemaModule:get()}|get_schemas(Rest)].

listener_handler(Socket) ->
    receive
        {tcp, Socket, <<"stop">>} ->
            init:stop(),
            ok;
        {tcp, Socket, <<"reload">>} ->
            config_serv ! reload,
            ok;
        {tcp_closed, Socket} ->
            ok
    end.

upgrade_handler(OldConfigFilename) ->
    {ok, OldConfig} = file:read_file(OldConfigFilename),
    case jsone:try_decode(OldConfig, [{object_format, proplist}]) of
        {ok, ParsedOldConfig, _} ->
            RevisionedParsedOldConfig =
                case ParsedOldConfig of
                    [{<<"system">>,
                      [{<<"conf-revision">>, OldRevision}|_]}|_] ->
                        ParsedOldConfig;
                    [{<<"system">>, SystemConfig}|Config] ->
                        OldRevision = 0,
                        [{<<"system">>,
                          [{<<"conf-revision">>, OldRevision}|SystemConfig]}|
                         Config]
                end,
            if
                OldRevision > ?CONF_REVISION ->
                    {error, downgrade_not_supported};
                OldRevision == ?CONF_REVISION ->
                    {ok, {OldRevision, ?CONF_REVISION, not_changed}};
                true ->
                    NewConfig =
                        upgrade_config(
                          RevisionedParsedOldConfig, OldRevision,
                          ?CONF_REVISION),
                    {ok, Binary} =
                        jsone:try_encode(
                          NewConfig,
                          [{float_format, [{decimals, 4}, compact]},
                           {indent, 2},
                           {object_key_type, value},
                           {space, 1},
                           native_forward_slash]),
                    {ok, {OldRevision, ?CONF_REVISION, Binary}}
            end;
        {error, Reason} ->
            {error, {config, {bad_json, Reason}}}
    end.

upgrade_config(Config, N, N) ->
    Config;
upgrade_config(OldConfig, N, M) when N == M - 1 ->
    upgrade(N, M, OldConfig, []);
upgrade_config(OldConfig, N, M) ->
    upgrade_config(upgrade_config(OldConfig, N, N + 1), N + 1, M).

%% 0 -> 1

upgrade(0, 1, [], _JsonPath) ->
    [];
upgrade(0, 1, [{<<"conf-revision">>, 0}|Rest], JsonPath) ->
    [{<<"conf-revision">>, 1}|upgrade(0, 1, Rest, JsonPath)];
upgrade(0, 1, [{<<"sync-address">>, SyncAddress},
               {<<"routing">>,
                [{<<"type">>, Type},
                 {<<"use-gps">>, UseGps},
                 {<<"longitude">>, Longitude},
                 {<<"latitude">>, Latitude},
                 {<<"f">>, F},
                 {<<"k">>, K},
                 {<<"public-key">>, PublicKey},
                 {<<"secret-key">>, SecretKey}]}|Rest], [<<"player">>]) ->
    [{<<"routing">>,
      [{<<"type">>, Type},
       {<<"use-gps">>, UseGps},
       {<<"longitude">>, Longitude},
       {<<"latitude">>, Latitude}]},
     {<<"sync-server">>,
      [{<<"address">>, SyncAddress},
       {<<"f">>, F},
       {<<"k">>, K},
       {<<"public-key">>, PublicKey},
       {<<"secret-key">>, SecretKey}]}|Rest];
upgrade(0, 1, [{Name, NestedConfig}|Config], JsonPath) ->
    [{Name, upgrade(0, 1, NestedConfig, [Name|JsonPath])}|
     upgrade(0, 1, Config, JsonPath)];
upgrade(0, 1, Config, _JsonPath) ->
    Config.

post_process(JsonTerm) ->
    try
        {ok, post_process(JsonTerm, JsonTerm, [])}
    catch
        throw:{failure, FailurePath, Reason} ->
            {error, FailurePath, Reason}
    end.

post_process([], _OriginalJsonTerm, _JsonPath) ->
    [];
post_process([{'peer-id', -1}|Rest], OriginalJsonTerm,
             [gaia] = JsonPath) ->
    PeerName = config_serv:json_lookup(OriginalJsonTerm, [gaia, 'peer-name']),
    PeerId = gaia_serv:generate_artificial_id(PeerName),
    [{'peer-id', PeerId}|post_process(Rest, OriginalJsonTerm, JsonPath)];
post_process([{members, Members} = NameValue|Rest], OriginalJsonTerm,
             [groups, gaia] = JsonPath) ->
    lists:foreach(
      fun(Member) ->
              case config_serv:json_lookup(
                     OriginalJsonTerm,
                     [gaia, peers, {name, Member}]) of
                  not_found ->
                      case config_serv:json_lookup(
                             OriginalJsonTerm, [gaia, 'peer-name']) of
                          Member ->
                              ok;
                          _ ->
                              Reason =
                                  ?l2b(io_lib:format("Peer ~s is missing",
                                                     [Member])),
                              throw({failure, JsonPath, Reason})
                      end;
                  _ ->
                      ok
              end
      end, Members),
    [NameValue|post_process(Rest, OriginalJsonTerm, JsonPath)];
post_process([{admin, Admin} = AdminValue|Rest], OriginalJsonTerm,
             [groups, gaia] = JsonPath) ->
    case config_serv:json_lookup(
           OriginalJsonTerm,
           [gaia, peers, {name, Admin}]) of
        not_found ->
            case config_serv:json_lookup(
                   OriginalJsonTerm, [gaia, 'peer-name']) of
                Admin ->
                    [AdminValue|post_process(Rest, OriginalJsonTerm, JsonPath)];
                _ ->
                    Reason =
                        ?l2b(io_lib:format("Peer ~s is missing",
                                           [Admin])),
                    throw({failure, JsonPath, Reason})
            end;
        _ ->
            [AdminValue|post_process(Rest, OriginalJsonTerm, JsonPath)]
    end;
post_process([{Name, Value}|Rest], OriginalJsonTerm, JsonPath)
  when is_list(Value) ->
    [{Name, post_process(Value, OriginalJsonTerm, [Name|JsonPath])}|
     post_process(Rest, OriginalJsonTerm, JsonPath)];
post_process([{_Name, _Value} = NameValue|Rest], OriginalJsonTerm, JsonPath) ->
    [NameValue|post_process(Rest, OriginalJsonTerm, JsonPath)];
post_process([List|Rest], OriginalJsonTerm, JsonPath) when is_list(List) ->
    [post_process(List, OriginalJsonTerm, JsonPath)|
     post_process(Rest, OriginalJsonTerm, JsonPath)];
post_process([Value|Rest], OriginalJsonTerm, JsonPath) ->
    [Value|post_process(Rest, OriginalJsonTerm, JsonPath)].

%%
%% Exported: stop
%%

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

%%
%% Exported: reload
%%

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
