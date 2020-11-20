%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Keep track on pisuger current voltage
%%% @end
%%% Created : 19 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(voltage_serv).

-export([start_link/0, start_link/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SAMPLE_INTERVAL, 5000).

-record(state,
	{
	 parent,
	 i2c,
	 voltage,
	 soc
	}).

start_link() ->
    start_link(1).

start_link(Bus) ->
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun initial_message_handler/1).

init(Parent, Bus) ->
    {ok,Port} = i2c_ip5209:open1(Bus),
    V0 = i2c_ip5209:read_voltage(Port),
    SOC0 = i2c_ip5209:parse_voltage_level(V0),
    {ok, #state { parent=Parent, i2c=Port,
		  voltage=V0, soc=SOC0 }}.

initial_message_handler(State) ->
    receive
        {neighbour_workers, _NeighbourWorkers} ->
            {swap_message_handler, fun ?MODULE:message_handler/1,State}
    end.

message_handler(State=#state{i2c=Port,parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, get_soc} ->
            {stop, From, {ok,State#state.soc}};

        {call, From, get_voltage} ->
            {stop, From, {ok,State#state.voltage}};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    after ?SAMPLE_INTERVAL ->
	    V1 = i2c_ip5209:read_voltage(Port),
	    SOC1 = i2c_ip5209:parse_voltage_level(V1),
	    {noreply, State#state{voltage=V1,soc=SOC1}}
    end.

