%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Keep track on pisuger batter and application status
%%% @end
%%% Created : 19 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(battery_serv).

-export([start_link/0, start_link/1]).
-export([set_voltage/2, get_voltage/1, get_soc/1]).
%% serv callback
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-define(SAMPLE_INTERVAL, 1000).

-define(APPLICATION_LED,  {col,8}).
-define(BLUE_LED_100, {col,7}).
-define(BLUE_LED_75,  {col,6}).
-define(BLUE_LED_50,  {col,5}).
-define(BLUE_LED_25,  {col,4}).

-record(state,
	{
	 parent,
	 ip5209,
	 tca8418,
	 voltage,
	 soc,
	 soc0,  %% last reported soc
	 toggle = false
	}).

start_link() ->
    start_link(1).

start_link(Bus) ->
    application:start(i2c),
    application:start(gpio),
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

set_voltage(Serv, V) when is_number(V), V >= 0, V =< 5.0 ->
    serv:call(Serv, {set_voltage, float(V)}).

get_voltage(Serv) ->
    serv:call(Serv, get_voltage).

get_soc(Serv) ->
    serv:call(Serv, get_soc).


init(Parent, Bus) ->
    {ok,TCA8418} = i2c_tca8418:open1(Bus),
    i2c_tca8418:gpio_init(TCA8418, ?APPLICATION_LED),
    i2c_tca8418:gpio_init(TCA8418, ?BLUE_LED_100),
    i2c_tca8418:gpio_init(TCA8418, ?BLUE_LED_75),
    i2c_tca8418:gpio_init(TCA8418, ?BLUE_LED_50),
    i2c_tca8418:gpio_init(TCA8418, ?BLUE_LED_25),

    i2c_tca8418:gpio_output(TCA8418, ?APPLICATION_LED),
    i2c_tca8418:gpio_output(TCA8418, ?BLUE_LED_100),
    i2c_tca8418:gpio_output(TCA8418, ?BLUE_LED_75),
    i2c_tca8418:gpio_output(TCA8418, ?BLUE_LED_50),
    i2c_tca8418:gpio_output(TCA8418, ?BLUE_LED_25),

    i2c_tca8418:gpio_set(TCA8418, ?APPLICATION_LED),
    i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_100),
    i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_75),
    i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_50),
    i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_25),

    %% {ok,IP5209}  = i2c_ip5209:open1(Bus),
    %% V0 = i2c_ip5209:read_voltage(Port),
    IP5209 = 1,
    V0 = 3.80,
    SOC0 = i2c_ip5209:parse_voltage_level(V0),
    update_soc(TCA8418, SOC0, true),
    {ok, #state { parent=Parent,
    	 	  ip5209 = IP5209, tca8418 = TCA8418,
		  voltage=V0, soc=SOC0, soc0=SOC0 }}.
		  

message_handler(State=#state{tca8418=TCA8418,ip5209=_IP5209,parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, get_soc} ->
            {reply, From, State#state.soc};

        {call, From, get_voltage} ->
            {reply, From, State#state.voltage};

        {call, From, {set_voltage,V}} ->
            {reply, From, ok,State#state { voltage = V }};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    after ?SAMPLE_INTERVAL ->
	    %% V1 = i2c_ip5209:read_voltage(Port),
	    V1 = State#state.voltage, %% simulated (set_voltage)
	    SOC1 = i2c_ip5209:parse_voltage_level(V1),
	    SOC0 = if abs(SOC1 - State#state.soc0) > 1.0 ->
			   SOC1;
		      true ->
			   State#state.soc0
		   end,
	    Toggle = not State#state.toggle,
	    update_soc(TCA8418, SOC0, Toggle),
	    {noreply, State#state{voltage=V1,soc=SOC1, soc0=SOC0,
	    	      		  toggle=Toggle}}
    end.

%% (0-5)     0   0   0   0
%% (5-10)    x   0   0   0
%% (10-30)   1   0   0   0
%% (30-40)   1   x   0   0
%% (40-60)   1   1   0   0
%% (60-70)   1   1   x   0
%% (70-80)   1   1   1   0
%% (80-90)   1   1   1   x
%% (90-100)  1   1   1   1

update_soc(TCA8418, Soc, Set) ->
    if
       Soc > 10; Soc >= 5, Soc =< 10, Set ->
       	   i2c_tca8418:gpio_set(TCA8418, ?BLUE_LED_25);
       true ->
           i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_25)
    end,
    if
       Soc > 40; Soc >= 30, Soc =< 40, Set ->
       	   i2c_tca8418:gpio_set(TCA8418, ?BLUE_LED_50);
       true ->
           i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_50)
    end,
    if
       Soc > 60; Soc >= 60, Soc =< 70, Set ->
       	   i2c_tca8418:gpio_set(TCA8418, ?BLUE_LED_75);
       true ->
           i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_75)
    end,
    if 
       Soc > 90; Soc >= 80, Soc =< 90, Set ->
       	   i2c_tca8418:gpio_set(TCA8418, ?BLUE_LED_100);
       true ->
           i2c_tca8418:gpio_clr(TCA8418, ?BLUE_LED_100)
    end.
