%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(key_serv).

-export([start_link/0, start_link/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-record(state,
	{
	 parent,
	 i2c,
	 events = []
	}).

start_link() ->
    start_link(1).

start_link(Bus) ->
    application:start(i2c),
    application:start(gpio),
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

init(Parent, Bus) ->
    %% {ok,Port} = i2c_tca8418:open1(Bus),
    ok =i2c_tca8418:open(Bus),
    Port = Bus,
    %% configure for 3x3 key matrix evaluation board    
    i2c_tca8418:configure3x3(Port),
    gpio:init(17),
    gpio:input(17),
    gpio:set_interrupt(17, rising),
    Events = i2c_tca8418:read_keys(Port),
    {ok, #state { parent=Parent, i2c=Port, events=Events }}.

message_handler(State=#state{i2c=Port,parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        {call, From, get_events} ->
            {reply, From, {ok,State#state.events}, State#state { events=[] }};

        {gpio_interrupt, 0, 17, _Value} ->
	    io:format("pin 17, value=~w\n", [_Value]),
	    Events = i2c_tca8418:read_keys(Port),
	    io:format("Got events = ~w\n", [Events]),
	    {noreply, State#state{ events=State#state.events++Events }};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.
    
