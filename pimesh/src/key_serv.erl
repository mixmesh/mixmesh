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

-export([is_locked/1]).
-export([hw_reset/1, hw_reset/2]).

-define(INT_PIN,  17).
-define(RESET_PIN, 27).
%% Led on raspberry pi, soon on TCA8414?
-define(RED_LED, 18).
-define(GREEN_LED, 24).

%% default lock code keys
-define(KEY_Asterisk, 31).
-define(KEY_Number,   33).

-define(BLINK_ON_TMO, 500).
-define(BLINK_OFF_TMO, 1000).

%% max time in ms between keys. (fixme)
-define(KEY_WAIT_TIME, 5000).
%% max time for pin code entry in ms (fixme)
-define(PINCODE_WAIT_TIME, 20000).
%% min back-off time in ms, for failed input attempt
-define(BACK_OF_TIME_1, 3).     %% first wait 3s
-define(BACK_OF_TIME_2, 4).     %% first wait 4s
-define(BACK_OF_TIME_3, 5).     %% first wait 5s
-define(BACK_OF_TIME_4, 10).    %% first wait 10s
-define(BACK_OF_TIME_5, 60).    %% first wait 1m
-define(BACK_OF_TIME_6, 3400).  %% first wait 1h
-define(BACK_OF_TIME_7, 86400).  %% max wait one day

-record(state,
	{
	 parent,
	 i2c,
	 locked = true,
	 code = "",
	 pincode = "123456",    %% digest? 
	 pincode_len = 6,       %% needed for digest!?
	 %% pincode_enter_key,     %% accept without enter key
	 pincode_enter_key = ?KEY_Number,  %% must enter with '#' after code
	 prev_key,  %% keep last key PRESSED! clear on release
	 pincode_lock_key1 = ?KEY_Asterisk,
	 pincode_lock_key2 = ?KEY_Number,
	 toggle = false,
	 blink_tmo = ?BLINK_OFF_TMO,
	 attempts = 0,
	 backoff = false
	}).

is_locked(Serv) ->
    serv:call(Serv, is_locked).

start_link() ->
    start_link(1).

start_link(Bus) ->
    application:start(i2c),
    application:start(gpio),
    ?spawn_server(fun(Parent) -> init(Parent, Bus) end,
		  fun ?MODULE:message_handler/1).

init(Parent, Bus) ->
    %% {ok,Port} = i2c_tca8418:open1(Bus),
    ok = i2c_tca8418:open(Bus),
    Port = Bus,
    gpio:init(?INT_PIN),
    gpio:init(?RESET_PIN),
    gpio:init(?GREEN_LED),
    gpio:init(?RED_LED),
    %% sleep since gpio:init is async, fixme somehow...
    %% we could use low-level api but we still need the 
    %% to poll on the gpioxyz/value file for interrupts...
    timer:sleep(1000), 

    gpio:set_direction(?INT_PIN, in),
    gpio:set_interrupt(?INT_PIN, falling),

    gpio:set_direction(?RESET_PIN, high),
    gpio:set_direction(?GREEN_LED, low),
    gpio:set_direction(?RED_LED, low),

    hw_reset(Port, {4,3}),

    Events = i2c_tca8418:read_events(Port),
    State0 = #state { parent=Parent, i2c=Port },
    State  = scan_events(Events, State0),
    {ok, State}.

hw_reset(I2C) ->
    hw_reset(I2C, {4,3}).

hw_reset(I2C, Config) ->
    timer:sleep(10),
    gpio:clr(?RESET_PIN),
    timer:sleep(10),
    gpio:set(?RESET_PIN),
    timer:sleep(10),
    %% configure for 4x3 key matrix evaluation board
    case Config of
	{3,3} ->
	    i2c_tca8418:configure_3x3(I2C);
	{4,3} ->
	    i2c_tca8418:configure_4x3(I2C)
    end.

message_handler(State=#state{i2c=Port,parent=Parent}) ->
    BlinkTmo = if State#state.backoff ->
		       infinity;
		  true ->
		       State#state.blink_tmo
	       end,
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, is_locked} ->
            {reply, From, State#state.locked, State};

        {gpio_interrupt, 0, ?INT_PIN, _Value} ->
	    io:format("pin ~w, value=~w\n", [?INT_PIN,_Value]),
	    Events = i2c_tca8418:read_events(Port),
	    State1 = if State#state.backoff ->
			     State;
			true ->
			     scan_events(Events, State)
		     end,
	    {noreply, State1};

	{timeout,_TRef,backoff} ->  %% backoff period is over
	    {noreply, State#state { backoff = false }};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    after BlinkTmo ->
	    Toggle = not State#state.toggle,
	    Tmo = 
		if Toggle ->
			gpio:set(?RED_LED),
			?BLINK_ON_TMO;
		   true ->
			gpio:clr(?RED_LED),
			?BLINK_OFF_TMO
		end,
	    {noreply, State#state { toggle = Toggle, blink_tmo = Tmo }}
    end.
%% either 
scan_events([{press,Key}|Es],State) when 
      not State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key1,
      Key =:= State#state.pincode_lock_key2
      ; %% or keys the other order
      not State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key2,
      Key =:= State#state.pincode_lock_key1 ->
    %% Lock device
    gpio:clr(?RED_LED),
    gpio:clr(?GREEN_LED),
    State1 = State#state { locked = true,
			   backoff = false,
			   attempts = 0,
			   code = [],
			   prev_key = undefined,
			   toggle = false,
			   blink_tmo = ?BLINK_OFF_TMO },
    scan_events(Es, State1);
scan_events([{press,Key}|Es],State) when 
      State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key1,
      Key =:= State#state.pincode_lock_key2
      ;
      State#state.locked,
      State#state.prev_key =:= State#state.pincode_lock_key2,
      Key =:= State#state.pincode_lock_key1 ->
    %% device is already locked, just reset code?
    scan_events(Es, State#state { code = [], prev_key = undefined });
scan_events([{press,Key}|Es], State) ->
    io:format("PRESS ~s\n", [[i2c_tca8418:keycode_to_sym(Key)]]),
    if State#state.locked ->
	    gpio:set(?GREEN_LED);
       true ->
	    ok
    end,
    State1 = add_key(Key, State),
    scan_events(Es, State1#state { prev_key = Key} );
scan_events([{release,Key}|Es], State) ->
    io:format("RELEASE ~s\n", [[i2c_tca8418:keycode_to_sym(Key)]]),
    State1 = 
	if State#state.locked ->
		gpio:clr(?GREEN_LED),
		case State#state.pincode_enter_key of
		    undefined -> check_pincode(State);
		    Key -> check_pincode(State);
		    _ -> State
		end;
	   true ->
		State
	end,
    scan_events(Es, State1#state { prev_key = undefined });
scan_events([Event|Es], State) ->
    io:format("key_serv: ignore event ~w\n", [Event]),
    scan_events(Es, State);
scan_events([], State) ->
    State.

%% fixme digest!
check_pincode(State) ->
    io:format("CODE=~s\n", [State#state.code]),
    if State#state.code =:= State#state.pincode ->
	    gpio:clr(?RED_LED),
	    gpio:set(?GREEN_LED),
	    State#state { locked = false,
			  attempts = 0,
			  toggle = true,
			  blink_tmo = infinity };
       true ->
	    Attempts = State#state.attempts + 1,
	    BackOff_Ms = backoff_s(Attempts)*1000,
	    erlang:start_timer(BackOff_Ms, self(), backoff),
	    gpio:set(?RED_LED),
	    gpio:clr(?GREEN_LED),
	    State#state { backoff = true, attempts = Attempts }
    end.

backoff_s(1) -> ?BACK_OF_TIME_1;
backoff_s(2) -> ?BACK_OF_TIME_2;
backoff_s(3) -> ?BACK_OF_TIME_3;
backoff_s(4) -> ?BACK_OF_TIME_4;
backoff_s(5) -> ?BACK_OF_TIME_5;
backoff_s(6) -> ?BACK_OF_TIME_6;
backoff_s(_) -> ?BACK_OF_TIME_7.

%% add key when 0-9 to pincode make sure length is
%% at most pincode_len
add_key(Key, State) ->
    Sym = i2c_tca8418:keycode_to_sym(Key),
    if Sym >= $0, Sym =< $9 ->
	    Code = State#state.code ++ [Sym],
	    Len = length(Code),
	    if Len =< State#state.pincode_len ->
		    State#state { code = Code };
	       true ->
		    [_|Code1] = Code,
		    State#state { code = Code1 }
	    end;
       true ->
	    State
    end.
