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
	 blink_tmo = ?BLINK_OFF_TMO
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

    gpio:set_direction(?INT_PIN, in),
    gpio:set_interrupt(?INT_PIN, falling),

    gpio:set_direction(?RESET_PIN, high),
    gpio:set_direction(?GREEN_LED, low),
    gpio:set_direction(?RED_LED, low),

    hw_reset(Port, {4,3}),

    Events = i2c_tca8418:read_keys(Port),
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
    receive
        {call, From, stop} ->
            {stop, From, ok};

        {call, From, is_locked} ->
            {reply, From, State#state.locked, State};

        {gpio_interrupt, 0, ?INT_PIN, _Value} ->
	    io:format("pin ~w, value=~w\n", [?INT_PIN,_Value]),
	    Events = i2c_tca8418:read_keys(Port),
	    State1 = scan_events(Events, State),
	    {noreply, State1};

        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    after State#state.blink_tmo ->
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

scan_events([{press,Key}|Es],State) when 
      State#state.prev_key =:= State#state.pincode_lock_key1,
      Key =:= State#state.pincode_lock_key2,
      not State#state.locked ->
    %% Lock device
    gpio:clr(?RED_LED),
    gpio:clr(?GREEN_LED),
    State1 = State#state { locked = true,
			   code = [],
			   prev_key = undefined,
			   toggle = false,
			   blink_tmo = ?BLINK_OFF_TMO },
    scan_events(Es, State1);
scan_events([{press,Key}|Es],State) when 
      State#state.prev_key =:= State#state.pincode_lock_key1,
      Key =:= State#state.pincode_lock_key2,
      State#state.locked ->
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
scan_events([], State) ->
    State.

%% fixme digest!
check_pincode(State) ->
    io:format("CODE=~s\n", [State#state.code]),
    if State#state.code =:= State#state.pincode ->
	    gpio:clr(?RED_LED),
	    gpio:set(?GREEN_LED),
	    State#state { locked = false,
			  toggle = true,
			  blink_tmo = infinity };
       true ->
	    %% set pinentry delay
	    State
    end.

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
