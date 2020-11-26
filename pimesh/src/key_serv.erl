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

-define(INT_PIN,  17).
-define(RESET_PIN, 27).
%% Led on raspberry pi, soon on TCA8414?
-define(RED_LED, 18).
-define(GREEN_LED, 24).

-record(state,
	{
	 parent,
	 i2c,
	 locked = true,
	 code = "",
	 pincode = "1379",      %% digest? 
	 pincode_len = 4,       %% needed for digest!?
	 pincode_accept,        %% accept without enter key
	 %%pincode_accept = $#,
	 pincode_lock   = "*#", %% two key sequence
	 require_pin = true,
	 require_toggle = false,
	 require_tmo = 1000
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
    %% configure for 4x3 key matrix evaluation board
    i2c_tca8418:configure_4x3(Port),

    gpio:init(?INT_PIN),
    gpio:init(?RESET_PIN),
    gpio:init(?GREEN_LED),
    gpio:init(?RED_LED),

    gpio:set_direction(?INT_PIN, in),
    gpio:set_interrupt(?INT_PIN, falling),

    gpio:set_direction(?RESET_PIN, high),
    gpio:set_direction(?GREEN_LED, low),
    gpio:set_direction(?RED_LED, low),

    %% fixme RESET the tc8418

    Events = i2c_tca8418:read_keys(Port),
    State0 = #state { parent=Parent, i2c=Port },
    State  = scan_events(Events, State0),
    {ok, State}.

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
    after State#state.require_tmo ->
	    Toggle = not State#state.require_toggle,
	    if Toggle -> gpio:set(?RED_LED);
	       true -> gpio:clr(?RED_LED)
	    end,
	    {noreply, State#state { require_toggle = Toggle }}
    end.
    
scan_events([{press,Key}|Es], State) ->
    io:format("PRESS ~s\n", [[i2c_tca8418:keycode_4x3_to_sym(Key)]]),
    if State#state.locked ->
	    gpio:set(?GREEN_LED);
       true ->
	    ok
    end,
    State1 = add_key(Key, State),
    scan_events(Es, State1);
scan_events([{release,Key}|Es], State) ->
    io:format("RELEASE ~s\n", [[i2c_tca8418:keycode_4x3_to_sym(Key)]]),
    Sym = i2c_tca8418:keycode_4x3_to_sym(Key),
    State1 = 
	if State#state.locked ->
		gpio:clr(?GREEN_LED),
		case State#state.pincode_accept of
		    Sym -> check_pincode(State);
		    undefined -> check_pincode(State);
		    _ -> State
		end;
	   true ->
		State
	end,
    scan_events(Es, State1);
scan_events([], State) ->
    State.

%% fixme digest!
check_pincode(State) ->
    io:format("CODE=~s\n", [State#state.code]),
    if State#state.code =:= State#state.pincode ->
	    gpio:clr(?RED_LED),
	    gpio:set(?GREEN_LED),
	    State#state { locked = false,
			  require_toggle = true,
			  require_pin = false,
			  require_tmo = infinity };
       true ->
	    %% set pinentry delay
	    State
    end.

%% add key when 0-9 to pincode make sure length is
%% at most pincode_len
add_key(Key, State) ->
    Sym = i2c_tca8418:keycode_4x3_to_sym(Key),
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
