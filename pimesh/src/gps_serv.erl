%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Parse and process GPS position data over NMEA 0183 module
%%% @end
%%% Created :  9 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(gps_serv).

-export([start_link/0, start_link/1]).
-export([get_position/1, get_time/1, get_tz/1, 
	 get_date/1, get_speed/1]).

-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-record(state,
	{
	 parent,
	 uart,
	 valid=false,
	 date,          %% {Year,Month,Day}
	 time,          %% {Hour,Minute,Seconds}
	 tz,            %% {Tz-hours, Tz-minutes}
	 long = 0.0,
	 lat = 0.0,
	 times = 0.0,    %% (0.0 - 86400.0)
	 speed = 0.0
	}).

-define(EARTH_RADIUS, 6371000.0).  %% radius in meters

start_link() ->
    start_link("/dev/serial0").

start_link(UartDeviceName) ->
    ?spawn_server(fun(Parent) -> init(Parent, UartDeviceName) end,
		  fun ?MODULE:message_handler/1).

get_position(Serv) ->
    serv:call(Serv, get_position).

get_time(Serv) ->
    serv:call(Serv, get_time).

get_date(Serv) ->
    serv:call(Serv, get_date).

get_tz(Serv) ->
    serv:call(Serv, get_tz).

get_speed(Serv) ->
    serv:call(Serv, get_speed).

init(Parent, UartDeviceName) ->
    Baud = 9600,
    UartOpts = [{mode,binary}, {baud, Baud}, {packet, line},
		{csize, 8}, {stopb,1}, {parity,none}, {active, once}],
    case uart:open1(UartDeviceName, UartOpts) of
	{ok,Uart} ->
	    {ok, #state { parent=Parent, uart=Uart }};
	Error = {erro,Reason} ->
	    io:format("~p\n", [{uart_open_error, Reason}]),
	    %% ?error_log({uart_open_error, Reason}),
	    Error
    end.

message_handler(State=#state{uart=Uart,parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        {call, From, get_position} ->
	    {reply, From, {State#state.valid,State#state.lat,State#state.long}};
        {call, From, get_time} ->
	    {reply, From, {State#state.valid,State#state.time}};
        {call, From, get_date} ->
	    {reply, From, {State#state.valid,State#state.date}};
        {call, From, get_tz} ->
	    {reply, From, {State#state.valid,State#state.tz}};
        {call, From, get_speed} ->
	    {reply, From, {State#state.valid,State#state.speed}};
	{uart, Uart, Line} ->
	    uart:setopts(Uart, [{active, once}]),
	    case parse(Line) of
		{error,Reason} ->
		    %% ?error_log({nmea_parser_error, Reason}),
		    io:format("~p\n", [{nmea_parse_error, Reason}]),
		    {noreply, State};
		{ok,{<<"GPGGA">>,[FUtc,FLat,FLa,FLong,FLo,FStat|_]}} ->
		    {Time,Times} = gps_time(FUtc),
		    Lat = latitude(FLat,FLa),
		    Long = longitude(FLong,FLo),
		    Stat = try binary_to_integer(FStat) of
			       Val -> Val 
			   catch error:_ ->
				   -1
			   end,
		    if State#state.valid, Stat >= 0,
		       is_float(Times),is_float(Lat),is_float(Long) ->
			    Lat0 = State#state.lat,
			    Long0 = State#state.long,
			    Dist = flat_distance(Lat0,Long0,Lat,Long),
			    Dt = Times - State#state.times,
			    Spd = if Dt >= 0.001 ->
					  (Dist / Dt) * 3.6;
				      true ->
					   0.0
				  end,
			    {noreply,State#state{time=Time,
						 times=Times,
						 long=Long,
						 lat=Lat,
						 speed=Spd}};
		       Stat >= 0,
		       is_float(Times),is_float(Lat),is_float(Long) ->
			    {noreply,State#state{valid=true,
						 time=Time,
						 times=Times,
						 long=Long,
						 lat=Lat,
						 speed=0.0}};
		       true ->
			    {noreply,State}
		    end;
		{ok,{<<"GPZDA">>,[FUtc,FDay,FMonth,FYear,FTzH,FTzM|_]}} ->
		    {Time,Times} = gps_time(FUtc),
		    {Date,Tz} = gps_date(FDay,FMonth,FYear,FTzH,FTzM),
		    {noreply,State#state{times=Times,
					 time=Time,
					 date=Date,
					 tz=Tz}};
		{ok,_Message} ->
		    %%io:format("gps_serv: skip message ~p\n", [_Message]),
		    %%?dbg_log_fmt("gps_serv: skip message ~p\n", [_Message]),
		    {noreply, State}
	    end;
        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
	    io:format("~p\n", [{unknown_message, UnknownMessage}]),
            %% ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

%% Take a NMEA log line from file or uart ...
%% return #name_messagae or {error,Reason}

parse(<<0,Line/binary>>) ->
    parse(Line); %% Removing spurious zero
parse(Line) ->
    case binary:split(Line, <<"*">>) of
	[<<$$,Message/binary>>] ->  %% assume no checksum present
	    [ID|Fs] = binary:split(Message, <<",">>, [global]),
	    {ok,{ID, Fs}};
	[<<$$,Message/binary>>, Cs] ->
	    case verify_checksum(Message, Cs) of
		ok ->
		    [ID|Fs] = binary:split(Message, <<",">>, [global]),
		    {ok,{ID,Fs}};
		Error ->
		    Error
	    end;
	_ ->
	    {error, no_message}
    end.

verify_checksum(Fs, <<X1,X2,_/binary>>) ->
    Sum = checksum(Fs),
    try list_to_integer([X1,X2],16) of
	Sum -> 
	    ok;
	_ ->
	    {error,invalid_checksum}
    catch
	error:_ ->
	    {error, bad_checksum}
    end.

checksum(Bin) ->
    checksum(Bin, 0).

checksum(<<C,Cs/binary>>, Sum) ->
    checksum(Cs, C bxor Sum);
checksum(<<>>, Sum) ->
    Sum.

%% fixme: second fraction .ss (two decimals?)
%% return secons since midnight
gps_time(<<H1,H0,M1,M0,S1,S0,$.,D1,D0,_Bin/binary>>) ->
    {Time,Times} = gps_time(H1,H0,M1,M0,S1,S0),
    {Time,Times + list_to_float([$0,$.,D1,D0])};
gps_time(<<H1,H0,M1,M0,S1,S0,_Bin/binary>>) ->
    gps_time(H1,H0,M1,M0,S1,S0).

gps_time(H1,H0,M1,M0,S1,S0) ->
    H = (H1-$0)*10 + (H0-$0),
    M = (M1-$0)*10 + (M0-$0),
    S = (S1-$0)*10 + (S0-$0),
    {{H,M,S},float((H*24 + M)*60 + S)}.

gps_date(D,M,Y,TzH,TzM) ->
    {{binary_to_integer(Y),binary_to_integer(M),binary_to_integer(D)},
     {binary_to_integer(TzH),binary_to_integer(TzM)}}.

longitude(<<>>,_) -> undefined;
longitude(Coord,<<"W">>) -> -coord_to_deg(Coord);
longitude(Coord,<<"E">>) -> coord_to_deg(Coord);
longitude(_,_) -> undefined.

latitude(<<>>,_) -> undefined;
latitude(Coord,<<"S">>) -> -coord_to_deg(Coord);
latitude(Coord,<<"N">>) -> coord_to_deg(Coord);
latitude(_,_) -> undefined.

%% https://www.movable-type.co.uk/scripts/latlong.html
%% convert (d)ddmm.mmmm -> d(dd) + mm.mmmm/60
coord_to_deg(Min = <<_,$.,_/binary>>) ->
    binary_to_float(Min)/60;
coord_to_deg(Min = <<_,_,$.,_/binary>>) ->
    binary_to_float(Min)/60;
coord_to_deg(Coord = <<D1,_,_,$.,_/binary>>) ->
    <<_,Min/binary>> = Coord,
    (D1-$0)+binary_to_float(Min)/60;
coord_to_deg(Coord = <<D1,D2,_,_,$.,_/binary>>) ->
    <<_,_,Min/binary>> = Coord,    
    (D1-$0)*10+(D2-$0)+binary_to_float(Min)/60;
coord_to_deg(Coord = <<D1,D2,D3,_,_,$.,_/binary>>) ->
    <<_,_,_,Min/binary>> = Coord,
    (D1-$0)*100+(D2-$0)*10+(D3-$0)+binary_to_float(Min)/60;
coord_to_deg(_) -> undefined.


%% return distancs in meters
-ifdef(not_used).

distance(Lat1,Long1,Lat2,Long2) ->
    Phi1 = deg_to_rad(Lat1),
    Phi2 = deg_to_rad(Lat2),
    DPhi = deg_to_rad(Lat2-Lat1),
    DLam = deg_to_rad(Long2-Long1),
    SinDPhi2 = math:sin(DPhi/2),
    SinDLam2 = math:sin(DLam/2),
    A = SinDPhi2*SinDPhi2 +
	math:cos(Phi1)*math:cos(Phi2)*SinDLam2*SinDLam2,
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1-A)),
    ?EARTH_RADIUS * C.

-endif.

%% return distancs in meters
flat_distance(Lat1,Long1,Lat2,Long2) ->
    Phi1 = deg_to_rad(Lat1),
    Phi2 = deg_to_rad(Lat2),    
    Lam1 = deg_to_rad(Long1),
    Lam2 = deg_to_rad(Long2),
    X = (Lam2 - Lam1) * math:cos((Phi1+Phi2)/2),
    Y = (Phi2 - Phi1),
    ?EARTH_RADIUS * math:sqrt(X*X + Y*Y).

deg_to_rad(Deg) ->
    Deg*(math:pi()/180).

