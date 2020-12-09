%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Parse and process GPS position data over NMEA 0183 module
%%% @end
%%% Created :  9 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(gps_serv).

-export([start_link/0, start_link/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

-record(state,
	{
	 parent,
	 uart,
	 valid=false,
	 long = 0.0,
	 lat = 0.0,
	 time = 0.0,
	 speed = 0.0
	}).

-define(EARTH_RADIUS, 6371000.0).  %% radius in meters

start_link() ->
    start_link("/dev/ttyAMA").

start_link(UartDeviceName) ->
    ?spawn_server(fun(Parent) -> init(Parent, UartDeviceName) end,
		  fun ?MODULE:message_handler/1).

init(Parent, UartDeviceName) ->
    Baud = 9600,
    UartOpts = [{mode,binary}, {baud, Baud}, {packet, line},
		{csize, 8}, {stopb,1}, {parity,none}, {active, once}],
    case uart:open1(UartDeviceName, UartOpts) of
	{ok,Uart} ->
	    {ok, #state { parent=Parent, uart=Uart }};
	Error = {erro,Reason} ->
	    ?error_log({uart_open_errro, Reason}),
	    Error
    end.

message_handler(State=#state{uart=Uart,parent=Parent}) ->
    receive
        {call, From, stop} ->
            {stop, From, ok};
        {call, From, position} ->
	    {reply, From, {State#state.long, State#state.lat}};
	{uart, Uart, Line} ->
	    case parse(Line) of
		{error,Reason} ->
		    ?error_log({nmea_parser_error, Reason}),
		    {noreply, State};
		{<<"GPGGA">>,[Utc,Lat,La,Long,Lo,Stat|_]} ->
		    Time2 = gps_utc(Utc),
		    Lat2 = latitude(Lat,La),
		    Long2 = longitude(Long,Lo),
		    Stat2 = try binary_to_integer(Stat) of
				Val -> Val 
			    catch error:_ ->
				    -1
			    end,
		    if State#state.valid, Stat2 >= 0,
		       is_float(Time2),is_float(Lat2),is_float(Long2) ->
			    Lat1 = State#state.lat,
			    Long1 = State#state.long,
			    Dist2 = flat_distance(Lat1,Lat2,Long1,Long2),
			    Dt = Time2 - State#state.time,
			    Spd2 = if Dt >= 0.001 ->
					   (Dist2 / Dt) * 3.6;
				      true ->
					   0.0
				   end,
			    {noreply,State#state{time=Time2,
						 long=Long2,
						 lat=Lat2,
						 speed=Spd2}};
		       Stat2 >= 0,
		       is_float(Time2),is_float(Lat2),is_float(Long2) ->
			    {noreply,State#state{valid=true,
						 time=Time2,
						 long=Long2,
						 lat=Lat2,
						 speed=0.0}};
		       true ->
			    {noreply,State}
		    end;
		_Message ->
		    ?dbg_log_fmt("gps_serv: skip message ~p\n", [_Message]),
		    uart:setopts(Uart, [{active, once}]),
		    {noreply, State}
	    end;
        {'EXIT', Parent, Reason} ->
	    exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
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


%% return gps_utc in float seconds since 1970
gps_utc(<<H1,H0,M1,M0,S1,S0,Bin/binary>>) ->
    {Date,_Time} = calendar:universal_time(),
    Time = {(H1-$0)*10 + (H0-$0),
	    (M1-$0)*10 + (M0-$0),
	    (S1-$0)*10 + (S0-$0)},
    DateTime = {Date,Time},
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
    case Bin of
	<<$.,_/binary>> ->
	    Seconds + binary_to_float(<<$0,Bin/binary>>);
	<<>> ->
	    float(Seconds)
    end.

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

