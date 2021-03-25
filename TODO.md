# TODO

* (J) Pincode, where is pincode used? and why?

* mixmesh start, add hardware, simulation shared keys etc

* update player_sync_serv to use player_decrypt_serv

* messure box HxBxD (lid indent dimensions)

	+ Keyboard magic hole mask
	+ Fan/Cooling holes
	* Buttons holes ? USB or not?
	+ Power On/Off button
	* Power Down button (COL8)
	* Wipe/Dead man's grip Button (COL9)
	+ USB power (piSuger2)
	+ USB ethernet


XBus public keys for above

     Key					Unit
     mixmesh.routing.type			hw|static|blind
     mixmesh.routing.static.location.latitude	degree  fixed
     mixmesh.routing.static.location.longitude	degree  fixed
     mixmesh.routing.static.speed		km/h	0.0

     mixmesh.routing.blind.location.latitude	degree	0.0
     mixmesh.routing.blind.location.longitude	degree	0.0
     mixmesh.routing.blind.speed		km/h	0.0

     subscribe to: mixmesh.routing.*.location to get long and lat

     mixmesh.system.enabled		boolean
     mixmesh.system.pin			boolean

     mixmesh.keypad.installed		boolean
     mixmesh.keypad.pinlength		unsigned
     mixmesh.keypad.enterkey		unsigned
     mixmesg.battery.installed		boolean

## Next week (possible discussion topics)

* We may need to decide which parameters in mixmesh.config which
  should be set to reloadable=true and handle accordingly. Jag har t
  ex väntat på att göra det möjligt att sätta om smtp/pop3/http portar
  från system settings i web appen. Det går att göra via REST mha av
  "edit-config" men inget lär hända. :-)

* Från player_serv.erl:
  %% TONY: Jag tror detta beror på att player_server_sync.erl
  %% dör på fel sätt.
  %% Sök efter "%% TONY: Should we die here? Probably not?" i
  %% player_sync_serv.erl 

* Från player_serv.erl:
  %% Note: RandomIndices may be so few that
  %% SuitableIndices ++ RandomIndices is less than N.
  %% Is this really the behaviour want?

### Tony

* Handle eremoteio if someone insert gadget USB cable while charging!
* servator release guide
* build a disk image to simplify everything, with a factor of 1000000
* select ip from ping interface, convert (0,0,0,0) => (A,B,C,D)
  depending on nodis ip.
* Add GPS coordinates to nodis
* Update configuration to store 
  buffer, local-keydir, received-messages, spooler
  in /var/erlang/mixmesh/

### Joakim

* Measure performance

  Simulator setup:

    * Buffer size: 1000
    * F: 0.2
    * K: 10
    * Each player sends a message each fourth minute to player p1 (!!)
    * The target player is positioned pang in the middle of the map
    * 20 players performs random walk on a 600x600m area
    * Average player speed 3m/s (10 km/h)
    * 75 meters neighbour distance
    * NOTE: Use mixmesh/etc/simulator-realistic.conf
    
  Note: The above parameters should be varied as well at some time

  Each 30 seconds this kind of report is printed on stdout:

  GPS-routing:

  ScaleFactor 1: (routing, normal speed)

  Simulator run time: 88.5518 minutes
  Scaled simulator run time: 88.5518 minutes
  Created messages: 1127 (11270)
  Delivered messages: 1029
  Relayed messages: 1984200
  Overwritten messages: 10683
  Delivery rate: 0.913
  Average delivery delay: 4.8608 minutes
  Mean delivery delay: 3.0957 minutes

  ScaleFactor 10 (routing):

  Simulator run time: 8.5513 minutes
  Scaled simulator run time: 85.5127 minutes
  Created messages: 1078 (10780)
  Delivered messages: 876
  Relayed messages: 1142000
  Overwritten messages: 10268
  Delivery rate: 0.8126
  Average delivery delay: 8.7255 minutes
  Mean delivery delay: 5.8655 minutes

  ScaleFactor 10 (blind):

  Simulator run time: 8.5516 minutes
  Scaled simulator run time: 85.5157 minutes
  Created messages: 1078 (10780)
  Delivered messages: 935
  Relayed messages: 1195200
  Overwritten messages: 10151
  Delivery rate: 0.8673
  Average delivery delay: 8.7609 minutes
  Mean delivery delay: 5.9307 minutes

  Next step (maybe):
    * Why is GPS routing performing so bad. Most probably because
      random_walk is too syntehtic an example. Maybe we need to take
      one step back and think on the bigger picture?

* Test the Rio buses simulation
[* REST: /system-hardware: gps, keyboard, battery]

### Both

* Player får privat nyckel som meddelande.
* Ska locked device vara message realy?
* Prata om en möjlig avnästlad lagring av config i app env.
* exchanging messages may be good a strategy when is comes to clients.
  that receive messages by never send (buggy or strange) or sending.
  messages but never receives.
* Test bluetooth connection, various clients.
* Factory reset, Initiate box!
* Kryptera kontakter med privat nyckel istället för med stretchad PIN.
* Kryptera privat nyckel (kolla i alla fall).

## Demo hardware
* Raspberry pi zero W/H
* Use pisuger2 battery that can read battery status and
  power-up rpi on progammable RTC, can possibly save som power
  (Ordered 2020-10-14)
* Membrane numeric keypad (with i2c) support for pincode entries
 Somthing like <https://www.seeedstudio.com/Sealed-Membrane-3x4-button-pad-with-sticker-p-1044.html> but with i2c interface?
* Total wipe BUTTON (under a easy to flip cover)
* LEDS
.* activity, indicate how many node buddies we see
.* battery status (5 leds) (- o o o o o +)
.* app status RED (down) YELLOW (start) GREEN (running)
.* bluetooth status  BLUE (connection runnning),
maybe BUTTON to terminate connection

## WEB ui
* configure bluetooth interface (on/off)

## Fixes needed

* Iron out the box installation procedure of the box in detail

## Buffer session

* Model way of message exchange.
* Look into message fullness behaviour during message exchange
* Message buffer size?
* May node have different max buffer size and parameters K and F?

## Red line

* Add scram-sha1 password digest support to POP3/SMTP servers
* Make it possible to have serveral mail accounts on a single box. Maybe.
* Move digested passwords from config file into a passwd file on disk (especially if we support several mail accounts on a single box). Use Argon2 or just encrypt the whole passwd file using secretbox or whatever.
* Add delays between login attempts to SMTP/POP3 servers (Argon2 hashing is to heavy)
* If the box use a central Keydir server over TOR or SSL it must tether on the phone Intenet connection. How?
* Support a hybrid encryption approach in order to support media streams
* Add resend/ack/seqnumber etc to better support B?
* Write bridge servers as hidden services on Tor to bridge traffic between different geograophic islans of Mixmesh nets (jocke*nyc@gmail.com) (The box must use the phone's Internet connection. How?)
* Talk to a campus and ask them if we can install our boxes in their attics, say 20 boxes on KTH. A 500kr each.
