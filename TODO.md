# TODO

## Possible input to new TODO (pseudo random order):

[* REST: /system-hardware: gps, keyboard, battery]
[* GPS service ]
[* PIN entry service]
[* Battery service]

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

## Next week

* Discuss "edit-config" in REST

  1) We need to decide which parameters in obscrete.config which
     should be set to reloadable=true and handle accordingly

  2) Jag har väntat på att göra det möjligt att sätta om
     smtp/pop3/http portar från system settings i web appen. Det går
     att via REST mha av "edit-config" men inget lär hända mtp (1).  

### Tony

* Handle eremoteio if someone insert gadget USB cable while charging!
* servator release guide
* build a disk image to simplify everything, with a factor of 1000000
* select ip from ping interface, convert (0,0,0,0) => (A,B,C,D)
  depending on nodis ip.
* Add GPS coordinates to nodis
* Update configuration to store 
  buffer, local-pki, received-messages, spooler
  in /var/erlang/obscrete/

### Joakim

* Run a bunch of statistics test with and without gps-routing
  * Delay until messages arrives
  * Total number of messages forwarded
  * Number of lost messages

* Test background maps (only epx renderer)
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
* If the box use a central PKI server over TOR or SSL it must tether on the phone Intenet connection. How?
* Support a hybrid encryption approach in order to support media streams
* Add resend/ack/seqnumber etc to better support B?
* Write bridge servers as hidden services on Tor to bridge traffic between different geograophic islans of Obscrete nets (jocke*nyc@gmail.com) (The box must use the phone's Internet connection. How?)
* Talk to a campus and ask them if we can install our boxes in their attics, say 20 boxes on KTH. A 500kr each.
