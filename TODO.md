# TODO

## Possible input to new TODO (pseudo random order):

[* REST: /system-hardware: gps, keyboard, battery]
[* GPS service ]
[* PIN entry service]
[* Battery service]

XBus public keys for above

     Key					Unit
     mixmesh.routing.type			hw|static|blind
     mixmesh.routing.hw.location.latitude	degree
     mixmesh.routing.hw.location.longitude	degree
     mixmesh.routing.hw.speed			km/h	

     mixmesh.routing.static.location.latitude	degree  fixed
     mixmesh.routing.static.location.longitude	degree  fixed
     mixmesh.routing.static.speed		km/h	0.0

     mixmesh.routing.blind.location.latitude	degree	0.0
     mixmesh.routing.blind.location.longitude	degree	0.0
     mixmesh.routing.blind.speed		km/h	0.0

     subscribe to: mixmesh.routing.*.location to get long and lat

     mixmesh.system.enabled			boolean
     mixmesh.system.pin				boolean

     mixmesh.keypad.installed		      boolean
     mixmesh.keypad.pinlength		      unsigned
     mixmesh.keypad.enterkey		      unsigned
     
     mixmesg.battery.installed			boolean
     mixmesh.battery.charging			boolean
     mixmesh.battery.voltage			V	(3.1-4.16)
     mixmesh.battery.intensity			V	(3.1-4.16)     
     mixmesh.battery.soc			%	(0-100)

     mixmesh.battery.is_power_plugged		boolean
     

## Next week

* Discuss "edit-config" in REST

  1) We need to decide which patameters in obscrete.config which should be set to reloadable=true

  2) Jag har väntat på att göra det möjligt att sätta om smtp/pop3/http portar från system settings i web appen. Det går att göra det via REST mha av "edit-config" men inget lär hända mtp (1).

* Discuss location routing:

  0) Let each player send a messages to all contacts with its position
     (or habitat) each hour.
  1) Maybe let each player have and short lived elegamal key pair (recreate it each then minutes)

### Tony

* servator release guide
* build a disk image to simplify everything, with a factor of 1000000
* select ip from ping interface, convert (0,0,0,0) => (A,B,C,D)
  depending on nodis ip.
* Add GPS coordinates to nodis
* Update configuration to store 
  buffer, local-pki, received-messages, spooler
  in /var/erlang/obscrete/

### Joakim

[* REST: /system-hardware: gps, keyboard, battery]

* Do a refresh of Rome simulation + add a background map
[* Add a square2 simulator plugin module based on smooth random walks (Perlin noise)]
* obscrete.conf-running does not handle updated obscrete.conf!

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
