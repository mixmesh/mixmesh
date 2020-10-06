# TODO
* Check how pre-population of buffers is done best, now broken in simualtor_serv.erl
* Start using reltool/servator create releases with boot scripts and ez files etc (ez files are not really needed since we have various other stuff like nifs etc)

---

## Possible input to new TODO (pseudo random order):

## Demo hardware
* Raspberry pi zero W/H
* Use pisuger2 battery that can read battery status and
* Power-up rpi on progammable RTC, can possibly save som power
* Membrane numeric keypad (with i2c) support for pincode entries
 Somthing like <https://www.seeedstudio.com/Sealed-Membrane-3x4-button-pad-with-sticker-p-1044.html> but with i2c interface?
* Total wipe BUTTON (under a easy to flip cover)
* LEDS
.* activity, indicate how many node buddies we see
.* battery status (5 leds) (- o o o o o +)
.* app status RED (down) YELLOW (start) GREEN (running)
.* bluetooth status  BLUE (connection runnning),
maybe BUTTON to terminate connection
* Bluetooth connection
.* none-routable (enough with no dns option?) with dhcpd.
.* Test bluetooth connection from Android and iOS.
* USB ether connection
.* usb connection, none-routable (enough with no dns option?)
* DHCPD for more than one interface
 both interface, pan0 and wlan0, must be able to hand out ONE address
 to ONE device a the time (sequrity?) 
 ) maybe fix the erlang implementation...?
 
## WEB ui
* use inets or exo/exo\_http\_server to serv REST api and some web pages.
* configure bluetooth interface (on/off)
* configure import/export/delete and show public keys, both yours and others.

## Fixes needed
* dialyzer, start using verify&sign, reencryption and padding.
* Iron out the box installation procedure of the box in detail
* Read/write public keys from PKI server?
..From a local only non-networking PKI server (as it is today) per player
* How should we import new keys into A1? An app? Mounted disk? How?
.* Examplel simple web ui, import export show delete
.* Export secret key once at startup
* Make players message buffers persistent!
* Look into message fullness behaviour during message exchange
* Do not reconnect with a neighbour player until a certain amount of time has elapsed (even though a player reappears) (nodis)
* Re-exchange messages with persistent neighbour players after a certain time (nodis)

## Red line
* Nudge message box parameters. K and F? Message buffer size? What is the max message size?
  1) Keep public key database encrypted on disk somehow
  2) From a central SSL based PKI-server over the net
     (The box must use the phone's Internet connection. How?)
  3) From a PKI server introduced as a hidden service on Tor
     (The box must use the phone's Internet connection. How?)
* How do we handle DOS attacks?
* Wait until the message buffer is full until players start to exchange message (maybe just create dummy messages galore)
* Move the players private key out from *.conf files and PIN-encode it
* Add SSL to SMTP and POP3 servers
B) Support a hybrid encryption approach in order to support media streams
* Add resend/ack/seqnumber etc to better support B?
* Write bridge servers as hidden services on Tor to bridge traffic between different geograophic islans of Obscrete nets (jocke*nyc@gmail.com) (The box must use the phone's Internet connection. How?)
* Talk to a campus and ask them if we can install our boxes in their attics, say 20 boxes on KTH. A 500kr each.