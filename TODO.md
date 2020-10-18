# TODO

## Possible input to new TODO (pseudo random order):

## Next week

### Tony

* Bluetooth connection
  none-routable (enough with no dns option?) with dhcpd.
* USB ether connection
  usb connection, none-routable (enough with no dns option?)
* Test bluetooth connection from Android and iOS.
* document all REST commands with examples!
* Clean elgamal.erl from obsolete comments and remove non-used functions

### Joakim

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
* configure import/export/delete and show public keys, both yours and others.

## Fixes needed

* packet mode for player_sync. Is packet 2 enough or do we need packet 4?
* Iron out the box installation procedure of the box in detail
* Read/write public keys from PKI server?
* How should we import new keys? An app? Mounted disk? How?
.* Example simple web ui, import export show delete
.* Export secret key once at startup

## Buffer session

* Model way of message exchange.
* Wait until the message buffer is full? until players start to exchange message (maybe just create dummy messages galor)
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
