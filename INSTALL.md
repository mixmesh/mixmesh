# Install mixmesh from scratch on raspberry pi Z

Here are all the steps necessary to install mixmesh
on a raspberry pi Z and get it running. You could for example
go here and buy a kit and you will get raspberry pi Z, box
and memory card a power adapter and some cables for 469 SEK:

    https://www.kjell.com/se/produkter/dator/raspberry-pi/raspberry-pi-zero-wifi-kit-enkortsdator-p88051

## Download the base image

Now you must down load an image and put it onto a SD card, the
box above comes with a 16GB card class 10.

    https://www.raspberrypi.org/

Download for example this image, or use a image managaer from rasperrypi.org


	https://downloads.raspberrypi.org/raspios_lite_armhf_latest

It is about 500 Mb. The downloaded image may be called

	2020-08-20-raspios-buster-armhf-lite.zip

So unpackit using unzip

	unzip 2020-08-20-raspios-buster-armhf-lite.zip

## Transfer the image to SD card

Now dd the image onto the inserted SD card following this guide

	https://www.raspberrypi.org/documentation/installation/installing-images/linux.md

Or (linux) locate the the device /dev/sdX (X is the name it got)
[Joakim: On my Ubuntu 20.04 LTS laptop the device was named /dev/mmcblk0]

	sudo umount /dev/sdX1
	sudo dd bs=4M if=2020-08-20-raspios-buster-armhf-lite.img of=/dev/sdX conv=fsync status=progress
	sudo sync

Remove the card and reinsert it in again.

## Prepare the image for start

Find the directory where the images /boot and /rootfs are located. Normally
under /media/<user>/rootfs  and /media/<user>/boot

### Enable ssh

	sudo touch boot/ssh

Creates a file that enable ssh server login. Then we must
make raspberry pi connect to your wifi, by updating the file
/etc/wpa\_supplicant/wpa\_supplicant.conf

### Configure WiFi

Add the contents, fill in your data in

    sudo emacs rootfs/etc/wpa_supplicant/wpa_supplicant.conf

add the lines

    network={
        ssid="Your-Home-network-Name-Here"
        psk="Your-Home-Network-Password-Here"
        key_mgmt=WPA-PSK
	}

## Boot and login

Save and sync (program sync) eject the card and insert it into the
Raspberry pi. When the LED has a steady green glow after a minuit
or so try login

	ssh pi@raspberrypi.local

default password is "raspberry"

## Install software needed

Hopefully you have a promt, if not, check password, SSID, login to the router
and find the IP of the raspberry pi and use that instead of reaspberrypi.local

### Update with the latest fixes

	sudo apt update
	sudo apt upgrade

Install needed packages

	sudo apt install git wget emacs-nox isc-dhcp-server bluez-tools libncurses-dev libssl-dev libgmp-dev libsodium-dev screen pulseaudio libasound2-dev libopus-dev libsbc-dev libudev-dev python3-pip pulseaudio-module-bluetooth

### Reconfiogure PulseAudio

Setup pulseaudio to run as a system daemon according to https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/SystemWide/

        sudo systemctl --global disable pulseaudio.service pulseaudio.socket
        sudo usermod -a -G audio pulse
        sudo usermod -a -G bluetooth pulse
        sudo usermod -a -G pulse-access pi
        sudo usermod -a -G pulse-access root

Make sure that these modules are loaded in /etc/pulse/system.pa (add them last to the file):

        # Updated by Mixmesh
        load-module module-bluetooth-policy
        load-module module-bluetooth-discover autodetect_mtu=yes
        load-module module-switch-on-connect
        load-module module-dbus-protocol

Add a systemd script to start PulseAudio as a system daemon:

        sudo cat > /etc/systemd/system/pulseaudio.service
[Unit]
Description=PulseAudio Daemon

[Install]
WantedBy=multi-user.target

[Service]
Type=simple
PrivateTmp=true
ExecStart=/usr/bin/pulseaudio --system --realtime --disallow-exit --no-cpu-limit
<Ctrl-D>

        sudo systemctl enable pulseaudio
        sudo systemctl start pulseaudio

### Patch the BCM chip

Patch the BCM chip to enable the bleutooth headset microphone as described in http://youness.net/raspberry-pi/how-to-connect-bluetooth-headset-or-speaker-to-raspberry-pi-3

        sudo cp mixmesh/bin/rc.local /etc/rc.local

### Pair a bluetooth headeset

pi@black:~ $ bluetoothctl 
Agent registered
[CHG] Controller B8:27:EB:61:F9:D0 Pairable: yes
[bluetooth]# pair 20:74:CF:C4:F4:A0
[bluetooth]# connect 20:74:CF:C4:F4:A0
Attempting to connect to 20:74:CF:C4:F4:A0
[OpenMove by AfterShokz]# trust 20:74:CF:C4:F4:A0
[CHG] Device 20:74:CF:C4:F4:A0 Trusted: yes
Changing 20:74:CF:C4:F4:A0 trust succeeded
[OpenMove by AfterShokz]# 

Note: Use "scan on" to figure out the device address of the bluetooth headset

### Install Erlang

Now download Erlang, unpack, configure, make and install

	mkdir src
	cd src
	wget https://github.com/erlang/otp/releases/download/OTP-X.Y.Z/otp_src_X.Y.Z.tar.gz
	tar xf otp_src_X.Y.Z.tar.gz
        cd otp_src_X.Y.Z/
	./configure
	make
	sudo make install
    sudo setcap 'cap_net_bind_service=+ep' /usr/local/lib/erlang/erts-11.1.4/bin/beam.smp

Add erlang libraries to your path

        cd
	emacs .bashrc

Append the lines

	export ERL_LIBS=$HOME/erlang
	export ERL_CRASH_DUMP_SECONDS=0
	export EDITOR=emacs

### Install Mixmesh

Fetch the top-level application

	mkdir erlang
	cd erlang
        git clone https://github.com/mixmesh/mixmesh

And make a link to the top-level makefile

	ln -s mixmesh/Makefile.top-level Makefile

Now we can clone other applications need and build everything

	make clone
	make

### Install vosk

    cd vosk/priv
	make py_install
	make copy_so
	make download

### Prepare dbus

    Start Erlang and run dbus:setup/0 (ERL_LIBS must be set)

    erl
    dbus:setup().
    
# (PiMesh)

For pimesh (MixMesh on Raspberry pi) you need the following packages
in the mixmesh directory.

	git clone https://github.com/tonyrog/gpio
	git clone https://github.com/tonyrog/pwm
	git clone https://github.com/tonyrog/i2c
	git clone https://github.com/tonyrog/uart
	git clone https://github.com/mixmesh/pimesh

### Setup configuration

    cd mixmesh
	sudo mkdir -p /etc/erlang/mixmesh
	sudo chown pi:pi /etc/erlang/mixmesh
	sudo mkdir -p /var/erlang/mixmesh
	sudo chown pi:pi /var/erlang/mixmesh
	./bin/mixmesh --self-signed-ssl-cert > cert.pem
	./bin/mkconfig /etc/erlang/mixmesh cert.pem mother
        sed 's#/home/pi/mixmesh#/etc/erlang/mixmesh#g' ./etc/mother.conf > ./etc/mother-local.conf

NOTE: Here we use the mother config file. YMMV.

### Set hardware

Edit the configuration created above and change the
hardware from 'none' to 'pimesh'

	"system": { ... "hardware": "pimesh" }

### Start application

	./bin/mixmesh --config ./etc/mother-local.conf

or start in a screen session

        screen
	./bin/mixmesh --config ./etc/mother-local.conf
        C-a d (to detach from screen)

to re-attach to a running screen session

        screen -r

        C-a c (to create a new shell)
        C-a " (to switch between shells)

# Make a servator release and install it

Servator can create releases from a running system, 
extracting the applications and start arguments from
the node it self

Get servator

    git clone https://github.com/tonyrog/servator
    cd servator
    make
	
Go to mixmesh directory and start the node

    cd mixmesh
    ./bin/mixmesh --config ./etc/mother-local.conf
    (mixmesh@localhost)1> servator:make_release(mixmesh).
	
Or if the release tag is not correct

    (mixmesh@localhost)1> servator:make_release(mixmesh, "x.y.z", release).
	
This will create a release directory

    mixmesh-X.Y/
	
To install that release do

    cd mixmesh-X.Y
    ./install.sh
    sudo setcap 'cap_net_bind_service=+ep' /var/erlang/mixmesh/erts-11.1/bin/beam.smp

Now copy the configure file to the final place

    cd mixmesh
    cp ./etc/mixmesh-local.conf /etc/erlang/mixmesh/mother-local.conf
	
Edit the /etc/erlang/mixmesh/mixmesh.run script to add the 
applcation specific optionfor parsing the json config files. 
Search for OPTS and set it.

    OPTS="--config $ETC/mother-local.conf"
	
IMPORTANT updated for systemd

In the mixmesh.run find and replace -detached with -noinput!

Now we should be able to start mixmesh the standard way

    /etc/erlang/mixmesh/mixmesh.run start

And check status

    /etc/erlang/mixmesh/mixmesh.run status

Maybe stop sometime

    /etc/erlang/mixmesh/mixmesh.run stop

# Install Systemd script

	cd mixmesh
    sudo cp ./etc/mixmesh.service /etc/systemd/system/
	sudo chmod u+rwx /etc/systemd/system/mixmesh.service

Enable the service, so it is run when machine is booting


	sudo systemctl enable mixmesh

Start it now

	sudo systemctl start mixmesh

Stop it now

	sudo systemctl stop mixmesh
