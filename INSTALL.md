# Install obscrete from scratch on raspberry pi Z

Here are all the steps necessary to install obscrete
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

	umount /dev/sdX1
	dd bs=4M if=2020-08-20-raspios-buster-armhf.img of=/dev/sdX conv=fsync
	sync

Remove the card and reinsert it in again.

## Prepare the image for start

Find the directory where the images /boot and /rootfs are located. Normally
under /media/<user>/rootfs  and /media/<user>/boot

### enable ssh

	sudo touch boot/ssh

Creates a file that enable ssh server login. Then we must
make raspberry pi connect to your wifi, by updating the file
/etc/wpa\_supplicant/wpa\_supplicant.conf

### update wpa\_supplicant

Add the contents, fill in your data

    sudo emacs rootfs/etc/wpa_supplicant/wpa\_supplicant.conf

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

### update with the lastest fixes

	sudo apt update
	sudo apt upgrade

 Install needed packages

	sudo apt install git wget emacs-nox isc-dhcp-server bluez-tools libncurses-dev libssl-dev libgmp-dev libsodium-dev

### install Erlang (fixme used patched version)

Now download Erlang, unpack, configure, make and install

	mkdir src
	cd src
	wget http://erlang.org/download/otp_src_23.1.tar.gz
	tar xf otp_src_23.1.tar.gz
        cd otp_src_23.1/
	./configure
	make
	sudo make install

Add erlang libraries to your path

	emacs .bashrc

Append the lines

	export ERL_LIBS=$HOME/erlang
	export ERL_CRASH_DUMP_SECONDS=0
	export EDITOR=emacs

### Install Obscrete

Fetch the top-level application

	mkdir erlang
	cd erlang
        git clone https://<your-username>@github.com/obscrete/obscrete

Update GITURL in obscrete/Makefile.top-level

        GITURL = https://<your-username>@github.com/obscrete

And make a link to the top-level makefile

	ln -s obscrete/Makefile.top-level Makefile

Now we can clone other applications need and build everything

	make clone
	make

### setup configuration

    cd obscrete
	sudo mkdir -p /etc/erlang/obscrete
	sudo chown pi:pi /etc/erlang/obscrete
	./bin/obscrete --self-signed-ssl-cert > cert.pem
	./bin/mkconfig /etc/erlang/obscrete cert.pem alice

### start application

        sed 's#/tmp/obscrete#/etc/erlang/obscrete#g' ./etc/obscrete.conf > ./etc/obscrete-local.conf
	./bin/obscrete --config ./etc/obscrete-local.conf

or start in a screen session

        screen
	./bin/obscrete --config ./etc/obscrete-local.conf
        C-a d (to detach from screen)

to re-attach to a running screen session

        screen -r

        C-a c (to create a new shell)
        C-a " (to switch between shells)
