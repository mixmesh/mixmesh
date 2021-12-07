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
	sudo dd bs=4M if=2020-08-20-raspios-buster-armhf-lite.img of=/dev/sdX conv=fsync
	sudo sync

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

	sudo apt install git wget emacs-nox isc-dhcp-server bluez-tools libncurses-dev libssl-dev libgmp-dev libsodium-dev screen

### install Erlang

Now download Erlang, unpack, configure, make and install

	mkdir src
	cd src
	wget http://erlang.org/download/otp_src_X.Y.Z.tar.gz
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

# (PiMesh)

For pimesh (MixMesh on Raspberry pi) you need the following packages
in the mixmesh directory.

	git clone https://github.com/tonyrog/gpio
	git clone https://github.com/tonyrog/pwm
	git clone https://github.com/tonyrog/i2c
	git clone https://github.com/tonyrog/tree_db
	git clone https://github.com/tonyrog/xbus
	git clone https://github.com/tonyrog/uart
	git clone https://github.com/mixmesh/pimesh

### setup configuration

    cd mixmesh
	sudo mkdir -p /etc/erlang/mixmesh
	sudo chown pi:pi /etc/erlang/mixmesh
	sudo mkdir -p /var/erlang/mixmesh
	sudo chown pi:pi /var/erlang/mixmesh
	./bin/mixmesh --self-signed-ssl-cert > cert.pem
	./bin/mkconfig /etc/erlang/mixmesh cert.pem alice
        sed 's#/tmp/mixmesh#/etc/erlang/mixmesh#g' ./etc/mixmesh.conf > ./etc/mixmesh-local.conf

### set hardware

Edit the configuration created above and change the
hardware from 'none' to 'pimesh'

	"system": { ... "hardware": "pimesh" }

### start application

	./bin/mixmesh --config ./etc/mixmesh-local.conf

or start in a screen session

        screen
	./bin/mixmesh --config ./etc/mixmesh-local.conf
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
    ./bin/mixmesh --config ./etc/mixmesh-local.conf
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
    cp ./etc/mixmesh-local.conf /etc/erlang/mixmesh/mixmesh-local.conf
	
Edit the /etc/erlang/mixmesh/mixmesh.run script to add the 
applcation specific optionfor parsing the json config files. 
Search for OPTS and set it.

    OPTS="--config $ETC/mixmesh-local.conf"
	
IMPORTANT updated for systemd

In the mixmesh.run find and replace -detached with -noinput!

Now we should be able to start mixmesh the standard way

    /etc/erlang/mixmesh/mixmesh.run start

And check status

    /etc/erlang/mixmesh/mixmesh.run status

Maybe stop sometime

    /etc/erlang/mixmesh/mixmesh.run stop

# install Systemd script

	cd mixmesh
    sudo cp ./etc/mixmesh.service /etc/systemd/system/
	chmod chmod u+rwx /etc/systemd/system/mixmesh.service
	
Enable the service, so it is run when machine is booting

	
	sudo systemctl enable mixmesh
	
Start it now

	sudo systemctl start mixmesh

Stop it now

	sudo systemctl stop mixmesh

	
