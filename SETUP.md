# Configure and setup the obsecrete computer

# SD card & enable interfaces

Resize the SD card to use all of the space.

    7 A1

Enable i2c interface.

    5 P5

Disable login over serial port but enable serial port hardware.

	5 P6

Setup locale

    4

Reboot

## edit the file /boot/config.txt

    dtoverlay=dwc2

also check that /boot/config.txt contains the line

	enable_uart=1

and also check that 

    /dev/serial0 

exists	

## check/update /etc/modules (or set in /boot/cmdline.txt )

	dwc2
	g_ether
	i2c-dev

# Network interfaces

Config file /etc/network/interfaces

    # Include files from /etc/network/interfaces.d:
    source-directory /etc/network/interfaces.d

    auto lo
    iface lo inet loopback

    # ad-hoc wifi (ipv6 only)
    auto wlan0
    #iface wlan0 inet dhcp
    allow-hotplug wlan0
    iface wlan0 inet static	
       address 192.168.2.245
       netmask 255.255.255.0
       gateway 192.168.2.1
       wpa-conf /etc/wpa_supplicant/wpa_supplicant.conf       
       dns-nameservers 192.168.2.1

    # iface wlan0 inet6 static
    #      wireless-channel 1
    #      wireless-essid ERLGAMAL
    #      wireless-mode ad-hoc

    # usb cabel (ipv4 network)
    auto usb0
    iface usb0 inet static
      address 10.1.3.1
      netmask 255.255.255.0

    auto pan0
    allow-hotplug pan0
    iface pan0 inet static
      address 10.1.4.1
      netmask 255.255.255.0

On the target the dhcp client MUST be stopped!

    sudo systemctl stop dhcpcd.service

# dhcpd

Config for /etc/dhcp/dhcpd.conf

    option domain-name "obscrete.org";

    default-lease-time 600;
    max-lease-time 7200;
    ddns-update-style none;

    authoritative;

    # usb0 network
    subnet 10.1.3.0 netmask 255.255.255.0 {
    #  option routers 10.1.3.1;
       option subnet-mask 255.255.255.0;
       range 10.1.3.2 10.1.3.20;
       interface usb0;
    }

    # pan0 subnet
    subnet 10.1.4.0 netmask 255.255.255.0 {
    #  option routers 10.1.4.1;
       option subnet-mask 255.255.255.0;
       range 10.1.4.2 10.1.4.20;
       interface pan0;
    }
	
# bluetooth

Update following files

/etc/systemd/network/pan0.netdev (FIXME, maybe remove)

	[NetDev]
	Name=pan0
	Kind=bridge


/etc/systemd/network/pan0.network

    [Match]
	Name=pan0


/etc/systemd/system/bt-agent.service

	[Unit]
	Description=Bluetooth Auth Agent

	[Service]
	ExecStart=/usr/bin/bt-agent -c NoInputNoOutput
	Type=simple

	[Install]
	WantedBy=multi-user.target
	
/etc/systemd/system/bt-network.service	

    [Unit]
	Description=Bluetooth NEP PAN
	After=pan0.network
	
	[Service]
	ExecStart=/usr/bin/bt-network -s nap pan0
	Type=simple
     
	[Install]
	WantedBy=multi-user.target
	
Then run

	sudo systemctl enable systemd-networkd
	sudo systemctl enable bt-agent
	sudo systemctl enable bt-network
	sudo systemctl start systemd-networkd
	sudo systemctl start bt-agent
	sudo systemctl start bt-network

	
Finally to pair, run: (temporary setup from usb interface?)

    sudo bt-adapter --set Discoverable 1
