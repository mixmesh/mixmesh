# Configure and setup the obsecrete computer

# resize SD card

# edit the file /boot/config.txt

    dtoverlay=dwc2

# update /etc/modules (or set in /boot/cmdline.txt )

	dwc2
	g_ether

# interfaces

Add to /etc/network/interfaces

    auto wlan0
    iface wlan0 inet6 static
        wireless-channel 1
        wireless-essid ERLGAMAL
        wireless-mode ad-hoc

    # usb cabel (ipv4 network)
    auto usb0
	allow-hotplug usb0
    iface usb0 inet static
      address 10.1.3.1
      netmask 255.255.255.0
	# pan0 bluetooth (ipv4 network)
	auto pan0
    iface pan0 inet static
      address 10.1.4.1
      netmask 255.255.255.0	

On the target there is no need to run dhcp client

    sudo systemctl stop dhcpcd.service

# dhcpd

Add to /etc/dhcp/dhcpd.conf

    subnet 10.1.3.0 netmask 255.255.255.0 {
      #  option routers 10.1.3.1;
      option subnet-mask 255.255.255.0;
      range 10.1.3.2 10.1.3.20;
      interface usb0;
    }
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
