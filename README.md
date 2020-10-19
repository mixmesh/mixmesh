# The Obscrete top-level application

## Build Obscrete

### Install external dependencies

You need to install GMP (for arbitrary precision arithmetic support) and Simple2D (for the simulator). Also libsodium is used.

On Ubuntu:

    $ sudo apt install libgmp-dev
    $ sudo apt install libsodium	

and

```
$ sudo apt install libsdl2-dev
$ sudo apt install libsdl2-image-dev
$ sudo apt install libsdl2-mixer-dev
$ sudo apt install libsdl2-ttf-dev
```

followed by

```
$ git clone https://github.com/simple2d/simple2d
$ cd simple2d
$ make
$ sudo make install
```

To run player tests the program mpop and swaks must be installed

```
$ sudo apt install mpop
$ sudo apt install swaks
```

To have the tor tests pass the test suite you need to install tor

`$ sudo apt install tor`

### Clone repositories

Create a directory where you will build obscrete

	$ mkdir path-to-obscrete
	$ cd path-to-obscrete

Then clone the obscrete app

	$ git clone git@github.com:obscrete/obscrete


If you want to clone one by one, or you must fix something else,
then this is the app list

```
$ git clone git@github.com:obscrete/apptools.git
$ git clone git@github.com:obscrete/elgamal.git
$ git clone git@github.com:obscrete/enacl.git
$ git clone git@github.com:obscrete/jsone.git
$ git clone git@github.com:obscrete/mail.git
$ git clone git@github.com:obscrete/mpa.git
$ git clone git@github.com:obscrete/obscrete.git
$ git clone git@github.com:obscrete/pki.git
$ git clone git@github.com:obscrete/player.git
$ git clone git@github.com:obscrete/rstar.git
$ git clone git@github.com:obscrete/simulator.git
$ git clone git@github.com:obscrete/tor.git
```

### Build repositories

Well, you could then enter obscrete app and write

    $ cd path-to-obscrete/obscrete
	$ make -f Makefile.top-level

Or you may be want to make it easy and create a link at top level?

    $ cd path-to-obscrete
	$ ln -s obscrete/Makefile.top-level Makefile

From this step you can fetch all the needed applications

	$ make clone

And if you (maybe later) want the simulator

	$ make simclone

Or everything

	$ make megaclone

To build all application and tests you type

	$ make

When developing you may want to setup ERL\_LIBS so it includes
the obscrete directory

	$ export ERL_LIBS=$ERL_LIBS:path-to-obscrete

### Does it work? - run the tests

	$ make runtests

Makefile.top-level has a number of other useful targets, e.g. clean,
mrproper, megapull and *dialyzer*.

## Prepare Obscrete

Create a self-signed certificate to be used by the SMTP/POP3 SSL servers:

`$ ./bin/obscrete --self-signed-ssl-cert > cert.pem`

and then create a mandatory file structure needed by Obscrete:

`$ ./bin/mkconfig /tmp/obscrete cert.pem alice`

mkconfig in this case created:

* /tmp/obscrete/pki/data
* /tmp/obscrete/alice/player/temp/
* /tmp/obscrete/alice/player/buffer/
* /tmp/obscrete/alice/player/pki/data/
* /tmp/obscrete/alice/player/maildrop/spooler/
* /tmp/obscrete/alice/player/ssl/

As it happens this is the file structure used by the configuration
files under ./obscrete/etc/*.conf.

## Start Obscrete

Start Obscrete with an appropriate configuration file, e.g.

`./bin/obscrete --config ./etc/obscrete.conf`

## Start simulator

To start the simulator, use ./etc/simulator.conf, e.g.

```
"simulator": {
    "enabled": true,
    "renderer" : "sdl",
    "data-set": "square"
}
```

The data set must be one of "circle", "square", "epfl", "roma" or
"it", and the renderer must be one of "sdl" or "epx".

NOTE: In order to start the Roma simulation you must first follow the instructions in [../simulator/priv/roma_taxi/README.md](https://github.com/obscrete/simulator/blob/main/priv/roma_taxi/README.md).

Update /etc/systemd/user.conf and /etc/systemd/system.conf so that:

`DefaultLimitNOFILE=65535`

You must reboot in order for the systemd change to take effect.

Then proceed with:

```
$ ulimit -n 65535
$ ./bin/obscrete --self-signed-ssl-cert > cert.pem
$ ./bin/mkconfig /tmp/obscrete cert.pem alice
$ ../simulator/bin/mkconfig cert.pem square
$ ./bin/obscrete --simulator --config ./etc/simulator.conf
```

![A very short simulation using the square data set](/doc/simulation.gif)

## Files

<dl>
  <dt>./bin/mkconfigdir</dt>
  <dd>Create the appropriate file structure needed to start Obscrete. You call this command with a single root directory as input, e.g. <code>./bin/mkconfigdir /tmp/obscrete/alice</code>.</dd>
  <dt>./bin/obscrete</dt>
  <dd>Start Obscrete, e.g. <code>./bin/obscrete --config etc/obscrete.conf</code>, but it can also reload the configuration file and stop Obscrete</dd>
  <dt>./bin/simulator</dt>
  <dd>Start a simulation, e.g. <code>./bin/simulator --config ./etc/obscrete.conf</code>
  <dt>./bin/run_test</dt>
  <dd>Run a test, e.g. <code>./bin/run_test --config ./etc/obscrete-do-nothing.conf belgamal</code>
  <dt>./src/obscrete_app.erl</dt>
  <dd>The top-level application module</dd>
  <dt>./src/obscrete_sup.erl</dt>
  <dd>The top-level supervisor module</dd>
  <dt>./src/obscrete_config_serv.erl</dt>
  <dd>Obscrete configuration file handling</dd>
  <dt>./src/obscrete_log_serv.erl</dt>
  <dd>Obscrete log handling</dd>
  <dt>./src/obscrete.erl</dt>
  <dd>Exports a single <code>start/0</code> which starts all other Obscrete applications, i.e. the <code>obscrete</code> command calls this function. We should use reltool to build releases with boot scripts and ez files etc instead.</dd>
</dl>
