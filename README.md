# The Mixmesh top-level application

## Give Erlang/OTP appropriate permissions

Make it possible for Erlang to bind to ports below 1024:

`$ sudo setcap 'cap_net_bind_service=+ep' /usr/local/lib/erlang/erts-11.1/bin/beam.smp`

(or whetver erts version you are using.)

## Build Mixmesh

### Install external dependencies

You need to install GMP (for arbitrary precision arithmetic support) and Simple2D (for the simulator). Also libsodium is used.

On Ubuntu:

```
$ sudo apt install libgmp-dev
$ sudo apt install libsodium-dev
```

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

Create a directory where you will build mixmesh

```
$ mkdir path-to-mixmesh
$ cd path-to-mixmesh
```

Then clone the mixmesh app

`$ git clone git@github.com:mixmesh/mixmesh`

If you want to clone one by one, or you must fix something else,
then this is the app list

```
$ git clone git@github.com:mixmesh/apptools.git
$ git clone git@github.com:mixmesh/elgamal.git
$ git clone git@github.com:mixmesh/enacl.git
$ git clone git@github.com:mixmesh/jsone.git
$ git clone git@github.com:mixmesh/mail.git
$ git clone git@github.com:mixmesh/mpa.git
$ git clone git@github.com:mixmesh/mixmesh.git
$ git clone git@github.com:mixmesh/keydir.git
$ git clone git@github.com:mixmesh/player.git
$ git clone git@github.com:mixmesh/rstar.git
$ git clone git@github.com:mixmesh/simulator.git
$ git clone git@github.com:mixmesh/tor.git
```

### Build repositories

Well, you could then enter mixmesh app and write

```
$ cd path-to-mixmesh/mixmesh
$ make -f Makefile.top-level
```

Or you may be want to make it easy and create a link at top level?

```
$ cd path-to-mixmesh
$ ln -s mixmesh/Makefile.top-level Makefile
```

From this step you can fetch all the needed applications

`$ make clone`

And if you (maybe later) want the simulator

`$ make simclone`

Or everything

`$ make megaclone`

To build all application and tests you type

`$ make`

When developing you may want to setup ERL\_LIBS so it includes
the mixmesh directory

`$ export ERL_LIBS=$ERL_LIBS:path-to-mixmesh`

### Does it work? - run the tests

`$ make runtests`

Makefile.top-level has a number of other useful targets, e.g. clean,
mrproper, megapull and *dialyzer*.

## Prepare Mixmesh

Create a self-signed certificate to be used by the SMTP/POP3 SSL servers:

`$ ./bin/mixmesh --self-signed-ssl-cert > cert.pem`

and then create a mandatory file structure needed by Mixmesh:

`$ ./bin/mkconfig /tmp/mixmesh cert.pem alice`

mkconfig in this case created:

* /tmp/mixmesh/keydir/data
* /tmp/mixmesh/alice/player/temp/
* /tmp/mixmesh/alice/player/buffer/
* /tmp/mixmesh/alice/player/keydir/data/
* /tmp/mixmesh/alice/player/maildrop/spooler/
* /tmp/mixmesh/alice/player/ssl/

As it happens this is the file structure used by the configuration
files under ./mixmesh/etc/*.conf.

## Start Mixmesh

Start Mixmesh with an appropriate configuration file, e.g.

`$ ./bin/mixmesh --config ./etc/mixmesh.conf`

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

NOTE: In order to start the Roma simulation you must first follow the instructions in [../simulator/priv/roma_taxi/README.md](https://github.com/mixmesh/simulator/blob/main/priv/roma_taxi/README.md).

Update /etc/systemd/user.conf and /etc/systemd/system.conf so that:

`DefaultLimitNOFILE=65535`

You must reboot in order for the systemd change to take effect.

Then proceed with:

```
$ ulimit -n 65535
$ ./bin/mixmesh --self-signed-ssl-cert > cert.pem
$ ./bin/mkconfig /tmp/mixmesh cert.pem alice
$ ../simulator/bin/mkconfig cert.pem square
$ ./bin/mixmesh --simulator --config ./etc/simulator.conf
```

![A very short simulation using the square data set](/doc/simulation.gif)

## Files

<dl>
  <dt>./bin/mkconfigdir</dt>
  <dd>Create the appropriate file structure needed to start Mixmesh. You call this command with a single root directory as input, e.g. <code>./bin/mkconfigdir /tmp/mixmesh/alice</code>.</dd>
  <dt>./bin/mixmesh</dt>
  <dd>Start Mixmesh, e.g. <code>./bin/mixmesh --config etc/mixmesh.conf</code>, but it can also reload the configuration file and stop Mixmesh</dd>
  <dt>./bin/simulator</dt>
  <dd>Start a simulation, e.g. <code>./bin/simulator --config ./etc/mixmesh.conf</code>
  <dt>./bin/run_test</dt>
  <dd>Run a test, e.g. <code>./bin/run_test --config ./etc/mixmesh-do-nothing.conf belgamal</code>
  <dt>./src/mixmesh_app.erl</dt>
  <dd>The top-level application module</dd>
  <dt>./src/mixmesh_sup.erl</dt>
  <dd>The top-level supervisor module</dd>
  <dt>./src/mixmesh_config_serv.erl</dt>
  <dd>Mixmesh configuration file handling</dd>
  <dt>./src/mixmesh_log_serv.erl</dt>
  <dd>Mixmesh log handling</dd>
  <dt>./src/mixmesh.erl</dt>
  <dd>Exports a single <code>start/0</code> which starts all other Mixmesh applications, i.e. the <code>mixmesh</code> command calls this function. We should use reltool to build releases with boot scripts and ez files etc instead.</dd>
</dl>
