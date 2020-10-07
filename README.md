# The Obscrete top-level application

## Build Obscrete

### Install external dependencies

You need to install GMP (for arbitrary precision arithmetic support) and Simple2D (for the simulator).

On Ubuntu:

`$ sudo apt install libgmp-dev`

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

```
$ git clone git@github.com:obscrete/apptools.git
$ git clone git@github.com:obscrete/elgamal.git
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

```
$ cd obscrete
$ export ERL_LIBS=..
$ make -f Makefile.top-level all
```

### Does it work? - run the tests

`make -f Makefile.top-level runtests`
	
Makefile.top-level has a number of other useful targets, e.g. clean,
mrproper, megapull and *dialyzer*.

## Prepare Obscrete

Create a mandatory file structure needed by Obscrete:

```
$ cd obscrete
$ ./bin/mkconfigdir /tmp/obscrete/alice
```

The above command creates something like:

* /tmp/obscrete/alice/pki/
* /tmp/obscrete/alice/player/temp/
* /tmp/obscrete/alice/maildrop/spooler/

These directories harmonize with the needs of the Obscrete
configuration files in ./obscrete/etc/*.conf

## Start Obscrete

Start Obscrete with an appropriate configuration file, e.g.

```
$ cd obscrete
$ ./bin/obscrete --config ./etc/obscrete.conf
```

## Start simulator

The simulator can be started like this:

```
$ cd obscrete
$ ulimit -n 4000
$ ./bin/mkconfigdir /tmp/obscrete/alice
$ ../simulator/bin/mkconfigdirs /tmp 100
$ ./bin/simulator --config ./etc/obscrete-simulator.conf
```

![A very short simulation](/doc/simulation.gif)

## Files

<dl>
  <dt>./bin/mkconfigdir</dt>
  <dd>Create the appropriate file structure needed to start Obscrete. You call this command with a single root directory as input, e.g. <code>./bin/mkconfigdir /tmp/obscrete/alice</code>.</dd>
  <dt>./bin/obscrete</dt>
  <dd>Start Obscrete, e.g. <code>./bin/obscrete --config etc/obscrete.conf</code>, but it can also reload the configuration file and stop Obscrete</dd>
  <dt>./bin/simulator</dt>
  <dd>Start a simulation, e.g. <code>./bin/simulator --config ./etc/obscrete-simulator.conf</code>
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
