# The Obscrete top-level application

## Build Obscrete

### Install external libraries

Install https://gmplib.org/

### Clone appropriate repositories

```
git clone git@github.com:obscrete/apptools.git
git clone git@github.com:obscrete/elgamal.git
git clone git@github.com:obscrete/jsone.git
git clone git@github.com:obscrete/mail.git
git clone git@github.com:obscrete/mpa.git
git clone git@github.com:obscrete/obscrete.git
git clone git@github.com:obscrete/pki.git
git clone git@github.com:obscrete/player.git
git clone git@github.com:obscrete/rstar.git
git clone git@github.com:obscrete/simulator.git
git clone git@github.com:obscrete/tor.git
```

### Make

```
cd obscrete
export ERL_LIBS=..
make -f Makefile.top-level
```

## Prepare Obscrete

`./bin/mkconfigdir /tmp/obscrete/alice`

This command created the appropriate directory structure needed to
start Obscrete, i.e.

* /tmp/obscrete/alice/pki/
* /tmp/obscrete/alice/player/temp/
* /tmp/obscrete/alice/maildrop/spooler/

## Start Obscrete

`./bin/obscrete --config ./etc/obscrete-simulator.conf`

This command started Obscrete with a ./etc/obscrete.conf configuration,
which happens to require the directory structure created above with
`mkconfigdir`.

## Start simulator

`./bin/obscrete --config ./etc/obscrete-simulator.conf -- -run simulator`

To be explained...

## Files

<dl>
  <dt>./bin/mkconfigdir</dt>
  <dd>Creates the appropriate file structure needed to start Obscrete. You call this command with a single root directory as input, e.g. <code>./mkconfigdir /tmp/obscrete/alice</code>.</dd>
  <dt>./bin/obscrete</dt>
  <dd>Starts Obscrete, e.g. <code>./bin/obscrete --config etc/obscrete.conf</code>, but it can also reload the configuration file and stop Obscrete</dd>
  <dt>./bin/unit_test</dt>
  <dd>Start a unit test, e.g. <code>./bin/unit_test --config ./etc/obscrete-do-nothing.conf belgamal</code>
  <dt>./src/obscrete_app.erl</dt>
  <dd>Top-level application module</dd>
  <dt>./src/obscrete_sup.erl</dt>
  <dd>Top-level supervisor module</dd>
  <dt>./src/obscrete_config_serv.erl</dt>
  <dd>Configuration file handling</dd>
  <dt>./src/obscrete_log_serv.erl</dt>
  <dd>Log handling</dd>
  <dt>./src/obscrete.erl</dt>
  <dd>Exports a single <code>start/0</code> which starts all other Obscrete applications, i.e. the <code>obscrete</code> command eventually calls this function. We should later on use reltool to build releases with boot scripts and ez files etc.</dd>
</dl>
