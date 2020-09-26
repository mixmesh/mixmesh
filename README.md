# The Obscrete top-level application

## Building

* Install https://gmplib.org/
* git clone git@github.com:obscrete/apptools.git
* git clone git@github.com:obscrete/elgamal.git
* git clone git@github.com:obscrete/json.git
* git clone git@github.com:obscrete/mpa.git
* git clone git@github.com:obscrete/obscrete.git
* git clone git@github.com:obscrete/pki.git
* git clone git@github.com:obscrete/rstar.git
* git clone git@github.com:obscrete/tor.git
* cd obscrete
* export ERL_LIBS=..
* make -f Makefile.top-level

## Commands

<dl>
  <dt>./bin/obscrete</dt>
  <dd>Can be used to start Obscrete, e.g. <code>./bin/obscrete --config etc/obscrete-no-players.conf</code>, but it can also reload the configuration file and stop Obscrete</dd>
  <dt>./bin/unit_test</dt>
  <dd>Can be used to start a unit test, e.g. <code>./bin/unit_test --config ./etc/obscrete-no-players.conf elgamal</code>
</dl>

## Configuration files

Typical configuration files can be found under <code>./etc/</code>

## Modules

<dl>
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
 
