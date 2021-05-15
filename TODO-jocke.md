1. Remove all #pk{} and #sk{} records from from all repositories and
replace them with maps. Make sure that everything still works. Do this
to prepare of next action point.

1. Update all code to use keys generated from
pgp_keys:generate_mixmesh_keys/1. Start to update ./bin/mixmesh and 
the rest will follow. A lot of stuff. In the end keys on disk willl be
pgp keys and all keys in memory/state will be maps.

1. Remove "public-key" and "secret-key" configurables from the mixmesh
conf files and put them in pgp files on disk.

1. Update player_serv.erl so that it uses the new Keydir Service
instead of the old "PKI" based ditto when it does remote key lookup.

1. The old "PKI" server is not needed anymore. Remove these files:
keydir/src/{keydir_network_client.erl,keydir_network.hrl,keydir_network_serv.hrl,keydir_serv.erl,remote_keydir_server_schema.erl}
and do the appropriate cleanup. Update the README.md file on top level
in the keydir repository as well. Remove the test suites as well.

1. Go through the Mixmesh REST API,
i.e. src/mixmesh/src/player_{normal,bootstrap}_service.erl, and make
sure that is uses best practice introduced in
keydir/src/keydir_service.erl. Check things like error handling, camel
casing and more. Lets be consistent in all our REST APIs.

1. Build a Keydir Service Web app on top of the Keydir Service REST
API. Let it have the same look and feel as the Mixmesh Web app (built
on top of the Mixmesh REST API).

1. Use iframes in the in the Mixmesh Web app to make it easy to import
keys from the Keydir Service Web app.

1. Remind Tony about dialyzer errors.
