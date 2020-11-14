# The Mixmesh REST API

This REST API should be expressive enough to perform the services needed to build the web app described in "[A Mixmesh web app](webapp.md)".

he resources described below are either available during the bootstrap or normal operation.

## A few RESTful links

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

# Bootstrap operation

## Resource: /bootstrap/install (**POST**)

Used to do a clean install of the box. A harsh cousin to `/bootstrap/reinstall`.

Implementation note: On success <obscrete-dir&gt;/obscrete.conf is
generated with the following parameters configured:

* initialization-time
* obscrete-dir
* pin
* pin-salt
* global-pki-server/data-dir
* nym
* sync-address
* public-key
* secret-key
* smtp-server/address
* smtp-server/password-digest
* pop3-server/address
* pop3-server/password-digest
* http-server/address
* http-server/password
* daemon/file/path
* dbg/file/path
* daemon/file/path

[player/priv/obscrete.conf.src](https://github.com/obscrete/player/blob/master/priv/obscrete.conf.src)
is used as a template.

### POST data

```json
{
  "nym": "<string (<32 characters)>",
  "smtp-password": "<string>",
  "pop3-password": "<string>",
  "http-password": "<string>",
  "sync-address": "<ip:port> (optional)",
  "smtp-address": "<ip:port> (optional)",
  "pop3-address": "<ip:port> (optional)",
  "http-address": "<ip:port> (optional)",
  "obscrete-dir": "<path> (optional)",
  "pin": "<six digits> (optional)"
}
```

### On success: 200 OK

```json
{
  "public-key": "<Base64 encoded public key>",
  "secret-key": "<Base64 encoded secret key>",
  "sync-address": "<ip:port>",
  "smtp-address": "<ip:port>",
  "pop3-address": "<ip:port>",
  "http-address": "<ip:port>",
  "obscrete-dir": "<path>",
  "pin": "<six digits>",
  "pin-salt": "<Base64 encoded pin salt>"
}
```

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

```
$ mkdir /tmp/obscrete-
$ ./bin/obscrete --bootstrap

$ curl --request POST --header "Content-Type: application/json" --data '{"nym": "alice", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello"}' http://127.0.0.1:8444/bootstrap/install
{
  "public-key": "BWFsaWNlt8GZL4n7Tbyqbr5JD6IUKbEO0TZeQ6JQOnFuj9ggayRm\/JhRs1\/QPF1UNzkRIPakugXCMjELYnuz8V+hEHWq4hOa5IzNp\/MSomKJamK4608bMgMwRQ4RKx5KbfD+V2NL3KictMM5QKFh+AWgjZN0SyN8VinhT4K7ye\/FRh3zl\/E=",
  "secret-key": "BWFsaWNlgNRUP8jfeJHgQH4CwDAkD1VvucDSqNxvdnbcu6mBmtNsfzHWeLxYKAcut\/+doKt+D5xZ\/dZ2RyG2AyxGJrb3atZvpBug1q71GqEFYHlAEk2E0qJDKHnxQs7R3I5z7c237hctQjc79tJI\/FqT\/9FAtmtKJd8OUWPDEN+WoUGr4lk2t8GZL4n7Tbyqbr5JD6IUKbEO0TZeQ6JQOnFuj9ggayRm\/JhRs1\/QPF1UNzkRIPakugXCMjELYnuz8V+hEHWq4hOa5IzNp\/MSomKJamK4608bMgMwRQ4RKx5KbfD+V2NL3KictMM5QKFh+AWgjZN0SyN8VinhT4K7ye\/FRh3zl\/E=",
  "sync-address": "0.0.0.0:9900",
  "smtp-address": "0.0.0.0:19900",
  "pop3-address": "0.0.0.0:29900",
  "http-address": "0.0.0.0:8444",
  "obscrete-dir": "/tmp/obscrete",
  "pin": "123456",
  "pin-salt": "B70E3LMi8O+IZxUDyPY8ug=="
}
```

In the example above the optional parameters were ommited, i.e. these optional parameters will not be in the final version anyway, but here all possible optional parameters are provided (with their current default values):

```json
$ curl --request POST --header "Content-Type: application/json" --data '{"nym": "alice", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello", "sync-address": "0.0.0.0:9900", "smtp-address": "0.0.0.0:19900", "pop3-address": "0.0.0.0:29900", "http-address": "0.0.0.0:8444", "obscrete-dir": "/tmp/obscrete", "pin": "123456"}' http://127.0.0.1:8444/bootstrap/install
{
  "public-key": "BWFsaWNlAb580nSD7dm2ltsNxz\/yO7nAikKko4FMxKzL8rF0EUY+lF0YX5J9ljlZuemxhh3QrQuKY2KnMJ0ATMfdSleyUUBsOEC2YzDFMpqR7Dx0iiQ6ZUZdfPoYlL9oC2mjOIOXmEtgBtTeeOeVPgk\/\/P5xrCaPjf8BC0fh\/90oU9YWLIV4",
  "secret-key": "BWFsaWNlgQK3RdpgKoErxfuNjH1dH0T1Zf27DT6T+BO0gJrTdHF8pb+lkkT19epLk7ofGlgh6HErwbvPfGIZzXlYpzdnmqRvLjOFg5s5aUqnimuIgfYm9fRGu6hRNZBgQRakulbm2zLYqsrYaR8fhpbXKpm5HH8sBkSfRuECTh+PPE5xZO49EwG+fNJ0g+3ZtpbbDcc\/8ju5wIpCpKOBTMSsy\/KxdBFGPpRdGF+SfZY5WbnpsYYd0K0LimNipzCdAEzH3UpXslFAbDhAtmMwxTKakew8dIokOmVGXXz6GJS\/aAtpoziDl5hLYAbU3njnlT4JP\/z+cawmj43\/AQtH4f\/dKFPWFiyFeA==",
  "sync-address": "0.0.0.0:9900",
  "smtp-address": "0.0.0.0:19900",
  "pop3-address": "0.0.0.0:29900",
  "http-address": "0.0.0.0:8444",
  "obscrete-dir": "/tmp/obscrete",
  "pin": "123456",
  "pin-salt": "vkEGjmGfrs5vmlExAVbZBA=="
}
```

Upon completion a new <obscrete-dir&gt;/obscrete.conf is created, i.e.  /tmp/obscrete/obscrete.conf. Bootstrap is now ready and Obscrete can be restarted to perform normal operation:

```
$ curl --request POST --header "Content-Type: application/json" --data '5' http://127.0.0.1:8444/bootstrap/restart
Yes, sir!

$ ./bin/obscrete --config /tmp/obscrete/obscrete.conf
```

----

## Resource: /bootstrap/reinstall (**POST**)

Used to reinstall the box using pre-existing keys. Nice cousin to `/install`.

Implementation note: On success <obscrete-dir&gt;/obscrete.conf is
generated with the following parameters configured:

* initialization-time
* obscrete-dir
* pin
* pin-salt
* global-pki-server/data-dir
* nym
* sync-address
* public-key
* secret-key
* smtp-server/address
* smtp-server/password-digest
* pop3-server/address
* pop3-server/password-digest
* http-server/address
* http-server/password
* daemon/file/path
* dbg/file/path
* daemon/file/path

[player/priv/obscrete.conf.src](https://github.com/obscrete/player/blob/master/priv/obscrete.conf.src)
is used as a template.

### POST data

```json
{
  "public-key": "<Base64 encoded public key>",
  "secret-key": "<Base64 encoded secret key>",
  "smtp-password": "<string>",
  "pop3-password": "<string>",
  "http-password": "<string>",
  "sync-address": "<ip:port> (optional)",
  "smtp-address": "<ip:port> (optional)",
  "pop3-address": "<ip:port> (optional)",
  "http-address": "<ip:port> (optional)",
  "obscrete-dir": "<path> (optional)",
  "pin": "<six digits> (optional)"
}
```

### On success: 200 OK

```json
{
  "nym": "<string (<32 characters)>",
  "sync-address": "<ip:port>",
  "smtp-address": "<ip:port>",
  "pop3-address": "<ip:port>",
  "http-address": "<ip:port>",
  "obscrete-dir": "<path>",
  "pin": "<six digits>",
  "pin-salt": "<Base64 encoded pin salt>"
}
```

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

```
$ mkdir /tmp/obscrete
$ ./bin/obscrete --bootstrap
$ curl --request POST --header "Content-Type: application/json" --data '{"public-key": "BWFsaWNlBbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d", "secret-key": "BWFsaWNlgMwhWxEO5Ovn0OpNnN62Mu9nvL7Zn1mzlgSBkfC2zZQII\/otb+1jPqLMCDQlFKqNEXGy\/N1PUhotV3w7JBitwsZSUeGfVi2gLJFEkrZ6tGjrUoN3eB65JIzpfQirlLX6oCO5Ab1t4rOmD4BsHvA+lYBbYw3QihArIGqcTyNrbiC1BbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello"}' http://127.0.0.1:8444/bootstrap/reinstall
{
  "nym": "alice",
  "sync-address": "0.0.0.0:9900",
  "smtp-address": "0.0.0.0:19900",
  "pop3-address": "0.0.0.0:29900",
  "http-address": "0.0.0.0:8444",
  "obscrete-dir": "/tmp/obscrete",
  "pin": "123456",
  "pin-salt": "carYm55t3hqcmUNiAd+zkA=="
}
```

The reinstall method also have optional parameters but they are not examplified here.

----

## Resource: /bootstrap/key/import (**POST**)

Used to import public keys from a file. The file is supposed to have been exported during normal operation using `/key/export`.

Implementation detail: In the end a PIN-encrypted file is generated, suitable to be loaded by the local PKI server during normal operation. This file typically ends up in `<obscrete-dir>/<nym>/player/local-pki/pki.db`.

### POST data

The POST data is supposed to be uploaded as multipart/form-data with the following form parts:

* nym
* obscrete-dir
* pin
* pin-salt
* key-file

See example below for more info.

### On success: 200 OK

The number of imported public keys.

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

In this example a file with public keys is first exported during
normal operation:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '"all"' http://127.0.0.1:8444/key/export
{
  "size": 101,
  "uri-path": "\/temp\/keys-66.bin"
}

$ curl --user alice:hello --digest --silent --output keys-66.bin http://127.0.0.1:8444/temp/keys-66.bin
```

And then the exported file is imported during bootstrap operation:

```
$ ./bin/obscrete --bootstrap
$ curl --form nym=alice --form obscrete-dir=/tmp/obscrete --form pin=123456 --form pin-salt=xFxxsWkBHF9SWcEQA4pvzg== --form key-file=@keys-66.bin http://127.0.0.1:8444/bootstrap/key/import
101
```

----

## Resource: /bootstrap/restart (**POST**)

Used to restart and enter normal operation.

### POST data

Time in seconds until restart.

### On success: 204 No Content

<nothing&gt;

### Typical usage

```
$ curl --request POST --header "Content-Type: application/json" --data '5' http://127.0.0.1:8444/bootstrap/restart
Yes, sir!
```

----

# Normal operation

## Resource: /get-config (**POST**)

Used to extract a filtered set of configuration values.

Note: The "secret-key" is only available for one hour after box
initialization.

### POST data

A partial JSON filter structure mimicing what can be configured in
obscrete.conf. All values set to true will be included in the success
response. This will be clear if you look at the example below.

### On success: 200 OK

All values that was picked in the filter represented as a partial JSON
structure.

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"nym": true, "spiridon": {"public-key": true}}}' http://127.0.0.1:8444/get-config
{
  "player": {
    "nym": "alice",
    "spiridon": {
      "public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
    }
  }
}
```

----

## Resource: /edit-config (**POST**)

Used to edit a partial set of configuration values.

### POST data

A partial JSON structure mimicing what can be configured in
obscrete.conf. All values will be validated according to rules used in
obscrete.conf. This will be clear if you look at the example below.

### On success: 200 No Content

Configuration has been updated.

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"http-server": {"password": true}}}' http://127.0.0.1:8444/get-config
{
  "player": {
    "http-server": {
      "password": "hello"
    }
  }
}

$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"spiridon": {"f": 0.3}, "http-server": {"password":"zooooop"}}}' http://127.0.0.1:8444/edit-config
Config has been updated

$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"http-server": {"password": true}}}' http://127.0.0.1:8444/get-config
{
  "player": {
    "http-server": {
      "password": "zooooop"
    }
  }
}
```

----

## Resource: /key (**GET**)

Used to show all available public keys. At most 100 keys will be shown.

### On success: 200 OK

```json
[
  {
    "nym": "<string (<32 characters)>",
    "public-key": "<Base64 encoded public key>"
  },
  ...
  {
    "nym": "<string (<32 characters)>",
    "public-key": "<Base64 encoded public key>"
  }
]
```

### Typical usage

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/key
[
  {
    "nym": "alice",
    "public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
  },
  {
    "nym": "p1",
    "public-key": "AnAxBNN4r35tVwRRktbu2N83GmvDvTBdmTNeMLP+u6lPSfM4\/Oby3tGF07qbtQdaZgteOAXj3pB7xNhJbARmril0avcbbXs\/HlfJlidui7JZM0T7Uu+qWmq7X3qAUnYA42rM6lEI7pnfuKn+X4SW\/HTbaW4kBBjq3f\/ERruD2W5c9KoI"
  },
  {
    "nym": "p11",
    "public-key": A3AxMLDb4hxZt3mSZ8Qa+kSfa2K4R\/ayLKhQX+RMNFn7NlS9cxG\/QXdtJy1S28abJ5HTKw+9S8pHw3caXjGCWWS8BfD77yhzbMQgA3Y9c\/\/gaL+nGPRO+4PmgpTykotSVe1VUPWUJO5fQ+oVFROGBjQDnZjLO0S7XI0Ekd37hCGTyS6d"
  },
  {
    "nym": "p12",
    "public-key": "BHAxMDABiS61z1AxsC2Kbx3GBrfb5pftV1\/piyCKOt\/\/DThArLGrxnnLTwz0flD8An33aoZmsAYBbJNE7k4HhL1F+cLvqZD\/d2oz2r0Lt4aBWCz2pDMas\/MIivQnbSJZWTse\/PxSuk95L0CfeKGgR61s5DAls652Rqsw4xsoIfibYJu26Pc="
  }
]
```

## Resource: /key/<nym&gt; (**GET**)

Used to show a key for a specific nym.

### On success: 200 OK

```json
{
  "nym": "<string (<32 characters)>",
  "public-key": "<Base64 encoded public key>"
}
```

### On failure: 404 Not Found

<nothing&gt;

### Typical usage

```
curl --user alice:hello --digest http://127.0.0.1:8444/key/alice
{
  "nym": "alice",
  "public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
}
```

----

## Resource: /key/filter (**POST**)

Used to show a *sub-string* filtered set of keys. At most 100 keys
will be shown.

### POST data

```json
["<string (<32 characters)>", ..., "<string (<32 characters)>"]
```

### On success: 200 OK

```json
[
  {
    "nym": "<string (<32 characters)>",
    "public-key": "<Base64 encoded public key>"
  },
  ...
  {
    "nym": "<string (<32 characters)>",
    "public-key": "<Base64 encoded public key>"
  }
]
```

### On failure: 404 Bad Request

Invalid filter

### Typical usage

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["p1"]' http://127.0.0.1:8444/key/filter
[
  {
    "nym": "p1",
    "public-key": "AnAxBNN4r35tVwRRktbu2N83GmvDvTBdmTNeMLP+u6lPSfM4\/Oby3tGF07qbtQdaZgteOAXj3pB7xNhJbARmril0avcbbXs\/HlfJlidui7JZM0T7Uu+qWmq7X3qAUnYA42rM6lEI7pnfuKn+X4SW\/HTbaW4kBBjq3f\/ERruD2W5c9KoI"
  },
  {
    "nym": "p11",
    "public-key": "A3AxMLDb4hxZt3mSZ8Qa+kSfa2K4R\/ayLKhQX+RMNFn7NlS9cxG\/QXdtJy1S28abJ5HTKw+9S8pHw3caXjGCWWS8BfD77yhzbMQgA3Y9c\/\/gaL+nGPRO+4PmgpTykotSVe1VUPWUJO5fQ+oVFROGBjQDnZjLO0S7XI0Ekd37hCGTyS6d"
  }
]
```

----

## Resource: /key (**PUT**)

Used to import new (or replace an existing) key.

### PUT data

A Base64 encoded public key.

### On success: 200 OK

The nym encoded in the public key.

### On failure: 400 Bad Request

Invalid key

### Typical usage

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/key/alice
{
  "nym": "alice",
  "public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
}

$ ./bin/obscrete --pin-salt
dNuxqGX2aQ2IHDsN1qVwKQ==

$ ./bin/obscrete --elgamal-keys 123456 DSzE7cTR+eBfqKtI694kAA== alice
-----BEGIN SECRET KEY-----
xSWN08SbX4ZBvJz2uNRa2MP6P/COMWcwAqB60dIYMdLxAI1r+jXY5KGNZA/5oCYCqhutKmAvd1c6h9CbNKGyDHoaMShBWu2qJjzVOob3vxsloOrUvugIUIkJnGn5wGwU6iVWudzIxZgwUIKv8I0n62HMI2g+gRZs58ePx6SeDvxZ4veIwJmGAKvC1nVCKnFA+bKkVfzIu4I4UNYgFzKinJoMURVDX2Hrh8KY6P/EgerIzc1kGL13QXfXRgYGRNMklTVzlm9GVeaKi7Wh24bHKrtla5dseA1xrdQRRHBNxOn/g0ZTRla5EZnasw1PQlGWT4PX8C9zo1A52tynkQSK8JTrC/zspTk4q68mBWpUnebuOUeMCOuG9mP4WBbwyG19xbddANu6Fto1boQzxM5qAw==
-----END SECRET KEY-----
-----BEGIN PUBLIC KEY-----
BWFsaWNlBJWFOMpEsNjUcO5WpVPt6Q/ob7+AYIt/iEn2yCzavZdzfuBnrjJ2T+0pjn5IUKpgM1IbLiSedagB+isHgr3NHxlmptGp6QvMBizh8/DsqvZCFO/dvjWUc1olnRrsnLp1S/IFcImj2Zb7vxLEmnnyjSdqLdXbw8YTCSDoWA38Llqm
-----END PUBLIC KEY-----

$ curl --user alice:hello --digest --request PUT --header "Content-Type: application/json" --data '"BWFsaWNlBJNUFPQyNBgndEf8QJLBY/kngZbjbCgWtpZhRUWtbDaEPxxmIrdWOZcpUa2yDauWNCZ/cZ4r7hSUXOW8TlJaqz2yJjG1OZ9nesloWrkrxDIU8xXjkZ7A6O2Trwf1xmYwMe17sp4BwR87lR8K3LBBYEwB1f3BFtle4zRCupxbAwGy"' http://127.0.0.1:8444/key
"alice"

$ curl --user alice:hello --digest http://127.0.0.1:8444/key/alice
{
  "nym": "alice",
  "public-key": "BWFsaWNlBJWFOMpEsNjUcO5WpVPt6Q\/ob7+AYIt\/iEn2yCzavZdzfuBnrjJ2T+0pjn5IUKpgM1IbLiSedagB+isHgr3NHxlmptGp6QvMBizh8\/DsqvZCFO\/dvjWUc1olnRrsnLp1S\/IFcImj2Zb7vxLEmnnyjSdqLdXbw8YTCSDoWA38Llqm"
}
```

----

## Resource: /key/delete (**POST**)

Used to delete a filtered set of keys.

### POST data

```json
["<string (<32 characters)>", ..., "<string (<32 characters)>"]
```

### On success: 200 OK

```json
[{"nym": "<string (<32 characters)>", "reason": "<string>"},
 ..
 {"nym": "<string (<32 characters)>", "reason": "<string>"}]
```

In other words: If *all* public keys referred to existed and were deleted an empty list is returned.

### Typical usage

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["p41", "p65"]}' http://127.0.0.1:8444/key/delete
[]

$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["p41", "p65"]' http://127.0.0.1:8444/key/delete
[
  {
    "nym": "p65",
    "reason": "No such key"
  },
  {
    "nym": "p41",
    "reason": "No such key"
  }
]
```

----

## Resource: /key/export (**POST**)

Used to export public keys.

### POST data

```json
["<string (<32 characters)>", ..., "<string (<32 characters)>"]
```

or

```json
"all"
```

### On success: 200 OK

```json
{
  "size": "<integer (number of exported public keys)>",
  "uri-path": "<string (URI to a file with the exported public keys)>"
}
```

### On failure: 400 Bad Request

Invalid nyms

### Typical usage

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '"all"' http://127.0.0.1:8444/key/export
{
  "size": 101,
  "uri-path": "\/temp\/keys-66.bin"
}
```

----

## Resource: /key/import (**POST**)

Used to import public keys from a file. The file is supposed to have been exported using `/key/export`.

### POST data

The POST data is supposed to be uploaded as multipart/form-data with a simgle "key-file" form part.

See example below for more info.

### On success: 200 OK

The number of imported public keys.

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

In this example a file with public keys is first exported:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '"all"' http://127.0.0.1:8444/key/export
{
  "size": 101,
  "uri-path": "\/temp\/keys-66.bin"
}

$ curl --user alice:hello --digest --silent --output keys-66.bin http://127.0.0.1:8444/temp/keys-66.bin
```

And then the exported file is imported:

```
$ ./bin/obscrete --bootstrap
$ curl --form nym=alice --form obscrete-dir=/tmp/obscrete --form pin=123456 --form pin-salt=xFxxsWkBHF9SWcEQA4pvzg== --form key-file=@keys-66.bin http://127.0.0.1:8444/bootstrap/key/import
101
```
