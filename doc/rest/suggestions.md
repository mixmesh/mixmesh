# The Mixmesh REST API

This REST API should be expressive enough to perform the services needed to build [the Mixmesh web app](webapp.html). The resources described below are either available during the bootstrap or normal operation.

## A few RESTful links

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/
* https://wikemacs.org/wiki/Markdown#Live_preview_as_you_type (always)

# Bootstrap operation

## Resource: /system/install (**POST**)

Used to do a clean install of the box. A harsh cousin to `/system/reinstall`.

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

$ curl --request POST --header "Content-Type: application/json" --data '{"nym": "alice", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello"}' http://127.0.0.1:8444/system/install
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
$ curl --request POST --header "Content-Type: application/json" --data '{"nym": "alice", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello", "sync-address": "0.0.0.0:9900", "smtp-address": "0.0.0.0:19900", "pop3-address": "0.0.0.0:29900", "http-address": "0.0.0.0:8444", "obscrete-dir": "/tmp/obscrete", "pin": "123456"}' http://127.0.0.1:8444/system/install
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
$ curl --request POST --header "Content-Type: application/json" --data '5' http://127.0.0.1:8444/system/restart
Yes, sir!

$ ./bin/obscrete --config /tmp/obscrete/obscrete.conf
```

----

## Resource: /system/reinstall (**POST**)

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
  "public-key": "&lt;Base64 encoded public key&gt;",
  "secret-key": "&lt;Base64 encoded secret key&gt;",
  "smtp-password": "&lt;string&gt;",
  "pop3-password": "&lt;string&gt;",
  "http-password": "&lt;string&gt;",
  "sync-address": "&lt;ip:port&gt; (optional)",
  "smtp-address": "&lt;ip:port&gt; (optional)",
  "pop3-address": "&lt;ip:port&gt; (optional)",
  "http-address": "&lt;ip:port&gt; (optional)",
  "obscrete-dir": "&lt;path&gt; (optional)",
  "pin": "&lt;six digits&gt; (optional)"
}
```

### On success: 200 OK

```json
{
  "nym": "&lt;string (<32 characters)&gt;",
  "sync-address": "&lt;ip:port&gt;",
  "smtp-address": "&lt;ip:port&gt;",
  "pop3-address": "&lt;ip:port&gt;",
  "http-address": "&lt;ip:port&gt;",
  "obscrete-dir": "&lt;path&gt;",
  "pin": "&lt;six digits&gt;",
  "pin-salt": "&lt;Base64 encoded pin salt&gt;"
}
```

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

```
$ mkdir /tmp/obscrete
$ ./bin/obscrete --bootstrap
$ curl --request POST --header "Content-Type: application/json" --data '{"public-key": "BWFsaWNlBbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d", "secret-key": "BWFsaWNlgMwhWxEO5Ovn0OpNnN62Mu9nvL7Zn1mzlgSBkfC2zZQII\/otb+1jPqLMCDQlFKqNEXGy\/N1PUhotV3w7JBitwsZSUeGfVi2gLJFEkrZ6tGjrUoN3eB65JIzpfQirlLX6oCO5Ab1t4rOmD4BsHvA+lYBbYw3QihArIGqcTyNrbiC1BbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello"}' http://127.0.0.1:8444/system/reinstall
{
  "nym": "alice",
  "sync-address": "0.0.0.0:9900",
  "smtp-address": "0.0.0.0:19900",
  "pop3-address": "0.0.0.0:29900",
  "http-address": "0.0.0.0:8444",
  "obscrete-dir": "\/tmp\/obscrete",
  "pin": "123456",
  "pin-salt": "carYm55t3hqcmUNiAd+zkA=="
}
```

The reinstall method also have optional parameters but they are not examplified here.

----

## Resource: /key/import (**POST**)

Used to import public keys from a contacts file. The file is supposed to have been exported during normal operation using `/key/export`.

Implementation detail: In the end a PIN-encrypted file is generated, suitable to be loaded by the local PKI server during normal operation. This file typically ends up in `<obscrete-dir>/<nym>/player/local-pki/pki.db`.

### POST data

The POST data is supposed to be uploaded as multipart/form-data content with the following form parts:

* nym
* obscrete-dir
* pin
* pin-salt

See example below for more info.

### On success: 200 OK

The number of imported contacts.

### On failure: 400 Bad Request

A nice description on why the request failed.

### Typical usage

Below I expect that a contacts file has been exported during normal
operation:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '"all"' http://127.0.0.1:8444/dj/key/export
{
  "size": 101,
  "uri-path": "\/temp\/keys-66.bin"
}

$ curl --user alice:hello --digest --silent --output keys-66.bin http://127.0.0.1:8444/temp/keys-66.bin
```

And finally an example on how to import it during bootstrap operation:

```
$ ./bin/obscrete --bootstrap
$ curl --form nym=alice --form obscrete-dir=/tmp/obscrete --form pin=123456 --form pin-salt=xFxxsWkBHF9SWcEQA4pvzg== --form key-file=@keys-66.bin http://127.0.0.1:8444/key/import
101
```

----

## Resource: /system/restart (**POST**)

Used to restart and enter normal operation.

### POST data

<time in seconds&gt;

### On success: 204 No Content

-

### Typical usage

```
$ curl --request POST --header "Content-Type: application/json" --data '5' http://127.0.0.1:8444/system/restart
Yes, sir!
```

----

















## Normal operation

### `/dj/get-config` (**POST**)

Used to get a filtered set of configuration values.

**NOTE**: The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top">A configuration filter</td>
    <td valign="top">A filtered configuration</td>
    <td valign="top">400, 404</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"nym": true, "spiridon": {"public-key": true}}}' http://127.0.0.1:8444/dj/get-config
{
  "player": {
    "nym": "alice",
    "spiridon": {
      "public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
    }
  }
}
```

### `/dj/edit-config` (**POST**)

Used to edit a partial set of configuration values.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top">A partial configuration</td>
    <td valign="top">204</td>
    <td valign="top">400, 404</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"http-server": {"password": true}}}' http://127.0.0.1:8444/dj/get-config
{
  "player": {
    "http-server": {
      "password": "hello"
    }
  }
}

$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"spiridon": {"f": 0.3}, "http-server": {"password":"zooooop"}}}' http://127.0.0.1:8444/dj/edit-config
Config has been updated

$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '{"player": {"http-server": {"password": true}}}' http://127.0.0.1:8444/dj/get-config
{
  "player": {
    "http-server": {
      "password": "zooooop"
    }
  }
}
```

### `/dj/key` (**GET**)

Used to show all available keys. At most 100 keys will be shown.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top">-</td>
    <td valign="top">200<pre lang="json">[{
  "nym": "&lt;string (<32 characters)&gt;",
  "public-key": "&lt;Base64 encoded public key&gt;"
 }]</pre></td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/dj/key
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

### `/dj/key/<nym>` (**GET**)

Used to show a key for a specific nym.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top">-</td>
    <td valign="top">200<br><code>&lt;Base64 encoded public key&gt;</code></td>
    <td valign="top">400, 404</td>
</tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/dj/key/alice
"BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
```

### `/dj/key/filter` (**POST**)

Used to show a *sub-string* filtered set of keys. At most 100 keys will be shown.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">["&lt;sub-string nym (<32 characters)&gt;"]</pre></td>
    <td valign="top">200<pre lang="json">[{
  "nym": "&lt;string (<32 characters)&gt;",
  "public-key": "&lt;Base64 encoded public key&gt;"
}]</pre></td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["p1"]' http://127.0.0.1:8444/dj/key/filter
[
  {
    nym: "p1",
    "public-key": "AnAxBNN4r35tVwRRktbu2N83GmvDvTBdmTNeMLP+u6lPSfM4\/Oby3tGF07qbtQdaZgteOAXj3pB7xNhJbARmril0avcbbXs\/HlfJlidui7JZM0T7Uu+qWmq7X3qAUnYA42rM6lEI7pnfuKn+X4SW\/HTbaW4kBBjq3f\/ERruD2W5c9KoI"
  },
  {
    nym: "p11",
    "public-key": "A3AxMLDb4hxZt3mSZ8Qa+kSfa2K4R\/ayLKhQX+RMNFn7NlS9cxG\/QXdtJy1S28abJ5HTKw+9S8pHw3caXjGCWWS8BfD77yhzbMQgA3Y9c\/\/gaL+nGPRO+4PmgpTykotSVe1VUPWUJO5fQ+oVFROGBjQDnZjLO0S7XI0Ekd37hCGTyS6d"
  }
]
```

### `/dj/key` (**PUT**)

Used to import new (or replace an existing) key.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><code>&lt;Base64 encoded public key&gt;</code></td>
    <td valign="top">200</td>
    <td valign="top">400, 403</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/dj/key/alice
"BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="

$ ./bin/obscrete --pin-salt
DSzE7cTR+eBfqKtI694kAA==

$ ./bin/obscrete --elgamal-keys 123456 DSzE7cTR+eBfqKtI694kAA== alice
-----BEGIN SECRET KEY-----
pqrvo9PEVoAUuEI9B6g5zxjaqqPcHSTEmNn5+qVv/lsgrgOlEygGmoUowPxP0LIqyVSOysAe33bPckGOlhYyX7bG2hR4H2U+Gb0zocfW0wuFCIbLPaN+q3+2OLOSqd0eEz9rDAtTA0x8cwxrAeHElbR0iVx+pOEhm/P9TotkAzYnoSXGhOO6e4wTqQ2MPAD+Zjf9pL0yjpQeykH+ZuttxAWJW/RVmXeKRHrbqzUMvk64DHzKBKt5cPeWevJSp3BkSWh+UiRZjl+ktxwdF92VH5gdo4YcOyetXIaOwQym8XIE/nGiosJEFHVpCo5lvlcRvd4IXkw5u1Q1bf3fN91pHEH+ArZL75QjD/hASNU3l6OHn/NCudGngBk5ZOlnLFHucz6F9bjuLEMnRtX+uZkJTzg=
-----END SECRET KEY-----
-----BEGIN PUBLIC KEY-----
BWFsaWNlBJNUFPQyNBgndEf8QJLBY/kngZbjbCgWtpZhRUWtbDaEPxxmIrdWOZcpUa2yDauWNCZ/cZ4r7hSUXOW8TlJaqz2yJjG1OZ9nesloWrkrxDIU8xXjkZ7A6O2Trwf1xmYwMe17sp4BwR87lR8K3LBBYEwB1f3BFtle4zRCupxbAwGy
-----END PUBLIC KEY-----

$ curl --user alice:hello --digest --request PUT --header "Content-Type: application/json" --data '"BWFsaWNlBJNUFPQyNBgndEf8QJLBY/kngZbjbCgWtpZhRUWtbDaEPxxmIrdWOZcpUa2yDauWNCZ/cZ4r7hSUXOW8TlJaqz2yJjG1OZ9nesloWrkrxDIU8xXjkZ7A6O2Trwf1xmYwMe17sp4BwR87lR8K3LBBYEwB1f3BFtle4zRCupxbAwGy"' http://127.0.0.1:8444/dj/key
Key has been updated

$ curl --user alice:hello --digest http://127.0.0.1:8444/dj/key/alice
"BWFsaWNlBJNUFPQyNBgndEf8QJLBY\/kngZbjbCgWtpZhRUWtbDaEPxxmIrdWOZcpUa2yDauWNCZ\/cZ4r7hSUXOW8TlJaqz2yJjG1OZ9nesloWrkrxDIU8xXjkZ7A6O2Trwf1xmYwMe17sp4BwR87lR8K3LBBYEwB1f3BFtle4zRCupxbAwGy"
```

### `/dj/key/delete` (**POST**)

Used to delete a filtered set of keys.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">["&lt;nym (<32 characters)&gt;"]</pre></td>
   <td valign="top">200<pre lang="json">{
  "failed": [{"nym": "&lt;string (<32 characters)&gt;",
              "reason": "&lt;string&gt;"}]
}</pre></td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["p41", "p65"]}' http://127.0.0.1:8444/dj/key/delete
[]

$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["p41", "p65"]' http://127.0.0.1:8444/dj/key/delete
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

### `/dj/key/export` (**POST**)

Used to export contacts/public keys.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">["&lt;nym (<32 characters)&gt;"]</pre></td>
    <td valign="top">200<br><code>&lt;Base64 encoded key bundle&gt;</code></td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '["alice", "p42"]' http://127.0.0.1:8444/dj/key/export
"AIUDcDQyAVhSeZp9niFBbEbUUwue0FpJnraVlsMUCJfMlGaOZHNqA1HD5uxevxbDumkCPcM693C9MWSb7k0FWaJgKeBWCLp9wpJ3DTz3nPcl5+A\/zOqA6z89MNGXEuGFv4\/+px4p0JZeT+7zIFMMHXUZmItfW3a04mcNn96POw87vw74f3J7AIYFYWxpY2XGAMPwGV5HSVk7JNUy6C6qz0gTUTtK5aCzLI002ni+utBlukXw9KISGO7Z0vdwydzlkUCh7jU1Dj4Ljmv5fCT12mjeyflTCP4RNnDmcL9K1b5h4CUCl4JlUvXboBw6FA+2BE3JqNzzhCDR0zsrlQOJz\/zWRvQSqRpnVrC3BSuY+w=="
```

### `/dj/key/import` (**POST**)

Used to import a key bundle.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><code>&lt;Base64 encoded key bundle&gt;</code></td>
    <td valign="top">204</td>
    <td valign="top">400, 403</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest --request POST --header "Content-Type: application/json" --data '"AIUDcDQyAVhSeZp9niFBbEbUUwue0FpJnraVlsMUCJfMlGaOZHNqA1HD5uxevxbDumkCPcM693C9MWSb7k0FWaJgKeBWCLp9wpJ3DTz3nPcl5+A\/zOqA6z89MNGXEuGFv4\/+px4p0JZeT+7zIFMMHXUZmItfW3a04mcNn96POw87vw74f3J7AIYFYWxpY2XGAMPwGV5HSVk7JNUy6C6qz0gTUTtK5aCzLI002ni+utBlukXw9KISGO7Z0vdwydzlkUCh7jU1Dj4Ljmv5fCT12mjeyflTCP4RNnDmcL9K1b5h4CUCl4JlUvXboBw6FA+2BE3JqNzzhCDR0zsrlQOJz\/zWRvQSqRpnVrC3BSuY+w=="' http://127.0.0.1:8444/dj/key/import
```
