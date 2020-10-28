# Suggestions: The making of a REST API

This suggested REST API is expressive enough to perform the operations described in the [suggested Web UI](../webui/suggestions.md).

I found some great advice here:

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

## Bootstrapping

### `/dj/wipe` (**POST**)

Used to wipe the box configuration. A harsh cousin to `dj/reinstall`.

Implementation note: On success $OBSCRETE_DIR/&lt;nym&gt;/obscrete.conf is generated and the following parameters are injected: "nym", "pin", "pin-salt", "public-key", "secret-key", "smtp-password", "pop3-password" and "http-password". obscrete/priv/obscrete.conf.src is used as a template.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "smtp-password": "&lt;string&gt;",
  "pop3-password": "&lt;string&gt;",
  "http-password": "&lt;string&gt;"
  "obscrete-dir": "&lt;path&gt; (optional)",
  "pin": "&lt;path&gt; (optional)"
}</pre></td>
    <td valign="top">200<pre>

  "public-key": "&lt;Base64 encoded public key&gt;",
  "secret-key": "&lt;Base64 encoded secret key&gt;"

</td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl -X POST -H "Content-Type: application/json" -d '{"nym": "alice", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello"}' http://127.0.0.1:8444/dj/wipe
```

### `/dj/reinstall` (**POST**)

Used to reinstall the box using pre-existing keys. Nice cousin to `dj/wipe`.

Implementation note: On success $OBSCRETE_DIR/&lt;nym&gt;/obscrete.conf is generated and the following parameters are injected: "nym", "pin", "pin-salt", "public-key", "secret-key", "smtp-password", "pop3-password" and "http-password". obscrete/priv/obscrete.conf.src is used as a template.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "public-key": "&lt;Base64 encoded public key&gt;",
  "secret-key": "&lt;Base64 encoded secret key&gt;",
  "smtp-password": "&lt;string&gt;",
  "pop3-password": "&lt;string&gt;",
  "http-password": "&lt;string&gt;"
  "key-bundle": "&lt;Base64 encoded key bundle&gt; (optional)",
  "obscrete-dir": "&lt;path&gt; (optional)",
  "pin": "&lt;six digits&gt; (optional)"
}</pre></td>
    <td valign="top">204</td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl -X POST -H "Content-Type: application/json" -d '{"public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic/81kb0EqkaZ1awtwUrmPs=", "secret-key": "JUitY4g+ezCu1VJ9G11RSnfvKqieoGb+C+Q+CH6f+6EWC/lu+YAey2g9iTcpf/xoa501SFfUTCG1cV16tU/o/VOd18/zE98F7Jd6e/2NeiM6yMrCQrbFnY/cugQPwbKw6jf8lnxiO1+kBdqX5a5Fgs7eTsChd44lJY1QeFM7/rNECWKmPonIY/NwD3mcA3iBpUwmD0RYGdEB6IXFc30xgR2avOAWd0e+5PMnyvVw//OC12vvkZAdtK4oL1gTfHoQ9B5YGILeFmZdScfrAMXaY7BkVqiCpIa+xK86dtqzf0Afa7G/vg3Lj8wf2CXhq0e4+wqXSqBuIVhLn9TxIPe1jfA5r4IfOqCMRqZKmbQD3ltxp7Ojt79leAOl2PARJFOd+XMlISNtJ4WcYXyboeRAzw==", "smtp-password": "baz", "pop3-password": "baz", "http-password": "hello"}' http://127.0.0.1:8444/dj/reinstall
```

### `/dj/restart` (**POST**)

Used to restart and enter normal operation.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">"&lt;time in seconds&gt;"</td>
    <td valign="top">204</td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl -X POST -H "Content-Type: application/json" -d '5' http://127.0.0.1:8444/dj/restart
```

## Normal operation

### `/dj/player` (**GET**)

Used to show all *available* information about the player.

**NOTE**: The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top">-</td>
    <td valign="top">200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "public-key": "&lt;Base64 encoded public key&gt;",
  "secret-key": "&lt;Base64 encoded secret key&gt;"
}</pre></td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/dj/player
{
  "nym": "alice",
  "public-key": "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs=",
  "secret-key": "JUitY4g+ezCu1VJ9G11RSnfvKqieoGb+C+Q+CH6f+6EWC\/lu+YAey2g9iTcpf\/xoa501SFfUTCG1cV16tU\/o\/VOd18\/zE98F7Jd6e\/2NeiM6yMrCQrbFnY\/cugQPwbKw6jf8lnxiO1+kBdqX5a5Fgs7eTsChd44lJY1QeFM7\/rNECWKmPonIY\/NwD3mcA3iBpUwmD0RYGdEB6IXFc30xgR2avOAWd0e+5PMnyvVw\/\/OC12vvkZAdtK4oL1gTfHoQ9B5YGILeFmZdScfrAMXaY7BkVqiCpIa+xK86dtqzf0Afa7G\/vg3Lj8wf2CXhq0e4+wqXSqBuIVhLn9TxIPe1jfA5r4IfOqCMRqZKmbQD3ltxp7Ojt79leAOl2PARJFOd+XMlISNtJ4WcYXyboeRAzw=="
}
```

### `/dj/player/filter` (**POST**)

Used to show a filtered set of information about the player.

**NOTE**: The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;boolean&gt;",
  "public-key": "&lt;boolean&gt;",
  "secret-key": "&lt;boolean&gt;"
}</pre></td>
    <td valign="top">200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "public-key": "&lt;Base64 encoded public key&gt;",
  "secret-key": "&lt;Base64 encoded secret key&gt;"
}</pre></td>
    <td valign="top">400, 404</td>
  </tr>
</table>

Omitted filter request fields are excluded from the response.

Typical usage:

```
$ curl --user alice:hello --digest -X POST -H "Content-Type: application/json" -d '{"nym": true, "public-key": true}' http://127.0.0.1:8444/dj/player/filter
```

### `/dj/player` (**PATCH**)

Used to patch the player. One or several of the fields in the Request can be given.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "smtp-server": {
    "password": "&lt;string&gt;"
  },
  "pop3-server": {
    "password": "&lt;string&gt;"
  }
}</pre></td>
    <td valign="top">204</td>
    <td valign="top">400, 404</td>
  </tr>
</table>

Typical usage:

`$ curl --user alice:hello --digest -v -X PATCH -H "Content-Type: application/json" -d '{"smtp-server:": {"password": "foobar"}}' http://127.0.0.1:8444/dj/player`

### `/dj/key` (**GET**)

Used to show all available keys. At most 100 keys will be returned.

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
  "BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs=",
  "AnAxBNN4r35tVwRRktbu2N83GmvDvTBdmTNeMLP+u6lPSfM4\/Oby3tGF07qbtQdaZgteOAXj3pB7xNhJbARmril0avcbbXs\/HlfJlidui7JZM0T7Uu+qWmq7X3qAUnYA42rM6lEI7pnfuKn+X4SW\/HTbaW4kBBjq3f\/ERruD2W5c9KoI",
  "A3AxMLDb4hxZt3mSZ8Qa+kSfa2K4R\/ayLKhQX+RMNFn7NlS9cxG\/QXdtJy1S28abJ5HTKw+9S8pHw3caXjGCWWS8BfD77yhzbMQgA3Y9c\/\/gaL+nGPRO+4PmgpTykotSVe1VUPWUJO5fQ+oVFROGBjQDnZjLO0S7XI0Ekd37hCGTyS6d",
  "BHAxMDABiS61z1AxsC2Kbx3GBrfb5pftV1\/piyCKOt\/\/DThArLGrxnnLTwz0flD8An33aoZmsAYBbJNE7k4HhL1F+cLvqZD\/d2oz2r0Lt4aBWCz2pDMas\/MIivQnbSJZWTse\/PxSuk95L0CfeKGgR61s5DAls652Rqsw4xsoIfibYJu26Pc="
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
    <td valign="top">200<br>&lt;Base64 encoded public key&gt;</td>
    <td valign="top">400, 404</td>
</tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest http://127.0.0.1:8444/dj/key/alice
"BWFsaWNlxgDD8BleR0lZOyTVMuguqs9IE1E7SuWgsyyNNNp4vrrQZbpF8PSiEhju2dL3cMnc5ZFAoe41NQ4+C45r+Xwk9dpo3sn5Uwj+ETZw5nC\/StW+YeAlApeCZVL126AcOhQPtgRNyajc84Qg0dM7K5UDic\/81kb0EqkaZ1awtwUrmPs="
```

### `/dj/key/filter` (**POST**)

Used to show a filtered set of keys. At most 100 keys will be returned.

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
$ curl --user alice:hello --digest -X POST -H "Content-Type: application/json" -d '["p1"]' http://127.0.0.1:8444/dj/key/filter
[
  "AnAxBNN4r35tVwRRktbu2N83GmvDvTBdmTNeMLP+u6lPSfM4\/Oby3tGF07qbtQdaZgteOAXj3pB7xNhJbARmril0avcbbXs\/HlfJlidui7JZM0T7Uu+qWmq7X3qAUnYA42rM6lEI7pnfuKn+X4SW\/HTbaW4kBBjq3f\/ERruD2W5c9KoI",
  "A3AxMLDb4hxZt3mSZ8Qa+kSfa2K4R\/ayLKhQX+RMNFn7NlS9cxG\/QXdtJy1S28abJ5HTKw+9S8pHw3caXjGCWWS8BfD77yhzbMQgA3Y9c\/\/gaL+nGPRO+4PmgpTykotSVe1VUPWUJO5fQ+oVFROGBjQDnZjLO0S7XI0Ekd37hCGTyS6d",
  "BHAxMDABiS61z1AxsC2Kbx3GBrfb5pftV1\/piyCKOt\/\/DThArLGrxnnLTwz0flD8An33aoZmsAYBbJNE7k4HhL1F+cLvqZD\/d2oz2r0Lt4aBWCz2pDMas\/MIivQnbSJZWTse\/PxSuk95L0CfeKGgR61s5DAls652Rqsw4xsoIfibYJu26Pc=",
  "A3AxMQTWAy7GdARTANZcvNfgW++zCfe+3ziaMp8+FW513nxRLiV8OspD\/BI9RAJuPhT8xi1uLRKH9lhBuJNakHHBJCmW44obJ\/lc4Bg7riv\/It09ka6uKOCbTZISogHPQFl9VTa3DQKEmttmk5OIqitsGU8tjp7hH4fbH\/0R4JzrCoBGSw=="
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
    <td valign="top"><pre lang="json">"&lt;Base64 encoded public key&gt;"</pre></td>
    <td valign="top">204</td>
    <td valign="top">400, 403</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest -X PUT -H "Content-Type: application/json" -d '"A3AxMQTWAy7GdARTANZcvNfgW++zCfe+3ziaMp8+FW513nxRLiV8OspD\/BI9RAJuPhT8xi1uLRKH9lhBuJNakHHBJCmW44obJ\/lc4Bg7riv\/It09ka6uKOCbTZISogHPQFl9VTa3DQKEmttmk5OIqitsGU8tjp7hH4fbH\/0R4JzrCoBGSw=="' http://127.0.0.1:8444/dj/key
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
$ curl --user alice:hello --digest -X POST -H "Content-Type: application/json" -d '["p41", "p65"]}' http://127.0.0.1:8444/dj/key/delete
[]

$ curl --user alice:hello --digest -X POST -H "Content-Type: application/json" -d '["p41", "p65"]' http://127.0.0.1:8444/dj/key/delete
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

Used to export a key bundle.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success</th>
    <th align="left">Failure</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">["&lt;nym (<32 characters)&gt;"]</pre></td>
    <td valign="top">200<br>&lt;Base64 encoded key bundle&gt;</td>
    <td valign="top">400</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest -X POST -H "Content-Type: application/json" -d '["alice", "p42"]' http://127.0.0.1:8444/dj/key/export
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
    <td valign="top"><pre lang="json">"&lt;Base64 encoded key bundle&gt"&gt;"</pre></td>
    <td valign="top">204</td>
    <td valign="top">400, 403</td>
  </tr>
</table>

Typical usage:

```
$ curl --user alice:hello --digest -X POST -H "Content-Type: application/json" -d '"AIUDcDQyAVhSeZp9niFBbEbUUwue0FpJnraVlsMUCJfMlGaOZHNqA1HD5uxevxbDumkCPcM693C9MWSb7k0FWaJgKeBWCLp9wpJ3DTz3nPcl5+A\/zOqA6z89MNGXEuGFv4\/+px4p0JZeT+7zIFMMHXUZmItfW3a04mcNn96POw87vw74f3J7AIYFYWxpY2XGAMPwGV5HSVk7JNUy6C6qz0gTUTtK5aCzLI002ni+utBlukXw9KISGO7Z0vdwydzlkUCh7jU1Dj4Ljmv5fCT12mjeyflTCP4RNnDmcL9K1b5h4CUCl4JlUvXboBw6FA+2BE3JqNzzhCDR0zsrlQOJz\/zWRvQSqRpnVrC3BSuY+w=="' http://127.0.0.1:8444/dj/key/import
```
