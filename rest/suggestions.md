# Suggestions: The making of a REST API

This REST API is expressive enough to perform the operations described mentioned in the [suggested Web UI](../webui/suggestions.md).

I found some great advice here:

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

### Resource `/dj/player` (**PUT**)

Used to recreate a player. This resource method **must** be called in order to call any other resource methods. The ELGamal keys under `player/spiridon/` (see below) will be recreated.

<table>
  <tr>
    <th>Request</th>
    <th>Success Response</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;"
}</pre></td>
    <td valign="top">204</td>
  </tr>
</table>

#### Example

After box initialization the player is disabled and its default nym is set to "admin". Do the following to enable the player and to rename it to "alice":

`$ curl --user admin:hello --digest -v -X PUT -H "Content-Type: application/json" -d '{"nym": "alice"}' http://127.0.0.1:8443/dj/player`

### Resource `/dj/player` (**GET**)

Used to show all available information about the player. The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th>Request</th>
    <th>Success Response</th>
    <th>Failure Response</th>
  </tr>
  <tr>
    <td valign="top">-</td>
    <td valign="top">200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "spiridon": {
    "public-key": "&lt;A link to a 2D-barcode&gt;",
    "secret-key": "&lt;A link to a 2D-barcode&gt;"
  }
}</pre></td>
    <td valign="top">404</td>
  </tr>
</table>

#### Example

`$ curl --user admin:hello --digest -v http://127.0.0.1:8443/dj/player`

### Resource `/dj/player/filter` (**POST**)

Used to show a filtered set of information about the player. The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th>Request</th>
    <th>Success Response</th>
    <th>Failure Response</th>
  </tr>
  <tr>
    <td valign="top">200<pre lang="json">{
  "nym": "&lt;boolean&gt;",
  "spiridon": {
    "public-key": "&lt;boolean&gt;",
    "secret-key": "&lt;boolean&gt;"
  }
}</pre></td>
    <td valign="top">200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "spiridon": {
    "public-key": "&lt;A link to a 2D-barcode&gt;",
    "secret-key": "&lt;A link to a 2D-barcode&gt;"
  }
}</pre></td>
    <td valign="top">404</td>
  </tr>
</table>

#### Example

`$ curl --user alice:hello --digest -v -X POST -H "Content-Type: application/json" -d '{"spiridon": {smtp-server:": true}}' http://127.0.0.1:8443/dj/player`

### Resource `/dj/player` (**PATCH**)

Used to patch the player. One or many of the JSON fields in the request can be specified.

<table>
  <tr>
    <th>Request</th>
    <th>Success Response</th>
    <th>Failure Response</th>
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
    <td valign="top">404</td>
  </tr>
</table>

#### Example

`$ curl --user alice:hello --digest -v -X PATCH -H "Content-Type: application/json" -d '{"smtp-server:": {"password": "foobar"}}' http://127.0.0.1:8443/dj/player`
