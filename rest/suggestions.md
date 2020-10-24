# Suggestions: The making of a REST API

This suggested REST API is expressive enough to perform the operations described in the [suggested Web UI](../webui/suggestions.md).

I found some great advice here:

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

### Resource `/dj/player` (**PUT**)

Used to (re)create a player. This resource method **must** be called first in order to call any other resource methods. The keys under `player/spiridon/` (see below) are recreated.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success Response</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;"
}</pre></td>
    <td valign="top">Status Code: 204</td>
  </tr>
</table>

After box initialization the player is disabled and its default nym is set to "admin". Do the following to enable the player and to rename it to "alice".

Typical use:

`$ curl --user admin:hello --digest -v -X PUT -H "Content-Type: application/json" -d '{"nym": "alice"}' http://127.0.0.1:8443/dj/player`

### Resource `/dj/player` (**GET**)

Used to show all available information about the player. The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success Response</th>
    <th align="left">Failure Response</th>
  </tr>
  <tr>
    <td valign="top">-</td>
    <td valign="top">Status Code: 200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "spiridon": {
    "public-key": "&lt;A link to a 2D-barcode&gt;",
    "secret-key": "&lt;A link to a 2D-barcode&gt;"
  }
}</pre></td>
    <td valign="top">Status Code: 404</td>
  </tr>
</table>

Typical use:

`$ curl --user admin:hello --digest -v http://127.0.0.1:8443/dj/player`

### Resource `/dj/player/filter` (**POST**)

Used to show a filtered set of information about the player. The "secret-key" is only available for one hour after box initialization.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success Response</th>
    <th align="left">Failure Response</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;boolean&gt;",
  "spiridon": {
    "public-key": "&lt;boolean&gt;",
    "secret-key": "&lt;boolean&gt;"
  }
}</pre></td>
    <td valign="top">Status Code: 200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;",
  "spiridon": {
    "public-key": "&lt;A link to a 2D-barcode&gt;",
    "secret-key": "&lt;A link to a 2D-barcode&gt;"
  }
}</pre></td>
    <td valign="top">Status Code: 404</td>
  </tr>
</table>

Note: A field excluded from the Request is the same thing as setting it to false.

Typical use:

`$ curl --user alice:hello --digest -v -X POST -H "Content-Type: application/json" -d '{"spiridon": {smtp-server:": true}}' http://127.0.0.1:8443/dj/player/filter`

### Resource `/dj/player` (**PATCH**)

Used to patch the player. One or many of the fields in the Request below can be given.

<table>
  <tr>
    <th align="left">Request</th>
    <th align="left">Success Response</th>
    <th align="left">Failure Response</th>
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
    <td valign="top">Status Code: 204</td>
    <td valign="top">Status Code: 404</td>
  </tr>
</table>

Typical use:

`$ curl --user alice:hello --digest -v -X PATCH -H "Content-Type: application/json" -d '{"smtp-server:": {"password": "foobar"}}' http://127.0.0.1:8443/dj/player`
