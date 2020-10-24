# Suggestions: The making of a REST API

This REST API is expressive enough to perform the operations described mentioned in the [suggested Web UI](../webui/suggestions.md).

I found some great advice here:

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

### Resource `/dj/player` (**PUT**)

This resource method **must** be called in order to use **any** other resource methods. This resource method recreates the ELGamal keys under `player/spiridon`.

<table>
  <tr>
    <th>Request</th>
    <th>Response</th>
  </tr>
  <tr>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;"
}</pre></td>
    <td valign="top">204</td>
  </tr>
</table>

#### Examples

After box initialization the player is disabled and its default nym is set to "admin". Do the following to enable the player and to rename it to "alice":

`$ curl --user admin:hello --digest -v -X PUT -H "Content-Type: application/json" -d '{"nym": "alice"}' http://127.0.0.1:8443/dj/player`




### Resource `/dj/player` (**GET**)




    <th>COMMENT(S)</th>



  <tr>
    <td valign="top">/dj/player</td>
    <td valign="top">GET</td>
    <td valign="top">-</td>
    <td valign="top">200<pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;">,
  "spiridon": {
    "public-key": "&lt;A link to a 2D-barcode&gt;",
    "secret-key": "&lt;A link to a 2D-barcode&gt;",
  },
  "smtp-server": {
    "password": "&lt;string&gt;"
  },
  "pop3-server": {
    "password": "&lt;string&gt;"
  }
}</pre></td>
    <td valign="top">404</td>
    <td valign="top">The secret-key is only shown for one hour after box initialization</td>
  </tr>

  <tr>
    <td valign="top">/dj/player</td>
    <td valign="top">PATCH</td>
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
    <td valign="top">One or many of the JSON fields in the request body may be provided</td>
  </tr>
</table>
