# Suggestions: The making of a REST API

This REST API is expressive enough to perform the operations described mentioned in the [suggested Web UI](../webui/suggestions.md).

I found some great advice here:

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

## Player

https://restfulapi.net/http-status-codes/

<table>
  <tr>
    <th>RESOURCE</th>
    <th>METHOD</th>
    <th>BODY</th>
    <th>SUCCESS</th>
    <th>FAILURE</th>
    <th>COMMENT(S)</th>
  </tr>

  <tr>
    <td valign="top">/dj/player</td>
    <td valign="top">PUT</td>
    <td valign="top"><pre lang="json">{
  "nym": "&lt;string (<32 characters)&gt;"
}</pre></td>
    <td valign="top">204</td>
    <td valign="top">-</td>
    <td valign="top">This <em>must</em> be done before any other resource method can be performed on the player resource. This resource method recreates the ElGamal keys.</td>
  </tr>

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
