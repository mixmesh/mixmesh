# Suggestions: The making of a REST API

This REST API is expressive enough to perform the operations described mentioned in the suggested [Web UI](../webui/suggestions.md).

I found some great advice here:

* https://restfulapi.net/
* https://restfulapi.net/http-status-codes/

## Player

https://restfulapi.net/http-status-codes/

<table>
  <tr>
    <th>REST URI</th>
    <th>METHOD</th>
    <th>REQUEST BODY</th>
    <th>ON SUCCESS</th>
    <th>ON FAILURE</th>
    <th>COMMENT(S)</th>
  </tr>

  <tr>
    <td>/dj/player</td>
    <td>PUT</td>
    <td><pre lang="json">{
  "nym": <string (at most 31 characters)>,
}</pre></td>
    <td>204 No Content</td>
    <td>-</td>
    <td>This <em>must</em> be done before any other resource method can be performed on the player resource. NOTE: This resource method recreates the ElGamal keys.</td>
  </tr>

  <tr>
    <td>/dj/player</td>
    <td>GET</td>
    <td>-</td>
    <td>200 OK:<br><pre lang="json">{
  "nym": <string (at most 31 characters)>,
  "smtp-server": {
    "password": <string>
  },
  "pop3-server": {
    "password": <string>
  }</pre></td>
    <td>404 Not Found</td>
    <td>-</td>
  </tr>

  <tr>
    <td>/dj/player</td>
    <td>PATCH</td>
    <td>-</td>
    <td>200 OK:<br><pre lang="json">{
  "smtp-server": {
    "password": <string>
  },
  "pop3-server": {
    "password": <string>
  }</pre></td>
    <td>404 Not Found</td>
    <td>One or many of the JSON fields in the request body may be provided</td>
  </tr>
</table>
