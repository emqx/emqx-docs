# REST API based MQTT 5.0 Enhanced Authentication

EMQX also supports the MQTT 5.0 enhanced authentication based on REST API. This authenticator is an implementation of [Salted Challenge Response Authentication Mechanism (SCRAM)](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism) authentication that uses an external web resource as a source of SCRAM authentication data.

After enabling, when a client initiates a connect request with SCRAM, EMQX will use the received `username` to construct an HTTP request to retrieve 
 authentication data.

SCRAM is a more complicated mechanism than password verification. It requires exchanging additional MQTT packages during connection. SCRAM authentication does not depend on external data sources, and it is simple and lightweight to use.

::: tip Prerequisite

- Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

- SCRAM authenticator only supports MQTT 5.0 connection.

- SCRAM authenticator only supports MQTT 5.0 connection.

- This is not an implementation of the RFC 7804: [Salted Challenge Response HTTP Authentication Mechanism](https://datatracker.ietf.org/doc/html/rfc7804).

:::

## HTTP Request and Response

The retrieve process is similar to an HTTP API call where EMQX, as the requesting client, constructs and initiates a request to the HTTP service in the format required by the "API", and the HTTP service returns the authentication data as required by the `username`.

- The response encoding format `content-type` must be `application/json`.
- The authentication data must contain `stored_key`, `server_key` and `salt`, all of them must be encoded in hex.
- Superuser is marked by `is_superuser` in the body, option value: `true`, `false`.
- You can set [client attributes](../../client-attributes/client-attributes.md) using the optional `client_attrs` field. Note that both keys and values must be strings.
- You can set an optional `acl` field in the response body to specify the client's permissions. See [Access Control List (ACL)](./acl.md) for more information.
- You can set an optional `expire_at` field in the response body to specify the expiration time of the client's authenticity, forcing the client to disconnect and get reauthenticated at reconnection. The value is a Unix timestamp in seconds.
- The HTTP response status code `Status Code` should be `200`, the `4xx/5xx` status code returned will ignore the body and determine the result to be `ignore` and continue with the authentication chain.

Example response:

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "stored_key": "008F5E0CC6316BB172F511E93E4756EEA876B5B5125F1CD2FD69A2C30F9A0D73",
    "server_key": "81466E185EC642AFAE1EFA75953735D6C0934D099149AAAB601D59F8F8162580",
    "salt": "6633653634383437393466356532333165656435346432393464366165393137"
    "is_superuser": true, // options: true | false, default value: false
    "client_attrs": { // optional 
        "role": "admin",
        "sn": "10c61f1a1f47"
    }
    "expire_at": 1654254601, // optional 
    "acl": // optional 
    [
        {
            "permission": "allow",
            "action": "subscribe",
            "topic": "eq t/1/#",
            "qos": [1]
        },
        {
            "permission": "deny",
            "action": "all",
            "topic": "t/3"
        }
    ]
}
```

## Configure with Dashboard

You can use EMQX Dashboard to finish the relevant configuration.

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **SCRAM** as **Mechanism**, and **HTTP Server** as **Backend**, this will lead us to the **Configuration** tab, as shown below.

<img src="./assets/the-img.png" alt="HTTP" style="zoom:67%;" />



**HTTP**:

- **Method**: Select HTTP request method, optional values: `get`, `post`

  :::tip

  The `POST` method is recommended. When using the `GET` method, some sensitive information (such as plain text passwords) may be exposed via HTTP server logs. Also, for untrusted environments, please use HTTPS.
   :::

- **URL**: Enter the URL address of the HTTP service.
- **Headers** (optional): HTTP request header. You can add several headers.

**Connection Configuration**:

- **Pool size** (optional): Input an integer value to define the number of concurrent connections from an EMQX node to an HTTP server. Default: **8**. 
- **Connect Timeout** (optional): Specify the waiting period before EMQX assumes the connection is timed out. Units supported include milliseconds, second, minute, and hour.
- **HTTP Pipelining** (optional): Input a positive integer to specify the maximum number of HTTP requests that can be sent without waiting for a response; default value: **100**.
- **Request Timeout** (optional): Specify the waiting period before EMQX assumes the request is timed out. Units supported include milliseconds, second, minute, and hour.

**TLS Configuration**: Turn on the toggle switch if you want to enable TLS. For more information on enabling TLS, see [Network and TLS](../../network/overview.md).

**Authentication configuration**:

- **Body**: Request template; for `POST` requests, it is sent as a JSON in the request body; for `GET` requests, it is encoded as a Query String in the URL. Mapping keys and values support using [placeholder](./authn.md#authentication-placeholders).
- **Algorithm**: password hash algorithm, options: `sha256` and `sha512`
- **iteration_count** (optional): Iteration-count parameter for SCRAM; Default: `4096`

After you finish the settings, click **Create**.
