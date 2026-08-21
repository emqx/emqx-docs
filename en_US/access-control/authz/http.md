# Use HTTP Service

::: tip
Starting from EMQX v5.8.0, the HTTP authenticator supports including ACL rules in the response body to preset permissions for clients. You are recommended to use the new format for better performance. For details, see [HTTP Authentication](../authn/http.md).
:::

EMQX supports the authorization based on the HTTP service. The user needs to build an external HTTP application as a data source by themselves. EMQX makes requests to the HTTP service and determines the authorization result based on the data returned by the HTTP API, thus achieving complex authorization logic.

::: tip Tip

- Knowledge about [basic EMQX authorization concepts](./authz.md)

:::

## HTTP Request and Response

When the client initiates a subscription or publishing operation, the HTTP Authorizer constructs and sends a request based on the configured request template. Users need to implement authorization logic in the authorization service and return the results according to the following requirements.

### Request

The request can use JSON format, with the following placeholders in the URL and request body:

- `${clientid}`: The client ID.
- `${username}`: The username used by the client on login.
- `${client_attrs.NAME}`: A client attribute. `NAME` will be replaced by an attribute name set based on predefined configurations at runtime. For details about the client attributes, see [MQTT Client Attributes](../../client-attributes/client-attributes.md).
- `${peerhost}`: The source IP address of the client.
- `${proto_name}`: The protocol name used by the client, e.g. `MQTT`, `CoAP`.
- `${mountpoint}`: The mountpoint of the gateway listener (topic prefix).
- `${action}`: The action being requested, e.g. `publish`, `subscribe`.
- `${topic}`: The topic (or topic filter) to be published or subscribed in the current request.
- `${qos}`: The QoS of the message to be published or subscribed in the current request.
- `${retain}`: Whether the message to be published in the current request is a retained message.
- `${zone}`: The client's Zone at runtime. The Zone is a logical classification of the client, such as region or environment, that can be dynamically applied based on the client's configuration.

### Response

After checking, the authorization service needs to return a response in the following format:

- Response `content-type` must be `application/json`.
- If the HTTP Status Code is `200`, the authorization result is granted by HTTP Body. It depends on the value of the `result` field:
  - `allow`: Allow Publish or Subscribe.
  - `deny`: Deny Publish or Subscribe.
  - `ignore`: Ignore this request, it will be handed over to the next authorizer.
- If the HTTP Status Code is `204`, it means that this Publish or Subscribe request is allowed.
- HTTP Status Codes other than `200` and `204`, mean "ignore", for example, this HTTP service not available.

<!--- NOTE: the code supports `application/x-www-form-urlencoded` too, but it is not very easy to extend in the future, hence hidden from doc -->

Example response:

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow" | "deny" | "ignore" // Default `"ignore"`
}
```

::: tip EMQX 4.x compatibility statement:

In version 4.x, EMQX only used the status code returned by the HTTP API, while the content is discarded. For example, `200` indicates `allow`, and `403` indicates `deny`. In order to provide more information to the user, we added the return of the request content in EMQX 5.0 version.

:::

::: tip

It is recommended to use the `POST` method. When using the `GET` method, some sensitive information may be exposed through HTTP server logs.

For untrusted environments, HTTPS should be used.

:::

## Configure Dynamic Hostname Resolution

By default, the HTTP authorizer resolves the hostname in `url` when the authorizer is created and uses a persistent connection pool. To resolve the hostname for every authorization request, set `hostname_resolution` to `dynamic`.

Dynamic hostname resolution also allows placeholders in the host part of `url`. For example, the following configuration routes authorization requests to different endpoints according to the client's `tenant` attribute:

```hocon
{
    type = http
    method = post
    url = "https://${client_attrs.tenant}.auth.example.com/authz"
    hostname_resolution = dynamic
    allowed_hosts = ["*.auth.example.com"]
    pool_size = 8
    headers {
        "Content-Type" = "application/json"
    }
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    ssl {
        enable = true
    }
}
```

When configuring dynamic hostname resolution, note the following:

- `hostname_resolution` accepts `static` or `dynamic`. The default value is `static`. You can also use `dynamic` with a literal hostname to resolve that hostname for every request.
- If the URL host contains placeholders, `hostname_resolution` must be `dynamic` and `allowed_hosts` must contain at least one entry.
- Each `allowed_hosts` entry must be an exact hostname, such as `auth.example.com`, or a wildcard pattern, such as `*.auth.example.com`. The wildcard matches hostnames under the specified suffix, but not the suffix itself. `allowed_hosts` has no effect when the URL uses a literal hostname.
- Within the URL authority, only the host can contain placeholders. The scheme must be `http` or `https`, and the port, if specified, must be a literal integer. URL userinfo and fragments are not supported. Placeholders in the URL path and query remain supported.
- If EMQX cannot render a valid hostname or the rendered hostname does not match `allowed_hosts`, EMQX does not send the HTTP request and the authorization check fails.
- In `dynamic` mode, requests to all rendered hosts share a connection pool. `pool_size` limits how many idle connections the pool can keep for reuse. Set it to `0` to disable connection reuse. `enable_pipelining` and `max_inactive` do not apply in this mode.
- For HTTPS requests in `dynamic` mode, EMQX applies the configured TLS options to the rendered host. Unless Server Name Indication (SNI) is explicitly configured, EMQX derives it from the rendered hostname.
- OAuth2 is not supported when `hostname_resolution` is `dynamic`.

## Configure with Dashboard

1. On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authorization** on the left navigation tree to enter the **Authorization** page. 

2. Click **Create** at the top right corner, select **HTTP Server** as **Backend**, and click **Next**. The **Configuration** tab is shown as below.

   <img src="./assets/authz-http_ee.png" alt="authz-http_ee" style="zoom:67%;" />

3. Follow the instructions below to do the configuration.

   **HTTP**: Configure the HTTP request method, URL, and request headers here.

   - **Request Method**: Select the HTTP request method, optional values: `GET`, `POST`.
   - **URL**: Enter the URL of the HTTP application. The host part can include [authorization placeholders](./authz.md#authorization-placeholders) when **Hostname Resolution** is set to `Dynamic`.
   - **Hostname Resolution**: Select `Static` to resolve a fixed hostname when creating the authorizer, or `Dynamic` to resolve the hostname for every request. The default option is `Static`. For more information, see [Configure Dynamic Hostname Resolution](#configure-dynamic-hostname-resolution).
   - **Allowed Hosts**: When the URL host contains placeholders, enter the exact hostnames or wildcard patterns that the rendered hostname is allowed to match.
   - **Headers** (optional): Configure the HTTP request headers. Keys and values support using [placeholders](./authz.md#authorization-placeholders).

   **Connection Configuration**: Configure concurrent connections, connection timeout, maximum HTTP requests, and request timeout.

   - **Pool size** (optional): In `Static` mode, specify the persistent connection pool size. The value must be at least `1`. In `Dynamic` mode, specify the number of connections that can be reused across requests, or set it to `0` to disable connection reuse. The default value is `8`.
   - **Connection Timeout** (optional): Enter the duration to wait for a connection timeout, with optional units: **hours**, **minutes**, **seconds**, **milliseconds**.
   - **HTTP Pipelining** (optional): Positive integer, specifies the maximum number of HTTP requests that can be sent without waiting for a response; default value: `100`. This setting does not apply when **Hostname Resolution** is set to `Dynamic`.
   - **Request Timeout** (optional): Enter the duration to wait for a request timeout, with optional units: **hours**, **minutes**, **seconds**, **milliseconds**.
   - **TLS Configuration**: Configure whether to enable TLS.

   **Authorization Configuration**: Complete the configuration of the HTTP request body here. <!--Related information needs to be added.-->

4. Click **Create** to finish the setting.

## Configure with Configuration Items

The HTTP authorization requires configuration with `type=http`.

HTTP `POST` and `GET` requests are supported. Each of them has some specific options. <!--For detailed information, see [authz:http_post](../../configuration/configuration-manual.html#authz:http_post) and [authz:http_get](../../configuration/configuration-manual.html#authz:http_get).-->

Example of an HTTP authorizer configured with `POST` request:

```bash
{
    type = http

    method = post
    url = "http://127.0.0.1:32333/authz/${peercert}?clientid=${clientid}"
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    headers {
        "Content-Type" = "application/json"
        "X-Request-Source" = "EMQX"
    }
}
```

Example of an HTTP authorizer configured with `GET` request:

```bash
{
    type = http

    method = get
    url = "http://127.0.0.1:32333/authz"
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    headers {
        "X-Request-Source" = "EMQX"
    }
}
```
