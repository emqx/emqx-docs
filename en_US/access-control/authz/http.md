# Use HTTP Service

EMQX supports the authorization based on the HTTP service. The user needs to build an external HTTP application as a data source by themselves. EMQX makes requests to the HTTP service and determines the authorization result based on the data returned by the HTTP API, thus achieving complex authorization logic.

::: tip Tip

- Knowledge about [basic EMQX authorization concepts](./authz.md)

:::

## HTTP Request and Response

When the client initiates a subscription or publishing operation, the HTTP Authorizer constructs and sends a request based on the configured request template. Users need to implement authorization logic in the authorization service and return the results according to the following requirements:

- Response `content-type` must be `application/json`.
- If the HTTP Status Code is `200`, the authorization result is granted by HTTP Body. It depends on the value of the `result` field:
  - `allow`: Allow Publish or Subscribe.
  - `deny`: Deny Publish or Subscribe.
  - `ignore`: Ignore this request, it will be handed over to the next authorizer.
- If the HTTP Status Code is `204`, it means that this Publish or Subscribe request is allowed.
- HTTP Status Codes other than `200` and `204`, mean "not match", for example, this authorizer is not applied.

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

## Configure with Dashboard

1. On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authorization** on the left navigation tree to enter the **Authorization** page. 

2. Click **Create** at the top right corner, select **HTTP Server** as **Backend**, and click **Next**. The **Configuration** tab is shown as below.

   ![authz-http_ee](./assets/authz-http_ee.png)

3. Follow the instructions below to do the configuration.

   **HTTP**: Configure the HTTP request method, the IP address and request headers here.

   - **Request Method**: Select the HTTP request method, optional values: `GET`, `POST`.
   - **URL**: Enter the IP address of the HTTP application.
   - **Headers** (optional): Configure the HTTP request headers. <!--The key, value, and add of this content.-->

   **Connection Configuration**: Configure concurrent connections, connection timeout, maximum HTTP requests, and request timeout.

   - **Pool size** (optional): Integer, specifies the number of concurrent connections from EMQX nodes to external HTTP servers; default value: **8**. <!--Is there a range?-->
   - **Connection Timeout** (optional): Enter the duration to wait for a connection timeout, with optional units: **hours**, **minutes**, **seconds**, **milliseconds**.
   - **HTTP Pipelining** (optional): Positive integer, specifies the maximum number of HTTP requests that can be sent without waiting for a response; default value: **100**.
   - **Request Timeout** (optional): Enter the duration to wait for a request timeout, with optional units: **hours**, **minutes**, **seconds**, **milliseconds**.
   - **TLS Configuration**: Configure whether to enable TLS.

   **Authorization Configuration**: Complete the configuration of the HTTP request body here. <!--Related information needs to be added.-->

4. Click **Create** to finish the setting.

## Configure with Configuration Items

The HTTP authorization requires configuration with `type=http`.

HTTP `POST` and `GET` requests are supported. Each of them has some specific options. <!--For detailed information, see [authz:http_post](../../configuration/configuration-manual.md#authz:http_post) and [authz:http_get](../../configuration/configuration-manual.md#authz:http_get).-->

Example of an HTTP authorizer configured with `POST` request:

```bash
{
    type = http
    enable = true

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
    enable = true

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

