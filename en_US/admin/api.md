# REST API

EMQX exposes an HTTP management API that follows the OpenAPI 3.0 specification.

EMQX provides multiple ways to explore and interact with the REST API. After EMQX is started, the following API specification endpoints are available:

| Endpoint | Format | Description |
| --- | --- | --- |
| `/api-spec.html` | HTML | Drill-down style API reference page for human reading. |
| `/api-spec.md` | Markdown | API reference in Markdown format, suited for AI agents and automation tools. |
| `/api-spec.json` | JSON | OpenAPI 3.0 specification in JSON format, suited for scripts and programmatic tooling. |
| `/api-spec/:tag[/:name]` | JSON | Focused OpenAPI 3.0 specification for an API tag, optionally narrowed by a matching request or response schema name. |
| `/api-docs/swagger.json` | JSON | Full OpenAPI 3.0 specification for external Swagger UI deployments and other compatible tools. |

All of the above endpoints require `swagger_support` to be set to `true` (the default) in the Dashboard configuration. Set it to `false` to disable all API documentation endpoints. For more information, see [Dashboard configuration](../configuration/dashboard.md).

Starting from EMQX 6.3.0, EMQX no longer bundles Swagger UI. For backward compatibility, requests to `/api-docs` or `/api-docs/index.html` return HTTP 308 and redirect to `/api-spec.html`. Except for `/api-docs/index.html` and `/api-docs/swagger.json`, other `/api-docs/*` subpaths that previously served Swagger UI assets return HTTP 404.

This section introduces how to work with the EMQX REST API.

::: tip
Starting from EMQX 6.3.0, [feature gates](../deploy/feature-gates.md) can disable optional features at startup. REST API paths provided by disabled features are not loaded as accessible API endpoints. When the `dashboard` feature is enabled, you can call `GET /api/v5/features` to view the resolved feature set.
:::

## Access API Specification Endpoints

Starting from EMQX 6.3.0, you must authenticate to retrieve API specification content from the endpoints listed above.

Authenticate programmatic requests with either Basic authentication using an API key and secret key or a bearer token. For instructions, see [Authentication](#authentication).

Access to the API specification is read-only and does not depend on the API key's role or scopes.

For `/api-spec.md`, `/api-spec.json`, `/api-spec/:tag[/:name]`, and `/api-docs/swagger.json`, a request with missing or invalid credentials returns HTTP `401`. The `WWW-Authenticate` response header advertises Basic and Bearer authentication. The response body matches the requested format and contains a minimal API specification. It describes the supported authentication schemes and lists two public endpoints: `POST /api/v5/login` for obtaining a bearer token and `GET /api/v5/status` for checking broker status. The minimal response does not include the requested API specification content.

For browser access, EMQX accepts a valid `emqx_auth` session cookie. An unauthenticated request to `/api-spec.html` returns HTTP `401` and displays a sign-in page instead of the full API Spec Explorer. This response advertises only Bearer authentication to prevent the browser from opening its native Basic authentication dialog. After you sign in with your Dashboard username and password, EMQX creates the `emqx_auth` session cookie and loads the full explorer. Signing out clears the session cookie.

Requests to `/api-docs` and `/api-docs/index.html` do not require authentication because these endpoints only redirect to `/api-spec.html`. Authentication is required after the redirect to access the full explorer.

## Basic Path

EMQX has version control on the REST API; all API paths from EMQX 5.0.0 start with `/api/v5`.

## HTTP Headers

Most API requests require the `Accept` header to be set to `application/json`, and then the response will be returned in JSON format unless otherwise specified.

## HTTP Response Status Code

EMQX follows the [HTTP Response Status Code](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) standard. The possible status codes are as follows:

| Codes | Description                                                  |
| ----- | ------------------------------------------------------------ |
| 200   | Request successfully, and the returned JSON data will provide more details |
| 201   | Created successfully, and the new object will be returned in the Body |
| 204   | Request successfully. Usually used for delete and update operations, and the returned Body will be empty |
| 400   | Bad Request. Usually request body or parameter error         |
| 401   | Unauthorized. Authentication credentials are missing, invalid, or expired. |
| 403   | Forbidden. Check if the object is in use or has dependency constraints. |
| 404   | Not Found. You can refer to the `message` field in the Body to check the reason |
| 409   | Conflict. The object already exists or the number limit is exceeded |
| 500   | Internal Server Error. Check the reason in the Body and logs |

## Authentication

EMQX's REST API supports two main methods for authentication: basic authentication using API keys and bearer token authentication.

### Basic Authentication Using API Keys

In this method, you use API keys and secret keys as the username and password to authenticate your API requests. EMQX's REST API follows [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework), where these credentials are required. Before using the EMQX REST API, you must create an API key. See [API Key Management](#api-key-management) for details.

::: tip Note

For security reasons, starting from EMQX 5.0.0, you cannot use Dashboard user credentials to authenticate REST API requests. Instead, you need to create and use API keys for authentication.

:::

#### Authenticate with API Keys

Once you have your API key and secret key, use the API key as the username and the secret key as the password for Basic Authentication.

Examples in different languages:

:::: tabs type:card
:::tab cURL

```bash
curl -X GET http://localhost:18083/api/v5/nodes \
     -u 4f33d24d7b8e448d:gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD \
     -H "Content-Type: application/json"
```

:::
::: tab Java

```java
import okhttp3.*;

import java.io.IOException;

public class EMQXNodesAPIExample {
    public static void main(String[] args) {
        try {
            String username = "4f33d24d7b8e448d";
            String password = "gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD";

            OkHttpClient client = new OkHttpClient();

            Request request = new Request.Builder()
                    .url("http://localhost:18083/api/v5/nodes")
                    .header("Content-Type", "application/json")
                    .header("Authorization", Credentials.basic(username, password))
                    .build();

            Response response = client.newCall(request).execute();
            System.out.println(response.body().string());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

:::
::: tab Python

```python
import urllib.request
import json
import base64

username = '4f33d24d7b8e448d'
password = 'gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD'

url = 'http://localhost:18083/api/v5/nodes'

req = urllib.request.Request(url)
req.add_header('Content-Type', 'application/json')

auth_header = "Basic " + base64.b64encode((username + ":" + password).encode()).decode()
req.add_header('Authorization', auth_header)

with urllib.request.urlopen(req) as response:
    data = json.loads(response.read().decode())

print(data)

```

:::
::: tab Go

```go
package main

import (
    "fmt"
    "net/http"
    "bytes"
    "encoding/json"
)

func main() {
    username := "4f33d24d7b8e448d"
    password := "gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD"

    url := "http://localhost:18083/api/v5/nodes"

    req, err := http.NewRequest("GET", url, nil)
    if err != nil {
        panic(err)
    }
    req.SetBasicAuth(username, password)
    req.Header.Set("Content-Type", "application/json")

    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    buf := new(bytes.Buffer)
    _, err = buf.ReadFrom(resp.Body)
    if err != nil {
        panic(err)
    }

    var data interface{}
    json.Unmarshal(buf.Bytes(), &data)
    fmt.Println(data)
}

```

:::
::: tab JavaScript

```js
const axios = require('axios')

const username = '4f33d24d7b8e448d'
const password = 'gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD'

axios
  .get('http://localhost:18083/api/v5/nodes', {
    auth: {
      username: username,
      password: password,
    },
    headers: {
      'Content-Type': 'application/json',
    },
  })
  .then((response) => {
    console.log(response.data)
  })
  .catch((error) => {
    console.log(error)
  })
```

:::
::::

### Bearer Token Authentication

As an alternative to API key-based authentication, you can use bearer tokens for secure and programmatic access to the EMQX REST API. To obtain a bearer token, send a request to the login API endpoint as described below.

#### Obtain a Bearer Token

To request a bearer token, make an HTTP `POST` request to the following login API endpoint:

```bash
POST http://your-emqx-address:8483/api/v5/login
```

**Headers:**

- `Content-Type: application/json`

**Request Body:**

```json
{
  "username": "admin",
  "password": "yourpassword"
}
```

- Replace `your-emqx-address` with the address or IP of your EMQX node.
- Replace `"admin"` and `"yourpassword"` with your EMQX Dashboard credentials.

The response will include the bearer token, which you can use to authenticate API requests.

#### Use Bearer Token for Authentication

Once you have the bearer token, include it in the `Authorization` header of your API requests, like this:

```bash
--header "Authorization: Bearer <your-token>"
```

## API Key Management

This section describes how to create and manage API keys and configure their roles, namespaces, and scopes.

### Create API Keys

#### Dashboard

You can manually create API keys on the Dashboard by navigating to **System** -> **API Keys**:

1. Click the **+ Create** button in the top right corner to open the Create dialog.
2. Configure the API key details:
   - **Name** (required): Enter a name for the API key.
   - **Expire At**: Leave empty for the key to never expire.
   - **Is Enable**: Defaults to enabled.
   - **Role**: Select a role (optional). See [Roles and Permissions](#roles-and-permissions).
   - **Namespace**: The switch is off by default. For a global administrator, leaving it off creates a global API key. Turn it on and select a namespace to create the key in that namespace. A namespaced administrator can create keys only in their own namespace.
   - **Permission Mode**: For an Administrator or Viewer key, select how to assign scopes. This field is not displayed for Publisher keys, which use the role-default `publish` scope. For scope behavior and restrictions, see [API Scopes](#api-scopes).
     - **Role Default Scopes**: Use the defaults for the selected role. Changes to the role defaults take effect automatically.
     - **System-level Permissions**: Grant only the `system` scope.
     - **Custom Restricted Permissions**: Select one or more scopes to limit which API areas the key can access. If you leave **Scopes** empty, the key cannot access scope-protected APIs.
   - **Scopes**: Appears when you select **Custom Restricted Permissions**. Select the scopes to grant.
   - **Note**: Optionally enter a description for the key.
3. Click **Confirm**. The API key and secret key are displayed in the **Created Successfully** dialog.

   ::: warning Important Notice

   Save the API key and secret key immediately. The secret key will not be shown again.

   :::

4. Click **Close** to dismiss the dialog.

**Permission Mode** is available only in the Dashboard. When using the REST API, configure the `scopes` field directly. For details, see [Default Behavior of `scopes`](#default-behavior-of-scopes).

You can view key details by clicking its name. Use the **Edit** button to change its expiration, status, role, permission mode, scopes, or note. Use the **Delete** button to remove the key.

#### REST API

Use a Dashboard user's bearer token to create or update an API key through the REST API. The API key management endpoints do not accept API key authentication.

Starting from EMQX 6.0.4, the request body for `POST /api/v5/api_key` and `PUT /api/v5/api_key/:name` accepts a top-level `namespace` field. For example, the following request creates an administrator API key in the `team-a` namespace:

```bash
curl -X POST "http://localhost:18083/api/v5/api_key" \
  -H "Authorization: Bearer <your-token>" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "team-a-key",
    "role": "administrator",
    "namespace": "team-a",
    "scopes": "unset"
  }'
```

Setting `scopes` to `"unset"` explicitly applies the role-default scopes. Omitting `scopes` from a create request has the same effect.

You can specify the namespace in either of these ways:

- Provide a bare role, such as `administrator`, together with the `namespace` field.
- Encode the namespace in the role as `ns:<namespace>::<role>`, such as `ns:team-a::administrator`.

Both forms remain supported. If a request contains both forms, the namespaces must match. EMQX returns HTTP 400 if they differ or if `namespace` is empty. An API key's namespace cannot be changed after the key is created.

Starting from EMQX 6.3.0, neither form can use a namespace listed in `multi_tenancy.deny_namespaces`. For configuration details, see [Denied Namespace Names](../multi-tenancy/namespace-global-settings.md#denied-namespace-names).

To create a global API key, omit `namespace` and use a role without a namespace prefix. Setting `namespace` to the string `"global"` does not select the global scope.

#### Bootstrap File

You can also create API keys using the bootstrap file method. Add the following configuration file to specify the file location:

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

In the specified file, add multiple API keys in the format `{API Key}:{Secret Key}:{?Role}:{?Scopes}`, separated by new lines:

- **API Key**: Any string as the key identifier.
- **Secret Key**: Use a random string as the secret key.
- **Role (optional)**: Specify the key's [role](#roles-and-permissions).
- **Scopes (optional)**: Specify the [API Scopes](#api-scopes) the key is allowed to access, as a comma-separated list. When omitted, the key receives the defaults for its role. Login-only scopes (`user_management`, `mfa_management`, `sso_management`, `api_key_management`) are not valid for API keys. If any of these appear in a bootstrap file entry, EMQX removes them on startup and logs a warning. The key is still created, but without those scopes.

For example:

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

Among the scopes that can be assigned to API keys, `system` is the only one that grants administrator-equivalent permissions. Starting from EMQX 6.0.4, if a bootstrap entry combines an administrator-equivalent scope with scopes that do not grant administrator-equivalent permissions, EMQX removes all administrator-equivalent scopes, keeps the remaining scopes, logs a warning, and continues to create or update the key. In contrast, the REST API rejects such a mixed scope list with HTTP 400 and does not apply any scope changes.

API keys created this way are valid indefinitely.

Each time EMQX starts, it will add the data set in the file to the API key list. If an API key already exists, its Secret Key, Role, and Scopes will be updated.

### Manage API Keys as a Namespaced Administrator

Starting from EMQX 6.0.4, a namespaced Dashboard administrator can manage API keys within their own namespace. The administrator must authenticate with a bearer token.

| Operation | Namespaced Administrator Behavior |
| --- | --- |
| Create an API key | Can create a key only in the administrator's namespace. Omitting the namespace, specifying the global namespace, or specifying another namespace returns HTTP 403. |
| List API keys | Sees only keys in the administrator's namespace. Global keys and keys in other namespaces are filtered from the response. |
| Read, update, or delete an API key | Can operate only on keys in the administrator's namespace. A key in another namespace returns HTTP 404 so that its existence is not disclosed. |
| Change an API key's namespace | Cannot move a key to another namespace. The update returns HTTP 400. |

A global Dashboard administrator can continue to manage API keys across all namespaces.

### Roles and Permissions

The REST API implements role-based access control. When creating an API key, you can assign one of the following three predefined roles:

- **Administrator**: This role can access all resources and is the default value if no role is specified. The corresponding role identifier is `administrator`.
- **Viewer**: This role can only view resources and data, corresponding to all GET requests in the REST API. The corresponding role identifier is `viewer`.
- **Publisher**: Designed specifically for MQTT message publishing, this role is limited to accessing APIs related to message publishing. The corresponding role identifier is `publisher`.

::: tip Note
`publisher` keys only accept the `publish` scope. When assigning scopes, any scope other than `publish` returns HTTP 400. If you change a key's role to `publisher`, include `"scopes": ["publish"]` or an empty list in the same request; otherwise the request is rejected if the key's existing scopes contain anything other than `publish`.
:::

### API Scopes

Scopes are a per-key permission dimension that declares which business areas of the REST API a key is allowed to reach. Scopes and [Roles and Permissions](#roles-and-permissions) are independent of each other and enforced together, forming two separate layers of access control:

| Dimension | Purpose | Granularity |
| --------- | ------- | ----------- |
| **Role** | Limits HTTP verbs (read-only vs. writes, publish-only, etc.) | Request action |
| **Scope** | Limits the API domain (clients, rules, monitoring, ...) | Resource area |

Every request is checked against both dimensions: the role check and the scope check. A request is accepted only when both checks pass.

In microservice and integration scenarios, external systems typically need access to only a subset of EMQX's management surface: a monitoring platform only needs the `monitoring` scope, a rules-publishing service only needs `data_integration`, and a cluster operator tool only needs `cluster_operations`. Scopes let you assign keys using the principle of least privilege, minimizing the blast radius if a key is ever leaked.

::: tip
Scope names are stable identifiers that do not change across EMQX upgrades. Even if a route's OpenAPI tag is renamed, a key configured with the same scope keeps working.
:::

#### Built-in API Key Scopes

EMQX provides 10 scopes for API keys:

| Scope | Name | Typical API areas |
| --- | --- | --- |
| `connections` | Connection management | `/clients`, `/subscriptions`, `/topics`, `/banned`, `/retainer`, `/file_transfer`, `/mqtt/delayed`, `/mqtt/topic_rewrite`, ... |
| `publish` | Message publishing | `/publish`, `/publish/bulk` |
| `data_integration` | Data integration | `/rules`, `/connectors`, `/actions`, `/schema_registry`, `/schema_validations`, `/message_transformations`, `/exhooks`, `/ai/*` |
| `access_control` | Access control | `/authentication`, `/authorization/*` |
| `gateways` | Protocol gateways | `/gateways`, `/coap/*`, `/lwm2m/*`, `/gcp_devices`, ... |
| `monitoring` | Monitoring data | `/metrics`, `/stats`, `/monitor*`, `/alarms`, `/trace`, `/slow_subscriptions`, `/telemetry`, `/prometheus/{auth,stats,data_integration,...}`, ... |
| `cluster_operations` | Cluster operations | `/cluster*`, `/nodes`, `/load_rebalance`, `/node_eviction`, `/mt/*`, ... |
| `system` | System configuration | `/configs*`, `/listeners*`, `/plugins*`, `/ds/*`, `/data/*`, `/status`, `/relup`, `/opentelemetry*`, `/prometheus`, ... |
| `audit` | Audit log | `/audit` |
| `license` | License | `/license*` |

::: warning Do Not Mix Administrator-Equivalent and Restricted Scopes

EMQX classifies `system`, `user_management`, `api_key_management`, and `sso_management` as administrator-equivalent scopes, referred to as `privilege scopes` in validation messages. Combining these scopes with restricted scopes would not reduce the account's effective permissions. Of the four scopes, only `system` can be assigned to API keys; the other three are described under [Login-Only Scopes](#login-only-scopes).

Therefore, starting from EMQX 6.0.4, an explicit scope list used to create or update an API key must contain either `system` alone or scopes that do not include `system`. A mixed list returns HTTP 400, and no changes are applied.

Existing mixed scope lists continue to work, with `system` remaining effective. The next explicit scope update must use either `system` alone or a list that does not include `system`. When such a key is edited in the Dashboard, the user is prompted to select a permission mode before saving.

:::

#### Login-Only Scopes

In addition to these API-key scopes, Dashboard login users have 4 login-only scopes that apply exclusively to browser sessions and cannot be assigned to API keys. For details on how these scopes are assigned and enforced for login users, see [Login User Scopes](../dashboard/system.md#login-user-scopes).

| Scope | Required role | Purpose |
| --- | --- | --- |
| `user_management` | Administrator | Manage Dashboard users. |
| `sso_management` | Administrator | Manage SSO backends and SSO user records. |
| `api_key_management` | Administrator | Manage API keys. |
| `mfa_management` | Any | Manage MFA for own account; administrators can manage other users' MFA. |

#### Restrictions for Namespaced Callers

Namespaced callers (users or API keys whose role is restricted to a specific namespace) are subject to additional endpoint-level restrictions beyond scope checks. Scope grants do not override these restrictions.

Namespaced API keys cannot call message publishing APIs, including `POST /api/v5/publish`. This restriction applies even if the key's scope list contains `publish`; assigning a scope does not override namespace-level restrictions.

Even when a namespaced caller has the `connections` or `monitoring` scope, the caller cannot access cluster-wide endpoints that read or manipulate raw MQTT message content, including retained and delayed message stores. The following message-related endpoints return `403 Forbidden`:

- `GET /clients/:clientid/mqueue_messages`
- `GET /clients/:clientid/inflight_messages`
- `GET /mqtt/retainer/messages`
- `GET /mqtt/retainer/message/:topic`
- `DELETE /mqtt/retainer/message/:topic`
- `DELETE /mqtt/retainer/messages`
- `GET /mqtt/delayed/messages`
- `GET /mqtt/delayed/messages/:node/:msgid`
- `DELETE /mqtt/delayed/messages/:node/:msgid`
- `DELETE /mqtt/delayed/messages/:topic`

For trace operations, `GET /trace` lists only traces within the caller's namespace. The following per-trace operations return `404 Not Found` when the trace belongs to a different namespace:

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

This behavior prevents the disclosure of traces in other namespaces. The bulk-delete operation (`DELETE /trace`) returns `403 Forbidden` for namespaced callers; only global administrators can clear all traces.

Dashboard login, SSO callbacks, and API key self-management endpoints (for example, `/api_key`) do not accept API-key authentication, regardless of the key's `scopes` configuration. This is a built-in Dashboard security boundary, unrelated to the scope model.

#### Default Behavior of `scopes`

Starting from EMQX 6.0.4, the `scopes` field on an API key follows these rules:

| Value of `scopes` | Meaning |
| --- | --- |
| **Absent in a create request** | Use the defaults for the selected role. |
| **Absent in an update request** | Preserve the key's current scope setting. |
| **Role-default sentinel** `"unset"` | Remove the explicit scope setting and use the defaults for the selected role. Changes to the role defaults take effect automatically. |
| **Empty list** `[]` | Every business endpoint is denied. Useful as a soft disable without removing the key. |
| **Explicit list** (e.g. `["monitoring", "cluster_operations"]`) | Only requests under those scopes are allowed. |

An explicit list that contains the same set of scopes as the role defaults has the same effect as `"unset"`. The key continues to follow changes to the role defaults. The comparison is order-independent.

When a bootstrap file entry omits the scopes segment, EMQX applies the defaults for the specified role when processing the file.

Scopes determine which API areas a key can access. They do not override the key's role or namespace restrictions. A request is allowed only when its role, scope, and namespace checks all pass.

#### List Available Scopes

EMQX exposes two endpoints to query the available scope catalogues:

- `GET /api/v5/api_key_scopes`: returns the scopes that can be assigned to API keys (the 10 business-domain scopes listed above). Authenticate with an API key.
- `GET /api/v5/user_scopes`: returns all scopes available to Dashboard login users, including the 4 login-only scopes. Authenticate with a bearer token.

Use these endpoints to populate a scope-picker UI or validate automation scripts:

```bash
# API key scopes
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# Login user scopes (requires bearer token)
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### Assign Scopes

Scopes can be set from any of the following entry points:

- **Dashboard**: When creating or editing a key under **System** -> **API Keys**, select a **Permission Mode**. Select individual scopes only for **Custom Restricted Permissions**.
- **REST API**: Include `"scopes": ["monitoring", "cluster_operations"]` in the create/update request body.
- **Bootstrap file**: Provide a comma-separated scope list as the 4th segment of each line, e.g. `my-app:my-secret:administrator:monitoring,cluster_operations`.

## Pagination

For some APIs with large amounts of data, pagination functionality is provided. There are 2 types of pagination methods based on the data characteristics.

### Page Number Pagination

In most APIs that support pagination, you can control the pagination by using the `page` (page number) and `limit` (page size) parameters. The maximum page size is `10000`. If the `limit` parameter is not specified, the default is `100`.

For example:

```bash
GET /clients?page=1&limit=100
```

In the response result, the `meta` field will contain pagination information. EMQX cannot predict the total number of data entries for requests using search conditions. Therefore, the `meta.hasnext` field indicates whether there is another page of data:

```json
{
  "data":[],
  "meta":{
    "count":0,
    "limit":20,
    "page":1,
    "hasnext":false
  }
}
```

### Cursor Pagination

In a few APIs where data changes rapidly, and page number pagination is inefficient, cursor pagination is used.

You can specify the starting position of the data using the `position` or `cursor` (starting position) parameter, and the `limit` (page size) parameter specifies the number of entries loaded from the starting position. The maximum page size is `10000`. If the `limit` parameter is not specified, it defaults to `100`.

For example:

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

The `meta` field in the response will contain pagination information, with `meta.position` or `meta.cursor` indicating the starting position of the next page:

```json
{
    "meta": {
        "start": "1716187698009179275_0",
        "position": "1716187698491337643_0"
    },
    "data": [
        {
            "inserted_at": "1716187698260190832",
            "publish_at": 1716187698260,
            "from_clientid": "mqttx_70e2eecf_10",
            "from_username": "undefined",
            "msgid": "000618DD161F682DF4450000F4160011",
            "mqueue_priority": 0,
            "qos": 0,
            "topic": "t/1",
            "payload": "SGVsbG8gRnJvbSBNUVRUWCBDTEk="
        }
    ]
}
```

This pagination method efficiently handles scenarios where data changes rapidly, ensuring continuity and efficiency in data retrieval.

## Error Codes

Besides the HTTP response status codes, EMQX also defines a list of error codes to identify specific errors.

When an error happens, the error code is returned in JSON format by the Body:

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| Error Codes                                    | Description                                                  |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | Wrong username or password <img width=200/>                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | Wrong username & password or key & secret                    |
| BAD_REQUEST                                    | Request parameters not legal                                 |
| NOT_MATCH                                      | Conditions not matched                                       |
| ALREADY_EXISTS                                 | Resources already exist                                      |
| BAD_CONFIG_SCHEMA                              | Configuration data not legal                                 |
| BAD_LISTENER_ID                                | Bad listener ID                                              |
| BAD_NODE_NAME                                  | Bad Node Name                                                |
| BAD_RPC                                        | RPC Failed. Check the cluster status and the requested node status |
| BAD_TOPIC                                      | Topic syntax error, topic needs to comply with the MQTT protocol standard |
| EXCEED_LIMIT                                   | Resources to be created exceed the maximum limit or minimum limit |
| INVALID_PARAMETER                              | Request parameters not legal and exceed the boundary value   |
| CONFLICT                                       | Conflicting request resources                                |
| NO_DEFAULT_VALUE                               | Request parameters do not use default values                 |
| DEPENDENCY_EXISTS                              | Resource depends on other resources                          |
| MESSAGE_ID_SCHEMA_ERROR                        | Message ID parsing error                                     |
| INVALID_ID                                     | Bad ID schema                                                |
| MESSAGE_ID_NOT_FOUND                           | Message ID does not exist                                    |
| NOT_FOUND                                      | Resource not found or does not exist                         |
| CLIENTID_NOT_FOUND                             | Client ID not found or does not exist                        |
| CLIENT_NOT_FOUND                               | Client not found or does not exist(usually not an MQTT client) |
| RESOURCE_NOT_FOUND                             | Resource not found                                           |
| TOPIC_NOT_FOUND                                | Topic not found                                              |
| USER_NOT_FOUND                                 | User not found                                               |
| INTERNAL_ERROR                                 | Server inter error                                           |
| SERVICE_UNAVAILABLE                            | Service unavailable                                          |
| SOURCE_ERROR                                   | Source error                                                 |
| UPDATE_FAILED                                  | Update fails                                                 |
| REST_FAILED                                    | Reset source or configuration fails                          |
| CLIENT_NOT_RESPONSE                            | Client not responding                                        |
