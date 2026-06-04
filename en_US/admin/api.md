# REST API

EMQX exposes an HTTP management API designed following the OpenAPI (Swagger) 3.0 specification.

EMQX provides multiple ways to explore and interact with the REST API. After EMQX is started, the following API specification endpoints are available:

<<<<<<< HEAD
| Endpoint | Format | Description |
| --- | --- | --- |
| `/api-spec.html` | HTML | Drill-down style API reference page for human reading. |
| `/api-spec.md` | Markdown | API reference in Markdown format, suited for AI agents and automation tools. |
| `/api-spec.json` | JSON | OpenAPI 3.0 specification in JSON format, suited for scripts and programmatic tooling. |
| `/api-docs/index.html` | HTML | Interactive Swagger UI with "Try it" support for testing API calls directly in the browser. **Deprecated**: will be removed in v7. |

All of the above endpoints require `swagger_support` to be set to `true` (the default) in the Dashboard configuration. Set it to `false` to disable all API documentation endpoints. For more information, see [Dashboard configuration](../configuration/dashboard.md).

This section introduces how to work with the EMQX REST API.
=======
The section introduces how to work with the EMQX REST API.
>>>>>>> origin/release-6.1

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
| 401   | Unauthorized. API key expires or does not exist.             |
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

### Create API Keys

#### Dashboard

You can manually create API keys on the Dashboard by navigating to **System** -> **API Key**:

1. Click the **+ Create** button in the top right corner to open the Create dialog.
2. Configure the API key details:
   - **Name** (required): Enter a name for the API key.
   - **Expire At**: Leave empty for the key to never expire.
   - **Is Enable**: Defaults to enabled.
   - **Role**: Select a role (optional). See [Roles and Permissions](#roles-and-permissions).
   - **Scopes**: Select the scopes to grant (optional). Defaults to all scope permissions. See [API Scopes](#api-scopes).
   - **Note**: Optionally enter a description for the key.
3. Click **Confirm**. The API key and secret key are displayed in the **Created Successfully** dialog.

   ::: warning Important Notice

   Save the API key and secret key immediately. The secret key will not be shown again.

   :::

4. Click **Close** to dismiss the dialog.

You can view key details by clicking its name, edit its expiration, status, or note via the **Edit** button, or remove it with the **Delete** button.

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
- **Scopes (optional)**: Specify the [API Scopes](#api-scopes) the key is allowed to access, as a comma-separated list. When omitted, the key receives all user-visible scopes by default (administrative all-allow, for backward compatibility with earlier releases). Login-only scopes (`user_management`, `mfa_management`, `sso_management`, `api_key_management`) are not valid for API keys. If any of these appear in a bootstrap file entry, EMQX removes them on startup and logs a warning. The key is still created, but without those scopes.

For example:

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

API keys created this way are valid indefinitely.

Each time EMQX starts, it will add the data set in the file to the API key list. If an API key already exists, its Secret Key, Role, and Scopes will be updated.

### Roles and Permissions

The REST API implements role-based access control. When creating an API key, you can assign one of the following three predefined roles:

- **Administrator**: This role can access all resources and is the default value if no role is specified. The corresponding role identifier is `administrator`.
- **Viewer**: This role can only view resources and data, corresponding to all GET requests in the REST API. The corresponding role identifier is `viewer`.
- **Publisher**: Designed specifically for MQTT message publishing, this role is limited to accessing APIs related to message publishing. The corresponding role identifier is `publisher`.

::: tip Note
`publisher` keys only accept the `publish` scope. When assigning scopes, any scope other than `publish` returns HTTP 400. If you change a key's role to `publisher`, include `"scopes": ["publish"]` or an empty list in the same request; otherwise the request is rejected if the key's existing scopes contain anything other than `publish`.
:::

### API Scopes

Scopes are a per-key permission dimension introduced in EMQX 5.10 that declare which business areas of the REST API a key is allowed to reach. Scopes and [Roles and Permissions](#roles-and-permissions) are independent of each other and enforced together, forming two separate layers of access control:

| Dimension | Purpose | Granularity |
| --------- | ------- | ----------- |
| **Role** | Limits HTTP verbs (read-only vs. writes, publish-only, etc.) | Request action |
| **Scope** | Limits the API domain (clients, rules, monitoring, ...) | Resource area |

Every request is checked against both dimensions: the role check and the scope check. A request is accepted only when both checks pass.

In microservice and integration scenarios, external systems typically need access to only a subset of EMQX's management surface: a monitoring platform only needs the `monitoring` scope, a rules-publishing service only needs `data_integration`, and a cluster operator tool only needs `cluster_operations`. Scopes let you assign keys using the principle of least privilege, minimizing the blast radius if a key is ever leaked.

#### Built-in Scopes

EMQX 5.10 ships with 10 scopes that you can combine freely when creating an API key:

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

In addition to these API-key scopes, Dashboard login users have four login-only scopes that apply exclusively to browser sessions and cannot be assigned to API keys. For details on how these scopes are assigned and enforced for login users, see [Login User Scopes](../dashboard/system.md#login-user-scopes).

| Scope | Required role | Purpose |
| --- | --- | --- |
| `user_management` | Administrator | Manage Dashboard users. |
| `sso_management` | Administrator | Manage SSO backends and SSO user records. |
| `api_key_management` | Administrator | Manage API keys. |
| `mfa_management` | Any | Manage MFA for own account; administrators can manage other users' MFA. |

::: tip
Scope names are stable identifiers that do not change across EMQX upgrades. Even if a route's OpenAPI tag is renamed, a key configured with the same scope keeps working.
:::

Dashboard login, SSO callbacks, and API key self-management endpoints (for example, `/api_key`) do not accept API-key authentication, regardless of the key's `scopes` configuration. This is a built-in Dashboard security boundary, unrelated to the scope model.

#### Default Behavior of `scopes`

The `scopes` field on an API key follows these rules:

| Value of `scopes` | Meaning |
| --- | --- |
| **Absent** (field missing) | All business endpoints are allowed. This is the backward-compatible default for keys created before the scopes feature existed. |
| **Empty list** `[]` | Every business endpoint is denied. Useful as a soft disable without removing the key. |
| **Explicit list** (e.g. `["monitoring", "cluster_operations"]`) | Only requests under those scopes are allowed. |

When a bootstrap file entry omits the scopes segment, the key is explicitly written with all user-visible scopes (administrative all-allow), so upgrades don't silently strip privileges from existing bootstrap-provisioned keys.

The same three-state model applies to Dashboard login users. When a login user's `scopes` field is absent, the user receives a role-derived default set: administrators get all scopes, including the four login-only ones; viewers get all 10 API-key scopes, but none of the four login-only scopes (including `mfa_management`) unless explicitly assigned.

#### List Available Scopes

EMQX exposes two endpoints to query the available scope catalogues:

- `GET /api/v5/api_key_scopes`: returns the scopes that can be assigned to API keys (the 10 business-domain scopes listed above). Authenticate with an API key.
- `GET /api/v5/user_scopes`: returns all scopes available to Dashboard login users, including the four login-only scopes. Authenticate with a bearer token.

Use these endpoints to populate a scope-picker UI or validate automation scripts:

```bash
# API key scopes
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# Login user scopes (requires bearer token)
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### Assign Scopes

Scopes can be set from any of the following entry points:

- **Dashboard**: When creating or editing a key under **System** -> **API Key**, tick the scopes to grant.
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

