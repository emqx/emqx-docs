# REST API

EMQX exposes an HTTP management API designed following the OpenAPI (Swagger) 3.0 specification.

After EMQX is started, you can visit [http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) to view the API document and execute the management APIs from the Swagger UI. By default, under the Dashboard configuration, `swagger_support` is set to `true`, indicating Swagger UI support is enabled, which means all Swagger-related features are turned on, such as generating interactive API documentation. You can also set it to `false` to disable this feature. For more information, see [Dashboard configuration](./configuration/dashboard.md).

This section introduces how to work with the EMQX REST API.

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

For security reasons, starting from EMQX 5.0.0, you cannot use Dashboard user credentials to authenticate REST API requests. Instead, you need to create and use API keys for authentication. Note that role-based API credentials are available only in the EMQX Enterprise edition.

:::

#### API Key Authentication Examples

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

This section describes how to create and manage API keys and configure their roles and scopes.

### Create API Keys

#### Dashboard

You can manually create API keys on the Dashboard by navigating to **System** -> **API Key**:

1. Click the **+ Create** button in the top-right corner to open the Create API Key dialog.
2. Configure the API key details:
   - **Name** (required): Enter a name for the API key.
   - **Expire At**: Leave empty for the key to never expire.
   - **Is Enable**: Defaults to enabled.
   - **Role**: Select a role (optional, EMQX Enterprise only). See [Roles and Permissions](#roles-and-permissions).
   - **Scopes**: Select the scopes to grant (optional). If omitted, the key uses the default scopes for the selected role. See [API Scopes](#api-scopes).
   - **Note**: Optionally enter a description for the key.
3. Click **Confirm**. The API key and secret key are displayed in the **Created Successfully** dialog.

   ::: warning Important Notice

   Save the API key and secret key immediately. The secret key will not be shown again.

   :::

4. Click **Close** to dismiss the dialog.

You can view key details by clicking its name. Use the **Edit** button to change its expiration, status, role, scopes, or note. Use the **Delete** button to remove the key.

#### Bootstrap File

You can also create API keys using the bootstrap file method. Add the following configuration to the `emqx.conf` file to specify the file location:

```bash
api_key {
  bootstrap_file = "etc/default_api_key.conf"
}
```

In the specified file, add multiple API keys in the format `{API Key}:{Secret Key}:{?Role}:{?Scopes}`, separated by new lines:

- **API Key**: Any string as the key identifier.
- **Secret Key**: Use a random string as the secret key.
- **Role (optional)**: Specify the key's [role](#roles-and-permissions), applicable only in the Enterprise edition.
- **Scopes (optional)**: Specify the [API scopes](#api-scopes) the key can access as a comma-separated list. When omitted, the key uses the default scopes for its role: the 10 API-key scopes for an Administrator or Viewer, or `publish` for a Publisher. Login-only scopes (`user_management`, `mfa_management`, `sso_management`, and `api_key_management`) are not valid for API keys. If a bootstrap entry contains these scopes, EMQX removes them, logs a warning, and continues to create or update the key. Starting from EMQX 5.10.5, if an entry contains both `system` and another valid API-key scope, EMQX removes `system`, keeps the other scopes, logs a warning, and continues to create or update the key.

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
`publisher` keys only accept the `publish` scope. When assigning scopes, any scope other than `publish` returns HTTP 400. If you change a key's role to `publisher`, include `"scopes": ["publish"]` or an empty list in the same request. Otherwise, the request is rejected if the key's existing scopes contain anything other than `publish`.
:::

### API Scopes

Scopes are a per-key permission dimension that declares which business areas of the REST API a key can access. Scopes and [Roles and Permissions](#roles-and-permissions) are independent and enforced together, forming two layers of access control:

| Dimension | Purpose | Granularity |
| --- | --- | --- |
| **Role** | Limits HTTP methods, such as read-only, write, or publish-only operations. | Request action |
| **Scope** | Limits the API domain, such as clients, rules, or monitoring. | Resource area |

Every request must pass both the role check and the scope check. Scopes let you apply the principle of least privilege by granting an integration access only to the API areas it needs.

::: tip
Scope names are stable identifiers that do not change across EMQX upgrades. Even if a route's OpenAPI tag is renamed, a key configured with the same scope keeps working.
:::

#### Built-in API Key Scopes

EMQX 5.10 provides 10 scopes for API keys:

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

::: warning Important Notice

EMQX classifies `system`, `user_management`, `api_key_management`, and `sso_management` as administrator-equivalent scopes, referred to as `privilege scopes` in validation messages. Combining these scopes with restricted scopes would not reduce the account's effective permissions. Of the four scopes, only `system` can be assigned to API keys; the other three are described under [Login-Only Scopes](#login-only-scopes). `mfa_management` is not an administrator-equivalent scope.

Starting from EMQX 5.10.5, when creating or updating an API key through the REST API or Dashboard, an explicitly provided scope list must contain either `system` alone or scopes that do not include `system`. If a create or update request explicitly provides a mixed list, EMQX rejects the request with HTTP 400. The error message begins with `Privilege scopes cannot be combined with other scopes`. A rejected create request does not create the key, and a rejected update request leaves the key unchanged. Omitting `scopes` or providing an empty list `[]` does not trigger this restriction.

API keys with mixed scope lists created before EMQX 5.10.5 continue to work, with `system` remaining effective. The next update that explicitly provides `scopes` must split the list by using either `system` alone or scopes that do not include `system`.

:::

#### Login-Only Scopes

In addition to the 10 API-key scopes, Dashboard login users have four login-only scopes that apply only to browser sessions and cannot be assigned to API keys. For details on how these scopes are assigned and enforced for login users, see [Login User Scopes](./dashboard/system.md#login-user-scopes).

| Scope | Required role | Purpose |
| --- | --- | --- |
| `user_management` | Administrator | Manage Dashboard users. |
| `sso_management` | Administrator | Manage SSO backends and SSO user records. |
| `api_key_management` | Administrator | Manage API keys. |
| `mfa_management` | Any | Manage MFA for the user's own account; administrators can manage other users' MFA. |

Dashboard login, SSO callbacks, and API key management endpoints (for example, `/api_key`) do not accept API key authentication, regardless of the key's `scopes` configuration. This is a built-in Dashboard security boundary and is unrelated to the scope model.

#### Default Behavior of `scopes`

The `scopes` field on an API key has the following behavior:

| Context or value | Meaning |
| --- | --- |
| Omitted when creating a key | Uses the default scopes for the selected role: the 10 API-key scopes for an Administrator or Viewer, or `publish` for a Publisher. |
| Omitted when updating a key | Preserves the key's current scope setting. |
| **Empty list** `[]` | Denies access to all scope-protected endpoints. Unmapped or public endpoints remain accessible. |
| **Explicit list**, such as `["monitoring", "cluster_operations"]` | Allows access only to scope-protected endpoints in the listed scopes. |

API keys upgraded from a version without scope support can retain a legacy unset scope value. For backward compatibility, this value allows access to all endpoints that API keys are otherwise allowed to access.

When a bootstrap entry omits Scopes, EMQX stores the default scopes for its role.

#### List Available Scopes

EMQX provides two endpoints for querying available scopes:

- `GET /api/v5/api_key_scopes`: Returns the 10 scopes that can be assigned to API keys. Authenticate with an API key.
- `GET /api/v5/user_scopes`: Returns all scopes available to Dashboard login users, including the four login-only scopes. Authenticate with a bearer token.

```bash
# API key scopes
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# Login user scopes (requires a bearer token)
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### Assign Scopes

You can assign scopes through any of the following methods:

- **Dashboard**: When creating or editing a key under **System** -> **API Key**, select the scopes to grant.
- **REST API**: Include `"scopes": ["monitoring", "cluster_operations"]` in the request body when creating or updating a key.
- **Bootstrap file**: Provide a comma-separated scope list as the fourth segment of each line, for example, `my-app:my-secret:administrator:monitoring,cluster_operations`.

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
