# REST API

EMQX exposes an HTTP management API designed following OpenAPI (Swagger) 3.0 specification.

After EMQX is started, you can visit [http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html)
to view the API document, and also execute the management APIs from this UI.

The section will introduce how to work with EMQX REST API.

## Basic Path

EMQX has version control on the REST API, all API paths from EMQX 5.0.0 start with `/api/v5`.

## Authentication

EMQX's REST API uses [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework).

You can create an API key by clicking **System** -> **API Key** from the left navigation menu on the Dashboard. For instructions on creating the API key, see [System - API Keys](../dashboard/system.md#api-keys).

:::tip
For security reasons, starting with EMQX 5.0.0, Dashboard user cannot be used for REST API authentication.
:::

You can use the generated API Key and Secret Key as the username and password for Basic authentication:

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

## HTTP Headers

Unless otherwise specified, most API requests require the `Accept` header to be set to `application/json`, and then the response will be returned in JSON format.

## HTTP Response Status Code

EMQX follows the [HTTP Response Status Code](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) standard, and the possible status codes are as follows:

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

## Pagination

Some APIs support pagination. You can control the pagination by using the `page` (page number) and `limit` (page size) parameters. The maximum page size is `10000`. If the `limit` parameter is not specified, the default is `100`.

For example:

```bash
GET /clients?page=1&limit=100
```

In the response result, the `meta` field will contain pagination information. For requests that use search conditions, EMQX cannot predict how many data entries there are, so the `meta.hasnext` field indicates whether there is more data on the next page:

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

