# REST API

EMQX 提供了管理监控 REST API，这些 API 遵循 OpenAPI (Swagger) 3.0 规范。

EMQX 服务启动后，您可以访问 [http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) 来查看 API 的文档。还可以直接在 Swagger UI 上尝试执行一些 API。默认情况下，Dashboard 配置下的 `swagger_support` 设置为 `true`，表示启用 Swagger UI 支持，即开启所有 swagger 相关的功能，例如生成交互式 API 文档。您也可以将它设置为 `false` 以禁用此功能。

本节将指导您快速开始使用 EMQX REST API。

## 基本路径

EMQX 在 REST API 上做了版本控制，EMQX 5.0.0 以后的所有 API 调用均以 `/api/v5` 开头。

## HTTP 请求头

除非有特殊说明，绝大多数 API 要求请求头中 `Accept` 值设置为 `application/json`，响应内容将以 JSON 格式返回。

## HTTP 响应状态码

EMQX 遵循 [HTTP 响应状态码](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)标准，可能的状态码如下：

| 状态码 | 描述                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | 请求成功，返回的 JSON 数据将提供更多信息                     |
| 201    | 创建成功，新建的对象将在 Body 中返回                         |
| 204    | 请求成功，常用于删除与更新操作，Body 不会返回内容            |
| 400    | 请求无效，例如请求体或参数错误                               |
| 401    | 未通过服务端认证，API 密钥过期或不存在时可能会发生           |
| 403    | 无权操作，检查操作对象是否正在使用或有依赖约束               |
| 404    | 找不到请求路径或请求的对象不存在，可参照 Body 中的 `message` 字段判断具体原因 |
| 409    | 请求的资源已存在或数量超过限制                               |
| 500    | 服务端处理请求时发生内部错误，可通过 Body 返回内容与日志判断具体原因 |

## 认证

EMQX 的 REST API 支持两种主要的认证方法：使用 API 密钥的基本认证和 Bearer Token 认证。

### 使用 API 密钥的基本认证

在这种方法中，您通过使用 API 密钥和密钥作为用户名和密码来对 API 请求进行身份验证。EMQX 的 REST API 基于 HTTP 基本认证框架，要求提供这些凭据。使用 EMQX REST API 之前，您需要先创建一个 API 密钥。

::: tip 注意
出于安全考虑，从 EMQX 5.0.0 开始 Dashboard 用户凭据无法用于 REST API 认证。您需要创建并使用 API 密钥进行认证。请注意，基于角色的 API 凭据仅适用于 EMQX 企业版。
:::

#### 创建 API 密钥

您可以在 Dashboard **系统设置** -> **API 密钥** 页面中手动创建用于认证的 API 密钥，详细操作请参考 [Dashboard - API 密钥](../dashboard/system.md#api-密钥)。

您也可以通过 bootstrap 文件的方式创建 API 密钥。在 `emqx.conf` 配置文件中添加以下配置，指定文件位置：

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

在指定的文件中通过多行分割的 `{API Key}:{Secret Key}:{?Role}` 的格式添加多个 API 密钥：

- **API Key**：任意字符串作为密钥标识。
- **Secret Key**：使用随机字符串作为密钥。
- **Role （可选）**：指定密钥的[角色](#角色与权限)，仅适用于企业版。

例如：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
```

通过此方式创建的 API 密钥有效期为永久有效。

每次 EMQX 启动时，会将文件中设置的数据将添加到 API 密钥列表中，如果存在相同的 API Key，则将更新其 Secret Key 与 Role。

#### 角色与权限

在 EMQX 企业版中，REST API 实现了基于角色的访问控制，API 密钥创建时，可以分配以下3个预定义的角色：

- **管理员**：此角色可以访问所有资源，未指定角色时默认使用此值。对应的角色标识为 `administrator`。
- **查看者**：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。对应的角色标识为 `viewer`。
- **发布者**：专门为 MQTT 消息发布定制，此角色仅限于访问与消息发布相关的 API。对应的角色标识为 `publisher`。

#### 认证方式

使用生成的 API Key 以及 Secret Key 分别作为 Basic 认证的用户名与密码，请求示例如下：

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

### 使用 Bearer Token 认证

除了基于 API 密钥的身份验证外，您还可以使用 Bearer Token 来实现对 EMQX REST API 的安全和程序化访问。要获取 Bearer Token，请按照以下说明向登录 API 端点发送请求。

#### 获取 Bearer Token

要请求 Bearer Token，请向以下登录 API 端点发送 HTTP `POST ` 请求：

```bash
POST http://your-emqx-address:8483/api/v5/login
```

**请求头:**

- `Content-Type: application/json`

**请求体:**

```json
{
  "username": "admin",
  "password": "yourpassword"
}
```

- 将 `your-emqx-address` 替换为您的 EMQX 节点的地址或 IP。
- 将 `"admin"` 和 `"yourpassword"` 替换为您的 EMQX Dashboard 凭证。

响应中将包含 Bearer Token，您可以使用该 Token 对 API 请求进行身份验证。

#### 使用 Bearer Token 进行身份认证

获取 Bearer Token 后，将其包含在您的 API 请求的 `Authorization` 标头中，如下所示：

```bash
--header "Authorization: Bearer <your-token>"
```

## 分页

在一些数据量较大的 API 中，提供了分页功能，根据数据特性，有2种分页方式。

### 页码分页

支持分页的绝大多数 API 中，您可以通过 `page`（页码） 和 `limit`（分页大小） 参数来控制分页，分页大小最大值为 `10000`，如果不指定 `limit` 参数，则默认为 `100`。

例如：

```bash
GET /clients?page=1&limit=100
```

响应结果中 `meta` 字段将包含分页信息，对于使用了搜索条件的请求，EMQX 无法预知有多少条数据，因此使用 `meta.hasnext` 字段则表示是否还有下一页数据：

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

### 游标分页

在少数数据变化较快、页码分页效率较低的 API 中，使用游标分页的方式。

您可以通过 `position` 或 `cursor`（起始位置）指定数据的开始位置， `limit`（分页大小）指定自开始位置之后加载的数据数量。分页大小最大值为 `10000`，如果不指定 `limit` 参数，则默认为 `100`。

例如：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

响应结果中的 `meta` 字段将包含分页信息，`meta.position` 或 `meta.cursor` 指示了下一页开始的位置：

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

通过这种分页方式，可以高效处理数据变化较快的场景，确保数据的连续性和获取效率。

## 错误码

HTTP 响应状态码能够直观的判断可能存在的问题，在此基础上 EMQX 定义了一系列的错误码来标识具体的错误原因。当发生错误时，错误码将通过 Body 以 JSON 格式返回，您可以根据错误码 `code` 了解错误分类，根据原因 `reason` 了解具体的错误信息：

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| 错误码                                         | 描述                                                                      |
| ---------------------------------------------- | ------------------------------------------------------------------------- |
| WRONG_USERNAME_OR_PWD                          | Wrong username or password <img width=200/>                               |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | Wrong username & password or key & secret                                 |
| BAD_REQUEST                                    | Request parameters not legal                                              |
| NOT_MATCH                                      | Conditions not matched                                                    |
| ALREADY_EXISTS                                 | Resources already exist                                                   |
| BAD_CONFIG_SCHEMA                              | Configuration data not legal                                              |
| BAD_LISTENER_ID                                | Bad listener ID                                                           |
| BAD_NODE_NAME                                  | Bad Node Name                                                             |
| BAD_RPC                                        | RPC Failed. Check the cluster status and the requested node status        |
| BAD_TOPIC                                      | Topic syntax error, topic needs to comply with the MQTT protocol standard |
| EXCEED_LIMIT                                   | Resources to be created exceed the maximum limit or minimum limit         |
| INVALID_PARAMETER                              | Request parameters not legal and exceed the boundary value                |
| CONFLICT                                       | Conflicting request resources                                             |
| NO_DEFAULT_VALUE                               | Request parameters do not use default values                              |
| DEPENDENCY_EXISTS                              | Resource depends on other resources                                       |
| MESSAGE_ID_SCHEMA_ERROR                        | Message ID parsing error                                                  |
| INVALID_ID                                     | Bad ID schema                                                             |
| MESSAGE_ID_NOT_FOUND                           | Message ID does not exist                                                 |
| NOT_FOUND                                      | Resource not found or does not exist                                      |
| CLIENTID_NOT_FOUND                             | Client ID not found or does not exist                                     |
| CLIENT_NOT_FOUND                               | Client not found or does not exist(usually not an MQTT client)            |
| RESOURCE_NOT_FOUND                             | Resource not found                                                        |
| TOPIC_NOT_FOUND                                | Topic not found                                                           |
| USER_NOT_FOUND                                 | User not found                                                            |
| INTERNAL_ERROR                                 | Server inter error                                                        |
| SERVICE_UNAVAILABLE                            | Service unavailable                                                       |
| SOURCE_ERROR                                   | Source error                                                              |
| UPDATE_FAILED                                  | Update fails                                                              |
| REST_FAILED                                    | Reset source or configuration fails                                       |
| CLIENT_NOT_RESPONSE                            | Client not responding                                                     |
