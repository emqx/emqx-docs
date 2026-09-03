# REST API

EMQX 提供遵循 OpenAPI 3.0 规范的管理 REST API。

EMQX 提供了多种方式来浏览和使用 REST API。EMQX 服务启动后，以下 API 规范端点可用：

| 端点 | 格式 | 描述 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 逐层展开式 API 参考页面，适合人工阅读。 |
| `/api-spec.md` | Markdown | Markdown 格式的 API 参考，适合 AI 代理和自动化工具使用。 |
| `/api-spec.json` | JSON | OpenAPI 3.0 规范的 JSON 格式，适合脚本和程序化工具使用。 |
| `/api-spec/:tag[/:name]` | JSON | 针对特定 API 标签的 OpenAPI 3.0 规范，可选择通过匹配请求或响应 Schema 名称进一步筛选。 |
| `/api-docs/swagger.json` | JSON | 完整的 OpenAPI 3.0 规范，适用于外部 Swagger UI 部署及其他兼容工具。 |

以上所有端点均需要 Dashboard 配置中的 `swagger_support` 设置为 `true`（默认值）。将其设置为 `false` 可禁用所有 API 文档端点。更多信息请参阅 [Dashboard 配置](../configuration/dashboard.md)。

从 EMQX 6.3.0 开始，EMQX 不再内置 Swagger UI。为保持向后兼容，访问 `/api-docs` 或 `/api-docs/index.html` 时，EMQX 将返回 HTTP 308 并重定向到 `/api-spec.html`。除 `/api-docs/index.html` 和 `/api-docs/swagger.json` 外，此前用于提供 Swagger UI 资源的其他 `/api-docs/*` 子路径将返回 HTTP 404。

本节将指导您快速开始使用 EMQX REST API。

::: tip
从 EMQX 6.3.0 开始，[功能门控](../deploy/feature-gates.md)可以在启动阶段禁用可选功能。已禁用功能提供的 REST API 路径不会加载为可访问的 API 端点。启用 `dashboard` 功能后，可以调用 `GET /api/v5/features` 查看解析后的功能集。
:::

## 访问 API 规范端点

从 EMQX 6.3.0 开始，必须通过认证才能从上述端点获取 API 规范内容。

程序化请求可以使用 API Key 和 Secret Key 进行基本认证，也可以使用 Bearer Token 认证。操作说明参见[认证](#认证)。

访问 API 规范属于只读操作，不受 API 密钥的角色或权限范围限制。

如果请求 `/api-spec.md`、`/api-spec.json`、`/api-spec/:tag[/:name]` 或 `/api-docs/swagger.json` 时未提供有效凭据，EMQX 将返回 HTTP `401`。`WWW-Authenticate` 响应头会声明支持基本认证和 Bearer Token 认证。响应体采用请求的格式，并包含一个最小化的 API 规范。该规范说明支持的认证方式，并列出两个公开端点：用于获取 Bearer Token 的 `POST /api/v5/login`，以及用于检查 Broker 状态的 `GET /api/v5/status`。该最小化响应不包含所请求的 API 规范内容。

在浏览器中访问时，EMQX 接受有效的 `emqx_auth` 会话 Cookie。未认证访问 `/api-spec.html` 时，EMQX 返回 HTTP `401`，并显示登录页面，而不是完整的 API Spec Explorer。该响应仅声明支持 Bearer Token 认证，以避免浏览器打开原生的基本认证对话框。使用 Dashboard 用户名和密码登录后，EMQX 会创建 `emqx_auth` 会话 Cookie 并加载完整的 API Spec Explorer。退出登录会清除该会话 Cookie。

访问 `/api-docs` 和 `/api-docs/index.html` 无需认证，因为这两个端点只会重定向到 `/api-spec.html`。重定向后，必须通过认证才能访问完整的 API Spec Explorer。

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
| 401    | 未通过服务端认证。认证凭据缺失、无效或已过期。               |
| 403    | 无权操作，检查操作对象是否正在使用或有依赖约束               |
| 404    | 找不到请求路径或请求的对象不存在，可参照 Body 中的 `message` 字段判断具体原因 |
| 409    | 请求的资源已存在或数量超过限制                               |
| 500    | 服务端处理请求时发生内部错误，可通过 Body 返回内容与日志判断具体原因 |

## 认证

EMQX 的 REST API 支持两种主要的认证方法：使用 API 密钥的基本认证和 Bearer Token 认证。

### 使用 API 密钥的基本认证

在这种方法中，您通过使用 API 密钥和密钥作为用户名和密码来对 API 请求进行身份验证。EMQX 的 REST API 基于 HTTP 基本认证框架，要求提供这些凭据。使用 EMQX REST API 之前，您需要先创建一个 API 密钥，详见 [API 密钥管理](#api-密钥管理)。

::: tip 注意
出于安全考虑，从 EMQX 5.0.0 开始 Dashboard 用户凭据无法用于 REST API 认证。您需要创建并使用 API 密钥进行认证。
:::

#### 使用 API 密钥认证

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

## API 密钥管理

本节介绍如何创建和管理 API 密钥，以及如何配置其角色、命名空间和权限范围。

### 创建 API 密钥

#### Dashboard

您可以在 Dashboard **系统设置** -> **API 密钥**页面中手动创建 API 密钥：

1. 单击页面右上角的**创建**按钮，打开创建对话框。
2. 配置 API 密钥详细信息：
   - **密钥名称**（必填）：输入 API 密钥的名称。
   - **到期时间**：留空表示永不过期。
   - **是否启用**：默认为启用。
   - **角色**：选择角色（可选），参见[角色与权限](#角色与权限)。
   - **命名空间**：开关默认关闭。对于全局管理员，保持关闭会创建全局 API 密钥；打开开关并选择一个命名空间，可在该命名空间中创建密钥。命名空间管理员只能在自己的命名空间中创建密钥。
   - **权限模式**：管理员或查看者密钥可选择权限范围分配方式。发布者密钥不显示此字段，并使用角色默认的 `publish` 权限范围。有关权限范围的行为和限制，参见 [API 权限范围](#api-权限范围)。
     - **角色默认权限**：使用所选角色的默认权限。角色默认权限发生变化时，新权限会自动生效。
     - **系统级权限**：仅授予 `system` 权限范围。
     - **自定义受限权限**：选择一个或多个权限范围，以限制密钥可访问的 API 区域。如果将**权限范围**留空，密钥不能访问受权限范围保护的 API。
   - **权限范围**：选择**自定义受限权限**后显示。选择要授予的权限范围。
   - **备注**：可选，填写密钥的描述信息。
3. 单击**确认**，API 密钥和 Secret Key 将显示在**创建成功**对话框中。

   ::: warning 重要提示

   请立即保存 API Key 和 Secret Key。Secret Key 后续将不再显示。

   :::

4. 单击**关闭**按钮关闭对话框。

**权限模式**仅用于 Dashboard。通过 REST API 创建或更新 API 密钥时，请直接配置 `scopes` 字段。详细语义参见[权限范围的默认行为](#权限范围的默认行为)。

点击密钥名称可查看详情。通过**编辑**按钮可修改到期时间、状态、角色、权限模式、权限范围和备注；通过**删除**按钮可移除密钥。

#### REST API

通过 REST API 创建或更新 API 密钥时，使用 Dashboard 用户的 Bearer Token 进行身份认证。API 密钥管理端点不接受 API 密钥认证。

从 EMQX 6.0.4 开始，`POST /api/v5/api_key` 和 `PUT /api/v5/api_key/:name` 的请求体支持顶层 `namespace` 字段。例如，以下请求在 `team-a` 命名空间中创建管理员 API 密钥：

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

将 `scopes` 设置为 `"unset"` 会显式应用角色默认权限范围。创建请求省略 `scopes` 时，效果相同。

可以通过以下任一方式指定命名空间：

- 使用 `administrator` 等不含命名空间的角色，并同时提供 `namespace` 字段。
- 将命名空间编码到角色中，格式为 `ns:<namespace>::<role>`，例如 `ns:team-a::administrator`。

以上两种方式均受支持。如果请求同时使用两种方式，二者指定的命名空间必须一致。如果命名空间不一致或 `namespace` 为空，EMQX 返回 HTTP 400。API 密钥创建后不能更改所属命名空间。

从 EMQX 6.3.0 开始，以上两种方式均不能使用 `multi_tenancy.deny_namespaces` 列表中的命名空间名称。配置方法请参见[禁止使用的命名空间名称](../multi-tenancy/namespace-global-settings.md#禁止使用的命名空间名称)。

创建全局 API 密钥时，应省略 `namespace` 并使用不含命名空间前缀的角色。将 `namespace` 设置为字符串 `"global"` 并不表示选择全局范围。

#### Bootstrap 文件

您也可以通过 bootstrap 文件的方式创建 API 密钥。在 `base.hocon` 配置文件中添加以下配置，指定文件位置：

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

在指定的文件中通过多行分割的 `{API Key}:{Secret Key}:{?Role}:{?Scopes}` 的格式添加多个 API 密钥：

- **API Key**：任意字符串作为密钥标识。
- **Secret Key**：使用随机字符串作为密钥。
- **Role（可选）**：指定密钥的[角色](#角色与权限)。
- **权限范围（可选）**：指定密钥可访问的 [API 权限范围](#api-权限范围)，多个范围用英文逗号分隔。省略时，密钥使用所属角色的默认权限。登录专属权限范围（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）不适用于 API 密钥。如果 bootstrap 文件条目中包含这些权限范围，EMQX 在启动时会将其移除并记录警告日志。密钥仍会被创建，但不含这些权限范围。

例如：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

在可分配给 API 密钥的权限范围中，只有 `system` 会授予等同管理员的权限。从 EMQX 6.0.4 开始，如果 bootstrap 条目将等同管理员权限的范围与不授予等同管理员权限的范围组合，EMQX 会移除所有等同管理员权限的范围、保留其余范围、记录警告，并继续创建或更新密钥。相比之下，REST API 会拒绝此类混合权限范围列表并返回 HTTP 400，且不会应用任何权限范围变更。

通过此方式创建的 API 密钥有效期为永久有效。

每次 EMQX 启动时，会将文件中设置的数据添加到 API 密钥列表中，如果存在相同的 API Key，则将更新其 Secret Key、Role 与权限范围。

### 命名空间管理员管理 API 密钥

从 EMQX 6.0.4 开始，命名空间 Dashboard 管理员可以管理自己命名空间中的 API 密钥。管理员必须使用 Bearer Token 进行身份认证。

| 操作 | 命名空间管理员行为 |
| --- | --- |
| 创建 API 密钥 | 只能在管理员所属的命名空间中创建密钥。省略命名空间、指定全局范围或指定其他命名空间时，均返回 HTTP 403。 |
| 查询 API 密钥列表 | 只能看到管理员所属命名空间中的密钥。响应会过滤全局密钥和其他命名空间中的密钥。 |
| 查看、更新或删除 API 密钥 | 只能操作管理员所属命名空间中的密钥。操作其他命名空间中的密钥时返回 HTTP 404，避免泄露密钥是否存在。 |
| 更改 API 密钥的命名空间 | 不能将密钥移动到其他命名空间，更新请求返回 HTTP 400。 |

全局 Dashboard 管理员仍可跨命名空间管理 API 密钥。

### 角色与权限

在 EMQX 企业版中，REST API 实现了基于角色的访问控制，API 密钥创建时，可以分配以下 3 个预定义的角色：

- **管理员**：此角色可以访问所有资源，未指定角色时默认使用此值。对应的角色标识为 `administrator`。
- **查看者**：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。对应的角色标识为 `viewer`。
- **发布者**：专门为 MQTT 消息发布定制，此角色仅限于访问与消息发布相关的 API。对应的角色标识为 `publisher`。

::: tip 注意
`publisher` 密钥只接受 `publish` 权限范围。分配权限范围时，除 `publish` 以外的任何权限范围都会返回 HTTP 400。如果您将某个密钥的角色更改为 `publisher`，请在同一请求中包含 `"scopes": ["publish"]` 或空列表；否则，若该密钥已有的权限范围中包含 `publish` 以外的项，请求将被拒绝。
:::

### API 权限范围

**权限范围**是 API 密钥的权限控制维度，用来声明一个密钥可以访问哪些业务领域的 API。它与[角色与权限](#角色与权限)相互独立、共同生效，形成两层权限控制：

| 维度 | 作用 | 粒度 |
| ---- | ---- | ---- |
| **Role（角色）** | 限制 HTTP 方法（只读 vs 可写、只能发布等） | 请求动作 |
| **权限范围** | 限制可访问的 API 领域（客户端、规则、监控等） | 资源领域 |

一次请求会先后通过两个检查：Role 校验和权限范围校验。只有两个检查都通过，请求才会被接受。

在微服务与集成场景中，不同的外部系统通常只需要访问 EMQX 的一部分管理接口：监控平台只需要 `monitoring` 权限范围的接口，规则发布服务只需要 `data_integration` 权限范围的接口，集群运维工具只需要 `cluster_operations` 权限范围的接口。通过权限范围，您可以按最小权限原则分配密钥，降低单个密钥被泄露带来的影响面。

::: tip 提示
权限范围名称是稳定标识符，不会随 EMQX 版本升级而改名；即便某个 API 的 OpenAPI tag 发生变化，只要您使用的是同一个权限范围，密钥行为保持不变。
:::

#### 内置 API 密钥权限范围

EMQX 提供 10 个 API 密钥权限范围：

| 权限范围 | 涵盖的典型 API 领域 |
| --- | --- |
| `connections`（连接管理） | `/clients`、`/subscriptions`、`/topics`、`/banned`、`/retainer`、`/file_transfer`、`/mqtt/delayed`、`/mqtt/topic_rewrite` 等 |
| `publish`（消息发布） | `/publish`、`/publish/bulk` |
| `data_integration`（数据集成） | `/rules`、`/connectors`、`/actions`、`/schema_registry`、`/schema_validations`、`/message_transformations`、`/exhooks`、`/ai/*` |
| `access_control`（访问控制） | `/authentication`、`/authorization/*` |
| `gateways`（协议网关） | `/gateways`、`/coap/*`、`/lwm2m/*`、`/gcp_devices` 等 |
| `monitoring`（监控数据） | `/metrics`、`/stats`、`/monitor*`、`/alarms`、`/trace`、`/slow_subscriptions`、`/telemetry`、`/prometheus/{auth,stats,data_integration,...}` 等 |
| `cluster_operations`（集群运维） | `/cluster*`、`/nodes`、`/load_rebalance`、`/node_eviction`、`/mt/*` 等 |
| `system`（系统配置） | `/configs*`、`/listeners*`、`/plugins*`、`/ds/*`、`/data/*`、`/status`、`/relup`、`/opentelemetry*`、`/prometheus` 等 |
| `audit`（审计日志） | `/audit` |
| `license`（许可证） | `/license*` |

::: warning 不得混合等同管理员权限的范围与受限权限范围

EMQX 将 `system`、`user_management`、`api_key_management` 和 `sso_management` 归为等同管理员权限的范围，校验错误消息中称为 `privilege scopes`。此类范围会授予等同管理员的权限，与受限权限范围组合并不能缩小账号的实际权限。在这 4 个范围中，只有 `system` 可以分配给 API 密钥；其余 3 个是下文介绍的[登录专属权限范围](#登录专属权限范围)。

因此，从 EMQX 6.0.4 开始，创建或更新 API 密钥时，显式权限范围列表必须仅使用 `system`，或使用不包含 `system` 的范围。混合列表会返回 HTTP 400，且不会应用任何变更。

已有的混合权限范围列表可以继续工作，其中 `system` 仍然有效。下次显式更新权限范围时，必须改为仅使用 `system`，或使用不包含 `system` 的列表。在 Dashboard 中编辑此类密钥时，系统会提示用户选择一种权限模式后再保存。

:::

#### 登录专属权限范围

除上述 10 个 API 密钥权限范围外，Dashboard 登录用户还拥有 4 个仅适用于浏览器会话的登录专属权限范围，这些权限范围不能分配给 API 密钥。有关这些权限范围在登录用户中的分配和生效方式，请参见[登录用户权限范围](../dashboard/system.md#登录用户权限范围)。

| 权限范围 | 所需角色 | 用途 |
| --- | --- | --- |
| `user_management` | 管理员 | 管理 Dashboard 用户。 |
| `sso_management` | 管理员 | 管理 SSO 后端与 SSO 用户记录。 |
| `api_key_management` | 管理员 | 管理 API 密钥。 |
| `mfa_management` | 任意 | 管理自己账号的 MFA；管理员可管理其他用户的 MFA。 |

#### 命名空间调用方限制

命名空间调用方（角色被限定在特定命名空间的用户或 API 密钥）在权限范围检查之外还受到额外的端点级限制。授予权限范围不能绕过这些限制。

命名空间 API 密钥不能调用消息发布 API，包括 `POST /api/v5/publish`。即使密钥的权限范围列表包含 `publish`，此限制仍然生效；授予权限范围不能覆盖命名空间级限制。

即使命名空间调用方已获得 `connections` 或 `monitoring` 权限范围，仍无法访问读取或操作集群级原始 MQTT 消息内容的端点，包括保留消息和延迟消息存储。以下消息相关端点返回 `403 Forbidden`：

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

对于追踪操作，`GET /trace` 仅列出调用方命名空间内的追踪记录。追踪记录属于其他命名空间时，以下单条追踪操作返回 `404 Not Found`：

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

此行为可避免泄露其他命名空间中的追踪记录。批量删除操作（`DELETE /trace`）对命名空间调用方返回 `403 Forbidden`，仅全局管理员可清空所有追踪记录。

Dashboard 自身的登录、SSO 回调以及 API 密钥自身的管理接口（例如 `/api_key`）不接受 API 密钥认证，与密钥的 `scopes` 配置无关。这属于 Dashboard 的内置安全边界，与权限范围模型无关。

#### 权限范围的默认行为

从 EMQX 6.0.4 开始，API 密钥的 `scopes` 字段遵循以下规则：

| `scopes` 字段的值 | 语义 |
| --- | --- |
| 创建请求中**未设置** | 使用所选角色的默认权限。 |
| 更新请求中**未设置** | 保留密钥当前的权限范围设置。 |
| 角色默认标记 `"unset"` | 移除显式权限范围设置并使用所选角色的默认权限。角色默认权限发生变化时，新权限会自动生效。 |
| **空列表** `[]` | 拒绝所有业务端点。常用于临时禁用密钥而不删除它。 |
| 显式列出的范围（如 `["monitoring", "cluster_operations"]`） | 只允许请求这些范围下的端点。 |

如果显式列表与角色默认权限包含相同的权限范围，其效果等同于 `"unset"`。该密钥会继续跟随角色默认权限的变化。比较时不考虑列表顺序。

Bootstrap 文件条目省略权限范围时，EMQX 在处理该文件时应用指定角色的默认权限。

权限范围决定密钥可以访问的 API 领域，不能覆盖密钥的角色或命名空间限制。只有角色、权限范围和命名空间检查全部通过时，请求才会被接受。

#### 查询可用范围

EMQX 提供两个端点用于查询可用的权限范围列表：

- `GET /api/v5/api_key_scopes`：返回可分配给 API 密钥的权限范围（即上述 10 个业务领域权限范围）。使用 API 密钥认证。
- `GET /api/v5/user_scopes`：返回 Dashboard 登录用户可用的全部权限范围，包含 4 个登录专属权限范围。使用 Bearer Token 认证。

可用于前端渲染权限范围选择 UI 或运维脚本校验配置：

```bash
# API 密钥权限范围
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# 登录用户权限范围（需要 Bearer Token）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### 如何分配权限范围

权限范围可以在以下任一入口指定：

- **Dashboard**：在**系统设置** -> **API 密钥**创建或编辑密钥时，选择**权限模式**。仅在选择**自定义受限权限**时单独选择权限范围。
- **REST API**：在创建 / 更新 API 密钥时，请求体加入 `"scopes": ["monitoring", "cluster_operations"]`。
- **Bootstrap 文件**：在每一行的第四段以逗号分隔范围名，例如 `my-app:my-secret:administrator:monitoring,cluster_operations`。

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
