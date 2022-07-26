# HTTP API

EMQX 提供了管理监控 REST API，这些 API 遵循 OpenAPI (Swagger) 3.0 规范。


EMQX 服务启动后，您可以访问 http://localhost:18083/api-docs/index.html 来
查看 API 的文档。还可以直接在 Swagger UI 上尝试执行一些 API。

`/api-docs` 端点不需要登录，如果需要进一步操作，您需要完成一些基本的配置工作。

本章节指导您完成 API 调用前的准备操作。

## 修改默认密码

EMQX 的 HTTP API 默认的访问用户名是 `admin` 密码是 `public`。
默认密码建议您立即修改。

修改默认密码，可以通过 emqx.conf 配置文件中加入，或修改 `dashboard.default_password`。
这个配置项会被用于初始化默认用户的配置。
您也可以通过配置 `EMQX_DASHBOARD__DEFAULT_PASSWORD` 来对这个参数初始化。

::: warning
EMQX 的管理员用户名和密码是保存在内置数据库中的。
一旦这个数据库初始化完成之后，再去修改配置文件或环境变量中的默认密码将不会生效。

如果需要在初始化之后对默认用户的密码进行修改，可以使用如下命令行。
`emqx ctl admins passwd admin new-password`
:::

## 创建管理员账户

### 用户

初始化了默认用户 (默认是 `admin`) 的登录密码之后，就可以创建其他的用户了。
创建的渠道有三个，从仪表盘的用户管理界面，REST API，和命令行工具 `emqx_ctl`。

::: warning
所有的用户都有管理员权限。
也就是说，EMQX现阶段不提供基于角色的权限管理能力。

如果需要创建一些脚本调用等用户进行 HTTP API 的访问，建议创建 API Key 。
:::

### API 密钥

您可以在 Dashboard “系统设置” -> “API 密钥” 界面点击创建，
也可以通过如下的 API 调用来创建一个新的 API 密钥。

```bash
curl -u 'admin:public' \
     -X 'POST' \ 'http://localhost:18083/api/v5/api_key' \
     -H 'accept: application/json' \
     -H 'Content-Type: application/json' \
     -d '{
            "name": "EMQX-API-KEY-3",
            "expired_at": "2022-12-05T02:01:34.186Z",
            "desc": "for testing",
            "enable": true,
            "expired": true
        }'
```

返回结果中包含认证密钥信息：

```bash
{
  "api_key": "a87465f14ca0d420",
  "api_secret": "LECuyY4VAnndsYRkjtWO2vFTi80FvohmhVgOeNeorMN",
  "created_at": "2022-06-21T22:28:23+02:00",
  "desc": "for testing",
  "enable": true,
  "expired": false,
  "expired_at": "2022-12-05T03:01:34+01:00",
  "name": "EMQX-API-KEY-3"
}
```

结果中的 `api_key` 和 `api_secret` 可以用于访问 REST API 时的 Basic 认证，例如：

```bash
curl -u a87465f14ca0d420:LECuyY4VAnndsYRkjtWO2vFTi80FvohmhVgOeNeorMN \
     -X 'GET' 'http://localhost:18083/api/v5/nodes' -H 'accept: text/plain'
```

请注意 `api_secret` 仅在创建时返回一次，请及时保存。
