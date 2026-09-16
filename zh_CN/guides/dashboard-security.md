# Dashboard 安全

本页面面向负责配置和保护 EMQX Dashboard 访问安全的管理员和运维人员，介绍首次登录、本地用户认证方式、Token 登录、密码管理、账户锁定、HTTPS 访问和基于角色的访问控制。

## 首次登录

全新安装 EMQX 后，通过浏览器打开 <http://localhost:18083/>，使用默认用户名 `admin` 和默认密码 `public` 登录。

首次登录后，系统会检测到您正在使用默认凭据，并强制要求在继续之前修改密码。新密码不能与原密码相同，且不建议再次使用 `public` 作为登录密码。

## 配置本地 Dashboard 用户的认证方式

从 EMQX 6.3.1 开始，EMQX 为本地 Dashboard 用户提供 SCRAM-SHA-256 挑战-响应端点。SCRAM 和密码登录使用相同的本地用户凭据进行认证，并签发 Dashboard Bearer Token。使用 SCRAM 时，客户端可以证明其持有密码，而无需在 HTTP 请求体中发送密码。

### 选择认证模式

通过 `dashboard.password_login` 选择允许使用的认证方式：

- `both`：同时接受 SCRAM-SHA-256 和基于密码的 `POST /api/v5/login` 请求。此项为默认值。
- `scram_only`：仅接受 SCRAM-SHA-256。基于密码的端点将返回 HTTP `403` 和错误码 `PASSWORD_LOGIN_DISABLED`。

### 为仅 SCRAM 模式做好准备

滚动升级期间请保留 `both`。只有在所有 EMQX 节点以及使用本地 Dashboard 用户凭据登录的客户端均支持 SCRAM 后，才能设置为 `scram_only`。使用本地 Dashboard 用户凭据获取 Bearer Token 的脚本和第三方客户端必须改用 `POST /api/v5/login/challenge` 和 `POST /api/v5/login/verify`。仅调用 EMQX 管理 REST API 的程序可以改用 API 密钥。

如果 EMQX 在服务端日志中提示某个本地用户需要迁移密码，请在启用 `scram_only` 前[重置用户密码](#重置密码)。

内置 API Spec Explorer 登录页面默认使用 SCRAM。安全浏览器上下文要求参见[浏览器访问](./api.md#浏览器访问)，完整的 SCRAM 流程参见[通过 SCRAM-SHA-256 获取 Bearer Token](./api.md#通过-scram-sha-256-获取-bearer-token)。

配置详情参见 [Dashboard 配置](./configuration/dashboard.md)。

## 通过 URL Token 登录

从 EMQX 5.6.0 开始，Dashboard 支持通过在 URL 中携带登录信息的方式进行免密登录。此功能适用于需要无缝跳转或集成的场景，可在无需用户手动输入凭据的情况下自动登录 Dashboard。

通过 URL Token 登录使用已有的 Dashboard Bearer Token，并不是一种独立的凭据类型。

EMQX Dashboard 通过管理 REST API 查询数据并执行管理操作。通过 URL Token 登录后，Dashboard 使用该 Bearer Token 对这些 API 请求进行认证。

### 获取 Dashboard Token

当 `dashboard.password_login` 设置为 `both` 时，可以通过基于密码的 `/login` 端点获取 Token。由于响应中不包含用户名，需要在对完整 JSON 载荷进行编码前手动添加用户名。以下命令会请求 Token、添加用户名，并对结果进行 Base64 编码：

```bash
curl -s -X POST "http://127.0.0.1:18083/api/v5/login" \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq '.username = "admin"' | base64
```

如果 `dashboard.password_login` 设置为 `scram_only`，请[通过 SCRAM-SHA-256 获取 Token](./api.md#通过-scram-sha-256-获取-bearer-token)。将用户名添加到 SCRAM 响应中，并对生成的 JSON 对象进行 Base64 编码，然后再构造登录 URL。

### 构造登录 URL

将经过 Base64 编码的登录信息嵌入到 `login_meta` 查询参数中。

对于 **EMQX 5.6.0 之前的版本**：

```bash
http://localhost:18083?login_meta=BASE64_ENCODED_STRING
```

该方式会跳转至默认的集群概览页面。

对于 **EMQX 5.6.0 及以上版本**：

```bash
http://localhost:18083/#/dashboard/overview?login_meta=BASE64_ENCODED_STRING
```

该方式支持在登录后跳转到指定页面。

请妥善保管 token，并设置合理的过期时间和访问权限范围。

## 管理密码

### 重置密码

可以通过 CLI 的 `admins` 命令重置 Dashboard 用户密码，详情参考[命令行 - admins](./cli.md#admins)：

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

### 密码过期

当 Dashboard 登录密码的使用时长超过配置的 `password_expired_time` 时，用户在下次登录时会被提示修改密码。具有**管理员**角色的用户也可以通过 [REST API](../guides/api.md) 更新该配置。

**示例**——将密码过期时间设置为 1 天：

```bash
curl -X 'PUT' \
  'http://admin:ppp@localhost:18083/api/v5/configs/dashboard' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"password_expired_time": "1d"}'
```

关于 `password_expired_time` 的完整说明，参考 [Dashboard 配置](./configuration/dashboard.md)。

## 账户锁定与解锁

当用户在 5 分钟内连续输入错误密码 5 次时，账户将被锁定 10 分钟，10 分钟后自动解锁。

具有**管理员**角色的用户可以通过 CLI 重置密码来手动解锁账户：

```bash
./bin/emqx ctl admins passwd <Username> <NewPassword>
```

管理员也可以通过后台配置调整锁定持续时间和触发锁定所需的失败次数，详情参考 [Dashboard 配置](./configuration/dashboard.md) 中的 `unsuccessful_login_max_attempts`、`unsuccessful_login_lock_duration` 和 `unsuccessful_login_interval` 配置项。

## 启用 HTTPS

Dashboard 默认监听 HTTP 端口 `18083`。要通过 HTTPS 访问 Dashboard，需配置 HTTPS 监听器并指定 TLS 证书和密钥：

```hocon
dashboard {
  listeners {
    https {
      bind = "0.0.0.0:18084"
      ssl_options {
        certfile = "${EMQX_ETC_DIR}/certs/cert.pem"
        keyfile  = "${EMQX_ETC_DIR}/certs/key.pem"
      }
    }
  }
}
```

如需禁用 HTTP 监听器、强制仅通过 HTTPS 访问，可将 HTTP 监听器的端口设置为 `0`：

```hocon
dashboard {
  listeners {
    http {
      bind = 0
    }
  }
}
```

完整的监听器和 TLS 配置选项，参考 [Dashboard 配置](./configuration/dashboard.md)。

## 基于角色的访问控制

从 EMQX 5.3 开始，Dashboard 用户被分配两种预定义角色之一，用于控制其操作权限。创建用户时可在**系统 > 用户**页面的**角色**下拉菜单中选择角色。

| 角色 | 权限 |
|---|---|
| **管理员** | 拥有对 EMQX 所有功能和资源的完全管理访问权限，包括客户端管理、系统配置、API 密钥和用户管理。 |
| **查看者** | 对所有数据和配置的只读访问权限，对应 REST API 中的所有 `GET` 请求，无权进行创建、修改或删除操作。 |

::: tip
Dashboard 用户名和密码不能直接作为 REST API 请求的 Basic 认证凭据。通过程序访问时，可以使用 [API 密钥](./api-keys.md)，也可以通过 Dashboard 登录流程获取短期 Bearer Token。长期运行的服务和无人值守的自动化任务应使用 API 密钥。
:::

用户管理的详细操作，参考[系统 > 用户](./dashboard/system.md#用户)。
