# Dashboard 安全

本页介绍 EMQX Dashboard 的安全相关功能，包括登录认证、密码管理、账户锁定、HTTPS 访问和基于角色的访问控制。

## 首次登录

全新安装 EMQX 后，通过浏览器打开 <http://localhost:18083/>，使用默认用户名 `admin` 和默认密码 `public` 登录。

首次登录后，系统会检测到您正在使用默认凭据，并强制要求在继续之前修改密码。新密码不能与原密码相同，且不建议再次使用 `public` 作为登录密码。

## 通过 URL Token 登录

从 EMQX 5.6.0 开始，Dashboard 支持通过在 URL 中携带登录信息的方式进行免密登录。此功能适用于需要无缝跳转或集成的场景，可在无需用户手动输入凭据的情况下自动登录 Dashboard。

### 使用方法

1. 使用 `/login` 接口获取身份验证 token。由于返回结果中不包含用户名，需要手动将用户名添加到 JSON 数据中再进行编码。以下命令可一步完成所有操作——请求 token、添加用户名并进行 Base64 编码：

   ```bash
   curl -s -X POST "http://127.0.0.1:18083/api/v5/login" \
     -H 'accept: application/json' \
     -H 'Content-Type: application/json' \
     -d '{"username": "admin","password": "public"}' | jq '.username = "admin"' | base64
   ```

2. 构造登录 URL，将编码后的字符串嵌入到 Dashboard URL 的 `login_meta` 查询参数中：

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

## 重置密码

可以通过 CLI 的 `admins` 命令重置 Dashboard 用户密码，详情参考[命令行 - admins](./cli.md#admins)：

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

## 密码过期

当 Dashboard 登录密码的使用时长超过配置的 `password_expired_time` 时，用户在下次登录时会被提示修改密码。具有**管理员**角色的用户也可以通过 [REST API](../develop/api.md) 更新该配置。

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
出于安全考虑，从 EMQX 5.0.0 开始，Dashboard 用户无法用于 REST API 认证。如需通过程序访问，请使用 [API 密钥](./api-keys.md)。
:::

用户管理的详细操作，参考[系统 > 用户](./dashboard/system.md#用户)。
