# EMQX Dashboard

EMQX 提供了一个内置的管理控制台，即 EMQX Dashboard。方便用户通过 Web 页面就能轻松管理和监控 EMQX 集群，并配置和使用所需的各项功能。

新的 UI / UX 设计风格的 EMQX Dashboard，优化了关键数据和指标数据的显示方式与内容，在提升视觉体验的同时，也提供了更全面、强大、易用的内置功能，如对于连接、订阅和发布时的认证与权限管理，支持使用规则引擎进行数据集成转化等。使用浏览器来快速便捷访问的方式，为用户使用 EMQX 进行更多物联网业务开发提供了便利。

![image](./assets/dashboard-preview.png)

## 主要功能

本节介绍了您可以通过 Dashboard 配置和管理的 EMQX 各项功能。

::: tip
从 EMQX 6.3.0 开始，部署时可以使用[功能门控](../deploy/feature-gates.md)在启动阶段禁用可选功能。功能被禁用后，Dashboard 会隐藏对应的菜单和页面。
:::

### [监控](./monitoring.md)

支持查看运行中的 EMQX 集群的整体连接数、订阅主题数、消息收发数量和流入流出速率，还包括节点列表和节点信息和一些系统指标信息，同时也可以对一些客户端连接与订阅数据等进行查看与管理。

### 访问控制

支持通过可视化的方式来新增和配置管理 EMQX 中的认证与授权机制。

### 数据集成

使用基于 SQL 的强大规则引擎和数据集成、或 Flow 编辑器的可视化功能进行低代码数据处理和集成，以帮助实时提取、过滤、丰富、转换、存储以及验证 MQTT 数据。

### 管理

#### 在线集群配置热更新

支持在线修改和更新包括 MQTT、日志，监听器等配置项，更新成功后即刻生效。

#### MQTT 高级特性配置

管理和配置主题重写、自动订阅、延迟发布和文件传输功能。

#### 管理系统扩展能力

支持自定义插件集成，通过内置网关的管理和配置来拓展连接协议，或使用钩子拦截模块之间的消息传递和事件传递来修改和扩展系统功能。

### 问题分析和诊断

除通过在线的 MQTT over WebSocket 客户端连接、主题监控来调试外，还支持使用如慢订阅查询，在线日志追踪来诊断和发现问题。

### 系统设置

管理和配置用户帐户、审计日志、API 密钥、License 设置和单点登录等功能。

## 启动 Dashboard

EMQX Dashboard 是一个 Web 应用程序，默认监听 `18083` 端口。下载安装 EMQX 并成功启动之后，可以通过浏览器打开 <http://localhost:18083/>（如部署在非本机的，可将 localhost 替换为实际 IP 地址）来访问和使用 EMQX Dashboard。

::: tip
在不启用 Dashboard 的情况下仍然可以正常使用 EMQX，Dashboard 只是为用户提供了可视化使用的选择。
:::

### 首次登录

对于首次安装和部署好 EMQX 的用户来说，浏览器打开 Dashboard 后可以使用默认用户名 `admin` 和默认密码 `public` 来进行登录使用。

首次登录后，系统会自动检测到您正在使用默认用户名和密码登录，并会强制要求修改默认密码，这有利于访问 Dashboard 的安全性提升，注意修改的密码不能与原密码相同，且不建议再次使用 `public` 做为登录密码。

### 通过 URL Token 登录 Dashboard

从 EMQX 5.6.0 开始，Dashboard 支持通过在 URL 中携带登录信息的方式进行免登录访问。

此功能适用于需要无缝跳转或集成场景，可在无需用户手动输入凭据的情况下，自动登录 Dashboard。

#### 使用方法

使用此登录方式的步骤如下：

1. 使用 `/login` 接口获取身份验证 token。由于返回结果中不包含用户名，你需要手动将用户名添加到 JSON 数据中，再进行编码。

   你可以通过以下命令一步完成所有操作，包括请求 token、添加用户名，以及将结果进行 Base64 编码：

   ```
   curl -s -X POST "http://127.0.0.1:18083/api/v5/login" \
     -H 'accept: application/json' \
     -H 'Content-Type: application/json' \
     -d '{"username": "admin","password": "public"}' | jq '.username = "admin"' | base64
   ```

2. 构造登录 URL。将编码后的字符串嵌入到 Dashboard URL 的 `login_meta` 查询参数中。例如：

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

通过 URL 携带 token 登录的方式，可以为用户提供无需手动登录的便捷访问体验。请确保妥善管理 token 的安全性，建议设置合理的过期时间和访问权限范围。

### 忘记密码

如果您忘记了 Dashboard 登录密码，可以通过 CLI 的 `admins` 命令进行重置，详情请参考 [命令行 - admins](../admin/cli.md#admins)：

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

### 密码过期

如果当前 Dashboard 登录密码的使用时长超过了配置的密码过期时间 (`password_expired_time`)，系统将在您登录时提示您修改密码。关于 `password_expired_time` 设置的详细信息，参考 [Dashboard 配置](../configuration/dashboard.md)。

“管理员”角色的用户也可以通过 [REST API](../admin/api.md) 配置密码过期时间。

**示例**：

```bash
curl -X 'PUT' \
  'http://admin:ppp@localhost:18083/api/v5/configs/dashboard' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"password_expired_time": "1d"}'
```

上述示例中，密码过期时间被设置为 1 天。

### 账户锁定与解锁

为了增强安全性，EMQX Dashboard 实现了“账户锁定与解锁”机制。当用户在 5 分钟内连续输入错误密码 5 次时，账户将被锁定 10 分钟。

具有“管理员”角色的用户可以通过 CLI 重置密码手动解锁账户。10 分钟后，账户将自动解锁，用户可以正常登录。

管理员还可以通过后台设置配置锁定持续时间和触发锁定所需的失败尝试次数。有关设置的详细信息，请参阅 [Dashboard 配置](../configuration/dashboard.md)。

## 配置 Dashboard

Dashboard 默认监听 HTTP 端口，端口号默认为 18083，用户可以启用 HTTPS 或更改监听器端口。更多关于 Dashboard 如何配置和修改的使用方法请参考 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。
