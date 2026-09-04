# 从文件加载 Secret

EMQX 的许多配置项保存敏感信息：SSL 监听器密钥口令、桥接/连接器的密码、OIDC 客户端密钥、S3 访问密钥、API Key 等。为了避免将这些值直接写入 `emqx.conf` 或 API 请求中，EMQX 在所有 secret 类型字段上支持 `file://` URL 前缀。EMQX 会在启动以及每次配置重载时从指定文件读取实际值。

## 语法

任何文档中标注为 secret（或 Dashboard 提示提到 `file://` 选项）的字段都支持以下形式：

```text
file://<文件路径>
```

路径可以是绝对路径，也可以是相对于 EMQX 工作目录的相对路径。文件内容将被作为 secret 值整体使用，但会做一项转换：

- 末尾空白字符被去除。末尾的换行、回车、空格、制表符会被去掉。开头及中间内容会原样保留。

示例：

```hocon
# 从文件加载 SSL 监听器密钥口令
listeners.ssl.default.ssl_options.password = "file://etc/certs/key-passphrase"

# 从文件加载 MQTT 桥接器密码
bridges.mqtt.upstream.password = "file:///run/secrets/upstream-mqtt-password"
```

## 集群部署注意事项

在 EMQX 集群中，每个节点都需要能够解析文件路径：

- 该文件必须在每个 EMQX 节点上都存在。同一路径对应每个节点的本地文件；EMQX 不会自动在节点之间复制文件。
- 各节点上的文件内容应保持一致；否则同一配置项在不同节点上将得到不同的 secret 值。
- 通过 Dashboard 或 REST API 修改配置时，`file://...` 字符串会被分发到所有节点，每个节点再各自打开本地副本。

常见做法是借助部署工具（Kubernetes Secrets、Ansible、配置管理工具等）在 EMQX 启动前以相同路径将 secret 文件分发到每个节点。

## 适用范围

只要配置 schema 中字段类型为 secret，`file://` 写法即生效。典型示例：

- **SSL/TLS 监听器**：`listeners.<type>.<name>.ssl_options.password`（密钥口令）。详见 [启用 SSL/TLS](../network/emqx-mqtt-tls.md)。
- **桥接和连接器**：密码、API Key、Secret Access Key、JWT Token，以及 `service_account_json` 等服务账户 JSON 凭证。
- **集群连接（Cluster Linking）**：`cluster.links[].password`。
- **Dashboard SSO（OIDC）**：`dashboard.sso.oidc.secret`。
- **许可证**：`license.key`（许可证字符串本身）。详见 [License 配置](../configuration/license.md)。
- **AI 补全**：`ai.completion_profile.api_key`。

这些字段的 Dashboard 提示会注明支持 `file://` 格式。

## 从文件加载节点 Cookie

从 EMQX 6.3.0 开始，`node.cookie` 及其环境变量覆盖项 `EMQX_NODE__COOKIE` 支持 `file://`。这是对普通 `string` 字段默认行为的明确例外。

为避免在 `emqx.conf` 中以明文保存节点 Cookie，可将 `node.cookie` 设置为文件 URL：

```hocon
node.cookie = "file:///run/secrets/emqx-cookie"
```

也可以设置 `EMQX_NODE__COOKIE`：

```bash
export EMQX_NODE__COOKIE='file:///run/secrets/emqx-cookie'
```

该路径可以指向普通文件或 FIFO（命名管道）。EMQX 仅在节点启动时解析一次 Cookie。配置重载不会再次读取文件或 FIFO。

使用 FIFO 时，编排系统必须在每次启动时将 Cookie 写入 FIFO，并且写入操作应先于 `emqx ctl` 等其他 `emqx` 命令。节点启动后的命令会从运行中的节点获取 Cookie，而不会再次读取 FIFO。

启动脚本会删除文件内容末尾的换行符。如果路径不存在、文件为空，或者解析出的 Cookie 包含反斜杠、单引号、双引号或空格，节点将无法启动。

EMQX 将解析出的 Cookie 直接传递给 Erlang VM，不会写入生成的 `data/configs/vm.*.args` 文件。在集群部署中，需要在每个节点上分发该文件或 FIFO，并确保所有节点读取到相同的 Cookie。更多信息，参见[设置节点 Cookie](../deploy/cluster/security.md#设置节点-cookie)。

## 日志与脱敏

EMQX 会对日志和 HTTP API 响应中的 secret 类型字段值进行脱敏。对于 `file://` 值，EMQX 仅记录文件路径，不记录文件内容。解析出的 secret 值不会写入日志。

## 不适用情况

除 `node.cookie` 等文档明确说明支持的字段外，普通 `string` 字段会将 `file://` 值作为字面字符串处理，而不是文件引用。使用前请根据字段的 schema 类型和文档确认是否支持 `file://`。
