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

某个字段是否支持 `file://`，请参考字段的 Dashboard 提示。

## 日志与脱敏

EMQX 会在日志和 HTTP API 响应中对 secret 值进行脱敏。若 secret 通过 `file://...` URL 配置，EMQX 在日志中记录的是路径本身（不会记录文件内容），便于运维人员排查节点实际读取的是哪个文件。从文件中提取出的真正 secret 值不会出现在任何日志中。

## 不适用情况

如果配置字段类型是普通 `string`（而非 secret 类型），即便加上 `file://` 前缀，也会被作为字面字符串处理，而非文件引用。使用前请确认字段的 schema 类型是否支持 `file://`。
