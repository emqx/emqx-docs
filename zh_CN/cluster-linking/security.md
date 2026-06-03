# 集群连接安全加固

集群连接（Cluster Linking）的底层使用标准 MQTT：每个集群以一个或多个 MQTT 客户端的身份连接到对端集群，这些连接不仅承载转发的用户消息，还承载控制面流量（路由同步和响应通道）。由于这些连接跨越了集群之间的网络边界，必须像对待其他公共 MQTT 监听器一样，对它们启用同等严格的认证与授权。

下面的加固措施建议在每个生产部署中都应用。每个集群都必须在接收对端连入的监听器上完成这些配置——即由**被连接**的那一方负责强制执行这些检查。

## 1. 规划 ClientID 和用户名

每个集群连接的 MQTT 连接都使用基于该连接 `clientid` 前缀生成的 ClientID——EMQX 会在前缀后追加 `:msg:<节点>` 等后缀作为最终的 ClientID。请按以下要求选取前缀：

- 对源集群唯一（例如，`cluster.name` 为 `A` 的集群使用 `clink-A-`）；
- 以分隔符（如 `-`）结尾，使得带锚点的正则 `^clink-A-` 只匹配来自集群 `A` 的连接，而不会误匹配类似 `clink-AB-...` 的对端集群；
- 不能与普通业务客户端使用的前缀冲突。

如果使用基于用户名的认证，请为集群连接分配专用用户名（例如 `clink-user:A`），不要复用给普通 MQTT 客户端。这些 ClientID 和用户名是认证与授权层进行识别的关键标识。

## 2. 启用认证

必须在接收集群连接的监听器上启用认证。如果不启用，任何能够访问该监听器的一方都可以冒充对端集群，向 `$LINK/` 控制命名空间下注入消息，从而干扰或窃听集群间流量。

完整的认证机制列表与配置方法，请从[认证](../access-control/authn/authn.md)总览开始。集群连接场景下常用以下两种方式：

- **TLS 双向认证（mTLS）**——安全性最强。对端集群提供由您控制的 CA 签发的客户端证书，监听器以 `verify = verify_peer` 与 `fail_if_no_peer_cert = true` 验证证书。详见 [X.509 证书认证](../access-control/authn/x509.md)。
- **用户名和密码**——在连接配置中设置 `username` / `password`，并在对端监听器上配置匹配的认证器。请妥善保存凭据并定期轮换。

也可以两者结合：传输层使用 mTLS，再叠加密码认证。

## 3. 启用授权

完成认证后，必须确保集群连接客户端只能使用 `$LINK/` 命名空间，并且**只有**集群连接客户端能够使用该命名空间。否则，已通过认证的其它客户端可能会向连接中注入伪造的路由更新或转发消息。

可用的授权数据源及其启用方式，请从[授权](../access-control/authz/authz.md)总览开始。下文示例使用 [ACL 文件](../access-control/authz/file.md)源，相同规则也可以通过任何其它授权器表达。

对端集群通过以下控制主题与本 Broker 通信。其中 `<Cluster>` 是对端集群自身的 `cluster.name`（即发起连接一侧所配置的 `cluster.name` 值），它在主题中**原样**出现，**不是**通配符或运行时替换。`<Actor>` 是按复制 actor 分配的内部子标识，请将其视为不透明值，并在 ACL 规则中使用 `+` 匹配。

| 操作 | 主题 | 用途 |
| --- | --- | --- |
| 发布 | `$LINK/cluster/msg/<Cluster>` | 转发的用户消息 |
| 发布 | `$LINK/cluster/route/<Cluster>` | 路由（订阅）同步 |
| 订阅 | `$LINK/cluster/resp/<Cluster>/<Actor>` | 本地 Broker 返回的响应 |

通配符 `$LINK/#` 上同时授予发布与订阅权限是推荐的起点——它覆盖了所有当前及未来的控制主题，无需在 EMQX 版本升级时跟踪主题结构变化。

假设本 Broker 接受两个 `cluster.name` 分别为 `A` 和 `C` 的对端集群连入，并且对端集群的连接配置中 `clientid` 分别为 `clink-A-` 和 `clink-C-`。下面的规则允许各对端集群使用 `$LINK/` 命名空间，禁止其它任何客户端访问该命名空间，并以默认拒绝规则收尾，使未被显式允许的客户端无法发布或订阅任何主题：

```erlang
%% 允许各对端集群使用 $LINK 控制命名空间
{allow, {clientid, {re, "^clink-A-"}}, all, ["$LINK/#"]}.
{allow, {clientid, {re, "^clink-C-"}}, all, ["$LINK/#"]}.

%% 禁止其它任何客户端访问 $LINK 命名空间
{deny, all, all, ["$LINK/#"]}.

%% ... 此处添加业务的 allow 规则 ...

%% 兜底规则：未被前面任何 allow 命中的请求一律拒绝
{deny, all}.
```

同时将兜底的 `{deny, all}` 与默认拒绝的授权器配置组合，使未匹配的授权检查 fail-closed：

```bash
authorization {
  no_match = deny
}
```

如果您倾向于使用枚举式的允许列表（更严格，但更脆弱——EMQX 未来引入的新控制主题需要手工补充），针对上面同样两个对端集群 `A` 和 `C`，等价规则如下：

```erlang
{allow, {clientid, {re, "^clink-A-"}}, publish,   ["$LINK/cluster/msg/A", "$LINK/cluster/route/A"]}.
{allow, {clientid, {re, "^clink-A-"}}, subscribe, ["$LINK/cluster/resp/A/+"]}.
{allow, {clientid, {re, "^clink-C-"}}, publish,   ["$LINK/cluster/msg/C", "$LINK/cluster/route/C"]}.
{allow, {clientid, {re, "^clink-C-"}}, subscribe, ["$LINK/cluster/resp/C/+"]}.
{deny, all}.
```

注意，主题中的 `<Cluster>` 占位符被替换为对端集群的实际 `cluster.name`（这里是 `A` 与 `C`），而 ClientID 正则匹配的则是对端连接配置中 `clientid` 字段的前缀——两者相互独立，新增对端集群时需要您手工保持一致。

## 4. 使用 TLS，优先 mTLS

对于跨越不受信任网络（公共互联网、跨云互联、合作伙伴网络等）的连接，TLS 是必须的。mTLS 在传输层进一步固定对端集群的身份，与上面的凭据校验形成互补。连接侧的 TLS 设置详见[配置 MQTT 连接](./configuration.md#配置-mqtt-连接)。

## 参见

- [认证](../access-control/authn/authn.md)
- [授权](../access-control/authz/authz.md)
- [使用 ACL 文件](../access-control/authz/file.md)
- [安全检查清单](../access-control/security-checklist.md)
