# 集群连接安全加固

集群连接的底层使用标准 MQTT：每个集群以一个或多个 MQTT 客户端的身份连接到对端集群，这些连接承载转发的用户消息和控制面流量（路由同步与响应通道）。由于连接跨越集群间的网络边界，接收这些连接的监听器需要与其他公开 MQTT 监听器同等级别的安全加固。

以下加固措施建议在每个生产部署中应用。每个集群都必须在接收对端连入的监听器上完成这些配置，即由被连接的一方负责强制执行这些检查。

## 规划 ClientID 和用户名

集群连接的每条 MQTT 连接使用基于连接配置中 `clientid` 前缀生成的 ClientID，EMQX 会在前缀后追加 `:msg:<节点>` 等后缀形成最终的 ClientID。请按以下要求选取前缀：

- 对源集群唯一（例如，`cluster.name` 为 `A` 的集群使用 `clink-A-`）；
- 以分隔符（如 `-`）结尾，使得锚定正则 `^clink-A-` 只匹配来自集群 `A` 的连接，而不会误匹配类似 `clink-AB-...` 的对端集群；
- 不能与业务客户端使用的前缀冲突。

如果使用密码认证，请为每个对端集群分配专用用户名（例如 `clink-user:A`），不要与普通 MQTT 客户端共用。

这些 ClientID 和用户名是认证与授权规则的匹配依据，请在配置这两个层之前先确定好。

## 启用认证

必须在接收集群连接的监听器上启用认证。否则，任何能访问该监听器的一方都可以冒充对端集群，向 `$LINK/` 控制命名空间注入流量，干扰或窃听集群间通信。

集群连接场景下常用以下两种认证方式：

- **TLS 双向认证（mTLS）：** 对端集群提供由您控制的 CA 签发的客户端证书，监听器以 `verify = verify_peer` 与 `fail_if_no_peer_cert = true` 验证证书。这是安全性最强的选项，因为它在传输层就固定了对端身份，同时兼作认证检查，无需额外配置密码认证器。详见 [X.509 证书认证](../../guides/access-control/authn/x509.md)。
- **用户名和密码：** 在连接配置中设置 `username` 和 `password`，并在对端监听器上配置匹配的认证器。请妥善保管凭据并定期轮换。

也可以两者结合：传输层使用 mTLS，再叠加密码认证。

对于跨越不受信任网络（公共互联网、跨云互联、合作伙伴网络等）的连接，TLS 是必须的。mTLS 在传输层进一步固定对端集群的身份，与上面的凭据校验形成互补。完整的认证机制列表请参阅[认证](../../guides/access-control/authn/authn.md)总览。连接侧的 TLS 配置详见[配置 MQTT 连接](./configuration.md#配置-mqtt-连接)。

## 启用授权

完成认证后，必须将集群连接客户端限定在 `$LINK/` 命名空间内，同时禁止其他任何客户端访问该命名空间。否则，已通过认证的无关客户端可能向连接中注入伪造的路由更新或转发消息。

对端集群通过以下控制主题与本 Broker 通信。其中 `<Cluster>` 是对端集群自身的 `cluster.name`（即发起连接一侧所配置的 `cluster.name` 值），它在主题中原样出现，不是通配符或运行时替换值。`<Actor>` 是按复制 actor 分配的内部子标识，在规则中使用 `+` 匹配即可。

| 操作 | 主题 | 用途 |
| --- | --- | --- |
| 发布 | `$LINK/cluster/msg/<Cluster>` | 转发的用户消息 |
| 发布 | `$LINK/cluster/route/<Cluster>` | 路由（订阅）同步 |
| 订阅 | `$LINK/cluster/resp/<Cluster>/<Actor>` | 本地 Broker 返回的响应 |

### ACL 配置

下文示例使用 [ACL 文件](../../guides/access-control/authz/file.md)源，相同规则同样适用于其他授权器。

为每个对端集群授予 `$LINK/#` 的发布与订阅权限。通配符覆盖所有当前及未来的控制主题，升级 EMQX 版本时无需修改规则。禁止其他所有客户端访问该命名空间，并以兜底拒绝规则收尾：

```erlang
%% 允许各对端集群使用 $LINK 控制命名空间
{allow, {clientid, {re, "^clink-A-"}}, all, ["$LINK/#"]}.
{allow, {clientid, {re, "^clink-C-"}}, all, ["$LINK/#"]}.

%% 禁止其他任何客户端访问 $LINK 命名空间
{deny, all, all, ["$LINK/#"]}.

%% ... 此处添加业务的 allow 规则 ...

%% 兜底规则：未被前面任何 allow 命中的请求一律拒绝
{deny, all}.
```

同时配置默认拒绝的授权器设置，使未命中规则的请求默认被拒绝：

```hocon
authorization {
  no_match = deny
}
```

如果您倾向于使用枚举式的允许列表（更严格，但更脆弱，EMQX 未来引入的新控制主题需要手工补充），针对上面同样两个对端集群 `A` 和 `C`，等价规则如下：

```erlang
{allow, {clientid, {re, "^clink-A-"}}, publish,   ["$LINK/cluster/msg/A", "$LINK/cluster/route/A"]}.
{allow, {clientid, {re, "^clink-A-"}}, subscribe, ["$LINK/cluster/resp/A/+"]}.
{allow, {clientid, {re, "^clink-C-"}}, publish,   ["$LINK/cluster/msg/C", "$LINK/cluster/route/C"]}.
{allow, {clientid, {re, "^clink-C-"}}, subscribe, ["$LINK/cluster/resp/C/+"]}.
{deny, all}.
```

注意，主题中的 `<Cluster>` 占位符被替换为对端集群的实际 `cluster.name`（这里是 `A` 与 `C`），而 ClientID 正则匹配的则是对端连接配置中 `clientid` 字段的前缀，两者相互独立，新增对端集群时需要您手工保持一致。

可用的授权数据源与配置选项详见[授权](../../guides/access-control/authz/authz.md)总览。

## 参见

- [认证](../../guides/access-control/authn/authn.md)
- [授权](../../guides/access-control/authz/authz.md)
- [使用 ACL 文件](../../guides/access-control/authz/file.md)
- [安全检查清单](../../guides/access-control/security-checklist.md)
