# 配置集群连接

本页面提供通过配置文件和 EMQX Dashboard 配置集群连接功能的指南。

## 通过配置文件配置集群连接

可以通过在 EMQX 配置中扩充 `cluster.links` 列表来设置集群之间的一个或多个连接。每个连接必须有唯一的远程集群名称，并且可以单独启用或禁用。

保持每个连接中的集群名称一致对于确保正确功能至关重要。在下面的示例中，远程集群名称应在其对应的配置文件中为 `emqx-eu-west`。

```bash
cluster {
  name = "emqx-us-east"
  links = [
    {
      name = "emqx-eu-west"
      server = "emqx.us-east.myinfra.net"
      username = "clink-user:us-east"
      password = "clink-password-no-one-knows"
      clientid = "clink-us-east"
      topics = ["global/#", "fwd/#", "cluster/+/status", ...]
      ssl {
        enable = true
        verify = verify_peer
        certfile = "etc/certs/client/emqx-us-east.pem"
        ...
      }
    }
    ...
  ]
}
```

确保远程 `emqx-eu-west` 集群在其配置文件中也有类似的连接配置到 `emqx-us-east`，以确保连接正常工作。

### 启用和禁用连接

配置的连接默认是启用的。可以通过将 `enable` 参数设置为 `false` 来禁用它。

禁用连接将阻止 EMQX 与远程集群通信。然而，这一操作并不会自动阻止远程集群与此集群通信，这可能会导致远程集群出现警告和报警。为避免这些问题，始终确保两边的连接都被禁用。

### 配置主题

`topics` 参数是一个 MQTT 主题过滤器列表，指定本地集群感兴趣的主题。本地集群期望从远程集群接收发布到这些主题的消息。这个列表可以为空，这意味着如果没有指定主题，本地集群将不会从远程集群接收任何消息。

### 配置 MQTT 连接

集群连接使用标准的 MQTT 作为底层协议，需要指定远程集群的 MQTT 监听端点为 `server`。

根据集群的大小和配置，可能会与远程集群建立多个 MQTT 客户端连接，每个客户端必须有唯一的 ClientID。可以通过设置 `clientid` 参数来控制这些 ClientID 的分配，该参数作为这些连接的 *ClientID 前缀*。

其他 MQTT 协议方面的参数，如认证和授权参数（`username`，`password`），也是可配置的。远程集群必须能够[认证](../access-control/authn/authn.md)这些连接，并[授权](../access-control/authz/authz.md)它们向集群连接设置指定的 MQTT 主题发布消息。例如，根据上述配置，远程集群可以有如下的 [ACL规则](../access-control/authz/file.md)来正常运行：

```erlang
%% 允许集群连接MQTT客户端操作"$LINK/#"主题
{allow, {clientid, {re, "^clink-us-east"}}, all, ["$LINK/#"]}.
...
```

此规则允许 ClientID 匹配正则表达式模式 `^clink-us-east` 的 MQTT 客户端发布和订阅任何以 `$LINK/` 开头的主题。`$LINK/` 是用于集群连接相关消息的控制主题前缀。这确保了订阅实体接收所有 `$LINK/` 命名空间下的相关消息，这些消息对于维护和管理集群连接是必要的。<!-- 这里需要解释一下 $LINK/#，这个是不是就是“控制主题”？帮助review这段描述是否正确-->

集群连接支持 TLS 连接。如果计划让集群通过公共互联网或任何其他不受信任的网络进行通信，TLS 是必须的。EMQX 还支持双向 TLS 认证，确保通信安全、保密和可信。

## 通过 Dashboard 配置集群连接

<!-- to do -->
