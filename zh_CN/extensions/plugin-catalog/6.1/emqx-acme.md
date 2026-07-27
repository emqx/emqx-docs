# EMQX ACME 插件

用于自动签发和续期 TLS 证书的 ACME 客户端插件。

该插件与兼容 ACME 的证书颁发机构（例如 Let's Encrypt）集成，为 EMQX 的 SSL 监听器自动签发和续期 TLS 证书。证书存储在 EMQX 托管的证书包中。


> ⚠️ **注意 —— 在 EMQX 重新部署时请持久化保留 `<data_dir>/certs2/`。**
>
> 插件需要在重启后保留的所有内容都位于
> `<data_dir>/certs2/global/<cert_bundle_name>/` 之下：
>
> - `chain.pem` + `key.pem` —— 已签发的*证书包*。一旦丢失，
>   插件会在下次启动时从头重新签发，从而占用 Let's Encrypt
>   针对每个域名的"每周 5 张重复证书"速率限制额度。
> - `acc-key.pem` —— ACME *账户密钥*，即 Let's Encrypt 为你
>   注册的身份标识。一旦丢失，每次重新部署都会开启一个全新的
>   账户，耗尽"每个 IP 每 3 小时 10 次 newAccount 调用"的额度，
>   并使你失去*吊销*先前账户证书的能力。
>
> 在 docker 上 `<data_dir>` 为 `/opt/emqx/data`，在 DEB/RPM 上为
> `/var/lib/emqx`。在 Docker 中，请将整个 `data/`（或至少
> `data/certs2/`）绑定挂载到宿主机卷；在 Kubernetes 中，请使用 PVC
> 作为其后端存储。插件会在首次签发时于证书包内生成账户密钥，并通过
> `emqx_managed_certs` 自动将其复制到集群的每个节点，因此全新的集群
> 无需手动分发密钥即可启动。

## 快速开始

面向可公开解析的域名的最简单节点配置：

1. 在 EMQX 仪表盘（*管理 → 插件*）中**安装并启用**该插件。
2. **配置**插件（只需关注下面四个字段；其余都可保持默认值）：
   - `domains = "mqtt.example.com"` —— 逗号分隔的列表（每个域名
     都必须能公开解析到本节点）。
   - `contact = "mailto:admin@example.com"` —— 逗号分隔的联系地址
     列表，用于接收 CA 的续期/吊销通知。
   - `challenge_port = 5080`（或任意 EMQX 能够绑定的高位端口）——
     在其前面放置反向代理或 iptables 重定向，使公网的 `:80` 能够到达
     它。参见[到达 80 端口](#到达-80-端口)。
   - `dir_url` —— 保持默认值（LE 生产环境）或在验证一切正常运行时
     切换到 staging 环境。
3. 在插件 UI 中**点击*立即签发/续期***。在*首次*签发时
   （证书包为空），插件将：
   - 若 `acc_key` 文件不存在，则自动在该路径生成 ACME 账户密钥。
   - 通过 HTTP-01 签发证书。
   - 重写 `listener_ids` 中的每个监听器（默认为 `ssl:default` 和
     `wss:default`），使其指向新证书包，从而使 MQTT TLS 无需进一步
     配置即可开始使用它。
   - 使用相同的证书在 `:18084` 上自动创建仪表盘 HTTPS 监听器
     （因为 `enable_dashboard_https = true` 是默认值）。

   在此后的每次运行中（续期，或证书包已存在时点击*立即签发/续期*），
   插件仅就地刷新证书包文件；监听器配置和仪表盘 HTTPS 设置保持不变，
   Erlang 的 SSL PEM 缓存会在不重启监听器的情况下重新加载新证书。
4. 通过 `https://your.domain:18084/` **重新加载仪表盘**，登录，然后
   在插件 UI 中点击*禁用仪表盘 HTTP 监听器*。除非你已经处于 HTTPS 上，
   否则该按钮不会执行任何操作 —— 一旦成功，明文的 `:18083` 将在整个
   集群范围内消失。**这是任何生产环境部署的推荐姿态**：保留仪表盘
   HTTP 监听器会使拥有证书的意义荡然无存。

就是这样。续期会通过周期性的 `check_interval_hours` 轮询自动进行；
无需进一步干预。

## 工作原理

1. 插件向配置的 CA 注册一个 ACME 账户（或复用现有账户）。
2. 它启动一个临时 HTTP 监听器，在签发期间响应 HTTP-01 挑战。
3. 已签发的证书链和私钥存储在一个托管证书包中。
   ACME 账户密钥则单独存放在由运维人员管理的 `acc_key` 路径
   （参见 [ACME 账户密钥](#acme-账户密钥)），永远不会写入证书包。
4. SSL 监听器通过 `ssl_options.managed_certs.bundle_name` 引用证书包 ——
   `listener_ids` 让插件在首次签发时为你重写该字段。
5. 一个周期性检查（每 `check_interval_hours` 一次）会在证书将于
   `renew_before_expiry_days` 内过期时对其进行续期。续期会就地刷新
   证书包文件；Erlang 的 SSL PEM 缓存会在不重启监听器的情况下加载
   新证书。

## 配置示例

各字段的详细参考位于插件的 avsc schema 中，并会内联渲染在仪表盘的
插件配置表单中（附带每个字段的 i18n 描述）。下面的 HOCON 示例展示了
一个典型的结构；如需字段级文档，请将鼠标悬停在仪表盘中的字段标签上。

```hocon
dir_url = "https://acme-v02.api.letsencrypt.org/directory"
# Comma-separated list of SAN domains for the cert.
domains = "mqtt.example.com,mqtt2.example.com"
# Comma-separated list of CA contact addresses (renewal/revocation notices).
contact = "mailto:admin@example.com,mailto:ops@example.com"
cert_bundle_name = "acme"
# Comma-separated list of listener IDs to migrate (each "ssl:<name>" or "wss:<name>").
listener_ids = "ssl:default,wss:default"
cert_type = "ec"
# High port EMQX can bind; reverse-proxy or iptables-redirect 80 -> this.
challenge_port = 5080
renew_before_expiry_days = 30
check_interval_hours = 24
enable_dashboard_https = true
dashboard_https_port = 18084
# acc_key is left unset; the plugin manages it inside the cert bundle.
```

然后配置一个 SSL 监听器以使用该证书包（或者，对于在 `listener_ids`
中列出的监听器，插件会在首次签发时为你重写此项）：

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    managed_certs {
      bundle_name = "acme"
    }
  }
}
```

## ACME 账户密钥

在 RFC 8555 中，ACME 账户私钥*即是*账户身份 —— 客户端在本地生成密钥，
发送一个用该密钥签名的 `newAccount` 请求，CA 便立即创建账户。不存在
一个让你"带外注册密钥"的门户。

**默认行为（几乎适用于所有部署，推荐）：** 保持 `acc_key` 未设置。
在首次签发时，插件会在内存中生成一个全新的 EC P-256 密钥（若
`cert_type = "rsa"` 则为 RSA-2048），然后通过
`emqx_managed_certs:add_managed_files/3` 将其存储，该操作会在**每个**
集群节点上将其写入
`<data_dir>/certs2/global/<cert_bundle_name>/acc-key.pem` —— 因此集群的
ACME 身份会被自动复制，无需手动分发密钥。后续签发会就地复用同一个
文件。生命周期：账户密钥与证书链一同位于同一证书包目录中；只要对
数据目录进行绑定挂载或使用 PVC 后端，你就万无一失（参见顶部的持久化
说明）。

**运维人员覆盖（设置了 `acc_key`）：** 当你必须将密钥固定到证书包
之外的路径时使用此方式 —— 例如挂载在某个已知位置的 Kubernetes Secret，
或与另一款软件共享的密钥。将 `acc_key` 设置为该 PEM 的 `file://` URI。
插件在每次签发时读取该文件且从不覆盖它；如果本地节点上该文件不存在，
则自动生成一个（此操作**不会**在集群范围内复制，因此对于集群，你必须
自行预先分发该文件）。如果 PEM 已加密，还需将 `acc_key_password` 设置为
指向一个明文密码文件的 `file://` URI。`${EMQX_ETC_DIR}` / `${VAR}` 会在
使用时展开，因此同一份配置可在 docker 与 DEB/RPM 之间通用。

## 到达 80 端口

ACME CA 始终针对被验证域名的 80 端口执行 HTTP-01 挑战 —— 这是由
RFC 8555 固定规定的，无法在 CA 端配置。然而 EMQX 以非 root 用户
（`emqx`）运行，无法绑定 1024 以下的端口，因此设置
`challenge_port = 80` 通常会以 `eacces` 失败。

受支持的做法是将 `challenge_port` 指向一个 EMQX *能够*绑定的高位端口
（例如 `5080`），并在其前面放置以下方案之一，使公网的 `domain:80`
能够到达 EMQX 的 `challenge_port`：

- **反向代理。** 在同一主机上运行 nginx/caddy/HAProxy（以 root 身份
  或通过 CAP_NET_BIND_SERVICE 能力），将
  `http://domain/.well-known/acme-challenge/*` 代理到
  `http://127.0.0.1:<challenge_port>`。其他路径可返回 404。
- **端口转发。** 在 Linux 上，用 iptables 将入站的 80 端口重定向到
  高位端口：

      iptables -t nat -A PREROUTING -p tcp --dport 80 \
                      -j REDIRECT --to-port 5080

  或使用 `socat`/`systemd` socket 激活来桥接这两个端口。
- **内核权限。** 授予 EMQX 二进制文件 `CAP_NET_BIND_SERVICE` 能力，
  使其可以直接绑定 80 端口：

      setcap 'cap_net_bind_service=+ep' \
             /opt/emqx/erts-*/bin/beam.smp

  这与操作系统和打包方式相关，且在容器化部署中不推荐使用 ——
  请优先采用反向代理方案。

## 前提条件

- 域名必须解析到 EMQX 节点的公网 IP。
- 80 端口（或配置的 `challenge_port`）必须可从互联网访问，以便进行
  HTTP-01 挑战验证。
- 对于 staging/测试，请使用 Let's Encrypt 的 staging URL：
  `https://acme-staging-v02.api.letsencrypt.org/directory`

## API 端点

可通过插件 API 网关在 `/api/v5/plugin_api/emqx_acme-<version>/` 下访问。

| 方法 | 路径 | 说明 |
|--------|------|-------------|
| GET | `/status` | 当前状态：`domains`、`cert_bundle_name`、`in_progress`、`last_result`、`last_check` 以及 `certificate`（签发者、主题、not_after、文件路径）。 |
| POST | `/issue` | 异步发起签发。返回 `202 {"result":"started"}`；轮询 `/status` 获取结果。若已有另一操作正在运行，则返回 `409`。 |
| POST | `/renew` | 与 `/issue` 结构相同，但用于续期。 |
| POST | `/disable_dashboard_http` | 在集群范围内设置 `dashboard.listeners.http.bind = 0`，停止明文监听器。如果未配置仪表盘 HTTPS 监听器，则以 `409 NO_HTTPS_LISTENER` 拒绝。 |

`/ui` 下的插件 UI 使用这些端点；通常你无需手动调用它们。

## 常见问题

### 针对 Let's Encrypt staging 签发成功，但针对生产环境失败

症状 —— 签发失败，错误信息包含以下字符串：

> `During secondary validation: DNS problem: query timed out looking up A for ...`

原因 —— Let's Encrypt **生产环境**会执行*多视角验证*：每个挑战都会
从多个地理位置各异的 LE 网络点重新检查，且**所有**检查都必须成功。
`"During secondary validation"` 标记意味着主验证器到达了你的挑战
监听器，但至少有一个次级验证器（通常位于另一个区域）未能及时解析
你的域名。LE staging 只做主检查，这就是为什么相同的配置针对 staging
能够成功。

这几乎总是 DNS 提供商的特性问题，而非 EMQX/插件路径的问题。免费的
动态 DNS 服务（DuckDNS、No-IP、Dynu……）通常只运行少量权威服务器，
既无区域复制也无 anycast；较远的解析器会看到缓慢或抖动的查询，
从而超出 LE 每个视角约 10 秒的预算。

修复方法：
- **使用具有全球覆盖的 DNS 提供商**（Cloudflare、Route 53、
  Google Cloud DNS、NS1……）并搭配一个真实域名。插件和 Let's
  Encrypt staging 路径均保持不变。
- **在重试生产环境之前，从多个区域验证解析** —— 例如
  `dig @8.8.8.8 your.domain`、`dig @1.1.1.1 your.domain`，再加上
  几个 LE 的[公共测试视角](https://letsdebug.net)。
- **不要疯狂重试。** LE 生产环境会对验证失败进行速率限制（每个
  账户每个主机名每小时 5 次失败授权）；耗尽它们会把"偶尔的抖动"
  变成"被锁定一小时"。先让 DNS 可靠，然后再签发。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.1.2 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_acme-0.2.0.tar.gz) |
| 6.1.3 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_acme-0.2.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
