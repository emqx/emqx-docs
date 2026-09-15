# EMQX ACME 插件

EMQX ACME 插件与兼容 ACME 的证书颁发机构（例如 Let's Encrypt）集成，可为 EMQX SSL 监听器自动签发和续期 TLS 证书。本页面介绍如何在 EMQX 6.1 中配置和使用该插件。签发的证书存储在 EMQX 托管的证书包中。

::: warning 重要提示
重新部署 EMQX 时，必须持久化 `<data_dir>/certs2/`。插件将以下文件存储在 `<data_dir>/certs2/global/<cert_bundle_name>/` 中：

- `chain.pem` 和 `key.pem`：已签发的证书包。如果这些文件丢失，插件会在下次启动时重新签发证书。新证书会计入 Let's Encrypt 针对每个域名的限制，即每周最多签发 5 张重复证书。
- `acc-key.pem`：用于标识 Let's Encrypt 注册账户的 ACME 账户密钥。如果此文件丢失，每次重新部署都会创建新账户。这可能会消耗每个 IP 地址每 3 小时最多创建 10 个新账户的额度，并导致无法吊销与原账户关联的证书。

在 Docker 中，`<data_dir>` 为 `/opt/emqx/data`；在 DEB/RPM 安装中，`<data_dir>` 为 `/var/lib/emqx`。使用 Docker 时，请将整个 `data/` 目录或至少 `data/certs2/` 绑定挂载到宿主机卷。在 Kubernetes 中，请使用持久卷声明（PVC）。首次签发时，插件在证书包中生成账户密钥，并通过 `emqx_managed_certs` 将密钥复制到每个集群节点。
:::

## 前提条件

- 域名必须解析到 EMQX 节点的公网 IP 地址。
- 公网 80 端口必须可从互联网访问，以完成 HTTP-01 挑战验证。如果 `challenge_port` 不是 `80`，请将公网 80 端口的流量转发到配置的 `challenge_port`。
- 使用 Let's Encrypt staging 环境进行测试时，请配置 URL `https://acme-staging-v02.api.letsencrypt.org/directory`。

## 快速开始

按照以下步骤，为具有公网可解析域名的单节点 EMQX 部署配置插件：

1. 在 EMQX Dashboard 中，点击**管理** -> **插件**，安装并启用该插件。
2. 配置以下字段。其他字段保持默认值：
   - `domains = "mqtt.example.com"`：输入以逗号分隔的域名列表。每个域名都必须能通过公网解析到此节点。
   - `contact = "mailto:admin@example.com"`：输入以逗号分隔的联系地址列表，用于接收证书颁发机构（CA）的续期和吊销通知。
   - `challenge_port = 5080`：输入 EMQX 可以绑定的高位端口。配置反向代理或 `iptables` 重定向，使公网 80 端口的流量能够到达该端口。参见[配置 80 端口访问](#配置-80-端口访问)。
   - `dir_url`：保留默认的 Let's Encrypt 生产环境 URL，或在测试配置时使用 staging 环境 URL。
3. 在插件 UI 中，点击**Issue / Renew Now**。首次签发时，证书包为空，插件会执行以下操作：
   - 如果托管证书包中不存在 ACME 账户密钥，则在证书包中生成账户密钥。
   - 通过 HTTP-01 签发证书。
   - 重写 `listener_ids` 中的每个监听器，使其使用新的证书包。`listener_ids` 的默认值为 `ssl:default,wss:default`。
   - 使用相同证书在 `18084` 端口创建 Dashboard HTTPS 监听器，因为 `enable_dashboard_https` 的默认值为 `true`。

   后续运行时，插件仅更新证书包文件。监听器配置和 Dashboard HTTPS 配置保持不变。Erlang SSL PEM 缓存会加载新证书，而无需重启监听器。
4. 打开 `https://your.domain:18084/` 并登录 Dashboard。在插件 UI 中，点击**Disable Dashboard HTTP Listener**。仅当通过 HTTPS 打开插件页面时，此按钮才可用。操作成功后，集群中的 `18083` 端口明文监听器将被禁用。生产环境建议使用此配置，因为保留 HTTP 监听器会继续允许通过明文 HTTP 访问 Dashboard。

插件按照 `check_interval_hours` 配置的间隔检查证书，并在需要时自动续期。

## 工作原理

1. 插件向配置的 CA 注册一个 ACME 账户（或复用现有账户）。
2. 插件启动临时 HTTP 监听器，在签发期间响应 HTTP-01 挑战。
3. 已签发的证书链和私钥存储在托管证书包中。默认情况下，插件还会将 ACME 账户密钥存储在此证书包中。如果配置了 `acc_key`，插件会改用该路径中由运维人员管理的文件。更多信息，参见[ACME 账户密钥](#acme-账户密钥)。
4. SSL 监听器通过 `ssl_options.managed_certs.bundle_name` 引用证书包。首次签发时，插件可以为 `listener_ids` 指定的监听器重写此字段。
5. 插件按照 `check_interval_hours` 配置的间隔检查证书。如果证书将在 `renew_before_expiry_days` 指定的时间内过期，插件会为其续期。续期会更新证书包文件，Erlang SSL PEM 缓存会加载新证书，而无需重启监听器。

## 配置示例

插件会在 Dashboard 配置表单中显示 `config_schema.avsc` 提供的字段说明。以下 HOCON 示例展示了典型的插件配置。将鼠标悬停在 Dashboard 中的字段标签上，可以查看相应说明。

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

然后配置 SSL 监听器以使用该证书包。对于 `listener_ids` 指定的监听器，插件会在首次签发时重写此配置。

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

在 RFC 8555 中，ACME 账户私钥用于标识账户。客户端在本地生成密钥，并发送使用该密钥签名的 `newAccount` 请求。CA 随后创建账户。无需通过其他门户单独注册密钥。

**默认行为：** 保持 `acc_key` 未设置。首次签发时，插件会在内存中生成 EC P-256 密钥。如果 `cert_type = "rsa"`，则生成 RSA-2048 密钥。随后，插件通过 `emqx_managed_certs:add_managed_files/3` 将密钥写入每个集群节点上的 `<data_dir>/certs2/global/<cert_bundle_name>/acc-key.pem`。后续签发会复用同一文件。请通过绑定挂载或 PVC 持久化数据目录，以保留账户密钥和证书链。参见本页面开头的持久化警告。

**运维覆盖：** 当密钥必须使用证书包以外的路径时，请设置 `acc_key`。例如，密钥可能来自挂载到指定位置的 Kubernetes Secret，或需要与其他软件共享。将 `acc_key` 设置为 PEM 文件的 `file://` URI。插件在每次签发时读取此文件，且不会覆盖该文件。如果本地节点上不存在此文件，插件会在该节点上生成一个。该文件不会在集群中复制，因此必须将其分发到每个集群节点。如果 PEM 文件已加密，请将 `acc_key_password` 设置为指向明文密码文件的 `file://` URI。`${EMQX_ETC_DIR}` 和 `${VAR}` 会在使用时展开，因此相同配置可用于 Docker 和 DEB/RPM 安装。

## 配置 80 端口访问

ACME CA 始终通过被验证域名的 80 端口执行 HTTP-01 挑战。此行为由 RFC 8555 定义，无法在 CA 端配置。EMQX 以非 root 用户 `emqx` 运行，通常无法绑定 `1024` 以下的端口。因此，设置 `challenge_port = 80` 通常会导致 `eacces` 错误。

将 `challenge_port` 设置为 EMQX 可以绑定的高位端口，例如 `5080`。然后使用以下方法之一，将公网 80 端口的流量转发到配置的 `challenge_port`：

- **反向代理：** 在同一主机上以 root 身份或通过 `CAP_NET_BIND_SERVICE` capability 运行 NGINX、Caddy 或 HAProxy。将 `http://domain/.well-known/acme-challenge/*` 代理到 `http://127.0.0.1:<challenge_port>`。其他路径可以返回 `404`。
- **端口转发：** 在 Linux 上，使用 `iptables` 将 80 端口的入站流量重定向到高位端口：

  ```bash
  iptables -t nat -A PREROUTING -p tcp --dport 80 \
                  -j REDIRECT --to-port 5080
  ```

  也可以使用 `socat` 或 `systemd` socket activation 连接两个端口。
- **内核 capability：** 为 EMQX 二进制文件授予 `CAP_NET_BIND_SERVICE` capability，使其可以直接绑定 80 端口：

  ```bash
  setcap 'cap_net_bind_service=+ep' \
         /opt/emqx/erts-*/bin/beam.smp
  ```

  此方法取决于操作系统和打包方式，不建议用于容器化部署。请优先使用反向代理。

## API 端点

下表列出了可通过插件 API 网关的 `/api/v5/plugin_api/emqx_acme-<version>/` 路径访问的主要端点：

| 方法 | 路径 | 说明 |
| --- | --- | --- |
| GET | `/status` | 返回当前状态，包括 `domains`、`cert_bundle_name`、`in_progress`、`last_result`、`last_check` 和 `certificate`。如果证书存在，`certificate` 包含 `exists`、`chain_path`、`key_path` 和 `expiry`；否则仅包含 `exists: false`。 |
| POST | `/issue` | 异步发起签发。返回 `202 {"result":"started"}`；轮询 `/status` 获取结果。若已有另一操作正在运行，则返回 `409`。 |
| POST | `/renew` | 与 `/issue` 结构相同，但用于续期。 |
| POST | `/disable_dashboard_http` | 在集群范围内设置 `dashboard.listeners.http.bind = 0`，停止明文监听器。如果未配置 Dashboard HTTPS 监听器，则返回 `409 NO_HTTPS_LISTENER`。 |

这些端点支持主要的证书管理操作。插件 UI 会执行这些操作，因此通常无需直接调用这些端点。

## 故障排查

### Let's Encrypt Staging 环境签发成功但生产环境签发失败

**症状：** 证书签发失败，错误信息包含以下内容：

> `During secondary validation: DNS problem: query timed out looking up A for ...`

**原因：** 此错误表示某次次级验证执行 DNS 查询时超时。Let's Encrypt 的 staging 和生产环境均使用多视角验证。因此，staging 环境中的请求成功并不表示后续生产环境请求一定成功。临时 DNS 或网络异常、DNS 响应不一致，或者域名 DNS 记录中的地址无法访问，都可能导致不同的验证结果。

**解决方法：**

- 确认域名的权威 DNS 服务器能够持续返回预期的 `A` 和 `AAAA` 记录。例如，运行 `dig @8.8.8.8 your.domain` 和 `dig @1.1.1.1 your.domain`。
- 确认域名的所有 `A` 和 `AAAA` 记录对应的地址均可通过公网 80 端口访问，并且流量能够到达配置的 `challenge_port`。
- 使用 [Let's Debug 诊断服务](https://letsdebug.net)，从外部验证视角检查域名。
- 避免反复重试。Let's Encrypt 生产环境允许每个账户的每个标识符每小时最多出现 5 次授权失败。请先解决 DNS 或网络问题，再次请求证书。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.2.1 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_acme-0.2.0.sha256)) |
| 6.2.2 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_acme-0.2.0.sha256)) |
| 6.2.3 | 0.2.0 | [emqx_acme-0.2.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_acme-0.2.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_acme-0.2.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
