# Helm Chart

此 Chart 用于在 Kubernetes 集群上通过 Helm 包管理器来部署 EMQX。

# 前提条件

+ Kubernetes 1.6+
+ Helm

# 安装 Chart

使用 `my-emqx` 的发布名称安装此 Chart：

+ 从 Github
  ```
  $ git clone https://github.com/emqx/emqx.git
  $ cd emqx/deploy/charts/emqx-enterprise
  $ helm install my-emqx .
  ```

+ 从 Chart 仓库
  ```
  helm repo add emqx https://repos.emqx.io/charts
  helm install my-emqx emqx/emqx-enterprise
  ```
  > 如果你想安装一个不稳定版本，你需要在执行 `helm install` 命令时添加 `--devel` 参数。

# 卸载 Chart

卸载/删除 `my-emqx` 部署：

```
$ helm del  my-emqx
```

# 配置

下表列出了 EMQX Chart 的可配置参数及其默认值。

| 参数                            | 描述                                                                                                                                                  | 默认值                                           |
|--------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------|
| `replicaCount`                       | 建议集群中的节点数为奇数，否则在发生网络分裂时，EMQX 集群将无法自动愈合。                  | 3                                                       |
| `image.repository`                   | EMQX 镜像名称                                                                                                                                              | emqx/emqx-enterprise                                    |
| `image.pullPolicy`                   | 镜像拉取策略                                                                                                                                        | IfNotPresent                                            |
| `image.pullSecrets `                 | 镜像拉取密钥                                                                                                                                       | `[]` (不为部署的 Pod 添加镜像拉取密钥) |
| `serviceAccount.create`              | 如果为 `true`，则创建一个新的服务账户                                                                                                                      | `true`                                                  |
| `serviceAccount.name`                | 要使用的服务账户。如果未设置且 `serviceAccount.create` 为 `true`，则使用完整名称模板生成一个名称                               |                                                         |
| `serviceAccount.annotations`         | 要添加到服务账户的注解                                                                                                                    |                                                         |
| `envFromSecret`                      | 在同一 Kubernetes 命名空间中拉取一个 Secret，其中包含将添加到环境中的值                                          | nil                                                     |
| `recreatePods`                       | 在升级期间强制重新创建 Pod，这对于始终应用最新配置很有用。                                            | false                                                   |
| `podAnnotations `                    | Pod 的注解                                                                                                                                          | `{}`                                                    |
| `podManagementPolicy`                | 要使用现有的 PVC 重新部署 Chart，该值必须设置为 Parallel 以避免死锁                                                                | `Parallel`                                              |
| `persistence.enabled`                | 使用 PVC 启用 EMQX 持久化                                                                                                                            | false                                                   |
| `persistence.storageClass`           | 后端 PVC 的存储类                                                                                                                                 | `nil` (使用 alpha 存储类注解)             |
| `persistence.existingClaim`          | EMQX 数据持久卷的现有声明名称，作为模板进行评估                                                                                     | ""                                                      |
| `persistence.accessMode`             | EMQX 卷的 PVC 访问模式                                                                                                                              | ReadWriteOnce                                           |
| `persistence.size`                   | EMQX 卷的 PVC 存储请求                                                                                                                          | 20Mi                                                    |
| `initContainers`                     | 在创建 EMQX 容器之前运行的容器。它们可以包含实用程序或设置脚本。                                                     | `{}`                                                    |
| `resources`                          | CPU/内存资源请求/限制                                                                                                                          | {}                                                      |
| `extraVolumeMounts`                  | 到默认后端容器的额外 volumeMounts。                                                                                                    | []                                                      |
| `extraVolumes`                       | 到默认后端 Pod 的额外 volumes。                                                                                                               | []                                                      |
| `nodeSelector`                       | 用于 Pod 分配的节点标签                                                                                                                               | `{}`                                                    |
| `tolerations`                        | 用于 Pod 分配的容忍度标签                                                                                                                         | `[]`                                                    |
| `affinity`                           | 节点/Pod 亲和性映射                                                                                                                                   | `{}`                                                    |
| `service.type`                       | Kubernetes 服务类型。                                                                                                                                     | ClusterIP                                               |
| `service.mqtt`                       | MQTT 端口。                                                                                                                                               | 1883                                                    |
| `service.mqttssl`                    | MQTT(SSL) 端口。                                                                                                                                          | 8883                                                    |
| `service.ws`                         | WebSocket/HTTP 端口。                                                                                                                                     | 8083                                                    |
| `service.wss`                        | WSS/HTTPS 端口。                                                                                                                                          | 8084                                                    |
| `service.dashboard`                  | Dashboard 和 API 端口。                                                                                                                                  | 18083                                                   |
| `service.customPorts`                | 在服务中暴露的自定义端口。                                                                                                                      | {}                                                      |
| `service.nodePorts.mqtt`             | MQTT 的 Kubernetes 节点端口。                                                                                                                               | nil                                                     |
| `service.nodePorts.mqttssl`          | MQTT(SSL) 的 Kubernetes 节点端口。                                                                                                                          | nil                                                     |
| `service.nodePorts.ws`               | WebSocket/HTTP 的 Kubernetes 节点端口。                                                                                                                     | nil                                                     |
| `service.nodePorts.wss`              | WSS/HTTPS 的 Kubernetes 节点端口。                                                                                                                          | nil                                                     |
| `service.nodePorts.dashboard`        | Dashboard 的 Kubernetes 节点端口。                                                                                                                          | nil                                                     |
| `service.customNodePorts`            | 自定义端口的 Kubernetes 节点端口。                                                                                                                       | {}                                                      |
| `service.loadBalancerClass`          | 此服务所属的负载均衡器实现                                                                                                     |                                                         |
| `service.loadBalancerIP`             | 服务的 loadBalancerIP                                                                                                                                   | nil                                                     |
| `service.loadBalancerSourceRanges`   | 当服务是 LoadBalancer 时允许的地址                                                                                                    | []                                                      |
| `service.externalIPs`                | 服务的 ExternalIPs                                                                                                                                  | []                                                      |
| `service.externalTrafficPolicy`      | 服务的外部流量策略                                                                                                                      | `Cluster`                                               |
| `service.annotations`                | 服务/ServiceMonitor 的注解                                                                                                                           | {}(作为模板评估)                             |
| `service.labels`                     | 服务/ServiceMonitor 的标签                                                                                                                                | {}(作为模板评估)                             |
| `ingress.dashboard.enabled`          | 为 EMQX Dashboard 启用 Ingress                                                                                                                            | false                                                   |
| `ingress.dashboard.ingressClassName` | 为 EMQX Dashboard 设置 Ingress 类                                                                                                                     |                                                         |
| `ingress.dashboard.path`             | EMQX Dashboard 的 Ingress 路径                                                                                                                              | /                                                       |
| `ingress.dashboard.pathType`         | EMQX Dashboard 的 Ingress pathType                                                                                                                          | `ImplementationSpecific`                                |
| `ingress.dashboard.hosts`            | EMQX Dashboard 的 Ingress 主机                                                                                                                             | dashboard.emqx.local                                    |
| `ingress.dashboard.tls`              | EMQX Dashboard 的 Ingress tls                                                                                                                               | []                                                      |
| `ingress.dashboard.annotations`      | EMQX Dashboard 的 Ingress 注解                                                                                                                       | {}                                                      |
| `ingress.dashboard.ingressClassName` | 为 EMQX Dashboard 设置 Ingress 类                                                                                                                     |                                                         |
| `ingress.mqtt.enabled`               | 为 MQTT 启用 Ingress                                                                                                                                      | false                                                   |
| `ingress.mqtt.ingressClassName`      | 为 MQTT 设置 Ingress 类                                                                                                                               |                                                         |
| `ingress.mqtt.path`                  | MQTT 的 Ingress 路径                                                                                                                                        | /                                                       |
| `ingress.mqtt.pathType`              | MQTT 的 Ingress pathType                                                                                                                                    | `ImplementationSpecific`                                |
| `ingress.mqtt.hosts`                 | MQTT 的 Ingress 主机                                                                                                                                       | mqtt.emqx.local                                         |
| `ingress.mqtt.tls`                   | MQTT 的 Ingress tls                                                                                                                                         | []                                                      |
| `ingress.mqtt.annotations`           | MQTT 的 Ingress 注解                                                                                                                                 | {}                                                      |
| `ingress.mqtt.ingressClassName`      | 为 MQTT 设置 Ingress 类                                                                                                                               |                                                         |
| `metrics.enable`                     | 如果设置为 true，需要安装 [prometheus-operator](https://github.com/prometheus-operator/prometheus-operator)，并且需要启用 emqx_prometheus | false                                                   |
| `metrics.type`                       | 目前我们只支持 "prometheus"                                                                                                                           | "prometheus"                                            |
| `ssl.enabled`                        | 启用 SSL 支持                                                                                                                                           | false                                                   |
| `ssl.useExisting`                    | 使用现有证书或让 cert-manager 生成一个                                                                                                    | false                                                   |
| `ssl.existingName`                   | 现有证书的名称                                                                                                                                 | emqx-tls                                                |
| `ssl.dnsnames`                       | 要生成的证书的 DNS 名称                                                                                                                  | {}                                                      |
| `ssl.commonName`                     | 要生成的证书的通用名称                                                                                                               |                                                         |
| `ssl.issuer.name`                    | 证书生成的颁发者名称                                                                                                                       | letsencrypt-dns                                         |
| `ssl.issuer.kind`                    | 证书生成的颁发者类型                                                                                                                       | ClusterIssuer                                           |

## EMQX 特定设置

下表列出了 Chart 中可配置的 [EMQX](https://www.emqx.io/) 特定参数及其默认值。
| 参数                                                                                                                                                              | 描述                                                                   | 默认值 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|---------------|
| `emqxConfig`                                                                                                                                                           | 以环境变量（可省略 `EMQX_` 前缀）或使用配置文件[命名空间点分表示法](https://docs.emqx.com/zh/emqx/latest/configuration/configuration.html#environment-variables)表示的[配置](https://docs.emqx.com/zh/emqx/latest/configuration/configuration.html)项的映射       | `nil`         |
| `emqxLicenseSecretName`                                                                                                                                                | 保存许可证信息的 Secret 名称（已弃用）         | `nil`         |
| `emqxLicenseSecretRef.name`                                                                                                                                         | 保存许可证信息的 Secret 名称                         | `""`         |
| `emqxLicenseSecretRef.key`                                                                                                                                          | 保存许可证信息的 Secret 中的键                          | `""`         |

## SSL 设置
`cert-manager` 使用 `tls.crt` 和 `tls.key` 这两个键生成包含证书数据的 Secret。Helm Chart 总是将这些键作为文件挂载到 `/tmp/ssl/`，这需要通过更改 emqx 配置文件或传递以下环境变量来显式配置：

```
  EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
  EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

如果您选择使用现有证书，请确保相应地更新文件名。

## 提示
如果 EMQX 集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2。
为了保留原始客户端的 IP 地址，您可以通过传递以下环境变量来更改 emqx 配置：

```
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

对于 HAProxy，您还需要以下 Ingress 注解：

```
haproxy-ingress.github.io/proxy-protocol: "v2"
```
