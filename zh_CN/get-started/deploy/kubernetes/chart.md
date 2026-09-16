# 使用 Helm Chart 在 Kubernetes 中部署 EMQX

本页面提供了使用 EMQX 官方 Helm Chart 在 Kubernetes 集群中部署 EMQX 的详细步骤。

EMQX Helm Chart 将部署所需的所有 Kubernetes 资源（如 StatefulSet、Service、ConfigMap、Ingress 规则和 Gateway API 路由）打包为一个可配置的 Helm Chart，从而简化了基于 Kubernetes 的部署过程。

## 前提条件

开始之前，请确保已安装并配置以下组件：

- 一个可用的 Kubernetes 集群（版本 ≥ 1.6）
- [Helm](https://github.com/helm/helm/releases)

## 安装 EMQX Helm Chart

你可以从 EMQX 的 GitHub 仓库或官方 Helm Chart 仓库安装 EMQX Helm Chart。

### 从 GitHub 安装

从 GitHub 安装并命名为 `my-emqx` 的 Chart：

```bash
$ git clone https://github.com/emqx/emqx.git
$ cd emqx/deploy/charts/emqx-enterprise
$ helm install my-emqx .
```

### 从 Helm 仓库安装

从官方 Helm 仓库安装并命名为 `my-emqx` 的 Chart：

```bash
helm repo add emqx https://repos.emqx.io/charts
helm install my-emqx emqx/emqx-enterprise
```

> 如果你希望安装不稳定版本，请添加 `--devel` 标志：
>
> ```bash
> helm install my-emqx emqx/emqx-enterprise --devel
> ```

## 卸载 Chart

要删除名为 `my-emqx` 的 Helm 部署并清理所有关联的 Kubernetes 资源：

**Helm v3 及以上版本**

```bash
$ helm uninstall my-emqx
```

**Helm v2（旧版）**

```bash
$ helm del my-emqx
```

## 配置参数

EMQX Helm Chart 通过 `values.yaml` 文件提供丰富的可配置参数。下表列出了一些关键参数及其默认值：

| 参数                            | 描述                                                                                                                                                  | 默认值                                           |
|--------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------|
| `replicaCount`                       | 建议集群中的节点数为奇数，否则在发生网络分裂时，EMQX 集群将无法自动愈合。                  | 3                                                       |
| `image.repository`                   | 使用的 EMQX 镜像仓库地址                                                                                                                            | emqx/emqx-enterprise                                    |
| `image.pullPolicy`                   | 镜像拉取策略                                                                                                                                        | IfNotPresent                                            |
| `image.pullSecrets `                 | 镜像拉取密钥                                                                                                                                       | `[]` (不为部署的 Pod 添加镜像拉取密钥) |
| `serviceAccount.create`              | 如果为 `true`，则创建一个新的服务账户                                                                                                                      | `true`                                                  |
| `serviceAccount.name`                | 要使用的服务账户。如果未设置且 `serviceAccount.create` 为 `true`，则使用完整名称模板生成一个名称。                              |                                                         |
| `serviceAccount.annotations`         | 要添加到服务账户的注解                                                                                                                    |                                                         |
| `envFromSecret`                      | 在同一 Kubernetes 命名空间中拉取一个 Secret，其中包含将添加到环境中的值                                          | nil                                                     |
| `recreatePods`                       | 在升级期间强制重新创建 Pod，这对于始终应用最新配置很有用。                                            | false                                                   |
| `podAnnotations `                    | Pod 的注解                                                                                                                                          | `{}`                                                    |
| `podManagementPolicy`                | 要使用现有的 PVC 重新部署 Chart，该值必须设置为 `Parallel` 以避免死锁。                                                             | `Parallel`                                              |
| `persistence.enabled`                | 是否启用持久化存储。                                                                                                                  | false                                                   |
| `persistence.storageClass`           | 后端 PVC 的存储类名称                                                                                                                               | `nil` (使用 alpha 存储类注解)             |
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
| `service.wsEnabled`                  | 是否在 Service 中发布 WebSocket 和 WSS 端口。启用 `httpRoute.ws.enabled` 或 `tlsRoute.wss.enabled` 时，该参数必须设为 `true`。 | true |
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

### EMQX 特有参数

以下表格列出了 Helm Chart 中可配置的 EMQX 专用参数及其默认值：
| 参数                                                                                                                                                              | 描述                                                                   | 默认值 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|---------------|
| `emqxConfig`                                                                                                                                                           | 一组[配置项](https://docs.emqx.com/zh/emqx/latest/configuration/configuration.html) 的映射，支持通过[环境变量](https://docs.emqx.com/zh/emqx/latest/configuration/configuration.html#%E7%8E%AF%E5%A2%83%E5%8F%98%E9%87%8F)（可省略 `EMQX_` 前缀）或使用 EMQX 配置文件中的命名空间点符号格式进行定义。 | `nil`         |
| `emqxLicenseSecretName`                                                                                                                                                | 存储许可证信息的 Secret 名称（已弃用） | `nil`         |
| `emqxLicenseSecretRef.name`                                                                                                                                         | 包含许可证信息的 Secret 名称                       | `""`         |
| `emqxLicenseSecretRef.key`                                                                                                                                          | 包含许可证信息的 Secret 中的键                        | `""`         |

## 配置功能门控

从 EMQX 6.3.0 开始，可以设置 `EMQX_FEATURES` 控制启动时可用的可选功能。例如：

```yaml
emqxConfig:
  EMQX_FEATURES: "dashboard,metrics,plugins"
```

功能门控只在 EMQX 启动时解析。如果修改该值，需要重新创建或重启 EMQX Pod。完整功能列表和依赖行为请参见[功能门控](../feature-gates.md)。

## 配置 Gateway API 路由

从 EMQX 6.3 开始，EMQX Enterprise Helm Chart 可以创建 Kubernetes Gateway API 路由，作为 Ingress 资源的替代方案。`HTTPRoute` 用于暴露 EMQX Dashboard 和 MQTT over WebSocket，`TLSRoute` 使用 TLS 透传暴露 MQTTS 和 WSS。

启用路由前，请完成以下准备工作：

- 安装 [Gateway API 控制器及其自定义资源定义（CRD）](https://gateway-api.sigs.k8s.io/guides/getting-started/introduction/)。
- 如需启用 `tlsRoute.mqtts` 或 `tlsRoute.wss`，请使用 Kubernetes 1.31 或更高版本，以及 Gateway API 1.5.0 或更高版本的 standard channel CRD。Gateway API 控制器必须支持 Passthrough 模式的 `TLSRoute`。
- 如需启用 `httpRoute.ws`，请使用无需 `ServicePort.appProtocol` 即可处理 HTTPRoute WebSocket 流量的 Gateway API 控制器。EMQX Helm Chart 不会在 WebSocket Service 端口上设置 `appProtocol`。
- 创建包含匹配监听器的 Gateway。
- 将 `tlsRoute.mqtts` 和 `tlsRoute.wss` 使用的 Gateway TLS 监听器配置为 `tls.mode: Passthrough`。TLS 连接由 EMQX 终结。

所有路由默认关闭。请为每个启用的路由设置 `parentRefs`，以将其关联到 Gateway。以下 `values.yaml` 示例启用了所有支持的路由：

```yaml
service:
  wsEnabled: true

httpRoute:
  dashboard:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: https
    hostnames:
      - dashboard.emqx.local
    path: /
    pathType: PathPrefix
  ws:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: https
    hostnames:
      - ws.emqx.local
    path: /mqtt
    pathType: PathPrefix

tlsRoute:
  mqtts:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: mqtts
    hostnames:
      - mqtt.emqx.local
  wss:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: wss
    hostnames:
      - wss.emqx.local
```

请根据实际环境替换 Gateway 名称、命名空间、监听器名称和主机名。如果 Gateway 与 Helm 发布实例位于不同命名空间，请在引用的每个 Gateway 监听器上配置 `allowedRoutes`，允许来自 Helm 发布实例所在命名空间的 Route。Helm Chart 将创建以下路由：

| 路由 | 后端 Service 端口 | 默认路径 |
| --- | --- | --- |
| `httpRoute.dashboard` | Dashboard 和 API 的 `18083` 端口 | `/` |
| `httpRoute.ws` | MQTT over WebSocket 的 `8083` 端口 | `/mqtt` |
| `tlsRoute.mqtts` | MQTTS 的 `8883` 端口 | 不适用 |
| `tlsRoute.wss` | WSS 的 `8084` 端口 | 不适用 |

::: warning 重要提示

启用 `httpRoute.ws` 或 `tlsRoute.wss` 时，请将 `service.wsEnabled` 保持为 `true`。否则，Helm 会停止渲染 Chart，并返回 `httpRoute.ws.enabled requires service.wsEnabled=true` 或 `tlsRoute.wss.enabled requires service.wsEnabled=true`。

:::

安装或升级 Helm 发布实例后，检查路由状态：

```bash
kubectl get httproute,tlsroute -o yaml
```

对于每个已关联的路由，确认 `status.parents` 中的 `Accepted` 和 `ResolvedRefs` 条件均为 `True`。如果控制器报告 `Programmed` 条件，还需确认该条件为 `True`。

Route 条件不能验证端到端流量。请向配置的 Dashboard 或 WebSocket 主机名发送请求，或通过 Gateway 建立 MQTTS 或 WSS 连接，以确认路由可以将流量转发到 EMQX。

Gateway API 路由参数如下：

| 参数 | 描述 | 默认值 |
| --- | --- | --- |
| `httpRoute.<route>.enabled` | 是否创建 `dashboard` 或 `ws` HTTPRoute。 | `false` |
| `httpRoute.<route>.annotations` | 添加到 HTTPRoute 的注解。 | `{}` |
| `httpRoute.<route>.labels` | 添加到 HTTPRoute 的标签。 | `{}` |
| `httpRoute.<route>.parentRefs` | 对父级 Gateway 和监听器的引用。 | `[]` |
| `httpRoute.<route>.hostnames` | 路由匹配的主机名。 | `dashboard` 默认为 `dashboard.emqx.local`；`ws` 默认为 `ws.emqx.local` |
| `httpRoute.<route>.path` | 路由匹配的路径。 | `dashboard` 默认为 `/`；`ws` 默认为 `/mqtt` |
| `httpRoute.<route>.pathType` | 路径匹配类型。 | `PathPrefix` |
| `tlsRoute.<route>.enabled` | 是否创建 `mqtts` 或 `wss` TLSRoute。 | `false` |
| `tlsRoute.<route>.annotations` | 添加到 TLSRoute 的注解。 | `{}` |
| `tlsRoute.<route>.labels` | 添加到 TLSRoute 的标签。 | `{}` |
| `tlsRoute.<route>.parentRefs` | 对父级 Gateway 和 TLS 监听器的引用。 | `[]` |
| `tlsRoute.<route>.hostnames` | 路由匹配的必填 SNI 主机名。列表必须至少包含一个有效的完全限定域名。 | `mqtts` 默认为 `mqtt.emqx.local`；`wss` 默认为 `wss.emqx.local` |

## SSL 设置
使用 `cert-manager` 时，TLS 证书会以 Kubernetes Secret 的形式保存，使用标准的键名：`tls.crt` 和 `tls.key`。EMQX Helm Chart 会将这些文件自动挂载至容器内的以下路径：

```
/tmp/ssl/
```

如需启用 SSL 支持，请显式指定证书路径，可通过配置文件或环境变量完成：

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip 提示

如果你使用的是已有证书（而非由 cert-manager 生成），请确保文件路径与挂载路径一致。

:::

## 代理协议支持（HAProxy / Nginx）

如果 EMQX 部署在支持 Proxy Protocol 的反向代理（如 HAProxy 或 Nginx）之后，可通过设置以下环境变量启用该功能：

```yaml
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

针对 HAProxy Ingress Controller，请在 Ingress 中添加如下注解：

```yaml
haproxy-ingress.github.io/proxy-protocol: "v2"
```

启用后可保留客户端的原始 IP 地址。
