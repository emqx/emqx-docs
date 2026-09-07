# 对 EMQX 集群执行滚动更新

## 目标

在不中断服务的情况下，对 EMQX 集群执行滚动更新。

## 背景

当 EMQX Pod 模板中的字段发生变化时，例如镜像、镜像拉取策略、资源请求或节点模板发生变化，EMQX Operator 会执行滚动更新。

滚动更新期间，Core 节点通过单个 StatefulSet 逐个 Pod 原地更新。Replicant 节点采用 Deployment 式的发布方式，并由 `maxUnavailable` 和 `maxSurge` 控制。默认情况下，Operator 会先执行节点疏散，迁移 MQTT 连接和会话，再删除 Pod。可将 `.spec.updateStrategy.evacuationStrategy.type` 设置为 `Disabled` 来禁用节点疏散。

## 解决方案

如果对 EMQX 自定义资源（CR）的修改会改变 Pod 模板，EMQX Operator 将比较预期模板与当前运行的工作负载，并持续更新集群，直到其管理的所有 Pod 都与新模板一致。

对于 Core 节点，Operator 会更新 StatefulSet 模板；如果已启用节点疏散，则先疏散选定的 Core Pod，然后使用新模板重新创建该 Pod，并等待其就绪后再更新下一个 Core Pod。对于 Replicant 节点，Operator 最多按照 `maxSurge` 指定的数量创建新的 Replicant Pod，同时最多按照 `maxUnavailable` 指定的数量疏散旧的 Replicant Pod。这些设置用于控制更新速度，并使提供服务的节点数保持在配置的范围内。

在 Core-Replicant 集群中，必须至少有一个更新后的 Core 节点就绪，才能开始更新 Replicant 节点；在 Replicant Pod 全部迁离旧版本之前，还会保留至少一个旧的 Core 节点。

## 操作步骤

### 配置更新策略

1. 创建一个 `apps.emqx.io/v3beta1` EMQX CR，并配置更新策略。

  ```yaml
  apiVersion: apps.emqx.io/v3beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx:@EE_VERSION@
    config:
      roots:
        license:
          key: "..."
    updateStrategy:
      type: RollingUpdate
      evacuationStrategy:
        # MQTT 客户端疏散速率，单位为连接数/秒：
        connectionEvictionRate: 1000
        # MQTT 会话疏散速率，单位为会话数/秒：
        sessionEvictionRate: 1000
        # 删除 Pod 前的等待时间：
        waitTakeover: 10
      replicants:
        maxUnavailable: 1
        maxSurge: 1
    coreTemplate:
      spec:
        replicas: 2
    replicantTemplate:
      spec:
        replicas: 3
    listenersServiceTemplate:
      spec:
        type: LoadBalancer
  ```

2. 将以上内容保存为 `emqx-update.yaml`，并使用 `kubectl apply` 部署：

  ```bash
  $ kubectl apply -f emqx-update.yaml
  emqx.apps.emqx.io/emqx created
  ```

3. 检查 EMQX 集群状态。

  确保 `STATUS` 为 `Ready`。此过程可能需要一些时间。

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

### 连接到 EMQX 集群

[MQTTX CLI](https://mqttx.app/cli) 是一款开源且兼容 MQTT 5.0 的命令行客户端，可用于开发和调试 MQTT 服务与应用，并支持自动重连。

获取 `emqx-listeners` Service 的外部地址。以下命令同时适用于发布 IP 地址或主机名的负载均衡器。

```bash
export EMQX_HOST="$(kubectl get service emqx-listeners -o jsonpath='{.status.loadBalancer.ingress[0].ip}{.status.loadBalancer.ingress[0].hostname}')"
```

确认 `EMQX_HOST` 中已包含地址后，使用 MQTTX 向端口 `1883` 上的默认 TCP 监听器建立 3,000 个连接：

```bash
mqttx bench conn -h "${EMQX_HOST}" -p 1883 -c 3000
[10:05:21 AM] › ℹ  Start the connect benchmarking, connections: 3000, req interval: 10ms
✔  success   [3000/3000] - Connected
[10:06:13 AM] › ℹ  Done, total time: 31.113s
```

### 触发更新

1. 更新 Core 和 Replicant Pod 模板中的注解，以触发滚动更新。时间戳可确保每次运行命令时注解都会获得一个新值。

  ```bash
  ROLLOUT_ID="$(date +%s)"

  kubectl patch emqx emqx --type=merge -p \
    "{\"spec\":{\"coreTemplate\":{\"metadata\":{\"annotations\":{\"docs.emqx.com/rollout-id\":\"${ROLLOUT_ID}\"}}},\"replicantTemplate\":{\"metadata\":{\"annotations\":{\"docs.emqx.com/rollout-id\":\"${ROLLOUT_ID}\"}}}}}"
  ```

  预期输出：

  ```text
  emqx.apps.emqx.io/emqx patched
  ```

2. 检查更新进度。

  ```bash
  $ kubectl get emqx emqx -o json | jq ".status.nodeEvacuations"
  [
    {
      "nodeName": "emqx@10.244.4.56",
      "initialConnections": 33,
      "initialSessions": 0,
      "connectionEvictionRate": 200,
      "sessionEvictionRate": 200,
      "state": "waiting_takeover",
      "sessionRecipients": [
        "emqx@10.244.4.57",
        "emqx@10.244.4.58"
      ]
    }
  ]
  ```

  | 字段                     | 说明                                             |
  |--------------------------|--------------------------------------------------|
  | `nodeName`               | 当前正在疏散的节点。                             |
  | `state`                  | 节点疏散阶段。                                   |
  | `sessionRecipients`      | MQTT 会话的接收节点。                            |
  | `sessionEvictionRate`    | 此节点的 MQTT 会话疏散速率（会话数/秒）。        |
  | `connectionEvictionRate` | 此节点的 MQTT 连接疏散速率（连接数/秒）。        |
  | `initialSessions`        | 此节点的初始会话数。                             |
  | `initialConnections`     | 此节点的初始连接数。                             |

  可查看对应的 [EMQX 节点状态](../reference/v3beta1-reference.md#emqxnode)中的 `connections` 和 `sessions` 计数器，估算节点疏散进度。

3. 等待更新完成。

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

  确保 `STATUS` 为 `Ready`。更新所需时间取决于 MQTT 客户端和会话的数量。

  更新完成后，可使用 `kubectl get pods` 验证所有 Pod 是否都在运行预期模板。

## Grafana 监控

以下监控图以 10,000 个连接为例，展示更新过程中的连接数变化。

<svg viewBox="0 0 920 360" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Replicant 滚动更新期间的连接数">
  <rect width="920" height="360" fill="#111827"/>
  <rect x="58" y="34" width="680" height="266" fill="#121a24" stroke="#263241"/>
  <g stroke="#263241" stroke-width="1">
    <path d="M58 60H738M58 108H738M58 156H738M58 204H738M58 252H738M58 300H738"/>
    <path d="M80 34V300M160 34V300M260 34V300M360 34V300M460 34V300M560 34V300M660 34V300M738"/>
  </g>
  <g fill="#9ca3af" font-family="sans-serif" font-size="12">
    <text x="25" y="304">0</text>
    <text x="20" y="256">2K</text>
    <text x="20" y="208">4K</text>
    <text x="20" y="160">6K</text>
    <text x="20" y="112">8K</text>
    <text x="14" y="64">10K</text>
    <text x="62" y="322">14:08</text>
    <text x="242" y="322">14:11</text>
    <text x="442" y="322">14:14</text>
    <text x="642" y="322">14:17</text>
  </g>
  <g fill="none" stroke-linecap="round" stroke-linejoin="round">
    <path d="M80 300 C105 300 135 180 160 62 C210 58 270 61 330 60 C390 61 445 59 505 60 C565 62 630 59 700 60" stroke="#73bf69" stroke-width="2"/>
    <path d="M160 217 L300 216 C330 218 355 254 382 300" stroke="#e24d42" stroke-width="1.2"/>
    <path d="M160 221 L400 222 C430 222 455 257 482 300" stroke="#8f7ee7" stroke-width="1.2"/>
    <path d="M160 225 L500 224 C530 225 565 259 600 300" stroke="#5794f2" stroke-width="1.2"/>
    <path d="M280 300 C315 300 350 246 382 218 L400 218 C430 218 455 205 482 200 L500 200 C535 200 570 190 600 180 L700 180" stroke="#f2cc0c" stroke-width="1.2"/>
    <path d="M380 300 C415 300 450 260 482 248 L500 248 C535 248 570 225 600 221 L700 221" stroke="#ff9830" stroke-width="1.2"/>
    <path d="M500 300 C535 300 570 283 600 264 L700 264" stroke="#56a64b" stroke-width="1.2"/>
  </g>
  <g font-family="sans-serif" font-size="9">
    <rect x="758" y="46" width="144" height="166" rx="4" fill="#101923" stroke="#263241"/>
    <circle cx="768" cy="66" r="3" fill="#73bf69"/><text x="776" y="69" fill="#d1d5db">总计 10K</text>
    <circle cx="768" cy="88" r="3" fill="#e24d42"/><text x="776" y="91" fill="#d1d5db">emqx-replicant-86f864f9-0</text>
    <circle cx="768" cy="110" r="3" fill="#8f7ee7"/><text x="776" y="113" fill="#d1d5db">emqx-replicant-86f864f9-1</text>
    <circle cx="768" cy="132" r="3" fill="#5794f2"/><text x="776" y="135" fill="#d1d5db">emqx-replicant-86f864f9-2</text>
    <circle cx="768" cy="154" r="3" fill="#f2cc0c"/><text x="776" y="157" fill="#d1d5db">emqx-replicant-648c45c7-0</text>
    <circle cx="768" cy="176" r="3" fill="#ff9830"/><text x="776" y="179" fill="#d1d5db">emqx-replicant-648c45c7-1</text>
    <circle cx="768" cy="198" r="3" fill="#56a64b"/><text x="776" y="201" fill="#d1d5db">emqx-replicant-648c45c7-2</text>
  </g>
  <g fill="#d1d5db" font-family="sans-serif">
    <text x="58" y="24" font-size="14">Replicant 滚动更新，maxSurge = 1，maxUnavailable = 1</text>
  </g>
</svg>

| 标签/前缀                   | 说明                                           |
|-----------------------------|------------------------------------------------|
| 总计                        | 连接总数，在图中显示为最上方的曲线。           |
| `emqx-replicant-86f864f9`   | 旧 Replicant Pod 集合的名称前缀。              |
| `emqx-replicant-648c45c7`   | 更新后 Replicant Pod 集合的名称前缀。          |

以上时间线展示了 EMQX Operator 如何平稳地执行滚动更新。在整个过程中，连接总数保持稳定，但实际情况会受到迁移速率、服务器容量和客户端重连策略等因素的影响。这种方式可减少服务中断、防止服务器过载，并提升整体服务稳定性。
