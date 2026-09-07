# 修改 EMQX 配置

## 目标

通过 EMQX 自定义资源中的 `.spec.config.roots` 修改 EMQX 配置。

## 配置 EMQX 集群

`apps.emqx.io/v3beta1` EMQX CRD 的 `.spec.config.roots` 接受与 JSON 兼容的 EMQX 顶层配置根项。在 YAML 清单中，每个根项应表示为与 [EMQX 配置 Schema](https://docs.emqx.com/zh/enterprise/v6.2.0/hocon/)对应的结构化 YAML 对象、数组或标量。

该字段不接受 include 或替换等仅适用于 HOCON 的结构。

从 `.spec.config.roots` 中删除根项表示 EMQX Operator 不再管理该根项。这一操作不会删除 EMQX 已持久化的值，也不会将该根项恢复为 Schema 默认值。如需将根项重置为已知值，请显式声明这些值。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署：

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     imagePullPolicy: IfNotPresent
     config:
       roots:
         # 配置名为 `test`、端口为 1884 的 TCP 监听器：
         listeners:
           tcp:
             test:
               bind: "0.0.0.0:1884"
               max_connections: 1024000
         license:
           key: "..."
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip
   不要配置 `node.cookie`，该设置由 EMQX Operator 管理。
   :::

   ::: tip
   EMQX Operator 将监听器设置等大多数配置写入 [`base.hocon`](../../../../configuration/configuration.md#基础配置文件)，并通过 EMQX Configs API 在运行时应用变更，无需重启 Pod。Operator 将 Dashboard 监听器和节点设置等仅在 EMQX 启动时生效的配置写入 [`emqx.conf`](../../../../configuration/configuration.md#不可变配置文件)。修改此类设置会触发受控滚动更新。
   :::

2. 等待 EMQX 集群就绪。使用 `kubectl get` 检查 EMQX 集群状态，并确保 `STATUS` 为 `Ready`。此过程可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

3. 检查 `ConfigApplied` 条件，确认目标配置已生效：

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   True    Applied    Desired configuration is active
   ```

## 验证配置

查看 EMQX 监听器状态。

```bash
$ kubectl exec -it emqx-core-0 -c emqx -- emqx ctl listeners
tcp:default
   listen_on: 0.0.0.0:1883
   acceptors: 16
   proxy_protocol : false
   running: true
   current_conn: 0
   max_conns : 1024000
tcp:test
   listen_on: 0.0.0.0:1884
   acceptors: 16
   proxy_protocol : false
   running: true
   current_conn: 0
   max_conns : 1024000
```

输出表明，端口 1884 上的新监听器正在运行。

## 修改需要重启的配置

部分配置变更会更新 Pod 模板并触发滚动更新。以下示例修改仅在 EMQX 启动时生效的 Dashboard HTTP 监听器。

1. 修补 EMQX 资源：

   ```bash
   kubectl patch emqx emqx --type=merge -p '{"spec":{"config":{"roots":{"dashboard":{"listeners":{"http":{"bind":"0.0.0.0:18084"}}}}}}}'
   ```

2. EMQX Operator 检测到变更后，检查 `ConfigApplied` 条件：

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   False    StartupConfigPending    Configuration roots require rolling restart: [dashboard]
   ```

   状态为 `False` 且原因为 `StartupConfigPending`，表示至少有一个就绪 Pod 仍在使用原配置，滚动更新正在进行。

3. 等待滚动更新完成：

   ```bash
   kubectl wait --for=condition=ConfigApplied emqx/emqx --timeout=10m
   ```

4. 检查 `ConfigApplied` 条件，确认新配置已生效：

   ```bash
   $ kubectl get emqx emqx -o jsonpath='{range .status.conditions[?(@.type=="ConfigApplied")]}{.status}{"\t"}{.reason}{"\t"}{.message}{"\n"}{end}'
   True    Applied    Desired configuration is active
   ```
