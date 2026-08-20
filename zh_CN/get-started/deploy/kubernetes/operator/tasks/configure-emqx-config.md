# 修改 EMQX 配置

## 目标

使用 EMQX 自定义资源中的 `.spec.config.data` 字段修改 EMQX 配置。

## 配置 EMQX 集群

EMQX CRD `apps.emqx.io/v2beta1` 支持通过 `.spec.config.data` 字段配置 EMQX 集群。有关完整的配置参考，请参阅[配置手册](https://docs.emqx.com/zh/enterprise/v6.0.0/hocon/)。

EMQX 使用 [HOCON](../../../../../guides/configuration/configuration.md#hocon-配置格式) 作为配置文件格式。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署：

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx:@EE_VERSION@
      imagePullPolicy: IfNotPresent
      config:
         # 配置一个名为 `test` 的 TCP 监听器，监听端口 1884：
         data: |
            listeners.tcp.test {
               bind = "0.0.0.0:1884"
               max_connections = 1024000
            }
            license {
              key = "..."
            }
      listenersServiceTemplate:
         spec:
            type: LoadBalancer
      dashboardServiceTemplate:
         spec:
            type: LoadBalancer
   ```

   ::: tip
   `.spec.config.data` 字段的内容作为 [`emqx.conf` 配置文件](../../../../../guides/configuration/configuration.md#不可变配置文件)提供给 EMQX 容器。
   :::

2. 等待 EMQX 集群就绪。

   使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 验证配置

查看 EMQX 监听器的状态。

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

这里我们可以看到端口 1884 上的新监听器正在运行。
