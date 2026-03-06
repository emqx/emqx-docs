# 修改 EMQX 日志等级

## 目标

修改 EMQX 集群中的日志等级。

## 配置 EMQX 集群

EMQX CRD `apps.emqx.io/v2` 支持通过 `.spec.config.data` 配置 EMQX 集群的日志等级。有关完整的配置参考，请参阅[配置手册](https://docs.emqx.com/zh/enterprise/v6.0.0/hocon/)。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署：

   ```yaml
   apiVersion: apps.emqx.io/v2
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       # 启用 debug 日志记录：
       data: |
         log.console.level = debug
         license {
           key = "..."
         }
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
   ```

2. 等待 EMQX 集群就绪。使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

   ```bash
   $ kubectl get emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 验证日志等级

1. 获取 EMQX 集群的外部 IP。

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
   ```

2. 使用 MQTTX CLI 连接到 EMQX 集群。

    [MQTTX CLI](https://mqttx.app/zh/cli) 是一款开源的 MQTT 5.0 命令行客户端工具，旨在帮助开发者更快地开始使用 MQTT 服务和应用。

   ```bash
   $ mqttx conn -h ${external_ip} -p 1883
   [4/17/2023] [5:17:31 PM] › … Connecting...
   [4/17/2023] [5:17:31 PM] › ✔ Connected
   ```

3. 查看 EMQX 容器日志。

   ```bash
   $ kubectl logs emqx-core-0 -c emqx
   ...
   2023-04-17T09:11:35.993031+00:00 [debug] msg: mqtt_packet_received, mfa: emqx_channel:handle_in/2, line: 360, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNECT(Q0, R0, D0, ClientId=mqttx_322680d9, ProtoName=MQTT, ProtoVsn=5, CleanStart=true, KeepAlive=30, Username=undefined, Password=), tag: MQTT
   2023-04-17T09:11:35.997066+00:00 [debug] msg: mqtt_packet_sent, mfa: emqx_connection:serialize_and_inc_stats_fun/1, line: 872, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0), tag: MQTT
   ```
