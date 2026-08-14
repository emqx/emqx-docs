# 在 EMQX 中开启 TLS

## 目标

使用 `extraVolumes` 和 `extraVolumeMounts` 字段自定义 TLS 证书。

## 基于 TLS 证书创建 Secret

Secret 是一种包含少量敏感信息的对象，例如密码、令牌或密钥。在本演示中，我们使用 Secret 存储 TLS 证书信息，因此在创建 EMQX 集群之前需要创建一个 Secret。

有关更多信息，请参阅 [Secret](https://kubernetes.io/zh-cn/docs/concepts/configuration/secret/#working-with-secrets) 文档。

将以下内容保存为 YAML 文件，并使用 `kubectl apply` 命令部署：

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: emqx-tls
type: kubernetes.io/tls
stringData:
  ca.crt: |
    -----BEGIN CERTIFICATE-----
    ...
    -----END CERTIFICATE-----
  tls.crt: |
    -----BEGIN CERTIFICATE-----
    ...
    -----END CERTIFICATE-----
  tls.key: |
    -----BEGIN RSA PRIVATE KEY-----
    ...
    -----END RSA PRIVATE KEY-----
```

:::tip
在此示例中，上述三个字段的内容被省略。请用您自己的证书内容填充。
* `ca.crt` 应包含 CA 证书。
* `tls.crt` 应包含服务器证书。
* `tls.key` 应包含服务器的私钥。
:::

## 配置 EMQX 集群

EMQX CRD `apps.emqx.io/v2beta1` 提供以下字段来为 EMQX 集群配置额外的卷和挂载点：
* `.spec.coreTemplate.extraVolumes`
* `.spec.coreTemplate.extraVolumeMounts`
* `.spec.replicantTemplate.extraVolumes`
* `.spec.replicantTemplate.extraVolumeMounts`

在本演示中，我们将使用这些字段为 EMQX 集群提供 TLS 证书。

Volumes 的类型有很多种。有关 Volumes 的信息，请参阅 [Volumes](https://kubernetes.io/zh-cn/docs/concepts/storage/volumes/#secret) 文档。这里我们使用的是 `secret` 卷类型。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署：

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx:@EE_VERSION@
    config:
      # 配置从 `emqx-tls` 卷挂载的 TLS 监听器证书：
      data: |
        listeners.ssl.default {
          bind = "0.0.0.0:8883"
          ssl_options {
            cacertfile = "/mounted/cert/ca.crt"
            certfile = "/mounted/cert/tls.crt"
            keyfile = "/mounted/cert/tls.key"
            gc_after_handshake = true
            handshake_timeout = 5s
          }
        }
        license {
          key = "..."
        }
    coreTemplate:
      spec:
        extraVolumes:
          - name: emqx-tls
            secret:
              secretName: emqx-tls
        extraVolumeMounts:
          - name: emqx-tls
            mountPath: /mounted/cert
    replicantTemplate:
      spec:
        extraVolumes:
          # 创建一个名为 `emqx-tls` 的 `secret` 卷类型：
          - name: emqx-tls
            secret:
              secretName: emqx-tls
        extraVolumeMounts:
          - name: emqx-tls
            # TLS 证书挂载到 EMQX 节点的目录：
            mountPath: /mounted/cert
    dashboardServiceTemplate:
      spec:
        type: LoadBalancer
    listenersServiceTemplate:
      spec:
        type: LoadBalancer
  ```

2. 等待 EMQX 集群就绪。

  使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

  ```bash
  $ kubectl get emqx
  NAME   STATUS   AGE
  emqx   Ready    10m
  ```

## 使用 MQTTX 验证 TLS 连接

[MQTTX CLI](https://mqttx.app/zh/cli) 是一款开源的 MQTT 5.0 命令行客户端工具，旨在帮助开发者快速开始使用 MQTT 服务和应用。

1. 获取 EMQX 监听器服务的外部 IP。

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
   ```

2. 使用 MQTTX CLI 订阅消息。连接到 TLS 监听器端口 8883，使用 `--insecure` 标志跳过证书验证。

   ```bash
   mqttx sub -h ${external_ip} -p 8883 -t "hello" -l mqtts --insecure
   [10:00:25] › … Connecting...
   [10:00:25] › ✔ Connected
   [10:00:25] › … Subscribing to hello...
   [10:00:25] › ✔ Subscribed to hello
   ```

3. 在单独的终端窗口中发布消息。

   ```bash
   mqttx pub -h ${external_ip} -p 8883 -t "hello" -m "hello world" -l mqtts --insecure
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```

4. 观察订阅客户端接收消息。

   ```bash
   [10:00:58] › payload: hello world
   ```

   这表明发布者和订阅者客户端都通过 TLS 连接成功与代理通信。
