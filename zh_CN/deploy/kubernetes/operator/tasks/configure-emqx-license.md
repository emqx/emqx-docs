# 管理 License

## 目标

- 配置 EMQX 企业版 License。
- 更新 EMQX 企业版 License。

## 配置 License

您可以在 EMQX 官网免费申请 EMQX 企业版 License：[申请 EMQX 企业版 License](https://www.emqx.com/zh/apply-licenses/emqx)。

## 配置 EMQX 集群

EMQX CRD `apps.emqx.io/v2beta1` 支持通过 `.spec.config.data` 字段配置 EMQX 集群 License。有关完整的配置参考，请参阅[配置手册](https://docs.emqx.com/zh/enterprise/v6.0.0/hocon/)。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx-ee
  spec:
    config:
      data: |
        license {
          key = "..."
        }
    image: emqx/emqx:@EE_VERSION@
    dashboardServiceTemplate:
      spec:
        type: LoadBalancer
  ```

  ::: tip
  `.spec.config.data` 字段中的 `license.key` 表示 License 内容。在此示例中，License 内容被省略。请用您自己的 License 密钥填充。
  :::

2. 等待 EMQX 集群就绪。

  使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

  ```bash
  $ kubectl get emqx emqx-ee
  NAME   STATUS   AGE
  emqx   Ready    10m
  ```

## 更新 License

1. 查看 License 信息。

  ```bash
  $ kubectl exec -it service/emqx-ee-headless -c emqx -- emqx ctl license info
  customer        : Evaluation
  email           : contact@emqx.io
  deployment      : default
  max_connections : 100
  start_at        : 2023-01-09
  expiry_at       : 2028-01-08
  type            : trial
  customer_type   : 10
  expiry          : false
  ```

  输出显示基本的 License 信息，包括申请人的信息、License 支持的最大连接数和过期时间。

2. 修改 EMQX CR 以更新 License。

  ```bash
  $ kubectl edit emqx emqx-ee
  ...
  spec:
    image: emqx/emqx:@EE_VERSION@
    config:
      data: |
        license {
          key = "${new_license_key}"
        }
  ...
  ```

3. 验证 License 是否已更新。

  ```bash
  $ kubectl exec -it service/emqx-ee-headless -c emqx -- emqx ctl license info
  customer        : Evaluation
  email           : contact@emqx.io
  deployment      : default
  max_connections : 100000
  start_at        : 2023-01-09
  expiry_at       : 2028-01-08
  type            : trial
  customer_type   : 10
  expiry          : false
  ```

  更新的 `max_connections` 字段清楚地表明 EMQX 企业版 License 已成功更新。请注意，License 更新可能需要一些时间，因此您可能需要重试该命令。
