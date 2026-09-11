# 管理 License

## 目标

- 配置 EMQX Enterprise License。
- 更新 EMQX Enterprise License。

## 配置 License

可以在 EMQX 官网免费申请 EMQX Enterprise License：[申请 EMQX Enterprise License](https://www.emqx.com/zh/apply-licenses/emqx)。

## 配置 EMQX 集群

`apps.emqx.io/v3beta1` EMQX CRD 支持通过 `.spec.config.roots.license` 配置 EMQX 集群的 License。有关完整的配置参考，请参阅[配置手册](https://docs.emqx.com/zh/enterprise/v6.2.0/hocon/)。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     config:
       roots:
         license:
           key: "..."
     image: emqx/emqx:@EE_VERSION@
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip

   `.spec.config.roots.license.key` 字段用于设置 License 密钥。请将本示例中的占位符替换为实际的 License 密钥。

   :::

2. 等待 EMQX 集群就绪。

   使用 `kubectl get` 检查 EMQX 集群状态，并确保 `STATUS` 为 `Ready`。此过程可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 更新 License

1. 查看 License 信息。

   ```bash
   $ kubectl exec -it service/emqx-headless -c emqx -- emqx ctl license info
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

   输出显示 License 的基本信息，包括申请人信息、License 支持的最大连接数和到期时间。

2. 修改 EMQX CR 以更新 License。

   ```bash
   $ kubectl edit emqx emqx
   ...
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "${new_license_key}"
   ...
   ```

3. 验证 License 是否已更新。

   ```bash
   $ kubectl exec -it service/emqx-headless -c emqx -- emqx ctl license info
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

   更新后的 `max_connections` 字段表明 EMQX Enterprise License 已成功更新。License 更新可能需要一些时间，因此可能需要重试该命令。
