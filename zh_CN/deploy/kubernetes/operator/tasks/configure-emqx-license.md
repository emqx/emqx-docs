# 管理 License

## 任务目标

- 配置 EMQX 企业版 License。
- 更新 EMQX 企业版 License。

## 配置 License

EMQX 企业版 License 可以在 EMQ 官网免费申请：[申请 EMQX 企业版 License](https://www.emqx.com/zh/apply-licenses/emqx)。

## 配置 EMQX 集群

`apps.emqx.io/v2beta1 EMQX` 支持通过 `.spec.config.data` 配置 EMQX 集群 License，EMQX 配置可以参考文档：[配置手册](../../../../configuration/configuration.md)。

> 在创建 EMQX 集群之后，如果需要更新 License，请通过 EMQX Dashboard 进行更新。

+ 将下面的内容保存成 YAML 文件，并通过 `kubectl apply` 命令部署它

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    config:
      data: |
        license {
          key = "${your_license_key}"
        }
    image: emqx/emqx-enterprise:@EE_VERSION@
    listenersServiceTemplate:
      spec:
        type: LoadBalancer
    dashboardServiceTemplate:
      spec:
        type: LoadBalancer
  ```

  > `config.data` 字段里面的 `license.key` 表示 Licesne 内容，此例中 License 内容被省略，请用户自行填充。

+ 等待 EMQX 集群就绪，可以通过 `kubectl get` 命令查看 EMQX 集群的状态，请确保 `STATUS` 为 `Running`，这个可能需要一些时间

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ 获取 EMQX 集群的 Dashboard External IP，访问 EMQX 控制台

  EMQX Operator 会创建两个 EMQX Service 资源，一个是 emqx-dashboard，一个是 emqx-listeners，分别对应 EMQX 控制台和 EMQX 监听端口。

  ```bash
  $ kubectl get svc emqx-ee-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'
  
  192.168.1.200
  ```

  通过浏览器访问 `http://192.168.1.200:18083` ，使用默认的用户名和密码 `admin/public` 登录 EMQX 控制台。

## 更新 License

+ 查看 License 信息

  ```bash
  $ pod_name="$(kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/db-role=core' -o json | jq --raw-output '.items[0].metadata.name')"
  $ kubectl exec -it ${pod_name} -c emqx -- emqx_ctl license info
  ```

  可以获取到如下输出，从输出结果可以看到我们申请的 License 的基本信息，包括申请人的信息和 License 支持最大连接数以及 License 过期时间等。
  ```bash
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

+ 修改 EMQX 自定义资源以更新 License
  ```bash
  $ kubectl edit emqx emqx
  ...
  spec:
    image: emqx/emqx-enterprise:@EE_VERSION@
    config:
      data: |
        license {
          key = "${new_license_key}"
        }
  ...
  ```

  + 查看 EMQX 集群 License 是否被更新

  ```bash
  $ pod_name="$(kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/db-role=core' -o json | jq --raw-output '.items[0].metadata.name')"
  $ kubectl exec -it ${pod_name} -c emqx -- emqx_ctl license info
  ```
  可以获取到类似如下的信息，从获取到 `max_connections` 字段可以看出 License 的内容已经更新，则说明 EMQX 企业版 License 更新成功。若证书信息没有更新，可以等待一会，License 的更新会有些时延。

  ```bash
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
