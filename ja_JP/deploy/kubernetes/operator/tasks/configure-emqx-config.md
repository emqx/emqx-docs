# EMQX設定の変更

## 目的

EMQXカスタムリソースの`.spec.config.data`フィールドを使用して、EMQXの設定を変更します。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2beta1`は、`.spec.config.data`フィールドを通じてEMQXクラスターの設定をサポートしています。完全な設定リファレンスについては、[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)を参照してください。

EMQXは設定ファイル形式として[HOCON](../../../../configuration/configuration.md#hocon-configuration-format)を使用しています。

1. 以下をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx:@EE_VERSION@
      imagePullPolicy: IfNotPresent
      config:
         # ポート1884で待ち受けるTCPリスナー `test` を設定：
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
   `.spec.config.data`フィールドの内容は、EMQXコンテナに対して[`emqx.conf`設定ファイル](../../../../configuration/configuration.md#immutable-configuration-file)として提供されます。
   :::

2. EMQXクラスターが準備完了になるまで待ちます。`kubectl get`でEMQXクラスターのステータスを確認し、`STATUS`が`Ready`になっていることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 設定の確認

EMQXリスナーのステータスを確認します。

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

ここで、ポート1884で新しいリスナーが稼働していることが確認できます。
