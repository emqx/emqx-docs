# LoadBalancer経由でEMQXクラスターにアクセスする

## 目的

LoadBalancerタイプのServiceを介してEMQXクラスターにアクセスします。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2` は以下をサポートしています：
* `.spec.dashboardServiceTemplate`を通じてEMQXダッシュボードのServiceを設定可能。
* `.spec.listenersServiceTemplate`を通じてEMQXクラスターのリスナーServiceを設定可能。

詳細は[該当ドキュメント](../reference/v2-reference.md#emqxspec)を参照してください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       data: |
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

   デフォルトで、EMQXはポート1883でMQTT TCPリスナー `tcp-default` を、ポート18083でダッシュボードHTTPリスナーを起動します。

   ユーザーは`.spec.config.data`を通じて新規または既存のリスナーを設定したり、EMQXダッシュボードから管理したりできます。

   EMQX Operatorはデフォルトのリスナー情報を自動的にServiceリソースに反映します。ユーザーが設定したServiceとEMQXが設定したリスナーで名前やポートが重複する場合、EMQX Operatorはユーザー設定を優先します。

   :::

2. EMQXクラスターがReady状態になるまで待ちます。

   `kubectl get`コマンドでEMQXクラスターの状態を確認し、`STATUS`が`Ready`になっていることを確認してください。準備完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## EMQXダッシュボードから新しいリスナーを追加する

1. 新しいリスナーを追加します。

   - EMQXダッシュボードを開き、**Management** -> **Listeners** に移動します。

   - **Add Listener**をクリックし、名前を`test`、ポートを`1884`として新しいリスナーを追加します。以下の図のように操作します：

     ![emqx-add-listener](./assets/configure-service/emqx-add-listener.png)

   - **Add**をクリックしてリスナーを作成します。以下の図のように新しいリスナーが作成されます。

     ![emqx-listeners](./assets/configure-service/emqx-listeners.png)

2. 新しいリスナーがServiceに反映されているか確認します。

   ```bash
   kubectl get svc
   
   NAME             TYPE       CLUSTER-IP       EXTERNAL-IP   PORT(S)                                         AGE
   emqx-dashboard   NodePort   10.105.110.235   <none>        18083:32012/TCP                                 13m
   emqx-listeners   NodePort   10.106.1.58      <none>        1883:32010/TCP,1884:30763/TCP                   12m
   ```

   この出力は、ポート1884の新しいリスナーが`emqx-listeners` Serviceリソースに反映されていることを示しています。

## MQTTXを使って新しいリスナーに接続する

1. EMQXリスナーServiceの外部IPを取得します。

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. MQTTX CLIを使って新しいリスナーに接続します。

   ```bash
   $ mqttx conn -h ${external_ip} -p 1884
   
   [4/17/2023] [5:17:31 PM] › … Connecting...
   [4/17/2023] [5:17:31 PM] › ✔ Connected
   ```
