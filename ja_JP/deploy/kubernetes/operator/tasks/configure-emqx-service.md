<<<<<<< HEAD
# LoadBalancer経由でEMQXクラスターにアクセスする

## 目的
=======
# LoadBalancerを介したEMQXクラスターへのアクセス

## タスク対象
>>>>>>> origin/release-5.10

LoadBalancerタイプのServiceを介してEMQXクラスターにアクセスします。

## EMQXクラスターの設定

<<<<<<< HEAD
EMQX CRD `apps.emqx.io/v2beta1` は以下をサポートしています：
* `.spec.dashboardServiceTemplate` を通じてEMQXダッシュボードのServiceを設定すること。
* `.spec.listenersServiceTemplate` を通じてEMQXクラスターのリスナーServiceを設定すること。

詳細は[該当ドキュメント](../reference/v2beta1-reference.md#emqxspec)を参照してください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。
=======
以下はEMQXカスタムリソースの関連設定例です。デプロイしたいEMQXのバージョンに応じて対応するAPIVersionを選択してください。具体的な対応関係は[EMQX Operator Compatibility](../operator.md)を参照してください。

Operatorは`.spec.dashboardServiceTemplate`でEMQXクラスターのダッシュボードServiceを、`.spec.listenersServiceTemplate`でEMQXクラスターのリスナーServiceを設定可能です。詳細は[Service](../api-reference.md#emqxspec)を参照してください。

+ 以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。
>>>>>>> origin/release-5.10

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
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

<<<<<<< HEAD
   ::: tip

   デフォルトでは、EMQXはポート1883でMQTT TCPリスナー `tcp-default` を起動し、ポート18083でダッシュボードHTTPリスナーを起動します。

   ユーザーは `.spec.config.data` を通じて新規または既存のリスナーを設定するか、EMQXダッシュボードで管理できます。

   EMQXオペレーターはデフォルトリスナー情報をServiceリソースに自動反映します。ユーザーが設定したServiceとEMQXが設定したリスナーで名前やポートが重複する場合、EMQXオペレーターはユーザー設定を優先します。

   :::
=======
  > デフォルトで、EMQXはポート1883に対応するMQTT TCPリスナー `tcp-default` と、ポート18083に対応するダッシュボードリスナー `dashboard-listeners-http-bind` を開きます。

  > ユーザーは`.spec.config.data`フィールドまたはEMQXダッシュボードを通じて新しいリスナーを追加できます。EMQX OperatorはService作成時にデフォルトのリスナー情報を自動的にServiceに注入しますが、ユーザーが設定したServiceとEMQXが設定したリスナーに名前やポートの重複がある場合は、ユーザーの設定が優先されます。

+ EMQXクラスターが準備完了になるまで待機します。`kubectl get`コマンドでEMQXクラスターの状態を確認し、`STATUS`が`Running`であることを確認してください。完了までに時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```
+ EMQXクラスターのダッシュボード外部IPを取得し、EMQXコンソールにアクセスします。

  EMQX OperatorはEMQXコンソール用の`emqx-dashboard`と、EMQXリスニングポート用の`emqx-listeners`の2つのEMQX Serviceリソースを作成します。
>>>>>>> origin/release-5.10

2. EMQXクラスターがReady状態になるまで待ちます。

   `kubectl get` コマンドでEMQXクラスターのステータスを確認し、`STATUS`が`Ready`になっていることを確認してください。完了までに時間がかかる場合があります。

<<<<<<< HEAD
   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## EMQXダッシュボードから新しいリスナーを追加する

1. 新しいリスナーを追加します。
=======
  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` でEMQXコンソールにログインします。

## MQTTX CLIによるEMQXクラスターへの接続

+ EMQXクラスターの外部IPを取得します。
>>>>>>> origin/release-5.10

   - EMQXダッシュボードを開き、**Management** -> **Listeners** に移動します。

<<<<<<< HEAD
   - **Add Listener** をクリックし、名前を `test`、ポートを `1884` に設定して新しいリスナーを追加します。以下の図を参照してください：
=======
+ MQTTX CLIを使ってEMQXクラスターに接続します。
>>>>>>> origin/release-5.10

     ![emqx-add-listener](./assets/configure-service/emqx-add-listener.png)

   - **Add** をクリックしてリスナーを作成します。以下の図のように、新しいリスナーが作成されます。

<<<<<<< HEAD
     ![emqx-listeners](./assets/configure-service/emqx-listeners.png)

2. 新しいリスナーがServiceに反映されているか確認します。

   ```bash
   kubectl get svc
   
   NAME             TYPE       CLUSTER-IP       EXTERNAL-IP   PORT(S)                                         AGE
   emqx-dashboard   NodePort   10.105.110.235   <none>        18083:32012/TCP                                 13m
   emqx-listeners   NodePort   10.106.1.58      <none>        1883:32010/TCP,1884:30763/TCP                   12m
   ```

   この出力は、ポート1884の新しいリスナーが `emqx-listeners` Serviceリソースに反映されていることを示しています。

## MQTTXを使って新しいリスナーに接続する

1. EMQXリスナーServiceの外部IPを取得します。

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```
=======
## EMQXダッシュボードを通じた新規リスナーの追加

+ 新しいリスナーの追加

  ブラウザでEMQXダッシュボードにログインし、メニューの「Configuration」→「Listeners」をクリックしてリスナーページに入ります。まず「Add Listener」ボタンをクリックし、名前を`test`、ポートを1884に設定したリスナーを追加します。以下の図のように操作してください。

  <div style="text-align:center">
  <img src="./assets/configure-service/emqx-add-listener.png" style="zoom: 50%;" alt="EMQXでのリスナー追加画面" />
  </div>

  次に「Add」ボタンをクリックしてリスナーを作成します。以下の図のように表示されます。

  <img src="./assets/configure-service/emqx-listeners.png" style="zoom:50%;" alt="EMQXで追加されたリスナーの一覧" />

  図からわかるように、作成した`test`リスナーが有効になっています。

+ 新規追加したリスナーがServiceに注入されているか確認します。
>>>>>>> origin/release-5.10

2. MQTTX CLIで新しいリスナーに接続します。

<<<<<<< HEAD
   ```bash
   $ mqttx conn -h ${external_ip} -p 1884
   
   [4/17/2023] [5:17:31 PM] › … Connecting...
   [4/17/2023] [5:17:31 PM] › ✔ Connected
   ```
=======
  NAME             TYPE       CLUSTER-IP       EXTERNAL-IP   PORT(S)                                         AGE
  emqx-dashboard   NodePort   10.105.110.235   <none>        18083:32012/TCP                                 13m
  emqx-listeners   NodePort   10.106.1.58      <none>        1883:32010/TCP,1884:30763/TCP                   12m
  ```

  出力結果から、新たに追加したポート1884のリスナーが`emqx-listeners` Serviceに注入されていることが確認できます。
>>>>>>> origin/release-5.10
