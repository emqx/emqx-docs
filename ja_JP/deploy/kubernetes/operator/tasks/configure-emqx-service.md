# LoadBalancer経由でEMQXクラスターにアクセスする

## タスク対象

LoadBalancerタイプのServiceを介してEMQXクラスターにアクセスします。

## EMQXクラスターの設定

以下はEMQXカスタムリソースの関連設定例です。デプロイしたいEMQXのバージョンに応じて対応するAPIVersionを選択してください。具体的な対応関係については[EMQX Operator Compatibility](../operator.md)を参照してください。

Operatorは`.spec.dashboardServiceTemplate`でEMQXクラスターのダッシュボードServiceを、`.spec.listenersServiceTemplate`でEMQXクラスターのリスナーServiceを設定可能です。詳細は[Service](../api-reference.md#emqxspec)をご覧ください。

+ 以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx-enterprise:@EE_VERSION@
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

  > デフォルトでは、EMQXはポート1883に対応するMQTT TCPリスナー `tcp-default` と、ポート18083に対応するダッシュボードリスナー `dashboard-listeners-http-bind` を開きます。

  > ユーザーは`.spec.config.data`フィールドやEMQXダッシュボードを通じて新しいリスナーを追加できます。EMQX OperatorはService作成時にデフォルトのリスナー情報を自動的に注入しますが、ユーザーが設定したServiceとEMQXが設定したリスナーの間で名前やポートが重複する場合は、ユーザーの設定が優先されます。

+ EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでクラスターの状態を確認し、`STATUS`が`Running`であることを確認してください。完了までに時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```
+ EMQXクラスターのダッシュボード外部IPを取得し、EMQXコンソールにアクセスします。

  EMQX OperatorはEMQXコンソール用の `emqx-dashboard` と、EMQXリスニングポート用の `emqx-listeners` の2つのEMQX Serviceリソースを作成します。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` でEMQXコンソールにログインします。

## MQTTX CLIでEMQXクラスターに接続する

+ EMQXクラスターの外部IPを取得します。

  ```bash
  external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
  ```

+ MQTTX CLIを使ってEMQXクラスターに接続します。

  ```bash
  $ mqttx conn -h ${external_ip} -p 1883

  [4/17/2023] [5:17:31 PM] › … Connecting...
  [4/17/2023] [5:17:31 PM] › ✔ Connected
  ```

## EMQXダッシュボードから新しいリスナーを追加する

+ 新しいリスナーを追加する

  ブラウザでEMQXダッシュボードにログインし、「Configuration」→「Listeners」をクリックしてリスナーページに入ります。まず「Add Listener」ボタンをクリックし、名前を `test`、ポートを1884に設定したリスナーを追加します。以下の図のように操作します。

  <div style="text-align:center">
  <img src="./assets/configure-service/emqx-add-listener.png" style="zoom: 50%;" />
  </div>

  次に「Add」ボタンをクリックしてリスナーを作成します。以下の図のように表示されます。

  <img src="./assets/configure-service/emqx-listeners.png" style="zoom:50%;" />

  図から、作成した `test` リスナーが有効になっていることが確認できます。

+ 新しく追加したリスナーがServiceに注入されているか確認する

  ```bash
  kubectl get svc

  NAME             TYPE       CLUSTER-IP       EXTERNAL-IP   PORT(S)                                         AGE
  emqx-dashboard   NodePort   10.105.110.235   <none>        18083:32012/TCP                                 13m
  emqx-listeners   NodePort   10.106.1.58      <none>        1883:32010/TCP,1884:30763/TCP                   12m
  ```

  出力結果から、新しく追加した1884ポートのリスナーが `emqx-listeners` Serviceに注入されていることがわかります。
