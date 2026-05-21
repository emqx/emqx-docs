# EMQXのログレベル変更

## 対象タスク

EMQXクラスターのログレベルを変更します。

## EMQXクラスターの設定

<<<<<<< HEAD
以下はEMQXカスタムリソースの関連設定です。デプロイしたいEMQXのバージョンに応じて対応するAPIVersionを選択してください。具体的な互換性については[EMQX Operator Compatibility](../operator.md)を参照してください。

`apps.emqx.io/v2beta1 EMQX`では、`.spec.config.data`を通じてEMQXクラスターのログレベルを設定できます。`config.data`の設定方法は[Configuration Manual](https://www.emqx.io/docs/en/v5.1/configuration/configuration-manual.html#configuration-manual)を参照してください。

> このフィールドはEMQXクラスター作成時のみ設定可能で、更新はサポートされていません。作成後にクラスターのログレベルを変更する場合は、EMQXダッシュボードから変更してください。

+ 以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。
=======
以下はEMQXカスタムリソースの関連設定です。デプロイしたいEMQXのバージョンに応じて対応するAPIVersionを選択してください。具体的な対応関係については[EMQX Operator Compatibility](../operator.md)をご参照ください。

`apps.emqx.io/v2beta1 EMQX`では、`.spec.config.data`を通じてEMQXクラスターのログレベルを設定できます。`config.data`の設定方法は[Configuration Manual](https://www.emqx.io/docs/en/v5.1/configuration/configuration-manual.html#configuration-manual)を参照してください。

> このフィールドはEMQXクラスター作成時のみ設定可能であり、更新はサポートしていません。作成後にクラスターのログレベルを変更する場合は、EMQXダッシュボードから変更してください。

+ 以下の内容をYAMLファイルとして保存し、kubectl applyコマンドでデプロイします。
>>>>>>> origin/release-5.9

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx-enterprise:@EE_VERSION@
    config:
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

  > `.spec.config.data`フィールドでEMQXクラスターのログレベルを`debug`に設定しています。

<<<<<<< HEAD
+ EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでクラスターの状態を確認し、`STATUS`が`Running`であることを確認してください。完了までに時間がかかる場合があります。
=======
+ EMQXクラスターが準備完了になるまで待ちます。kubectl getコマンドでEMQXクラスターの状態を確認し、`STATUS`がRunningであることを確認してください。準備完了までに時間がかかる場合があります。
>>>>>>> origin/release-5.9

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

<<<<<<< HEAD
+ EMQX Operatorは2つのEMQX Serviceリソースを作成します。1つは`emqx-dashboard`、もう1つは`emqx-listeners`で、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。
=======
+ EMQX Operatorは2つのEMQX Serviceリソースを作成します。1つはemqx-dashboard、もう1つはemqx-listenersで、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。
>>>>>>> origin/release-5.9

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

<<<<<<< HEAD
  ブラウザで`http://192.168.1.200:18083`にアクセスし、デフォルトのユーザー名とパスワード`admin/public`でEMQXコンソールにログインします。

## ログレベルの確認

[MQTTX CLI](https://mqttx.app/cli)はオープンソースのMQTT 5.0コマンドラインクライアントツールで、開発者がMQTTサーバーやアプリケーションの開発・デバッグを迅速に行うために設計されています。

+ EMQXクラスターの外部IPを取得します。
=======
  ブラウザで`http://192.168.1.200:18083`にアクセスし、デフォルトのユーザー名とパスワード`admin/public`でEMQXコンソールにログインしてください。

## ログレベルの確認

[MQTTX CLI](https://mqttx.app/cli)はオープンソースのMQTT 5.0コマンドラインクライアントツールで、開発者がMQTTサーバーやアプリケーションの開発・デバッグをより迅速に行うために設計されています。

+ EMQXクラスターのExternal IPを取得します。
>>>>>>> origin/release-5.9

  ```bash
  external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
  ```

+ MQTTX CLIを使ってEMQXクラスターに接続します。

  ```bash
  $ mqttx conn -h ${external_ip} -p 1883

  [4/17/2023] [5:17:31 PM] › … Connecting...
  [4/17/2023] [5:17:31 PM] › ✔ Connected
  ```

+ コマンドラインでEMQXクラスターのログ情報を確認します。

  ```bash
  $ kubectl logs emqx-core-0 -c emqx
  ```

<<<<<<< HEAD
  以下のようなログが出力されれば、EMQXがクライアントからのCONNECTメッセージを受信し、CONNACKメッセージをクライアントに返信していることを示します。
=======
  以下のようなログが取得できれば、クライアントからCONNECTメッセージを受信し、CONNACKメッセージを返したことを意味します。
>>>>>>> origin/release-5.9

  ```bash
  2023-04-17T09:11:35.993031+00:00 [debug] msg: mqtt_packet_received, mfa: emqx_channel:handle_in/2, line: 360, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNECT(Q0, R0, D0, ClientId=mqttx_322680d9, ProtoName=MQTT, ProtoVsn=5, CleanStart=true, KeepAlive=30, Username=undefined, Password=), tag: MQTT
  2023-04-17T09:11:35.997066+00:00 [debug] msg: mqtt_packet_sent, mfa: emqx_connection:serialize_and_inc_stats_fun/1, line: 872, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0), tag: MQTT
  ```
