# EMQXのログレベル変更

## 対象タスク

EMQXクラスターのログレベルを変更します。

## EMQXクラスターの設定

以下はEMQXカスタムリソースの関連設定です。デプロイしたいEMQXのバージョンに応じて対応するAPIVersionを選択してください。具体的な対応関係については[EMQX Operator Compatibility](../operator.md)をご参照ください。

`apps.emqx.io/v2beta1 EMQX`では、`.spec.config.data`を通じてEMQXクラスターのログレベルを設定できます。`config.data`の設定方法は[Configuration Manual](https://www.emqx.io/docs/en/v5.1/configuration/configuration-manual.html#configuration-manual)を参照してください。

> このフィールドはEMQXクラスター作成時のみ設定可能であり、更新はサポートしていません。作成後にクラスターのログレベルを変更する場合は、EMQXダッシュボードから変更してください。

+ 以下の内容をYAMLファイルとして保存し、kubectl applyコマンドでデプロイします。

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

+ EMQXクラスターが準備完了になるまで待ちます。kubectl getコマンドでEMQXクラスターの状態を確認し、`STATUS`がRunningであることを確認してください。準備完了までに時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ EMQX Operatorは2つのEMQX Serviceリソースを作成します。1つはemqx-dashboard、もう1つはemqx-listenersで、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

  ブラウザで`http://192.168.1.200:18083`にアクセスし、デフォルトのユーザー名とパスワード`admin/public`でEMQXコンソールにログインしてください。

## ログレベルの確認

[MQTTX CLI](https://mqttx.app/cli)はオープンソースのMQTT 5.0コマンドラインクライアントツールで、開発者がMQTTサーバーやアプリケーションの開発・デバッグをより迅速に行うために設計されています。

+ EMQXクラスターのExternal IPを取得します。

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

  以下のようなログが取得できれば、クライアントからCONNECTメッセージを受信し、CONNACKメッセージを返したことを意味します。

  ```bash
  2023-04-17T09:11:35.993031+00:00 [debug] msg: mqtt_packet_received, mfa: emqx_channel:handle_in/2, line: 360, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNECT(Q0, R0, D0, ClientId=mqttx_322680d9, ProtoName=MQTT, ProtoVsn=5, CleanStart=true, KeepAlive=30, Username=undefined, Password=), tag: MQTT
  2023-04-17T09:11:35.997066+00:00 [debug] msg: mqtt_packet_sent, mfa: emqx_connection:serialize_and_inc_stats_fun/1, line: 872, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0), tag: MQTT
  ```
