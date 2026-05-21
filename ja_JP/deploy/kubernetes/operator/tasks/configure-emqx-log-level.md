# EMQXのログレベル変更

<<<<<<< HEAD
## 目的
=======
## 対象タスク
>>>>>>> origin/release-5.10

EMQXクラスターのログレベルを変更します。

## EMQXクラスターの設定

<<<<<<< HEAD
EMQX CRD `apps.emqx.io/v2beta1` は、`.spec.config.data` を通じてEMQXクラスターのログレベルを設定することをサポートしています。完全な設定リファレンスについては、[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)を参照してください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       # デバッグログを有効化：
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

2. EMQXクラスターが準備完了になるまで待ちます。
=======
以下はEMQXカスタムリソースの関連設定です。デプロイしたいEMQXのバージョンに応じて対応するAPIVersionを選択してください。具体的な対応関係については[EMQX Operator Compatibility](../operator.md)をご参照ください。

`apps.emqx.io/v2beta1 EMQX`では、`.spec.config.data`を通じてEMQXクラスターのログレベルを設定できます。`config.data`の設定方法は[Configuration Manual](https://www.emqx.io/docs/en/v5.1/configuration/configuration-manual.html#configuration-manual)を参照してください。

> このフィールドはEMQXクラスター作成時のみ設定可能であり、更新はサポートしていません。作成後にクラスターのログレベルを変更する場合は、EMQXダッシュボードから変更してください。

+ 以下の内容をYAMLファイルとして保存し、kubectl applyコマンドでデプロイします。
>>>>>>> origin/release-5.10

   `kubectl get`コマンドでEMQXクラスターのステータスを確認し、`STATUS`が`Ready`になっていることを確認してください。準備完了までに時間がかかる場合があります。

<<<<<<< HEAD
   ```bash
   $ kubectl get emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## ログレベルの確認
=======
  > `.spec.config.data`フィールドでEMQXクラスターのログレベルを`debug`に設定しています。

+ EMQXクラスターが準備完了になるまで待ちます。kubectl getコマンドでEMQXクラスターの状態を確認し、`STATUS`がRunningであることを確認してください。準備完了までに時間がかかる場合があります。
>>>>>>> origin/release-5.10

1. EMQXクラスターのExternal IPを取得します。

<<<<<<< HEAD
   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
   ```
=======
+ EMQX Operatorは2つのEMQX Serviceリソースを作成します。1つはemqx-dashboard、もう1つはemqx-listenersで、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。
>>>>>>> origin/release-5.10

2. MQTTX CLIを使ってEMQXクラスターに接続します。

   [MQTTX CLI](https://mqttx.app/cli)は、開発者がMQTTサービスやアプリケーションをより迅速に利用開始できるよう設計されたオープンソースのMQTT 5.0コマンドラインクライアントツールです。

<<<<<<< HEAD
   ```
   $ mqttx conn -h ${external_ip} -p 1883
   [4/17/2023] [5:17:31 PM] › … Connecting...
   [4/17/2023] [5:17:31 PM] › ✔ Connected
   ```

3. EMQXコンテナのログを確認します。

   ```bash
   $ kubectl logs emqx-core-0 -c emqx
   ...
   2023-04-17T09:11:35.993031+00:00 [debug] msg: mqtt_packet_received, mfa: emqx_channel:handle_in/2, line: 360, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNECT(Q0, R0, D0, ClientId=mqttx_322680d9, ProtoName=MQTT, ProtoVsn=5, CleanStart=true, KeepAlive=30, Username=undefined, Password=), tag: MQTT
   2023-04-17T09:11:35.997066+00:00 [debug] msg: mqtt_packet_sent, mfa: emqx_connection:serialize_and_inc_stats_fun/1, line: 872, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0), tag: MQTT
   ```
=======
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
>>>>>>> origin/release-5.10
