# EMQXのログレベル変更

## 目的

EMQXクラスターのログレベルを変更します。

## EMQXクラスターの設定

`apps.emqx.io/v3beta1` EMQX CRDは、`.spec.config.roots.log`を通じてログレベルの設定をサポートしています。詳細な設定リファレンスは[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.2.0/hocon/)をご参照ください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       # デバッグログを有効化:
       roots:
         log:
           console:
             level: debug
         license:
           key: "..."
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
   ```

2. EMQXクラスターが準備完了になるまで待ちます。

   `kubectl get`でEMQXクラスターの状態を確認し、`STATUS`が`Ready`になっていることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## ログレベルの確認

1. EMQXクラスターのExternal IPを取得します。

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
   ```

2. MQTTX CLIを使ってEMQXクラスターに接続します。

   [MQTTX CLI](https://mqttx.app/cli)は、開発者がMQTTサービスやアプリケーションを迅速に利用開始できるよう設計されたオープンソースのMQTT 5.0コマンドラインクライアントツールです。

   ```
   $ mqttx conn -h ${external_ip} -p 1883
   [4/17/2023] [5:17:31 PM] › … Connecting...
   [4/17/2023] [5:17:31 PM] › ✔ Connected
   ```

3. EMQXコンテナのログを確認します。

   ```bash
   $ kubectl logs emqx-core-0 -c emqx
   ...
   2023-04-17T09:11:35.993031+00:00 [debug] msg: mqtt_packet_received, mfa: emqx_channel:handle_in/2, line: 360, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNECT(Q0, R0, D0, ClientId=mqttx_322680d9, ProtoName=MQTT, ProtoVsn=5, CleanStart=true, KeepAlive=30, Username=undefined, Password=undefined), tag: MQTT
   2023-04-17T09:11:35.997066+00:00 [debug] msg: mqtt_packet_sent, mfa: emqx_connection:serialize_and_inc_stats_fun/1, line: 872, peername: 218.190.230.144:59457, clientid: mqttx_322680d9, packet: CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0), tag: MQTT
   ```
