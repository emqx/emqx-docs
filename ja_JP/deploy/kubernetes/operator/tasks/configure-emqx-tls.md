# EMQXでTLSを有効化する

## 目的

`extraVolumes` および `extraVolumeMounts` フィールドを使用してTLS証明書をカスタマイズします。

## TLS証明書に基づくSecretの作成

Secretは、パスワード、トークン、キーなどの少量の機密情報を格納するオブジェクトです。本デモではTLS証明書情報を保存するためにSecretを使用するため、EMQXクラスターを作成する前にSecretを作成する必要があります。

詳細については、[Secret](https://kubernetes.io/docs/concepts/configuration/secret/#working-with-secrets)のドキュメントをご参照ください。

以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: emqx-tls
type: kubernetes.io/tls
stringData:
  ca.crt: |
    -----BEGIN CERTIFICATE-----
    ...
    -----END CERTIFICATE-----
  tls.crt: |
    -----BEGIN CERTIFICATE-----
    ...
    -----END CERTIFICATE-----
  tls.key: |
    -----BEGIN RSA PRIVATE KEY-----
    ...
    -----END RSA PRIVATE KEY-----
```

:::tip
上記3つのフィールドの内容は省略しています。ご自身の証明書内容で埋めてください。
* `ca.crt` はCA証明書を含めてください。
* `tls.crt` はサーバー証明書を含めてください。
* `tls.key` はサーバーの秘密鍵を含めてください。
:::

## EMQXクラスターの設定

EMQXのCRD `apps.emqx.io/v2beta1` には、EMQXクラスターに追加のボリュームおよびマウントポイントを設定するための以下のフィールドがあります：
* `.spec.coreTemplate.extraVolumes`
* `.spec.coreTemplate.extraVolumeMounts`
* `.spec.replicantTemplate.extraVolumes`
* `.spec.replicantTemplate.extraVolumeMounts`

本デモでは、これらのフィールドを使用してTLS証明書をEMQXクラスターに提供します。

ボリュームには多くの種類があります。ボリュームの詳細については、[Volumes](https://kubernetes.io/docs/concepts/storage/volumes/#secret)のドキュメントをご参照ください。ここでは `secret` ボリュームタイプを使用します。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       # `emqx-tls` ボリュームからマウントされたTLSリスナー証明書を設定：
       data: |
         listeners.ssl.default {
           bind = "0.0.0.0:8883"
           ssl_options {
             cacertfile = "/mounted/cert/ca.crt"
             certfile = "/mounted/cert/tls.crt"
             keyfile = "/mounted/cert/tls.key"
             gc_after_handshake = true
             handshake_timeout = 5s
           }
         }
         license {
           key = "..."
         }
     coreTemplate:
       spec:
         extraVolumes:
           - name: emqx-tls
             secret:
               secretName: emqx-tls
         extraVolumeMounts:
           - name: emqx-tls
             mountPath: /mounted/cert
     replicantTemplate:
       spec:
         extraVolumes:
           # `emqx-tls` という名前の `secret` ボリュームタイプを作成：
           - name: emqx-tls
             secret:
               secretName: emqx-tls
         extraVolumeMounts:
           - name: emqx-tls
             # TLS証明書がEMQXノードにマウントされるディレクトリ：
             mountPath: /mounted/cert
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
   ```

2. EMQXクラスターが準備完了になるまで待ちます。

   `kubectl get`コマンドでEMQXクラスターのステータスを確認し、`STATUS`が`Ready`になっていることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## MQTTXでTLS接続を検証する

[MQTTX CLI](https://mqttx.app/cli)は、開発者がMQTTサービスやアプリケーションを迅速に開始できるよう設計されたオープンソースのMQTT 5.0コマンドラインクライアントツールです。

1. EMQXリスナーサービスの外部IPを取得します。

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
   ```

2. MQTTX CLIでメッセージをサブスクライブします。

   TLSリスナーポート8883に接続し、証明書検証をスキップするために `--insecure` フラグを使用します。

   ```bash
   mqttx sub -h ${external_ip} -p 8883 -t "hello" -l mqtts --insecure
   [10:00:25] › … Connecting...
   [10:00:25] › ✔ Connected
   [10:00:25] › … Subscribing to hello...
   [10:00:25] › ✔ Subscribed to hello
   ```

3. 別のターミナルウィンドウでメッセージをパブリッシュします。

   ```bash
   mqttx pub -h ${external_ip} -p 8883 -t "hello" -m "hello world" -l mqtts --insecure
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```

4. サブスクライバークライアントがメッセージを受信することを確認します。

   ```bash
   mqttx pub -h ${external_ip} -p 8883 -t "hello" -m "hello world" -l mqtts --insecure
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```

   これにより、パブリッシャーとサブスクライバーの両クライアントがTLS接続を介してブローカーと正常に通信できていることが示されます。
