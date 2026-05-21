# EMQXでTLSを有効化する

<<<<<<< HEAD
## 目的

`extraVolumes` および `extraVolumeMounts` フィールドを使用してTLS証明書をカスタマイズします。

## TLS証明書に基づくSecretの作成

Secretは、パスワード、トークン、キーなどの少量の機密情報を格納するオブジェクトです。本デモではTLS証明書情報を保存するためにSecretを使用するため、EMQXクラスターを作成する前にSecretを作成する必要があります。

詳細については、[Secret](https://kubernetes.io/docs/concepts/configuration/secret/#working-with-secrets)のドキュメントをご参照ください。
=======
## タスク対象

`extraVolumes`および`extraVolumeMounts`フィールドを使用してTLS証明書をカスタマイズします。

## TLS証明書に基づくSecretの作成

Secretは、パスワード、トークン、キーなどの少量の機密情報を含むオブジェクトです。詳細は以下のドキュメントをご参照ください：[Secret](https://kubernetes.io/docs/concepts/configuration/secret/#working-with-secrets)。本記事ではTLS証明書情報を保存するためにSecretを使用するため、EMQXクラスターを作成する前にTLS証明書に基づくSecretを作成する必要があります。

+ 以下をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。
>>>>>>> origin/release-5.10

以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。

<<<<<<< HEAD
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
=======
  > `ca.crt`はCA証明書の内容、`tls.crt`はサーバー証明書の内容、`tls.key`はサーバー秘密鍵の内容を示します。本例では上記3つのフィールドの内容は省略していますので、ご自身の証明書の内容で埋めてください。

## EMQXクラスターの設定

以下はEMQXカスタムリソースの関連設定例です。デプロイするEMQXのバージョンに応じて対応するAPIVersionを選択してください。詳細な対応関係は[EMQX Operator Compatibility](../operator.md)をご参照ください。

`apps.emqx.io/v2beta1 EMQX`は`.spec.coreTemplate.extraVolumes`、`.spec.coreTemplate.extraVolumeMounts`、`.spec.replicantTemplate.extraVolumes`、および`.spec.replicantTemplate.extraVolumeMounts`フィールドをサポートしており、EMQXクラスターに追加のボリュームとマウントポイントを設定できます。本記事ではこれら2つのフィールドを使ってEMQXクラスターのTLS証明書を設定します。

ボリュームには多くの種類があります。ボリュームの説明は以下のドキュメントをご参照ください：[Volumes](https://kubernetes.io/docs/concepts/storage/volumes/#secret)。本記事では`secret`タイプを使用します。

+ 以下をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。
>>>>>>> origin/release-5.10

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

<<<<<<< HEAD
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
=======
  > `.spec.coreTemplate.extraVolumes`フィールドはボリュームのタイプを`secret`、名前を`emqx-tls`として設定しています。

  > `.spec.coreTemplate.extraVolumeMounts`フィールドはTLS証明書をEMQXにマウントするディレクトリを`/mounted/cert`に設定しています。

  > `.spec.config.data`フィールドはTLSリスナーの証明書パスを設定しています。TLSリスナーの詳細な設定についてはドキュメント：[Configuration Manual](../../../../configuration/configuration.md)をご参照ください。

+ EMQXクラスターが起動するまで待ちます。`kubectl get`コマンドでEMQXクラスターの状態を確認し、`STATUS`が`Running`になっていることを確認してください。起動には時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ EMQXクラスターのExternal IPを取得し、EMQXコンソールにアクセスします。

  EMQX Operatorは2つのEMQX Serviceリソースを作成します。1つはemqx-dashboard、もう1つはemqx-listenersで、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。

   ```bash
   $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'
  
   192.168.1.200
   ```

   ブラウザで`http://192.168.1.200:18083`にアクセスし、デフォルトのユーザー名とパスワード`admin/public`でEMQXコンソールにログインします。

## MQTTX CLIを使ったTLS接続の検証

[MQTTX CLI](https://mqttx.app/cli)はオープンソースのMQTT 5.0コマンドラインクライアントツールで、開発者がMQTTサーバーやアプリケーションの開発・デバッグを迅速に行うために設計されています。

+ EMQXクラスターのExternal IPを取得します。
>>>>>>> origin/release-5.10

[MQTTX CLI](https://mqttx.app/cli)は、開発者がMQTTサービスやアプリケーションを迅速に開始できるよう設計されたオープンソースのMQTT 5.0コマンドラインクライアントツールです。

<<<<<<< HEAD
1. EMQXリスナーサービスの外部IPを取得します。
=======
+ MQTTX CLIでメッセージをサブスクライブします。
>>>>>>> origin/release-5.10

   ```bash
   external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
   ```

2. MQTTX CLIでメッセージをサブスクライブします。

<<<<<<< HEAD
   TLSリスナーポート8883に接続し、証明書検証をスキップするために `--insecure` フラグを使用します。
=======
+ 新しいターミナルウィンドウを開き、MQTTX CLIでメッセージをパブリッシュします。
>>>>>>> origin/release-5.10

   ```bash
   mqttx sub -h ${external_ip} -p 8883 -t "hello" -l mqtts --insecure
   [10:00:25] › … Connecting...
   [10:00:25] › ✔ Connected
   [10:00:25] › … Subscribing to hello...
   [10:00:25] › ✔ Subscribed to hello
   ```

3. 別のターミナルウィンドウでメッセージをパブリッシュします。

<<<<<<< HEAD
   ```bash
   mqttx pub -h ${external_ip} -p 8883 -t "hello" -m "hello world" -l mqtts --insecure
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```
=======
+ サブスクライブしているターミナルウィンドウで受信したメッセージを確認します。
>>>>>>> origin/release-5.10

4. サブスクライバークライアントがメッセージを受信することを確認します。

   ```bash
   mqttx pub -h ${external_ip} -p 8883 -t "hello" -m "hello world" -l mqtts --insecure
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```

   これにより、パブリッシャーとサブスクライバーの両クライアントがTLS接続を介してブローカーと正常に通信できていることが示されます。
