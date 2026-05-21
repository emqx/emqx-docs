<<<<<<< HEAD
# EMQX設定の変更

## 目的

EMQXカスタムリソースの`.spec.config.data`フィールドを使用して、EMQXの設定を変更します。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2beta1`は、`.spec.config.data`フィールドを通じてEMQXクラスターの設定をサポートしています。完全な設定リファレンスについては、[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)を参照してください。

EMQXは設定ファイル形式として[HOCON](../../../../configuration/configuration.md#hocon-configuration-format)を使用しています。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。
=======
# EMQXの設定変更

## 対象タスク

EMQXカスタムリソースの`config.data`によってEMQXの設定を変更します。

## EMQXクラスターの設定

EMQXのメイン設定ファイルは`/etc/emqx.conf`です。バージョン5.0以降、EMQXは設定ファイル形式として[HOCON](https://www.emqx.io/docs/en/v5.1/configuration/configuration.html#hocon-configuration-format)を採用しています。

`apps.emqx.io/v2beta1 EMQX`は`.spec.config.data`フィールドを通じてEMQXクラスターの設定をサポートしています。`config.data`による設定については、以下のドキュメントを参照してください：[設定マニュアル](https://www.emqx.io/docs/en/v5.1/configuration/configuration-manual.html#configuration-manual)。

+ 以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。
>>>>>>> origin/release-5.10

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx:@EE_VERSION@
      imagePullPolicy: IfNotPresent
      config:
         # ポート1884で待ち受けるTCPリスナー`test`を設定します：
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

<<<<<<< HEAD
   ::: tip
   `.spec.config.data`フィールドの内容は、EMQXコンテナに対して[`emqx.conf`設定ファイル](../../../../configuration/configuration.md#immutable-configuration-file)として提供されます。
   :::

2. EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでEMQXクラスターのステータスを確認し、`STATUS`が`Ready`であることを確認してください。完了までに時間がかかる場合があります。
=======
   > `.spec.config.data`フィールドでは、EMQXクラスター用のTCPリスナーを設定しています。リスナー名は`test`で、リッスンポートは`1884`です。

+ EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでEMQXクラスターの状態を確認し、`STATUS`が`Running`であることを確認してください。完了までに時間がかかる場合があります。
>>>>>>> origin/release-5.10

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

<<<<<<< HEAD
## 設定の確認

EMQXのリスナーの状態を確認します。
=======
+ EMQXクラスターのダッシュボード外部IPを取得し、EMQXコンソールにアクセスします。

  EMQXオペレーターは2つのEMQX Serviceリソースを作成します。1つは`emqx-dashboard`、もう1つは`emqx-listeners`で、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
ここで、ポート1884で新たに設定したリスナーが稼働していることが確認できます。
=======
  192.168.1.200
  ```

  ブラウザで`http://192.168.1.200:18083`にアクセスし、デフォルトのユーザー名とパスワード`admin/public`でEMQXコンソールにログインします。

## 設定の確認

+ EMQXクラスターのリスナー情報を表示します。

   ```bash
   $ kubectl exec -it emqx-core-0 -c emqx -- emqx ctl listeners
   ```

   以下のような出力が得られます。これは、設定した`test`という名前のリスナーが有効になっていることを示しています。

   ```bash
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
>>>>>>> origin/release-5.10
