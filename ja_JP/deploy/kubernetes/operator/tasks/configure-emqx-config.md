# EMQXの設定変更

## 対象タスク

EMQXカスタムリソースの`config.data`によってEMQXの設定を変更します。

## EMQXクラスターの設定

EMQXのメイン設定ファイルは`/etc/emqx.conf`です。バージョン5.0以降、EMQXは設定ファイル形式として[HOCON](https://www.emqx.io/docs/en/v5.1/configuration/configuration.html#hocon-configuration-format)を採用しています。

`apps.emqx.io/v2beta1 EMQX`は`.spec.config.data`フィールドを通じてEMQXクラスターの設定をサポートしています。`config.data`による設定については、以下のドキュメントを参照してください：[設定マニュアル](https://www.emqx.io/docs/en/v5.1/configuration/configuration-manual.html#configuration-manual)。

+ 以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx-enterprise:5.10
      imagePullPolicy: IfNotPresent
      config:
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

   > `.spec.config.data`フィールドでは、EMQXクラスター用のTCPリスナーを設定しています。リスナー名は`test`で、リッスンポートは`1884`です。

+ EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでEMQXクラスターの状態を確認し、`STATUS`が`Running`であることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   IMAGE                         STATUS    AGE
   emqx   emqx/emqx-enterprise:5.10.0   Running   10m
   ```

+ EMQXクラスターのダッシュボード外部IPを取得し、EMQXコンソールにアクセスします。

  EMQXオペレーターは2つのEMQX Serviceリソースを作成します。1つは`emqx-dashboard`、もう1つは`emqx-listeners`で、それぞれEMQXコンソールとEMQXのリスニングポートに対応しています。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

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
