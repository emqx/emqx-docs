# Google Kubernetes Engine に EMQX をデプロイする

EMQX Operator を使用すると、Google Kubernetes Engine（GKE）上に EMQX をデプロイできます。これにより、GCP 上でマネージド Kubernetes クラスターを簡単にデプロイできるようになります。GKE を利用することで、運用のオーバーヘッドを GCP に委ねることが可能です。GKE 上に EMQX をデプロイすることで、Kubernetes のスケーラビリティと柔軟性を活かしつつ、マネージドサービスの簡便さと利便性を享受できます。EMQX Operator を GKE 上で使用することで、クラウド上で MQTT ブローカーを簡単にデプロイおよび管理でき、ビジネス目標に集中できます。

## はじめる前に

GKE 上に EMQX をデプロイする前に、以下の前提条件を満たしていることを確認してください。

- Google Cloud Platform 上の GKE クラスター
  - プロジェクトで GKE API を有効にする必要があります。セットアップ手順は [Google Kubernetes Engine ドキュメント](https://cloud.google.com/kubernetes-engine/) を参照してください。

- GKE クラスターに接続するための動作する `kubectl` 設定
  - ローカルの `kubectl` インストールを使用して接続する場合は、[GKE クラスターへの接続](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-access-for-kubectl) を参照してください。
  
  - GCP コンソールの Cloud Shell から直接接続する場合は、[Cloud Shell で GKE クラスターを管理する](https://cloud.google.com/code/docs/shell/create-configure-gke-cluster) を参照してください。

- クラスターに EMQX Operator がインストールされていること
  - 詳細は [EMQX Operator のインストール](./getting-started.md) を参照してください。

## EMQX クラスターの迅速なデプロイ

以下の例は、基本的な EMQX カスタムリソース（CR）設定を示しています。

1. 次のドキュメントを YAML ファイルとして保存し、`kubectl apply` でデプロイします。

    ::: warning 注意

    CPU とメモリの制限を指定する場合は、最低でも 250m CPU と 512Mi メモリを確保してください。詳細は [Autopilot のリソース要求](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-resource-requests) を参照してください。

    :::

   ```yaml
   apiVersion: apps.emqx.io/v2
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
     coreTemplate:
       spec:
         volumeClaimTemplates:
         ## ストレージクラスの詳細：https://cloud.google.com/kubernetes-engine/docs/concepts/persistent-volumes#storageclasses
           storageClassName: standard
           resources:
             requests:
               storage: 10Gi
           accessModes:
           - ReadWriteOnce
     dashboardServiceTemplate:
       spec:
         ## ロードバランサーの詳細：https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balancing
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         ## ロードバランサーの詳細：https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balancing
         type: LoadBalancer
   ```

2. EMQX クラスターが準備完了になるまで待ちます。

   `kubectl get` コマンドで EMQX クラスターのステータスを確認し、`STATUS` が `Ready` になっていることを確認してください。準備完了までに時間がかかる場合があります。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m2s
   ```

3. EMQX ダッシュボードの外部 IP を取得します。

   EMQX Operator は `dashboardServiceTemplate` の設定に基づき、EMQX ダッシュボード用の Service リソースを作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   34.122.174.166
   ```

4. `http://34.122.174.166:18083` にアクセスしてダッシュボードを開きます。

   デフォルトの認証情報でログインします：
   
    - **ユーザー名:** `admin`
    - **パスワード:** `public`

## サブスクライブとパブリッシュ

このハンズオンでは、開発者が MQTT サービスやアプリケーションを迅速にテストできるオープンソースの MQTT 5.0 コマンドラインクライアントツールである [MQTTX CLI](https://mqttx.app/cli) を使用します。

1. EMQX TCP リスナーの外部 IP を取得します。

   EMQX Operator は、設定された各リスナーに対して自動的に Service リソースを作成します。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  接続中...
   [10:00:25] › ✔  接続完了
   [10:00:25] › …  hello にサブスクライブ中...
   [10:00:25] › ✔  hello にサブスクライブしました
   ```

3. 別のターミナルで EMQX クラスターに接続し、メッセージをパブリッシュします。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   
   [10:00:58] › …  接続中...
   [10:00:58] › ✔  接続完了
   [10:00:58] › …  メッセージをパブリッシュ中...
   [10:00:58] › ✔  メッセージをパブリッシュしました
   ```

4. サブスクライバーがメッセージを受信するのを確認します。

   ```shell
   [10:00:58] › payload: hello world
   ```

## LoadBalancer による TLS オフロードについての注意点

執筆時点で、Google LoadBalancer は TLS からプレーン TCP へのトラフィックの終端（TLS ターミネーション）をサポートしていません。可能な回避策については、この[ディスカッション](https://github.com/emqx/emqx-operator/discussions/312)を参照してください。
