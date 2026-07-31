# Google Kubernetes Engine に EMQX をデプロイする

EMQX Operator を使用すると、Google Kubernetes Engine（GKE）上に EMQX をデプロイできます。これにより、GCP 上でマネージド Kubernetes クラスターを簡単にデプロイでき、運用のオーバーヘッドを GCP に委ねることが可能です。GKE 上に EMQX をデプロイすることで、Kubernetes のスケーラビリティと柔軟性を活かしつつ、マネージドサービスのシンプルさと利便性を享受できます。EMQX Operator を GKE 上で利用すれば、クラウド上で MQTT ブローカーを簡単にデプロイ・管理でき、ビジネスの目標に集中できます。

## はじめに

GKE 上に EMQX をデプロイする前に、以下の前提条件を満たしていることを確認してください。

- Google Cloud Platform 上に GKE クラスターがあること
  - プロジェクトで GKE API を有効化する必要があります。セットアップ手順は [Google Kubernetes Engine ドキュメント](https://cloud.google.com/kubernetes-engine/) を参照してください。

- GKE クラスターに接続するための `kubectl` の設定が完了していること
  - ローカルの `kubectl` から接続する場合は、[GKE クラスターへの接続](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-access-for-kubectl) を参照してください。
  
  - GCP コンソールの Cloud Shell から直接接続する場合は、[Cloud Shell での GKE クラスター管理](https://cloud.google.com/code/docs/shell/create-configure-gke-cluster) を参照してください。

- クラスターに EMQX Operator がインストールされていること
  - 詳細は [EMQX Operator のインストール](./getting-started.md) を参照してください。

  ::: warning 注意
  
  GKE に cert-manager をデフォルト設定でインストールすると、ブートストラップに問題が発生する可能性があります。リーダー選出で別のネームスペースを使用するために、`--set global.leaderElection.namespace=cert-manager` の設定を追加してください。詳細は [cert-manager 互換性ドキュメント](https://cert-manager.io/docs/installation/compatibility/) をご覧ください。
  
  :::

## EMQX クラスターを素早くデプロイする

以下の例は、基本的な EMQX カスタムリソース（CR）設定です。

1. 次の内容を YAML ファイルとして保存し、`kubectl apply` でデプロイします。

    ::: warning 注意

    CPU とメモリのリソース制限を指定する場合は、最低でも CPU 250m、メモリ 512Mi を確保してください。詳細は [Autopilot のリソース要求](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-resource-requests) を参照してください。

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

   EMQX Operator は、`dashboardServiceTemplate` の設定に基づいて EMQX ダッシュボード用の Service リソースを作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   34.122.174.166
   ```

4. ダッシュボードに `http://34.122.174.166:18083` でアクセスします。

   デフォルトの認証情報でログインしてください。
   
    - **ユーザー名:** `admin`
    - **パスワード:** `public`

## サブスクライブとパブリッシュ

この手順では、開発者が MQTT サービスやアプリケーションを素早くテストできるオープンソースの MQTT 5.0 コマンドラインクライアントツールである [MQTTX CLI](https://mqttx.app/cli) を使用します。

1. EMQX TCP リスナーの外部 IP を取得します。

   EMQX Operator は、設定された各リスナーに対して Service リソースを自動的に作成します。

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

4. サブスクライバーがメッセージを受信する様子を確認します。

   ```shell
   [10:00:58] › payload: hello world
   ```

## LoadBalancer による TLS オフロードに関する注意点

執筆時点では、Google LoadBalancer は TLS からプレーン TCP へのトラフィックの終端をサポートしていません。可能な回避策については、こちらの [ディスカッション](https://github.com/emqx/emqx-operator/discussions/312) をご参照ください。
