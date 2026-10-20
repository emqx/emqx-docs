# Google Kubernetes EngineへのEMQXデプロイ

EMQX Operatorを使用すると、Google Kubernetes Engine（GKE）上にEMQXをデプロイできます。これにより、GCPで管理されたKubernetesクラスターのデプロイが簡素化されます。GKEを利用することで、運用のオーバーヘッドをGCPに委ねることが可能です。GKE上にEMQXをデプロイすることで、Kubernetesのスケーラビリティと柔軟性を活かしつつ、管理されたサービスのシンプルさと利便性を享受できます。EMQX OperatorをGKEで利用すれば、クラウド上でMQTTブローカーを簡単にデプロイおよび管理でき、ビジネス目標に集中できます。

## はじめる前に

GKE上にEMQXをデプロイする前に、以下の前提条件を満たしていることを確認してください。

- Google Cloud Platform上にGKEクラスターがあること
  - プロジェクトでGKE APIを有効にする必要があります。セットアップ手順は[Google Kubernetes Engineのドキュメント](https://cloud.google.com/kubernetes-engine/)を参照してください。

- GKEクラスターに接続するための動作する`kubectl`設定
  - ローカルの`kubectl`インストールを使って接続する場合は、[GKEクラスターへの接続](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-access-for-kubectl)を参照してください。
  
    GCPコンソールのCloud Shellから直接接続する場合は、[Cloud ShellでのGKEクラスター管理](https://cloud.google.com/code/docs/shell/create-configure-gke-cluster)を参照してください。
  
- クラスターにEMQX Operatorがインストールされていること
  - 詳細は[EMQX Operatorのインストール](./getting-started.md)を参照してください。

## EMQXクラスターの迅速なデプロイ

以下の例は基本的なEMQXカスタムリソース（CR）設定を示しています。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

    ::: warning 注意

    CPUおよびメモリのリソース制限を指定する場合は、最低でも250m CPUと512Miメモリを確保してください。詳細は[Autopilotのリソースリクエスト](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-resource-requests)を参照してください。

    :::

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "..."
     coreTemplate:
       spec:
         persistentVolumeClaimSpec:
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

2. EMQXクラスターがReady状態になるまで待ちます。

   `kubectl get`コマンドでEMQXクラスターの状態を確認し、`STATUS`が`Ready`であることを確認してください。準備完了までに時間がかかる場合があります。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m2s
   ```

3. EMQXダッシュボードの外部IPを取得します。

   EMQX Operatorは`dashboardServiceTemplate`の設定に基づき、EMQXダッシュボード用のServiceリソースを作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   34.122.174.166
   ```

4. `http://34.122.174.166:18083`でダッシュボードを開きます。

   デフォルトの認証情報でログインしてください。
   
    - **ユーザー名:** `admin`
    - **パスワード:** `public`

## サブスクライブとパブリッシュ

このハンズオンでは、オープンソースのMQTT 5.0コマンドラインクライアントツールである[MQTTX CLI](https://mqttx.app/cli)を使用し、MQTTサービスやアプリケーションの迅速なテストを支援します。

1. EMQX TCPリスナーの外部IPを取得します。

   EMQX Operatorは設定された各リスナーに対して自動的にServiceリソースを作成します。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  接続中...
   [10:00:25] › ✔  接続完了
   [10:00:25] › …  helloにサブスクライブ中...
   [10:00:25] › ✔  helloにサブスクライブ完了
   ```

3. 別のターミナルでEMQXクラスターに接続し、メッセージをパブリッシュします。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   
   [10:00:58] › …  接続中...
   [10:00:58] › ✔  接続完了
   [10:00:58] › …  メッセージをパブリッシュ中...
   [10:00:58] › ✔  メッセージをパブリッシュ完了
   ```

4. サブスクライバーがメッセージを受信する様子を確認します。

   ```shell
   [10:00:58] › payload: hello world
   ```

## LoadBalancerによるTLSオフロードについての注意点

執筆時点では、Google LoadBalancerはTLSからプレーンTCPへのトラフィックの終了（ターミネーション）をサポートしていません。可能な回避策については、この[ディスカッション](https://github.com/emqx/emqx-operator/discussions/312)を参照してください。
