# Amazon Elastic Kubernetes Service に EMQX をデプロイする

EMQX Operator は Amazon Container Service の EKS（Elastic Kubernetes Service）上での実行をサポートしています。Amazon EKS は、コンテナ化されたアプリケーションのデプロイ、管理、スケーリングを簡素化するマネージド Kubernetes サービスです。EKS は Kubernetes のコントロールプレーンとノードグループを提供し、ノードの置き換え、アップグレード、パッチ適用を自動で処理します。また、AWS のロードバランサー、RDS、IAM などのサービスをサポートし、他の Kubernetes エコシステムツールとシームレスに統合されます。

詳細な紹介については、[Amazon EKS とは](https://docs.aws.amazon.com/eks/latest/userguide/what-is-eks.html)をご参照ください。

## はじめに

EKS 上に EMQX をデプロイする前に、以下の前提条件を満たしていることを確認してください。

- EKS クラスターを作成する。<br/>詳細は [Amazon EKS クラスターの作成](https://docs.aws.amazon.com/eks/latest/userguide/getting-started.html) をご覧ください。

- kubectl を設定して EKS クラスターに接続できるようにする。<br/>詳細は [kubectl を使用してクラスターに接続する](https://docs.aws.amazon.com/eks/latest/userguide/getting-started-console.html#eks-configure-kubectl) をご覧ください。

- クラスターに AWS Load Balancer Controller をデプロイする。<br/>詳細は [ネットワークロードバランサーの作成](https://docs.aws.amazon.com/eks/latest/userguide/network-load-balancing.html) をご覧ください。

- クラスターに Amazon EBS CSI ドライバーをインストールする。<br/>詳細は [Amazon EBS CSI ドライバー](https://docs.aws.amazon.com/eks/latest/userguide/ebs-csi.html) をご覧ください。

- EMQX Operator をインストールする。<br/>詳細は [EMQX Operator のインストール](./getting-started.md) をご参照ください。

## EMQX クラスターを素早くデプロイする

以下の例は、EKS 上にデプロイするための EMQX カスタムリソース（CR）設定の一例です。

1. 下記の内容を YAML ファイルとして保存し、`kubectl apply` でデプロイします。

   ```yaml
   # WaitForFirstConsumer バインディングモードを持つ EBS StorageClass を設定
   # これにより、ボリュームはそれを使用するポッドと同じ AZ に作成されます
   apiVersion: storage.k8s.io/v1
   kind: StorageClass
   metadata:
     name: ebs-sc
   provisioner: ebs.csi.aws.com
   volumeBindingMode: WaitForFirstConsumer
   ---
   apiVersion: apps.emqx.io/v2beta1
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
         ## EMQX カスタムリソースはこのフィールドのランタイム更新をサポートしていません
         volumeClaimTemplates:
           storageClassName: ebs-sc
           resources:
             requests:
               storage: 10Gi
           accessModes:
             - ReadWriteOnce
     dashboardServiceTemplate:
       metadata:
         ## 詳細: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/annotations/
         annotations:
           ## NLB がインターネット向けか内部向けかを指定。指定がなければ内部向けがデフォルト。
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## 詳細: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
     listenersServiceTemplate:
       metadata:
         ## 詳細: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/annotations/
         annotations:
           ## NLB がインターネット向けか内部向けかを指定。指定がなければ内部向けがデフォルト。
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## 詳細: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
   ```

2. EMQX クラスターが Ready 状態になるまで待ちます。

   以下のコマンドでステータスを確認します。`STATUS` フィールドが `Ready` になるまで数分かかる場合があります。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     55s
   ```

3. EMQX ダッシュボードの外部 IP を取得し、アクセスします。

   EMQX Operator は `dashboardServiceTemplate` の設定に基づき、EMQX ダッシュボード用の Service を作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   192.168.1.200
   ```

4. ダッシュボードを `http://192.168.1.200:18083` で開きます。

   初期ログイン情報は以下の通りです：

     - **ユーザー名:** `admin`
     - **パスワード:** `public`

## サブスクライブとパブリッシュ

このハンズオンでは、オープンソースの MQTT 5.0 コマンドラインクライアントツールである [MQTTX CLI](https://mqttx.app/cli) を使用し、MQTT サービスやアプリケーションの動作を素早くテストします。

1. EMQX TCP リスナーの外部 IP を取得します。

   EMQX Operator は設定された各リスナーに対して自動的に Service リソースを作成します。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   
   [10:00:25] › … 接続中...
   [10:00:25] › ✔ 接続完了
   [10:00:25] › … hello にサブスクライブ中...
   [10:00:25] › ✔ hello にサブスクライブしました
   ```

3. 別のターミナルで EMQX クラスターに接続し、メッセージをパブリッシュします。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   
   [10:00:58] › … 接続中...
   [10:00:58] › ✔ 接続完了
   [10:00:58] › … メッセージをパブリッシュ中...
   [10:00:58] › ✔ メッセージをパブリッシュしました
   ```

4. サブスクライバー側でメッセージを受信していることを確認します。

   ```shell
   [10:00:58] › payload: hello world
   ```

## ロードバランサーで TLS 暗号化を終端する

AWS ネットワークロードバランサー（NLB）を利用して EMQX の TLS トラフィックを終端することが可能です。以下の手順に従ってください。

1. [AWS コンソール](https://us-east-2.console.aws.amazon.com/acm/home)で関連する証明書をインポートします。証明書 ID をクリックして詳細ページを開き、証明書の ARN を控えておきます。

    ::: tip
証明書／キーのインポート形式については、[証明書のインポート](https://docs.aws.amazon.com/acm/latest/userguide/import-certificate-format.html)をご参照ください。
    :::

2. EMQX Service のメタデータに以下のアノテーションを追加します。例：

    ```yaml
    ## AWS Certificate Manager で管理されている 1 つ以上の証明書の ARN を指定します。
    service.beta.kubernetes.io/aws-load-balancer-ssl-cert: arn:aws:acm:us-west-2:xxxxx:certificate/xxxxxxx
    ## ロードバランサーと Kubernetes ポッド間のバックエンドトラフィックに TLS を使用するかを指定します。
    service.beta.kubernetes.io/aws-load-balancer-backend-protocol: tcp
    ## TLS リスナーを持つフロントエンドポートを指定します。これにより、AWS NLB サービス経由でポート 1883 にアクセスする際は TLS 認証が必要ですが、
    ## K8S サービスのポートに直接アクセスする場合は TLS 認証は不要です。
    service.beta.kubernetes.io/aws-load-balancer-ssl-ports: "1883"
    ```

    ::: tip
    `service.beta.kubernetes.io/aws-load-balancer-ssl-cert` の値は、手順1で控えた ARN と一致させてください。
    :::
