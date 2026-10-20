# Amazon Elastic Kubernetes Service上でのEMQXデプロイ

EMQX OperatorはAmazon Container Service EKS（Elastic Kubernetes Service）上での実行をサポートしています。Amazon EKSは、コンテナ化されたアプリケーションのデプロイ、管理、スケーリングを簡素化するマネージドKubernetesサービスです。EKSはKubernetesのコントロールプレーンとノードグループを提供し、ノードの交換、アップグレード、パッチ適用を自動で行います。Load Balancer、RDS、IAMなどのAWSサービスをサポートし、他のKubernetesエコシステムツールともシームレスに統合されます。

詳細な紹介については、[Amazon EKSとは](https://docs.aws.amazon.com/eks/latest/userguide/what-is-eks.html)を参照してください。

## はじめに

EKS上にEMQXをデプロイする前に、以下の前提条件を完了していることを確認してください。

- EKSクラスターを作成する。<br/>詳細は[Amazon EKSクラスターの作成](https://docs.aws.amazon.com/eks/latest/userguide/getting-started.html)を参照してください。

- kubectlを設定してEKSクラスターに接続する。<br/>詳細は[kubectlを使ってクラスターに接続する](https://docs.aws.amazon.com/eks/latest/userguide/getting-started-console.html#eks-configure-kubectl)を参照してください。

- クラスターにAWS Load Balancer Controllerをデプロイする。<br/>詳細は[ネットワークロードバランサーの作成](https://docs.aws.amazon.com/eks/latest/userguide/network-load-balancing.html)を参照してください。

- クラスターにAmazon EBS CSIドライバーをインストールする。<br/>詳細は[Amazon EBS CSIドライバー](https://docs.aws.amazon.com/eks/latest/userguide/ebs-csi.html)を参照してください。

- EMQX Operatorをインストールする。<br/>詳細は[EMQX Operatorのインストール](./getting-started.md)を参照してください。

## EMQXクラスターの迅速なデプロイ

以下の例は、EKS上でのデプロイに関連するEMQXカスタムリソース（CR）の設定例です。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   # WaitForFirstConsumerバインディングモードでEBS StorageClassを設定
   # これにより、ボリュームはそれを使用するPodと同じAZに作成されます
   apiVersion: storage.k8s.io/v1
   kind: StorageClass
   metadata:
     name: ebs-sc
   provisioner: ebs.csi.aws.com
   volumeBindingMode: WaitForFirstConsumer
   ---
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
         ## EMQXカスタムリソースはこのフィールドのランタイム更新をサポートしていません
         persistentVolumeClaimSpec:
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
           ## NLBがインターネット向けか内部向けかを指定。未指定の場合は内部向けがデフォルト。
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
           ## NLBがインターネット向けか内部向けかを指定。未指定の場合は内部向けがデフォルト。
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## 詳細: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
   ```

2. EMQXクラスターがReady状態になるまで待ちます。

   以下のコマンドでステータスを確認します。`STATUS`フィールドが`Ready`になるまで数分かかる場合があります。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     55s
   ```

3. EMQXダッシュボードの外部IPを取得し、アクセスします。

   EMQX Operatorは`dashboardServiceTemplate`の設定に基づいてEMQXダッシュボード用のServiceを作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   192.168.1.200
   ```

4. ダッシュボードにアクセスします：`http://192.168.1.200:18083`

   デフォルトの認証情報でログインしてください：

     - **ユーザー名:** `admin`
     - **パスワード:** `public`

## サブスクライブとパブリッシュ

このハンズオンでは、開発者がMQTTサービスやアプリケーションを素早くテストできるオープンソースのMQTT 5.0コマンドラインクライアントツールである[MQTTX CLI](https://mqttx.app/cli)を使用します。

1. EMQX TCPリスナーの外部IPを取得します。

   EMQX Operatorは設定された各リスナーに対して自動的にServiceリソースを作成します。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   
   [10:00:25] › … 接続中...
   [10:00:25] › ✔ 接続完了
   [10:00:25] › … helloにサブスクライブ中...
   [10:00:25] › ✔ helloにサブスクライブ完了
   ```

3. 別のターミナルでEMQXクラスターに接続し、メッセージをパブリッシュします。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   
   [10:00:58] › … 接続中...
   [10:00:58] › ✔ 接続完了
   [10:00:58] › … メッセージをパブリッシュ中...
   [10:00:58] › ✔ メッセージをパブリッシュしました
   ```

4. サブスクライバーがメッセージを受信する様子を確認します。

   ```shell
   [10:00:58] › payload: hello world
   ```

## LoadBalancerでTLS暗号化を終端する

AWS Network Load Balancer（NLB）を使用してEMQXのTLSトラフィックを終端できます。以下の手順に従ってください。

1. [AWSコンソール](https://us-east-2.console.aws.amazon.com/acm/home)で関連する証明書をインポートします。証明書IDをクリックして詳細ページを開き、証明書ARNを控えてください。

    ::: tip
証明書／キーのインポート形式については、[証明書のインポート](https://docs.aws.amazon.com/acm/latest/userguide/import-certificate-format.html)を参照してください。
    :::

2. EMQX Serviceのメタデータに以下のアノテーションを追加します。例：

    ```yaml
    ## AWS Certificate Managerで管理されている1つ以上の証明書のARNを指定します。
    service.beta.kubernetes.io/aws-load-balancer-ssl-cert: arn:aws:acm:us-west-2:xxxxx:certificate/xxxxxxx
    ## ロードバランサーとKubernetes Pod間のバックエンドトラフィックにTLSを使用するかどうかを指定します。
    service.beta.kubernetes.io/aws-load-balancer-backend-protocol: tcp
    ## TLSリスナーを持つフロントエンドポートを指定します。これにより、AWS NLBサービス経由でポート1883にアクセスする際はTLS認証が必要ですが、
    ## K8Sサービスのポートに直接アクセスする場合はTLS認証は不要です。
    service.beta.kubernetes.io/aws-load-balancer-ssl-ports: "1883"
    ```

    ::: tip
    `service.beta.kubernetes.io/aws-load-balancer-ssl-cert`の値は、手順1で控えたARNと一致させてください。
    :::
