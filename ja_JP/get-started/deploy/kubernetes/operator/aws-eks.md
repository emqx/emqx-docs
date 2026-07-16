# Amazon Elastic Kubernetes Service 上での EMQX デプロイ

EMQX Operator は、Amazon Container Service EKS（Elastic Kubernetes Service）上での EMQX デプロイをサポートしています。Amazon EKS は、コンテナ化されたアプリケーションのデプロイ、管理、スケールを容易にするマネージド Kubernetes サービスです。EKS は Kubernetes のコントロールプレーンとノードグループを提供し、ノードの置換、アップグレード、パッチ適用を自動で処理します。さらに、AWS の Load Balancer、RDS、IAM などのサービスをサポートし、他の Kubernetes エコシステムツールとシームレスに統合されます。詳細は [Amazon EKS とは](https://docs.aws.amazon.com/eks/latest/userguide/what-is-eks.html) をご参照ください。

## はじめに

開始する前に、以下を準備してください。

- Amazon Container Service を有効化し、EKS クラスターを作成すること。詳細は [Amazon EKS クラスターの作成](https://docs.aws.amazon.com/eks/latest/userguide/getting-started.html) をご参照ください。

- ローカルに kubectl ツールをインストールし、EKS クラスターに接続すること。詳細は [kubectl を使用してクラスターに接続する](https://docs.aws.amazon.com/eks/latest/userguide/getting-started-console.html#eks-configure-kubectl) をご参照ください。

- クラスターに AWS Load Balancer Controller をデプロイすること。詳細は [ネットワーク Load Balancer の作成](https://docs.aws.amazon.com/eks/latest/userguide/network-load-balancing.html) をご参照ください。

- クラスターに Amazon EBS CSI ドライバーをインストールすること。詳細は [Amazon EBS CSI ドライバー](https://docs.aws.amazon.com/eks/latest/userguide/ebs-csi.html) をご参照ください。

- EMQX Operator をインストールすること。詳細は [EMQX Operator のインストール](./getting-started.md) をご参照ください。

## EMQX クラスターの迅速なデプロイ

以下は EMQX カスタムリソースの関連設定例です。

+ 下記の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイしてください。

  ```yaml
  # WaitForFirstConsumer バインディングモードで EBS StorageClass を設定
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
    image: emqx/emqx-enterprise:@EE_VERSION@
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
          ## NLB がインターネット向けか内部向けかを指定。指定しない場合は内部がデフォルト。
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
          ## NLB がインターネット向けか内部向けかを指定。指定しない場合は内部がデフォルト。
          service.beta.kubernetes.io/aws-load-balancer-type: external
          service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
      spec:
        type: LoadBalancer
        ## 詳細: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
        loadBalancerClass: service.k8s.aws/nlb
  ```

+ EMQX クラスターの準備が整うまで待ちます。`kubectl get` コマンドで EMQX クラスターのステータスを確認し、`STATUS` が `Running` となっていることを確認してください。準備には時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ EMQX クラスターのダッシュボード外部 IP を取得し、EMQX コンソールにアクセスします。

  EMQX Operator は 2 つの EMQX Service リソースを作成します。1 つは emqx-dashboard、もう 1 つは emqx-listeners で、それぞれ EMQX コンソールと EMQX のリスニングポートに対応しています。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` を使って EMQX コンソールにログインしてください。

## MQTTX アプリケーションを使ったメッセージのパブリッシュ／サブスクライブ

[MQTTX CLI](https://mqttx.app/cli) はオープンソースの MQTT 5.0 コマンドラインクライアントツールで、開発者が MQTT サービスやアプリケーションをより迅速に開発・デバッグできるよう設計されています。

+ EMQX クラスターの外部 IP を取得します。

  ```bash
  external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
  ```

+ ニュースをサブスクライブします。

  ```bash
  $ mqttx sub -t 'hello' -h ${external_ip} -p 1883

  [10:00:25] › … 接続中...
  [10:00:25] › ✔ 接続完了
  [10:00:25] › … hello にサブスクライブ中...
  [10:00:25] › ✔ hello にサブスクライブ完了
  ```

+ 新しいターミナルウィンドウを開き、メッセージをパブリッシュします。

  ```bash
  $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'

  [10:00:58] › … 接続中...
  [10:00:58] › ✔ 接続完了
  [10:00:58] › … メッセージをパブリッシュ中...
  [10:00:58] › ✔ メッセージパブリッシュ完了
  ```

+ サブスクライブしているターミナルウィンドウで受信したメッセージを確認します。

  ```bash
  [10:00:58] › ペイロード: hello world
  ```

## LoadBalancer での TLS 暗号化の終端

Amazon EKS では、NLB を使用して TLS 終端を行うことができます。手順は以下の通りです。

1. [AWS コンソール](https://us-east-2.console.aws.amazon.com/acm/home) で関連する証明書をインポートし、証明書 ID をクリックして詳細ページに入り、ARN 情報を控えます。

    :::tip

    証明書とキーのインポート形式については、[証明書のインポート](https://docs.aws.amazon.com/acm/latest/userguide/import-certificate-format.html) をご参照ください。

    :::

2. EMQX カスタムリソースの metadata に以下のようなアノテーションを追加します。

    ```yaml
    ## AWS Certificate Manager が管理する 1 つ以上の証明書の ARN を指定します。
    service.beta.kubernetes.io/aws-load-balancer-ssl-cert: arn:aws:acm:us-west-2:xxxxx:certificate/xxxxxxx
    ## ロードバランサーと Kubernetes ポッド間のバックエンドトラフィックに TLS を使用するかどうかを指定します。
    service.beta.kubernetes.io/aws-load-balancer-backend-protocol: tcp
    ## TLS リスナーを持つフロントエンドポートを指定します。これにより、AWS NLB サービス経由でポート 1883 にアクセスする際は TLS 認証が必要ですが、
    ## K8S サービスのポートに直接アクセスする場合は TLS 認証は不要です。
    service.beta.kubernetes.io/aws-load-balancer-ssl-ports: "1883"
    ```

    > `service.beta.kubernetes.io/aws-load-balancer-ssl-cert` の値は、ステップ 1 で控えた ARN 情報です。
