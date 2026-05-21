# Google Kubernetes Engine に EMQX をデプロイする

EMQX Operator を使用すると、Google Kubernetes Engine（GKE）上に EMQX をデプロイできます。これにより、GCP 上でマネージド Kubernetes クラスターを簡単にデプロイできるようになります。GKE を利用することで、運用のオーバーヘッドを GCP に委ね、アプリケーションのデプロイと管理に専念できます。EMQX を GKE 上にデプロイすることで、Kubernetes のスケーラビリティと柔軟性を活かしつつ、マネージドサービスのシンプルさと利便性を享受できます。EMQX Operator と GKE を組み合わせることで、クラウド上で MQTT ブローカーを簡単にデプロイおよび管理でき、ビジネスの目標に集中できます。

## はじめに

以下の準備が必要です。

- Google Cloud Platform 上で GKE クラスターを作成するには、GCP サブスクリプションで GKE サービスを有効にする必要があります。詳細は Google Kubernetes Engine のドキュメントをご参照ください。

- kubectl コマンドを使って GKE クラスターに接続するには、ローカルマシンに kubectl ツールをインストールし、クラスターの KubeConfig を取得して接続します。あるいは、GCP コンソールの Cloud Shell を利用して kubectl でクラスターを管理することも可能です。

  - kubectl を使って GKE クラスターに接続するには、ローカルマシンに kubectl ツールをインストールし設定する必要があります。詳細は [Connect to a GKE cluster](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-access-for-kubectl) のドキュメントをご覧ください。

  - Cloud Shell を使って GKE クラスターに接続する場合は、GCP コンソールの Cloud Shell から直接接続し、kubectl でクラスターを管理できます。詳細は [Manage a GKE cluster with Cloud Shell](https://cloud.google.com/code/docs/shell/create-configure-gke-cluster) のドキュメントをご参照ください。

- EMQX Operator のインストールについては、[Install EMQX Operator](./getting-started.md) をご覧ください。

## EMQX クラスターを素早くデプロイする

以下は EMQX カスタムリソースの関連設定例です。デプロイしたい EMQX のバージョンに応じて、対応する APIVersion を選択してください。詳細な互換性については [EMQX Operator Compatibility](./operator.md) をご参照ください。

  ::: warning
  CPU とメモリのリソースリクエストを行う場合、CPU は 250m 以上、メモリは 512M 以上である必要があります。

  - [Autopilot におけるリソースリクエスト](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-resource-requests)
    :::

以下の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイしてください。

```yaml
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

EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認できます。STATUS が `Running` になるまでしばらく時間がかかる場合がありますのでご注意ください。

```bash
$ kubectl get emqx emqx
NAME   IMAGE                              STATUS    AGE
emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
```

EMQX クラスターの External IP を取得し、EMQX コンソールにアクセスします。

EMQX Operator は、EMQX コンソール用の `emqx-dashboard` と、EMQX リスニングポート用の `emqx-listeners` の2つの EMQX Service リソースを作成します。

```shell
$ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

34.122.174.166
```

Web ブラウザで http://34.122.174.166:18083 にアクセスし、デフォルトのユーザー名とパスワード `admin/public` でログインしてください。

## MQTTX CLI を使って EMQX クラスターに接続しメッセージをパブリッシュ／サブスクライブする

[MQTTX CLI](https://mqttx.app/cli) は、開発者が GUI を使わずに MQTT サービスやアプリケーションの開発・デバッグを迅速に行うためのオープンソースの MQTT 5.0 コマンドラインクライアントツールです。

- EMQX クラスターの External IP を取得

```shell
external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
```

- メッセージをサブスクライブする

```shell
$ mqttx sub -t 'hello' -h ${external_ip} -p 1883

[10:00:25] › …  接続中...
[10:00:25] › ✔  接続完了
[10:00:25] › …  hello をサブスクライブ中...
[10:00:25] › ✔  hello のサブスクライブ完了
```

- 新しいターミナルウィンドウを開き、メッセージを送信する

```shell
$ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'

[10:00:58] › …  接続中...
[10:00:58] › ✔  接続完了
[10:00:58] › …  メッセージをパブリッシュ中...
[10:00:58] › ✔  メッセージのパブリッシュ完了
```

- サブスクライブ側のターミナルウィンドウで受信したメッセージを確認

```shell
[10:00:58] › payload: hello world
```

## TLS オフロードに LoadBalancer を使用する

Google のロードバランサーは TCP 証明書をサポートしていないため、TCP 証明書のオフロード問題については [discussion](https://github.com/emqx/emqx-operator/discussions/312) をご確認ください。
