# Azure Kubernetes Service 上に EMQX をデプロイする

EMQX Operator は Azure Kubernetes Service（AKS）上への EMQX デプロイをサポートしています。AKS は、Azure 上でマネージド Kubernetes クラスターのデプロイを簡素化し、運用の負荷を Azure に委ねます。ホスト型 Kubernetes サービスとして、Azure はヘルスモニタリングやメンテナンスなどの重要なタスクを担当します。AKS クラスターを作成すると、コントロールプレーンが自動的に作成・構成されます。このコントロールプレーンはユーザーから抽象化されたマネージド Azure リソースとして無償で提供されます。ユーザーは AKS クラスターに接続されたノードのみを管理し、費用を支払います。

## はじめに

以下の準備が必要です。

- Azure 上で AKS クラスターを作成するには、まず Azure サブスクリプションで AKS サービスを有効化する必要があります。詳細は [Azure Kubernetes Service](https://learn.microsoft.com/en-us/azure/aks/) のドキュメントを参照してください。

- kubectl コマンドで AKS クラスターに接続するには、ローカルに kubectl ツールをインストールし、クラスターの KubeConfig を取得して接続します。あるいは、Azure ポータルの Cloud Shell を使って kubectl でクラスターを管理することも可能です。
  - kubectl で AKS クラスターに接続するには、ローカルマシンに kubectl ツールをインストール・設定する必要があります。詳細は [Connect to an AKS cluster](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-cli) のドキュメントを参照してください。
  - CloudShell を使って AKS クラスターに接続する場合は、Azure CloudShell を利用して kubectl でクラスターを管理します。詳細は [Manage an AKS cluster in Azure CloudShell](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-portal?tabs=azure-cli) のドキュメントを参照してください。

- EMQX Operator のインストールについては、[Install EMQX Operator](./getting-started.md) をご覧ください。

## EMQX クラスターの迅速なデプロイ

以下は EMQX カスタムリソースの関連設定例です。デプロイしたい EMQX のバージョンに応じて対応する APIVersion を選択してください。対応関係の詳細は [EMQX Operator Compatibility](./operator.md) を参照してください。

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
        ## ストレージクラスの詳細：https://learn.microsoft.com/en-us/azure/aks/concepts-storage#storage-classes
        storageClassName: default
        resources:
          requests:
            storage: 10Gi
        accessModes:
        - ReadWriteOnce
  dashboardServiceTemplate:
    spec:
      ## ロードバランサーの詳細：https://learn.microsoft.com/en-us/azure/aks/load-balancer-standard
      type: LoadBalancer
  listenersServiceTemplate:
    spec:
      ## ロードバランサーの詳細：https://learn.microsoft.com/en-us/azure/aks/load-balancer-standard
      type: LoadBalancer
```

EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認できます。STATUS が `Running` になるまで時間がかかる場合がありますのでご注意ください。

```bash
$ kubectl get emqx emqx
NAME   IMAGE                              STATUS    AGE
emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
```

EMQX クラスターの External IP を取得し、EMQX コンソールにアクセスします。

EMQX Operator は EMQX の Service リソースを2つ作成します。1つは EMQX コンソール用の `emqx-dashboard`、もう1つは EMQX のリスニングポート用の `emqx-listeners` です。

```shell
$ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

20.245.230.91
```

ブラウザで http://20.245.230.91:18083 にアクセスし、EMQX コンソールを開きます。デフォルトのユーザー名とパスワードは `admin/public` です。

## MQTTX CLI を使って EMQX クラスターに接続しメッセージをパブリッシュ／サブスクライブする

[MQTTX CLI](https://mqttx.app/cli) は、GUI を使わずに MQTT サービスやアプリケーションの開発・デバッグを高速化するためのオープンソースの MQTT 5.0 コマンドラインクライアントツールです。

- EMQX クラスターの External IP を取得します。

```shell
external_ip=$(kubectl get svc emqx -o json | jq '.status.loadBalancer.ingress[0].ip')
```

- メッセージをサブスクライブします。

```shell
$ mqttx sub -t 'hello' -h ${external_ip} -p 1883

[10:00:25] › …  接続中...
[10:00:25] › ✔  接続完了
[10:00:25] › …  hello をサブスクライブ中...
[10:00:25] › ✔  hello のサブスクライブ完了
```

- 新しいターミナルウィンドウを開き、メッセージを送信します。

```shell
$ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'

[10:00:58] › …  接続中...
[10:00:58] › ✔  接続完了
[10:00:58] › …  メッセージをパブリッシュ中...
[10:00:58] › ✔  メッセージのパブリッシュ完了
```

- サブスクライブしているターミナルウィンドウで受信したメッセージを確認します。

```shell
[10:00:58] › ペイロード: hello world
```

## LoadBalancer による TLS オフロードについて

Azure LoadBalancer は TCP 証明書をサポートしていないため、TCP 証明書のオフロード問題についてはこの[ドキュメント](https://github.com/emqx/emqx-operator/discussions/312)を参照してください。
