<<<<<<< HEAD
# Azure Kubernetes Service上でのEMQXのデプロイ

EMQX Operatorは、Azure Kubernetes Service（AKS）上でのEMQXのデプロイをサポートしています。AKSは、Azure上でマネージドKubernetesクラスターを簡単にデプロイできるようにし、運用の負荷をAzure側に委ねます。ホステッドKubernetesサービスとして、Azureはヘルスモニタリングやメンテナンスなどの重要なタスクを担当します。AKSクラスターが作成されると、Azureは追加費用なしでKubernetesコントロールプレーンのプロビジョニングと管理を自動的に行います。

## はじめる前に

AKS上でEMQXをデプロイする前に、以下の前提条件を満たしていることを確認してください。

- Azureサブスクリプション内にAKSクラスターがあること  
  * AKSクラスターの作成および設定については、[Azure Kubernetes Serviceドキュメント](https://learn.microsoft.com/en-us/azure/aks/)を参照してください。

- AKSクラスターに接続するための動作する`kubectl`設定  
  - ローカルにインストールされた`kubectl`を使用して接続する場合は、[AKSクラスターへの接続](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-cli)の手順に従ってください。  
  - Azure Cloud Shellを使用して接続する場合は、[Azure CloudShellでのAKSクラスター管理](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-portal?tabs=azure-cli)を参照してください。

- クラスターにEMQX Operatorがインストールされていること  
  - インストール手順については、[EMQX Operatorのインストール](./getting-started.md)を参照してください。
  

## EMQXクラスターの迅速なデプロイ

以下の例は、EMQXカスタムリソース（CR）の基本的な設定例です。

1. YAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
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

2. EMQXクラスターがReady状態になるまで待ちます。

   `kubectl get`コマンドでクラスターの状態を確認し、`STATUS`が`Ready`になっていることを確認してください。起動には時間がかかる場合があります。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m5s
   ```

3. EMQXダッシュボードの外部IPを取得し、アクセスします。
=======
# Azure Kubernetes Service に EMQX をデプロイする

EMQX Operator は Azure Kubernetes Service（AKS）上への EMQX のデプロイをサポートしています。AKS は、Azure 上でマネージド Kubernetes クラスターのデプロイを簡素化し、運用の負担を Azure に委ねることができます。ホステッド Kubernetes サービスとして、Azure はヘルスモニタリングやメンテナンスなどの重要なタスクを担当します。AKS クラスターを作成すると、コントロールプレーンが自動的に作成および構成されます。このコントロールプレーンは、ユーザーから抽象化されたマネージド Azure リソースとして無償で提供されます。ユーザーは AKS クラスターに接続されたノードの管理と料金のみを負担します。

## はじめに

以下の準備が必要です：

- Azure 上で AKS クラスターを作成するには、まず Azure サブスクリプションで AKS サービスを有効化する必要があります。詳細は [Azure Kubernetes Service](https://learn.microsoft.com/en-us/azure/aks/) のドキュメントをご参照ください。

- kubectl コマンドを使って AKS クラスターに接続するには、ローカルに kubectl ツールをインストールし、クラスターの KubeConfig を取得して接続します。あるいは、Azure ポータルの Cloud Shell を利用して kubectl でクラスターを管理することも可能です。
  - kubectl で AKS クラスターに接続するには、ローカルマシンに kubectl ツールをインストールし設定する必要があります。詳細は [Connect to an AKS cluster](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-cli) のドキュメントをご参照ください。
  - CloudShell を使って AKS クラスターに接続する場合は、Azure CloudShell を利用して kubectl でクラスターを管理します。詳細は [Manage an AKS cluster in Azure CloudShell](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-portal?tabs=azure-cli) のドキュメントをご参照ください。

- EMQX Operator のインストールについては、[Install EMQX Operator](./getting-started.md) をご覧ください。

## EMQX クラスターを素早くデプロイする

以下は EMQX カスタムリソースの関連設定例です。デプロイしたい EMQX のバージョンに応じて適切な APIVersion を選択してください。対応関係の詳細は [EMQX Operator Compatibility](./operator.md) をご参照ください。

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

EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターのステータスを確認できます。STATUS が `Running` になるまでには時間がかかる場合がありますのでご注意ください。

```bash
$ kubectl get emqx emqx
NAME   IMAGE                              STATUS    AGE
emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
```

EMQX クラスターの External IP を取得し、EMQX コンソールにアクセスします。

EMQX Operator は 2 つの EMQX Service リソースを作成します。1 つは EMQX コンソール用の `emqx-dashboard`、もう 1 つは EMQX のリスニングポート用の `emqx-listeners` です。
>>>>>>> origin/release-5.10

   EMQX Operatorは`dashboardServiceTemplate`の設定に基づいて自動的にServiceを作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   20.245.230.91
   ```

<<<<<<< HEAD
4. ダッシュボードを`http://20.245.230.91:18083`で開きます。

    デフォルトの認証情報でログインしてください：

     - **ユーザー名:** `admin`
     - **パスワード:** `public`

## MQTTXを使ったサブスクライブとパブリッシュ
=======
Web ブラウザで http://20.245.230.91:18083 にアクセスし、EMQX コンソールを開きます。初期のユーザー名とパスワードは `admin/public` です。

## MQTTX CLI を使って EMQX クラスターに接続しメッセージをパブリッシュ／サブスクライブする

[MQTTX CLI](https://mqttx.app/cli) は、GUI を使わずに MQTT サービスやアプリケーションの開発・デバッグを迅速に行うためのオープンソースの MQTT 5.0 コマンドラインクライアントツールです。

- EMQX クラスターの External IP を取得します。
>>>>>>> origin/release-5.10

この手順では、開発者がMQTTサービスやアプリケーションを素早くテストできるオープンソースのMQTT 5.0コマンドラインクライアントツールである[MQTTX CLI](https://mqttx.app/cli)を使用します。

<<<<<<< HEAD
1. EMQX TCPリスナーの外部IPを取得します。
=======
- メッセージをサブスクライブします。
>>>>>>> origin/release-5.10

   EMQX Operatorは、設定された各リスナーに対してServiceリソースを自動的に作成します。

<<<<<<< HEAD
   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。
=======
[10:00:25] › …  接続中...
[10:00:25] › ✔  接続完了
[10:00:25] › …  hello をサブスクライブ中...
[10:00:25] › ✔  hello をサブスクライブしました
```

- 新しいターミナルウィンドウを開き、メッセージを送信します。
>>>>>>> origin/release-5.10

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  接続中...
   [10:00:25] › ✔  接続完了
   [10:00:25] › …  helloにサブスクライブ中...
   [10:00:25] › ✔  helloにサブスクライブしました
   ```

<<<<<<< HEAD
3. 別のターミナルでEMQXクラスターに接続し、メッセージをパブリッシュします。

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

## LoadBalancerによるTLSオフロードについての注意点

L3/L4ロードバランサーであるAzure LoadBalancerはTLS終端をサポートしていません。可能な回避策については、こちらの[ディスカッション](https://github.com/emqx/emqx-operator/discussions/312)を参照してください。
=======
[10:00:58] › …  接続中...
[10:00:58] › ✔  接続完了
[10:00:58] › …  メッセージをパブリッシュ中...
[10:00:58] › ✔  メッセージをパブリッシュしました
```

- サブスクライブしているターミナルウィンドウで受信したメッセージを確認します。

```shell
[10:00:58] › ペイロード: hello world
```

## LoadBalancer による TLS オフロードについて

Azure LoadBalancer は TCP 証明書をサポートしていないため、TCP 証明書のオフロード問題を解決するにはこちらの[ドキュメント](https://github.com/emqx/emqx-operator/discussions/312)をご参照ください。
>>>>>>> origin/release-5.10
