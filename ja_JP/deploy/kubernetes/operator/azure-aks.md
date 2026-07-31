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

   EMQX Operatorは`dashboardServiceTemplate`の設定に基づいて自動的にServiceを作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   20.245.230.91
   ```

4. ダッシュボードを`http://20.245.230.91:18083`で開きます。

    デフォルトの認証情報でログインしてください：

     - **ユーザー名:** `admin`
     - **パスワード:** `public`

## MQTTXを使ったサブスクライブとパブリッシュ

この手順では、開発者がMQTTサービスやアプリケーションを素早くテストできるオープンソースのMQTT 5.0コマンドラインクライアントツールである[MQTTX CLI](https://mqttx.app/cli)を使用します。

1. EMQX TCPリスナーの外部IPを取得します。

   EMQX Operatorは、設定された各リスナーに対してServiceリソースを自動的に作成します。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  接続中...
   [10:00:25] › ✔  接続完了
   [10:00:25] › …  helloにサブスクライブ中...
   [10:00:25] › ✔  helloにサブスクライブしました
   ```

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
