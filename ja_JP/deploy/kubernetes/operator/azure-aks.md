# Azure Kubernetes Service 上への EMQX デプロイ

EMQX Operator は Azure Kubernetes Service（AKS）上への EMQX デプロイをサポートしています。AKS は、Azure におけるマネージド Kubernetes クラスターのデプロイを簡素化し、運用の負担を Azure に委ねます。ホスト型 Kubernetes サービスとして、Azure はヘルスモニタリングやメンテナンスなどの重要なタスクを担当します。AKS クラスターが作成されると、Azure は Kubernetes コントロールプレーンを自動的にプロビジョニングおよび管理し、追加費用は発生しません。

## はじめに

AKS 上に EMQX をデプロイする前に、以下の前提条件を満たしていることを確認してください。

- Azure サブスクリプション内に AKS クラスターが存在すること  
  * AKS クラスターの作成および設定については、[Azure Kubernetes Service ドキュメント](https://learn.microsoft.com/en-us/azure/aks/)を参照してください。

- AKS クラスターに接続するための `kubectl` 設定が有効であること  
  - ローカルにインストールした `kubectl` で接続する場合は、[AKS クラスターへの接続](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-cli)の手順に従ってください。  
  - Azure Cloud Shell を利用する場合は、[Azure CloudShell での AKS クラスター管理](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-portal?tabs=azure-cli)を参照してください。

- クラスターに EMQX Operator がインストールされていること  
  - インストール方法については、[EMQX Operator のインストール](./getting-started.md)を参照してください。
  

## EMQX クラスターの迅速なデプロイ

以下は EMQX カスタムリソース（CR）の基本的な設定例です。

1. YAML ファイルとして保存し、`kubectl apply` でデプロイします。

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

2. EMQX クラスターが Ready 状態になるまで待ちます。

   `kubectl get` コマンドでクラスターの状態を確認し、`STATUS` が `Ready` であることを確認してください。起動には時間がかかる場合があります。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m5s
   ```

3. EMQX ダッシュボードの外部 IP を取得し、アクセスします。

   EMQX Operator は `dashboardServiceTemplate` の設定に基づき、Service を自動作成します。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   20.245.230.91
   ```

4. ダッシュボードに `http://20.245.230.91:18083` でアクセスします。

    以下のデフォルト認証情報でログインしてください：

     - **ユーザー名:** `admin`
     - **パスワード:** `public`

## MQTTX を使ったサブスクライブとパブリッシュ

この手順では、開発者が MQTT サービスやアプリケーションを素早くテストできるオープンソースの MQTT 5.0 コマンドラインクライアントツールである [MQTTX CLI](https://mqttx.app/cli) を使用します。

1. EMQX TCP リスナーの外部 IP を取得します。

   EMQX Operator は設定された各リスナーに対して Service リソースを自動作成します。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. トピックにサブスクライブします。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  接続中...
   [10:00:25] › ✔  接続完了
   [10:00:25] › …  hello にサブスクライブ中...
   [10:00:25] › ✔  hello にサブスクライブ完了
   ```

3. 別のターミナルで EMQX クラスターに接続し、メッセージをパブリッシュします。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   [10:00:58] › …  接続中...
   [10:00:58] › ✔  接続完了
   [10:00:58] › …  メッセージをパブリッシュ中...
   [10:00:58] › ✔  メッセージパブリッシュ完了
   ```

4. サブスクライバーがメッセージを受信する様子を確認します。

   ```shell
   [10:00:58] › payload: hello world
   ```

## LoadBalancer による TLS オフロードについての注意点

L3/L4 ロードバランサーである Azure LoadBalancer は TLS 終端をサポートしていません。可能な回避策については、こちらの[ディスカッション](https://github.com/emqx/emqx-operator/discussions/312)を参照してください。
