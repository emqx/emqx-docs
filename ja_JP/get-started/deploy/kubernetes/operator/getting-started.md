# OperatorのインストールとEMQXのデプロイ

本セクションでは、EMQX Operatorの環境準備、Operatorのインストール、およびOperatorを使用したEMQXのデプロイ方法について説明します。以下の手順に従うことで、Operatorを使って効率的かつ安定的にEMQXをインストールおよび管理できます。

## 環境の準備

EMQX Operatorをデプロイする前に、以下のコンポーネントが準備されていることを確認してください。

- バージョン1.27以降の[ Kubernetes](https://kubernetes.io/docs/concepts/overview/)クラスターで、`StatefulSetAutoDeletePVC`機能ゲートが有効になっていること。この機能ゲートは対応するKubernetesバージョンではデフォルトで有効です。
- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでKubernetesクラスターの状態を確認できます。

## EMQX Operatorのインストール

1. 以下のコマンドでEMQX Operatorをインストールします。

   ```bash
   $ kubectl apply --server-side=true -f https://github.com/emqx/emqx-operator/releases/latest/download/install.yaml
   ```

   このコマンドは最新のEMQX Operatorリリースをダウンロードし、クラスター全体のEMQX CRDをインストールし、`emqx-operator-system`ネームスペースにコントローラーをデプロイします。

   または、EMQX Operator Helmチャートを使ってインストールすることも可能です。

   ```bash
   helm repo add emqx https://repos.emqx.io/charts
   helm repo update
   helm install emqx-operator emqx/emqx-operator \
     --version 3.0.0 \
     --namespace emqx-operator-system \
     --create-namespace
   ```

   このチャートはEMQX CRDとコントローラーをインストールします。マニフェストまたはHelmのいずれかのインストール方法を選択し、次に示す準備完了の確認に進んでください。

2. EMQX Operatorが準備完了になるまで待ちます。

   ```bash
   $ kubectl wait --for=condition=Ready pods --namespace emqx-operator-system -l "control-plane=controller-manager"
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operatorが稼働したら、EMQXのデプロイに進めます。

## EMQXのデプロイ

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

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
           key: "evaluation"
   ```

   このマニフェストはシングルノードのEMQXクラスターをデプロイします。これはデフォルトのCommunity Licenseでサポートされています。複数ノードの構成や既存クラスターのスケールアウトを行う場合は、クラスタリングをサポートするライセンスを設定してください。本例では`license.key`を`"evaluation"`に設定しています。

   EMQX CRDの詳細については、[リファレンスドキュメント](./reference/v3beta1-reference.md)をご参照ください。

2. EMQXクラスターが準備完了になるまで待ちます。

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   `STATUS`が`Ready`になっていることを確認してください。EMQXクラスターが準備完了になるまでに時間がかかる場合があります。

## トラブルシューティング

EMQX OperatorはKubernetes APIに対して限定的なイベントのみを公開しています。

```sh
kubectl get events --sort-by=.lastTimestamp
```

または、EMQXリソースが`Ready`状態にならない場合は、コントローラーマネージャーのログを確認してください。

```sh
kubectl logs -l "control-plane=controller-manager" --tail=-1 --namespace emqx-operator-system
```

## パブリッククラウドでのデプロイ

EMQX Operatorを使ってマネージドKubernetesサービス上にEMQXをデプロイする場合は、以下のガイドをご参照ください。

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
