# OperatorのインストールとEMQXのデプロイ

本セクションでは、EMQX Operatorの環境準備、Operatorのインストール、およびOperatorを使用したEMQXのデプロイ方法について説明します。以下の手順に従うことで、Operatorを使ってEMQXを効率的かつ安定的にインストールおよび管理できます。

## 環境の準備

EMQX Operatorをデプロイする前に、以下のコンポーネントが準備されていることを確認してください。

- Kubernetesバージョン1.24以上が稼働している[Kubernetes](https://kubernetes.io/docs/concepts/overview/)環境。

- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでKubernetesクラスターの状態を確認できます。

## EMQX Operatorのインストール

1. 以下のコマンドでEMQX Operatorをインストールします。

   ```bash
   $ kubectl apply --server-side=true -f https://github.com/emqx/emqx-operator/releases/latest/download/install.yaml
   ```

   このコマンドは最新の2.3.xリリースをダウンロードし、クラスター全体のEMQX CRDをインストールし、`emqx-operator-system`という別のネームスペースにコントローラーサービスをデプロイします。

2. EMQX Operatorが準備完了になるまで待ちます。

   ```bash
   $ kubectl wait --for=condition=Ready pods --namespace emqx-operator-system -l "control-plane=controller-manager"
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operatorが稼働したら、EMQXのデプロイに進めます。

## EMQXのデプロイ

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

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
   ```

   EMQX CRDの詳細については、[リファレンスドキュメント](./reference/v2beta1-reference.md)を参照してください。

2. EMQXクラスターが準備完了になるまで待ちます。

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   `STATUS`が`Ready`となっていることを確認してください。EMQXクラスターが準備完了になるまでには時間がかかる場合があります。

## トラブルシューティング

EMQX OperatorはKubernetes APIに対して限定的なイベントのみを公開しています。

```sh
kubectl get events --sort-by=.lastTimestamp
```

また、EMQXリソースが`Ready`ステータス条件に達しない場合は、コントローラーマネージャーのログを確認してください。

```sh
kubectl logs -l "control-plane=controller-manager" --tail=-1 --namespace emqx-operator-system
```

## パブリッククラウドでのデプロイ

EMQX Operatorを使用してマネージドKubernetesサービス上にEMQXをデプロイするためのガイドは以下をご参照ください。

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
