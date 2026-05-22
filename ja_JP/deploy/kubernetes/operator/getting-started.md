# OperatorのインストールとEMQXのデプロイ

本セクションでは、EMQX Operatorの環境準備、Operatorのインストール、およびOperatorを使用したEMQXのデプロイ方法について説明します。以下の手順に従うことで、Operatorを使って効率的かつ安定的にEMQXをインストールおよび管理できます。

## 環境の準備

EMQX Operatorをデプロイする前に、以下のコンポーネントが準備されていることを確認してください。

<<<<<<< HEAD
- Kubernetesバージョン1.24以上が稼働している[Kubernetes](https://kubernetes.io/docs/concepts/overview/)環境。

- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでKubernetesクラスターの状態を確認できます。

## EMQX Operatorのインストール

1. 以下のコマンドでEMQX Operatorをインストールします。
=======
- Kubernetesバージョン1.24以上が稼働している[Kubernetes](https://kubernetes.io/docs/concepts/overview/)環境

- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでクラスターの状態を確認できます。

- [Helm](https://helm.sh) 3以上

## EMQX Operatorのインストール

1. `cert-manager`をインストールして起動します。

   ::: tip
   `cert-manager`のバージョンは`1.1.6`以上が必要です。すでに`cert-manager`がインストールおよび起動している場合は、この手順をスキップしてください。
   :::

   Helmを使って`cert-manager`をインストールできます。
>>>>>>> origin/release-6.1

   ```bash
   $ kubectl apply --server-side=true -f https://github.com/emqx/emqx-operator/releases/latest/download/install.yaml
   ```

<<<<<<< HEAD
   このコマンドは最新の2.3.xリリースをダウンロードし、クラスター全体にEMQXのCRDをインストールし、`emqx-operator-system`という別のネームスペースにコントローラーサービスをデプロイします。

2. EMQX Operatorが準備完了になるまで待ちます。

   ```bash
   $ kubectl wait --for=condition=Ready pods --namespace emqx-operator-system -l "control-plane=controller-manager"
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operatorが稼働したら、EMQXのデプロイに進めます。

## EMQXのデプロイ

=======
   または、公式の[cert-managerインストールガイド](https://cert-manager.io/docs/installation/)に従ってください。

2. 以下のコマンドでEMQX Operatorをインストールします。

   ```bash
   $ helm repo add emqx https://repos.emqx.io/charts
   $ helm repo update
   $ helm upgrade --install emqx-operator emqx/emqx-operator \
     --namespace emqx-operator-system \
     --create-namespace
   ```

3. EMQX Operatorが準備完了になるまで待ちます。

   ```bash
   $ kubectl wait --for=condition=Ready pods -l "control-plane=controller-manager" -n emqx-operator-system
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operatorが起動したら、EMQXのデプロイに進めます。

## EMQXのデプロイ

:::: tabs type:card

::: tab EMQX Enterprise 5

>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
   EMQXのCRDの詳細については、[リファレンスドキュメント](./reference/v2beta1-reference.md)をご参照ください。

2. EMQXクラスターが準備完了になるまで待ちます。
=======
   EMQX CRDの詳細については、[リファレンスドキュメント](./reference/v2beta1-reference.md)をご参照ください。

2. EMQXクラスターが準備完了になるまで待ちます。

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx-ee   Ready     2m55s
   ```

   `STATUS`が`Ready`になっていることを確認してください。EMQXクラスターが準備完了になるまでには時間がかかる場合があります。

:::

::: tab EMQX Open Source 5

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx:@CE_VERSION@
   ```

   EMQX CRDの詳細については、[リファレンスドキュメント](./reference/v2beta1-reference.md)をご参照ください。

2. EMQXクラスターが準備完了になるまで待ちます。
>>>>>>> origin/release-6.1

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

<<<<<<< HEAD
   `STATUS`が`Ready`であることを確認してください。EMQXクラスターが準備完了になるまでに時間がかかる場合があります。
=======
   `STATUS`が`Ready`になっていることを確認してください。EMQXクラスターが準備完了になるまでには時間がかかる場合があります。内部で多くの処理が行われています。
>>>>>>> origin/release-6.1

## トラブルシューティング

EMQX OperatorはKubernetes APIに対して限定的なイベントのみを公開しています。

<<<<<<< HEAD
```sh
kubectl get events --sort-by=.lastTimestamp
```

または、EMQXリソースが`Ready`状態にならない場合は、コントローラーマネージャーのログを確認してください。

```sh
kubectl logs -l "control-plane=controller-manager" --tail=-1 --namespace emqx-operator-system
```

## パブリッククラウドへのデプロイ

EMQX Operatorを使用してマネージドKubernetesサービス上にEMQXをデプロイする際は、以下のガイドをご利用ください。
=======
## パブリッククラウドへのデプロイ

EMQX Operatorを使用してマネージドKubernetesサービス上にEMQXをデプロイするには、以下のガイドをご利用ください。
>>>>>>> origin/release-6.1

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
