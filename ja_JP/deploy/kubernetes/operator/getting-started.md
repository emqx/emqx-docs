# OperatorのインストールとEMQXのデプロイ

<<<<<<< HEAD
本セクションでは、EMQX Operatorの環境準備、Operatorのインストール、およびOperatorを使用したEMQXのデプロイ方法について説明します。以下の手順に従うことで、Operatorを使って効率的かつ安定的にEMQXをインストールおよび管理できます。
=======
本セクションでは、EMQX Operatorの環境を効率的に構築し、Operatorをインストールした後にEMQXをデプロイする手順についてご案内します。本セクションの手順に従うことで、EMQX Operatorを用いたEMQXの効果的なインストールおよび管理が可能になります。
>>>>>>> origin/release-5.10

## 環境の準備

EMQX Operatorをデプロイする前に、以下のコンポーネントが準備されていることを確認してください。

<<<<<<< HEAD
- Kubernetesバージョン1.24以上が稼働している[Kubernetes](https://kubernetes.io/docs/concepts/overview/)環境

- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでクラスターの状態を確認できます。
=======
- 稼働中の[Kubernetesクラスター](https://kubernetes.io/docs/concepts/overview/)。Kubernetesのバージョンについては[How to selector Kubernetes version](./operator.md#how-to-selector-kubernetes-version)をご参照ください。

- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでKubernetesクラスターの状態を確認できます。
>>>>>>> origin/release-5.10

- [Helm](https://helm.sh) 3以上

## EMQX Operatorのインストール

1. `cert-manager`をインストールして起動します。

   ::: tip
<<<<<<< HEAD
   `cert-manager`のバージョンは`1.1.6`以上が必要です。すでに`cert-manager`がインストールおよび起動している場合は、この手順をスキップしてください。
=======
   `cert-manager`のバージョンは`1.1.6`以上が必要です。すでに`cert-manager`がインストールおよび起動されている場合は、この手順をスキップしてください。
>>>>>>> origin/release-5.10
   :::

   Helmを使って`cert-manager`をインストールできます。

   ```bash
   $ helm repo add jetstack https://charts.jetstack.io
   $ helm repo update
   $ helm upgrade --install cert-manager jetstack/cert-manager \
     --namespace cert-manager \
     --create-namespace \
     --set crds.enabled=true
   ```

<<<<<<< HEAD
   または、公式の[cert-managerインストールガイド](https://cert-manager.io/docs/installation/)に従ってください。

=======
   または、[cert-managerインストールガイド](https://cert-manager.io/docs/installation/)に従ってインストールしてください。

   ::: warning
   Google Kubernetes Engine（GKE）にデフォルト設定でcert-managerをインストールすると、ブートストラップの問題が発生する可能性があります。そのため、`--set global.leaderElection.namespace=cert-manager`の設定を追加し、リーダー選出に別のネームスペースを使用するように構成してください。詳細は[cert-manager互換性](https://cert-manager.io/docs/installation/compatibility/)をご確認ください。
   :::

>>>>>>> origin/release-5.10
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

<<<<<<< HEAD
Operatorが起動したら、EMQXのデプロイに進めます。

=======
Operatorのインストールが完了したので、次のステップに進む準備が整いました。[Deploy EMQX](#deploy-emqx)セクションでは、EMQX Operatorを使ってEMQXをデプロイする方法を学べます。

また、Operatorを使ったEMQXのアップグレードやアンインストール方法に興味がある場合は、本セクションの続きをご覧ください。

>>>>>>> origin/release-5.10
## EMQXのデプロイ

:::: tabs type:card

::: tab EMQX Enterprise 5

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx-ee
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       data: |
         license {
           key = "..."
         }
   ```

<<<<<<< HEAD
   EMQX CRDの詳細については、[リファレンスドキュメント](./reference/v2beta1-reference.md)をご参照ください。

2. EMQXクラスターが準備完了になるまで待ちます。
=======
   EMQX CRDの詳細については、[リファレンスドキュメント](./api-reference.md)をご参照ください。

2. EMQXクラスターが稼働するまで待ちます。
>>>>>>> origin/release-5.10

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx-ee   Ready     2m55s
   ```

<<<<<<< HEAD
   `STATUS`が`Ready`になっていることを確認してください。EMQXクラスターが準備完了になるまでには時間がかかる場合があります。
=======
   `STATUS`が`Running`になっていることを確認してください。EMQXクラスターの準備には時間がかかる場合があります。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
   EMQX CRDの詳細については、[リファレンスドキュメント](./reference/v2beta1-reference.md)をご参照ください。

2. EMQXクラスターが準備完了になるまで待ちます。
=======
   EMQX CRDの詳細については、[リファレンスドキュメント](./api-reference.md)をご参照ください。

2. EMQXクラスターが稼働するまで待ちます。
>>>>>>> origin/release-5.10

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

<<<<<<< HEAD
   `STATUS`が`Ready`になっていることを確認してください。EMQXクラスターが準備完了になるまでには時間がかかる場合があります。内部で多くの処理が行われています。
=======
   `STATUS`が`Running`になっていることを確認してください。EMQXクラスターの準備には時間がかかる場合があります。
>>>>>>> origin/release-5.10

:::

::::

## パブリッククラウドへのデプロイ

<<<<<<< HEAD
EMQX Operatorを使用してマネージドKubernetesサービス上にEMQXをデプロイするには、以下のガイドをご利用ください。
=======
EMQX Operatorを使用してパブリッククラウドプラットフォームにEMQXをデプロイするには、以下のガイドをご参照ください。
>>>>>>> origin/release-5.10

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
