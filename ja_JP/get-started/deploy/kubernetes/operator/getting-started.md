# OperatorのインストールとEMQXのデプロイ

本セクションでは、EMQX Operatorの環境を効率的に構築し、Operatorをインストールした後にEMQXをデプロイする手順についてご案内します。本セクションの手順に従うことで、EMQX Operatorを用いたEMQXの効果的なインストールおよび管理が可能になります。

## 環境の準備

EMQX Operatorをデプロイする前に、以下のコンポーネントが準備されていることを確認してください。

- 稼働中の[Kubernetesクラスター](https://kubernetes.io/docs/concepts/overview/)。Kubernetesのバージョンについては[How to selector Kubernetes version](./operator.md#how-to-selector-kubernetes-version)をご参照ください。

- Kubernetesクラスターにアクセス可能な[kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)ツール。`kubectl cluster-info`コマンドでKubernetesクラスターの状態を確認できます。

- [Helm](https://helm.sh) 3以上

## EMQX Operatorのインストール

1. `cert-manager`をインストールして起動します。

   ::: tip
   `cert-manager`のバージョンは`1.1.6`以上が必要です。すでに`cert-manager`がインストールおよび起動されている場合は、この手順をスキップしてください。
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

   または、[cert-managerインストールガイド](https://cert-manager.io/docs/installation/)に従ってインストールしてください。

   ::: warning
   Google Kubernetes Engine（GKE）にデフォルト設定でcert-managerをインストールすると、ブートストラップの問題が発生する可能性があります。そのため、`--set global.leaderElection.namespace=cert-manager`の設定を追加し、リーダー選出に別のネームスペースを使用するように構成してください。詳細は[cert-manager互換性](https://cert-manager.io/docs/installation/compatibility/)をご確認ください。
   :::

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

Operatorのインストールが完了したので、次のステップに進む準備が整いました。[Deploy EMQX](#deploy-emqx)セクションでは、EMQX Operatorを使ってEMQXをデプロイする方法を学べます。

また、Operatorを使ったEMQXのアップグレードやアンインストール方法に興味がある場合は、本セクションの続きをご覧ください。

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
     image: emqx/emqx-enterprise:@EE_VERSION@
     config:
       data: |
         license {
           key = "..."
         }
   ```

   EMQX CRDの詳細については、[リファレンスドキュメント](./api-reference.md)をご参照ください。

2. EMQXクラスターが稼働するまで待ちます。

   ```bash
   $ kubectl get emqx

   NAME      IMAGE                              STATUS    AGE
   emqx-ee   emqx/emqx-enterprise:@EE_VERSION@  Running   2m55s
   ```

   `STATUS`が`Running`になっていることを確認してください。EMQXクラスターの準備には時間がかかる場合があります。

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

   EMQX CRDの詳細については、[リファレンスドキュメント](./api-reference.md)をご参照ください。

2. EMQXクラスターが稼働するまで待ちます。

   ```bash
   $ kubectl get emqx

   NAME   IMAGE                   STATUS    AGE
   emqx   emqx/emqx:@CE_VERSION@  Running   2m55s
   ```

   `STATUS`が`Running`になっていることを確認してください。EMQXクラスターの準備には時間がかかる場合があります。

:::

::::

## パブリッククラウドへのデプロイ

EMQX Operatorを使用してパブリッククラウドプラットフォームにEMQXをデプロイするには、以下のガイドをご参照ください。

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
