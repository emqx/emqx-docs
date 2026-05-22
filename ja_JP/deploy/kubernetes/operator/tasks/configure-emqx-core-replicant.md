# Core + Replicant クラスターの有効化

## 目的

- `coreTemplate` フィールドを通じて EMQX クラスターの Core ノードを構成する。
- `replicantTemplate` フィールドを通じて EMQX クラスターの Replicant ノードを構成する。

## Core ノードと Replicant ノード

<<<<<<< HEAD
EMQX クラスター内のノードは、Core ノードまたは Replicant ノードのいずれかの役割を持ちます。
* Core ノードはクラスター内のデータ永続化を担当し、ルーティングテーブル、MQTT クライアントチャネル、保持メッセージ、クラスター構成、アラーム、ダッシュボードのユーザー認証情報などの共有クラスター状態の正式な情報源として機能します。
=======
EMQX クラスター内のノードは、Core ノードまたは Replicant ノードのいずれかの役割を持ちます。  
* Core ノードはクラスター内のデータ永続化を担当し、ルーティングテーブル、MQTT クライアントチャネル、保持メッセージ、クラスター設定、アラーム、ダッシュボードのユーザー認証情報などの共有クラスター状態の権威ある情報源として機能します。  
>>>>>>> origin/release-6.1
* Replicant ノードはステートレスとして設計されており、データベース操作には参加しません。Replicant ノードの追加や削除はクラスターのデータ冗長性に影響を与えません。

典型的な EMQX クラスターにおける Core ノードと Replicant ノード間の通信は、以下の図の通りです。

  <div style="text-align:center">
<<<<<<< HEAD
  <img src="./assets/configure-core-replicant/mria-core-replicant.png" style="zoom:30%;" alt="Core と Replicant ノード間の通信" />
=======
  <img src="./assets/configure-core-replicant/mria-core-replicant.png" style="zoom:30%;" alt="Core と Replicant ノードの通信図" />
>>>>>>> origin/release-6.1
  </div>

EMQX の Core-Replicant アーキテクチャの詳細については、[クラスターアーキテクチャ](../../../cluster/mria-introduction.md)ドキュメントを参照してください。

:::tip
<<<<<<< HEAD
EMQX クラスターには少なくとも 1 つの Core ノードが必要です。高可用性の観点から、EMQX Operator では EMQX クラスターに少なくとも 3 つの Core ノードを持つことを推奨しています。
=======
EMQX クラスターには最低でも 1 つの Core ノードが必要です。高可用性を確保するために、EMQX Operator では EMQX クラスターに最低 3 つの Core ノードを推奨しています。
>>>>>>> origin/release-6.1
:::

## EMQX クラスターの構成

<<<<<<< HEAD
EMQX CRD `apps.emqx.io/v2` では、`.spec.coreTemplate` フィールドを通じて EMQX クラスターの Core ノードを、`.spec.replicantTemplate` フィールドを通じて Replicant ノードを構成できます。
=======
EMQX CRD `apps.emqx.io/v2beta1` では、`.spec.coreTemplate` フィールドを通じて EMQX クラスターの Core ノードを、`.spec.replicantTemplate` フィールドを通じて Replicant ノードを構成できます。
>>>>>>> origin/release-6.1

1. 以下の内容を YAML ファイルとして保存し、`kubectl apply` でデプロイします。

   ```yaml
<<<<<<< HEAD
   apiVersion: apps.emqx.io/v2
=======
   apiVersion: apps.emqx.io/v2beta1
>>>>>>> origin/release-6.1
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
         replicas: 2
         resources:
           requests:
             cpu: 250m
             memory: 512Mi
     replicantTemplate:
       spec:
         replicas: 3
         resources:
           requests:
             cpu: 250m
             memory: 1Gi
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   上記の例では、EMQX CR は 2 つの Core ノードと 3 つの Replicant ノードからなる EMQX クラスターを定義しています。

<<<<<<< HEAD
   Core ノードは最低 512Mi のメモリを必要とし、Replicant ノードは最低 1Gi のメモリを必要とします。これらの制約は実際の業務負荷に応じて調整可能です。一般的に、Replicant ノードはすべてのクライアント要求を受け入れるため、同時接続数が多い場合は Replicant ノードのリソースを多めに確保することが望ましいです。

2. EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Ready` になっていることを確認してください。準備完了までに時間がかかる場合があります。
=======
   Core ノードは最低 512Mi のメモリが必要であり、Replicant ノードは最低 1Gi のメモリが必要です。これらのリソースは実際の業務負荷に応じて調整可能です。通常、Replicant ノードはすべてのクライアント要求を受け入れるため、多数の同時接続に対応するために Replicant ノードのリソースは多めに確保されることがあります。

2. EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Ready` になることを確認してください。準備完了までに時間がかかる場合があります。
>>>>>>> origin/release-6.1

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## EMQX クラスターの確認

EMQX CR の `.status` フィールドを確認することで、クラスター内のすべてのノード情報を閲覧できます。

```bash
$ kubectl get emqx emqx -o json | jq .status.coreNodes
[
  {
    "name": "emqx@emqx-core-adcdef012-0.emqx-headless.default.svc.cluster.local",
    "podName": "emqx-core-adcdef012-0",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "core",
    "version": "@EE_VERSION@",
    "sessions": 0,
    "connections": 0
  },
  {
    "name": "emqx@emqx-core-adcdef012-1.emqx-headless.default.svc.cluster.local",
    "podName": "emqx-core-adcdef012-1",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "core",
    "version": "@EE_VERSION@",
    "sessions": 0,
    "connections": 0
  }
]
```

```bash
$ kubectl get emqx emqx -o json | jq .status.replicantNodes
[
  {
    "name": "emqx@10.244.4.56",
    "podName": "emqx-replicant-adcdef012-0",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "replicant",
    "version": "@EE_VERSION@",
    "sessions": 42,
    "connections": 42
  },
  {
    "name": "emqx@10.244.4.57",
    "podName": "emqx-replicant-adcdef012-1",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "replicant",
    "version": "@EE_VERSION@",
    "sessions": 11,
    "connections": 11
  },
  {
    "name": "emqx@10.244.4.58",
    "podName": "emqx-replicant-adcdef012-2",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "replicant",
    "version": "@EE_VERSION@",
    "sessions": 13,
    "connections": 13
  }
]
```
