# Core + Replicant クラスターの有効化

## 目的

- `coreTemplate` フィールドを通じて EMQX クラスターの Core ノードを設定します。
- `replicantTemplate` フィールドを通じて EMQX クラスターの Replicant ノードを設定します。

## Core ノードと Replicant ノード

EMQX クラスター内のノードは、Core ノードまたは Replicant ノードのいずれかの役割を持ちます。  
* Core ノードはクラスター内のデータ永続化を担当し、ルーティングテーブル、MQTT クライアントチャネル、保持メッセージ、クラスター設定、アラーム、ダッシュボードのユーザー認証情報などの共有クラスター状態の権威あるソースとして機能します。  
* Replicant ノードはステートレスとして設計されており、データベース操作には参加しません。Replicant ノードの追加や削除はクラスターのデータ冗長性に影響を与えません。

典型的な EMQX クラスターにおける Core ノードと Replicant ノード間の通信は、以下の図のように示されます。

  <div style="text-align:center">
  <img src="./assets/configure-core-replicant/mria-core-replicant.png" style="zoom:30%;" alt="Core ノードと Replicant ノードの通信図" />
  </div>

EMQX の Core-Replicant アーキテクチャの詳細については、[クラスターアーキテクチャ](../../../../../develop/cluster/mria-introduction.md)ドキュメントをご参照ください。

:::tip
EMQX クラスターには少なくとも 1 つの Core ノードが必要です。高可用性の観点から、EMQX Operator では EMQX クラスターに少なくとも 3 つの Core ノードを持つことを推奨しています。
:::

## EMQX クラスターの設定

EMQX CRD `apps.emqx.io/v2beta1` では、`.spec.coreTemplate` フィールドを通じて EMQX クラスターの Core ノードを設定でき、`.spec.replicantTemplate` フィールドを通じて Replicant ノードを設定できます。

1. 以下の内容を YAML ファイルとして保存し、`kubectl apply` でデプロイします。

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

   Core ノードは最低 512Mi のメモリが必要であり、Replicant ノードは最低 1Gi のメモリが必要です。これらの制約は実際の業務負荷に応じて調整可能です。通常、Replicant ノードはすべてのクライアントリクエストを受け入れるため、多数の同時接続に対応するために Replicant ノードのリソースは多めに設定されることがあります。

2. EMQX クラスターが準備完了状態になるまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Ready` となっていることを確認してください。準備完了までに時間がかかる場合があります。

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
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "core",
    "version": "@EE_VERSION@"
  },
  {
    "name": "emqx@emqx-core-adcdef012-1.emqx-headless.default.svc.cluster.local",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "core",
    "version": "@EE_VERSION@"
  }
]
```

```bash
$ kubectl get emqx emqx -o json | jq .status.replicantNodes
[
  {
    "name": "emqx@10.244.4.56",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "replicant",
    "version": "@EE_VERSION@"
  },
  {
    "name": "emqx@10.244.4.57",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "replicant",
    "version": "@EE_VERSION@"
  },
  {
    "name": "emqx@10.244.4.58",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "replicant",
    "version": "@EE_VERSION@"
  }
]
```
