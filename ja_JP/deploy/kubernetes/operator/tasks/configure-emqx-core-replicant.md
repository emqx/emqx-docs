# Core + Replicant クラスターの有効化（EMQX 5.x）

## 対象タスク

- `coreTemplate` フィールドを使用して EMQX クラスターの Core ノードを構成する。
- `replicantTemplate` フィールドを使用して EMQX クラスターの Replicant ノードを構成する。

## Core ノードと Replicant ノード

:::tip
Core + Replicant クラスターは EMQX Enterprise Edition のみがサポートしています。
:::

EMQX 5.0 では、EMQX クラスター内のノードは Core ノードと Replicant ノードの2つの役割に分けられます。Core ノードはクラスター内のすべての書き込み操作を担当し、EMQX 4.x クラスターのノードの動作と一致します。Core ノードは EMQX データベース [Mria](https://github.com/emqx/mria) の実際のデータソースとして機能し、ルーティングテーブル、セッション、設定、アラーム、ダッシュボードのユーザー情報などのデータを格納します。一方、Replicant ノードはステートレスとして設計されており、データの書き込みには参加しません。Replicant ノードの追加や削除はクラスターのデータ冗長性に影響を与えません。EMQX 5.0 のアーキテクチャの詳細については、以下のドキュメントをご参照ください：[EMQX 5.0 Architecture](../../../cluster/mria-introduction.md)。Core ノードと Replicant ノードのトポロジ構造は以下の図の通りです。

  <div style="text-align:center">
  <img src="./assets/configure-core-replicant/mria-core-repliant.png" style="zoom:30%;" alt="CoreノードとReplicantノードのトポロジ構造" />
  </div>

:::tip
EMQX クラスターには最低1つの Core ノードが必要です。高可用性の観点から、EMQX Operator は EMQX クラスターに最低3つの Core ノードを推奨しています。
:::

## EMQX クラスターの構成

`apps.emqx.io/v2beta1 EMQX` は、`.spec.coreTemplate` フィールドを通じて EMQX クラスターの Core ノードを構成し、`.spec.replicantTemplate` フィールドを使用して Replicant ノードを構成できます。詳細は以下をご参照ください：[API Reference](../api-reference.md#emqxspec)。

+ 以下の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイします。

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

  > 上記 YAML では、Core ノード2台と Replicant ノード3台からなる EMQX クラスターを宣言しています。Core ノードは最低512Miのメモリが必要で、Replicant ノードは最低1Giのメモリが必要です。実際の業務負荷に応じて調整してください。実際の業務では Replicant ノードがすべてのクライアントリクエストを受け付けるため、Replicant ノードに必要なリソースはより多くなります。

+ EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Running` であることを確認してください。準備には時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ EMQX クラスターのダッシュボード外部IPを取得し、EMQX コンソールにアクセスします。

  EMQX Operator は2つの EMQX Service リソースを作成します。1つは emqx-dashboard、もう1つは emqx-listeners で、それぞれ EMQX コンソールと EMQX のリスニングポートに対応しています。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` を使って EMQX コンソールにログインしてください。

## EMQX クラスターの検証

  クラスター内のすべてのノード情報は、EMQX カスタムリソースの `.status` を確認することで取得できます。

  ```bash
  $ kubectl get emqx emqx -o json | jq .status.coreNodes
  [
    {
      "node": "emqx@emqx-core-0.emqx-headless.default.svc.cluster.local",
      "node_status": "running",
      "otp_release": "27.2-3/15.2",
      "role": "core",
      "version": "@EE_VERSION@"
    },
    {
      "node": "emqx@emqx-core-1.emqx-headless.default.svc.cluster.local",
      "node_status": "running",
      "otp_release": "27.2-3/15.2",
      "role": "core",
      "version": "@EE_VERSION@"
    },
     {
      "node": "emqx@emqx-core-2.emqx-headless.default.svc.cluster.local",
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
      "node": "emqx@10.244.4.56",
      "node_status": "running",
      "otp_release": "27.2-3/15.2",
      "role": "replicant",
      "version": "@EE_VERSION@"
    },
    {
      "node": "emqx@10.244.4.57",
      "node_status": "running",
      "otp_release": "27.2-3/15.2",
      "role": "replicant",
      "version": "@EE_VERSION@"
    },
    {
      "node": "emqx@10.244.4.58",
      "node_status": "running",
      "otp_release": "27.2-3/15.2",
      "role": "replicant",
      "version": "@EE_VERSION@"
    }
  ]
  ```
