# EMQX クラスターのローリングアップデートを実行する

EMQX Operator 3.0 を使用して、EMQX クラスターのグレースフルなローリングアップデートを実行します。

## はじめに

- [EMQX Operator 3.0 をインストール](../getting-started.md)し、`kubectl` を設定して Kubernetes クラスターにアクセスできるようにします。
- Kubernetes 環境が `LoadBalancer` サービス用の外部アドレスをプロビジョニングできることを確認してください。
- 同時接続数が少なくとも3,000をサポートする有効な EMQX Enterprise ライセンスを用意してください。この例のライセンスプレースホルダーをライセンスキーに置き換えます。詳細は [ライセンス管理](./configure-emqx-license.md) を参照してください。
- ノードの避難状況を確認するために `jq` をインストールしてください。
- テスト接続を生成するために [MQTTX CLI](https://mqttx.app/cli) をインストールしてください。
- Core-Replicant クラスターの場合、Core レプリカを最低2つ設定してください。EMQX Operator は Core レプリカが2未満の Core-Replicant 構成を拒否します。

## ローリングアップデートの仕組み

EMQX Operator は、EMQX カスタムリソースの Pod テンプレート（イメージ、イメージプルポリシー、リソース要求、Core および Replicant テンプレートなど）に変更があった場合にローリングアップデートを実行します。ロールアウトは以下の段階で進行します。

1. **Core ノードの更新：** Operator は同じ StatefulSet 内の Core Pod を1つずつ更新します。各 Pod を新しいテンプレートで再作成し、Pod が準備完了になるまで待機します。少なくとも1つの更新済み Core ノードが準備完了になった後に Replicant のロールアウトが開始されます。
2. **Replicant ノードの更新：** Operator は Deployment スタイルのロールアウトを行い、`maxSurge` までの更新済み Replicant Pod を作成し、`maxUnavailable` までの旧 Replicant Pod を避難させます。これらの制限はロールアウト速度と可用性を制御します。Operator は、Replicant Pod の移行が完了するまで前のリビジョンの Core ノードを少なくとも1つ維持し、各リビジョンの移行中に Core ノードが利用可能であることを保証します。
3. **ロールアウトの完了：** Replicant の移行後、Operator は Core ノードの更新を完了します。すべての希望する Pod が準備完了となり、旧 Replicant Pod がなくなるとロールアウトは完了します。

デフォルトでは、Operator は Pod を削除する前に設定されたレートで MQTT 接続とセッションを避難させます。ノードの避難を無効にするには、`.spec.updateStrategy.evacuationStrategy.type` を `Disabled` に設定してください。

以下の手順は、EMQX をアップグレードせずに Pod テンプレートのアノテーションを変更してこのメカニズムを示します。EMQX をアップグレードする場合は `.spec.image` を更新してください。同じローリングアップデートの仕組みが適用されます。

## アップデート戦略の設定

1. `apps.emqx.io/v3beta1` の EMQX CR を作成し、アップデート戦略を設定します。

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
          key: "..."
    updateStrategy:
      type: RollingUpdate
      evacuationStrategy:
        # MQTT クライアント避難レート（秒あたりの接続数）:
        connectionEvictionRate: 1000
        # MQTT セッション避難レート（秒あたりのセッション数）:
        sessionEvictionRate: 1000
        # Pod 削除前の待機時間（秒）:
        waitTakeover: 10
      replicants:
        maxUnavailable: 1
        maxSurge: 1
    coreTemplate:
      spec:
        replicas: 2
    replicantTemplate:
      spec:
        replicas: 3
    listenersServiceTemplate:
      spec:
        type: LoadBalancer
  ```

  ::: warning

  `maxUnavailable` と `maxSurge` は両方とも `0` にできません。`maxUnavailable` が `100%` の場合、`maxSurge` は `0` より大きくなければなりません。フィールド定義、デフォルト値、検証ルールについては [ReplicantsUpdateStrategy](../reference/v3beta1-reference.md#replicantsupdatestrategy) を参照してください。

  :::

2. 上記内容を `emqx-update.yaml` として保存し、`kubectl apply` でデプロイします。

  ```bash
  $ kubectl apply -f emqx-update.yaml
  emqx.apps.emqx.io/emqx created
  ```

3. EMQX クラスターの状態を確認します。

  `STATUS` が `Ready` であることを確認してください。完了までに時間がかかる場合があります。

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

## テスト接続の確立

MQTTX CLI を使用して MQTT 接続を確立し、ローリングアップデート中の接続避難を観察します。MQTTX CLI は自動再接続に対応しています。

`emqx-listeners` サービスの外部アドレスを取得します。以下のコマンドは、IP アドレスまたはホスト名のいずれかを公開するロードバランサーに対応しています。

```bash
export EMQX_HOST="$(kubectl get service emqx-listeners -o jsonpath='{.status.loadBalancer.ingress[0].ip}{.status.loadBalancer.ingress[0].hostname}')"
```

`EMQX_HOST` にアドレスが設定されたら、MQTTX を使ってデフォルトの TCP リスナー（ポート `1883`）に3,000接続を開きます。

```bash
mqttx bench conn -h "${EMQX_HOST}" -p 1883 -c 3000
[10:05:21 AM] › ℹ  Start the connect benchmarking, connections: 3000, req interval: 10ms
✔  success   [3000/3000] - Connected
[10:06:13 AM] › ℹ  Done, total time: 31.113s
```

## アップデートのトリガー

1. Core と Replicant の Pod テンプレートのアノテーションを更新してローリングアップデートをトリガーします。コマンドはタイムスタンプとランダムサフィックスを組み合わせて、同じアノテーション値の再利用を減らします。

  ```bash
  ROLLOUT_ID="$(date +%s)-${RANDOM}"

  kubectl patch emqx emqx --type=merge -p \
    "{\"spec\":{\"coreTemplate\":{\"metadata\":{\"annotations\":{\"docs.emqx.com/rollout-id\":\"${ROLLOUT_ID}\"}}},\"replicantTemplate\":{\"metadata\":{\"annotations\":{\"docs.emqx.com/rollout-id\":\"${ROLLOUT_ID}\"}}}}}"
  ```

  期待される出力：

  ```text
  emqx.apps.emqx.io/emqx patched
  ```

2. アップデートプロセスの状態を確認します。

  ```bash
  $ kubectl get emqx emqx -o json | jq ".status.nodeEvacuations"
  [
    {
      "nodeName": "emqx@10.244.4.56",
      "initialConnections": 33,
      "initialSessions": 0,
      "connectionEvictionRate": 200,
      "sessionEvictionRate": 200,
      "state": "waiting_takeover",
      "sessionRecipients": [
        "emqx@10.244.4.57",
        "emqx@10.244.4.58"
      ]
    }
  ]
  ```

  | フィールド名               | 説明                                                            |
  |---------------------------|-----------------------------------------------------------------|
  | `nodeName`                | 現在避難中のノード名。                                           |
  | `state`                   | ノードの避難フェーズ。                                           |
  | `sessionRecipients`       | MQTT セッションの受信先ノード。                                 |
  | `sessionEvictionRate`     | このノードの MQTT セッション避難レート（秒あたりのセッション数）。 |
  | `connectionEvictionRate`  | このノードの MQTT 接続避難レート（秒あたりの接続数）。           |
  | `initialSessions`         | このノードの初期セッション数。                                  |
  | `initialConnections`      | このノードの初期接続数。                                        |

  ノードの避難進捗は対応する [EMQX ノードステータス](../reference/v3beta1-reference.md#emqxnode) の `connections` と `sessions` カウンターを確認して推定できます。

3. アップデート完了まで待ちます。

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

  `STATUS` が `Ready` であることを確認してください。MQTT クライアント数やセッション数によっては、アップデートに時間がかかる場合があります。

  アップデート完了後、`kubectl get pods` で全ての Pod が希望するテンプレートで稼働していることを確認できます。

## ローリングアップデートのイラスト

以下の図は、3,000接続の Replicant ローリングアップデート中の接続分布の一例を示しています。Grafana の測定データに基づくものではありません。

<svg viewBox="0 0 920 360" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Illustrative connection distribution during a Replicant rolling update">
  <rect width="920" height="360" fill="#111827"/>
  <rect x="58" y="34" width="680" height="266" fill="#121a24" stroke="#263241"/>
  <g stroke="#263241" stroke-width="1">
    <path d="M58 60H738M58 108H738M58 156H738M58 204H738M58 252H738M58 300H738"/>
    <path d="M80 34V300M160 34V300M260 34V300M360 34V300M460 34V300M560 34V300M660 34V300M738 34V300"/>
  </g>
  <g fill="#9ca3af" font-family="sans-serif" font-size="12">
    <text x="25" y="304">0</text>
    <text x="16" y="256">600</text>
    <text x="10" y="208">1,200</text>
    <text x="10" y="160">1,800</text>
    <text x="10" y="112">2,400</text>
    <text x="10" y="64">3,000</text>
    <text x="350" y="322">Update progress</text>
  </g>
  <g fill="none" stroke-linecap="round" stroke-linejoin="round">
    <path d="M80 300 C105 300 135 180 160 62 C210 58 270 61 330 60 C390 61 445 59 505 60 C565 62 630 59 700 60" stroke="#73bf69" stroke-width="2"/>
    <path d="M160 217 L300 216 C330 218 355 254 382 300" stroke="#e24d42" stroke-width="1.2"/>
    <path d="M160 221 L400 222 C430 222 455 257 482 300" stroke="#8f7ee7" stroke-width="1.2"/>
    <path d="M160 225 L500 224 C530 225 565 259 600 300" stroke="#5794f2" stroke-width="1.2"/>
    <path d="M280 300 C315 300 350 246 382 218 L400 218 C430 218 455 205 482 200 L500 200 C535 200 570 190 600 180 L700 180" stroke="#f2cc0c" stroke-width="1.2"/>
    <path d="M380 300 C415 300 450 260 482 248 L500 248 C535 248 570 225 600 221 L700 221" stroke="#ff9830" stroke-width="1.2"/>
    <path d="M500 300 C535 300 570 283 600 264 L700 264" stroke="#56a64b" stroke-width="1.2"/>
  </g>
  <g font-family="sans-serif" font-size="9">
    <rect x="758" y="46" width="144" height="166" rx="4" fill="#101923" stroke="#263241"/>
    <circle cx="768" cy="66" r="3" fill="#73bf69"/><text x="776" y="69" fill="#d1d5db">Total 3,000</text>
    <circle cx="768" cy="88" r="3" fill="#e24d42"/><text x="776" y="91" fill="#d1d5db">emqx-replicant-86f864f9-0</text>
    <circle cx="768" cy="110" r="3" fill="#8f7ee7"/><text x="776" y="113" fill="#d1d5db">emqx-replicant-86f864f9-1</text>
    <circle cx="768" cy="132" r="3" fill="#5794f2"/><text x="776" y="135" fill="#d1d5db">emqx-replicant-86f864f9-2</text>
    <circle cx="768" cy="154" r="3" fill="#f2cc0c"/><text x="776" y="157" fill="#d1d5db">emqx-replicant-648c45c7-0</text>
    <circle cx="768" cy="176" r="3" fill="#ff9830"/><text x="776" y="179" fill="#d1d5db">emqx-replicant-648c45c7-1</text>
    <circle cx="768" cy="198" r="3" fill="#56a64b"/><text x="776" y="201" fill="#d1d5db">emqx-replicant-648c45c7-2</text>
  </g>
  <g fill="#d1d5db" font-family="sans-serif">
    <text x="58" y="24" font-size="14">Illustrative Replicant rolling update, maxSurge = 1, maxUnavailable = 1</text>
  </g>
</svg>

| ラベル／プレフィックス           | 説明                                               |
|------------------------------|----------------------------------------------------|
| Total                        | 図の最上部の線として示される接続数の合計例。       |
| `emqx-replicant-86f864f9`    | 旧 Replicant Pod のセットの名前プレフィックス例。   |
| `emqx-replicant-648c45c7`    | 更新済み Replicant Pod のセットの名前プレフィックス例。 |

Replicant のロールアウト中、避難された Pod から切断されたクライアントは、更新済み Pod を含む他の稼働中 Pod に再接続する可能性があります。図中の線はこの再分配を示したものであり、測定値ではありません。実際の接続数はノード避難レート、クラスターの利用可能容量、クライアントの再接続動作により変動します。
