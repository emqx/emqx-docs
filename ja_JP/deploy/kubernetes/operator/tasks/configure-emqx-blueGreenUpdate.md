# EMQXクラスターのブルーグリーンアップグレードの実施

## 目的

ブルーグリーンデプロイメントによるEMQXクラスターのグレースフルアップグレードを実施します。

## 背景

従来のEMQXクラスターのデプロイメントでは、StatefulSetのデフォルトのローリングアップグレード戦略を用いてEMQX Podを更新することが一般的です。しかし、この方法には以下の2つの問題があります。

<<<<<<< HEAD
* ローリングアップデート中は、新旧両方のPodが対応するServiceにより選択されるため、終了処理中の古いPodにMQTTクライアントが接続してしまい、頻繁な切断と再接続が発生する可能性があります。
* ローリングアップデートの過程では、新しいPodが起動して準備完了になるまで時間がかかるため、任意の時点でサービスを提供できるPodは_N - 1_に限られ、サービスの可用性が低下する恐れがあります。
=======
* ローリングアップデート中、新旧のPodが対応するServiceにより選択されるため、終了処理中の古いPodにMQTTクライアントが接続され、頻繁な切断と再接続が発生する可能性があります。
* ローリングアップデートの過程では、新しいPodの起動と準備完了に時間がかかるため、任意の時点でサービスを提供できるPodは_N - 1_に制限され、サービスの可用性が低下する恐れがあります。
>>>>>>> origin/release-6.1

```mermaid
timeline
				section Update start
					Current Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
				section Rolling update
					Current Cluster<br>Have Endpoint
						: pod-0
						: pod-1
					Update Cluster<br>Have Endpoint
						: pod-2
					Current Cluster<br>Have Endpoint
						: pod-0
					Update Cluster<br>Have Endpoint
						: pod-1
						: pod-2
				section Finish Update
					Update Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
```

## 解決策

EMQX Operatorはデフォルトでブルーグリーンデプロイメントを実施します。対応するEMQX CRを通じてEMQXクラスターを更新すると、EMQX Operatorがアップグレードを開始します。

<<<<<<< HEAD
アップグレード全体の流れは大まかに以下のステップに分かれます。

1. 更新された仕様の新しいEMQXノード群を作成する。
2. 新しいノード群が準備完了になったら、Serviceリソースを新しいノード群に切り替え、新規接続が古いノード群にルーティングされないようにする。
3. 既存のMQTT接続を制御された速度で安全に古いノード群から新しいノード群へ移行し、再接続の嵐を防ぐ。
=======
アップグレード全体の流れは以下のステップに大別されます。

1. 更新された仕様で新しいEMQXノード群を作成する。
2. 新しいノード群が準備完了したら、Serviceリソースを新しいノード群に切り替え、新規接続が古いノード群にルーティングされないようにする。
3. 既存のMQTT接続を制御された速度で古いノード群から新しいノード群へ安全に移行し、再接続の嵐を回避する。
>>>>>>> origin/release-6.1
4. 古いEMQXノード群を段階的にスケールダウンする。
5. アップグレードを完了する。

```mermaid
timeline
				section Update start
					Current Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
				section Create update cluster
					Current Cluster
						: pod-0
						: pod-1
						: pod-2
					Update Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
				section Updating cluster
					Current Cluster
						: pod-0
						: pod-1
					Update Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
					Current Cluster
						: pod-0
					Update Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
				section Finish Update
					Update Cluster<br>Have Endpoint
						: pod-0
						: pod-1
						: pod-2
```

## 手順

### アップデート戦略の設定

<<<<<<< HEAD
1. `apps.emqx.io/v2` のEMQX CRを作成し、アップデート戦略を設定します。
=======
1. `apps.emqx.io/v2beta1`のEMQX CRを作成し、アップデート戦略を設定します。
>>>>>>> origin/release-6.1

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
    updateStrategy:
      evacuationStrategy:
<<<<<<< HEAD
        # MQTTクライアントの避難速度（秒あたりの接続数）：
        connEvictRate: 1000
        # MQTTセッションの避難速度（秒あたりのセッション数）：
        sessEvictRate: 1000
        # Pod削除前の待機時間（秒）：
        waitTakeover: 10
      # すべてのノードが準備完了後、アップグレード開始までの待機時間（秒）：
=======
        # MQTTクライアントの退避速度（接続数/秒）:
        connEvictRate: 1000
        # MQTTセッションの退避速度（セッション数/秒）:
        sessEvictRate: 1000
        # Pod削除前の待機時間（秒）:
        waitTakeover: 10
      # すべてのノードが準備完了後、アップグレード開始までの待機時間（秒）:
>>>>>>> origin/release-6.1
      initialDelaySeconds: 10
      type: Recreate
  ```

<<<<<<< HEAD
2. 上記内容を `emqx-update.yaml` として保存し、`kubectl apply` でデプロイします。
=======
2. 上記内容を`emqx-update.yaml`として保存し、`kubectl apply`でデプロイします。
>>>>>>> origin/release-6.1

  ```bash
  $ kubectl apply -f emqx-update.yaml
  emqx.apps.emqx.io/emqx created
  ```

3. EMQXクラスターの状態を確認します。

<<<<<<< HEAD
  `STATUS` が `Ready` になるまで待ちます。完了までに時間がかかる場合があります。
=======
  `STATUS`が`Ready`であることを確認してください。準備完了まで時間がかかる場合があります。
>>>>>>> origin/release-6.1

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

### EMQXクラスターへの接続

<<<<<<< HEAD
[MQTTX](https://mqttx.app/cli) は自動再接続をサポートしたオープンソースのMQTT 5.0対応コマンドラインクライアントツールで、MQTTサービスやアプリケーションの開発・デバッグに役立ちます。

MQTTXを用いてEMQXクラスターに接続します。
=======
[MQTTX](https://mqttx.app/cli)は、MQTT 5.0に対応したオープンソースのコマンドラインクライアントツールで、自動再接続機能を備え、MQTTサービスやアプリケーションの開発・デバッグを支援します。

MQTTXを使ってEMQXクラスターに接続します。
>>>>>>> origin/release-6.1

```bash
mqttx bench conn -h ${IP} -p ${PORT} -c 3000
[10:05:21 AM] › ℹ  接続ベンチマークを開始、接続数: 3000、リクエスト間隔: 10ms
✔  成功   [3000/3000] - 接続完了
[10:06:13 AM] › ℹ  完了、合計時間: 31.113秒
```

### アップグレードのトリガー

1. Podテンプレートの任意の変更がEMQX Operatorのアップグレード戦略をトリガーします。

<<<<<<< HEAD
  本例ではPodの `ImagePullPolicy` を変更してアップグレードをトリガーします。
=======
  本例では、Podの`ImagePullPolicy`を変更してアップグレードをトリガーします。
>>>>>>> origin/release-6.1

  ```bash
  $ kubectl patch emqx emqx --type=merge -p '{"spec": {"imagePullPolicy": "Never"}}'
  emqx.apps.emqx.io/emqx patched
  ```

<<<<<<< HEAD
2. アップグレードの進捗状況を確認します。
=======
2. アップグレードの進行状況を確認します。
>>>>>>> origin/release-6.1

  ```bash
  $ kubectl get emqx emqx -o json | jq ".status.nodeEvacuationsStatus"
  [
    {
      "nodeName": "emqx@emqx-54fc496fb4-2.emqx-headless.default.svc.cluster.local",
      "initialConnections": 33,
      "initialSessions": 0,
      "connectionEvictionRate": 200,
      "sessionEvictionRate": 200,
      "state": "waiting_takeover",
      "sessionRecipients": [
        "emqx@emqx-5d87d4c6bd-2.emqx-headless.default.svc.cluster.local",
        "emqx@emqx-5d87d4c6bd-1.emqx-headless.default.svc.cluster.local",
        "emqx@emqx-5d87d4c6bd-0.emqx-headless.default.svc.cluster.local"
      ]
    }
  ]
  ```

<<<<<<< HEAD
  | フィールド名               | 説明                                                                 |
  |-------------------------|----------------------------------------------------------------------|
  | `nodeName`              | 現在避難中のノード名。                                                |
  | `state`                 | ノードの避難フェーズ。                                                |
  | `sessionRecipients`     | MQTTセッションの受け入れ先ノード群。                                 |
  | `sessionEvictionRate`   | このノードのMQTTセッション避難速度（秒あたりのセッション数）。       |
  | `connectionEvictionRate`| このノードのMQTT接続避難速度（秒あたりの接続数）。                   |
  | `initialSessions`       | このノードの初期セッション数。                                       |
  | `initialConnections`    | このノードの初期接続数。                                             |

  ノードの避難進捗は、対応する[EMQXノードのステータス](../reference/v2-reference.md#emqxnode)内の `connections` および `sessions` カウンターを参照することで推測できます。

3. アップグレード完了まで待ちます。
=======
  | フィールド                  | 説明                                                                 |
  |----------------------------|----------------------------------------------------------------------|
  | `node`                     | 現在退避中のノード。                                                  |
  | `state`                    | ノードの退避フェーズ。                                                |
  | `session_recipients`       | MQTTセッションの受け取り先。                                         |
  | `session_eviction_rate`    | 当該ノードのMQTTセッション退避速度（セッション数/秒）。              |
  | `connection_eviction_rate` | 当該ノードのMQTT接続退避速度（接続数/秒）。                          |
  | `initial_sessions`         | 当該ノードの初期セッション数。                                       |
  | `initial_connected`        | 当該ノードの初期接続数。                                             |
  | `current_sessions`         | 当該ノードの現在のセッション数。                                     |
  | `current_connected`        | 当該ノードの現在の接続数。                                           |

3. アップグレード完了まで待機します。
>>>>>>> origin/release-6.1

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

<<<<<<< HEAD
  `STATUS` が `Ready` であることを確認してください。MQTTクライアント数やセッション数によってはアップグレードに時間がかかる場合があります。

  アップグレード完了後、`kubectl get pods` で古いEMQXノードが削除されていることを確認できます。

## Grafanaによるモニタリング

以下のモニタリンググラフは、アップグレード中の接続数を10,000接続の例で示しています。

![](./assets/configure-emqx-blueGreenUpdate/grafana.png)

| ラベル／プレフィックス       | 説明                                                  |
|----------------------------|-------------------------------------------------------|
| Total                      | 接続数の合計。グラフの最上位の線で表示されます。       |
| `emqx-86f864f975`          | 古い3つのEMQXノード群の名前プレフィックス。           |
| `emqx-648c45c747`          | アップグレードされた3つのEMQXノード群の名前プレフィックス。|

このタイムラインは、EMQX Operatorがいかにスムーズにブルーグリーンアップグレードを実施するかを示しています。プロセス全体を通じて接続数は安定しており（移行速度、サーバー容量、クライアントの再接続戦略などの要因に依存します）、サービスの中断を最小限に抑え、サーバーの過負荷を防ぎ、全体的なサービスの安定性を向上させています。
=======
  `STATUS`が`Ready`であることを確認してください。MQTTクライアント数やセッション数によってはアップグレードに時間がかかる場合があります。

  アップグレード完了後、`kubectl get pods`で古いEMQXノードが削除されていることを確認できます。

## Grafanaによるモニタリング

以下のモニタリンググラフは、アップグレード中の接続数（例として10,000接続）を示しています。

![](./assets/configure-emqx-blueGreenUpdate/grafana.png)

| ラベル／プレフィックス       | 説明                                                       |
|-----------------------------|------------------------------------------------------------|
| Total                       | 接続の合計数。グラフの最上位の線として表示されます。       |
| `emqx-ee-86f864f975`        | 古いEMQXノード3台の名前プレフィックス。                    |
| `emqx-ee-648c45c747`        | アップグレード済みのEMQXノード3台の名前プレフィックス。    |

このタイムラインは、EMQX Operatorがスムーズなブルーグリーンアップグレードを実施する様子を示しています。アップグレード中も接続数の合計は安定しており（移行速度、サーバーキャパシティ、クライアントの再接続戦略などの要因による）、サーバーの過負荷を防ぎつつサービスの安定性を高めています。
>>>>>>> origin/release-6.1
