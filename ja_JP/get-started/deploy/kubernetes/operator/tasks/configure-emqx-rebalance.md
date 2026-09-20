# クラスターの負荷再分散

## タスク対象

MQTT接続の負荷再分散方法。

## なぜ負荷再分散が必要か

クラスターの負荷再分散とは、クライアント接続およびセッションをあるノード群から別のノード群へ強制的に移行する操作です。ノードのバランスを取るために移行すべき接続数を自動的に計算し、高負荷ノードから低負荷ノードへ対応する数の接続とセッションを移行することで、ノード間の負荷バランスを実現します。この操作は、新規ノードの参加やノードの再起動後にバランスを取るために通常必要となります。

負荷再分散の価値は主に以下の2点です：

- **システムのスケーラビリティ向上**：MQTT接続は永続的な性質を持つため、クラスターがスケールアウトしても既存のノードへの接続は自動的に新規ノードへ移行しません。これを解決するために、負荷再分散機能を利用して過負荷のノードから新規追加ノードへ接続をスムーズに移行できます。このプロセスにより、クラスター全体の負荷分散が均等化され、スループット、応答速度、リソース利用率が向上します。
- **運用コストの削減**：負荷が偏っているクラスターでは、一部のノードが過負荷で他のノードがアイドル状態となることがあります。負荷再分散機能を使うことでクラスター内の負荷を自動調整し、作業負荷の均等化を図り、運用・保守コストを削減できます。

EMQXクラスターの負荷再分散については、以下のドキュメントをご参照ください：[Rebalancing](../../../../../guides/cluster/rebalancing.md)

## 負荷再分散の使い方

EMQX Operatorにおけるクラスター負荷再分散の対応CRDは`Rebalance`であり、その例は以下のとおりです：

```yaml
apiVersion: apps.emqx.io/v2beta1
kind: Rebalance
metadata:
   name: rebalance-sample
spec:
   instanceName: emqx-ee
   rebalanceStrategy:
     connEvictRate: 10
     sessEvictRate: 10
     waitTakeover: 10
     waitHealthCheck: 10
     absConnThreshold: 100
     absSessThreshold: 100
     relConnThreshold: "1.1"
     relSessThreshold: "1.1"
```

> Rebalanceの設定については、以下のドキュメントをご参照ください：[Rebalanceリファレンス](../reference/v2beta1-reference.md#rebalancestrategy)。

## 負荷再分散のテスト

### 再分散前のクラスター負荷分布

再分散前に、意図的に接続数が偏ったEMQXクラスターを作成し、GrafanaとPrometheusでクラスター負荷を監視しました：

![](./assets/configure-emqx-rebalance/before-rebalance.png)

グラフの通り、クラスターは4つのEMQXノードで構成されており、3つのノードはそれぞれ10,000接続を処理していますが、1つのノードは**ゼロ**接続です。

以下の例では、4つのノード全体に負荷を均等に分散するための再分散操作を実演します。

#### Rebalanceタスクの送信

`Rebalance`リソースを作成して再分散処理を開始します：

```yaml
apiVersion: apps.emqx.io/v1beta4
kind: Rebalance
metadata:
   name: rebalance-sample
spec:
   instanceName: emqx-ee
   instanceKind: EmqxEnterprise
   rebalanceStrategy:
     connEvictRate: 10
     sessEvictRate: 10
     waitTakeover: 10
     waitHealthCheck: 10
     absConnThreshold: 100
     absSessThreshold: 100
     relConnThreshold: "1.1"
     relSessThreshold: "1.1"
```

ファイル名を`rebalance.yaml`として保存し、以下のコマンドでRebalanceタスクを送信します：

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

#### Rebalanceの進捗確認

以下のコマンドを実行してEMQXクラスターの再分散状況を確認します：

```bash
$ kubectl get rebalances rebalance-sample -o json | jq '.status.rebalanceStates'
{
     "state": "wait_health_check",
     "session_eviction_rate": 10,
     "recipients":[
         "emqx-ee@emqx-ee-3.emqx-ee-headless.default.svc.cluster.local",
     ],
     "node": "emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local",
     "donors":[
         "emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local",
         "emqx-ee@emqx-ee-1.emqx-ee-headless.default.svc.cluster.local",
         "emqx-ee@emqx-ee-2.emqx-ee-headless.default.svc.cluster.local"
     ],
     "coordinator_node": "emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local",
     "connection_eviction_rate": 10
}
```
> `rebalanceStates`フィールドの詳細な説明は、以下のドキュメントをご参照ください：[rebalanceStatesリファレンス](../reference/v2beta1-reference.md#rebalancestate)。

#### 完了まで待機

タスクのステータスが`Completed`になるまで監視します：

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> `STATUS`フィールドはRebalanceタスクのライフサイクル状態を示します：
>
> | ステータス      | 意味                                         |
> | -------------- | --------------------------------------------- |
> | **Processing** | 再分散処理が進行中です。                      |
> | **Completed**  | 再分散処理が正常に完了しました。              |
> | **Failed**     | 再分散処理でエラーが発生し停止しました。      |

### 再分散後のクラスター負荷分布

![](./assets/configure-emqx-rebalance/after-rebalance.png)

上図はRebalance完了後のクラスター負荷を示しています。クライアント接続の移行は全体を通じてスムーズかつ安定的に行われています。クラスター全体の接続数は再分散前と同じく**10,000**のままです。

再分散前は1つのノードが**0**接続、他の3ノードがそれぞれ**10,000**接続を持っていましたが、再分散後は4ノードすべてに均等に接続が再配分されています。各ノードの負荷は約**2,500**接続で安定し、一貫しています。

クラスターがバランス状態に達したかどうかは、EMQX Operatorが以下の条件で評価します：

```
avg(source node connection number) < avg(target node connection number) + abs_conn_threshold
or
avg(source node connection number) < avg(target node connection number) * rel_conn_threshold
```

設定されたRebalanceの閾値および実際の接続数を用いると：

- ソースノード平均：`avg(2553 + 2553 + 2554) ≈ 2553`
- ターゲットノード平均：`2340`
- 条件判定：`2553 < 2340 * 1.1`

条件が成立するため、Operatorはクラスターがバランス状態に達したと判断し、再分散タスクは正常に完了したとみなします。
