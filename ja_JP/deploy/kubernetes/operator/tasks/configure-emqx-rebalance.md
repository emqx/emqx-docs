# クラスター負荷のリバランス

## タスク対象

MQTT接続のリバランス方法。

## なぜ負荷リバランスが必要か

クラスター負荷リバランスとは、クライアント接続およびセッションをあるノード群から別のノード群へ強制的に移行する操作です。ノード間のバランスを取るために移行すべき接続数を自動で計算し、高負荷ノードから低負荷ノードへ対応する数の接続とセッションを移行します。通常、新規ノードの参加やノードの再起動後にバランスを取るために必要となります。

リバランスの価値は主に以下の2点です：

- **システムのスケーラビリティ向上**：MQTT接続は永続的な性質を持つため、クラスター拡張時に既存ノードへの接続が自動的に新規ノードへ移行しません。これを解決するために、負荷リバランス機能を使って過負荷のノードから新規追加ノードへ接続をスムーズに移行できます。これによりクラスター全体の負荷分散が改善され、スループット、応答速度、リソース利用率が向上します。
- **運用コストの削減**：負荷が偏っているクラスターでは、一部のノードが過負荷で他がアイドル状態となることがあります。負荷リバランス機能を使うことでクラスター内の負荷を自動調整し、作業負荷の均等化と運用コストの削減が可能です。

EMQXクラスターの負荷リバランスについては、以下のドキュメントをご参照ください：[Rebalancing](../../../cluster/rebalancing.md)

## 負荷リバランスの使い方

EMQX Operatorにおけるクラスターリバランスの対応CRDは`Rebalance`であり、その例は以下の通りです：

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

> Rebalanceの設定については、以下のドキュメントをご参照ください：[Rebalance reference](../reference/v2beta1-reference.md#rebalancestrategy)。

## 負荷リバランスのテスト

### リバランス前のクラスター負荷分布

リバランス前に、意図的に接続数が偏ったEMQXクラスターを作成し、GrafanaとPrometheusでクラスター負荷を監視しました：

![](./assets/configure-emqx-rebalance/before-rebalance.png)

グラフに示されているように、クラスターは4つのEMQXノードで構成されており、3つのノードはそれぞれ10,000接続を処理していますが、1つのノードは**0**接続です。

以下の例では、4つのノード全体に負荷を均等に分散させるためのリバランス操作の実施方法を示します。

#### リバランスタスクの提出

`Rebalance`リソースを作成してリバランス処理を開始します：

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

ファイル名を`rebalance.yaml`として保存し、以下のコマンドでRebalanceタスクを提出します：

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

#### リバランス進捗の確認

以下のコマンドを実行して、EMQXクラスターのリバランス状況を確認します：

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

> `rebalanceStates`フィールドの詳細については、以下のドキュメントをご参照ください：[rebalanceStates reference](../reference/v2beta1-reference.md#rebalancestate)。

#### 完了まで待機

タスクの状態が`Completed`になるまで監視します：

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> `STATUS`フィールドはRebalanceタスクのライフサイクル状態を示します：
>
> | 状態           | 意味                                         |
> | -------------- | -------------------------------------------- |
> | **Processing** | リバランス処理が進行中です。                 |
> | **Completed**  | リバランス処理が正常に完了しました。         |
> | **Failed**     | リバランス処理でエラーが発生し停止しました。 |

### リバランス後のクラスター負荷分布

![](./assets/configure-emqx-rebalance/after-rebalance.png)

上図はRebalance完了後のクラスター負荷を示しています。クライアント接続の移行は全体を通じてスムーズかつ安定して行われています。クラスター内の接続総数はリバランス前と同じく**10,000**のままです。

リバランス前は1ノードが**0**接続、他の3ノードがそれぞれ**10,000**接続を保持していましたが、リバランス後は4ノードすべてに均等に接続が再分配されました。各ノードの負荷は約**2,500**接続で安定し、一貫しています。

クラスターがバランス状態に達したかどうかは、EMQX Operatorが以下の条件で評価します：

```
avg(ソースノードの接続数) < avg(ターゲットノードの接続数) + abs_conn_threshold
または
avg(ソースノードの接続数) < avg(ターゲットノードの接続数) * rel_conn_threshold
```

設定されたRebalanceの閾値と実際の接続数を用いると：

- ソースノード平均：`avg(2553 + 2553 + 2554) ≈ 2553`
- ターゲットノード平均：`2340`
- 条件判定：`2553 < 2340 * 1.1`

条件が成立したため、Operatorはクラスターがバランス状態に達したと判断し、リバランスタスクは正常に完了したと見なします。
