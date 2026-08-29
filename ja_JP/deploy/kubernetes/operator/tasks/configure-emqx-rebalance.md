# クラスター負荷のリバランス

## タスク対象

MQTT接続のリバランス方法。

## なぜ負荷リバランスが必要か

クラスター負荷のリバランスとは、クライアント接続およびセッションをあるノード群から別のノード群へ強制的に移行する操作のことです。ノード間のバランスを取るために移行すべき接続数を自動的に計算し、高負荷ノードから低負荷ノードへ対応する数の接続とセッションを移行します。通常、新規参加やノードの再起動後にバランスを取るためにこの操作が必要となります。

リバランスの価値は主に以下の2点です：

- **システムのスケーラビリティ向上**：MQTT接続は永続的であるため、クラスターがスケールアウトしても既存の接続は自動的に新規ノードへ移行しません。これを解決するために、負荷リバランス機能を使って過負荷のノードから新規追加ノードへ接続をスムーズに移行できます。これによりクラスター全体の負荷分散が改善され、スループット、応答速度、リソース利用率が向上します。
- **運用コストの削減**：負荷の偏りがあるクラスターでは、一部のノードが過負荷で他がアイドル状態になることがあります。負荷リバランス機能を使うことでクラスター内の負荷を自動調整し、作業の均等化を実現し、運用・保守コストを削減できます。

EMQXクラスターの負荷リバランスについては、ドキュメント：[Rebalancing](../../../cluster/rebalancing.md) をご参照ください。

## 負荷リバランスの使い方

EMQX Operatorにおけるクラスターリバランスの対応CRDは `Rebalance` であり、例は以下の通りです：

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

> Rebalanceの設定については、ドキュメント：[Rebalance reference](../reference/v2beta1-reference.md#rebalancestrategy) をご参照ください。

## 負荷リバランスのテスト

### リバランス前のクラスター負荷分布

リバランス前に、意図的に接続数が偏ったEMQXクラスターを作成し、GrafanaとPrometheusでクラスター負荷を監視しました：

![](./assets/configure-emqx-rebalance/before-rebalance.png)

グラフの通り、クラスターは4つのEMQXノードで構成されています。3つのノードはそれぞれ10,000接続を処理し、1つのノードは**ゼロ**接続です。

以下の例では、4つのノードすべてに負荷を均等に分散させるリバランス操作の方法を示します。

#### リバランスタスクの提出

`Rebalance` リソースを作成してリバランスを開始します：

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

ファイル名を `rebalance.yaml` として保存し、以下のコマンドでRebalanceタスクを提出します：

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

#### リバランス進捗の確認

以下のコマンドを実行してEMQXクラスターのリバランス状況を確認します：

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
> `rebalanceStates` フィールドの詳細な説明は、ドキュメント：[rebalanceStates reference](../reference/v2beta1-reference.md#rebalancestate) をご参照ください。

#### 完了まで待機

タスクのステータスが `Completed` になるまで監視します：

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> `STATUS` フィールドはRebalanceタスクのライフサイクル状態を示します：
>
> | ステータス       | 意味                                         |
> | ---------------- | -------------------------------------------- |
> | **Processing**   | リバランス処理中。                           |
> | **Completed**    | リバランスが正常に完了。                     |
> | **Failed**       | リバランスでエラーが発生し停止。             |

### リバランス後のクラスター負荷分布

![](./assets/configure-emqx-rebalance/after-rebalance.png)

上図はリバランス完了後のクラスター負荷を示しています。クライアント接続の移行は全体を通じてスムーズかつ安定しています。クラスター内の接続総数はリバランス前と同じく**10,000**のままです。

リバランス前は1ノードが**0**接続、3ノードがそれぞれ**10,000**接続でしたが、リバランス後は4ノードすべてに均等に接続が再分配されています。各ノードの負荷は約**2,500**接続で安定し、一貫しています。

クラスターがバランス状態に達したかどうかは、EMQX Operatorが以下の条件で評価します：

```
avg(source node connection number) < avg(target node connection number) + abs_conn_threshold
or
avg(source node connection number) < avg(target node connection number) * rel_conn_threshold
```

設定されたRebalanceの閾値と実際の接続数を用いると：

- ソースノード平均：`avg(2553 + 2553 + 2554) ≈ 2553`
- ターゲットノード平均：`2340`
- 条件判定：`2553 < 2340 * 1.1`

条件を満たすため、Operatorはクラスターがバランス状態に達したと判断し、リバランスタスクは正常に完了したと結論づけます。
