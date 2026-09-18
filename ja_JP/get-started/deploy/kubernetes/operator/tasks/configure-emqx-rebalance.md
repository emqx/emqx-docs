# クラスター負荷再分散（EMQX Enterprise）

## タスク対象

MQTT接続の再分散方法。

## なぜ負荷再分散が必要か

クラスター負荷再分散とは、クライアント接続およびセッションをあるノード群から別のノード群へ強制的に移行する操作です。ノード間のバランスを取るために移行すべき接続数を自動的に計算し、高負荷ノードから低負荷ノードへ対応する数の接続とセッションを移行します。通常、新しいノードの参加やノードの再起動後にバランスを取るために必要となる操作です。

再分散の価値は主に以下の2点です：

- **システムのスケーラビリティ向上**：MQTT接続は永続的な性質を持つため、クラスターがスケールアウトしても既存ノードへの接続は自動的に新ノードへ移行しません。これを解決するために、負荷再分散機能を使って過負荷のノードから新規追加ノードへ接続をスムーズに移行できます。このプロセスにより、クラスター全体の負荷分散が改善され、スループット、応答速度、リソース利用率が向上します。
- **運用コストの削減**：負荷が偏っているクラスター（過負荷のノードとアイドル状態のノードが混在する場合）では、負荷再分散機能を使ってクラスター内の負荷を自動調整できます。これにより作業負荷の均等化が図られ、運用コストの削減につながります。

EMQXクラスターの負荷再分散については、以下のドキュメントを参照してください：[Rebalancing](../../../../../guides/cluster/rebalancing.md)

## 負荷再分散の使い方

EMQX Operatorにおけるクラスター再分散の対応CRDは`Rebalance`であり、その例は以下の通りです：

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

> Rebalanceの設定については、以下のドキュメントを参照してください：[Rebalance reference](../api-reference.md#rebalancestrategy)。

## 負荷再分散のテスト

### 再分散前のクラスター負荷分布

再分散前に負荷が偏ったクラスターを構築し、Grafana + PrometheusでEMQXクラスターの負荷を監視しました：

![](./assets/configure-emqx-rebalance/before-rebalance.png)

図から、現在のクラスターには4つのEMQXノードがあり、そのうち3つはそれぞれ10,000接続を持ち、残り1つは0接続であることがわかります。次に、4つのノードの負荷が均衡するように再分散操作を行う方法を示します。

- Rebalanceタスクの提出

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

上記内容を`rebalance.yaml`として保存し、以下のコマンドでRebalanceタスクを提出します：

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

EMQXクラスターの再分散状況を確認するには、以下のコマンドを実行します：

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
> `rebalanceStates`フィールドの詳細な説明は、以下のドキュメントを参照してください：[rebalanceStates reference](../api-reference.md#rebalancestate)。

Rebalanceタスクの完了を待ちます：

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> Rebalanceには3つの状態があります：Processing、Completed、Failed。Processingは再分散タスクが進行中であることを示し、Completedはタスクが完了したことを示し、Failedはタスクが失敗したことを示します。

### 再分散後のクラスター負荷分布

![](./assets/configure-emqx-rebalance/after-rebalance.png)

上図はRebalance完了後のクラスター負荷を示しています。グラフから、Rebalanceプロセスが非常にスムーズに行われたことがわかります。クラスター全体の接続数は依然として10,000であり、再分散前と一致しています。4つのノードの接続数が変化し、3つのノードの一部接続が新規拡張ノードへ移行しました。再分散後、4つのノードの負荷は安定し、接続数は約2,500前後で変動しません。

クラスターがバランス状態に達する条件は以下の通りです：

```
avg(ソースノードの接続数) < avg(ターゲットノードの接続数) + abs_conn_threshold
または
avg(ソースノードの接続数) < avg(ターゲットノードの接続数) * rel_conn_threshold
```

設定したRebalanceパラメータと接続数を代入すると、`avg(2553 + 2553 + 2554) < 2340 * 1.1`となり、現在のクラスターはバランス状態に達しているため、Rebalanceタスクはクラスター負荷の再分散に成功したことがわかります。
