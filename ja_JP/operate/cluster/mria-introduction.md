# アーキテクチャ

<!--ユーザーがすべてのノードをコアノードとしてクラスタを操作する方法についてのセクションを追加する必要があります-->

EMQX 5.0 は、[Mria](https://github.com/emqx/mria) を用いてクラスタアーキテクチャを再設計し、EMQX の水平スケーラビリティを大幅に向上させました。新しい設計により、単一クラスタで1億の MQTT 接続をサポート可能です。

<img src="./assets/EMQX_Mria_architecture.png" alt="EMQX Mria" style="zoom: 40%;" />

この [Mria](https://github.com/emqx/mria) では、各ノードはコアノードまたはレプリカントのいずれかの役割を担います。  
コアノードはデータベースのデータレイヤーとして機能します。  
レプリカントノードはコアノードに接続し、コアノードからのデータ更新を受動的にレプリケーションします。コアノードとレプリカントノードの動作については、[EMQX クラスタリング](../../design/clustering.md) をご参照ください。

デフォルトでは、すべてのノードがコアノードの役割を担うため、クラスタは [EMQX 4.x](https://docs.emqx.com/en/enterprise/v4.4/getting-started/cluster.html#node-discovery-and-autocluster) と同様に動作します。これは3ノード以下の小規模クラスタに推奨されます。コア＋レプリカントモードは、クラスタに3ノードを超える場合にのみ推奨されます。

## コア＋レプリカントモードの有効化

コア＋レプリカントモードを有効にするには、特定のノードをレプリカントノードとして指定する必要があります。これは `node.role` パラメータを `replicant` に設定することで実現します。加えて、自動クラスタ検出戦略（`cluster.discovery_strategy`）を有効にする必要があります。

::: tip

レプリカントノードは、コアノードを検出するために `manual` 検出戦略を使用できません。

:::

設定例：

```bash
node {
    ## ノードをレプリカントノードとして設定する場合：
    role = replicant
}
cluster {
    ## 静的検出戦略を有効化：
    discovery_strategy = static
    static.seeds = [emqx@host1.local, emqx@host2.local]
}
```

## 監視とデバッグ

<!-- TODO 後続で数値タイプ Gauge または Counter を補足 -->

Mria のパフォーマンスは Prometheus メトリクスや Erlang コンソールを使って監視できます。

### Prometheus 指標

Prometheus と連携してクラスタの動作を監視可能です。Prometheus との連携方法は [ログと可観測性 - Prometheus 連携](../../observability/prometheus.md) をご覧ください。

#### コアノード

| 指標名                             | 説明                                                         |
| ---------------------------------- | ------------------------------------------------------------ |
| `emqx_mria_last_intercepted_trans` | ノード起動以降にシャードが受信したトランザクション数       |
| `emqx_mria_weight`                 | コアノードの瞬間的な負荷                                      |
| `emqx_mria_replicants`             | コアノードに接続しているレプリカントノード数。シャードごとに集計されます。 |
| `emqx_mria_server_mql`             | レプリカントノードに送信待ちのトランザクション数。少ないほど良い。<br />この指標が増加傾向にある場合は、コアノードを増やす必要があります。 |

#### レプリカントノード

| 指標名                         | 説明                                                         |
| ------------------------------ | ------------------------------------------------------------ |
| `emqx_mria_lag`                | レプリカントが上流のコアノードにどれだけ遅れているかを示します。少ないほど良い。 |
| `emqx_mria_bootstrap_time`     | レプリカントノードの起動時間。正常に動作していれば安定した値となります。 |
| `emqx_mria_bootstrap_num_keys` | 起動時にコアノードからコピーしたデータベースレコード数。正常に動作していれば安定した値となります。 |
| `emqx_mria_message_queue_len`  | メッセージレプリケーション時のキュー長。0付近が望ましい。 |
| `emqx_mria_replayq_len`        | レプリカントノード内部のリプレイキュー長。少ないほど良い。 |

### コンソールコマンド

Erlang コンソールで `emqx eval 'mria_rlog:status().'` コマンドを実行することで、クラスタの稼働状況を監視できます。

EMQX クラスタが正常に動作している場合、現在のログレベル、処理済みメッセージ数、破棄されたメッセージ数などのステータス情報一覧が取得できます。

<!--ここにクエリ文と返却メッセージを追加し、Erlang コンソールのリンク https://www.erlang.org/doc/man/shell.html を貼る予定-->

## 疑似分散クラスタ

EMQX はテストや開発用途向けに疑似分散クラスタ機能も提供しています。これは、単一マシン上で複数の EMQX インスタンスを起動し、それぞれをクラスタのノードとして構成するセットアップを指します。

最初のノードを起動した後、以下のコマンドで2番目のノードを起動し、手動でクラスタに参加させます。ポート競合を避けるために、いくつかのリスニングポートを調整する必要があります。

```bash
EMQX_NODE__NAME='emqx2@127.0.0.1' \
    EMQX_LOG__FILE_HANDLERS__DEFAULT__FILE='log2/emqx.log' \
    EMQX_STATSD__SERVER='127.0.0.1:8124' \
    EMQX_LISTENERS__TCP__DEFAULT__BIND='0.0.0.0:1882' \
    EMQX_LISTENERS__SSL__DEFAULT__BIND='0.0.0.0:8882' \
    EMQX_LISTENERS__WS__DEFAULT__BIND='0.0.0.0:8082' \
    EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8085' \
    EMQX_DASHBOARD__LISTENERS__HTTP__BIND='0.0.0.0:18082' \
    EMQX_NODE__DATA_DIR="./data2" \
./bin/emqx start

./bin/emqx ctl cluster join emqx1@127.0.0.1
```

上記の例は手動でクラスタを作成する方法です。自動クラスタリングの方法は [自動クラスタリング](./create-cluster.md#auto-clustering) セクションもご参照ください。

ダッシュボードは、すべてのクラスタノードが同じポート番号を使用することを前提に設計されています。単一マシン上で異なるポートを使うとダッシュボードのUIに問題が生じる可能性があるため、本番環境での使用は推奨されません。

<!--疑似分散クラスタのクイックスタートを追加予定 @WIVWIV -->
