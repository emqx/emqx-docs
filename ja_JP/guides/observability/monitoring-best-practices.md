# 本番環境モニタリングのベストプラクティス

本番環境のデプロイメントでは、EMQXダッシュボードを超えたモニタリングが必要です。ダッシュボードは現在のブローカー状態を表示しますが、ブローカーやホストが利用不可になった場合にオペレーターへ通知することはできません。本ページのモニタリングガイダンスは、サービス停止、冗長性喪失、リソース枯渇を早期に検知し、対応可能なタイミングを提供します。

このガイダンスはEMQX Enterpriseの本番環境デプロイメントに適用されます。例示しているしきい値は出発点として扱い、サービスレベル目標（SLO）、テスト済みのキャパシティ、トラフィックパターン、復旧時間に応じて調整してください。

## 本番環境モニタリングシステムの設計

モニタリングシステムを設計する際は以下の原則に従ってください：

1. **EMQXのメトリクスを外部モニタリングシステムにエクスポートする。**

   包括的なモニタリングには[PrometheusのPullモード](./prometheus.md#configure-pull-mode-integration)を推奨します。ロードバランサー経由ではなく、各EMQXノードを直接スクレイプしてください。これにより、障害や孤立したノードが健全なノードに隠されることを防げます。すべてのターゲットのPrometheus `up` メトリクスを監視してください。

2. **EMQX組み込みアラームを転送する。**

   環境に合わせてEMQXの組み込みアラームしきい値を設定し、[Webhookまたはシステムトピック](./alarms.md#get-alarms)を使ってアラームイベントを外部通知システムに送信してください。オペレーターがダッシュボード上のアラームに気づくことだけに依存しないでください。

3. **クラスター外部からエンドツーエンドのMQTTチェックを実行する。**

   合成クライアントは、本番クライアントと同じロードバランサー、TLSリスナー、認証経路を通じて接続する必要があります。クライアントは固有識別されたメッセージをパブリッシュし、サブスクリプションで受信し、トータルのレイテンシを計測します。このチェックにより、ブローカーのメトリクスだけでは検出できない障害を検知できます。

4. **ホストやコンテナプラットフォームを監視する。**

   EMQXはOS、Kubernetes、クラウドプロバイダーの監視を代替しません。CPUスロットリング、メモリプレッシャー、ディスク容量とレイテンシ、ファイルディスクリプタ使用状況、ネットワークエラー、コンテナ再起動、時刻同期状態を収集してください。

5. **ログを集中収集する。**

   すべてのノードから警告、エラー、クリティカルログをEMQXクラスター外のストレージに送信してください。JSON形式のログを推奨します。これにより、構造化された `msg`、`node` などのコンテキストフィールドでアラートルールをマッチさせやすくなります。ログはメトリクスや組み込みアラームで表現されない状態を明らかにします。

6. **モニタリングをEMQXから独立させる。**

   EMQXノード、アベイラビリティゾーン、またはクラスター全体が利用不可でも、モニタリングおよび通知経路は利用可能なままでなければなりません。

::: tip

検知に必要な時間より短い間隔でメトリクスを収集してください。例えば、15秒間隔でスクレイプし、2回連続失敗でアラートを出す設定なら、単一のスクレイプ失敗で1分以内にターゲットの到達不能を検知可能です。

:::

## SLO、キャパシティ基準値、アラートしきい値の設定

固定のしきい値を本番にコピーする代わりに、以下のプロセスを用いてください：

1. 接続成功率、パブリッシュから配信成功率、レイテンシなどのユーザー視点のSLOを定義する。
2. 代表的な[パフォーマンステスト](../performance/overview.md)を実施し、飽和前のリソース使用量、メッセージレート、レイテンシを記録する。
3. 少なくとも1つの通常の業務サイクルを観察し、日次または週次のピークを特定する。
4. 警告しきい値はテスト済みの安全キャパシティより低く設定し、キャパシティ追加やメンテナンス予定を立てる余裕を持たせる。クリティカルしきい値は即時対応が必要なポイントに設定する。
5. トラフィック増加、トポロジー変更、アップグレード、永続セッションやデータ統合の変更後にしきい値を見直す。

単一の固定パーセンテージでアラートを出すのは避けてください。ディスク枯渇が24時間以内に予測される場合や、接続数が1週間以内にテスト済みキャパシティに達する場合など、トレンドや予測に基づくアラートは、より有用なメンテナンス猶予を提供します。

## 監視すべき先行指標

予防的アラートは、クラスターがまだトラフィックを処理している間に状態悪化を検知すべきです。調査やメンテナンスに十分な時間を残す警告しきい値と、即時対応が必要なクリティカルしきい値を設定してください。以下の各条件について、関連するシグナルとオペレーター向けの推奨対応を示します。

### クラスターおよびランタイムの健全性

**Mriaレプリケーション圧力**

- **早期警告条件:** レプリケーション遅延やキューが通常のピークを超えて持続、または排出されずに増加し続ける。
- **関連シグナル:** Replicantノードでは `emqx_mria_lag`、`emqx_mria_message_queue_len`、`emqx_mria_replayq_len` を監視。Coreノードでは `emqx_mria_server_mql` と `emqx_mria_weight` を監視。
- **推奨対応:** Replicantとその上流Coreノードのログとメトリクスの相関を確認。遅延収集失敗、分散ポートの過負荷、長いスケジューラ停止、Mnesia過負荷、Mriaレプリケーションエラーを探す。ネットワークのレイテンシ・ロス、CPU、ディスクI/Oも確認。書き込み圧力を軽減するかCoreのキャパシティを追加し、Replicantの遅れ拡大を防ぐ。

`emqx_mria_lag` はReplicantシャードが上流Coreシャードに対して遅れているトランザクション数であり、秒数ではありません。書き込みバースト時の短時間のスパイクは正常です。代表的なピークトラフィック時の最大値を超えて持続した場合や、Mriaキューのメトリクスとともに正のトレンドが続く場合にアラートを出してください。ノードと `shard` ごとにグループ化してアラートを出すことを推奨します。詳細は[監視とデバッグ](../../develop/cluster/mria-introduction.md#monitor-and-debug)を参照してください。

**設定の収束**

- **早期警告条件:** ノード間で `emqx_conf_sync_txid` が通常の設定展開時間を超えて異なるまま。
- **関連シグナル:** すべてのノードの `emqx_conf_sync_txid` と設定同期ログ。
- **推奨対応:** 追加の設定変更を停止し、遅れているノードを特定。クラスター接続性と設定同期エラーを調査。メンテナンスや次の設定変更前に収束を回復する。

**ランタイムバックログ**

- **早期警告条件:** ランキューやメールボックスサイズが基準値を超えて持続。
- **関連シグナル:** `emqx_vm_run_queue`、`emqx_vm_mnesia_tm_mailbox_size`、`emqx_vm_broker_pool_max_mailbox_size`、組み込みのオーバーロードアラーム、`busy_dist_port` イベント。
- **推奨対応:** 持続的な過負荷、ストレージ遅延、クラスター通信問題を調査し、リクエストレイテンシやキューのさらなる増加を防ぐ。

### リソースとキャパシティ

**CPUプレッシャー**

- **早期警告条件:** CPU使用率が通常ピークを10〜15分間超過。
- **関連シグナル:** `emqx_vm_cpu_use`、ホストCPU、ロード、コンテナスロットリング。
- **推奨対応:** 増加の原因となるワークロードや統合を特定。トラフィックのリバランスやキャパシティ追加を行い飽和を防ぐ。EMQX組み込みCPUアラームはデフォルトで80%に設定。

**メモリプレッシャー**

- **早期警告条件:** メモリ使用率が警告しきい値を超えるか、ホストやコンテナの上限に向けて増加。
- **関連シグナル:** `emqx_vm_used_memory`、`emqx_vm_total_memory`、ホストやコンテナのメモリ、EMQXメモリアラーム。
- **推奨対応:** 接続、セッション、キュー、統合の増加を調査。EMQX 6.3.0以降は [`emqx ctl session-top`](../cli.md#session-top) で最も多くのMQTTペイロードバイトを保持するセッションや最長のメッセージキューを特定可能。OSによるプロセス終了を防ぐため、キャパシティ追加や増加源の削減を行う。EMQX組み込みシステムメモリアラームはデフォルトで70%。

**オーバーロード保護の活動**

- **早期警告条件:** オーバーロード保護のカウンターが増加、特に接続クローズや遅延タイムアウト。
- **関連シグナル:** `emqx_overload_protection_new_conn`、`emqx_overload_protection_delay_timeout`、`emqx_overload_protection_delay_ok`、`emqx_overload_protection_gc`、`emqx_overload_protection_hibernation`。これらはオーバーロード保護が有効な場合のみエクスポートされる。
- **推奨対応:** ブローカーはすでにリソース圧力を緩和中。CPU、メモリ、ランキュー、メールボックス、接続の変動とイベントを相関させる。さらなるクライアント影響を防ぐため負荷軽減やキャパシティ追加を検討。

**ディスクプレッシャー**

- **早期警告条件:** 空き容量が運用予備を下回るか、次のメンテナンスまでに枯渇すると予測される。
- **関連シグナル:** ホストやボリュームの空きバイト数、空きinode数、I/Oレイテンシ、ディスク増加率。
- **推奨対応:** 保持ポリシーに従いデータを削除するかボリュームを拡張。一般的な開始点は空き容量20%で警告、10%でクリティカルアラート。

**ブローカーキャパシティ**

- **早期警告条件:** 接続数、セッション数、サブスクリプション数、トピック数がテスト済みまたはライセンス上限に近づく。
- **関連シグナル:** `emqx_connections_count`、`emqx_sessions_count`、`emqx_subscriptions_count`、`emqx_topics_count`、EMQX Enterpriseでは `emqx_license_max_sessions`。
- **推奨対応:** キャパシティテスト結果と成長を比較。上限到達前にノード追加やトラフィック移動を行う。過去の `*_max` メトリクスを設定上限とみなさない。

### メッセージ配信と依存関係

**メッセージロス**

- **早期警告条件:** 予期しないドロップカウンターが増加。
- **関連シグナル:** `emqx_messages_dropped_*`、`emqx_delivery_dropped_*`。
- **推奨対応:** 原因を調査。キュー満杯、クォータ超過、受信最大数超過、期限切れメッセージのドロップは過負荷や誤った制限を示す可能性あり。`no_subscribers` や `no_local` のドロップは一部アプリケーションで想定される。

**認証・認可依存の健全性**

- **早期警告条件:** 有効なプロバイダーやソースが接続されていない（ステータスが `0`）、認証・認可のレイテンシが通常ピークを超える、認証失敗や認可拒否が予期せず増加。
- **関連シグナル:** `/api/v5/prometheus/auth` の `emqx_authn_enable`、`emqx_authn_status`、`emqx_authn_latency`、`emqx_authn_failed`、`emqx_authz_enable`、`emqx_authz_status`、`emqx_authz_latency`、`emqx_authz_deny`。
- **推奨対応:** 外部データベース、HTTPサービス、LDAPサーバー、ネットワーク、接続プールを確認。クライアントトラフィックと失敗のスパイクを相関させ、バックエンド問題と無効な認証情報、アプリケーション変更、攻撃を区別。

**データ統合の健全性**

- **早期警告条件:** 有効なコネクターやアクションが切断されている、`emqx_action_queuing` や `emqx_action_inflight` が排出されず増加、遅延応答、リトライ、失敗、ドロップが増加。
- **関連シグナル:** `/api/v5/prometheus/data_integration` の `emqx_connector_enable`、`emqx_connector_status`、`emqx_action_enable`、`emqx_action_status`、`emqx_action_queuing`、`emqx_action_inflight`、およびEMQXの `resource` アラーム。
- **推奨対応:** 外部サービスとネットワークを確認し、バッファ容量とリトライ動作を検証。キューやインフライトリクエストの増加は失敗やドロップの前兆となる。

### 有効期限リスク

**証明書およびライセンスの有効期限**

- **早期警告条件:** 組織の更新猶予期間内に有効期限が到来。
- **関連シグナル:** `emqx_cert_expiry_at`、EMQX Enterpriseでは `emqx_license_expiry_at`。
- **推奨対応:** 証明書やライセンスを更新しデプロイ。一般的な開始点は有効期限30日前に警告、7日前にクリティカルアラート。

### メトリクスの利用可能性を確認する

ダッシュボードに表示されるブローカーカウンターの説明は[統計とメトリクス](./metrics-and-stats.md)を参照してください。基本的なブローカー、認証・認可、データ統合のメトリクスは別々のPrometheusエンドポイントで公開されます。メトリクスの利用可能性はエディションや有効化された機能によって異なります。ルール作成前にデプロイメントの該当エンドポイントを確認してください。

## ログを集中管理し、選択的にアラートを設定する

### クラスター外部でログを収集する

ノードのログの唯一のコピーをそのノードに保持しないでください。ノード障害時に診断に必要な証拠が失われる可能性があります。すべてのノードからログをEMQXクラスター外の中央システムに送信し、クラスター、ノード、ノード役割、EMQXバージョン、アベイラビリティゾーンのラベルを付与してください。

[JSONログ形式](./log.md#log-format)を使用し、警告、エラー、クリティカルイベントを最低限保持してください。ログはコンソールやファイル出力から収集可能で、[OpenTelemetry](./opentelemetry/logs.md)経由でエクスポートも可能です。設定および本番収集のガイダンスは[ログ](./log.md)を参照してください。

ログコレクターと転送経路は、ヘルスメトリクスやアプリケーションログ量に依存しない明示的なハートビートで監視してください。ノードがログを生成しないだけでアラートを出さないでください。アイドル状態や正常なノードは設定された重大度で報告すべきログがない場合があります。

### ターゲットを絞ったログアラートを定義する

以下のイベントとガイダンスを参考にログベースのアラートルールを定義してください：

| 条件 | ログシグナル | アラートガイダンス |
| --- | --- | --- |
| Mria遅延観測失敗 | `prometheus_mria_shard_lag_refresh_exception` | 繰り返し発生する場合にアラート。エクスポーターはMria遅延をキャッシュしており、リフレッシュがタイムアウトすると前回値を継続エクスポートし安定して見える可能性あり。 |
| Erlang VMまたはノード間通信の圧力 | `busy_dist_port`、`long_schedule`、`long_gc`、Mnesia過負荷メッセージ | 持続的または繰り返し発生時にアラート。Mriaキュー、CPU、レイテンシと相関させる。これらのイベントはクライアントに見える劣化の前兆となる。 |
| Mriaレプリケーションまたはトポロジー障害 | `gap_in_the_tlog`、`mria_lb_split_brain` | 担当オペレーターに即時通知。構造化フィールドからノード、シャード、エージェント、期待シーケンス番号、実際のシーケンス番号を取得。 |
| セッションバッファペイロード圧力 | `session_buffer_high_watermark` | 警告ごとに調査。しきい値がセッション単位のリスクを示す場合はアラート設定。複数クライアントで警告発生やメモリ増加、`busy_dist_port` と相関する場合はアラート優先度を上げる。`clientid`、`mqueue_length`、`inflight_count`、`total_payload_bytes`、`total_payload_bytes_high_watermark` を取得し、[`emqx ctl session-top`](../cli.md#session-top) で最も多くペイロードを保持するセッションを特定。 |
| バッファリングまたはメッセージキュー圧力 | `data_bridge_buffer_overflow`、`unrecoverable_resource_error`、`dropped_msg_due_to_mqueue_is_full` | 予期しない発生やアプリケーション許容損失率を超えた場合にアラート。アクションおよびメッセージドロップカウンターと相関させる。 |
| 設定同期失敗 | `sync_data_from_node_failed`、`cluster_rpc_apply_failed` | 設定変更やノード起動中に即時アラート。すべてのノードが意図した設定に収束しているか確認。 |

すべての警告レベルログが即時通知を要するわけではありません。例えば認証失敗や不正なクライアントトラフィックは低頻度なら想定内です。選択した `msg` 値、重大度レベル、持続的なイベントレート、通常基準からの逸脱に基づきアラートを設定してください。予期しないクリティカルイベントは即時対応が必要とみなします。

### ログのスロットリングに注意する

EMQXは選択された繰り返しログイベントをスロットリングします。ログクエリは元のイベント数を過小評価する可能性があります。ダッシュボードやアラートに `log_events_throttled_during_last_period` を含め、その `dropped` フィールドで抑制されたメッセージを特定してください。詳細は[ログのスロットリング](./log.md#log-throttling)を参照。

## 障害検知は別途行う

[監視すべき先行指標](#leading-indicators-to-monitor)の指標は早期警告を提供しますが、障害検知アラートの代わりにはなりません。以下の条件はサービスや冗長性がすでに失われていることを示します。発生時に担当オペレーターへ即時通知するようアラートを設定してください：

- Prometheusの `up == 0`
- 合成MQTTチェックの失敗
- `emqx_cluster_nodes_running` が計画クラスターサイズを下回る
- `emqx_cluster_nodes_stopped` が増加
- `emqx_vm_uptime_ms` の予期しないリセット
- EMQXの `partition` アラーム

これらの障害発生前に、[監視すべき先行指標](#leading-indicators-to-monitor)の早期警告指標で悪化を検知し、メンテナンスを計画する十分な時間を確保してください。

## Prometheusアラートルールの例

以下の設定はPrometheusアラートルールの出発点としてコピー可能です。本番環境で使用する前に以下を確認してください：

- 例では[Prometheusサーバー設定例](./prometheus.md#prometheus-server-configuration-example)のジョブ名を使用。スクレイプジョブ名が異なる場合は `job` マッチャーを更新してください。
- クラスター喪失ルールは計画クラスターサイズ3ノードを想定。実際のサイズに置き換えてください。
- 他の例示しきい値はデプロイメントに適した値に置き換えてください。
- Prometheusジョブに複数クラスターが含まれる場合は設定収束ルールをクラスターラベルで集約してください。
- Mriaトレンドルールにはピークトラフィック基準に基づく絶対しきい値も追加してください。例は持続的な正の傾きを検出しますが、大きく安定したバックログもアラート対象にすべきです。
- ディスク枯渇、メモリ制限、コンテナ再起動、ネットワーク健全性に関してはホストやプラットフォーム固有のルールを追加してください。

```yaml
groups:
  - name: emqx-early-warning
    rules:
      - alert: EMQXMRIAReplicationLagGrowing
        expr: deriv(emqx_mria_lag{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Mria replication lag is growing on {{ $labels.instance }} shard {{ $labels.shard }}"

      - alert: EMQXMRIAReplicationQueueGrowing
        expr: deriv(emqx_mria_server_mql{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_message_queue_len{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_replayq_len{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "A Mria replication queue is growing on {{ $labels.instance }} shard {{ $labels.shard }}"

      - alert: EMQXSustainedHighCPU
        expr: emqx_vm_cpu_use{job="emqx_stats"} > 80
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX CPU usage is high on {{ $labels.instance }}"

      - alert: EMQXSustainedHighMemory
        expr: 100 * emqx_vm_used_memory{job="emqx_stats"} / emqx_vm_total_memory{job="emqx_stats"} > 70
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX host memory usage is high on {{ $labels.instance }}"

      - alert: EMQXOverloadProtectionActive
        expr: sum by (instance) (increase(emqx_overload_protection_new_conn{job="emqx_stats"}[5m])) > 0 or sum by (instance) (increase(emqx_overload_protection_delay_timeout{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX overload protection is closing or timing out client work on {{ $labels.instance }}"

      - alert: EMQXConfigurationNotConverged
        expr: max(emqx_conf_sync_txid{job="emqx_stats"}) != min(emqx_conf_sync_txid{job="emqx_stats"})
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "EMQX nodes report different configuration transaction IDs"

      - alert: EMQXDeliveryQueueFullDrops
        expr: sum by (instance) (increase(emqx_delivery_dropped_queue_full{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX dropped messages because a delivery queue was full"

      - alert: EMQXActionQueueGrowing
        expr: deriv(emqx_action_queuing{job="emqx_data_integration"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX data integration action {{ $labels.id }} has a growing queue on {{ $labels.instance }}"

      - alert: EMQXActionFailures
        expr: sum by (instance, id) (increase(emqx_action_failed{job="emqx_data_integration"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX data integration action {{ $labels.id }} is failing"

      - alert: EMQXAuthenticationBackendUnavailable
        expr: (emqx_authn_enable{job="emqx_auth"} == 1 and on (instance, id) emqx_authn_status{job="emqx_auth"} == 0) or (emqx_authz_enable{job="emqx_auth"} == 1 and on (instance, type) emqx_authz_status{job="emqx_auth"} == 0)
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "An enabled EMQX authentication or authorization backend is unavailable on {{ $labels.instance }}"

      - alert: EMQXCertificateExpiresSoon
        expr: emqx_cert_expiry_at{job="emqx_stats"} > 0 and (emqx_cert_expiry_at{job="emqx_stats"} - time()) < 30 * 24 * 60 * 60
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "EMQX listener certificate expires within 30 days"

  - name: emqx-failure-detection
    rules:
      - alert: EMQXMetricsTargetDown
        expr: up{job="emqx_stats"} == 0
        for: 30s
        labels:
          severity: critical
        annotations:
          summary: "EMQX metrics target {{ $labels.instance }} is unreachable"

      - alert: EMQXClusterLostNode
        expr: min by (job) (emqx_cluster_nodes_running{job="emqx_stats"}) < 3
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "EMQX cluster has fewer than 3 running nodes"
```

カウンタメトリクスは通常増加のみです。絶対値ではなく、増加率や時間窓での増加に対してアラートを設定してください。リソースゲージには `for` 期間を設定し、一時的なトラフィックスパイクで不要なアラートが発生しないようにします。

## アラートを実用的にする

1. **アラートのコンテキストと所有者を定義する。**

   各アクション可能なアラートは、影響を受けるクラスター、該当する場合はノード、現在値、しきい値を特定すべきです。ダッシュボードリンク、所有者、調査・緩和・解決方法を記載したランブックも含めてください。ランブックには状態確認方法、サービス保護、冗長性回復、スケール、リバランス、再起動、修復の判断基準を記載します。

2. **アラート通知と復旧をテストする。**

   信頼する前にアラート経路全体をテストしてください。非本番環境や承認済みテスト期間中に、スクレイプターゲット停止、テストしきい値の引き下げ、テスト統合の切断を意図的に行い、アラートが正しいオペレーターに届き、十分なコンテキストを含み、復旧後にクリアされることを確認してください。

3. **メンテナンス手順を準備する。**

   警告アラートを使い、冗長性が残っている間にメンテナンスをスケジュールしてください。クラスター変更前にバックアップの利用可能性、残存ノードの負荷耐性、アラートシステムの健全性を確認してください。関連手順には[バックアップとリストア](../backup-restore.md)、[ノード退避とクラスター負荷リバランス](../cluster/rebalancing.md)、[EMQX Enterpriseローリングアップグレード](../../get-started/deploy/rolling-upgrades.md)があります。

## 本番環境準備チェックリスト

- すべてのEMQXノードとそのホストまたはコンテナが外部モニタリングシステムで可視化されている。
- 組み込みアラームがEMQX外部に転送され、テスト済みである。
- すべてのノードの警告、エラー、クリティカルログが集中管理され、収集パイプラインが監視されている。
- 外部の合成MQTTチェックが本番クライアント経路をカバーしている。
- Mriaレプリケーション、設定収束、ランタイムバックログのアラートに所有者とランブックが定義されている。
- オーバーロード保護、CPU、メモリ、ディスク、ブローカーキャパシティのアラートに所有者とランブックが定義されている。
- 認証・認可、メッセージドロップ、データ統合のアラートに所有者とランブックが定義されている。
- 証明書とライセンスの有効期限アラートに所有者とランブックが定義されている。
- 選択されたMria、VMプレッシャー、バッファオーバーフロー、設定同期ログイベントに対して、レートベースまたは即時アラートが重大度に応じて設定されている。
- ターゲットダウン、合成MQTT、クラスターサイズ、パーティションのアラートが障害を検知し、担当オペレーターに即時通知している。
- 警告しきい値は通常のメンテナンスおよびキャパシティプロビジョニングに十分な猶予を持つ。
- ダッシュボードは現在値と関連する業務サイクルのトレンドを表示している。
- アラート通知、バックアップ復元、ローリングメンテナンス手順が定期的にテストされている。
