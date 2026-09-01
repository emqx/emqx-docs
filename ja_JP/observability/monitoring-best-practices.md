# 本番環境モニタリングのベストプラクティス

本番環境でのデプロイメントでは、EMQXダッシュボード以上のモニタリングが必要です。ダッシュボードは現在のブローカー状態を表示しますが、ブローカーやホストが利用不可になった場合にオペレーターへ通知することはできません。本ページのモニタリングガイダンスは、サービス停止、冗長性喪失、リソース枯渇を早期に検知し、対応可能なタイミングを提供します。

このガイダンスはEMQX Enterpriseの本番環境デプロイメントに適用されます。例示した閾値は出発点として扱い、サービスレベル目標（SLO）、テスト済みの容量、トラフィックパターン、復旧時間に応じて調整してください。

## 本番環境モニタリングシステムの設計

モニタリングシステムを設計する際は、以下の原則に従ってください。

1. **EMQXのメトリクスを外部モニタリングシステムにエクスポートする。**

   包括的なモニタリングには、[PrometheusのPullモード](./prometheus.md#configure-pull-mode-integration)が推奨されます。ロードバランサー経由ではなく、各EMQXノードを直接スクレイプしてください。これにより、障害や孤立したノードが正常なノードに隠されることを防げます。すべてのターゲットに対してPrometheusの`up`メトリクスを監視してください。

2. **EMQX組み込みアラームを転送する。**

   環境に合わせてEMQX組み込みアラームの閾値を設定し、[Webhookまたはシステムトピック](./alarms.md#get-alarms)を使ってアラームイベントを外部通知システムに送信してください。オペレーターがダッシュボードのアラームに気づくことに依存しないでください。

3. **クラスター外部からエンドツーエンドのMQTTチェックを実行する。**

   合成クライアントは、本番クライアントと同じロードバランサー、TLSリスナー、認証経路を通じて接続する必要があります。クライアントは一意に識別可能なメッセージをパブリッシュし、サブスクリプションで受信し、合計レイテンシを測定します。このチェックは、ブローカーのメトリクスだけでは検知できない障害を検出します。

4. **ホストまたはコンテナプラットフォームを監視する。**

   EMQXはOS、Kubernetes、クラウドプロバイダーの監視に代わるものではありません。CPUスロットリング、メモリプレッシャー、ディスク容量とレイテンシ、ファイルディスクリプタ使用率、ネットワークエラー、コンテナ再起動、時刻同期状態を収集してください。

5. **ログを中央集約する。**

   すべてのノードから警告、エラー、クリティカルログをEMQXクラスター外のストレージに送信してください。構造化された`msg`、`node`などのコンテキストフィールドにマッチするアラートルールを作成しやすいため、JSON形式のログを推奨します。ログはメトリクスや組み込みアラームで表現されない状態を明らかにします。

6. **モニタリングをEMQXから独立させる。**

   EMQXノード、アベイラビリティゾーン、またはクラスター全体が利用不可になった場合でも、モニタリングおよび通知経路は利用可能でなければなりません。

::: tip

検知に必要な時間より短い間隔でメトリクスを収集してください。例えば、15秒間隔のスクレイプと2回連続失敗でアラートを出す設定なら、単一のスクレイプ失敗だけで1分以内に到達不能なターゲットを検知可能です。

:::

## SLO、容量ベースライン、アラート閾値の設定

固定の閾値を本番にコピーする代わりに、以下のプロセスを用いてください。

1. 接続成功率、パブリッシュから配信成功率、レイテンシなど、ユーザーに見えるSLOを定義する。
2. 代表的な[パフォーマンステスト](../performance/overview.md)を実施し、飽和前のリソース使用率、メッセージレート、レイテンシを記録する。
3. 少なくとも1つの通常の業務サイクルを観察し、日次または週次のピークを特定する。
4. 警告閾値はテスト済みの安全容量より低く設定し、容量追加やメンテナンスの余裕時間を確保する。クリティカル閾値は即時対応が必要なポイントに設定する。
5. トラフィック増加、トポロジ変更、アップグレード、永続セッションやデータ統合の変更後に閾値を見直す。

固定パーセンテージのみでアラートを出すのは避けてください。ディスク枯渇が24時間以内に予測される、接続数が1週間以内にテスト済み容量に達するなどのトレンドや予測アラートは、より有用な保守リードタイムを提供します。

## 監視すべき先行指標

予防的なアラートは、クラスターがまだトラフィックを処理している間に状態悪化を検知すべきです。調査や保守のための時間を残す警告閾値と、即時対応が必要なクリティカル閾値を設定してください。以下の各条件について、関連するシグナルとオペレーターへの推奨対応を示します。

### クラスターおよびランタイムの健全性

**Mriaレプリケーション圧力**

- **早期警告条件:** レプリケーション遅延やキューが通常のピークを超えて持続、または減少せず増加し続ける。
- **関連シグナル:** レプリカントノードでは`emqx_mria_lag`、`emqx_mria_message_queue_len`、`emqx_mria_replayq_len`を監視。コアノードでは`emqx_mria_server_mql`、`emqx_mria_weight`を監視。
- **推奨対応:** レプリカントとその上流コアノードのログとメトリクスを相関分析。遅延収集失敗、分配ポートの過負荷、長時間のスケジューラ停止、Mnesia過負荷、Mriaレプリケーションエラーを確認。ネットワークレイテンシ・ロス、CPU、ディスクI/Oもチェック。書き込み圧力を軽減するか、コア容量を増強し、レプリカントの遅れ拡大を防ぐ。

`emqx_mria_lag`はレプリカントシャードが上流コアシャードに対して遅れているトランザクション数であり、秒数ではありません。書き込みバースト時の短時間のスパイクは正常です。代表的なピークトラフィックで観測された最大値を超えて持続する場合、またはMriaキューメトリクスとともに持続的な増加傾向を示す場合にアラートを出してください。ノードと`shard`の両方でアラートをグルーピングしてください。シャードごとに状態が異なることがあるためです。各Mriaメトリクスの詳細は[監視とデバッグ](../deploy/cluster/mria-introduction.md#monitor-and-debug)を参照してください。

**設定収束**

- **早期警告条件:** ノード間で`emqx_conf_sync_txid`が通常の設定展開時間を超えて異なる。
- **関連シグナル:** すべてのノードの`emqx_conf_sync_txid`と設定同期ログ。
- **推奨対応:** 追加の設定変更を停止し、遅れているノードを特定。クラスター接続性と設定同期エラーを調査。保守や次の設定変更前に収束を回復する。

**ランタイムバックログ**

- **早期警告条件:** ランキューやメールボックスサイズが基準値を超えて持続。
- **関連シグナル:** `emqx_vm_run_queue`、`emqx_vm_mnesia_tm_mailbox_size`、`emqx_vm_broker_pool_max_mailbox_size`、組み込みの過負荷アラーム、`busy_dist_port`イベント。
- **推奨対応:** 過負荷の持続、ストレージの遅延、クラスター通信の問題を調査し、リクエストレイテンシやキューのさらなる増加を防ぐ。

### リソースと容量

**CPUプレッシャー**

- **早期警告条件:** CPU使用率が通常ピークを10〜15分間超過。
- **関連シグナル:** `emqx_vm_cpu_use`、ホストCPU、ロード、コンテナスロットリング。
- **推奨対応:** 増加の原因となるワークロードや統合を特定。トラフィックの再分散や容量追加を行い飽和を防ぐ。EMQXの組み込みCPUアラームはデフォルトで80%に設定。

**メモリプレッシャー**

- **早期警告条件:** メモリ使用率が警告閾値を超えるか、ホストまたはコンテナの制限に近づく。
- **関連シグナル:** `emqx_vm_used_memory`、`emqx_vm_total_memory`、ホストまたはコンテナのメモリ、EMQXメモリアラーム。
- **推奨対応:** 接続数、セッション、キュー、統合の増加を調査。OSによるプロセス終了を防ぐため、容量追加または増加源の削減を行う。EMQXの組み込みシステムメモリアラームはデフォルトで70%。

**過負荷保護の活動**

- **早期警告条件:** 過負荷保護のカウンターが増加、特に接続切断や遅延タイムアウト。
- **関連シグナル:** `emqx_overload_protection_new_conn`、`emqx_overload_protection_delay_timeout`、`emqx_overload_protection_delay_ok`、`emqx_overload_protection_gc`、`emqx_overload_protection_hibernation`。過負荷保護が有効な場合のみエクスポートされる。
- **推奨対応:** ブローカーは既にリソース圧力を緩和中。CPU、メモリ、ランキュー、メールボックス、接続の変動と相関させて分析。さらなるクライアント影響を防ぐため負荷軽減または容量追加を検討。

**ディスクプレッシャー**

- **早期警告条件:** 空き容量が運用予備を下回るか、次回メンテナンスまでに枯渇すると予測される。
- **関連シグナル:** ホストまたはボリュームの空きバイト数、空きinode数、I/Oレイテンシ、ディスク増加率。
- **推奨対応:** 保持ポリシーに従いデータ削除またはボリューム拡張を実施。一般的な目安は警告が空き容量20%、クリティカルが10%。

**ブローカー容量**

- **早期警告条件:** 接続数、セッション数、サブスクリプション数、トピック数がテスト済みまたはライセンス上限に近づく。
- **関連シグナル:** `emqx_connections_count`、`emqx_sessions_count`、`emqx_subscriptions_count`、`emqx_topics_count`、EMQX Enterpriseでは`emqx_license_max_sessions`。
- **推奨対応:** 容量テスト結果と比較し、上限到達前にノード追加やトラフィック移動を行う。過去の`*_max`ゲージ値を設定容量の上限とみなさないこと。

### メッセージ配信と依存関係

**メッセージロス**

- **早期警告条件:** 予期しないドロップカウンターの増加。
- **関連シグナル:** `emqx_messages_dropped_*`、`emqx_delivery_dropped_*`。
- **推奨対応:** 原因を調査。キュー満杯、クォータ超過、受信最大数超過、期限切れメッセージのドロップは過負荷や誤設定を示す場合がある。`no_subscribers`や`no_local`のドロップは一部アプリケーションで予期される。

**認証・認可依存性の健全性**

- **早期警告条件:** 有効なプロバイダーやソースが未接続（ステータス`0`）、認証・認可レイテンシが通常ピークを超過、認証失敗や認可拒否が予期せず増加。
- **関連シグナル:** `/api/v5/prometheus/auth`の`emqx_authn_enable`、`emqx_authn_status`、`emqx_authn_latency`、`emqx_authn_failed`、`emqx_authz_enable`、`emqx_authz_status`、`emqx_authz_latency`、`emqx_authz_deny`。
- **推奨対応:** 外部DB、HTTPサービス、LDAPサーバー、ネットワーク、接続プールを確認。障害の急増をクライアントトラフィックと相関させ、バックエンド問題か無効な資格情報、アプリ変更、攻撃かを判別。

**データ統合の健全性**

- **早期警告条件:** 有効なコネクターやアクションが切断、`emqx_action_queuing`や`emqx_action_inflight`が減少せず増加、遅延応答、リトライ、失敗、ドロップが増加。
- **関連シグナル:** `/api/v5/prometheus/data_integration`の`emqx_connector_enable`、`emqx_connector_status`、`emqx_action_enable`、`emqx_action_status`、`emqx_action_queuing`、`emqx_action_inflight`、およびEMQXの`resource`アラーム。
- **推奨対応:** 外部サービスとネットワークを確認し、バッファ容量とリトライ動作を検証。キューやインフライトリクエストの増加は失敗やドロップの前兆となる。

### 有効期限リスク

**証明書およびライセンスの有効期限**

- **早期警告条件:** 組織の更新リードタイム内に有効期限が迫る。
- **関連シグナル:** `emqx_cert_expiry_at`、EMQX Enterpriseでは`emqx_license_expiry_at`。
- **推奨対応:** 証明書またはライセンスを更新しデプロイ。一般的な目安は有効期限30日前に警告、7日前にクリティカルアラート。

### メトリクスの利用可能性を確認

ダッシュボードに表示されるブローカーカウンターの説明は[統計とメトリクス](./metrics-and-stats.md)を参照してください。基本的なブローカー、認証・認可、データ統合のメトリクスは別々のPrometheusエンドポイントで公開されます。メトリクスの利用可能性はエディションや有効化された機能によって異なります。ルール作成前にデプロイメントの該当エンドポイントを確認してください。

## ログを中央集約し選択的にアラート設定

### クラスター外部でログを収集

ノードのログをそのノードにのみ保持しないでください。ノード障害時に診断に必要な証拠が失われる可能性があります。すべてのノードからログをEMQXクラスター外の中央システムに送信し、クラスター、ノード、ノード役割、EMQXバージョン、アベイラビリティゾーンのラベルを付与してください。

[JSONログ形式](./log.md#log-format)を使用し、警告、エラー、クリティカルイベントを最低限保持してください。ログはコンソールやファイル出力から収集可能で、[OpenTelemetry](./opentelemetry/logs.md)経由でエクスポートも可能です。設定と本番収集のガイダンスは[ログ](./log.md)を参照してください。

ログ収集システムと転送経路は、ヘルスメトリクスやアプリケーションログ量に依存しない明示的なハートビートで監視してください。ノードがログを生成しないだけでアラートを出さないでください。アイドル状態や正常なノードは設定された重大度で報告すべきログがない場合があります。

### 目的に応じたログアラートを定義

以下のイベントとガイダンスを参考に、ログベースのアラートルールを定義してください。

| 条件 | ログシグナル | アラートガイダンス |
| --- | --- | --- |
| Mria遅延観測失敗 | `prometheus_mria_shard_lag_refresh_exception` | 繰り返し発生した場合にアラート。エクスポーターはMria遅延をキャッシュしており、リフレッシュタイムアウト時は前回値を安定してエクスポートし続ける可能性あり。 |
| Erlang VMまたはノード間通信圧力 | `busy_dist_port`、`long_schedule`、`long_gc`、Mnesia過負荷メッセージ | 持続的な発生率や繰り返しイベントでアラート。Mriaキュー、CPU、レイテンシと相関させる。これらはクライアントに見える劣化の前兆となる。 |
| Mriaレプリケーションまたはトポロジ障害 | `gap_in_the_tlog`、`mria_lb_split_brain` | 直ちに担当オペレーターに通知。構造化フィールドからノード、シャード、エージェント、期待シーケンス番号、実際のシーケンス番号を取得。 |
| バッファリングまたはメッセージキュー圧力 | `data_bridge_buffer_overflow`、`unrecoverable_resource_error`、`dropped_msg_due_to_mqueue_is_full` | 予期せぬ発生や許容損失率を超えた場合にアラート。アクションおよびメッセージドロップカウンターと相関させる。 |
| 設定同期失敗 | `sync_data_from_node_failed`、`cluster_rpc_apply_failed` | 設定変更やノード起動中に直ちにアラート。すべてのノードが意図した設定に収束しているか確認。 |

すべての警告レベルログが即時通知を要するわけではありません。例えば認証失敗や不正なクライアントトラフィックは低頻度であれば想定内です。選択した`msg`値、重大度レベル、持続的なイベント発生率、通常基準からの逸脱に基づいてアラートを設定してください。予期せぬクリティカルイベントは即時対応が必要とみなします。

### ログスロットリングを考慮

EMQXは選択された繰り返しログイベントをスロットリングします。ログクエリは元のイベント数を過小評価する可能性があります。ダッシュボードやアラートに`log_events_throttled_during_last_period`を含め、その`dropped`フィールドで抑制されたメッセージを把握してください。詳細は[ログスロットリング](./log.md#log-throttling)を参照。

## 障害検知は別途行う

[監視すべき先行指標](#leading-indicators-to-monitor)は早期警告を提供しますが、障害検知アラートに代わるものではありません。以下の条件はサービスまたは冗長性が既に失われていることを示します。これらの条件が発生した場合は、直ちに担当オペレーターに通知するアラートを設定してください。

- Prometheusの`up == 0`
- 合成MQTTチェックの失敗
- `emqx_cluster_nodes_running`が計画クラスターサイズを下回る
- `emqx_cluster_nodes_stopped`が増加
- `emqx_vm_uptime_ms`の予期しないリセット
- EMQXの`partition`アラーム

これらの障害発生前に[監視すべき先行指標](#leading-indicators-to-monitor)で劣化を検知し、保守スケジュールの余裕を確保してください。

## Prometheusアラートルールの例

以下の設定はPrometheusアラートルールの出発点としてコピー可能です。本番環境で使用する前に以下を確認してください。

- 例は[Prometheusサーバー設定例](./prometheus.md#prometheus-server-configuration-example)のジョブ名を使用しています。スクレイプジョブの名前が異なる場合は`job`マッチャーを更新してください。
- クラスター喪失ルールは計画クラスターサイズ3ノードを想定しています。`3`を実際のクラスターサイズに置き換えてください。
- 他の例示閾値もデプロイメントに適した値に置き換えてください。
- Prometheusジョブに複数クラスターが含まれる場合、設定収束ルールはクラスターラベルで集約してください。
- Mriaトレンドルールにはピークトラフィック基準の絶対閾値も追加してください。例は持続的な増加傾向を検知しますが、大きく安定したバックログもアラート対象とすべきです。
- ディスク枯渇、メモリ制限、コンテナ再起動、ネットワーク健全性についてはホストやプラットフォーム固有のルールを追加してください。

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
          summary: "Mriaレプリケーション遅延が{{ $labels.instance }}のシャード{{ $labels.shard }}で増加中"

      - alert: EMQXMRIAReplicationQueueGrowing
        expr: deriv(emqx_mria_server_mql{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_message_queue_len{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_replayq_len{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Mriaレプリケーションキューが{{ $labels.instance }}のシャード{{ $labels.shard }}で増加中"

      - alert: EMQXSustainedHighCPU
        expr: emqx_vm_cpu_use{job="emqx_stats"} > 80
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "{{ $labels.instance }}でEMQXのCPU使用率が高い"

      - alert: EMQXSustainedHighMemory
        expr: 100 * emqx_vm_used_memory{job="emqx_stats"} / emqx_vm_total_memory{job="emqx_stats"} > 70
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "{{ $labels.instance }}でEMQXホストのメモリ使用率が高い"

      - alert: EMQXOverloadProtectionActive
        expr: sum by (instance) (increase(emqx_overload_protection_new_conn{job="emqx_stats"}[5m])) > 0 or sum by (instance) (increase(emqx_overload_protection_delay_timeout{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "{{ $labels.instance }}でEMQX過負荷保護がクライアント処理を切断またはタイムアウト"

      - alert: EMQXConfigurationNotConverged
        expr: max(emqx_conf_sync_txid{job="emqx_stats"}) != min(emqx_conf_sync_txid{job="emqx_stats"})
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "EMQXノード間で設定トランザクションIDが異なる"

      - alert: EMQXDeliveryQueueFullDrops
        expr: sum by (instance) (increase(emqx_delivery_dropped_queue_full{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQXで配信キュー満杯によりメッセージがドロップされた"

      - alert: EMQXActionQueueGrowing
        expr: deriv(emqx_action_queuing{job="emqx_data_integration"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "{{ $labels.instance }}でEMQXデータ統合アクション{{ $labels.id }}のキューが増加中"

      - alert: EMQXActionFailures
        expr: sum by (instance, id) (increase(emqx_action_failed{job="emqx_data_integration"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "{{ $labels.instance }}でEMQXデータ統合アクション{{ $labels.id }}が失敗中"

      - alert: EMQXAuthenticationBackendUnavailable
        expr: (emqx_authn_enable{job="emqx_auth"} == 1 and on (instance, id) emqx_authn_status{job="emqx_auth"} == 0) or (emqx_authz_enable{job="emqx_auth"} == 1 and on (instance, type) emqx_authz_status{job="emqx_auth"} == 0)
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "{{ $labels.instance }}で有効なEMQX認証または認可バックエンドが利用不可"

      - alert: EMQXCertificateExpiresSoon
        expr: emqx_cert_expiry_at{job="emqx_stats"} > 0 and (emqx_cert_expiry_at{job="emqx_stats"} - time()) < 30 * 24 * 60 * 60
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "EMQXリスナー証明書が30日以内に有効期限切れ"

  - name: emqx-failure-detection
    rules:
      - alert: EMQXMetricsTargetDown
        expr: up{job="emqx_stats"} == 0
        for: 30s
        labels:
          severity: critical
        annotations:
          summary: "EMQXメトリクスターゲット{{ $labels.instance }}が到達不能"

      - alert: EMQXClusterLostNode
        expr: min by (job) (emqx_cluster_nodes_running{job="emqx_stats"}) < 3
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "EMQXクラスターの稼働ノード数が3未満"
```

カウンタメトリクスは通常増加のみです。絶対値ではなく、一定期間の増加率や増加量でアラートを設定してください。リソースゲージには`for`期間を設定し、短時間のトラフィックスパイクで不要なアラートが発生しないようにします。

## アラートを実効的にする

1. **アラートのコンテキストと担当者を定義する。**

   実効的なアラートは、影響を受けるクラスター、該当する場合はノード、現在値、閾値を特定できる必要があります。ダッシュボードリンク、担当者、調査・緩和・解決手順を記載したランブックも含めてください。ランブックには、状態の確認方法、サービス保護、冗長性回復、スケール、リバランス、再起動、修復の判断基準を記述します。

2. **アラート通知と復旧をテストする。**

   依存前にアラート経路全体をテストしてください。非本番環境や承認済みテスト期間中に、スクレイプターゲット停止、テスト閾値の引き下げ、テスト統合の切断を意図的に行い、アラートが正しい担当者に届き、十分なコンテキストを含み、復旧後にクリアされることを確認してください。

3. **保守手順を準備する。**

   警告アラートを使って冗長性が残るうちに保守を計画してください。クラスター変更前にバックアップの利用可能性、残存ノードの負荷耐性、アラートシステムの健全性を確認します。関連手順は[バックアップとリストア](../operations/backup-restore.md)、[ノード退避とクラスター負荷リバランス](../deploy/cluster/rebalancing.md)、[EMQX Enterpriseローリングアップグレード](../deploy/rolling-upgrades.md)を参照してください。

## 本番環境準備チェックリスト

- すべてのEMQXノードおよびそのホスト・コンテナが外部モニタリングシステムに可視化されている。
- 組み込みアラームがEMQX外部に転送され、テスト済みである。
- すべてのノードから警告、エラー、クリティカルログが中央に保存され、収集パイプラインが監視されている。
- 外部の合成MQTTチェックが本番クライアント経路をカバーしている。
- Mriaレプリケーション、設定収束、ランタイムバックログのアラートに担当者とランブックが定義されている。
- 過負荷保護、CPU、メモリ、ディスク、ブローカー容量のアラートに担当者とランブックが定義されている。
- 認証・認可、メッセージドロップ、データ統合のアラートに担当者とランブックが定義されている。
- 証明書およびライセンス有効期限アラートに担当者とランブックが定義されている。
- 選択したMria、VMプレッシャー、バッファオーバーフロー、設定同期のログイベントに対して、レートベースまたは即時アラートが設定されている。
- ターゲットダウン、合成MQTT、クラスターサイズ、パーティションのアラートが障害を検知し、担当オペレーターに即時通知する。
- 警告閾値はチームの通常保守および容量プロビジョニングプロセスに十分な余裕を残している。
- ダッシュボードは現在値と関連する業務サイクルのトレンドを表示している。
- アラート通知、バックアップ復元、ローリング保守手順が定期的にテストされている。
