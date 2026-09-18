# ブローカーのヘルス指標

このページは、EMQXブローカーの監視に最も役立つPrometheusメトリクスの厳選リファレンスです。これらのメトリクスの公開とスクレイピング方法については、[Prometheusとの統合](./prometheus.md)を併せてご参照ください。

指標は以下の4つの領域に分類されています。

1. **システム**：オペレーティングシステムおよびErlang VMのリソース。
2. **ブローカー**：接続およびメッセージトラフィック、ブローカーの状態。
3. **認証と認可**：接続時のアイデンティティチェックおよびメッセージごとのACL判定。
4. **データ統合**：ルール、アクション、コネクター、ブリッジ。

すべてのメトリクスはEMQXのPrometheusエンドポイント（`/api/v5/prometheus/stats`、`/api/v5/prometheus/auth`、`/api/v5/prometheus/data_integration`）で公開されています。エンドポイントの詳細や`mode`クエリパラメータについては、[Prometheusとの統合](./prometheus.md#configure-pull-mode-integration)をご覧ください。

::: tip コレクターのデフォルト設定について

EMQXネイティブのメトリクスは、上流のErlang VMコレクターに依存しません。EMQX 6.3では、`prometheus.collectors`の下にある`vm_dist`、`vm_statistics`、`vm_system_info`、`vm_memory`はデフォルトで`enabled`です。`mnesia`および`vm_msacc`はデフォルトで`disabled`となっています。

既存のレガシーフォーマットの設定は従来の動作を維持します。すべての6つのフラットな`prometheus.*_collector`フィールドはデフォルトで`disabled`であり、明示的に設定された値は変更されません。

:::

## システム

ハードウェア層に最も近い信号です。ブローカーが異常な場合、通常これらのいずれかが最初に変化します。

### CPU

| メトリクス | 説明 |
|------------|-------|
| `emqx_vm_cpu_use` | 使用中のCPU割合（パーセント）。 |
| `emqx_vm_cpu_idle` | アイドル状態のCPU割合（パーセント）。 |

### メモリ

`erlang_vm_memory_`プレフィックスのメトリクスは、`prometheus.collectors.vm_memory`が`enabled`である必要があります。

| メトリクス | 説明 |
|------------|-------|
| `emqx_vm_total_memory` | システム全体のメモリ容量（バイト）。 |
| `emqx_vm_used_memory` | 使用中のシステムメモリ（バイト）。 |
| `erlang_vm_memory_bytes` | Erlang VMによって割り当てられた総メモリ（`kind="system"`または`kind="processes"`）。 |
| `erlang_vm_memory_processes_bytes` | Erlangプロセスに割り当てられたメモリ（`usage="used"`または`usage="free"`）。 |
| `erlang_vm_memory_system_bytes` | アトム、バイナリ、ロード済みコード、ETSテーブル、その他オーバーヘッドを含むシステムメモリの使用状況。 |
| `erlang_vm_memory_atom_bytes` | アトムに割り当てられたメモリ（`usage="used"`または`usage="free"`）。 |

### ファイルディスクリプター

| メトリクス | 説明 |
|------------|-------|
| `emqx_vm_max_fds` | ブローカープロセスのソフトFD上限。 |

### Erlangプロセスとスケジューラー負荷

| メトリクス | 説明 |
|------------|-------|
| `emqx_vm_run_queue` | 現在のスケジューラーのランキュー長。継続的にゼロ以外の場合、CPU飽和を示します。 |
| `erlang_vm_processes` | 現在のErlangプロセス数（`vm_system_info`コレクターが有効である必要があります）。 |
| `erlang_vm_process_limit` | 設定された最大Erlangプロセス数。 |

### 内部メールボックス監視

| メトリクス | 説明 |
|------------|-------|
| `emqx_vm_mnesia_tm_mailbox_size` | Mnesiaトランザクションマネージャのメールボックス深度。高い値はトランザクション競合を示します。 |
| `emqx_vm_broker_pool_max_mailbox_size` | ブローカーディスパッチプール内の最大メールボックスサイズ。高い値はサブスクライバー側のバックプレッシャーを示します。 |

### アップタイム

| メトリクス | 説明 |
|------------|-------|
| `emqx_vm_uptime_ms` | ブローカーの稼働時間（ミリ秒）。急激に小さな値に下がる場合はノードの再起動を意味します。 |

### クラスター複製のヘルス（Mria）

| メトリクス | 説明 |
|------------|-------|
| `emqx_mria_lag` | レプリカノードごとの複製遅延。 |
| `emqx_mria_replicants` | レプリカ数。 |
| `emqx_mria_bootstrap_time` | 最後のブートストラップにかかった時間。 |
| `emqx_mria_message_queue_len` | Mriaのメールボックス長。 |

### オーバーロード保護

| メトリクス | 説明 |
|------------|-------|
| `emqx_overload_protection_new_conn` | オーバーロードによって拒否された接続数。 |
| `emqx_overload_protection_gc` | オーバーロード保護によって強制されたガベージコレクション。 |
| `emqx_overload_protection_hibernation` | トリガーされたプロセスのハイバネーション数。 |
| `emqx_overload_protection_delay_ok` | 遅延適用が成功した回数。 |
| `emqx_overload_protection_delay_timeout` | 遅延試行がタイムアウトした回数。 |

## ブローカー

コアの運用信号です。メッセージ関連のカウンターのレートを監視し、特に`dropped`系列に注意してください。

### クラスターのトポロジー

| メトリクス | 説明 |
|------------|-------|
| `emqx_cluster_nodes_running` | 稼働中のクラスター・ノード数。 |
| `emqx_cluster_nodes_stopped` | 停止中のクラスター・ノード数。0より大きい場合はアラート対象。 |
| `emqx_conf_sync_txid` | 適用された最後のクラスター設定トランザクションID。ノード間で異なる場合は同期問題を示します。 |

### ライセンス（エンタープライズ）

| メトリクス | 説明 |
|------------|-------|
| `emqx_license_expiry_at` | ライセンスの有効期限（UNIXエポック秒）。 |
| `emqx_license_issued_at` | ライセンス発行日時。 |
| `emqx_license_max_sessions` | ライセンスによるセッション上限。 |
| `emqx_cert_expiry_at` | リスナー証明書の有効期限。 |

### 接続、セッション、チャネル

| メトリクス | 説明 |
|------------|-------|
| `emqx_connections_count` | 現在の接続数。 |
| `emqx_connections_max` | 起動以降の最大接続数。 |
| `emqx_live_connections_count` | 現在接続中（TCP接続中）のクライアント数。 |
| `emqx_live_connections_max` | 最大ライブ接続数。 |
| `emqx_sessions_count` | アクティブなセッション数（現在切断中の永続セッションも含む）。 |
| `emqx_sessions_max` | 最大セッション数。 |
| `emqx_cluster_sessions_count` | クラスター全体のセッション数。 |
| `emqx_cluster_sessions_max` | クラスター全体の最大セッション数。 |
| `emqx_channels_count` | チャネルプロセス数（接続クライアントごとに1つ）。 |
| `emqx_channels_max` | 最大チャネル数。 |

### サブスクリプションとトピック

| メトリクス | 説明 |
|------------|-------|
| `emqx_subscriptions_count` | サブスクリプション数。 |
| `emqx_subscriptions_max` | 最大サブスクリプション数。 |
| `emqx_subscriptions_shared_count` | 共有サブスクリプション数。 |
| `emqx_subscriptions_shared_max` | 最大共有サブスクリプション数。 |
| `emqx_subscribers_count` | サブスクライバープロセス数。 |
| `emqx_topics_count` | ユニークなトピック数。 |
| `emqx_topics_max` | 最大トピック数。 |
| `emqx_routes_count` | ルートテーブルのサイズ。 |
| `emqx_routes_max` | 最大ルートテーブルサイズ。 |
| `emqx_durable_subscriptions_count` | 永続セッションのサブスクリプション数。 |
| `emqx_durable_subscriptions_max` | 最大永続セッションサブスクリプション数。 |

### 保持済み、遅延、禁止

| メトリクス | 説明 |
|------------|-------|
| `emqx_retained_count` | 保持されたメッセージ数。 |
| `emqx_retained_max` | 最大保持数。 |
| `emqx_delayed_count` | 遅延パブリッシュキューの深さ。 |
| `emqx_delayed_max` | 最大遅延キュー深さ。 |
| `emqx_banned_count` | 禁止されたクライアント／ユーザー名／IPのエントリ数。 |

### メッセージ

| メトリクス | 説明 |
|------------|-------|
| `emqx_messages_received` | クライアントから受信したアプリケーションレベルのメッセージ数。 |
| `emqx_messages_sent` | クライアントに送信したアプリケーションレベルのメッセージ数。 |
| `emqx_messages_publish` | 発行されたPUBLISHパケット数。 |
| `emqx_messages_delivered` | サブスクライバーへの配信数（1つのパブリッシュメッセージが複数の配信を生むことがあります）。 |
| `emqx_messages_acked` | サブスクライバーから受信したアック（ACK）数。 |
| `emqx_messages_forward` | ノード間のメッセージ転送数。 |
| `emqx_messages_retained` | 保持メッセージのイベント数。 |
| `emqx_messages_delayed` | 遅延パブリッシュのキューイング数。 |

### メッセージドロップ（問題の最初の兆候）

| メトリクス | 説明 |
|------------|-------|
| `emqx_messages_dropped` | ドロップされたメッセージの合計数。 |
| `emqx_messages_dropped_expired` | メッセージの有効期限切れによるドロップ数。 |
| `emqx_messages_dropped_no_subscribers` | サブスクライバーが存在しなかったためのドロップ数。 |
| `emqx_messages_dropped_quota_exceeded` | クライアントごとのクォータ超過によるドロップ数。 |
| `emqx_messages_dropped_receive_maximum` | サブスクライバーのMQTT v5のreceive-maximumクォータ超過によるドロップ数。 |

### サブスクライバーごとの配信ドロップ

| メトリクス | 説明 |
|------------|-------|
| `emqx_delivery_dropped` | ドロップされた配信の合計数。 |
| `emqx_delivery_dropped_expired` | 配信前に期限切れとなった数。 |
| `emqx_delivery_dropped_no_local` | MQTT v5のno-localルールによるドロップ数。 |
| `emqx_delivery_dropped_qos` | サポートされていないQoSによるドロップ数。 |
| `emqx_delivery_dropped_queue_full` | サブスクライバーのメッセージキューが満杯によるドロップ数。 |
| `emqx_delivery_dropped_too_large` | サブスクライバーの最大パケットサイズを超過したためのドロップ数。 |

### バイト数

| メトリクス | 説明 |
|------------|-------|
| `emqx_bytes_received` | 受信した合計バイト数。 |
| `emqx_bytes_sent` | 送信した合計バイト数。 |

### パケットレベル（プロトコルデバッグ用ダッシュボード）

| メトリクス | 説明 |
|------------|-------|
| `emqx_packets_received` | 受信したパケット数。 |
| `emqx_packets_sent` | 送信したパケット数。 |
| `emqx_packets_connect` | 受信したCONNECTパケット数。 |
| `emqx_packets_connack_sent` | 送信したCONNACKパケット数。 |
| `emqx_packets_connack_error` | 0以外の理由コードを含むCONNACK（多くのクライアント認証失敗がここに表示されます）。 |
| `emqx_packets_disconnect_received` | 受信したDISCONNECTパケット数。 |
| `emqx_packets_disconnect_sent` | 送信したDISCONNECTパケット数。 |
| `emqx_packets_publish_received` | 受信したPUBLISHパケット数。 |
| `emqx_packets_publish_sent` | 送信したPUBLISHパケット数。 |
| `emqx_packets_publish_error` | 受け入れられなかったPUBLISHパケット数。 |
| `emqx_packets_publish_auth_error` | 認可により拒否されたPUBLISHパケット数。 |
| `emqx_packets_puback_received` | 受信したPUBACKパケット数（QoS 1）。 |
| `emqx_packets_puback_sent` | 送信したPUBACKパケット数（QoS 1）。 |
| `emqx_packets_pubrec_received` | 受信したPUBRECパケット数（QoS 2）。 |
| `emqx_packets_pubrec_sent` | 送信したPUBRECパケット数（QoS 2）。 |
| `emqx_packets_pubrel_received` | 受信したPUBRELパケット数（QoS 2）。 |
| `emqx_packets_pubrel_sent` | 送信したPUBRELパケット数（QoS 2）。 |
| `emqx_packets_pubcomp_received` | 受信したPUBCOMPパケット数（QoS 2）。 |
| `emqx_packets_pubcomp_sent` | 送信したPUBCOMPパケット数（QoS 2）。 |
| `emqx_packets_subscribe_received` | 受信したSUBSCRIBEパケット数。 |
| `emqx_packets_suback_sent` | 送信したSUBACKパケット数。 |
| `emqx_packets_subscribe_error` | 失敗したSUBSCRIBEパケット数。 |
| `emqx_packets_subscribe_auth_error` | 認可により拒否されたSUBSCRIBEパケット数。 |
| `emqx_packets_unsubscribe_received` | 受信したUNSUBSCRIBEパケット数。 |
| `emqx_packets_unsuback_sent` | 送信したUNSUBACKパケット数。 |
| `emqx_packets_unsubscribe_error` | 失敗したUNSUBSCRIBEパケット数。 |
| `emqx_packets_pingreq_received` | 受信したPINGREQパケット数。 |
| `emqx_packets_pingresp_sent` | 送信したPINGRESPパケット数。 |

### クライアントライフサイクル（フックトリガーカウンター）

| メトリクス | 説明 |
|------------|-------|
| `emqx_client_connect` | 受信したCONNECT。 |
| `emqx_client_connack` | 送信したCONNACK。 |
| `emqx_client_connected` | `client.connected`フックが発火。 |
| `emqx_client_disconnected` | `client.disconnected`フックが発火。 |
| `emqx_client_disconnected_reason` | 理由別の切断カウント。 |
| `emqx_client_subscribe` | Subscribeフックが発火。 |
| `emqx_client_unsubscribe` | Unsubscribeフックが発火。 |

### セッションライフサイクル

| メトリクス | 説明 |
|------------|-------|
| `emqx_session_created` | 作成されたセッション数。 |
| `emqx_session_resumed` | 再開された永続セッション数。 |
| `emqx_session_takenover` | 新しいクライアントによって引き継がれたセッション数。 |
| `emqx_session_discarded` | 廃棄されたセッション数（既存セッションのクリーンスタート）。 |
| `emqx_session_terminated` | 終了したセッション数。 |

## 認証と認可

HTTP、LDAP、またはデータベースバックエンドが認証経路にある場合に、ブローカーまたはバックエンドのどちらが遅延または障害の原因かを判断するためにこれらのメトリクスを使用します。

### 接続時認証結果

| メトリクス | 説明 |
|------------|-------|
| `emqx_authentication_success` | 成功した認証（匿名を除く）。 |
| `emqx_authentication_success_anonymous` | 匿名認証の成功。 |
| `emqx_authentication_failure` | 認証失敗。 |

### 認可判定

| メトリクス | 説明 |
|------------|-------|
| `emqx_authorization_allow` | 許可された判定数。 |
| `emqx_authorization_deny` | 拒否された判定数。 |
| `emqx_authorization_nomatch` | マッチするルールなし（`no_match`設定にフォールバック）。 |
| `emqx_authorization_matched_allow` | 許可ルールがマッチして発火。 |
| `emqx_authorization_matched_deny` | 拒否ルールがマッチして発火。 |
| `emqx_authorization_cache_hit` | キャッシュヒット数。 |
| `emqx_authorization_cache_miss` | キャッシュミス数。 |
| `emqx_authorization_superuser` | スーパーユーザーバイパス経路。 |

### 認証チェーンの状態

| メトリクス | 説明 |
|------------|-------|
| `emqx_authn_total` | 設定された認証プロバイダー数。 |
| `emqx_authn_enable` | プロバイダーごとの有効フラグ（0 / 1）。 |
| `emqx_authn_status` | プロバイダーごとのリソース状態。 |
| `emqx_authn_users_count` | プロバイダーごとのユーザーレコード数（パスワード、mnesia、DBバックエンドの場合）。 |

### プロバイダーごとの認証ランタイムカウンター

| メトリクス | 説明 |
|------------|-------|
| `emqx_authn_success` | プロバイダーごとの成功数。 |
| `emqx_authn_failed` | プロバイダーごとの失敗数。 |
| `emqx_authn_nomatch` | プロバイダーごとの無視数（チェーンは次のプロバイダーへ継続）。 |
| `emqx_authn_latency` | プロバイダーごとのバックエンドレイテンシ。 |

### 認可ソースの状態

| メトリクス | 説明 |
|------------|-------|
| `emqx_authz_total` | 設定された認可ソース数。 |
| `emqx_authz_enable` | ソースごとの有効フラグ（0 / 1）。 |
| `emqx_authz_status` | ソースごとのリソース状態。 |
| `emqx_authz_rules_count` | ソースごとのルールレコード数（ファイル、mnesia、DBバックエンド）。 |

### ソースごとの認可ランタイムカウンター

| メトリクス | 説明 |
|------------|-------|
| `emqx_authz_allow` | ソースごとの許可判定数。 |
| `emqx_authz_deny` | ソースごとの拒否判定数。 |
| `emqx_authz_nomatch` | ソースごとの無視数（チェーンは継続）。 |
| `emqx_authz_latency` | ソースごとのバックエンドレイテンシ。 |

### 組み込みDBサイズ

| メトリクス | 説明 |
|------------|-------|
| `emqx_authn_builtin_record_count` | 組み込み認証データベースのユーザー数。 |
| `emqx_authz_builtin_record_count` | 組み込み認可データベースのルール数。 |

## データ統合

トラフィックはルールエンジンに入り、アクションやコネクターに分岐し、外部システムに到達します。各レイヤーは独自のカウンターを公開しています。順に読み取ることでメッセージがどこで失われているかがわかります。

### インベントリ

| メトリクス | 説明 |
|------------|-------|
| `emqx_rules_count` | 設定されたルール数。 |
| `emqx_actions_count` | 設定されたアクション数。 |
| `emqx_connectors_count` | 設定されたコネクター数。 |
| `emqx_schema_registrys_count` | スキーマレジストリのエントリ数。 |

### リソースごとの状態

| メトリクス | 説明 |
|------------|-------|
| `emqx_rule_enable` | ルールの有効フラグ（0 / 1）。 |
| `emqx_action_enable` | アクションの有効フラグ（0 / 1）。 |
| `emqx_action_status` | アクションリソースの状態。 |
| `emqx_connector_enable` | コネクターの有効フラグ（0 / 1）。 |
| `emqx_connector_status` | コネクターリソースの状態。 |

### ルールエンジン：ルールごとのカウンター

| メトリクス | 説明 |
|------------|-------|
| `emqx_rule_matched` | ルールのWHERE句にマッチしたメッセージ数。 |
| `emqx_rule_passed` | ルールを通過したメッセージ数。 |
| `emqx_rule_failed` | ルール処理の失敗数。 |
| `emqx_rule_failed_exception` | ルール処理中のErlang例外。 |
| `emqx_rule_failed_no_result` | SQLが結果を返さなかった回数。 |

### ルールエンジン：アクションのサブカウンター

| メトリクス | 説明 |
|------------|-------|
| `emqx_rule_actions_total` | ルールからのアクション呼び出し回数。 |
| `emqx_rule_actions_success` | アクションが成功した回数。 |
| `emqx_rule_actions_failed` | アクションが失敗した回数。 |
| `emqx_rule_actions_failed_unknown` | 原因不明の失敗回数。 |
| `emqx_rule_actions_failed_out_of_service` | 下流リソースが異常なための失敗。 |
| `emqx_rule_actions_discarded` | アクションが破棄された回数（例：レート制限）。 |

### アクションのスループット

| メトリクス | 説明 |
|------------|-------|
| `emqx_action_matched` | アクションにルーティングされたメッセージ数。 |
| `emqx_action_received` | アクションキューで受信した数。 |
| `emqx_action_success` | アクション呼び出しが成功した回数。 |
| `emqx_action_failed` | アクション呼び出しが失敗した回数。 |
| `emqx_action_late_reply` | タイムアウト後に応答が届いた回数。 |
| `emqx_action_retried` | リトライ試行回数。 |
| `emqx_action_retried_success` | リトライ後に成功した回数。 |
| `emqx_action_retried_failed` | すべてのリトライ後に失敗した回数。 |

### アクションのキューとインフライト

| メトリクス | 説明 |
|------------|-------|
| `emqx_action_inflight` | インフライトリクエスト数。 |
| `emqx_action_queuing` | キューイング中（ディスパッチ待ち）の長さ。 |

### アクションのドロップ

これらの系列のいずれかで非ゼロのレートがある場合、下流システムが異常かアクションの設定ミスを示します。

| メトリクス | 説明 |
|------------|-------|
| `emqx_action_dropped` | アクションレイヤーでドロップされた合計数。 |
| `emqx_action_dropped_queue_full` | キューの上限に達した回数。 |
| `emqx_action_dropped_resource_stopped` | 対象リソースが停止しているためのドロップ。 |
| `emqx_action_dropped_resource_not_found` | 対象リソースが見つからないためのドロップ。 |
| `emqx_action_dropped_expired` | ディスパッチ前にメッセージが期限切れとなった回数。 |
| `emqx_action_dropped_other` | その他の理由によるドロップ。 |

## 最小限の「ブローカー異常」パネル

Grafanaダッシュボードに限られた数の系列しか表示できない場合、以下が最も重要な指標です。ほとんどの本番問題は、イベント発生から数秒以内にこれらのいずれかが変動します。

- `rate(emqx_messages_dropped[1m])`：非ゼロはブローカーが処理を拒否または失っていることを示します。
- `rate(emqx_action_dropped[1m])`：統合レイヤーで処理が失われています。
- `emqx_cluster_nodes_stopped`：0より大きい場合はメンバーが失われています。
- `rate(emqx_overload_protection_new_conn[1m])`：ブローカーが新規接続を積極的に拒否しています。
- `rate(emqx_authentication_failure[1m])`：急増は通常バックエンド問題または攻撃を示します。
- `emqx_vm_run_queue`：継続的にゼロ以上はCPU飽和を示します。
- `emqx_mria_lag`：数秒以上の値は複製遅延を意味します。
- `emqx_license_expiry_at - time()`（エンタープライズ）：ライセンス期限までのカウントダウン。

## EMQX 6.3におけるPrometheusメトリクスの互換性

EMQX 6.3では、PrometheusのVMおよびMnesiaコレクターのメトリクス名がpromtool準拠の名前に変更されました。以前の名前を使用しているPromQL式、レコーディングおよびアラートルール、カスタムGrafanaダッシュボードは更新してください。

| EMQX 6.3以前 | EMQX 6.3 |
| --- | --- |
| `erlang_mnesia_failed_transactions` | `erlang_mnesia_failed_transactions_total` |
| `erlang_mnesia_committed_transactions` | `erlang_mnesia_committed_transactions_total` |
| `erlang_mnesia_logged_transactions` | `erlang_mnesia_logged_transactions_total` |
| `erlang_mnesia_restarted_transactions` | `erlang_mnesia_restarted_transactions_total` |
| `erlang_vm_memory_atom_bytes_total` | `erlang_vm_memory_atom_bytes` |
| `erlang_vm_memory_bytes_total` | `erlang_vm_memory_bytes` |
| `erlang_vm_memory_processes_bytes_total` | `erlang_vm_memory_processes_bytes` |
| `erlang_vm_memory_system_bytes_total` | `erlang_vm_memory_system_bytes` |
| `erlang_vm_statistics_context_switches` | `erlang_vm_statistics_context_switches_total` |
| `erlang_vm_statistics_garbage_collection_number_of_gcs` | `erlang_vm_statistics_garbage_collection_number_of_gcs_total` |
| `erlang_vm_statistics_garbage_collection_words_reclaimed` | `erlang_vm_statistics_garbage_collection_words_reclaimed_total` |
| `erlang_vm_statistics_garbage_collection_bytes_reclaimed` | `erlang_vm_statistics_garbage_collection_bytes_reclaimed_total` |
| `erlang_vm_statistics_runtime_milliseconds` | `erlang_vm_statistics_runtime_seconds_total` |
| `erlang_vm_statistics_wallclock_time_milliseconds` | `erlang_vm_statistics_wallclock_time_seconds_total` |
| `erlang_vm_port_count` | `erlang_vm_ports` |
| `erlang_vm_process_count` | `erlang_vm_processes` |
| `erlang_vm_atom_count` | `erlang_vm_atoms` |

`emqx_vm_process_messages_in_queues`メトリクスはEMQX 6.3で廃止されました。このメトリクスを使用しているクエリは削除してください。EMQX 6.3では直接の代替メトリクスは提供されていません。
