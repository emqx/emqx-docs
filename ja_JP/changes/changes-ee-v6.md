# EMQX Enterprise v6.0

## 6.0.0

*リリース日: 2025-09-29*

EMQX 6.0.0 にアップグレードする前に、必ず互換性のない変更と既知の問題を確認してください。

### 機能ハイライト

- **メジャーバージョンリリース**：EMQX Enterprise 6.0.0 は EMQX Enterprise v6 シリーズの最初のリリースであり、大幅なアーキテクチャの改善と新機能をもたらします。
- **AWS 統合の強化**：S3 または S3Tables データ統合を使用する場合、EC2 インスタンスからのインスタンスメタデータサービス v2 API をサポートします。これにより、手動で AWS 認証情報を設定することなく、IAM ロールを活用して S3 バケットにシームレスにアクセスでき、セキュリティが向上します。
- **Elixir サポート**：すべてのパッケージに Mix ビルドシステムを介した Elixir サポートが付属し、EMQX を Elixir コミュニティに開放し、IEx コンソールでより良いツールを可能にします。
- **新しいデータ統合**：
  - Google BigQuery にデータを追加するための BigQuery コネクタとアクション
  - Snowflake アクションの Snowpipe Streaming アップロードモード（プレビュー機能）
  - S3Tables アクションの Parquet 形式のサポート
- **永続ストレージの最適化**：新しい RocksDB 設定オプションとデフォルトの ASN1 シリアル化スキーマにより、RAM 使用量とストレージ効率が大幅に向上しました。
- **LDAP サポートの強化**：LDAP 認可は JSON 形式の拡張 ACL ルールをサポートし、LDAP 認証はクライアント側のキャッシュを使用して LDAP から直接 ACL ルールを取得できます。
- **トレースの改善**：最大トレース数（`trace.max_traces`）とトレースファイルサイズ（`trace.max_file_size`）の設定可能な制限、および原子リークを防ぐための最適化された実装。
- **クラスター管理**：新しい `cluster.description` 設定オプションにより、ユーザーは EMQX ダッシュボードでカスタムクラスターの説明を設定および表示できます。

### 拡張

#### メッセージキュー

- [#15789](https://github.com/emqx/emqx/pull/15789) `topic/filter` によって識別されるメッセージのコレクションであるメッセージキューを実装しました。各キューには明示的なライフサイクルがあり、キューの存続期間中にキューのトピックフィルターに一致する公開済みメッセージが自動的に補充されます。クライアントは、`$q/topic/filter` の形式で特別なトピックをサブスクライブすることにより、キューからメッセージを協調して消費できます。

#### コア MQTT 機能

- [#15805](https://github.com/emqx/emqx/pull/15805) シャーディングされたファンアウトメッセージ配信を処理するための専用ワーカープールを導入しました。以前は、ブローカープールがサブスクリプション管理とメッセージディスパッチの両方を処理していたため、スケジューリングの競合が発生する可能性がありました。この変更により、ファンアウトディスパッチのワークロードが独自のプールに分離され、pub/sub 操作のよりバランスの取れた効率的な処理が保証されます。

#### アクセス制御

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証と認可のための外部リソース管理を最適化しました。以前は、EMQX は無効な認証器または認可器に設定されたリソースに接続したままでした。
- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP 認証と認可を強化しました。LDAP 認可は JSON 形式の拡張 ACL ルールをサポートするようになりました。LDAP 認証は LDAP から ACL ルールを取得できるようになりました。これらのルールはクライアントのメタデータにキャッシュされるため、追加の LDAP クエリなしで認可が実行されます。
- [#15730](https://github.com/emqx/emqx/pull/15730) 認証結果に基づいてクライアント ID を上書きするサポートを追加しました。認証バックエンドが正常な認証時に `clientid_override` 属性を返した場合、クライアントの元のクライアント ID が置き換えられます。
  次のバックエンドが `clientid_override` をサポートするようになりました：
  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis
- [#15820](https://github.com/emqx/emqx/pull/15820) より良いセキュリティデフォルトのために、設定 `authorization.no_match` のデフォルト値を `allow` から `deny` に変更しました。

#### クラスタリング

- [#15600](https://github.com/emqx/emqx/pull/15600) EMQX クラスターに説明的なラベルを追加できる新しい設定オプション `cluster.description` を導入しました。この説明は `PUT /cluster` を介して更新でき、`GET /cluster` API で取得できます。

#### LLM ベースの MQTT データ処理

- [#15467](https://github.com/emqx/emqx/pull/15467) AI 補完プロバイダーのトランスポート設定オプションを公開しました。ユーザーは接続タイムアウトと AI 補完プロバイダーへの最大接続数を設定できるようになりました。これにより、メッセージスループットが高く、プロバイダーが負荷状態にある場合に `checkout_timeout` エラーを防ぐことができます。
- Flow designer は [Google Gemini モデル](https://docs.mqttce.com/ja/emqx/v6.0/flow-designer/gemini-node-quick-start.html)との統合をサポートしています。
- [#15631](https://github.com/emqx/emqx/pull/15631) AI プロバイダーで利用可能なすべてのモデルを一覧表示する新しい API エンドポイントを追加しました。
- [#15467](https://github.com/emqx/emqx/pull/15467) AI 補完プロバイダーのトランスポートオプションを公開しました。これらのオプションにより、AI 補完プロバイダーへの接続タイムアウトと最大接続数を設定できます。
- [#15724](https://github.com/emqx/emqx/pull/15724) AI 補完プロバイダーと補完プロファイルに `openai_response` タイプを導入し、OpenAI の `response` API を使用するようにしました。

#### データ統合

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX は [BigQuery](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/bigquery.html) とのデータ統合をサポートしています。
- [#15401](https://github.com/emqx/emqx/pull/15401) [Snowflake アクション](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/snowflake.html#_create-a-rule-with-snowflake-sink)に Snowpipe Streaming アップロードモードのサポートを追加しました。
  *注意：Snowpipe Streaming は現在*[*プレビュー機能*](https://docs.snowflake.com/en/release-notes/preview-features)*であり、AWS でホストされている Snowflake アカウントでのみ利用可能です。*
- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producer Connector と Action のヘルスチェックにレート制限を追加し、AWS API のクォータに準拠し、クラスターの動作を改善しました。
  - `ListStreams` と `DescribeStream` へのヘルスチェックコールは、それぞれコネクタあたり 5/s と 10/s に制限され、AWS のレート制限に一致します。
  - クラスター内のコアノードによって分散リミッターが調整され、これらの制限が一貫して適用されます。
  - ヘルスチェックがスロットルされたりタイムアウトした場合、コネクタまたはアクションは切断されたとマークされるのではなく、以前のステータスを保持します。
  また、新しい `resource_opts.health_check_interval_jitter` を導入しました。これは `resource_opts.health_check_interval` に均一なランダム遅延を追加し、同じコネクタ下の複数のアクションが同時にヘルスチェックを実行する可能性を減らします。
- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary` の応答と `GET /actions/:id` によって返される `fallback_actions` セクションに `tags` フィールドを追加しました。これにより一貫性が向上し、クライアントはアクションとフォールバックアクションのタグメタデータを取得できます。
- [#15360](https://github.com/emqx/emqx/pull/15360) S3 Tables Action で Parquet 形式のデータファイルを書き込むサポートを追加しました。
- [#15176](https://github.com/emqx/emqx/pull/15176) GreptimeDB Connector クライアントをアップグレードし、自動作成されるテーブルのデフォルトの存続時間を設定するためのオプションの新しいパラメータ `ttl` をサポートしました。
- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX は [AWS AlloyDB](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/alloydb.html)、[CockroachDB](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/cockroachdb.html)、[AWS Redshift](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/redshift.html) とのデータ統合をサポートしています。
- [#15635](https://github.com/emqx/emqx/pull/15635) [RocketMQ アクション](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/data-bridge-rocketmq.html#_create-a-rule-with-rocketmq-sink-for-message-storage)に新しい `key` と `tag` テンプレートフィールドを追加し、メッセージのキーとタグのカスタマイズを可能にしました。また、`Produce Strategy` フィールドに新しい `key_dispatch` オプションを導入しました。
- [#15621](https://github.com/emqx/emqx/pull/15621) `access_key_id` と `secret_access_key` は S3Tables Connector のオプションフィールドになりました。省略された場合、EMQX がデプロイされている EC2 インスタンスのインスタンスメタデータサービス v2 API から取得されます。
- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを `3.8.3.0` にアップグレードしました。これにより、EMQX が実行されている EC2 インスタンスが設定されたバケットへの読み書きに正しい IAM 権限を持っている限り、アクセスキー ID とシークレットアクセスキーを指定せずに [S3 コネクタ](https://docs.mqttce.com/ja/emqx/v6.0/data-integration/s3.html#_create-a-connector)を設定できます。
- [#15583](https://github.com/emqx/emqx/pull/15583) [#15585](https://github.com/emqx/emqx/pull/15585) `brod` クライアントを 4.4.4 バージョンに更新し、より広範な Kafka API のサポートを拡張しました。この更新は `JoinGroups` API バージョン `v0` - `v1` の非推奨に対応しています。
- [#15628](https://github.com/emqx/emqx/pull/15628) HStreamDB データ統合を削除しました。
- [#15544](https://github.com/emqx/emqx/pull/15544) Datalayers Integration に Arrow Flight SQL NIF ドライバのサポートを追加しました。
- [#15637](https://github.com/emqx/emqx/pull/15637) RabbitMQ Action のメッセージヘッダーとプロパティのテンプレート化をサポートしました。
- [#15864](https://github.com/emqx/emqx/pull/15864) 非推奨の「Bridges V1」API と設定スキーマを削除しました。`/bridges/*` 以下のすべてのエンドポイントと `bridges` ルートキー以下の設定エントリは利用できなくなりました。データ統合は完全に「コネクタ/アクション/ソース」モデルに移行しました。

#### スマートデータハブ

- [#15525](https://github.com/emqx/emqx/pull/15525) まだ使用中の内部スキーマの削除を防止しました。スキーマがスキーマ検証またはメッセージ変換によって参照されている場合、実行時エラーや設定の不整合を避けるために削除できなくなりました。

#### 永続ストレージ

- [#15463](https://github.com/emqx/emqx/pull/15463) 永続ストレージの RAM 使用量とストレージ効率を改善しました。
  - RocksDB のメモリ使用量とストレージパフォーマンスの制御を改善するために、永続ストレージに以下の設定パラメータを導入しました：
    - `durable_storage.messages.rocksdb.write_buffer_size`：シャードごとの RocksDB memtable サイズ。
    - `durable_storage.messages.rocksdb.cache_size`：シャードごとの RocksDB ブロックサイズ。
    - `durable_storage.messages.rocksdb.max_open_files`：シャードごとに RocksDB が使用するファイル記述子の数を制限します。
    - `durable_storage.messages.layout.wildcard_thresholds`：`wildcard_optimized_v2` ストレージレイアウトのワイルドカードしきい値を調整できます。
  - さらに、保存されたメッセージのデフォルトの `serialization_schema` が `asn1` に変更されました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールは、現在のシステム設定を HOCON 形式でエクスポートするようになり、セキュリティのために機密情報（パスワードやシークレットなど）は自動的に編集されます。

#### 名前空間

- [#15841](https://github.com/emqx/emqx/pull/15841) 名前空間セッションのセッション数の更新レートを改善しました。
  - 名前空間の接続数が 1000 未満の場合、セッション数はオンデマンドで更新されます。
  - 1000 以上の接続を持つ名前空間の場合、カウントは 5 秒ごとに更新されます。
  6.0 より前のバージョンからのローリングアップグレード中、内部追跡テーブルの変更によりセッション数が一致しない場合があります。これは想定内の動作です。クライアントがアップグレードされたノードに再接続すると、セッション数は徐々に安定し、すべてのノードが 6.0 以降を実行すると正確になります。

#### 可観測性

- [#15594](https://github.com/emqx/emqx/pull/15594) アクティブなクラスター全体のトレースの最大数を制御するための新しい設定オプション `trace.max_traces` を導入しました。この制限は `emqx ctl trace` を使用して管理されるノードローカルトレースには適用されません。
  この更新では、作成されたトレースごとの潜在的なアトムリークを排除するためにトレース実装も最適化されました。
- [#15556](https://github.com/emqx/emqx/pull/15556) 個々のトレースの最大ファイルサイズを制限するための新しい設定オプション `trace.max_file_size` を導入しました。
- [#15650](https://github.com/emqx/emqx/pull/15650) 自動トレースログローテーションを実装しました。
  トレースファイルサイズが `trace.max_file_size` を超えると、EMQX は後続のすべてのイベントを破棄し、`stderr` に理解不能な警告を発するのではなく、最も古いイベントの一部を破棄し、最新のイベントを保持します。
  したがって、これも意味します：
  * EMQX はアクティブなトレースごとに複数のトレースログファイルを維持します。トレースディレクトリのレイアウトもそれに応じて変更されました。
  * トレース API はこの動作を反映するように更新されました。ログストリーム API は、遅いコンシューマのためにストリームが古くなった場合など、新しいエラーを返す可能性があります。
- [#15904](https://github.com/emqx/emqx/pull/15904) Trace API を介したトレース設定の表示と更新をサポートします。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval` 設定をデフォルトで無効にし、全体的なパフォーマンスの安定性を向上させました。これは、CPU の変動とメッセージ遅延の増加を引き起こし、Erlang の組み込みガベージコレクタに比べてほとんど利点がなかったためです。
- [#15539](https://github.com/emqx/emqx/pull/15539) パフォーマンスと安定性を向上させるために Erlang VM パラメータを最適化しました：
  - 分散チャネルのバッファサイズを 32 MB（`+zdbbl 32768`）に増やし、集中的な Mnesia 操作中の `busy_dist_port` アラームを防ぎます。
  - スケジューラのビジーウェイトを無効にし（`+sbwt none +sbwtdcpu none +sbwtdio none`）、オペレーティングシステムによって報告される CPU 使用率を下げます。
  - スケジューラのバインディングタイプを `db`（`+stbt db`）に設定し、メッセージの遅延を減らします。
- [#15451](https://github.com/emqx/emqx/pull/15451) TCP リスナー用の実験的な `socket` バックエンドを導入し、メッセージ処理の遅延を改善し、計算リソースの使用量を削減することを目指しています。この機能は、新しい `tcp_backend` リスナーオプションで有効にできます。

#### ビルドとツール

- [#15484](https://github.com/emqx/emqx/pull/15484) ビルドシステムを [Elixir](https://elixir-lang.org/) の [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html) に切り替え、すべてのパッケージにネイティブ Elixir サポートを含めるようにしました。この変更により、開発者ツールが改善され、必要に応じて Elixir の依存関係と統合でき、[IEx](https://hexdocs.pm/iex/IEx.html) シェルをより強力な EMQX コンソールとして使用できるようになります。

#### ライセンス

- [#15921](https://github.com/emqx/emqx/pull/15921) クラスター全体の最大秒間トランザクション数（TPS）に対するライセンスアラームを導入しました。
  - 各ノードは、過去 10 秒間に送受信された MQTT メッセージの平均数として TPS を計算します。
  - 総クラスター TPS は 5 秒ごとに集計されます。
  - 観測された TPS がライセンス制限を超えると、アラームがトリガーされます。
  - アラームは、より高い TPS 許容量を持つライセンスが適用されるまでアクティブなままです。

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) 環境変数 `QUICER_SKIP_NIF_LOAD=1` を設定することで QUIC スタックの読み込みを無効にするサポートを追加しました。

### 修正

#### コア MQTT 機能

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断されたクライアントの共有サブスクリプションに対する冗長なクリーンアップ操作を削除しました。これらの操作は、高い切断ボリューム下でクラッシュしやすく、グローバルブローカーの状態に不整合を引き起こす可能性がありました。
- [#15361](https://github.com/emqx/emqx/pull/15361) 不正な（短すぎる）長さの `User-Property` ペアを解析する際の `function_clause` エラーを修正しました。
- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket 接続のセッション有効期限切れ時に時折発生する警告レベルのログイベントとクラッシュを修正しました。この問題は、最近の WebSocket パフォーマンス改善によって導入されたものです。ブローカーの容量には影響しませんでしたが、次のようなログエントリが生成されていました：
  - `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  - `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

#### 展開

- [#15580](https://github.com/emqx/emqx/pull/15580) EMQX Enterprise Helm チャートに新しい `emqxLicenseSecretRef` 変数を追加しました。これにより、ユーザーは EMQX ライセンスキーを含む Kubernetes Secret を指定でき、ライセンスが自動的に適用されます。
  これは、機能していなかった `emqxLicenseSecretName` 変数を置き換えるものです。この変数は、シークレットファイルを作成してマウントしましたが、ライセンスを EMQX に渡しませんでした。
- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートで、デフォルト値で EMQX をデプロイすると複数のレプリカが起動し、1 つのノードを除くすべてのノードがクラッシュする問題を修正しました。クラスター化されたデプロイには商用ライセンスが必要なため、チャートはデフォルトで単一のレプリカになりました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTP バージョンを 26.2.5.2 から 26.2.5.14 にアップグレードしました。このアップグレードには、EMQX に影響を与える OTP からの 2 つの TLS 関連の修正が含まれています：
  - 証明書の更新中の競合状態によって引き起こされる TLS 接続のクラッシュを修正しました。
  - RSASSA-PSS パラメータで署名された RSA 証明書のサポートを追加しました。以前は、このような証明書は TLS ハンドシェイクが `bad_certificate` / `invalid_signature` エラーで失敗する原因となる可能性がありました。

#### アクセス制御

- [#15489](https://github.com/emqx/emqx/pull/15489) シングルサインオン（SSO）設定の OIDC 発行者 URL 検証を修正しました。以前は、ポート番号を含む発行者 URL（例：`https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`）は `bad_port_number` エラーで拒否されていました。これらの URL は現在サポートされています。

#### クラスタリング

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された場合に、クラスター全体のルーティングテーブルと共有サブスクリプションの状態に不整合が蓄積する可能性のある競合状態を解決しました。

#### ルールエンジン

- [#15569](https://github.com/emqx/emqx/pull/15569) `direct_dispatch` テンプレートが空または非ブール値に解決された場合に Republish Rule Action が失敗する問題を修正しました。これらの場合、デフォルト値 `false` が使用されるようになりました。

#### データ統合

- [#15522](https://github.com/emqx/emqx/pull/15522) `username` が提供されていない場合に Snowflake Connector が正しく起動しない問題を修正しました。
- [#15476](https://github.com/emqx/emqx/pull/15476) `emqx_connector_aggreg_delivery` のコールバックが欠落しており、集約モードのアクション（Azure Blob Storage、Snowflake、S3 Tables など）の配信プロセスステータスをフォーマットする際にクラッシュする問題を修正しました。
  これは、失敗時または `gen_server:format_status/1` で配信プロセスを検査する際に発生しました。問題は解決され、より詳細な配信ステータス情報がログに記録されるようになりました。
- [#15394](https://github.com/emqx/emqx/pull/15394) 予期しない非同期応答によりアクションメトリクスが不整合になるまれな競合状態を修正しました。
- [#15647](https://github.com/emqx/emqx/pull/15647) コネクタ設定で指定された MongoDB アカウントが `foo` コレクションに対する `find` クエリの権限を持っていない場合に、MongoDB コネクタが `Disconnected` とマークされる問題を修正しました。
- [#15603](https://github.com/emqx/emqx/pull/15603) MQTT ブリッジで、古い接続が `Connected` と表示され、自動的に再接続されない問題を修正しました。
- [#15383](https://github.com/emqx/emqx/pull/15383) MQTT ブリッジの潜在的なリソースリークを修正しました。ブリッジの起動に失敗した場合、トピックインデックステーブルが適切にクリーンアップされませんでした。
- [#15786](https://github.com/emqx/emqx/pull/15786) RocketMQ コネクタをプローブする際の潜在的なアトムリークを修正しました。
- [#15806](https://github.com/emqx/emqx/pull/15806) Oracle アクション作成時の検証を改善しました。以前は、まれに無効な SQL ステートメントを含むアクションが正常に追加されることがありました。
- [#15848](https://github.com/emqx/emqx/pull/15848) Oracle コネクタのエラー報告を改善しました。コネクタが切断された場合、そのステータスにはより具体的な理由が含まれるようになり、診断が容易になりました。

#### スマートデータハブ

- [#15839](https://github.com/emqx/emqx/pull/15839) `map<_, _>` フィールドを使用する Protobuf スキーマのエンコーディング問題を修正しました。
  以前は、`map<string, string>` フィールドを含むスキーマが有効なペイロードをエンコードできず、不可解なランタイムエラーが発生していました。
  
  スキーマの例：
  ```protobuf
  syntax = "proto3";
  
  message test {
  map<string, string> args = 1;
  }
  ```
  
  ルールの例：
  ```sql
  SELECT
  schema_encode('xxx', json_decode(payload), 'test') as protobuf_test
  FROM
  "t/#"
  ```
  
  エンコードに失敗したペイロードの例：
  ```json
  {
  "args": {
  "env": "stag"
  }
  }
  ```
  
  以前のエラーは次のようになります：
  ```
  2025-06-17T06:59:22.725785+00:00 [warning] tag: RULE_SQL_EXEC, clientid: c_emqx, msg: SELECT_clause_exception, reason: {error,{gpb_type_error,{bad_unicode_string,[{value,env},{path,"test.args.key"}]}},[{'$schema_parser_xxx',mk_type_error,3,[{file,"$schema_parser_xxx.erl"},{line,437}]},{'$schema_parser_xxx','-v_map<string,string>/3-lc$^0/1-0-',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx','v_map<string,string>',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx',v_msg_test,3,[{file,"$schema_parser_xxx.erl"},{line,404}]},{'$schema_parser_xxx',encode_msg,3,[{file,"$schema_parser_xxx.erl"},{line,73}]},{emqx_schema_registry_serde,with_serde,2,[{file,"emqx_schema_registry_serde.erl"},{line,212}]}...
  ```

#### 可観測性

- [#15931](https://github.com/emqx/emqx/pull/15931) EMQX アラームシステムに関連する 2 つの問題を修正しました：
  - ノード起動中に表示される可能性のある、無害だが誤解を招くエラーログのバグを解決しました。例：
    ```
    [error] Generic event handler emqx_alarm_handler crashed ...
    Reason: {aborted,{no_exists,[emqx_activated_alarm,runq_overload]}}
    ```
  - 特定の条件下でアラーム起動タイムアウトが接続プロセスをクラッシュさせるバグを修正しました。

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) 未定義のパケットフィールドを参照するクライアント情報上書きテンプレートによって引き起こされる NATS ゲートウェイのクラッシュを修正しました。システムは未定義の原子の代わりに空のバイナリを返すようになりました。

#### MQTT over QUIC

- [#15614](https://github.com/emqx/emqx/pull/15614) QUIC リスナー：TLS キーロギング（`SSLKEYLOGFILE`）が有効な場合、ハンドシェイクが失敗しても EMQX は TLS キーをダンプするようになりました。

#### クラスターリンク

- [#15894](https://github.com/emqx/emqx/pull/15894) 以前は、`GET /cluster/links` を介してすべてのクラスターリンクを一覧表示すると、無効なリンクは `inconsistent` ステータスで返されていました。現在は `disconnected` として返されます。

#### パフォーマンス

- [#15696](https://github.com/emqx/emqx/pull/15696) WebSocket (WS) および WebSocket Secure (WSS) リスナーに接続レート制限のサポートを追加しました。
  `max_conn_rate` および `max_conn_burst` 設定オプションが適用されるようになりました。定義されたレートを超える着信接続は、既存の TCP リスナーの動作と一致して、受け入れられるとすぐに閉じられます。
  さらに、`max_connections` の動作が更新されました。接続制限を超えると、WS/WSS リスナーは HTTP ハンドシェイクの前にすぐに接続を閉じ、HTTP 429 応答を返す代わりにソケットを突然閉じます。
- [#15854](https://github.com/emqx/emqx/pull/15854) デフォルトの `active_n` 値を `100` から `10` に減らし、特に高いメッセージレートと小さなペイロードでの MQTT クライアントの応答性を向上させました。
  `active_n` を低くすると、TCP レイヤーでより多くのバックプレッシャーが発生し、デフォルトの `Receive-Maximum` of `32` よりも厳しくなります。これは、次のようなシナリオで役立ちます：
  - クライアントプロセスが外部の認可チェックによってブロックされている
  - データ統合操作がメッセージ処理を遅延させている
  - システムが重い負荷状態にあるか、リソース制限に近づいている
- [#15981](https://github.com/emqx/emqx/pull/15981) 大量の監査ログのクリーンアップ中に Mnesia トランザクションがブロックされることによる過剰なメモリ増加を防ぎました。これにより、大量の監査ログメンテナンス操作中のシステムの安定性とメモリ効率が向上します。