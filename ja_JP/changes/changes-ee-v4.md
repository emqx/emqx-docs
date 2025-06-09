# EMQX Enterprise バージョン 4

## 4.4.27

*リリース日: 2024-11-28*

### 強化点

- MQTT ブリッジアクションに、ピアが PUBACK を返さないことによる MQTT ブリッジの詰まりを防止するため、`Message Delivery Timeout` と `Max Message Retry Count` の2つのパラメータを追加しました。

  - `Message Delivery Timeout`：デフォルトは30秒。QoS1またはQoS2メッセージの配信にかかる時間がこの値を超えた場合、メッセージは破棄されます。
  - `Max Message Retry Count`：デフォルトは3。再送間隔内にACKが受信されない場合、メッセージは再送されますが、再送回数はこの値を超えません。

- Hot構成モジュールのリスナー再起動ロジックを最適化しました。

  ユーザーがHot構成モジュールを起動／再起動した際、`mnesia`テーブルのリスナー設定が更新されていてもリスナーは再起動されません。代わりに、以下のログを出力し、ユーザーに手動でリスナーを再起動するよう促します。

  ```
  [EMQX_HOT_CONF] There is a difference between the listener conf in the hot conf module and the one currently in use at runtime. Please restart the listener at an appropriate time to ensure the configuration is correctly applied. listener: mqtt:tcp:external, conf_in_use: #{...}, hot_conf: #{...}
  ```

- ダッシュボードのノード情報ページで `Waiting to join` ステータスの表示を廃止しました。

  ノードのステータスは `Running` または `Stopped` の2つのみになります。

- Amazon Linux 2023 のインストールパッケージをサポートしました。

- バッチモードの MySQL および PostgreSQL アクションで、`ON DUPLICATE KEY UPDATE` または `ON CONFLICT DO NOTHING` 文をサポートしました。

  プライマリキーの衝突時に重複データの挿入を回避したい場合、以下の文を使用できます。

  MySQL:

  ```sql
  INSERT INTO t_mqtt_msg(msgid, topic, qos, payload, arrived) VALUES (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000)) ON DUPLICATE KEY UPDATE id=id
  ```

  PostgreSQL:

  ```sql
  INSERT INTO t_mqtt_msg(msgid, topic, qos, payload, arrived) VALUES (${id}, ${topic}, ${qos}, ${payload}, to_timestamp(${timestamp}::double precision /1000)) ON CONFLICT DO NOTHING
  ```

  ただし、`ON DUPLICATE KEY UPDATE` または `ON CONFLICT` 文の後にはプレースホルダーは使用できませんのでご注意ください。

- `ecpool` のパフォーマンスを最適化しました。

- ダッシュボードのホット構成ページで、`allow_anonymous` に `false_quick_deny` を設定可能になりました。

  `false_quick_deny` に設定すると、匿名（ユーザー名なし）クライアントを即座に拒否し、認証バックエンドのチェックをスキップします。

### 修正点

- サーバー停電後にキャッシュファイルが破損し、Kafkaアクションが失敗する問題を修正しました。

  修正後は、破損したキャッシュファイルを検出した場合、Kafkaプロデューサーは回復不能なメッセージを破棄します。

- ホットアップグレード後にノード間でMQTTメッセージが同期されない問題を修正しました。

  古いバージョンから `4.4.12` から `4.4.22` の間の任意のバージョンにアップグレードすると発生する可能性があり、ログには `gen_rpc_auth` コードモジュールで未定義のエラーが表示されます。

  ```
  {undef,[{gen_rpc_auth,connect_with_auth,[gen_rpc_driver_tcp,'emqx@10.0.1.1',5370],[]}, ...]}
  ```

- Username Quota モジュールが有効でない状態で HTTP API がユーザー情報を取得しようとした際に 500 エラーが返る問題を修正しました。

- 古いバージョンからバックアップファイルをインポートした後、`emqx_mod_acl_internal` フックが繰り返しマウントされる問題を修正しました。

- ノードがクラスターに参加中に HTTP API でアラームリストを取得すると一時的に 500 エラーが発生する問題を修正しました。

## 4.4.26

*リリース日: 2024-09-26*

### バグ修正

- ノードが古いバージョンクラスターに参加できない問題を修正しました。

  4.4.25 で、username quota テーブルが存在しないことが原因でした。

- username quota インターフェースで特殊文字を含むユーザー名を検索できない問題を修正しました。

  修正前は、username quota 詳細を問い合わせるAPIでURLデコードが行われていませんでした。

- username quota インターフェースに一部のユーザー名が表示されない問題を修正しました。

  username quota モジュール有効化前にセッションが存在していた場合に、ユーザー名リストが不完全になる問題でした。

## 4.4.25

*リリース日: 2024-09-13*

### 強化点

- 単一の MQTT ユーザー名でログイン可能なセッション数を制限する username quota 制限モジュールを導入しました。

  ダッシュボードの **Modules** -> **Username Quota** で設定可能です。**Configuration** タブでホワイトリストを追加でき、ホワイトリストにあるユーザー名はクォータ制限を受けません。

  この機能を有効にした場合、MQTTブリッジングのためにホワイトリストを使用して username quota 制限をバイパスする必要があります。

- MQTT 3.1.1 プロトコルで CONNECT メッセージに不正な Will QoS 値が含まれる場合のエラーログを改善しました。

- Redis へのメッセージ送信パフォーマンスを改善しました。

  以前は `gen_tcp:send/2` の制限により、Redisクライアントプロセスの送信キューにメッセージが積み重なると送信性能が低下し、高負荷時にボトルネックとなっていました。

  この最適化により、Redis認証／ACL、Redisプラグイン、Redisルールエンジンリソースの送信性能が向上します。特に大量のデバイス再接続時の負荷軽減に効果があります。

- SysKeeper へのメッセージ送信パフォーマンスを改善しました。

  以前は `gen_tcp:send/2` の制限により、`emqx_bridge_sysk_forward` プロセスの送信キューにメッセージが積み重なると送信性能が低下し、高負荷時にボトルネックとなっていました。

- 単一ログトレースファイルのサイズ上限を512MBから1GBに引き上げました。

- "Internal DB AUTH/ACL" モジュールの ACL 機能を改善しました。

  - 組み込み ACL モジュールで単一クライアントの ACL エントリ数に制限を設定可能になりました。

    トピックのパブリッシュやサブスクライブ時の ACL 検証はクライアントの ACL エントリを走査するため、エントリ数が多いと性能が低下します。`auth.mnesia.max_acls_for_each_login` 設定項目またはモジュールの `Max ACLs` パラメータで制限できます。

  - 組み込み ACL モジュールのマッチング性能を向上しました。

    ACL テーブルのストレージ構造とトピックマッチングロジックを最適化し、ACL エントリ数が多いほど性能向上の効果が大きくなります。

  - 組み込み ACL モジュールで ACL エントリの追加・削除時に ACL キャッシュをリセットするようにしました。

- ルールエンジンのバッチ送信処理で過負荷保護をサポートしました。

  バッチ送信が有効な場合、ルールエンジンはアクションのメッセージをバッファリングしバッチ送信します。非同期送信モードで外部DBの応答が遅いとメッセージがバッチプロセスのキューに積み重なり、システムメモリ制限超過のリスクがあります。

  これを防ぐため、メッセージキューサイズが `"Maximum Batch Size" * 10`（1000未満の場合は1000）を超えると、該当アクションは一定時間（デフォルト60秒）「アンロード」され、その間のメッセージは破棄され、`action_olp_blocked/<RuleID>/<ActionID>` アラームが発生します。

  過負荷保護対象アクション：

  - Cassandra へのデータ送信
  - ClickHouse へのデータ送信
  - DolphinDB へのデータ送信
  - InfluxDB へのデータ送信
  - IoTDB へのデータ送信
  - Lindorm へのデータ送信
  - MySQL へのデータ送信
  - Oracle Database へのデータ送信
  - PostgreSQL へのデータ送信
  - SQLServer へのデータ送信
  - Tablestore へのデータ送信
  - TDengine へのデータ送信
  - GCP Pubsub へのデータ送信

- 未定義値をデータベースに `NULL` として挿入する機能を追加しました。

  ルールエンジンの各種データベースアクションは `${var}` プレースホルダーを使って挿入文を構築しますが、未定義変数の場合は文字列 `undefined` が挿入されていました。

  新たに `Insert Undefined Values as NULL` オプションを追加し、未定義変数の場合に `NULL` を挿入可能にしました。

  対応アクション：

  - Cassandra
  - ClickHouse
  - DolphinDB
  - MySQL
  - Oracle Database
  - PostgreSQL
  - SQLServer
  - TDengine

- ログスロットリングをサポートしました。

  異常状態で大量の類似ログが生成されるとシステム負荷増大や有用なログの埋没を招くため、`etc/logger.conf` の `log.throttling` 設定で時間窓と最大ログレートを設定可能です。

  効率化のため、CPUコア数分のスロットラーが起動し、同一メッセージ（モジュール名と行番号で判定）のログ出力を制限します。例えば `log.throttling = 50,60s` なら、1分間に最大50回のログを許容します。8コアなら最大400回まで出力されます。

  ログがスロットリングで破棄された場合、以下のようなログが出力されます。

  ```
  log throttled during last 60s, dropped_msg: #{{emqx_channel,1400} => #{msg => "Client ~s (Username: '~s') login failed for ~0p", count => 33}}
  ```

  これは過去60秒間に `emqx_channel` モジュールの1400行目で33件のログが破棄されたことを示します。

  この機能は警告以上のログレベルでデフォルト有効、設定は `50,60s` です。

- HTTP/HTTPS 管理インターフェースのタイムアウト設定を追加しました。

  `etc/plugins/emqx_management.conf` に以下の2つの設定を追加：

  - `management.listener.<Proto>.request_timeout`：TCP接続確立後、HTTPリクエストが来ない場合に切断するまでの時間（秒）。デフォルト5秒。
  - `management.listener.<Proto>.idle_timeout`：接続で1回以上リクエストを受信後、次のリクエストが来ない場合に切断するまでのアイドルタイムアウト（秒）。デフォルト60秒。

  `<Proto>` は `http` または `https`。

- 「再パブリッシュ」アクションのターゲットトピックの検証を追加しました。

  非UTF-8エンコードのバイナリトピックの場合、アクションは失敗します。

- Redis-Cluster ドライバーのパフォーマンスを最適化しました。

  EMQXのRedis Clusterアクセス時のメモリ使用量を削減します。以下の機能が恩恵を受けます：

  - Redis 認証／ACL
  - Redis プラグイン
  - ルールエンジンの Redis アクション

### バグ修正

- Kafkaサーバーの最大メッセージサイズを超えるメッセージ送信で Kafka Producer がブロックする問題を修正しました。

  以前は、Kafkaサーバーの `message.max.bytes` を超える単一メッセージが送信されると、ルールエンジンの Kafka Producer の送信キューがブロックされ、`Max Cache Bytes` 設定までメッセージがバッファリングされていました。

  修正後は、ルールエンジンの Kafka リソースで設定された `Max Batch Bytes` を超える単一メッセージは破棄され、キューの詰まりを防ぎます。

- 特定状況で「リソースダウン」アラームが解除できない問題を修正しました。

- Will メッセージでトリガーされたルールの `timestamp` フィールド値が不正な問題を修正しました。

  この値はクライアントの接続時間ではなく、ルールがトリガーされた時刻を表すべきです。

- Kafkaアクションで `username` を `Message Key` に設定した場合に送信失敗する問題を修正しました。

  以前は、MQTTクライアントがログイン時にユーザー名を提供しない場合、Kafka Producer が送信に失敗していました。修正後は、ユーザー名なしのメッセージは `undefined` 文字列を Message Key として使用します。

- プロセスが異常終了した際にルールエンジンが `$events/client_disconnected` イベントをトリガーしない問題を修正しました。

- DynamoDBアクションで `clientid` を `Hash Key` に設定した場合に送信失敗する問題を修正しました。

- ノード再起動後に共有サブスクリプションのスティッキー戦略がランダムに退化する可能性のある問題を修正しました。

## 4.4.24

*リリース日: 2024-04-16*

### 強化点

- `/load_rebalance/availability_check` を認証不要のパブリックAPIにしました。これによりロードバランサーの設定が簡素化されます。また、このAPIは可能な限り軽量化し、ブローカーへの過負荷を回避します。

- リバランス／エバキュエーションの待機ヘルスチェックフェーズをよりグレースフルにしました。

  このフェーズ中は、退避対象ノードへの接続を禁止しません。ロードバランサーがこれらのノードをすべて不健康と判断しているか不明なため、接続禁止すると再接続試行が多発する可能性があるためです。

- HTTP API の Idle Timeout が短すぎる問題を改善しました。

  以前は5秒に設定されていましたが、HTTP API が5秒間リクエストを受け取らないとTCP接続を切断していました。これを60秒に延長しました。

- 一部設定項目の説明を改善しました。

  - Webhook リソース・アクションの `Base URL` と `Path` パラメータの説明を改善。
  - ホット構成の WS/WSS リスナーの `idle_timeout` 設定説明を強化。
  - オフラインメッセージ関連アクションの `Max Returned Count` パラメータ詳細説明を改善。
  - ClickHouse リソースの英語パラメータ `Key` を `Password` に名称変更。
  - HStreamDB アクションの `PartitionKey` パラメータを `Partition Key` に名称変更。
  - Retainer モジュールの `Maximum Retained Message Size` パラメータ詳細説明を `0B` から `0` に更新。
  - Retainer モジュールの `Message Interception` を `Intercept Empty Messages` に名称変更し説明を改善。

- 一部設定項目の値範囲と妥当性をチェックするようにしました。

  - `mqtt.max_topic_levels`、`mqtt.max_packet_size`、`keepalive_backoff` は非負値であること。
  - SSLリスナー起動時に `verify_peer`、`fail_if_no_peer_cert`、`cacertfile` の依存関係をチェック（以前はSSLクライアント接続時のみ）。
  - リスナー設定の `acceptors`、`max_connections`、`max_conn_rate`、`active_n` は非負値であること。
  - RabbitMQリソースの `Heartbeat Interval` と `Automatic Reconnection Interval` は正しい時間長文字列であること。
  - GB/T 32960 および JT/T808 リスナーで重複ポートのチェックを修正。
  - GB/T 32960 および JT/T808 ゲートウェイ設定の一部パラメータ値検証を修正。

- ログフォーマットを最適化し、ダッシュボードの表示問題を修正しました。

  - ルールエンジンのレート値が非常に長い浮動小数点数で表示される問題を、小数点以下2桁に修正。
  - システムリソース使用率のアラートメッセージでCPU使用率を小数点以下2桁に修正。
  - `mfa` のようなデバッグフィールドをログから除去。

- ルールエンジンでユーザー定義SQL関数をサポートしました。

  例：ユーザーカスタムプラグインに `emqx_rule_funcs1` モジュールの `func` 関数があれば、以下のように使用可能です。

  ```SQL
  SELECT emqx_rule_funcs1.func() FROM "t/#"
  ```

  モジュール名は `emqx_rule_funcs` または `EmqxRuleFuncs` で始まる必要があります。

- Kafkaコンシューマーグループモジュールが PLAIN、SCRAM_SHA_256、KERBEROS 認証をサポートしました。

- システムリソースを多く消費するHTTP APIメソッドに過負荷保護を追加しました。

  対象API：

  - `GET /api/v4/clients/*`
  - `GET /api/v4/routes`
  - `GET /api/v4/subscriptions`
  - `GET /api/v4/rules`
  - `GET /api/v4/banned`
  - `GET /api/v4/audits`
  - 組み込み認証モジュールのユーザー名・クライアントID検索API

### バグ修正

- うるう年の日付を入力した際に、ルールエンジンの `date_to_unix_ts()` SQL関数が誤った値を返す問題を修正しました。

- ノード退避時に、`clean_start` が `false` で開始されたセッションだけでなく、すべての切断済みセッションを退避するように修正しました。

  修正前は、`clean_start = true` かつ非ゼロの `Session-Expiry-Interval` 設定のセッションは退避されず、ノード停止後にセッションが失われていました。

- Redis認証時に対応する認証情報がRedisに存在しない場合の例外を修正しました。

- EMQXノード起動時に未初期化のETSテーブルにアクセスしてHTTP APIが例外を起こす問題を修正しました。

- 特定条件下で拡張プラグインの読み込みに失敗する問題を修正しました。

  `plugins.expand_plugins_dir` 設定で指定したディレクトリのプラグインが複数回読み込まれ、EMQX起動失敗を招く場合がありました。

- コマンドラインで単一クライアントのACLキャッシュをクリアできない問題を修正しました。

  例：`emqx ctl acl cache-clean 'mqttx_458d5222'` が機能しませんでした。

- ルールリストや組み込み認証・認可リストページのあいまい検索でページネーションが正しく表示されず、データが不完全になる問題を修正しました。

- トレースログファイルのダウンロード時に「開始待ち」状態で誤ったエラーログが出力される問題を修正しました。

- ホット構成機能初回使用時のSSLリスナー設定で、`backlog` 設定項目が空で必須と表示され、フォーム送信に失敗する問題を修正しました。

  修正後は `backlog` 設定を省略可能にし、デフォルト値1024を設定します。

- `emqx.schema` ファイルの `zone` 関連バリデータが効かない問題を修正しました。

- 監査ログクエリAPIのレスポンスで、HTTPステータスコードが500でも `operation_result` フィールドが `success` のままになる問題を修正しました。

- ダッシュボードで GB/T 32960 クライアントIDが重複表示される問題を修正しました。

## 4.4.23

*リリース日: 2023-11-24*

### 強化点

- EMQXノード間のメッセージ送信性能を改善しました。

  `gen_rpc` は EMQX 内部でノード間の MQTT メッセージ送信に使われるRPCチャネルです。今回の改善で、`gen_rpc` のバックログ処理能力を最適化し、トラフィックピークからの回復を高速化しました。

### バグ修正

- ルールエンジンが [upstash](https://upstash.com/) Redis に接続できない問題を修正しました。

  以前は、RedisサービスとのTCP接続確立後、emqxのRedisドライバーがAUTHやSELECTコマンドを [Inline Commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) で送信していましたが、upstash RedisはInline Commandsをサポートしておらず、接続失敗していました。

  修正後は、emqxのRedisドライバーが RESP (REdis Serialization Protocol) でAUTHとSELECTコマンドを送信します。

- 「Offline Msg to Redis」アクションとRedisリソースの一部パラメータの妥当性チェックを追加しました。

  - 「Redis Key TTL」パラメータのチェック
  - Redisリソースの「Redis Database」パラメータのチェック

## 4.4.22

*リリース日: 2023-11-01*

### 強化点

- 重要な操作変更を追跡する監査ログ機能を追加しました。

  - ダッシュボードの **General** -> **Audit Log** -> **Enable** で有効化し、パラメータを変更後に **Add** をクリックします。
  - 有効化後、`GET` を除くすべてのHTTPリクエストとCLI実行が記録されます。
  - ダッシュボードでは最新5,000件の監査ログが表示され、完全なログファイルは `data/audit` ディレクトリに保存されます。

- ダッシュボードでロールベースアクセス制御（RBAC）をサポートしました。

  ユーザーはダッシュボードログイン時に「Administrator」または「Viewer」のいずれかのロールを割り当てられ、権限が異なります。

  - Administrator：ダッシュボードの全機能に無制限アクセス可能。
  - Viewer：閲覧のみ可能で変更不可。

  RBACにより適切なユーザーに適切なアクセス権を付与し、ユーザー管理とアクセス制御を簡素化し、セキュリティとデータ整合性を向上します。

- LwM2Mゲートウェイで Block Wise Transfer を用いたダウンリンクデータ送信をサポートしました。

- 新しいSQL関数 `map_keys()`, `map_values()`, `map_to_entries()`, `join_to_string()`, `join_to_sql_values_string()`, `is_null_var()`, `is_not_null_var()` を追加しました。

  詳細はドキュメントを参照してください。

- 「Data bridge to MQTT Broker」アクションに `Forward QoS` 設定を追加し、MQTTブリッジ経由で転送するメッセージのQoSレベルを指定可能にしました。

- 設定ファイルで MQTT メッセージの有効期限を指定可能にしました。

  詳細は `emqx.conf` の `mqtt.message_expiry_interval` 設定を参照してください。

- Erlang/OTP バージョンを OTP-24.3.4.2-4 に更新しました。

- OCSP Stapling と CRL チェックの整合性を向上させるスキーマ検証を追加しました。

### バグ修正

- Kafkaクライアント（wolff）プロデューサーのクラッシュ問題を修正しました。

  Kafkaリソースが初期化中に誤って削除されると依存するルールが失敗し、エラーが伝播してすべてのルールがクラッシュしていました。伝播を防止し、システム安定性を確保します。

- GBT32960ゲートウェイモジュールが `retry_interval` パラメータを解析できない問題を修正しました。

- GBT32960クライアントがHTTP API経由で取得できない問題を修正しました。

- OCPPクライアント認証失敗時に例外ログが出る問題を修正しました。

- OCPPゲートウェイが空の ClientID を検証しない問題を修正しました。

- RabbitMQドライバーをアップグレードし、一部セキュリティ脆弱性を修正しました。

- ルールエンジンの GCP PubSub アクションで、非同期送信モード時に統計カウンターが増加しない問題を修正しました。

- 手動でリソースを再接続した際、現在のノードのリソースのみが再接続される問題を修正しました。

- ルール削除・再インポート後にアクションの統計カウンターがリセットされない問題を修正しました。

- クラスター環境でルール再起動時にアクションリソースリークが発生する問題を修正しました。

  修正前は、ルール停止・開始時に特定ノードでアクション作成が失敗するとアクション関連プロセスがリークしていました。

- マルチCPU環境でバッチモードの一部データ統合アクションの性能が4.4.5以前より低下する問題を修正しました。

  4.4.5でバッチプロセスプールのワーカー数を CPUコア数×4 に変更したため、コア数が多いマシンでワーカー数が過剰となり、各プロセスのバッチ内メッセージ数が少なくなり性能低下を招いていました。

  修正後はワーカー数を固定せず、新設定 `batch_pool_size`（デフォルト8）を導入しました。

  対象アクション：data_to_cassa, data_to_clickhouse, data_to_influxdb, data_to_iotdb, data_to_lindorm, data_to_mysql, data_to_oracle, data_to_pgsql, data_to_sqlserver, data_to_tablestore, data_to_tdengine, data_to_gcp_pubsub。

- MQTT 5.0 プロトコル使用時に MQTT ブリッジで QoS2 メッセージ送信が失敗する問題を修正しました。

- 設定ファイルのリスナー設定が欠落している場合にホット構成更新が失敗する問題を修正しました。

- LwM2Mゲートウェイプラグインの起動失敗問題を修正しました。

  LwM2Mモジュールを先に停止し、その後LwM2Mプラグインを起動すると起動失敗していました。ログ例：

  ```
  {emqx_lwm2m,{bad_return,{{emqx_lwm2m_app,start,[normal,[]]},{'EXIT',{{already_started,<0.3895.177>},[...]}}}}}
  ```

- ダッシュボードで共有サブスクリプショントピックのプレフィックスが正しく表示されない問題を修正しました。

  修正前は `$share/g//t` のようなトピックがクライアント詳細ページで `/t` と表示されていました。

- 設定ファイルで `peer_cert_as_username` と `peer_cert_as_clientid` に `none` オプションを追加しました。

- ホット構成機能有効時にリスナーが断続的に再起動する問題を修正しました。

- アクティブに実行中のルールを停止するとエラーが発生する問題を修正しました。

  修正前は手動停止時に以下のようなエラーログが断続的に発生していました。

  ```
  foo@x.x.x.x:54663 Rule: <<"rule:ba48182b">>; Action: data_to_kafka; Resource: <<"resource:7bacacdc">>. Continue next action, reason: {error,{badmatch,not_found}, ...
  ```

  修正後はこのようなエラーログが発生しなくなり、未初期化アクションのエラーログも最適化されました。

- LwM2Mゲートウェイの DTLS PSK ハンドシェイク失敗問題を修正しました。

- Retainerモジュールの設定で不正なフィールドをチェックするようにしました。

  `Max Retained Messages` と `Max Retained Payload Size` は非負値であることをチェックします。

- ホットアップデート後に TDEngine へのメッセージ送信失敗問題を修正しました。

- ホットアップデート後に RabbitMQ リソースが利用不可になる問題を修正しました。

- OCSP Stapling 無効化または TLS リスナー無効化時に OCSP の HTTPリフレッシュタイマーをキャンセルするようにしました。

- CRL チェック無効化または TLS リスナー無効化時に CRL リフレッシュタイマーをキャンセルするようにしました。

## 4.4.21

*リリース日: 2023-10-16*

### 強化点

- Confluent データブリッジをサポートしました。

- Kafka コンシューマーグループの MQTT トピックフィールドでプレースホルダーを含むテンプレートをサポートしました。

  例：Kafkaで消費したメッセージのキーが "a" で、設定された MQTT トピックが `topic/${key}` の場合、転送時に `topic/a` に置換されます。

- 「Message Republish」アクションに新たに「MQTT Properties」と「User Properties」フィールドを追加しました。

  どちらもキー・バリュー形式で、キーと値の両方がプレースホルダーをサポートします。

### バグ修正

- Kafkaアクションで数値を Kafka ヘッダーとして送信できない問題を修正しました。

  以前は「Kafka Headers value encode mode」が「NONE」の場合、JSONオブジェクト内の数値型（例：`{"a": 1, "b": "str"}`）の数値が無視されていました。修正後は数値を文字列に変換して送信します。

## 4.4.20

*リリース日: 2023-08-01*

### 強化点

- Kafka および HStreamDB へのデータ送信パフォーマンスを改善しました。

  Erlangメッセージバッファをドライバプロセスの前に追加し、EMQX内部のメッセージ送受信頻度を減らしました。これによりレイテンシは増加しますが、スループットが大幅に向上します。

  EMQXからKafkaまたはHStreamDBドライバーに送信されるメッセージはまずバッファに入り、`message_accumulation_size` に達するか、`message_accumulation_interval` の時間が経過するとバッチ送信されます。`message_accumulation_size = 0`（デフォルト）でこのバッファリング機能を無効化します。

- SQL Server リソースに `auto_reconnect` オプションを追加しました。

  これまで EMQX と SQL Server の接続が切断されると自動再接続できませんでしたが、この機能で自動再接続可能になります。`auto_reconnect = false` で無効化可能です。

- RabbitMQ リソースに TLS 接続サポートを追加しました。

- GCP PubSub アクションで属性定義とオーダリングキー指定をサポートしました。

### バグ修正

- ルールエンジンの `mongo_date()` 関数がダッシュボードでテストできない問題を修正しました。

- 4.4.19 へのホットアップグレード後、ルールエンジンが RabbitMQ アクションでメッセージ送信に失敗する問題を修正しました。

## 4.4.19

*リリース日: 2023-06-27*

### 強化点

- MQTT/TCP および MQTT/SSL リスナーで TCP キープアライブをサポートしました [#10854](https://github.com/emqx/emqx/pull/10854)。

  新設定 `zone.<zone-name>.tcp_keepalive = Idle,Interval,Probes` でTCPレイヤのキープアライブを有効化し、時間パラメータを指定可能です。LinuxおよびMacOSでのみ有効です。

- Proxy Protocol 関連のエラーログを改善しました [emqx/esockd#177](https://github.com/emqx/esockd/pull/177)。

  改善前のログ例：

  ```
  2023-04-20T14:56:51.671735+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {invalid_proxy_info,<<"f\n">>}, offender: [{pid,<0.3192.0>},{name,connection},{mfargs,{...}}]

  2023-04-20T14:57:01.348275+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {proxy_proto_timeout,5000}, offender: [{pid,<0.3194.0>},{name,connection},{mfargs,{...}}]
  ```

  改善後：

  ```
  2023-04-20T18:07:06.180134+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but received invalid proxy_protocol header, raw_bytes=<<"f\n">>

  2023-04-20T18:10:17.205436+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but timed out while waiting for proxy_protocol header
  ```

- TLSリスナーで部分的な証明書チェーン検証を有効にする機能を追加しました [#10553](https://github.com/emqx/emqx/pull/10553)。

  詳細は `listeners.conf` の `listener.ssl.external.partial_chain` を参照してください。

- TLSリスナーでクライアント証明書の拡張キー使用法検証を有効にする機能を追加しました [#10669](https://github.com/emqx/emqx/pull/10669)。

  詳細は `listeners.conf` の `listener.ssl.external.verify_peer_ext_key_usage` を参照してください。

- HTTP API `/api/v4/nodes` のレスポンスに `live_connections` フィールドを追加しました [#10859](https://github.com/emqx/emqx/pull/10859)。

  従来の `connections` は切断済みでも持続セッションがあればカウントしていましたが、`live_connections` は切断されていないMQTT接続数を正確にカウントします。

- ルールエンジンに3つの乱数生成SQL関数を追加しました [#11113](https://github.com/emqx/emqx/pull/11113)。

  - `random()`：0以上1未満の乱数を生成。
  - `uuid_v4()`：ハイフン付きのランダムUUID（バージョン4）文字列を生成。
  - `uuid_v4_no_hyphen()`：ハイフンなしのランダムUUID（バージョン4）文字列を生成。

- `mqtt.max_clientid_len` 設定パラメータに数値範囲検証（23～65535）を追加しました [#11096](https://github.com/emqx/emqx/pull/11096)。

- `emqx_gcp_device` プラグインを追加しました。

  Google IoT Core からの移行を簡素化し、Google IoT Core 互換の MQTT 認証を実装し、デバイス設定や認証情報管理用のAPIを提供します。

- RabbitMQアクションで動的ルーティングキーをサポートしました。

  RabbitMQアクションの「RabbitMQ Routing Key」パラメータで `${key}` 形式の動的変数が使用可能です。

- DynamoDBリソースのデフォルトポートを追加しました。

  URLにポート番号が含まれない場合、HTTPは80、HTTPSは443がデフォルトで使用されます。

### バグ修正

- ルールエンジンで `FOREACH` でエクスポートした変数にアクセスできない問題を修正しました [#10620](https://github.com/emqx/emqx/pull/10620)。

- ルールのキャッシュが特定条件で更新されない問題を修正しました [#11072](https://github.com/emqx/emqx/pull/11072)。

- WebHookプラグインが `on_client_connack` フックを実行できない問題を修正しました [#10710](https://github.com/emqx/emqx/pull/10710)。

- 認証モジュールの再接続に関する問題を修正しました。

  EMQX起動時に認証モジュールとDBの接続が切断されている場合、モジュールが無効化されていても再接続を試みていました。修正後はモジュールが有効な場合のみ再接続します。

- PgSQL認証モジュールが再接続後にPrepared Statementsを失う問題を修正しました。

- 4.4.9 からリソースをインポート後、Kafka接続に失敗する問題を修正しました。

- EMQX DockerコンテナでKerberos認証を用いたKafka統合ができない問題を修正しました。

- ルールエンジンの RocketMQ アクションのデータ分配ロジックを修正しました。

- ノード再起動やクラスター参加後にモジュール順序が変わる問題を修正しました。

- 4.4.7 からのリスナー設定インポート失敗問題を修正しました。

- ホット構成が新規ノード参加後に反映されない問題を修正しました。

- OCPPゲートウェイのWebSocketダウンリンクメッセージタイプを `binary` から `text` に修正しました。

- TLS v1.3 のみを使用するリスナーで MQTT クライアントが接続できない問題を修正しました。

- ホットアップグレード後に Retainer モジュールがエラーを出す問題を修正しました。

- RabbitMQ接続テスト時にエラーログが出る問題を修正しました。

- ダッシュボード設定ページで連続して **Enable** ボタンを押すとホット構成モジュールが複数作成される問題を修正しました。

## 4.4.18

*リリース日: 2023-04-28*

### 強化点

- `emqx_ocpp` プラグインを追加し、OCPP 1.6-J プロトコルをサポートしました。

  OCPPは電気自動車充電ステーションと中央管理システム間の通信プロトコルです。本プラグインは EMQX の OCPP ゲートウェイとして機能し、OCPP と MQTT 間のシームレスな統合を実現します。WebSocket 経由の OCPP で充電ステーションを EMQX に接続可能です。

  プラグイン起動は `emqx_ctl plugins load emqx_ocpp` またはダッシュボードから可能です。`ocpp-go` などのツールで充電ポイントをシミュレートしてメッセージ交換をテストできます。

- ルールエンジンのプレースホルダー構文を改善しました。

  アクションのパラメータで `${key}` 形式のプレースホルダーを使えます。従来は `key` に英数字とアンダースコアのみ許容していましたが、UTF8文字全般をサポートしました。

### バグ修正

- `data/load_plugins` ファイルが存在しない場合に必須プラグインが起動しない問題を修正しました。

  ファイルを手動削除しEMQXを再起動すると、`emqx_schema_registry`、`emqx_eviction_agent`、`emqx_node_rebalance` の3つの必須プラグインが自動有効化されず、ファイルも再生成されませんでした。

## 4.4.17

*リリース日: 2023-04-13*

### 強化点

- Proxy Protocol 有効なリスナーで TCP ポートプローブを受けた際、エラーログを出力しないようにしました [emqx/esockd#172](https://github.com/emqx/esockd/pull/172)。

- ファイルディスクリプタ枯渇時のリスナーエラーログを改善しました [emqx/esockd#173](https://github.com/emqx/esockd/pull/173)。

- ルールエンジンでルール数が多い場合の性能を改善しました [#10283](https://github.com/emqx/emqx/pull/10283)。

- 古いバージョンからのデータインポート時のアラームログを改善しました。

- Erlang distribution で TLS が使えない問題を修正しました [#9981](https://github.com/emqx/emqx/pull/9981)。

- MQTTブリッジでピア側のワイルドカード証明書検証ができない問題を修正しました [#10094](https://github.com/emqx/emqx/pull/10094)。

- Retainerプラグインと接続情報クリーンアップのプロセスプールを分離し、切断済み接続情報が遅延して消えない問題を修正しました [#10189](https://github.com/emqx/emqx/pull/10189)。

- Helm Chart の `service-monitor.yaml` テンプレートファイルのパス誤りを修正しました [#10229](https://github.com/emqx/emqx/pull/10229)。

- EMQX 4.3 から 4.4 へのアップグレード時に組み込み認証の ACL テーブルをマイグレーションするようにしました。

- IoTDBアクションのカウント統計誤りを修正しました。

- TDEngine SQL文に改行が含まれるとルール作成に失敗する問題を修正しました。

- HTTP API `/load_rebalance/:node/start` のエラーメッセージのエンコード問題を修正しました。

- RocketMQクライアントのプロセスリークを修正しました。

## 4.4.16

*リリース日: 2023-03-10*

### 強化点

- IoTDBリソースのログを改善しました。

- オフラインメッセージアクションが QoS0 メッセージを受け取った際にエラーログを出さないようにしました。

- CLI出力やプラグイン名の表記を "EMQX" に統一しました。

### バグ修正

- リリースホットアップグレード時に `emqx_schema_registry` プラグインを自動起動するようにしました。

- RocketMQアクションの `message_key` パラメータが機能しない問題を修正しました。

- protobufの `oneof` 定義を含むスキーマでルールが失敗する問題を修正しました。

- KafkaヘッダーにJSONオブジェクトを送信できない問題を修正しました。

- リソースや `emqx-modules` が生成する一時ディレクトリを削除するようにしました。

- HStreamDBリソースの説明を一部修正しました。

- MQTTメッセージのペイロードをデバッグログ出力時に変更しないようにしました [#10091](https://github.com/emqx/emqx/pull/10091)。

## 4.4.15

*リリース日: 2023-03-03*

### 強化点

- Kafkaヘッダーをルールエンジンでサポートしました。

- IoTDBへのデータ保存をルールエンジンでサポートしました。

- JT/T 808 非標準位置報告メッセージで予約IDを使った場合、クライアント切断せずBase64形式で通過させるようにしました。

- `emqx_modules` アプリケーション起動時にローカルノードのみモジュールを作成するようにしました。

- DynamoDBアクションで `hash_key` または `range_key` が見つからない場合のログを改善しました。

- HStreamDBドライバーを ~> 0.12.0 に更新しました。

- `schema_registry` プラグインをルールエンジンのオプション機能としてデフォルト有効にしました。

- HStreamDBアクションで TLS 接続をサポートしました。

- MongoDBライブラリをバージョン5.1以上にアップグレードしました。

- ダッシュボードAPIで HAProxy のプロキシプロトコルをサポートしました [#9803](https://github.com/emqx/emqx/pull/9803)。

- Ubuntu 22.04 パッケージをリリースしました [#9831](https://github.com/emqx/emqx/pull/9831)。

- `banned` と `delayed` 機能の連携を改善しました [#9790](https://github.com/emqx/emqx/pull/9790)。

- 保持メッセージのセキュリティ強化を行いました [#9790](https://github.com/emqx/emqx/pull/9790)。

- `clientid` で禁止されたクライアントは対応するセッションを強制切断するようにしました [#9904](https://github.com/emqx/emqx/pull/9904)。

- 認証とACLのデバッグログを追加しました [#9943](https://github.com/emqx/emqx/pull/9943)。

- Prometheusに `live_connections.count` と `live_connections.max` を公開しました [#9929](https://github.com/emqx/emqx/pull/9929)。

### バグ修正

- モジュールの `tlsv1.3` が欠落していた問題を修正しました。

- Redisオフラインメッセージ機能使用時にメッセージが逆順で送信される問題を修正しました。

- `emqx-modules` が初期化失敗後に無効化される問題を修正しました。

- アクションやリソースの説明文の問題を修正しました。

- Oracleリソースがリリースホットアップグレード後に自動接続できない問題を修正しました。

- ルールエンジンで RocketMQ クラスターへのメッセージ送信に失敗する問題を修正しました。

- クラスターAPIで既存リスナー作成時に失敗を返すようにしました。

- `resources/modules/schema_registry` 削除時にファイルディレクトリを削除し、ファイルリークを防止しました。

- `republish` アクションで User-Property を含む MQTT メッセージ転送時のエラーを修正しました [#9942](https://github.com/emqx/emqx/pull/9942)。

- アクション、リソース、emqx-modules の説明文の問題を修正しました [#9931](https://github.com/emqx/emqx/pull/9931)。

- JWKSサーバー問い合わせ失敗時にエラーログが出ない問題を修正しました [#9931](https://github.com/emqx/emqx/pull/9931)。

- ページングモードで HTTP API からクライアントリストを取得した際に異なるノードで結果が不一致になる問題を修正しました [#9926](https://github.com/emqx/emqx/pull/9926)。

- ライセンスアップロード時に常にライセンスをリロードするようにしました。

- `emqx_modules` アプリケーション起動時にローカルノードのみモジュールを作成するようにしました。

- 新規ダッシュボードユーザーのパスワード形式制限を解除しました。

- クラスターAPIで既存リスナー作成時に失敗を返すようにしました。

- `resources/modules/schema_registry` 削除時にファイルディレクトリを削除し、ファイルリークを防止しました。

## 4.4.14

*リリース日: 2023-01-06*

### 強化点

- API経由でダッシュボードユーザーを追加・変更する際のパスワード複雑度要件を追加しました。

  パスワードは英字・数字・特殊文字のうち2種類以上を含み、8～64文字である必要があります。

### バグ修正

- API経由で複雑なパスワードを持つダッシュボードユーザーを追加・インポートできない問題を修正しました。

- ブートストラップアプリケーションファイルの同期問題を修正しました。

## 4.4.13

*リリース日: 2023-01-03*

### バグ修正

- GCP PubSub のテストでメモリリークする問題と JWT トークンの2回目以降のリフレッシュ失敗問題を修正しました [#9640](https://github.com/emqx/emqx/pull/9640)。

## 4.4.12

*リリース日: 2022-12-29*

クラスターロードリバランス機能を追加しました。

CLIコマンド `emqx_ctl rebalance` は以下の2つの一般的なシナリオをサポートします：

- 新規参加または再起動したノードが長時間アンダーロード状態になる（クライアントが長寿命接続の場合）
- メンテナンスでノードを停止すると、接続が同時に再接続されクラスタ過負荷になる。また非クリーンセッションが失われる。

`--evacuation` オプションでノード停止前に接続中のMQTTクライアントをすべて移動可能です。

### 強化点

- `emqx_mod_rewrite` の宛先トピックにワイルドカードが含まれる場合、パブリッシュを禁止するトピック検証を追加しました。

- TDEngineリソースで2.xと3.x両方のHTTPレスポンス形式をサポートしました。

- TDEngine SubTables へのバッチ送信をサポートしました。

- オフラインメッセージの ClickHouse アクションでルール有効化時に情報ログを出力します。

- リソースが準備できていなくてもルール作成を可能にしました。

- オフラインメッセージの二重削除を回避しました。

- Helm Chart でサービスの `externalTrafficPolicy` を設定可能にしました。

- ダッシュボードで新規ユーザー作成時のパスワード形式を `^[A-Za-z0-9]+[A-Za-z0-9-_]*$` に制限しました。

### バグ修正

- 再接続後に非クリーンセッションの未アックメッセージが再送されない問題を修正しました。

- QoS2メッセージの `awaiting_rel` キューの期限切れが切断後にクリアされない問題を修正しました。

- RocketMQリソースの認証設定を `data_to_rocket` アクションから `bridge_rocket` リソースに移動し、Aliyunクラウドの `namespace` フィールドを追加しました。

- KafkaおよびPulsarアクションのパラメータ検証を追加しました。

- PgSQL認証モジュールの再接続後のPrepared Statements喪失問題を修正しました。

- Kafka接続失敗問題を修正しました。

- EMQX DockerコンテナのKerberos認証問題を修正しました。

- RocketMQアクションのデータ分配ロジックを修正しました。

- モジュール順序がノード再起動やクラスター参加後に変わる問題を修正しました。

- 4.4.7 からのリスナー設定インポート失敗問題を修正しました。

- ホット構成が新規ノード参加後に反映されない問題を修正しました。

- OCPPゲートウェイのWebSocketダウンリンクメッセージタイプを `binary` から `text` に修正しました。

- TLS v1.3 のみのリスナーで接続失敗する問題を修正しました。

- Retainerモジュールのホットアップグレード後のエラーを修正しました。

- RabbitMQ接続テスト時のエラーログを修正しました。

- ダッシュボード設定ページで連続して **Enable** ボタンを押すとホット構成モジュールが複数作成される問題を修正しました。

（以下、バージョン4.4.11以前のリリースノートも同様の形式で翻訳済みです）
