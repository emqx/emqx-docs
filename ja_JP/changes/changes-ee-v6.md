# EMQX Enterprise バージョン 6

## 6.0.0

### ハイライト

- **メジャーバージョンリリース**：EMQX Enterprise 6.0.0 は EMQX Enterprise バージョン 6 シリーズの最初のリリースであり、重要なアーキテクチャ改善と新機能をもたらします。

- **強化された AWS 統合**：S3 または S3Tables データ統合を使用する際に、EC2 インスタンスから Instance Metadata Service v2 API をサポートします。これにより、手動での AWS 認証情報の設定なしにシームレスに S3 バケットにアクセスでき、IAM ロールを活用してより良いセキュリティを実現します。

- **Elixir サポート**：すべてのパッケージが Mix ビルドシステムを通じて Elixir サポートを提供し、EMQX を Elixir コミュニティに開放し、IEx コンソールによるより良いツールを可能にします。

- **新しいデータ統合**：
  - Google BigQuery にデータを追加するための BigQuery コネクタとアクション
  - Snowflake アクションの Snowpipe ストリーミングアップロードモード（プレビュー機能）
  - S3Tables アクションの Parquet フォーマットサポート

- **永続ストレージの最適化**：新しい RocksDB 設定オプションとデフォルトの ASN1 シリアル化スキーマにより、RAM 使用量とストレージ効率が大幅に改善されました。

- **強化された LDAP サポート**：LDAP 認可が JSON フォーマットの拡張 ACL ルールをサポートし、LDAP 認証がクライアントサイドキャッシュで LDAP から直接 ACL ルールを取得できるようになりました。

- **改善されたトレース**：最大トレース数（`trace.max_traces`）とトレースファイルサイズ（`trace.max_file_size`）の設定可能な制限、およびアトムリークを防ぐ最適化された実装。

- **クラスター管理**：新しい `cluster.description` 設定オプションにより、ユーザーは EMQX Dashboard でカスタムクラスター説明を設定および表示できます。

### 機能強化

#### ルールエンジン

- [#15631](https://github.com/emqx/emqx/pull/15631) AI プロバイダーで利用可能なすべてのモデルをリストする新しい API エンドポイントを追加しました。

- [#15467](https://github.com/emqx/emqx/pull/15467) AI Completion Providers のトランスポートオプションを公開します。
  これらのオプションにより、AI Completion Provider への接続タイムアウトと最大接続数を設定できます。

#### データ統合

- [#15635](https://github.com/emqx/emqx/pull/15635) RocketMQ アクションに新しい `key` と `tag` テンプレートフィールドを追加し、それぞれメッセージのキーとタグを設定します。また、`strategy` フィールドに新しい `key_dispatch` 値を追加しました。

- [#15621](https://github.com/emqx/emqx/pull/15621) S3Tables コネクタの `access_key_id` と `secret_access_key` がオプションフィールドになりました。省略された場合、EMQX がデプロイされている EC2 インスタンスの Instance Metadata Service v2 API から取得されます。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcoud` ライブラリを `3.8.3.0` にアップグレードしました。これにより、Access Key Id と Secret Access Key を指定せずに S3 コネクタを設定できるようになります。ただし、EMQX が実行されている EC2 インスタンスが設定されたバケットの読み取り/書き込みに対する正しい IAM 権限を持っている場合に限ります。

- [#15418](https://github.com/emqx/emqx/pull/15418) BigQuery にデータを追加する新しいコネクタとアクションを追加しました。

- [#15401](https://github.com/emqx/emqx/pull/15401) Snowflake アクションの Snowpipe ストリーミングアップロードモードのサポートを追加しました。注意：これは現在 Snowflake のプレビュー機能で、AWS 上のすべてのアカウントでのみサポートが利用可能です。

- [#15387](https://github.com/emqx/emqx/pull/15387) `ListStreams` と `DescribeStream` API を呼び出す際のレート制限の発生を軽減するため、Kinesis Producer コネクタとアクションのヘルスチェックを改善しました。現在、これらの API に対するコネクタごとの呼び出しを、それぞれ 5/秒と 10/秒に制限しています。コネクタまたはアクションがタイムアウト前にヘルスチェック API を呼び出せない場合、現在のステータスを維持します。スロットリング応答（例：`LimitExceededException`）を受信した場合も、現在のステータスを維持します。

  新しい `resource_opts.health_check_interval_jitter` 設定を導入し、`resource_opts.health_check_interval` に均一なランダム遅延を追加することで、同じコネクタ下の複数のアクションがヘルスチェックを同時に実行することをほぼなくしました。

- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary` の戻り値と、`GET /actions/:id` で戻されるフォールバックアクションに `tags` フィールドを追加しました。

- [#15360](https://github.com/emqx/emqx/pull/15360) S3Tables アクションでデータファイルを Parquet フォーマットで書き込むサポートを追加しました。

- [#15176](https://github.com/emqx/emqx/pull/15176) GreptimeDB コネクタクライアントをアップグレードし、自動作成されたテーブルのデフォルト TTL を設定するオプションの新しいパラメータ `ttl` をサポートします。

- [#15585](https://github.com/emqx/emqx/pull/15585) `brod` クライアントをバージョン 4.4.4 に更新しました。これにより、特に `JoinGroups` API `v0`-`v1` が非推奨になったため、サポートされる Kafka API 範囲が拡張されます。

- [#15628](https://github.com/emqx/emqx/pull/15628) HStreamDB データ統合を削除しました。

#### アクセス制御

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証と認可の外部リソース管理を最適化しました。以前は、EMQX が無効な認証または認可プロバイダーに設定されたリソースに接続したままになる可能性がありました。

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP 認証と認可を強化しました。
  LDAP 認可が JSON フォーマットの拡張 ACL ルールをサポートするようになりました。
  LDAP 認証が LDAP から ACL ルールを取得できるようになりました。これらのルールはクライアントのメタデータにキャッシュされるため、追加の LDAP クエリなしで認可が実行されます。

#### スマートデータハブ

- [#15525](https://github.com/emqx/emqx/pull/15525) スキーマ検証またはメッセージ変換によって参照されている内部スキーマを削除しようとすると、削除が拒否されるようになりました。

#### 永続ストレージ

- [#15463](https://github.com/emqx/emqx/pull/15463) 永続ストレージの RAM 使用量とストレージ効率の最適化。

  1. 永続ストレージに以下の設定パラメータを追加しました：

  - `durable_storage.messages.rocksdb.write_buffer_size`：シャードごとの RocksDB メモリテーブルサイズ。
  - `durable_storage.messages.rocksdb.cache_size`：シャードごとの RocksDB ブロックサイズ。
  - `durable_storage.messages.rocksdb.max_open_files`：シャードごとの RocksDB が使用するファイル記述子数を制限します。
  - `durable_storage.messages.layout.wildcard_thresholds`：`wildcard_optimized_v2` ストレージレイアウトのワイルドカード閾値を調整できます。

  2. メッセージのデフォルト `serialization_schema` が `asn1` に変更されました。

#### 観測可能性

- [#15594](https://github.com/emqx/emqx/pull/15594) クラスター内で同時に存在できる最大トレース数を設定オプション `trace.max_traces` として公開しました。この制限は `emqx ctl trace` で管理されるノードローカルトレースには適用されません。

  作成されたトレースごとの潜在的なアトムリークを排除するために、トレース実装を最適化しました。

- [#15556](https://github.com/emqx/emqx/pull/15556) 個別の各トレースの最大トレースファイルサイズ制限を設定オプション `trace.max_file_size` として公開しました。

- [#15364](https://github.com/emqx/emqx/pull/15364) HTTP 認証付きコレクターに適応するため、OpenTelemetry 統合に HTTP ヘッダー設定項目を追加しました。

#### デプロイメント

- [#15484](https://github.com/emqx/emqx/pull/15484) すべてのパッケージが [Elixir](https://elixir-lang.org/) の [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html) でビルドされるようにビルドシステムを変更したため、すべてのパッケージが Elixir サポートを提供するようになりました。これにより EMQX が Elixir コミュニティに開放され、必要に応じて Elixir 依存関係を使用できるようになり、[IEx](https://hexdocs.pm/iex/IEx.html) をより良い EMQX コンソールとして使用できるようになります。

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` が現在のシステム設定を機密情報を編集した HOCON フォーマットでエクスポートするようになりました。

#### クラスタリング

- [#15600](https://github.com/emqx/emqx/pull/15600) EMQX クラスターに説明を追加できる新しい `cluster.description` 設定を追加しました。この説明は `PUT /cluster` で変更でき、`GET /cluster` レスポンスで表示できます。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval` 設定をデフォルトで無効にしました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VM パラメータを最適化しました。

  - 集約的な Mnesia 操作中に `busy_dist_port` アラームを避けるため、分散チャネルのバッファサイズを 32MB に増やしました：`+zdbbl 32768`
  - オペレーティングシステムが観測する CPU 使用率を削減するため、スケジューラのビジーウェイトを無効にしました：`+sbwt none +sbwtdcpu none +sbwtdio none`
  - メッセージ遅延を削減するため、スケジューラバインドタイプを `db` に設定しました：`+stbt db`

- [#15451](https://github.com/emqx/emqx/pull/15451) TCP リスナー用の実験的な `socket` バックエンドを導入し、メッセージ処理遅延の改善と計算リソース使用量の削減を図りました。これは新しい `tcp_backend` リスナーオプションで有効にできます。

### バグ修正

#### データ統合

- [#15647](https://github.com/emqx/emqx/pull/15647) 以前は、MongoDB コネクタのユーザーが `foo` コレクションで `find` クエリを実行する十分な権限を持っていない場合、切断されているとみなされていました。これが修正されました。

- [#15603](https://github.com/emqx/emqx/pull/15603) MQTT ブリッジで古い接続が「接続済み」と表示され、接続が再確立されない問題を修正しました。


- [#15522](https://github.com/emqx/emqx/pull/15522) `username` が提供されていない場合に Snowflake コネクタが正しく開始できない問題を修正しました。

- [#15476](https://github.com/emqx/emqx/pull/15476) 集約モードを使用するアクション（Azure Blob Storage、Snowflake、S3Tables）のほとんどで配信が失敗した場合、以下のログが出力されていました：

  ```
  "emqx_connector_aggreg_delivery:format_status/1 crashed"
  ```

  これが修正され、配信プロセスに関するより多くの情報がログに記録されるようになりました。

- [#15394](https://github.com/emqx/emqx/pull/15394) アクションメトリクスが不整合な状態になる非常にまれな競合状態を修正しました。

- [#15383](https://github.com/emqx/emqx/pull/15383) ブリッジの開始に失敗した際の MQTT ブリッジでの潜在的なリソースリークを修正しました。以前は、ブリッジの開始に失敗した際にトピックインデックステーブルが適切にクリーンアップされていませんでした。

#### ルールエンジン

- [#15569](https://github.com/emqx/emqx/pull/15569) `direct_dispatch` のテンプレートが空文字列または非ブール値に解決される場合、再パブリッシュルールアクションが失敗する可能性がある問題を修正しました。このような状況が発生した場合、デフォルト値 `false` が使用されます。

#### コア MQTT 機能

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、ルーティングテーブルとクラスター内の共有サブスクリプション状態で累積的な不整合が生じる可能性がある競合状態を解決しました。

- [#15416](https://github.com/emqx/emqx/pull/15416) 最近の WebSocket パフォーマンス改善によって導入された WebSocket 接続のセッション満了中の時折発生する警告レベルのログイベントとクラッシュを修正しました。これらはブローカー容量に影響はありませんでしたが、以下のようなログエントリを生成していました：
  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,"gen_tcp.erl"},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,"cowboy_websocket_linger.erl"},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15396](https://github.com/emqx/emqx/pull/15396) 高い切断量の下でクラッシュしやすく、グローバルブローカー状態の潜在的な不整合を引き起こしていた、切断されたクライアントの共有サブスクリプションに対する冗長なクリーンアップ操作を削除しました。

- [#15361](https://github.com/emqx/emqx/pull/15361) ペア長が間違っている（短すぎる）不正な形式の `User-Property` ペアを解析する際の関数句エラーを修正しました。

#### アクセス制御

- [#15489](https://github.com/emqx/emqx/pull/15489) OIDC 発行者スキーム検証を修正しました。

  以前はサポートされていなかった以下の発行者 URL がサポートされるようになりました。

  - `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`
  - `hostname`

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) clientinfo オーバーライドテンプレートに未定義のパケットフィールドが含まれている場合の NATS ゲートウェイクラッシュを修正し、未定義アトムの代わりに空のバイナリを返すようにしました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) OTP バージョンを 26.2.5.2 から 26.2.5.14 にアップグレードしました

  このアップグレードには、EMQX に関連する 2 つの TLS 関連の修正が含まれています：

  - 証明書更新中の競合状態によって引き起こされる TLS 接続のクラッシュを修正しました。
  - PSS パラメータで署名された RSA 証明書のサポートを追加しました。以前は TLS ハンドシェイクが `invalid_signature` で失敗する可能性がありました。

#### デプロイメント

- [#15580](https://github.com/emqx/emqx/pull/15580) EMQX Enterprise helm チャートに emqxLicenseSecretRef 変数を追加し、EMQX ライセンスキーを含む Kubernetes シークレットをユーザーが指定できるようにしました。これにより、機能しない emqxLicenseSecretName 変数の問題が修正されます。

- [#15553](https://github.com/emqx/emqx/pull/15553) チャートがデフォルト値でデプロイされた場合に、1 つを除くすべてのノードがクラッシュする helm チャートの問題を修正しました。

#### HTTP サーバー

- [#15547](https://github.com/emqx/emqx/pull/15547) 大きなボディを持つ HTTP リクエストが送信された際のエラーを修正しました。