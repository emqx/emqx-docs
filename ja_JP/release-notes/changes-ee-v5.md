# EMQX Enterprise Version 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboardのユーザーアカウント管理エンドポイントへのAPIキーアクセスを制限しました。

  以前は、`administrator`ロールを持つAPIキーがHTTP Basic認証を介してDashboardのユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出すことが可能でした。これにより、APIキーが他のDashboardユーザーのMFAをリセットまたは無効化したり、パスワードを変更したりでき、意図された人間のDashboardセッションと機械のAPIキーの分離を回避できていました。

  これらのエンドポイントは現在、APIキー経由でアクセスすると `401 API_KEY_NOT_ALLOW` を返し、既に `/users`、`/users/:username`、`/logout`、`/api_key` へのAPIキーアクセスをブロックしているポリシーに合わせています。Dashboardユーザーは引き続きDashboard UIからベアラートークン（JWT）セッションを使って自身のMFAやパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイント経由でのDashboardアカウントおよびAPIキーのエクスポート・インポートをAPIキーから制限しました。

  APIキーで呼び出された `POST /data/export` は、生成されるアーカイブから `dashboard_users` と `api_keys` のmnesiaテーブルセットを静かに省略します。APIキーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれている場合、`403 FORBIDDEN` を返します。

  Dashboardのベアラートークン（ログイン）呼び出しは影響を受けず、DashboardユーザーとAPIキーを含む完全なデータベースのバックアップと復元が継続して可能です。

  これは権限昇格のギャップを解消します。既存の `/users` および `/api_key` エンドポイントはAPIキーによるDashboardログイン資格情報とAPIキー記録へのアクセスを拒否していますが、APIキー保有者はデータバックアップエンドポイントを経由してこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証されていない `GET /status?format=json` レスポンスからEMQXリリースバージョン（`rel_vsn`）を削除し、認証されていない呼び出し元にブローカーのバージョンを開示しないようにしました。バージョン情報は認証済みのノード情報API経由で引き続き利用可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされたtarballのパス・トラバーサルに対するプラグインインストールエンドポイントを強化しました。インストールパスは、プラグインインストールディレクトリ外に解決されるエントリを含むtarballの展開を拒否します。

  これは多層防御の一環です。このエンドポイントは既にDashboardログイン/APIキー認証と明示的な `emqx ctl plugins allow <name-vsn>` 許可リストエントリによって制御されており、認証されていないまたは権限のない呼び出し元がこのコードパスに到達できませんでした。新しいチェックは両方のゲートが意図的に開かれてプラグインをアップロードする場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install`（およびそれをラップするDashboardアップロード）経由のプラグインインストール成功時に、アップロードを許可したクラスタ全体の `emqx ctl plugins allow <name-vsn>` エントリを即座に取り消すようにしました。同じ許可を別の（潜在的に異なる）tarballに再利用できなくなります。5分間のTTLは引き続き適用されますが、この変更により一般的なパスでの許可期間が短縮されます。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトのプラグインパッケージに `.sha256` チェックサムサイドカーを公開し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式EMQX Dockerイメージを強化し、イメージスキャナーの指摘を解消しました：

  - ランタイムイメージビルド時にDebianのセキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取り込みました。
  - 未使用の `libgnutls30t64` パッケージを削除しました。EMQXはErlang/OTPを通じてOpenSSLでTLSを扱い、GnuTLSはリンクしないため、`curl` の推移的依存関係としてのみ存在し、スキャナー報告に現れていました。
  - Debianの `curl` パッケージを、[stunnel/static-curl](https://github.com/stunnel/static-curl) からの静的リンク済みバイナリ（OpenSSL、HTTP/2、HTTP/3対応、RTMP・GnuTLS非対応）に置き換えました。Debianパッケージは `librtmp1` 経由で `libgnutls30t64` を再導入していましたが、静的バイナリはこれを回避しつつ、`curl` を呼ぶコンテナのヘルスチェックは変更なく動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2のSSL Common Name / Subjectをクライアント識別に取り込む前にサニタイズしました。

  リスナーが `proxy_protocol = true` に設定されている場合、PROXY-ProtocolのSSL TLVバイト列にASCII制御文字が含まれている接続をブローカーは拒否します（これは既にMQTTで取り込む `clientid`、`username`、`password` に対しても適用されているバイトクラスと同じです）。これにより、攻撃者制御のバイトが `${cert_common_name}` や `${cert_subject}` テンプレートを介してHTTP認証、認可、ルールエンジンのヘッダー値に密輸されることを防ぎます。

  追加の防御層として、HTTP認証および認可クライアントは、レンダリングされたヘッダー名または値にCR、LF、NULバイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTTの `clientid` / `username` / `password` に適用されているバイトクラスチェックを、`ClientInfo` とHTTPリクエストテンプレートに供給される他のフィールドに拡張しました：

  - `peersni`（TLS Server Name Indication。PROXY-Protocol v2の `authority` TLVからも受け入れ）は接続取り込み境界で検証されます。制御文字があると接続が拒否され、警告がログに記録されます。
  - `mqtt.client_attrs_init` Variform式で生成されたクライアント属性値は制御文字を含む場合、警告付きで破棄されます。これにより `${client_attrs.tns}` のようなテンプレートが注入バイトを下流に運べなくなります。
  - HTTPアクション / ブリッジコネクターのヘッダー描画は、レンダリングされた名前または値にNUL、CR、LFが含まれるヘッダーを破棄します。

#### クラスタリング

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期機構を導入しました。ルーティングテーブルのスキーマバージョンは `v3` にステップアップし、`v2` との後方互換性も提供します。

  スキーマv3では、各ノード（コアまたはレプリカント）が自身に向かうルーティングテーブルエントリの完全な所有権を持ち、ピアノードはこれらのエントリに対して読み取り専用アクセスのみを持ちます。これにより、パーティション耐性が向上し、パーティション化されたクラスタ内のピアノードが他ノードの代わりにルーティングテーブルを変更できなくなります。また、レプリカントノードの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3対応ノードがv2のみ対応のクラスタに参加すると、互換性のためにv2を使い続けます。既存ノードのいずれかが互換モードの場合、新規ノードも互換モードを使います。クラスタをv3に切り替えるには、アップグレード後にクラスタ全体を再起動してください。自動切り替えを防ぐには、`broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスタがv3に切り替わった後はローリングダウングレードは不可能です。

  ノードの現在のルーティングスキーマバージョンを確認するには：

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) 分散ポートのErlang inetポートオプション設定をサポートし、デフォルトのバッファサイズを1MBに設定しました。

  以前は、Erlang分散ポートは非常に小さいデフォルトのポートバッファ（1460バイト、一部プラットフォームでは約9KB）を使っており、分散ポートバッファ（`+zdbbl`）を大きく設定しても（例：32MB）、パフォーマンスボトルネックが発生していました。これによりクラスタ通信の信頼性が低下し、`erpc timeout` エラー、Mnesiaトランザクションの混雑、多コアノードサポートの劣化が見られました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) ノードごとのルートテーブルエントリ数をエクスポートする `emqx_routes_count` と `emqx_routes_max` のPrometheusメトリクスを追加しました。EMQX v4の `emqx_routes_count` メトリクスに類似しています。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon` をデフォルトでシステム全体のメモリ統計のみ収集するよう設定し、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Prometheusメトリクス収集のオーバーヘッドを、Mria統計の誤った重複クエリを避けることで削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報をPrometheusゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスタ全体のライセンス整合性をノードごとのCLIチェックなしで監視可能にしました。

  タイムスタンプはライセンスの発行日・有効期限日のUTC深夜のUnixエポック秒です。ライセンスが利用不可の場合は3つのメトリクスすべて0を出力します。アラートルールでは `emqx_license_expiry_at == 0` を「利用不可」シグナルとして使ってください（`max_sessions == 0` はトライアルライセンスの期限切れも示す可能性があります）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSONデータおよびJWTトークンからドット区切りのキー経路で値を抽出する2つの新しいVariform式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  `json_value` はJSONバイナリ文字列からドット区切りパスでネスト構造を辿って値を抽出します。`jwt_value` はJWTトークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例えば、`username` がJSONオブジェクトの場合、`json_value(username, 'shop.floor')` でフィールドにアクセスできます。`password` がカスタムクレームを持つJWTの場合、`jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセスできます。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) APIキーとDashboardログインユーザーのスコープベースの細粒度アクセス制御を導入しました。

  APIキーはOpenAPIタグ由来のスコープで特定のAPIパスカテゴリに制限可能です。スコープなしのキーは完全アクセス（後方互換）を維持し、空のスコープリストはすべてのスコープ付きパスを拒否します。`publisher` APIキーのロールは現在 `[publish]` のみ許可されます。

  Dashboardログインユーザーは既存のロールチェックに加え、任意の `scopes` フィールドを持ちます。Dashboard専用エンドポイントをカバーする4つの新スコープ：`user_management`、`sso_management`、`api_key_management` は管理者専用、`mfa_management` は強制MFA免除のため任意のロールが利用可能です。APIキーはこれらのログイン専用スコープを持てません。

  2つの新しいカタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` はベアラー認証済み呼び出し元なら誰でもアクセス可能です。`scopes` フィールドは `GET /users`、`POST /users`、`PUT /users/:username` のレスポンスにも含まれ、明示的に設定されていない場合はロールデフォルトのスコープリストを返します。

  振る舞いの変更点：

  - `dashboard.default_username` ユーザーはブレイクグラスアカウントとして保護されます。削除、管理者からの降格、`scopes` フィールド設定はできず、`description` のみ変更可能です。これにより他の管理者がスコープを失った場合でもオペレーターが管理者アクセスを保持できます。
  - ユーザー自身のレコードに対するセルフサービスはスコープを尊重します。パスワード変更とMFAセルフエンドポイントのみスコープチェックをバイパスし、`PUT /users/:self` などその他の操作はユーザースコープの対象です。
  - `PUT /users/:username` と `PUT /api_key/:name` は、リクエストボディに `scopes` フィールドがない場合、永続化されたスコープに対してロール変更を検証します。ユーザーの降格やAPIキーのロール変更は、永続化されたスコープと互換性がなければ拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）用のバックエンドごとの `force_mfa` オプションを追加しました。

  有効にすると、SSOユーザーはIDP側MFA設定に関わらずDashboardトークン取得前にTOTP MFAセットアップまたは検証を完了する必要があります。3つのMFA状態をサポート：`not_configured`（セットアップ強制）、`enabled`（検証必須）、`admin_disabled`（MFAスキップ）。新APIエンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` がMFAフローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行後5分で期限切れとなり、パッケージのSHA-256ハッシュにピン留め可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>` は64文字の小文字16進ダイジェストを受け入れ、内容がハッシュと一致しないアップロードを `403 Forbidden` で拒否します。`sha256:` 引数を省略した場合は `<name-vsn>.tar.gz` という名前の任意ペイロードを受け入れる従来の動作を維持します。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808ゲートウェイのダウンリンクメッセージにカスタム `msg_sn` をサポートしました。

  ダウンリンクMQTTメッセージペイロードのヘッダーに `msg_sn` フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケースのメッセージシーケンスを制御可能です。

  また、JT/T 808ゲートウェイの `string_encoding` がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。以前は `string_encoding`（例：`gbk`）はアップリンクメッセージ解析にのみ使われていましたが、現在は `string_encoding: gbk` 設定時にアップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）が正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafkaソースのポーリング動作を改善し、レコードがない場合に空バッチを即座に返すのではなく、データを待つために短時間待機するようにしました。これにより不要なポーリング遅延が減り、Kafkaコンシューマーが新規レコードをより安定して受信できます。

- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erlを1.1.13から1.1.18にアップグレードし、InfluxDBコネクターに `ping_with_auth` オプション（デフォルトfalse）を追加しました。これにより、一部のInfluxDB互換サービスで認証情報を含むヘルスチェックが可能になります。

#### デプロイメント

- [#16853](https://github.com/emqx/emqx/pull/16853) v5ライセンスパーサーをv6ライセンスキーに対してフォワード互換にしました。

### バグ修正

#### コアMQTT機能

- [#17097](https://github.com/emqx/emqx/pull/17097) `retainer.enable` を実際のランタイムスイッチとして復元しました。これにより、`mqtt.retain_available` に依存せずにMQTTのリテインドメッセージプロトコルサポートを有効にしつつ、リテインドメッセージの保存を無効化できます。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッションテイクオーバーや破棄時に `disconnected_at` が `connected_at` より後になるタイムスタンプ順序の問題を修正しました。

  以前は `disconnected_at` が新セッションの `connected_at` 設定後の `ensure_disconnected` で遅れて記録されていました。これにより `disconnected_at > connected_at` となり、外部のクライアントプレゼンス状態追跡が困難でした。

  修正では、テイクオーバー開始時または破棄受信時に即座に `disconnected_at` を記録し、新セッションの `connected_at` より遅れないようにしました。これにより外部システムでのタイムスタンプ順序が正しくなります。

  注：これらのイベントが異なるクラスタノードから発せられる場合、観測される順序はノード間の時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  以前は一部クライアントでサブスクリプション一覧取得が失敗し、出力がありませんでした。現在は通常および共有サブスクリプションの両方で確実に動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) DashboardおよびREST APIで反映されるチャネル情報（`mqueue_len`、`inflight_cnt`）が、セッションテイクオーバーのリプレイ完了直後に即時更新されるよう修正しました。以前は15秒ごとの統計更新タイミングまで遅延していました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) `$events/client/connack` ルールイベントにドキュメントにはあるが実際のイベントデータに欠落していた `connected_at` フィールドを追加しました。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なルールメタデータのタイムスタンプを無視するようにしました。

  以前は、非整数の `metadata.created_at` や `metadata.last_modified_at`（例：日付文字列）が含まれるルールをAPI経由で取得・一覧表示すると内部エラーで失敗することがありました。

  現在は無効なメタデータタイムスタンプを無視し、通常の生成タイムスタンプにフォールバックするため、破損したメタデータがあってもルールAPIレスポンスが利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQコネクター/アクション/ソースで、接続やチャネルプロセスが予期せず終了した場合に再起動せずに切断状態のままになる問題を修正しました。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定インポート時のクラッシュを修正しました。

  以前は大量インポート時に以下のようなクラッシュメッセージで失敗することがありました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) 集約モードのAzure Blob Storageアクションのヘルスチェックが、コンテナ内のBlob数が多すぎるとタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTPおよびGCP PubSubアクションで、理由が `closing` の一時的な接続エラーを回復可能として扱い、ログノイズを削減しました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTTソースで、コネクターが `clean_start = false` でセッションにメッセージがあるブローカーに再接続した場合、メッセージがルールアクションをトリガーしない問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDBコネクター/アクションで、`write_syntax` リテラルやMQTTペイロードからの値を書き込む際にUnicodeテキストが保持されるよう修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQLコネクターでプリペアドステートメント無効時のクエリ実行を修正しました。以前は並行クエリが混在しエラーを引き起こすことがありました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQコネクターの分離を修正しました。誤設定や到達不能なRocketMQコネクターが同一ノード上の他のRocketMQコネクターを不安定化させなくなりました。以前は1つのコネクターの到達不能が共有クライアントスーパーバイザーを最大60秒間停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard操作がハングしていました。

  TCP/TLS接続タイムアウトのデフォルトも60秒から10秒に短縮され、誤設定サーバーが速やかに失敗として検出されるようになりました。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時にMongoDBプロセスへのタイムアウト呼び出しが回復不能エラーとして扱われ再試行されなかった問題を修正しました。該当イベントではメッセージが再試行されます。

  発生時のログ例：

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis Sentinelコネクターで、RedisデータノードとSentinelノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquetファイルへの書き込みで、必須キーが `undefined` または `null` に設定されたオブジェクトがある場合、エラーを発生させず破損ファイルを生成していた問題を修正しました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafkaクライアントライブラリを `brod` 4.5.2 → 4.5.4、`wolff` 4.1.7 → 4.1.10 にアップグレードしました。

  Kafkaプロデューサー・コンシューマー統合に以下の修正をもたらします：

  - SASL再認証中の接続競合状態を修正し、キューイングされたプロデュース要求のドロップや `sync` プロデュース呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQクライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサー要求のメモリ増加問題を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTTクライアント依存を `1.14.6` から `1.15.1` にアップグレードしました。

  これにより、MQTTブリッジ、MQTTソース、その他アウトバウンドMQTT接続を使うコネクターに以下のユーザー向け改善がもたらされます：

  - キープアライブタイマーからpingrespタイムアウトをトラックし、pingresp処理が設定された `keepalive` 間隔と整合するようにしました。
  - QUICでピアの `recv` 中止後、両方向を破棄するのではなく送信方向のみ中止し、半クローズされたQUICストリームの保留送信が静かにドロップされなくなりました。

#### クラスタリング

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスタ回復時間を改善しました。

  内蔵Mriaデータベース管理システムがトランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTPを27.3.4.2-6から27.3.4.2-7にアップグレードしました。

  これにより、ノードの起動中にネットワークパーティションが発生した場合のMQTTルーティングテーブル不整合の競合状態が解消されます。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTPを27.3.4.2-8にアップグレードしました。この修正がないと、ノードがクラスタに接続していない場合にMriaアプリの起動がEMQX起動時にハングする可能性があります。

- [#17220](https://github.com/emqx/emqx/pull/17220) `bin/emqx` と `bin/emqx_ctl` の呼び出しが稼働中ブローカーの `nodeup`/`nodedown` イベントをトリガーし、ブローカーログに誤解を招く `cm_registry_node_down` 警告を出していた問題を修正しました。これらスクリプトで起動される一時的なヘルパーノードは、意図通り隠しErlangノードとして登録されるようになりました。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスタ回復を改善しました。

  以前は、レプリカントノードに接続されたクライアントの一部がグローバルレジストリから失われ、セッションテイクオーバー時の不整合やDashboardに誤情報が表示されていました。

  この修正では、ネットワークパーティションが解消された際に既存クライアントを再登録するバックグラウンドプロセスを追加しました。また、グローバルレジストリ再構築中に「Broker is recovering after a network partition」という新しいアラームを発生させます。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動回復できる新しい自動修復アルゴリズムを導入し、ネットワークパーティションからのクラスタ回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれている場合、`required_field: node.cookie` スキーマチェックエラーでクラスタ設定インポートが失敗する問題を修正しました。読み取り専用の設定ルート（`node`、`rpc`）は事前チェック前に破棄され、実行中ノードの値が検証に使われます。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスタノード間で実効設定が同じでも生の設定表現が異なる場合に発生する騒々しく誤解を招く `emqx ctl conf cluster_sync status` 診断を修正しました。

  実際の設定変更に対応しない生の表現差異を抑制しつつ、実効設定が不整合な場合は警告を出します。また、一方のノードにのみ存在する生設定キーがある場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスタがネットワークパーティションを経験した際に発生する可能性があるグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く `emqx ctl conf cluster_sync status` 警告を修正しました。

  以前は、データインポートや起動時設定読み込みで、同一のアクション、ソース、ブリッジ、ルールメタデータであっても `created_at` や `last_modified_at` がノード間で異なる場合がありました。コマンドはこれらのタイムスタンプ差異を無視し、実際の設定差異のみ報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) ルートレプリケーションが応答しないターゲットクラスタへの接続でスタックした際のCluster Link応答性を改善しました。このようなCluster Linkの削除がより早く完了します。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後に同一クライアントIDの重複または古いグローバルセッションレジストリエントリが残るリークを修正しました。

  廃棄およびテイクオーバーキックRPCハンドラは、対象プロセスが生存しない場合もレジストリ行を削除し、接続パスの登録スロットルはトゥームストーン行（ローカルチャネル状態なし）を認識して再利用をブロックせずにこれらを回収します。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) CRLキャッシュの回帰を修正しました。`emqx_crl_cache:evict/1` が内部URL状態を完全にクリアしていませんでした。削除後、同じCRL URLは次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとのHTTP取得の繰り返しを回避します。

- [#17012](https://github.com/emqx/emqx/pull/17012) パスワードなしのCONNECTパケット時にパスワードベース認証バックエンドがチェーンを継続するよう修正しました。以前はパスワードなし接続時に最初のパスワード認証器がエラーを返し、後続認証器が試行されませんでした。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSOログインで、IDプロバイダーが `+json` 構造化シンタックスサフィックスを持つ `Content-Type`（例：`application/jwk-set+json; charset=utf-8`）のJWKSレスポンスを返す場合に `provider_not_ready` で失敗する問題を修正しました。これらのレスポンスは有効なJWKSコンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URLエンコードされたユーザー名（例：メールアドレス）を持つSSOユーザーのDashboard RBACチェックを修正し、`force_mfa` 無効時のビューア自身によるMFA無効化リクエストが正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PIDがログデータフィールドとして出力されるようにしました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 以前は特定の競合状態で以下のような長く難解なログが出力されていました：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQXはより意味のある情報をログに出力し、問題のデバッグを支援します。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開時のプラグイン起動警告のノイズを削減しました。

  クラスタ設定同期時にEMQXはローカルノードからプラグイン設定を取得しようとしなくなり、起動時の `config_not_found_on_node` 警告が繰り返されるのを防ぎます。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答を受信した際に警告ログを追加しました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が `emsgsize`（受信パケットが `mqtt.max_packet_size` を超過）でクライアント接続終了時、ログレベルを情報から警告に変更しました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用報告を改善しました。

  ブローカーはcgroup v2、cgroup v1、ホストの `/proc/meminfo` のメモリ読み取り値を比較し、最も制約の厳しい値を使用します。非ゼロの最小合計値が勝ち、合計が同じ場合は使用率の大きい方が勝ちます。

  これにより以下の誤解を招く読み取りを修正します：

  - コンテナのcgroupメモリ制限が厳しい場合にホストビューが高使用率を示すがcgroup制限は低い（またはその逆）
  - メモリ制限なしでマウントされたcgroupで報告使用率が約0%に収束する

  過負荷保護の閾値と `Memory used` メトリクスは実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace` が `ruleid` をトレースフィルタータイプとして受け付けるよう修正しました。以前は `emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する `trace add ...`）がCLI引数パーサに `ruleid` フィルターがなく汎用エラーになっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### デプロイメント

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) `GET /monitor_current` HTTP APIに新たに2つのメトリクス `rules_matched` と `actions_executed` および対応するレートを追加しました。これらは一致したルール数とアクション実行率（成功＋失敗）を追跡します。
- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQXはTLS 1.3のステートレスセッションチケットを使ったセッション再開をサポートしました。これによりクライアントはサーバー側のセッション状態を保存せずにTLSセッションを再開できます。

  **設定**

  - **ノードレベル**: `node.tls_stateless_tickets_seed`

    TLS 1.3ステートレスセッションチケット生成に使う秘密鍵シード。

  - **リスナーレベル**: `listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - `node.tls_stateless_tickets_seed` が設定（空でない）され、リスナーSSLオプションで `session_tickets` が有効な場合のみチケットが生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、チケットは生成されず、リスナー起動時にエラーログが出力されます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808ゲートウェイに `jt808.frame.parse_unknown_message` 設定オプションを追加し、未知のメッセージIDのメッセージを解析して透過的に転送可能にしました。
- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808プロトコル2019をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合でIoTDBテーブルモデルをサポートしました。

### バグ修正

#### コアMQTT機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエスト-レスポンス情報プロパティ処理時の型不一致によるMQTT v5接続のクラッシュを修正しました。
- [#16514](https://github.com/emqx/emqx/pull/16514) ブローカーメッセージがクライアントの広告する `Maximum-Packet-Size` を超える場合にWebSocket接続がクラッシュするバグを修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に `undefined` を返していた問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これはEMQX v4の後方互換性修正です。これらの関数はEMQX v5以降ではドキュメント化されていません。推奨される使い方はルール評価コンテキストのフィールドを直接参照することです。例：`SELECT clientid ...`（`SELECT clientid()`ではなく）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックで現在のEMQXノードに割り当てられたパーティションのみのリーダー接続を検証するようにし、不要なアイドル接続と誤警報を防止しました。

  以前はKafkaコンシューマコネクターがすべてのパーティションのリーダー接続を検証していました。クラスタ展開では各ノードがパーティションのサブセットのみを所有し、割り当てられていないパーティションリーダーへの接続がアイドル状態になります。Kafkaはアイドル接続をタイムアウト（デフォルト10分）後に切断するため、誤った接続警報が発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafkaリクエストタイムアウトをメタデータリクエストタイムアウトの2倍以上（最小30秒）に自動設定するようにし、メタデータリクエストが長引く場合の不要な再接続やリトライを削減しました。特にメタデータリクエストタイムアウトが小さい値に設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続テストやコネクター停止時のタイムアウトを引き起こす競合状態を修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) REST APIドライバーを使うIoTDBコネクターのヘルスチェックでクライアント認証情報が検証されていなかった問題を修正しました。ヘルスチェックで軽量なno-opクエリを送信し、誤設定の認証情報を早期に検出可能にしました。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsarクライアントを2.1.2にアップグレードしました。

  Pulsarプロデューサーアクションの `batch_size` が `1` に設定されている場合、単一メッセージを単一要素バッチではなくエンコードします。これによりKey Share戦略を使ったコンシューマーの負荷分散が可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTTソースのコネクターが再接続した際にトピックが再サブスクライブされず、コネクター再起動までソースが動作停止する問題を修正しました。現在は再接続時に自動的に再サブスクライブします。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS接続失敗の問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードを使うアクションで、複数回のヘルスチェック失敗後にコネクターが切断されるとフォールバックアクションが2回トリガーされる問題を修正しました。

#### クラスタリング

- [#16269](https://github.com/emqx/emqx/pull/16269) リモート側が再ブートストラップを必要としているのに誤ってスキップしていたCluster Linkルートレプリケーションプロトコルの回復シーケンスの問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立したCluster Linkが設定され、一部のリンクが長期間ダウンした場合に、古いルートレプリケーション状態のクリーンアップ中に内部ルーティングテーブルから生きたルートが誤って削除される問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc` を `3.5.1` にアップグレードしました。

  以前のバージョンでは、ピアノードが到達不能な場合に接続タイムアウトによる長いクラッシュログのテールが発生していました。新バージョンは長いテールをなくし、クラッシュログをより読みやすい `error` ログに変換し、頻繁な `"failed_to_connect_server"` ログもスロットリングしてログスパムを防ぎます。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスタ自動クリーン手順の堅牢性を改善しました。以前はノード起動時に自動クリーン機能を無効にすると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSOバックエンドに `idp_signs_envelopes` と `idp_signs_assertions` オプションを追加し、署名検証を制御可能にしました。これらは以前正しく機能していませんでした。両方のオプションは後方互換のためデフォルト `false` で、IdPがSAMLレスポンスに署名する場合は明示的に有効化が必要です。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0未満からアップグレード後にMulti-Factor Authentication（MFA）が有効化できなかった問題を修正しました。これはログインユーザーデータベースレコードの互換性問題によるものです。

- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC発行者URLが保存時に末尾スラッシュ付きに自動正規化され、OIDCプロバイダーのディスカバリードキュメントが末尾なし発行者を返す場合に不一致エラーとなる問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) リソース例外発生時のログ量を削減しました。これらのログはスロットリングされ、一部の大きな項目はマスクされます。
- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc` エラーのログフォーマッタークラッシュを修正しました。以前は `gen_rpc` が特定のエラーメッセージ（例：送信タイムアウト）をログ出力するとEMQXがクラッシュしていました。フォーマッターはこれらのエラーを正しく処理するようになりました。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) CANバスIDパラメータ（0x0110〜0x01FF）に対し、JT/T 808ゲートウェイのパラメータ設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正しました。これらはJSONで文字列型ではなくBYTE[8]型のbase64エンコードを使うべきです。

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS上の接続モードで動作するCoAPゲートウェイを修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808ゲートウェイにGBK文字エンコーディングサポートを追加しました。

  JT/T 808プロトコルはSTRING型フィールドにGBKエンコーディングを指定しています。新しい `frame.string_encoding` 設定オプション：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）
  - `gbk`：デバイスからのGBK文字列をUTF-8に変換しMQTTに渡し、MQTTからのUTF-8文字列をGBKに変換してデバイスに送信

  対象はナンバープレート、運転手名、テキストメッセージ、エリア名、クライアントパラメータなどの文字列フィールドです。MQTTペイロードはこの設定に関わらず常にUTF-8です。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージの破棄に関するログ（`buffer_worker_dropped_expired_messages`）を警告レベルにし、リソースIDごとにスロットリングするようにしました。これにより特定の外部リソースがメッセージ受信速度に追いついていないことを識別しやすくなります。
- [#16206](https://github.com/emqx/emqx/pull/16206) Kafka Producerコネクターに `allow_auto_topic_creation` 設定オプションを追加しました。有効にすると、クライアントがメタデータフェッチリクエストを送信した際にトピックが存在しなければKafkaが自動生成を許可します。
- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDBコネクターにカスタムタイムスタンプカラム名（`ts_column`）パラメータ指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` のデフォルト値を `chunk` から `frame` に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト4KB）を超える場合のCPU使用率が大幅に削減されます。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQXは `DISCONNECT` パケットを送信せずに接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` APIのパフォーマンスを最適化しました。クラスタに約5万クライアント以上存在する場合、クライアントリスト取得API呼び出しが非常に遅くなるかタイムアウトすることがありました。

### バグ修正

#### コアMQTT機能

- [#15884](https://github.com/emqx/emqx/pull/15884) ごく稀に、グローバルルーティングテーブルが長期間クラスタを離れたノードのルーティング情報を無期限に保持する問題を修正しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスタ内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積される競合状態を修正しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使うクライアントが `session_stepdown_request_exception` と `calling_self` エラーでクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` の組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) ルールのSQLにルール環境の `metadata` フィールドが含まれていない場合、Republishフォールバックアクションが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafkaデータ統合で `not_all_kafka_partitions_connected` イベント発生時のログ詳細を改善しました。
- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを含むコネクター設定のロードや再起動時に発生する可能性のあるOOMクラッシュを修正しました。
- [#16138](https://github.com/emqx/emqx/pull/16138) Redisクラスタのフェイルオーバー問題を修正しました。以前は定期的な `PING` コマンドの失敗がトポロジー更新をトリガーせず、フェイルオーバー後に古いトポロジーを使い続けて回復できませんでした。修正後は失敗した `PING` 応答がトポロジートポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して迅速に回復します。

#### 可観測性

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価時に生成される過剰な監査ログを削減しました。
- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログのクリーンアップ中にMnesiaトランザクションがブロックされ急速なメモリ増加を引き起こす問題を修正しました。

## 5.10.1

*リリース日: 2025-09-18*

EMQX 5.10.1 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### パフォーマンス

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアするようにし、不要なメモリ消費を削減しました。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。クライアントID、ユーザー名、パスワード、トピックなどのフィールドは、生パケットのスライスではなく新しいバイナリにコピーされ（64バイト超の場合）、Erlang VMのバイナリメモリ使用量を削減します。

#### アクセス制御

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP認証・認可を強化しました。

  LDAP認可は既存の単純なトピックリストに加え、JSONを使った拡張ACLルール形式をサポートしました。認証時にLDAPからACLルールを取得し、クライアントのメタデータにキャッシュして認可時のLDAP問い合わせを削減します。

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証・認可の外部リソース管理を最適化しました。以前は無効化された認証・認可プロバイダーに設定されたリソースに接続し続けることがありました。

#### データ統合

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 TablesアクションでParquet形式のデータファイル書き込みをサポートしました。

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producerコネクターとアクションのヘルスチェックにレート制限を追加し、AWS APIクォータに準拠しクラスタ動作を改善しました。

  - `ListStreams` と `DescribeStream` へのヘルスチェック呼び出しをそれぞれコネクター単位で5/s、10/sに制限しました。
  - クラスタ内のコアノードが調整する分散リミッターで一貫してこれらの制限を適用します。
  - ヘルスチェックがスロットリングまたはタイムアウトした場合、コネクターやアクションは切断状態にせず以前の状態を保持します。

  また、同じコネクター下の複数アクションが同時にヘルスチェックを実行する可能性を減らすため、`resource_opts.health_check_interval_jitter` を追加し、`resource_opts.health_check_interval` に一様ランダム遅延を加えます。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを `3.8.3.0` にアップグレードしました。これにより、EC2インスタンスが適切なIAM権限を持つ場合、アクセスキーIDとシークレットアクセスキーを指定せずにS3コネクターを設定可能になりました。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTTコネクターの `static_clientids` 設定で、クライアントIDごとにユーザー名とパスワードを指定可能になりました。これはAzure IoT Hubのように各デバイス（クライアントID）に固有の認証情報が必要なシナリオで有用です。クラスタ環境で複数ノード間の接続成功を支援します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTPアクションのHTTPリクエストタイムアウトを `resource_opts.request_ttl` 設定で調整可能にしました。以前は30秒固定で変更できませんでした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) アクティブなアラームを管理者が強制的に非アクティブ化できるAPIエンドポイントを追加しました。
- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry統合にHTTP認証が必要なコレクターに対応するため、カスタムHTTPヘッダー設定項目を追加しました。
- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS認証のリソースが `disconnected` とマークされた際に返される情報を改善しました。
- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary` と `GET /sources_summary` エンドポイントのレスポンスおよび `GET /actions/:id` のフォールバックアクションに `tags` フィールドを追加しました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールが現在のシステム設定をHOCON形式でエクスポートし、パスワードやシークレットなどの機密情報を自動的にマスクするようになりました。

### バグ修正

#### コアMQTT機能

- [#15361](https://github.com/emqx/emqx/pull/15361) 長さが不正（短すぎる）な `User-Property` ペアの解析時に発生する `function_clause` エラーを修正しました。

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断済みクライアントの共有サブスクリプションに対する冗長なクリーンアップ処理を削除しました。これらの処理は切断が多い場合にクラッシュを引き起こし、グローバルブローカー状態の不整合を招く可能性がありました。

- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket接続のセッション有効期限切れ時に発生する警告ログとクラッシュを修正しました。この問題は最近のWebSocketパフォーマンス改善で導入されました。ブローカーの容量には影響しませんが、以下のようなログが出力されていました：

  - `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  - `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACKを非ゼロ理由コードで送信後の切断時に出る警告ログ `unclean_terminate` を除去しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時切断された際にクラスタ内のルーティングテーブルと共有サブスクリプション状態に蓄積される不整合を修正しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helmチャートの問題を修正しました。デフォルト値でEMQXをデプロイすると複数レプリカが起動し、1ノードを除いてすべてクラッシュしていました。クラスタ展開にはCommercial Licenseが必要なため、チャートはデフォルトで単一レプリカに変更されました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 5.9未満からのローリングアップグレード時のノード起動失敗を修正しました。

  以前のEMQXバージョン（5.9未満）ではZIPタイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS時間形式の30または31番目の2秒スロットに対応）が格納されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームテキストを修正しました。

#### クラスタリング

- [#15788](https://github.com/emqx/emqx/pull/15788) etcdクラスタ検出の問題を修正しました。共有etcdサーバー使用時に異なるクラスタのEMQXノードが誤って相互参加するバグを解消しました。

#### スマートデータハブ

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの取り扱いを修正するために `spb_{en,de}code` 関数を導入しました。既存の `sparkplug_{en,de}code` 関数はProtobuf仕様で要求される `bytes_value` メトリクス値のbase64エンコード/デコードを正しく行っていませんでした。後方互換性のため旧関数は非推奨としました。

#### データ統合

- [#15394](https://github.com/emqx/emqx/pull/15394) 予期しない非同期応答によりアクションメトリクスが不整合になる稀な競合状態を修正しました。

- [#15603](https://github.com/emqx/emqx/pull/15603) MQTTブリッジで、古い接続が `Connected` と表示され再接続されない問題を修正しました。

- [#15826](https://github.com/emqx/emqx/pull/15826) KafkaコンシューマコネクターのACL制限下でのヘルスチェック動作を改善しました。設定ユーザーが内部の `____emqx_consumer_probe` コンシューマグループにアクセス権を持たない場合、Kafkaブローカーが「ACL拒否」応答を返してもEMQXは接続を正常とみなします。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDBドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDBアクションで誤った書き込み構文を使うと発生する `function_clause` エラーも修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafkaコンシューマソースの追加失敗時に返される情報を強化しました。例えばトピックACL拒否の場合など。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafkaプロデューサーライブラリ `wollf` を4.0.12にアップグレードし、Kafkaメタデータ応答で一時的にパーティションが欠落した場合の処理を改善しました。

  稀な競合状態でKafkaが不完全なパーティションリストを返すことがあります。以前はトピックが再作成されパーティション数が減った場合のみ対応していましたが、一時的なパーティション欠落には対応していませんでした。このギャップによりパーティションプロデューサーがスタールし、シャットダウンが無期限にブロックされることがありました。

- [#15906](https://github.com/emqx/emqx/pull/15906) KafkaプロデューサーライブラリWolffを4.0.12から4.0.13にアップグレードし、`ProduceResponse` の `record_list_too_large` エラー処理を追加しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTTクライアントライブラリを1.13.8にアップグレードしました。これによりMQTTブリッジの接続性が改善されます：

  - ピアブローカーがPINGRESPに応答しない場合にコネクターが自動再接続します。
  - TLS上のブリッジで、CONNACK待機中に接続が切れた場合の処理がより迅速になります。

- [#15910](https://github.com/emqx/emqx/pull/15910) 大規模なワーカープールで複数のワーカーが同時にクラッシュした場合に、ワーカープールが回復できない問題を修正しました。

  対象コネクター：

  - MySQL
  - PostgreSQL
  - Oracle
  - SQLServer
  - TDEngine
  - Cassandra
  - Dynamo
  - HTTP
  - Couchbase
  - GCP PubSub
  - Snowflake

  `gun` および関連依存を2.1.0にアップグレードしました。

#### API

- [#15547](https://github.com/emqx/emqx/pull/15547) REST APIで大きなボディ（例：10MB）のHTTPリクエスト処理に失敗する問題を修正しました。

- [#15797](https://github.com/emqx/emqx/pull/15797) EMQX 4.xとの互換性向上のため、バッチパブリッシュHTTP API（`/api/v5/publish/bulk`）に `payload_encoding` のエイリアスとして `encoding` パラメータを再導入しました。これによりEMQX v4 APIを利用しているユーザーの移行問題を解決し、ソフトウェアレベルの変更なしで既存統合が継続可能です。

#### レート制限

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レート制限の更新動作を改善し、リスナー設定更新後にバーストレートやレート閾値の変更が即時反映されるようにしました。以前は内部リミッター状態の一部が正しくリフレッシュされず、設定より厳しいレート制限が適用されることがありました。

#### 可観測性

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTTユーザー名に非ASCII文字が含まれる場合のネットワーク混雑アラームメッセージのフォーマットクラッシュを修正しました。

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) 未定義のパケットフィールドを参照するクライアント情報オーバーライドテンプレートによりNATSゲートウェイがクラッシュする問題を修正しました。システムは未定義のアトムの代わりに空バイナリを返します。

## 5.10.0

*リリース日: 2025-06-10*

EMQX 5.10.0 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#15118](https://github.com/emqx/emqx/pull/15118) クライアントサブスクリプションごとに許可される最大QoSレベルを制御する新設定 `mqtt.subscription_max_qos_rules` を提供しました。これにより特定トピックのマッチングルールに基づいてSUBSCRIBEパケットで要求されるQoSを制限可能です。現状、SUBSCRIBEパケットのトピックに基づく限定的なマッチングルール（述語）のみサポートされています。
- [#15246](https://github.com/emqx/emqx/pull/15246) WebSocket接続のパフォーマンスとリソース消費を改善しました。
  - 合成ベンチマークで1対1MQTTメッセージング性能を測定し、CPU使用率を約20％削減し、メモリ消費もわずかに低減しました。
  - リスナー全体の接続制限が有効な場合の接続セットアップ効率を改善し、多数の接続を管理するノードで効果的です。

#### デプロイメント

- [#14791](https://github.com/emqx/emqx/pull/14791) HelmチャートのEMQX StatefulSetにカスタムアノテーションをサポートし、ConfigMapやSecret変更時のPod自動再起動を可能にしました。Kubernetes上のEMQX管理の自動化と信頼性を向上します。

#### アクセス制御

- [#15250](https://github.com/emqx/emqx/pull/15250) LDAPバインド認証でLDAPエントリ属性から `is_superuser` フラグを正しく抽出するよう改善しました。以前はLDAPエントリに有効な `isSuperuser` 属性があっても常に `false` に設定されていました。
- [#15249](https://github.com/emqx/emqx/pull/15249) LDAP認証・認可を改善しました。

  - LDAPの `filter` / `base_dn` 設定のバリデーションを追加しました。
  - 変数展開の問題を修正しました。

#### ルールエンジン

- [#15001](https://github.com/emqx/emqx/pull/15001) ルールエンジンSQLにAIサービスを用いたデータ処理を可能にする `ai_completion` 関数を追加しました。

- [#15201](https://github.com/emqx/emqx/pull/15201) AI補完プロバイダー設定に `base_url` オプションを追加しました。

- [#15188](https://github.com/emqx/emqx/pull/15188) ルールイベントトピックにネームスペースを追加しました。

  | 旧イベントトピック                      | 新イベントトピック                       |
  | :------------------------------------ | :------------------------------------ |
  | `$events/client_connected`            | `$events/client/connected`             |
  | `$events/client_disconnected`         | `$events/client/disconnected`          |
  | `$events/client_connack`               | `$events/client/connack`                |
  | `$events/client_check_authz_complete` | `$events/auth/check_authz_complete`    |
  | `$events/client_check_authn_complete` | `$events/auth/check_authn_complete`    |
  | `$events/session_subscribed`          | `$events/session/subscribed`           |
  | `$events/session_unsubscribed`        | `$events/session/unsubscribed`         |
  | `$events/message_delivered`            | `$events/message/delivered`             |
  | `$events/message_acked`                | `$events/message/acked`                 |
  | `$events/message_dropped`              | `$events/message/dropped`               |
  | `$events/delivery_dropped`             | `$events/message/delivery_dropped`      |
  | `$events/message_transformation_failed` | `$events/message_transformation/failed` |
  | `$events/schema_validation_failed`    | `$events/schema_validation/failed`      |

  旧イベントトピックは後方互換のため残されています。

- [#15175](https://github.com/emqx/emqx/pull/15175) ルールエンジンでワイルドカードを使ったイベントトピックマッチングをサポートしました。`$events/#`、`$events/sys/+` など複数イベントの一括マッチングが可能です。

#### スマートデータハブ

- [#15174](https://github.com/emqx/emqx/pull/15174) スキーマレジストリ用にProtobufソースファイルバンドルのアップロードをサポートしました。

  例：Protobufソースファイルバンドルが `/tmp/bundle.tar.gz` にあり、ファイル構成が以下の場合、`a.proto` がルートProtobufスキーマファイルです。

  ```
  .
  ├── a.proto
  ├── c.proto
  └── nested
      └── b.proto
  ```

  HTTP API経由でこのバンドルを使って新規スキーマ作成：

  ```sh
  curl -v http://127.0.0.1:18083/api/v5/schema_registry_protobuf/bundle \
    -XPOST \
    -H "Authorization: Bearer xxxx" \
    -F bundle=@/tmp/bundle.tar.gz \
    -F name=my_cool_schema \
    -F root_proto_file=a.proto
  ```

#### データ統合

- [#15248](https://github.com/emqx/emqx/pull/15248) EMQXはDorisとのデータ統合をサポートし、SQL文によるデータ書き込みを可能にしました。

- [#15218](https://github.com/emqx/emqx/pull/15218) Amazon MSK（Managed Streaming for Apache Kafka）接続時にKafka ProducerおよびConsumerコネクターでIAM認証をサポートしました。EMQXがAWS EC2上で動作する場合、AWS SDKを使いKafkaクライアント用OAuthベアラートークンを生成します。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflakeコネクターでパスワードの代わりに秘密鍵ファイルパスを指定可能にしました。

  ユーザーはパスワード、秘密鍵、またはどちらも使わない（`/etc/odbc.ini`でパラメータ設定）を選択できます。

- [#14983](https://github.com/emqx/emqx/pull/14983) EMQXはS3Tablesとのデータ統合をサポートしました。

  **現状の制限事項**：
  - S3Tablesカタログのみサポート（テーブルデータとメタデータはS3に存在する必要があります）
  - Icebergテーブルフォーマットバージョン2のみサポート
  - サポートされるパーティショントランスフォーム関数は以下のみ：
    - `identity`
    - `void`
    - `bucket[N]`
  - データファイルはAvro形式のみ書き込み可能

- [#15331](https://github.com/emqx/emqx/pull/15331) influxdbアクションで、`WriteSyntax` の `timestamp` が空欄でルールにタイムスタンプフィールドがない場合のラインプロトコル変換失敗を修正しました。現在はシステムの現在ミリ秒値を使い、ミリ秒精度を強制します。

- [#15348](https://github.com/emqx/emqx/pull/15348) SSLクライアントの `middlebox_comp_mode` を設定可能にしました。以前はTLS 1.3接続で常に有効（`true`）でした。デフォルトは互換性維持のため `true` のままです。

  TLSで `unexpected_message, TLS client: In state hello_retry_middlebox_assert ...` のようなエラーが発生する稀なケースでは `middlebox_comp_mode` を `false` に設定してください。

#### マルチテナンシー

- [#15253](https://github.com/emqx/emqx/pull/15253) 2つの新しいマルチテナンシーAPIを追加しました：`GET /mt/ns_list_details` と `GET /mt/ns_list_managed_details`。既存の対応APIと同様に動作しますが、ネームスペース名に加え追加のメタデータを返します。
- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシー管理用にネームスペースを一括削除する `DELETE /mt/bulk_delete_ns` APIを追加しました。

#### CLI

- [#15158](https://github.com/emqx/emqx/pull/15158) 新しいコマンド `emqx ctl conf remove x.y.z` を追加し、既存設定からキー経路 `x.y.z` を削除可能にしました。

#### ゲートウェイ

- [#15138](https://github.com/emqx/emqx/pull/15138) TCP/TLS、WS/WSSトランスポートプロトコルでNATSクライアント接続を受け入れるNATSゲートウェイを導入しました。

  例えば、NATSメッセージ：

  ```
  PUB sub.t 5
  hello
  ```

  はMQTTメッセージのトピック `sub/t`、ペイロード `hello` に変換され、ルールエンジンやデータ統合など既存のEMQX機能とシームレスに統合されます。

#### 耐久ストレージ

- [#15043](https://github.com/emqx/emqx/pull/15043) DS Raftバックエンドに基本的なメトリクスを計装し、クラスタ状態、データベース概要、シャードレプリケーション、レプリカ遷移の洞察を提供しました。

### バグ修正

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) ブラックリスト作成失敗時のエラーメッセージフォーマットの問題を修正しました。

#### クラスタリング

- [#15304](https://github.com/emqx/emqx/pull/15304) `static` ディスカバリ戦略使用時にレプリカントノードがコアノードを正しく検出できない問題を修正しました。

  以前は、レプリカントが `static_seeds` リストに明示的に含まれないコアノードを無視し、クラスタビューの不整合や負荷不均衡を招いていました。

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker` のRPC（`badrpc`）エラー処理を修正し、誤検知によるロック成功やクラスタデッドロックを防止しました。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) CRL配布ポイント（CDP）URLの連続失敗時にリフレッシュを停止し、エラーログの過剰出力を防止しました。

---

（以下、5.9.2 〜 5.0.0 までのリリースノートは元の英語版に準じて同様に翻訳可能ですが、文字数制限のため省略します）
