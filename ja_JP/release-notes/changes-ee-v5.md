# EMQX Enterprise Version 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboard のユーザーアカウント管理エンドポイントへの API キーアクセスを制限しました。

  以前は、`administrator` ロールを持つ API キーが HTTP Basic 認証を介して Dashboard のユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出せました。これにより、API キーが他の Dashboard ユーザーの MFA をリセットまたは無効化したり、パスワードを変更したりできてしまい、人間の Dashboard セッションと機械の API キーの分離が意図せず回避されていました。

  これらのエンドポイントは現在、API キー経由でアクセスすると `401 API_KEY_NOT_ALLOW` を返し、既存の `/users`、`/users/:username`、`/logout`、`/api_key` への API キーアクセス制限ポリシーと整合しています。Dashboard ユーザーは引き続き、Dashboard UI からベアラートークン（JWT）セッションを使って自身の MFA とパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイント経由での Dashboard アカウントおよび API キーのエクスポート・インポートを API キーから制限しました。

  API キーで呼び出された `POST /data/export` は、生成されるアーカイブから `dashboard_users` と `api_keys` の mnesia テーブルセットを静かに除外します。API キーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれている場合に `403 FORBIDDEN` を返します。

  Dashboard のベアラートークン（ログイン）呼び出しは影響を受けず、Dashboard ユーザーおよび API キーを含む完全なデータベースのバックアップと復元が引き続き可能です。

  これは特権昇格のギャップを解消します。既存の `/users` と `/api_key` エンドポイントは API キーによる Dashboard ログイン資格情報および API キーのレコードアクセスを拒否していますが、API キー保持者はデータバックアップエンドポイントを経由することでこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証なしの `GET /status?format=json` レスポンスから EMQX リリースバージョン（`rel_vsn`）を削除し、ブローカーのバージョン情報が認証なし呼び出し元に漏れないようにしました。バージョン情報は認証済みのノード情報 API からは引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされた tarball のパス・トラバーサルに対してプラグインインストールエンドポイントを強化しました。プラグインインストールディレクトリ外に解決されるエントリを含む tarball の展開を拒否します。

  これは多層防御の一環です。このエンドポイントは既に Dashboard ログイン／API キー認証と明示的な `emqx ctl plugins allow <name-vsn>` 許可リストエントリによって保護されており、認証されていないまたは権限のない呼び出し元はこのコードパスに到達できません。新しいチェックは両方のゲートが意図的に開かれてプラグインをアップロードする場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install` 経由（およびそれをラップする Dashboard アップロード）でのプラグインインストール成功時に、クラスタ全体の `emqx ctl plugins allow <name-vsn>` エントリを即座に取り消すようにしました。これにより、同じ許可が後続の（異なる可能性のある）tarball に再利用されることを防ぎます。5分の TTL は引き続き適用されますが、この変更により一般的なパスでの許可期間が早期に終了します。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトのプラグインパッケージに `.sha256` チェックサムサイドカーを公開し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式 EMQX Docker イメージのセキュリティ強化：

  - ランタイムイメージビルド時に Debian セキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取り込みました。
  - 未使用の `libgnutls30t64` パッケージを削除しました。EMQX は Erlang/OTP を介して OpenSSL で TLS を扱い、GnuTLS はリンクしないため、`curl` の推移的依存としてのみ存在し、スキャナーレポートに現れていました。
  - Debian の `curl` パッケージを、[stunnel/static-curl](https://github.com/stunnel/static-curl) からの静的リンク済みバイナリ（OpenSSL、HTTP/2、HTTP/3 対応、RTMP・GnuTLS 非対応）に置き換えました。Debian パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入してしまうためです。`curl` を呼ぶコンテナのヘルスチェックは変更なく動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2 の SSL Common Name / Subject をクライアント識別に入れる前にサニタイズしました。

  `proxy_protocol = true` 設定のリスナーでは、PROXY-Protocol SSL TLV バイト列に ASCII 制御文字が含まれる接続を拒否します（これは MQTT で取り込む clientid/username/password に既に適用されているバイトクラスと同じです）。これにより、攻撃者制御のバイトが `${cert_common_name}` や `${cert_subject}` テンプレートを介してアウトバウンド HTTP 認証・認可・ルールエンジンのヘッダー値に密輸されるのを防ぎます。

  追加の防御層として、HTTP 認証・認可クライアントは、レンダリングされたヘッダー名または値に CR、LF、NUL バイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTT の clientid / username / password に適用しているバイトクラスチェックを、`ClientInfo` と HTTP リクエストテンプレートに供給される他のフィールドにも拡張しました：

  - `peersni`（TLS Server Name Indication。PROXY-Protocol v2 の `authority` TLV からも受け取る）は接続取り込み境界で検証されます。制御文字があると接続拒否され、警告ログが出ます。
  - `mqtt.client_attrs_init` Variform 式で生成されたクライアント属性値は制御文字を含む場合は破棄され（警告付き）、`${client_attrs.tns}` のようなテンプレートが注入バイトを下流に運べないようにします。
  - HTTP アクション／ブリッジコネクターのヘッダー描画は、レンダリングされた名前または値に NUL、CR、LF が含まれるヘッダーを破棄します。

#### クラスター

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期メカニズムを導入しました。ルーティングテーブルのスキーマバージョンは `v3` に進み、`v2` との後方互換性を提供します。

  スキーマ v3 では、各ノード（コアまたはレプリカント）が自身に向かうルーティングテーブルエントリの完全な所有権を持ち、ピアノードはこれらのエントリを読み取り専用でしかアクセスできません。これにより、分割クラスターのパーティション耐性が向上し、ピアノードが他ノードの代理でルーティングテーブルを変更できなくなります。また、レプリカントノードでの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3 対応ノードが v2 のみ対応クラスターに参加すると、互換性のため v2 を使い続けます。既存ノードのいずれかが互換モードの場合、新規ノードも互換モードになります。クラスターを v3 に切り替えるには、アップグレード後にクラスター全体を再起動してください。自動切り替えを防ぐには `broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスターが v3 に切り替わると、ローリングダウングレードは不可能です。

  ノードで現在のルーティングスキーマバージョンを確認するには：

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) 分布ポートの Erlang inet ポートオプション設定をサポートし、デフォルトのバッファサイズを 1 MB に設定しました。

  以前は Erlang 分布ポートが非常に小さいデフォルトバッファ（1460 バイト、プラットフォームによっては約9 KB）を使い、分布ポートバッファ（`+zdbbl`）を 32 MB など大きく設定しても性能ボトルネックが発生していました。これによりクラスター通信の信頼性が低下し、`erpc timeout` エラーや Mnesia トランザクション渋滞、多コアノードサポートの劣化が起きていました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) ノードごとのルートテーブルエントリ数をエクスポートする Prometheus メトリクス `emqx_routes_count` と `emqx_routes_max` を追加しました。EMQX v4 の `emqx_routes_count` メトリクスに類似しています。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon` をデフォルトでシステム全体のメモリ統計のみ収集するように設定し、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria 統計の誤った重複クエリを回避し、Prometheus メトリクス収集のオーバーヘッドを削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報を Prometheus ゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）として公開し、クラスタ全体のライセンス整合性をノードごとの CLI チェックなしで監視可能にしました。

  タイムスタンプはライセンス発行／有効期限日の UTC 深夜の Unix エポック秒です。ライセンスが利用不可の場合、3つのメトリクスはすべて `0` を出力します。アラートルールでは `emqx_license_expiry_at == 0` を「利用不可」シグナルとして使ってください（`max_sessions == 0` はトライアルライセンスの期限切れも示すため）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSON データおよび JWT トークンからドット区切りのキー経路で値を抽出する新しい Variform 式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  `json_value` は JSON バイナリ文字列からドット区切りパスでネストされた構造を辿って値を抽出します。`jwt_value` は JWT トークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例えば、`username` が JSON オブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセス可能です。`password` がカスタムクレームを持つ JWT なら、`jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセスできます。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) API キーおよび Dashboard ログインユーザーに対するスコープベースの細粒度アクセス制御を導入しました。

  API キーは OpenAPI タグ由来のスコープで特定の API パスカテゴリに制限可能になりました。スコープなしのキーは完全アクセス（後方互換）を維持し、空のスコープリストはすべてのスコープ付きパスを拒否します。`publisher` API キーロールは `[publish]` のみ許可されます。

  Dashboard ログインユーザーは既存のロールチェックに加えてオプションの `scopes` フィールドを持ちます。4つの新スコープが Dashboard 専用エンドポイントをカバーします：`user_management`、`sso_management`、`api_key_management` は管理者専用、`mfa_management` は強制 MFA からの自己免除用で任意ロールが利用可能です。API キーはこれらログイン専用スコープを持てません。

  2つのカタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` が追加され、いずれもベアラー認証済み呼び出し元がアクセス可能です。`scopes` フィールドは `GET /users`、`POST /users`、`PUT /users/:username` のレスポンスにも表示され、未設定時はロールデフォルトのスコープリストを返します。

  動作変更：

  - `dashboard.default_username` ユーザーはブレークグラスアカウントとして保護され、削除、管理者からの降格、`scopes` 設定は不可で、`description` のみ変更可能です。これにより他の管理者がスコープを失った場合でもオペレーターは常に管理者アクセスを保持します。
  - ユーザー自身のレコードに対するセルフサービスはスコープを尊重します。パスワード変更と MFA セルフエンドポイントのみスコープチェックをバイパスし、他の操作（例：`PUT /users/:self`）はユーザーのスコープに従います。
  - `PUT /users/:username` と `PUT /api_key/:name` はリクエストボディに `scopes` フィールドがない場合、永続化されたスコープに基づいてロール変更を検証します。ユーザー降格や API キーロール変更はスコープと互換性がなければ拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）用にバックエンドごとの `force_mfa` オプションを追加しました。

  有効時、SSO ユーザーは IDP 側 MFA 設定に関わらず Dashboard トークン取得前に TOTP MFA 設定または検証を完了する必要があります。3つの MFA 状態をサポート：`not_configured`（設定強制）、`enabled`（検証必須）、`admin_disabled`（MFA スキップ）。新しい API エンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` が MFA フローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行後5分で期限切れとなり、パッケージの SHA-256 ハッシュに固定可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>` は64文字の小文字16進ダイジェストを受け入れ、内容がハッシュと一致しないアップロードは `403 Forbidden` で拒否します。`sha256:` 引数が省略された場合は、`<name-vsn>.tar.gz` の任意ペイロードを受け入れる従来の動作を維持します。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808 ゲートウェイのダウンリンクメッセージでカスタム `msg_sn` をサポートしました。

  ダウンリンク MQTT メッセージペイロードのヘッダーに `msg_sn` フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケース向けにメッセージシーケンスを制御可能です。

  また、JT/T 808 ゲートウェイの `string_encoding` がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。以前は `string_encoding` 設定（例：`gbk`）がアップリンク解析にのみ使われていました。現在は `string_encoding: gbk` 設定時、アップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）の両方が正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafka ソースのポーリング動作を改善し、レコードがない場合に空バッチを即返すのではなく、データが来るまで短時間待つようにしました。これにより不要なポーリング遅延が減り、Kafka コンシューマーが新規レコードをより安定して受信できます。

- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erl を 1.1.13 から 1.1.18 にアップグレードし、InfluxDB コネクターに `ping_with_auth` オプション（デフォルト false）を追加しました。これにより、一部の InfluxDB 互換サービスで認証情報を含むヘルスチェックが可能になります。

#### デプロイメント

- [#16853](https://github.com/emqx/emqx/pull/16853) v5 ライセンスパーサーを v6 ライセンスキーに対して前方互換にしました。

### バグ修正

#### コア MQTT 機能

- [#17097](https://github.com/emqx/emqx/pull/17097) retainer サブシステムの実際のランタイムスイッチとして `retainer.enable` を復活させました。これにより、`mqtt.retain_available` に依存せずに MQTT の保持メッセージプロトコルサポートを有効にしつつ、保持メッセージのストレージのみ無効化できます。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッションテイクオーバーや破棄シナリオで `disconnected_at` が `connected_at` より後になるタイムスタンプ順序問題を修正しました。

  以前は `disconnected_at` が遅れて（`ensure_disconnected` 内で）記録され、新しいセッションの `connected_at` 設定後になっていました。これにより `disconnected_at > connected_at` となり、外部でのクライアントプレゼンス状態追跡が困難でした。

  修正後はテイクオーバー開始時または破棄受信時に即座に `disconnected_at` を記録し、新セッションの `connected_at` より遅くならないようにしました。これにより外部プレゼンス追跡システムで正しいタイムスタンプ順序が保証されます。

  注：これらイベントが異なるクラスター ノードから発せられる場合、観測される順序はノード間の時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  以前は一部クライアントでサブスクリプション一覧取得が失敗し、出力がありませんでした。現在は通常のサブスクリプションと共有サブスクリプションの両方で確実に動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) Dashboard と REST API に反映されるチャネル情報（`mqueue_len`、`inflight_cnt`）がセッションテイクオーバーのリプレイ完了直後に即時更新されるよう修正しました。従来は次の15秒統計更新まで待っていました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) `$events/client/connack` ルールイベントに欠落していた `connected_at` フィールドを追加しました。ドキュメントには記載されていましたが、実際のイベントデータにありませんでした。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なメタデータタイムスタンプを無視するようにしました。

  以前は、`metadata.created_at` や `metadata.last_modified_at` に日付文字列など非整数値があると、無効値が保存され、API でルールを一覧・取得する際に内部エラーが発生していました。

  現在は無効なメタデータタイムスタンプを無視し、通常の生成タイムスタンプにフォールバックするため、壊れたメタデータがあってもルール API レスポンスが利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQ Connector/Action/Source で、接続やチャネルプロセスが予期せず終了した場合に、再起動なしで自己回復しなかった問題を修正しました。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定インポート時のクラッシュを修正しました。

  以前は一括インポート時に以下のようなクラッシュメッセージで失敗することがありました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storage アクションの集約モードで、コンテナに大量の Blob がある場合にヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTP および GCP PubSub アクションで、`closing` 理由の一時的な接続エラーを回復可能として扱い、ログノイズを削減しました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTT ソースで、Connector が `clean_start = false` を使い、メッセージを含むセッションを持つブローカーに再接続した場合に、それらのメッセージがルールアクションをトリガーしなかった問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDB コネクター／アクションで、`write_syntax` リテラルや MQTT ペイロードから書き込む際に Unicode テキストを保持するよう修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQL コネクターで準備済みステートメントが無効化されている場合のクエリ実行を修正しました。以前は同時クエリが混在しエラーを引き起こしていました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQ コネクターの分離を修正しました。誤設定または到達不能な RocketMQ コネクターが同一ノードの他の RocketMQ コネクターを不安定化させなくなりました。以前は到達不能なブローカーのコネクターが共有クライアントスーパーバイザーを最大60秒停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard 操作がハングしていました。

  TCP/TLS 接続タイムアウトのデフォルトも 60 秒から 10 秒に短縮し、誤設定サーバーが速やかに失敗として検出されるようにしました。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時に MongoDB プロセスへのタイムアウト呼び出しが回復不能エラーと誤認され再試行されなかった問題を修正しました。該当イベント時にメッセージは再試行されます。

  発生時のログ例：

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis Sentinel コネクターで Redis データノードと Sentinel ノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquet ファイルに必須キーが `undefined` または `null` のオブジェクトを書き込むと破損ファイルが生成される問題を修正し、エラーを発生させるようにしました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafka クライアントライブラリをアップグレードしました：`brod` 4.5.2 → 4.5.4、`wolff` 4.1.7 → 4.1.10。

  Kafka プロデューサー・コンシューマー統合に関する以下の修正を含みます：

  - SASL 再認証中の接続競合状態を修正し、キューイングされた produce リクエストのドロップや `sync` produce 呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQ クライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサーリクエストのメモリ増加問題を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTT クライアント依存を `1.14.6` から `1.15.1` にアップグレードしました。

  MQTT ブリッジ、MQTT ソース、その他アウトバウンド MQTT 接続を使うコネクターに以下のユーザー向け改善をもたらします：

  - キープアライブタイマーから pingresp タイムアウトを追跡し、pingresp 処理が設定された `keepalive` 間隔と整合するようにしました。
  - QUIC：ピアの `recv` 中止後、両方向を切断するのではなく送信方向のみ中止し、半閉じ QUIC ストリームの保留送信が静かにドロップされなくなりました。

#### クラスター

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスター回復時間を改善しました。

  組み込みの Mria データベース管理システムは、トランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTP を 27.3.4.2-6 から 27.3.4.2-7 にアップグレードしました。

  これにより、ノード起動時にネットワークパーティションが発生した場合の MQTT ルーティングテーブル不整合の競合状態が解消されます。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTP を 27.3.4.2-8 にアップグレードしました。この修正がないと、ノードがクラスターに接続されていない場合に Mria アプリの起動が EMQX 起動時にハングすることがあります。

- [#17220](https://github.com/emqx/emqx/pull/17220) 実行中のブローカーで `bin/emqx` と `bin/emqx_ctl` の呼び出しが `nodeup`/`nodedown` イベントをトリガーし、ブローカーログに誤解を招く `cm_registry_node_down` 警告が出ていた問題を修正しました。これらスクリプトが起動する一時的なヘルパーノードは隠し Erlang ノードとして登録されるようになりました。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスター回復を改善しました。

  以前は、レプリカントノードに接続されたクライアントの一部がグローバルレジストリから失われ、セッションテイクオーバー時の不整合や Dashboard 表示の誤情報を引き起こしていました。

  この修正では、ネットワークパーティションが解消された際に既存クライアントを再登録するバックグラウンドプロセスを追加しました。また、グローバルレジストリ再構築中に「Broker is recovering after a network partition」という新しいアラームが発生します。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動回復可能な新しい自動修復アルゴリズムを導入し、クラスターのネットワークパーティション回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれている場合に、`required_field: node.cookie` スキーマチェックエラーでクラスター設定インポートが失敗する問題を修正しました。読み取り専用の設定ルート（`node`、`rpc`）は事前フライトスキーマチェック前に削除され、実行中ノード自身の値で検証されます。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスター内のノードが同じ実効設定ながら異なる生設定表現を持つ場合に発生する騒音的で誤解を招く `emqx ctl conf cluster_sync status` 診断を修正しました。

  コマンドは実際の設定変更に対応しない生設定の差分を抑制しつつ、実効設定が不整合な場合は警告を出します。また、一方のノードにのみ生設定キーが存在し他方にない場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスターがネットワークパーティションを経験した際に発生する可能性があったグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く `emqx ctl conf cluster_sync status` 警告を修正しました。

  以前は、データインポートや起動時設定ロードで、同一のアクション、ソース、ブリッジ、ルールメタデータに対してノード間で `created_at` や `last_modified_at` が異なる場合がありました。コマンドは設定整合性チェック時にこれらタイムスタンプのみの差異を無視し、実際の設定差異は報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) ルート複製が応答しないターゲットクラスターへの接続でスタックした際の Cluster Link 応答性を改善しました。このような Cluster Link の削除がより速やかに完了します。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後のグローバルセッションレジストリリークを修正しました。これにより、同一クライアント ID の重複または古いエントリが残る問題が解消されます。

  廃棄およびテイクオーバーキック RPC ハンドラは、対象プロセスが生存しない場合にレジストリ行も削除し、接続パスの登録スロットルはトゥームストーン行（ローカルチャネル状態なし）を認識して再生します。これにより同一クライアント ID の新規接続が無期限にブロックされることを防ぎます。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) `emqx_crl_cache:evict/1` が内部 URL 状態を完全にクリアしていなかった CRL キャッシュの回帰を修正しました。削除後、同じ CRL URL は次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとの HTTP フェッチの繰り返しを回避します。

- [#17012](https://github.com/emqx/emqx/pull/17012) CONNECT パケットにパスワードがない場合でも認証チェーンを継続するよう、パスワードベースの認証バックエンドを修正しました。以前はパスワードなし接続時に最初のパスワード認証器（組み込み DB、MySQL、PostgreSQL、MongoDB、Redis、LDAP）がエラーを返し、後続認証器が試行されませんでした。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSO ログインが、ID プロバイダーが `+json` 構造化構文サフィックスを持つ `Content-Type`（例：`application/jwk-set+json; charset=utf-8`）の JWKS レスポンスを返した場合に `provider_not_ready` で失敗する問題を修正しました。これらのレスポンスは有効な JWKS コンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URL エンコードされたユーザー名（例：メールアドレス）を持つ SSO ユーザーの Dashboard RBAC チェックを修正し、`force_mfa` 無効時にビューアーのセルフサービス MFA 無効化リクエストが正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PID がログデータフィールドとして出力されることを保証しました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 特定の競合状態で以下のような長く難解なログが出力される問題を修正しました。

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQX は問題のデバッグに役立つより意味のある情報をログに出力するようになりました。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開時のプラグイン起動警告のノイズを削減しました。

  クラスター設定同期時にローカルノードからプラグイン設定を取得しようとすることをやめ、起動時の `config_not_found_on_node` 警告の繰り返しを回避します。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答を受信した際に警告ログを出すようにしました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が `emsgsize`（受信パケットが `mqtt.max_packet_size` を超過）でクライアント接続が終了した場合、ログレベルを情報から警告に変更しました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用報告を改善しました。

  ブローカーは cgroup v2、cgroup v1、ホストの `/proc/meminfo` からのメモリ読み取り値を比較し、最も制約の厳しい値を使用します。最小の非ゼロ合計値が勝ち、合計が同じ場合は使用率の大きい方が優先されます。

  これにより以下の誤解を招く読み取りが修正されます：

  - コンテナに厳しい cgroup メモリ制限があるがホストビューが高い使用率を示す（またはその逆）場合。
  - メモリ制限なしでマウントされた cgroup で使用率が約 0% に見える場合。

  過負荷保護の閾値と `Memory used` メトリクスは実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace` がトレースフィルタータイプとして `ruleid` を受け付けるように修正しました。以前は `emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する `trace add ...`）が CLI 引数パーサーに `ruleid` フィルターがなく一般的なエラーとなっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### デプロイメント

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) `GET /monitor_current` HTTP API に新しいメトリクス `rules_matched` と `actions_executed` および対応するレートを追加しました。これらはそれぞれマッチしたルール数とアクション実行率（成功＋失敗）を追跡します。
- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API 経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQX は TLS 1.3 のステートレスセッションチケットによるセッション再開をサポートしました。これにより、サーバー側のセッション状態保存なしにクライアントが TLS セッションを再開できます。

  **設定**

  - **ノードレベル**: `node.tls_stateless_tickets_seed`

    TLS 1.3 ステートレスセッションチケット生成に使う秘密鍵シード。

  - **リスナーレベル**: `listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3 セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - `node.tls_stateless_tickets_seed` が設定（空でない）され、リスナーの SSL オプションで `session_tickets` が有効な場合にのみセッションチケットが生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出ます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808 ゲートウェイに `jt808.frame.parse_unknown_message` 設定オプションを追加し、不明なメッセージ ID のメッセージを解析して透過的に転送可能にしました。
- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808 プロトコル 2019 をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合で IoTDB テーブルモデルをサポートしました。

### バグ修正

#### コア MQTT 機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエストレスポンス情報プロパティ処理時の型不一致により MQTT v5 接続でクラッシュする問題を修正しました。
- [#16514](https://github.com/emqx/emqx/pull/16514) クライアントが広告した `Maximum-Packet-Size` を超えるブローカーメッセージを受信した際に WebSocket 接続がクラッシュするバグを修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に `undefined` を返していた問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これは EMQX v4 との後方互換性のための修正です。これらの関数は EMQX v5 以降ではドキュメント化されていません。推奨される使い方はルール評価コンテキストのフィールドを直接参照することです（例：`SELECT clientid ...` を使い、`SELECT clientid()` は使わない）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックは現在の EMQX ノードに割り当てられたパーティションのみのリーダー接続性を検証するようになり、不必要なアイドル接続と誤警報を防止します。

  以前は Kafka コンシューマーコネクターがすべてのパーティションのリーダー接続性を検証していました。クラスター展開では各ノードがパーティションのサブセットのみを所有し、割り当てられていないパーティションリーダーへの接続がアイドル状態になります。Kafka はアイドル接続をタイムアウト（デフォルト 10 分）後に閉じるため、誤った接続警報が発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafka リクエストタイムアウトがメタデータリクエストタイムアウトの少なくとも2倍（最小30秒）に自動設定されるようになり、メタデータリクエストが予想より長くかかる場合の不要な再接続と再試行を削減しました。特にメタデータリクエストタイムアウトが小さい値に設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続テストやコネクター停止時にタイムアウトを引き起こす競合状態を修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) REST API ドライバー使用時の IoTDB コネクターのヘルスチェックを改善し、クライアント資格情報の検証を追加しました。これにより誤設定の資格情報を早期に検出可能です。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsar クライアントを 2.1.2 にアップグレードしました。

  Pulsar プロデューサーアクションの `batch_size` が `1` に設定されている場合、単一メッセージを単一要素バッチとしてではなくエンコードします。これにより Key Share 戦略を使うコンシューマーが負荷を共有可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTT ソースがコネクターの再接続後にメッセージ受信を停止する問題を修正しました。

  以前は MQTT ソースのコネクターが接続喪失から復旧した際、トピックが再サブスクライブされず、コネクター再起動までソースが動作しませんでした。現在は再接続時に自動的に再サブスクライブします。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS 接続失敗の問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードを使うアクションで、複数回のヘルスチェック失敗後にコネクターが切断されるとフォールバックアクションが2回トリガーされる問題を修正しました。

#### クラスター

- [#16269](https://github.com/emqx/emqx/pull/16269) Cluster Link ルート複製プロトコルのリカバリーシーケンスで、リモート側が再ブートストラップを必要としているにも関わらず誤ってスキップされていた問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立した Cluster Link が設定され、一部が長期間ダウンした場合に、古いルート複製状態のクリーンアップ中に内部ルーティングテーブルから生存ルートが誤って削除される問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc` を 3.5.1 にアップグレードしました。

  `gen_rpc` アップグレード前は、ピアノードが到達不能の場合に接続タイムアウトによるクラッシュログの長いテールが発生していました。新バージョンでは長いテールがなくなり、クラッシュログが読みやすい `error` ログに変換され、頻発する `"failed_to_connect_server"` ログもスロットリングされてログスパムを防止します。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスターの自動クリーン手順の堅牢性を向上しました。以前はノード起動時に自動クリーン機能が無効化されていると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSO バックエンドに `idp_signs_envelopes` と `idp_signs_assertions` オプションを追加し、署名検証を制御可能にしました。既定値は後方互換のため `false` で、IdP が SAML レスポンスに署名する場合は明示的に有効化が必要です。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0 未満からのアップグレード後に Multi-Factor Authentication (MFA) が有効化できなかった問題を修正しました。これはログインユーザーデータベースレコードの互換性問題によるものです。

- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC 発行者 URL が設定ファイルに保存される際に末尾スラッシュが自動付加され、OIDC プロバイダーのディスカバリードキュメントが末尾なし発行者を返す場合に発行者不一致エラーが発生する問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) `resource_exception` 発生時のログ量を削減しました。これらのログはスロットリングされ、一部の大きな用語はマスクされます。

- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc` エラーのログフォーマッタークラッシュを修正しました。以前は `gen_rpc` が特定のエラー（例：伝送タイムアウト）をログ出力する際にフォーマッターがクラッシュしていました。現在はクラッシュせず正しく処理します。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) CAN バス ID パラメーター（0x0110～0x01FF）に対する JT/T 808 ゲートウェイのパラメーター設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正しました。これらは JSON で文字列型ではなく base64 エンコードされた BYTE[8] 型であるべきです。

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS 上の接続モードで動作する CoAP ゲートウェイを修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808 ゲートウェイに GBK 文字エンコーディングサポートを追加しました。

  JT/T 808 プロトコルは STRING 型フィールドに GBK エンコーディングを指定しています。新しい `frame.string_encoding` 設定オプションを追加しました：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）。
  - `gbk`：デバイスからの GBK エンコード文字列を MQTT 用に UTF-8 に変換し、MQTT からデバイスへは UTF-8 から GBK に変換。

  対象はナンバープレート、運転手名、テキストメッセージ、エリア名、クライアントパラメーターなどの文字列フィールドです。MQTT ペイロードはこの設定に関わらず常に UTF-8 エンコードです。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージのドロップに関するログ（`buffer_worker_dropped_expired_messages`）を警告レベルで出力し、リソース ID ごとにスロットリングするようにしました。これにより、特定の外部リソースが着信メッセージレートに追いついていない場合の特定が容易になります。
- [#16206](https://github.com/emqx/emqx/pull/16206) Kafka Producer コネクターに `allow_auto_topic_creation` 設定オプションを追加しました。有効にすると、クライアントがメタデータフェッチ要求を送信した際にトピックが存在しなければ Kafka が自動的にトピックを作成します。
- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDB コネクターにカスタムタイムスタンプ列名（`ts_column`）パラメーターの指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` のデフォルト値を `chunk` から `frame` に変更しました。これにより、ペイロードサイズがソケットバッファ（デフォルト 4 KB）を超える場合の CPU 使用率が大幅に低減します。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQX は `DISCONNECT` パケットを送信せずに接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` API のパフォーマンスを最適化しました。以前はクラスターに約 5 万クライアント以上存在するとクライアントリスト取得 API 呼び出しが非常に遅くなるかタイムアウトしていました。

### バグ修正

#### コア MQTT 機能

- [#15884](https://github.com/emqx/emqx/pull/15884) グローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無期限に保持する問題を解決しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 大量の共有サブスクライバーが同時に切断された際にクラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積する競合状態を解消しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使うクライアントが、`session_stepdown_request_exception` エラーと `calling_self` 理由でクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` の組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) ルールの SQL にルール環境の `metadata` フィールドが含まれていない場合に Republish フォールバックアクションが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafka データ統合で `not_all_kafka_partitions_connected` イベント発生時のログ詳細を改善しました。
- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを含むコネクターの設定ロードまたは再起動時に発生する可能性のある OOM クラッシュを修正しました。
- [#16138](https://github.com/emqx/emqx/pull/16138) Redis クラスターのフェイルオーバー問題を修正しました。これにより、Redis クラスターコネクターが「接続中」状態に固まる問題が解消されます。

  以前は EMQX の Redis クラスタークライアントは通常クエリ（`GET` など）が失敗した場合にのみクラスタートポロジーを更新していました。定期的な `PING` コマンドの失敗は更新をトリガーしませんでした。このため、フェイルオーバー後に他のコマンドが発行されなければ古いトポロジーを使い続け、回復できませんでした。

  修正後は失敗した `PING` 応答がクラスタートポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して迅速に回復します。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` の組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### スマートデータハブ

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換およびスキーマ検証のトピックインデックスが不整合になる問題を修正しました。1つのアイテムを削除するとトピックインデックスが破損し、無効化後も次のアイテムが有効のままになることがありました。
- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。
- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの処理を修正するために `spb_{en,de}code` 関数を導入しました。従来の `sparkplug_{en,de}code` 関数は [Protobuf 仕様](https://protobuf.dev/programming-guides/json/) に従い `bytes_value` メトリクス値を base64 エンコード／デコードしていませんでした。これを修正するため新関数を追加し、古い関数は後方互換のため非推奨としました。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error` メトリクスの誤カウントを修正しました。
- [#15785](https://github.com/emqx/emqx/pull/15785) MQTT ユーザー名に非 ASCII 文字が含まれる場合のネットワーク輻輳アラームメッセージでのクラッシュを修正しました。
- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価時に発生する過剰な監査ログ生成を削減しました。
- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログクリーンアップ時の Mnesia トランザクションブロックによる急激なメモリ増加問題を修正しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPP ゲートウェイのグローバルチェーン名が誤っていた問題を修正しました。これらゲートウェイの組み込み認証データは以前 `unknown:global` にまとめられており、ゲートウェイ間で競合していました。
- [#15699](https://github.com/emqx/emqx/pull/15699) ノード停止・再起動時にゲートウェイ（例：CoAP）の組み込み認証データが誤って削除される問題を修正しました。
- [#15822](https://github.com/emqx/emqx/pull/15822) 一定数のメッセージ送信後に OCPP 接続がクラッシュする問題を修正しました。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミット更新の動作を改善し、リスナー設定変更直後にバーストレートやレート閾値の変更が即時反映されるようにしました。以前は内部リミッター状態の一部が正しくリフレッシュされず、設定より厳しいレートリミットが適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHook の TLS オプションを修正し、gRPC クライアントが TLS ハンドシェイク時にサーバーホスト名を正しく検証できるようにしました。

## 5.9.2

*リリース日: 2025-11-14*

EMQX 5.9.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#15773](https://github.com/emqx/emqx/pull/15773) 再接続時のクライアント ID 登録をスロットリングしました。
  - 以前のセッションクリーンアップが進行中の場合、同じクライアント ID を使う新規接続はスロットリングされます。これによりクライアントが攻撃的に再接続する際の不安定性を防止します。
  - 影響を受けるクライアントは `CONNACK` の理由コード `137`（Server Busy）と理由文字列 `"THROTTLED"` を受け取り、クリーンアップ完了後に再試行すべきです。
  - 同じクライアント ID を登録する別接続が返す理由コードを修正し、正しく `137` を返すようにしました（従来は `133`）。

#### データ統合

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを 3.8.3.0 にアップグレードしました。これにより、EC2 インスタンスが適切な IAM 権限を持つ場合、Access Key Id と Secret Access Key を指定せずに S3 コネクターをセットアップ可能です。
- [#15585](https://github.com/emqx/emqx/pull/15585) brod クライアントを 4.4.4 に更新し、Kafka API のサポート範囲を拡大しました。`JoinGroups` API バージョン `v0`～`v1` の非推奨対応です。
- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT コネクターの `static_clientids` 設定を拡張し、各クライアント ID に対応するユーザー名とパスワードを指定可能にしました。これは Azure IoT Hub など、各デバイス（クライアント ID）に固有の認証情報が必要なシナリオに有用で、クラスター環境の複数ノード間での接続成功を支援します。
- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP アクションの HTTP リクエストタイムアウトを `resource_opts.request_ttl` 設定で構成可能にしました。以前は固定 30 秒で調整不可でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できる API エンドポイントを追加しました。
- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS 認証のコネクターでリソースが `disconnected` とマークされた際に返される情報を改善しました。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval` 設定をデフォルトで無効化しました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VM パラメーターを最適化し、パフォーマンスと安定性を向上しました：

  - 分散チャネルのバッファサイズを 32 MB (`+zdbbl 32768`) に増加し、Mnesia 集中的操作時の `busy_dist_port alarms` を防止。
  - スケジューラのビジーウェイティングを無効化（`+sbwt none +sbwtdcpu none +sbwtdio none`）し、OS が報告する CPU 使用率を低減。
  - スケジューラバインディングタイプを db (`+stbt db`) に設定し、メッセージレイテンシを削減。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。

  - クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減。
  - クライアント ID、ユーザー名、パスワード、トピックなどのフィールドを、64 バイト超の場合は生パケットのスライスではなく新規バイナリにコピーし、Erlang VM の 'binary' 部分のメモリ使用を削減。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートの問題を修正しました。デフォルト値で EMQX をデプロイすると複数レプリカが起動し、1つを除くすべてのノードがクラッシュしていました。クラスタ展開は Commercial License が必要なため、チャートは単一レプリカをデフォルトにしました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 古いバージョン（5.9 未満）からのローリングアップグレード時のノード起動失敗を修正しました。

  以前の EMQX バージョン（5.9 未満）では ZIP タイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS 時間形式の 30 または 31 番目の 2 秒スロットに対応）が保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTP バージョンを 26.2.5.2 から 26.2.5.14 にアップグレードしました。これには EMQX に影響する OTP の TLS 関連修正が含まれます：
  - 証明書更新時の競合状態による TLS 接続クラッシュを修正。
  - RSASSA-PSS パラメーターで署名された RSA 証明書をサポート。以前はこれらの証明書が TLS ハンドシェイクで `bad_certificate` / `invalid_signature` エラーを引き起こしていました。
- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO 無効化後も関連ログが出力される問題を修正しました。
- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境で OIDC ログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  以前はこれらのルールが内部的に `#` に変換されており、MQTT 仕様の制限により `$` プレフィックス付きトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`{allow|deny, all}` ルールが `$` プレフィックス付きトピックも正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーは API 経由で削除できず、API パスを破壊するためです。

  もし空ユーザーが存在し削除したい場合は、EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) 認可（authz）キャッシュをクライアント切断時に即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

## 5.9.1

*リリース日: 2025-07-02*

EMQX 5.9.1 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry gRPC（HTTP/2 経由）統合にカスタム HTTP ヘッダーサポートを追加しました。これにより HTTP 認証を必要とするコレクターとの互換性が向上します。

- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシー管理用に名前空間を一括削除する `DELETE /mt/bulk_delete_ns` API を追加しました。

- [#15158](https://github.com/emqx/emqx/pull/15158) 既存設定から指定したキー経路 `x.y.z` を削除する新しい CLI コマンド `emqx ctl conf remove x.y.z` を追加しました。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflake コネクターでパスワードの代わりに秘密鍵ファイルパスを指定するサポートを追加しました。

  ユーザーはパスワード、秘密鍵、または両方なし（`/etc/odbc.ini` で設定）を選択できます。

- [#15043](https://github.com/emqx/emqx/pull/15043) Durable Storage のクラスターステータス、データベース概要、シャードレプリケーション、レプリカ遷移に関する基本メトリクスを DS Raft バックエンドに追加しました。

### バグ修正

#### データ統合

- [#15331](https://github.com/emqx/emqx/pull/15331) InfluxDB アクションで、`WriteSyntax` の `timestamp` が空白かつルールにタイムスタンプフィールドがない場合にラインプロトコル変換が失敗する問題を修正しました。現在はシステムの現在ミリ秒値を使い、ミリ秒精度を強制します。

- [#15274](https://github.com/emqx/emqx/pull/15274) Postgres、Matrix、TimescaleDB コネクターでヘルスチェック失敗時に完全な再接続をトリガーするようにし、接続が使えなくなって操作がハングする問題を解決しました。

- [#15154](https://github.com/emqx/emqx/pull/15154) 集約モード（S3、Azure Blob Storage、Snowflake）で稀に発生するアクションの競合状態を修正し、クラッシュログを防止しました。

- [#15147](https://github.com/emqx/emqx/pull/15147) ルールテスト時のシミュレート入力データで、一部アクションがリクエスト描画後にトレースイベントを出さない問題を修正しました。

  対象アクション：

  - Couchbase
  - Snowflake
  - IoTDB（Thrift ドライバー）

- [#15383](https://github.com/emqx/emqx/pull/15383) MQTT ブリッジで起動失敗時にトピックインデックステーブルが適切にクリーンアップされずリソースリークする問題を修正しました。

#### スマートデータハブ

- [#15224](https://github.com/emqx/emqx/pull/15224) Dashboard 経由で外部スキーマレジストリを更新するとパスワードが `******` に上書きされる問題を修正し、更新時にパスワードを正しく保持するようにしました。

- [#15190](https://github.com/emqx/emqx/pull/15190) メッセージ変換で QoS とトピックのハードコード設定をサポートしました。

#### 可観測性

- [#15299](https://github.com/emqx/emqx/pull/15299) OpenTelemetry メトリクスエクスポート時の `badarg` エラーを修正しました。

#### テレメトリー

- [#15216](https://github.com/emqx/emqx/pull/15216) プラグインが有効な場合に `emqx_telemetry` プロセスがクラッシュする問題を修正しました。

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) ブラックリスト作成失敗時のエラーメッセージの書式を修正しました。

#### クラスター

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker` の RPC (`badrpc`) エラー処理を修正し、誤検知によるロック成功を防止しました。これによりクラスター展開でのロック状態不整合やデッドロックのリスクを低減します。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) CRL 配布ポイント（CDP） URL の連続失敗時にリフレッシュを停止し、エラーログの過剰出力を防止するように改善しました。

## 5.9.0

*リリース日: 2025-05-02*

EMQX 5.9.0 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#14721](https://github.com/emqx/emqx/pull/14721) 遅延パブリッシュのインターバル制限を 4294967 秒（約49.7日）から 42949670 秒（約497日）に変更しました。
- [#14595](https://github.com/emqx/emqx/pull/14595) `retainer.enable` フラグを非推奨化しました。Retainer はゾーン設定の `mqtt.retain_available` フラグに基づき自動で開始・停止します。

#### インストールとデプロイメント

- [#14930](https://github.com/emqx/emqx/pull/14930) macOS 15（Sequoia）向けパッケージのリリースを開始しました。
- [#14590](https://github.com/emqx/emqx/pull/14590) 評価ライセンス下のノードの最大アップタイムを1ヶ月に制限しました。アップタイム上限に達すると新規接続を拒否します。

#### ネームスペース

- [#14261](https://github.com/emqx/emqx/pull/14261) MQTT クライアント管理のネームスペース機能を強化しました。

  **新機能**：

  - ネームスペースクライアント認識：`tns` 属性を持つ MQTT クライアントをネームスペースクライアントとして扱います。
  - ネームスペースインデックス：クライアント ID インデックスに MQTT クライアントネームスペース（`tns`）を追加し、マルチテナンシーシナリオをサポートします。

  **API**：

  - ネームスペース一覧取得（ページネーション対応）：`/api/v5/mt/ns_list`
  - ネームスペース内クライアントセッション一覧取得（ページネーション対応）：`/api/v5/mt/:ns/client_list`
  - ネームスペース内ライブクライアントセッション数取得：`/api/v5/mt/:ns/client_count`

  **設定**：

  - ネームスペースごとのセッション上限：`multi_tenancy.default_max_sessions` 設定を追加しました。

  注：

  - 管理者ネームスペース（管理ユーザーグループ）はこのプルリクエストに含まれておらず、開発中です。

- [#14884](https://github.com/emqx/emqx/pull/14884) ネームスペース設定管理用 HTTP API を追加しました。

- [#14840](https://github.com/emqx/emqx/pull/14840) ネームスペース機能のクライアントおよびテナントレートリミッター設定用 HTTP API エンドポイントを追加しました。

#### 認証と認可

- [#14584](https://github.com/emqx/emqx/pull/14584) Dashboard 2FA（2要素認証）ログイン用の認証アプリを強化しました。

  LDAP 認可は JSON を使った拡張 ACL ルール形式をサポートし、クライアント情報に基づく認証時に LDAP から ACL ルールを取得可能になりました。これらはクライアントのメタデータにキャッシュされ、認可時の LDAP クエリを削減します。

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証・認可の外部リソース管理を最適化しました。以前は無効化された認証・認可プロバイダーに設定されたリソースに接続し続けることがありました。

#### データ統合

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 Tables アクションで Parquet 形式のデータファイル書き込みをサポートしました。

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producer コネクターおよびアクションのヘルスチェックにレート制限を追加し、AWS API クォータに準拠しクラスターの動作を改善しました。

  - `ListStreams` と `DescribeStream` へのヘルスチェック呼び出しをそれぞれコネクター単位で 5/s と 10/s に制限し、AWS のレート制限に合わせました。
  - 分散リミッターはクラスターのコアノードで調整され、一貫した制限を実施します。
  - ヘルスチェックがスロットルまたはタイムアウトした場合、コネクターやアクションは切断状態にせず、前の状態を保持します。

  また、`resource_opts.health_check_interval_jitter` を追加し、`resource_opts.health_check_interval` に一様ランダム遅延を加えて同一コネクター下の複数アクションの同時ヘルスチェックを減らします。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを 3.8.3.0 にアップグレードしました。EC2 インスタンスが適切な IAM 権限を持つ場合、Access Key Id と Secret Access Key を指定せずに S3 コネクターをセットアップ可能です。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT コネクターの `static_clientids` 設定を拡張し、各クライアント ID に対応するユーザー名とパスワードを指定可能にしました。これは Azure IoT Hub など、各デバイス（クライアント ID）に固有の認証情報が必要なシナリオに有用で、クラスター環境の複数ノード間での接続成功を支援します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP アクションの HTTP リクエストタイムアウトを `resource_opts.request_ttl` 設定で構成可能にしました。以前は固定 30 秒で調整不可でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できる API エンドポイントを追加しました。

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry 統合に HTTP 認証付きコレクターに対応する HTTP ヘッダー設定項目を追加しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS 認証のコネクターでリソースが `disconnected` とマークされた際に返される情報を改善しました。

- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary`、`GET /sources_summary` エンドポイントのレスポンス、および `GET /actions/:id` のフォールバックアクションに `tags` フィールドを追加しました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールが現在のシステム設定を HOCON 形式でエクスポートするようになり、パスワードやシークレットなどの機密情報は自動的にマスクされます。

### バグ修正

#### コア MQTT 機能

- [#15361](https://github.com/emqx/emqx/pull/15361) 不正な（長さ不足の）`User-Property` ペアを解析した際の `function_clause` エラーを修正しました。

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断済みクライアントの共有サブスクリプションの冗長なクリーンアップ処理を削除しました。これらは切断数が多い場合にクラッシュやグローバルブローカー状態の不整合を引き起こしていました。

- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket 接続のセッション有効期限切れ時に発生する警告ログとクラッシュを修正しました。この問題は最近の WebSocket パフォーマンス改善で導入されました。ブローカー容量には影響しませんが、以下のようなログが出力されていました：
  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK が非ゼロ理由コードで送信された後の切断時に `unclean_terminate` 警告ログを削除しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 大量の共有サブスクライバーが同時に切断された際にクラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積する競合状態を解消しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  以前はこれらのルールが内部的に `#` に変換されており、MQTT 仕様の制限により `$` プレフィックス付きトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`{allow|deny, all}` ルールが `$` プレフィックス付きトピックも正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーは API 経由で削除できず、API パスを破壊するためです。

  もし空ユーザーが存在し削除したい場合は、EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) 認可（authz）キャッシュをクライアント切断時に即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートの問題を修正しました。デフォルト値で EMQX をデプロイすると複数レプリカが起動し、1つを除くすべてのノードがクラッシュしていました。クラスタ展開は Commercial License が必要なため、チャートは単一レプリカをデフォルトにしました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 古いバージョン（5.9 未満）からのローリングアップグレード時のノード起動失敗を修正しました。

  以前の EMQX バージョン（5.9 未満）では ZIP タイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS 時間形式の 30 または 31 番目の 2 秒スロットに対応）が保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTP バージョンを 26.2.5.2 から 26.2.5.14 にアップグレードしました。これには EMQX に影響する OTP の TLS 関連修正が含まれます：
  - 証明書更新時の競合状態による TLS 接続クラッシュを修正。
  - RSASSA-PSS パラメーターで署名された RSA 証明書をサポート。以前はこれらの証明書が TLS ハンドシェイクで `bad_certificate` / `invalid_signature` エラーを引き起こしていました。
- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO 無効化後も関連ログが出力される問題を修正しました。
- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境で OIDC ログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  以前はこれらのルールが内部的に `#` に変換されており、MQTT 仕様の制限により `$` プレフィックス付きトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`{allow|deny, all}` ルールが `$` プレフィックス付きトピックも正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーは API 経由で削除できず、API パスを破壊するためです。

  もし空ユーザーが存在し削除したい場合は、EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) 認可（authz）キャッシュをクライアント切断時に即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使うクライアントが、`session_stepdown_request_exception` エラーと `calling_self` 理由でクラッシュする問題を修正しました。

  <details>
  <summary>エラーログ</summary>

  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### データ統合

- [#15616](https://github.com/emqx/emqx/pull/15616) Kafka 接続は、デフォルトのプローブトピックに対して `topic_authorization_failed` エラーが返された場合でも正常とみなすようになりました。

- [#15826](https://github.com/emqx/emqx/pull/15826) Kafka コンシューマーコネクターの ACL 制限下でのヘルスチェック動作を改善しました。以前は、Kafka ブローカーがヘルスチェックに使う内部 `____emqx_consumer_probe` コンシューマーグループへのアクセス権がない場合、ヘルスチェックが失敗していました。今回の修正で、Kafka ブローカーが「ACL denied」応答を返した場合でも接続は正常とみなされます。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDB ドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDB アクションで誤った書き込み構文を使った場合に発生する `function_clause` エラーも修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafka コンシューマーソースの追加失敗時（例：トピック ACL 拒否）に返される情報を充実させました。

- [#15910](https://github.com/emqx/emqx/pull/15910) コネクターのワーカープールで複数のワーカーが同時にクラッシュした場合に回復できない問題を修正しました。

  修正対象コネクター：

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

  `gun` と関連依存を 2.1.0 にアップグレードしました。

#### API

- [#15547](https://github.com/emqx/emqx/pull/15547) REST API で大きなボディ（例：10MB）の HTTP リクエスト処理に失敗する問題を修正しました。

- [#15797](https://github.com/emqx/emqx/pull/15797) EMQX 4.x との互換性向上のため、バッチパブリッシュ HTTP API（`/api/v5/publish/bulk`）に `encoding` パラメーターを `payload_encoding` のエイリアスとして再導入しました。これにより、EMQX v4 API を利用する既存統合がソフトウェア変更なしで継続利用可能です。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミット更新の動作を改善し、リスナー設定変更直後にバーストレートやレート閾値の変更が即時反映されるようにしました。以前は内部リミッター状態の一部が正しくリフレッシュされず、設定より厳しいレートリミットが適用されることがありました。

#### 可観測性

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTT ユーザー名に非 ASCII 文字が含まれる場合のネットワーク輻輳アラームメッセージでのクラッシュを修正しました。

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) 未定義のパケットフィールドを参照するクライアント情報オーバーライドテンプレートが原因で NATS ゲートウェイがクラッシュする問題を修正しました。システムは未定義アトムの代わりに空バイナリを返すようになりました。

## 5.10.0

*リリース日: 2025-06-10*

EMQX 5.10.0 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#15118](https://github.com/emqx/emqx/pull/15118) クライアントサブスクリプションごとに許可される最大 QoS レベルを制御する新設定 `mqtt.subscription_max_qos_rules` を追加しました。これにより、特定トピックにマッチするルールに基づき SUBSCRIBE パケットの QoS 要求を制限可能です。現在はトピックに基づく限定的なマッチングルール（述語）のみサポートしています。
- [#15246](https://github.com/emqx/emqx/pull/15246) WebSocket 接続のパフォーマンスとリソース消費を改善しました。
    * 合成ベンチマークで 1対1 MQTT メッセージング性能を測定し、CPU 使用率を約 20% 削減、メモリ消費も若干低減。
    * リスナー全体の接続制限が有効な場合の接続セットアップ効率を改善し、多数接続を管理するノードで効果的。

#### デプロイメント

- [#14791](https://github.com/emqx/emqx/pull/14791) Helm チャートの EMQX StatefulSet にカスタムアノテーションをサポートし、ConfigMap や Secret 変更時の自動 Pod 再起動を可能にしました。これにより Kubernetes 上の EMQX 管理の自動化と信頼性が向上します。

#### アクセス制御

- [#15250](https://github.com/emqx/emqx/pull/15250) LDAP バインド認証で LDAP エントリの `is_superuser` フラグを正しく抽出するよう改善しました。
  以前は LDAP エントリに有効な `isSuperuser` 属性があっても常に `false` に設定されていました。
- [#15249](https://github.com/emqx/emqx/pull/15249) LDAP 認証・認可を改善しました。

  * LDAP の `filter`/`base_dn` 設定に対するバリデーションを追加。
  * 変数展開の問題を修正。

#### ルールエンジン

- [#15001](https://github.com/emqx/emqx/pull/15001) ルールエンジン SQL に AI サービスを使う `ai_completion` 関数を追加しました。

- [#15201](https://github.com/emqx/emqx/pull/15201) AI 補完プロバイダー設定に `base_url` オプションを追加しました。

- [#15188](https://github.com/emqx/emqx/pull/15188) ルールイベントトピックにネームスペースを追加しました。

  | 旧イベントトピック                      | 新イベントトピック                       |
  | :-------------------------------------- | :-------------------------------------- |
  | `$events/client_connected`              | `$events/client/connected`              |
  | `$events/client_disconnected`           | `$events/client/disconnected`           |
  | `$events/client_connack`                | `$events/client/connack`                |
  | `$events/client_check_authz_complete`   | `$events/auth/check_authz_complete`     |
  | `$events/client_check_authn_complete`   | `$events/auth/check_authn_complete`     |
  | `$events/session_subscribed`            | `$events/session/subscribed`            |
  | `$events/session_unsubscribed`          | `$events/session/unsubscribed`          |
  | `$events/message_delivered`             | `$events/message/delivered`             |
  | `$events/message_acked`                 | `$events/message/acked`                 |
  | `$events/message_dropped`               | `$events/message/dropped`               |
  | `$events/delivery_dropped`              | `$events/message/delivery_dropped`      |
  | `$events/message_transformation_failed` | `$events/message_transformation/failed` |
  | `$events/schema_validation_failed`      | `$events/schema_validation/failed`      |

  旧イベントトピックは後方互換のため残されています。

- [#15175](https://github.com/emqx/emqx/pull/15175) ルールエンジンでワイルドカードを使ったイベントトピックマッチングをサポートしました。これにより `$events/#`、`$events/sys/+` など複数イベントの一括マッチが可能です。

#### スマートデータハブ

- [#15174](https://github.com/emqx/emqx/pull/15174) スキーマレジストリ用の Protobuf ソースファイルバンドルアップロードをサポートしました。

  例として、Protobuf ソースファイルバンドルが `/tmp/bundle.tar.gz` にあり、以下のファイル構成で `a.proto` がルートスキーマファイルの場合：

  ```
  .
  ├── a.proto
  ├── c.proto
  └── nested
      └── b.proto
  ```

  HTTP API でこのバンドルを使い新規スキーマを作成する例：

  ```sh
  curl -v http://127.0.0.1:18083/api/v5/schema_registry_protobuf/bundle \
    -XPOST \
    -H "Authorization: Bearer xxxx" \
    -F bundle=@/tmp/bundle.tar.gz \
    -F name=my_cool_schema \
    -F root_proto_file=a.proto
  ```

#### データ統合

- [#15248](https://github.com/emqx/emqx/pull/15248) EMQX は [Doris](https://doris.apache.org/) とのデータ統合をサポートし、SQL 文によるデータ書き込みを可能にしました。

- [#15218](https://github.com/emqx/emqx/pull/15218) Amazon MSK（Managed Streaming for Apache Kafka）接続時の Kafka Producer および Consumer コネクターで IAM 認証をサポートしました。EMQX が AWS EC2 上で動作する場合、AWS SDK を使って Kafka クライアント用の OAuth ベアラートークンを生成します。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflake コネクターでパスワードの代わりに秘密鍵ファイルパスを指定するサポートを追加しました。

  ユーザーはパスワード、秘密鍵、または両方なし（`/etc/odbc.ini` で設定）を選択できます。

- [#14983](https://github.com/emqx/emqx/pull/14983) EMQX は S3Tables とのデータ統合をサポートしました。

  **現在の制限**：
  - [S3Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) カタログのみ対応（テーブルデータとメタデータは S3 に存在する必要あり）。
  - [Iceberg テーブルフォーマットバージョン 2](https://iceberg.apache.org/spec/#version-2-row-level-deletes) のみ対応。
  - サポートされるパーティション変換関数は以下のみ：
    - `identity`
    - `void`
    - `bucket[N]`
  - データファイルは [Avro](https://avro.apache.org/docs/1.12.0/specification/) 形式のみ。

- [#15331](https://github.com/emqx/emqx/pull/15331) InfluxDB アクションで、`WriteSyntax` の `timestamp` が空白かつルールにタイムスタンプフィールドがない場合にラインプロトコル変換が失敗する問題を修正しました。現在はシステムの現在ミリ秒値を使い、ミリ秒精度を強制します。

- [#15348](https://github.com/emqx/emqx/pull/15348) SSL クライアントの `middlebox_comp_mode` を設定可能にしました。以前は TLS 1.3 接続で常に有効（`true`）でしたが、デフォルトは互換性維持のため `true` のままです。

  TLS 失敗時に `unexpected_message, TLS client: In state hello_retry_middlebox_assert ...` のようなエラーが出る稀なケースでは、`middlebox_comp_mode` を `false` に設定してください。

#### マルチテナンシー

- [#15253](https://github.com/emqx/emqx/pull/15253) 2つの新しいマルチテナンシー API を追加しました：`GET /mt/ns_list_details` と `GET /mt/ns_list_managed_details`。既存の対応 API と同様に動作しますが、名前空間名に加え関連メタデータも返します。

- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシー管理用に名前空間を一括削除する `DELETE /mt/bulk_delete_ns` API を追加しました。

#### CLI

- [#15158](https://github.com/emqx/emqx/pull/15158) 既存設定から指定したキー経路 `x.y.z` を削除する新しい CLI コマンド `emqx ctl conf remove x.y.z` を追加しました。

#### ゲートウェイ

- [#15138](https://github.com/emqx/emqx/pull/15138) TCP/TLS、WS/WSS トランスポートプロトコルで NATS クライアント接続を受け入れる NATS ゲートウェイを導入しました。

  例えば、NATS メッセージを以下のように MQTT メッセージに変換し、トピック `sub/t`、ペイロード `hello` として EMQX のルールエンジンやデータ統合など既存機能とシームレスに統合可能です：

  ```
  PUB sub.t 5
  hello
  ```

#### Durable Storage

- [#15043](https://github.com/emqx/emqx/pull/15043) DS Raft バックエンドに基本的なメトリクスを計測するインストルメンテーションを追加し、クラスター状態、データベース概要、シャードレプリケーション、レプリカ遷移の可視化を可能にしました。

### バグ修正

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) ブラックリスト作成失敗時のエラーメッセージ書式を修正しました。

#### クラスター

- [#15304](https://github.com/emqx/emqx/pull/15304) `static` 発見戦略使用時にレプリカントノードがコアノードを正しく発見できない問題を修正しました。

  以前は `static_seeds` リストに明示的に含まれないコアノードをレプリカントが無視していました。これによりクラスターのビュー不整合や負荷不均衡が発生していました。

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker` の RPC (`badrpc`) エラー処理を修正し、誤検知によるロック成功を防止しました。これによりクラスター展開でのロック状態不整合やデッドロックのリスクを低減します。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) CRL 配布ポイント（CDP） URL の連続失敗時にリフレッシュを停止し、エラーログの過剰出力を防止するように改善しました。

---

（以下、5.8.11 以降のリリースノートは元の英語テキストのまま保持してください）
