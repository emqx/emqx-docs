# EMQX Enterprise Version 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboard のユーザーアカウント管理エンドポイントへの API キーアクセスを制限しました。

  以前は、`administrator` ロールを持つ API キーが HTTP Basic 認証を介して Dashboard のユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出せました。これにより、API キーが他の Dashboard ユーザーの MFA をリセットまたは無効化したり、他の Dashboard ユーザーのパスワードを変更したりでき、人間の Dashboard セッションと機械の API キーの分離が意図せず回避されていました。

  これらのエンドポイントは、API キー経由のアクセス時に `401 API_KEY_NOT_ALLOW` を返すようになり、既存の `/users`、`/users/:username`、`/logout`、および `/api_key` への API キーアクセスをブロックするポリシーと整合します。Dashboard ユーザーは引き続き Dashboard UI からベアラートークン（JWT）セッションを使って自身の MFA とパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイント経由での Dashboard アカウントおよび API キーのエクスポート・インポートを API キーから制限しました。

  API キーで呼び出された `POST /data/export` は、生成されるアーカイブから `dashboard_users` と `api_keys` の mnesia テーブルセットを静かに除外します。API キーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれている場合、`403 FORBIDDEN` を返します。

  Dashboard のベアラートークン（ログイン）呼び出しは影響を受けず、Dashboard ユーザーおよび API キーを含む完全なデータベースのバックアップと復元が引き続き可能です。

  これは特権昇格のギャップを解消します。既存の `/users` および `/api_key` エンドポイントは API キーによる Dashboard ログイン資格情報および API キー記録へのアクセスを拒否していますが、API キー保持者はデータバックアップエンドポイントを経由してこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証されていない `GET /status?format=json` レスポンスから EMQX リリースバージョン（`rel_vsn`）を削除し、未認証呼び出し元にブローカーのバージョン情報が漏れないようにしました。バージョン情報は認証済みのノード情報 API で引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされた tarball のパストラバーサルに対してプラグインインストールエンドポイントを強化しました。インストールパスは、プラグインインストールディレクトリ外に解決されるエントリを含む tarball の展開を拒否します。

  これは多層防御です：このエンドポイントは既に Dashboard ログイン／API キー認証と明示的な `emqx ctl plugins allow <name-vsn>` 許可リストエントリで制御されており、未認証または未許可の呼び出し元はこのコードパスに到達できません。新しいチェックは、両方のゲートが意図的に開かれてプラグインをアップロードする場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install`（およびそれをラップする Dashboard アップロード）によるプラグインインストール成功時に、アップロードを許可したクラスタ全体の `emqx ctl plugins allow <name-vsn>` エントリを即座に取り消すようにしました。同じ許可が後続の（異なる可能性のある）tarball に再利用されるのを防ぎます。5分間の TTL は引き続き適用されますが、この変更により一般的なパスでのウィンドウを早期に閉じます。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトのプラグインパッケージに `.sha256` チェックサムサイドカーを公開し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式 EMQX Docker イメージのセキュリティ強化：

  - ランタイムイメージビルド時に Debian セキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取得。
  - 未使用の `libgnutls30t64` パッケージを削除。EMQX は Erlang/OTP 経由で OpenSSL を使用し、GnuTLS はリンクしないため、`curl` の推移的依存としてのみ存在し、スキャナーレポートに現れていました。
  - Debian の `curl` パッケージを、[stunnel/static-curl](https://github.com/stunnel/static-curl) の静的リンク済みバイナリ（OpenSSL、HTTP/2、HTTP/3対応、RTMP・GnuTLS非対応）に置換。Debian パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入していましたが、静的バイナリはこれを回避しつつ、`curl` を呼ぶコンテナのヘルスチェックは変更なく動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2 の SSL Common Name / Subject をクライアント識別に入れる前にサニタイズしました。

  `proxy_protocol = true` 設定のリスナーでは、PROXY-Protocol SSL TLV バイトに ASCII 制御文字が含まれる接続を拒否します（MQTT で取り込む `clientid` / `username` / `password` で既に拒否されているバイトクラスと同じ）。これにより、攻撃者制御のバイトが `${cert_common_name}` および `${cert_subject}` テンプレート経由で HTTP 認証・認可・ルールエンジンのヘッダー値に密輸されるのを防ぎます。

  追加の防御層として、HTTP 認証および認可クライアントは、レンダリングされたヘッダー名または値に CR、LF、NUL バイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTT の `clientid` / `username` / `password` に適用されているバイトクラスチェックを、`ClientInfo` および HTTP リクエストテンプレートにフィードされる他のフィールドにも拡張しました：

  - `peersni`（TLS Server Name Indication。PROXY-Protocol v2 の `authority` TLV からも受け入れ）は接続取り込み境界で検証されます。制御文字があると接続拒否し警告ログを出します。
  - `mqtt.client_attrs_init` の Variform 式で生成されるクライアント属性値は制御文字を含む場合は破棄（警告付き）され、`${client_attrs.tns}` のようなテンプレートが注入バイトを下流に運べないようにします。
  - HTTP アクション／ブリッジコネクターのヘッダー描画は、レンダリングされた名前または値に NUL、CR、LF を含むヘッダーを破棄します。

#### クラスター

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期機構を導入しました。ルーティングテーブルのスキーマバージョンは `v3` に更新され、`v2` との後方互換性を提供します。

  スキーマ v3 では、各ノード（コアまたはレプリカント）が自身に向かうルーティングテーブルエントリの完全な所有権を持ち、ピアノードはこれらのエントリに対して読み取り専用アクセスのみを持ちます。これにより、分割クラスターのパーティション耐性が向上し、ピアノードが他ノードの代理でルーティングテーブルを変更できなくなります。また、レプリカントノードの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3 対応ノードが v2 のみ対応クラスターに参加すると、互換性のために v2 を使い続けます。クラスター内に互換モードのノードがあれば新規ノードも互換モードを使います。クラスターを v3 に切り替えるにはアップグレード後にクラスター全体を再起動してください。自動切り替えを防ぐには `broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスターが v3 に切り替わるとローリングダウングレードは不可能です。

  ノードの現在のルーティングスキーマバージョンを確認するには：

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) 分布ポートの Erlang inet ポートオプション設定をサポートし、デフォルトのバッファサイズを 1 MB に設定しました。

  以前は Erlang 分布ポートは非常に小さいデフォルトバッファ（1460 バイト、プラットフォームによっては約 9 KB）を使っており、分布ポートバッファ（`+zdbbl`）を大きく設定しても（例：32 MB）性能ボトルネックが発生していました。これによりクラスター通信の信頼性が低下し、`erpc timeout` エラーや Mnesia トランザクションの混雑、多コアノードサポートの劣化が起きていました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) ノードごとのルートテーブルエントリ数をエクスポートする Prometheus メトリクス `emqx_routes_count` と `emqx_routes_max` を追加しました。EMQX v4 の `emqx_routes_count` メトリクスと同様です。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon` をデフォルトでシステム全体のメモリ統計のみ収集するよう設定し、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria 統計の誤った繰り返しクエリを回避し、Prometheus メトリクス収集のオーバーヘッドを削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報を Prometheus ゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスタ全体のライセンス整合性をノード単位の CLI チェックなしにアラート可能にしました。

  タイムスタンプはライセンス発行／有効期限日の UTC 深夜の Unix エポック秒です。ライセンスが利用できない場合は全てのメトリクスが `0` を出力します。アラートルールでは `emqx_license_expiry_at == 0` を「利用不可」シグナルとして使ってください（`max_sessions == 0` は有効期限切れのトライアルライセンスを示す場合があります）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSON データと JWT トークンからドット区切りキーで値を抽出する Variform 式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  `json_value` は JSON バイナリ文字列からドット区切りパスでネストされた構造を辿って値を抽出します。`jwt_value` は JWT トークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例：`username` が JSON オブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセス可能。`password` がカスタムクレームを持つ JWT なら `jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセス可能です。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) API キーと Dashboard ログインユーザーに対するスコープベースの細粒度アクセス制御を導入しました。

  API キーは OpenAPI タグ由来のスコープで特定の API パスカテゴリに制限可能です。スコープなしのキーは完全アクセス（後方互換）を保持し、空のスコープリストは全てのスコープ付きパスを拒否します。`publisher` API キーロールは `[publish]` のみ許可されます。

  Dashboard ログインユーザーは既存のロールチェックに加えオプションの `scopes` フィールドを持ちます。4つの新スコープは Dashboard 専用エンドポイントをカバーし、`user_management`、`sso_management`、`api_key_management` は管理者専用、`mfa_management` は強制 MFA 免除の自己管理用で任意ロールが利用可能です。API キーはこれらログイン専用スコープを持てません。

  2つのカタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` はベアラー認証ユーザーがアクセス可能です。`GET /users`、`POST /users`、`PUT /users/:username` のレスポンスにも `scopes` フィールドが含まれ、明示的に設定されていない場合はロールデフォルトのスコープリストが返されます。

  振る舞いの変更点：

  - `dashboard.default_username` ユーザーはブレークグラスアカウントとして保護されます。削除不可、管理者から降格不可、`scopes` フィールド設定不可で、`description` のみ変更可能です。これにより他の管理者がスコープを失った場合でもオペレーターが管理者アクセスを常に保持します。
  - 自己レコードのセルフサービスはスコープを尊重します。パスワード変更や MFA セルフエンドポイントのみスコープチェックをバイパスし、`PUT /users/:self` など他の操作はユーザーのスコープに従います。
  - `PUT /users/:username` と `PUT /api_key/:name` はリクエストボディに `scopes` フィールドがない場合、永続化されたスコープに対してロール変更を検証します。ユーザー降格や API キーロール変更はスコープと互換性がなければ拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）用にバックエンドごとの `force_mfa` オプションを追加しました。

  有効時、SSO ユーザーは IDP 側 MFA 設定に関わらず Dashboard トークン取得前に TOTP MFA 設定または検証を完了する必要があります。MFA 状態は `not_configured`（設定強制）、`enabled`（検証必須）、`admin_disabled`（MFA スキップ）の3つをサポートします。新しい API エンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` が MFA フローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行後5分で期限切れになり、パッケージの SHA-256 ハッシュにピン留め可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>` は64文字の小文字16進ダイジェストを受け入れ、内容が一致しないアップロードは `403 Forbidden` で拒否されます。`sha256:` 引数を省略した場合は `<name-vsn>.tar.gz` という名前の任意ペイロードを受け入れる従来の動作を維持します。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808 ゲートウェイのダウンリンクメッセージでカスタム `msg_sn` をサポートしました。

  ダウンリンク MQTT メッセージペイロードのヘッダーに `msg_sn` フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケースのメッセージシーケンスを制御可能です。

  また、JT/T 808 ゲートウェイの `string_encoding` がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。以前は `string_encoding` 設定（例：`gbk`）はアップリンクメッセージの解析にのみ使われていました。現在は `string_encoding: gbk` 設定時、アップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）が正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafka ソースのポーリング動作を改善し、フェッチリクエストが空バッチを即座に返すのではなくデータを短時間待つようにしました。これにより不必要なポーリング遅延が減り、Kafka コンシューマーが新規レコードをより安定して受信可能になります。

- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erl を 1.1.13 から 1.1.18 にアップグレードし、InfluxDB コネクターに `ping_with_auth` オプション（デフォルト false）を追加しました。これにより、一部の InfluxDB 互換サービスで認証情報を含むヘルスチェックが可能になります。

#### デプロイメント

- [#16853](https://github.com/emqx/emqx/pull/16853) v5 ライセンスパーサーを v6 ライセンスキーに対してフォワード互換にしました。

### バグ修正

#### コア MQTT 機能

- [#17097](https://github.com/emqx/emqx/pull/17097) `retainer.enable` を実際のランタイムスイッチとして復元しました。これにより、`mqtt.retain_available` に頼らず、MQTT の保持メッセージプロトコルサポートを有効にしつつ保持メッセージストレージを無効化できます。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッションテイクオーバーや破棄シナリオで `disconnected_at` が `connected_at` より遅くなるタイムスタンプ順序問題を修正しました。

  以前は `disconnected_at` が新セッションの `connected_at` 設定後の `ensure_disconnected` で遅れて記録されていました。これにより `disconnected_at > connected_at` となり、外部のクライアントプレゼンス状態追跡が困難でした。

  修正ではテイクオーバー開始時または破棄受信時に即座に `disconnected_at` を記録し、新セッションの `connected_at` より遅くならないようにします。これにより外部プレゼンス追跡システムで正しいタイムスタンプ順序が保証されます。

  注：これらイベントが異なるクラスター ノードから発行される場合、観測される順序はノード間時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  以前はサブスクリプション一覧取得が一部クライアントで失敗し出力が無くなることがありました。修正後は通常のサブスクリプションと共有サブスクリプションの両方で確実に動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) Dashboard と REST API のチャネル情報（`mqueue_len`、`inflight_cnt`）がセッションテイクオーバー再生完了直後に即時更新されるよう修正しました。以前は次の15秒統計更新まで遅延していました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) ドキュメントにはあるが実際のイベントデータに存在しなかった `$events/client/connack` ルールイベントの `connected_at` フィールドを追加しました。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なルールメタデータのタイムスタンプを無視するようにしました。

  以前は `metadata.created_at` や `metadata.last_modified_at` に日付文字列などの非整数値があると、API でルール一覧や取得時に内部エラーで失敗していました。

  修正後は無効なメタデータタイムスタンプ値を無視し、通常の生成タイムスタンプにフォールバックするため、破損したメタデータがあってもルール API レスポンスが利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQ コネクター／アクション／ソースで、接続やチャネルプロセスが予期せず死んだ場合に再起動なしで自己回復しない問題を修正しました。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定インポート時のクラッシュを修正しました。

  以前は一括インポート時に以下のようなクラッシュが発生していました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storage アクションの集約モードで、コンテナに多数の BLOB がある場合にヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTP および GCP PubSub アクションで、理由が `closing` の一時的な接続エラーを回復可能として扱い、ログノイズを低減しました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTT ソースで、コネクターが `clean_start = false` を使い、メッセージを含むセッションを持つブローカーに再接続した場合に、メッセージがルールアクションをトリガーしなかった問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDB コネクター／アクションで、`write_syntax` リテラルや MQTT ペイロードから書き込む際に Unicode テキストを保持するよう修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQL コネクターでプリペアドステートメントが無効な場合のクエリ実行を修正しました。以前は同時クエリが混在してエラーを起こしていました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQ コネクターの分離を修正しました。設定ミスや到達不能な RocketMQ コネクターが同一ノードの他の RocketMQ コネクターを不安定化させなくなりました。以前は到達不能なブローカーのコネクターが共有クライアントスーパーバイザーを最大60秒停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard 操作がハングしていました。

  TCP/TLS 接続タイムアウトのデフォルトも 60 秒から 10 秒に短縮され、設定ミスサーバーが素早く失敗として検出されます。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時に MongoDB プロセスへのタイムアウト呼び出しが回復不能エラーとして扱われて再試行されなかった問題を修正しました。これによりメッセージは再試行されます。

  発生時のログ例：

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis Sentinel コネクターで、Redis データノードと Sentinel ノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquet ファイルに必須キーが `undefined` または `null` のオブジェクトを書き込むと破損ファイルが生成される問題を修正し、エラーを発生させるようにしました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafka クライアントライブラリをアップグレードしました：`brod` 4.5.2 → 4.5.4、`wolff` 4.1.7 → 4.1.10。

  Kafka プロデューサーとコンシューマー統合に以下の修正を含みます：

  - SASL 再認証中の接続レースコンディションを修正し、キューイングされたプロデュース要求のドロップや `sync` プロデュース呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返される問題を解消。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQ クライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサーリクエストのメモリ増加問題を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTT クライアント依存を `1.14.6` から `1.15.1` にアップグレードしました。

  MQTT ブリッジ、MQTT ソース、その他アウトバウンド MQTT 接続を使うコネクターに以下のユーザー向け改善をもたらします：

  - キープアライブタイマーから pingresp タイムアウトを追跡し、pingresp 処理を設定された `keepalive` 間隔に同期。
  - QUIC：ピアの `recv` 中止後、両方向を切断せず送信方向のみ中止し、半クローズされた QUIC ストリームの保留送信が静かにドロップされなくなりました。

#### クラスター

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスター回復時間を改善しました。

  組み込み Mria データベース管理システムは、トランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTP を 27.3.4.2-6 から 27.3.4.2-7 にアップグレードしました。

  これにより、起動中にノードがネットワークパーティションを経験した場合に MQTT ルーティングテーブルの不整合を引き起こすレースコンディションを解消します。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTP を 27.3.4.2-8 にアップグレードしました。これがないと、ノードがクラスターに接続されていない場合に Mria アプリの起動が EMQX 起動中にハングすることがあります。

- [#17220](https://github.com/emqx/emqx/pull/17220) `bin/emqx` と `bin/emqx_ctl` の呼び出しが稼働中ブローカーで `nodeup`/`nodedown` イベントをトリガーし、ブローカーログに誤解を招く `cm_registry_node_down` 警告が出る問題を修正しました。これらスクリプトが起動する一時ヘルパーノードは意図通り Erlang の隠しノードとして登録されます。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスター回復を改善しました。

  以前はレプリカントノードに接続されたクライアントの一部がグローバルレジストリから失われ、セッションテイクオーバー時に不整合な振る舞いや Dashboard 表示の誤情報を引き起こしていました。

  この修正では、ネットワークパーティションが回復した際に既存クライアントを再登録するバックグラウンドプロセスを追加し、グローバルレジストリ再構築中に「Broker is recovering after a network partition」アラームを発報します。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動回復できる新しい自動修復アルゴリズムを導入し、ネットワークパーティションからのクラスター回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれている場合のクラスター設定インポート失敗を修正しました。読み取り専用の設定ルート（`node`、`rpc`）は事前スキーマチェック前に除外され、実行中ノードの値が検証に使われます。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスター化されたノードで効果的な設定は同じだが生の設定表現が異なる場合に発生する騒々しく誤解を招く `emqx ctl conf cluster_sync status` 診断を修正しました。

  実際の設定変更に対応しない生の表現差分を抑制しつつ、効果的な設定が不整合な場合は警告を出します。また、あるノードにのみ存在し他にない生の設定キーがある場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスターがネットワークパーティションを経験した際に発生するグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く `emqx ctl conf cluster_sync status` 警告を修正しました。

  以前はデータインポートや起動時設定読み込みで、同一のアクション、ソース、ブリッジ、ルールメタデータであってもノード間で `created_at` や `last_modified_at` が異なる場合がありました。コマンドはこれらタイムスタンプのみの差分を無視し、実際の設定差分は報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) 反応しないターゲットクラスターへのルート複製がスタックした際の Cluster Link 応答性を改善し、そのような Cluster Link の削除がより早く完了するようにしました。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後のグローバルセッションレジストリリークを修正しました。これにより同一クライアント ID の重複または古いエントリが残る問題を解消します。

  廃棄およびテイクオーバーキック RPC ハンドラは、対象プロセスが生存しない場合もレジストリ行を削除し、接続パスの登録スロットルは墓石行（ローカルチャネル状態なし）を認識して新規接続を無期限にブロックせずに回収します。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) `emqx_crl_cache:evict/1` が内部 URL 状態を完全にクリアしなかった CRL キャッシュの回帰を修正しました。削除後、同一 CRL URL は次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとの HTTP フェッチの繰り返しを回避します。

- [#17012](https://github.com/emqx/emqx/pull/17012) パスワードなしの CONNECT パケットでパスワードベース認証バックエンドが認証チェーンを継続するよう修正しました。以前はパスワードなしクライアント接続時に最初のパスワードベース認証器（組み込み DB、MySQL、PostgreSQL、MongoDB、Redis、LDAP）がエラーを返し、後続認証器が試行されませんでした。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSO ログインで、ID プロバイダーが `+json` 構造化構文サフィックスを持つ `Content-Type`（例：`application/jwk-set+json; charset=utf-8`）の JWKS レスポンスを返す場合に `provider_not_ready` で失敗する問題を修正しました。これらのレスポンスは有効な JWKS コンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URL エンコードされたユーザー名（メールアドレスなど）を持つ SSO ユーザーの Dashboard RBAC チェックを修正し、`force_mfa` 無効時のビューワー自身による MFA 無効化リクエストが正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PID がログデータフィールドとして出力されるようにしました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 以前は特定のレースコンディションで以下のような長く難解なログが出力されていました：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQX はより意味のある情報をログに出力し、問題のデバッグを支援します。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開でのプラグイン起動時のノイズワーニングを削減しました。

  クラスター設定同期時にローカルノードからプラグイン設定を取得しようとせず、起動時の `config_not_found_on_node` 警告の繰り返しを回避します。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答受信時に警告ログを出すようにしました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が `emsgsize`（受信パケットが `mqtt.max_packet_size` を超過）でクライアント接続終了時のログレベルを info から warning に変更しました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用報告を改善しました。

  ブローカーは cgroup v2、cgroup v1、ホストの `/proc/meminfo` のメモリ読み取り値を比較し、最も制約の厳しい値を使用します。最小の非ゼロ合計値が勝ち、合計が同じ場合は使用率の大きい方を採用します。

  これにより以下の誤解を招く読み取りを修正します：

  - コンテナに厳しい cgroup メモリ制限があるがホストビューは高使用率を示す場合（例：cgroup 制限は <10% なのにホストは >70%）、またはその逆。
  - メモリ制限が設定されていない cgroup がマウントされている場合、報告される使用率が約 0% に崩壊する。

  過負荷保護の閾値や `Memory used` メトリクスは実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace` がトレースフィルタータイプとして `ruleid` を受け付けるように修正しました。以前は `emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する `trace add ...` 形式）が CLI 引数パーサーに `ruleid` フィルターがなく汎用エラーとなっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### デプロイメント

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) `GET /monitor_current` HTTP API に新たに `rules_matched` と `actions_executed` のメトリクスおよび対応するレートを追加しました。これらはそれぞれマッチしたルール数とアクション実行率（成功＋失敗）を追跡します。

- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API 経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQX は TLS 1.3 のステートレスセッションチケットによるセッション再開をサポートしました。これによりクライアントはサーバー側のセッション状態保存なしに TLS セッションを再開可能です。

  **設定**

  - **ノードレベル**：`node.tls_stateless_tickets_seed`

    TLS 1.3 ステートレスセッションチケット生成に使う秘密鍵シード。

  - **リスナーレベル**：`listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3 セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - `node.tls_stateless_tickets_seed` が設定（空でない）され、リスナーの SSL オプションで `session_tickets` が有効な場合にのみセッションチケットが生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出力されます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808 ゲートウェイに `jt808.frame.parse_unknown_message` 設定オプションを追加し、未知のメッセージ ID を持つメッセージを解析し透過的に転送可能にしました。

- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808 プロトコル 2019 をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合に IoTDB テーブルモデルのサポートを追加しました。

### バグ修正

#### コア MQTT 機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエストレスポンス情報プロパティ処理時の型不一致による MQTT v5 接続のクラッシュを修正しました。

- [#16514](https://github.com/emqx/emqx/pull/16514) クライアントが広告した `Maximum-Packet-Size` を超えるブローカーメッセージ受信時に WebSocket 接続がクラッシュするバグを修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に `undefined` を返す問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これは EMQX v4 との後方互換性修正です。これらの関数は EMQX v5 以降ではドキュメント化されていません。推奨される使用法は、ルール評価コンテキストのフィールドを直接参照することです。例：`SELECT clientid ...`（`SELECT clientid()` ではなく）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックで現在の EMQX ノードに割り当てられたパーティションのみリーダー接続性を検証するようにしました。これにより不要なアイドル接続と誤警報を防止します。

  以前は Kafka コンシューマーコネクターが全パーティションのリーダー接続性をチェックしていました。クラスター展開時、各ノードはパーティションのサブセットのみ所有し、割り当てられていないパーティションリーダーへの接続がアイドル状態になります。Kafka はアイドル接続をタイムアウト（デフォルト10分）で切断するため、誤警報が発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafka リクエストタイムアウトをメタデータリクエストタイムアウトの少なくとも2倍（最小30秒）に自動設定するようにし、メタデータリクエストが予想より長くかかる場合の不要な再接続やリトライを減らしました。特にメタデータリクエストタイムアウトが小さい値に設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続性テストやコネクター停止時のタイムアウトを引き起こすレースコンディションを修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) IoTDB コネクターの REST API ドライバー使用時に、ヘルスチェックでクライアント認証情報が検証されていなかった問題を修正しました。ヘルスチェックは軽量な no-op クエリを送信し、誤設定の認証情報を早期に検出します。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsar クライアントを 2.1.2 にアップグレードしました。

  Pulsar プロデューサーアクションの `batch_size` が `1` に設定されている場合、単一メッセージをエンコードし、単一要素バッチではなくなります。これにより、Key Share 戦略を使ってコンシューマーが負荷を共有可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTT ソースのコネクターが再接続した際にトピックが再サブスクライブされず、コネクター再起動までソースが動作停止する問題を修正しました。再接続時に自動的に再サブスクライブされます。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS 接続失敗問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードのアクションで、コネクターが複数回のヘルスチェック失敗後に切断された場合にフォールバックアクションが2回トリガーされる問題を修正しました。

#### クラスター

- [#16269](https://github.com/emqx/emqx/pull/16269) Cluster Link ルート複製プロトコルのリカバリーシーケンスで、リモート側が再ブートストラップを必要としているのに誤ってスキップされていた問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立した Cluster Link が設定され、一部が長期間ダウンした場合に、古いルート複製状態のクリーンアップ中に内部ルーティングテーブルから生きたルートが誤って削除される問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc` を `3.5.1` にアップグレードしました。

  これ以前は、ピアノードが到達不能な場合に接続タイムアウトによるクラッシュログの長いテールが発生していました。新バージョンは長いテールをなくし、クラッシュログをより読みやすい `error` ログに変換し、頻発する `"failed_to_connect_server"` ログもスロットリングしてログスパムを防止します。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスターの自動クリーン手順の堅牢性を改善しました。以前はノード起動時に自動クリーン機能を無効化すると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSO バックエンドに `idp_signs_envelopes` と `idp_signs_assertions` オプションを追加し、IDP が署名する場合の署名検証を制御可能にしました。両オプションは後方互換のためデフォルト `false` で、IDP が SAML レスポンスに署名する場合は明示的に有効化が必要です。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0 未満からのアップグレード後に Multi-Factor Authentication（MFA）が有効化できなかった問題を修正しました。これはログインユーザーデータベースレコードの互換性問題によるものです。

- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC 発行者 URL が保存時に末尾スラッシュ付きに自動正規化され、OIDC プロバイダーのディスカバリードキュメントが末尾スラッシュなしの発行者を返す場合に不一致エラーとなる問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) リソース例外発生時のログ量を削減しました。これらのログはスロットリングされ、一部の大きな項目はマスクされます。

- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc` エラーのログフォーマッタクラッシュを修正しました。以前は `gen_rpc` が特定のエラーメッセージ（例：送信タイムアウト）をログ出力すると EMQX がクラッシュしていました。フォーマッタはこれらを正しく処理しクラッシュしなくなりました。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) CAN バス ID パラメーター（0x0110～0x01FF）に対する JT/T 808 ゲートウェイのパラメーター設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正しました。これらは JSON で文字列型ではなく base64 エンコードされた BYTE[8] 型を使うべきです。

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS 上の接続モードで動作する CoAP ゲートウェイを修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808 ゲートウェイに GBK 文字エンコーディングサポートを追加しました。

  JT/T 808 プロトコルは STRING 型フィールドに GBK エンコーディングを指定しています。新しい `frame.string_encoding` 設定オプション：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）
  - `gbk`：デバイスからの GBK エンコード文字列を MQTT 用に UTF-8 に変換し、MQTT からデバイスへは UTF-8 から GBK に変換

  これはナンバープレート、運転手名、テキストメッセージ、エリア名、クライアントパラメーターなどの文字列フィールドに影響します。MQTT ペイロードは常に UTF-8 エンコードを使用します。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージのドロップに関するログ（`buffer_worker_dropped_expired_messages`）を警告レベルで出力し、リソース ID ごとにスロットリングするようにしました。これにより特定の外部リソースが受信メッセージレートに追いついていない場合の特定が容易になります。

- [#16206](https://github.com/emqx/emqx/pull/16206) Kafka Producer コネクターに `allow_auto_topic_creation` 設定オプションを追加しました。有効時、クライアントがメタデータフェッチ要求を送信した際にトピックが存在しなければ Kafka が自動的にトピックを作成します。

- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDB コネクターにカスタムタイムスタンプカラム名（`ts_column`）パラメーターの指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` オプションのデフォルト値を `chunk` から `frame` に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト4 KB）を超える場合の CPU 使用率が大幅に低減します。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQX は `DISCONNECT` パケットを送信せず接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` API のパフォーマンスを最適化しました。以前はクラスターに約5万クライアント以上いる場合、クライアントリスト取得 API 呼び出しが非常に遅くなるかタイムアウトしていました。

### バグ修正

#### コア MQTT 機能

- [#15884](https://github.com/emqx/emqx/pull/15884) ごく稀に、長期間クラスターから離脱したノードのルーティング情報がグローバルルーティングテーブルに無期限に保持される問題を解決しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された場合に、クラスターのルーティングテーブルと共有サブスクリプション状態に不整合が蓄積するレースコンディションを解決しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使うクライアントが `session_stepdown_request_exception` の `calling_self` エラーでクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` 組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) ルールの SQL に `metadata` フィールドが含まれていない場合に Republish フォールバックアクションが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafka データ統合で `not_all_kafka_partitions_connected` イベント発生時のログ詳細を改善しました。

- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを持つコネクターを含む設定のロードや再起動時に発生する可能性のあるメモリ不足（OOM）クラッシュを修正しました。

- [#16138](https://github.com/emqx/emqx/pull/16138) Redis クラスターのフェイルオーバー問題を修正しました。以前は Redis クラスタークライアントが通常クエリ（`GET` など）失敗時にのみクラスタートポロジーを更新していましたが、定期的な `PING` コマンド失敗時は更新されませんでした。そのためフェイルオーバー後にコマンドが発行されないと古いトポロジーを使い続け、回復できませんでした。修正後は失敗した `PING` 応答もトポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して速やかに回復します。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` 組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### スマートデータハブ

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換とスキーマ検証のインデックス問題を修正しました。1つのアイテム削除でトピックインデックスが破損し、無効化後も次のアイテムがアクティブのままになる問題を解消しました。

- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの処理を修正するために `spb_{en,de}code` 関数を導入しました。従来の `sparkplug_{en,de}code` 関数は [Protobuf 仕様](https://protobuf.dev/programming-guides/json/) に従い `bytes_value` メトリクス値を base64 エンコード／デコードしませんでした。これを解決するため、新しい関数を導入し、旧関数は後方互換のため非推奨としました。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error` メトリクスの誤カウントを修正しました。

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTT ユーザー名に非 ASCII 文字が含まれる場合のネットワーク混雑アラームメッセージのクラッシュを修正しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価中に発生する過剰な監査ログエントリを削減しました。

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログのクリーンアップ時に Mnesia トランザクションがブロックされ急激なメモリ増加を引き起こす問題を修正しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPP ゲートウェイのグローバルチェーン名を修正しました。これらゲートウェイの組み込み認証データは以前 `unknown:global` にまとめられ、ゲートウェイ間で競合していました。

- [#15699](https://github.com/emqx/emqx/pull/15699) ノード停止・再起動時にゲートウェイ（例：CoAP）の組み込み認証データが誤って削除される問題を修正しました。

- [#15822](https://github.com/emqx/emqx/pull/15822) 一定数のメッセージ送信後に OCPP 接続がクラッシュする問題を修正しました。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミットの更新動作を改善し、リスナー設定更新後にバーストレートやレート閾値の変更が即時適用されるようにしました。以前は内部リミッター状態の一部が正しく更新されず、設定より厳しいレートリミットが適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHook の TLS オプションを修正し、gRPC クライアントが TLS ハンドシェイク中にサーバーホスト名を正しく検証できるようにしました。

## 5.9.2

*リリース日: 2025-11-14*

EMQX 5.9.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#15773](https://github.com/emqx/emqx/pull/15773) 再接続時のクライアント ID 登録をスロットリングしました。

  - 以前のセッションクリーンアップが進行中の場合、同一クライアント ID を使う新規接続はスロットリングされます。これによりクライアントが過剰に再接続して不安定になるのを防ぎます。
  - 影響を受けるクライアントは `CONNACK` の理由コード `137`（Server Busy）と理由文字列 `"THROTTLED"` を受け取り、クリーンアップ完了後に再試行すべきです。
  - 同一クライアント ID 登録時の理由コードが誤って `133` だった問題を修正し、正しく `137` を返すようにしました。

#### データ統合

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを 3.8.3.0 にアップグレードしました。これにより、EC2 インスタンスが適切な IAM 権限を持つ場合、Access Key Id と Secret Access Key を指定せずに S3 コネクターをセットアップ可能です。

- [#15585](https://github.com/emqx/emqx/pull/15585) brod クライアントを 4.4.4 に更新し、より広範な Kafka API をサポートしました。この更新は `JoinGroups` API バージョン `v0` - `v1` の非推奨対応です。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT コネクターの `static_clientids` 設定を拡張し、クライアント ID ごとにユーザー名とパスワードを指定可能にしました。これは Azure IoT Hub など、各デバイス（クライアント ID）に固有の認証情報が必要なシナリオで有用です。クラスター環境の複数ノードでの接続成功を支援します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP アクションの HTTP リクエストタイムアウトを `resource_opts.request_ttl` 設定で変更可能にしました。以前は固定の 30 秒で調整不可でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できる API エンドポイントを追加しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS 認証のコネクターでリソースが `disconnected` とマークされた際に返される情報を改善しました。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval` 設定をデフォルトで無効化しました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VM パラメーターを最適化し、パフォーマンスと安定性を向上させました：

  - 分散チャネルのバッファサイズを 32 MB (`+zdbbl 32768`) に増加し、Mnesia 集中的操作時の `busy_dist_port` アラームを防止。
  - スケジューラのビジーウェイティングを無効化し、OS から見た CPU 使用率を低減。
  - スケジューラバインディングタイプを db に設定し、メッセージレイテンシを削減。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。

  - 認可（authz）キャッシュはクライアント切断時に即時クリアされ、不要なメモリ消費を削減。
  - クライアント ID、ユーザー名、パスワード、トピックなどのフィールドは、64 バイト超の場合は生パケットのスライスではなく新規バイナリにコピーされ、Erlang VM の 'binary' 部分のメモリ使用を削減。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートの問題を修正しました。デフォルト値で EMQX をデプロイすると複数レプリカが起動し、1つを除く全ノードがクラッシュしていました。クラスタ展開は Commercial License が必要なため、チャートのデフォルトは単一レプリカに変更されました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 古いバージョン（5.9 未満）からのローリングアップグレード時のノード起動失敗を修正しました。

  以前の EMQX バージョン（5.9 未満）では ZIP タイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS 時間形式の30または31番目の2秒スロットに対応）が保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTP バージョンを 26.2.5.2 から 26.2.5.14 にアップグレードしました。このアップグレードには EMQX に影響する OTP の TLS 関連修正が含まれます：

  - 証明書更新中のレースコンディションによる TLS 接続クラッシュを修正。
  - RSASSA-PSS パラメーターで署名された RSA 証明書のサポートを追加。以前はこれらの証明書が TLS ハンドシェイクで `bad_certificate` / `invalid_signature` エラーを引き起こしていました。

- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO 無効化後も関連ログが出力される問題を修正しました。

- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境で OIDC ログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  以前はこれらのルールが内部的に `#` に変換されていましたが、MQTT 仕様の制限により `$` プレフィックスのトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`$` プレフィックスを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止する検証を追加しました。空ユーザーは後で HTTP API から削除できず、API パスを破壊します。

  もし該当ユーザーを削除したい場合は EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートの問題を修正しました。デフォルト値で EMQX をデプロイすると複数レプリカが起動し、1つを除く全ノードがクラッシュしていました。クラスタ展開は Commercial License が必要なため、チャートのデフォルトは単一レプリカに変更されました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 古いバージョン（5.9 未満）からのローリングアップグレード時のノード起動失敗を修正しました。

  以前の EMQX バージョン（5.9 未満）では ZIP タイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS 時間形式の30または31番目の2秒スロットに対応）が保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### クラスター

- [#15788](https://github.com/emqx/emqx/pull/15788) etcd クラスター検出問題を修正しました。共有 etcd サーバーを使う際に異なるクラスターの EMQX ノードが誤って相互に参加してしまう問題を解決しました。これは etcd クライアントライブラリのバグによるものでした。

#### スマートデータハブ

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの処理を修正するために `spb_{en,de}code` 関数を導入しました。従来の `sparkplug_{en,de}code` 関数は [Protobuf 仕様](https://protobuf.dev/programming-guides/json/) に従い `bytes_value` メトリクス値を base64 エンコード／デコードしませんでした。これを解決するため、新しい関数を導入し、旧関数は後方互換のため非推奨としました。

#### データ統合

- [#15394](https://github.com/emqx/emqx/pull/15394) 非同期応答の予期しない到着によりアクションメトリクスが不整合になる稀なレースコンディションを修正しました。

- [#15603](https://github.com/emqx/emqx/pull/15603) MQTT ブリッジで、スタール接続が `Connected` と表示され自動再接続しない問題を修正しました。

- [#15826](https://github.com/emqx/emqx/pull/15826) 制限付き ACL の Kafka コンシューマーコネクターのヘルスチェック動作を改善しました。以前は内部 `____emqx_consumer_probe` コンシューマーグループへのアクセス権がないユーザーでヘルスチェックが失敗していました。Kafka ブローカーが「ACL denied」応答を返した場合、EMQX は接続を正常と見なします。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDB ドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDB アクションで特定の誤った書き込み構文を使った場合に発生する `function_clause` エラーも修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafka コンシューマーソースの追加失敗時に返される情報を充実させました。例えばトピック ACL が拒否された場合など。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafka プロデューサーライブラリ wollf を 4.0.12 にアップグレードし、Kafka メタデータ応答で一時的にパーティションが欠落する場合の処理を改善しました。

  稀な競合状態で Kafka が不完全なパーティションリストを返すことがあります。以前はトピックがより少ないパーティションで再作成された場合のみ対応していましたが、一時的にパーティションが欠落する場合は対応していませんでした。このギャップによりパーティションプロデューサーがスタックしシャットダウンが無期限にブロックされる可能性がありました。

- [#15906](https://github.com/emqx/emqx/pull/15906) Kafka プロデューサーライブラリ Wolff を 4.0.12 から 4.0.13 にアップグレードし、`ProduceResponse` の `record_list_too_large` エラー処理を追加しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTT クライアントライブラリを 1.13.8 にアップグレードしました。これにより MQTT ブリッジの接続性が改善されます：

  - ピアブローカーが PINGRESP に応答しない場合にコネクターが自動再接続。
  - TLS 上のブリッジで CONNACK 待機中に接続が切れた場合の処理が迅速化。

- [#15910](https://github.com/emqx/emqx/pull/15910) 大規模ワーカープールで複数ワーカーが同時にクラッシュした場合にワーカープールが回復できない問題を修正しました。

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

- [#15547](https://github.com/emqx/emqx/pull/15547) REST API で大きなボディ（例：10MB）を持つ HTTP リクエストの処理失敗を修正しました。

- [#15797](https://github.com/emqx/emqx/pull/15797) EMQX 4.x との互換性向上のため、バッチパブリッシュ HTTP API（`/api/v5/publish/bulk`）に `encoding` パラメーターを `payload_encoding` のエイリアスとして再導入しました。これにより EMQX v4 API を使う既存統合の移行問題を解決し、ソフトウェアレベルの変更なしに継続利用可能です。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミットの更新動作を改善し、リスナー設定更新後にバーストレートやレート閾値の変更が即時適用されるようにしました。以前は内部リミッター状態の一部が正しく更新されず、設定より厳しいレートリミットが適用されることがありました。

#### 可観測性

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTT ユーザー名に非 ASCII 文字が含まれる場合のネットワーク混雑アラームメッセージのクラッシュを修正しました。

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) 未定義のパケットフィールドを参照するクライアント情報オーバーライドテンプレートにより NATS ゲートウェイがクラッシュする問題を修正しました。システムは未定義アトムの代わりに空バイナリを返します。

## 5.10.0

*リリース日: 2025-06-10*

EMQX 5.10.0 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#15118](https://github.com/emqx/emqx/pull/15118) クライアントサブスクリプションごとの最大 QoS レベルを制御する新しい設定オプション `mqtt.subscription_max_qos_rules` を追加しました。これにより、特定トピックのマッチングルールに基づき SUBSCRIBE パケットで要求される QoS を制限可能です。現在はトピックに基づく限定的なマッチングルール（述語）のみサポートしています。

- [#15246](https://github.com/emqx/emqx/pull/15246) WebSocket 接続のパフォーマンスとリソース消費を改善しました。

  - 合成ベンチマークで 1対1 MQTT メッセージング性能測定において CPU 使用率を約20%削減し、メモリ消費もわずかに低減。
  - リスナー全体の接続制限が有効な場合の接続セットアップ効率を改善。特に多数の接続を管理するノードで効果的。

#### デプロイメント

- [#14791](https://github.com/emqx/emqx/pull/14791) Helm チャートの EMQX StatefulSet にカスタムアノテーションをサポートし、ConfigMap や Secret の変更時にポッドを自動再起動可能にしました。これにより Kubernetes 上の EMQX 管理の自動化と信頼性が向上します。

#### アクセス制御

- [#15250](https://github.com/emqx/emqx/pull/15250) LDAP バインド認証で LDAP エントリ属性から `is_superuser` フラグを正しく抽出するように改善しました。

  以前は LDAP エントリに有効な `isSuperuser` 属性が含まれていても `is_superuser` 値は常に `false` に設定されていました。

- [#15249](https://github.com/emqx/emqx/pull/15249) LDAP 認証・認可を改善しました。

  - LDAP の `filter`／`base_dn` 設定のバリデーションを追加。
  - 変数展開の問題を修正。

#### ルールエンジン

- [#15001](https://github.com/emqx/emqx/pull/15001) AI サービスを使ってデータ処理を行う `ai_completion` 関数をルールエンジン SQL に追加しました。

- [#15201](https://github.com/emqx/emqx/pull/15201) AI 補完プロバイダー設定に `base_url` オプションを追加しました。

- [#15188](https://github.com/emqx/emqx/pull/15188) ルールイベントトピックにネームスペースを導入しました。

  | 以前のイベントトピック                    | 新しいイベントトピック                     |
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

  以前のイベントトピックは後方互換のため残されています。

- [#15175](https://github.com/emqx/emqx/pull/15175) ルールエンジンでワイルドカードを使ったイベントトピックマッチングをサポートしました。これにより `$events/#`、`$events/sys/+` など複数イベントを一括マッチ可能です。

#### スマートデータハブ

- [#15174](https://github.com/emqx/emqx/pull/15174) スキーマレジストリ用に Protobuf ソースファイルバンドルのアップロードをサポートしました。

  例：Protobuf ソースファイルバンドルが `/tmp/bundle.tar.gz` にあり、以下のファイル構成で `a.proto` がルート Protobuf スキーマファイルの場合：

  ```
  .
  ├── a.proto
  ├── c.proto
  └── nested
      └── b.proto
  ```

  HTTP API 経由でこのバンドルを使って新しいスキーマを作成するには：

  ```sh
  curl -v http://127.0.0.1:18083/api/v5/schema_registry_protobuf/bundle \
    -XPOST \
    -H "Authorization: Bearer xxxx" \
    -F bundle=@/tmp/bundle.tar.gz \
    -F name=my_cool_schema \
    -F root_proto_file=a.proto
  ```

#### データ統合

- [#15248](https://github.com/emqx/emqx/pull/15248) EMQX は [Doris](https://doris.apache.org/) とのデータ統合をサポートし、SQL 文を使ったデータ書き込みを可能にしました。

- [#15218](https://github.com/emqx/emqx/pull/15218) Amazon MSK（Managed Streaming for Apache Kafka）接続時に Kafka Producer と Consumer コネクターで IAM 認証をサポートしました。EMQX が AWS EC2 上で動作する場合、AWS SDK を使って Kafka クライアント用 OAuth ベアラートークンを生成します。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflake コネクターでパスワードの代わりに秘密鍵ファイルパスを指定するサポートを追加しました。

  ユーザーはパスワード、秘密鍵、またはどちらも使わない（`/etc/odbc.ini` に設定）を選択可能です。

- [#14983](https://github.com/emqx/emqx/pull/14983) EMQX は S3Tables とのデータ統合をサポートしました。

  **現在の制限事項**：

  - S3Tables カタログのみサポート（テーブルデータとメタデータは S3 に存在する必要あり）
  - Iceberg テーブルフォーマットバージョン 2 のみサポート
  - サポートされるパーティショントランスフォーム関数：

    - `identity`
    - `void`
    - `bucket[N]`

  - データファイルは Avro フォーマットのみ書き込み可能

- [#15331](https://github.com/emqx/emqx/pull/15331) influxdb アクションで、`WriteSyntax` の `timestamp` が空欄でルールにタイムスタンプフィールドがない場合に行プロトコル変換が失敗する問題を修正しました。現在はシステムの現在ミリ秒値を使い、ミリ秒精度を強制します。

- [#15348](https://github.com/emqx/emqx/pull/15348) SSL クライアントの `middlebox_comp_mode` を設定可能にしました。以前は TLS 1.3 接続で常に有効（`true`）でしたが、デフォルトは互換性維持のため `true` のままです。

  TLS 1.3 で `unexpected_message, TLS client: In state hello_retry_middlebox_assert ...` のようなエラーが発生する稀なケースでは `middlebox_comp_mode` を `false` に設定してみてください。

#### マルチテナンシー

- [#15253](https://github.com/emqx/emqx/pull/15253) 2つの新しいマルチテナンシー API を追加しました：`GET /mt/ns_list_details` と `GET /mt/ns_list_managed_details`。既存の対応 API と同様に動作しますが、ネームスペース名に加え関連メタデータも返します。

- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシー管理用にネームスペース一括削除 API `DELETE /mt/bulk_delete_ns` を追加しました。

#### CLI

- [#15158](https://github.com/emqx/emqx/pull/15158) 新しいコマンド `emqx ctl conf remove x.y.z` を追加し、既存設定からキー `x.y.z` を削除可能にしました。

#### ゲートウェイ

- [#15138](https://github.com/emqx/emqx/pull/15138) TCP/TLS、WS/WSS トランスポートプロトコルで NATS クライアント接続を受け入れる NATS ゲートウェイを導入しました。

  例：NATS メッセージを MQTT メッセージに変換し、トピック `sub/t` とペイロード `hello` で送信します。ルールエンジンやデータ統合など既存の EMQX 機能とシームレスに統合されます。

  ```
  PUB sub.t 5
  hello
  ```

#### 永続ストレージ

- [#15043](https://github.com/emqx/emqx/pull/15043) DS Raft バックエンドに基本的なメトリクスを計測する計装を追加し、クラスター状態、データベース概要、シャードレプリケーション、レプリカ遷移を可視化可能にしました。

### バグ修正

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) ブラックリスト作成失敗時のエラーメッセージフォーマットを修正しました。

#### クラスター

- [#15304](https://github.com/emqx/emqx/pull/15304) `static` ディスカバリ戦略使用時にレプリカントノードがコアノードを発見できない問題を修正しました。

  以前は `static_seeds` リストに明示的に含まれないコアノードをレプリカントが無視していました。これによりクラスターのビュー不整合や負荷不均衡が発生していました。

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker` の RPC (`badrpc`) エラー処理を修正し、誤検知によるロック成功を防止しました。これによりクラスター展開時のロック状態不整合やデッドロックを防ぎます。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) CRL 配布ポイント（CDP）処理を改善し、連続失敗時（デフォルト60秒）に CDP URL を追放し、以降のリフレッシュ試行を停止することでエラーログの多発を防止し安定性を向上しました。

---

（以下、同様の形式で 5.10.1 から 5.0.0 までのリリースノートも必要に応じて翻訳可能です）
