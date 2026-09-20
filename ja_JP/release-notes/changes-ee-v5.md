# EMQX Enterprise バージョン 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboard のユーザーアカウント管理エンドポイントへの API キーアクセスを制限しました。

  これまで、`administrator` ロールを持つ API キーは HTTP Basic 認証を介して Dashboard のユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出せました。これにより、API キーが他の Dashboard ユーザーの MFA をリセットまたは無効化したり、パスワードを変更したりでき、意図された人間の Dashboard セッションと機械の API キーの分離が回避されていました。

  これらのエンドポイントは API キー経由でアクセスすると `401 API_KEY_NOT_ALLOW` を返すようになり、既存の `/users`、`/users/:username`、`/logout`、`/api_key` への API キーアクセスをブロックするポリシーと整合しています。Dashboard ユーザーは引き続き Dashboard UI からベアラートークン（JWT）セッションを使って自身の MFA とパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイント経由での Dashboard アカウントおよび API キーのエクスポート・インポートを API キーから制限しました。

  API キーで呼び出された `POST /data/export` は、生成されるアーカイブから `dashboard_users` と `api_keys` の mnesia テーブルセットを静かに省略します。API キーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれている場合に `403 FORBIDDEN` を返します。

  Dashboard のベアラートークン（ログイン）呼び出しは影響を受けず、Dashboard ユーザーと API キーを含む完全なデータベースのバックアップと復元を継続して行えます。

  これは権限昇格のギャップを解消します。既存の `/users` と `/api_key` エンドポイントは API キーによる Dashboard ログイン資格情報と API キー記録へのアクセスを拒否していますが、API キー保持者はデータバックアップエンドポイント経由でこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証なしの `GET /status?format=json` レスポンスから EMQX リリースバージョン（`rel_vsn`）を削除し、認証なしの呼び出し元にブローカーのバージョンを開示しないようにしました。バージョン情報は認証済みのノード情報 API で引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされた tarball に対するパス・トラバーサル攻撃に対してプラグインインストールエンドポイントを強化しました。プラグインインストール先ディレクトリ外に解凍される tarball の抽出を拒否します。

  これは多層防御です。エンドポイントは既に Dashboard ログイン/API キー認証と明示的な `emqx ctl plugins allow <name-vsn>` 許可リストエントリで制限されており、認証なしまたは権限なしの呼び出し元はこのコードパスに到達できません。新しいチェックは両方のゲートが意図的に開かれた場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install`（およびそれをラップする Dashboard アップロード）によるプラグインインストール成功時に、アップロードを許可したクラスタ全体の `emqx ctl plugins allow <name-vsn>` エントリを即座に取り消すようにしました。これにより同じ許可が別の（異なる可能性のある） tarball に再利用されることを防ぎます。5分間の TTL は引き続き適用されますが、一般的なパスでの許可期間を早期に短縮します。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトのプラグインパッケージに `.sha256` チェックサムサイドカーを公開し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式 EMQX Docker イメージを強化し、イメージスキャナの指摘を解消しました。

  - ランタイムイメージビルド中に Debian セキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取得。
  - 未使用の `libgnutls30t64` パッケージを削除。EMQX は Erlang/OTP 経由で OpenSSL を使用し、GnuTLS はリンクしないため、`curl` の推移的依存としてのみ存在し、スキャナレポートに現れていました。
  - Debian の `curl` パッケージを、[stunnel/static-curl](https://github.com/stunnel/static-curl) の静的リンク済みバイナリ（OpenSSL、HTTP/2、HTTP/3 対応、RTMP・GnuTLS 非対応）に置換。Debian パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入していましたが、静的バイナリはこれを回避し、`curl` を呼ぶコンテナのヘルスチェックは変更なく動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2 の SSL Common Name / Subject をクライアント識別に取り込む前にサニタイズしました。

  リスナーが `proxy_protocol = true` の場合、PROXY-Protocol SSL TLV バイトに ASCII 制御文字が含まれる接続を拒否します（MQTT で取り込む clientid/username/password と同じバイトクラスを拒否）。これにより、攻撃者制御のバイトが `${cert_common_name}` や `${cert_subject}` テンプレート経由で HTTP 認証・認可・ルールエンジンのヘッダー値に密輸されるのを防ぎます。

  追加の防御層として、HTTP 認証・認可クライアントはレンダリングされたヘッダー名または値に CR、LF、NUL バイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTT の clientid / username / password に適用されているバイトクラスチェックを、`ClientInfo` と HTTP リクエストテンプレートに供給される他のフィールドにも拡張しました。

  - `peersni`（TLS Server Name Indication。PROXY-Protocol v2 の `authority` TLV からも受け入れ）を接続取り込み境界で検証。制御文字があれば接続拒否し警告ログを出力。
  - `mqtt.client_attrs_init` の Variform 式で生成されるクライアント属性値は制御文字を含む場合破棄（警告付き）。`${client_attrs.tns}` のようなテンプレートで注入バイトが下流に流れるのを防止。
  - HTTP アクション / ブリッジコネクターのヘッダー描画は、レンダリングされた名前または値に NUL、CR、LF が含まれるヘッダーを破棄。

#### クラスター

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期機構を導入しました。ルーティングテーブルのスキーマバージョンは `v3` に更新され、`v2` との後方互換性も提供しています。

  スキーマ v3 では、各ノード（コアまたはレプリカント）が自身に向かうルーティングテーブルエントリの完全な所有権を持ち、ピアノードにはこれらのエントリへの読み取り専用アクセスのみを許可します。これにより、分割クラスターにおけるパーティション耐性が向上し、ピアノードが他ノードの代理でルーティングテーブルを変更できなくなります。また、レプリカントノードの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3 対応ノードが v2 のみ対応クラスターに参加すると v2 を使い続けます。既存ノードのいずれかが互換モードの場合も互換モードを使用します。クラスターを v3 に切り替えるにはアップグレード後にクラスター全体を再起動してください。自動切り替えを防ぐには `broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスターが v3 に切り替わるとローリングダウングレードは不可能です。

  ノードの現在のルーティングスキーマバージョンを確認するには:

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) 分布ポートの Erlang inet ポートオプション設定をサポートし、デフォルトのバッファサイズを 1 MB に設定しました。

  これまで Erlang 分布ポートは非常に小さいデフォルトバッファ（1460 バイト、プラットフォームによっては約 9 KB）を使い、分布ポートバッファ（`+zdbbl`）を 32 MB など大きく設定しても性能ボトルネックが発生していました。これによりクラスター通信の信頼性が低下し、`erpc timeout` エラーや Mnesia トランザクションの輻輳、多コアノードサポートの劣化が起きていました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) ノードごとのルートテーブルエントリ数をエクスポートする Prometheus メトリクス `emqx_routes_count` と `emqx_routes_max` を追加しました。EMQX v4 の `emqx_routes_count` メトリクスに類似しています。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon` をデフォルトでシステム全体のメモリ統計のみ収集するように設定し、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria 統計の誤った重複クエリを回避し、Prometheus メトリクス収集のオーバーヘッドを削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報を Prometheus ゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスター全体のライセンス整合性をノードごとの CLI チェックなしでアラート可能にしました。

  タイムスタンプはライセンス発行/有効期限日の UTC 深夜の Unix エポック秒です。ライセンスがない場合は 3 つのメトリクスすべて 0 を出力します。アラートルールでは `emqx_license_expiry_at == 0` を「利用不可」シグナルとして使ってください（`max_sessions == 0` は試用期限切れも示す可能性があります）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSON データや JWT トークンからドット区切りのキー経路で値を抽出する新しい Variform 式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  `json_value` は JSON バイナリ文字列からネストされた構造をドット区切りパスで辿って値を抽出します。`jwt_value` は JWT トークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例えば、`username` が JSON オブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセスできます。`password` がカスタムクレームを持つ JWT なら `jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセスできます。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) API キーと Dashboard ログインユーザーに対する細粒度のスコープベースアクセス制御を導入しました。

  API キーは OpenAPI タグ由来のスコープで特定の API パスカテゴリに制限可能になりました。スコープなしのキーは従来通りフルアクセス（後方互換）です。空のスコープリストはすべてのスコープ付きパスを拒否します。`publisher` API キーロールは `[publish]` のみ許可されます。

  Dashboard ログインユーザーは既存のロールチェックに加え任意の `scopes` フィールドを持ちます。4つの新スコープは Dashboard 専用エンドポイントをカバーし、`user_management`、`sso_management`、`api_key_management` は管理者のみ、`mfa_management` は強制 MFA 免除のため任意のロールで利用可能です。API キーはこれらログイン専用スコープを持てません。

  2つのカタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` はベアラー認証済み呼び出し元なら誰でもアクセス可能です。`scopes` フィールドは `GET /users`、`POST /users`、`PUT /users/:username` のレスポンスにも表示され、未設定時はロールデフォルトのスコープリストを返します。

  動作変更点:

  - `dashboard.default_username` ユーザーはブレークグラスアカウントとして保護されます。削除不可、管理者から降格不可、`scopes` 設定不可で、`description` のみ変更可能です。これにより他の管理者がスコープを誤設定してもオペレーターは常に管理権限を保持できます。
  - ユーザー自身のレコードに対するセルフサービスはスコープを尊重します。パスワード変更や MFA セルフエンドポイントのみスコープチェックをバイパスし、`PUT /users/:self` などはユーザースコープの対象です。
  - `PUT /users/:username` と `PUT /api_key/:name` はリクエストボディに `scopes` フィールドがない場合、永続化されたスコープに対してロール変更を検証します。ユーザー降格や API キーロール変更はスコープと互換性がなければ拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）向けにバックエンドごとの `force_mfa` オプションを追加しました。

  有効時、SSO ユーザーは IDP 側 MFA 設定に関わらず Dashboard トークン取得前に TOTP MFA のセットアップまたは検証を完了する必要があります。3つの MFA 状態をサポートします：`not_configured`（セットアップ強制）、`enabled`（検証必須）、`admin_disabled`（MFA スキップ）。新 API エンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` が MFA フローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行から 5 分後に期限切れとなり、パッケージの SHA-256 ハッシュにピン留め可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>` は 64 文字の小文字 16 進ダイジェストを受け入れ、内容が一致しないアップロードは `403 Forbidden` で拒否されます。`sha256:` 引数が省略された場合は `<name-vsn>.tar.gz` という名前の任意のペイロードを受け入れる以前の動作を維持します。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808 ゲートウェイのダウンリンクメッセージにカスタム `msg_sn` をサポートしました。

  ダウンリンク MQTT メッセージペイロードのヘッダーに `msg_sn` フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケースのメッセージシーケンスを制御可能です。

  また、JT/T 808 ゲートウェイの `string_encoding` がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。これまで `string_encoding` 設定（例：`gbk`）はアップリンクメッセージの解析にのみ使われていましたが、現在はアップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）両方で正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafka ソースのポーリング動作を改善し、レコードがない場合に空バッチを即返すのではなく、データが来るまで短時間待つようにしました。これにより不要なポーリング遅延が減り、Kafka コンシューマーが新規レコードをより安定して受信できます。

- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erl を 1.1.13 から 1.1.18 にアップグレードし、InfluxDB コネクターに `ping_with_auth` オプション（デフォルト false）を追加しました。これにより一部の InfluxDB 互換サービスで認証情報付きのヘルスチェックが可能になります。

#### デプロイ

- [#16853](https://github.com/emqx/emqx/pull/16853) v5 ライセンスパーサーを v6 ライセンスキーと互換性のある前方互換にしました。

### バグ修正

#### コア MQTT 機能

- [#17097](https://github.com/emqx/emqx/pull/17097) retainer サブシステムの実行時スイッチとして `retainer.enable` を復活させました。これにより、`mqtt.retain_available` に依存せず、MQTT の保持メッセージプロトコルサポートを有効にしたまま保持メッセージのストレージだけを無効化できます。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッションテイクオーバーや破棄時に `disconnected_at` が `connected_at` より遅くなるタイムスタンプ順序の問題を修正しました。

  これまでは `disconnected_at` が新セッションの `connected_at` 設定後の `ensure_disconnected` で遅れて記録されていました。このため `disconnected_at > connected_at` となり、外部のクライアントプレゼンス状態追跡が困難でした。

  修正ではテイクオーバー開始時または破棄受信時に即座に `disconnected_at` を記録し、新セッションの `connected_at` より遅くならないようにしています。これにより外部プレゼンス追跡システムで正しいタイムスタンプ順序が保証されます。

  注：これらイベントが異なるクラスターのノードから発行される場合、観測される順序はノード間の時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  これまでは一部クライアントでサブスクリプション一覧取得に失敗し、出力が返らないことがありました。修正後は通常サブスクリプションと共有サブスクリプションの両方で安定して動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) Dashboard と REST API に反映されるチャネル情報（`mqueue_len`、`inflight_cnt`）が、セッションテイクオーバーリプレイ完了直後に即時更新されるように修正しました。これまでは次の 15 秒ごとの統計更新まで待っていました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) `$events/client/connack` ルールイベントにドキュメントにはあるが実際にはなかった `connected_at` フィールドを追加しました。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なルールメタデータのタイムスタンプを無視するようにしました。

  これまでは、非整数の `metadata.created_at` や `metadata.last_modified_at`（例：日付文字列）があると、EMQX は無効値を保存し、API 経由でルール一覧や取得時に内部エラーで失敗することがありました。

  修正後は無効なメタデータタイムスタンプを無視し、通常の生成タイムスタンプにフォールバックするため、破損したメタデータがあってもルール API レスポンスが利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQ コネクター/アクション/ソースで、一部接続やチャネルプロセスが予期せず終了した場合に再起動なしで自己回復しなかった問題を修正しました。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定インポート時のクラッシュを修正しました。

  大量インポート時に以下のようなクラッシュメッセージで失敗することがありました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storage アクションの集約モードで、コンテナ内の Blob 数が多すぎる場合にヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTP および GCP PubSub アクションで、理由 `closing` の一時的接続エラーを回復可能として扱い、ログノイズを減らしました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTT ソースで、コネクターが `clean_start = false` を使い、メッセージを含むセッションのあるブローカーに再接続した際にメッセージがルールアクションをトリガーしなかった問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDB コネクター/アクションで、`write_syntax` リテラルや MQTT ペイロードから書き込む際に Unicode テキストが保持されるように修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQL コネクターでプリペアドステートメント無効時のクエリ実行を修正しました。これまでは同時クエリが混在してエラーを起こすことがありました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQ コネクターの分離を修正しました。設定ミスや到達不能な RocketMQ コネクターが同一ノードの他の RocketMQ コネクターを不安定化させなくなりました。これまでは、到達不能なブローカーのコネクターが共有クライアント監督者を最大 60 秒間停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard 操作がハングすることがありました。

  TCP/TLS 接続タイムアウトのデフォルトも 60 秒から 10 秒に短縮し、設定ミスサーバーが素早く失敗として検出されるようにしました。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時に MongoDB プロセスへのタイムアウト呼び出しが回復不能エラーと誤認されてリトライされなかった問題を修正しました。該当イベントでメッセージはリトライされます。

  発生時のログ例:

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis Sentinel コネクターで Redis データノードと Sentinel ノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquet ファイル書き込み時に、必須キーが `undefined` または `null` のオブジェクトがあると破損ファイルが生成されていた問題を修正しました。エラーを発生させるようにしました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafka クライアントライブラリをアップグレードしました：`brod` 4.5.2 → 4.5.4、`wolff` 4.1.7 → 4.1.10。

  Kafka プロデューサーとコンシューマー統合に以下の修正を含みます：

  - SASL 再認証中の接続競合状態を修正し、キューイングされたプロデュース要求のドロップや `sync` プロデュース呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQ クライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサー要求のメモリ増加問題を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTT クライアント依存を `1.14.6` から `1.15.1` にアップグレードしました。

  MQTT ブリッジ、MQTT ソース、その他アウトバウンド MQTT 接続を使うコネクターに以下の改善をもたらします：

  - キープアライブタイマーから pingresp タイムアウトを追跡し、pingresp 処理を設定された `keepalive` 間隔に合わせて維持。
  - QUIC: ピアの `recv` 中止後、両方向を切断せず送信方向のみ中止し、半閉じ QUIC ストリームの保留送信が静かに破棄されなくなりました。

#### クラスター

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスター回復時間を改善しました。

  内蔵 Mria データベース管理システムは、トランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTP を 27.3.4.2-6 から 27.3.4.2-7 にアップグレードしました。

  これにより、ノード起動時にネットワークパーティションが発生した場合の MQTT ルーティングテーブル不整合の競合状態が解消されます。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTP を 27.3.4.2-8 にアップグレードしました。この修正がないと、ノードがクラスターに接続していない場合に Mria アプリの起動が EMQX 起動時に停止することがあります。

- [#17220](https://github.com/emqx/emqx/pull/17220) `bin/emqx` と `bin/emqx_ctl` の呼び出しが稼働中ブローカーの `nodeup`/`nodedown` イベントをトリガーし、ブローカーログに誤解を招く `cm_registry_node_down` 警告が出ていた問題を修正しました。これらスクリプトが起動する一時的なヘルパーノードは隠し Erlang ノードとして登録されるようになりました。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスター回復を改善しました。

  以前はレプリカントノードに接続された一部クライアントがグローバルレジストリから失われ、セッションテイクオーバー時の不整合や Dashboard 表示情報の誤りが発生していました。

  この修正では、ネットワークパーティションが解消された際に既存クライアントを再登録するバックグラウンドプロセスを追加しました。また、グローバルレジストリ再構築中に「Broker is recovering after a network partition」アラームを発報します。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動回復できる新しい自動修復アルゴリズムを導入し、ネットワークパーティションからのクラスター回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれていた場合に、`required_field: node.cookie` スキーマチェックエラーでクラスター設定インポートが失敗する問題を修正しました。読み取り専用の設定ルート（`node`、`rpc`）は事前スキーマチェック前に破棄され、稼働中ノードの値で検証されるようになりました。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスター化されたノード間で同じ実効設定だが生の設定表現が異なる場合に、`emqx ctl conf cluster_sync status` の騒音および誤解を招く警告を修正しました。

  実効設定に対応しない生の表現差分を抑制しつつ、実効設定不整合時は警告を出します。また、一方のノードにのみ存在する生設定キーがある場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスターがネットワークパーティションを経験した際に発生する可能性のあったグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く `emqx ctl conf cluster_sync status` 警告を修正しました。

  これまではデータインポートや起動時設定ロードで、同一のアクション、ソース、ブリッジ、ルールメタデータに対しノード間で `created_at` や `last_modified_at` が異なる場合がありました。コマンドはこれらタイムスタンプのみの差分を無視し、実際の設定差分のみ報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) 応答のないターゲットクラスターへのルート複製が停滞していた際の Cluster Link 応答性を改善し、そのような Cluster Link の削除がより早く完了するようにしました。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後のグローバルセッションレジストリリークを修正しました。これにより同一クライアント ID の重複または古いエントリが残る問題が解消されます。

  廃棄およびテイクオーバーキック RPC ハンドラは、対象プロセスが生存しない場合にレジストリ行を削除します。接続パスの登録スロットルはトゥームストーン行（ローカルチャネル状態なし）を認識し、新規接続を無期限にブロックする代わりにそれらを回収します。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) `emqx_crl_cache:evict/1` が内部の URL 状態を完全にクリアしなかった CRL キャッシュの回帰を修正しました。削除後、同じ CRL URL は次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとの HTTP フェッチの繰り返しを回避します。

- [#17012](https://github.com/emqx/emqx/pull/17012) パスワードなしの CONNECT パケット時にパスワードベース認証バックエンドが認証チェーンを継続するよう修正しました。これまではパスワードなし接続時に最初のパスワード認証器がエラーを返し、後続認証器の試行がブロックされていました。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSO ログインで、ID プロバイダーが `+json` 構造化シンタックスサフィックスを持つ `Content-Type`（例：`application/jwk-set+json; charset=utf-8`）の JWKS レスポンスを返した際に `provider_not_ready` で失敗する問題を修正しました。これらのレスポンスは有効な JWKS コンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URL エンコードされたユーザー名（例：メールアドレス）を持つ SSO ユーザーの Dashboard RBAC チェックを修正し、`force_mfa` 無効時にビューアーのセルフサービス MFA 無効化リクエストが正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PID がログデータフィールドとして出力されることを保証しました。

- [#16699](https://github.com/emqx/emqx/pull/16699) これまで特定の競合状態下で以下のような長く難解なログが出力されることがありました。

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQX はより意味のある情報をログに出力し、問題のデバッグを支援します。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開時のプラグイン起動警告のノイズを削減しました。

  クラスター設定同期時にローカルノードからプラグイン設定を取得しようとする試みをやめ、起動時の `config_not_found_on_node` 警告の繰り返しを回避します。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答を受信した際に警告ログを追加しました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が `emsgsize`（受信パケットが `mqtt.max_packet_size` を超過）でクライアント接続が切断された場合、ログレベルを情報から警告に変更しました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用報告を改善しました。

  ブローカーは cgroup v2、cgroup v1、ホストの `/proc/meminfo` のメモリ読み取り値を比較し、最も制約の厳しい値を採用します。非ゼロの合計値が最小のものを優先し、合計値が同じ場合は使用率の大きい方を採用します。

  これにより以下の誤解を招く読み取りを修正します：

  - コンテナの cgroup メモリ制限が厳しい場合にホストビューが高い使用率を示す、またはその逆。
  - メモリ制限なしの cgroup がマウントされているホストで使用率が約 0% に収束する。

  過負荷保護の閾値や `Memory used` メトリクスは実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace` が `ruleid` をトレースフィルタータイプとして受け入れるよう修正しました。これまでは `emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する `trace add ...`）で CLI 引数パーサーに `ruleid` フィルターがなく、一般的なエラーとなっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### デプロイ

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) `GET /monitor_current` HTTP API に新たに `rules_matched` と `actions_executed` の 2 つのメトリクスと対応するレートを追加しました。これらはそれぞれマッチしたルール数とアクション実行率（成功＋失敗）を追跡します。
- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API 経由でパブリッシュされたメッセージのエンドツーエンドトレースをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQX は TLS 1.3 のステートレスセッションチケットによるセッション再開をサポートしました。これによりサーバー側のセッション状態保存なしにクライアントが TLS セッションを再開できます。

  **設定**

  - **ノードレベル**: `node.tls_stateless_tickets_seed`

    TLS 1.3 ステートレスセッションチケット生成に使う秘密鍵シード。

  - **リスナーレベル**: `listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3 セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - `node.tls_stateless_tickets_seed` が設定（空でない）され、リスナーの SSL オプションで `session_tickets` が有効な場合のみセッションチケットが生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出ます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808 ゲートウェイに `jt808.frame.parse_unknown_message` 設定オプションを追加し、未知のメッセージ ID を持つメッセージを解析して透過的に転送可能にしました。
- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808 プロトコル 2019 をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合で IoTDB テーブルモデルをサポートしました。

### バグ修正

#### コア MQTT 機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエストレスポンス情報プロパティ処理時の型不一致による MQTT v5 接続のクラッシュを修正しました。
- [#16514](https://github.com/emqx/emqx/pull/16514) クライアントの広告 `Maximum-Packet-Size` を超えるブローカーメッセージを受信した際に WebSocket 接続がクラッシュする問題を修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に `undefined` を返していた問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これは EMQX v4 の後方互換性修正です。これら関数は EMQX v5 以降では文書化されていません。推奨はルール評価コンテキストのフィールドを直接参照する方法です（例：`SELECT clientid ...`）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックで現在の EMQX ノードに割り当てられたパーティションのみリーダー接続を検証し、不要なアイドル接続や誤警報を防止しました。

  これまでは Kafka コンシューマーコネクターがすべてのパーティションのリーダー接続をチェックしていました。クラスター展開では各ノードがパーティションのサブセットのみを所有し、割り当てられていないパーティションリーダーへの接続はアイドル状態になります。Kafka はアイドル接続をタイムアウト（デフォルト 10 分）後に切断するため、誤警報が発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafka リクエストタイムアウトをメタデータリクエストタイムアウトの少なくとも 2 倍（最低 30 秒）に自動設定し、メタデータリクエストが予想より長くかかる場合の不要な再接続とリトライを減らしました。特にメタデータリクエストタイムアウトが小さい値に設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続テストやコネクター停止時の競合状態によるタイムアウト問題を修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) REST API ドライバー使用時の IoTDB コネクターのヘルスチェックを改善し、クライアント認証情報の検証を追加しました。これにより誤設定の早期検出が可能になりました。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsar クライアントを 2.1.2 にアップグレードしました。

  Pulsar プロデューサーアクションの `batch_size` が `1` に設定されている場合、単一メッセージを単一要素バッチではなくエンコードします。これにより Key Share 戦略を使ったコンシューマーの負荷分散が可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTT ソースのコネクターが再接続後にトピックを再サブスクライブしなかった問題を修正しました。これによりソースが停止する問題を解消しました。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS 接続失敗問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードのアクションで、コネクターが複数回のヘルスチェック失敗後に切断された場合にフォールバックアクションが二重にトリガーされる問題を修正しました。

#### クラスター

- [#16269](https://github.com/emqx/emqx/pull/16269) Cluster Link ルート複製プロトコルのリカバリーシーケンスで、リモート側が再ブートストラップを必要としているのに誤ってスキップしていた問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) Cluster Link ガベージコレクションロジックで、複数の独立した Cluster Link が設定され、一部が長期間ダウンした場合に内部ルーティングテーブルから生きたルートを誤って削除していた問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc` を `3.5.1` にアップグレードしました。

  これ以前はピアノードが到達不能な場合に接続タイムアウトで長時間のクラッシュログが発生していました。新バージョンは長いクラッシュログを廃止し、クラッシュログを読みやすい `error` ログに変換し、頻発する `"failed_to_connect_server"` ログを抑制します。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスターの自動クリーン手順の堅牢性を向上しました。これまではノード起動時に自動クリーンが無効化されていると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSO バックエンドに `idp_signs_envelopes` と `idp_signs_assertions` オプションを追加し、IdP が署名した SAML レスポンスの署名検証を制御可能にしました。両オプションは後方互換のためデフォルト `false` で、IdP が SAML レスポンスに署名する場合は明示的に有効化が必要です。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0 未満からのアップグレード後に MFA を有効化できなかった問題を修正しました。これはログインユーザーデータベースレコードの互換性問題が原因でした。

- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC 発行者 URL が設定ファイルに保存される際に末尾スラッシュが自動付加され、OIDC プロバイダーの発行者と不一致となる問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) リソース例外発生時のログ量を削減しました。これらログはスロットリングされ、一部の大きな項目はマスクされます。
- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc` エラーのログフォーマッタクラッシュを修正しました。これまでは `gen_rpc` が特定のエラー（例：送信タイムアウト）をログ出力すると EMQX がクラッシュしていました。フォーマッタはこれらエラーを正しく処理します。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) CAN バス ID パラメータ（0x0110～0x01FF）に対する JT/T 808 ゲートウェイのパラメータ設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正しました。JSON では文字列型ではなく base64 エンコードされた BYTE[8] 型を使用します。

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS 上の接続モードで動作する CoAP ゲートウェイを修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808 ゲートウェイに GBK 文字エンコーディングサポートを追加しました。

  JT/T 808 プロトコルは STRING 型フィールドに GBK エンコーディングを指定しています。新しい `frame.string_encoding` 設定オプションを追加しました：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）。
  - `gbk`：デバイスからの GBK エンコード文字列を MQTT 用に UTF-8 に変換し、MQTT からデバイスへは UTF-8 を GBK に変換。

  対象はナンバープレート、運転手名、テキストメッセージ、エリア名、クライアントパラメータなどの文字列フィールドです。MQTT ペイロードはこの設定に関係なく常に UTF-8 エンコードを使用します。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージの破棄（`buffer_worker_dropped_expired_messages`）に関するログを警告レベルで出力し、リソース ID ごとにスロットリングするようにしました。これにより特定の外部リソースが受信メッセージレートに追いつけずメッセージ破棄が発生している状況を識別しやすくなります。
- [#16206](https://github.com/emqx/emqx/pull/16206) Kafka Producer コネクターに `allow_auto_topic_creation` 設定オプションを追加しました。有効にすると、クライアントがメタデータフェッチ要求を送信した際にトピックが存在しなければ Kafka が自動作成を許可します。
- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDB コネクターにカスタムタイムスタンプ列名（`ts_column`）パラメーター指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` オプションのデフォルト値を `chunk` から `frame` に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト 4 KB）を超える場合の CPU 使用率が大幅に削減されます。

  **注意**: `parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQX は `DISCONNECT` パケットを送信せず接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` API のパフォーマンスを最適化しました。クラスターに約 50,000 クライアント以上存在する場合、クライアント一覧取得 API 呼び出しが非常に遅くなるかタイムアウトすることがありました。

### バグ修正

#### コア MQTT 機能

- [#15884](https://github.com/emqx/emqx/pull/15884) グローバルルーティングテーブルが、長期間クラスターを離れたノードのルーティング情報を無期限に保持する問題を解決しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルおよび共有サブスクリプション状態に不整合が蓄積される競合状態を解消しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使用するクライアントが、`session_stepdown_request_exception` エラーでクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  これまでは `jq` の組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) ルールの SQL に `metadata` フィールドが含まれていない場合、Republish フォールバックアクションが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例:

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafka データ統合で `not_all_kafka_partitions_connected` イベント発生時のログ詳細を改善しました。
- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを持つコネクターを含む設定のロードや再起動時に発生する可能性のある OOM クラッシュを修正しました。
- [#16138](https://github.com/emqx/emqx/pull/16138) Redis クラスターのフェイルオーバー問題を修正しました。これまでは定期的な `PING` コマンドの失敗がトポロジー更新をトリガーせず、フェイルオーバー後に古いトポロジーを使い続けて回復できないことがありました。修正後は失敗した `PING` 応答がトポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して迅速に回復します。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  これまでは `jq` の組み込み関数 `index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### スマートデータハブ

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換とスキーマ検証のインデックス問題を修正しました。1 件削除するとトピックインデックスが破損し、無効化後も次のアイテムが有効のままになることがありました。
- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。
- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの処理を修正するために `spb_{en,de}code` 関数を導入しました。既存の `sparkplug_{en,de}code` 関数は [Protobuf 仕様](https://protobuf.dev/programming-guides/json/) に準拠せず `bytes_value` を base64 エンコード/デコードしませんでした。後方互換性のため旧関数は非推奨です。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error` メトリクスの誤カウントを修正しました。
- [#15785](https://github.com/emqx/emqx/pull/15785) MQTT ユーザー名に非 ASCII 文字が含まれる場合のネットワーク輻輳アラームメッセージのクラッシュを修正しました。
- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）のループ評価時に生成される過剰な監査ログを削減しました。
- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログのクリーンアップ時に Mnesia トランザクションがブロックされ急速なメモリ増加を引き起こす問題を修正しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPP ゲートウェイのグローバルチェーン名を修正しました。これらゲートウェイの組み込み認証データは以前 `unknown:global` にまとめられており、ゲートウェイ間で競合していました。
- [#15699](https://github.com/emqx/emqx/pull/15699) ノード停止・再起動時にゲートウェイ（例：CoAP）の組み込み認証データが誤って削除される問題を修正しました。
- [#15822](https://github.com/emqx/emqx/pull/15822) 一定数のメッセージ送信後に OCPP 接続がクラッシュする問題を修正しました。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) リスナー設定更新後に接続レートリミットの変更（バーストレートや閾値など）が即時反映されるように改善しました。これまでは内部リミッター状態の一部が正しくリフレッシュされず、設定より厳しいレート制限が適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHook TLS オプションを修正し、gRPC クライアントが TLS ハンドシェイク時にサーバーのホスト名を正しく検証できるようにしました。

## 5.10.1

*リリース日: 2025-09-18*

EMQX 5.10.1 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### パフォーマンス

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアするようにし、不要なメモリ消費を削減しました。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。クライアント ID、ユーザー名、パスワード、トピックなどのフィールドは、64 バイトを超える場合に生パケットのスライスではなく新しいバイナリにコピーされ、Erlang VM の 'binary' 部分のメモリ使用が減少します。

#### アクセス制御

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP 認証・認可を強化しました。

  LDAP 認可は既存の単純トピックリストに加え JSON を使った拡張 ACL ルール形式をサポートしました。認証時にクライアント情報に基づいて LDAP から ACL ルールを取得し、クライアントのメタデータにキャッシュして認可時の LDAP クエリを繰り返さないようにしました。

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証・認可の外部リソース管理を最適化しました。これまでは無効化された認証・認可プロバイダーに設定されたリソースに接続し続けることがありました。

#### データ統合

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 Tables アクションで Parquet 形式のデータファイル書き込みをサポートしました。

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producer コネクターとアクションのヘルスチェックにレート制限を追加し、AWS API クォータに準拠しクラスター挙動を改善しました。

  - `ListStreams` と `DescribeStream` へのヘルスチェック呼び出しをそれぞれコネクター単位で 5/s と 10/s に制限し、AWS のレート制限に合わせました。
  - クラスター内のコアノードが調整する分散リミッターでこれら制限を一貫して適用します。
  - ヘルスチェックがスロットリングまたはタイムアウトした場合、コネクターやアクションは切断状態にせず前回の状態を維持します。

  また、`resource_opts.health_check_interval_jitter` を追加し、同一コネクター下の複数アクションが同時にヘルスチェックを行う可能性を減らすために `resource_opts.health_check_interval` に均一ランダム遅延を加えます。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを `3.8.3.0` にアップグレードしました。これにより、EC2 インスタンスが適切な IAM 権限を持つ場合にアクセスキー ID とシークレットアクセスキーを指定せずに S3 コネクターを設定可能になりました。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT コネクターの `static_clientids` 設定で、クライアント ID ごとにユーザー名とパスワードを指定可能になりました。これは Azure IoT Hub など、各デバイス（クライアント ID）に固有の認証情報が必要なシナリオで有用です。クラスター環境の複数ノード間での接続成功を支援します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP アクションの HTTP リクエストタイムアウトを `resource_opts.request_ttl` 設定で変更可能にしました。これまでは 30 秒固定で変更不可でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) アクティブなアラームを強制的に非アクティブ化する API エンドポイントを追加しました。
- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry 統合で HTTP 認証が必要なコレクターに対応するため、カスタム HTTP ヘッダー設定項目を追加しました。
- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS 認証のコネクターでリソースが `disconnected` とマークされた際に返される情報を改善しました。
- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary`、`GET /sources_summary` エンドポイントおよび `GET /actions/:id` のフォールバックアクションに `tags` フィールドを追加しました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールが現在のシステム設定を HOCON 形式でエクスポートするようになりました。パスワードやシークレットなどの機密情報は自動的にマスクされます。

### バグ修正

#### コア MQTT 機能

- [#15361](https://github.com/emqx/emqx/pull/15361) 不正な（短すぎる）長さの `User-Property` ペアを解析した際の `function_clause` エラーを修正しました。

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断されたクライアントの共有サブスクリプションに対する冗長なクリーンアップ処理を削除しました。これらは高切断数時にクラッシュを引き起こし、グローバルブローカー状態の不整合を招くことがありました。

- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket 接続のセッション期限切れ時に発生する警告レベルログイベントとクラッシュを修正しました。この問題は最近の WebSocket パフォーマンス改善で導入されました。ブローカー容量には影響しませんが、以下のようなログが出力されていました：
  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK が非ゼロ理由コードで送信された後の切断時に出る警告ログ `unclean_terminate` を削除しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積される競合状態を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  これまではこれらルールは内部的に `#` に変換されていましたが、MQTT 仕様の制限により `$` プレフィックスのトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`{allow|deny, all}` ルールが `$` プレフィックスを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) ビルトインデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーは API 経由で削除できず、API パスを破壊するためです。

  もし空ユーザーを削除したい場合は EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアするようにし、不要なメモリ消費を削減しました。

---

（以下、同様に 5.10.0 以前のリリースノートも必要に応じて翻訳を続けてください。）
