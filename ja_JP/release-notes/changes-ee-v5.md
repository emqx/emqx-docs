# EMQX Enterprise Version 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboard のユーザーアカウント管理エンドポイントへの API キーアクセスを制限しました。

  以前は、`administrator` ロールを持つ API キーが HTTP Basic 認証を介して Dashboard のユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出せました。これにより、API キーが他の Dashboard ユーザーの MFA をリセットまたは無効化したり、パスワードを変更したりでき、人間の Dashboard セッションとマシンの API キーの分離を回避していました。

  これらのエンドポイントは現在、API キー経由のアクセス時に `401 API_KEY_NOT_ALLOW` を返し、既存の `/users`、`/users/:username`、`/logout`、および `/api_key` への API キーアクセスをブロックするポリシーと整合しています。Dashboard ユーザーは引き続き Dashboard UI からベアラートークン（JWT）セッションを使用して自身の MFA とパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイント経由での Dashboard アカウントおよび API キーのエクスポート・インポートを API キーから制限しました。

  API キーで呼び出された `POST /data/export` は、生成されるアーカイブから `dashboard_users` と `api_keys` の mnesia テーブルセットを静かに除外します。API キーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれている場合、`403 FORBIDDEN` を返します。

  Dashboard のベアラートークン（ログイン）呼び出しは影響を受けず、Dashboard ユーザーと API キーを含む完全なデータベースのバックアップと復元を継続して行えます。

  これは権限昇格のギャップを解消します。既存の `/users` と `/api_key` エンドポイントはすでに API キーによる Dashboard ログイン資格情報と API キー記録へのアクセスを拒否していますが、API キー保持者はデータバックアップエンドポイント経由でこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 未認証の `GET /status?format=json` レスポンスから EMQX リリースバージョン (`rel_vsn`) を削除し、ブローカーのバージョン情報が未認証呼び出し元に漏れないようにしました。バージョン情報は認証済みのノード情報 API では引き続き利用可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされた tarball によるパス・トラバーサルに対してプラグインインストールエンドポイントを強化しました。インストールパスは、プラグインインストールディレクトリ外に解決されるエントリを含む tarball の展開を拒否します。

  これは多層防御の一環です。このエンドポイントはすでに Dashboard ログイン／API キー認証と明示的な `emqx ctl plugins allow <name-vsn>` 許可リストエントリで保護されているため、未認証または未許可の呼び出し元がこのコードパスに到達することはありません。新しいチェックは、両方のゲートが意図的に開かれてプラグインをアップロードする場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install` 経由（およびそれをラップする Dashboard アップロード）でのプラグインインストール成功時に、アップロードを許可したクラスタ全体の `emqx ctl plugins allow <name-vsn>` エントリを即座に取り消すようにしました。同じ許可が別の（潜在的に異なる） tarball に再利用されることを防ぎます。5分の TTL は引き続き適用されますが、この変更により一般的なパスでの許可期間が早期に終了します。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトのプラグインパッケージに `.sha256` チェックサムサイドカーを公開し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式 EMQX Docker イメージの強化：

  - ランタイムイメージビルド時に Debian セキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取り込みました。
  - 未使用の `libgnutls30t64` パッケージを削除。EMQX は Erlang/OTP 経由で OpenSSL を使用し、GnuTLS はリンクしません。`curl` の推移的依存としてのみ存在し、スキャナーレポートに現れていました。
  - Debian の `curl` パッケージを、[stunnel/static-curl](https://github.com/stunnel/static-curl) の静的リンク済みバイナリ（OpenSSL、HTTP/2、HTTP/3 対応、RTMP・GnuTLS 非対応）に置換。Debian パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入していましたが、静的バイナリはこれを回避しつつ、`curl` を呼ぶコンテナのヘルスチェックは変更なしで動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2 の SSL コモンネーム／サブジェクトをクライアント識別情報に取り込む前にサニタイズしました。

  `proxy_protocol = true` に設定されたリスナーでは、PROXY-Protocol SSL TLV バイト列に ASCII 制御文字が含まれている接続を拒否します（これは MQTT で取り込む clientid/username/password に対して既に適用されているバイトクラスと同じです）。これにより、攻撃者が制御するバイトが `${cert_common_name}` や `${cert_subject}` テンプレートを介して HTTP 認証、認可、ルールエンジンのヘッダー値に密輸されるのを防ぎます。

  追加の防御層として、HTTP 認証・認可クライアントは、レンダリングされたヘッダー名または値に CR、LF、NUL バイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTT の clientid / username / password に適用されているバイトクラスチェックを、`ClientInfo` と HTTP リクエストテンプレートの他のフィールドにも拡張しました：

  - `peersni`（TLS サーバー名表示；PROXY-Protocol v2 の `authority` TLV からも受け入れ）を接続取り込み境界で検証。制御文字があると接続拒否し警告ログを出します。
  - `mqtt.client_attrs_init` の Variform 式で生成されるクライアント属性値は制御文字を含む場合は破棄（警告付き）されるため、`${client_attrs.tns}` のようなテンプレートで注入バイトが下流に流れることはありません。
  - HTTP アクション／ブリッジコネクターのヘッダー描画は、レンダリングされた名前または値に NUL、CR、LF が含まれるヘッダーを破棄します。

#### クラスタリング

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期機構を導入しました。ルーティングテーブルのスキーマバージョンは `v3` にステップアップし、`v2` との後方互換性も提供します。

  スキーマ v3 では、各ノード（コアまたはレプリカント）が自身に向かうルーティングテーブルエントリの完全所有権を持ち、ピアノードはこれらのエントリに対して読み取り専用アクセスのみを持ちます。これにより、パーティション分割されたクラスターでピアノードが他ノードの代理でルーティングテーブルを変更できなくなり、クラスターのパーティション耐性が向上します。また、レプリカントノードの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3 対応ノードが v2 のみ対応クラスターに参加すると v2 を継続使用します。既存ノードのいずれかが互換モードの場合も互換モードを使用します。クラスターを v3 に切り替えるにはアップグレード後にクラスター全体を再起動してください。自動切り替えを防ぐには `broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスターが v3 に切り替わるとローリングダウングレードは不可能です。

  ノードの現在のルーティングスキーマバージョンを確認するには：

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) Erlang inet ポートオプションを分散ポート用に設定可能にし、デフォルトのバッファサイズを 1MB にしました。

  以前は Erlang 分散ポートのデフォルトバッファが非常に小さく（1460 バイト、一部プラットフォームでは約 9KB）、分散ポートバッファ（`+zdbbl`）を大きく設定しても性能ボトルネックが発生していました。これによりクラスター通信の信頼性が低下し、`erpc timeout` エラーや Mnesia トランザクションの混雑、多コアノードのサポート劣化が発生していました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) ノードごとのルートテーブルエントリ数をエクスポートする Prometheus メトリクス `emqx_routes_count` と `emqx_routes_max` を追加しました。EMQX v4 の `emqx_routes_count` メトリクスに類似しています。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon` をデフォルトでシステム全体のメモリ統計のみ収集するよう設定し、プロセスメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria 統計の誤った重複クエリを回避し、Prometheus メトリクス収集のオーバーヘッドを削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報を Prometheus ゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスタ全体のライセンス整合性をノードごとの CLI チェックなしでアラート可能にしました。

  タイムスタンプはライセンス発行／有効期限日の UTC 真夜中の Unix エポック秒です。ライセンスが利用できない場合はすべてのメトリクスが `0` を出力します。アラートルールでは `emqx_license_expiry_at == 0` を「利用不可」信号として使用してください（`max_sessions == 0` はトライアルライセンスの期限切れも示す可能性があります）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSON データおよび JWT トークンからドット区切りのキー経路で値を抽出する Variform 式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  `json_value` は JSON バイナリ文字列からドット区切りパスを使ってネスト構造を辿って値を抽出します。`jwt_value` は JWT トークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例：`username` が JSON オブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセス可能。`password` がカスタムクレームを持つ JWT なら `jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセス可能。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) API キーおよび Dashboard ログインユーザーに対する細粒度のスコープベースアクセス制御を導入しました。

  API キーは OpenAPI タグ由来のスコープで特定の API パスカテゴリに制限可能です。スコープなしのキーは完全アクセスを保持（後方互換）、空のスコープリストはすべてのスコープ付きパスを拒否します。`publisher` API キーロールは `[publish]` のみ許可されます。

  Dashboard ログインユーザーは既存のロールチェックに加えて任意の `scopes` フィールドを持ちます。4つの新スコープが Dashboard 専用エンドポイントをカバーします：`user_management`、`sso_management`、`api_key_management` は管理者専用、`mfa_management` は強制 MFA からの自己除外用で任意ロールが利用可能。API キーはこれらログイン専用スコープを持てません。

  2つのカタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` が追加され、どちらもベアラー認証済み呼び出し元がアクセス可能です。`scopes` フィールドは `GET /users`、`POST /users`、`PUT /users/:username` のレスポンスにも表示され、明示的に設定されていない場合はロールデフォルトのスコープリストを返します。

  挙動変更：

  - `dashboard.default_username` ユーザーはブレークグラスアカウントとして保護されます。削除不可、管理者から降格不可、`scopes` フィールド設定不可。`description` のみ変更可能。これにより他の管理者がスコープを失った場合でもオペレーターが管理者アクセスを保持します。
  - 自己レコードのセルフサービスはスコープを尊重します。パスワード変更および MFA セルフエンドポイントのみスコープチェックをバイパスし、その他の操作（例：`PUT /users/:self`）はユーザーのスコープに従います。
  - `PUT /users/:username` と `PUT /api_key/:name` はリクエストボディに `scopes` フィールドがない場合、永続化されたスコープに対してロール変更を検証します。ユーザーの降格や API キーロールの変更は、永続スコープと互換性がなければ拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）用のバックエンドごとの `force_mfa` オプションを追加しました。

  有効化すると、SSO ユーザーは IDP 側 MFA 設定に関わらず Dashboard トークン取得前に TOTP MFA 設定または検証を完了する必要があります。3つの MFA 状態をサポート：`not_configured`（設定強制）、`enabled`（検証必須）、`admin_disabled`（MFA スキップ）。新 API エンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` が MFA フローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行後 5 分で期限切れとなり、パッケージの SHA-256 ハッシュにピン留め可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>` は 64 文字の小文字16進ダイジェストを受け入れ、内容が一致しないアップロードは `403 Forbidden` で拒否されます。`sha256:` 引数が省略された場合は `<name-vsn>.tar.gz` という名前の任意ペイロードを受け入れる従来の動作を維持します。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808 ゲートウェイのダウンリンクメッセージにカスタム `msg_sn` をサポートしました。

  ダウンリンク MQTT メッセージペイロードのヘッダーに `msg_sn` フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケースのメッセージシーケンス制御を可能にします。

  また、JT/T 808 ゲートウェイの `string_encoding` がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。以前は `string_encoding` 設定（例：`gbk`）はアップリンクメッセージの解析にのみ使われていましたが、現在はアップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）の両方が正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafka ソースのポーリング動作を改善し、レコードがない場合に空バッチを即返すのではなく、データが来るまで短時間待機するようにしました。これにより不要なポーリング遅延が減り、Kafka コンシューマーが新規レコードをより安定して受信できます。

- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erl を 1.1.13 から 1.1.18 にアップグレードし、InfluxDB コネクターに `ping_with_auth` オプション（デフォルト false）を追加しました。これにより、一部の InfluxDB 互換サービスで認証情報を含むヘルスチェックが可能になります。

#### デプロイ

- [#16853](https://github.com/emqx/emqx/pull/16853) v5 ライセンスパーサーを v6 ライセンスキーに対して前方互換対応にしました。

### バグ修正

#### コア MQTT 機能

- [#17097](https://github.com/emqx/emqx/pull/17097) `retainer.enable` を実際のランタイムスイッチとして復活させました。これにより、`mqtt.retain_available` に頼るのではなく、MQTT の保持メッセージプロトコルサポートを有効にしたまま保持メッセージの保存のみを無効化できます。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッション引き継ぎや破棄時に `disconnected_at` が `connected_at` より後になるタイムスタンプ順序の問題を修正しました。

  以前は `disconnected_at` が遅すぎて（`ensure_disconnected` 内）、新セッションの `connected_at` より後に記録されていました。これにより `disconnected_at > connected_at` となり、外部のクライアントプレゼンス状態追跡が困難になっていました。

  修正では、引き継ぎ開始時または破棄受信時に即座に `disconnected_at` を記録し、新セッションの `connected_at` より遅くならないようにしました。これにより外部プレゼンス追跡が正しく行えます。

  注：これらイベントが異なるクラスタノードから発せられる場合、観測される順序はノード間の時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  以前は一部クライアントでサブスクリプション一覧取得に失敗し、出力が得られませんでした。現在は通常のサブスクリプションと共有サブスクリプションの両方で確実に動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) Dashboard と REST API に反映されるチャネル情報（`mqueue_len`、`inflight_cnt`）が、セッション引き継ぎリプレイ完了直後に即時更新されるよう修正しました。以前は次の 15 秒統計更新まで待っていました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) `$events/client/connack` ルールイベントに欠落していた `connected_at` フィールドを追加しました。ドキュメントには記載されていましたが、実際のイベントデータに存在しませんでした。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なメタデータタイムスタンプを無視するようにしました。

  以前は、`metadata.created_at` や `metadata.last_modified_at` に日付文字列などの非整数値が含まれると、EMQX が無効値を保存し、API 経由でルールを一覧・取得する際に内部エラーで失敗していました。

  現在は無効なメタデータタイムスタンプを無視し、通常の生成タイムスタンプにフォールバックするため、破損したメタデータがあってもルール API レスポンスは利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQ コネクター／アクション／ソースで、接続やチャネルプロセスが予期せず終了すると、再起動なしに自己回復しない問題を修正しました。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定インポート時のクラッシュを修正しました。

  大量インポート時に以下のようなクラッシュメッセージで失敗していました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storage アクションの集約モードで、コンテナ内の Blob が多すぎるとヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTP および GCP PubSub アクションで、`closing` 理由の一時的な接続エラーを回復可能として扱い、ログノイズを削減しました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTT ソースで、コネクターが `clean_start = false` を使い、メッセージを含むセッションを持つブローカーに再接続した場合に、メッセージがルールアクションをトリガーしない問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDB コネクター／アクションで、`write_syntax` リテラルや MQTT ペイロードから書き込む値の Unicode テキストを保持するよう修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQL コネクターでプリペアドステートメント無効時のクエリ実行を修正。以前は同時クエリが干渉しエラーを起こしていました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQ コネクターの分離を修正。設定ミスや到達不能な RocketMQ コネクターが同一ノード上の他のコネクターを不安定化させなくなりました。以前は到達不能なブローカーが共有クライアントスーパーバイザーを最大 60 秒停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard 操作がハングしていました。

  TCP/TLS 接続タイムアウトも 60 秒から 10 秒に短縮され、設定ミスサーバーが速やかに失敗として検出されます。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時に MongoDB プロセスへのタイムアウト呼び出しが回復不能エラーとして扱われ再試行されなかった問題を修正しました。現在はこのケースで再試行されます。

  発生時のログ例：

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis Sentinel コネクターで Redis データノードと Sentinel ノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquet ファイルに必須キーが `undefined` または `null` のオブジェクトを書き込むと破損ファイルが生成される問題を修正し、エラーを発生させるようにしました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafka クライアントライブラリをアップグレード：`brod` を 4.5.2 から 4.5.4 に、`wolff` を 4.1.7 から 4.1.10 に。

  Kafka プロデューサー・コンシューマー統合に以下の修正を含みます：

  - SASL 再認証中の接続競合を修正し、キューイングされた produce リクエストのドロップと `sync` produce 呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQ クライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサーリクエストのメモリ増加を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTT クライアント依存を `1.14.6` から `1.15.1` にアップグレードしました。

  MQTT ブリッジ、MQTT ソース、その他のアウトバウンド MQTT 接続を使うコネクターに以下のユーザー向け改善をもたらします：

  - キープアライブタイマーから pingresp タイムアウトを追跡し、pingresp 処理を設定された `keepalive` 間隔と同期させました。
  - QUIC：ピアの `recv` 中止後、両方向を切断するのではなく送信方向のみ中止し、半クローズド QUIC ストリームの保留送信が黙ってドロップされなくなりました。

#### クラスタリング

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスター回復時間を改善しました。

  内蔵 Mria データベース管理システムは、トランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTP を 27.3.4.2-6 から 27.3.4.2-7 にアップグレードしました。

  起動時にノードがネットワークパーティションを経験した場合に MQTT ルーティングテーブルの不整合を引き起こす競合状態を修正します。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTP を 27.3.4.2-8 にアップグレードしました。この修正がないと、ノードがクラスターに接続されていない場合に Mria アプリの起動が EMQX 起動時にハングする可能性があります。

- [#17220](https://github.com/emqx/emqx/pull/17220) `bin/emqx` と `bin/emqx_ctl` の呼び出しが稼働中ブローカーの `nodeup`/`nodedown` イベントをトリガーし、ブローカーログに誤解を招く `cm_registry_node_down` 警告が出ていた問題を修正。これらスクリプトが起動する一時ヘルパーノードは隠し Erlang ノードとして登録されるようになりました。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスター回復を改善しました。

  以前はレプリカントノードに接続されたクライアントの一部がグローバルレジストリから失われ、セッション引き継ぎ時に不整合が生じ、Dashboard に誤った情報が表示されていました。

  この修正では、ネットワークパーティションが修復された際に既存クライアントを再登録するバックグラウンドプロセスを追加し、グローバルレジストリ再構築中に「Broker is recovering after a network partition」という新しいアラームを発生させます。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動回復可能な新しい自動修復アルゴリズムを導入し、ネットワークパーティションからのクラスター回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれている場合に、`required_field: node.cookie` スキーマチェックエラーでクラスタ設定インポートが失敗する問題を修正。読み取り専用の設定ルート（`node`、`rpc`）は事前チェック前に破棄され、実行中ノードの値が検証に使われます。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスター内のノードが実効設定は同じでも生の設定表現が異なる場合に、`emqx ctl conf cluster_sync status` のノイズの多い誤解を招く診断を修正。

  実際の設定変更に対応しない生の表現差分を抑制しつつ、実効設定不整合時は警告を継続。片方のノードにのみ存在し他方にない生設定キーがある場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスターがネットワークパーティションを経験した際に発生する可能性のあったグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く `emqx ctl conf cluster_sync status` 警告を修正。

  以前はデータインポートや起動時設定ロードで、同一のアクション、ソース、ブリッジ、ルールメタデータであってもノード間で `created_at` や `last_modified_at` が異なる場合がありました。現在はクラスタ設定整合性チェック時にこれらタイムスタンプのみの差分を無視し、実際の設定差分は報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) ルート複製が応答しないターゲットクラスターへの接続でスタックした際の Cluster Link 応答性を改善。該当する Cluster Link の削除がより早く完了します。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後のグローバルセッションレジストリリークを修正し、同一クライアント ID の重複または古いエントリが残る問題を解消しました。

  廃棄および引き継ぎキック RPC ハンドラーは、対象プロセスが生存しない場合にレジストリ行も削除し、接続パスの登録スロットルはトゥームストーン行（ローカルチャネル状態なし）を認識して新規接続を無期限にブロックするのを防ぎます。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) `emqx_crl_cache:evict/1` が内部 URL 状態を完全にクリアしなかった CRL キャッシュの回帰を修正。削除後、同一 CRL URL は次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとの HTTP フェッチの繰り返しを回避します。

- [#17012](https://github.com/emqx/emqx/pull/17012) CONNECT パケットにパスワードがない場合でも、パスワードベース認証バックエンドが認証チェーンを継続するよう修正。以前はパスワードなし接続時に最初のパスワード認証器がエラーを返し、後続認証器が試行されませんでした。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSO ログインで、ID プロバイダーが `+json` 構造化構文サフィックスを持つ `Content-Type`（例：`application/jwk-set+json; charset=utf-8`）の JWKS レスポンスを返した場合に `provider_not_ready` で失敗する問題を修正。これらのレスポンスは有効な JWKS コンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URL エンコードされたユーザー名（メールアドレスなど）を持つ SSO ユーザーの Dashboard RBAC チェックを修正し、`force_mfa` 無効時にビューワーのセルフサービス MFA 無効化要求が正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PID がログデータフィールドとして出力されることを保証しました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 以前は特定の競合状態で以下のような長く難解なログが出力されることがありました：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQX は現在、問題のデバッグに役立つより意味のある情報をログに出力します。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開時のプラグイン起動警告のノイズを削減しました。

  クラスター設定同期時にローカルノードからプラグイン設定を取得しようとせず、起動時の `config_not_found_on_node` 警告の繰り返しを回避します。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答受信時に警告ログを追加しました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が `emsgsize`（受信パケットが `mqtt.max_packet_size` を超過）でクライアント接続終了時のログを情報レベルから警告レベルに変更しました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用報告を改善しました。

  ブローカーは cgroup v2、cgroup v1、およびホストの `/proc/meminfo` のメモリ読み取り値を比較し、最も制約の厳しい値を使用します。非ゼロの合計値が最小のものを優先し、合計が同じ場合は使用率の大きい方を優先します。

  これにより以下の誤解を招く読み取りを修正します：

  - コンテナに厳しい cgroup メモリ制限がある場合、ホストビューが高い使用率を示すが cgroup 制限は低い（またはその逆）場合。
  - メモリ制限なしでマウントされた cgroup で、報告される使用率が約 0% に収束する場合。

  過負荷保護の閾値と `Memory used` メトリクスは、実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace` が `ruleid` をトレースフィルタータイプとして受け付けるよう修正しました。以前は `emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する `trace add ...` 形式）が CLI 引数パーサーに `ruleid` フィルターがないため一般エラーになっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### デプロイ

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15 (Sequoia) 向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) HTTP API `GET /monitor_current` に新たに `rules_matched` と `actions_executed` の 2 つのメトリクスと対応するレートを追加しました。これらはそれぞれマッチしたルール数とアクション実行率（成功＋失敗）を追跡します。

- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API 経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQX は TLS 1.3 のステートレスセッションチケットを使ったセッション再開をサポートしました。これによりサーバー側のセッション状態保存なしにクライアントが TLS セッションを再開可能です。

  **設定**

  - **ノードレベル**：`node.tls_stateless_tickets_seed`

    TLS 1.3 ステートレスセッションチケット生成用の秘密鍵シード。

  - **リスナーレベル**：`listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3 セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - `node.tls_stateless_tickets_seed` が設定され（空でない）、かつリスナー SSL オプションで `session_tickets` が有効な場合にのみセッションチケットが生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出力されます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808 ゲートウェイに `jt808.frame.parse_unknown_message` 設定オプションを追加し、不明なメッセージ ID を持つメッセージを解析して透過的に転送可能にしました。

- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808 プロトコル 2019 をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合に IoTDB テーブルモデルのサポートを追加しました。

### バグ修正

#### コア MQTT 機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエストレスポンス情報プロパティ処理時の型不一致による MQTT v5 接続のクラッシュを修正しました。

- [#16514](https://github.com/emqx/emqx/pull/16514) ブローカーからクライアントの広告する `Maximum-Packet-Size` を超えるメッセージを受信した際に WebSocket 接続がクラッシュする問題を修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に `undefined` を返していた問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これは EMQX v4 との後方互換性修正です。これらの関数は EMQX v5 以降ではドキュメント化されていません。推奨される使い方は、ルール評価コンテキストのフィールドを直接参照すること、例：`SELECT clientid ...`（`SELECT clientid()` ではなく）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックで現在の EMQX ノードに割り当てられたパーティションのみリーダー接続を検証し、不要なアイドル接続と誤警報を防止しました。

  以前は Kafka コンシューマーコネクターがすべてのパーティションのリーダー接続を検証していました。クラスター展開では各ノードがパーティションのサブセットのみ所有し、割り当てられていないパーティションリーダーへの接続がアイドル状態になります。Kafka はアイドル接続をタイムアウト（デフォルト 10 分）後に切断するため、誤った接続障害アラームが発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafka リクエストタイムアウトをメタデータリクエストタイムアウトの少なくとも 2 倍（最低 30 秒）に自動設定し、メタデータリクエストが長引く際の不要な再接続とリトライを削減しました。特にメタデータリクエストタイムアウトが小さい値に設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続テストやコネクター停止時に発生する可能性のある競合状態によるタイムアウトを修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) REST API ドライバー使用時の IoTDB コネクターのヘルスチェックを改善し、クライアント認証情報を検証するようにしました。これにより誤設定を早期に検出可能です。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsar クライアントを 2.1.2 にアップグレードしました。

  Pulsar プロデューサーアクションの `batch_size` が `1` に設定されている場合、単一メッセージを単一要素バッチとしてではなくエンコードします。これにより、Key Share 戦略を使ったコンシューマーの負荷分散が可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTT ソースのコネクターが再接続後にトピックの再サブスクライブを行わず、ソースが動作停止する問題を修正しました。現在は再接続時に自動的に再サブスクライブします。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS 接続失敗の問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードのアクションで、コネクターが複数回のヘルスチェック失敗後に切断されるとフォールバックアクションが 2 回トリガーされる問題を修正しました。

#### クラスタリング

- [#16269](https://github.com/emqx/emqx/pull/16269) Cluster Link ルート複製プロトコルの回復シーケンスで、リモート側が再ブートストラップを必要としているにもかかわらず誤ってスキップされていた問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立した Cluster Link が設定され、一部が長期間ダウンした場合に、古いルート複製状態のクリーンアップ中に内部ルーティングテーブルからライブルートが誤って削除される問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc` を `3.5.1` にアップグレードしました。

  以前のバージョンでは、ピアノードが到達不能の場合に接続タイムアウトによる長いクラッシュログが発生していました。新バージョンでは長いクラッシュログがなくなり、クラッシュログはより読みやすい `error` ログに変換され、頻繁な `"failed_to_connect_server"` ログもスロットリングされてログスパムを防止します。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスター自動クリーン処理の堅牢性を向上しました。以前はノード起動時に自動クリーン機能が無効化されていると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSO バックエンドに `idp_signs_envelopes` と `idp_signs_assertions` オプションを追加し、IdP が SAML レスポンスに署名する場合の署名検証を制御可能にしました。両オプションは後方互換のためデフォルト `false` で、IdP が署名する場合は明示的に有効化が必要です。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0 未満からアップグレード後に MFA が有効化できなかった問題を修正しました。これはログインユーザーデータベースレコードの互換性問題が原因でした。

- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC 発行者 URL が設定ファイル保存時に末尾スラッシュ付きに自動正規化され、OIDC プロバイダーの発行者が末尾スラッシュなしの場合に不一致エラーとなる問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) リソース例外発生時のログ量を削減しました。これらログはスロットリングされ、一部の大きな項目はマスクされます。

- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc` エラーのログフォーマッタークラッシュを修正しました。以前は `gen_rpc` が特定のエラーメッセージ（例：送信タイムアウト）をログ出力すると EMQX がクラッシュしていました。現在はこれらメッセージを正しく処理します。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) CAN バス ID パラメータ（0x0110～0x01FF）に対する JT/T 808 ゲートウェイのパラメータ設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正しました。これらは JSON で文字列型ではなく base64 エンコードされた BYTE[8] データ型を使用すべきです。

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS 上の接続モードで動作する CoAP ゲートウェイを修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808 ゲートウェイに GBK 文字エンコーディングサポートを追加しました。

  JT/T 808 プロトコルは STRING 型フィールドに GBK エンコーディングを指定しています。新しい `frame.string_encoding` 設定オプションを追加：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）。
  - `gbk`：デバイスからの GBK エンコード文字列を MQTT 用に UTF-8 に変換し、MQTT からデバイスへは UTF-8 を GBK に変換。

  対象はナンバープレート、ドライバー名、テキストメッセージ、エリア名、クライアントパラメータなどの文字列フィールドです。MQTT ペイロードはこの設定に関わらず常に UTF-8 エンコードです。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージのドロップに関するログ（`buffer_worker_dropped_expired_messages`）を警告レベルで出力し、リソース ID ごとにスロットリングするようにしました。これにより、特定の外部リソースが受信メッセージレートに追いついていない場合を識別しやすくなります。

- [#16206](https://github.com/emqx/emqx/pull/16206) Kafka Producer コネクターに `allow_auto_topic_creation` 設定オプションを追加しました。有効化すると、クライアントがメタデータフェッチリクエストを送信した際にトピックが存在しなければ Kafka が自動的にトピックを作成します。

- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDB コネクターにカスタムタイムスタンプカラム名（`ts_column`）パラメーターの指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` オプションのデフォルト値を `chunk` から `frame` に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト 4KB）を超える場合の CPU 使用率が大幅に低減します。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQX は `DISCONNECT` パケットを送信せずに接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` API のパフォーマンスを最適化しました。クラスターに約 5 万以上のクライアントが存在する場合、クライアントリスト取得 API 呼び出しが非常に遅くなるかタイムアウトする問題を改善しました。

### バグ修正

#### コア MQTT 機能

- [#15884](https://github.com/emqx/emqx/pull/15884) まれに、グローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無期限に保持する問題を修正しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積する競合状態を修正しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使用するクライアントが `session_stepdown_request_exception` エラー（原因：`calling_self`）でクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` 組み込み関数 `index`（例：`.key | index("name")`）の使用でメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) 発生元ルールの SQL にルール環境の `metadata` フィールドが含まれない場合に Republish フォールバックアクションが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafka データ統合で `not_all_kafka_partitions_connected` イベント発生時のログ詳細を改善しました。

- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを持つコネクターを含む設定のロードや再起動時に発生する可能性のある OOM クラッシュを修正しました。

- [#16138](https://github.com/emqx/emqx/pull/16138) Redis クラスターのフェイルオーバー問題を修正し、Redis データノードと Sentinel ノードの認証設定を分離してサポートしました。

  以前は Redis クラスタークライアントが通常クエリ失敗時のみクラスタートポロジーを更新し、定期的な `PING` 失敗は更新トリガーになっていませんでした。そのためフェイルオーバー後に古いトポロジーを使い続け、回復できないことがありました。

  修正後は `PING` 失敗もトポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して迅速に回復します。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` 組み込み関数 `index`（例：`.key | index("name")`）の使用でメモリリークが発生していました。

#### スマートデータハブ

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換およびスキーマ検証が不整合に動作する可能性があるインデックス問題を修正。1 件削除するとトピックインデックスが破損し、無効化後も次のアイテムが有効のままになることがありました。

- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの取り扱いを修正するために `spb_{en,de}code` 関数を導入しました。従来の `sparkplug_{en,de}code` 関数は Protobuf 仕様で要求される `bytes_value` メトリクス値の base64 エンコード／デコードを行っていませんでした。後方互換のため旧関数は非推奨としました。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error` メトリクスの誤カウントを修正しました。

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTT ユーザー名に非 ASCII 文字が含まれる場合のネットワーク輻輳アラームメッセージ整形時のクラッシュを修正しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価による過剰な監査ログ生成を削減しました。

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログクリーンアップ時の Mnesia トランザクションブロックによる急激なメモリ増加を防止しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPP ゲートウェイのグローバルチェーン名の誤りを修正しました。これらゲートウェイの組み込み認証データは以前 `unknown:global` にまとめられており、ゲートウェイ間で競合していました。

- [#15699](https://github.com/emqx/emqx/pull/15699) ノード停止・再起動時にゲートウェイ（例：CoAP）の組み込み認証データが誤って削除される問題を修正しました。

- [#15822](https://github.com/emqx/emqx/pull/15822) 一定数のメッセージ送信後に OCPP 接続がクラッシュする問題を修正しました。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミット更新の動作を改善し、リスナー設定更新直後にバーストレートやレート閾値の変更が即時反映されるようにしました。以前は内部リミッター状態の一部が正しくリフレッシュされず、設定より厳しいレート制限が適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHook の TLS オプションを修正し、gRPC クライアントが TLS ハンドシェイク時にサーバーのホスト名を正しく検証できるようにしました。

## 5.9.2

*リリース日: 2025-11-14*

EMQX 5.9.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#15773](https://github.com/emqx/emqx/pull/15773) 再接続時のクライアント ID 登録をスロットリングしました。

  - 以前のセッションクリーンアップが進行中の場合、同一クライアント ID を使う新規接続はスロットリングされます。これによりクライアントの過剰再接続による不安定化を防止します。
  - 影響を受けるクライアントは `CONNACK` の理由コード `137`（Server Busy）と理由文字列 `"THROTTLED"` を受け取り、クリーンアップ完了後に再試行すべきです。
  - 同一クライアント ID 登録時に返される理由コードの誤りを修正し、`133` ではなく正しく `137` を返します。

#### データ統合

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを 3.8.3.0 にアップグレードしました。これにより、EC2 インスタンスの IAM 権限が適切に設定されていれば、アクセスキー ID とシークレットアクセスキーを指定せずに S3 コネクターをセットアップ可能です。

- [#15585](https://github.com/emqx/emqx/pull/15585) brod クライアントを 4.4.4 に更新し、より広範な Kafka API をサポートしました。これにより `JoinGroups` API バージョン `v0` ～ `v1` の非推奨対応が含まれます。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT コネクターの `static_clientids` 設定で、各クライアント ID に対してユーザー名とパスワードを指定可能にしました。これにより、Azure IoT Hub のように各デバイスに固有の認証情報が必要なシナリオで、クラスタ環境の複数ノード間での接続成功を支援します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP アクションの HTTP リクエストタイムアウトを `resource_opts.request_ttl` 設定で調整可能にしました。以前は 30 秒で固定でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できる API エンドポイントを追加しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS 認証の各コネクターでリソースが `disconnected` とマークされた際に返される情報を改善しました。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval` 設定をデフォルトで無効化しました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VM パラメーターを最適化し、性能と安定性を向上しました：

  - 分散チャネルのバッファサイズを 32 MB (`+zdbbl 32768`) に増加し、Mnesia 集中的操作時の `busy_dist_port` アラームを防止。
  - スケジューラのビジーウェイティングを無効化し（`+sbwt none +sbwtdcpu none +sbwtdio none`）、OS から見た CPU 使用率を低減。
  - スケジューラバインディングタイプを `db` に設定し、メッセージレイテンシを削減。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善。

  - クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減。
  - クライアント ID、ユーザー名、パスワード、トピックなどのフィールドを、生パケットのスライスではなく新しいバイナリにコピー（64 バイト超の場合）し、Erlang VM の `binary` 部分のメモリ使用を削減。

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` オプションのデフォルト値を `chunk` から `frame` に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト 4KB）を超える場合の CPU 使用率が大幅に低減します。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQX は `DISCONNECT` パケットを送信せずに接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` API のパフォーマンスを最適化しました。クラスターに約 5 万以上のクライアントが存在する場合、クライアントリスト取得 API 呼び出しが非常に遅くなるかタイムアウトする問題を改善しました。

- [#15884](https://github.com/emqx/emqx/pull/15884) まれに、グローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無期限に保持する問題を修正しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積する競合状態を修正しました。

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK が非ゼロ理由コードで送信された後の切断時に発生する `unclean_terminate` 警告ログを削除しました。

#### デプロイ

- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートの問題を修正しました。デフォルト値で EMQX をデプロイすると複数レプリカが起動し、1 ノード以外がクラッシュしていました。クラスタ展開には Commercial License が必要なため、チャートはデフォルトで単一レプリカに設定されます。

- [#15712](https://github.com/emqx/emqx/pull/15712) 5.9 未満からのローリングアップグレード時にノード起動失敗する問題を修正しました。

  以前の EMQX バージョン（5.9 未満）では ZIP タイムスタンプエンコーダのバグで、アーカイブエントリに無効な「秒」値（DOS 時間形式の 30 または 31 番目の 2 秒スロットに対応）が保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTP を 26.2.5.2 から 26.2.5.14 にアップグレードしました。これには EMQX に影響する TLS 関連の 2 つの修正が含まれます：

  - 証明書更新時の競合状態による TLS 接続クラッシュを修正。
  - RSASSA-PSS パラメーターで署名された RSA 証明書のサポートを追加。以前はこれらの証明書が `bad_certificate` / `invalid_signature` エラーで TLS ハンドシェイク失敗を引き起こしていました。

- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO 無効化後も関連ログが出力される問題を修正しました。

- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境で OIDC ログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  以前はこれらのルールが内部的に `#` に変換されていましたが、MQTT 仕様の制限により `$` プレフィックスのトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特殊な内部値を使い、`{allow|deny, all}` ルールが `$` プレフィックスを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーは API 経由で削除できず、API パスを破壊するためです。

  もし空ユーザーが存在し削除したい場合は、EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減しました。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  以前は `jq` 組み込み関数 `index`（例：`.key | index("name")`）の使用でメモリリークが発生していました。

#### 可観測性

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログクリーンアップ時の Mnesia トランザクションブロックによる急激なメモリ増加を防止しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価による過剰な監査ログ生成を削減しました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### 耐久ストレージ

- [#14674](https://github.com/emqx/emqx/pull/14674) EMQX 耐久ストレージが作成する RocksDB の情報ログファイル数とサイズの上限を制限しました。

## 5.10.1

*リリース日: 2025-09-18*

EMQX 5.10.1 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### パフォーマンス

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減しました。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。クライアント ID、ユーザー名、パスワード、トピックなどのフィールドを、生パケットのスライスではなく新しいバイナリにコピー（64 バイト超の場合）し、Erlang VM の `binary` 部分のメモリ使用を削減しました。

#### アクセス制御

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP 認証・認可を強化しました。

  LDAP 認可は既存の単純トピックリストに加え JSON を使った拡張 ACL ルール形式をサポート。認証時にクライアント情報に基づく LDAP からの ACL ルール取得も可能になり、認可時の LDAP クエリを繰り返し実行することなくクライアントメタデータにキャッシュします。

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証・認可の外部リソース管理を最適化しました。以前は無効化された認証・認可プロバイダーに設定されたリソースに接続し続けることがありました。

#### データ統合

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 Tables アクションで Parquet 形式のデータファイル書き込みをサポートしました。

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producer コネクターとアクションのヘルスチェックにレート制限を追加し、AWS API クォータに準拠しクラスターの動作を改善しました。

  - `ListStreams` と `DescribeStream` へのヘルスチェック呼び出しはそれぞれコネクター単位で 5/s と 10/s に制限され、AWS のレート制限に合わせています。
  - 分散リミッターはクラスターのコアノードで調整され、一貫した制限を実現します。
  - ヘルスチェックがスロットリングまたはタイムアウトした場合、コネクターやアクションは切断状態にせず、前回の状態を維持します。

  また、新しい `resource_opts.health_check_interval_jitter` を導入し、`resource_opts.health_check_interval` に一様ランダム遅延を加えて、同一コネクター下の複数アクションが同時にヘルスチェックを実行する可能性を減らします。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを 3.8.3.0 にアップグレードしました。これにより、EC2 インスタンスの IAM 権限が適切に設定されていれば、アクセスキー ID とシークレットアクセスキーを指定せずに S3 コネクターをセットアップ可能です。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT コネクターの `static_clientids` 設定で、各クライアント ID に対してユーザー名とパスワードを指定可能にしました。これにより、Azure IoT Hub のように各デバイスに固有の認証情報が必要なシナリオで、クラスタ環境の複数ノード間での接続成功を支援します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP アクションの HTTP リクエストタイムアウトを `resource_opts.request_ttl` 設定で調整可能にしました。以前は 30 秒で固定でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できる API エンドポイントを追加しました。

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry gRPC（HTTP/2 経由）統合でカスタム HTTP ヘッダーをサポートしました。これにより HTTP 認証を必要とするコレクターに対応可能です。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS 認証の各コネクターでリソースが `disconnected` とマークされた際に返される情報を改善しました。

- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary`、`GET /sources_summary` エンドポイントのレスポンスおよび `GET /actions/:id` で返されるフォールバックアクションに `tags` フィールドを追加しました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールが現在のシステム設定を HOCON 形式でエクスポートするようになり、パスワードやシークレットなどの機密情報は自動的にマスクされます。

### バグ修正

#### コア MQTT 機能

- [#15361](https://github.com/emqx/emqx/pull/15361) 無効な（短すぎる）長さの `User-Property` ペアを解析した際の `function_clause` エラーを修正しました。

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断されたクライアントの共有サブスクリプションに対する冗長なクリーンアップ処理を削除しました。これらは高切断量時にクラッシュしやすく、グローバルブローカー状態の不整合を引き起こしていました。

- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket 接続のセッション有効期限切れ時に発生する警告レベルのログイベントとクラッシュを修正しました。この問題は最近の WebSocket パフォーマンス改善で導入されました。ブローカーの容量には影響しませんが、以下のようなログが出力されていました：

  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK が非ゼロ理由コードで送信された後の切断時に発生する `unclean_terminate` 警告ログを削除しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積する競合状態を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACL ルールの処理を修正しました。

  以前はこれらのルールが内部的に `#` に変換されていましたが、MQTT 仕様の制限により `$` プレフィックスのトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特殊な内部値を使い、`{allow|deny, all}` ルールが `$` プレフィックスを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーは API 経由で削除できず、API パスを破壊するためです。

  もし空ユーザーが存在し削除したい場合は、EMQX コンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減しました。

#### デプロイ

- [#15553](https://github.com/emqx/emqx/pull/15553) Helm チャートの問題を修正しました。デフォルト値で EMQX をデプロイすると複数レプリカが起動し、1 ノード以外がクラッシュしていました。クラスタ展開には Commercial License が必要なため、チャートはデフォルトで単一レプリカに設定されます。

- [#15712](https://github.com/emqx/emqx/pull/15712) 5.9 未満からのローリングアップグレード時にノード起動失敗する問題を修正しました。

  以前の EMQX バージョン（5.9 未満）では ZIP タイムスタンプエンコーダのバグで、アーカイブエントリに無効な「秒」値（DOS 時間形式の 30 または 31 番目の 2 秒スロットに対応）が保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正しました。

#### クラスタリング

- [#15788](https://github.com/emqx/emqx/pull/15788) etcd クラスター検出の問題を修正しました。共有 etcd サーバーを使用した際に異なるクラスターの EMQX ノードが誤って相互に参加する問題が、etcd クライアントライブラリのバグにより発生していました。

---

（以下のバージョンのリリースノートも同様の形式で必要に応じて翻訳可能です）
