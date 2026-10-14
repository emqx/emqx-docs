# EMQX Enterprise Version 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4 へアップグレードする前に、破壊的変更点と既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboardのユーザーアカウント管理エンドポイントへのAPIキーアクセスを制限しました。

  以前は、`administrator`ロールを持つAPIキーがHTTP Basic認証経由でDashboardのユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出せました。これにより、APIキーが他のDashboardユーザーのMFAをリセットまたは無効化したり、パスワードを変更したりでき、人間のDashboardセッションと機械のAPIキーの分離が意図せず突破されていました。

  これらのエンドポイントは現在、APIキー経由のアクセス時に `401 API_KEY_NOT_ALLOW` を返し、既存の `/users`、`/users/:username`、`/logout`、および `/api_key` へのAPIキーアクセス制限と整合しています。Dashboardユーザーは引き続き、ダッシュボードUIからベアラートークン（JWT）セッションを使って自身のMFAとパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイント経由でのDashboardアカウントおよびAPIキーのエクスポート・インポートをAPIキーから制限しました。

  APIキーで呼び出された `POST /data/export` は、結果のアーカイブから `dashboard_users` と `api_keys` のmnesiaテーブルセットを静かに除外します。APIキーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれる場合に `403 FORBIDDEN` を返します。

  Dashboardのベアラートークン（ログイン）呼び出しは影響を受けず、DashboardユーザーとAPIキーを含む完全なデータベースのバックアップと復元を引き続き行えます。

  これにより権限昇格のギャップが解消されます。既存の `/users` と `/api_key` エンドポイントはAPIキーによるDashboardログイン資格情報とAPIキー記録へのアクセスを拒否していますが、APIキー保持者はデータバックアップエンドポイントを経由してこれらの制限を回避できていました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証されていない `GET /status?format=json` レスポンスからEMQXリリースバージョン（`rel_vsn`）を削除し、未認証呼び出し元にブローカーのバージョンを開示しないようにしました。バージョン情報は認証済みのノード情報API経由で引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされたtarballのパス・トラバーサルに対してプラグインインストールエンドポイントを強化しました。プラグインインストールディレクトリ外のパスに解決されるtarballのエントリは抽出を拒否します。

  これは多層防御です：このエンドポイントは既にDashboardログイン／APIキー認証および明示的な `emqx ctl plugins allow <name-vsn>` 許可リストエントリで制限されているため、未認証または未許可の呼び出し元がこのコードパスに到達することはありません。新しいチェックは両方のゲートが意図的に開かれてプラグインをアップロードする場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install`（およびそれをラップするDashboardアップロード）によるプラグインインストール成功時に、クラスタ全体の `emqx ctl plugins allow <name-vsn>` エントリを即座に取り消すようにしました。これにより同じ許可が別の（潜在的に異なる）tarballのアップロードに再利用されることを防ぎます。5分のTTLは引き続き適用されますが、一般的なパスでの許可期間を早期に短縮します。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトでプラグインパッケージと共に `.sha256` チェックサムサイドカーを公開し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式EMQX Dockerイメージのセキュリティ強化：

  - ランタイムイメージビルド時にDebianのセキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取り込みました。
  - 未使用の `libgnutls30t64` パッケージを削除しました。EMQXはErlang/OTP経由でOpenSSLを使用してTLS通信を行い、GnuTLSはリンクしないため、`curl` の推移的依存としてのみ存在しており、スキャナー報告に現れていました。
  - Debianの `curl` パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入してしまうため、OpenSSL対応の静的リンク済み `curl` バイナリ（<https://github.com/stunnel/static-curl>）に置き換えました（HTTP/2、HTTP/3対応、RTMP・GnuTLS非対応）。`curl` を呼ぶコンテナのヘルスチェックは変更なく動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2のSSLコモンネーム／サブジェクトをクライアント識別情報に入れる前にサニタイズしました。

  `proxy_protocol = true` のリスナー設定時に、PROXY-Protocol SSL TLVバイト列にASCII制御文字が含まれる接続をブローカーが拒否します（MQTTで取り込まれるclientid/username/passwordで既に拒否されているバイトクラスと同じ）。これにより攻撃者制御下のバイトが `${cert_common_name}` や `${cert_subject}` テンプレート経由でHTTP認証・認可・ルールエンジンのヘッダー値に密輸されるのを防ぎます。

  追加の防御層として、HTTP認証・認可クライアントはレンダリングされたヘッダー名や値にCR、LF、NULバイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTTのclientid / username / passwordに適用されているバイトクラスチェックを、`ClientInfo` とHTTPリクエストテンプレートに供給される他のフィールドにも拡張しました：

  - `peersni`（TLSサーバーネームインジケーション。PROXY-Protocol v2の `authority` TLVからも受け入れ）を接続取り込み境界で検証。制御文字があると接続拒否し警告ログを出力。
  - `mqtt.client_attrs_init` Variform式で生成されるクライアント属性値は制御文字を含む場合に破棄（警告付き）され、`${client_attrs.tns}` のようなテンプレートでのバイト注入を防止。
  - HTTPアクション／ブリッジコネクターのヘッダー生成は、レンダリングされた名前や値にNUL、CR、LFが含まれるヘッダーを破棄。

#### クラスタリング

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期メカニズムを導入しました。ルーティングテーブルのスキーマバージョンは `v3` に上がり、`v2` との後方互換性を提供します。

  スキーマv3では、各ノード（コアまたはレプリカ）が自身に向けられたルーティングテーブルエントリの完全な所有権を持ち、ピアノードはこれらのエントリに対して読み取り専用アクセスのみを持ちます。これにより、パーティション化されたクラスタでピアノードが他ノードの代理でルーティングテーブルを変更できなくなり、EMQXクラスターのパーティション耐性が向上します。また、レプリカノードでの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3対応ノードがv2のみ対応のクラスタに参加すると、互換性のためv2を使い続けます。既存ノードのいずれかが互換モードの場合、新規ノードも互換モードを使います。クラスタをv3に切り替えるにはアップグレード後にクラスタ全体を再起動してください。自動切り替えを防ぐには `broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスタがv3に切り替わった後はローリングダウングレードはできません。

  ノードで現在のルーティングスキーマバージョンを確認するには：

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) 分散ポートのErlang inetポートオプション設定をサポートし、デフォルトのバッファサイズを1MBに設定しました。

  以前は分散ポートのデフォルトバッファが非常に小さく（1460バイト、プラットフォームによっては約9KB）、分散ポートバッファ（`+zdbbl`）を大きく設定してもパフォーマンスボトルネックが発生していました。これによりクラスタ通信の信頼性が低下し、`erpc timeout` エラーやMnesiaトランザクションの渋滞、多コアノードのサポート低下が起きていました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) EMQX v4の `emqx_routes_count` メトリクスに類似した、ノードごとのルートテーブルエントリ数をエクスポートする `emqx_routes_count` と `emqx_routes_max` Prometheusメトリクスを追加しました。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon` をデフォルトでシステム全体のメモリ統計のみ収集するようにし、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria統計の誤った重複クエリを回避し、Prometheusメトリクス収集のオーバーヘッドを削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報をPrometheusゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスタ全体のライセンス整合性をノードごとのCLIチェックなしで監視可能にしました。

  タイムスタンプはライセンスの発行日／有効期限日のUTC深夜のUnixエポック秒です。ライセンスが利用不可の場合は3つのメトリクスすべてが `0` を返します。アラートルールでは `emqx_license_expiry_at == 0` を「利用不可」信号として使ってください（`max_sessions == 0` は期限切れトライアルライセンスを示す場合もあるため）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSONデータやJWTトークンからドット区切りのキー経路で値を抽出する2つの新しいVariform式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  `json_value` はJSONバイナリ文字列からドット区切りパスでネストされた構造を辿って値を抽出します。`jwt_value` はJWTトークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例：`username` がJSONオブジェクトなら `json_value(username, 'shop.floor')` でフィールドにアクセス可能。`password` がカスタムクレームを持つJWTなら `jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセス可能。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) APIキーとDashboardログインユーザーに対する細粒度のスコープベースアクセス制御を導入しました。

  APIキーはOpenAPIタグ由来のスコープで特定のAPIパスカテゴリに制限可能になりました。スコープなしのキーは完全アクセスを保持し（後方互換）、空のスコープリストはすべてのスコープ付きパスを拒否します。`publisher` APIキーのロールは `[publish]` のみ許可に制限されました。

  Dashboardログインユーザーは既存のロールチェックに加えてオプションの `scopes` フィールドを持ちます。Dashboard専用エンドポイントをカバーする4つの新スコープ：`user_management`、`sso_management`、`api_key_management`（管理者のみ）、`mfa_management`（任意のロールが強制MFAから自己除外可能）。APIキーはこれらのログイン専用スコープを持てません。

  2つの新カタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` はベアラー認証済み呼び出し元が利用可能です。`scopes` フィールドは `GET /users`、`POST /users`、`PUT /users/:username` のレスポンスにも表示され、明示的に設定されていない場合はロールデフォルトのスコープリストが返されます。

  動作変更：

  - `dashboard.default_username` ユーザーはブレークグラスアカウントとして保護されます。削除、管理者からの降格、`scopes` フィールドの設定は不可で、`description` のみ変更可能。これにより他の管理者がスコープを失った場合でもオペレーターが管理アクセスを保持できます。
  - ユーザー自身のレコードに対するセルフサービスはスコープを尊重します。パスワード変更とMFAセルフエンドポイントのみスコープチェックをバイパスし、`PUT /users/:self` など他の操作はユーザーのスコープに従います。
  - `PUT /users/:username` と `PUT /api_key/:name` はリクエストボディに `scopes` フィールドがない場合、永続化されたスコープに対してロール変更を検証します。ユーザーの降格やAPIキーのロール変更は、永続スコープと互換性がない場合に拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）用にバックエンドごとの `force_mfa` オプションを追加しました。

  有効化すると、IDP側のMFA設定に関わらずSSOユーザーはDashboardトークン取得前にTOTP MFAのセットアップまたは検証を完了する必要があります。3つのMFA状態をサポート：`not_configured`（セットアップ強制）、`enabled`（検証必須）、`admin_disabled`（MFAスキップ）。新APIエンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` がMFAフローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行後5分で期限切れとなり、パッケージのSHA-256ハッシュにピン留め可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>` は64文字の小文字16進ダイジェストを受け入れ、内容がその値にハッシュされないアップロードは `403 Forbidden` で拒否されます。`sha256:` 引数を省略した場合は `<name-vsn>.tar.gz` という名前の任意ペイロードを受け入れる従来の動作を維持します。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808ゲートウェイのダウンリンクメッセージでカスタム `msg_sn` をサポートしました。

  ダウンリンクMQTTメッセージのペイロードヘッダーに `msg_sn` フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケースのメッセージシーケンスを制御可能です。

  また、JT/T 808ゲートウェイの `string_encoding` がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。以前は `string_encoding` 設定（例：`gbk`）はアップリンクメッセージの解析にのみ使われていましたが、現在は `string_encoding: gbk` 設定時にアップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）の両方が正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafkaソースのポーリング挙動を改善し、レコードがない場合に空バッチを即時返すのではなく、データが来るまで少し待つようにしました。これにより不要なポーリング遅延が減り、Kafkaコンシューマーが新規レコードをより安定して受信できます。
- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erl を1.1.13から1.1.18にアップグレードし、InfluxDBコネクターに `ping_with_auth` オプション（デフォルトfalse）を追加しました。これにより一部のInfluxDB互換サービスで認証情報を含むヘルスチェックが可能になります。

#### デプロイメント

- [#16853](https://github.com/emqx/emqx/pull/16853) v5ライセンスパーサーをv6ライセンスキーに対しても前方互換にしました。

### バグ修正

#### コアMQTT機能

- [#17097](https://github.com/emqx/emqx/pull/17097) `retainer.enable` をリテイナーサブシステムの実際のランタイムスイッチとして復活させました。これにより、MQTTのリテインドメッセージプロトコルサポートを有効にしたまま、リテインドメッセージの保存だけを無効化できるようになりました。以前はプロトコルレイヤーでリテインドパブリッシュを拒否する `mqtt.retain_available` に依存していました。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッションテイクオーバーや破棄時に `disconnected_at` が `connected_at` より後になるタイムスタンプ順序の問題を修正しました。

  以前は `disconnected_at` が遅れて（`ensure_disconnected` 内で）記録され、新しいセッションの `connected_at` 設定後だったため、`disconnected_at > connected_at` となる競合状態が発生し、外部でのクライアントプレゼンス状態追跡が困難でした。

  修正によりテイクオーバー開始時または破棄受信時に即座に `disconnected_at` を記録し、新しいセッションの `connected_at` より遅くならないようにしました。これにより外部プレゼンス追跡システムで正しいタイムスタンプ順序が保証されます。

  注：これらのイベントが異なるクラスタノードから発せられる場合、観測される順序はノード間の時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  以前は一部クライアントでサブスクリプション一覧取得が失敗し、出力が返らないことがありました。現在は通常のサブスクリプションと共有サブスクリプションの両方で確実に動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) DashboardとREST APIに反映されるチャネル情報（`mqueue_len`、`inflight_cnt`）が、セッションテイクオーバーのリプレイ完了直後に即時更新されるよう修正しました。以前は15秒ごとの統計更新まで待っていました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) `$events/client/connack` ルールイベントに欠落していた `connected_at` フィールドを追加しました。フィールドはドキュメントには記載されていましたが実際のイベントデータにはありませんでした。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なメタデータタイムスタンプを無視するようにしました。

  以前は、`metadata.created_at` や `metadata.last_modified_at` に整数以外の値（例：日付文字列）が含まれると、EMQXが無効な値を保存し、API経由でルールを一覧・取得する際に内部エラーで失敗することがありました。

  現在は無効なメタデータタイムスタンプ値を無視し、通常の生成タイムスタンプにフォールバックするため、メタデータが不正でもルールAPIレスポンスが利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQコネクター／アクション／ソースで、接続やチャネルプロセスが予期せず終了した場合に再接続せずに切断状態のままになる問題を修正しました。再起動なしで自己回復しなかった問題です。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定のインポート時のクラッシュを修正しました。

  以前は一括インポート時に以下のようなクラッシュメッセージで失敗することがありました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storageアクションの集約モードで、コンテナ内に多数のBlobがある場合にヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTPおよびGCP PubSubアクションで、理由が `closing` の一時的な接続エラーを回復可能として扱い、ログノイズを削減しました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTTソースで、コネクターが `clean_start = false` を使い、メッセージを含むセッションを持つブローカーに再接続した場合に、メッセージがルールアクションをトリガーしなかった問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDBコネクター／アクションで、`write_syntax` リテラルやMQTTペイロードから書き込む値のUnicodeテキストを保持するよう修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQLコネクターでプリペアドステートメント無効時のクエリ実行を修正しました。以前は同時クエリが混在してエラーが発生していました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQコネクターの分離を修正しました。設定ミスや到達不能なRocketMQコネクターが同一ノード上の他のRocketMQコネクターを不安定化させなくなりました。以前は到達不能なブローカーを持つ1つのコネクターが共有クライアントスーパーバイザーを最大60秒停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard操作がハングしていました。

  デフォルトのTCP/TLS接続タイムアウトも60秒から10秒に短縮され、設定ミスのサーバーが迅速に失敗として検出されます。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時にMongoDBプロセスへのタイムアウト呼び出しが回復不能エラーとして扱われ再試行されなかった問題を修正しました。該当メッセージは再試行されます。

  影響を受ける環境では以下のようなログが出力されていました：

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis SentinelコネクターでRedisデータノードとSentinelノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquetファイルに必須キーが `undefined` または `null` のオブジェクトを書き込むとファイルが破損する問題を修正し、エラーを発生させるようにしました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafkaクライアントライブラリをアップグレードしました：`brod` を4.5.2から4.5.4に、`wolff` を4.1.7から4.1.10に。

  これによりKafkaプロデューサー・コンシューマー統合に以下の修正が含まれます：

  - SASL再認証中の接続競合状態を修正し、キューイングされたproduceリクエストのドロップや `sync` produce呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQクライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサーリクエストのメモリ増加問題を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTTクライアント依存を `1.14.6` から `1.15.1` にアップグレードしました。

  これによりMQTTブリッジ、MQTTソース、その他アウトバウンドMQTT接続を使うコネクターに以下のユーザー向け改善が含まれます：

  - pingrespタイムアウトを別タイマーではなくキープアライブタイマーから追跡し、pingresp処理が設定された `keepalive` 間隔と整合。
  - QUIC：ピアの `recv` 中止後は送信方向のみ中止し、両方向を切断しないため、半クローズされたQUICストリームの保留送信が黙って破棄されなくなりました。

#### クラスタリング

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスタ回復時間を改善しました。

  組み込みMriaデータベース管理システムがトランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTPを27.3.4.2-6から27.3.4.2-7にアップグレードしました。

  これにより、ノード起動時にネットワークパーティションが発生するとMQTTルーティングテーブルの不整合が起きる競合状態が解消されます。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTPを27.3.4.2-8にアップグレードしました。この修正がないと、ノードがクラスタに接続されていない場合にMriaアプリの起動がEMQX起動時にハングすることがあります。

- [#17220](https://github.com/emqx/emqx/pull/17220) 実行中ブローカーで `bin/emqx` と `bin/emqx_ctl` の呼び出しが `nodeup`/`nodedown` イベントをトリガーし、ブローカーログに誤解を招く `cm_registry_node_down` 警告が出る問題を修正しました。これらスクリプトが起動する一時的なヘルパーノードは隠しErlangノードとして登録されるようになりました。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスタ回復を改善しました。

  以前はレプリカノードに接続されたクライアントの一部がグローバルレジストリから失われ、セッションテイクオーバー時の不整合やDashboardの誤表示を引き起こしていました。

  修正により、ネットワークパーティションが解消された際に既存クライアントを再登録するバックグラウンドプロセスを追加しました。また、グローバルレジストリ再構築中に「Broker is recovering after a network partition」という新しいアラームを発生させます。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動的に回復可能な新しいオートヒールアルゴリズムを導入し、クラスタ回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれる場合に、クラスタ設定インポートが `required_field: node.cookie` のスキーマチェックエラーで失敗する問題を修正しました。読み取り専用の設定ルート（`node`、`rpc`）は事前のスキーマチェック前に削除され、実行中ノードの値で検証されます。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスタノード間で同じ実効設定だが生の設定表現が異なる場合に `emqx ctl conf cluster_sync status` が騒がしく誤解を招く診断を出す問題を修正しました。

  コマンドは実際の設定差分に対応しない生の表現差分を抑制し、実効設定が不整合な場合のみ警告を出します。また、生の設定キーが片方のノードにのみ存在する場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスタがネットワークパーティションを経験した際に発生する可能性のあるグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く `emqx ctl conf cluster_sync status` 警告を修正しました。

  以前はデータインポートや起動時設定ロードで、同一のアクション、ソース、ブリッジ、ルールメタデータがノード間で `created_at` や `last_modified_at` のみ異なる場合がありました。コマンドはこれらのタイムスタンプ差分を無視し、実際の設定差分のみ報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) ルートレプリケーションが応答しないターゲットクラスタへの接続でスタックした際のCluster Linkの応答性を改善し、そのようなCluster Linkの削除を早く完了するようにしました。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後のMnesiaオートヒールで同じクライアントIDの重複または古いグローバルセッションレジストリエントリが残るリークを修正しました。

  破棄およびテイクオーバーキックRPCハンドラーは対象プロセスが生存しない場合にレジストリ行を削除し、接続パスの登録スロットルは墓石行（ローカルチャネル状態なし）を認識して新規接続を無期限にブロックせずに回収します。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) CRLキャッシュの回帰を修正し、`emqx_crl_cache:evict/1` が内部URL状態を完全にクリアしなかった問題を解消しました。削除後、同じCRL URLは次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとのHTTPフェッチの繰り返しを防ぎます。

- [#17012](https://github.com/emqx/emqx/pull/17012) パスワードなしのCONNECTパケット時にパスワードベース認証バックエンドが認証チェーンを継続するよう修正し、接続を即時拒否しないようにしました。

  以前はパスワードなしで接続すると、チェーン内の最初のパスワードベース認証器（組み込みデータベース、MySQL、PostgreSQL、MongoDB、Redis、LDAP）がエラーを返し、後続の認証器が試されませんでした。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSOログインで、IDプロバイダーが `Content-Type` に `+json` 構造化構文サフィックスを含むJWKSレスポンス（例：`application/jwk-set+json; charset=utf-8`）を返すと `provider_not_ready` で失敗する問題を修正しました。これらのレスポンスを有効なJWKSコンテンツとして受け入れます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URLエンコードされたユーザー名（例：メールアドレス）を持つSSOユーザーのDashboard RBACチェックを修正し、`force_mfa` 無効時にビューアーのセルフサービスMFA無効化リクエストが正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PIDがログデータフィールドとして確実に出力されるようにしました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 特定の競合状態下で以下のような長くて難解なログが出力される問題を修正しました：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQXはより意味のある情報をログに出力し、問題のデバッグを支援します。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開時のプラグイン起動警告のノイズを削減しました。

  クラスタ設定同期時にEMQXがローカルノードからプラグイン設定を取得しようとすることをやめ、起動時の `config_not_found_on_node` 警告の繰り返しを回避します。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答を受信した際に警告ログを追加しました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が `emsgsize`（受信パケットが `mqtt.max_packet_size` を超過）でクライアント接続終了が発生した場合、ログレベルを情報から警告に引き上げました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用量報告を改善しました。

  ブローカーはcgroup v2、cgroup v1、ホストの `/proc/meminfo` の中で最も制約の厳しいメモリ読み取り値を選択します（最小の非ゼロ合計値が勝ち、使用率が大きい方が同率の場合はそちらを採用）。以前は読み取り値が誤解を招くことがありました：cgroup制限が厳しいコンテナでホストビューが高使用率を示しつつcgroup制限がほぼ枯渇している場合やその逆、メモリ制限なしでマウントされたcgroupでcgroup読み取り値が使用率を約0%に圧縮する場合など。過負荷保護閾値と `Memory used` メトリクスは実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace` がトレースフィルタータイプとして `ruleid` を受け付けるように修正しました。以前は `emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する `trace add ...`）がCLI引数パーサーに `ruleid` フィルターがなく一般エラーになっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3 へアップグレードする前に、破壊的変更点と既知の問題を必ずご確認ください。

### 強化点

#### デプロイメント

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) `GET /monitor_current` HTTP APIに対して、マッチしたルール数を示す `rules_matched` とアクション実行率（成功＋失敗）を示す `actions_executed` の2つの新しいメトリクスと対応するレートを追加しました。
- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQXはTLS 1.3のステートレスセッションチケットを使ったセッション再開をサポートし、サーバー側のセッション状態保存なしでクライアントがTLSセッションを再開可能になりました。

  **設定**

  - **ノードレベル**：`node.tls_stateless_tickets_seed`

    TLS 1.3ステートレスセッションチケット生成に使う秘密鍵シード。

  - **リスナーレベル**：`listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - `node.tls_stateless_tickets_seed` が設定され（空でない）かつリスナーSSLオプションで `session_tickets` が有効な場合のみセッションチケットが生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出力されます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808ゲートウェイに `jt808.frame.parse_unknown_message` 設定オプションを追加し、不明なメッセージIDのメッセージを解析して透過的に転送可能にしました。
- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808プロトコル2019をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合でIoTDBテーブルモデルをサポートしました。

### バグ修正

#### コアMQTT機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエストレスポンス情報プロパティ処理時の型不一致によるMQTT v5接続のクラッシュを修正しました。
- [#16514](https://github.com/emqx/emqx/pull/16514) クライアントの広告する `Maximum-Packet-Size` を超えるブローカーメッセージ受信時にWebSocket接続がクラッシュするバグを修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に `undefined` を返していた問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これはEMQX v4の後方互換性修正です。これらの関数はEMQX v5以降ではドキュメント化されていません。推奨される使い方はルール評価コンテキストのフィールドを直接参照することで、例：`SELECT clientid ...`（関数呼び出しではなく）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックは現在のEMQXノードに割り当てられたパーティションのみのリーダー接続性を検証し、不要なアイドル接続と誤警報を防止します。

  以前はKafkaコンシューマーコネクターがすべてのパーティションのリーダー接続性をチェックしていました。クラスタ展開では各ノードがパーティションのサブセットのみを所有しており、未割り当てパーティションリーダーへの接続がアイドル状態になります。Kafkaはアイドル接続をタイムアウト（デフォルト10分）で切断するため、誤警報が発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafkaリクエストタイムアウトをメタデータリクエストタイムアウトの少なくとも2倍（最低30秒）に自動設定するようにし、メタデータリクエストが長引く際の不要な再接続とリトライを減らしました。特にメタデータリクエストタイムアウトが短く設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続性テストやコネクター停止時のタイムアウト競合状態を修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) REST APIドライバー使用時のIoTDBコネクターのヘルスチェックを改善しました。

  以前はヘルスチェックでクライアント資格情報が検証されていませんでした。現在は軽量なNo-opクエリを送信し、誤設定された資格情報を早期に検出可能にしました。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsarクライアントを2.1.2にアップグレードしました。

  Pulsarプロデューサーアクションの `batch_size` が `1` に設定されている場合、単一メッセージを単一要素バッチではなくエンコードします。これによりKey Share戦略を使ったコンシューマーの負荷分散が可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTTソースがコネクターの再接続後にメッセージ受信を停止する問題を修正しました。

  以前はMQTTソースのコネクターが接続喪失から回復した際にトピックが再サブスクライブされず、ソースが動作停止していました。現在は再接続時に自動的に再サブスクライブします。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS接続失敗の問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードのアクションが複数回のヘルスチェック失敗後にコネクターが切断されるとフォールバックアクションを2回トリガーしていた問題を修正しました。

#### クラスタリング

- [#16269](https://github.com/emqx/emqx/pull/16269) Cluster Linkのルートレプリケーションプロトコル回復シーケンスで、リブートストラップが必要な場合に誤ってスキップされていた問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立したCluster Linkが設定され、一部のリンクが長期間ダウンした場合に、内部ルーティングテーブルから生きているルートが誤って削除されるCluster Linkのガベージコレクションロジックの問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc` を `3.5.1` にアップグレードしました。

  `gen_rpc` アップグレード前は、ピアノードが到達不能な場合に接続タイムアウトによるクラッシュログの長いテールが発生していました。新バージョンは長いテールをなくし、クラッシュログを読みやすい `error` ログに変換し、頻発する `"failed_to_connect_server"` ログもスロットリングしてログスパムを防ぎます。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスタの自動クリーン手順の堅牢性を改善しました。以前はノード起動時に自動クリーン機能が無効化されていると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSOバックエンドに `idp_signs_envelopes` と `idp_signs_assertions` オプションを追加し、署名検証を制御可能にしました。これらは以前正しく機能していませんでした。両オプションは後方互換のためデフォルトで `false` で、IdPがSAMLレスポンスに署名する場合は明示的に有効化する必要があります。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0未満からアップグレード後に多要素認証（MFA）が有効化できなかった問題を修正しました。原因はログインユーザーデータベースレコードの非互換性です。
- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC発行者URLが設定ファイル保存時に末尾スラッシュ付きに自動正規化され、OIDCプロバイダーのディスカバリードキュメントが末尾スラッシュなしの発行者を返すと不一致エラーになる問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) リソース例外（`resource_exception`）発生時のログ量を削減しました。これらのログはスロットリングされ、一部の大きな用語はマスクされます。
- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc` エラーのログフォーマッタークラッシュを修正しました。以前は `gen_rpc` が特定のエラーメッセージ（例：送信タイムアウト）をログ出力するとEMQXが「FORMATTER CRASH」でクラッシュしていました。フォーマッターはこれらのエラーメッセージを正しく処理します。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) JT/T 808ゲートウェイのパラメーター設定（0x8103）およびクエリ応答（0x0104）メッセージ処理で、CANバスIDパラメーター（0x0110〜0x01FF）がJSONで文字列型ではなくbase64エンコードのBYTE[8]型を使うべき問題を修正しました。

- [#16606](https://github.com/emqx/emqx/pull/16606) CoAPゲートウェイのDTLS接続モード動作を修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808ゲートウェイでGBK文字エンコーディングをサポートしました。

  JT/T 808プロトコルはSTRING型フィールドにGBKエンコーディングを指定しています。新しい `frame.string_encoding` 設定オプションを追加しました：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）。
  - `gbk`：デバイスからのGBKエンコード文字列をMQTT用にUTF-8に変換し、MQTTからデバイスへはUTF-8をGBKに変換。

  対象はナンバープレート、運転者名、テキストメッセージ、エリア名、クライアントパラメーターなどの文字列フィールドです。MQTTペイロードはこの設定に関わらず常にUTF-8エンコードを使用します。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2 へアップグレードする前に、破壊的変更点と既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージのドロップに関するログ（`buffer_worker_dropped_expired_messages`）を警告レベルで出力し、リソースIDごとにスロットリングするようにしました。これにより特定の外部リソースが受信メッセージレートに追いついていない場合を特定しやすくなります。
- [#16206](https://github.com/emqx/emqx/pull/16206) Kafkaプロデューサーコネクターに `allow_auto_topic_creation` 設定オプションを追加しました。有効化すると、クライアントがメタデータフェッチ要求を送信した際に存在しないトピックをKafkaが自動作成することを許可します。
- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDBコネクターにカスタムタイムスタンプカラム名（`ts_column`）パラメーター指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` オプションのデフォルト値を `chunk` から `frame` に変更しました。この変更により、ペイロードサイズがソケットバッファ（デフォルトは4KB）を超える場合のCPU使用率を大幅に削減できます。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが許容最大サイズを超えると、EMQXは `DISCONNECT` パケットを送信する代わりに接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` APIのパフォーマンスを最適化しました。以前はクラスターに約50,000クライアント以上が存在する場合、クライアントリスト取得のAPI呼び出しが非常に遅くなったりタイムアウトすることがありました。

### バグ修正

#### Core MQTT 機能

- [#15884](https://github.com/emqx/emqx/pull/15884) ごく稀に、グローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無期限に保持してしまう問題を修正しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルおよび共有サブスクリプション状態に不整合が蓄積される可能性がある競合状態を解消しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証およびメモリベースのセッションを使用するクライアントで、`session_stepdown_request_exception` が `calling_self` エラーにより発生しクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数におけるメモリリークを修正しました。

  以前は、`jq` の組み込み関数 `index`（例：`.key | index("name")`）を使用するとメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) ルール環境の `metadata` フィールドを含まないSQLを持つルールで、Republish Fallback Actionが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafkaデータ統合で `not_all_kafka_partitions_connected` イベント発生時のログ詳細を改善しました。
- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを含むコネクターの設定読み込みや再起動時に発生する可能性のあるメモリ不足（OOM）クラッシュを修正しました。
- [#16138](https://github.com/emqx/emqx/pull/16138) Redisクラスターのフェイルオーバー問題を修正し、コネクターが「接続中」状態で停止する問題を解消しました。

  以前は、EMQXのRedisクラスタークライアントは通常のクエリ（`GET`など）が失敗した場合にのみクラスターのトポロジーを更新していましたが、定期的な `PING` コマンドの失敗では更新されませんでした。そのため、フェイルオーバー後に他のコマンドが発行されないと、古いトポロジーを使い続けて回復できない問題がありました。

  この修正により、`PING` の失敗応答でもトポロジーの更新がトリガーされ、フェイルオーバー検知と迅速な回復が可能となりました。

- [#16212](https://github.com/emqx/emqx/pull/16212) バッファキューがメモリモードの場合、Kafkaプロデューサーのlinger時間を削除しました。

#### オブザーバビリティ

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価時に生成される過剰な監査ログエントリを削減しました。
- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログクリーンアップ時にMnesiaトランザクションがブロックされ、メモリが急増する問題を修正しました。

## 5.10.1

*リリース日: 2025-09-18*

EMQX 5.10.1にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### パフォーマンス

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアするようにし、不要なメモリ消費を削減しました。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。クライアントID、ユーザー名、パスワード、トピックなどのフィールドは、64バイトを超える場合は生パケットのスライスではなく新しいバイナリにコピーされ、Erlang VMの 'binary' 部分のメモリ使用を減らします。

#### アクセス制御

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP認証および認可を強化しました。LDAP認可は従来の単純なトピックリストに加え、JSON形式の拡張ACLルールをサポートします。認証時にクライアント情報に基づきLDAPからACLルールを取得し、クライアントのメタデータにキャッシュして認可時のLDAP問い合わせを削減します。

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証および認可の外部リソース管理を最適化しました。以前は無効化された認証または認可プロバイダーに設定されたリソースにEMQXが接続し続けることがありました。

#### データ統合

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 Tables ActionでParquet形式のデータファイル書き込みをサポートしました。

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producerコネクターおよびアクションのヘルスチェックにレート制限を追加し、AWS APIクォータに準拠しクラスター挙動を改善しました。

  - `ListStreams` と `DescribeStream` へのヘルスチェック呼び出しは、それぞれコネクター単位で5回/秒、10回/秒に制限されます。
  - クラスター内のコアノードが分散リミッターを調整し、一貫した制限を実施します。
  - ヘルスチェックがスロットルされたりタイムアウトした場合、コネクターやアクションは切断状態にせず、以前の状態を維持します。

  また、`resource_opts.health_check_interval_jitter` という新設定を導入し、`resource_opts.health_check_interval` に均一なランダム遅延を加えることで、同一コネクター下の複数アクションが同時にヘルスチェックを行う確率を減らします。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを `3.8.3.0` にアップグレードしました。これにより、EMQXが動作するEC2インスタンスに適切なIAM権限があれば、アクセスキーIDやシークレットアクセスキーを指定せずにS3コネクターを設定可能です。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTTコネクターの `static_clientids` 設定で、クライアントIDごとにユーザー名とパスワードを指定可能になりました。これはAzure IoT Hubのように各デバイス（クライアントID）に固有の認証情報が必要なケースで有用で、クラスター環境の複数ノードでの接続成功率向上に役立ちます。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTPアクションのHTTPリクエストタイムアウトを `resource_opts.request_ttl` 設定で調整可能にしました。従来は30秒で固定されていました。

#### オブザーバビリティ

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できるAPIエンドポイントを追加しました。
- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry連携にHTTP認証付きコレクター対応のHTTPヘッダー設定項目を追加しました。
- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS認証のリソースが `disconnected` 状態になった際に返される情報を改善しました。
- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary` および `GET /sources_summary` のレスポンスに `tags` フィールドを追加し、`GET /actions/:id` のフォールバックアクションにも同様の拡張を行いました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールが現在のシステム設定をHOCON形式でエクスポートするようになり、パスワードやシークレットなどの機密情報は自動的にマスクされます。

### バグ修正

#### Core MQTT 機能

- [#15361](https://github.com/emqx/emqx/pull/15361) 不正（長さ不足）な `User-Property` ペアの解析時に発生する `function_clause` エラーを修正しました。

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断済みクライアントの共有サブスクリプションに対する冗長なクリーンアップ処理を削除しました。これらは大量切断時にクラッシュを引き起こし、グローバルブローカー状態の不整合を招く可能性がありました。

- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket接続のセッション期限切れ時に発生する警告ログやクラッシュを修正しました。この問題は最近のWebSocket性能改善によって導入され、ブローカーの容量には影響しませんが以下のようなログが出力されていました：
  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK送信後に非ゼロ理由コードで切断された際の警告ログ `unclean_terminate` を排除しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際にクラスター内のルーティングテーブルと共有サブスクリプション状態に不整合が蓄積される競合状態を解消しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACLルールの処理を修正しました。

  これらのルールは内部的に `#` に変換されていましたが、MQTT仕様の制限により `$` で始まるトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使用し、`$` プレフィックス付きトピックも正しくマッチするようにしています。

- [#15844](https://github.com/emqx/emqx/pull/15844) ビルトインデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーはHTTP APIで削除できずAPIパスを破壊するためです。

  もし空ユーザーが存在し削除したい場合は、EMQXコンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアすることで不要なメモリ消費を削減しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helmチャートの問題を修正しました。デフォルト値でEMQXをデプロイすると複数レプリカが起動し、1ノード以外がクラッシュする問題がありました。現在はクラスターデプロイにはCommercial Licenseが必要なため、デフォルトは単一レプリカに変更されています。

- [#15712](https://github.com/emqx/emqx/pull/15712) 旧バージョン（5.9未満）からのローリングアップグレード時にノード起動に失敗する問題を修正しました。

  旧バージョンではZIPタイムスタンプエンコーダのバグにより、DOS時間形式の30または31番目の2秒スロットに該当する無効な秒数値がアーカイブエントリに保存されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのテキストを修正し、ライブ接続数ではなくセッションクォータを正しく反映するようにしました。

#### クラスター

- [#15788](https://github.com/emqx/emqx/pull/15788) etcdクラスター探索の問題を修正しました。共有etcdサーバー使用時に異なるクラスターのEMQXノードが誤って相互に参加してしまう問題は、etcdクライアントライブラリのバグによるものでした。

#### Smart Data Hub

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの処理を修正するために `spb_{en,de}code` 関数を導入しました。従来の `sparkplug_{en,de}code` 関数は [Protobuf仕様](https://protobuf.dev/programming-guides/json/) に従い `bytes_value` をbase64エンコード/デコードすべきところを行っていませんでした。互換性維持のため旧関数は非推奨となります。

#### データ統合

- [#15394](https://github.com/emqx/emqx/pull/15394) 非同期応答の予期しない影響でアクションのメトリクスが不整合になる稀な競合状態を修正しました。

- [#15603](https://github.com/emqx/emqx/pull/15603) MQTTブリッジで古い接続が `Connected` と表示され自動再接続されない問題を修正しました。

- [#15826](https://github.com/emqx/emqx/pull/15826) 制限付きACL環境でKafkaコンシューマコネクターのヘルスチェックが失敗する問題を改善しました。Kafkaブローカーが内部の `____emqx_consumer_probe` コンシューマグループへのアクセスを拒否した場合でも、EMQXは接続を正常とみなします。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDBドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDBアクションで誤った書き込み構文を使用した場合に発生する `function_clause` エラーも修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafkaコンシューマソースの追加失敗時（例：トピックACL拒否）に返される情報を充実させました。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafkaプロデューサーライブラリ wollf を `4.0.12` にアップグレードし、Kafkaメタデータ応答で一時的にパーティションが欠落するケースの処理を改善しました。

  稀な競合状態でKafkaが不完全なパーティションリストを返すことがあります。従来はトピック再作成時のパーティション数減少時のみ対応していましたが、一時的なパーティション欠落時は対応していませんでした。このギャップによりパーティションプロデューサーが停止しシャットダウンが無期限にブロックされることがありました。

- [#15906](https://github.com/emqx/emqx/pull/15906) Kafkaプロデューサーライブラリ Wolff を `4.0.12` から `4.0.13` にアップグレードし、`ProduceResponse` の `record_list_too_large` エラー処理を追加しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTTクライアントライブラリを1.13.8にアップグレードしました。これによりMQTTブリッジの接続性が向上します：

  - ピアブローカーがPINGRESPに応答しない場合、自動的に再接続します。
  - CONNACK待機中にTLS接続が切断された場合の処理がより迅速になります。

- [#15910](https://github.com/emqx/emqx/pull/15910) 大規模なワーカープールで複数のワーカーが同時にクラッシュした際に、ワーカープールが障害から回復できない問題を修正しました。

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

  また、`gun` および関連依存関係を2.1.0にアップグレードしました。

#### API

- [#15547](https://github.com/emqx/emqx/pull/15547) REST APIで大きなボディ（例：10MB）のHTTPリクエスト処理に失敗する問題を修正しました。

- [#15797](https://github.com/emqx/emqx/pull/15797) EMQX 4.xとの互換性向上のため、バッチパブリッシュHTTP API（`/api/v5/publish/bulk`）で `payload_encoding` の別名として `encoding` パラメータを再導入しました。これにより、旧APIを利用する既存統合がソフトウェア変更なしに継続利用可能です。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミット更新の挙動を改善し、リスナー設定更新直後にバーストレートや閾値の変更が即時反映されるようにしました。以前は内部リミッター状態の一部が正しく更新されず、設定より厳しい制限が適用されることがありました。

#### オブザーバビリティ

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTTユーザー名に非ASCII文字が含まれる場合にネットワーク混雑アラームメッセージのフォーマットで発生するクラッシュを修正しました。

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) NATSゲートウェイで、クライアント情報オーバーライドテンプレートが未定義のパケットフィールドを参照した際にクラッシュする問題を修正しました。未定義のアトムの代わりに空のバイナリを返すようにしました。

## 5.10.0

*リリース日: 2025-06-10*

EMQX 5.10.0にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### Core MQTT 機能

- [#15118](https://github.com/emqx/emqx/pull/15118) 新しい設定オプション `mqtt.subscription_max_qos_rules` を追加し、クライアントのサブスクリプションごとに許可される最大QoSレベルを制御可能にしました。これにより、特定のトピックにマッチするルールに基づき、SUBSCRIBEパケットで要求されるQoSを制限できます。現在はSUBSCRIBEパケットのトピックに基づく限定的なマッチングルール（述語）のみサポートしています。

- [#15246](https://github.com/emqx/emqx/pull/15246) WebSocket接続のパフォーマンスとリソース消費を改善しました。
  * 合成ベンチマークで1対1のMQTTメッセージング性能を測定し、CPU使用率を約20%削減し、メモリ消費もわずかに低減しました。
  * リスナー全体の接続数制限が有効な場合の接続セットアップ効率を向上させ、特に多数接続を管理するノードで効果があります。

#### デプロイメント

- [#14791](https://github.com/emqx/emqx/pull/14791) HelmチャートのEMQX StatefulSetにカスタムアノテーションをサポート追加し、ConfigMapやSecretの変更時に自動的にPodを再起動できるようにしました。これによりKubernetes上のEMQX管理の自動化と信頼性が向上します。

#### アクセス制御

- [#15250](https://github.com/emqx/emqx/pull/15250) LDAPバインド認証を改善し、LDAPエントリ属性から `is_superuser` フラグを正しく抽出するようにしました。以前はLDAPエントリに有効な `isSuperuser` 属性があっても常に `false` に設定されていました。

- [#15249](https://github.com/emqx/emqx/pull/15249) LDAP認証および認可を改善しました。

  * LDAPの `filter` および `base_dn` 設定のバリデーションを追加しました。
  * 変数展開の問題を修正しました。

#### ルールエンジン

- [#15001](https://github.com/emqx/emqx/pull/15001) ルールエンジンSQLにAIサービスを利用する `ai_completion` 関数を追加しました。

- [#15201](https://github.com/emqx/emqx/pull/15201) AI補完プロバイダー設定に `base_url` オプションを追加しました。

- [#15188](https://github.com/emqx/emqx/pull/15188) ルールイベントトピックにネームスペースを導入しました。

  | 旧イベントトピック                    | 新イベントトピック                         |
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

- [#15175](https://github.com/emqx/emqx/pull/15175) ルールエンジンでイベントトピックのワイルドカードマッチングをサポートしました。`$events/#`、`$events/sys/+` など複数イベントを一括マッチ可能です。

#### Smart Data Hub

- [#15174](https://github.com/emqx/emqx/pull/15174) スキーマレジストリ用にProtobufソースファイルバンドルのアップロードをサポートしました。

  例として、Protobufソースファイルバンドルが `/tmp/bundle.tar.gz` にあり、以下のファイル構成で `a.proto` がルートスキーマファイルの場合：

  ```
  .
  ├── a.proto
  ├── c.proto
  └── nested
      └── b.proto
  ```

  HTTP APIでこのバンドルを使って新しいスキーマを作成する例：

  ```sh
  curl -v http://127.0.0.1:18083/api/v5/schema_registry_protobuf/bundle \
    -XPOST \
    -H "Authorization: Bearer xxxx" \
    -F bundle=@/tmp/bundle.tar.gz \
    -F name=my_cool_schema \
    -F root_proto_file=a.proto
  ```

#### データ統合

- [#15248](https://github.com/emqx/emqx/pull/15248) EMQXは[Doris](https://doris.apache.org/)とのデータ統合をサポートし、SQL文によるデータ書き込みが可能です。

- [#15218](https://github.com/emqx/emqx/pull/15218) Amazon MSK（Managed Streaming for Apache Kafka）接続時のKafka ProducerおよびConsumerコネクターでIAM認証をサポートしました。EMQXがAWS EC2上で動作する場合、AWS SDKを使ってKafkaクライアント用のOAuth Bearerトークンを生成します。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflakeコネクターでパスワードの代わりに秘密鍵ファイルパスを指定可能にしました。

  ユーザーはパスワード、秘密鍵、またはどちらも使用しない（`/etc/odbc.ini`で設定）いずれかを選択してください。

- [#14983](https://github.com/emqx/emqx/pull/14983) EMQXはS3Tablesとのデータ統合をサポートしました。

  **現在の制限事項**：
  - [S3Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) カタログのみサポート（テーブルデータとメタデータはS3に存在する必要があります）。
  - [Icebergテーブルフォーマットバージョン2](https://iceberg.apache.org/spec/#version-2-row-level-deletes) のみサポート。
  - サポートされるパーティショントランスフォーム関数は以下のみ：
    - `identity`
    - `void`
    - `bucket[N]`
  - データファイルは[Avro](https://avro.apache.org/docs/1.12.0/specification/)形式でのみ書き込み。

- [#15331](https://github.com/emqx/emqx/pull/15331) influxdbアクションで、`WriteSyntax` の `timestamp` が空欄かつルールにタイムスタンプフィールドがない場合にラインプロトコル変換が失敗する問題を修正しました。現在はシステムの現在ミリ秒値を使用し、ミリ秒精度を強制します。

- [#15348](https://github.com/emqx/emqx/pull/15348) SSLクライアント向けに `middlebox_comp_mode` を設定可能にしました。従来はTLS 1.3接続で常に有効（`true`）でしたが、デフォルトは互換性維持のため `true` のままです。

  TLSが `unexpected_message, TLS client: In state hello_retry_middlebox_assert ...` のようなエラーで失敗する稀なケースでは、`middlebox_comp_mode` を `false` に設定してください。

#### マルチテナンシー

- [#15253](https://github.com/emqx/emqx/pull/15253) 新しいマルチテナンシーAPI `GET /mt/ns_list_details` と `GET /mt/ns_list_managed_details` を追加しました。既存の対応APIと似ていますが、ネームスペース名に加え関連メタデータも返します。

- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシー管理用にネームスペースを一括削除する `DELETE /mt/bulk_delete_ns` APIを追加しました。

#### CLI

- [#15158](https://github.com/emqx/emqx/pull/15158) 新しいコマンド `emqx ctl conf remove x.y.z` を追加し、既存設定からキー `x.y.z` を削除可能にしました。

#### ゲートウェイ

- [#15138](https://github.com/emqx/emqx/pull/15138) TCP/TLS、WS/WSSトランスポートプロトコルでNATSクライアント接続を受け入れるNATSゲートウェイを導入しました。

  例として、以下のNATSメッセージをトピック `sub/t`、ペイロード `hello` のMQTTメッセージに変換し、ルールエンジンやデータ統合など既存のEMQX機能とシームレスに連携します：

  ```
  PUB sub.t 5
  hello
  ```

#### Durable Storage

- [#15043](https://github.com/emqx/emqx/pull/15043) DS Raftバックエンドに基本的なメトリクスを計測する計測機能を追加し、クラスター状態、データベース概要、シャードレプリケーション、レプリカ遷移の洞察を提供します。

### バグ修正

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) 新しい禁止リストレコード作成失敗時のエラーメッセージ形式が不正だった問題を修正しました。

#### クラスター

- [#15304](https://github.com/emqx/emqx/pull/15304) `static` 探索戦略使用時にレプリカノードがコアノード探索で問題を起こす問題を修正しました。

  以前は `static_seeds` リストに明示的に含まれないコアノードを無視し、クラスターのビュー不整合や負荷不均衡を招いていました。

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker` のRPC（`badrpc`）エラー処理を修正し、誤検知によるロック成功判定を防止しました。これによりクラスター環境でのロック状態不整合やデッドロックを回避します。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) CRL配布ポイント（CDP）処理を改善しました。CDP URLが継続的に更新失敗（デフォルトタイムアウト60秒）した場合、該当URLを除外し、繰り返しエラーログを防止します。

#### ルールエンジン

- [#15247](https://github.com/emqx/emqx/pull/15247) `emqx ctl conf remove dashboard.sso.<BACKEND_NAME>` 実行時に `function_clause` エラーログが出力される問題を修正しました。

#### Smart Data Hub

- [#15285](https://github.com/emqx/emqx/pull/15285) 外部HTTPスキーマリクエストに `content-type` ヘッダーを追加しました。
- [#15224](https://github.com/emqx/emqx/pull/15224) ダッシュボード経由で外部スキーマレジストリを更新するとパスワードが `******` に誤って変更される問題を修正しました。
- [#15190](https://github.com/emqx/emqx/pull/15190) メッセージ変換でQoSおよびトピックのハードコード設定をサポートしました。

#### データ統合

- [#15274](https://github.com/emqx/emqx/pull/15274) Postgres、Matrix、TimescaleDBコネクターのヘルスチェック失敗時に完全再接続をトリガーするようにしました。以前は接続が使用不能になりハングする場合があり、OOM問題を引き起こす可能性がありました。

- [#15234](https://github.com/emqx/emqx/pull/15234) ルールテスト時にアクション未インストールやRepublish Fallbackアクションの場合にトレースイベントを追加し、フロントエンドでシミュレーション入力データのテスト結果を確認可能にしました。

- [#15219](https://github.com/emqx/emqx/pull/15219) Clickhouseコネクターのヘルスチェックタイムアウト時のログ量を削減し、タイムアウト時は `disconnected` ではなく `connecting` 状態にマークするようにしました。これにより完全再接続がトリガーされなくなります。

- [#15154](https://github.com/emqx/emqx/pull/15154) 集約モード（S3、Azure Blob Storage、Snowflake）で動作するアクションの稀な競合状態により以下のようなクラッシュログが発生する問題を修正しました：

  ```
  ** Reason for termination ==
  ** {function_clause,[{emqx_connector_aggregator,handle_close_buffer,[...], ...
  ```

- [#15147](https://github.com/emqx/emqx/pull/15147) シミュレーション入力データでルールテスト時、一部アクションがリクエストレンダリング後にトレースイベントを出力しない問題を修正しました。

  対象アクション：
  - Couchbase
  - Snowflake
  - IoTDB（Thriftドライバー）

- [#15306](https://github.com/emqx/emqx/pull/15306) コネクターのヘルスチェック応答で、依存する全アクションおよびソースのヘルスチェックを常にトリガーしていた問題を修正しました。

#### マルチテナンシー

- [#15242](https://github.com/emqx/emqx/pull/15242) マルチテナンシーのリミッター設定後にノード再起動すると、初期化時に以下のようなログが出力される問題を修正しました：

  ```
  2025-05-15T16:45:13.276895+08:00 [error] clientid: ns3mqttx_620053b2_100, msg: hook_callback_exception, peername: 127.0.0.1:39364, username: ns3, reason: {limiter_group_not_found,{mt_tenant,<<"ns3">>}}, stacktrace: [{emqx_limiter,connect,1,[{file,"emqx_limiter.erl"},{line,134}]}
  ```

#### オブザーバビリティ

- [#15299](https://github.com/emqx/emqx/pull/15299) OpenTelemetryメトリクスエクスポート時の `badarg` エラーを修正しました。

#### テレメトリー

- [#15216](https://github.com/emqx/emqx/pull/15216) プラグインが有効な場合に `emqx_telemetry` プロセスがクラッシュする問題を修正しました。

## 5.9.2

*リリース日: 2025-11-14*

EMQX 5.9.2にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### Core MQTT 機能

- [#15773](https://github.com/emqx/emqx/pull/15773) 再接続時のクライアントID登録をスロットルする機能を追加しました。
  - 以前のセッションクリーンアップが進行中の場合、同じクライアントIDによる新規接続をスロットルし、クライアントの過剰な再接続による不安定化を防止します。
  - 対象クライアントには `CONNACK` で理由コード `137`（Server Busy）と理由文字列 `"THROTTLED"` を返し、クリーンアップ完了後に再試行するよう促します。
  - 同じクライアントIDを登録する別接続の理由コードを誤って `133` 返していた問題を修正し、正しく `137` を返すようにしました。

#### データ統合

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud` ライブラリを `3.8.3.0` にアップグレードしました。これにより、EMQXが動作するEC2インスタンスに適切なIAM権限があれば、アクセスキーIDやシークレットアクセスキーを指定せずにS3コネクターを設定可能です。

- [#15585](https://github.com/emqx/emqx/pull/15585) brodクライアントをバージョン4.4.4に更新し、より広範なKafka APIをサポートしました。この更新は `JoinGroups` APIのバージョン `v0` - `v1` の非推奨対応です。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTTコネクターの `static_clientids` 設定で、クライアントIDごとにユーザー名とパスワードを指定可能になりました。これはAzure IoT Hubのように各デバイス（クライアントID）に固有の認証情報が必要なケースで有用で、クラスター環境の複数ノードでの接続成功率向上に役立ちます。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTPアクションのHTTPリクエストタイムアウトを `resource_opts.request_ttl` 設定で調整可能にしました。従来は30秒で固定されていました。

#### オブザーバビリティ

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できるAPIエンドポイントを追加しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS認証のリソースが `disconnected` 状態になった際に返される情報を改善しました。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval` 設定をデフォルトで無効化しました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VMパラメータを最適化し、パフォーマンスと安定性を向上しました：

  - 分散チャネルのバッファサイズを32MB（`+zdbbl 32768`）に増加し、Mnesiaの集中的な操作時の `busy_dist_port` アラームを防止。
  - スケジューラのビジーウェイトを無効化（`+sbwt none +sbwtdcpu none +sbwtdio none`）し、OSから見たCPU使用率を低減。
  - スケジューラバインディングタイプをdbに設定（`+stbt db`）しメッセージレイテンシを削減。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。

  - クライアント切断時に認可（authz）キャッシュを即時クリアし、不要なメモリ消費を削減。
  - クライアントID、ユーザー名、パスワード、トピックなどのフィールドは、64バイトを超える場合は生パケットのスライスではなく新しいバイナリにコピーし、Erlang VMの 'binary' 部分のメモリ使用を減らします。

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` オプションのデフォルト値を `chunk` から `frame` に変更しました。この変更により、ペイロードサイズがソケットバッファ（デフォルトは4KB）を超える場合のCPU使用率を大幅に削減できます。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが許容最大サイズを超えると、EMQXは `DISCONNECT` パケットを送信する代わりに接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` APIのパフォーマンスを最適化しました。以前はクラスターに約50,000クライアント以上が存在する場合、クライアントリスト取得のAPI呼び出しが非常に遅くなったりタイムアウトすることがありました。

- [#15884](https://github.com/emqx/emqx/pull/15884) ごく稀に、グローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無期限に保持してしまう問題を修正しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際に、クラスター内のルーティングテーブルおよび共有サブスクリプション状態に不整合が蓄積される可能性がある競合状態を解消しました。

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK送信後に非ゼロ理由コードで切断された際の警告ログ `unclean_terminate` を排除しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helmチャートの問題を修正しました。デフォルト値でEMQXをデプロイすると複数レプリカが起動し、1ノード以外がクラッシュする問題がありました。現在はクラスターデプロイにはCommercial Licenseが必要なため、デフォルトは単一レプリカに変更されています。

- [#15580](https://github.com/emqx/emqx/pull/15580) EMQX Enterprise Helmチャートに新しい変数 `emqxLicenseSecretRef` を追加しました。これにより、EMQXライセンスキーを含むKubernetes Secretを指定し、ライセンスを自動的に適用可能です。

  これはライセンスをEMQXに渡さなかった非機能の `emqxLicenseSecretName` 変数の代替です。

- [#15712](https://github.com/emqx/emqx/pull/15712) 旧バージョン（5.9未満）からのローリングアップグレード時にノード起動に失敗する問題を修正しました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームのメッセージを修正し、ライブ接続数ではなくセッションクォータを正しく反映するようにしました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTPバージョンを26.2.5.2から26.2.5.14にアップグレードしました。このアップグレードにはEMQXに影響するTLS関連の修正が含まれます：
  - 証明書更新時の競合によるTLS接続クラッシュを修正。
  - RSASSA-PSSパラメータで署名されたRSA証明書のサポートを追加。これまではTLSハンドシェイクが `bad_certificate` / `invalid_signature` エラーで失敗していました。

- [#16237](https://github.com/emqx/emqx/pull/16237) SSO無効化後もOIDC SSO関連ログが出力される問題を修正しました。

- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境でOIDCログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACLルールの処理を修正しました。

- [#15844](https://github.com/emqx/emqx/pull/15844) ビルトインデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可（authz）キャッシュを即時クリアすることで不要なメモリ消費を削減しました。

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証およびメモリベースのセッションを使用するクライアントで、`session_stepdown_request_exception` が `calling_self` エラーにより発生しクラッシュする問題を修正しました。

  <details>
  <summary>エラーログ</summary>

  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### データ統合

- [#15616](https://github.com/emqx/emqx/pull/15616) Kafka接続は、デフォルトのプロービングトピックで `topic_authorization_failed` エラーが返されても正常とみなすようになりました。

- [#15826](https://github.com/emqx/emqx/pull/15826) 制限付きACL環境下でのKafkaコンシューマーコネクターのヘルスチェック動作を改善しました。従来、設定されたユーザーに内部の `____emqx_consumer_probe` コンシューマーグループへのアクセス権限がない場合、Kafkaコンシューマーコネクターのヘルスチェックが失敗することがありました。この修正により、Kafkaブローカーが「ACL拒否」の応答を返した場合でも、EMQXは接続を正常とみなします。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDBドライバーにおけるatomおよびプロセスリークを修正しました。

  また、GreptimeDB Actionsで特定の誤った書き込み構文を使用した場合に発生する可能性のあった `function_clause` エラーを修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafkaコンシューマーソースの追加に失敗した際（例：トピックACLが拒否された場合など）、返される情報を充実させました。

- [#15850](https://github.com/emqx/emqx/pull/15850) MQTTブリッジで、古い接続が誤って `Connected` と表示され、再接続に失敗する問題を修正しました。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafkaプロデューサーライブラリwollfを `4.0.12` にアップグレードし、Kafkaメタデータ応答で一時的にパーティションが欠落している場合の処理を改善しました。

  稀なレースコンディションで、Kafkaが不完全なパーティションリストを返すことがあります。従来はトピックがパーティション数を減らして再作成された場合のみ対応していましたが、一時的にパーティションが欠落している場合には対応していませんでした。このギャップにより、パーティションプロデューサーが停止し、シャットダウンが無期限にブロックされる可能性がありました。

- [#15906](https://github.com/emqx/emqx/pull/15906) KafkaプロデューサーライブラリWolffを `4.0.12` から `4.0.13` にアップグレードしました。これにより、`ProduceResponse` の `record_list_too_large` エラーに対応しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTTクライアントライブラリを1.13.8にアップグレードしました。これによりMQTTブリッジの接続性が向上しました：

  - ピアブローカーがPINGRESPに応答しない場合、コネクターが自動的に再接続します。
  - TLS経由のブリッジで、CONNACK待機中に接続が切断された場合の処理がより迅速になりました。

- [#15910](https://github.com/emqx/emqx/pull/15910) 大規模なワーカープールで複数のワーカーが同時にクラッシュした場合、ワーカープールの回復に失敗する問題を修正しました。

  対象および修正済みのコネクター：

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

  また、`gun` と関連依存関係を2.1.0にアップグレードしました。

- [#16010](https://github.com/emqx/emqx/pull/16010) 発生元ルールのSQLにルール環境の `metadata` フィールドが含まれていない場合に、Republish Fallback Actionが `function_clause` エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafkaデータ統合で `not_all_kafka_partitions_connected` イベントが発生した際のログ詳細を改善しました。

- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のActionsを含むコネクターを含む設定の読み込みや再起動時に発生する可能性のあるメモリ不足（OOM）クラッシュを修正しました。

- [#16138](https://github.com/emqx/emqx/pull/16138) Redisクラスターのフェイルオーバー問題を修正しました。この問題により、コネクターが「接続中」状態のまま停止できなくなることがありました。

  従来、EMQXのRedisクラスタークライアントは通常のクエリ（例：`GET`）が失敗した場合にのみクラスターのトポロジーを更新していましたが、定期的な `PING` コマンドの失敗では更新がトリガーされませんでした。そのため、フェイルオーバー後に他のコマンドが発行されない場合、古いトポロジーを使い続けて回復できないことがありました。

  この修正により、`PING` 応答の失敗もクラスターのトポロジー更新をトリガーし、フェイルオーバー検知と迅速な回復を可能にしました。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数におけるメモリリークを修正しました。

  従来、`jq` の組み込み関数 `index`（例：`.key | index("name")`）を使用するとメモリリークが発生していました。

#### Smart Data Hub

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換およびスキーマ検証で不整合を引き起こす可能性のあったインデックス問題を修正しました。1つのアイテムを削除するとトピックインデックスが破損し、無効化された後も次のアイテムがアクティブのままになることがありました。

- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value` メトリクスの正しい処理のために `spb_{en,de}code` 関数を導入しました。従来の `sparkplug_{en,de}code` 関数は、[Protobuf仕様](https://protobuf.dev/programming-guides/json/)に従い `bytes_value` の値をbase64エンコード/デコードすべきところを対応していませんでした。これを解決するため、新たに `spb_{en,de}code` 関数を追加し、旧関数は後方互換性のため非推奨としました。

#### オブザーバビリティ

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error` メトリクスの誤カウントを修正しました。

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTTユーザー名に非ASCII文字が含まれる場合にネットワーク輻輳アラームメッセージのフォーマットで発生するクラッシュを修正しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）のループ評価時に生成される過剰な監査ログエントリを削減しました。

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログのクリーンアップ中にMnesiaトランザクションがブロックされ、メモリが急増する問題を修正しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPPゲートウェイのグローバルチェーン名が誤っていた問題を修正しました。これらのゲートウェイの組み込み認証データが `unknown:global` にまとめられており、ゲートウェイ間で競合が発生していました。

- [#15699](https://github.com/emqx/emqx/pull/15699) ゲートウェイ（例：CoAP）の組み込み認証データがノード停止や再起動時に誤って削除される問題を修正しました。

- [#15822](https://github.com/emqx/emqx/pull/15822) OCPP接続が一定数のメッセージ送信後にクラッシュする問題を修正しました。

#### レートリミット

- [#15794](https://github.com/emqx/emqx/pull/15794) 接続レートリミットの更新動作を改善し、リスナー設定変更後にバーストレートやレート閾値の変更が即時反映されるようにしました。従来は内部リミッター状態の一部が正しく更新されず、設定よりも厳しいレート制限が適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHookのTLSオプションを修正し、gRPCクライアントがTLSハンドシェイク時にサーバーのホスト名を正しく検証できるようにしました。

## 5.9.1

*リリース日: 2025-07-02*

EMQX 5.9.1にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry gRPC（HTTP/2経由）統合においてカスタムHTTPヘッダーのサポートを追加しました。これにより、HTTP認証を必要とするコレクターとの互換性が向上します。

- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシ管理用に、ネームスペースを一括削除できる `DELETE /mt/bulk_delete_ns` APIを追加しました。

- [#15158](https://github.com/emqx/emqx/pull/15158) 新しいコマンド `emqx ctl conf remove x.y.z` を追加し、既存設定からキーのパス `x.y.z` を削除できるようにしました。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflakeコネクターでパスワードの代わりに秘密鍵ファイルパスを指定できるように対応しました。

  ユーザーはパスワード、秘密鍵、またはどちらも使わない（`/etc/odbc.ini`でパラメータ設定）いずれかを選択してください。

- [#15043](https://github.com/emqx/emqx/pull/15043) DS Raftバックエンドに基本的なメトリクスを追加し、クラスター状態、データベース概要、シャードレプリケーション、レプリカ遷移の可視化を強化しました。

### バグ修正

#### データ統合

- [#15331](https://github.com/emqx/emqx/pull/15331) InfluxDBアクションで、`WriteSyntax` の `timestamp` が空欄かつルールにタイムスタンプフィールドがない場合にラインプロトコル変換が失敗する問題を修正しました。現在のシステム時刻（ミリ秒単位）を使用し、ミリ秒精度を強制します。

- [#15274](https://github.com/emqx/emqx/pull/15274) Postgres、Matrix、TimescaleDBコネクターのレジリエンスを向上させ、ヘルスチェック失敗時に完全な再接続をトリガーするようにしました。従来は失敗したヘルスチェックが接続を壊れた状態にし、操作がハングしメモリ不足を引き起こす可能性がありました。

- [#15154](https://github.com/emqx/emqx/pull/15154) 集約モードで動作するActions（例：S3、Azure Blob Storage、Snowflake）で稀に発生するレースコンディションによるクラッシュを修正しました。エラー例：

  ```
  ** Reason for termination ==
  ** {function_clause,[{emqx_connector_aggregator,handle_close_buffer,[...], ...
  ```

- [#15147](https://github.com/emqx/emqx/pull/15147) ルールテストでシミュレート入力データを使用した際、一部のActionsがリクエストレンダリング後にトレースイベントを発行しない問題を修正しました。

  対象Actions：

  - Couchbase
  - Snowflake
  - IoTDB（Thriftドライバー）

- [#15383](https://github.com/emqx/emqx/pull/15383) MQTTブリッジで起動失敗時にトピックインデックステーブルが正しくクリーンアップされずリソースリークが発生する可能性を修正しました。インデックステーブルを正しく削除するようにしました。

#### Smart Data Hub

- [#15224](https://github.com/emqx/emqx/pull/15224) ダッシュボード経由で外部スキーマレジストリを更新した際、既存のパスワードが `******` に上書きされる問題を修正しました。更新時にパスワードが正しく保持されます。

- [#15190](https://github.com/emqx/emqx/pull/15190) メッセージ変換でQoSおよびトピックにハードコード値を指定できるように強化しました。

#### オブザーバビリティ

- [#15299](https://github.com/emqx/emqx/pull/15299) OpenTelemetryメトリクスのエクスポート時に発生する `badarg` エラーを修正しました。

#### テレメトリ

- [#15216](https://github.com/emqx/emqx/pull/15216) プラグイン有効化時に発生する可能性のあった `emqx_telemetry` プロセスのクラッシュを修正しました。

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) ブラックリスト作成失敗時に返されるエラーメッセージのフォーマットを修正しました。

#### クラスタリング

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker` モジュールでの `badrpc` エラー処理を修正し、チャネル登録時のデッドロックリスクを低減しました。これまでは誤検知によりクラスタ状態が不整合となり、デッドロックが発生する可能性がありました。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) 証明書失効リスト（CRL）配布ポイントURLのリフレッシュ処理を改善し、繰り返し失敗した場合はリフレッシュを停止（デフォルト60秒）するようにしました。これにより到達不能なURLによる過剰なエラーログを防ぎ、システムの安定性を向上させます。

## 5.9.0

*リリース日: 2025-05-02*

EMQX 5.9.0にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#14721](https://github.com/emqx/emqx/pull/14721) 遅延パブリッシュの間隔制限を4294967秒（約49.7日）から42949670秒（約497日）に変更しました。

- [#14595](https://github.com/emqx/emqx/pull/14595) `retainer.enable` フラグを非推奨化しました。Retainerはゾーン設定の `mqtt.retain_available` フラグに基づき自動的に開始・停止します。

#### インストールとデプロイメント

- [#14930](https://github.com/emqx/emqx/pull/14930) macOS 15（Sequoia）パッケージのリリースを開始しました。

- [#14590](https://github.com/emqx/emqx/pull/14590) 評価ライセンスで動作するノードの最大アップタイムを1ヶ月に制限しました。制限到達後は新規接続を拒否します。

#### ネームスペース

- [#14261](https://github.com/emqx/emqx/pull/14261) MQTTクライアント管理におけるネームスペース機能を強化しました。

  **新機能**：

  - ネームスペースクライアント認識：`tns` 属性を持つMQTTクライアントをネームスペースクライアントとして扱います。
  - ネームスペースインデックス：クライアントIDインデックスにMQTTクライアントのネームスペース（`tns`）を追加し、マルチテナンシ対応を強化。

  **API**：

  - ネームスペース一覧取得（ページネーション対応）：`/api/v5/mt/ns_list`
  - ネームスペース内クライアントセッション一覧取得（ページネーション対応）：`/api/v5/mt/:ns/client_list`
  - ネームスペース内アクティブクライアントセッション数取得：`/api/v5/mt/:ns/client_count`

  **設定**：

  - ネームスペースごとのセッション上限：`multi_tenancy.default_max_sessions` 設定を追加。

  注記：

  - 管理者ネームスペース（管理者ユーザーグループ）は本プルリクエストに含まれておらず、現在開発中です。

- [#14884](https://github.com/emqx/emqx/pull/14884) ネームスペース設定管理用のHTTP APIを追加しました。

- [#14840](https://github.com/emqx/emqx/pull/14840) ネームスペース機能のクライアントおよびテナントレートリミッター設定用HTTP APIエンドポイントを追加しました。

#### 認証と認可

- [#14584](https://github.com/emqx/emqx/pull/14584) ダッシュボードの2要素認証（2FA）ログインに対する認証アプリのサポートを追加しました。

- [#14979](https://github.com/emqx/emqx/pull/14979) 認証および認可テンプレートで `zone` と `listener` のサポートを追加しました。また、ACLルールの `who` マッチ条件にも `zone` と `listener` を追加しました。

  これにより、リスナー単位またはゾーン単位のアクセス制御が可能になります。例：

  - HTTP認証器へのリクエストボディテンプレートで `zone` 名を送信：`{"username": "${username}", "zone": "${zone}"}`
  - `acl.conf` でSSLリスナー経由のクライアントのみ `${username}/#` をサブスクライブ許可：`{allow, {listener, "ssl:default"}, subscribe, ["${username}/#"]}.`

- [#14976](https://github.com/emqx/emqx/pull/14976) 認証器の事前条件設定をサポートしました。

  これにより、クライアント情報に基づき認証器の呼び出しを選択的に制御でき、不必要な認証リクエストを回避可能です。例として、`tcp:default` 経由のクライアントのみHTTP認証器を呼び出し、`ssl:default` 経由のクライアントのみPostgre認証器を呼び出す設定が可能です。

- [#14966](https://github.com/emqx/emqx/pull/14966) デフォルトのダッシュボード管理者ユーザーの削除を可能にしました。ただし、削除には少なくとも他に1人の管理者ユーザーが存在する必要があります。

- [#14358](https://github.com/emqx/emqx/pull/14358) LDAP認証/認可テンプレートで使用可能な変数を他の認証/認可ソースと同様に制限し、サポート外の変数はレンダリングされないようにしました。

- [#14610](https://github.com/emqx/emqx/pull/14610) 外部ソースから取得または組み込みデータベースに格納された認可ルールの追加フィールドを処理可能にしました。

  新たにサポートするフィルター：

  - `username_re`：ユーザー名の正規表現フィルター
  - `clientid_re`：クライアントIDの正規表現フィルター
  - `ipaddr`：IPアドレス/マスクによるフィルター

  これらのフィルターがすべて一致した場合にのみルールが適用されます。

- [#14329](https://github.com/emqx/emqx/pull/14329) 認証および認可で外部リクエストに使用するテンプレートで `${peerport}` 変数を利用可能にしました。

- [#14286](https://github.com/emqx/emqx/pull/14286) 認証および認可のノードレベルキャッシュを実装しました。

  一部の認証・認可方式は外部サービス呼び出しを必要とし、クライアントの頻繁な再接続時にEMQXおよび外部サービスの負荷が増大します。

  本機能により、以下のバックエンドでノードレベルのキャッシュを有効化可能です：

  - HTTP
  - LDAP
  - MongoDB
  - MySQL
  - PostgreSQL
  - Redis

#### REST API

- [#14254](https://github.com/emqx/emqx/pull/14254) `/status` HTTPエンドポイントでクラスター名を返すようにしました。

- [#14972](https://github.com/emqx/emqx/pull/14972) プラグイン設定の個別ダウンロード/アップロードAPIを実装しました。

- [#15013](https://github.com/emqx/emqx/pull/15013) ルールエンジンHTTP APIのルール情報に新たに `action_details` フィールドを追加しました。このフィールドには各ルールが参照するアクションのタイプ、名前、状態が含まれます。

- [#14735](https://github.com/emqx/emqx/pull/14735) ルールエンジンHTTP APIのルール情報に `last_modified_at` フィールドを追加しました。

#### クラスタリング

- [#14766](https://github.com/emqx/emqx/pull/14766) Durable Storageのデータレプリケーションを担当するノードがクラスタから離脱するのを防ぐため、`emqx ctl cluster leave` コマンドに保護機能を追加しました。

- [#14040](https://github.com/emqx/emqx/pull/14040) ノードのリバランス処理中の内部RPC呼び出しにタイムアウトを追加しました。従来はノードが応答しない場合にリバランス処理がハングすることがありました。

- [#14892](https://github.com/emqx/emqx/pull/14892) クラスターのロードリバランスを強化しました：

  - core/replicantクラスターでの負荷不均衡を修正。従来、特定条件下で全てのトランザクションが単一のcoreノードに送られることがありました。
  - coreノードに対するreplicantノードのリバランス用CLIコマンドを追加：
    - `emqx_ctl cluster core rebalance plan`
    - `emqx_ctl cluster core rebalance status`
      - `emqx_ctl cluster core rebalance confirm`
      - `emqx_ctl cluster core rebalance abort`

- [#14907](https://github.com/emqx/emqx/pull/14907) ノードの避難処理の安定性を改善しました。従来、避難処理がデッドループに陥り手動介入が必要になることがありました。

#### データ統合

- [#14118](https://github.com/emqx/emqx/pull/14118) Mysqlアクションで `ON DUPLICATE KEY UPDATE` をサポートしました。

  例：

  ```
  INSERT INTO t1 (a,b,c) VALUES (${id},${clientid},${qos}) ON DUPLICATE KEY UPDATE a=a;
  ```

  ただし、`ON DUPLICATE KEY UPDATE` 節内でのプレースホルダー（`${var}`）はサポートしていません。

- [#14629](https://github.com/emqx/emqx/pull/14629) S3およびAzure Blob Storageアクションで [JSON Lines](https://jsonlines.org/) コンテナタイプをサポートしました。

- [#14642](https://github.com/emqx/emqx/pull/14642) ローカルディスクにイベントをJSON Lines形式でログ出力する新しいコネクターおよびアクションタイプを追加しました。

- [#14996](https://github.com/emqx/emqx/pull/14996) RabbitMQアクションでデフォルトエクスチェンジの使用をサポートしました。

- [#14901](https://github.com/emqx/emqx/pull/14901) スキーマレジストリに新しいスキーマタイプ `external_http` を追加しました。このスキーマタイプにより、ペイロードに対して任意の操作を行い、結果をルールで使用できる外部HTTPサーバーを設定可能です。

- [#14722](https://github.com/emqx/emqx/pull/14722) MQTTコネクターに新たに `connect_timeout` オプションを追加しました。接続確立待機中に接続処理が停止できる最大秒数を制御します。値を小さくすると接続問題のフィードバックが早くなります。

- [#14615](https://github.com/emqx/emqx/pull/14615) `ehttpc` HTTPドライバーを利用する複数の統合で `max_inactive` パラメータの設定をサポートしました。

- [#14459](https://github.com/emqx/emqx/pull/14459) フォールバックアクションをサポートしました。

  これらは、データ統合アクションでメッセージ処理に失敗した場合（バッファオーバーフローやTTL到達によるドロップを含む）にトリガーされます。すべてのデータ統合アクションで設定可能です。

  Kafkaアクションの設定例：

  ```
  actions.kafka_producer.my_action {
    fallback_actions = [
      {kind = reference, type = mqtt, name = mqtt_fallback_publisher},
      {kind = republish, args = {topic = "fallback/action/republish"}}
    ]
    # ...
  }
  ```

- [#14582](https://github.com/emqx/emqx/pull/14582) シリアライズ前後の内部JSON表現の不要な前後処理を回避するようにしました。

#### 管理

- [#14845](https://github.com/emqx/emqx/pull/14845) ゲートウェイ設定やリスナー変更時に既存リスナーの不要な再起動を回避しました。

- [#14773](https://github.com/emqx/emqx/pull/14773) ゾーンまたはリスナーに設定されたレートリミット機能（`bytes_rate`、`messages_rate`、`max_conn_rate`）を改善しました。

  - レートリミットアルゴリズムを簡素化し、クライアントへのバックプレッシャーをかけずにメッセージをドロップします。QoS1/QoS2メッセージの場合は適切な理由コードも返します。
  - レートリミットのランタイム再設定を可能にしました。
  - ウィンドウサイズと制限を明示的に指定可能にしました。例：`messages_rate = "300/5m"` は5分間に300メッセージ、`messages_rate = "10/10s"` は10秒間に10メッセージを意味します。後者は変動に対してより厳格です。
  - バーストレートはゾーンおよびリスナーオプションの `messages_burst`、`bytes_burst`、`max_conn_burst` で指定可能です。例：`messages_burst = 1000/h` は1時間あたり1000メッセージの追加送信を許可します。

- [#14341](https://github.com/emqx/emqx/pull/14341) ネームスペース機能でネームスペースごとのセッション数制限をサポートしました。`client.authenticate` フックポイントの `quota_exceeded` エラー理由をサポートします。

- [#14679](https://github.com/emqx/emqx/pull/14679) `exhook.proto` をv3に更新し、OnMessagePublishコールバックで `User-Property` パラメータを渡せるようにしました。

- [#14963](https://github.com/emqx/emqx/pull/14963) プラグインに新しい `on_health_check/1` コールバックを追加し、ヘルスステータスを提供可能にしました。ヘルスステータスはHTTP APIおよびCLI経由でエクスポートされます。

#### ダッシュボード

- [#14750](https://github.com/emqx/emqx/pull/14750) ダッシュボードで「共有サブスクリプション」値の表示を修正しました。従来は古い値が表示されることがありました。

- [#14638](https://github.com/emqx/emqx/pull/14638) ダッシュボードのデフォルトパスワード定義にファイルシークレット（例：`file://...`）を使用可能にしました。

- [#14255](https://github.com/emqx/emqx/pull/14255) ダッシュボードユーザーのパスワード有効期限機能を導入しました。

- [#15014](https://github.com/emqx/emqx/pull/15014) ダッシュボードのセキュリティを強化し、複数回のログイン失敗後に一定期間ログインをブロックする機能を追加しました。試行回数とロック期間は設定可能です。

- [#15132](https://github.com/emqx/emqx/pull/15132) SAMLベースのシングルサインオンで、Assertion Consumer Service（ACS）からのレスポンスのContent-Typeが誤って `application/xml` となっていた問題を修正し、正しく `application/x-www-form-urlencoded` に変更しました。これにより一部のIDプロバイダーとの互換性が向上します。

#### オブザーバビリティ

- [#14794](https://github.com/emqx/emqx/pull/14794) ログトレースのHTTP APIインターフェースに `payload_limit` パラメータを追加しました。従来はペイロードが1024バイトを超えると切り捨てられていましたが、この制限を設定可能にしました。

- [#14876](https://github.com/emqx/emqx/pull/14876) ルールエンジンのエンドツーエンドトレーシングを追加しました。対象は以下のエントリです：

  - ルールをトリガーするクライアントパブリッシュメッセージ
  - ルールをトリガーするクライアントイベントおよびアラートイベント
  - ソーストリガーのルール
  - ルールによって実行されるアクション

  制限事項：フォールバックアクショントレースは現在サポートしていません。

- [#14723](https://github.com/emqx/emqx/pull/14723) Prometheus Push Gateway設定に `method` オプションを追加しました。従来のデフォルトは `post` でしたが、現在は `put` です。

  `put` メソッドはPushgateway内の同一ジョブのメトリクスを置き換え、削除されたメトリクスが残留するのを防ぎます。

  詳細は[PUTメソッド](https://github.com/prometheus/pushgateway?tab=readme-ov-file#put-method)を参照してください。

- [#14636](https://github.com/emqx/emqx/pull/14636) `packets.publish.dropped` メトリクスを非推奨化し、より意味のある2つの新メトリクスに置き換えました：

  - `messages.dropped.quota_exceeded`：クライアントが設定されたレート制限（例：QoS 0メッセージ数）を超えた場合にトリガー
  - `messages.dropped.receive_maximum`：セッションがQoS 2メッセージのReceive Maximum制限に達した場合にトリガー

  これにより、異なる条件でメッセージがドロップされる理由をより正確に把握できます。

- [#14540](https://github.com/emqx/emqx/pull/14540) 認証および認可のレイテンシ測定を設定可能にし、Prometheusヒストグラムとしてメトリクスを公開します。

- [#14264](https://github.com/emqx/emqx/pull/14264) crash_dumpファイルにタイムスタンプを追加し、次回のクラッシュダンプで上書きされないようにしました。

- [#15119](https://github.com/emqx/emqx/pull/15119) セッションレジストリテーブルサイズのハイウォーターマークメトリクスを追加しました。これはアクティブセッションのピーク数を示し、ダッシュボードの概要ページでセッションリソース使用状況の監視に役立ちます。

- [#15117](https://github.com/emqx/emqx/pull/15117) cinfo認証式評価失敗時の警告ログを簡潔化し、クラッシュと誤認されにくくしました。

  旧ログ例：

  ```
  2025-04-25T13:15:59.993395+00:00 [warning] tag: AUTHN, clientid: mqttx_a50058aa, msg: authenticator_error, peername: 127.0.0.1:60842,
  reason: {case_clause,{error,#{error => #{reason => var_unbound,var_name => <<"cert_common_name">>},
  cause => "clientinfo_auth_expression_evaluation_error"}}},
  stacktrace: [{emqx_authn_cinfo,do_check,2,[{file,"emqx_authn_cinfo.erl"},{line,94}]},{emqx_authn_cinfo,check,2,[{file,"emqx_authn_cinfo.erl"},{line,82}]},{emqx_authn_chains,authenticate_with_provider,2,...
  ```

  新ログ例：

  ```
  2025-04-25T15:46:50.748732+02:00 [warning] clientid: client1,
  msg: clientinfo_auth_expression_evaluation_error,
  peername: 127.0.0.1:53919,
  reason: #{reason => var_unbound,var_name => <<"cert_common_name">>}
  ```

#### CLI

- [#14691](https://github.com/emqx/emqx/pull/14691) CLIコマンド `emqx ctl data export` でエクスポートするデータのフィルタリング機能を追加しました。`cluster.hocon` のルートキーやテーブルセットを指定可能で、`POST /data/export` と同様の動作を実現します。

#### 設定ファイル

- [#14647](https://github.com/emqx/emqx/pull/14647) `cluster.hocon` のバックアップを設定可能な間隔で作成するようにしました。単一の設定更新ごとにバックアップを作成するのではなく、複数の変更をまとめてバックアップすることでバックアップ数を削減します。

#### プラグインと拡張

- [#14957](https://github.com/emqx/emqx/pull/14957) プラグイン設定更新処理を強化しました：

  - プラグインの `on_config_changed` コールバックの応答を尊重するようにしました。これにより、停止中のプラグインでも設定更新時に適切にコールバックが呼ばれます。
  - `on_config_changed` コールバックの結果を尊重する新しいプラグイン設定更新方法を導入しました。

#### ゲートウェイ

- [#14017](https://github.com/emqx/emqx/pull/14017) GB/T 32960ゲートウェイでカスタマイズされたInfoReportデータメッセージタイプの解析をサポートしました。

#### MQTT over QUIC

- [#14431](https://github.com/emqx/emqx/pull/14431) 新しいQUICスタック quicer 0.2.3 に切り替えました：

  - msquic 2.3.8 + パッチ
  - リソース管理の進化
  - リスナーの動的設定変更対応準備

#### システムアップグレード

- [#14639](https://github.com/emqx/emqx/pull/14639) EMQXをErlang/OTP 27でリリースしました。

### バグ修正

#### コアMQTT機能

- [#14707](https://github.com/emqx/emqx/pull/14707) strict_modeでDUPフラグがセットされたQoS 2のPUBLISHパケットが誤って無効と判断される問題を修正しました。

- [#14192](https://github.com/emqx/emqx/pull/14192) 認証/認可期限切れで切断されたクライアントが遺言メッセージを送信できるようにしました。従来は認可期限切れ直後の送信で認可ルールを通過できず送信できませんでした。

- [#14122](https://github.com/emqx/emqx/pull/14122) QoS 2およびQoS 1のメッセージに対する `PUBACK` と `PUBREC`/`PUBCOMP` の処理を修正しました。

  従来、ブローカーはQoS 2メッセージに対する `PUBACK` やQoS 1メッセージに対する `PUBREC`/`PUBCOMP` を受け入れていましたが、現在はこのような不正なパケットを送信するクライアントを切断します。

- [#15106](https://github.com/emqx/emqx/pull/15106) `GET api/v5/clients_v2` APIで重複した `clientid` が返されるバグを修正しました。`chaninfo` イベントの誤復活によりクライアントデータが重複して表示されていました。修正によりクライアント一覧の重複表示が解消されます。

- [#14906](https://github.com/emqx/emqx/pull/14906) Mriaを0.8.12.1に更新し、予期しない終了シグナルによる警告を解消しました。

  ```
  2025-01-10T20:00:00+00:00 [warning] clientid: C1, msg: emqx_session_mem_unknown_message, message: {'EXIT',<0.123456.0>,normal}
  ```

- [#15084](https://github.com/emqx/emqx/pull/15084) クライアント属性 `zone` と `listener` を文字列関数の入力として使用可能にしました。

  従来、`regex_match` などの関数で `zone` と `listener` が内部的にatomであるため例外が発生していました。

#### インストール

- [#14624](https://github.com/emqx/emqx/pull/14624) macOSリリースパッケージのOpenSSL動的リンク問題を修正しました。

  EMQXのzipパッケージはmacOSで起動に失敗することがありました。これはquicerアプリケーションがシステムインストールのOpenSSLに動的リンクしており、EMQXビルドプロセスで署名されていなかったためです。

  現在は動的リンクを無効化し、macOS向けOTPもOpenSSLの動的リンクを無効化しています。

#### REST API

- [#14771](https://github.com/emqx/emqx/pull/14771) `GET /clients_v2` HTTP APIの返却結果数が要求した制限を超える問題を修正しました。

  注意：ローリングアップグレード中は、全ノード移行完了までこのAPIで全クライアントが一覧化されない場合があります。旧コアノードに対してAPIリクエストを行うことで全クライアントを取得可能です。

- [#14182](https://github.com/emqx/emqx/pull/14182) `POST /publish` HTTP APIで遅延メッセージをパブリッシュした場合、従来は202レスポンスと理由コード16（「該当サブスクライバーなし」）が返されていましたが、現在は200レスポンスとメッセージIDが返されます。

#### MQTT Durable Sessions

- [#14674](https://github.com/emqx/emqx/pull/14674) EMQX Durable Storageが作成するRocksDBの情報ログファイル数とサイズを制限しました。

- [#14498](https://github.com/emqx/emqx/pull/14498) Durable Sessionsのパフォーマンスを改善しました：

  - アイドル状態のDurable SessionはCPUを消費しなくなりました。
  - QoSアップグレード機能を修正し、有効時にサブスクライバーがサブスクライブQoSより高いQoSのメッセージを受信しなくなりました。

- [#14933](https://github.com/emqx/emqx/pull/14933) DS RaftバックエンドのDurable Storageが、既にクラスタを離脱したストレージサイトに割り当てられる稀なエッジケースを修正しました。

#### 認証と認可

- [#14777](https://github.com/emqx/emqx/pull/14777) JWT認証の設定更新を修正しました。外部JWKSエンドポイントを使用する構成で一部フィールドが正しく更新されないことがありました。

- [#14556](https://github.com/emqx/emqx/pull/14556) ノード起動またはシャットダウン時に稀に発生する誤検知の認証成功を修正しました。

- [#15059](https://github.com/emqx/emqx/pull/15059) Redis認証設定を無効な値で更新した際の反応を修正しました。

  従来は認証器がクラッシュし認証適用が停止することがありましたが、現在は適切なエラーをユーザーに返し更新を拒否します。

- [#14303](https://github.com/emqx/emqx/pull/14303) `scram:http` 認証の不具合を修正しました。HTTPコネクターに送信されるリクエストが不正で認証失敗を引き起こしていました。

#### クラスタリング

- [#14778](https://github.com/emqx/emqx/pull/14778) 稼働中ノードの `data/certs` または `data/authz` ディレクトリに壊れたシンボリックリンクがある場合、別ノードがクラスタに参加できないバグを修正しました。

- [#14936](https://github.com/emqx/emqx/pull/14936) 稀にグローバルルーティングテーブルが長期間クラスタを離脱したノードのルーティング情報を保持し続ける問題を修正しました。

- [#14977](https://github.com/emqx/emqx/pull/14977) `emqx ctl conf cluster_sync status` コマンドのノード表示順序を修正しました。新旧設定のノード名が逆に表示されていました。

#### クラスターリンク

- [#15067](https://github.com/emqx/emqx/pull/15067) クラスターリンクのルートレプリケーションに関する複数の問題を修正しました。

  - クラスターリンクの誤設定時にレプリケーションプロセスが不安定な再接続ループに陥り、誤設定解消後もレプリケーションが妨げられる問題。
  - 存在しないMQTTクライアント接続を閉じようとしてクラッシュする問題。
  - ルーティングテーブルに共有サブスクリプションがある場合にレプリケーション起動時にクラッシュする問題。

#### ルールエンジン

- [#14849](https://github.com/emqx/emqx/pull/14849) `POST /rule_test` のレスポンスから誤って含まれていた内部フィールド `event_type` を削除しました。これは実際のイベントには存在しないため、ルールテスト出力で混乱を招いていました。

- [#15056](https://github.com/emqx/emqx/pull/15056) ペイロードがJSONリストオブジェクトのMQTTメッセージで、`foreach` 文内で明示的にペイロードをデコードする必要がなくなりました。

#### Smart Data Hub

- [#14988](https://github.com/emqx/emqx/pull/14988) バックアップ復元時にスキーマレジストリより先にスキーマ検証やメッセージ変換の設定がインポートされてしまい検証エラーとなる問題を修正しました。

#### データ統合

- [#14716](https://github.com/emqx/emqx/pull/14716) Actions/Sourcesの追加・削除を設定変更外で非同期に行うようにしました。これにより設定とリソース状態の不整合がタイムアウトで発生する可能性を排除します。

- [#14519](https://github.com/emqx/emqx/pull/14519) 既に一部のSourcesが設定されているノードの（再）起動時に、トラフィック開始が早すぎてメトリクスが存在しないために一時的な警告ログが出力される問題を修正しました。

  ログ例：

  ```
  2025-01-08T07:48:36.421822+00:00 [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey,received}, stacktrace: ..., event: received, kind: error, hint: transient failures may occur when restarting a resource, resource_id: <<"source:mqtt:tset:connector:mqtt:test">>
  ```

- [#14992](https://github.com/emqx/emqx/pull/14992) コネクターの接続性テスト時に稀に発生するリソースリークの可能性を修正しました。

- [#15000](https://github.com/emqx/emqx/pull/15000) CLIまたはHTTP API経由で設定を読み込む際にコネクター、アクション、ソースの不安定化を引き起こす問題を修正しました。

- [#15010](https://github.com/emqx/emqx/pull/15010) 従来、任意のコネクターを無効化する際に、健康状態に関わらず約5～10秒かかっていましたが、これを修正しました。ただし、アクションを持つコネクターや健康状態が悪い場合は無効化に時間がかかることがあります。

- [#15051](https://github.com/emqx/emqx/pull/15051) TDengineコネクターのパラメータ検証を追加し、ドライバーのバージョンを更新してエラーメッセージを明確化しました。

- [#15012](https://github.com/emqx/emqx/pull/15012) RabbitMQアクションの `publish_confirmation_timeout` パラメータが1000倍されていた問題を修正しました。

- [#14989](https://github.com/emqx/emqx/pull/14989) Kinesisコネクションおよびアクションの（再）起動時およびヘルスチェック時のAPI呼び出し回数を削減しました。

  従来は、コネクターの各ワーカーごとに `ListStreams` リクエストを実行し、アクションのヘルスチェックも各ワーカーごとに `DescribeStream` を実行していました。

  現在は、コネクターとアクションは各ワーカーが順番にAPIリクエストを試行し、最初に成功した応答があれば `connected` とみなします。これにより、最良ケースでは各コネクター・アクションがヘルスチェック時に1回のAPIリクエストのみを実行します。最悪ケースでは失敗した場合に各ワーカーが1回ずつリクエストを行う可能性があります。

- [#14767](https://github.com/emqx/emqx/pull/14767) Kafkaプロデューサーがパーティション数の少ないトピック再作成をスムーズに処理するようになりました。従来は失われたパーティションのプロデューサーが残り続けて大量のエラーログを出力していました。

- [#14121](https://github.com/emqx/emqx/pull/14121) Kafkaコンシューマーコネクターの `health_check_topic` 設定を非推奨化しました。このパラメータは実際には使用されていませんでした。

- [#15116](https://github.com/emqx/emqx/pull/15116) Kafkaコネクターのヘルスチェックプローブ時に `topic_authorization_failed` を有効な応答として許可するようにしました。これにより、ACLを強制するKafkaサービスでデフォルトのヘルスチェックトピックへのアクセスが制限されている場合の互換性が向上しました。

#### 管理

- [#14931](https://github.com/emqx/emqx/pull/14931) 設定 `mqtt.max_qos_allowed` をSUBACKパケットの付与されたサブスクライブQoSおよび理由コードとして使用するようにしました。従来はSUBACKの理由コードがサブスクライブQoSに固定されていました。

- [#14975](https://github.com/emqx/emqx/pull/14975) 一部TLSリスナーオプションのオンザフライ更新が適用されず、無効化・有効化のサイクルが必要だった問題を修正しました。

- [#15037](https://github.com/emqx/emqx/pull/15037) 動的に作成されたゾーンに対するレートリミットが適用されない問題を修正しました。従来はEMQXノード起動後にゾーンが作成された場合、レートリミットが適用されませんでした。

#### 設定ファイル

- [#15087](https://github.com/emqx/emqx/pull/15087) `hocon` ライブラリの問題を修正しました。文字列のワンライナーで末尾が単独のバックスラッシュの場合、設定ファイルの解析に失敗していました。

#### プラグインと拡張

- [#15073](https://github.com/emqx/emqx/pull/15073) `exhook`設定のサーバーURLに対するバリデーターを追加しました。これにより、有効なURLのみが保存可能となり、無効なURLはエラーを発生させて保存を防止します。これにより、以前は無効なURLが受け入れられていたインポート処理時の問題を回避できます。

- [#14774](https://github.com/emqx/emqx/pull/14774) プラグイン関連の問題を修正しました。既存の設定ファイルがない状態でプラグインを起動した際に、クラスターのノードからプラグイン設定ファイルを取得できない問題を修正しました。

- [#14826](https://github.com/emqx/emqx/pull/14826) Exhookサーバーが返す"IGNORE"が効果を発揮しない問題を修正しました。

- [#15018](https://github.com/emqx/emqx/pull/15018) CLI経由で無効な`exhook`設定をインポートしようとした際に`badarg`エラーでクラッシュするバグを修正しました。

- [#15108](https://github.com/emqx/emqx/pull/15108) ExHookに組み込みのgRPCヘルスチェック機構を追加し、外部フックサーバーの実際の可用性を正確に反映する接続状態を保証します。

  この修正により、サーバー停止後に状態が古いままになる問題を解決しました。設定で有効にすると自動再接続もサポートされます。

#### MQTT over QUIC

- [#14775](https://github.com/emqx/emqx/pull/14775) QUICリスナー：設定リロード後にゾーン設定が適用されない問題を修正しました。

## 5.8.11

*リリース日: 2026-06-22*

### 強化

#### セキュリティ強化

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証されていない`GET /status?format=json`レスポンスからEMQXリリースバージョン(`rel_vsn`)を削除し、認証されていない呼び出し元にブローカーのバージョン情報が漏れないようにしました。バージョン情報は認証済みのノード情報API経由で引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) [#17202](https://github.com/emqx/emqx/pull/17202) プラグインインストールエンドポイントを、アップロードされたtarballのパストラバーサル攻撃から保護し、インストール許可リストを厳格化しました。

  - インストールパスは、`..`セグメントを含むかプラグインインストールディレクトリ外に解決されるtarballの展開を拒否します。
  - `emqx ctl plugins allow <name-vsn>`の許可エントリは発行後5分で期限切れとなり、`emqx ctl plugins allow <name-vsn> sha256:<HEX>`でパッケージのSHA-256ハッシュに固定可能です。`sha256:<HEX>`は64文字の小文字16進数ダイジェストを受け付け、内容が固定ハッシュと一致しないアップロードは`403 Forbidden`で拒否されます。`sha256:`引数を省略した場合は従来通り`<name-vsn>.tar.gz`という名前の任意のペイロードを受け入れます。
  - `POST /api/v5/plugins/install`（およびそれをラップするダッシュボードのアップロード）による成功したインストールは、クラスタ全体で許可エントリを即座に取り消し、同じ許可が別のtarballに再利用されるのを防ぎます。5分のTTLは引き続き適用されますが、この変更により一般的な経路での許可期間が短縮されます。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2のSSL Common NameおよびSubjectフィールドの制御文字混入を防止するためにサニタイズを実施しました。

  `proxy_protocol = true`でリスナーを構成している場合、ブローカーはPROXY-Protocol SSL TLVバイト列にASCII制御文字が含まれる接続を拒否します（これはMQTTの`clientid`、`username`、`password`で既に拒否されているバイトクラスと同じです）。これにより、攻撃者制御のバイトが`${cert_common_name}`や`${cert_subject}`テンプレートを介してHTTP認証・認可やルールエンジンのヘッダー値に混入するのを防ぎます。

  HTTP認証・認可クライアントも、レンダリングされたヘッダー名または値にCR、LF、NULバイトが含まれる場合はリクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTTの`clientid`、`username`、`password`に適用されているバイトクラスチェックを、`ClientInfo`やHTTPリクエストテンプレートに供給される他のフィールドにも拡張しました：

  - `peersni`（TLSサーバー名表示。PROXY-Protocol v2の`authority` TLVからも受け入れ）は接続取り込み境界で検証され、制御文字があると接続拒否と警告ログ出力が行われます。
  - `mqtt.client_attrs_init`のVariform式で生成されるクライアント属性値は制御文字を含む場合は破棄され（警告付き）、`${client_attrs.tns}`のようなテンプレートでの注入を防ぎます。
  - HTTPアクションやブリッジコネクターのヘッダー描画は、レンダリングされた名前または値にNUL、CR、LFが含まれるヘッダーを破棄します。

- [#17581](https://github.com/emqx/emqx/pull/17581) JT/T 808ゲートウェイを修正し、認証時に受け入れた電話番号を接続IDとして使用するようにしました。これにより、登録コード認証で不一致があった場合や異なる電話番号のアップリンクフレームを拒否します。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式EMQX Dockerイメージのセキュリティ強化を行い、イメージスキャナーの指摘を解消しました：

  - ランタイムイメージビルド時にDebianのセキュリティアップグレードを適用し、最新のパッチ済み`libssl3t64`を取り込みました。
  - 未使用の`libgnutls30t64`パッケージを削除しました。EMQXはErlang/OTP経由でOpenSSLを使用しておりGnuTLSはリンクしないため、`curl`の依存としてのみ存在していました。
  - Debianの`curl`パッケージを[stunnel/static-curl](https://github.com/stunnel/static-curl)の静的リンク済みバイナリ（OpenSSL、HTTP/2、HTTP/3対応、RTMP・GnuTLS非対応）に置き換えました。Debian版は`librtmp1`経由で`libgnutls30t64`を再導入していましたが、静的バイナリはこれを回避しつつ`curl`を使ったコンテナのヘルスチェックはそのまま動作します。

### バグ修正

#### Core MQTT機能

- [#17573](https://github.com/emqx/emqx/pull/17573) MQTT v5のユーザープロパティ解析コストを二次から線形に削減しました。

  以前はCONNECT、PUBLISH、SUBSCRIBEパケットに多数のユーザープロパティが含まれると、各プロパティがリスト末尾に追加されるため所有接続プロセスのスケジューラ時間が超線形に増加していました。解析はエントリ数に線形にスケールしつつ、ワイヤー順序を保持します。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) ドキュメントには記載されていたが実際のデータに存在しなかった`$events/client/connack`ルールイベントの`connected_at`フィールドを追加しました。

#### データ統合

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTTソースで、Connectorが`clean_start = false`を使用し、セッションにメッセージがあるブローカーに再接続した場合にメッセージがルールアクションをトリガーしなかった問題を修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQLコネクターでプリペアドステートメントが無効な場合のクエリ実行を修正しました。以前は並行クエリがインターリーブしてエラーが発生していました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQコネクターの分離を修正しました。設定ミスや到達不能なRocketMQコネクターが同一ノード上の他のRocketMQコネクターを不安定にしなくなりました。以前は到達不能なブローカーのコネクターが共有クライアントスーパーバイザーを最大60秒停止させ、兄弟コネクターが`resource_health_check_timed_out`でフラップし、ダッシュボード操作がハングしていました。

  TCP/TLS接続タイムアウトのデフォルトも60秒から10秒に短縮し、設定ミスのサーバーが素早く失敗として検出されるようにしました。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時にMongoDBプロセスへのタイムアウト呼び出しが回復不能エラーと解釈されてリトライされなかった問題を修正しました。この場合はメッセージをリトライします。

  発生時には以下のようなログが出力されていました：

  ```
  {"stacktrace":["{emqx_mongodb,on_query,3,[{file,\"emqx_mongodb.erl\"},{line,236}]}","{emqx_resource_buffer_worker,apply_query_fun,9,[{file,\"emqx_resource_buffer_worker.erl\"},{line,1514}]}","{emqx_resource_buffer_worker,call_query2,8,[{file,\"emqx_resource_buffer_worker.erl\"},{line,1355}]}","{emqx_resource_buffer_worker,do_flush,2,[{file,\"emqx_resource_buffer_worker.erl\"},{line,768}]}","{gen_statem,loop_state_callback,11,[{file,\"gen_statem.erl\"},{line,3735}]}","{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,329}]}"],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[<0.215642180.3>,{checkout,#Ref<0.2539918795.141557761.165483>,true},5000]}}}}}"}...
  ```

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafkaクライアントライブラリを`brod` 4.5.2 → 4.5.4、`wolff` 4.1.7 → 4.1.10にアップグレードしました。

  このアップデートにはKafkaプロデューサー・コンシューマー統合に関する以下のユーザー向け修正が含まれます：

  - SASL再認証中の接続競合状態を修正し、キューイングされたproduceリクエストのドロップや`syn` produce呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQクライアント依存を`v0.7.2`にアップグレードし、非同期プロデューサーリクエストのメモリ増加問題を修正しました。

- [#17414](https://github.com/emqx/emqx/pull/17414) Azure Blob Storageコネクターのヘルスチェックが、ストレージアカウントにコンテナが多すぎる場合にタイムアウトしたり大きな帯域幅コストが発生する問題を修正しました。これは#16935の関連修正です。

- [#17567](https://github.com/emqx/emqx/pull/17567) Kafkaクライアントライブラリ`brod`を4.5.5にアップグレードしました。

  Kafka 2.2.0に対するコンシューマーグループ参加失敗を修正しました。ブローカーが`member_id_required`を返し、brodが割り当てられたメンバーIDを破棄してリトライに使わなかった問題を修正。

- [#17597](https://github.com/emqx/emqx/pull/17597) 認証が必要なMongoDB 8.0+への接続失敗を修正しました。ドライバーは以前認証前に`buildInfo`を問い合わせて認証方式を選択していましたが、MongoDB 8.0ではこのコマンドは認証済み呼び出し元に限定されています。ドライバーはプローブをスキップし、すべてのサポート対象MongoDBバージョンで受け入れられるSCRAM-SHA-1を直接使用するようになりました。

- [#17624](https://github.com/emqx/emqx/pull/17624) GCP Pub/Subコンシューマーソースで、サービスアカウントにトピックのサブスクリプション作成権限がない状態で作成された場合、後に権限が付与されても`connected`状態にならなかった問題を修正しました。

#### ゲートウェイ

- [#17426](https://github.com/emqx/emqx/pull/17426) JT/T 808ゲートウェイのスキーマ検証を修正し、`allow_anonymous`が`true`のときに`registry`および`authentication`のURLが空または省略されていても許可するようにしました。以前は`not_empty`バリデーターが両フィールドに常に適用されており、匿名モードで使われないにもかかわらず空文字の送信で400エラーとなっていました。

#### アクセス制御

- [#17169](https://github.com/emqx/emqx/pull/17169) APIキーによるダッシュボードアカウントおよびAPIキーのエクスポート・インポートをデータバックアップエンドポイント経由で制限しました。

  APIキーで呼び出された`POST /data/export`は結果のアーカイブから`dashboard_users`および`api_keys`のmnesiaテーブルセットを静かに除外します。APIキーで呼び出された`POST /data/import`はアップロードされたバックアップにこれらのテーブルセットが含まれる場合`403 FORBIDDEN`を返します。

  ダッシュボードのベアラートークン（ログイン）呼び出しは影響を受けず、引き続きダッシュボードユーザーとAPIキーを含む完全なデータベースのバックアップ・復元が可能です。

  これは権限昇格のギャップを解消します。既存の`/users`および`/api_key`エンドポイントはAPIキーによるダッシュボードログイン資格情報およびAPIキー記録へのアクセスを拒否していますが、APIキー保持者はデータバックアップエンドポイントを経由してこれらの制限を回避できていました。

- [#17451](https://github.com/emqx/emqx/pull/17451) [#17553](https://github.com/emqx/emqx/pull/17553) バックアップファイルのダウンロードを制限し、ダッシュボードアカウントやAPIキー記録を含むアーカイブはダッシュボード管理者のみがダウンロード可能としました。APIキー呼び出しはこれらの機密記録を含まないアーカイブのダウンロードは引き続き可能です。

- [#17539](https://github.com/emqx/emqx/pull/17539) `esaml`依存を`v1.1.5`にアップグレードし、SAMLレスポンスおよびメタデータの解析時にXMLエンティティ展開を無効化しました。これにより、悪意のあるSAML XMLによる外部またはカスタムエンティティの展開を防止し、SAML SSO処理の安全性を向上させます。

- [#17645](https://github.com/emqx/emqx/pull/17645) JWT認証で使用されるJWKS取得クライアントのHTTP/1.1プロトコル準拠問題を修正しました。以前のバージョンはErlang/OTPの`inets` HTTPクライアントの長年のデフォルトにより空の`TE:`ヘッダーを送信していましたが、これはPingFederateなど一部のIDプロバイダーで`503`やTCPリセットで拒否されます。EMQXはJWKS取得時に有効な`TE: trailers`ヘッダーを明示的に送信します。

#### クラスター

- [#17220](https://github.com/emqx/emqx/pull/17220) 実行中のブローカーで`bin/emqx`および`bin/emqx_ctl`の呼び出しが`nodeup`/`nodedown`イベントをトリガーし、誤解を招く`cm_registry_node_down`警告がログに出る問題を回避しました。これらスクリプトで起動する一時的なヘルパーノードは意図通り非表示のErlangノードとして登録されます。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた`cluster.hocon`に部分的な`node`セクションが含まれていた場合に、クラスター設定インポートが`required_field: node.cookie`スキーマチェックエラーで失敗する問題を修正しました。読み取り専用ルート(`node`, `rpc`)はデータインポートの対象外のため、プリフライトスキーマチェック前にインポート設定から除外し、実行中ノードの値を検証に使用します。

- [#17424](https://github.com/emqx/emqx/pull/17424) 同じクライアントIDの重複または古いグローバルセッションレジストリのリークを修正しました。

  Discardおよびtakeover-kick RPCハンドラーは対象プロセスが生存していない場合にレジストリ行も削除し、接続パスの登録スロットルは墓石行（ローカルチャネル状態なし）を認識して再利用を防ぎます。

#### 可観測性

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用量報告を改善しました。

  ブローカーはcgroup v2、cgroup v1、ホストの`/proc/meminfo`のメモリ値を比較し、最も制約の厳しい値を使用します。最小の非ゼロ合計値が優先され、合計が同じ場合は使用率の大きい方を採用します。

  これにより以下の誤解を招く読み取りを修正します：

  - コンテナに厳しいcgroupメモリ制限があるがホスト側の使用率が70%以上でcgroup制限は10%未満、またはその逆。
  - メモリ制限なしでマウントされたcgroupにより使用率が約0%に低下。

  過負荷保護の閾値と`Memory used`メトリクスは実際にプロセスを制約する制限値を反映します。

## 5.8.10

*リリース日: 2026-04-16*

### 強化

#### 可観測性とパフォーマンス

- [#16746](https://github.com/emqx/emqx/pull/16746) デフォルトで`os_mon`がシステム全体のメモリ統計のみを収集するよう設定し、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria統計の誤った重複クエリを回避し、Prometheusメトリクス収集のオーバーヘッドを削減しました。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafkaソースのポーリング動作を改善し、レコードがない場合に空バッチを即時返すのではなく、データ待ちのために短時間待機するようにしました。これにより不要なポーリング遅延が減り、Kafkaコンシューマーが新規レコードをより安定して受信できます。

#### ライセンス

- [#16853](https://github.com/emqx/emqx/pull/16853) v5ライセンスパーサーをv6ライセンスキーに対しても前方互換性を持つようにしました。

### バグ修正

#### クラスター

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスター回復時間を改善しました。

  内部のトランザクション同期イベント生成に使われる内部テーブルの完全同期待ちを廃止しました。

#### データ統合

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTTソースのConnectorが接続を回復した際にトピックの再サブスクライブが行われず、Connectorの再起動まで動作しなかった問題を修正しました。現在は再接続時に再サブスクライブします。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafkaリクエストタイムアウトをメタデータリクエストタイムアウトの少なくとも2倍、最低30秒に自動設定するようにし、メタデータリクエストが長引く場合の不要な再接続やリトライを減らしました。

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQ Connector/Action/Sourceで、接続やチャネルプロセスが予期せず死んだ場合に切断状態となり、再起動なしに回復しなかった問題を修正しました。

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storageアクションの集約モードで、コンテナに多数のBlobがあるとヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTPおよびGCP PubSubアクションで、理由`closing`の一時的接続エラーを回復可能エラーとして扱うようにし、ログノイズを減らしました。

#### ゲートウェイ

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS経由の接続モードでCoAPゲートウェイを修正しました。

- [#17030](https://github.com/emqx/emqx/pull/17030), [#17042](https://github.com/emqx/emqx/pull/17042) UDPおよびDTLS接続のCoAPクライアントテイクオーバー処理を修正しました。これにより再接続クライアントのテイクオーバールーティングとトークン検証が改善され、DTLSトークンテイクオーバー猶予期間が設定されたキープアライブウィンドウと同期します。

#### 運用

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に`emqx ctl subscriptions list`がクラッシュする問題を修正しました。

  修正前は一部クライアントのサブスクリプション一覧取得に失敗し、出力が得られませんでした。

  修正後は通常および共有サブスクリプションの両方で確実に動作します。

#### セキュリティ

- [#16690](https://github.com/emqx/emqx/pull/16690) `emqx_crl_cache:evict/1`が内部URL状態を完全にクリアしなかったCRLキャッシュの回帰を修正しました。

  追放後、同じCRL URLは次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとのHTTP取得の重複が回避されます。

- [#17012](https://github.com/emqx/emqx/pull/17012) パスワードなしのCONNECTパケットでパスワードベース認証バックエンドが認証チェーンを継続するよう修正しました。以前はパスワードなし接続で最初のパスワード認証器がエラーを返し、後続の認証器が試行されませんでした。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PIDがログデータフィールドとして出力されるようにしました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 特定の競合状態で以下のような長く難解なログが出力される問題を修正し、より意味のある情報を出力するようにしました。

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

## 5.8.9

*リリース日: 2025-12-31*

### 強化

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15 (Sequoia)向けパッケージのリリースを開始しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS認証の各コネクターで、リソースが`disconnected`とマークされた際に返される情報を改善しました。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTPアクションのHTTPリクエストタイムアウトを`resource_opts.request_ttl`と同じ値にしました。以前は固定で30秒でした。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTTコネクターの`static_clientids`設定を拡張し、各clientidに関連付けるユーザー名とパスワードを指定可能にしました。
  注意：この設定はまだダッシュボードから更新できません。ダッシュボード対応は5.8.10で追加予定です。

### バグ修正

#### Core MQTT機能

- [#16349](https://github.com/emqx/emqx/pull/16349) MQTT v5接続でリクエスト-レスポンス情報プロパティ処理時の型不一致によるクラッシュを修正しました。

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証機構とメモリーセッションを使用したクライアントが`session_stepdown_request_exception`エラーと`calling_self`理由でクラッシュする問題を修正しました。

  例：

  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK送信後に非ゼロ理由コードで切断された際の`unclean_terminate`警告ログを削除しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTTクライアントライブラリを1.13.8にアップグレードしました。

  これによりMQTTブリッジの接続性が改善されます：

  - ピアブローカーがPINGRESPを返さない場合にコネクターが自動再接続します。
  - TLS障害時のブリッジはCONNACK待ち中の接続切断をより迅速に処理します。

- [#15884](https://github.com/emqx/emqx/pull/15884) 稀にグローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無期限に保持する問題を修正しました。

  これにより、多数の共有サブスクライバーが同時に切断した際のルーティングテーブルおよび共有サブスクリプション状態の不整合蓄積を防止します。

#### クラスター

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc`を`3.5.1`にアップグレードしました。

  これ以前のバージョンでは、到達不能ノードがある場合に接続タイムアウトによるクラッシュログが長時間続く問題がありました。新バージョンでは長いクラッシュログがなくなり、より読みやすい`error`ログに変わり、`failed_to_connect_server`の頻発ログも抑制されます。

#### クラスターリンク

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立したクラスターリンクが設定され、一部のリンクが長期間ダウンした場合に、内部ルーティングテーブルの生きているルートが誤って削除されるガベージコレクションロジックの問題を修正しました。

- [#16269](https://github.com/emqx/emqx/pull/16269) クラスターリンクのルート複製プロトコル回復シーケンスで、リモート側が再ブートストラップを必要としているにもかかわらず誤ってスキップされていた問題を修正しました。

#### データ統合

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsarクライアントを2.1.2にアップグレードしました。

  Pulsarプロデューサーアクションの`batch_size`が`1`に設定されている場合、単一メッセージを単一要素バッチとしてではなくエンコードします。これによりKey Share戦略を使うコンシューマーの負荷分散が改善されます。

- [#16383](https://github.com/emqx/emqx/pull/16383) IoTDBコネクターのREST APIドライバー使用時に、認証情報がヘルスチェックで検証されていなかった問題を修正しました。現在は無効または誤設定のクライアント認証情報を早期に検出します。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続テストやコネクター停止時にタイムアウトが発生する競合状態を修正しました。

- [#16263](https://github.com/emqx/emqx/pull/16263) ヘルスチェックは現在のEMQXノードに割り当てられたパーティションのみのリーダー接続性を検証し、不要なアイドル接続や誤警報を防ぎます。

  以前はKafkaソースコネクターがすべてのパーティションのリーダー接続性をチェックしていました。クラスター展開では各ノードがパーティションのサブセットのみを所有し、未割当パーティションのリーダー接続がアイドル状態となっていました。Kafkaはアイドル接続をタイムアウト（デフォルト10分）後に切断するため、誤った接続性警報が発生していました。

- [#16138](https://github.com/emqx/emqx/pull/16138) Redisクラスターのフェイルオーバー問題を修正しました。失敗した`PING`応答がクラスタートポロジーのリフレッシュをトリガーし、フェイルオーバー後にコネクター管理が迅速に回復し最新のRedisクラスタートポロジーを反映します。

  以前はEMQXのRedisクラスタークライアントは通常クエリ（GETなど）が失敗した場合のみトポロジーを更新し、定期的なPINGコマンドの失敗では更新しませんでした。これによりフェイルオーバー後に新しいクエリがなければコネクターが「接続中」状態のまま古いトポロジーを使い続けることがありました。

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafkaデータ統合で`not_all_kafka_partitions_connected`が発生した際のログ詳細を修正しました。

- [#15906](https://github.com/emqx/emqx/pull/15906) KafkaプロデューサーライブラリWolffを4.0.12から4.0.13にアップグレードし、ProduceResponseの`record_list_too_large`エラー処理を追加しました。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafkaプロデューサーライブラリwolffを4.0.12にアップグレードし、Kafkaメタデータ応答で一時的にパーティションが欠落する場合の処理を改善しました。

  稀な競合状態でKafkaが不完全なパーティションリストを返すことがあります。以前はトピックがパーティション数を減らして再作成された場合のみ対応していましたが、一時的にパーティションが欠落する場合は対応していませんでした。このギャップによりパーティションプロデューサーが停止しシャットダウンが無期限にブロックされることがありました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafkaコンシューマーソースの追加失敗時に、例えばトピックACL拒否による場合の返却情報を充実させました。

- [#15826](https://github.com/emqx/emqx/pull/15826) KafkaブローカーがACL拒否応答を返しても接続を正常とみなすようにしました。以前はKafkaコンシューマーコネクターでヘルスチェックに使う特別な`____emqx_consumer_probe`グループの読み取り権限がないとヘルスチェックが失敗していました。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDBドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDBアクションで誤った書き込み構文を使った場合に発生する`function_clause`エラーを修正しました。

- [#15910](https://github.com/emqx/emqx/pull/15910) 複数のワーカーが同時にクラッシュした場合に、ワーカープールが回復できない問題を修正しました。

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

  `gun`および関連依存を2.1.0にアップグレードしました。

#### セキュリティと認証

- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO無効化後もログが出力される問題を修正しました。

- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスターでOIDCコールバックがログイン時にセッションを見つけられない問題を修正しました。

- [#15844](https://github.com/emqx/emqx/pull/15844) 組み込みデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーはHTTP API経由で削除できなくなり、APIパスが壊れるためです。

  既に存在する場合はEMQXコンソールで以下を実行して削除してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACLルールの処理を修正しました。

  以前はこれらのルールが内部的に`#`に変換されており、MQTT仕様の制限で`$`で始まるトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`$`で始まるトピックも含めて正しくマッチします。

- [#15899](https://github.com/emqx/emqx/pull/15899) メモリ使用を改善し、クライアント切断時に認可（authz）キャッシュを即時クリアして不要なメモリ消費を減らしました。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの`jq`関数のメモリリークを修正しました。

  以前は`jq`組み込み関数`index`（例：`.key | index("name")`）の使用でメモリリークが発生していました。

#### 可観測性

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログをクリーンアップする際のMnesiaトランザクションブロッキングによる急激なメモリ増加を防止しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートコンソールからの操作による過剰な監査ログ生成を削減しました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクオータアラームのテキストを修正しました。

#### 永続ストレージ

- [#14674](https://github.com/emqx/emqx/pull/14674) EMQXのパーシステンスストレージで作成されるRocksDBの情報ログファイルの数とサイズを制限しました。

## 5.8.8

*リリース日: 2025-09-04*

### 強化

#### デプロイメント

- [#15813](https://github.com/emqx/emqx/pull/15813) Debian 13 (Trixie)向けパッケージリリースを追加し、DockerイメージのベースをDebian 13に更新しました。

#### Core MQTT機能

- [#15773](https://github.com/emqx/emqx/pull/15773) 再接続時のクライアントID登録をスロットル制御しました。
  - 以前のセッションクリーンアップが進行中の場合、同じクライアントIDを使う新規接続はスロットルされます。これによりクライアントの過剰再接続による不安定化を防止します。
  - 対象クライアントは`CONNACK`で理由コード`137`（Server Busy）と理由文字列`"THROTTLED"`を受け取り、クリーンアップ完了後に再試行すべきです。
  - 同じクライアントIDを登録した別接続の理由コード返却を修正し、正しく`137`を返すようにしました（以前は`133`でした）。

#### データ統合

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud`ライブラリを`3.8.3.0`にアップグレードしました。これにより、EC2インスタンスに適切なIAM権限があれば、アクセスキーIDやシークレットアクセスキーを指定せずにS3コネクターを設定可能になります。

- [#15585](https://github.com/emqx/emqx/pull/15585) brodクライアントを4.4.4にアップグレードし、より広範なKafka APIをサポートしました。`JoinGroups` APIバージョン`v0`〜`v1`の廃止に対応しています。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に非アクティブ化できるAPIエンドポイントを追加しました。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) Erlangの組み込みガベージコレクターに比べて効果が薄く、CPU変動やメッセージレイテンシ増加を引き起こしていた`node.global_gc_interval`設定をデフォルトで無効化しました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VMパラメーターを最適化し、パフォーマンスと安定性を改善しました：
  - 分散チャネルのバッファサイズを32MBに増加（`+zdbbl 32768`）、Mnesiaの集中的な操作時の`busy_dist_port`アラームを防止。
  - スケジューラのビジーウェイティングを無効化（`+sbwt none +sbwtdcpu none +sbwtdio none`）し、OS報告のCPU使用率を低減。
  - スケジューラバインディングタイプをdbに設定（`+stbt db`）し、メッセージレイテンシを低減。

### バグ修正

#### デプロイメント

- [#15580](https://github.com/emqx/emqx/pull/15580) EMQX Enterprise Helmチャートに新たな`emqxLicenseSecretRef`変数を追加しました。これにより、EMQXライセンスキーを含むKubernetes Secretを指定してライセンスを自動適用可能になります。

  これにより、ライセンスをEMQXに渡さずに秘密ファイルを作成・マウントするだけだった非機能の`emqxLicenseSecretName`変数は置き換えられます。

#### クラスター

- [#14778](https://github.com/emqx/emqx/pull/14778) ノードの`data/certs`または`data/authz`ディレクトリに壊れたシンボリックリンクがある場合に、ノードが稼働中クラスターに参加できない問題を修正しました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTPのバージョンを26.2.5.2から26.2.5.14にアップグレードしました。このアップグレードにはEMQXに影響するTLS関連の2つの修正が含まれます：
  - 証明書更新時の競合状態によるTLS接続クラッシュを修正。
  - RSASSA-PSSパラメーターで署名されたRSA証明書のサポートを追加。以前はこれらの証明書でTLSハンドシェイクが`bad_certificate`/`invalid_signature`エラーで失敗していました。

#### データ統合

- [#15616](https://github.com/emqx/emqx/pull/15616) Kafka接続は、デフォルトのプローブトピックに対して`topic_authorization_failed`エラーが返されても正常とみなすようになりました。

#### ルールエンジン

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換およびスキーマ検証で、アイテム削除時にトピックインデックスが破損し、無効化後もアイテムがアクティブのままになる問題を修正しました。

- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) サブスクリプション認証失敗時に`packets.subscribe.auth_error`メトリクスがインクリメントされない問題を修正しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPPゲートウェイのグローバルチェーン名を修正しました。これらゲートウェイの組み込み認証データは以前`unknown:global`にまとめられており、ゲートウェイ間で競合が発生していました。

- [#15699](https://github.com/emqx/emqx/pull/15699) ゲートウェイ（例：CoAP）の組み込み認証データがノード停止・再起動時に誤って削除される問題を修正しました。

- [#15822](https://github.com/emqx/emqx/pull/15822) OCPP接続が一定数のメッセージ送信後にクラッシュする問題を修正しました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHookのTLSオプションを修正し、gRPCクライアントがTLSハンドシェイク時にサーバーホスト名を正しく検証できるようにしました。

## 5.8.7

*リリース日: 2025-07-02*

### 強化

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry gRPC（HTTP/2上）統合でカスタムHTTPヘッダーをサポートしました。これによりHTTP認証が必要なコレクターとの互換性が向上します。

### バグ修正

- [#15383](https://github.com/emqx/emqx/pull/15383) MQTTブリッジの潜在的なリソースリークを修正しました。ブリッジ起動失敗時にトピックインデックステーブルが正しくクリーンアップされていませんでしたが、この修正で正しく削除されます。

- [#15331](https://github.com/emqx/emqx/pull/15331) InfluxDBアクションで、`WriteSyntax`の`timestamp`が空欄かつルールでタイムスタンプフィールドが指定されていない場合にラインプロトコル変換が失敗する問題を修正しました。現在はシステムの現在ミリ秒値を使用し、ミリ秒精度を強制します。

- [#15274](https://github.com/emqx/emqx/pull/15274) Postgres、Matrix、TimescaleDBコネクターのレジリエンスを改善し、ヘルスチェック失敗時に完全な再接続をトリガーするようにしました。以前はヘルスチェック失敗で接続が壊れたまま操作がハングし、メモリ不足になる可能性がありました。

- [#15224](https://github.com/emqx/emqx/pull/15224) ダッシュボードから外部スキーマレジストリを更新した際に既存のパスワードが`******`で上書きされる問題を修正しました。パスワードは正しく保持されます。

- [#14989](https://github.com/emqx/emqx/pull/14989) Kinesisコネクターとアクションの起動およびヘルスチェック時のAWS API呼び出し回数を大幅に削減しました。これにより大規模な接続プールや複数コネクター使用時のAWS Kinesis APIレート制限（例：`ListStreams`、`DescribeStream`）超過によるヘルスチェック失敗を防止します。

- [#15299](https://github.com/emqx/emqx/pull/15299) OpenTelemetryメトリクスのエクスポート時に発生する`badarg`エラーを修正しました。

## 5.8.6

*リリース日: 2025-03-25*

EMQX 5.8.6にアップグレードする前に破壊的変更と既知の問題を必ずご確認ください。

### 強化

- [#14869](https://github.com/emqx/emqx/pull/14869) `$events/client_disconnected`イベントペイロードに`connected_at`タイムスタンプフィールドを追加しました。これにより切断クライアントの元の接続セッション時間を追跡可能になり、遅延した切断イベントが新しい接続状態を上書きするのを防ぎます。

  以前は不安定なネットワークでクライアントが頻繁に再接続すると、遅延した切断イベントによりセッション追跡が誤ることがありました。今回の更新で`connected_at`フィールドがイベントペイロードに含まれ、システムトピックと同様の挙動となり正確なセッション状態管理が可能になります。

- [#14855](https://github.com/emqx/emqx/pull/14855) JT/T 808ゲートウェイに新設定`ignore_unsupported_frames`を追加しました。これによりゲートウェイが解析できないメッセージ送信時にデバイスの切断を防止します。

- [#14858](https://github.com/emqx/emqx/pull/14858) EMQXはTDengine Cloudとのデータ統合をサポートします。TDengine Cloudは認証に追加のトークンパラメーターを必要とし、TDengineコネクターでこれをサポートしました。

### バグ修正

#### Core MQTT機能

- [#14815](https://github.com/emqx/emqx/pull/14815) QoS 2メッセージのパケットID解放を修正しました。以前はクライアントが最大設定数の保留中QoS 2メッセージのPUBREL送信に失敗し切断した場合、Max Awaiting PUBREL Timeoutを超過してもパケットIDが解放されませんでした。

#### インストールとデプロイメント

- [#14797](https://github.com/emqx/emqx/pull/14797) macOSリリースパッケージの起動問題をOpenSSLの動的リンクに起因する不具合を修正（バックポート #14624）。

  以前は、macOS上のEMQX ZIPパッケージで、`quicer`アプリケーションがシステムにインストールされたOpenSSLに動的リンクしていたため、EMQXのビルドプロセスで署名されていないOpenSSLを参照し、起動に失敗することがありました。今回、OpenSSLの動的リンクを無効化し、macOSに搭載されているOTPと整合させました。これにより、macOS 13以降でEMQXが安定して起動するようになりました。

#### 認証

- [#14847](https://github.com/emqx/emqx/pull/14847) ワイルドカードHTTPSエンドポイントに対するJWKS認証失敗を修正。以前は、ワイルドカードホスト名を使用するHTTPSエンドポイントから鍵を取得できず、認証が成功しませんでした。
- [#14786](https://github.com/emqx/emqx/pull/14786) 外部JWKSエンドポイントを使用したJWT認証設定の更新時の不具合を修正。以前は、旧設定と新設定の両方でJWKSが有効な場合、一部の設定が正しく反映されませんでした。

#### REST API

- [#14834](https://github.com/emqx/emqx/pull/14834) データバックアップファイルのダウンロード時に誤った`Content-Type`ヘッダーが返される問題を修正。以前は、バックアップファイルのレスポンスヘッダーで`application/json`が使用されていましたが、正しくは`application/octet-stream`です。
- [#14863](https://github.com/emqx/emqx/pull/14863) `cluster/:node/invite_async` REST APIの問題を修正。以前は、ダウンしているノードをコーディネーターとして使用しようとすることがありました。

#### ルールエンジン

- [#14824](https://github.com/emqx/emqx/pull/14824) SQLルールテスターでアラームイベントの`details`キーを処理する際のHTTP 500エラーを修正。`alarm_activated`や`alarm_deactivated`イベントのテスト時に、`details`キー内のネストされたマップの不適切な処理が原因でした。

#### データ統合

- [#14796](https://github.com/emqx/emqx/pull/14796) Pulsarプロデューサーのインフライト状態リークを修正。修正前は、Pulsarクライアントのインフライト状態がリークし、コネクターのインフライトカウンターがゼロに戻らない問題がありました。x86環境でのPulsarおよびKafkaプロデューサーのパフォーマンスも改善しました。

  また、Pulsarアクションの`buffer.memory_overload_protection`パラメータを正しくサポートしました。以前はこの設定が無効で、メモリ使用量が制御されませんでした。

- [#14902](https://github.com/emqx/emqx/pull/14902) SQL Serverアクションの接続失敗時のエラーハンドリングを改善し、`IMC0x` SQLSTATEエラーを回復可能とみなすようにしました。これにより、外部MSSQLサービスが一時的に利用不可の場合でもメッセージの損失を防ぎ、再試行のためにメッセージを適切にキャッシュします。

  また、接続の健全性チェックを強化し、切断された接続を正確に検出してコネクターの再接続を試みることで、不安定なネットワーク環境下でのSQL Serverコネクターの信頼性を向上させました。

- [#14833](https://github.com/emqx/emqx/pull/14833) Kafkaプロデューサーがパーティション数の少ないKafkaトピックの再作成をスムーズに処理するよう改善。以前は、失われたパーティションのプロデューサーが残存し、リトライや大量のエラーログ出力を引き起こしていました。

#### オブザーバビリティ

- [#14800](https://github.com/emqx/emqx/pull/14800) `warning`レベルのログ`dropped_qos0_msg`をスロットル制御。

- [#14793](https://github.com/emqx/emqx/pull/14793) MQTT接続における`protocol_error`のトレースログを追加。

  以前は、クライアントが無効または予期しないMQTTパケットを送信し`protocol_error`が発生した場合、EMQXのログには詳細が不足し、問題の診断が困難でした。

  例えば、すでに接続済みのクライアントが2回目の`CONNECT`パケットを送信した場合、`socket_force_closed`と`protocol_error`がログに記録されますが、原因の詳細は示されませんでした。

  今回の更新により、`socket_force_closed`の前に`unexpected_connect_packet`が`conn_state=connected`付きでログに記録され、プロトコル違反の原因をより明確に把握できるようになりました。

- [#14813](https://github.com/emqx/emqx/pull/14813) WebSocketクライアントに送信されたアウトゴーイングメッセージがエンドツーエンドトレースに記録されない問題を修正。

- [#14880](https://github.com/emqx/emqx/pull/14880) SQL Serverコネクターのヘルスチェック失敗時のログを改善。`timeout errors`や`unexpected_SELECT_1_result`など、より正確な失敗理由と詳細な診断情報をログに出力し、トラブルシューティングを支援します。

#### プラグイン

- [#14802](https://github.com/emqx/emqx/pull/14802) プラグイン用の新CLIコマンドを追加：

  ```bash
   emqx ctl plugins allow NAME-VSN
  ```

  HTTP APIやダッシュボード経由でプラグインをインストールする前に、このコマンドでパッケージの明示的な許可が必要となり、セキュリティが向上し不正なインストールを防止します。

#### ゲートウェイ

- [#14756](https://github.com/emqx/emqx/pull/14756) JT/T 808ゲートウェイを改善し、匿名認証が有効な場合、登録応答にデフォルトの認証コード`anonymous`を含めるようにしました。これにより、一部クライアントが空の認証コードを解析できない問題を回避します。

## 5.8.5

*リリース日: 2025-02-25*

EMQX 5.8.5へアップグレードする前に、破壊的変更と既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#14454](https://github.com/emqx/emqx/pull/14454) リテイナーに`max_publish_rate`オプションを追加。各ノードで保持メッセージの最大パブリッシュレートを制御します。この制限を超えたメッセージは配信されますが、保持されません。

  このオプションは、保持メッセージストレージへの負荷を制限するのに有効です。

- [#14456](https://github.com/emqx/emqx/pull/14456) SYNフラッド攻撃からEMQXリスナーを保護する簡易ファイアウォールスクリプト`bin/emqx_fw`を追加。Linuxのみ対応。

- [#14496](https://github.com/emqx/emqx/pull/14496) `POST /data/export` APIの`root_keys`パラメータに対する追加検証を実装。無効なルートキーはエラーとなり、無視されなくなりました。

#### アクセス制御

- [#14494](https://github.com/emqx/emqx/pull/14494) MongoDB認可を強化し、複雑なクエリをサポート。

  - セレクターフィルター設定でトップレベルの`$orderby`演算子をサポートし、認可チェック時のクエリ結果のソートが可能に。
  - MongoDB認可でのページネーション制御のため、`skip`および`limit`オプションを追加。

- [#14570](https://github.com/emqx/emqx/pull/14570) HTTP認証・認可設定でHTTPヘッダーにプレースホルダーを使用可能に。

- [#14665](https://github.com/emqx/emqx/pull/14665) ACLルールの前提条件としてクライアント属性をサポート。クライアント属性に基づく細かなアクセス制御が可能に。

  例：`"type"`属性が`"internal"`のクライアントは全トピックのパブリッシュ・サブスクライブを許可。

  `{allow, {client_attr, "type", "internal"}, all, ["#"]}.`

  また、`"type"`属性が`"external-"`で始まるクライアントはパブリッシュを拒否。

  `{deny, {client_attr, "type", {re, "external-.*"}}, publish, ["#"]}.`

#### ルールエンジン

- [#14627](https://github.com/emqx/emqx/pull/14627) 新たにシステムアラームの有効化・無効化時に発火するイベント`$events/sys/alarm_activated`と`$events/sys/alarm_deactivated`を追加。

#### データ統合

- [#14404](https://github.com/emqx/emqx/pull/14404) MQTTコネクターで静的なクライアントID指定をサポート。

- [#14450](https://github.com/emqx/emqx/pull/14450) MQTTソースに`no-local`フラグ設定を追加。クライアントが自身でパブリッシュしたメッセージを受信しないようにできます。

- [#14507](https://github.com/emqx/emqx/pull/14507) 軽量なHTTP API `GET /actions_summary` と `GET /sources_summary` を追加。既存の`GET /actions`や`GET /sources`と似ていますが、エンティティの完全な設定を返さず、より高速かつリソース消費を抑えた概要情報を提供します。

- [#14524](https://github.com/emqx/emqx/pull/14524) Couchbaseコネクターのヘルスチェック失敗時に詳細なエラーメッセージを追加。

- [#14572](https://github.com/emqx/emqx/pull/14572) Kafka、Azure Event Hub、Confluentプロデューサーの`parameters.buffer.memory_overload_protection`のデフォルト値を`true`に変更。

  これにより、Kafkaサービスが長時間ダウンした場合のメモリ過負荷やOOMエラーのリスクを軽減します。

- [#14626](https://github.com/emqx/emqx/pull/14626) KafkaおよびPulsarアクションのパーティションごとのバッファサイズのデフォルトを256MBに変更。

#### オブザーバビリティ

- [#14437](https://github.com/emqx/emqx/pull/14437) Prometheus出力に新たに`emqx_vm_mnesia_tm_mailbox_size`と`emqx_vm_broker_pool_max_mailbox_size`のゲージを追加。これらはEMQX内部プロセスのメールボックスサイズを監視し、システム過負荷の指標となります。一定の閾値を超えるとアラームが発生します。

- [#14645](https://github.com/emqx/emqx/pull/14645) 証明書失効リスト（CRL）取得時の初回フェッチのデバッグログを強化。成功時は`debug`、失敗時は`warning`レベルで詳細ログを出力します。

- [#14656](https://github.com/emqx/emqx/pull/14656) Prometheusプッシュ機能を強化し、より多くのメトリクスをサポート。クラスター名をJobラベルの変数名として使用可能に。

- [#14479](https://github.com/emqx/emqx/pull/14479) Opentelemetry統合のエンドツーエンドトレースにおける認証・認可バックエンドの詳細なトレース情報を追加。

- [#14644](https://github.com/emqx/emqx/pull/14644) Opentelemetry統合のエンドツーエンドトレースでクライアント提供のtraceparentをサポート。

- [#14657](https://github.com/emqx/emqx/pull/14657) エンドツーエンドトレースのホワイトリストが`broker.publish`スパン（メッセージ配信スパン）にも適用されるように改善。

  以前は、サブスクライバーのメッセージ受信をトレースするには、パブリッシャーやトピックをホワイトリストに追加する必要があり、他のサブスクライバーへの配信もトレースされてしまい不要なスパンが生成されていました。

  今回の更新により、サブスクライバーのクライアントIDをホワイトリストに追加するだけで、特定サブスクライバーへのメッセージ配信スパンをトレース可能になりました。

- [#14589](https://github.com/emqx/emqx/pull/14589) および [#14689](https://github.com/emqx/emqx/pull/14689) メッセージ変換および検証の全ログメッセージタイプをスロットル制御。

#### MQTT over QUIC

- [#14583](https://github.com/emqx/emqx/pull/14583) QUICリスナーがTLSシークレットを`SSLKEYLOGFILE`環境変数にダンプ可能に。Wiresharkなどのツールでライブまたはキャプチャ済みのQUICトラフィックを復号し、QUIC内のMQTTパケットを解析できます。

  設定例：
  `EMQX_LISTENERS__QUIC__DEFAULT__SSLKEYLOGFILE=/tmp/EMQX_SSLKEYLOGFILE`

  ※トラブルシューティング目的の隠し設定です。

- [#14597](https://github.com/emqx/emqx/pull/14597) 接続終了時の非同期ストリーム読み取り中止を実装。

  セッションが「乗っ取られた」「破棄された」「キックされた」場合、従来はグレースフルシャットダウンを行い、応答しない古いクライアントで最大3秒のブロックが発生していました。

  グレースフルシャットダウンは両端間の協調シグナルに依存し、MQTT.DISCONNECTパケットを相手に届けてからトランスポートを閉じます。相手が応答しない場合、遅延が発生しました。

  今回の改善では、終了時にストリームを半閉じにし、読み取り（recv）操作を中止しつつ書き込み（send）は維持します。これによりMQTT.DISCONNECTパケットは相手に届き、遅延なく終了処理が行われます。

  **メリット**：

  - 相手が到達不能または応答なしの場合のブロック時間を短縮。
  - 終了通知を適切に相手に伝え、接続終了動作を改善。
  - セッション乗っ取りやクリーンスタート（破棄）時のレイテンシを低減。

### バグ修正

#### コアMQTT機能

- [#14405](https://github.com/emqx/emqx/pull/14405) `mqtt.max_packet_size`の`256MB`設定を`268435455`バイトに変換。

  EMQXは以前、`mqtt.max_packet_size`に`256MB`を設定可能でしたが、これはプロトコル仕様で許容される最大値より1バイト多い値です。後方互換性のため、`256MB`設定は引き続き許容されますが、内部的に`268435455`に静かに変換されます。

- [#14508](https://github.com/emqx/emqx/pull/14508) 大量のクライアント再接続時のEMQXパフォーマンスを改善。

- [#14608](https://github.com/emqx/emqx/pull/14608) MQTTセッションメッセージキューでFIFO（先入れ先出し）セマンティクスを強制。キューが満杯の場合、最も古いメッセージから順に破棄されます。

- [#14609](https://github.com/emqx/emqx/pull/14609) オーバーヘッド保護の高メモリ閾値を`sysmon.os.sysmem_high_watermark`に修正。起動時や設定変更時に正しく更新されるようになり、動的なメモリ保護閾値を反映します。

- [#14654](https://github.com/emqx/emqx/pull/14654) 最大セッション数に達しても、以前のセッションが有効（期限切れやクリーンアップされていない）ならクライアントが再接続可能に。

- [#14588](https://github.com/emqx/emqx/pull/14588) コンテナ環境（AWS EKSなど）でのメモリ使用量報告を改善。ホストカーネルバージョン、cgroupバージョン、コンテナ管理サービスのcgroupfsマウント方法などに起因する誤差を軽減。

#### ライセンス

- [#14568](https://github.com/emqx/emqx/pull/14568) 切断済みセッションもライセンス制限に含めるよう強化。以前は接続中セッションのみカウントしていましたが、セッション保持が有効な切断済みセッションも対象となります。ライセンス制限に達した場合、新規のオフライン保持セッションは拒否され、リソース管理とライセンス遵守が向上します。

#### 認証

- [#14585](https://github.com/emqx/emqx/pull/14585) パスワードハッシュ比較の大文字小文字区別問題を修正。外部システムとの連携時にケースの違いで認証失敗する問題を解消し、大文字小文字を区別しない比較を行うようにしました。

#### ゲートウェイ

- [#14484](https://github.com/emqx/emqx/pull/14484) Exprotoゲートウェイがサーバーエンドポイントでホスト名を使用できない問題を修正。

- [#14489](https://github.com/emqx/emqx/pull/14489) クラスター内のノードでゲートウェイが有効化されていない場合に`api/v5/gateways`エンドポイントへアクセスすると500エラーになる問題を修正。適切なレスポンスを返すようにし、APIの安定性を向上。

- [#14501](https://github.com/emqx/emqx/pull/14501) ゲートウェイクライアントクエリHTTP APIが常に`keepalive`値を`0`で返す問題を修正。正しい`keepalive`値を返し、ゲートウェイが設定されたアイドルタイムアウトを遵守するように。

- [#14503](https://github.com/emqx/emqx/pull/14503) ゲートウェイにリスナーが存在しない場合、404エラーではなく空リストを返すように修正。以前はAPI経由でリスナーが未設定のゲートウェイ（例：LwM2M）にアクセスすると404が返されていました。

- [#14511](https://github.com/emqx/emqx/pull/14511) クライアント認証失敗時にStompゲートウェイが不要なログを出力する問題を解消。

- [#14653](https://github.com/emqx/emqx/pull/14653) Stompゲートウェイのキープアライブ動作を修正。ハートビートパケットがチェックタイマー直後に受信された場合に接続が切断されていた問題を、わずかな遅延を許容するよう改善。平均してハートビート間隔の約1.5倍で接続が切断され、より信頼性の高いキープアライブを実現。

#### ルールエンジン

- [#14622](https://github.com/emqx/emqx/pull/14622) 32以上の変換や検証がトピックにマッチする場合に、メッセージ変換とスキーマ検証の実行順序が任意になる問題を修正。

#### データ統合

- [#14518](https://github.com/emqx/emqx/pull/14518) コネクターの起動をCLIやHTTP API経由の設定読み込み時に非同期化。以前は起動時にコネクターがハングすると設定インポート全体がタイムアウトしていました。

  また、ノードの（再）起動時も非同期起動に変更し、起動時間を短縮。さらに、設定インポート時にソースがコネクターより先に追加される問題を修正し、初期化順序を正しくしました。

- [#14545](https://github.com/emqx/emqx/pull/14545) RabbitMQアクションがRabbitMQが応答しない場合に削除できない問題を修正。

- [#14550](https://github.com/emqx/emqx/pull/14550) MQTTコネクターの接続プール内のMQTTクライアントが一部切断された場合に自動再接続に失敗する問題を修正。切断されたクライアントは自動的に再接続され、接続信頼性が向上。

- [#14555](https://github.com/emqx/emqx/pull/14555) MQTTソースで共有トピックがソース削除や更新時に正しくサブスクライブ解除されない問題を修正。

- [#14650](https://github.com/emqx/emqx/pull/14650) RedisクラスターモードでRedisマスター・スレーブのフェイルオーバー後に`no_connection`エラーから復旧できない問題を修正するため、`eredis_cluster`ライブラリをバージョン`0.8.8`に更新。

- [#14671](https://github.com/emqx/emqx/pull/14671) MQTTアクションの問題を修正。MQTTコネクターの接続が閉じられた際に稀に発生するレースコンディションにより、メッセージ送信や再試行が失敗する問題を解消。TCP接続の切断（`tcp_closed`）やクライアント切断は回復可能なエラーとして扱います。

- [#14695](https://github.com/emqx/emqx/pull/14695) コネクター更新時のバリデーションエラーに関するHTTP APIのエラーメッセージを改善。

- [#14697](https://github.com/emqx/emqx/pull/14697) 同名のソースとアクションが同一コネクターを使用し、ルール依存関係がある場合にアクションやソースを削除できない問題を修正。

- [#14427](https://github.com/emqx/emqx/pull/14427) GCP PubSubプロデューサーコネクターのヘルスチェック失敗時により詳細なエラーメッセージを提供。

- [#14451](https://github.com/emqx/emqx/pull/14451) PostgreSQLアクションのタイムスタンプ列に無効な入力があった場合の大規模なクラッシュレポートを修正。エラーを適切に処理し、簡潔で分かりやすいエラーメッセージをログに出力するように改善。

- [#14552](https://github.com/emqx/emqx/pull/14552) Kafkaサービスがダウン中にバッファオーバーフローが発生した際のKafkaプロデューサーの`unexpected_id`クラッシュを修正。EMQX Enterprise 5.8.1で導入されたバグ。

- [#14560](https://github.com/emqx/emqx/pull/14560) Oracleアクションが複雑なSQLテンプレートの問題でヘルスチェックに失敗する問題を修正。

- [#14563](https://github.com/emqx/emqx/pull/14563) KafkaおよびPulsarプロデューサーでバッファオーバーフローやリクエスト期限切れによりメッセージがドロップされた場合、対応するルールの「failed」カウンターが正しく増加しなかった問題を修正。これによりルールページの統計指標が正確になります。

- [#14567](https://github.com/emqx/emqx/pull/14567) S3コネクターを無効化または削除後にS3 HTTPプールが停止されない問題を修正。

- [#14631](https://github.com/emqx/emqx/pull/14631) Kafka、Azure Event Hub、Confluentプロデューサーアクションのメモリ過負荷保護を強化。高メモリ閾値到達時にバッファデータをより積極的に破棄し、OOM状態になるリスクを低減。

- [#14705](https://github.com/emqx/emqx/pull/14705) Kafkaコネクターの接続性チェックを改善し、認証が必要な場合に資格情報やヘルスチェックトピックが未設定だと誤って成功判定していた問題を修正。ダミーヘルスチェックトピック`emqx-connector-connectivity-probe`を導入し、Kafkaが有効なメタデータまたは`unknown_topic_or_partition`応答を返した場合のみ正常とみなします。

#### クラスター

- [#14536](https://github.com/emqx/emqx/pull/14536) クラスター管理操作における稀なレースコンディションを修正。これにより、特定のクラスター管理操作がハングし、ノード再起動までクラスター変更が不可能になる問題を解消。`mria:join/1`操作のグローバルロックを強化し、同時参加が干渉しないようにしました。

- [#14548](https://github.com/emqx/emqx/pull/14548) ノードがダウン中に新ノードがクラスターに参加すると再起動時に`** FATAL ** Failed to merge schema: {aborted,function_clause}`エラーでクラッシュする問題を修正。ノードがスムーズに再起動できるようになりました。

- [#14662](https://github.com/emqx/emqx/pull/14662) すべてのコアノードの内部データベースが消去されたクラスターに再参加したレプリカントノードが、一部のRPC呼び出し操作に参加できない問題を修正。

#### 管理

- [#14543](https://github.com/emqx/emqx/pull/14543) WS、WSS、ゲートウェイリスナー経由で接続されたクライアントに対し、一部のExHooksがクラッシュする内部互換性問題を修正。

#### オブザーバビリティ

- [#14544](https://github.com/emqx/emqx/pull/14544) TCPまたはTLSリスナーを無効化するとPrometheusメトリクス収集プロセスがクラッシュする問題を修正。

- [#14466](https://github.com/emqx/emqx/pull/14466) トレースイベントスイッチがサンプリング比率100%設定時に反映されない問題を修正。

- [#14666](https://github.com/emqx/emqx/pull/14666) 設定項目`opentelemetry.traces.max_queue_size`をREST APIおよびダッシュボードから設定可能に。これまでは設定ファイルやOS環境変数のみ対応。

## 5.8.4

*リリース日: 2024-12-26*

EMQX 5.8.4へアップグレードする前に、破壊的変更と既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#13739](https://github.com/emqx/emqx/pull/13739) クラスター全体のモニター（統計）データをクリアする機能を追加。`api/v5/monitor`エンドポイントに`DELETE`リクエストを送信することで、収集済みのすべてのモニタリングメトリクスをクリア可能。

- [#14247](https://github.com/emqx/emqx/pull/14247) クライアントメタデータに`tns`属性が存在する場合、ログに`client_attrs.tns`を記録。ただし、クライアントIDがすでに`tns`値でプレフィックスされている場合は重複を避けて記録しません。

- [#14353](https://github.com/emqx/emqx/pull/14353) セッションのリバランスおよび避難処理の堅牢性を向上。以前はクラスタリングエラー時にデッドループに陥ることがありました。

#### ルールエンジン

- [#14369](https://github.com/emqx/emqx/pull/14369) ルールエンジンにサイズ関連の関数を2つ追加：
  - `is_empty` : マップや配列が空の場合に`true`を返す。
  - `map_size` : マップのサイズを返す。

#### データ統合

- [#14110](https://github.com/emqx/emqx/pull/14110) Pulsarドライバーのメトリクス報告を追加し、オブザーバビリティを向上。以下のメトリクスが報告されます：
  - キューイング中メッセージ数
  - インフライトメッセージ数
  - ドロップされたメッセージ数
- [#14410](https://github.com/emqx/emqx/pull/14410) EMQXが[Aliyun Tablestore](https://cn.aliyun.com/product/ots)とのデータ統合をサポート。
- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDBデータ統合の実装をリファクタリングし、バッチ性能を改善。

#### 設定ファイル

- [#14269](https://github.com/emqx/emqx/pull/14269) 新たに`etc/base.hocon`設定ファイルを追加。設定管理と明確化を強化。

  これまで`emqx.conf`が唯一の手動設定ファイルでしたが、設定上書き階層の最上位にあるため混乱を招いていました。`emqx.conf`で設定された変更はUI/API/CLI経由で即時反映されますが、ノード再起動後は持続しません。

  これを解決するため、`etc/base.hocon`を基盤設定レイヤーとして追加。設定の優先順位は上から順に：

  1. 環境変数
  2. `etc/emqx.conf`
  3. `data/configs/cluster.hocon`
  4. `etc/base.hocon`

  `etc/base.hocon`は基底レイヤーとして機能し、ノード起動後も変更可能ですが、一貫した動作と適切な設定上書きを保証します。

#### オブザーバビリティ

- [#14360](https://github.com/emqx/emqx/pull/14360) Prometheusメトリクスにリスナーのシャットダウン回数をシャットダウン理由別にカウントする`emqx_client_disconnected_reason`カウンターを追加。例：

  ```
  emqx_client_disconnected_reason{node="emqx@127.0.0.1",reason="takenover"} 1
  emqx_client_disconnected_reason{node="emqx@127.0.0.1",reason="kicked"} 1
  ```

  現状はTCPおよびTLSリスナーのみ対応。

### バグ修正

#### コアMQTT機能

- [#14248](https://github.com/emqx/emqx/pull/14248) クラスターノード間の断続的な接続問題を修正。これによりクラスター全体のルーティングテーブル状態の一部喪失を防ぎ、一貫性と信頼性を向上。

- [#14272](https://github.com/emqx/emqx/pull/14272) CLI経由で読み込んだ`auto_subscribe`設定が成功メッセージを表示するが反映されない問題を修正。

- [#14424](https://github.com/emqx/emqx/pull/14424) 排他的サブスクリプションに関連するメンバーシップメッセージが`unexpected_info`警告として誤ってログに記録される問題を修正。

#### REST API

- [#14317](https://github.com/emqx/emqx/pull/14317) ページネーション計算時にHTTP APIが空ページを返す問題を修正。

#### データ統合

- [#14318](https://github.com/emqx/emqx/pull/14318) HTTPコネクターの状態初期化に関する問題を修正。HTTPアクションがトラフィック処理中に基盤コネクターが再起動されると`function_clause`エラーでクラッシュする問題を解消。以前のログ例：

  ```
  20:42:36.850 [error] msg: "resource_exception", info: #{error => {error, function_clause}, id => <<"action:http:a:connector:http:a">>, name => call_query, ...
  ```

- [#14319](https://github.com/emqx/emqx/pull/14319) リソース管理の内部状態機械をリファクタリングし、複数のレースコンディションバグを解消。例として、HTTPアクションのヘルスチェックが不安定な場合に以下のようなエラーが発生していました：

  ```
  2024-11-29T14:58:17.994119+00:00 [error] msg: action_not_found, connector: <<"connector:http:a">>, action_id: <<"action:http:a:connector:http:a">
  ```

- [#14362](https://github.com/emqx/emqx/pull/14362) リソースマネージャーの状態機械をリファクタリングし、状態不整合を引き起こすレースコンディションを防止。

- [#14429](https://github.com/emqx/emqx/pull/14429) ルールアクションのメトリクス処理を修正。基盤コネクターが無効化されている場合、失敗カウンターが`unknown`と`out_of_service`の両方で増加していた問題を修正し、`out_of_service`のみ増加するように改善。

- [#14291](https://github.com/emqx/emqx/pull/14291) Pulsarプロデューサードライバーをアップグレードし、トピック検索時の`Redirect` `LookupType`応答の処理を修正。修正前は、`Redirect`応答を受けた際に誤って返されたブローカーに接続しようとしてパブリッシュに失敗していました。ログ例：

  ```
  2024-11-25T20:40:54.140659+00:00 [error] [pulsar-producer][persistent://public/default/p3-partition-0] Response error:'ServiceNotReady', msg:"Namespace bundle for topic (persistent://public/default/p3-partition-0) not served by this instance. Please redo the lookup. Request is denied: namespace=public/default"
  ```

- [#14345](https://github.com/emqx/emqx/pull/14345) 既存の有効なKafkaコンシューマーソースを持つノードを起動または再起動すると、Kafkaコンシューマースーパーバイザーの起動失敗によりクラッシュログが出力される問題を修正。ログ例：

  ```
  2024-12-03T17:11:31.861584+00:00 [warning] tag: CONNECTOR/KAFKA_CONSUMER, msg: add_channel_failed, reason: #{reason => {noproc,{gen_server,call,[emqx_bridge_sup,{start_child,#{id => emqx_bridge_kafka_consumer_sup,restart => permanent,shutdown => infinity,start => {emqx_bridge_kafka_consumer_sup,start_link,[]},type => supervisor,modules => [emqx_bridge_kafka_consumer_sup]}},infinity]}},stacktrace => [{gen_server,call,3,[{file,"gen_server.erl"},{line,419}]},{emqx_bridge_kafka_impl_consumer,ensure_consumer_supervisor_started,0,[{file,"src/emqx_bridge_kafka_impl_consumer.erl"},{line,395}]},{emqx_bridge_kafka_impl_consumer,start_consumer,5,[{file,"src/emqx_bridge_kafka_impl_consumer.erl"},{line,427}]},{emqx_bridge_kafka_impl_consumer,on_add_channel,4,[{file,"src/emqx_bridge_kafka_impl_consumer.erl"},{line,254}]},{emqx_resource,call_add_channel,5,[{file,"src/emqx_resource.erl"},{line,565}]},{emqx_resource_manager,add_channels_in_list,2,[{file,"src/emqx_resource_manager.erl"},{line,872}]},{emqx_resource_manager,channels_health_check,2,[{file,"src/emqx_resource_manager.erl"},{line,1304}]},{emqx_resource_manager,continue_resource_health_check_not_connected,2,[{file,"src/emqx_resource_manager.erl"},{line,1235}]},{gen_statem,loop_state_callback,11,[{file,"gen_statem.erl"},{line,1397}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,241}]}],exception => exit}, resource_id: <<"connector:kafka_consumer:a">>, channel_id: <<"source:kafka_consumer:b:connector:kafka_consumer:a">>
  ```

- [#14362](https://github.com/emqx/emqx/pull/14362) MySQLドライバーを更新し、クラッシュ後の復旧を改善。修正前は以下のようなログが出力されていました：

  ```
  crasher: initial call: mysql_conn:init/1, pid: <0.8940.0>, registered_name: [], exit: {timeout,[{gen_server,init_it,6,[{file,\"gen_server.erl\"},{line,961}]},{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,241}]}]} Supervisor: {<0.8938.0>,ecpool_worker_sup}. Context: start_error. Reason: timeout. Offender: id={worker,1},pid=undefined. Generic server <0.23934.0> terminating. Reason: {{case_clause,{error,closed}},[{mysql_protocol,fetch_response,7,[{file,\"mysql_protocol.erl\"},{line,569}]},{mysql_conn,query,4,[{file,\"mysql_conn.erl\"},{line,648}]},{mysql_conn,handle_call,3,[{file,\"mysql_conn.erl\"},{line,316}]},{gen_server,try_handle_call,4,[{file,\"gen_server.erl\"},{line,1131}]},{gen_server,handle_msg,6,[{file,\"gen_server.erl\"},{line,1160}]},{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,241}]}]} Supervisor: {<0.23913.0>,ecpool_worker_sup}. Context: shutdown. Reason: reached_max_restart_intensity. Offender: id={worker,8}
  ```

- [#14375](https://github.com/emqx/emqx/pull/14375) Kafkaコンシューマーソースのドライラン結果に詳細なエラー情報を追加し、Kafkaブローカーの問題診断を支援。

#### コマンドラインインターフェース

- [#14357](https://github.com/emqx/emqx/pull/14357) `bin/emqx help`コマンドの不具合を修正。正しい使用方法情報が表示されるようになりました。

#### 設定ファイル

- [#14371](https://github.com/emqx/emqx/pull/14371) クライアントIDオーバーライド式で`undefined`や`null`が文字列として表示される問題を修正。これらは空文字列として正しく表示されるようになりました。

- [#14376](https://github.com/emqx/emqx/pull/14376) 存在しないログファイルディレクトリを指定した場合に、デフォルトのログディレクトリ`${EMQX_LOG_DIR}`にフォールバックするよう設定インポートを強化。

#### オブザーバビリティ

- [#14267](https://github.com/emqx/emqx/pull/14267) シークレット文字列がファイルパス（例：`file:///path/to/the/secret`）の場合、ログやHTTPレスポンスでのマスキングを回避するようログ動作を変更。

- 永続ライセンスの`emqx_license_expiry_at` Prometheus値取得時の`function_clause`エラーを解消。

#### ゲートウェイ

- [#14445](https://github.com/emqx/emqx/pull/14445) JT/T 808クライアントで無効な下流制御メッセージを受信した際に接続処理がクラッシュする問題を修正。

## 5.8.3

*リリース日: 2024-12-05*

EMQX 5.8.3へアップグレードする前に、破壊的変更と既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#14219](https://github.com/emqx/emqx/pull/14219) 接続レートリミッターを強化し、システムのレジリエンスを向上。

  - **高接続レート時のシステム安定性と応答性の改善**：従来は接続レート制限超過時にリスナーのアクセプターが新規接続を無視し、短期間に多数のクライアントが接続・再接続すると回復不能な状態になることがありました。現在は、リスナーは保留中の接続を受け入れますが、レート制限に達した場合は即座に切断します。これによりリソース負荷を軽減し、ピーク時のシステムレジリエンスを向上します。

  - **新リスナーオプション`nolinger`を追加**：`true`に設定すると、ソケットクローズ時に即座にTCP-RSTを送信し、SYNフラッド攻撃の緩和や接続処理効率の向上に寄与します。

  - **MQTTリスナーの`max_connection`設定はシステム制限で上限を設定**：OSの`ulimit`や`node.process_limit`による制限を超える設定（`infinity`や過大値）は自動的にシステム最大値に調整されます。

  - **SSLリスナーの`ssl_options`は変更前に検証**：従来は無効なSSLオプション（非対応のTLSバージョンなど）を受け入れ、リスナー再設定後にクライアント接続失敗を引き起こしていました。今回の更新で：

    - 無効なSSLオプション設定がある場合、ノード起動に失敗します。

    - ダッシュボードや設定API経由で無効なSSLオプションを適用しようとすると、`400`ステータスコードで拒否されます。

#### 認証・認可

- [#14147](https://github.com/emqx/emqx/pull/14147) LDAP拡張マッチフィルターで`memberOf`構文をサポート。例：`(&(objectClass=class)(memberOf:1.2.840.113556.1.4.1941:=CN=GroupName,OU=emqx,DC=WL,DC=com))`

#### データ統合

- [#14166](https://github.com/emqx/emqx/pull/14166) RabbitMQプロデューサーで`exchange`および`routing_key`をテンプレート値として設定可能に。ペイロードのフィールドに基づく動的ルーティングが可能。

  例：ペイロードの`akey`フィールドに基づき`routing_key`を`${payload.akey}`で設定。

  ※バッチモードでは、`exchange`と`routing_key`のテンプレート値はバッチ内のすべてのメッセージで一定である必要があります。

- [#14176](https://github.com/emqx/emqx/pull/14176) ルールエンジンのRabbitMQソースアクションに`queue`、`exchange`、`routing_key`の追加メタデータを公開。ルール内でこれらのフィールドを直接参照可能。

  例：

  ```sql
  select *,
     queue as payload.queue, exchange as payload.exchange, routing_key as payload.routing_key
  from
     "$bridges/rabbitmq:test"
  ```

- [#14218](https://github.com/emqx/emqx/pull/14218) S3互換ストレージプロバイダー向けにvhostスタイルのバケットアクセスとリダイレクト処理を改善。S3ブリッジおよびファイル転送バックエンド設定で利用可能。

#### 設定

- [#14195](https://github.com/emqx/emqx/pull/14195) クライアントIDオーバーライド機能を追加。

  `mqtt.clientid_override={Expression}`設定により、クライアントIDのカスタムオーバーライドが可能に。これに伴い、`use_userid_as_clientid`および`peer_cert_as_clientid`オプションは非推奨となり、6.0まで互換性維持のため残存します。

#### MQTT over QUIC

- [#14283](https://github.com/emqx/emqx/pull/14283) QUICトランスポートを改善し、`quicer`を0.1.9にアップグレード。

  - 異常シナリオでのリモートストリームリソースの早期解放。

  - トラブルシューティング用APIを追加。詳細は https://github.com/emqx/quic/compare/0.1.6...0.1.9 を参照。

### バグ修正

#### Core MQTTの機能強化

- [#14201](https://github.com/emqx/emqx/pull/14201) WebSocket接続がレートリミットに達した際に `check_gc` 警告が表示されるのを防止しました。

- [#14215](https://github.com/emqx/emqx/pull/14215) リテイナーが無効化されている場合に、RESTまたはCLI経由でリテイナーを呼び出すと例外が発生する問題を修正しました。

- [#14223](https://github.com/emqx/emqx/pull/14223) WebSocketのクローズ理由がアトムとして返されるようにし、クラッシュを回避しました。

- [#14260](https://github.com/emqx/emqx/pull/14260) CONNECTパケットがアイドルタイムアウト（デフォルト15秒）前に完全に受信されなかった場合に、接続プロセスがクラッシュする稀なレースコンディションを解消しました。

- [#14268](https://github.com/emqx/emqx/pull/14268) CONNECTパケットがアイドルタイムアウト前に完全に受信されなかった際にWebSocket接続プロセスがクラッシュする別の稀なレースコンディションを修正しました。

- [#14266](https://github.com/emqx/emqx/pull/14266) `emqtt` をバージョン1.13.0から1.13.5に更新しました。詳細は[emqttの変更履歴](https://github.com/emqx/emqtt/blob/1.13.5/changelog.md)をご参照ください。

#### Durable Sessions（永続セッション）

- [#14160](https://github.com/emqx/emqx/pull/14160) MQTT仕様に準拠し、永続セッションのサブスクリプションにおける `$` で始まるトピックへのトピックマッチングルールの適用を正しく行うようにしました。

- [#14229](https://github.com/emqx/emqx/pull/14229) Durable StorageのRaft/RocksDBバックエンド実装における複数の問題を修正しました。これらは特定のケースでDurable Shared Subscriptionsが使用する内部データベースの正確性やレプリカの収束に影響を及ぼす可能性がありました。

- [#14298](https://github.com/emqx/emqx/pull/14298) DS Raft/RocksDBバックエンドの一時的なリモートシャード障害に対するレジリエンスを向上させ、シャードの更新ポーリング時に発生していた永続セッションのクラッシュを防止しました。

#### REST API

- [#14117](https://github.com/emqx/emqx/pull/14117) REST APIドキュメントにおいて、`Users` エンドポイントが誤って `Basic` 認証をサポートしていると記載されていた問題を修正しました。

#### 認証

- [#14314](https://github.com/emqx/emqx/pull/14314) 以前は機能していなかった `scram:http` 認証を修正しました。

- [#14305](https://github.com/emqx/emqx/pull/14305) 認証で使用されていたハッシュアルゴリズム `MD4`、`MD5`、`RIPEMD-160` のサポートを削除しました。これらは[NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard)に準拠していません。

#### ルールエンジン

- [#14217](https://github.com/emqx/emqx/pull/14217) スキーマレジストリのエンドポイントに関するサンプル設定の誤りを修正しました。

#### データ統合

- [#14172](https://github.com/emqx/emqx/pull/14172) HTTP APIを用いてコネクターの接続テストを行う際に、HTTPリクエストがタイムアウトした場合にリソースが残存する可能性があったレースコンディションを解消しました。

- [#14178](https://github.com/emqx/emqx/pull/14178) クラスター内の異なるノードでルールが同時に削除された場合に、設定同期が特定のノードで停止してしまう問題を修正しました。

- [#14226](https://github.com/emqx/emqx/pull/14226) 高負荷時にノードがリソースメトリクス（例：アクションやソース）を見失い、再起動なしに復旧できなくなるシナリオを緩和しました。リソースの再起動やメトリクスのリセット時に、失われたメトリクスの再作成を試みるようにしています。

  さらに、`matched` のような「ホットパス」メトリクスに関連するメトリクス失敗の警告ログを抑制し、過剰なログ出力を防止しています。

- [#14265](https://github.com/emqx/emqx/pull/14265) MQTT Sourceアクションがサブスクライブに失敗した場合に、コネクター停止時に `badkey` エラーが発生する問題を修正しました。

- [#14296](https://github.com/emqx/emqx/pull/14296) 遅延起動する `ecpool_worker` により `ecpool_sup` がブロックされる問題を防止しました。

- [#14126](https://github.com/emqx/emqx/pull/14126) Oracle統合のプリペアドステートメントに関する問題を修正しました。修正前は存在しないテーブルカラムを参照するなど無効なプリペアドステートメントが適用されると、最も古いバージョンのステートメントが適用されて不整合が生じていました。

- [#14181](https://github.com/emqx/emqx/pull/14181) KafkaおよびPulsarのパブリッシャーを、COMMITファイルが破損している場合でもクラッシュしないようにレジリエンスを向上させました。ディスクモードバッファのCOMMITファイルが破損している場合は無視されます。これにより、過去に送信されたメッセージの一部が再送される可能性はありますが、パブリッシャーのクラッシュは防止されます。

#### 設定

- [#14180](https://github.com/emqx/emqx/pull/14180) 変数が `undefined` または `null` にバインドされている場合に、variform式が `'undefined'` を返す問題を修正し、代わりに空文字列を返すようにしました。

- [#14289](https://github.com/emqx/emqx/pull/14289) 異なる環境から設定をインポートする際のログファイルパス問題を修正しました。Dockerでは `EMQX_LOG_DIR` 環境変数が `/opt/emqx/log` に設定されますが、RPM/DEBパッケージでは `/var/log/emqx/` です。修正前はログファイルパス（デフォルトファイルハンドラーおよび監査ハンドラー）が環境変数展開されており、存在しないディレクトリへのパスが原因でクラッシュする可能性がありました。

  修正後はエクスポート時に環境変数展開を行わず、古いバージョンの絶対パスは新環境に存在しない場合に環境変数に戻されます。

- [#14313](https://github.com/emqx/emqx/pull/14313) レプリカントノードでREST APIのブートストラップAPIキー読み込み時にEMQXが起動停止する問題を修正し、ブートストラップAPIキーはコアノードでのみ読み込まれるようにしました。

#### ゲートウェイ

- [#14276](https://github.com/emqx/emqx/pull/14276) JT/T808メッセージ解析失敗時のエラーログを強化し、トラブルシューティングに役立つ詳細情報を提供するようにしました。

#### 拡張機能

- [#14243](https://github.com/emqx/emqx/pull/14243) 一部のゲートウェイで `client.connect` フックがトリガーされない問題を修正しました。

#### MQTT over QUIC

- [#14258](https://github.com/emqx/emqx/pull/14258) QUIC接続のシャットダウンタイムアウトを短縮しました。従来は優雅なシャットダウンに5秒のタイムアウトがありましたが、クライアントが応答しない場合にEMQXが以下のような警告をログに出力したり、ダッシュボードで切断時にタイムアウトが発生することがありました。

  ```
  [warning] msg: session_stepdown_request_timeout, action: discard,
  ```

  現在は「キック」アクションで1秒、その他のシナリオで3秒に短縮されています。

## 5.8.2

*リリース日: 2024-11-12*

EMQX 5.8.2にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 機能強化

#### Core MQTTの機能強化

- [#14059](https://github.com/emqx/emqx/pull/14059) リテイナーに新しい設定オプションを追加し、保持メッセージのメッセージ有効期限間隔の上限を設定可能にしました。これにより、ストレージが逼迫している場合にガベージコレクションがより早くメッセージを削除できます。

- [#14072](https://github.com/emqx/emqx/pull/14072) 仮想マシンの印刷可能範囲をUnicodeに更新しました。これにより、メッセージ内の特定のバイナリデータの可読性が向上します。例えば、以前は `<<116,101,115,116,228,184,173,230,150,135>>` と表示されていたバイナリが、`<<"test中文"/utf8>>` のようにより明確に表示されます。

#### MQTT Durable Sessions（永続セッション）

- [#14130](https://github.com/emqx/emqx/pull/14130) アイドル状態の永続セッションのCPU使用率を削減しました。

  以前はアイドル状態の永続セッションが定期的にDSストリームのリストを更新していましたが、今回の変更でストリームの検出がイベントベースになり、アイドル時のCPU消費が大幅に低減されました。また、新しいストリーム通知の遅延も短縮され、エンドツーエンド処理の長尾レイテンシが事実上解消されました。

#### REST API

- [#13889](https://github.com/emqx/emqx/pull/13889) `/api/v5/monitor_current` および `/api/v5/metrics` APIのパフォーマンスを向上させました。

  これらのAPIは以前、クラスター内のノードを順次ループしてクエリしていましたが、現在は並列でクエリを送信し、応答時間を短縮しています。レイテンシはクラスター内で最も遅いノードに依存します。

  また、`/api/v5/monitor_current` APIに `node` パラメータを追加し、クラスター全体ではなく特定ノードへのターゲットクエリが可能になりました。例：`?aggregate=false&node=emqx@node1.domain.name` は指定ノードのデータのみを返します。

#### EMQXクラスター

- [#13903](https://github.com/emqx/emqx/pull/13903) レプリカントノードが自身と同じリリースバージョンのコアノードを見つけられない場合にユーザーに通知するログを追加しました。

#### セキュリティ

- [#13923](https://github.com/emqx/emqx/pull/13923) 認証、認可、マウントポイントテンプレートに `zone` サポートを追加しました。

  以前はクライアントの `zone` を認証や認可ルールで参照するには `client_attrs` 経由でアクセスする必要がありましたが、現在は `${zone}` プレースホルダーを直接使用可能で、ルール作成が簡素化され、ゾーンごとの設定が可能になりました。

  例として、以下のACLルールは `${zone}` を用いてクライアントの割り当てゾーンに基づく権限を動的に適用します：`{allow, all, all, ["${zone}/${username}/#"]}`。

- [#14102](https://github.com/emqx/emqx/pull/14102) 秘密ファイルからのSSL秘密鍵パスフレーズサポートを追加しました。

  `password` に `...ssl.password = "file://{path-to-secret-file}"` を設定すると、EMQXは秘密ファイルからパスフレーズを読み取ります。

#### データ統合

- [#14065](https://github.com/emqx/emqx/pull/14065) データ統合に新しい `queuing_bytes` メトリクスを追加しました。このメトリクスは特定のアクションのバッファリングに使用されるRAMおよび/またはディスクリソースを示します。現在、Pulsar Producerアクションのみこのメトリクスをサポートしていません。

- [#14044](https://github.com/emqx/emqx/pull/14044) IoTDB Thriftドライバーを強化し、`server` フィールドに複数アドレスを指定可能にしました。現在の接続が失敗した場合、次のアドレスに接続を試みます。

- [#14048](https://github.com/emqx/emqx/pull/14048) Kafka/Confluent/Azure Event Hub Producerアクションのレジリエンスを向上させました。これらのアクションが正常に作成され健康状態が良好な場合、不明なトピックを検出しても不健康状態にはならず、そのままメッセージをキューイングし続けます。

- [#14079](https://github.com/emqx/emqx/pull/14079) Kafka Consumerソースに `max_wait_time` 設定オプションを追加し、Kafkaブローカーからのフェッチ応答を待つ最大時間を設定可能にしました。

#### オブザーバビリティ

- [#14096](https://github.com/emqx/emqx/pull/14096) `emqx_conf_sync_txid` をPrometheusメトリクスとして公開し、クラスター内の各ノードの設定ファイル同期状況を監視可能にしました。

#### MQTT over QUIC

- [#13814](https://github.com/emqx/emqx/pull/13814) MQTT over QUICマルチストリーム向けの接続スコープキープアライブを導入しました。

  制御ストリームがアイドル状態でもデータストリームがアクティブな場合、MQTT接続を維持します。

  以前はクライアントがアイドルの制御ストリームで `MQTT.PINGREQ` を送信する必要がありましたが、現在は接続ごとに全ストリームのアクティビティを共有状態で管理し、接続が生存しているかを判定します。これにより、Head-of-Line（HOL）ブロッキングによるキープアライブタイムアウトのリスクが低減されます。

- [#13984](https://github.com/emqx/emqx/pull/13984) Quicer NIFライブラリがシステムの `libcrypto` にリンクするようになり、セキュリティ、パフォーマンス、およびシステムOpenSSLアップデートとの互換性が向上しました。

  **注意:** RHEL 7/CentOS 7はOpenSSL 1.0.xを使用し続けるため、この変更は適用されません。

- [#14112](https://github.com/emqx/emqx/pull/14112) QUICリスナーに `ssl_options.hibernate_after` サポートを追加し、QUICトランスポートのメモリフットプリントを削減しました。

### バグ修正

#### Core MQTT機能

- [#13931](https://github.com/emqx/emqx/pull/13931) `gen_rpc` ライブラリをバージョン3.4.1に更新し、クライアントソケット初期化エラーがサーバー側でノードレベルのクラッシュに発展する問題を修正しました。

- [#13969](https://github.com/emqx/emqx/pull/13969) 大量の期限切れ保持メッセージがある場合でも効率的にリソースを使用できるよう、期限切れ保持メッセージの定期クリーンアップを最適化しました。

- [#14068](https://github.com/emqx/emqx/pull/14068) ゲートウェイ実装モジュール全てに `handle_frame_error/2` コールバックを追加し、メッセージ解析エラーを処理可能にしました。

- [#14037](https://github.com/emqx/emqx/pull/14037) 新規ノードが既存クラスターに参加する際、ピアノードの一時的な利用不可を許容する内部データベースのブートストラップ処理を改善しました。

- [#14116](https://github.com/emqx/emqx/pull/14116) クラスター参加後にリテイナーのデフォルト設定が誤って生成される問題を修正しました。

#### MQTT Durable Sessions（永続セッション）

- [#14042](https://github.com/emqx/emqx/pull/14042) サブスクリプションパラメータ（QoS、`no_local`、`upgrade_qos`など）更新後の永続セッションのクラッシュを修正しました。

- [#14052](https://github.com/emqx/emqx/pull/14052) cgroups使用時のメモリ使用量報告を修正しました。

- [#14055](https://github.com/emqx/emqx/pull/14055) `/clients_v2` APIがオフラインクライアントのフィルタリング引数を正しく適用するように修正しました。以前は `username` フィルターのみが適用され、他の引数は無視されていました。

- [#14151](https://github.com/emqx/emqx/pull/14151) `/clients_v2` APIの `conn_state` フィルターが永続セッションのオフラインクライアントに対して誤って `connected` と判定する問題を修正しました。

- [#14057](https://github.com/emqx/emqx/pull/14057) セッション永続化有効時にEMQX 5.7.xからアップグレードした際、Messages DSデータベースが起動しない互換性問題を修正しました。これはデータベース構成スキーマのわずかな違いによるものでした。

#### REST API

- [#14023](https://github.com/emqx/emqx/pull/14023) `GET /monitor` HTTP APIで、指定された時間ウィンドウによって返される値が実際より高く表示される問題を修正しました。1時間以内のデータポイントではダッシュボード上の視覚的な問題に留まりますが、1時間を超えるデータポイントではデータの歪みが恒久的に発生していました。

  対象メトリクスは以下の通りです：

  - `disconnected_durable_sessions`
  - `subscriptions_durable`
  - `subscriptions`
  - `topics`
  - `connections`
  - `live_connections`

#### EMQXクラスター

- [#13996](https://github.com/emqx/emqx/pull/13996) `emqx conf fix` コマンド使用時に、特にノードの設定キーが欠落している場合に発生する断続的なクラッシュを修正しました。

#### セキュリティ

- [#13922](https://github.com/emqx/emqx/pull/13922) CRL（証明書失効リスト）キャッシュのキーを完全なDistribution Point（DP）URLに変更しました。以前はURLのパス部分のみを使用していたため、複数のDPが同じパスを共有すると競合が発生していました。

- [#13924](https://github.com/emqx/emqx/pull/13924) JWT認証失敗時にJWKキーがデバッグログに漏洩する問題を修正しました。

- [#13998](https://github.com/emqx/emqx/pull/13998) 無効な設定でOIDCが構成された場合にSSO機能がクラッシュする問題を修正しました。

#### データ統合

- [#13916](https://github.com/emqx/emqx/pull/13916) IotDB Thriftドライバーの「接続テスト」で例外が発生する問題を修正しました。

- [#14001](https://github.com/emqx/emqx/pull/14001) コネクター、アクション、ソース、認証、認可などのリソースが一時的に切断された後に誤って接続済みかつ正常なチャネルと報告するレースコンディションを解消しました。これにより、`action_not_found` の過剰なログ出力を防止します。

- [#13913](https://github.com/emqx/emqx/pull/13913) アクションおよびソースのHTTP APIで、リソース更新または削除時にタイムアウトが発生すると500ステータスコードを返す問題を修正しました。

- [#14101](https://github.com/emqx/emqx/pull/14101) 同名のソースとアクションが同時に作成されている場合にリソース削除が失敗する問題を修正しました。

- [#13901](https://github.com/emqx/emqx/pull/13901) Postgres統合のプリペアドステートメントを修正しました。無効なプリペアドステートメント（例：未知のテーブルカラム参照）を使用すると、古いバージョンのプリペアドステートメントが適用されていました。

- [#14005](https://github.com/emqx/emqx/pull/14005) SSL有効時にIoTDB Thriftドライバーが機能しない問題を修正しました。

- [#14125](https://github.com/emqx/emqx/pull/14125) IoTDBのThriftドライバーは非同期モードをサポートしていないため、設定で非同期モードが指定された場合にエラーログを出力するようにしました。

- [#14008](https://github.com/emqx/emqx/pull/14008) 集約モード（例：S3、Azure Blob Storage、Snowflake）のアクションで、アップロード時に集約バッチがスキップされる可能性があるレースコンディションを修正しました。

- [#14015](https://github.com/emqx/emqx/pull/14015) Kafka/Confluent/Azure Event Hub Producerアクションのディスクバッファリングで、再起動後に新しいメッセージが到着するまでキューイングされたメッセージが送信されない問題を修正しました。この修正は固定トピック（プレースホルダーなし）のアクションに適用されます。また、EMQX 5.7.2以前の古いディスクバッファディレクトリを検出すると、自動的に現在の構造にリネームしてデータ損失を防止します。

- [#14069](https://github.com/emqx/emqx/pull/14069) Cassandra統合のプリペアドステートメントを修正しました。EMQXアクションのSQLテンプレートが変更された際に、更新されたステートメントがCassandra用に準備できず書き込み失敗が発生していました。

- [#14079](https://github.com/emqx/emqx/pull/14079) Kafkaで複数のパーティションが同じパーティションリーダーを共有する場合のレイテンシ問題を修正しました。以前はKafkaが1接続あたり1つのフェッチリクエストのみ許可するため、フェッチリクエストがブロックされていました。修正後は各パーティションコンシューマーがパーティションリーダーに対して独自のTCP接続を確立し、遅延を防止します。

- [#14106](https://github.com/emqx/emqx/pull/14106) 単一のKafka Consumerコネクター内で同じKafkaトピックを持つソースの重複を禁止するバリデーションを追加しました。

- [#14120](https://github.com/emqx/emqx/pull/14120) Pulsarコネクターのヘルスチェックでタイムアウト時の不要なログノイズを削減しました。以前はタイムアウトが繰り返しエラーログを生成していました。

#### オブザーバビリティ

- [#13909](https://github.com/emqx/emqx/pull/13909) ペイロードがUTF-8の可読文字として表示できない場合のログフォーマットを修正しました。

- [#14061](https://github.com/emqx/emqx/pull/14061) `emqx_cm:request_stepdown/3` が失敗した際のログ情報を改善しました。

  同じClientIDを持つ別チャネルを終了する必要がある場合、対象チャネルが既に閉じられているとレースコンディションが発生し、役に立たないエラーログやスタックトレースが生成されていました。これらは今後生成されません。

- [#14070](https://github.com/emqx/emqx/pull/14070) コネクターの状態情報が長大になるため、エラーおよび警告ログから削除しました。問題解析時は `emqx_resource:list_instances_verbose/0` で状態を確認できます。以下は修正前のログ例です：

  ```
  pid: <0.43914.0>, connector: connector:sqlserver:connector-05a2e105, reason: [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Argument data type varchar is invalid for argument 2 of dateadd function. SQLSTATE IS: 42000, state: {"resource_opts":{"start_timeout":5000,"start_after_created":true,"health_check_interval":15000},"pool_name":"connector:sqlserver:connector-05a2e105","installed_channels":{"action:sqlserver:action-4b033621:connector:sqlserver:connector-05a2e105":{"sql_templates":{"batch_insert_temp":{"send_message":{"batch_insert_tks":["{str,<<\" ( \">>}","{var,[<<\"messageId\">>]}","{str,<<\", \">>}","{var,[<<\"measurement\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_1\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_2\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_3\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_4\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_1\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_2\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_3\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_4\">>]}","{str,<<\", DATEADD(MS, \">>}","{var,[<<\"ms_shift\">>]}","{str,<<\", DATEADD(S, \">>}","{var,[<<\"s_shift\">>]}","{str,<<\", '19700101 00:00:00:000') ))\">>}"],"batch_insert_part":"insert into TransactionLog(MessageId, Measurement, Fault1, Fault2, Fault3, Fault4, Value1, Value2, Value3, Value4, DateStamp) \r\n"}}}}}},msg: invalid_request
  ```

- [#14099](https://github.com/emqx/emqx/pull/14099) MQTTメッセージ内のUTF-8文字列検証失敗時に発生していたエラーレベルのログを削除しました。

- [#14091](https://github.com/emqx/emqx/pull/14091) サポートされていない書き込み構文をユーザーが指定した際にログに `function_clause` が含まれる問題を修正しました。

  未対応構文の例：

  ```bash
  weather,location=us-midwest,season=summer temperature=82 ${timestamp}u
  ```

  修正前は以下のように `function_clause` エラーがログに含まれていました：

  ```
  pid: <0.558392.0>, info: {"stacktrace":["{emqx_bridge_influxdb_connector,parse_timestamp,[[1719350482910000000,<<\"u\">>]],[{file,\"emqx_bridge_influxdb_connector.erl\"},{line,692}]}", ...], ..., "error":"{error,function_clause}"}, tag: ERROR, msg: resource_exception
  ```

  この修正により、構文エラー時に `function_clause` がログから除外され、ログの明瞭性が向上しました。

#### 監査ログ

- [#14152](https://github.com/emqx/emqx/pull/14152) 過度に長い監査ログ内容の保存を防ぐため、ログの切り詰めを実装しました。

#### クラスターリンク

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクの `topics` 設定において、重複するトピックフィルターが存在するとクロスクラスター間のメッセージルーティングが不整合かつ不完全になる問題を修正しました。各トピックフィルターは個別に処理されますが、重複フィルターはクロスクラスターのルーティングに追加の複雑さをもたらす可能性があります。

- [#13929](https://github.com/emqx/emqx/pull/13929) クラスターリンクが時折停止し、上流クラスターの手動再起動まで動作しなくなる問題を修正しました。

## 5.8.1

*リリース日: 2024-10-14*

EMQX 5.8.1にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 重要な変更

- [#13956](https://github.com/emqx/emqx/pull/13956) `gen_rpc` ライブラリをバージョン3.4.1に更新し、ノードクラッシュ問題を修正しました。以前はRPCチャネル確立中にノードが強制シャットダウンされると、クラスターピアノードがクラッシュする可能性がありました。

### 機能強化

#### Core MQTTの機能強化

- [#13525](https://github.com/emqx/emqx/pull/13525) `shared_subscription_strategy` が `sticky` に設定されている場合の初期ピック戦略を指定する新設定 `shared_subscription_initial_sticky_pick` を追加しました。

- [#13942](https://github.com/emqx/emqx/pull/13942) HTTPクライアントが最新リクエストの期限切れ後10秒間アクティビティがない場合に自動再接続するようになりました。以前はサーバー応答を無期限に待機し、サーバーがリクエストを破棄するとタイムアウトしていました。

  この変更は以下のコンポーネントに影響します：

  - HTTP認証
  - HTTP認可
  - Webhook（HTTPコネクター）
  - GCP PubSubコネクター
  - S3コネクター
  - InfluxDBコネクター
  - Couchbaseコネクター
  - IoTDBコネクター
  - Snowflakeコネクター

#### 認証と認可

- [#13863](https://github.com/emqx/emqx/pull/13863) 生のACLルールのトピック名テンプレートで `${cert_common_name}` プレースホルダーをサポートしました。

- [#13864](https://github.com/emqx/emqx/pull/13864) LDAPクエリフィルターで `memberOf` 構文を使用可能にしました。

- [#13810](https://github.com/emqx/emqx/pull/13810) クライアント情報認証を追加しました。

  `cinfo` タイプのクライアント情報認証は、クライアントのプロパティや属性をユーザー定義ルールに照らしてチェックする軽量認証機構です。ルールはVariform式を用いてマッチ条件と認証結果を定義します。例えば、ユーザー名なしのクライアントを迅速に遮断するには、マッチ条件を `str_eq(username, '')`、チェック結果を `deny` と設定します。

- [#13792](https://github.com/emqx/emqx/pull/13792) 禁止クライアントAPI `GET /banned` がクエリ文字列のフィルターでルールを検索可能になりました。

  利用可能なフィルター：

  - clientid
  - username
  - peerhost
  - like_clientid
  - like_username
  - like_peerhost
  - like_peerhost_net

  新規禁止クライアント追加時、`until` パラメータ未指定のエントリーのデフォルト有効期限を1年から `infinite` に変更しました。

#### ルールエンジン

- [#13773](https://github.com/emqx/emqx/pull/13773) 無効化されたルールアクションは `out_of_service` 警告を発生させなくなりました。

  以前はアクションが無効化されていると `msg: out_of_service` の警告ログが出力され、`actions.failed` カウンターが増加していました。

  変更後は無効化アクションは `msg: discarded` のデバッグログを出力し、新設のカウンター `actions.discarded` が増加します。

- [#13804](https://github.com/emqx/emqx/pull/13804) Confluent Schema Registryを外部プロバイダーとしてスキーマレジストリで使用可能にしました。

#### データ統合

- [#13716](https://github.com/emqx/emqx/pull/13716) IoTDBコネクター向けにThriftドライバーを導入しました。

- [#13745](https://github.com/emqx/emqx/pull/13745) EMQXがSnowflakeとのデータ統合をサポートしました。

- [#13783](https://github.com/emqx/emqx/pull/13783) Kafkaプロデューサーの非同期モード時のバッファRAM使用量を削減しました。

- [#13861](https://github.com/emqx/emqx/pull/13861) SQLテンプレート内の未定義変数をデータベース書き込み時に `NULL` として扱う新設定 `undefined_vars_as_null` を一部データ統合アクションに追加しました。

  対象Sinkアクション：

  - MySQL
  - ClickHouse
  - SQLServer
  - TDengine
  - DynamoDB

#### MQTT over QUIC

- [#13814](https://github.com/emqx/emqx/pull/13814) MQTT over QUICマルチストリーム向けの接続スコープキープアライブ機能を導入しました。

  制御ストリームがアイドルでも他のデータストリームがアクティブな場合にMQTT接続を維持します。

  以前はクライアントがアイドルの制御ストリームで `MQTT.PINGREQ` を送信する必要がありましたが、現在は接続ごとに全ストリームのアクティビティを共有状態で管理し、接続生存判定に利用します。これにより、Head-of-Line（HOL）ブロッキングによるキープアライブタイムアウトリスクが低減し、接続安定性が向上します。

#### Durable Storage（永続ストレージ）

- [#13788](https://github.com/emqx/emqx/pull/13788) DS Shared Subscriptions機能が無効化されている場合、フルスタートアップシーケンスの実行を防止しました。これにより、不要な内部データベースの初期化や大量のディスク使用を回避します。

#### クラスターリンク

- [#13835](https://github.com/emqx/emqx/pull/13835) 指定したクラスターリンクのメトリクスをリセットする `PUT /cluster/links/link/:name/metrics/reset` HTTP APIエンドポイントを追加しました。

#### ダッシュボード

- [#13873](https://github.com/emqx/emqx/pull/13873) `/api/v5/monitor` エンドポイントのパフォーマンスを改善しました。

  特にノード数の多いクラスターでタイムアウトが頻発していたモニターページのパフォーマンスを向上させています。

  主な改善点：

  - クラスター内ノードからのメトリクス取得を並行RPC呼び出しに変更
  - クエリ時間範囲に応じたデータダウンサンプリングを導入
    - 過去1時間：10秒間隔
    - 過去1日：1分間隔
    - 過去3日：5分間隔
    - 過去7日：10分間隔
  - EMQX停止期間のダミーデータポイントを追加し、ダッシュボードのタイムラインにギャップを表示可能に

#### エンタープライズライセンス

- [#13910](https://github.com/emqx/emqx/pull/13910) エンタープライズエディションのライセンスチェック性能を向上させました。

### バグ修正

#### Core MQTT機能

- [#13702](https://github.com/emqx/emqx/pull/13702) ノードダウン時に対応する排他サブスクリプションをクリーンアップするようにしました。

- [#13708](https://github.com/emqx/emqx/pull/13708) 共有サブスクリプションの「sticky」戦略が「random」に劣化する問題を修正しました。

- [#13733](https://github.com/emqx/emqx/pull/13733) `emqx ctl conf load` コマンドでhttpsリスナー設定時に `cacertfile` をオプション化しました。

- [#13742](https://github.com/emqx/emqx/pull/13742) クライアントが `#` または `+` にサブスクライブした際に `$` で始まるトピックの保持メッセージを受信する問題を修正しました。

- [#13754](https://github.com/emqx/emqx/pull/13754) WebSocket接続が一貫して切断される問題を修正しました。

- [#13756](https://github.com/emqx/emqx/pull/13756) ブローカー割り当てクライアントIDによりランダム性を持たせました。

- [#13790](https://github.com/emqx/emqx/pull/13790) MQTTコネクターのデフォルトハートビート間隔を300秒から160秒に短縮しました。

  この変更により、クラウドプロバイダーによるロードバランサーやファイアウォールのアイドル制限（通常3〜5分）によるTCP接続タイムアウトを防止します。

- [#13832](https://github.com/emqx/emqx/pull/13832) 永続セッション有効時に `/publish` REST APIエンドポイントで500エラーが発生する問題を修正しました。

- [#13842](https://github.com/emqx/emqx/pull/13842) UTF-8文字列検証例外を修正しました。

- [#13956](https://github.com/emqx/emqx/pull/13956) `gen_rpc` ライブラリを3.4.1に更新し、クライアントソケット初期化エラーがサーバー側でノードクラッシュに発展する問題を修正しました。

#### アップグレードとマイグレーション

- [#13731](https://github.com/emqx/emqx/pull/13731) EMQX 5.4.0で稼働するクラスターがEMQX 5.8.0にアップグレードできない問題を解決しました。これには5.4.0で作成された特定の内部データベーステーブルを新スキーマに合わせて更新するマイグレーション手順が含まれます。

#### 認証

- [#13726](https://github.com/emqx/emqx/pull/13726) Kerberos認証ライブラリを更新し、認証リクエストが同時に初期化される場合に失敗することがあるFILEタイプキャッシュからMEMORYタイプキャッシュに変更しました。

#### ルールエンジン

- [#13735](https://github.com/emqx/emqx/pull/13735) デコード対象ペイロードが無効な場合のメッセージ変換エラーメッセージを改善しました。

- [#13769](https://github.com/emqx/emqx/pull/13769) JSONスキーマバリデーション（ドラフト3）で `extends` プロパティを使用すると常にバリデーション失敗する問題を修正しました。

#### データ統合

- [#13851](https://github.com/emqx/emqx/pull/13851) IoTDB Thriftドライバーの「接続テスト」で発生する例外を修正しました。

- [#13724](https://github.com/emqx/emqx/pull/13724) Azure Blob StorageおよびS3アクションの集約モードで、最大レコード数到達後に集約データ送信がより早くトリガーされるようにしました。

- [#13734](https://github.com/emqx/emqx/pull/13734) Azure Blob Storageコネクターの設定エラーメッセージをより分かりやすくしました。

- [#13736](https://github.com/emqx/emqx/pull/13736) Kafkaプロデューサーをクライアント再認証対応にアップグレードしました。詳細は[kafka_protocol#122](https://github.com/kafka4beam/kafka_protocol/pull/122)をご参照ください。

  また、以下のマイナーな問題も修正しました：

  - [PR#13727](https://github.com/emqx/emqx/pull/13727) および [wolff#74](https://github.com/kafka4beam/wolff/pull/74) における `unexpected_info` エラーログ
  - Kafka接続のレースコンディションによる `einval` クラッシュレポート [kafka_protocol#124](https://github.com/kafka4beam/kafka_protocol/pull/124)

- [#13896](https://github.com/emqx/emqx/pull/13896) Pulsarクライアントを `0.8.3` から `0.8.4` にアップグレードしました（[pulsar#61](https://github.com/emqx/pulsar-client-erl/pull/61)参照）。

  修正前はプロデューサークライアントが「ソケットエラー」（通常の「ソケットクローズ」ではない）を経験すると、エラーハンドリングなしに閉じたソケットにデータ送信を続けていました。EMQXダッシュボードでは `total` カウンターが増加し続ける一方、`success`、`failed`、`dropped` カウンターは増加しませんでした。

- [#13897](https://github.com/emqx/emqx/pull/13897) Microsoft SQL ServerコネクターがMicrosoft ODBC 18に対応しました。

- [#13902](https://github.com/emqx/emqx/pull/13902) MySQL統合のプリペアドステートメントの問題を修正しました。無効なプリペアドステートメント使用時に古いバージョンに戻る問題がありました。

- [#13906](https://github.com/emqx/emqx/pull/13906) PostgreSQL統合のプリペアドステートメントの問題を修正しました。無効なプリペアドステートメント使用時に古いバージョンに戻る問題がありました。

- [#13921](https://github.com/emqx/emqx/pull/13921) Pulsarプロデューサーアクションの `sync_timeout` パラメータ変更がリクエストタイムアウトに反映されない問題を修正しました。

  また、`resource_opts.request_ttl` 設定は期待通りにTTLに影響しなかったため非推奨とし、混乱防止のため削除しました（現在は `retention_period` 設定で管理）。

- [#13959](https://github.com/emqx/emqx/pull/13959) Pulsarクライアントを `0.8.4` から `0.8.5` にアップグレードしました（[pulsar#62](https://github.com/emqx/pulsar-client-erl/pull/62)参照）。特定のレースコンディションでプロデューサーがクライアントプロセスとの通信に失敗し、クライアントプロセスが予期せず停止し自動再起動しない問題を修正しました。修正前は手動での再起動が唯一の対処法でした。

- [#13965](https://github.com/emqx/emqx/pull/13965) IotDB Sinkでバッチモード使用時に発生する関数節エラーを修正しました。

- [#13971](https://github.com/emqx/emqx/pull/13971) EMQX Enterprise 5.8.0で導入されたKafkaプロデューサーのバグを修正しました。初期化段階でメタデータ取得に失敗するとプロデューサーがクラッシュしていました。

- [#13973](https://github.com/emqx/emqx/pull/13973) Microsoft SQL Server統合でサーバー接続停止時に複数のエラーおよび警告がログに記録される問題を修正しました。

#### 管理と運用

- [#13963](https://github.com/emqx/emqx/pull/13963) 監査ログ機能に関する以下の問題を修正しました：

  - 監査ログ機能がシングルサインオン（SSO）機能と非互換で、SSOイベントごとに例外が発生していた問題
  - 不正アクセス試行（例：POST専用エンドポイントへのGETリクエスト）がログに記録されていなかった問題

#### クラスターリンク

- [#13888](https://github.com/emqx/emqx/pull/13888) クラスターリンクのHTTP APIでclientidなしのリンク更新ができない問題を修正しました。

- [#13927](https://github.com/emqx/emqx/pull/13927) ローカルクラスターに非常に多くのトピックが存在する場合にクラスターリンクのブートストラップ処理がクラッシュする問題を修正しました。

## 5.8.0

*リリース日: 2024-08-28*

アップグレード前に[5.8の既知の問題](./known-issues-5.8.md)をご確認ください。

### 機能強化

#### クラスターリンク

- [#13126](https://github.com/emqx/emqx/pull/13126) 複数の独立したEMQXクラスターを接続・通信可能にするクラスターリンク機能を導入しました。これにより、地理的に分散したクラスター間でもクライアント間の効率的なメッセージ交換が可能となり、MQTT展開の柔軟性とリーチが向上します。

#### Core MQTT機能

- [#13009](https://github.com/emqx/emqx/pull/13009) レート制限によるメッセージ受信一時停止のログレベルを `debug` から `warning` に変更しました。`socket_receive_paused_by_rate_limit` ログは過剰出力を防ぐためスロットリングされます。

#### 認証と認可

- [#13350](https://github.com/emqx/emqx/pull/13350) 接続中クライアントのサーバー名を取得し、クライアント情報の `peersni` として保存する機能を追加しました。

- [#12418](https://github.com/emqx/emqx/pull/12418) JWT認証を強化し、複数オブジェクトのリストによるクレーム検証をサポートしました：

  ```
  [
    {
      name = "claim_name",
      value = "${username}"
    },
    ...
  ]
  ```

  期待値は他の認証機構と同様にテンプレートとして扱われ、`${username}` や `${clientid}` のような任意の式が使用可能になりました。以前は固定の `"${username}"` や `"${clientid}"` のみ対応していました。

  `verify_claims` パラメータのドキュメントも改善しました。

- [#13229](https://github.com/emqx/emqx/pull/13229) 認証テンプレートで `${cert_pem}` プレースホルダーをサポートしました。

- [#13324](https://github.com/emqx/emqx/pull/13324) EMQXダッシュボードがOIDCプロトコル対応のIDサービス（例：Okta）と連携し、OIDCベースのシングルサインオン（SSO）を可能にしました。

- [#13534](https://github.com/emqx/emqx/pull/13534) スーパーユーザーが認可チェックをバイパスした際のトレースログを追加しました。

- [#13601](https://github.com/emqx/emqx/pull/13601) GSSAPIメカニズム（SASL-GSSAPI with Kerberos V5）を用いたKerberos認証をEMQXに追加しました。これによりMQTTクライアントとサーバー間で非セキュアネットワーク上でも安全に認証可能になります。

#### データ統合

- [#13144](https://github.com/emqx/emqx/pull/13144) ブリッジバッファのオーバーフロー時にメッセージがドロップされるログレベルを `info` から `warning` に変更し、スロットリングを追加しました。以前はデフォルトログ設定で表示されませんでした。

- [#13492](https://github.com/emqx/emqx/pull/13492) `GET /connectors` および `GET /connectors/:id` APIに、特定コネクターに依存するアクションおよびソースのリストを含めるようにしました。また、`GET /actions`、`GET /sources`、`GET /actions/:id`、`GET /sources/:id` APIは特定アクションまたはソースに関連付けられたルールのリストを返します。

- [#13505](https://github.com/emqx/emqx/pull/13505) HTTP APIでデータ統合アクションまたはソースのIDに基づくルールフィルタリングを追加しました。

- [#13506](https://github.com/emqx/emqx/pull/13506) ルールエンジンイベントに `peerhost` フィールドに加え、`peername` フィールド（`IP:PORT`形式の文字列）を追加しました。

- [#13516](https://github.com/emqx/emqx/pull/13516) `republish` アクションに `direct_dispatch` 引数を追加しました。

`direct_dispatch` が `true` に設定されている場合（またはテンプレートから `true` としてレンダリングされる場合）、メッセージはサブスクライバーに直接ディスパッチされます。この機能は、追加のルールのトリガーや同じルールの再帰的な起動を防ぐのに役立ちます。

- [#13573](https://github.com/emqx/emqx/pull/13573) クライアント接続イベントおよびメッセージの `publish` イベントのSQLコンテキストに `client_attrs` を導入しました。  
  これにより、ユーザーはルールのSQL文内でクライアント属性にアクセスできるようになりました。例えば、`SELECT client_attrs.attr1 AS attribute1` のように記述し、データ統合アクション内で `${attribute1}` を利用できます。

- [#13640](https://github.com/emqx/emqx/pull/13640) ルール用に新しいSQL関数 `coalesce/2` と `coalesce_ne/2` を追加しました。  

  これらの関数はルールSQL式内のnull値処理を簡素化します。例えば、以下のような記述の代わりに：

  ```
  SELECT
    CASE
      WHEN is_null(payload.path.to.value) THEN
        0
      ELSE
        payload.path.to.value
    END AS my_value
  ```

  より簡潔に `SELECT coalesce(payload.path.to.value, 0) AS my_value` と書けます。

- [#12959](https://github.com/emqx/emqx/pull/12959) Kafka Producerコネクターにおいて、ヘルスチェック用の専用トピックを設定する新しいオプションを導入しました。  
  これにより、パーティションリーダーとの接続問題（誤った資格情報や欠落など）をより正確に検出できます。

- [#12961](https://github.com/emqx/emqx/pull/12961) Kafka ConsumerソースでグループIDを事前にカスタマイズする設定オプションを追加しました。

- [#13069](https://github.com/emqx/emqx/pull/13069) EMQXはAzure Blob Storageとのデータ統合をサポートしました。

- [#13199](https://github.com/emqx/emqx/pull/13199) メッセージ変換機能を実装しました。この機能により、ルールエンジンでSQLルールを定義することなく、シンプルなvariform構文だけで受信メッセージの変換や拡張が可能です。

  **利用例:**  
  Avro形式でエンコードされたメッセージを受信し、それをJSONにデコードしたいとします。さらに、パブリッシュしたクライアントのクライアント属性から取得した `tenant` 属性をトピックの先頭に付加してからルールエンジンで処理したい場合、以下の設定で実現できます。

  ```hocon
  message_transformation {
    transformations = [
      {
        name = mytransformation
        failure_action = drop
        payload_decoder = {type = avro, schema = myschema}
        payload_encoder = {type = json}
        operations = [
          {key = "topic", value = "concat([client_attrs.tenant, '/', topic])"}
        ]
      }
    ]
  }
  ```

  この設定は `mytransformation` という名前の変換を指定し、  
  - 指定したスキーマでAvro形式のペイロードをデコードし、  
  - ペイロードをJSON形式にエンコードし、  
  - クライアント属性の `tenant` と元のトピックを連結してトピックを変更します。

- [#13415](https://github.com/emqx/emqx/pull/13415) EMQXはCouchbaseとのデータ統合をサポートしました。

- [#13463](https://github.com/emqx/emqx/pull/13463) GCP PubSub Producerアクションを強化し、PubSubからHTTPステータスコード502（Bad Gateway）または503（Service Unavailable）を受け取った場合に自動リトライを行うようにしました。リトライはリクエストが成功するかメッセージのTTLに達するまで継続されます。

- [#13546](https://github.com/emqx/emqx/pull/13546) Pulsar Producerアクションにクエリモードの設定オプションを追加し、Pulsarサービスに送信する前のデータクエリ方法をカスタマイズ可能にしました。

- [#13650](https://github.com/emqx/emqx/pull/13650) EMQXはDataLayersとのデータ統合をサポートしました。

#### オペレーション

- [#13202](https://github.com/emqx/emqx/pull/13202) クラスター設定の不整合を修正するためのコマンド `emqx ctl conf cluster_sync fix` を導入しました。このコマンドは、すべてのノードの設定を最も高い `tnx_id` を持つノードの設定に同期し、クラスター全体の整合性を確保します。

- [#13250](https://github.com/emqx/emqx/pull/13250) `cluster.discovery_strategy` に新しい値 `singleton` を追加しました。このオプションを選択すると、実質的にクラスタリングが無効になり、ノードは他のノードとの接続試行を拒否します。

- [#13370](https://github.com/emqx/emqx/pull/13370) durableストレージ用の新しいストレージレイアウト `wildcard_optimized` を追加しました。主な改善点は以下の通りです。  
  - 新レイアウトは固有のレイテンシを持ちません。  
  - MQTTメッセージはより省スペースな形式でシリアライズされます。

- [#13524](https://github.com/emqx/emqx/pull/13524) 専用トピックをより効果的に管理するためのCLIインターフェース `emqx ctl exclusive` を追加しました。これにより管理者は専用トピックのサブスクリプション状態を正確に管理・トラブルシュートでき、予期しない障害を防止できます。

- [#13597](https://github.com/emqx/emqx/pull/13597) プラグインが自身で使用する証明書ファイルを格納・管理するための薄いラッパー関数を追加しました。これにより、証明書のガベージコレクション機能による誤削除を防止します。

- [#13626](https://github.com/emqx/emqx/pull/13626) リスナーの有効化/無効化を切り替える新コマンド `emqx ctl listeners enable <Identifier> <Bool>` を追加しました。

- [#13493](https://github.com/emqx/emqx/pull/13493) RPCライブラリ `gen_rpc` をバージョン3.4.0にアップグレードしました。この更新により、RPCサーバーのデフォルトソケットオプションが `true` から `active-100` に変更され、RPCサーバーが高負荷時にピアノードへのバックプレッシャーを導入します。

- [#13665](https://github.com/emqx/emqx/pull/13665) prometheusエンドポイントに新しいメトリクス `emqx_actions_count` を追加しました。これはすべてのルールによって追加されたアクション数（RepublishアクションやConsole Outputアクションを含む）をカウントします。

- [#13434](https://github.com/emqx/emqx/pull/13434) `rpc` 設定を簡素化しました。新設定 `rpc.server_port` を追加し、`rpc.tcp_server_port` と `rpc.ssl_server_port` を置き換えます。  
  また、`rpc.tcp_client_num` はTCPとSSLの両方に対応するため `rpc.client_num` に名称変更されました。旧設定名は後方互換のためエイリアスとして残されています。

### バグ修正

#### コアMQTT機能

- [#12944](https://github.com/emqx/emqx/pull/12944) `strict_mode=false` の場合に非UTF8クライアントIDのクライアントが接続しようとするとクラッシュする問題を修正しました。

- [#13006](https://github.com/emqx/emqx/pull/13006) 保持中、遅延、引き継ぎセッションメッセージの検証を強化し、正規表現による禁止クライアントIDルールに準拠するようにしました。これにより、ネットワーク遅延や別セッションによる引き継ぎメッセージが禁止ルールを回避する問題を解消しました。

#### 認証と認可

- [#13024](https://github.com/emqx/emqx/pull/13024) デフォルトACL拒否ルールを追加し、`+/#` トピックパターンへのサブスクライブを拒否するようにしました。EMQXはデフォルトで `#` トピックへのサブスクライブを拒否しているため、整合性のため `+/#` も拒否します。

- [#13040](https://github.com/emqx/emqx/pull/13040) HTTP認証を改善しました。  
  * HTTPの `Content-Type` ヘッダーが欠落または不明な場合のエラーロギングを詳細化。  
  * 認証HTTPリクエストのクエリパラメーターが二重エンコードされる問題を修正。  
  * POSTメソッドでJSONコンテンツタイプを設定した認証リクエストにおいて、JSONテンプレートが有効なJSONにレンダリングされない場合のエラーメッセージを改善。例えば、`${password}` プレースホルダーに非UTF8のパスワードが渡された場合などのデバッグが容易になりました。

- [#13196](https://github.com/emqx/emqx/pull/13196) 組み込み認可データベースにACLルール数の制限を追加し、クライアントまたはユーザーごとにデフォルト100ルールに制限しました。

- [#13584](https://github.com/emqx/emqx/pull/13584) HTTP認可作成時にHTTPヘッダーリストが空の場合に発生するエラーを修正しました。

- [#13618](https://github.com/emqx/emqx/pull/13618) `authorization/sources` エンドポイントの型仕様を改善し、より明確で簡潔なエラーメッセージを提供するようにしました。

- [#13624](https://github.com/emqx/emqx/pull/13624) 組み込みオーソライザーでクライアントまたはユーザーのルール更新時に `max_rules` 制限を超える問題を修正しました。

- [#13678](https://github.com/emqx/emqx/pull/13678) チェーン内の認証器削除操作を冪等にし、存在しない認証器の削除でも常に成功するようにしました。

#### データ統合

- [#13207](https://github.com/emqx/emqx/pull/13207) `republish` ルールエンジンアクションの成功・失敗判定を正確に反映するよう改善しました。以前は、メッセージがサブスクライバーに届かなくても成功メトリクスが増加していましたが、失敗時は正しく失敗メトリクスが増加します。

- [#13425](https://github.com/emqx/emqx/pull/13425) MQTTコネクターのエラーログをより明確かつ詳細に改善しました。

- [#13589](https://github.com/emqx/emqx/pull/13589) HTTP API経由でIDに文字列 `"null"` を指定したルール作成を許可してしまう問題を修正し、不整合な設定を防止しました。

- [#13414](https://github.com/emqx/emqx/pull/13414) RabbitMQコネクターのエラーログをより明確かつ詳細に改善しました。

#### ファイル転送

- [#12514](https://github.com/emqx/emqx/pull/12514) ファイル転送コマンドの結果報告を `$file-response/${clientid}` チャンネルに送信する際、アセンブル処理中にチャネルが切断されるとステータスメッセージが失われる問題を修正しました。専用プロセスでアセンブル状態を監視し、切断後も確実にメッセージを送信します。

#### オペレーション

- [#13078](https://github.com/emqx/emqx/pull/13078) EMQX Management APIでJSONボディを含むリクエストに `Content-Type: application/json` ヘッダーが必須であることを検証し、欠落時は `400 Bad Request` ではなく `415 Unsupported Media Type` を返すよう改善しました。

- [#13225](https://github.com/emqx/emqx/pull/13225) 認証・認可APIのセキュリティを強化し、パスワードなどの機密情報をレスポンスで `******` にマスクするようにしました。以前は元のパスワードが返されることがありました。

#### ゲートウェイ

- [#13607](https://github.com/emqx/emqx/pull/13607) API経由で表示されるCoAPサブスクライブのQoSレベルが実際のQoSと異なる問題を修正しました。この不整合によりダッシュボードでの表示が誤っていました。

## 5.7.2

*リリース日: 2024-08-07*

### 強化

- [#13317](https://github.com/emqx/emqx/pull/13317) 新しい認可ソースごとのメトリクスタイプ `ignore` を追加しました。  
  これは認可ソースが認可を試みたものの、適用不可やエラーにより判定不能となった場合にインクリメントされます。

- [#13336](https://github.com/emqx/emqx/pull/13336) 空のEMQXノードまたはクラスターの組み込みデータベースにCSVまたはJSON形式のブートストラップファイルを使って認証データを初期化する機能を追加しました。新しい設定項目 `bootstrap_file` と `bootstrap_type` が導入されます。

- [#13348](https://github.com/emqx/emqx/pull/13348) ログ設定に `payload_encode` フィールドを追加し、ログデータ内のペイロードのフォーマットを指定可能にしました。

- [#13436](https://github.com/emqx/emqx/pull/13436) JWKSリクエストにカスタムリクエストヘッダーを追加するオプションを追加しました。

- [#13507](https://github.com/emqx/emqx/pull/13507) ルールエンジンおよびvariform式に新しい組み込み関数 `getenv` を追加し、環境変数へのアクセスを容易にしました。  

  制約は以下の通りです。  
  - OS環境変数を読む際に `EMQXVAR_` プレフィックスが付加されます。例えば、`getenv('FOO_BAR')` は `EMQXVAR_FOO_BAR` を読みます。  
  - OS環境から読み込んだ値は不変です。

- [#13521](https://github.com/emqx/emqx/pull/13521) LDAPクエリのタイムアウトにより接続が使えなくなり、以降のクエリが古い結果を返す問題を修正しました。タイムアウト時に自動再接続を行うように改善しています。

- [#13528](https://github.com/emqx/emqx/pull/13528) データ統合の回復不能エラーイベントに対してログスロットリングを適用しました。

- [#13548](https://github.com/emqx/emqx/pull/13548) REST API経由でプラグイン設定が更新された際に、オプションで `<PluginName>_app` モジュールの `on_config_changed/2` コールバック関数を呼び出せるようにしました。  
  例えばプラグイン名とバージョンが `my_plugin-1.0.0` の場合、`my_plugin_app:on_config_changed/2` が呼び出されます。

- [#13386](https://github.com/emqx/emqx/pull/13386) 空のEMQXノードまたはクラスターにCSV形式のブートストラップファイルを使って禁止クライアントリストを初期化する機能を追加しました。設定項目は `banned.bootstrap_file` です。  
  ファイルはカンマ区切りのCSVで、1行目はヘッダー行でなければなりません。利用可能なヘッダーは以下の通りです。

  - as :: 必須  
  - who :: 必須  
  - by  :: 任意  
  - reason :: 任意  
  - at :: 任意  
  - until :: 任意  

  詳細は[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。  
  以降の各行はヘッダーと同数のカラムを持ち、省略した場合は `undefined` とみなされます。

- [#13452](https://github.com/emqx/emqx/pull/13518) Kafkaプロデューサーアクションの `topic` 設定でテンプレートが使用可能になりました。  

  Kafkaのトピック自動作成が無効なため、トピックは事前に存在している必要があります。存在しないトピックへのメッセージ送信は回復不能エラーとなります。  
  また、テンプレートに必要な情報がメッセージコンテキストに不足している場合も回復不能エラーとなります。例えばテンプレートが `t-${t}` でメッセージに `t` が定義されていない場合などです。  
  詳細は[Kafka動的トピックの設定](../develop/data-integration/data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。  

  この機能はAzure Event HubsおよびConfluent Platformのプロデューサー統合でもサポートされています。

- [#13504](https://github.com/emqx/emqx/pull/13504) `scram` 認証機構のHTTPバックエンドを導入しました。  

  このバックエンドは外部Webリソースを利用してSCRAM認証データ（クライアントの保存キー、サーバーキー、ソルト）を提供します。  
  また、`is_superuser`、`client_attrs`、`expire_at`、`acl` などの追加認証・認可拡張フィールドもサポートします。  

  注：これはRFC 7804（Salted Challenge Response HTTP Authentication Mechanism）の実装ではありません。

- [#13441](https://github.com/emqx/emqx/pull/13441) CoAPゲートウェイの接続モードを強化しました。UDP接続は常に対応するゲートウェイ接続の `clientid` にバインドされます。

### バグ修正

- [#13222](https://github.com/emqx/emqx/pull/13222) `CONNECT` パケットのWillメッセージに関するフラグチェックとエラーハンドリングの問題を修正しました。  
  詳細仕様は以下を参照してください。  
  - MQTT-v3.1.1-[MQTT-3.1.2-13], MQTT-v5.0-[MQTT-3.1.2-11]  
  - MQTT-v3.1.1-[MQTT-3.1.2-14], MQTT-v5.0-[MQTT-3.1.2-12]  
  - MQTT-v3.1.1-[MQTT-3.1.2-15], MQTT-v5.0-[MQTT-3.1.2-13]

- [#13307](https://github.com/emqx/emqx/pull/13307) `ekka` ライブラリをバージョン0.19.5に更新しました。このバージョンは `mria` 0.8.8を使用し、オートヒール機能を強化しています。以前はすべてのコアノードが到達可能な場合のみ動作していましたが、今回の更新でコアノードの過半数が生存していればオートヒールが適用されます。詳細は[Mria PR](https://github.com/emqx/mria/pull/180)を参照してください。

- [#13334](https://github.com/emqx/emqx/pull/13334) MQTT v3.1.1のCONNECTパケットにおける `PasswordFlag` の厳格モードチェックを実装し、プロトコル仕様に準拠させました。  
  注：バグ互換性のため、このチェックは厳格モード時のみ行われます。

- [#13344](https://github.com/emqx/emqx/pull/13344) `POST /clients/:clientid/subscribe/bulk` APIが、APIリクエストを受けたノードが指定された `clientid` との接続を保持していない場合に正しく動作しない問題を修正しました。

- [#13358](https://github.com/emqx/emqx/pull/13358) `authn_complete_event` イベントの `reason` 表示が誤っていた問題を修正しました。

- [#13375](https://github.com/emqx/emqx/pull/13375) リスナー設定の `max_conn_rate`、`messages_rate`、`bytes_rate` にデフォルト値として `infinity` を追加しました。

- [#13382](https://github.com/emqx/emqx/pull/13382) `emqtt` ライブラリをバージョン0.4.14に更新し、不整合状態のプールを再利用できなかった問題を修正しました。

- [#13389](https://github.com/emqx/emqx/pull/13389) `pbkdf2` の `Derived Key Length` が負の整数になる問題を修正しました。

- [#13389](https://github.com/emqx/emqx/pull/13389) 認可ルールのトピックが誤って解析される問題を修正しました。

- [#13393](https://github.com/emqx/emqx/pull/13393) ノードがクラスターに参加後、プラグインアプリケーションが再起動に失敗し、フックが正しくインストールされず不整合状態になる問題を修正しました。

- [#13398](https://github.com/emqx/emqx/pull/13398) コマンドラインで組み込み認可データベースをリロードした際にACLルールが誤ってクリアされる問題を修正しました。

- [#13403](https://github.com/emqx/emqx/pull/13403) 環境変数設定のオーバーライドでパスワードがログに出力されるセキュリティ問題を修正しました。

- [#13408](https://github.com/emqx/emqx/pull/13408) 無効なソルトやパスワードタイプによる認証試行で発生する `function_clause` クラッシュを修正し、エラーハンドリングを改善しました。

- [#13419](https://github.com/emqx/emqx/pull/13419) `/configs` APIのクラッシュログメッセージでヒントが文字化けする問題を修正しました。

- [#13422](https://github.com/emqx/emqx/pull/13422) `force_shutdown.max_heap_size` オプションを0に設定してチューニングを無効化できない問題を修正しました。

- [#13442](https://github.com/emqx/emqx/pull/13442) アクション/ソースのヘルスチェック間隔設定が無視され、コネクターの間隔が使われていた問題を修正しました。これによりアクション/ソースごとに独立した正確なヘルスチェックが可能になりました。

- [#13503](https://github.com/emqx/emqx/pull/13503) コネクターが初回起動時に設定されたヘルスチェック間隔を遵守しない問題を修正しました。更新や再起動で正しい間隔が適用されていました。

- [#13515](https://github.com/emqx/emqx/pull/13515) ノードダウン時に同一クライアントが同じ専用トピックにサブスクライブできない問題を修正しました。

- [#13527](https://github.com/emqx/emqx/pull/13527) ルールエンジンでメッセージパブリッシュイベントのSQLテスト実行時に、`$bridges/...` ソースを `FROM` 句に含めると常に結果が返らない問題を修正しました。

- [#13541](https://github.com/emqx/emqx/pull/13541) リスナーのCRLチェック無効化がリスナー再起動を必要とする問題を修正しました。

- [#13305](https://github.com/emqx/emqx/pull/13305) Redisコネクターのエラーハンドリングを改善しました。  
  Redisモードが `single` または `sentinel` でユーザー名・パスワード未設定の場合、ダッシュボードのコネクターテストでタイムアウトエラーが発生していましたが、ユーザーにわかりやすいエラーメッセージを返すようにしました。  
  また、すべてのRedisコネクタータイプで詳細なエラー情報を追加しました。

- [#13327](https://github.com/emqx/emqx/pull/13327) Kafka、Confluent、Azure Event Hubs統合で、同じコネクターとトピックを共有する複数アクションが一つのアクション削除・無効化時に互いに影響を与える問題を修正しました。

- [#13345](https://github.com/emqx/emqx/pull/13345) スキーマレジストリで、名前が長すぎるか不正な形式のスキーマ作成時のエラーメッセージを明確化しました。

- [#13420](https://github.com/emqx/emqx/pull/13420) スキーマ検証設定で空のトピックフィルターリストを許可しないバリデーションルールを実装しました。空リストは意味のあるメッセージ変換ができないためです。

- [#13543](https://github.com/emqx/emqx/pull/13543) スキーマレジストリのProtobufスキーマ内部キャッシュがスキーマ削除・更新後に正しくクリーンアップされない問題を修正しました。

- [#13332](https://github.com/emqx/emqx/pull/13332) Amazon S3コネクターの不適切な設定時によりわかりやすいエラーメッセージを提供するよう改善しました。

- [#13552](https://github.com/emqx/emqx/pull/13552) EMQXプラグインの起動タイムアウト制限を追加し、デフォルト10秒に設定しました。これにより問題のあるプラグインが起動時にランタイムエラーを引き起こし、EMQXの停止・再起動時にメイン起動処理がハングする問題を防止します。

## 5.7.1

*リリース日: 2024-06-26*

### 強化

- [#12983](https://github.com/emqx/emqx/pull/12983) 認証完了イベント用の新しいルールエンジンイベント `$events/client_check_authn_complete` を追加しました。

- [#13175](https://github.com/emqx/emqx/pull/13175) Postgresベースのコネクターに `disable_prepared_statements` オプションを追加しました。  
  これはPGBouncerやTransactionモードのSupabaseなど、プリペアドステートメントセッション機能をサポートしないエンドポイント向けです。

- [#13180](https://github.com/emqx/emqx/pull/13180) Erlang/OTP 26上でEMQXを実行した際のクライアントメッセージ処理性能を改善し、ファンインモードでメッセージスループットを10%向上させました。

- [#13191](https://github.com/emqx/emqx/pull/13191) EMQX DockerイメージをErlang/OTP 26対応にアップグレードしました。  
  EMQX本体はv5.5以降Erlang/OTP 26で動作していましたが、DockerイメージはErlang/OTP 25でした。今回のアップグレードで全リリースがErlang/OTP 26対応となります。  
  これにより、古いバージョンのEMQXが新しいバージョンのノードとクラスターを組む際にスキーマレジストリで発生していた以下の問題が解消されます。

  ```
  Error loading module '$schema_parser___CiYAWBja87PleCyKZ58h__SparkPlug_B_BUILT-IN':,
  This BEAM file was compiled for a later version of the runtime system than the current (Erlang/OTP 25).
  ```

  古いバージョンがすでにクラスターに参加している場合は、該当ノードで以下コマンドを実行し再起動してください。

  ```shell
  emqx eval 'lists:foreach(fun(Key) -> mnesia:dirty_delete(emqx_ee_schema_registry_protobuf_cache_tab, Key) end, mnesia:dirty_all_keys(emqx_ee_schema_registry_protobuf_cache_tab)).'
  ```

- [#13242](https://github.com/emqx/emqx/pull/13242) EMQXダッシュボードのリスナー起動速度を大幅に向上させました。

- [#13172](https://github.com/emqx/emqx/pull/13172) RedisのHSET（またはHMSET）複数フィールド値を準備するためのルール関数 `map_to_redis_hset_args` を追加しました。  
  例えば、`payload.value` が複数データフィールドのマップの場合、  
  `SELECT  map_to_redis_hset_args(payload.value) as hset_fields FROM  "t/#"` のように記述し、Redisアクションで `HMSET name1 ${hset_fields}` のコマンドテンプレートをレンダリングできます。

- [#13210](https://github.com/emqx/emqx/pull/13210) スキーマ検証の挿入・更新時に、参照されるスキーマやメッセージタイプがスキーマレジストリに存在するかをEMQXが検証するようになりました。

- [#13211](https://github.com/emqx/emqx/pull/13211) TLSリスナーを強化し、より柔軟なTLS検証をサポートしました。

  - `partial_chain` サポート：`partial_chain` オプションを `true` に設定すると、不完全な証明書チェーンでも接続を許可します。詳細は[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

  - 証明書のキー使用法検証：  
    [rfc5280](https://www.rfc-editor.org/rfc/rfc5280#section-4.2.1.12)で定義された拡張キー使用法（Extended Key Usage）を検証するオプション `verify_peer_ext_key_usage` を追加しました。TLSハンドシェイク時にピア証明書が特定のキー使用（例："serverAuth"）を持つことを強制し、セキュリティを強化します。詳細は[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

- [#13274](https://github.com/emqx/emqx/pull/13274) RocketMQコネクターでSSL設定をサポートしました。

### バグ修正

- [#13156](https://github.com/emqx/emqx/pull/13156) EMQX v5.7.0へのアップデート後にダッシュボードのモニタリングページがクラッシュする問題を修正しました。

- [#13164](https://github.com/emqx/emqx/pull/13164) HTTP認可リクエストボディのエンコードを修正しました。  
  以前は `accept` ヘッダーからフォーマットを取得していましたが、`content-type` ヘッダーを尊重するように変更しました。  
  また、v4互換のため `access` テンプレート変数を追加しました。SUBSCRIBEアクションのアクセスコードは `1`、PUBLISHアクションは `2` です。

- [#13238](https://github.com/emqx/emqx/pull/13238) サポートされていないContent-TypeヘッダーのHTTP認可リクエストでのエラーメッセージを改善しました。

- [#13258](https://github.com/emqx/emqx/pull/13258) MQTT-SNゲートウェイが依存関係の起動順序不正により正しく再起動しない問題を修正しました。

- [#13273](https://github.com/emqx/emqx/pull/13273) 複数設定のURIの取り扱いを修正・改善しました。  
  * 認証・認可設定で、パスなしURI（例：`https://example.com?q=x`）が誤って拒否されていた問題を修正し、正しく有効と認識するようにしました。  
  * コネクター設定で、ユーザー情報やフラグメントを含むURIが誤って許容されていた問題を改善しました。

- [#13276](https://github.com/emqx/emqx/pull/13276) durableメッセージストレージの内部状態の一部が新世代ストレージ設定時に正しく永続化されない問題を修正しました。世代管理はメッセージの有効期限管理やクリーンアップに重要で、これにより再起動後にメッセージが失われる問題が解消されました。

- [#13291](https://github.com/emqx/emqx/pull/13291) ダウンしているdurableストレージサイトがアップとして報告される問題を修正しました。

- [#13290](https://github.com/emqx/emqx/pull/13290) `$ bin/emqx ctl rules show rule_0hyd` コマンドでデータ統合アクション付きルールを表示すると出力がない問題を修正しました。

- [#13293](https://github.com/emqx/emqx/pull/13293) データバックアップからの復元時に保持メッセージの再インデックス作業を自動化しました。以前はバックアップファイルインポート後に手動で `emqx ctl retainer reindex start` コマンドを実行する必要がありました。  
  また、`retainer.backend.storage_type` が `ram` の場合でも保持メッセージのバックアップエクスポートが可能になりました。以前は `disc` のみ対応していました。

- [#13147](https://github.com/emqx/emqx/pull/13147) ルールエンジンのprotobufデコード関数でのデコード失敗時に、何が問題かを明確に示すエラーメッセージを追加しました。

- [#13140](https://github.com/emqx/emqx/pull/13140) republishアクションのテキストトレースがクラッシュし正しく表示されない問題を修正しました。

- [#13148](https://github.com/emqx/emqx/pull/13148) リソース接続待ちタイムアウト時に `/connectors/:connector-id/start` が500 HTTPステータスを返す問題を修正しました。

- [#13181](https://github.com/emqx/emqx/pull/13181) コネクター停止時にタイムアウトした場合、コネクタープロセスを強制終了するようにしました。これにより、アクションやソースの無効化失敗時のエラーメッセージが明確になりました。

- [#13216](https://github.com/emqx/emqx/pull/13216) MQTTブリッジの `clientid_prefix` 設定を尊重するようにしました。EMQX v5.4.1以降、クライアントIDは最大23バイトに制限されています。  
  - プレフィックスなし：従来通り長いクライアントIDはハッシュ化され23バイトに収まります。  
  - プレフィックスあり：  
    - プレフィックスが19バイト以下の場合、プレフィックスは保持され、残りの部分が4バイトにハッシュ化されます。  
    - プレフィックスが20バイト以上の場合、クライアントIDの短縮は行わず、プレフィックスを完全に保持します。

- [#13189](https://github.com/emqx/emqx/pull/13189) Microsoft SQL ServerやMySQLとのデータ統合で、テーブル名やカラム名に `values` を含むSQLテンプレートが使えなかった問題を修正しました。

- [#13070](https://github.com/emqx/emqx/pull/13070) Kafkaコネクターのエラーログを改善し、到達不能なadvertised listenerなどの詳細エラー情報を取得可能にしました。ログの冗長性を抑えるため、同一エラーは初回のみログ出力し、発生回数をカウントします。

- [#13093](https://github.com/emqx/emqx/pull/13093) Kafkaコンシューマーグループの安定性を改善しました。Kafkaグループコーディネーター再起動後にコンシューマーグループが二度リバランスする問題を解消しました。

- [#13277](https://github.com/emqx/emqx/pull/13277) Kafkaプロデューサーで `message_too_large` エラー発生時のエラーハンドリングを改善しました。  
  以前はサーバー側の `max.message.bytes` 調整を期待して大きすぎるメッセージバッチを再送し続けていました。  
  今回の修正で、大きすぎるメッセージは単一メッセージバッチに分割してリトライし、それでもサイズ超過の場合は破棄してデータフローを維持します。

- [#13130](https://github.com/emqx/emqx/pull/13130) Redisアクションのバッチリクエストのトレースメッセージを改善し、コマンド間にスペースとセミコロンを挿入して読みやすくしました。

- [#13136](https://github.com/emqx/emqx/pull/13136) Oracleアクションのテンプレートレンダリングトレースを改善し、可読性を向上させました。

- [#13197](https://github.com/emqx/emqx/pull/13197) AWS S3データ統合で、ダッシュボードUIやコネクターAPI経由で提供されたTLS証明書・キーをファイルシステムに自動保存できない問題を修正しました。

- [#13227](https://github.com/emqx/emqx/pull/13227) 集約モードで動作するAWS S3 Sinkで、設定の無効なキー・テンプレートがセットアップ時にエラーとして報告されず、後に回復困難なクラッシュを引き起こす問題を修正しました。

## 5.7.0

*リリース日: 2024-05-27*

### 強化

#### MQTT

Durable Sessions機能を実装しました。これはMQTTのPersistent Sessionsとそのメッセージをディスクに永続化し、EMQXクラスター内の複数ノード間でセッションメタデータとMQTTメッセージを継続的にレプリケートします。これにより効果的なフェイルオーバーとリカバリーが可能となり、サービスの継続性と高可用性を実現し、システムの信頼性を向上させます。

EMQX durableストレージに関するメトリクスをPrometheusに追加しました：

- `emqx_ds_egress_batches`  
- `emqx_ds_egress_batches_retry`  
- `emqx_ds_egress_batches_failed`  
- `emqx_ds_egress_messages`  
- `emqx_ds_egress_bytes`  
- `emqx_ds_egress_flush_time`  
- `emqx_ds_store_batch_time`  
- `emqx_ds_builtin_next_time`  
- `emqx_ds_storage_bitfield_lts_counter_seek`  
- `emqx_ds_storage_bitfield_lts_counter_next`  
- `emqx_ds_storage_bitfield_lts_counter_collision`  

注：これらのメトリクスはセッション永続化が有効な場合のみ表示されます。  
永続化されたメッセージ数もダッシュボードに追加されています。

Durable Sessions機能の詳細は [MQTT Durable Sessions](../guides/durability/durability_introduction.md) を参照してください。

#### セキュリティ

- [#12947](https://github.com/emqx/emqx/pull/12947) JWT認証で新しいオプション `disconnect_after_expire` をサポートしました。有効にすると、JWTトークンの有効期限切れ後にクライアントが切断されます。

  注：これは破壊的変更です。このオプションはデフォルトで有効になっており、デフォルト動作が変更されています。以前はJWTが有効期限切れでもクライアントは接続を維持できましたが、現在は有効期限切れ後に切断されます。以前の動作を維持したい場合は `disconnect_after_expire` を `false` に設定してください。

#### データ処理と統合

- [#12711](https://github.com/emqx/emqx/pull/12711) スキーマ検証機能を追加しました。この機能により、特定のトピックフィルターに対して検証を設定すると、パブリッシュされたメッセージに対して検証が実行されます。検証結果が不合格の場合、メッセージは破棄され、設定に応じてクライアントが切断されることがあります。詳細は [Schema Validation](../develop/data-integration/schema-validation.md) を参照してください。

- [#12899](https://github.com/emqx/emqx/pull/12899) RocketMQデータ統合で、ネームスペースとキーのディスパッチ戦略をサポートしました。

- [#12671](https://github.com/emqx/emqx/pull/12671) ルールエンジンSQL言語に `unescape` 関数を追加し、文字列内のエスケープシーケンス展開を可能にしました。SQL言語の文字列リテラルはエスケープコード（例：`\n`、`\t`）をサポートしていないため、この追加によりSQL式内での柔軟な文字列操作が可能になります。

- [#12898](https://github.com/emqx/emqx/pull/12898) IoTDBブリッジでiotdb 1.3.0およびバッチ挿入（batch_size/batch_time）オプションをサポートしました。

- [#12934](https://github.com/emqx/emqx/pull/12934) AWS S3アクションにCSV形式ファイル集約を追加しました。

#### オブザーバビリティ

- [#12827](https://github.com/emqx/emqx/pull/12827) ルールのトレースに新しいルールIDトレースフィルターを追加し、クライアントIDフィルターと併用可能にしました。テスト用途として、新しいHTTP APIエンドポイント（rules/:id/test）を使い、ルールを人工的に適用し、レンダリング後にアクションを停止することも可能です。

- [#12863](https://github.com/emqx/emqx/pull/12863) トレースログエントリをJSONオブジェクト形式でフォーマットできるようになりました。トレースパターン作成時にフォーマッターを "json" に設定してください。

- [#12844](https://github.com/emqx/emqx/pull/12844) CPU使用率とアイドル統計値の精度保持を修正し、Prometheus統計メトリクスおよびOpenTelemetryガバナンスメトリクスで常に小数点以下2桁で保存されるようにしました。

#### 拡張性

- [#12872](https://github.com/emqx/emqx/pull/12872) クライアント属性機能を実装しました。これは各クライアントに対してキー・バリュー形式の追加プロパティを設定可能にします。プロパティ値はMQTTクライアント接続情報（ユーザー名、クライアントID、TLS証明書など）から生成したり、認証成功時の戻り値から設定できます。これらのプロパティはEMQXの認証、認可、データ統合、MQTT拡張機能で利用可能です。静的なクライアントIDを直接使うよりも柔軟で、様々なビジネスシナリオに対応しやすく、開発効率と適応性を向上させます。

  **`client_attrs` の初期化**

  `client_attrs` フィールドは以下の `clientinfo` フィールドのいずれかから初期化できます：

  - `cn`：TLSクライアント証明書のコモンネーム  
  - `dn`：TLSクライアント証明書の識別名（Subject）  
  - `clientid`：クライアントが提供したMQTTクライアントID  
  - `username`：クライアントが提供したユーザー名  
  - `user_property`：MQTT CONNECTパケットの 'User-Property' からプロパティ値を抽出

  **認証応答による拡張**

  認証応答から `client_attrs` に属性をマージ可能です。サポートされる認証バックエンドは：

  - **HTTP**：HTTPレスポンスボディのJSONオブジェクト内の `client_attrs` フィールド  
  - **JWT**：JWT内の `client_attrs` クレーム

  **認証・認可での利用**

  認証前に `client_attrs` が初期化されていれば、外部認証リクエストで利用可能です。例えば、HTTPサーバー向けのリクエストテンプレート内で `${client_attrs.property1}` を使えます。

  - 認可設定やリクエストテンプレートで `client_attrs` を利用し、柔軟な制御が可能です。例：`acl.conf` で `{allow, all, all, ["${client_attrs.namespace}/#"]}` と記述し、`namespace` 属性に基づく許可を設定できます。

  - 他の認可バックエンドでも `${client_attrs.namespace}` をリクエストテンプレート内で利用可能です。

  クライアント属性機能の詳細は [Client Attributes](../develop/client-attributes/client-attributes.md) を参照してください。

- [#12910](https://github.com/emqx/emqx/pull/12910) プラグインの設定管理とスキーマ検証を追加しました。スキーマにメタデータで注釈を付けることも可能になり、ダッシュボードでのUIレンダリングが容易になります。詳細は[プラグインテンプレート](https://github.com/emqx/emqx-plugin-template/pull/126)および[プラグインドキュメント](../guides/extensions/plugins.md)をご参照ください。

#### 運用と管理

<!-- This is not ready to GA in 5.7
- [#12798](https://github.com/emqx/emqx/pull/12798) ページ番号の代わりにカーソルを使用した新しい `GET /api/v5/clients_v2` APIを追加しました。これは現在複数回テーブルを走査している旧APIエンドポイントより効率的です。
-->

- [#12923](https://github.com/emqx/emqx/pull/12923) builtin認証データベースに誤った形式をインポートした際、より具体的なエラーを返すようにしました。

- [#12940](https://github.com/emqx/emqx/pull/12940) `PUT /configs` APIに `ignore_readonly` 引数を追加しました。

  この変更前は、読み取り専用のルートキー（`cluster`、`rpc`、`node`）を含む生の設定を送信するとEMQXは400（BAD_REQUEST）を返していました。

  この拡張により、`PUT /configs?ignore_readonly=true` と呼び出すと、読み取り専用のルート設定キーを無視して残りを適用します。監視のため、読み取り専用キーが破棄された場合は情報レベルのログが記録されます。

  また、設定のHOCON構文エラーによる例外（500エラー）を修正し、構文エラー時は400（BAD_REQUEST）を返すようにしました。

- [#12957](https://github.com/emqx/emqx/pull/12957) macOS 14（Apple Silicon）およびUbuntu 24.04 Noble Numbat（LTS）向けパッケージのビルドを開始しました。

- [#12883](https://github.com/emqx/emqx/pull/12883) 永続ストレージ管理のためのREST APIエンドポイントとCLIコマンドを追加しました。

  新しいRESTエンドポイント：

  - `/ds/sites`
  - `/ds/sites/:site`
  - `/ds/storages`
  - `/ds/storages/:ds`
  - `/ds/storages/:ds/replicas`
  - `/ds/storages/:ds/replicas/:site`

  新しいCLIコマンド：

  - `ds set_replicas`
  - `ds join`
  - `ds leave`

### バグ修正

#### セキュリティ

- [#12887](https://github.com/emqx/emqx/pull/12887) sasl scramを用いたMQTT強化認証を修正しました。
- [#12962](https://github.com/emqx/emqx/pull/12962) TLSクライアントがワイルドカード証明書に対してサーバーホスト名を検証できるようになりました。例えば、`*.example.com` の証明書が発行されている場合、`srv1.example.com` のようなホスト名を検証可能です。

#### MQTT

- [#12996](https://github.com/emqx/emqx/pull/12996) `emqx_retainer`アプリケーションのプロセスリークを修正しました。以前は、保持メッセージ受信中にクライアントが切断されるとプロセスリークが発生していました。
- [#12855](https://github.com/emqx/emqx/pull/12855) クライアントが共有トピックにサブスクライブまたはサブスクライブ解除した際に、クライアントのサブスクリプション／解除通知用のシステムトピックメッセージが正しくシリアライズされない問題を修正しました。また、`/topics`エンドポイントの`$queue`共有トピックのフォーマットエラーも修正しました。
- [#12976](https://github.com/emqx/emqx/pull/12976) ソケット切断済みのセッションを引き継ぐ際に `client.disconnected` イベントが誤って発生する問題を修正しました。

#### データ処理と統合

- [#12653](https://github.com/emqx/emqx/pull/12653) ルールエンジン関数 `bin2hexstr` が8で割り切れないビットサイズのビットストリング入力をサポートするようになりました。こうしたビットストリングはルールエンジン関数 `subbits` から返されることがあります。

- [#12657](https://github.com/emqx/emqx/pull/12657) ルールエンジンのSQLベース言語で、配列リテラル内に定数や変数参照以外の式を配置できなかった問題を修正しました。これにより、任意の式を配列要素として使用可能になりました。

  例えば以下のような記述が可能です：

  ```bash
  select
  [21 + 21, abs(-abs(-2)), [1 + 1], 4] as my_array
  from "t/#"
  ```

<!-- This is a fix for not new feature in this release

- [#12707](https://github.com/emqx/emqx/pull/12707) データベースに永続クライアントセッションのIPとポートを保持するようにしました。
-->

- [#12932](https://github.com/emqx/emqx/pull/12932) HTTPアクションリクエストが503（Service Unavailable）を受け取った場合、失敗としてマークされ再試行されなかった問題を修正し、設定可能な回数だけ再試行するようにしました。

- [#12948](https://github.com/emqx/emqx/pull/12948) コネクター更新時に `Authorization` などの機微なHTTPヘッダー値が `******` に置換される問題を修正しました。

- [#12895](https://github.com/emqx/emqx/pull/12895) DynamoDBコネクターとアクションの一部設定キーが欠落していた問題を修正しました。

<!-- This is a fix for not new feature in this release

- [#12950](https://github.com/emqx/emqx/pull/12950) スキーマ検証設定時のトピック重複防止バリデーションを追加しました。
-->

- [#13018](https://github.com/emqx/emqx/pull/13018) Postgres/Timescale/Matrixコネクターで接続断時のログスパムを軽減しました。

- [#13118](https://github.com/emqx/emqx/pull/13118) ルールエンジンのテンプレートレンダリングにおけるパフォーマンス問題を修正しました。

- [#12880](https://github.com/emqx/emqx/pull/12880) InfluxDBアクション設定で、タグセット値にリテラルの整数や浮動小数点数が含まれるとシリアライズに失敗する問題を修正しました。タグセット値は正しく文字列として扱われます。タグセットの詳細は[Line Protocol - Tag Set](https://docs.influxdata.com/influxdb/v2/reference/syntax/line-protocol/#tag-set)をご参照ください。

#### 可観測性

- [#12765](https://github.com/emqx/emqx/pull/12765) 統計の `subscribers.count` と `subscribers.max` に共有サブスクライバーを含めるように修正しました。以前は非共有サブスクライバーのみがカウントされていました。

#### 運用と管理

- [#12812](https://github.com/emqx/emqx/pull/12812) リソースのヘルスチェックをノンブロッキング操作にしました。これにより、リソースの更新や削除が長時間のヘルスチェックでブロックされなくなります。

- [#12830](https://github.com/emqx/emqx/pull/12830) チャンネル（アクション/ソース）のヘルスチェックをノンブロッキング操作にしました。これにより、アクションやソースの更新・削除が長時間のヘルスチェックでブロックされなくなります。

<!-- This is a fix for not new feature in this release

- [#12874](https://github.com/emqx/emqx/pull/12874) セッション再接続前にサブスクリプションが変更された場合の永続メッセージ再生の整合性を保証：
  - 永続セッションは受信したQoS2メッセージのinflightパケットIDを保存。
  - 永続セッションと非永続セッション間の重複サブスクリプションに関する一貫した動作を保証。
  - REST APIで永続サブスクリプションを一覧表示。
  -->

- [#12993](https://github.com/emqx/emqx/pull/12993) 不明なゾーンを扱うリスナー設定更新APIを修正しました。

  修正前は、例えば `{"zone": "unknown"}` のような不明なゾーンでリスナー設定を更新すると変更が受理され、接続したすべてのクライアントがクラッシュしていました。
  修正後は、不明なゾーン名でリスナーを更新すると「Bad request」レスポンスが返されます。

- [#13012](https://github.com/emqx/emqx/pull/13012) MQTTリスナーの設定オプション `access_rules` を以下のように改善しました：

  * 無効なアクセスルール設定時に不明瞭なエラーメッセージでリスナーがクラッシュすることがなくなり、設定エラーとして処理されます。
  * 複数のルールをカンマ区切りの単一文字列で追加可能になりました（例："allow 10.0.1.0/24, deny all"）。

- [#13041](https://github.com/emqx/emqx/pull/13041) HTTP認証エラーログメッセージを改善しました。POSTメソッドでContent-Typeヘッダーが欠落している場合、スタックトレース付きの読みにくい例外ではなく、意味のあるエラーメッセージを出力します。

- [#13077](https://github.com/emqx/emqx/pull/13077) この修正により、EMQXはアクション設定をコネクター起動時または再起動時にのみグローバル設定から読み込み、コネクター内に最新の設定を保持するようになりました。以前はアクション設定の更新が無効化・有効化なしに反映されないことがあり、古い設定でアクションが動作する場合がありました。

- [#13090](https://github.com/emqx/emqx/pull/13090) コネクターが無効化されているアクションやソースの起動試行時に、コネクター自体を起動しようとしなくなりました。

- [#12871](https://github.com/emqx/emqx/pull/12871) 退避されたノードの起動処理を修正しました。以前は、退避停止せずに停止したノードが再起動しませんでした。

- [#12888](https://github.com/emqx/emqx/pull/12888) バックアップデータインポート後のライセンス関連設定消失問題を修正しました。

#### ゲートウェイ

- [#12902](https://github.com/emqx/emqx/pull/12902) MQTTメッセージのContent-typeをStompメッセージに渡すようにしました。

- [#12892](https://github.com/emqx/emqx/pull/12892) OCPPゲートウェイの下流BootNotification処理のエラーを修正しました。また、`gateways/ocpp/listeners`エンドポイントが現在の接続数を正しく返すよう修正しました。

- [#12909](https://github.com/emqx/emqx/pull/12909) UDPリスナーのエラーやクローズ時のプロセス処理を修正しました。これにより、UDPリスナーがエラー発生時に正常に停止・再起動されるようになりました。

- [#13001](https://github.com/emqx/emqx/pull/13001) syskeeperフォワーダーが接続喪失時に再接続しない問題を修正しました。

- [#13010](https://github.com/emqx/emqx/pull/13010) JT/T 808ゲートウェイが認証サービスからの登録認証失敗時にREGISTER_ACKメッセージに正しく応答できない問題を修正しました。


## 5.6.1

*リリース日: 2024-04-18*

### バグ修正

- [#12759](https://github.com/emqx/emqx/pull/12759) EMQXはスキーマ検証エラーでアップロードに失敗した無効なバックアップファイルを自動的に削除するようになりました。この修正により、有効な設定ファイルのみが表示・保存され、システムの信頼性が向上します。

- [#12766](https://github.com/emqx/emqx/pull/12766) エラー理由 `message_queue_too_long` を `mailbox_overflow` に名称変更しました。

  `mailbox_overflow` は対応する設定パラメータ `force_shutdown.max_mailbox_size` と整合しています。

- [#12773](https://github.com/emqx/emqx/pull/12773) HTTPクライアントライブラリをアップグレードしました。

  HTTPクライアントライブラリ（`gun-1.3`）は標準ポート（80番のhttp、443番のhttps）に対して `Host` ヘッダーに誤って `:portnumber` を付加していました。これにより、AWS LambdaやAlibaba Cloud HTTPゲートウェイなど、厳密な `Host` ヘッダーチェックを行うサーバーやゲートウェイで `InvalidCustomDomain.NotFound` や「指定されたCustomDomainが存在しません」などのエラーが発生していました。

- [#12802](https://github.com/emqx/emqx/pull/12802) `emqx ctl cluster leave` コマンドによるクラスターからのノード削除処理を改善しました。以前は、設定されたクラスターの `discovery_strategy` が `manual` でない場合、停止しない限りノードが誤って同じクラスターに再参加することがありました。今回の更新で、`cluster leave` コマンド実行時にノードのクラスター検出が自動的に無効化され、再参加を防止します。クラスター検出を再度有効化するには、`emqx ctl discovery enable` コマンドを使用するかノードを再起動してください。

- [#12814](https://github.com/emqx/emqx/pull/12814) EMQXの `/clients/{clientid}/mqueue_messages` および `/clients/{clientid}/inflight_messages` APIのエラーハンドリングを改善しました。対応内容：

  - **内部タイムアウト**：システム負荷が高い場合などにEMQXが5秒のデフォルトタイムアウト内にInflightまたはMqueueメッセージ一覧を取得できない場合、APIは500エラーで `{"code":"INTERNAL_ERROR","message":"timeout"}` を返し、トラブルシューティング用の詳細ログを記録します。
  - **クライアントシャットダウン**：API呼び出し中にクライアント接続が切断された場合、APIは404エラーで `{"code": "CLIENT_SHUTDOWN", "message": "Client connection has been shutdown"}` を返します。これにより、クライアント接続中断時のフィードバックが明確になります。

- [#12824](https://github.com/emqx/emqx/pull/12824) 統計メトリクス `subscribers.count` と `subscribers.max` に共有サブスクライバーを含めるように更新しました。以前は非共有サブスクライバーのみがカウントされていました。

- [#12826](https://github.com/emqx/emqx/pull/12826) EMQXのソースデータ統合と保持メッセージのインポート機能に関する問題を修正しました。修正前：

  - バックアップファイルに指定されたデータ統合のソースがインポートされていませんでした。これにはQoSやトピックなどの特定のコネクターとパラメータを含む `sources.mqtt` カテゴリの設定が含まれます。
  - 保持メッセージの `mnesia` テーブルのインポートがサポートされていませんでした。

- [#12843](https://github.com/emqx/emqx/pull/12843) `emqx ctl cluster leave` コマンド実行後のレプリケーターノードにおける `cluster_rpc_commit` トランザクションIDのクリーンアップ処理を修正しました。適切にクリアされないとコアノードでの設定更新が妨げられていました。

- [#12882](https://github.com/emqx/emqx/pull/12882) EMQXデータ統合のRocketMQアクションで、メッセージが正しく設定されたトピックにルーティングされるよう修正しました。以前は複数のアクションが単一のRocketMQコネクターを共有する場合、すべてのメッセージが最初のアクションのトピックに誤って送信されていました。今回の修正では、トピックごとに異なるRocketMQワーカーセットを起動し、トピック間のメッセージ混在を防止します。

- [#12885](https://github.com/emqx/emqx/pull/12885) ダッシュボードの「監視」メニューで「保持メッセージ」が表示されない問題を修正しました。

  「保持メッセージ」バックエンドAPIは `qlc` ライブラリを使用しています。この問題は、`qlc` の `file_sorter` 関数が書き込み不可のディレクトリ `/opt/emqx` を一時ファイル保存に使用しようとしたために発生していました。これはDocker展開でのディレクトリ所有権変更に起因します。

  本更新で `/opt/emqx` ディレクトリの所有権を `emqx:emqx` に変更し、保持メッセージ取得を含むすべての操作がアクセスエラーなく実行できるようにしました。

## 5.6.0

*リリース日: 2024-03-28*

### 機能強化

- [#12326](https://github.com/emqx/emqx/pull/12326) セッション追跡に登録履歴を追加しました。EMQXは期限切れのセッションも含めて登録履歴を監視可能になりました。`broker.session_history_retain` を設定することで、期限切れセッションの記録を指定期間保持します。

  - **セッション数API**：API `GET /api/v5/sessions_count?since=1705682238` を使用すると、指定したUNIXエポックタイムスタンプ（秒単位）以降にクラスター内でアクティブだったセッション数を取得できます。これにより、時間経過に伴うセッション活動の分析が可能です。

  - **クラスターセッションゲージによるメトリクス拡張**：クラスター内のセッション数を追跡する新しいゲージメトリクス `cluster_sessions` を追加しました。このメトリクスはPrometheusにも統合されています：

    ```
    # TYPE emqx_cluster_sessions_count gauge
    emqx_cluster_sessions_count 1234
    ```

    注意：このメトリクスは概算値として扱ってください。データ収集と計算の非同期性により、正確な値とは異なる場合があります。

- [#12398](https://github.com/emqx/emqx/pull/12398) ダッシュボード設定に `swagger_support` オプションを公開し、Swagger APIドキュメントの有効化・無効化を可能にしました。

- [#12467](https://github.com/emqx/emqx/pull/12467) クラスター検出でAAAA DNSレコードタイプのサポートを開始しました。

- [#12483](https://github.com/emqx/emqx/pull/12483) `emqx ctl conf cluster_sync tnxid ID` を `emqx ctl conf cluster_sync inspect ID` に名称変更しました。

  後方互換性のため `tnxid` は残されていますが非推奨であり、5.7で削除予定です。

- [#12495](https://github.com/emqx/emqx/pull/12495) 新しいAWS S3コネクターとアクションを導入しました。

- [#12499](https://github.com/emqx/emqx/pull/12499) クライアント禁止機能を拡張し、以下のルールを追加しました：

  * `clientid` を正規表現でマッチング
  * クライアントの `username` を正規表現でマッチング
  * クライアントのピアアドレスをCIDR範囲でマッチング

  **重要注意**：多数の広範囲マッチングルール（特定のclientid、username、ホストに限定しない）を実装するとシステムパフォーマンスに影響を与える可能性があります。最適なシステム効率を維持するため、これらの拡張禁止ルールは慎重に使用してください。

- [#12509](https://github.com/emqx/emqx/pull/12509) すべての認証者／認可ソースの順序を再設定するAPIを実装しました。

- [#12517](https://github.com/emqx/emqx/pull/12517) 設定ファイルで複数行文字列値をサポートし、インデントを保持して可読性と保守性を向上させました。`"""~` と `~"""` マーカーを使い、インデントされた行を引用する構文を導入しました。例：

  ```
  rule_xlu4 {
    sql = """~
      SELECT
        *
      FROM
        "t/#"
    ~"""
  }
  ```
  詳細は[HOCON 0.42.0](https://github.com/emqx/hocon/releases/tag/0.42.0)のリリースノートをご参照ください。

- [#12520](https://github.com/emqx/emqx/pull/12520) ログスロットリングを実装しました。この機能は、設定された時間内に同じイベントが複数回発生した場合、最初の1回のみをログに記録し、それ以外をドロップしてログの洪水を防ぎます。

  ログスロットリングは以下の重要かつ繰り返し発生しやすいログイベントに適用されます：

  - `authentication_failure`
  - `authorization_permission_denied`
  - `cannot_publish_to_topic_due_to_not_authorized`
  - `cannot_publish_to_topic_due_to_quota_exceeded`
  - `connection_rejected_due_to_license_limit_reached`
  - `dropped_msg_due_to_mqueue_is_full`

- [#12561](https://github.com/emqx/emqx/pull/12561) クライアントのインフライトメッセージおよびメッセージキュー（mqueue）メッセージの一覧を取得するHTTP APIを実装しました。これにより、メッセージキューとインフライトメッセージの詳細な把握と効果的な制御が可能になります。

  最初のチャンクを取得する例：
  - `GET /clients/{clientid}/mqueue_messages?limit=100`
  - `GET /clients/{clientid}/inflight_messages?limit=100`

  位置指定なしで最初のチャンクを取得する例：
  - `GET /clients/{clientid}/mqueue_messages?limit=100&position=none`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position=none`

  次のチャンクを取得する例：
  - `GET /clients/{clientid}/mqueue_messages?limit=100&position={position}`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position={position}`

  ここで `{position}` は前回レスポンスの `meta.position` フィールドの値（不透明な文字列トークン）です。

  順序と優先度：

  - **Mqueueメッセージ**：キュー順（FIFO）で優先度の高い順に並びます。デフォルトではすべてのmqueueメッセージの優先度は0です。
  - **インフライトメッセージ**：インフライトストレージに挿入されたタイムスタンプ順（古い順から新しい順）に並びます。

- [#12590](https://github.com/emqx/emqx/pull/12590) ログメッセージから `mfa` メタデータを削除し、明瞭性を向上させました。

- [#12641](https://github.com/emqx/emqx/pull/12641) テキストログフォーマッターのフィールド順序を改善しました。新しい順序は以下の通りです：

  `tag` > `clientid` > `msg` > `peername` > `username` > `topic` > [その他のフィールド]

- [#12670](https://github.com/emqx/emqx/pull/12670) エンドポイント `/monitor_current` と `/monitor_current/nodes/:node` に `shared_subscriptions` フィールドを追加しました。

- [#12679](https://github.com/emqx/emqx/pull/12679) DockerイメージのベースをDebian 11からDebian 12にアップグレードしました。

- [#12700](https://github.com/emqx/emqx/pull/12700) バイトサイズのHOCONフィールドで "b" と "B" 単位のサポートを開始しました。

  例えば、以下の3つのフィールドはすべて1024バイトとして扱われます：

  ```
  bytesize_field = "1024b"
  bytesize_field2 = "1024B"
  bytesize_field2 = 1024
  ```

- [#12719](https://github.com/emqx/emqx/pull/12719) `/clients` APIが複数の `clientid` と `username` を同時にクエリ可能になり、より柔軟で強力なクライアント接続監視が可能になりました。さらに、APIレスポンスに含めるクライアント情報フィールドをカスタマイズ可能になりました。

  複数クライアント/ユーザー名クエリの例：

  - 複数クライアントIDを指定：`/clients?clientid=client1&clientid=client2`
  - 複数ユーザー名を指定：`/clients?username=user11&username=user2`
  - 複数クライアントIDとユーザー名を組み合わせ：`/clients?clientid=client1&clientid=client2&username=user1&username=user2`

  レスポンスに含めるフィールド選択の例：

  - すべてのフィールドを含める：`/clients?fields=all`（`fields` パラメータ省略時もすべてのフィールドが返されます）
  - 特定のフィールドのみ指定：`/clients?fields=clientid,username`

- [#12330](https://github.com/emqx/emqx/pull/12330) Cassandraブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12353](https://github.com/emqx/emqx/pull/12353) OpenTSDBブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12376](https://github.com/emqx/emqx/pull/12376) Kinesisブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12386](https://github.com/emqx/emqx/pull/12386) GreptimeDBブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12423](https://github.com/emqx/emqx/pull/12423) RabbitMQブリッジをコネクター、アクション、ソースコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12425](https://github.com/emqx/emqx/pull/12425) ClickHouseブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12439](https://github.com/emqx/emqx/pull/12439) Oracleブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12449](https://github.com/emqx/emqx/pull/12449) TDEngineブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12488](https://github.com/emqx/emqx/pull/12488) RocketMQブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12512](https://github.com/emqx/emqx/pull/12512) HStreamDBブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされますが、新しいフィールドが追加されているため手動アップグレードを推奨します。

- [#12543](https://github.com/emqx/emqx/pull/12543) DynamoDBブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12595](https://github.com/emqx/emqx/pull/12595) Kafka Consumerブリッジをコネクターとソースコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12619](https://github.com/emqx/emqx/pull/12619) Microsoft SQL Serverブリッジをコネクターとアクションコンポーネントに分割しました。ブリッジのHTTP APIとの互換性は維持されており、設定は自動的にアップグレードされます。

- [#12381](https://github.com/emqx/emqx/pull/12381) 新しいSQL関数を追加しました：`map_keys()`, `map_values()`, `map_to_entries()`, `join_to_string()`, `join_to_sql_values_string()`, `is_null_var()`, `is_not_null_var()`。

  関数の詳細と使用方法は[組み込みSQL関数](../develop/data-integration/rule-sql-builtin-functions.md)ドキュメントをご参照ください。

- [#12427](https://github.com/emqx/emqx/pull/12427) Kafkaデータ統合で使用可能なKafkaパーティション数の上限を指定する機能を導入しました。

- [#12577](https://github.com/emqx/emqx/pull/12577) GCP PubSub ProducerおよびConsumerコネクターの `service_account_json` フィールドをJSONエンコードされた文字列として受け入れるように更新しました。以前のHOCONマップ形式も引き続きサポートされていますが推奨されません。

- [#12581](https://github.com/emqx/emqx/pull/12581) スキーマレジストリにJSONスキーマを追加しました。

  JSON Schemaは[Draft 03](http://tools.ietf.org/html/draft-zyp-json-schema-03)、[Draft 04](http://tools.ietf.org/html/draft-zyp-json-schema-04)、[Draft 05](https://datatracker.ietf.org/doc/html/draft-wright-json-schema-00)をサポートしています。

- [#12602](https://github.com/emqx/emqx/pull/12602) IoTDBコネクターのヘルスチェックを強化し、単なるソケット接続確認ではなく `ping` APIを使用するようにしました。

- [#12336](https://github.com/emqx/emqx/pull/12336) 非同期タスク管理を改善し、チャネルのクリーンアップを専用プールに分離しました。これにより、高レイテンシ環境でのチャネルクリーンアップが他の非同期操作（ルートクリーンアップなど）を妨げる問題を解決しました。

- [#12725](https://github.com/emqx/emqx/pull/12725) 利用可能なソースタイプを一覧表示するREST APIを実装しました。

- [#12746](https://github.com/emqx/emqx/pull/12746) `username` ログフィールドを追加しました。MQTTクライアントが空でないユーザー名で接続している場合、ログやトレースに `username` フィールドが含まれます。

- [#12785](https://github.com/emqx/emqx/pull/12785) ログハンドラーに `timestamp_format` 設定オプションを追加しました。以下の設定が可能です：

  - `auto`：使用中のログフォーマッターに基づき自動判別。テキストフォーマッターは `rfc3339`、JSONフォーマッターは `epoch` を使用。
  - `epoch`：マイクロ秒精度のUnixエポック形式。
  - `rfc3339`：RFC3339準拠の日時文字列（例：`2024-03-26T11:52:19.777087+00:00`）。

- [#12417](https://github.com/emqx/emqx/pull/12417) MQTTメッセージの有効期限を設定ファイルで指定可能にしました。詳細は `mqtt.conf.example` の `message_expiry_interval` 設定をご参照ください。

### バグ修正

- [#11868](https://github.com/emqx/emqx/pull/11868) セッション引き継ぎ後に遺言メッセージがパブリッシュされないバグを修正しました。

- [#12347](https://github.com/emqx/emqx/pull/12347) MQTTエグレスデータブリッジのルールSQLで処理されるメッセージが、データが不完全またはブリッジ設定で定義されたプレースホルダーが欠落している場合でも常に有効な形式でレンダリングされるように修正しました。これにより、以前は不正と判定され破棄されていたメッセージが正しく処理されます。

  `payload` と `topic` テンプレート内の変数が未定義の場合、リテラルの `undefined` 文字列ではなく空文字列としてレンダリングされます。

- [#12472](https://github.com/emqx/emqx/pull/12472) ローリングアップグレード中に `/api/v5/actions/` および `/api/v5/sources/` エンドポイントの一部読み取り操作で500エラーが発生する問題を修正しました。

- [#12492](https://github.com/emqx/emqx/pull/12492) MQTT v5クライアント向けに `CONNACK` メッセージで `Receive-Maximum` プロパティを返すようにしました。これはクライアントの `Receive-Maximum` 設定とサーバーの `max_inflight` 設定の最小値を考慮したものです。以前は決定値が `CONNACK` に返されていませんでした。

  注意：これらのAPI応答強化に伴い、切断済みセッションを含むためクライアント総数が実際より多く報告される既知の問題があります。

- [#12505](https://github.com/emqx/emqx/pull/12505) Kafkaプロデューサークライアント `wolff` をバージョン1.10.1から1.10.2にアップグレードしました。この新バージョンは各コネクターごとに長期間維持されるメタデータ接続を持ち、アクションおよびコネクターのヘルスチェックにおける接続確立頻度を低減しEMQXのパフォーマンスを最適化します。

- [#12513](https://github.com/emqx/emqx/pull/12513) 複数の洪水ログイベントのログレベルを `warning` から `info` に変更しました。

- [#12530](https://github.com/emqx/emqx/pull/12530) `frame_too_large` イベントや不正な `CONNECT` パケット解析失敗時のエラー報告を改善し、トラブルシューティングに役立つ追加情報を提供するようにしました。

- [#12541](https://github.com/emqx/emqx/pull/12541) DNSレコードによるautocluster設定の検証ステップを追加しました。`dns` 戦略で `a` または `aaaa` レコードタイプを使用する場合、すべてのノードはホスト名に（静的な）IPアドレスを使用する必要があります。

- [#12566](https://github.com/emqx/emqx/pull/12566) REST APIキーのブートストラップファイルを改善しました：

  - ファイル内の空行をスキップし、以前のエラー発生を防止。
  - ブートストラップファイルで指定されたAPIキーは最優先され、既存のキーと競合する場合は古いキーが自動削除されます。

- [#12646](https://github.com/emqx/emqx/pull/12646) ルールエンジンの日時文字列パーサーの問題を修正しました。以前は秒単位の精度で指定された日時文字列のみタイムゾーン補正が有効でした。

- [#12652](https://github.com/emqx/emqx/pull/12652) ドキュメントにはあるが実装に存在しなかったパラメータ4および5の `subbits` 関数を追加しました。

- [#12663](https://github.com/emqx/emqx/pull/12663) Prometheusエンドポイント `/prometheus/stats` で取得可能な `emqx_vm_cpu_use` と `emqx_vm_cpu_idle` メトリクスが、OS起動以降の平均CPU使用率を誤って示していた問題を修正し、現在のCPU使用率とアイドル時間を正確に反映するようにしました。

- [#12668](https://github.com/emqx/emqx/pull/12668) SQL関数 `date_to_unix_ts()` を `calendar:datetime_to_gregorian_seconds/1` を用いてリファクタリングし、入力日時形式のバリデーションを追加しました。

- [#12672](https://github.com/emqx/emqx/pull/12672) ノードの起動時ブート設定生成処理を修正し、`{data_dir}/configs/cluster.hocon` を読み込むようにしました。以前はダッシュボードでのログ設定変更が `{data_dir}/configs/cluster.hocon` に保存されても、初期ブート設定生成時に `etc/emqx.conf` のみが読み込まれ、ログセグメントファイルの一部が失われることがありました。

  現在は `{data_dir}/configs/cluster.hocon` と `etc/emqx.conf` を同時に読み込み、`emqx.conf` の設定が優先されてブート設定を生成します。

- [#12696](https://github.com/emqx/emqx/pull/12696) アクションやソースの再接続試行時に誤ったエラーメッセージがHTTP APIで返される問題を修正しました。

- [#12714](https://github.com/emqx/emqx/pull/12714) Prometheus API `/prometheus/stats` エンドポイントで報告される複数のメトリクスの不正確さを修正しました。対象メトリクス：

  - `emqx_cluster_sessions_count`
  - `emqx_cluster_sessions_max`
  - `emqx_cluster_nodes_running`
  - `emqx_cluster_nodes_stopped`
  - `emqx_subscriptions_shared_count`
  - `emqx_subscriptions_shared_max`

  さらに、`/stats` エンドポイントの `subscriptions.shared.count` と `subscriptions.shared.max` フィールドが、共有サブスクリプションのクライアント切断や解除後に即時更新されない問題も修正しました。

- [#12390](https://github.com/emqx/emqx/pull/12390) クラスター参加処理中に `/license` APIリクエストがクラッシュする問題を修正しました。

- [#12411](https://github.com/emqx/emqx/pull/12411) Cassandraデータ統合で `int` カラムに `null` 値が `1853189228` として挿入されるバグを修正しました。

- [#12522](https://github.com/emqx/emqx/pull/12522) Kafkaブートストラップホストの解析処理を改善し、カンマの後の空白を除去するようにして、接続タイムアウトやDNS解決失敗を防止しました。

- [#12656](https://github.com/emqx/emqx/pull/12656) GCP PubSub Producerアクション作成時にトピック検証を追加し、トピックが存在しないか資格情報の権限不足の場合に失敗通知を行うようにしました。

- [#12678](https://github.com/emqx/emqx/pull/12678) DynamoDBコネクターの接続失敗理由を明確に報告するように改善しました。

- [#12681](https://github.com/emqx/emqx/pull/12681) RocketMQブリッジ／アクションへのメッセージ送信時に秘密情報がデバッグレベルのログに記録されるセキュリティ問題を修正しました。

- [#12715](https://github.com/emqx/emqx/pull/12715) イングレスデータ統合ソースのコネクターにアクティブなチャネルがある場合、設定更新時にクラッシュする問題を修正しました。

- [#12767](https://github.com/emqx/emqx/pull/12767) 5.0.1から5.5.1へのアップグレード時にKafka Producer設定が原因で失敗する問題を修正しました。Kafka Producer設定をEMQX 5.5.1以降の新しいアクションおよびコネクター設定形式に正しく変換します。

- [#12768](https://github.com/emqx/emqx/pull/12768) EMQX 5.4.0以降で、特に5.4.0未満からのローリングアップグレード時に起動失敗する問題を修正しました。問題はv1およびv2のルーティングテーブルが空の場合のルーティングスキーマ初期化に関連します。

  ノードは起動時にローカルのルーティングテーブルが空の場合、デフォルトでv2ルーティングテーブルを使用する代わりに、クラスター全体で使用されているルーティングスキーマのバージョンを取得しようとします。これにより、混在バージョンのクラスターでルーティングストレージスキーマの分岐が減少します。

  もし実行中のクラスターで競合が検出された場合、EMQXはログにクリティカルレベルのエラーメッセージと手動解決方法を出力します。同じメッセージは標準エラーにも出力され、ログハンドラー未設定時でも見逃されません。

- [#12786](https://github.com/emqx/emqx/pull/12786) レプリカントノードが異なるEMQXアプリケーションバージョンのコアノードに接続することを防ぐ厳格なチェックを追加しました。これによりローリングアップグレード中、少なくとも1つのコアノードが同じEMQXリリースバージョンを実行している場合にのみレプリカントノードが動作可能になります。

## 5.5.1

*リリース日: 2024-03-06*

### 機能強化

- [#12497](https://github.com/emqx/emqx/pull/12497) MongoDBコネクターのパフォーマンスを改善し、データベース操作の効率を向上させました。この改善はMongoDB Erlangドライバーの改良にも支えられています（[mongodb-erlang PR](https://github.com/emqx/mongodb-erlang/pull/41)）。

### バグ修正

- [#12471](https://github.com/emqx/emqx/pull/12471) EMQX 5.0.2からのアップグレード時にデータ統合設定が正しく読み込まれない問題を修正しました。

- [#12598](https://github.com/emqx/emqx/pull/12598) HTTP API経由で共有トピックフィルターのサブスクライブおよびサブスクライブ解除ができない問題を修正しました。

  対象API：

  - `/clients/:clientid/subscribe`
  - `/clients/:clientid/subscribe/bulk`
  - `/clients/:clientid/unsubscribe`
  - `/clients/:clientid/unsubscribe/bulk`

- [#12601](https://github.com/emqx/emqx/pull/12601) LDAPドライバーのログが記録されない問題を修正しました。現在はすべてのログが `info` レベルで記録されます。

- [#12606](https://github.com/emqx/emqx/pull/12606) 指定されたSSL証明書ファイルが存在しない場合にPrometheus APIがクラッシュする問題を修正しました。証明書ファイルが存在しない場合、`emqx_cert_expiry_at` メトリクスは0を返し、証明書が存在しないことを示します。

- [#12608](https://github.com/emqx/emqx/pull/12608) IoTDBアクションでクエリデータに `payload` フィールドがない場合に発生する `function_clause` エラーを修正しました。

- [#12610](https://github.com/emqx/emqx/pull/12610) LDAPコネクターへの接続が一定時間後に予期せず切断される問題を修正しました。

- [#12620](https://github.com/emqx/emqx/pull/12620) HTTPサーバーコネクターの `debug` レベルログから認証・認可資格情報を含む機微なHTTPヘッダー情報をマスキングし、セキュリティリスクを軽減しました。

- [#12632](https://github.com/emqx/emqx/pull/12632) ルールエンジンのSQL組み込み関数 `date_to_unix_ts` がうるう年の3月1日以降の日付で誤った結果を返す問題を修正しました。

## 5.5.0

*リリース日: 2024-02-01*

### 機能強化

- [#12085](https://github.com/emqx/emqx/pull/12085) EMQXをOTPバージョン26.1.2-2の機能を活用するようアップグレードしました。注意：Dockerイメージは引き続きOTP 25.3.2でビルドされています。

- [#12189](https://github.com/emqx/emqx/pull/12189) EMQX JWT認証のACLクレーム形式を拡張し、より柔軟にしました。新しい形式は配列構造をサポートし、ファイルベースのACLルールにより近づいています。

  例：

  ```json
  [
  {
    "permission": "allow",
    "action": "pub",
    "topic": "${username}/#",
    "qos": [0, 1],
    "retain": true
  },
  {
    "permission": "allow",
    "action": "sub",
    "topic": "eq ${username}/#",
    "qos": [0, 1]
  },
  {
    "permission": "deny",
    "action": "all",
    "topics": ["#"]
  }
  ]
  ```

  新形式では、マッチするルールがない場合に自動的に拒否されることはなく、認可チェーンは他の設定された認可者に進みます。チェーン全体でマッチしない場合は、最終的に `authorization.no_match` のデフォルト許可設定に従います。

- [#12267](https://github.com/emqx/emqx/pull/12267) `cluster/:node/invite` インターフェースに新たに `timeout` パラメータを追加し、デフォルトタイムアウト問題に対応しました。以前の5秒のデフォルトタイムアウトは、EMQXクラスター参加に通常より長い時間がかかるためHTTP API呼び出しのタイムアウトを引き起こしていました。

さらに、EMQXはクラスタにノードを非同期で招待する方法をサポートする新しいAPI `/cluster/:node/invite_async` を追加し、参加状況を確認するための新しい `cluster/invitation` APIを導入しました。

- [#12272](https://github.com/emqx/emqx/pull/12272) EMQXの`retain` APIに以下の更新を導入しました：

  - すべての保持メッセージを削除する新しいAPI `DELETE /retainer/messages` を追加。
  - API `GET /retainer/messages` のクエリ文字列にオプションのトピックフィルター `topic` パラメータを追加。例えば、クエリ文字列 `topic=t/1` を使用すると特定のトピックの保持メッセージをフィルタリングでき、メッセージ取得の効率が向上します。

- [#12277](https://github.com/emqx/emqx/pull/12277) トピック名で遅延メッセージを削除するための `mqtt/delayed/messages/:topic` APIを追加。

- [#12278](https://github.com/emqx/emqx/pull/12278) REST APIのページネーションAPIの最大ページサイズを`3000`から`10000`に調整。

- [#12289](https://github.com/emqx/emqx/pull/12289) 認可キャッシュが特定のトピックを除外できるようになりました。指定されたトピックおよびトピックフィルターのリストに対して、EMQXは認可キャッシュを生成しません。このリストは設定項目 `authorization.cache.excludes` またはダッシュボードから設定可能です。これらの特定トピックに対しては、常にリアルタイムで権限チェックが行われ、以前のキャッシュ結果に依存しないため、認可結果の鮮度が保証されます。

- [#12329](https://github.com/emqx/emqx/pull/12329) `broker.routing.batch_sync` 設定項目を追加し、グローバルルーティングテーブルへのサブスクリプション同期をバッチ処理で行う専用プロセスプールを有効化。これにより、ネットワークレイテンシによる遅延が起きやすいクロスノード通信の頻度が削減されます。複数のサブスクリプション更新をまとめて処理することで、クラスター内のレプリカノードとコアノード間の同期が高速化され、ブローカープールの負荷も軽減され、過負荷リスクが最小化されます。

- [#12333](https://github.com/emqx/emqx/pull/12333) アクションおよびコネクターに `tags` フィールドを追加。`description` フィールド（自由記述の注釈）と同様に、`tags` はフィルタリングやグルーピングのための注釈として利用可能です。

- [#12072](https://github.com/emqx/emqx/pull/12072) GreptimeDBのデータ統合で非同期データ書き込みをサポートし、パフォーマンスを向上。

- [#12194](https://github.com/emqx/emqx/pull/12194) Kafkaパブリッシャーのパフォーマンスを改善。

- [#12247](https://github.com/emqx/emqx/pull/12247) InfluxDB用ブリッジを分割し、コネクターおよびアクションAPIで利用可能に。旧ブリッジAPIとの互換性は維持。

- [#12299](https://github.com/emqx/emqx/pull/12299) 監視性向上のため、より多くのメトリクスを公開：

  Monitor API:
  - `/api/v5/monitor_current` に `retained_msg_count` フィールドを追加。
  - `/api/v5/monitor_current` に `license_quota` フィールドを追加。
  - `/api/v5/monitor_current/nodes/{node}` に `retained_msg_count` と `node_uptime` フィールドを追加。
  - `/api/v5/monitor_current/nodes/{node}` に `retained_msg_count`、`license_quota`、`node_uptime` フィールドを追加。

  Prometheus API:
  - `/api/v5/prometheus/stats` に `emqx_cert_expiry_at` と `emqx_license_expiry_at` を追加し、TLSリスナー証明書の有効期限およびライセンスの有効期限を表示。
  - `/api/v5/prometheus/auth` エンドポイントを追加し、すべての認証者および認可者の実行回数や稼働状況などのメトリクスを提供。
  - `/api/v5/prometheus/data_integration` エンドポイントを追加し、すべてのルール、アクション、コネクターの実行回数や状態などのメトリクスを提供。

  制限事項:
  Prometheusプッシュゲートウェイは `/api/v5/prometheus/stats?mode=node` の内容のみサポート。

  詳細なAPI情報およびメトリクスタイプについてはswagger APIドキュメントを参照してください。

- [#12196](https://github.com/emqx/emqx/pull/12196) ルートクリーンアップ時のネットワーク効率を改善。

  以前はノードがダウンすると、そのノードへの各ルート削除操作が他のすべての生存ノード間で交換されていました。この変更により、生存ノード間で1回の「マッチ＆削除」操作のみが交換され、必要なネットワークパケット数が大幅に削減され、クラスター間ネットワークの負荷が軽減されます。
  この最適化は、特にネットワークレイテンシが高くなりがちな地理的に分散したEMQXデプロイメントで有効です。

- [#12354](https://github.com/emqx/emqx/pull/12354) データ統合の同時作成および更新をサポートし、バックアップファイルのインポート時など操作速度を大幅に向上。

- [#12396](https://github.com/emqx/emqx/pull/12396) `authentication/:id/import_users` インターフェースのユーザーインポート機能を強化：

  - プレーンテキストパスワードのユーザーを簡単にインポートできる新パラメータ `?type=plain` を追加。既存のハッシュ化パスワード対応を補完。
  - `content-type: application/json` のサポートを強化し、HTTPボディでJSON形式の送信が可能に。これにより、従来のCSVファイル用 `multipart/form-data` のみ対応から拡張。

- [#11902](https://github.com/emqx/emqx/pull/11902) 一方向のNari SysKeeper 2000ネットワーク分離ゲートウェイを介したMQTTメッセージブリッジ機能を強化。

- [#12388](https://github.com/emqx/emqx/pull/12388) QUICリスナーで全体の接続数ではなく、リスナーごとの接続数を表示するように変更。

- [#12325](https://github.com/emqx/emqx/pull/12325) QUICリスナーが既存接続を切断せずにリスナーのバインディングをリロード可能に。

- [#12274](https://github.com/emqx/emqx/pull/12274) QUIC MQTTリスナーのTLS設定を既存接続を切断せずに動的に更新可能に。更新失敗時には前のTLS設定にロールバックするフェイルセーフ機構を実装。

- [#12348](https://github.com/emqx/emqx/pull/12348) Elasticsearchとのデータ統合をサポート。

### バグ修正

- [#12232](https://github.com/emqx/emqx/pull/12232) ノードが強制的にクラスタから離脱した後にクラスタコミットログテーブルが削除されない問題を修正。
- [#12243](https://github.com/emqx/emqx/pull/12243) グローバルルーティング状態の不整合を引き起こす微妙なレースコンディション群を修正。
- [#12269](https://github.com/emqx/emqx/pull/12269) `/clients` インターフェースのエラーハンドリングを改善。クエリ文字列検証失敗時に汎用の500ではなく、詳細なエラーメッセージ付き400ステータスを返すように。
- [#12285](https://github.com/emqx/emqx/pull/12285) CoAPゲートウェイでパケットサイズ削減のため短縮パラメータ名をサポート。例：`clientid=bar` を `c=bar` と表記可能に。
- [#12303](https://github.com/emqx/emqx/pull/12303) 保持メッセージのインデックス付けを修正。ワイルドカードサブスクライブのクライアントが関連しない保持メッセージを受信する問題を解消。
- [#12305](https://github.com/emqx/emqx/pull/12305) `emqx_cm` に不完全なクライアント／接続情報が渡され、内部不整合やメモリ使用量・ノード退避処理に影響を与える問題を修正。
- [#12306](https://github.com/emqx/emqx/pull/12306) HTTP API経由でパスワードパラメータを更新後にコネクターの接続テストが正常に動作しない問題を修正。
- [#12359](https://github.com/emqx/emqx/pull/12359) 特定のタイプのデータブリッジを設定したノード再起動時にエラーメッセージが発生し、ブリッジが失敗状態に陥る問題を修正。手動再起動が必要だった問題を解消。
- [#12404](https://github.com/emqx/emqx/pull/12404) 大量メッセージ流量のデータ統合再起動時にデータ統合メトリクスの収集が停止する問題を修正。
- [#12282](https://github.com/emqx/emqx/pull/12282) MySQLブリッジ作成失敗時のHTTP APIエラー応答を改善。未定義カラムを含むMySQL Sinkの削除問題も解決。
- [#12291](https://github.com/emqx/emqx/pull/12291) 機密パラメータを含む設定更新時にクラスタ設定ファイルに不要な `"******"` 文字列が残る問題を修正。
- [#12301](https://github.com/emqx/emqx/pull/12301) InfluxDBのラインプロトコルで数値リテラルが文字列型として保存される問題を修正。
- [#12317](https://github.com/emqx/emqx/pull/12317) MongoDBアクションスキーマから未対応の `resource_opts.batch_size` フィールドを削除。

## 5.4.1

*リリース日: 2024-01-09*

### 機能強化

- [#12261](https://github.com/emqx/emqx/pull/12261) IoTDB用ブリッジを分割し、コネクターおよびアクションAPIで利用可能に。旧ブリッジAPIとの互換性は維持。

### バグ修正

- [#12234](https://github.com/emqx/emqx/pull/12234) EMQX 5.4.0以前のバージョンで定義された `emqx.conf` のOpen Telemetry設定との互換性問題を解消し、旧設定の最新EMQXリリースへの統合を円滑化。
- [#12236](https://github.com/emqx/emqx/pull/12236) MQTTブローカーのデータ統合におけるクライアントID生成をMQTT 3.1仕様の23バイト制限に準拠させる修正。クライアントIDはユーザー指定のコネクター名を接頭辞に、ノード名のSHAハッシュの最初の8バイトとプールメンバーIDを続ける形式に。23バイトを超える場合は追加のSHAハッシュと23文字への切り詰めを適用。
- [#12238](https://github.com/emqx/emqx/pull/12238) EMQX 5.3.2のHTTPアクション機能で導入されたエラーフォーマット設定との互換性問題を解消。
- [#12240](https://github.com/emqx/emqx/pull/12240) `/file_transfer` APIの返却値を元の生データ形式に変更。時間単位（例："1h"）の秒数変換を防ぎ、呼び出し元が初期設定値を受け取れるように。これは他の取得APIとの整合性を保つため。
- [#12241](https://github.com/emqx/emqx/pull/12241) S3 APIとの追加HTTPヘッダー設定がS3ストレージバックエンドのファイル転送を妨げるバグを修正。
- [#12246](https://github.com/emqx/emqx/pull/12246) Dockerでポート11883のデフォルト公開を停止し、Helmチャートからも削除。このポートはもはや使用されていません。
- [#12249](https://github.com/emqx/emqx/pull/12249) `/configs` APIで読み取り専用設定値の変更試行時にレスポンスメッセージが文字化けする問題を修正。
- [#12250](https://github.com/emqx/emqx/pull/12250) `file_transfer` 設定の `secret_access_key` が誤ってマスク文字（`*****`）に更新される問題を修正し、元のキー値を保持。
- [#12256](https://github.com/emqx/emqx/pull/12256) パスワードなしでMySQLリソースに接続できない問題を修正。
- [#12264](https://github.com/emqx/emqx/pull/12264) バージョン5.4のレプリカノードが、5.4未満のコアノードが稼働するクラスタにローリングアップグレード中に参加できない問題を修正。

## 5.4.0

*リリース日: 2023-12-23*

### 機能強化

- [#12114](https://github.com/emqx/emqx/pull/12114) ClientInfoに `peerport` フィールドを追加。ExHookのメッセージ `ClientInfo` と `ConnInfo` にも `peerport` フィールドを追加。
- [#11884](https://github.com/emqx/emqx/pull/11884) Prometheus APIと設定を改良：

  - 関連設定をグループ化し、可読性と保守性を向上。
  - スクレイプAPIエンドポイントにベーシック認証を導入する `enable_basic_auth` 設定を追加し、セキュリティ強化。
  - 既存の後方互換性を維持しつつコードをリファクタリング。

- [#11896](https://github.com/emqx/emqx/pull/11896) ブリッジのパスワード、トークン、シークレットキーなど機密認証フィールドの設定を強化。ファイルシステムに保存されたシークレットを `file://` プレフィックスで安全に参照可能にし、機密データの取り扱いを向上。
- [#11921](https://github.com/emqx/emqx/pull/11921) Open Telemetryログハンドラーを導入し、Open Telemetryログデータモデルに準拠したログイベントのフォーマットを実現。フォーマット済みログイベントを設定済みのOpen Telemetryコレクターやバックエンドにエクスポート可能にし、ログ管理と統合機能を強化。
- [#11935](https://github.com/emqx/emqx/pull/11935) デフォルトで新しい `v2` ルーティングストアスキーマに切り替え。新スキーマはサブスクリプションとルーティング性能を向上、特に共通ワイルドカードプレフィックスを持つトピックフィルターの同時サブスクライブ時に効果的。ただしメモリ使用量が若干増加。別インデックス不要で、以前のバージョンで稀に発生したルーティング状態の不整合はほぼ解消。

  クラスターを旧バージョンからローリングアップグレードした場合、完全再起動まで `v1` ストアを継続使用。

  ユーザーは `broker.routing.storage_schema` を `v1` に設定して旧スキーマを選択可能。ただし完全再起動が必要。

- [#11984](https://github.com/emqx/emqx/pull/11984) Open Telemetry分散トレーシング機能を実装。
- [#12017](https://github.com/emqx/emqx/pull/12017) 設定およびユーザーデータのインポート・エクスポート専用REST APIを実装。
- [#12040](https://github.com/emqx/emqx/pull/12040) QUICプロトコルスタックをアップグレード。
- [#12201](https://github.com/emqx/emqx/pull/12201) TCP/SSL/WS/WSS MQTTリスナー設定のホットアップデートをサポート。リスナー再起動やクライアント切断なしで多くの設定変更が可能。ただし以下の制限あり：

  - TCP/SSLリスナーでは以下の変更にリスナー再起動とクライアント再接続が必要：

    - `bind`
    - `tcp_options.backlog`

  - WS/WSSリスナーでは以下のトランスポート関連設定変更時にリスニングソケットは再オープンされるが、既存接続は維持：

    - `bind`
    - `tcp_options.*`
    - `ssl_options.*`

- [#11608](https://github.com/emqx/emqx/pull/11608) LDAPバインド操作を新しい認証者として統合し、より柔軟かつ安全なユーザー認証を提供。
- [#11766](https://github.com/emqx/emqx/pull/11766) REST API向けの予備的なロールベースアクセス制御を実装。3つの事前定義ロール：

  - 管理者（Administrator）：すべてのリソースにアクセス可能。
  - ビューアー（Viewer）：リソースとデータの閲覧のみ可能。REST APIのGETリクエストに対応。
  - パブリッシャー（Publisher）：MQTTメッセージパブリッシュに特化し、メッセージパブリッシュ関連エンドポイントのアクセスに限定。

- [#11773](https://github.com/emqx/emqx/pull/11773) ダッシュボードで監査ログ管理をサポート。デバイスのキックアウトやルール作成・削除などEMQXデバイス・データの変更操作を閲覧可能。
- [#11778](https://github.com/emqx/emqx/pull/11778) Microsoft Entra Identity（旧Azure Active Directory）をSAMLシングルサインオン（SSO）に統合。
- [#11811](https://github.com/emqx/emqx/pull/11811) REST APIキーのブートストラップファイル形式を改善し、ロール付き初期化をサポート。

  新形式：`api_key:api_secret:role`

  `role` は省略可能でデフォルトは `administrator`。

- [#11852](https://github.com/emqx/emqx/pull/11852) GB/T 32960ゲートウェイを追加し、GBT32960車載ネットワークプロトコル経由で車両がEMQXに接続可能に。
- [#11883](https://github.com/emqx/emqx/pull/11883) JT/T808ゲートウェイを追加し、JT/T 808車載ネットワークプロトコル経由で車両がEMQXに接続可能に。
- [#11885](https://github.com/emqx/emqx/pull/11885) 電気自動車（EV）充電ステーション向けのOCPPゲートウェイを追加し、OCPP（Open Charge Point Protocol）経由でEMQXにアクセス可能に。
- [#11971](https://github.com/emqx/emqx/pull/11971) `/api/v5/load_rebalance/availability_check` を認証不要のパブリックAPIに変更し、ロードバランサー設定を簡素化。

  待機ヘルスチェックフェーズ中のリバランス／退避処理の優雅性を向上。退避対象ノードへの接続は禁止されなくなりました。このフェーズではロードバランサーがノードを不健康とマークしているか不明なため、接続禁止は複数の再接続失敗を招く恐れがあるためです。

- [#12013](https://github.com/emqx/emqx/pull/12013) データブリッジ設計を調整し、コネクターとアクション（シンク）に分割。コネクターは外部システムとの統合管理に使用され、複数のアクションで再利用可能。アクションはデータ処理方法の設定に特化。これにより柔軟性とスケーラビリティが向上し、データ統合設定と管理が明確化。

  調整対象のデータブリッジはPostgreSQL、Timescale、Matrixで、コネクターおよびアクションAPIに分割されたが旧データブリッジAPIとの互換性は維持。

- [#12016](https://github.com/emqx/emqx/pull/12016) ライセンスキー管理を強化。

  `license.key` 設定に `"file://"` プレフィックス付きのファイルパスを指定してライセンスキーをファイルから読み込み可能に。
  また、`license.key = default` に設定するとデフォルトのトライアルライセンスに戻せる機能を追加。トライアルライセンスへの復帰を簡素化。

- [#12129](https://github.com/emqx/emqx/pull/12129) デフォルトライセンスを2023年1月発行の旧ライセンスから更新。同時にライセンス容量を100接続から25接続に調整。

### バグ修正

- [#10976](https://github.com/emqx/emqx/pull/10976) 共有サブスクリプションにおけるトピックフィルターの重複処理を修正。以前はサブスクリプションオプションの保存方法が不十分で、特定の順序とトピックでの「サブスクライブ-アンサブスクライブ」処理中にメッセージルーティング失敗やノード間のルーティングテーブル漏洩が発生。
- [#12048](https://github.com/emqx/emqx/pull/12048) COAPゲートウェイのサブスクリプションオプション無視バグを修正。
- [#12078](https://github.com/emqx/emqx/pull/12078) grpc-erlを0.6.12にアップグレード。grpcクライアントが依存アプリを遅延起動する可能性のあるデッドロック問題を修正。
- [#12081](https://github.com/emqx/emqx/pull/12081) `gen_rpc` ライブラリを3.3.1に更新。主な性能改善：

  - パケット送信前の余分なメモリ割り当て回避。
  - ローカル呼び出しでのネットワークバイパス。
  - デバッグログでの機密データ漏洩防止 [#12202](https://github.com/emqx/emqx/pull/12202)

- [#12111](https://github.com/emqx/emqx/pull/12111) ログイン直後にAPIトークンが一時的に利用不可となるレースコンディションを修正。
- [#12121](https://github.com/emqx/emqx/pull/12121) クラスター内の異なるノードで同時に設定更新すると古いビューが返される問題を修正。
- [#12158](https://github.com/emqx/emqx/pull/12158) UpstashホストのRedisにルールエンジンが接続できない問題を修正。

  修正前はRedisドライバーがAUTHやSELECTコマンドをInline Commandsで送信していたが、UpstashはInline Commands非対応で接続失敗。
  修正後はRESP（REdis Serialization Protocol）でコマンド送信。

- [#12176](https://github.com/emqx/emqx/pull/12176) MQTT-SNクライアントからの `DISCONNECT` パケットは接続成立の有無に関わらず常にアック（ACK）を返すよう修正。
- [#12180](https://github.com/emqx/emqx/pull/12180) DTLS有効のMQTT-SNゲートウェイが起動できない問題を修正。デフォルトリスナー設定とDTLS実装の非互換が原因。
- [#12219](https://github.com/emqx/emqx/pull/12219) ダッシュボードからの設定更新時にファイル転送S3設定のシークレット復号化問題を修正。
- [#12141](https://github.com/emqx/emqx/pull/12141) 無効なトピックフィルターで `/v5/topics` APIがHTTP 500の `InternalError` を返す問題を修正。
- [#12059](https://github.com/emqx/emqx/pull/12059) デフォルトのタイムワープモードを `multi-time-warp` に変更。詳細は [time_correction_#multi-time-warp-mode](https://www.erlang.org/doc/apps/erts/time_correction#multi-time-warp-mode) を参照。

## 5.3.2

*リリース日: 2023-12-01*

### 機能強化

- [#11752](https://github.com/emqx/emqx/pull/11752) コア・レプリカ間のデータベース同期用RPCドライバーを `gen_rpc` から `rpc` に変更し、レイテンシを改善。
- [#11785](https://github.com/emqx/emqx/pull/11785) "Viewer" ロールのユーザーが自身のパスワードを変更可能に。ただし他ユーザーのパスワード変更は不可。
- [#11787](https://github.com/emqx/emqx/pull/11787) `emqx` コマンドの性能を改善。
- [#11790](https://github.com/emqx/emqx/pull/11790) Redis認可ソースのRedisコマンドにバリデーションを追加。認証・認可処理時のRedisコマンド解析を `redis-cli` 互換にし、引用符付き引数をサポート。
- [#11541](https://github.com/emqx/emqx/pull/11541) ファイル転送機能を強化。クライアントは `$file-async/...` トピックにコマンドを送信し、 `$file-response/{clientId}` トピックでコマンド実行結果をサブスクライブする非同期方式を利用可能に。MQTT v3.1/v3.1.1やMQTTブリッジ利用クライアントに適した簡便なファイル転送方法。詳細は [EIP-0021](https://github.com/emqx/eip) を参照。

### バグ修正

- [#11757](https://github.com/emqx/emqx/pull/11757) 存在しないトレースファイルのダウンロード時にエラー応答コードを500から404に修正。
- [#11762](https://github.com/emqx/emqx/pull/11762) `built_in_database` 認可ソース削除時にACLレコードが完全に削除されない問題を修正。再作成時の残存レコード問題を解消。
- [#11771](https://github.com/emqx/emqx/pull/11771) API／ダッシュボード経由の認証管理でBcryptソルトラウンドのバリデーションを修正。
- [#11780](https://github.com/emqx/emqx/pull/11780) `pbkdf2` パスワードハッシュアルゴリズムの `iterations` フィールドが0設定可能だった問題を修正。正の整数でなければならない。
- [#11791](https://github.com/emqx/emqx/pull/11791) EMQX CoAPゲートウェイでハートビートが接続の活性維持に機能していなかった問題を修正。
- [#11797](https://github.com/emqx/emqx/pull/11797) `built_in_database` 認可ソース管理APIの挙動を変更。設定されていない場合は404を返すようにし、以前の20Xレスポンスを修正。
- [#11965](https://github.com/emqx/emqx/pull/11965) MongoDBリソースが利用不可でもEMQXサービスの正常終了を保証するよう改善。
- [#11975](https://github.com/emqx/emqx/pull/11975) ピアとサーバーの同時ソケットクローズによるレースコンディションで冗長なエラーログが発生する問題を修正。イベント処理を改善し不要なエラーメッセージを排除。
- [#11987](https://github.com/emqx/emqx/pull/11987) TCP/SSLソケットの `active_n` オプション設定時に接続クラッシュする問題を修正。ソケットが既に閉じられている場合の `case_clause` クラッシュを防止。
- [#11731](https://github.com/emqx/emqx/pull/11731) ファイル転送機能のホットコンフィグ対応を追加。
- [#11754](https://github.com/emqx/emqx/pull/11754) Postgresブリッジのログフォーマットを改善。ドライバーのエラーメッセージに含まれるUnicode文字の問題を修正。

## 5.3.1

*リリース日: 2023-11-14*

### 機能強化

- [#11637](https://github.com/emqx/emqx/pull/11637) mnesiaのテーブル待機時の問題調査を支援する追加診断チェックを導入。ライブラリアップデート：`ekka` を0.15.15、`mria` を0.6.4に更新。
- [#11581](https://github.com/emqx/emqx/pull/11581) 機能プレビュー：EMQX v5.4.0でデータブリッジを基にした *Connector* と *Action* の概念を導入予定。既存のデータブリッジは段階的にConnectorとActionに移行。Connectorは外部システムとの統合管理用、Actionはデータ処理方法の設定用。Connectorは複数のActionで再利用可能で柔軟性とスケーラビリティを向上。KafkaパブリッシャーとAzure Event Hubパブリッシャーの移行は完了。
- ダッシュボードはルールエンジンのメッセージ再パブリッシュアクションのMQTT 5.0パブリッシュ属性設定をサポート。

### バグ修正

- [#11565](https://github.com/emqx/emqx/pull/11565) jqライブラリをv0.3.10からv0.3.11にアップグレード。jq_portプログラムはオンデマンド起動となり、EMQXでjq関数を使用しない限りユーザーのプロセスに現れない。アイドル状態のjq_portは一定時間後に自動終了。注：ほとんどのEMQXユーザーはNIFモードでjqを使用しており影響なし。
- [#11676](https://github.com/emqx/emqx/pull/11676) デバッグレベルログから機密情報を一部隠蔽。
- [#11697](https://github.com/emqx/emqx/pull/11697) EMQXバックプレーンネットワーク（`gen_rpc`）で古いTLSバージョンと暗号スイートを無効化。TLSv1.3をサポートし、新設定パラメータ `EMQX_RPC__TLS_VERSIONS` と `EMQX_RPC__CIPHERS` を追加。

  対応する `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) IPv6ネットワークでのクラスタリングを修正。`rpc.listen_address` と `rpc.ipv6_only` 設定を追加し、EMQXクラスタRPCサーバーとクライアントでIPv6を使用可能に。
- [#11747](https://github.com/emqx/emqx/pull/11747) QUICスタックをmsquic 2.2.3にアップグレード。
- [#11796](https://github.com/emqx/emqx/pull/11796) RPCスキーマを修正し、クライアント／サーバーが同じトランスポートドライバーを使用するよう保証。
- [#11798](https://github.com/emqx/emqx/pull/11798) `./bin/emqx data import [FILE]` 実行後にノードが起動できない問題を修正。

  `apikey_key` と `apikey_name` の連携を強化し、一貫性と一意性を向上。
  - ダッシュボードでAPIキー生成時、`apikey_key` は人間可読の `apikey_name` から一意値を生成。
  - ブートストラップファイルでAPIキー生成時、`apikey_name` は関連する `apikey_key` から一意値を生成。

- [#11813](https://github.com/emqx/emqx/pull/11813) RPCクライアントのSSLポートが設定済みサーバーポートと一致するようスキーマを修正。HelmチャートでRPCポートが正しく開放されることを保証。
- [#11819](https://github.com/emqx/emqx/pull/11819) opentelemetryライブラリをv1.3.1-emqxにアップグレード。エクスポートされるメトリクスのタイムスタンプ不正を修正。
- [#11861](https://github.com/emqx/emqx/pull/11861) リモートコンソールシェルで過剰な警告メッセージが表示される問題を修正。
- [#11722](https://github.com/emqx/emqx/pull/11722) `sync` クエリモードのKafkaプロデューサーブリッジが `connecting` 状態でメッセージをバッファリングしない問題を修正。
- [#11724](https://github.com/emqx/emqx/pull/11724) 内部バッファリングにより、Kafkaへの送信メッセージが成功後も失敗としてカウントされるメトリクス問題を修正。
- [#11728](https://github.com/emqx/emqx/pull/11728) LDAPフィルタ文字列パーサーを強化：

  - フィルタ文字列内の特殊文字を自動エスケープ。
  - `dn` をフィルタ値として使用できなかったバグを修正。

- [#11733](https://github.com/emqx/emqx/pull/11733) セッションテイクオーバーやチャネル退避時に、セッションがEMQX v5.2.x以前のリモートノードに存在するとクラッシュする非互換問題を解消。
- [#11750](https://github.com/emqx/emqx/pull/11750) HTTP認証およびHTTPブリッジでHTTPリクエストボディのログ記録とトレースを停止。
- [#11760](https://github.com/emqx/emqx/pull/11760) Cassandraブリッジのヘルスチェックで使用されるCQLクエリを簡素化し、Cassandraサーバーログの警告を削減。

- [#11886](https://github.com/emqx/emqx/pull/11886) 旧プラグインとの互換性を修正。

  現在、EMQXはフックポイント名を検証し、無効なフックポイントは登録不可。しかし旧バージョンのプラグインテンプレートや実際のプラグインで誤ったフックポイントが使われている場合があるため、これらの旧フックポイントの登録を許可しつつ非推奨警告を発行。従来通りこれらのフックは呼び出されません。

- [#11897](https://github.com/emqx/emqx/pull/11897) クラスター内のノードがほぼ同時に起動した際の設定同期におけるループレースコンディション問題を修正。

## 5.3.0

*リリース日: 2023-09-29*

### 機能強化

- [#11597](https://github.com/emqx/emqx/pull/11597) ekkaを0.15.13にアップグレード。主な変更点：

  - Mriaを0.6.2にアップグレード。
  - ブートストラップデータ同期のバッチサイズ設定機能を追加（詳細は [Mria PR](https://github.com/emqx/mria/pull/159)）。
  - mria_membershipプロセスの信頼性を向上（詳細は [Mria PR](https://github.com/emqx/mria/pull/156)）。
  - ログメッセージのフォーマットエラーを修正。
  - EMQX設定に `node.default_bootstrap_batch_size` オプションを追加。
  
  この値を増やすことで、特にクラスター間ネットワークレイテンシが高く、EMQX組み込みデータベースに大量データ（例：サブスクリプション数が多い）がある場合のレプリカノード起動時間を大幅に短縮可能。

- [#11620](https://github.com/emqx/emqx/pull/11620) ルールエンジンにバイト列サイズを取得するSQL関数 `bytesize` を追加。例：`SELECT * FROM "t/#" WHERE bytesize(payload) > 10`。
- [#11642](https://github.com/emqx/emqx/pull/11642) QUICトランスポートのopenssl3対応準備としてquicerをバージョン0.0.200に更新。
- [#11610](https://github.com/emqx/emqx/pull/11610) ダッシュボードの予備的なロールベースアクセス制御を実装。

  2つの事前定義ロール：
  - 管理者（Administrator）：すべてのリソースにアクセス可能。
  - ビューアー（Viewer）：リソースとデータの閲覧のみ可能。REST APIのGETリクエストに対応。

- [#11631](https://github.com/emqx/emqx/pull/11631) シングルサインオン（SSO）機能を追加し、LDAPと統合。
- [#11656](https://github.com/emqx/emqx/pull/11656) SAML 2.0サポートをSSOに統合。
- [#11599](https://github.com/emqx/emqx/pull/11599) CLI、REST API、ダッシュボードからの操作を別ログファイルに記録する監査ログをサポート。

### バグ修正

- [#11682](https://github.com/emqx/emqx/pull/11682) ファイルログハンドラーで「回転サイズ」が `infinity` に設定されるとログ記録が停止する問題を修正。
- [#11567](https://github.com/emqx/emqx/pull/11567) EMQXのグレースフルシャットダウン（`emqx stop` コマンド）を改善：

  - タイムアウトを1分から2分に延長。
  - 設定タイムアウト内に停止できない場合はエラーメッセージを表示。
  - シャットダウン中に定期的に状態メッセージを表示。

- [#11584](https://github.com/emqx/emqx/pull/11584) Windowsで `os_mon` モジュールが利用不可の場合のテレメトリ報告エラーを修正。
- [#11605](https://github.com/emqx/emqx/pull/11605) CMD_overriddenログの重大度を警告から情報に下げ。
- [#11622](https://github.com/emqx/emqx/pull/11622) RPCライブラリ `gen_rpc` を2.8.1から3.1.0にアップグレード。
- [#11623](https://github.com/emqx/emqx/pull/11623) ライブラリ `esockd` を5.9.6から5.9.7にアップグレード。主な変更点：

  - プロキシプロトコルのエラーとタイムアウト処理を強化 [esockd pr#178](https://github.com/emqx/esockd/pull/178)
  - `ssl_error` 例外のログレベルを情報に下げ [esockd pr#180](https://github.com/emqx/esockd/pull/180)
  - 不正なMQTTパケット解析例外のログレベルをエラーから情報に下げ。
  - `emqx ctl listeners` コマンド出力でTLSハンドシェイク失敗（`ssl_error`）や不正パケット（`frame_error`）発生時に `shutdown_count` カウンターを増加。

- [#11661](https://github.com/emqx/emqx/pull/11661) ログフォーマッターのバグを修正。`log.HANDLER.formatter` が 'json' の場合、ログ行が有効なJSONでなくなっていた問題（v5.0.4で導入）。
- [#11627](https://github.com/emqx/emqx/pull/11627) HStreamDBブリッジのリソースクリーンアップを修正。設定更新時にhstreamdbクライアント／プロデューサーが正しく停止されずエラーが発生していた。

## 5.2.1

*リリース日: 2023-09-20*

### 機能強化

- [#11487](https://github.com/emqx/emqx/pull/11487) bcryptのワークファクターを5〜10の範囲に制限。高すぎる値はCPUリソースを過剰消費。bcryptライブラリを並列ハッシュ評価対応に更新。
- [#11568](https://github.com/emqx/emqx/pull/11568) ルールのリパブリッシュアクションでMQTT 5.0のパブリッシュプロパティおよびユーザープロパティのテンプレート定義をサポート。
- [#11612](https://github.com/emqx/emqx/pull/11612) ノード退避時に `clean_start` が `false` のセッションだけでなく、すべての切断済みセッションを退避。
- [#11532](https://github.com/emqx/emqx/pull/11532) 不正パケット解析時のエラーメッセージを改善し、より明確に。

### バグ修正

- [#11493](https://github.com/emqx/emqx/pull/11493) `/api/v5/publish` の悪いリクエスト時のRESP APIドキュメントのレスポンス例を修正。以前は本文にリストが返る例があったが実際はそうではなかった。
- [#11499](https://github.com/emqx/emqx/pull/11499) Erlang/OTPを25.3.2-2にアップグレードし、mnesia_hookログから機密データを除外。
- [#11506](https://github.com/emqx/emqx/pull/11506) 存在しないトレースログファイルのダウンロード時に空ファイルが返されていた問題を修正。空のトレースログファイルに対してはHTTP 404と `{"code":"NOT_FOUND","message":"Trace is empty"}` を返すように。
- [#11522](https://github.com/emqx/emqx/pull/11522) ルールエンジンのスキーマレジストリでスキーマ名が長すぎる場合のエラーメッセージを改善。
- [#11531](https://github.com/emqx/emqx/pull/11531) 特定のクライアントIDに対する認可キャッシュクリーニングCLIが正しく動作しない問題を修正。
- [#11564](https://github.com/emqx/emqx/pull/11564) クラスターのパーティション自動修復機能を修正。複数パーティションに分割されたクラスターの自動修復を実装。
- [#11568](https://github.com/emqx/emqx/pull/11568) 不適切に定義された組み込みルールアクション設定がカスタムユーザ関数として誤解釈される問題を修正。
- [#11394](https://github.com/emqx/emqx/pull/11394) Kafkaプロデューサークライアント `wolff` を1.7.6から1.7.7にアップグレード。初期化失敗時にすべてのKafkaプロデューサーがクラッシュする可能性のあるレースコンディションを修正。
- [#11401](https://github.com/emqx/emqx/pull/11401) ルールSQLの `mongo_date` 関数の動作を修正。EMQXダッシュボードのSQL文テスト時にISODate(*)形式の文字列を返すようにし、MongoDBの日付保存形式と整合。
- [#11547](https://github.com/emqx/emqx/pull/11547) 複数のemqx_bridge問題を修正：

  - Cassandraブリッジでユーザー名／パスワードなし設定時の接続エラーを修正（`authenticator: AllowAllAuthenticator` 設定時は認証不要）。
  - SQL Serverブリッジで空パスワードによる接続エラーを修正。
  - Oracleブリッジで `username` を必須フィールドに。
  - IoTDBブリッジでスキームなしのベースURL設定時のエラーを修正（例：`<host>:<port>`）。

- [#11630](https://github.com/emqx/emqx/pull/11630) コアノードが `mria_schema:bootstrap/0` 状態でスタックし、新規ノードがクラスタに参加できなくなる問題を修正。

## 5.2.0

*リリース日: 2023-09-07*

### 機能強化

- [#10697](https://github.com/emqx/emqx/pull/10697) StatefulSetの `minReadySeconds` 設定を可能に。これによりアップグレードや再起動コマンドでトリガーされる個々のPod再起動間に時間差を設けられる。
- [#11124](https://github.com/emqx/emqx/pull/11124) Amazon Linux 2023向けパッケージをリリース。
- [#11289](https://github.com/emqx/emqx/pull/11289) Debian 12向けパッケージをリリース。
- [#11290](https://github.com/emqx/emqx/pull/11290) `jq` 依存を0.3.10に更新。`oniguruma` ライブラリを6.9.8にアップデートし、いくつかの軽微なセキュリティ修正を含む。
- [#11291](https://github.com/emqx/emqx/pull/11291) RocksDBバージョンを1.8.0-emqx-1に更新（ekka 0.15.6経由）。

- [#11390](https://github.com/emqx/emqx/pull/11390) EMQX構成に`node.broker_pool_size`、`node.generic_pool_size`、`node.channel_cleanup_batch_size`オプションを追加しました。これらのオプションを調整することで、クラスター間接続のネットワークレイテンシが高い場合でもパフォーマンスを大幅に向上させることが可能です。

- [#11429](https://github.com/emqx/emqx/pull/11429) MongoDBコネクターおよびブリッジにおいて、レガシープロトコルの検出を設定するオプションを追加しました。

- [#11436](https://github.com/emqx/emqx/pull/11436) すべての`banned`データをクリアするための新しいAPIエンドポイント`DELETE/banned`を追加しました。

- [#11438](https://github.com/emqx/emqx/pull/11438) `mqtt.max_packet_size`の型を文字列からbyteSizeに変更し、有効な数値範囲をより適切に表現できるようにしました。後方互換性のため文字列も引き続き受け付けます。

- [#11469](https://github.com/emqx/emqx/pull/11469) Redis認証でユーザー名を指定可能に対応しました。

- [#11496](https://github.com/emqx/emqx/pull/11496) Erlang VMのPrometheusエクスポーターをデフォルトで無効化し、パフォーマンスとセキュリティを向上させました。

- [#11497](https://github.com/emqx/emqx/pull/11497) ブローカーのメトリクス収集とエクスポートを強化し、メッセージ、オーバーロード保護、認可、認証に関する新しいメトリクスを追加、OpenTelemetryの命名の一貫性も改善しました。

- [#10647](https://github.com/emqx/emqx/pull/10647) [GreptimeDB](https://github.com/GreptimeTeam/greptimedb)のデータ統合を実装しました。

- [#11261](https://github.com/emqx/emqx/pull/11261) Amazon Kinesis Data Streamsプロデューサーのデータ統合を実装しました。

- [#11329](https://github.com/emqx/emqx/pull/11329) Azure Event Hubプロデューサーのデータ統合を実装しました。

- [#11363](https://github.com/emqx/emqx/pull/11363) RabbitMQブリッジにTLS接続サポートを追加しました。

- [#11367](https://github.com/emqx/emqx/pull/11367) EMQX 4.4からGCP IoT Hub認証サポートを移植しました。

- [#11386](https://github.com/emqx/emqx/pull/11386) LDAPを新しい認証方式として統合しました。

- [#11392](https://github.com/emqx/emqx/pull/11392) LDAPを認可ソースとして統合しました。

- [#11402](https://github.com/emqx/emqx/pull/11402) KafkaコンシューマーブリッジのトピックマッピングでMQTTトピックをプレースホルダーで定義するサポートを追加しました。これによりMQTTトピックを動的に設定可能です。

- [#11403](https://github.com/emqx/emqx/pull/11403) GCP PubSubプロデューサーブリッジでメッセージ属性とオーダリングキーのテンプレート定義をサポートしました。

  また、HOCONライブラリを更新し、配列内のオブジェクトが異なる行にあっても連結されてしまう問題を修正しました。

- [#11459](https://github.com/emqx/emqx/pull/11459) Kafkaブリッジのヘルスチェック間隔を設定するオプションを追加しました。

- [#11478](https://github.com/emqx/emqx/pull/11478) HStreamDBブリッジのサポートを追加しました（TCPおよびTLS接続両対応）。HStreamDB `v0.16.1`に対応しています。

  ドライバーは[PR#11530](https://github.com/emqx/emqx/pull/11530)で`0.4.5+v0.16.1`に更新されました。

- [#11389](https://github.com/emqx/emqx/pull/11389) Mria 0.6.0で導入された新APIを活用し、複数のインデックス更新操作を単一のMnesiaアクティビティに統合することで、保持メッセージのパブリッシュレイテンシを改善しました。

- [#11396](https://github.com/emqx/emqx/pull/11396) ルールエンジンのランタイムにトピックインデックスを導入し、ルール定義で設定されたトピックフィルターへのメッセージトピックのマッチングを高速化しました。これにより大量のルールを扱う際のEMQXのパフォーマンスが大幅に向上します。

- [#11399](https://github.com/emqx/emqx/pull/11399) ルールエンジンのプレースホルダー構文を改善しました。再パブリッシュアクションはペイロード変数内の文字列を動的に埋め込むためのプレースホルダー構文`\${key}`をサポートしています。改善前は`key`に英数字とアンダースコアのみが使用可能でしたが、現在は任意のUTF8文字をサポートします。

- [#11405](https://github.com/emqx/emqx/pull/11405) `date_to_unix_ts`関数のエラーメッセージをより分かりやすくしました。

- [#11490](https://github.com/emqx/emqx/pull/11490) 各種認証バックエンドで未定義のパスワードに対する高速なエラーハンドリングを追加しました。これにより認証処理の一貫性とユーザーフレンドリーさが向上します。

### バグ修正

- [#11065](https://github.com/emqx/emqx/pull/11065) EMQXシャットダウン時の不要なエラーメッセージを抑制しました。

- [#11279](https://github.com/emqx/emqx/pull/11279) EMQXでデバッグ/トレースログが有効な場合に、大きなペイロードのメッセージ送信ができない問題を修正しました。

- [#11296](https://github.com/emqx/emqx/pull/11296) `emqx ctl import`コマンドでEMQXバックアップファイルから追加設定をインポート可能にしました：

  - rule_engine（以前はバグによりインポートされていませんでした）
  - topic_metrics（以前は未実装）
  - slow_subs（以前は未実装）

- [#11327](https://github.com/emqx/emqx/pull/11327) ekkaを0.15.8、mriaを0.15.8、optvarを1.0.5に更新し、断続的なアサーション失敗を修正しました。

- [#11346](https://github.com/emqx/emqx/pull/11346) ekkaを0.15.9に更新し、ロック取得がタイムアウトした際に発生するetcdロックの残留問題を修正しました。

- [#11347](https://github.com/emqx/emqx/pull/11347) OCSPリクエストパスが適切にURLエンコードされるように修正しました。

- [#11352](https://github.com/emqx/emqx/pull/11352) RocksDB非対応のWindowsやその他プラットフォームで起動時に発生するクラッシュ問題を修正しました（詳細は[issue #11345](https://github.com/emqx/emqx/issues/11345)）。

- [#11388](https://github.com/emqx/emqx/pull/11388) `emqx_router_sup`の再起動強度を増加させ、通常条件下で発生する断続的なクラッシュに対する耐性を向上させました。これによりEMQXアプリケーション全体のシャットダウンを回避します。

  例として、`emqx_router_helper`経由でレプリカントからコアノードに委譲されるmriaの書き込み/削除呼び出しが、コアノードの停止・再起動・未準備状態で失敗する場合があります。修正後の再起動強度によりシステムの安定稼働が維持されます。

  これは5.1.3からのアップグレード時に設定ファイルやEMQX Operator設定に残っていたオプションに起因する問題を修正します。

- [#11424](https://github.com/emqx/emqx/pull/11424) APIのタイムスタンプ最大値チェックを追加し、有効なUnixタイムスタンプであることを保証しました。

- [#11445](https://github.com/emqx/emqx/pull/11445) WindowsプラットフォームでのVMクラッシュ防止のため、os_monアプリケーションモニターサポートを削除しました。非Windowsプラットフォームでは引き続き機能します。

- [#11454](https://github.com/emqx/emqx/pull/11454) 大きなペイロードでのデバッグ/トレース時に発生していたクラッシュを修正しました（[#11279](https://github.com/emqx/emqx/pull/11279)で導入された問題）。

- [#11456](https://github.com/emqx/emqx/pull/11456) CA証明書ファイルのPEMが空でも許容するように、非空チェックのバリデーションを削除しました。

- [#11466](https://github.com/emqx/emqx/pull/11466) `ssl_options.ciphers`設定を空文字列に設定した際のクラッシュを修正しました。

- [#11480](https://github.com/emqx/emqx/pull/11480) ルールエンジンのSQL関数で不正な引数が渡された場合のエラーハンドリングとテストを改善しました。

- [#11520](https://github.com/emqx/emqx/pull/11520) `packets_connack_sent`メトリクスが非ゼロの`ack_flag`付きCONNACKパケット送信時に増加しなかった問題を修正しました。

- [#11523](https://github.com/emqx/emqx/pull/11523) `/configs` APIで無効な証明書/キーを指定した際の誤解を招くプロンプトを修正しました。

- [#11534](https://github.com/emqx/emqx/pull/11534) ブリッジが正常でない場合のデータブリッジ統計の増加処理を修正しました。現在は不健康なブリッジに送信されたメッセージはドロップメッセージとしてカウントされます。

- [#11540](https://github.com/emqx/emqx/pull/11540) 無効な名前でブリッジ作成を試みた際のHTTPレスポンスを改善しました。

- [#11548](https://github.com/emqx/emqx/pull/11548) クラスター全体でプラグインの順序が更新されない問題を修正しました。

- [#11366](https://github.com/emqx/emqx/pull/11366) EMQX Operatorで`bootstrapConfig`に一部ブリッジ設定が指定されている場合にPodが起動しない問題を修正しました。

- [#11453](https://github.com/emqx/emqx/pull/11453) InfluxDBブリッジの接続性テストで誤検知が発生する問題を修正しました。

- [#11461](https://github.com/emqx/emqx/pull/11461) ブリッジ接続性テストのタイムアウトを設定されたヘルスチェックタイムアウトにより近づけました。

- [#11492](https://github.com/emqx/emqx/pull/11492) GreptimeDBブリッジの接続性テストで誤検知が発生する問題を修正しました。

- [#11508](https://github.com/emqx/emqx/pull/11508) Kafkaブリッジでヘッダーが無効な値に変換された際のエラーハンドリングを修正しました。

- [#11513](https://github.com/emqx/emqx/pull/11513) Kafkaプロデューサーブリッジで`timestamp`フィールドに正しいテンプレートが使用されないバグを修正しました。

- [#11527](https://github.com/emqx/emqx/pull/11527) Kafkaヘッダーテンプレート処理に関する問題を修正しました。プレースホルダーがキー・バリューの配列（例：`[{"key": "foo", "value": "bar"}]`）に解決される場合に発生していました。

## 5.1.1

*リリース日: 2023-07-27*

### 強化

- [#10667](https://github.com/emqx/emqx/pull/10667) MongoDBコネクターおよびブリッジを別アプリケーションとしてリファクタリングし、コード構造を改善しました。

- [#11115](https://github.com/emqx/emqx/pull/11115) TTL切れによりバッファリングされたメッセージが破棄された際の情報ログを追加しました。

- [#11133](https://github.com/emqx/emqx/pull/11133) `retainer`の設定で`deliver_rate`を`delivery_rate`に名称変更し、旧`deliver_rate`との互換性を維持しました。

- [#11137](https://github.com/emqx/emqx/pull/11137) ダッシュボードのリスナー設定をSSL設定用のネストされた`ssl_options`フィールドにリファクタリングしました。

- [#11138](https://github.com/emqx/emqx/pull/11138) k8sの`api_server`のデフォルト値を`http://127.0.0.1:9091`から`https://kubernetes.default.svc:443`に変更しました。

  - `emqx_ctl conf show cluster`は`discovery_strategy=static`の場合、関連しない設定項目（`etcd/k8s/dns`関連）を表示しなくなりました。
  - 非推奨の`zones`設定キーを`emqx_ctl conf show_keys`から削除しました。

- [#11165](https://github.com/emqx/emqx/pull/11165) `/configs/limiter` APIを`swagger.json`から削除しました。APIの機能自体は変更ありません。

- [#11166](https://github.com/emqx/emqx/pull/11166) ルールエンジンに以下の3つのランダムSQL関数を追加しました：

  - `random()`: 0以上1未満のランダムな数値を生成
  - `uuid_v4()`: ランダムなUUID（バージョン4）文字列を生成
  - `uuid_v4_no_hyphen()`: ハイフンなしのランダムなUUID（バージョン4）文字列を生成

- [#11180](https://github.com/emqx/emqx/pull/11180) HOCON形式の設定ファイルをリロード可能な新しい設定API `/configs`（GET/PUT）を追加しました。

- [#11226](https://github.com/emqx/emqx/pull/11226) リスナーのスイッチを`enable`に統一し、旧`enabled`との互換性を維持しました。

- [#11249](https://github.com/emqx/emqx/pull/11249) ライセンス接続使用量アラームの水位を読み書きするREST APIエンドポイント`/license/setting`を追加しました。

- [#11251](https://github.com/emqx/emqx/pull/11251) クラスターのトポロジーを返すREST APIエンドポイント`/cluster/topology`を追加しました。GETリクエストでRLOGコアノードとレプリカントノード間の接続状況を表示します。

- [#11253](https://github.com/emqx/emqx/pull/11253) Webhook/HTTPブリッジを独立したErlangアプリケーションにリファクタリングしました。将来的な柔軟性向上と単独アプリケーションとしての実行が可能になります。

- [#11079](https://github.com/emqx/emqx/pull/11079) Kafkaブリッジのプロデューサーモードでメッセージのカスタムヘッダーをサポートしました。

- [#11132](https://github.com/emqx/emqx/pull/11132) QoSレベルとRetainフラグに基づくMQTTアクション認可をサポートしました。これにより、クライアントが特定のQoSレベルでパブリッシュ/サブスクライブする権限や保持メッセージのパブリッシュ権限を検証可能です。

- [#11207](https://github.com/emqx/emqx/pull/11207) 複数のデータブリッジのドライバーを更新し、セキュリティを強化し機密データの漏洩を防止しました。対象は以下の通りです：

  - TDengine
  - MongoDB
  - MySQL
  - Clickhouse

- [#11241](https://github.com/emqx/emqx/pull/11241) スキーマレジストリを独立したErlangアプリケーションにリファクタリングしました。将来的な柔軟性向上が期待されます。

- [#11020](https://github.com/emqx/emqx/pull/11020) emqtt依存関係をアップグレードし、デバッグログでの機密データ漏洩を防止しました。

- [#11135](https://github.com/emqx/emqx/pull/11135) ルールエンジンの時刻オフセットパーサーを改善し、統一されたエラーコードを返すようにしました。

- [#11236](https://github.com/emqx/emqx/pull/11236) REST API `/clients`エンドポイントのクライアント検索速度をデフォルトパラメータで改善しました。

### バグ修正

- [#11004](https://github.com/emqx/emqx/pull/11004) トピックリライトの宛先トピックにワイルドカードを使用できなくしました。

- [#11026](https://github.com/emqx/emqx/pull/11026) ルールエンジン内の`div`と`mod`演算の不整合を修正しました。以前は`div`は中置演算子のみ、`mod`は関数呼び出しのみでしたが、両方とも関数呼び出しと中置構文の両方で使用可能になりました。

- [#11037](https://github.com/emqx/emqx/pull/11037) HTTPコネクター起動時にリモートターゲットへの接続失敗時に説明的なエラーを返すようにしました。

- [#11039](https://github.com/emqx/emqx/pull/11039) Redisコネクターのデータベース番号検証を修正し、負の数を無効にしました。

- [#11074](https://github.com/emqx/emqx/pull/11074) MQTT-5.0プロトコル仕様[MQTT-3.8.3-4]に準拠するようバグを修正しました。

- [#11077](https://github.com/emqx/emqx/pull/11077) 非整数ポートでリスナーのバインド更新時に発生するクラッシュを修正しました。

- [#11094](https://github.com/emqx/emqx/pull/11094) Kafkaプロデューサーの接続エラーがブリッジ再接続時に報告されない問題を修正しました。

- [#11103](https://github.com/emqx/emqx/pull/11103) `erlcloud`依存関係を更新しました。

- [#11106](https://github.com/emqx/emqx/pull/11106) ブリッジリソースの`worker_pool_size`の最大値検証を追加しました。最大値は1024に制限し、不合理なワーカー数による大メモリ消費を防止します。

- [#11118](https://github.com/emqx/emqx/pull/11118) REST APIのバリデーションエラーをより分かりやすくしました。範囲外エラーは`{"value": 42, "reason": {"expected": "1..10"}, ...}`の形式で返され、以前の`expected_type`は`expected`に置き換えられました。

- [#11126](https://github.com/emqx/emqx/pull/11126) 非同期モードのブリッジにおけるルールメトリクスの失敗カウンターが正しく設定されるよう修正しました。

- [#11134](https://github.com/emqx/emqx/pull/11134) ログにおいて大文字の`authorization`ヘッダーの値がマスクされない問題を修正しました。

- [#11139](https://github.com/emqx/emqx/pull/11139) Redisコネクターを独立したErlangアプリケーションにリファクタリングし、コード構造を改善しました。

- [#11145](https://github.com/emqx/emqx/pull/11145) EkkaおよびMriaに複数の修正と改善を加えました。

  Ekka:
  - クラスター検出ログメッセージを実際のイベントに即して一貫性を持たせました。
  [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - 非推奨のクラスター自動クリーン設定パラメータを削除し、Mriaに移行しました。
  [Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:
  - Pingはレプリカントノードのみで実行されるようにしました。以前は停止中および稼働中のレプリカント両方にPingを試み、タイムアウトエラーが発生していました。
  [Mria PR](https://github.com/emqx/mria/pull/146)
  - `$mria_rlog_sync`テーブルのコピー時に`null_copies`ストレージを使用するようにしました。この修正は現時点でEMQXには影響ありません。
  [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) クラスターから離脱済みノードに設定更新操作を同期しようとした際の問題を修正しました。

- [#11150](https://github.com/emqx/emqx/pull/11150) emqx_pskアプリ起動時にMriaテーブルを待機し、初期PSKファイルがなくてもPSKデータがレプリカントノードに同期されるようにしました。

- [#11151](https://github.com/emqx/emqx/pull/11151) MySQLコネクターを独立したErlangアプリケーションにリファクタリングし、コード構造を改善しました。

- [#11158](https://github.com/emqx/emqx/pull/11158) retainerのmnesiaバックエンド起動時にMriaテーブルを待機し、クラスター参加時のエラーを回避しました。

- [#11162](https://github.com/emqx/emqx/pull/11162) Webhookブリッジの非同期クエリモードで4XXや5XXのHTTPステータスコードが成功として扱われる問題を修正しました。

- [#11164](https://github.com/emqx/emqx/pull/11164) ルールアクションメッセージからデータ抽出する際に、`json_decode(payload)`を呼ばずにネストされたプレースホルダー（例：`${payload.a.b.c}`）をサポートする機能を再導入しました。

- [#11172](https://github.com/emqx/emqx/pull/11172) 以下の場合にルールエンジンSQLの`payload`フィールドが重複する問題を修正しました：

  - `foreach`文で`as`サブ式なしかつ全フィールド選択（`*`または`do`省略）
  
    例：`FOREACH payload.sensors FROM "t/#"`
  
  - `payload`フィールドと全フィールドを同時に選択
  
    例：`SELECT payload.sensors, * FROM "t/#"`

- [#11174](https://github.com/emqx/emqx/pull/11174) ingress MQTTブリッジからの`server`キーのエンコードを修正しました。修正前はASCIIコードの整数リストとしてエンコードされていました。

- [#11184](https://github.com/emqx/emqx/pull/11184) `mqtt.max_packet_size`の設定値の最大値をプロトコルで定義された256MBに制限しました。

- [#11192](https://github.com/emqx/emqx/pull/11192) atom型使用時に不正なHOCONファイルが生成される問題を修正し、HOCONファイルのキーやラテン1文字列の不要な`"`を削除しました。

- [#11195](https://github.com/emqx/emqx/pull/11195) REST APIでStompゲートウェイの特定クライアントに対して重複サブスクリプションが作成される問題を修正しました。

- [#11206](https://github.com/emqx/emqx/pull/11206) CoAPクライアントの接続モードで`username`と`password`パラメータをオプションにしました。

- [#11208](https://github.com/emqx/emqx/pull/11208) LwM2Mクライアントの異常なデータ統計問題を修正しました。

- [#11211](https://github.com/emqx/emqx/pull/11211) HTTP APIの存在しないリソースに対する`DELETE`操作が一貫して`404`を返すようにしました。

- [#11214](https://github.com/emqx/emqx/pull/11214) ノードがクラスターに参加した際に設定同期が正しく行われない問題を修正しました。

- [#11229](https://github.com/emqx/emqx/pull/11229) `emqx ctl conf load`で設定変更後にプラグインが起動/停止できなくなる問題を修正しました。

- [#11237](https://github.com/emqx/emqx/pull/11237) `/prometheus` APIの`headers`のデフォルト値をリストではなくマップに修正しました。

- [#11250](https://github.com/emqx/emqx/pull/11250) WebSocketパケット内のMQTTパケットの順序が逆転するバグを修正しました。

- [#11271](https://github.com/emqx/emqx/pull/11271) REST APIおよび設定でパーセンテージ型の範囲を0%〜100%に統一しました。例：`sysom.os.sysmem_high_watermark=101%`は無効です。

- [#11272](https://github.com/emqx/emqx/pull/11272) ログの誤字を修正し、異常な`PUBREL`パケットが誤って`pubrec`と表示されていた問題を修正しました。

- [#11281](https://github.com/emqx/emqx/pull/11281) 特殊な共有サブスクリプショントピックプレフィックス`$queue/`のサポートを復元しました。

- [#11294](https://github.com/emqx/emqx/pull/11294) `emqx ctl cluster join`、`leave`、`status`コマンドを修正しました。

- [#11306](https://github.com/emqx/emqx/pull/11306) ルールアクションメトリクスでドロップされたリクエストがカウントされない不整合を修正しました。

- [#11309](https://github.com/emqx/emqx/pull/11309) EMQXアプリケーションの起動順序を改善し、ビルドスクリプトを簡素化しコード再利用性を向上しました。

- [#11322](https://github.com/emqx/emqx/pull/11322) EMQXバックアップファイルから追加設定をインポート可能にしました（`emqx ctl import`コマンド）：

  - rule_engine（以前はバグによりインポートされていませんでした）
  - topic_metrics（以前は未実装）
  - slow_subs（以前は未実装）

- [#10645](https://github.com/emqx/emqx/pull/10645) Oracle Database、PostgreSQL、MySQL、Kafka Producerデータブリッジのヘルスチェックを強化し、対象テーブル/トピックの存在を確認するようにしました。

- [#11107](https://github.com/emqx/emqx/pull/11107) MongoDBブリッジのヘルスチェックで失敗理由を返すようにしました。

- [#11139](https://github.com/emqx/emqx/pull/11139) Redisブリッジを独立したErlangアプリケーションにリファクタリングし、コード構造を改善し保守性を向上しました。

- [#11151](https://github.com/emqx/emqx/pull/11151) MySQLブリッジを独立したErlangアプリケーションにリファクタリングし、コード構造を改善し保守性を向上しました。

- [#11163](https://github.com/emqx/emqx/pull/11163) MongoDBブリッジの`topology.pool_size`を非表示にし、混乱を避けるため1に固定しました。

- [#11175](https://github.com/emqx/emqx/pull/11175) MySQL接続時に存在しないホスト名を指定した場合、REST APIで503ではなく400エラーを返すようにしました。

- [#11198](https://github.com/emqx/emqx/pull/11198) レプリカントノードでのグローバルリバランス状態評価を修正しました。以前は`/api/v5/load_rebalance/global_status` APIが不完全な結果を返すことがありました。

- [#11223](https://github.com/emqx/emqx/pull/11223) InfluxDBブリッジで小数点と整数が混在するフィールドがあるとInflux Line Protocolのシリアライズに失敗し書き込みができなくなる問題を修正しました。

  参考：[InfluxDB v2.7 Line-Protocol](https://docs.influxdata.com/influxdb/v2.7/reference/syntax/line-protocol/#float)

- [#11225](https://github.com/emqx/emqx/pull/11225) PostgreSQL/Timescale/MatrixDBブリッジ設定の`username`フィールドを必須にしました。

- [#11242](https://github.com/emqx/emqx/pull/11242) ノードがクラスターに参加した際にemqx_ee_schema_registryを再起動するようにしました。これはMriaテーブルを使用しているため、関連するMriaシャードプロセスを起動し、Core/Replicantモードで正しく動作させるためです。

- [#11266](https://github.com/emqx/emqx/pull/11266) TDengineの`insert`構文のサポートを修正・改善しました：

  1. テンプレートで複数テーブルへの挿入をサポートしました。

     例：

     ```
     insert into table_1 values (${ts}, '${id}', '${topic}')
     table_2 values (${ts}, '${id}', '${topic}')
     ```

  2. プレフィックス/サフィックスとプレースホルダーの混在をサポートしました。

     例：

     ```
     insert into table_${topic} values (${ts}, '${id}', '${topic}')
     ```

     注意：これは破壊的変更です。以前は文字列型の値は自動的に引用符で囲まれていましたが、現在は明示的に引用符で囲む必要があります。

     例：

     ```
     insert into table values (${ts}, '${a_string}')
     ```

- [#11307](https://github.com/emqx/emqx/pull/11307) Oracleブリッジのテーブル存在チェックでより親切なメッセージを返すように修正しました。

- [#11316](https://github.com/emqx/emqx/pull/11316) OracleブリッジでPool Size値が考慮されない問題を修正しました。

- [#11326](https://github.com/emqx/emqx/pull/11326) Oracleブリッジのテーブル検証時のエラー返却チェックを修正しました。

## 5.1.0

*リリース日: 2023-06-21*

### 強化

- [#11035](https://github.com/emqx/emqx/pull/11035) Cassandraドライバーをアップグレードし、データブリッジログでのユーザー名およびパスワード漏洩を防止しました。

- [#10584](https://github.com/emqx/emqx/pull/10584) SSL通信のログレベル設定を追加しました。

- [#10678](https://github.com/emqx/emqx/pull/10678) カウンターインクリメント呼び出しを最適化し、インクリメントがゼロの場合は処理を省略するようにしました。

- [#10690](https://github.com/emqx/emqx/pull/10690) Webhookブリッジにリトライ機構を追加しスループットを向上させました。リクエスト失敗時にバッファリング層をブロックせずにリトライを行います。

- [#10702](https://github.com/emqx/emqx/pull/10702) 新しい設定オプション`keepalive_multiplier`を導入し、旧`keepalive_backoff`を非推奨にしました。これによりEMQXはクライアントのKeepaliveタイムアウト状態を「クライアント要求Keepalive間隔」×`keepalive_multiplier`で判定します。

- [#10698](https://github.com/emqx/emqx/pull/10698) ランタイム中の設定アクセス時のメモリ使用を最適化しました。

- [#10778](https://github.com/emqx/emqx/pull/10778) Pulsarプロデューサーブリッジをリファクタリングし、初期化フェーズでのクラッシュ時のリソースリークを防止しました。

- [#10813](https://github.com/emqx/emqx/pull/10813) Kafkaプロデューサーおよびコンシューマーブリッジをリファクタリングし、初期化フェーズでのクラッシュ時のリソースリークを防止しました。

- [#10858](https://github.com/emqx/emqx/pull/10858) ルールエンジンSQL言語にユーティリティ関数`timezone_to_offset_seconds/1`を追加しました。これはタイムゾーン文字列（例："+02:00"、"Z"、"local"）を秒単位のオフセットに変換します。

- [#10841](https://github.com/emqx/emqx/pull/10841) KafkaおよびPulsarプロデューサーブリッジで`key_dispatch`戦略選択時にメッセージキーが空でないことを検証するスキーマバリデーションを追加しました。

- [#10754](https://github.com/emqx/emqx/pull/10754) MQTTブリッジを強化し、接続プーリングと並列処理を活用してスループットを大幅に向上させました。これにより単一のMQTTブリッジはリモートブローカーへの接続に複数の`clientid`プールを使用します。

- [#10782](https://github.com/emqx/emqx/pull/10782) retainer設定に新しい`deliver_rate`オプションを追加し、セッションごとの最大配信レートを制限可能にしました。

- [#10877](https://github.com/emqx/emqx/pull/10877) RocketMQドライバーをアップグレードし、機密データのセキュリティを強化しました。

- [#10598](https://github.com/emqx/emqx/pull/10598) ExProtoにUnary型のコールバックメソッドを追加し、メッセージの順序乱れ問題を回避しました。

- [#10895](https://github.com/emqx/emqx/pull/10895) 多くのブリッジをリファクタリングし、初期化フェーズでのクラッシュ時のリソースリークを防止しました。

- [#10790](https://github.com/emqx/emqx/pull/10790) ゾーンごとの設定読み取りのオーバーヘッドを削減し、ランタイム中の設定アクセスを最適化しました。

- [#10892](https://github.com/emqx/emqx/pull/10892) Oracle Databaseブリッジ作成時にSIDまたはサービス名の設定を必須にしました。

- [#10910](https://github.com/emqx/emqx/pull/10910) データブリッジリソースの`auto_restart_interval`を非推奨にし、`health_check_interval`に統合しました。また`request_timeout`を`request_ttl`に名称変更し、デフォルト値を15秒から45秒に変更しました。これにより障害時のデータブリッジ回復挙動が一貫化されます。

- [#10929](https://github.com/emqx/emqx/pull/10929) Erlang/OTPを25.3.2-1にアップグレードしました。

- [#10909](https://github.com/emqx/emqx/pull/10909) ゲートウェイ用の非推奨HTTP APIを削除しました。

- [#10908](https://github.com/emqx/emqx/pull/10908) RocketMQブリッジをリファクタリングし、初期化フェーズでのリソースリークを防止しました。

- [#10924](https://github.com/emqx/emqx/pull/10924) Influxdbブリッジコネクターをリファクタリングし、初期化フェーズでのリソースリークを防止しました。

- [#10944](https://github.com/emqx/emqx/pull/10944) GCP PubSubブリッジを改善し、ノード再起動後にメッセージ送信に失敗する可能性を低減しました。

- [#10933](https://github.com/emqx/emqx/pull/10933) MQTT/TCPおよびMQTT/SSLリスナーでTCPキープアライブ設定をサポートしました。

- [#10948](https://github.com/emqx/emqx/pull/10948) 一部HTTP APIに`live_connections`フィールドを追加しました。対象は以下の通りです：

  - `/monitor_current`, `/monitor_current/nodes/{node}`
  - `/monitor/nodes/{node}`, `/monitor`
  - `/node/{node}`, `/nodes`

- [#10941](https://github.com/emqx/emqx/pull/10941) `prometheus.vm_dist_collector=disabled`設定時のPrometheusメトリクス収集速度を改善し、`erlang_vm_statistics_run_queues_length_total`メトリクスを`erlang_vm_statistics_run_queues_length`に名称変更しました。

- [#10985](https://github.com/emqx/emqx/pull/10985) `emqx ctl`コマンドの`cluster_call`を`conf cluster_sync`に名称変更しました。旧コマンドは引き続き使用可能ですが、使用情報には含まれません。

- [#10988](https://github.com/emqx/emqx/pull/10988) データブリッジ作成失敗時のログセキュリティを改善し、機密データを常にマスクするようにしました。

- [#10926](https://github.com/emqx/emqx/pull/10926) リスナーの状態フラグに`enable`を追加し、旧`enabled`と併用可能にしました。これにより他の状態フラグとの命名の一貫性が向上します。

- [#10970](https://github.com/emqx/emqx/pull/10970) Kafkaプロデューサーブリッジに`query_mode`パラメータを追加し、非同期モードと同期モードを切り替え可能にしました。デフォルトは非同期モードです。

- [#10676](https://github.com/emqx/emqx/pull/10676) CLIコマンド`emqx ctl export`および`emqx ctl import`を追加し、構成およびユーザーデータのエクスポート/インポートを可能にしました。これにより稼働中のEMQXクラスターから設定と組み込みデータをエクスポートし、同一または別のクラスターにインポートできます。

- [#11003](https://github.com/emqx/emqx/pull/11003) KafkaブリッジでTCPキープアライブ設定を追加しました。

- [#10961](https://github.com/emqx/emqx/pull/10961) ゲートウェイリスナーの`max_connections`設定に無制限（infinity）を許可し、最大接続数無制限をサポートしました。

- [#11019](https://github.com/emqx/emqx/pull/11019) JWTログのセキュリティを改善し、出力前にマスクするようにしました。

- [#11024](https://github.com/emqx/emqx/pull/11024) Pulsarプロデューサーブリッジ作成/更新時に`connecting`状態が表示される可能性を低減する小規模改善を行いました。

- [#11034](https://github.com/emqx/emqx/pull/11034) ブローカー設定を非表示にし、`broker.shared_subscription_strategy`を`mqtt.shared_subscription_strategy`に変更しました。これは`mqtt`に属する設定です。

- [#11045](https://github.com/emqx/emqx/pull/11045) バージョン`5.1.0`でリスナーの認証およびゾーン関連APIを正式に削除しました。

- [#11062](https://github.com/emqx/emqx/pull/11062) 設定キー`log.file.to`を`log.file.path`に名称変更しました。

### バグ修正

- [#11018](https://github.com/emqx/emqx/pull/11018) Stompゲートウェイの複数の問題を修正しました：

  - `is_superuser`が正しく動作しない問題を修正
  - メッセージ配信時にマウントポイントが削除されない問題を修正
  - メッセージまたはサブスクリプション要求が失敗した場合、ERRORメッセージ送信後にStompクライアントを即時切断するように修正

- [#11051](https://github.com/emqx/emqx/pull/11051) 証明書の`depth`（リスナーSSLオプション）が非負整数であることを検証するバリデーションを追加しました。

- [#10563](https://github.com/emqx/emqx/pull/10563) サブスクリプションの`no_local`フラグが正しく機能しない問題を修正しました。

- [#10653](https://github.com/emqx/emqx/pull/10653) ゲートウェイ認証のTLS証明書およびキーをデータディレクトリに保存し、メモリリーク問題を修正しました。

- [#10682](https://github.com/emqx/emqx/pull/10682) 遺言メッセージのタイムスタンプがセッション作成時に誤って割り当てられていた問題を修正し、切断時刻を正しく割り当てるようにしました。

- [#10701](https://github.com/emqx/emqx/pull/10701) Amazon Linux 2用RPMパッケージがopenssl 1.0でビルドされたErlang/OTPを使用していたためTLS v1.3をサポートしていなかった問題を修正しました。

- [#10677](https://github.com/emqx/emqx/pull/10677) ルールAPIで存在しないルール削除時に404 HTTPエラーコードを返すように修正しました。

- [#10715](https://github.com/emqx/emqx/pull/10715) `client.connected`フックでクライアント証明書を取得可能にしました。以前は接続確立後にメモリ節約のため削除されていました。

- [#10737](https://github.com/emqx/emqx/pull/10737) GatewayのHTTP APIが特殊文字を含むClientIDを正しく処理できない問題を修正しました。

- [#10809](https://github.com/emqx/emqx/pull/10809) ノードシャットダウンや再起動時に発生する`** ERROR ** Mnesia post_commit hook failed: error:badarg`エラーメッセージを修正しました。Mriaプルリクエスト：[https://github.com/emqx/mria/pull/142](https://github.com/emqx/mria/pull/142)

- [#10807](https://github.com/emqx/emqx/pull/10807) ライセンスチェックに関するデバッグログの出力頻度を削減しました。

- [#10818](https://github.com/emqx/emqx/pull/10818) `emqx_ctl traces`コマンドの`traces start`が一部フィルターで正しく動作しない問題を修正しました。

- [#10600](https://github.com/emqx/emqx/pull/10600) emqx_statsdアプリケーションを削除しました。

- [#10820](https://github.com/emqx/emqx/pull/10820) クラスターライセンス更新後に新規ノードが古いライセンスを使用し続ける問題を修正しました。例：emqx-operatorでスケールアップ時に発生。

- [#10851](https://github.com/emqx/emqx/pull/10851) 不正APIログで機密データをマスクしました。

- [#10884](https://github.com/emqx/emqx/pull/10884) ノードがクラスター参加中にルール情報やメトリクス取得でクラッシュする問題を修正しました。

- [#10887](https://github.com/emqx/emqx/pull/10887) ブリッジへのリクエストが長時間リトライされる可能性がある問題を修正しました。これは低スループット環境でバッファリング層が接続性やドライバー問題を検知するのに時間がかかっていたためです。

- [#10878](https://github.com/emqx/emqx/pull/10878) RabbitMQブリッジの脆弱性を修正し、パスワードがログファイルに露出する問題を防止しました。

- [#10871](https://github.com/emqx/emqx/pull/10871) CoAP接続切断後もダッシュボードで接続が存在すると表示され、削除やメッセージ投稿リクエストが反映されない問題を修正しました。

- [#10880](https://github.com/emqx/emqx/pull/10880) 複数クライアントを一括キックアウトする新しいREST API `POST /clients/kickout/bulk`を追加しました。

- [#10913](https://github.com/emqx/emqx/pull/10913) ノードがクラスターから離脱後もプラグインステータスREST APIにクラスター内ノードの状態が含まれる問題を修正しました。

- [#10923](https://github.com/emqx/emqx/pull/10923) チャンネル情報登録の競合状態を修正しました。高負荷時にクライアントが切断されてもダッシュボードのクライアント一覧に残る問題の原因の一つです。

- [#10930](https://github.com/emqx/emqx/pull/10930) 期間データ型のスキーマバリデーションを追加し、不正値の使用によるクラッシュを防止しました。

- [#10952](https://github.com/emqx/emqx/pull/10952) リスナーSSLオプションで`verify = verify_none`の場合に`fail_if_no_peer_cert`を有効にできないようにしました。これらのオプションの組み合わせは接続エラーを引き起こしていました。リスナー作成・更新時に検証を行います。

-   注意: 以前許可されていた `fail_if_no_peer_cert = true` と `verify = verify_none` を含む古いリスナー設定は、この修正を適用後に読み込みに失敗し、手動で修正する必要があります。
-   [#10951](https://github.com/emqx/emqx/pull/10951) MQTT-SN ゲートウェイで、`mountpoint` がメッセージパブリッシュに反映されなかった問題を修正しました。
-   [#10943](https://github.com/emqx/emqx/pull/10943) クラスター検出のための UDP マルチキャスト機構を非推奨にしました。
    この機能は主に実際の本番利用がほとんどないため、5.0 から非推奨化が計画されていました。5.1 ではまだコードは削除されていませんが、ドキュメント上のインターフェースは格下げされています。
-   [#10902](https://github.com/emqx/emqx/pull/10902) 自ノードより新しいバージョンのノードから `cluster.hocon` ファイルを同期しないようにしました。
    クラスターのローリングアップグレード中に、古いバージョンのノードが何らかの理由で再起動する際、新しいバージョンのノードから `cluster.hocon` をコピーすると起動に失敗する可能性があります。この修正により、古いバージョンのノードは新しいバージョンのノードから `cluster.hocon` をコピーせず、自身のファイルを使って起動します。
-   [#10967](https://github.com/emqx/emqx/pull/10967) リバランス API のエラーメッセージのフォーマットを修正しました。以前は内部の Erlang 構造体の不明瞭なダンプが表示されていました。
    ノード退避 CLI と API に `wait_health_check` オプションを追加しました。これはノードが「不健康状態」を報告するが実際の退避を開始しない時間間隔です。ロードバランサー（存在する場合）が退避ノードをバランシングから除外し、クライアントの（再）接続を退避ノードに転送しないようにするために必要です。
-   [#10911](https://github.com/emqx/emqx/pull/10911) 名前が255バイトを超えるブリッジ作成時に表示されるエラーメッセージとログをわかりやすくしました。
-   [#10983](https://github.com/emqx/emqx/pull/10983) リスナーが TLS v1.3 のみを使用する設定の場合に MQTT クライアントが TLS 接続できなかった問題を修正しました。
    問題は TLS 接続時に TLS v1.3 と互換性のないオプションを使用していたことにあります。
-   [#10977](https://github.com/emqx/emqx/pull/10977) サブスクリプション数メトリクスの更新遅延を修正し、Stomp ゲートウェイの設定問題を修正しました。
-   [#10950](https://github.com/emqx/emqx/pull/10950) MQTT-SN ゲートウェイで `enable_qos` オプションが反映されない問題を修正しました。
-   [#10999](https://github.com/emqx/emqx/pull/10999) Kafka の 'Partition Count Refresh Interval' と 'Offset Commit Interval' のスキーマ検証を変更し、許容最大値を超える値を受け付けないようにしました。
-   [#10997](https://github.com/emqx/emqx/pull/10997) ClickHouse ブリッジで、`request_ttl` が無限大に設定されていても、ClickHouse サーバーが閉じられた際にメッセージがドロップされる問題を修正しました。閉じた接続によるエラーを回復可能なエラーとして扱うようにしました。
-   [#10994](https://github.com/emqx/emqx/pull/10994) HTTP コネクターで使用される `proxy-authorization` ヘッダーをマスクし、ログファイルへの秘密情報漏洩を防止しました。
-   [#10996](https://github.com/emqx/emqx/pull/10996) 不明な HTTP/API リクエストに対しては、ダッシュボードの index.html ではなくデフォルトで 404 エラーを返すようにしました。
-   [#11005](https://github.com/emqx/emqx/pull/11005) AuthN HTTP のトレースログで `method` フィールドが正しく表示されない問題を修正しました。
-   [#11006](https://github.com/emqx/emqx/pull/11006) QUIC リスナーのデフォルト証明書ファイルパスを修正しました。
    変更前は環境変数 `${EMQX_ETC_DIR}` がプレフィックスとして付いていましたが、QUIC リスナーで使用時に展開されていませんでした。
-   [#10998](https://github.com/emqx/emqx/pull/10998) MongoDB ブリッジリソースに対して `batch_size` オプションを許可しないようにしました。MongoDB コネクターは現在バッチ処理をサポートしておらず、指定された場合は `batch_size` を強制的に 1 に設定します。
-   [#10955](https://github.com/emqx/emqx/pull/10955) MQTT-SN ゲートウェイで事前定義トピック設定の削除が機能しない問題を修正しました。
-   [#11025](https://github.com/emqx/emqx/pull/11025) Pulsar Producer ブリッジで競合状態により発生する可能性のあった `case_clause` エラーを修正しました。
-   [#11030](https://github.com/emqx/emqx/pull/11030) Listeners HTTP API 使用時のバリデーションエラー発生時のエラーメッセージを改善しました。
-   [#11033](https://github.com/emqx/emqx/pull/11033) ExProto ゲートウェイの `AuthenticateRequest` にある `mountpoint` フィールドを非推奨にしました。
    このフィールドは e4.x で導入されましたが、e5.0 では `gateway.exproto.mountpoint` による設定が提供されているため、Authenticate リクエストでの上書きは不要です。

    また、`subscriptions_max`、`inflight_max`、`mqueue_max` のデフォルト値を `infinity` に更新しました。
-   [#11040](https://github.com/emqx/emqx/pull/11040) Kafka Producer のヘルスチェック問題を修正し、Kafka ブローカーへの接続が切断された場合のメッセージ損失を防止しました。
-   [#11038](https://github.com/emqx/emqx/pull/11038) Pulsar Producer のヘルスチェック問題を修正し、Pulsar ブローカーへの接続が切断された場合のメッセージ損失を防止しました。
-   [#11042](https://github.com/emqx/emqx/pull/11042) リスナーの `max_connections` が文字列に設定されている場合に REST API `GET /listeners` がクラッシュする問題を修正しました。
-   [#11028](https://github.com/emqx/emqx/pull/11028) tlsv1.3 を含み tlsv1.2 を除外する複数 TLS バージョンのリスナー設定を禁止しました。
    このようなバージョンギャップのある TLS 設定は接続エラーを引き起こしていました。
    また、選択された TLS バージョンと互換性のない TLS オプションは破棄しログに記録します。

    注意: 上記のバージョンギャップを含む古いリスナー設定は、この修正適用後に読み込みに失敗し、手動で修正する必要があります。
-   [#11031](https://github.com/emqx/emqx/pull/11031) InfluxDB ブリッジの作成時およびステータス確認時の認証情報検証を修正しました。
-   [#11056](https://github.com/emqx/emqx/pull/11056) 新規作成したリスナーが正しく起動しない場合がある問題を修正しました。
    システムデフォルトリスナーを削除し、'default' という名前で新規追加した場合に正しく起動しませんでした。
    -   特定ノードでの設定失敗がダッシュボードの利用不可を引き起こすバグを修正しました。
-   [#11070](https://github.com/emqx/emqx/pull/11070) `cluster.autoclean` 設定項目が反映されない問題を修正しました。
-   [#11092](https://github.com/emqx/emqx/pull/11092) および [#11100](https://github.com/emqx/emqx/pull/11100) レプリカノードが `mria_lb:core_nodes()` 呼び出しのタイムアウトによりコアノードに接続できない問題を修正しました。
    関連する Mria のプルリクエスト: [https://github.com/emqx/mria/pull/143](https://github.com/emqx/mria/pull/143)

## 5.0.4

_リリース日: 2023-05-26_

### 機能強化

- [#10389](https://github.com/emqx/emqx/pull/10389) `cluster.core_nodes` と `cluster.statics.seeds` の設定フォーマットを統一しました。両方とも配列形式 `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` とカンマ区切り文字列 `"emqx1@127.0.0.1,emqx2@127.0.0.1"` の両方をサポートします。

- [#10392](https://github.com/emqx/emqx/pull/10392) フォーマット済み日付を整数のタイムスタンプに変換する新関数 `date_to_unix_ts/3` を導入しました。

  `date_to_unix_ts(TimeUnit, FormatString, InputDateTimeString)`

- [#10426](https://github.com/emqx/emqx/pull/10426) 設定優先度の仕組みを最適化し、`etc/emqx.conf` の設定変更が EMQX 再起動後に反映されない問題を修正しました。

  新しい仕組みの詳細は [Configure Override Rules](https://docs.emqx.com/en/enterprise/v5.0/configuration/configuration.html#configure-override-rules) を参照してください。

- [#10457](https://github.com/emqx/emqx/pull/10457) StatsD との統合を非推奨にしました。

- [#10458](https://github.com/emqx/emqx/pull/10458) プラグイン設定オプションのレベルを低く設定しました。ユーザーは通常ダッシュボード経由でプラグインを管理し、手動での変更は稀なためです。

- [#10491](https://github.com/emqx/emqx/pull/10491) 設定ファイル内の SSL オプションを一貫させるため、`etcd.ssl` を `etcd.ssl_options` に名称変更しました。

- [#10512](https://github.com/emqx/emqx/pull/10512) データファイル内の Unicode 文字の格納形式を改善し、Unicode 文字を格納可能にしました。例: `SELECT * FROM "t/1" WHERE clientid = "-测试专用-"`。

- [#10568](https://github.com/emqx/emqx/pull/10568) `emqx ctl listeners` コマンドに `shutdown_count` 表示を追加しました。

- [#10588](https://github.com/emqx/emqx/pull/10588) トレースログの時間精度を秒からマイクロ秒に向上しました。例: `2023-05-02T08:43:50+00:00` から `2023-05-02T08:43:50.237945+00:00` へ。

- [#10623](https://github.com/emqx/emqx/pull/10623) `force_shutdown` 設定の `max_message_queue_len` を `max_mailbox_size` に名称変更しました。旧名称はエイリアスとして残し後方互換性を維持しています。

- [#10713](https://github.com/emqx/emqx/pull/10713) Webhook の `resource_option.request_timeout` を非表示にし、`http` の `request_timeout` 値を使用するようにしました。

- [#10075](https://github.com/emqx/emqx/pull/10075) ノードのリバランス／退避機能を追加しました。詳細は [EIP doc](https://github.com/emqx/eip/blob/main/implemented/0020-node-rebalance.md) を参照してください。

- [#10378](https://github.com/emqx/emqx/pull/10378) Pulsar Producer ブリッジを実装し、現在はプロデューサーのみサポートしています。

- [#10408](https://github.com/emqx/emqx/pull/10408) ルールエンジンの SQL ライク言語に MongoDB 日付型の値を作成するための組み込み関数を3つ導入しました。

- [#10409](https://github.com/emqx/emqx/pull/10409) [#10337](#10337) スキーマレジストリで [Protocol Buffers](https://protobuf.dev/) と [Apache Avro](https://avro.apache.org/) スキーマをサポートしました。

- [#10425](https://github.com/emqx/emqx/pull/10425) OpenTSDB データブリッジを実装しました。

- [#10498](https://github.com/emqx/emqx/pull/10498) Oracle Database ブリッジを実装しました。

- [#10560](https://github.com/emqx/emqx/pull/10560) Apache IoTDB 向けのエンタープライズデータブリッジを追加しました。

- [#10417](https://github.com/emqx/emqx/pull/10417) 一時的な参照を排除し、設定項目取得のパフォーマンスを改善しました。

- [#10430](https://github.com/emqx/emqx/pull/10430) `retainer` 機能の設定を簡素化し、`flow_control` を重要でないフィールドとしてマークしました。

- [#10511](https://github.com/emqx/emqx/pull/10511) 一部リソースログのセキュリティとプライバシーを向上させ、ログ内の機密情報をマスクしました。

- [#10525](https://github.com/emqx/emqx/pull/10525) MQTT パケット処理あたりのリソース使用量を削減しました。

- [#10528](https://github.com/emqx/emqx/pull/10528) ホットコードパスのメモリフットプリントを削減しました。ホットパスはメッセージ処理、接続管理、認証、認可などの頻繁に実行されるコア機能を含みます。

- [#10591](https://github.com/emqx/emqx/pull/10591) および [#10625](https://github.com/emqx/emqx/pull/10625) リミッターの設定を改善しました。

  - リミッター設定の複雑さを軽減しました。

  - `configs/limiter` API をこのリファクタリングに合わせて更新しました。

  - リミッター設定のメモリ使用量を削減しました。

- [#10487](https://github.com/emqx/emqx/pull/10487) レートが `infinity` のリミッターインスタンスを最適化し、メモリと CPU 使用率を削減しました。

- [#10490](https://github.com/emqx/emqx/pull/10490) 以前のデフォルト接続レート制限 `1000/s` を削除しました。

- [#10077](https://github.com/emqx/emqx/pull/10077) QUIC TLS のパスワード保護証明書ファイルをサポートしました。

### バグ修正

- [#10340](https://github.com/emqx/emqx/pull/10340) `systemd` 経由で EMQX を停止した際にクラッシュログが出力される問題を修正しました。

- [#10369](https://github.com/emqx/emqx/pull/10369) 一部の EMQX ノードがダウンしている場合に `/api/v5/monitor_current` API エンドポイントで発生するエラーを修正しました。

  修正前は HTTP 500 と以下のメッセージが返されることがありました:

  `{"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...`

- [#10407](https://github.com/emqx/emqx/pull/10407) アラームシステムのクラッシュ問題を修正しました。

  - Mnesia のダーティ操作を活用し、余計な呼び出しを回避して `emqx_alarm` のパフォーマンスを向上。

  - 既にトリガーされたアラームを再活性化するために `emqx_resource_manager` を使用。

  - 新たに開発されたフェイルセーフな `emqx_alarm` API を実装し、アラームの有効化／無効化を制御。これによりアラームタイムアウトによる `emqx_resource_manager` のクラッシュを防止。

  - アラームシステムは以下の同時発生条件でクラッシュしやすい:

    - 多数のリソースが失敗し、ブリッジが繰り返しエラーでアラームをトリガーし続ける場合。

    - システムが非常に高負荷状態にある場合。

- [#10420](https://github.com/emqx/emqx/pull/10420) 認証・認可モジュールの HTTP リクエスト URL 作成時のパス処理を修正しました。

  - 外部サーバーが元の URL と正規化された URL を同等に扱うとは限らないため、不必要な URL 正規化を避けました。これにより [#10411](https://github.com/emqx/emqx/issues/10411) のようなバグを防止。

  - パスセグメントが二重に HTTP エンコードされる問題を修正。

- [#10422](https://github.com/emqx/emqx/pull/10422) 単一ノードクラスターで外部プラグインを環境変数経由で設定できなかったバグを修正。

- [#10448](https://github.com/emqx/emqx/pull/10448) e5.0.3 で導入されたリミッター設定の互換性問題を修正しました。`capacity` が `infinity` の場合、アップグレード時に破損していました。

  e5.0.3 で `capacity` を `burst` に置き換えたため、この修正で `capacity = infinity` は自動的に `burst = 0` に変換されます。

- [#10462](https://github.com/emqx/emqx/pull/10462) `broker.shared_dispatch_ack_enabled` 設定を非推奨にしました。これは切断されたクライアントの共有サブスクリプションセッションへのメッセージ配信を防ぐためのものでしたが、e5.0.0 以降は期限切れセッションの共有サブスクリプションメッセージがグループ内の他セッションに再配信されるため不要になりました。詳細は <https://github.com/emqx/emqx/pull/9104> を参照。

- [#10463](https://github.com/emqx/emqx/pull/10463) ブリッジ API のエラーハンドリングを改善しました。Webhook ブリッジの URL が無効な場合、API は '500' ではなく '400' エラーを返します。

- [#10484](https://github.com/emqx/emqx/pull/10484) ローリングアップグレード中に設定の優先度が反映されない問題を修正しました。例として、e5.0.2 で認可設定を変更し、e5.0.3 にローリングアップグレードすると認可がデフォルトに戻っていました。

- [#10495](https://github.com/emqx/emqx/pull/10495) 誤って削除されたリミッター API `/configs/limiter` を復活させました。

- [#10500](https://github.com/emqx/emqx/pull/10500) Mria に以下の修正・機能追加を行いました:

  - `mria:join/1,2` をグローバルロックで保護し、2 ノードが同時に相互参加しようとする競合を防止 [Mria PR](https://github.com/emqx/mria/pull/137)

  - 新関数 `mria:sync_transaction/4,3,2` を実装。トランザクションがローカルノードにインポートされるまで呼び出し元をブロック（ローカルノードがレプリカの場合）。通常は `mria:transaction/3,2` と同等 [Mria PR](https://github.com/emqx/mria/pull/136)

  - `mria:running_nodes/0` を最適化 [Mria PR](https://github.com/emqx/mria/pull/135)

  - レプリカノードで呼び出される `mria:ro_transaction/2` を最適化 [Mria PR](https://github.com/emqx/mria/pull/134)

- [#10518](https://github.com/emqx/emqx/pull/10518) Mria に以下の修正・機能追加を行いました:

  - mria_membership 内で `mria_rlog:role/1` を安全に呼び出し、RPC 失敗時の gen_server クラッシュを防止 [Mria PR](https://github.com/emqx/mria/pull/139)

  - 将来の拡張のために `?rlog_sync` テーブルに追加フィールドを追加 [Mria PR](https://github.com/emqx/mria/pull/138)

- [#10556](https://github.com/emqx/emqx/pull/10556) `Authorization` ヘッダーが初期化時に渡される場合、`emqx_connector_http` 内の機密データをマスクしました。

- [#10571](https://github.com/emqx/emqx/pull/10571) EMQX 停止時の不要なクラッシュレポートの出力を停止しました。

- [#10659](https://github.com/emqx/emqx/pull/10659) `sysmon.os.mem_check_interval` が無効化されている場合に EMQX が起動できない問題を修正しました。

- [#10717](https://github.com/emqx/emqx/pull/10717) inflight ウィンドウが満杯の際にバッファリングレイヤープロセスが大量の CPU を消費する問題を修正しました。

- [#10724](https://github.com/emqx/emqx/pull/10724) HTTP API ドキュメントの全エンドポイントにサマリーを追加しました（"http://<emqx_host_name>:18083/api-docs" でアクセス可能）。

- [#10726](https://github.com/emqx/emqx/pull/10726) ヘルスチェック間隔と自動再起動間隔が 1ms から 1時間の範囲をサポートするようになりました。

- [#10728](https://github.com/emqx/emqx/pull/10728) ルールエンジンの `FOREACH` - `DO` 節でエクスポートされた変数にアクセスできなかった問題を修正しました。

  ペイロード例: `{"date": "2023-05-06", "array": ["a"]}`

  SQL 文:

  `FOREACH   payload.date as date, payload.array as elem DO   date, elem FROM "t/#"  -- {"date": "2023-05-06", "array": ["a"]}`

  修正前は `FOREACH` でエクスポートされた `date` 変数が `DO` 節でアクセスできず、出力は `[{"elem": "a","date": "undefined"}]` となっていました。

- [#10742](https://github.com/emqx/emqx/pull/10742) 認可ファイルソース保存前にルールの正当性チェックを強制しました。誤ったルール保存による EMQX 再起動失敗を防止します。

- [#10743](https://github.com/emqx/emqx/pull/10743) ノードがクラスター参加中にブリッジ情報やメトリクス取得を試みるとクラッシュする問題を修正しました。

- [#10755](https://github.com/emqx/emqx/pull/10755) データブリッジリソース更新の競合状態を修正しました。

  EMQX リソース更新の「削除＋作成」処理で、ブリッジ作成に時間がかかるとダッシュボードのリクエストがタイムアウトし、作成完了前に更新が開始されるとランタイムから誤って削除される問題がありました。

  この修正により、ブリッジリソース更新の競合状態を解消し、新規リソースの正確な識別と追加を保証し、ランタイムと設定ファイルの状態の整合性を維持します。

- [#10761](https://github.com/emqx/emqx/pull/10761) ダッシュボードリスナーの SSL 証明書のデフォルト値が正しく展開されず、`verify_peer` と `cacertfile` のデフォルト設定使用時に HTTPS が利用できなかった問題を修正しました。

- [#10672](https://github.com/emqx/emqx/pull/10672) リスナーの `ssl_options` にデフォルト値がないために起動失敗する問題を修正しました。例として、`EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8089' ./bin/emqx console` コマンドでクラッシュしていました。

- [#10738](https://github.com/emqx/emqx/pull/10738) TDEngine データブリッジが「スーパーテーブル」および「自動テーブル作成」をサポートしました。修正前はテンプレートにスーパーテーブルを含む挿入が失敗していました。

  例: `insert into ${clientid} using msg TAGS (${clientid}) values (${ts},${msg})`

- [#10746](https://github.com/emqx/emqx/pull/10746) ルールエンジンのテスト API `rule_test` にイベント `$events/delivery_dropped` のサポートを追加しました。

- [#10747](https://github.com/emqx/emqx/pull/10747) バージョン 4.4 からルールエンジン関数の時間フォーマット修正を移植しました。

- [#10760](https://github.com/emqx/emqx/pull/10760) ノードがクラスター参加中にブリッジ統計ページ取得時の「内部エラー 500」を修正しました。

- [#10801](https://github.com/emqx/emqx/pull/10801) API `/topics/{topic}` と `/topics` でトピック名の二重パーセントデコードを回避しました。

- [#10817](https://github.com/emqx/emqx/pull/10817) ブリッジリソースオプション `auto_restart_interval` の設定値処理を修正し、`infinity` を設定可能にしました。

## 5.0.3

_リリース日: 2023-05-08_

### 機能強化

- [#10128](https://github.com/emqx/emqx/pull/10128) SSL MQTT リスナーの OCSP ステープリングをサポートしました。

- [#10156](https://github.com/emqx/emqx/pull/10156) 設定オーバーレイの順序を変更しました:

  新規インストール時は `emqx.conf` + 環境変数が API 更新設定 (`cluster.hocon`) の上にオーバーレイされます。

  既存バージョンからのアップグレード時（`data` ディレクトリに `cluster-override.conf` が存在する場合）は従来通り `cluster-override.conf` が `emqx.conf` + 環境変数の上にオーバーレイされます。

  `data/configs/cluster-override.conf` は非推奨とみなされ、アップグレード後は `emqx.conf` を更新して `cluster-override.conf` によって上書きされる設定を削除し、`cluster-override.conf` の設定を `cluster.hocon` に移行することを推奨します。

  アップグレード後も `local-override.conf`（存在する場合）は従来通り読み込まれますが、`emqx.conf` に統合することが推奨されます。

- [#10164](https://github.com/emqx/emqx/pull/10164) TLS MQTT リスナーの CRL チェックをサポートしました。

- [#10207](https://github.com/emqx/emqx/pull/10207) OpenAPI (swagger) ドキュメントの可読性を向上しました。以前は長文かつ翻訳されていない `summary` がありましたが、スキーマ i18n データベースの簡潔な `label` フィールドを利用するようにしました。

- [#10210](https://github.com/emqx/emqx/pull/10210) 無害なエラーレベルログを削減しました。
  以前は Mria の停止／再起動時に Mnesia コールバック（フック）失敗が時折発生していましたが、停止前にコールバックを解除するようにしました。詳細は [Mria PR](https://github.com/emqx/mria/pull/133) を参照。

- [#10224](https://github.com/emqx/emqx/pull/10224) Helm チャートで `clusterIP` をカスタマイズ可能にし、固定 IP を設定できるようにしました。

- [#10263](https://github.com/emqx/emqx/pull/10263) Elixir 式評価用のコマンド `eval-ex` を追加しました。

- [#10278](https://github.com/emqx/emqx/pull/10278) すべてのゲートウェイのディレクトリ構造をリファクタリングしました。

- [#10206](https://github.com/emqx/emqx/pull/10206) すべてのデータブリッジで非同期クエリモードをサポートしました。

  以前はリソース（ブリッジなど）のクエリモードを同期に設定すると、非同期呼び出しをサポートしていても強制的に同期呼び出しされていました。

- [#10306](https://github.com/emqx/emqx/pull/10306) 多くのブリッジで非同期クエリモードをサポートしました。

  これは [#10206](https://github.com/emqx/emqx/pull/10206) のフォローアップです。以前は Cassandra、MongoDB、MySQL、Postgres、Redis、RocketMQ、TDengine のブリッジは同期モードのみでしたが、非同期モードもサポートされました。

- [#10318](https://github.com/emqx/emqx/pull/10318) ルールエンジン SQL 言語の FROM 節で、これまでダブルクオート（"）のみ許可されていましたが、シングルクオート（'）もサポートしました。

- [#10336](https://github.com/emqx/emqx/pull/10336) ルールエンジン設定管理用の `/rule_engine` API エンドポイントを追加しました。

- [#10354](https://github.com/emqx/emqx/pull/10354) `bad max_heap_size` 設定時のエラーメッセージを詳細化しました。`message_queue_too_long` エラー発生時に現在値と最大値をログに記録します。

- [#10358](https://github.com/emqx/emqx/pull/10358) `flapping_detect/conn_congestion/stats` 設定を非表示にし、`flapping_detect.enable` を非推奨にしました。

- [#10359](https://github.com/emqx/emqx/pull/10359) メトリクスは API ハンドラーが使用しない箇所で暗黙的に収集されなくなりました。代わりに別のバックプレーン RPC がクラスター全体のメトリクスを収集します。

- [#10373](https://github.com/emqx/emqx/pull/10373) `trace.payload_encode` 設定を非推奨にし、HTTP API でトレース作成時に `payload_encode=[text,hidden,hex]` オプションを追加しました。

- [#10381](https://github.com/emqx/emqx/pull/10381) `auto_subscribe` 設定項目を非表示にし、HTTP API 経由でのみ変更可能にしました。

- [#10391](https://github.com/emqx/emqx/pull/10391) 設定ファイルを簡素化するため、多数の高度なオプションを非表示にしました。

  対象は `rewrite`、`topic_metric`、`persistent_session_store`、`overload_protection`、`flapping_detect`、`conn_congestion`、`stats`、`auto_subscribe`、`broker_perf`、`shared_subscription_group`、`slow_subs`、`ssl_options.user_lookup_fun`、および `node` と `dashboard` セクションの一部高度項目です。関連 PR は [#10358](https://github.com/emqx/emqx/pull/10358)、[#10381](https://github.com/emqx/emqx/pull/10381)、[#10385](https://github.com/emqx/emqx/pull/10385) を参照。

- [#10404](https://github.com/emqx/emqx/pull/10404) バッファワーカーのデフォルトキューモードを `memory_only` に変更しました。変更前は `volatile_offload` で、高負荷時にディスク操作が多発しパフォーマンスが大幅に低下していました。

- [#10140](https://github.com/emqx/emqx/pull/10140) Cassandra をブリッジの新バックエンドとして統合しました。現時点では Cassandra バージョン 3.x のみサポートし、4.x は未対応です。

- [#10143](https://github.com/emqx/emqx/pull/10143) RocketMQ データ統合ブリッジを追加しました。

- [#10165](https://github.com/emqx/emqx/pull/10165) InfluxDB データブリッジの `write_syntax` でエスケープされた特殊文字をサポートしました。InfluxDB ラインプロトコルに準拠した文字列要素の使用が可能です。

- [#10211](https://github.com/emqx/emqx/pull/10211) `broker.broker_perf` 設定と API ドキュメントを非表示にしました。`route_lock_type` と `trie_compaction` は使用頻度が低く、効果を得るにはクラスター全体の再起動が必要なため、ユーザーに公開しない方針です。詳細は <https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions> を参照。

- [#10294](https://github.com/emqx/emqx/pull/10294) MongoDB ブリッジ設定で `${field}` 構文を使い、メッセージ内のフィールドを参照できるようにしました。これにより挿入先コレクションを動的に選択可能です。

- [#10363](https://github.com/emqx/emqx/pull/10363) Microsoft SQL Server ブリッジを実装しました。

- [#10573](https://github.com/emqx/emqx/pull/10573) 同期クエリモード使用時の Webhook ブリッジのパフォーマンスを改善しました。バッチ処理なし設定の他のブリッジのパフォーマンスも向上する見込みです。

### バグ修正

- [#10145](https://github.com/emqx/emqx/pull/10145) 内部ヘルスチェックがエラー状態の場合、`GET /bridges/:id` のレスポンスに `status_reason` フィールドを追加しました。ブリッジが `disconnected` 状態の場合にエラー理由を含み、失敗したブリッジのアラームメッセージにも同様のエラー情報を含めます。

- [#10172](https://github.com/emqx/emqx/pull/10172) デフォルト ACL ルールの正規表現を修正し、ユーザー名（dashboard）で `$SYS/#` のサブスクライブ指定が可能になりました。

- [#10174](https://github.com/emqx/emqx/pull/10174) ライブラリ `esockd` を 5.9.4 から 5.9.6 にアップグレードしました。プロキシプロトコルヘッダー送信前に接続が切断された際の不要なエラーレベルログを修正。

- [#10195](https://github.com/emqx/emqx/pull/10195) API スキーマの説明に生の HTML を含む場合にラベルを追加し、生成ドキュメントのフォーマット崩れを防止しました。

- [#10196](https://github.com/emqx/emqx/pull/10196) 生成されるオンラインドキュメントのメニューで使用されるスキーマのサマリーと説明を小文字化しました。

- [#10209](https://github.com/emqx/emqx/pull/10209) 禁止クライアントをキックアウトする際に遺言メッセージ（LWT）がパブリッシュされるバグを修正しました。

- [#10225](https://github.com/emqx/emqx/pull/10225) プラグイン名が既存プラグイン名の先頭と一致する場合でもインストールを許可しました。例: `emqx_plugin_template_a` がインストール済みでも `emqx_plugin_template` のインストールをブロックしません。

- [#10226](https://github.com/emqx/emqx/pull/10226) `/bridges` API のバリデーションエラーを適切に処理し、`400` を返すようにしました（以前は `500`）。

- [#10242](https://github.com/emqx/emqx/pull/10242) ログデータのフィールド名衝突を修正しました。修正前は一部デバッグログで誤った Erlang PID が報告され、セッションテイクオーバーのトラブルシューティングに影響していました。

- [#10257](https://github.com/emqx/emqx/pull/10257) LwM2M ゲートウェイで `auto_observe` が機能しなかった問題を修正しました。

  修正前はトークンなしで `OBSERVE` リクエストが送信され、LwM2M クライアントが処理できず失敗していました。

  修正後はクライアントが持つリソースリストを正しくオブザーブし、未知のリソースは無視し、以下の警告ログを出力します:

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) 起動失敗時のログ出力を改善しました。設定ファイル破損による起動失敗時の過剰なログ出力とクラッシュダンプファイル生成を抑制します。

- [#10297](https://github.com/emqx/emqx/pull/10297) `eval` コマンドを v4 と後方互換にし、Elixir ノードでも Erlang 式のみ評価するようにしました。Elixir 式は `eval-ex` コマンドを使用してください。

- [#10300](https://github.com/emqx/emqx/pull/10300) Elixir ビルドでプラグインが環境変数経由で設定できなかった問題を修正しました。

- [#10315](https://github.com/emqx/emqx/pull/10315) `/mqtt/delayed/messages` API の `limit` と `page` パラメータチェックでのクラッシュを修正しました。

- [#10317](https://github.com/emqx/emqx/pull/10317) リスナー単位の認証情報を十分な検証前に公開しないようにしました。

- [#10323](https://github.com/emqx/emqx/pull/10323) セキュリティ上の理由から、API 例のパスワードフィールド値を `******` に置き換えました。

- [#10410](https://github.com/emqx/emqx/pull/10410) emqx.conf にゲートウェイ設定がある場合の設定チェック失敗を修正しました。
  この問題は v5.0.22 の [#10278](https://github.com/emqx/emqx/pull/10278) で導入され、起動時の設定チェックが抜けていました。

- [#10533](https://github.com/emqx/emqx/pull/10533) ログに無害なノイズが発生する問題を修正しました。

  ブリッジへの遅い同期呼び出し時に、遅延応答が既に応答を期待していない接続プロセスに送信され、エラーログを出力していました。

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  これらのログは無害ですが、不要にユーザーを混乱させる可能性がありました。

- [#10449](https://github.com/emqx/emqx/pull/10449) 認証 HTTP (`authn_http`) 作成時の `ssl_options` と `header` 設定を検証し、不正な SSL 設定で作成は成功しても認証全体が使用不能になる問題を修正しました。

- [#10548](https://github.com/emqx/emqx/pull/10548) HTTP ドライバーの競合状態を修正し、リクエストの再試行ではなくエラーとなっていた問題を解決しました。
  関連修正はドライバーの [emqx/ehttpc#45](https://github.com/emqx/ehttpc/pull/45) を参照。

- [#10201](https://github.com/emqx/emqx/pull/10201) TDengine データブリッジで SQL テンプレートから冗長なデータベース名を削除しました。

- [#10270](https://github.com/emqx/emqx/pull/10270) ClickHouse データブリッジで設定ダイアログのテストボタン押下時のエラーメッセージを改善しました。

- [#10324](https://github.com/emqx/emqx/pull/10324) ダッシュボード経由で誤設定された ClickHouse ブリッジの再接続試行時にエラーメッセージが表示されなかった問題を修正しました。

- [#10438](https://github.com/emqx/emqx/pull/10438) DynamoDB データブリッジの設定項目用語誤りを修正しました:

  - `database` を `table` に変更

  - `username` を `aws_access_key_id` に変更

  - `password` を `aws_secret_access_key` に変更

## 5.0.2

_リリース日: 2023-04-12_

### 機能強化

- [#10022](https://github.com/emqx/emqx/pull/10022) Rocky Linux 9（Red Hat Enterprise Linux 9 互換）および Intel プラットフォーム向け macOS 12 のインストールパッケージをリリースしました。

- [#10139](https://github.com/emqx/emqx/pull/10139) EMQX Helm Chart に `extraVolumeMounts` を追加し、ユーザーが ACL ルールファイルなどを EMQX インスタンスにマウント可能にしました（詳細は [#9052](https://github.com/emqx/emqx/issues/9052) を参照）。

- [#9893](https://github.com/emqx/emqx/pull/9893) `clean_start=false` フラグで接続した場合、ブラックリスト機能で禁止されたクライアントがパブリッシュしたメッセージをセッション内でフィルタリングするようにしました。
  以前は禁止クライアントのメッセージがサブスクライバーに配信されていました。

- [#9986](https://github.com/emqx/emqx/pull/9986) Helm チャートに MQTT イングレスを追加し、不要な mgmt 参照を削除しました。

- [#9564](https://github.com/emqx/emqx/pull/9564) Kafka コンシューマーブリッジを実装し、Kafka からメッセージを消費して MQTT トピックにパブリッシュ可能にしました。

- [#9881](https://github.com/emqx/emqx/pull/9881) InfluxDB 接続のヘルスチェック関連のエラーログを改善しました。

- [#9985](https://github.com/emqx/emqx/pull/9985) ClickHouse データブリッジを実装しました。

- [#10123](https://github.com/emqx/emqx/pull/10123) `/bridges` API のパフォーマンスを改善しました。
  クラスター内ノード数が多いかノードが高負荷の場合にリクエストタイムアウトが発生していました。

- [#9998](https://github.com/emqx/emqx/pull/9998) クライアント認証用 HTTP サービスのエラーログでリクエストボディをマスクし、セキュリティを強化しました。

- [#10026](https://github.com/emqx/emqx/pull/10026) メトリクスは `/bridges/:id/metrics` エンドポイントのみで公開され、他の API 操作では返されなくなりました。

- [#10052](https://github.com/emqx/emqx/pull/10052) デーモンモードの起動失敗ログを改善しました。

### バグ修正

- [#10013](https://github.com/emqx/emqx/pull/10013) `/gateways/:name/clients` API のエラーケースの戻り値構造を修正しました。

- [#10014](https://github.com/emqx/emqx/pull/10014) 存在しないノードに対する `/monitor(_current)/nodes/:node` API が `400` ではなく `404` を返すようにしました。

- [#10027](https://github.com/emqx/emqx/pull/10027) Docker 環境で環境変数 `EMQX_NODE__NAME` によるノード名設定を許可しました。

- [#10050](https://github.com/emqx/emqx/pull/10050) ブリッジ API が存在しないリソースに対して一貫して `404` ステータスコードを返すようにしました。

- [#10055](https://github.com/emqx/emqx/pull/10055) 設定パラメータ `mqtt.max_awaiting_rel` が機能していなかった問題を修正しました。

- [#10056](https://github.com/emqx/emqx/pull/10056) `/bridges` API のステータスコードを修正しました。
  依存するルールが有効なデータブリッジを削除しようとした場合に `403` ではなく `400` を返します。
  データブリッジが有効でない状態で操作（開始・停止・再起動）を呼び出した場合も `403` ではなく `400` を返します。

- [#10066](https://github.com/emqx/emqx/pull/10066) `/briges_probe` と `[/node/:node]/bridges/:id/:operation` API 呼び出し時のエラーメッセージを読みやすく改善し、HTTP ステータスコードを `500` から `400` に変更しました。

- [#10074](https://github.com/emqx/emqx/pull/10074) `PUT /authorization/sources/:type` のリクエストボディ内の `type` と URL パラメータの `type` が一致するか検証するようにしました。

- [#10079](https://github.com/emqx/emqx/pull/10079) `shared_subscription_strategy` の説明誤りを修正しました。

- [#10085](https://github.com/emqx/emqx/pull/10085) `/authorization/sources/:source[/*]` で存在しないソースへのリクエストに対し、一貫して `404` を返すようにしました。

- [#10098](https://github.com/emqx/emqx/pull/10098) MongoDB 認可設定時に MongoDB コネクターがクラッシュする問題を修正しました。

- [#10100](https://github.com/emqx/emqx/pull/10100) 強化認証を使用するクライアントで、認証メッセージが遅延または失われた場合にクライアントプロセスがクラッシュする問題を修正しました。

- [#10107](https://github.com/emqx/emqx/pull/10107) ブリッジ API 操作で不明な `bridge-id` の場合に `400` ではなく `404` を返すようにしました。

- [#10117](https://github.com/emqx/emqx/pull/10117) クラスター参加中のノードが他ノードでインストール済みのプラグインを持たない場合に発生するエラーを修正しました。参加ノードは必要なプラグインを他ノードからコピーします。

- [#10118](https://github.com/emqx/emqx/pull/10118) EMQX レプリカノードのクラスター手動参加に関する問題を修正しました。

- [#10119](https://github.com/emqx/emqx/pull/10119) `statsd.server` が空文字列の場合のクラッシュを修正しました。

- [#10124](https://github.com/emqx/emqx/pull/10124) MongoDB のデフォルトハートビート間隔を増加し、MongoDB ログファイルへの過剰なログ出力リスクを低減しました。

- [#10130](https://github.com/emqx/emqx/pull/10130) ダッシュボードで環境変数由来の値が文字化けする問題を修正しました。

- [#10132](https://github.com/emqx/emqx/pull/10132) `systemctl stop emqx` コマンド実行時に発生する一部エラーログを修正しました。修正前は `jq` と `os_mon` アプリケーションが適切に停止されていませんでした。

- [#10144](https://github.com/emqx/emqx/pull/10144) emqx CLI が読み取り専用の emqx ディレクトリで Erlang cookie を設定できなかった問題を修正しました。

- [#10154](https://github.com/emqx/emqx/pull/10154) ブリッジとコネクターのデフォルト `resume_interval` を `health_check_interval` と `request_timeout / 3` の最小値に変更し、リクエストタイムアウト問題を解決しました。

- [#10157](https://github.com/emqx/emqx/pull/10157) 新規リスナー作成時にデフォルトレートリミット設定が正しく適用されない問題を修正しました。

- [#10237](https://github.com/emqx/emqx/pull/10237) `/nodes/:node[/metrics|/stats]` API で不明なノード名に対し `404` を返すようにしました。

- [#10251](https://github.com/emqx/emqx/pull/10251) 使用中のイングレス型ブリッジ削除時にルール依存関係が通知されない問題を修正しました。

- [#10313](https://github.com/emqx/emqx/pull/10313) コアノードまたはレプリカノード起動時に `cluster-override.conf` ファイルがコアノードからのみコピーされるようにしました。

- [#10327](https://github.com/emqx/emqx/pull/10327) 回復不能なデータブリッジエラー受信時に「actions.failed.unknown」ルールメトリクスカウンターを増加させないようにしました。

- [#10095](https://github.com/emqx/emqx/pull/10095) MySQL コネクターがバッチモード時に、クライアントが各バッチで不要な `PREPARE` ステートメントを繰り返し送信し、サーバーリソース枯渇を引き起こす問題を修正しました。

## 5.0.1

_リリース日: 2023-03-10_

### 機能強化

- [#10019](https://github.com/emqx/emqx/pull/10019) QUICリスナーの低レベルチューニング設定を追加しました。
- [#10059](https://github.com/emqx/emqx/pull/10059) ルールエンジンAPIから返されるエラーを、スタックトレースを含む生のエラーをダンプするのではなく、より人間に読みやすい形式に整形しました。
- [#9213](https://github.com/emqx/emqx/pull/9213) HelmチャートにPod Disruption Budgetを追加しました。
- [#9949](https://github.com/emqx/emqx/pull/9949) QUICトランスポートのマルチストリーム対応およびQUIC TLSのca-cert対応を追加しました。
- [#9932](https://github.com/emqx/emqx/pull/9932) `bridges`に新しいバックエンドとして`TDengine`を統合しました。
- [#9967](https://github.com/emqx/emqx/pull/9967) 新しい共通TLSオプション 'hibernate_after' を追加し、アイドル接続ごとのメモリフットプリントを削減します。デフォルトは5秒です。

### バグ修正

- [#10009](https://github.com/emqx/emqx/pull/10009) `GET /trace/:name/log` の `bytes` パラメータが符号付き32ビット整数を超えないように検証を追加しました。

- [#10015](https://github.com/emqx/emqx/pull/10015) 環境変数から提供された誤ったEMQXノードクッキーによるエラーを防止するため、フェイルファスト機構を実装しました。  
  以前は誤ったクッキーが提供されてもコマンドはノードへのpingを試み、「Node xxx not responding to pings」というエラーメッセージが表示されていました。  
  新しい実装では、クッキーの不一致が検出されると、クッキーが誤っている旨のメッセージをログに記録し、pingを試みずにエラーコード1でコマンドを終了します。

- [#10020](https://github.com/emqx/emqx/pull/10020) バッチ処理有効時（`batch_size` > 1）の非同期モードでのブリッジメトリクスの問題を修正しました。

- [#10021](https://github.com/emqx/emqx/pull/10021) `emqx_ctl cluster join` コマンドのターゲットノードが起動していない場合のエラーメッセージを修正しました。

- [#10032](https://github.com/emqx/emqx/pull/10032) クラスター内の一部ノードのリソースがまだ「initializing/connecting」状態の場合、`bridges/` APIがリソースのメトリクス情報欠如によりクラッシュする問題を修正しました。メトリクス情報のないリソースは無視されます。

- [#10041](https://github.com/emqx/emqx/pull/10041) InfluxDBブリッジにおいて、`write_syntax` ドキュメントに整数値プレースホルダー注釈を追加し、`timestamp` フィールドに定数値を設定可能にしました。

- [#10042](https://github.com/emqx/emqx/pull/10042) `core` クラスターがパーティション化された際（例：coreノードがクラスターを離脱した場合）の `replicant` ノードの挙動を改善しました。  
  以前はcoreクラスターが完全に復旧するまでreplicantノードがcoreノードへの接続をリバランスできず、`[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster` というエラーメッセージが表示されていました。

- [#10054](https://github.com/emqx/emqx/pull/10054) Data-Bridgeの `/bridges_probe` APIで接続テスト時にパスワードが難読化されたまま使用される問題を修正しました。

- [#10058](https://github.com/emqx/emqx/pull/10058) 未使用のQUIC TLSオプションを非推奨化しました。QUICリスナーで使用されるTLSオプションは以下のみです：

  - cacertfile
  - certfile
  - keyfile
  - verify

- [#10076](https://github.com/emqx/emqx/pull/10076) Webhookブリッジのエラー処理を修正し、接続タイムアウトを再試行可能なエラーとして扱うようにしました。  
  修正前は接続タイムアウトが回復不能なエラーとして扱われ、リクエストが破棄されていました。

- [#10078](https://github.com/emqx/emqx/pull/10078) 無効なQUICリスナー設定がセグメンテーションフォルトを引き起こす問題を修正しました。

- [#10084](https://github.com/emqx/emqx/pull/10084) 異なるEMQXバージョンのcoreノードをクラスターに参加させる際の問題を修正しました。

  [Mria PR](https://github.com/emqx/mria/pull/127)

- [#10086](https://github.com/emqx/emqx/pull/10086) HTTPクライアントehttpcを `0.4.7` にアップグレードしました。  
  これ以前のバージョンでは、認証・認可・WebhookのHTTPクライアントが、`body`が空であってもContent-Typeヘッダーが設定されている場合にクラッシュする可能性がありました。  
  詳細は [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44) をご参照ください。

- [#9939](https://github.com/emqx/emqx/pull/9939) Mnesia起動前でも `emqx ctl cluster` コマンドを実行可能にしました。  
  これにより、EMQXの `replicant` ノードで `manual` ディスカバリーストラテジーが利用可能になりました。

- [#9958](https://github.com/emqx/emqx/pull/9958) Client IDが存在しない場合に `clients` APIが返すエラーコードとエラーメッセージを修正しました。

- [#9961](https://github.com/emqx/emqx/pull/9961) `bin/emqx` で非ブートコマンド実行時にノード名やクッキーの設定ファイル解析を回避するようにしました。

- [#9974](https://github.com/emqx/emqx/pull/9974) メモリ使用量をダッシュボードと同じデータソースからstatsdおよびPrometheusに報告するように修正しました。  
  修正前はコンテナ環境で適切に動作しない古いデータソースを使用していました。

- [#9997](https://github.com/emqx/emqx/pull/9997) Swagger APIスキーマ生成を修正しました。`deprecated` メタデータフィールドはSwagger仕様に従い常にboolean型となります。

- [#10007](https://github.com/emqx/emqx/pull/10007) Kafkaブリッジの設定 `memory_overload_protection` のデフォルト値を `true` から `false` に変更しました。  
  EMQXは過負荷保護によりメッセージがドロップされた場合にログとカウンターで記録しますが、デフォルトではこれらに基づくアラートがなく、`true` のままだと通知なしにメッセージが失われる可能性があります。  
  現状では管理者がリスクと利点を理解した上で明示的に設定することを推奨します。

- [#10087](https://github.com/emqx/emqx/pull/10087) InfluxDBへのデータ挿入時に `timestamp` 設定が空（未定義）の場合、デフォルトテンプレート `${timestamp}` を使用するようにしました。  
  これ以前はテンプレート未指定時に誤ったタイムスタンプが挿入されていました。

## 5.0.0

_リリース日: 2023-02-03_

EMQX Enterprise 5.0は完全に新しいリリースです。本バージョンでは新しいクラスターアーキテクチャを実装し、主要機能を洗練させ、多数の新機能を導入しました。

### **新しいCore + Replicaクラスタリングアーキテクチャ**

EMQX Enterprise 5.0は全く新しいCore + Replicaクラスタリングアーキテクチャを採用し、より優れたスケーラビリティとレジリエンスを提供します。

- 単一クラスターで最大23ノード、1億以上のMQTT接続をサポート可能となり、EMQX Enterprise 4.xと比べて[10倍の増加](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)を実現しました。
- Replicantノードはステートレスであるため、動的スケーリング時もクラスターのパフォーマンスが安定します。
- 大規模展開時のスプリットブレインのリスクを低減し、ビジネスへの影響を最小化します。

[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator)はこの新しいクラスタリングアーキテクチャに対応しており、EMQXでスケーラブルかつステートレスなMQTTサービスを簡単にデプロイ可能です。

### MQTT over QUICのサポート

トランスポート層にQUICを採用することで、EMQX Enterprise 5.0はMQTT over QUIC伝送プロトコルをサポートします。

MQTT over QUIC（Quick UDP Internet Connections）は、IoTネットワークにおいて低レイテンシで信頼性が高く安全なデータ通信を提供することを目的とした新しいトランスポートプロトコルです。性能テストの結果から、MQTT over QUICは従来のネットワークプロトコルが抱える遅延や信頼性の問題、高レイテンシやセキュリティの脆弱性を克服できることが示されています。そのため、MQTT over QUICはIoV（Internet of Vehicles）やモバイルデータ収集シナリオにおけるデータ収集方法を革新する可能性があると言われています。

EMQX Enterprise 5.0では、MQTT over QUICリスナーを作成し、EMQ SDKを使ってIoTデバイスと接続できます。EMQはOASISのメンバーとしてMQTTプロトコルのドラフトを提出しています。

詳細は[MQTT over QUIC: Next-Generation IoT Standard Protocol](https://www.emqx.com/en/blog/getting-started-with-mqtt-over-quic-from-scratch)をご参照ください。

### 視覚化された**ルールオーケストレーションと双方向データ統合**

EMQX Enterprise 5.0はIoTデータのリアルタイム処理機能を提供し、より柔軟でローコードな方法でサードパーティデータシステムとの統合をサポートします。

**Flowデザイナーによる視覚的オーケストレーションルール**

EMQX Enterprise 5.0はEMQXダッシュボードに視覚的なFlowsページを追加し、ルール管理を容易にしました。Flowsエディターでは、データのフィルタリング、処理、ブリッジのフローを簡単に閲覧・監視できます。IoTハードウェアとデータフローの接続を視覚化することで、開発者はビジネスに意味のある作業に集中できます。

将来的にはドラッグ＆ドロップによるルールオーケストレーションとデータブリッジ作成をサポートする予定です。

**より柔軟な双方向データ統合**

デバイスから外部システムへのデータブリッジに加え、EMQX Enterprise 5.0はルール処理後に外部データシステムから特定クライアントへのデータブリッジもサポートします。例えば他のMQTTサービスやKafkaなどです。

双方向データ統合はクラウドからデバイスへのメッセージ送信に適しており、大規模メッセージのリアルタイム処理と配信をサポートするEMQX 5.0はIoTサービスの応用シナリオに多くの可能性を提供します。

**ディスクベースのバッファキュー**

EMQX Enterprise 5.0はデータブリッジサービスをより良くサポートするためにバッファキュー機能を提供します。この機能により、異常接続時に生成されたメッセージを一時的にキャッシュし、接続復旧後に送信を継続できます。

このバッファキュー機能はデータ統合の信頼性を確保し、ビジネスの可用性を大幅に向上させます。

**対応データ統合**

EMQX Enterprise 5.0は現在、以下のデータシステムとの統合をサポートしています：

1. Webhook
2. MQTT
3. Kafka
4. InfluxDB
5. MySQL
6. Redis
7. GCP Pub/Sub
8. MongoDB

今後さらに多くのデータシステムをサポート予定です。ご期待ください。

### **強化されたセキュリティ管理**

**より柔軟なアクセス制御**

EMQX Enterprise 5.0はパスワード認証、LDAP、JWT、PSK、X.509証明書などの認証オプションを提供し、メッセージのパブリッシュおよびサブスクライブに対する認可チェックも行います。

設定ファイルに加え、EMQXダッシュボードからもアクセス制御を設定可能で、より柔軟かつユーザーフレンドリーな方法です。このダッシュボード設定により、EMQXクラスターのアクセス制御を再起動なしで有効化できます。

また、クラスターおよびノードレベルでの統計メトリクスを提供し、アクセス制御の稼働状況を監視しやすくしています。主な指標は以下の通りです：

- Allow: 認証／認可成功数
- Deny: 認証／認可失敗数
- No match: クライアント認証／認可データ未検出数
- Rate: リクエスト率

**Limiterによる過負荷保護**

EMQXは過負荷保護機構と新しいLimiter機能を導入しました。Limiterはより正確かつ階層的なレート制御を提供し、クライアント、リスナー、ノードレベルでのクライアント動作制限をサポートします。

これら2つの機能の組み合わせにより、クライアントの過負荷や過剰なリクエストトラフィックを防ぎ、システムの安定稼働を保証します。

### 使いやすく観測性が向上したEMQXダッシュボード

EMQX Enterprise 5.0ではEMQXダッシュボードを新しいUIデザインスタイルで再設計し、視覚的な体験を向上させ、より強力で使いやすい機能をサポートします。ユーザーはクライアント接続の管理、各種サブスクライブ／パブリッシュリクエストの認証・認可、データブリッジやルールエンジンを通じた異なるデータシステムとの統合を新しいEMQXダッシュボードで行えます。

**主な改善点は以下の通りです：**

- アクセス制御管理
- Flowsエディターによるデータ統合の視覚化
- より強力なホットコンフィグレーション
- スロウサブスクリプションやログトレースなどの診断ツールの充実
- 保持メッセージ管理やパブリッシュスケジュールの延期など強力なデータ管理機能

### より柔軟な拡張性

EMQX Enterprise 5.0では、スタンドアロンのプラグインパッケージで拡張プラグインをコンパイル、配布、インストールできるようになりました。これらのパッケージはダッシュボード経由でアップロードし、EMQXクラスターを再起動せずに設定を完了できます。

標準プラグインには完全なドキュメントとウェブサイトURLが付属し、ユーザーは簡単に利用方法を理解し、開発者とコミュニケーションを取れます。

**使いやすいExHook/gRPC**

ユーザーは複数のExHookを同時に作成可能です。関連メトリクスにより、各ExHookに登録されたフック（およびその引数）の詳細な使用統計を確認でき、ExHook拡張の負荷を把握しやすくなっています。

**より“ネイティブ”なマルチプロトコル接続**

ゲートウェイは統一設計レイヤーで再実装されました。EMQX Enterprise 5.0は各プロトコル機能ごとに独自のクライアント管理ページとセキュリティ認証設定を提供し、よりプロトコルネイティブなアクセス管理を可能にします。

各ゲートウェイは独自の認証方式を設定できるため、異なるゲートウェイデバイスの認証情報を相互に分離し、高度なセキュリティ要件に対応できます。
