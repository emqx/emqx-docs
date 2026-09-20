# EMQX Enterprise Version 5

## 5.10.4

*リリース日: 2026-06-01*

EMQX 5.10.4へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17039](https://github.com/emqx/emqx/pull/17039) Dashboardのユーザーアカウント管理エンドポイントへのAPIキーアクセスを制限しました。

  以前は、`administrator`ロールを持つAPIキーがHTTP Basic認証を介してDashboardのユーザー管理エンドポイント`POST/DELETE /users/:username/mfa`および`POST /users/:username/change_pwd`を呼び出せました。これにより、APIキーが他のDashboardユーザーのMFAをリセットまたは無効化したり、パスワードを変更したりでき、人間のDashboardセッションと機械のAPIキーの分離が意図せず回避されていました。

  これらのエンドポイントは、APIキー経由でアクセスすると`401 API_KEY_NOT_ALLOW`を返すようになり、既存の`/users`、`/users/:username`、`/logout`、`/api_key`へのAPIキーアクセス制限ポリシーと整合します。Dashboardユーザーは引き続きDashboard UIからベアラートークン（JWT）セッションを使って自身のMFAやパスワードを管理できます。

- [#17169](https://github.com/emqx/emqx/pull/17169) データバックアップエンドポイントを介したDashboardアカウントおよびAPIキーのエクスポート・インポートをAPIキーから制限しました。

  APIキーで呼び出された`POST /data/export`は、生成されるアーカイブから`dashboard_users`および`api_keys`のmnesiaテーブルセットを静かに除外します。APIキーで呼び出された`POST /data/import`は、アップロードされたバックアップにこれらのテーブルセットが含まれる場合`403 FORBIDDEN`を返します。

  Dashboardのベアラートークン（ログイン）呼び出しは影響を受けず、DashboardユーザーとAPIキーを含む完全なデータベースのバックアップと復元が可能です。

  これは権限昇格のギャップを解消します。既存の`/users`および`/api_key`エンドポイントはAPIキーによるDashboardログイン資格情報とAPIキー記録へのアクセスを拒否していますが、APIキー保持者はデータバックアップエンドポイントを経由することでこれらの制限を回避できました。

- [#17188](https://github.com/emqx/emqx/pull/17188) 認証されていない`GET /status?format=json`レスポンスからEMQXリリースバージョン（`rel_vsn`）を削除し、ブローカーのバージョン情報が非認証呼び出し元に漏れないようにしました。バージョン情報は認証済みのノード情報APIで引き続き取得可能です。

- [#17200](https://github.com/emqx/emqx/pull/17200) アップロードされたtarballのパストラバーサルに対してプラグインインストールエンドポイントを強化しました。プラグインインストールディレクトリ外に解凍されるエントリを含むtarballは拒否されます。

  これは多層防御の一環です。エンドポイントはすでにDashboardログイン/APIキー認証および`emqx ctl plugins allow <name-vsn>`の明示的な許可リストエントリで保護されており、未認証または未許可の呼び出し元はこのコードパスに到達できません。新しいチェックは両方のゲートが意図的に開かれてプラグインをアップロードする場合でもインストールディレクトリを保護します。

- [#17202](https://github.com/emqx/emqx/pull/17202) `POST /api/v5/plugins/install`（およびそれをラップするDashboardアップロード）によるプラグインインストール成功時に、アップロードを許可したクラスタ全体の`emqx ctl plugins allow <name-vsn>`エントリを即座に取り消すようにしました。同じ許可が別の（潜在的に異なる）tarballに再利用されることを防ぎます。5分のTTLは引き続き適用されますが、一般的なパスでの窓口を早めに閉じます。

- [#17253](https://github.com/emqx/emqx/pull/17253) 公式ダウンロードサイトのプラグインパッケージに`.sha256`チェックサムサイドカーを追加し、ユーザーがダウンロードしたプラグインアーカイブの整合性を検証できるようにしました。

- [#17276](https://github.com/emqx/emqx/pull/17276) 公式EMQX Dockerイメージを強化し、イメージスキャナの指摘を解消しました：

  - ランタイムイメージビルド時にDebianのセキュリティアップグレードを適用し、最新のパッチ済み`libssl3t64`を取り込みました。
  - 未使用の`libgnutls30t64`パッケージを削除しました。EMQXはErlang/OTP経由でOpenSSLを使用し、GnuTLSはリンクしないため、`curl`の推移的依存としてのみ存在しスキャナレポートに表示されていました。
  - Debianの`curl`パッケージは`librtmp1`経由で`libgnutls30t64`を再導入するため、OpenSSL対応の静的リンク済み`curl`バイナリ（<https://github.com/stunnel/static-curl>）に置き換えました。`curl`を呼ぶコンテナのヘルスチェックは変更なく動作します。

- [#17314](https://github.com/emqx/emqx/pull/17314) PROXY-Protocol v2のSSL Common Name / Subjectをクライアント識別に入れる前にサニタイズしました。

  `proxy_protocol = true`設定のリスナーでは、PROXY-Protocol SSL TLVバイトにASCII制御文字が含まれる接続を拒否します（これはMQTT経由の`clientid`/`username`/`password`で既に拒否されているバイトクラスと同じです）。これにより、攻撃者制御のバイトが`${cert_common_name}`や`${cert_subject}`テンプレートを介してHTTP認証・認可・ルールエンジンのヘッダー値に密輸されるのを防ぎます。

  追加の防御層として、HTTP認証・認可クライアントは、レンダリングされたヘッダー名または値にCR、LF、NULバイトが含まれる場合、リクエスト送信を拒否します。

- [#17322](https://github.com/emqx/emqx/pull/17322) MQTTの`clientid`/`username`/`password`に適用されているバイトクラスチェックを、`ClientInfo`およびHTTPリクエストテンプレートに供給される他のフィールドに拡張しました：

  - `peersni`（TLSのServer Name Indication。PROXY-Protocol v2の`authority` TLVからも受け取る）は接続受け入れ境界で検証されます。制御文字が含まれると接続拒否かつ警告ログ出力。
  - `mqtt.client_attrs_init`のVariform式で生成されるクライアント属性値は制御文字含有時に破棄され（警告付き）、`${client_attrs.tns}`のようなテンプレートでのバイト注入を防ぎます。
  - HTTPアクション／ブリッジコネクタのヘッダー描画は、レンダリングされた名前または値にNUL、CR、LFが含まれるヘッダーを破棄します。

#### クラスター

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期機構を導入しました。ルーティングテーブルのスキーマバージョンは`v3`に上げられ、`v2`との後方互換性も提供されます。

  スキーマv3では、各ノード（コアまたはレプリカント）が自分に向かうルーティングテーブルエントリの完全な所有権を持ち、ピアノードはこれらのエントリに対して読み取り専用アクセスのみを持ちます。これにより、パーティション耐性が向上し、パーティション化されたクラスター内のピアノードが他ノードの代理でルーティングテーブルを変更できなくなります。また、レプリカントノードの`SUBACK`レイテンシも改善されます。

  **後方互換性:** v3対応ノードがv2のみ対応のクラスターに参加すると、互換性のためv2を使い続けます。クラスター内に互換モードのノードがいる場合も互換モードを使います。クラスターをv3に切り替えるにはアップグレード後に完全なクラスター再起動を行います。自動切り替えを防ぐには`broker.routing.storage_schema`を`v2`に設定してください。

  **ダウングレード注意:** クラスターがv3に切り替わるとローリングダウングレードは不可能です。

  ノードの現在のルーティングスキーマバージョンを確認するには：

  ```bash
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17156](https://github.com/emqx/emqx/pull/17156) 分散ポートのErlang inetポートオプション設定をサポートし、デフォルトの`buffer`サイズを1MBに設定しました。

  以前は分散ポートのデフォルトバッファが非常に小さく（1460バイト、プラットフォームによっては約9KB）、分散ポートバッファ（`+zdbbl`）を大きく設定しても性能ボトルネックとなっていました。これによりクラスター通信の信頼性が影響を受け、`erpc timeout`エラー、Mnesiaトランザクションの輻輳、多コアノードサポートの劣化が発生していました。

#### 可観測性

- [#17074](https://github.com/emqx/emqx/pull/17074) EMQX v4の`emqx_routes_count`メトリクスに類似した、ノードごとのルートテーブルエントリ数をエクスポートするPrometheusメトリクス`emqx_routes_count`と`emqx_routes_max`を追加しました。
- [#16746](https://github.com/emqx/emqx/pull/16746) `os_mon`をデフォルトでシステム全体のメモリ統計のみ収集するよう設定し、プロセスごとのメモリスキャンのオーバーヘッドを削減しました。
- [#16911](https://github.com/emqx/emqx/pull/16911) Mria統計の誤った繰り返しクエリを回避し、Prometheusメトリクス収集のオーバーヘッドを削減しました。

- [#17161](https://github.com/emqx/emqx/pull/17161) ノードごとのライセンス情報をPrometheusゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスター全体のライセンス整合性をノードごとのCLIチェックなしで監視可能にしました。

  タイムスタンプはライセンスの発行/有効期限日のUTC深夜のUnixエポック秒です。ライセンスが利用不可の場合は全てのメトリクスが`0`を出力します。アラートルールでは`emqx_license_expiry_at == 0`を「利用不可」シグナルとして使ってください（`max_sessions == 0`は試用期限切れも示すため）。

#### アクセス制御

- [#16792](https://github.com/emqx/emqx/pull/16792) JSONデータおよびJWTトークンからドット区切りのキー経路で値を抽出する新しいVariform式ヘルパー関数`json_value`と`jwt_value`を追加しました。

  `json_value`はJSONバイナリ文字列からネスト構造をドット区切りパスで辿って値を抽出します。`jwt_value`はJWTトークンのペイロードをデコードし、同様のパス構文でクレーム値を抽出します。

  例：`username`がJSONオブジェクトなら`json_value(username, 'shop.floor')`でフィールドにアクセス可能。`password`がカスタムクレームを持つJWTなら`jwt_value(password, 'client_attrs.unitid')`でネスト値にアクセス可能。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) APIキーおよびDashboardログインユーザーのスコープベースの細粒度アクセス制御を導入しました。

  APIキーはOpenAPIタグ由来のスコープで特定のAPIパスカテゴリに制限可能です。スコープなしのキーは従来通り全アクセス可能で後方互換性があります。空のスコープリストは全スコープパスを拒否します。`publisher`ロールのAPIキーは`[publish]`に制限されます。

  Dashboardログインユーザーは既存のロールチェックに加え任意の`scopes`フィールドを持ちます。Dashboard専用エンドポイント用の4つの新スコープ：`user_management`、`sso_management`、`api_key_management`（管理者のみ）、`mfa_management`（強制MFA免除の自己管理用、全ロール可）。APIキーはこれらログイン専用スコープを持てません。

  スコープ語彙を公開する新カタログエンドポイント`GET /api_key_scopes`と`GET /user_scopes`を追加し、いずれもベアラー認証でアクセス可能。`GET /users`、`POST /users`、`PUT /users/:username`レスポンスにも`scopes`フィールドを追加。未設定時はロールデフォルトスコープリストを返します。

  動作変更：

  - `dashboard.default_username`ユーザーはブレークグラスアカウントとして保護されます。削除不可、管理者から降格不可、`scopes`設定不可。`description`のみ変更可能。これにより他の管理者がスコープを失った場合でもオペレーターは管理権限を保持します。
  - ユーザー自身のレコードに対するセルフサービスはスコープを尊重します。パスワード変更とMFA自己管理エンドポイントのみスコープチェックをバイパスし、`PUT /users/:self`など他操作はスコープ検査対象です。
  - `PUT /users/:username`と`PUT /api_key/:name`はリクエストボディに`scopes`がない場合、永続化済みの有効スコープに対してロール変更を検証します。ユーザー降格やAPIキーのロール変更はスコープと互換性がなければ拒否されます。

- [#16943](https://github.com/emqx/emqx/pull/16943) SSO（OIDC/SAML/LDAP）用のバックエンドごとの`force_mfa`オプションを追加しました。

  有効時、SSOユーザーはIDP側MFA設定に関わらずDashboardトークン発行前にTOTP MFAセットアップまたは検証を完了する必要があります。3つのMFA状態をサポート：`not_configured`（セットアップ強制）、`enabled`（検証必須）、`admin_disabled`（MFAスキップ）。新APIエンドポイント`POST /sso/mfa/setup`と`POST /sso/mfa/verify`でMFAフローを処理します。

- [#17200](https://github.com/emqx/emqx/pull/17200) プラグインインストール許可リストエントリ（`emqx ctl plugins allow <name-vsn>`）は発行後5分で期限切れとなり、パッケージのSHA-256ハッシュに紐付け可能になりました。

  `emqx ctl plugins allow <name-vsn> sha256:<HEX>`は64文字の小文字16進ダイジェストを受け付け、内容が一致しないアップロードは`403 Forbidden`で拒否されます。`sha256:`引数省略時は従来通り任意の`<name-vsn>.tar.gz`ペイロードを受け入れます。

#### ゲートウェイ

- [#16655](https://github.com/emqx/emqx/pull/16655) JT/T 808ゲートウェイのダウンリンクメッセージでカスタム`msg_sn`をサポートしました。

  ダウンリンクMQTTメッセージペイロードのヘッダーに`msg_sn`フィールドがある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりにその値を使用します。これにより外部システムが特定ユースケースでメッセージ順序制御可能になります。

  また、JT/T 808ゲートウェイの`string_encoding`がダウンリンクメッセージのシリアライズに適用されていなかった問題を修正しました。以前は`string_encoding`設定（例：`gbk`）はアップリンク解析にのみ使われていました。現在は`string_encoding: gbk`設定時、アップリンク解析（GBK→UTF-8）とダウンリンクシリアライズ（UTF-8→GBK）の両方が正しく動作します。

#### データ統合

- [#16961](https://github.com/emqx/emqx/pull/16961) Kafkaソースのポーリング挙動を改善し、レコードがない場合に空バッチを即時返すのではなく、データが来るまで短時間待つようにしました。これにより不要なポーリング遅延が減り、Kafkaコンシューマーが新規レコードをより安定して受信できます。

- [#17098](https://github.com/emqx/emqx/pull/17098) influxdb-client-erlを1.1.13から1.1.18にアップグレードし、InfluxDBコネクターに`ping_with_auth`オプション（デフォルトfalse）を追加しました。これにより、一部のInfluxDB互換サービスで認証情報を含むヘルスチェックが可能になります。

#### デプロイメント

- [#16853](https://github.com/emqx/emqx/pull/16853) v5ライセンスパーサーをv6ライセンスキーに対してフォワード互換にしました。

### バグ修正

#### コアMQTT機能

- [#17097](https://github.com/emqx/emqx/pull/17097) `retainer.enable`を実際のランタイムスイッチとして復活させました。これにより、`mqtt.retain_available`に頼らず、MQTTのリテインドメッセージプロトコルサポートを有効にしつつリテインドメッセージの保存を無効化できます。

- [#16671](https://github.com/emqx/emqx/pull/16671) セッションテイクオーバーや破棄シナリオで`disconnected_at`が`connected_at`より遅くなるタイムスタンプ順序問題を修正しました。

  以前は`disconnected_at`が新セッションの`connected_at`設定後の`ensure_disconnected`で遅れて記録されていました。これにより`disconnected_at > connected_at`となり、外部のクライアントプレゼンス状態追跡が困難でした。

  修正ではテイクオーバー開始時または破棄受信時に即座に`disconnected_at`を記録し、新セッションの`connected_at`より遅くならないようにします。これにより外部プレゼンス追跡システムで正しいタイムスタンプ順序が保証されます。

  注：これらのイベントが異なるクラスターのノードから発せられる場合、観測される順序はノード間の時計同期にも依存します。

- [#16732](https://github.com/emqx/emqx/pull/16732) 共有サブスクリプションが存在する場合に`emqx ctl subscriptions list`がクラッシュする問題を修正しました。

  以前は一部クライアントでサブスクリプション一覧取得が失敗し、出力が得られませんでした。現在は通常のサブスクリプションと共有サブスクリプションの両方で確実に動作します。

- [#17386](https://github.com/emqx/emqx/pull/17386) DashboardおよびREST APIのチャネル情報（`mqueue_len`、`inflight_cnt`）がセッションテイクオーバー再生完了直後に即時更新されるよう修正しました。以前は次の15秒統計更新まで待っていました。

#### ルールエンジン

- [#17210](https://github.com/emqx/emqx/pull/17210) `$events/client/connack`ルールイベントに欠落していた`connected_at`フィールドを追加しました。ドキュメントには記載されていましたが実際のイベントデータにはありませんでした。

- [#17106](https://github.com/emqx/emqx/pull/17106) ルール作成・更新時に無効なルールメタデータのタイムスタンプを無視するようにしました。

  以前は`metadata.created_at`や`metadata.last_modified_at`に日付文字列など整数でない値が含まれると、API経由でルールを一覧・取得する際に内部エラーが発生していました。

  現在は無効なメタデータタイムスタンプを無視し、通常の生成タイムスタンプにフォールバックするため、破損したメタデータがあってもルールAPIレスポンスが利用可能です。

#### データ統合

- [#16724](https://github.com/emqx/emqx/pull/16724) RabbitMQコネクター/アクション/ソースで、一部の接続やチャネルプロセスが予期せず終了した場合に再起動なしで自己回復しなかった問題を修正しました。

- [#16854](https://github.com/emqx/emqx/pull/16854) ブリッジ設定インポート時のクラッシュを修正しました。

  以前は一括インポート時に以下のようなクラッシュメッセージで失敗することがありました。

  `Failed to import the following config path: "actions", reason: {error, {config_update_crashed, {badarity, {#Fun<emqx_bridge_v2.16.79877859>, ['_computed',...`

- [#16935](https://github.com/emqx/emqx/pull/16935) Azure Blob Storageアクションの集約モードで、コンテナ内のBlob数が多すぎる場合にヘルスチェックがタイムアウトする問題を修正しました。

- [#16971](https://github.com/emqx/emqx/pull/16971) HTTPおよびGCP PubSubアクションで、`closing`理由の一時的な接続エラーを回復可能として扱うようにし、ログノイズを低減しました。

- [#17085](https://github.com/emqx/emqx/pull/17085) MQTTソースで、コネクターが`clean_start = false`を使い、メッセージを含むセッションのあるブローカーに再接続した場合にメッセージがルールアクションをトリガーしなかった問題を修正しました。

- [#17105](https://github.com/emqx/emqx/pull/17105) InfluxDBコネクター/アクションで、`write_syntax`リテラルやMQTTペイロードから書き込む値のUnicodeテキストを保持するよう修正しました。

- [#17109](https://github.com/emqx/emqx/pull/17109) PostgreSQLコネクターでプリペアドステートメント無効時のクエリ実行を修正しました。以前は同時クエリが干渉してエラーを起こすことがありました。

- [#17112](https://github.com/emqx/emqx/pull/17112) RocketMQコネクターの分離性を修正しました。設定ミスや到達不能なRocketMQコネクターが同一ノード上の他のRocketMQコネクターを不安定化させなくなりました。以前は到達不能なブローカーの1つが共有クライアントスーパーバイザーを最大60秒停止させ、兄弟コネクターが`resource_health_check_timed_out`でフラップし、Dashboard操作がハングしました。

  TCP/TLS接続タイムアウトのデフォルトも60秒から10秒に短縮し、設定ミスのサーバーが早期に失敗として検出されるようにしました。

- [#17179](https://github.com/emqx/emqx/pull/17179) 高負荷時にMongoDBプロセスへのタイムアウト呼び出しが回復不能エラーとして扱われ再試行されなかった問題を修正しました。これらのイベントではメッセージが再試行されます。

  発生時のログ例：

  ```text
  {"stacktrace":["{emqx_mongodb,on_query,3,...}","{emqx_resource_buffer_worker,apply_query_fun,9,...}",...],"request":"...","name":"call_query","id":"action:mongodb:xxx:connector:mongodb:xxx","error":"{error,{case_clause,{error,{timeout,{gen_server,call,[...,{checkout,...},5000]}}}}}"}
  ```

- [#17256](https://github.com/emqx/emqx/pull/17256) Redis SentinelコネクターでRedisデータノードとSentinelノードの認証設定を分離してサポートしました。

- [#17292](https://github.com/emqx/emqx/pull/17292) Parquetファイルに必須キーが`undefined`または`null`のオブジェクトを書き込むと破損ファイルが生成される問題を修正し、エラーを発生させるようにしました。

- [#17301](https://github.com/emqx/emqx/pull/17301) Kafkaクライアントライブラリを`brod` 4.5.2→4.5.4、`wolff` 4.1.7→4.1.10にアップグレードしました。

  Kafkaプロデューサー・コンシューマー統合に以下の修正を含みます：

  - SASL再認証中の接続競合状態を修正し、キューイングされたproduceリクエストのドロップや`sync` produce呼び出しのタイムアウトを防止。
  - リーダー接続の再接続を改善し、アイドルタイムアウト切断直後に古い死んだ接続が返されなくなりました。

- [#17346](https://github.com/emqx/emqx/pull/17346) RocketMQクライアント依存を`v0.7.2`にアップグレードし、非同期プロデューサーリクエストのメモリ増加問題を修正しました。

- [#17298](https://github.com/emqx/emqx/pull/17298) `emqtt` MQTTクライアント依存を`1.14.6`から`1.15.1`にアップグレードしました。

  MQTTブリッジ、MQTTソース、その他アウトバウンドMQTT接続を使うコネクターに以下のユーザー向け改善をもたらします：

  - キープアライブタイマーからpingrespタイムアウトを追跡し、pingresp処理が設定された`keepalive`間隔と整合するようにしました。
  - QUIC：ピアの`recv`中止後は送信方向のみ中止し、両方向を切断しないようにしました。これにより半クローズのQUICストリーム上の保留送信が静かにドロップされなくなりました。

#### クラスター

- [#16729](https://github.com/emqx/emqx/pull/16729) 全ノード同時再起動後のクラスター回復時間を改善しました。

  内蔵のMriaデータベース管理システムはトランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17164](https://github.com/emqx/emqx/pull/17164) Erlang/OTPを27.3.4.2-6から27.3.4.2-7にアップグレードしました。

  これにより、ノード起動時にネットワークパーティションが発生した場合にMQTTルーティングテーブルの不整合を引き起こす競合状態が解消されます。

- [#17195](https://github.com/emqx/emqx/pull/17195) emqx-OTPを27.3.4.2-8にアップグレードしました。これがないと、ノードがクラスターに接続されていない場合にMriaアプリの起動がEMQX起動時にハングすることがあります。

- [#17220](https://github.com/emqx/emqx/pull/17220) 実行中のブローカーで`bin/emqx`および`bin/emqx_ctl`呼び出しが`nodeup`/`nodedown`イベントをトリガーし、ブローカーログに誤解を招く`cm_registry_node_down`警告が出る問題を修正しました。これらスクリプトが起動する一時的なヘルパーノードは隠しErlangノードとして登録されるようになりました。

- [#17257](https://github.com/emqx/emqx/pull/17257) ネットワークパーティション後のクラスター回復を改善しました。

  以前はレプリカントノードに接続されたクライアントの一部がグローバルレジストリから失われ、セッションテイクオーバー時の不整合やDashboard表示の誤りを引き起こしていました。

  この修正ではネットワークパーティション回復時に既存クライアントを再登録するバックグラウンドプロセスを追加し、「Broker is recovering after a network partition」という新しいアラームをグローバルレジストリ再構築中に発報します。

- [#17270](https://github.com/emqx/emqx/pull/17270) 重複するネットワークパーティションを自動回復可能な新しい自動修復アルゴリズムを導入し、ネットワークパーティションからのクラスター回復を改善しました。

- [#17306](https://github.com/emqx/emqx/pull/17306) エクスポートされた`cluster.hocon`に部分的な`node`セクションが含まれている場合にクラスタ設定インポートが`required_field: node.cookie`スキーマチェックエラーで失敗する問題を修正しました。読み取り専用設定ルート（`node`、`rpc`）は事前スキーマチェック前に破棄され、実行中ノードの値で検証されます。

- [#17313](https://github.com/emqx/emqx/pull/17313) クラスター化されたノードで同じ実効設定だが異なる生設定表現を持つ場合に`emqx ctl conf cluster_sync status`診断が騒がしく誤解を招く問題を修正しました。

  コマンドは実効設定の不整合時に警告を出しつつ、実際の設定変更に対応しない生表現の差異は抑制します。また、生設定キーが片方のノードにのみ存在する場合のクラッシュも回避します。

- [#17382](https://github.com/emqx/emqx/pull/17382) クラスターがネットワークパーティションを経験した際に発生するグローバルチャネルレジストリの破損を修正しました。

- [#17387](https://github.com/emqx/emqx/pull/17387) 生成されたタイムスタンプメタデータによる誤解を招く`emqx ctl conf cluster_sync status`警告を修正しました。

  以前はデータインポートや起動時設定ロードで`created_at`や`last_modified_at`メタデータがノード間で異なり、同一のアクションやソース、ブリッジ、ルールメタデータでも差異が生じていました。コマンドは設定整合性チェック時にこれらタイムスタンプのみの差異を無視し、実際の設定差異は報告します。

- [#17402](https://github.com/emqx/emqx/pull/17402) 応答しないターゲットクラスターへのルート複製が接続に詰まった際のCluster Link応答性を改善し、そのようなCluster Linkの削除を迅速化しました。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後のグローバルセッションレジストリリークを修正しました。これにより同一クライアントIDの重複または古いエントリが残る問題が解消されます。

  廃棄およびテイクオーバーキックRPCハンドラは対象プロセスが生存しない場合もレジストリ行を削除し、接続パスの登録スロットルはトゥームストーン行（ローカルチャネル状態なし）を認識して再利用をブロックせずに回収します。

#### アクセス制御

- [#16690](https://github.com/emqx/emqx/pull/16690) `emqx_crl_cache:evict/1`が内部URL状態を完全にクリアしなかったCRLキャッシュの回帰を修正しました。削除後、同一CRL URLは次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続ごとのHTTPフェッチの繰り返しを回避します。

- [#17012](https://github.com/emqx/emqx/pull/17012) パスワードなしのCONNECTパケット時に認証チェーンが進行するよう、パスワードベース認証バックエンドを修正しました。以前はパスワードなし接続時に最初のパスワード認証器がエラーを返し、後続認証器が試行されませんでした。

- [#17101](https://github.com/emqx/emqx/pull/17101) OIDC SSOログインで、IDプロバイダーが`+json`構造化シンタックスサフィックスを持つ`Content-Type`（例：`application/jwk-set+json; charset=utf-8`）のJWKSレスポンスを返す場合に`provider_not_ready`で失敗する問題を修正しました。これらのレスポンスは有効なJWKSコンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URLエンコードされたユーザー名（例：メールアドレス）を持つSSOユーザーのDashboard RBACチェックを修正し、`force_mfa`無効時のビューワー自己サービスMFA無効化要求が正しく動作するようにしました。

#### 可観測性

- [#16672](https://github.com/emqx/emqx/pull/16672) Erlang PIDがログデータフィールドとして出力されるようにしました。

- [#16699](https://github.com/emqx/emqx/pull/16699) 以前は特定の競合状態で以下のような長く難解なログが出力されていました：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  EMQXは問題のデバッグに役立つより意味のある情報をログに出力するようにしました。

- [#16785](https://github.com/emqx/emqx/pull/16785) シングルノード展開時のプラグイン起動警告のノイズを削減しました。

  クラスター設定同期時にローカルノードからプラグイン設定を取得しようとせず、起動時の`config_not_found_on_node`警告の繰り返しを回避します。

- [#16862](https://github.com/emqx/emqx/pull/16862) 既に期限切れのリクエストに対する非同期応答受信時に警告ログを追加しました。

- [#16954](https://github.com/emqx/emqx/pull/16954) 理由が`emsgsize`（受信パケットが`mqtt.max_packet_size`を超過）時のクライアント接続終了を情報レベルから警告レベルに変更しました。

- [#17255](https://github.com/emqx/emqx/pull/17255) コンテナ内のメモリ使用量報告を改善しました。

  ブローカーはcgroup v2、cgroup v1、ホストの`/proc/meminfo`からのメモリ読み取り値を比較し、最も制約の厳しい値を使用します。最小の非ゼロ合計値が勝ち、合計が同じ場合は使用率の大きい方が勝ちます。

  これにより以下のケースで誤解を招く読み取りを修正します：

  - コンテナに厳しいcgroupメモリ制限があるがホストビューが高い使用率を示す場合（例：ホスト70%以上、cgroup10%未満）、またはその逆。
  - メモリ制限なしでcgroupがマウントされている場合、報告される使用率が約0%に崩壊する場合。

  過負荷保護の閾値や`Memory used`メトリクスは実際にプロセスを制約する制限を反映します。

#### 管理

- [#17365](https://github.com/emqx/emqx/pull/17365) `emqx ctl trace`が`ruleid`をトレースフィルタータイプとして受け入れるよう修正しました。以前は`emqx ctl trace start <name> ruleid <rule-id> <log-level>`（および対応する`trace add ...`形式）がCLI引数パーサーに`ruleid`フィルターがなく一般エラーとなっていました。他のフィルタータイプ（`client`、`topic`、`ip_address`）は影響を受けていません。

## 5.10.3

*リリース日: 2026-01-28*

EMQX 5.10.3へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### デプロイメント

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

#### 可観測性

- [#16135](https://github.com/emqx/emqx/pull/16135) `GET /monitor_current` HTTP APIに新たに`rules_matched`と`actions_executed`の2つのメトリクスと対応するレートを追加しました。これらはそれぞれマッチしたルール数とアクション実行率（成功＋失敗）を追跡します。

- [#16324](https://github.com/emqx/emqx/pull/16324) HTTP API経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

#### セキュリティ

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQXはTLS 1.3のステートレスセッションチケットを使ったセッション再開をサポートしました。これによりサーバー側のセッション状態保存なしにクライアントがTLSセッションを再開可能です。

  **設定**

  - **ノードレベル**：`node.tls_stateless_tickets_seed`

    TLS 1.3ステートレスセッションチケット生成用の秘密鍵シード。

  - **リスナーレベル**：`listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報含む）

  **注意**

  - `node.tls_stateless_tickets_seed`が設定され（空でない）かつリスナーSSLオプションで`session_tickets`が有効な場合のみセッションチケットが生成されます。
  - `session_tickets`が有効でも`node.tls_stateless_tickets_seed`が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出ます。

#### ゲートウェイ

- [#16220](https://github.com/emqx/emqx/pull/16220) JT808ゲートウェイに`jt808.frame.parse_unknown_message`設定オプションを追加し、未知のメッセージIDを持つメッセージを解析して透過的に転送可能にしました。

- [#16596](https://github.com/emqx/emqx/pull/16596) JT/T 808プロトコル2019をサポートしました。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合でIoTDBテーブルモデルをサポートしました。

### バグ修正

#### コアMQTT機能

- [#16349](https://github.com/emqx/emqx/pull/16349) リクエストレスポンス情報プロパティ処理時の型不一致によりMQTT v5接続がクラッシュする問題を修正しました。

- [#16514](https://github.com/emqx/emqx/pull/16514) クライアントが広告する`Maximum-Packet-Size`を超えるブローカーメッセージを受信した際にWebSocket接続がクラッシュする問題を修正しました。

#### ルールエンジン

- [#16489](https://github.com/emqx/emqx/pull/16489) 以下のルール関数が常に`undefined`を返す問題を修正しました：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  注：これはEMQX v4の後方互換性修正です。これらの関数はEMQX v5以降ではドキュメント化されていません。推奨される使用法はルール評価コンテキストのフィールドを直接参照することです（例：`SELECT clientid ...`）。

#### データ統合

- [#16263](https://github.com/emqx/emqx/pull/16263) Kafkaコンシューマーコネクターのヘルスチェックは現在のEMQXノードに割り当てられたパーティションのみのリーダー接続性を検証し、不要なアイドル接続と誤警報を防止します。

  以前は全パーティションのリーダー接続性を検証していました。クラスター展開では各ノードがパーティションのサブセットのみを所有し、割り当てられていないパーティションリーダーへの接続がアイドル状態となっていました。Kafkaはアイドル接続をタイムアウト（デフォルト10分）後に切断するため、誤った接続性警報が発生していました。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafkaリクエストタイムアウトをメタデータリクエストタイムアウトの2倍以上（最小30秒）に自動設定し、メタデータリクエストが想定より長くかかる場合の不要な再接続・再試行を減らしました。特にメタデータリクエストタイムアウトが小さい値に設定されている場合に有効です。

- [#16336](https://github.com/emqx/emqx/pull/16336) ダッシュボードからの接続性テストやコネクター停止時にタイムアウトが発生する競合状態を修正しました。

- [#16383](https://github.com/emqx/emqx/pull/16383) REST APIドライバーを使うIoTDBコネクターのヘルスチェックでクライアント認証情報が検証されていなかった問題を修正しました。ヘルスチェックで軽量なNo-opクエリを送信し、誤設定された認証情報を早期検出可能にしました。

- [#16415](https://github.com/emqx/emqx/pull/16415) Apache Pulsarクライアントを2.1.2にアップグレードしました。

  Pulsarプロデューサーアクションの`batch_size`が`1`に設定されている場合、単一メッセージを単一要素バッチとしてではなく個別にエンコードします。これによりKey Share戦略を使ったコンシューマーの負荷分散が可能になります。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTTソースがコネクターの再接続後にメッセージ受信を停止する問題を修正しました。

  以前はMQTTソースのコネクターが接続喪失から回復した際にトピックが再サブスクライブされず、コネクター再起動まで動作停止していました。現在は再接続時に自動的に再サブスクライブします。

- [#16585](https://github.com/emqx/emqx/pull/16585) GreptimeDB TLS接続失敗問題を修正しました。

- [#16622](https://github.com/emqx/emqx/pull/16622) 非同期クエリモードのアクションで、コネクターが複数回のヘルスチェック失敗後に切断された場合にフォールバックアクションが2回トリガーされる問題を修正しました。

#### クラスター

- [#16269](https://github.com/emqx/emqx/pull/16269) Cluster Linkルート複製プロトコル回復シーケンスで、リモート側が再ブートストラップを必要としているにもかかわらず誤ってスキップされていた問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数の独立したCluster Linkが設定され、一部が長期間ダウンした場合に、古いルート複製状態のクリーンアップ中に内部ルーティングテーブルから生きたルートが誤って削除されるCluster Linkガベージコレクションロジックの問題を修正しました。

- [#16452](https://github.com/emqx/emqx/pull/16452) `gen_rpc`を`3.5.1`にアップグレードしました。

  `gen_rpc`アップグレード前は、ピアノードが到達不能な場合に接続タイムアウトによるクラッシュログの長いテールが発生していました。新バージョンは長いテールがなくなり、クラッシュログをより読みやすい`error`ログに変換し、頻繁な`"failed_to_connect_server"`ログもスロットリングしてログスパムを防止します。

- [#16543](https://github.com/emqx/emqx/pull/16543) クラスターの自動クリーン手順の堅牢性を改善しました。以前はノード起動時に自動クリーン機能が無効化されていると、設定変更後も有効化されませんでした。

#### セキュリティ

- [#16625](https://github.com/emqx/emqx/pull/16625) SAML SSOバックエンドに`idp_signs_envelopes`と`idp_signs_assertions`オプションを追加し、署名検証を制御可能にしました。これらはデフォルト`false`で、IdPがSAMLレスポンスに署名する場合は明示的に有効化が必要です。

#### アクセス制御

- [#16304](https://github.com/emqx/emqx/pull/16304) EMQX 5.3.0未満からのアップグレード後にMulti-Factor Authentication（MFA）が有効化できなかった問題を修正しました。これはログインユーザーデータベースレコードの互換性問題によるものです。

- [#16541](https://github.com/emqx/emqx/pull/16541) OIDC発行者URLが保存時に末尾スラッシュ付きに自動正規化され、OIDCプロバイダーのディスカバリードキュメントが末尾スラッシュなしの発行者を返すと不一致エラーになる問題を修正しました。

#### 可観測性

- [#16418](https://github.com/emqx/emqx/pull/16418) リソース例外（`resource_exception`）発生時のログ量を削減しました。これらのログはスロットリングされ、一部の大きなタームはマスクされます。

- [#16535](https://github.com/emqx/emqx/pull/16535) `gen_rpc`エラーのログフォーマッタークラッシュを修正しました。以前は`gen_rpc`が特定のエラーメッセージ（例：送信タイムアウト）をログ出力するときにフォーマッターがクラッシュしていました。現在はクラッシュせず正しく処理します。

#### ゲートウェイ

- [#16609](https://github.com/emqx/emqx/pull/16609) CANバスIDパラメーター（0x0110～0x01FF）に対するJT/T 808ゲートウェイのパラメーター設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正しました。これらはJSONで文字列型ではなくBASE64エンコードされたBYTE[8]型を使うべきでした。

- [#16606](https://github.com/emqx/emqx/pull/16606) DTLS上の接続モードで動作するCoAPゲートウェイを修正しました。

- [#16627](https://github.com/emqx/emqx/pull/16627) JT/T 808ゲートウェイにGBK文字エンコーディングサポートを追加しました。

  JT/T 808プロトコルはSTRING型フィールドにGBKエンコーディングを指定しています。新しい`frame.string_encoding`設定オプションを追加しました：

  - `utf8`（デフォルト）：文字列をそのまま通過（後方互換）
  - `gbk`：デバイスからのGBK文字列をUTF-8に変換し、MQTTからデバイスへはUTF-8からGBKに変換

  対象はナンバープレート、運転手名、テキストメッセージ、エリア名、クライアントパラメーターなどの文字列フィールドです。MQTTペイロードはこの設定に関わらず常にUTF-8エンコーディングです。

## 5.10.2

*リリース日: 2025-11-11*

EMQX 5.10.2へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### データ統合

- [#16183](https://github.com/emqx/emqx/pull/16183) 期限切れメッセージのドロップに関するログ（`buffer_worker_dropped_expired_messages`）を警告レベルで出力し、リソースIDごとにスロットリングするようにしました。これにより特定の外部リソースがメッセージ処理に追いついていない場合の特定が容易になります。

- [#16206](https://github.com/emqx/emqx/pull/16206) Kafka Producerコネクターに`allow_auto_topic_creation`設定を追加しました。有効時、クライアントがメタデータフェッチ要求を送信した際にトピックが存在しなければKafkaが自動作成を許可します。

- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDBコネクターにカスタムタイムスタンプカラム名（`ts_column`）パラメーター指定をサポートしました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の`parse_unit`オプションのデフォルト値を`chunk`から`frame`に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト4KB）を超える場合のCPU使用率が大幅に削減されます。

  **注意**：`parse_unit = frame`の場合、`PUBLISH`パケットが最大許容サイズを超えると、EMQXは`DISCONNECT`パケットを送信せず接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` APIのパフォーマンスを最適化しました。以前はクラスターに約5万クライアント以上存在するとクライアントリスト取得APIが非常に遅くなったりタイムアウトしたりしていました。

### バグ修正

#### コアMQTT機能

- [#15884](https://github.com/emqx/emqx/pull/15884) 稀にグローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無限に保持する問題を解決しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際にクラスター内のルーティングテーブルおよび共有サブスクリプション状態に蓄積的不整合が生じる競合状態を解消しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証およびメモリベースセッションを使うクライアントが`session_stepdown_request_exception`の`calling_self`エラーでクラッシュする問題を修正しました。

    <details> <summary>エラーログ例</summary>

    ```
    2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
    ```

    </details>

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの`jq`関数のメモリリークを修正しました。

  以前は`jq`組み込み関数`index`（例：`.key | index("name")`）を使うとメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) ルールのSQLに`metadata`フィールドが含まれていない場合に、Republishフォールバックアクションが`function_clause`エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafkaデータ統合で`not_all_kafka_partitions_connected`イベント発生時のログ詳細を改善しました。

- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを持つコネクターを含む設定のロードや再起動時に発生する可能性のあるOOMクラッシュを修正しました。

- [#16138](https://github.com/emqx/emqx/pull/16138) Redisクラスターフェイルオーバー問題を修正しました。以前はRedisクラスタクライアントが通常のクエリ失敗時のみクラスタートポロジーを更新し、定期的な`PING`失敗では更新しませんでした。これによりフェイルオーバー後に古いトポロジーを使い続け、接続状態が「接続中」のままになることがありました。修正後は`PING`失敗もトポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して迅速に復旧します。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの`jq`関数のメモリリークを修正しました。

#### スマートデータハブ

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換やスキーマ検証のトピックインデックスが不整合になる問題を修正しました。1つのアイテムを削除するとトピックインデックスが破損し、無効化したはずの次のアイテムが有効なままになる問題がありました。

- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value`メトリクスの処理を修正する`spb_{en,de}code`関数を導入しました。従来の`sparkplug_{en,de}code`関数は[Protobuf仕様](https://protobuf.dev/programming-guides/json/)で要求される`bytes_value`のBase64エンコード/デコードを行っていませんでした。新関数導入により正しい処理を行い、旧関数は後方互換のため非推奨となりました。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error`メトリクスのカウント誤りを修正しました。

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTTユーザー名に非ASCII文字が含まれる場合のネットワーク輻輳アラームメッセージでのクラッシュを修正しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価時に発生する過剰な監査ログ出力を削減しました。

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログクリーンアップ時のMnesiaトランザクションブロックによる急激なメモリ増加を防止しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPPゲートウェイのグローバルチェーン名が誤っていた問題を修正しました。これらゲートウェイの組み込み認証データは以前`unknown:global`にまとめられており、ゲートウェイ間で競合が発生していました。

- [#15699](https://github.com/emqx/emqx/pull/15699) ノード停止・再起動時にゲートウェイ（例：CoAP）の組み込み認証データが誤って削除される問題を修正しました。

- [#15822](https://github.com/emqx/emqx/pull/15822) 一定数のメッセージ送信後にOCPP接続がクラッシュする問題を修正しました。

#### レート制限

- [#15794](https://github.com/emqx/emqx/pull/15794) リスナー設定更新後に接続レート制限の変更（バーストレートやレート閾値など）が即時反映されるよう動作を改善しました。以前は内部リミッター状態の一部が正しく更新されず、設定より厳しいレート制限が適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHookのTLSオプションを修正し、gRPCクライアントがTLSハンドシェイク時にサーバーホスト名を正しく検証できるようにしました。

## 5.9.2

*リリース日: 2025-11-14*

EMQX 5.9.2へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#15773](https://github.com/emqx/emqx/pull/15773) 再接続時のクライアントID登録をスロットリングしました。

  - 以前のセッションクリーンアップが進行中の場合、同じクライアントIDを使う新規接続はスロットリングされます。これによりクライアントが過剰に再接続する際の不安定性を防止します。
  - 影響を受けるクライアントは`CONNACK`で理由コード`137`（Server Busy）と理由文字列`"THROTTLED"`を受け取り、クリーンアップ完了後に再試行すべきです。
  - 同じクライアントIDを登録する別の接続が返す理由コードを修正し、`133`ではなく正しく`137`を返します。

#### データ統合

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud`ライブラリを3.8.3.0にアップグレードしました。これにより、EC2インスタンスが適切なIAM権限を持つ場合、アクセスキーIDやシークレットアクセスキーを指定せずにS3コネクターをセットアップ可能です。

- [#15585](https://github.com/emqx/emqx/pull/15585) brodクライアントを4.4.4に更新し、より広範なKafka APIをサポートしました。これにより`JoinGroups` APIバージョン`v0`～`v1`の非推奨対応が含まれます。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTTコネクターの`static_clientids`設定でクライアントIDごとにユーザー名とパスワードを指定可能になりました。これによりAzure IoT Hubのように各デバイスが固有の認証情報を必要とするシナリオで、クラスター環境の複数ノード間での接続成功率が向上します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTPアクションのHTTPリクエストタイムアウトを`resource_opts.request_ttl`設定で調整可能にしました。以前は30秒の固定値でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に無効化できるAPIエンドポイントを追加しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS認証のコネクターでリソースが`disconnected`とマークされた際に返される情報を改善しました。

#### パフォーマンス

- [#15536](https://github.com/emqx/emqx/pull/15536) `node.global_gc_interval`設定をデフォルトで無効化しました。

- [#15539](https://github.com/emqx/emqx/pull/15539) Erlang VMパラメーターを最適化し、性能と安定性を向上しました：

  - 分散チャネルのバッファサイズを32MB（`+zdbbl 32768`）に増加し、Mnesia集中的操作時の`busy_dist_port`アラームを防止。
  - スケジューラのビジーウェイティングを無効化（`+sbwt none +sbwtdcpu none +sbwtdio none`）し、OSから見たCPU使用率を低減。
  - スケジューラのバインディングタイプを`db`に設定し、メッセージレイテンシを低減。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。

  - クライアント切断時に認可キャッシュを即時クリアし、不要なメモリ消費を削減。
  - クライアントID、ユーザー名、パスワード、トピックなどのフィールドを、64バイト超の場合は生パケットのスライスではなく新しいバイナリにコピーし、Erlang VMのバイナリメモリ使用を削減。

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の`parse_unit`オプションのデフォルト値を`chunk`から`frame`に変更しました。これによりペイロードサイズがソケットバッファ（デフォルト4KB）を超える場合のCPU使用率が大幅に削減されます。

  **注意**：`parse_unit = frame`の場合、`PUBLISH`パケットが最大許容サイズを超えると、EMQXは`DISCONNECT`パケットを送信せず接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` APIのパフォーマンスを最適化しました。以前はクラスターに約5万クライアント以上存在するとクライアントリスト取得APIが非常に遅くなったりタイムアウトしたりしていました。

- [#15884](https://github.com/emqx/emqx/pull/15884) 稀にグローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無限に保持する問題を解決しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際にクラスター内のルーティングテーブルおよび共有サブスクリプション状態に蓄積的不整合が生じる競合状態を解消しました。

- [#15872](https://github.com/emqx/emqx/pull/15872) CONNACK送信後に非ゼロ理由コードで切断された際の`unclean_terminate`警告ログを削除しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helmチャートの問題を修正しました。デフォルト値でEMQXをデプロイすると複数レプリカが起動し、1ノード以外がクラッシュしていました。クラスタ展開はCommercial Licenseが必要なため、チャートのデフォルトは単一レプリカに変更されました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 5.9未満からのローリングアップグレード時のノード起動失敗を修正しました。

  以前のEMQXバージョン（5.9未満）ではZIPタイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS時間形式の30または31番目の2秒スロットに対応）が格納されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームメッセージを修正し、ライブ接続数ではなくセッションクォータを正しく反映するようにしました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTPを26.2.5.2から26.2.5.14にアップグレードしました。これにはEMQXに影響するTLS関連の2つの修正が含まれます：

  - 証明書更新時の競合状態によるTLS接続クラッシュを修正。
  - RSASSA-PSSパラメーターで署名されたRSA証明書のサポートを追加。以前はこれらの証明書が`bad_certificate`/`invalid_signature`エラーでTLSハンドシェイク失敗を引き起こしていました。

- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO無効化後もOIDC SSO関連ログが出力される問題を修正しました。

- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境でOIDCログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACLルールの処理を修正しました。

  以前はこれらのルールが内部的に`#`に変換され、MQTT仕様の制限により`$`で始まるトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`$`で始まるトピックを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) ビルトインデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーはAPI経由で削除できずAPIパスを破壊するためです。

  空ユーザーを削除したい場合はEMQXコンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可キャッシュを即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

#### データ統合

- [#15616](https://github.com/emqx/emqx/pull/15616) Kafka接続は、デフォルトのプローブトピックに対して`topic_authorization_failed`エラーが返された場合でも正常とみなすようになりました。

- [#15826](https://github.com/emqx/emqx/pull/15826) Kafkaコンシューマーコネクターの制限付きACL下でのヘルスチェック挙動を改善しました。以前はKafkaブローカーがヘルスチェックに使う内部`____emqx_consumer_probe`コンシューマーグループへのアクセス権がない場合にヘルスチェックが失敗していました。修正後はKafkaブローカーが「ACL拒否」応答を返しても接続は正常とみなします。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDBドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDBアクションで不正な書き込み構文を使った場合に発生する`function_clause`エラーを修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafkaコンシューマーソースの追加失敗時（例：トピックACL拒否）に返される情報を充実させました。

- [#15850](https://github.com/emqx/emqx/pull/15850) MQTTブリッジが古い接続を`Connected`として誤表示し、自動再接続しない問題を修正しました。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafkaプロデューサーライブラリ`wollf`を4.0.12にアップグレードし、Kafkaメタデータ応答で一時的にパーティションが欠落する場合の処理を改善しました。

  稀な競合状態でKafkaが不完全なパーティションリストを返すことがあります。以前はトピック再作成でパーティション数が減った場合のみ対応していましたが、一時的にパーティションが欠落する場合は対応していませんでした。このギャップによりパーティションプロデューサーがスタックしシャットダウンが無限にブロックされる可能性がありました。

- [#15906](https://github.com/emqx/emqx/pull/15906) KafkaプロデューサーライブラリWolffを4.0.12から4.0.13にアップグレードし、`ProduceResponse`の`record_list_too_large`エラー処理を追加しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTTクライアントライブラリを1.13.8にアップグレードしました。これによりMQTTブリッジの接続性が改善されます：

  - ピアブローカーがPINGRESPに応答しない場合にコネクターが自動再接続します。
  - CONNACK待機中にTLS接続が切断された場合のブリッジ障害処理が迅速化されます。

- [#15910](https://github.com/emqx/emqx/pull/15910) 大規模ワーカープールで複数ワーカーが同時にクラッシュした場合に、ワーカープールが障害から回復できない問題を修正しました。

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

  `gun`および関連依存を2.1.0にアップグレードしました。

#### API

- [#15547](https://github.com/emqx/emqx/pull/15547) REST APIで大きなボディ（例：10MB）を含むHTTPリクエスト処理に失敗する問題を修正しました。

- [#15797](https://github.com/emqx/emqx/pull/15797) EMQX 4.xとの互換性向上のため、バッチパブリッシュHTTP API（`/api/v5/publish/bulk`）に`encoding`パラメーターを`payload_encoding`のエイリアスとして再導入しました。これによりEMQX v4 APIを利用する既存統合の移行問題を解決し、ソフトウェアレベルの変更なしに動作継続が可能です。

#### レート制限

- [#15794](https://github.com/emqx/emqx/pull/15794) リスナー設定更新後に接続レート制限の変更（バーストレートやレート閾値など）が即時反映されるよう動作を改善しました。以前は内部リミッター状態の一部が正しく更新されず、設定より厳しいレート制限が適用されることがありました。

#### 可観測性

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTTユーザー名に非ASCII文字が含まれる場合のネットワーク輻輳アラームメッセージでのクラッシュを修正しました。

#### ゲートウェイ

- [#15342](https://github.com/emqx/emqx/pull/15342) 未定義のパケットフィールドを参照するクライアント情報オーバーライドテンプレートが原因でNATSゲートウェイクラッシュが発生する問題を修正しました。システムは未定義のアトムの代わりに空のバイナリを返します。

## 5.10.1

*リリース日: 2025-09-18*

EMQX 5.10.1へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### パフォーマンス

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可キャッシュを即時クリアし、不要なメモリ消費を削減しました。

- [#15907](https://github.com/emqx/emqx/pull/15907) システムメモリ使用量を改善しました。クライアントID、ユーザー名、パスワード、トピックなどのフィールドは、生パケットのスライスではなく64バイト超の場合は新しいバイナリにコピーされ、Erlang VMのバイナリメモリ使用を削減します。

#### アクセス制御

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP認証・認可を強化しました。

  LDAP認可はJSONを使った拡張ACLルール形式をサポートし、従来の単純なトピックリストに加え、クライアント情報に基づくLDAPからのACLルール取得とクライアントのメタデータへのキャッシュをサポートします。これにより認可時のLDAP問い合わせを繰り返し回避します。

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証・認可プロバイダーが無効化されているリソースへの接続をEMQXが維持し続ける問題を修正し、外部リソース管理を最適化しました。

#### データ統合

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 TablesアクションでParquet形式のデータファイル書き込みをサポートしました。

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis Producerコネクターとアクションのヘルスチェックにレート制限を追加し、AWS APIクォータに準拠しクラスター動作を改善しました。

  - `ListStreams`と`DescribeStream`へのヘルスチェック呼び出しはそれぞれコネクターごとに5/s、10/sに制限され、AWSレート制限に合わせています。
  - 分散リミッターはクラスター内のコアノードで調整され、一貫した制限を実現します。
  - ヘルスチェックがスロットリングまたはタイムアウトした場合、コネクターやアクションは切断状態にせず前回の状態を保持します。

  また、新しい`resource_opts.health_check_interval_jitter`を導入し、`resource_opts.health_check_interval`に一様ランダム遅延を加え、同一コネクター下の複数アクションが同時にヘルスチェックを実行する確率を減らします。

- [#15542](https://github.com/emqx/emqx/pull/15542) `erlcloud`ライブラリを3.8.3.0にアップグレードしました。EC2インスタンスが適切なIAM権限を持つ場合、アクセスキーIDやシークレットアクセスキーを指定せずにS3コネクターをセットアップ可能です。

- [#15845](https://github.com/emqx/emqx/pull/15845) MQTTコネクターの`static_clientids`設定でクライアントIDごとにユーザー名とパスワードを指定可能になりました。Azure IoT Hubのように各デバイスが固有の認証情報を必要とするシナリオで、クラスター環境の複数ノード間での接続成功率が向上します。

- [#15911](https://github.com/emqx/emqx/pull/15911) HTTPアクションのHTTPリクエストタイムアウトを`resource_opts.request_ttl`設定で調整可能にしました。以前は30秒の固定値でした。

#### 可観測性

- [#15499](https://github.com/emqx/emqx/pull/15499) 管理者がアクティブなアラームを強制的に無効化できるAPIエンドポイントを追加しました。

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry gRPC（HTTP/2経由）統合にカスタムHTTPヘッダー設定を追加し、HTTP認証が必要なコレクターに対応しました。

- [#15944](https://github.com/emqx/emqx/pull/15944) LDAP、Syskeeper、IoTDB、Snowflake（集約）、JWKS認証のコネクターでリソースが`disconnected`とマークされた際に返される情報を改善しました。

- [#15371](https://github.com/emqx/emqx/pull/15371) `GET /actions_summary`と`GET /sources_summary`のレスポンスおよび`GET /actions/:id`のフォールバックアクションに`tags`フィールドを追加しました。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump`ツールが現在のシステム設定をHOCON形式でエクスポートするようになり、パスワードやシークレットなどの機密情報は自動的にマスクされます。

### バグ修正

#### コアMQTT機能

- [#15361](https://github.com/emqx/emqx/pull/15361) 長さが不正（短すぎる）な`User-Property`ペアの解析時に発生する`function_clause`エラーを修正しました。

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断されたクライアントの共有サブスクリプションに対する冗長なクリーンアップ処理を削除しました。これらは高切断量時にクラッシュを引き起こし、グローバルブローカー状態の不整合を招いていました。

- [#15416](https://github.com/emqx/emqx/pull/15416) WebSocket接続のセッション期限切れ時に発生する警告ログとクラッシュを修正しました。これは最近のWebSocket性能改善で導入された問題で、ブローカー容量には影響しませんが以下のようなログを生成していました：

  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15872](https://github.com/emqx/emqx/pull/15872) 非ゼロ理由コードのCONNACK送信後の切断時に発生する`unclean_terminate`警告ログを削除しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時に切断された際にクラスター内のルーティングテーブルおよび共有サブスクリプション状態に蓄積的不整合が生じる競合状態を解消しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACLルールの処理を修正しました。

  以前はこれらのルールが内部的に`#`に変換され、MQTT仕様の制限により`$`で始まるトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`$`で始まるトピックを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) ビルトインデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーはAPI経由で削除できずAPIパスを破壊するためです。

  空ユーザーを削除したい場合はEMQXコンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可キャッシュを即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

#### デプロイメント

- [#15553](https://github.com/emqx/emqx/pull/15553) Helmチャートの問題を修正しました。デフォルト値でEMQXをデプロイすると複数レプリカが起動し、1ノード以外がクラッシュしていました。クラスタ展開はCommercial Licenseが必要なため、チャートのデフォルトは単一レプリカに変更されました。

- [#15712](https://github.com/emqx/emqx/pull/15712) 5.9未満からのローリングアップグレード時のノード起動失敗を修正しました。

  以前のEMQXバージョン（5.9未満）ではZIPタイムスタンプエンコーダのバグにより、アーカイブエントリに無効な「秒」値（DOS時間形式の30または31番目の2秒スロットに対応）が格納されていました。

- [#15863](https://github.com/emqx/emqx/pull/15863) ライセンスクォータアラームメッセージを修正し、ライブ接続数ではなくセッションクォータを正しく反映するようにしました。

#### セキュリティ

- [#15581](https://github.com/emqx/emqx/pull/15581) Erlang/OTPを26.2.5.2から26.2.5.14にアップグレードしました。これにはEMQXに影響するTLS関連の2つの修正が含まれます：

  - 証明書更新時の競合状態によるTLS接続クラッシュを修正。
  - RSASSA-PSSパラメーターで署名されたRSA証明書のサポートを追加。以前はこれらの証明書が`bad_certificate`/`invalid_signature`エラーでTLSハンドシェイク失敗を引き起こしていました。

- [#16237](https://github.com/emqx/emqx/pull/16237) OIDC SSO無効化後もOIDC SSO関連ログが出力される問題を修正しました。

- [#16217](https://github.com/emqx/emqx/pull/16217) マルチノードクラスター環境でOIDCログインコールバックがユーザーセッションを見つけられない問題を修正しました。

#### アクセス制御

- [#15818](https://github.com/emqx/emqx/pull/15818) `{allow|deny, all}` ACLルールの処理を修正しました。

  以前はこれらのルールが内部的に`#`に変換され、MQTT仕様の制限により`$`で始まるトピック（例：`$testtopic/1`）にマッチしませんでした。現在は特別な内部値を使い、`$`で始まるトピックを含む任意のトピックに正しくマッチします。

- [#15844](https://github.com/emqx/emqx/pull/15844) ビルトインデータベース認証器に空のユーザー名を追加することを禁止するバリデーションを追加しました。空ユーザーはAPI経由で削除できずAPIパスを破壊するためです。

  空ユーザーを削除したい場合はEMQXコンソールで以下を実行してください：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#15899](https://github.com/emqx/emqx/pull/15899) クライアント切断時に認可キャッシュを即時クリアし、不要なメモリ消費を削減することでメモリ管理を改善しました。

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証およびメモリベースセッションを使うクライアントが`session_stepdown_request_exception`の`calling_self`エラーでクラッシュする問題を修正しました。

  <details>
  <summary>エラーログ</summary>

  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### データ統合

- [#15616](https://github.com/emqx/emqx/pull/15616) Kafka接続は、デフォルトのプローブトピックに対して`topic_authorization_failed`エラーが返された場合でも正常とみなすようになりました。

- [#15826](https://github.com/emqx/emqx/pull/15826) Kafkaコンシューマーコネクターの制限付きACL下でのヘルスチェック挙動を改善しました。以前はKafkaブローカーがヘルスチェックに使う内部`____emqx_consumer_probe`コンシューマーグループへのアクセス権がない場合にヘルスチェックが失敗していました。修正後はKafkaブローカーが「ACL拒否」応答を返しても接続は正常とみなします。

- [#15827](https://github.com/emqx/emqx/pull/15827) GreptimeDBドライバーのアトムおよびプロセスリークを修正しました。

  GreptimeDBアクションで不正な書き込み構文を使った場合に発生する`function_clause`エラーを修正しました。

- [#15836](https://github.com/emqx/emqx/pull/15836) Kafkaコンシューマーソースの追加失敗時（例：トピックACL拒否）に返される情報を充実させました。

- [#15850](https://github.com/emqx/emqx/pull/15850) MQTTブリッジが古い接続を`Connected`として誤表示し、自動再接続しない問題を修正しました。

- [#15866](https://github.com/emqx/emqx/pull/15866) Kafkaプロデューサーライブラリ`wollf`を4.0.12にアップグレードし、Kafkaメタデータ応答で一時的にパーティションが欠落する場合の処理を改善しました。

  稀な競合状態でKafkaが不完全なパーティションリストを返すことがあります。以前はトピック再作成でパーティション数が減った場合のみ対応していましたが、一時的にパーティションが欠落する場合は対応していませんでした。このギャップによりパーティションプロデューサーがスタックしシャットダウンが無限にブロックされる可能性がありました。

- [#15906](https://github.com/emqx/emqx/pull/15906) KafkaプロデューサーライブラリWolffを4.0.12から4.0.13にアップグレードし、`ProduceResponse`の`record_list_too_large`エラー処理を追加しました。

- [#15902](https://github.com/emqx/emqx/pull/15902) MQTTクライアントライブラリを1.13.8にアップグレードしました。これによりMQTTブリッジの接続性が改善されます：

  - ピアブローカーがPINGRESPに応答しない場合にコネクターが自動再接続します。
  - CONNACK待機中にTLS接続が切断された場合のブリッジ障害処理が迅速化されます。

- [#15910](https://github.com/emqx/emqx/pull/15910) 大規模ワーカープールで複数ワーカーが同時にクラッシュした場合に、ワーカープールが障害から回復できない問題を修正しました。

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

  `gun`および関連依存を2.1.0にアップグレードしました。

- [#16010](https://github.com/emqx/emqx/pull/16010) ルールのSQLに`metadata`フィールドが含まれていない場合に、Republishフォールバックアクションが`function_clause`エラーで失敗する問題を修正しました。

  エラーログ例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) Kafkaデータ統合で`not_all_kafka_partitions_connected`イベント発生時のログ詳細を改善しました。

- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを持つコネクターを含む設定のロードや再起動時に発生する可能性のあるOOMクラッシュを修正しました。

- [#16138](https://github.com/emqx/emqx/pull/16138) Redisクラスターフェイルオーバー問題を修正しました。以前はRedisクラスタクライアントが通常のクエリ失敗時のみクラスタートポロジーを更新し、定期的な`PING`失敗では更新しませんでした。これによりフェイルオーバー後に古いトポロジーを使い続け、接続状態が「接続中」のままになることがありました。修正後は`PING`失敗もトポロジー更新をトリガーし、コネクターがフェイルオーバーを検知して迅速に復旧します。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの`jq`関数のメモリリークを修正しました。

#### スマートデータハブ

- [#15706](https://github.com/emqx/emqx/pull/15706) メッセージ変換やスキーマ検証のトピックインデックスが不整合になる問題を修正しました。1つのアイテムを削除するとトピックインデックスが破損し、無効化したはずの次のアイテムが有効なままになる問題がありました。

- [#15708](https://github.com/emqx/emqx/pull/15708) ノード再起動後に外部スキーマレジストリがリロードされない問題を修正しました。

- [#15810](https://github.com/emqx/emqx/pull/15810) `bytes_value`メトリクスの処理を修正する`spb_{en,de}code`関数を導入しました。従来の`sparkplug_{en,de}code`関数は[Protobuf仕様](https://protobuf.dev/programming-guides/json/)で要求される`bytes_value`のBase64エンコード/デコードを行っていませんでした。新関数導入により正しい処理を行い、旧関数は後方互換のため非推奨となりました。

#### 可観測性

- [#15639](https://github.com/emqx/emqx/pull/15639) `packets.subscribe.auth_error`メトリクスのカウント誤りを修正しました。

- [#15785](https://github.com/emqx/emqx/pull/15785) MQTTユーザー名に非ASCII文字が含まれる場合のネットワーク輻輳アラームメッセージでのクラッシュを修正しました。

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価時に発生する過剰な監査ログ出力を削減しました。

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量の監査ログクリーンアップ時のMnesiaトランザクションブロックによる急激なメモリ増加を防止しました。

#### ゲートウェイ

- [#15679](https://github.com/emqx/emqx/pull/15679) ExProto、JT/T 808、GB/T 32960、OCPPゲートウェイのグローバルチェーン名が誤っていた問題を修正しました。これらゲートウェイの組み込み認証データは以前`unknown:global`にまとめられており、ゲートウェイ間で競合が発生していました。

- [#15699](https://github.com/emqx/emqx/pull/15699) ノード停止・再起動時にゲートウェイ（例：CoAP）の組み込み認証データが誤って削除される問題を修正しました。

- [#15822](https://github.com/emqx/emqx/pull/15822) 一定数のメッセージ送信後にOCPP接続がクラッシュする問題を修正しました。

#### レート制限

- [#15794](https://github.com/emqx/emqx/pull/15794) リスナー設定更新後に接続レート制限の変更（バーストレートやレート閾値など）が即時反映されるよう動作を改善しました。以前は内部リミッター状態の一部が正しく更新されず、設定より厳しいレート制限が適用されることがありました。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) ExHookのTLSオプションを修正し、gRPCクライアントがTLSハンドシェイク時にサーバーホスト名を正しく検証できるようにしました。

## 5.9.1

*リリース日: 2025-07-02*

EMQX 5.9.1へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

- [#15364](https://github.com/emqx/emqx/pull/15364) OpenTelemetry gRPC（HTTP/2経由）統合にカスタムHTTPヘッダー設定を追加し、HTTP認証が必要なコレクターに対応しました。

- [#15160](https://github.com/emqx/emqx/pull/15160) マルチテナンシー管理用に名前空間を一括削除する`DELETE /mt/bulk_delete_ns` APIを追加しました。

- [#15158](https://github.com/emqx/emqx/pull/15158) 新しい`emqx ctl conf remove x.y.z`コマンドを追加し、既存設定からキー`x.y.z`を削除可能にしました。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflakeコネクターでパスワードの代わりに秘密鍵ファイルパスを指定可能にしました。

  ユーザーはパスワード、秘密鍵、またはどちらも使わない（`/etc/odbc.ini`で設定）を選択可能です。

- [#15043](https://github.com/emqx/emqx/pull/15043) DS Raftバックエンドに基本的なメトリクスを追加し、クラスター状態、データベース概要、シャードレプリケーション、レプリカ遷移を可視化可能にしました。

### バグ修正

#### データ統合

- [#15331](https://github.com/emqx/emqx/pull/15331) InfluxDBアクションで`WriteSyntax`の`timestamp`が空欄かつルールにタイムスタンプフィールドがない場合にラインプロトコル変換が失敗する問題を修正しました。現在はシステムの現在ミリ秒値を使用し、ミリ秒精度を強制します。

- [#15274](https://github.com/emqx/emqx/pull/15274) Postgres、Matrix、TimescaleDBコネクターのヘルスチェック失敗時に完全再接続をトリガーするようにし、接続が使えなくなって操作がハングする問題を防止しました。

- [#15154](https://github.com/emqx/emqx/pull/15154) 集約モード（S3、Azure Blob Storage、Snowflake）で稀に発生するアクションの競合状態を修正し、以下のようなクラッシュログを防止しました：

  ```
  ** Reason for termination ==
  ** {function_clause,[{emqx_connector_aggregator,handle_close_buffer,[...], ...
  ```

- [#15147](https://github.com/emqx/emqx/pull/15147) ルールテスト時に一部アクションがリクエスト描画後にトレースイベントを発行しない問題を修正しました。

  対象アクション：

  - Couchbase
  - Snowflake
  - IoTDB（Thriftドライバー）

- [#15383](https://github.com/emqx/emqx/pull/15383) MQTTブリッジの潜在的リソースリークを修正しました。ブリッジ起動失敗時にトピックインデックステーブルが正しくクリーンアップされていませんでした。

#### スマートデータハブ

- [#15224](https://github.com/emqx/emqx/pull/15224) Dashboard経由で外部スキーマレジストリを更新するとパスワードが`******`に上書きされる問題を修正しました。更新時にパスワードが正しく保持されます。

- [#15190](https://github.com/emqx/emqx/pull/15190) メッセージ変換でQoSとトピックのハードコード値設定をサポートしました。

#### 可観測性

- [#15299](https://github.com/emqx/emqx/pull/15299) OpenTelemetryメトリクスエクスポート時の`badarg`エラーを修正しました。

#### テレメトリー

- [#15216](https://github.com/emqx/emqx/pull/15216) プラグイン有効化時に`emqx_telemetry`プロセスがクラッシュする問題を修正しました。

#### アクセス制御

- [#15184](https://github.com/emqx/emqx/pull/15184) ブラックリスト作成失敗時のエラーメッセージフォーマットを修正しました。

#### クラスター

- [#15180](https://github.com/emqx/emqx/pull/15180) `ekka_locker`のRPC（`badrpc`）エラー処理を修正し、誤検知によるロック成功判定を防止しました。これによりクラスター展開でのロック状態不整合やデッドロックを防止します。

#### セキュリティ

- [#15159](https://github.com/emqx/emqx/pull/15159) CRL配布ポイント（CDP）URLの連続失敗時にリフレッシュを停止し、エラーログの過剰出力を防止しました（デフォルトタイムアウト60秒）。

## 5.9.0

*リリース日: 2025-05-02*

EMQX 5.9.0へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コアMQTT機能

- [#14721](https://github.com/emqx/emqx/pull/14721) 遅延パブリッシュの間隔制限を4294967秒（約49.7日）から42949670秒（約497日）に変更しました。

- [#14595](https://github.com/emqx/emqx/pull/14595) `retainer.enable`フラグを非推奨化しました。リテイナーはゾーン設定の`mqtt.retain_available`フラグに基づき自動的に起動・停止します。

#### インストールとデプロイメント

- [#14930](https://github.com/emqx/emqx/pull/14930) macOS 15（Sequoia）向けパッケージのリリースを開始しました。

- [#14590](https://github.com/emqx/emqx/pull/14590) 評価ライセンス下のノードの最大アップタイムを1ヶ月に制限しました。アップタイム制限に達すると新規接続を拒否します。

#### ネームスペース

- [#14261](https://github.com/emqx/emqx/pull/14261) MQTTクライアント管理のためのネームスペース機能を強化しました。

  **新機能**：

  - ネームスペースクライアント認識：`tns`属性を持つMQTTクライアントをネームスペースクライアントとして扱います。
  - ネームスペースインデックス：クライアントIDインデックスにMQTTクライアントネームスペース（`tns`）を追加し、マルチテナンシーシナリオをサポートします。

  **API**：

  - ネームスペース一覧取得（ページネーション対応）：`/api/v5/mt/ns_list`
  - ネームスペース内クライアントセッション一覧取得（ページネーション対応）：`/api/v5/mt/:ns/client_list`
  - ネームスペース内アクティブクライアントセッション数取得：`/api/v5/mt/:ns/client_count`

  **設定**：

  - ネームスペースごとのセッション制限：`multi_tenancy.default_max_sessions`設定を追加しました。

  注：

  - 管理者ネームスペース（管理ユーザーグループ）はこのプルリクエストに含まれておらず開発中です。

- [#14884](https://github.com/emqx/emqx/pull/14884) ネームスペース設定管理用のHTTP APIを追加しました。

- [#14840](https://github.com/emqx/emqx/pull/14840) ネームスペース機能のクライアントおよびテナントレートリミッター設定用HTTP APIエンドポイントを追加しました。

#### 認証・認可

- [#14584](https://github.com/emqx/emqx/pull/14584) Dashboard 2FA（2要素認証）ログイン用の認証アプリをサポートしました。

- [#14979](https://github.com/emqx/emqx/pull/14979) 認証・認可テンプレートに`zone`および`listener`を追加し、ACLルールの`who`マッチ条件に`zone`と`listener`を追加しました。

  これによりリスナーやゾーンごとのアクセス制御が可能になります。例：

  - HTTP認証器へのリクエストボディテンプレートで`{"username": "${username}", "zone": "${zone}"}`のように`zone`名を送信。
  - `acl.conf`でSSLリスナー経由のクライアントのみ`${username}/#`へのサブスクライブを許可：`{allow, {listener, "ssl:default"}, subscribe, ["${username}/#"]}.`

- [#14976](https://github.com/emqx/emqx/pull/14976) 認証器の事前条件設定をサポートしました。

  これによりクライアント情報に基づく認証器の選択的呼び出しが可能になり、不必要な認証リクエストを回避できます。例：`tcp:default`経由のクライアントのみHTTP認証器を呼び出し、`ssl:default`経由のクライアントのみPostgre認証器を呼び出す設定。

- [#14966](https://github.com/emqx/emqx/pull/14966) デフォルトDashboard管理ユーザーの削除を可能にしました。ただし少なくとももう1人の管理ユーザーが存在する必要があります。

- [#14358](https://github.com/emqx/emqx/pull/14358) LDAP認証・認可テンプレートで使用可能な変数を制限し、他の認証・認可ソースで許可されているもののみにしました。サポート外変数はレンダリングされません。

- [#14610](https://github.com/emqx/emqx/pull/14610) 外部ソースから取得またはビルトインデータベースに保存された認可ルールの追加フィールドを処理可能にしました。

  新たにサポートするフィルター：

  - `username_re`：ユーザー名の正規表現フィルター
  - `clientid_re`：クライアントIDの正規表現フィルター
  - `ipaddr`：IPアドレス/マスクによるフィルター

  これらのフィルターは存在する場合すべてマッチする場合にのみルールが適用されます。

- [#14329](https://github.com/emqx/emqx/pull/14329) 認証・認可の外部リクエストテンプレートで`${peerport}`変数を利用可能にしました。

- [#14286](https://github.com/emqx/emqx/pull/14286) 認可・認証のノードレベルキャッシュを実装しました。

  一部の認証・認可方式は外部サービス呼び出しを必要とし、クライアントの頻繁な再接続時にEMQXおよび外部サービスへの過負荷を招くことがあります。

  この機能によりHTTP、LDAP、MongoDB、MySQL、PostgreSQL、Redisの認証・認可バックエンドでノードレベルキャッシュを有効化可能です。

#### REST API

- [#14254](https://github.com/emqx/emqx/pull/14254) `/status` HTTPエンドポイントでクラスター名を返すようにしました。

- [#14972](https://github.com/emqx/emqx/pull/14972) 個別プラグイン設定のダウンロード/アップロードAPIを実装しました。

- [#15013](https://github.com/emqx/emqx/pull/15013) ルール情報に新たに`action_details`フィールドを追加しました。これは各ルールが参照するアクションのタイプ、名前、状態を含みます。

- [#14735](https://github.com/emqx/emqx/pull/14735) ルール情報に`last_modified_at`フィールドを追加しました。

#### クラスター

- [#14766](https://github.com/emqx/emqx/pull/14766) Durable Storageデータレプリケーションを担うノードがクラスターから離脱するのを防ぐ`emqx ctl cluster leave`コマンドの安全策を追加しました。

- [#14040](https://github.com/emqx/emqx/pull/14040) ノードリバランス中の内部RPC呼び出しにタイムアウトを追加しました。以前はノードが応答しないとリバランス処理がハングすることがありました。

- [#14892](https://github.com/emqx/emqx/pull/14892) クラスター負荷リバランスを強化しました：

  - コア/レプリカントクラスターの負荷不均衡を修正。以前は条件によってレプリカントの全トランザクションが単一コアノードに送られることがありました。
  - コアノードに対するレプリカントノードのリバランス用CLIコマンドを追加：

    - `emqx_ctl cluster core rebalance plan`
    - `emqx_ctl cluster core rebalance status`
    - `emqx_ctl cluster core rebalance confirm`
    - `emqx_ctl cluster core rebalance abort`

- [#14907](https://github.com/emqx/emqx/pull/14907) ノード避難の安定性を改善しました。以前は避難処理がデッドループに陥り手動介入が必要になることがありました。

#### データ統合

- [#14118](https://github.com/emqx/emqx/pull/14118) MySQLアクションで`ON DUPLICATE KEY UPDATE`をサポートしました。

  例：

  ```
  INSERT INTO t1 (a,b,c) VALUES (${id},${clientid},${qos}) ON DUPLICATE KEY UPDATE a=a;
  ```

  ただし`ON DUPLICATE KEY UPDATE`句内でのプレースホルダー（`${var}`）はサポートしません。

- [#14629](https://github.com/emqx/emqx/pull/14629) S3およびAzure Blob StorageアクションでJSON Linesコンテナタイプをサポートしました。

- [#14642](https://github.com/emqx/emqx/pull/14642) ローカルディスクにJSON Lines形式でイベントログを記録する新しいコネクターおよびアクションタイプを追加しました。

- [#14996](https://github.com/emqx/emqx/pull/14996) RabbitMQアクションでデフォルトエクスチェンジの使用をサポートしました。

- [#14901](https://github.com/emqx/emqx/pull/14901) スキーマレジストリに新しいスキーマタイプ`external_http`を追加しました。このスキーマタイプでは外部HTTPサーバーを設定し、ペイロードに任意の操作を行い結果をルールで使用可能にします。

- [#14722](https://github.com/emqx/emqx/pull/14722) MQTTコネクターに`connect_timeout`オプションを追加しました。これは接続確立待機時間（秒）を制御し、低い値は接続問題のフィードバック時間を改善します。

- [#14615](https://github.com/emqx/emqx/pull/14615) `ehttpc` HTTPドライバーを使う複数統合で`max_inactive`パラメーター設定をサポートしました。

- [#14459](https://github.com/emqx/emqx/pull/14459) フォールバックアクションをサポートしました。

  これは、データ統合アクションでメッセージ処理に失敗した場合（バッファオーバーフローやTTL到達でドロップされた場合も含む）にトリガーされます。すべてのデータ統合アクションで設定可能です。

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

- [#14582](https://github.com/emqx/emqx/pull/14582) 内部JSON表現のシリアライズ前後の不要な前処理・後処理を回避しました。

#### 管理

- [#14845](https://github.com/emqx/emqx/pull/14845) ゲートウェイ設定やリスナー変更時の不要なリスナー再起動を回避しました。

- [#14773](https://github.com/emqx/emqx/pull/14773) ゾーンやリスナーに設定可能なレート制限機能を改善しました。

  - レート制限アルゴリズムを単純化し、クライアントにバックプレッシャーをかけずメッセージをドロップします。QoS1/QoS2メッセージには適切な理由コードも返します。
  - レート制限はランタイムで再設定可能です。
  - レート制限設定でウィンドウサイズとウィンドウあたりの制限を明示的に指定可能です。例：`messages_rate = "300/5m"`は5分あたり300メッセージ（変動あり）、`messages_rate = "10/10s"`は10秒あたり10メッセージ（変動厳格）を意味します。
  - バーストレートは`messages_burst`、`bytes_burst`、`max_conn_burst`ゾーン・リスナーオプションで指定可能です。例：`messages_burst = 1000/h`は1時間あたり1000メッセージの追加送信を許可します。

- [#14341](https://github.com/emqx/emqx/pull/14341) ネームスペース機能でネームスペースごとのセッション数制限をサポートしました。`client.authenticate`フックポイントコールバックで`quota_exceeded`エラー理由をサポートします。

- [#14679](https://github.com/emqx/emqx/pull/14679) `exhook.proto`をv3に更新し、OnMessagePublishコールバックで`User-Property`パラメーターを渡せるようにしました。

- [#14963](https://github.com/emqx/emqx/pull/14963) プラグインに`on_health_check/1`コールバックを追加し、プラグインのヘルス状態をHTTP APIおよびCLIでエクスポート可能にしました。

#### ダッシュボード

- [#14750](https://github.com/emqx/emqx/pull/14750) Dashboardの「共有サブスクリプション」値表示を修正しました。以前は古い値が表示されることがありました。

- [#14638](https://github.com/emqx/emqx/pull/14638) Dashboardのデフォルトパスワードにファイルシークレット（`file://...`）を使うことをサポートしました。

- [#14255](https://github.com/emqx/emqx/pull/14255) Dashboardユーザーのパスワード有効期限機構を導入しました。

- [#15014](https://github.com/emqx/emqx/pull/15014) Dashboardのログイン試行失敗が一定回数続いた場合にログインを一定期間ブロックするセキュリティ強化を行いました。試行回数とロック期間は設定可能です。

- [#15132](https://github.com/emqx/emqx/pull/15132) SAML SSO統合で、Assertion Consumer Service（ACS）のレスポンスのContent-Typeが誤って`application/xml`となっていた問題を修正し、`application/x-www-form-urlencoded`に変更しました。これにより一部IDプロバイダーとの互換性が向上しました。

#### 可観測性

- [#14794](https://github.com/emqx/emqx/pull/14794) Log TraceのHTTP APIインターフェースに`payload_limit`パラメーターを追加しました。以前はペイロードサイズが1024バイトを超えると切り詰められていましたが、現在はこの制限を設定可能です。

- [#14876](https://github.com/emqx/emqx/pull/14876) ルールエンジンのエンドツーエンドトレーシングをサポートしました。以下のエントリをトレース可能です：

  - ルールをトリガーするクライアントパブリッシュメッセージ
  - ルールをトリガーするクライアントイベントおよびアラートイベント
  - ソーストリガーのルール
  - ルールで実行されるアクション

  制限：フォールバックアクショントレーシングは現在未対応です。

- [#14723](https://github.com/emqx/emqx/pull/14723) Prometheus Push Gateway設定に`method`オプションを追加しました。従来は`post`がデフォルトでしたが、`put`に変更しました。

  `put`メソッドはPushgateway内の同じジョブのメトリクスを置き換え、EMQXクラスターから削除されたメトリクスがPushgatewayに残るのを防ぎます。

  詳細は[PUT method](https://github.com/prometheus/pushgateway?tab=readme-ov-file#put-method)を参照してください。

- [#14636](https://github.com/emqx/emqx/pull/14636) `packets.publish.dropped`メトリクスを廃止し、より意味のある以下の2つのメトリクスに置き換えました：

  - `messages.dropped.quota_exceeded`：QoS 0メッセージ数制限超過時にトリガー
  - `messages.dropped.receive_maximum`：QoS 2メッセージのReceive Maximum制限到達時にトリガー

  これらは異なる条件でメッセージがドロップされる理由をより正確に示します。

- [#14540](https://github.com/emqx/emqx/pull/14540) 認証・認可のレイテンシ計測を設定可能にしました。メトリクスはPrometheusヒストグラムとして公開されます。

- [#14264](https://github.com/emqx/emqx/pull/14264) クラッシュダンプファイルにタイムスタンプを追加し、次回クラッシュダンプで上書きされないようにしました。

- [#15119](https://github.com/emqx/emqx/pull/15119) セッションレジストリテーブルサイズのピーク数を示すハイウォーターマークメトリクスを追加しました。Dashboardの概要ページに表示され、セッションリソース使用状況の監視に役立ちます。

- [#15117](https://github.com/emqx/emqx/pull/15117) cinfo認証式評価失敗の警告ログを簡潔化し、クラッシュ誤認を減らしました。

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

- [#14691](https://github.com/emqx/emqx/pull/14691) `emqx ctl data export`コマンドでエクスポートするデータのフィルター指定が可能になりました。`cluster.hocon`のルートキーやテーブルセットを`POST /data/export`同様に指定できます。

#### 設定ファイル

- [#14647](https://github.com/emqx/emqx/pull/14647) `cluster.hocon`のバックアップ間隔を設定可能にしました。単一の設定更新ごとにバックアップを作成するのではなく、複数の変更をまとめてバックアップし、バックアップ数を削減します。

#### プラグイン・拡張

- [#14957](https://github.com/emqx/emqx/pull/14957) プラグイン設定更新処理を強化しました：

  - プラグインの`on_config_changed`コールバック応答を尊重するようにしました。これにより停止中のプラグインでも設定変更時にコールバックが正しく呼ばれます。
  - `on_config_changed`コールバックの結果を尊重する新しい設定更新方式を導入しました。

#### ゲートウェイ

- [#14017](https://github.com/emqx/emqx/pull/14017) GB/T 32960ゲートウェイでカスタマイズされたInfoReportデータメッセージタイプの解析をサポートしました。

#### MQTT over QUIC

- [#14431](https://github.com/emqx/emqx/pull/14431) QUICスタックをquicer 0.2.3に切り替えました：

  - msquic 2.3.8 + パッチ
  - リソース管理の強化
  - リスナーの動的設定変更対応準備

#### システムアップグレード

- [#14639](https://github.com/emqx/emqx/pull/14639) EMQXはErlang/OTP 27でリリースされるようになりました。

### バグ修正

#### コアMQTT機能

- [#14707](https://github.com/emqx/emqx/pull/14707) `strict_mode`時にDUPフラグがセットされたQoS 2のPUBLISHパケットが誤って無効と判定される問題を修正しました。

- [#14192](https://github.com/emqx/emqx/pull/14192) 認証・認可期限切れで切断されたクライアントが遺言メッセージを送信できなかった問題を修正しました。これらクライアントは認可期限切れ直後に切断され、遺言メッセージが認可ルールを通過できませんでした。

- [#14122](https://github.com/emqx/emqx/pull/14122) QoS 2およびQoS 1のパブリッシュメッセージに対する`PUBACK`および`PUBREC`/`PUBCOMP`の処理を修正しました。

  以前はQoS 2およびQoS 1のメッセージに対応しないパケット識別子を参照する`PUBACK`や`PUBREC`/`PUBCOMP`パケットをブローカーが受け入れていました。現在はこのような動作をするクライアントは切断されます。

- [#15106](https://github.com/emqx/emqx/pull/15106) `GET api/v5/clients_v2` APIで`clientid`が重複して返されるバグを修正しました。これは`chaninfo`イベントの誤復活によりクライアントデータが重複していたためです。修正によりクライアントページの重複表示が解消されます。

- [#14906](https://github.com/emqx/emqx/pull/14906) Mriaを0.8.12.1にアップグレードし、予期しない終了シグナルによる警告を削除しました。

  ```
  2025-01-10T20:00:00+00:00 [warning] clientid: C1, msg: emqx_session_mem_unknown_message, message: {'EXIT',<0.123456.0>,normal}
  ```

- [#15084](https://github.com/emqx/emqx/pull/15084) クライアント属性`zone`および`listener`を各種文字列関数の入力として使えるようにしました。

  以前は`zone`や`listener`が内部的にアトムであったため、`regex_match`などの関数で例外が発生していました。

#### インストール

- [#14624](https://github.com/emqx/emqx/pull/14624) macOSリリースパッケージのOpenSSL動的リンク問題を修正しました。

  EMQX ZIPパッケージはmacOSで`quicer`アプリがシステムインストールのOpenSSLを動的リンクしており、EMQXビルドプロセスで署名されていませんでした。現在はOpenSSLの動的リンクを無効化し、macOS 13以降でもEMQXが確実に起動するようにしました。

#### REST API

- [#14771](https://github.com/emqx/emqx/pull/14771) `GET /clients_v2` HTTP APIが要求した制限を超える結果を返す問題を修正しました。

  注意：ローリングアップグレード中はこのAPIが全クライアントをリストできない場合があります。回避策として古いコアノードにAPIリクエストを送ると全クライアントが取得可能です。

- [#14182](https://github.com/emqx/emqx/pull/14182) 遅延メッセージを`POST /publish` HTTP APIでパブリッシュすると、`202`応答と理由コード16（"no matching subscribers"）が返されていましたが、現在はメッセージ識別子とともに`200`応答を返します。

#### MQTT耐久セッション

- [#14674](https://github.com/emqx/emqx/pull/14674) EMQX耐久ストレージが作成するRocksDBのinfoログファイル数とサイズを制限しました。

- [#14498](https://github.com/emqx/emqx/pull/14498) 耐久セッションのパフォーマンスを改善しました：

  - アイドル耐久セッションはCPUサイクルを消費しなくなりました。
  - QoSアップグレード機能を修正し、有効時にサブスクライバーがサブスクライブQoSより高いQoSのメッセージを受信しなくなりました。

- [#14933](https://github.com/emqx/emqx/pull/14933) DS Raftバックエンドの耐久ストレージが長期間クラスターを離れたストレージサイトに割り当てられる稀なエッジケースを修正しました。

#### 認証・認可

- [#14777](https://github.com/emqx/emqx/pull/14777) JWT認証の設定更新を修正しました。外部JWKSエンドポイント設定で一部フィールドが正しく更新されない問題を修正しました。

- [#14556](https://github.com/emqx/emqx/pull/14556) ノード起動またはシャットダウン中に誤認証が発生する稀な問題を修正しました。

- [#15059](https://github.com/emqx/emqx/pull/15059) Redis認証設定の無効値更新に対する反応を修正しました。

  以前は認証器がクラッシュし認証時に適用されなくなっていました。現在はユーザーに適切なエラーを返し、更新を拒否します。

- [#14303](https://github.com/emqx/emqx/pull/14303) `scram:http`認証の問題を修正しました。HTTPコネクターへのリクエストが不正で認証失敗していました。

#### クラスター

- [#14778](https://github.com/emqx/emqx/pull/14778) `data/certs`または`data/authz`ディレクトリに壊れたシンボリックリンクがあるとノードがクラスタ参加に失敗する問題を修正しました。

- [#14936](https://github.com/emqx/emqx/pull/14936) 稀にグローバルルーティングテーブルが長期間クラスターを離れたノードのルーティング情報を無限に保持する問題を解決しました。

-
