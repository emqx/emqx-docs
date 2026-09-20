# EMQX Enterprise Version 6

## 6.0.3

*リリース日: 2026-06-17*

EMQX 6.0.3 へのアップグレード前に、破壊的変更点および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ強化

- [#17040](https://github.com/emqx/emqx/pull/17040) Dashboard のユーザーアカウント管理エンドポイントへの API キーアクセスを制限しました。

  以前は、`administrator` ロールを持つ API キーが HTTP Basic 認証を用いて Dashboard のユーザー管理エンドポイント `POST/DELETE /users/:username/mfa` および `POST /users/:username/change_pwd` を呼び出せました。これにより、API キーが他の Dashboard ユーザーの MFA をリセットまたは無効化したり、パスワードを変更したりできてしまい、本来の人間の Dashboard セッションと機械の API キーの分離が回避されていました。

  これらのエンドポイントは API キー経由でアクセスすると `401 API_KEY_NOT_ALLOW` を返すようになり、既存の `/users`、`/users/:username`、`/logout`、`/api_key` への API キーアクセス制限ポリシーと整合しています。Dashboard ユーザーは引き続き Dashboard UI からベアラートークン（JWT）セッションを使って自身の MFA とパスワードを管理できます。

- [#17065](https://github.com/emqx/emqx/pull/17065) ルールエンジンから到達可能なコネクターおよびブリッジ設定に対する SSRF 保護を追加しました。

  `rule_engine.ssrf.enable` を `true` に設定すると、EMQX はコネクター、ブリッジ、アクションの設定に対してアウトバウンド SSRF ポリシーを適用します。ポリシーはターゲットを評価し、`rule_engine.ssrf.deny_hosts` の完全一致は即座に拒否され、解決されたターゲット IP は `rule_engine.ssrf.allow_cidrs` と `rule_engine.ssrf.deny_cidrs` に照らしてチェックされます。デフォルトの拒否範囲にはループバック、リンクローカル（クラウドのインスタンスメタデータエンドポイント含む）、RFC 1918、ULA、未指定、マルチキャスト範囲が含まれます。チェックは設定更新時に実行され、HTTP の `url` フィールドだけでなく、すべてのコネクター系の `server`、`servers`、`bootstrap_hosts` フィールドにも適用されます。

  この機能はデフォルトで無効化されており、内部サービスを正当に指すコネクターとの互換性を保ちます。マルチテナントや外部公開環境の運用者は、ネットワーク層のイグレスファイアウォールと併せて有効化を推奨します。

- [#17173](https://github.com/emqx/emqx/pull/17173) API キーによる Dashboard アカウントおよび API キーのデータバックアップエンドポイント経由のエクスポート・インポートを制限しました。

  API キーで呼び出された `POST /data/export` は、生成されるアーカイブから `dashboard_users` と `api_keys` の mnesia テーブルセットを静かに省略します。API キーで呼び出された `POST /data/import` は、アップロードされたバックアップにこれらのテーブルセットが含まれている場合 `403 FORBIDDEN` を返します。

  Dashboard のベアラートークン（ログイン）呼び出しは影響を受けず、Dashboard ユーザーおよび API キーを含む完全なデータベースのバックアップと復元が可能です。

  これは権限昇格のギャップを解消します。既存の `/users` および `/api_key` エンドポイントは API キーによる Dashboard ログイン資格情報および API キー記録へのアクセスを拒否していますが、API キー保持者はデータバックアップエンドポイント経由でこれらの制限を回避できていました。

- [#17187](https://github.com/emqx/emqx/pull/17187) 未認証の `GET /status?format=json` レスポンスから EMQX リリースバージョン (`rel_vsn`) を削除し、ブローカーのバージョン情報が未認証者に漏れないようにしました。バージョン情報は認証済みのノード情報 API で引き続き取得可能です。

- [#17201](https://github.com/emqx/emqx/pull/17201) プラグインインストールエンドポイントのアップロードされた tarball に対するパストラバーサル攻撃を防ぎ、インストール許可リストを厳格化しました。

  - インストールパスは、プラグインインストールディレクトリ外に展開される tarball の抽出を拒否します。
  - `emqx ctl plugins allow <name-vsn>` エントリは発行から 5 分後に期限切れとなり、`emqx ctl plugins allow <name-vsn> sha256:<HEX>` でパッケージの SHA-256 ハッシュに固定できます。固定ハッシュと一致しないアップロードは `403 Forbidden` で拒否されます。`sha256:` 引数を省略した場合は従来通り `<name-vsn>.tar.gz` という名前のペイロードを受け入れます。
  - HTTP プラグインインストールエンドポイント（およびそれをラップする Dashboard のアップロード）での成功したインストールは、クラスタ全体で即座に許可エントリを取り消し、同じ許可を異なる tarball で再利用できなくします。

- [#17309](https://github.com/emqx/emqx/pull/17309) PROXY-Protocol v2 の SSL Common Name および Subject フィールドに含まれる制御文字を除去し、クライアント識別情報への不正な文字混入を防止しました。

  `proxy_protocol = true` に設定されたリスナーでは、PROXY-Protocol SSL TLV バイト列に ASCII 制御文字が含まれる接続を拒否します（MQTT で受信する `clientid`、`username`、`password` と同様のバイトクラス）。これにより、攻撃者制御のバイトが `${cert_common_name}` および `${cert_subject}` テンプレートを介して HTTP 認証・認可やルールエンジンのヘッダー値に混入するのを防ぎます。

  HTTP 認証・認可クライアントも、レンダリングされたヘッダー名または値に CR、LF、NUL バイトが含まれる場合はリクエストを送信しなくなりました。

- [#17315](https://github.com/emqx/emqx/pull/17315) MQTT の `clientid`、`username`、`password` に適用されているバイトクラスチェックを、`ClientInfo` および HTTP リクエストテンプレートに使用される他のフィールドにも拡張しました。

  - `peersni`（TLS Server Name Indication、PROXY-Protocol v2 の `authority` TLV からも受け入れ）は接続受け入れ境界で検証され、制御文字があれば接続拒否と警告ログ出力を行います。
  - `mqtt.client_attrs_init` の Variform 式で生成されるクライアント属性値は、制御文字を含む場合は警告付きで破棄され、`${client_attrs.tns}` のようなテンプレートに制御文字が注入されるのを防ぎます。
  - HTTP アクションおよびブリッジコネクターのヘッダー生成は、レンダリングされた名前または値に NUL、CR、LF が含まれるヘッダーを破棄します。

- [#17330](https://github.com/emqx/emqx/pull/17330) `proxy_protocol` 有効な TCP および SSL リスナーの PROXY Protocol v2 TLV パーサーを強化しました。以前は、宣言長がバッファを超える TLV があるとパーサーが静かに TLV ストリームを切り詰め、後続フィールドを破棄していました。現在は厳格に処理し、不正な TLV ストリームは接続拒否と警告ログ出力を行います。

- [#17440](https://github.com/emqx/emqx/pull/17440) `GET /api/v5/data/files/<filename>`（バックアップファイルダウンロード）をグローバル Dashboard 管理者に制限しました。バックアップアーカイブには Dashboard アカウント（パスワードハッシュや MFA/TOTP 状態含む）や API キー記録が含まれるため、API キー呼び出し、Dashboard ビューアー、名前空間管理者はダウンロードできなくなりました。バックアップディレクトリの一覧取得（`GET /api/v5/data/files`）は従来通りアクセス可能です。

- [#17491](https://github.com/emqx/emqx/pull/17491) ゲートウェイ認証 API、エラーパス、デバッグログでパスワードやシークレットが露出する問題を修正しました。ゲートウェイ認証 API レスポンスはシークレットをマスクしつつ元の設定構造を保持します。以下のログパスは生パスワードやシークレットを出力しなくなりました：ゲートウェイ認証失敗、リスナー起動エラー、ExProto 認証ログ、CoAP トークン必須ログ、LwM2M 無効登録ログ。

- [#17501](https://github.com/emqx/emqx/pull/17501) 名前空間管理者の Dashboard ユーザーによる名前空間境界を越えた MQTT メッセージ内容の読み取りを禁止しました。

  - 以下のエンドポイントは非グローバル呼び出し元に対し `403 FORBIDDEN` を返します。これらは呼び出し元の名前空間外の MQTT ペイロードを露出する可能性があるためです。以前は名前空間ユーザーが他の名前空間のメッセージを読み取り・削除できました。

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - トレース API は名前空間スコープ化されました：`GET /trace` は呼び出し元の名前空間で作成されたトレースのみを一覧表示します。個別トレースエンドポイント（`/trace/:name`、`/trace/:name/download`、`/trace/:name/log`、`/trace/:name/log_detail`、`/trace/:name/stop`）は他の名前空間のトレースに対して `404` を返し、存在を隠します。一括 `DELETE /trace` はグローバル管理者専用で、名前空間呼び出し元は `403` となります。名前空間管理者は自身のトレースの作成、一覧、ダウンロード、ストリーム、停止、削除を引き続き行えます。

#### クラスタリング

- [#17076](https://github.com/emqx/emqx/pull/17076) 新しいルーティングテーブル同期機構を導入しました。ルーティングテーブルのスキーマバージョンは `v3` に上がり、`v2` との後方互換性も保持しています。

  スキーマ v3 では、各ノード（コアまたはレプリカント）が自身に向けられたルーティングテーブルエントリを完全に所有し、ピアノードはこれらのエントリに読み取り専用アクセスのみを持ちます。これにより、パーティション耐性が向上し、パーティション化されたクラスタでピアノードが他ノードの代理でルーティングテーブルを変更できなくなります。また、レプリカントノードの `SUBACK` レイテンシも改善されます。

  **後方互換性:** v3 対応ノードが v2 のみ対応クラスタに参加すると、互換性のため v2 を使い続けます。クラスタを v3 に切り替えるにはアップグレード後にクラスタ全体を再起動してください。自動切り替えを防ぐには `broker.routing.storage_schema` を `v2` に設定します。

  **ダウングレード注意:** クラスタが v3 に切り替わるとローリングダウングレードは不可能です。

  ノードの現在のルーティングスキーマバージョンを確認するには：

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152), [#17181](https://github.com/emqx/emqx/pull/17181) Erlang inet ポートオプション（接続・リッスン両方）を分散ポートに設定可能にし、デフォルトのバッファサイズを 1 MB にしました。

  以前は Erlang 分散ポートのデフォルトバッファが非常に小さく（1460 バイト、プラットフォームによっては約 9 KB）、分散ポートバッファ（`+zdbbl`）を 32 MB など大きく設定しても性能ボトルネックとなり、クラスタ通信の信頼性低下や `erpc timeout` エラー、Mnesia トランザクション渋滞、多コアノードの性能劣化を引き起こしていました。

- [#17221](https://github.com/emqx/emqx/pull/17221) MQTT メッセージ転送のクラスタリンク診断を改善しました。

  メッセージ転送接続に問題がある場合、リンクリソースの状態およびアラームに切断理由が含まれるようになり、設定問題の特定が容易になります。

- [#17530](https://github.com/emqx/emqx/pull/17530) クラスタリンクは非コミュニティライセンスが必要になりました。デフォルトのコミュニティライセンスでは設定されたリンクは非アクティブのままで（メッセージ転送やルート複製なし）、REST API はリンク有効化の試みを拒否し、非コミュニティライセンスのロードを促す明確なヒントを返します。リンクの無効化および削除は引き続き可能で、レガシー設定の整理に利用できます。ライセンスアップグレード後は Dashboard または REST API からノード再起動なしでリンクを有効化できます。

#### 可観測性

- [#16656](https://github.com/emqx/emqx/pull/16656) `busy_port` や `long_schedule` などのシステムモニター報告にプロセスラベルを含め、トラブルシューティングを容易にしました。

- [#16744](https://github.com/emqx/emqx/pull/16744) HTTP API 経由でパブリッシュされたメッセージのエンドツーエンドトレーシングをサポートしました。

- [#16757](https://github.com/emqx/emqx/pull/16757) `os_mon` のデフォルト設定をシステム全体のメモリ統計収集のみに変更し、プロセス毎のメモリスキャンオーバーヘッドを削減しました。

- [#16911](https://github.com/emqx/emqx/pull/16911) Prometheus メトリクス収集のオーバーヘッドを削減し、Mria 統計の誤った重複クエリを回避しました。

- [#17018](https://github.com/emqx/emqx/pull/17018) Prometheus スクレイピング API 呼び出し時の他ノードへの呼び出し回数を削減し、API 呼び出しの応答速度を向上させ、クラスタ負荷時のタイムアウト発生を減らしました。

  特に、レプリカントノードが関心を持つ `emqx_mria_lag` メトリクスは、API 呼び出し毎に更新するのではなく、デフォルトで 10 秒毎に定期更新されます。

- [#17031](https://github.com/emqx/emqx/pull/17031) ライセンス使用監査用にセッションのハイウォーターマーク履歴を追加しました。

  EMQX は日次ピークセッション数を記録し、少なくとも 24 か月分の履歴を保持します。運用者は `emqx ctl license history` で `--period daily|monthly` および `--json` オプション付きでクエリ可能です。新しい `license.high_watermark_timezone` 設定で日付境界を制御できます。

- [#17162](https://github.com/emqx/emqx/pull/17162) ノード毎のライセンス情報を Prometheus ゲージ（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）で公開し、クラスタ全体のライセンス整合性をノード毎の CLI チェックなしで監視可能にしました。

  これらのゲージを比較することで、クラスタノード間のライセンス不整合をアラートできます。実装は単一の `emqx_license_checker:dump/0` gen_server 呼び出しで 3 つの値を取得し、Prometheus スクレイプ毎の冗長な往復を排除しています。

- [#17176](https://github.com/emqx/emqx/pull/17176) ノード毎のルートテーブルエントリ数をエクスポートする Prometheus メトリクス `emqx_routes_count` と `emqx_routes_max` を追加しました。

- [#17329](https://github.com/emqx/emqx/pull/17329) `/api/v5/prometheus/stats` エンドポイントにノード全体のゲージメトリクスを 2 つ追加しました：

  - `emqx_vm_uptime_ms`: EMQX ノードのアップタイム（ミリ秒）
  - `emqx_vm_max_fds`: ノードが利用可能な最大ファイルディスクリプタ数

- [#17558](https://github.com/emqx/emqx/pull/17558) `GET /monitor_current` HTTP API に新たに 2 つのメトリクスと対応するレートを追加しました：`rules_matched` と `actions_executed`。それぞれルールマッチ数とアクション実行率（成功＋失敗）を追跡します。

  また、非バッチモード（`batch_size = 1`）でのアクション呼び出しの過小カウントを修正し、アクションコールバック呼び出し毎にカウンターをインクリメントするようにしました。

#### アクセス制御

- [#16741](https://github.com/emqx/emqx/pull/16741) SAML SSO バックエンドの署名検証挙動を制御する設定オプション `idp_signs_envelopes` と `idp_signs_assertions` を追加しました。

  以前は IdP 証明書フィンガープリントがメタデータから抽出されず `esaml` への検証に渡されなかったため、SAML 署名検証が正しく機能していませんでした。

  どちらも既存設定との後方互換性のためデフォルトは `false` です。署名検証を有効にしたい場合は、IdP が SAML レスポンスに署名する設定の場合に明示的に `true` にしてください。

- [#16942](https://github.com/emqx/emqx/pull/16942), [#17235](https://github.com/emqx/emqx/pull/17235) API キーおよび Dashboard ログインユーザーの細粒度スコープベースアクセス制御を導入しました。

  API キーはオプションの `scopes` フィールドをサポートし、設定時はロールチェックに加え管理スコープカタログに基づく認可を行います。`publisher` ロールは `publish` スコープのみに制限されます。

  Dashboard ログインユーザーもロールベースチェックに加えて `scopes` をサポートします。4 つのログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）は Dashboard 専用エンドポイントをカバーします。`user_management`、`sso_management`、`api_key_management` は管理者専用、`mfa_management` は強制 MFA 免除のため任意のロールが保持可能です。API キーはこれらログイン専用スコープを使用できません。

  新しいカタログエンドポイント `GET /api_key_scopes` と `GET /user_scopes` はベアラー認証呼び出し元にスコープ語彙を公開します。`GET /users`、`POST /users`、`PUT /users/:username` はレスポンスに `scopes` を含み、明示的に設定されていない場合はロールデフォルトスコープを表示します。

  新スコープモデルに伴う挙動変更：

  - `dashboard.default_username` ユーザーは保護されたブレークグラスアカウントとなり、削除、管理者降格、スコープ割当は不可で、`description` のみ変更可能です。既存の最後の管理者チェックは他の管理者に適用され続けます。
  - 自己サービス更新は専用のパスワード変更および MFA 自己エンドポイントを除きスコープを尊重します。例えば、`user_management` を持たないビューアは自身のパスワード変更と MFA 管理は可能ですが、他のプロフィール編集はできません。
  - `PUT /users/:username` と `PUT /api_key/:name` はリクエストに `scopes` がない場合、永続化されたスコープに基づきロール変更を検証し、不整合な降格や変更は拒否します。
  - API キーブートストラップファイルはオプションのスコープ列（`key:secret:role:scopes`）を受け入れます。不明またはロール非互換のスコープは警告付きで破棄され、既存の 3 列ファイルは読み込み可能です。
  - SAML SP メタデータエンドポイント（`GET /sso/saml/metadata`）は認証不要でアクセス可能になり、`/sso/saml/acs` と整合します。

- [#16943](https://github.com/emqx/emqx/pull/16943), [#17361](https://github.com/emqx/emqx/pull/17361) SSO（LDAP、OIDC、SAML）用のバックエンド単位 `force_mfa` 強制を追加しました。

  有効時、SSO ユーザーは IDP 側 MFA 設定に関わらず Dashboard トークン取得前に TOTP MFA 設定または検証を完了する必要があります。新 API エンドポイント `POST /sso/mfa/setup` と `POST /sso/mfa/verify` が MFA フローを処理します。

  管理者は `/users/:username/mfa` の `POST` / `DELETE` で既存ユーザーを個別に免除または強制でき、これがライブバックエンドポリシーを上書きします。`force_mfa = true` バックエンドの SSO ユーザーが自身で MFA を無効化した場合、次回ログイン時に再設定が必要です。管理者による無効化のみがライブポリシーからの免除となります。

- [#17178](https://github.com/emqx/emqx/pull/17178) `emqx ctl api_keys add` CLI コマンドが REST API で既にサポートされているスコープベースの権限制御に対応し、`--scopes <scope1,scope2,...>` オプションを受け入れるようになりました。

#### ゲートウェイ

- [#16736](https://github.com/emqx/emqx/pull/16736) JT/T 808 ゲートウェイをプロトコル更新、エンコーディング対応、メッセージ処理修正で改善しました。

  - JT/T 808 プロトコル 2019 を追加。
  - 不明メッセージを透過的に転送する `jt808.frame.parse_unknown_message` オプションを追加。
  - 新しい `frame.string_encoding` オプションで GBK 文字エンコーディングをサポート。デフォルトの `utf8` モードは既存のパススルー動作を維持し、`gbk` はデバイスからの GBK エンコード文字列を MQTT 用 UTF-8 に変換し、MQTT からデバイスへは UTF-8 を GBK に変換します。ナンバープレート、運転手名、テキストメッセージ、エリア名、クライアントパラメータなどに適用。MQTT ペイロードは常に UTF-8 です。
  - ダウンリンクメッセージでカスタム `msg_sn` 値をサポート。MQTT ペイロードのヘッダーに `msg_sn` がある場合、ゲートウェイは自動生成のチャネルシーケンス番号の代わりに使用します。
  - CAN バス ID パラメータ（0x0110～0x01FF）向けのパラメータ設定（0x8103）およびクエリ応答（0x0104）メッセージ処理を修正。JSON では文字列型ではなく base64 エンコードの BYTE[8] 型を使用すべきです。
  - JT/T 808 0x0702 運転手識別報告メッセージの解析を修正。

- [#17013](https://github.com/emqx/emqx/pull/17013) GBT32960 ゲートウェイに GBT32960-2025 プロトコルサポートを追加しました。

  ゲートウェイはフレームヘッダー（2016 は `##`、2025 は `$$`）でプロトコルバージョンを自動検出し、バージョン固有の解析・シリアライズを処理します：

  - 新しい 2025 情報タイプ：車両、駆動モーター、燃料電池、エンジン、位置情報、アラーム、電池電圧・温度、燃料電池スタック、スーパーキャパシタ、スーパーキャパシタ極限、デジタル署名。
  - 新コマンド：アクティベーション（0x09/0x0A）。
  - パラメータクエリ・設定（0x02/0x03）のパラメータサイズがバージョン依存（2025 は BYTE、2016 は WORD）。
  - BMS バッテリーパックエンコードフィールドを用いた 2025 車両ログイン。

#### データ統合

- [#16511](https://github.com/emqx/emqx/pull/16511) データ統合で IoTDB テーブルモデルをサポートしました。

- [#16962](https://github.com/emqx/emqx/pull/16962) Kafka ソースのポーリング挙動を改善し、レコードがない場合に空バッチを即返すのではなく、わずかに待機してデータを待つようにしました。これにより不要なポーリング遅延が減り、Kafka コンシューマーが新規レコードをより安定的に受信できます。

- [#17025](https://github.com/emqx/emqx/pull/17025) InfluxDB データベースのヘルスチェックと認証検証方法を変更しました。

  もはや `SHOW DATABASES` を実行してチェックしません。これは一部の監査システムで誤ってシステム侵入と判定される恐れがありました。

  詳細は [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54) を参照してください。

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT イングレスブリッジが、リモートブローカーが MQTT 5 サブスクリプション識別子をサポートする場合に、`$queue/{name}/{bind-filter}` として公開されたリモートメッセージキューからの消費をサポートしました。サブスクリプション識別子が利用できない場合はキューサブスクリプションを拒否し、通常のトピックサブスクリプションはリモートブローカーが受け入れない場合に識別子なしで自動リトライします。

- [#17104](https://github.com/emqx/emqx/pull/17104) 集約アップロードアクション（Azure Blob Storage、Amazon S3、GCS、Snowflake、S3 Tables）で、blob 名テンプレートに日付パーツプレースホルダーを追加しました。プレースホルダーは集約開始時刻に対してレンダリングされ、デフォルトは UTC です。これにより Spark、Databricks、Synapse で直接利用可能な Hive パーティション形式のオブジェクトレイアウト（例：`year=2025/month=04/day=22/hour=07/...`）が可能になります。

  対応プレースホルダー：

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}`（年内通算日）

  各プレースホルダーは明示的なタイムゾーン接頭辞を付けられます：

  - `utc`（デフォルト）：例 `${datetime.utc.YYYY}`
  - `local`（EMQX ノードのシステムタイムゾーン）：例 `${datetime.local.YYYY}`

- [#17136](https://github.com/emqx/emqx/pull/17136) InfluxDB コネクターに `ping_with_auth` オプションを追加しました。有効時は認証が必要な InfluxDB 互換サービスのヘルスチェックに設定済み認証情報を含めます。また、InfluxDB コネクター／アクションで `write_syntax` リテラルや MQTT ペイロードから書き込む際に Unicode テキストを保持するよう修正しました。

- [#17165](https://github.com/emqx/emqx/pull/17165) アクションに `resource_opts.dispatch_strategy` オプションを追加しました。

  新オプションのデフォルトは `per_clientid` で、従来のバッファワーカーディスパッチ動作を維持します。`random` に設定すると、明示的な `pick_key` がないクエリはランダムなディスパッチキーを使い、少数クライアントが大量メッセージをパブリッシュする場合に複数バッファワーカーへのトラフィック分散を助けます。

- [#17170](https://github.com/emqx/emqx/pull/17170)、[#17282](https://github.com/emqx/emqx/pull/17282)、[#17297](https://github.com/emqx/emqx/pull/17297) MQTT ブリッジコネクターおよびクラスタリンク設定に TCP ソケットチューニング用の `tcp_opts`（`nodelay`、`sndbuf`、`recbuf`、`buffer`、`keepalive`、`delay_send`、`active_n`）を追加しました。未設定項目は OS / `gen_tcp` のデフォルトを維持します。`delay_send`（デフォルトオフ）は小さな書き込みをまとめてスループットを向上させる代わりにレイテンシをわずかに増加させます。

- [#17474](https://github.com/emqx/emqx/pull/17474) IoTDB REST API コネクターのヘルスチェックオーバーヘッドを、すべてのデータベース一覧取得からバージョンクエリに変更して削減しました。

- [#17481](https://github.com/emqx/emqx/pull/17481) MQTT ブリッジのイングレス（ソース）サブスクリプションに `retain_as_published` オプションを追加しました。リモートブローカーが MQTT 5.0 で接続し、`retain_as_published = true` の場合、転送メッセージの元の `retain` フラグを保持し、上流の保持メッセージを忠実に再パブリッシュできます。デフォルトで有効で、`proto_ver` が `v3` または `v4` の場合は無効です。

  また、`bridge_mode = true` と `proto_ver = v5` の併用時に警告ログを出すようにしました。MQTT 5.0 では従来のブリッジモードフラグは無効で、個別サブスクリプションで `retain_as_published` を設定してください。

- [#17508](https://github.com/emqx/emqx/pull/17508) PostgreSQL および TimescaleDB コネクター接続で `application_name` スタートアップパラメータを `emqx` に設定しました。

  これにより PostgreSQL ログや `pg_stat_activity` などのビューで EMQX データベースセッションの識別が容易になります。

- [#17594](https://github.com/emqx/emqx/pull/17594) Google Cloud Pub/Sub および BigQuery コネクターの `service_account_json` 設定に `file://` 形式のシークレットファイルを指定可能にし、外部ファイルからサービスアカウント認証情報を注入できるようにしました。

#### プラグイン

- [#16735](https://github.com/emqx/emqx/pull/16735) EMQX は `/api/v5/plugin_api/{plugin}/...` 配下でプラグイン定義の HTTP API コールバックをサポートしました。

  これによりプラグイン開発者は Dashboard API サービス経由でプラグイン固有の API エンドポイントを一貫した認証・HTTP エラー処理付きで公開できます。

- [#16849](https://github.com/emqx/emqx/pull/16849) プラグイン API エンドポイントのクッキー認証フォールバックを追加しました。

  Dashboard が提供するプラグイン UI iframe は、`Authorization` ヘッダーがない場合に `emqx_auth` クッキーで認証可能です。これは `/api/v5/plugin_api/...` パスにのみ適用されます。

- [#17549](https://github.com/emqx/emqx/pull/17549) EMQX Backup Sync プラグインを追加しました。データバックアップ API を使い、プライマリクラスタからセカンダリクラスタへ選択した設定を定期的に同期します。HTTPS 通信の TLS オプションは設定可能です。

#### REST API

- [#16718](https://github.com/emqx/emqx/pull/16718) REST API Swagger 仕様を改善しました。

  以前は要約と説明が混在していましたが、要約は簡潔で句読点なし、説明は詳細を提供するように分離しました。

- [#16958](https://github.com/emqx/emqx/pull/16958) EMQX HTTP API ドキュメントの閲覧を容易にするため、専用の `/api-spec` エンドポイントと Dashboard API 仕様エクスプローラーを追加しました。

  Dashboard はタグスコープおよびドリルダウン可能な OpenAPI スライスを提供し、`dashboard.swagger_support` を `false` にするとこれらのエンドポイントは Swagger と共に無効化されます。CLI にも `emqx ctl api_keys` コマンドを追加し、API キーの一覧表示、詳細表示、追加、削除、有効化、無効化をコマンドラインから行えます。

#### デプロイメント

- [#17079](https://github.com/emqx/emqx/pull/17079) Helm チャートに `service.wsEnabled` オプションを追加し、MQTT WebSocket リスナーが無効な場合に ws/wss サービスポートエントリの出力を抑制可能にしました。既存動作維持のためデフォルトは `true` です。

### バグ修正

#### コア MQTT 機能

- [#16651](https://github.com/emqx/emqx/pull/16651) 高負荷時に既に閉じられたソケットを操作して接続プロセスクラッシュが稀に発生する問題を修正しました。修正前は `{badmatch,{ok,{sock_error,closed}...` のエラーログが典型的でした。

- [#16675](https://github.com/emqx/emqx/pull/16675) セッションテイクオーバーや破棄時に `disconnected_at` が `connected_at` より後になるタイムスタンプ順序の問題を修正しました。

  以前は `disconnected_at` が遅れて記録され、新セッションの `connected_at` 設定後だったため、`disconnected_at > connected_at` の競合状態が発生し、外部のクライアントプレゼンス状態追跡が困難でした。

  修正後はテイクオーバー開始時または破棄受信時に即座に `disconnected_at` を記録し、常に新セッションの `connected_at` より前となるようにしました。

- [#16684](https://github.com/emqx/emqx/pull/16684) `mqtt.client_attrs_init` 式でパスワードを使用可能にしました。例えば `jwt_value` にパスワードを渡せます。

  以前はパスワードがレンダリングコンテキストに追加される前に `client_attrs_init` が実行されていたため、パスワード依存の式が解決できませんでした。

- [#16715](https://github.com/emqx/emqx/pull/16715) 保持された `$SYS` メッセージ（例：ブローカー／ノード識別トピック）が有効期限なしで保存され、StatefulSet ローテーション後に Dashboard に古いノード識別子が残る問題を修正しました。

  新規パブリッシュされた保持 `$SYS` メッセージには `Message-Expiry-Interval = 3600`（1 時間）が含まれます。

  既存の古い保持 `$SYS` エントリは、空の保持メッセージを該当トピックにパブリッシュして手動でクリア可能です：

  ```
  emqx eval 'emqx:publish(emqx_message:set_flag(retain, true, emqx_message:make(emqx_sys, <<"$SYS/brokers/emqx@127.0.0.1/sysdescr">>, <<>>))).'
  ```

  コマンド内のトピックは削除したい古い `$SYS/...` トピックに置き換えてください。

- [#16731](https://github.com/emqx/emqx/pull/16731) 共有サブスクリプションが存在する場合に `emqx ctl subscriptions list` がクラッシュする問題を修正しました。

  修正前は一部クライアントでサブスクリプション一覧取得が失敗し、出力が返らないことがありました。

  修正後は通常サブスクリプションと共有サブスクリプションの両方で安定して動作します。

- [#16779](https://github.com/emqx/emqx/pull/16779) 不正な最初のパケットを無効な CONNECT パケットとして分類し、ログにより良いプロトコルヒントを追加しました。

- [#16781](https://github.com/emqx/emqx/pull/16781) 保持メッセージが利用できない場合の CONNECT 検証を修正しました。

  `mqtt.retain_available` が `false` の場合、Will Retain がセットされた CONNECT パケットは正しく CONNACK 理由コード `Retain not supported (0x9A)` で拒否されます。

- [#16782](https://github.com/emqx/emqx/pull/16782) MQTT v5 の無効な PUBLISH プロパティ処理を修正しました。

  クライアントが `Subscription-Identifier` を含む PUBLISH パケットを送信した場合、EMQX はプロトコルエラーとして扱い切断します。

- [#16783](https://github.com/emqx/emqx/pull/16783) MQTT v5 SUBSCRIBE の `Subscription-Identifier` 上限検証を修正しました。

  MQTT 仕様で定義された最大有効値 `268435455`（0x0FFFFFFF）を受け入れます。

- [#16956](https://github.com/emqx/emqx/pull/16956) 受信パケットが `mqtt.max_packet_size` を超えた場合（理由 `emsgsize`）、クライアント接続終了ログを情報レベルから警告レベルに変更しました。

- [#17139](https://github.com/emqx/emqx/pull/17139) 保持サブシステムの `retainer.enable` を実際のランタイムスイッチとして復活させました。

  これにより、保持メッセージのストレージを無効化しつつ MQTT 保持メッセージプロトコルサポートは有効にでき、`mqtt.retain_available` に頼る必要がなくなります。

- [#17172](https://github.com/emqx/emqx/pull/17172) クライアントが切断直前に送信した MQTT パケット（例：PUBACK）が、接続プロセスのメールボックスに未処理で残っている場合に失われる問題を修正しました。接続プロセスはシャットダウン前にメールボックスを正しく処理し、ソケットクローズ後もパケットを処理します。

- [#17353](https://github.com/emqx/emqx/pull/17353) `socket` TCP バックエンドで、クライアント接続が繰り返し送信輻輳した場合に MQTT パケットが誤った順序で送信される可能性がある問題を修正しました。このシナリオは実際には非常に稀です。

- [#17383](https://github.com/emqx/emqx/pull/17383) セッションテイクオーバー後、Dashboard および REST API のチャネル情報（`mqueue_len`、`inflight_cnt`）がテイクオーバー再生完了直後に即時更新されるようにし、15 秒ごとの統計更新まで待たないようにしました。

- [#17515](https://github.com/emqx/emqx/pull/17515) QoS 0 のメッセージキューサブスクリプションで、キューサブスクライバーのローカルインフライトウィンドウが満杯になるとメッセージ受信が停止する問題を修正しました。

- [#17569](https://github.com/emqx/emqx/pull/17569) MQTT v5 のユーザープロパティ解析コストを二次関数的から線形に削減しました。

  以前は多くのユーザープロパティを含む CONNECT、PUBLISH、SUBSCRIBE パケットが、各プロパティをリスト末尾に追加するため接続プロセスのスケジューラ時間が超線形に増加していました。解析はエントリ数に線形スケールしつつ、ワイヤー順序は保持されます。

#### ルールエンジン

- [#16699](https://github.com/emqx/emqx/pull/16699) 競合条件下で以下のような長く難解なログが出力される問題を修正しました：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  現在は問題のデバッグに役立つより意味のある情報を出力します。

- [#16847](https://github.com/emqx/emqx/pull/16847) メッセージ変換式で非 ASCII の Unicode 文字列を使用した場合のクラッシュを修正しました。

- [#17211](https://github.com/emqx/emqx/pull/17211) ドキュメントに記載されていたが実際のデータに欠落していた `$events/client/connack` ルールイベントの `connected_at` フィールドを追加しました。

#### データ統合

- [#16622](https://github.com/emqx/emqx/pull/16622) アクションが非同期クエリモードを使い、複数回のヘルスチェック後にコネクターが切断された場合にフォールバックアクションが二重にトリガーされる問題を修正しました。

- [#16659](https://github.com/emqx/emqx/pull/16659) 5.10.0 以前の静的 clientid を持つ古い MQTT コネクター設定を新しい EMQX バージョンで使う際、設定ルートの username および password が無視される問題を修正しました。

  現在はルートの資格情報が clientid ごとのものとマージされ、後者が優先されます。

- [#16685](https://github.com/emqx/emqx/pull/16685) 5.10.x から 6.0.y へのアップグレード後に Sparkplug B メトリクスエイリアスマッピングが失敗する問題を修正しました。

  6.0.0 未満で生成された Protobuf コードのキャッシュが新しいコードの期待と合わず、Sparkplug B のエイリアスマッピングが動作しませんでした。

- [#16723](https://github.com/emqx/emqx/pull/16723) RabbitMQ コネクター／アクション／ソースで、接続やチャネルプロセスが異常終了した場合に再接続せず切断状態のままになる問題を修正しました。

- [#16742](https://github.com/emqx/emqx/pull/16742) GreptimeDB の TLS 接続失敗問題を修正しました。

- [#16796](https://github.com/emqx/emqx/pull/16796) コネクターアクションで複数行 SQL 文の処理を修正しました。

- [#16863](https://github.com/emqx/emqx/pull/16863) 既に期限切れのリクエストに対する非同期応答を受け取った際に警告ログを出すようにしました。

- [#16890](https://github.com/emqx/emqx/pull/16890) ExHook で再接続リロード成功時に同じサーバー名が重複登録され、コールバックが繰り返し発行される問題を修正しました。

- [#16936](https://github.com/emqx/emqx/pull/16936) Azure Blob Storage アクションの集約モードで、コンテナ内の blob 数が多いとヘルスチェックがタイムアウトする問題を修正しました。

- [#16955](https://github.com/emqx/emqx/pull/16955) Kafka プロデューサーアクションの誤ったヘルスチェック警告ログを排除しました。

  Kafka プロデューサーが長時間アイドルすると Kafka が接続を切断し、同時期にヘルスチェックが走ると「not_all_kafka_partitions_connected」警告が誤って出ていました。

- [#16972](https://github.com/emqx/emqx/pull/16972) HTTP および GCP PubSub アクションで、理由 `closing` の一時的接続エラーを回復可能エラーとして扱い、ログノイズを減らしました。

- [#17084](https://github.com/emqx/emqx/pull/17084) MQTT ソースで、`clean_start = false` のコネクターがセッションメッセージを持つブローカーに再接続した際にルールアクションがトリガーされない問題を修正しました。

- [#17111](https://github.com/emqx/emqx/pull/17111) PostgreSQL コネクターのプリペアドステートメント無効モードでのクエリ実行を修正しました。以前は同時クエリが干渉してエラーとなっていました。

- [#17113](https://github.com/emqx/emqx/pull/17113) RocketMQ コネクターの分離を修正しました。誤設定や到達不能な RocketMQ コネクターが同一ノードの他の RocketMQ コネクターを不安定化させなくなりました。

  以前は到達不能なブローカーのコネクターが共有クライアントスーパーバイザーを最大 60 秒停止させ、兄弟コネクターが `resource_health_check_timed_out` でフラップし、Dashboard 操作がハングしていました。

  TCP/TLS 接続タイムアウトのデフォルトも 60 秒から 10 秒に短縮され、誤設定サーバーが早期に失敗として検出されます。

- [#17180](https://github.com/emqx/emqx/pull/17180) 高負荷時に MongoDB プロセスへのタイムアウト呼び出しが回復不能エラーと誤認され再試行されない問題を修正しました。現在は再試行されます。

- [#17216](https://github.com/emqx/emqx/pull/17216) Timescale/PostgreSQL アクションで、引用符付き JSON 数値文字列が `FLOAT` カラムにマッピングされた際にデータベース接続プロセスがクラッシュする代わりに構造化されたパラメータエラーを報告するように修正しました。

- [#17250](https://github.com/emqx/emqx/pull/17250) Redis Sentinel コネクターで Redis データノードと Sentinel ノードの認証設定を分離してサポートしました。

- [#17293](https://github.com/emqx/emqx/pull/17293) Parquet ファイルに必須キーを持つオブジェクトを `undefined`/`null` 値で書き込む際に破損ファイルが生成される問題を修正し、エラーを発生させるようにしました。

- [#17303](https://github.com/emqx/emqx/pull/17303) Kafka クライアントライブラリをアップグレードしました：`brod` を 4.5.2 から 4.5.4、`wolff` を 4.1.9 から 4.1.10 に。

  主な修正点：

  - `brod`: Kafka 接続再認証時の競合状態修正（`kafka_protocol` 4.3.4）
  - `wolff`: 高メモリ負荷制御下で最小バッファリザーブを維持し、プロデューサーのインフライトデータ枯渇を防止。リザーブ超過分のみドロップ。

- [#17347](https://github.com/emqx/emqx/pull/17347) RocketMQ クライアント依存を `v0.7.2` にアップグレードし、非同期プロデューサーリクエストのメモリ増加問題を修正しました。

- [#17439](https://github.com/emqx/emqx/pull/17439) Azure Blob Storage コネクターのヘルスチェックがストレージアカウント内のコンテナ数が多い場合にタイムアウトや大きな帯域コストを発生させる問題を修正しました。#16935 の修正の補完です。

- [#17450](https://github.com/emqx/emqx/pull/17450) `/prometheus/data_integration` Prometheus エンドポイントが `mode=node` で 500 エラーを返す問題を修正しました。これは手動編集でアクションのコネクターが存在しない不整合設定がある場合に発生しました。

- [#17568](https://github.com/emqx/emqx/pull/17568) Kafka クライアントライブラリ `brod` を 4.5.5 にアップグレードしました。

  コンシューマグループ：古い Kafka ブローカー（例：2.2.0）が返す `member_id_required` エラーコード付きのジョイン応答でブローカー割当のメンバー ID を尊重するようにしました。以前はエラー時にメンバー ID が破棄され、リトライが成功しませんでした。

- [#17579](https://github.com/emqx/emqx/pull/17579) Redis Sentinel コネクターでリソース毎に分離された Sentinel マネージャーを使用し、リソース停止時にクリーンアップするように修正しました。これによりコネクター間で Sentinel 状態が共有される問題を回避します。

- [#17584](https://github.com/emqx/emqx/pull/17584) Snowflake 集約コネクターのヘルスチェックで返されるデータ量を制限しました。既存スキーマ数が非常に多い場合にヘルスチェック実行時間が大幅に短縮されます。

- [#17588](https://github.com/emqx/emqx/pull/17588) Kinesis 統合のコネクターおよびアクションのヘルスチェックで返されるデータ量を制限しました。既存スキーマ数が非常に多い場合にヘルスチェック実行時間が大幅に短縮されます。

- [#17595](https://github.com/emqx/emqx/pull/17595) S3 および S3 Tables 統合のコネクターのヘルスチェックで返されるデータ量を制限しました。既存バケット数が非常に多い場合にヘルスチェック実行時間が大幅に短縮されます。

#### クラスタリング

- [#16393](https://github.com/emqx/emqx/pull/16393) 不安定なネットワーク条件下でのクラスタリンクのルート複製安定性を改善しました。

- [#16739](https://github.com/emqx/emqx/pull/16739) 全ノード同時再起動後のクラスタ回復時間を改善しました。

  組み込み Mria データベース管理システムはトランザクション同期イベント生成に使う内部テーブルの完全同期を待たなくなりました。

- [#17132](https://github.com/emqx/emqx/pull/17132) レプリカントノードで生設定やランタイム状態が乖離している場合にトピックメトリクスの追加・削除が失敗し、`cluster_rpc_apply_failed` アラームが発生しクラスタ RPC 複製が停滞する問題を修正しました。重複追加や欠落削除はイニシエーター側のみ拒否し、レプリカントは冪等的に適用します。

- [#17182](https://github.com/emqx/emqx/pull/17182) mria 用 emqx-OTP を 27.3.4.2-8 に更新しました。

  この変更がないと、EMQX 起動時にクラスタ接続されていない場合に Mria アプリの起動がハングすることがあります。

- [#17214](https://github.com/emqx/emqx/pull/17214) クラスタリンクのメッセージ転送 MQTT クライアントの切断イベントに関する難解なエラーログを削除し、トラブルシューティングに十分な文脈を持つユーザーフレンドリーなメッセージに置き換えました。

  以下のようなエラーは今後エラーログに現れません：

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) `bin/emqx` および `bin/emqx_ctl` の実行が稼働中ブローカーで `nodeup`/`nodedown` イベントをトリガーし、誤解を招く `cm_registry_node_down` 警告をログに出す問題を修正しました。これらスクリプトが起動する一時ヘルパーノードは隠し Erlang ノードとして登録されます。

- [#17269](https://github.com/emqx/emqx/pull/17269) ネットワークパーティション後のクラスタ回復を改善しました。

  - 以前はレプリカントノードに接続されたクライアントの一部がグローバルレジストリから失われ、テイクオーバー時の不整合や Dashboard 表示誤りを引き起こしていました。

    この修正でネットワークパーティション回復時に既存クライアントを再登録するバックグラウンドプロセスを追加し、「Broker is recovering after a network partition」という新しいアラームを追加しました。

  - 重複ネットワークパーティションを自動回復する新しいクラスタ自動修復アルゴリズムを導入しました。

- [#17343](https://github.com/emqx/emqx/pull/17343) クラスタ設定複製バグを修正しました。データバックアップのインポートや `emqx ctl conf load` / `PUT /api/v5/configs` で `file` タイプの認可ソースを含む設定を読み込むと、ピアノードが `cluster_rpc_apply_failed` / `failed_to_read_acl_file` エラーで遅延することがありました。

  インポーターは ACL ファイルをローカルに書き込み、インラインの `rules` を `path` に置き換えてクラスタに送信していましたが、ピアノードは該当ファイルを持たず適用できませんでした。現在はインラインの `rules` を保持し、各ピアノードが複製された内容から自身の ACL ファイルを作成します。

- [#17348](https://github.com/emqx/emqx/pull/17348) クラスタノード間で実効設定は同じだが生設定表現が異なる場合に `emqx ctl conf cluster_sync status` の誤解を招くノイズを削減しました。

  生設定の差異で実効設定に対応しないものは抑制し、実効設定が不整合な場合のみ警告を出します。生設定キーが片方にしかない場合のクラッシュも回避します。

  また、アクション、ソース、ブリッジ、ルールメタデータの `created_at` と `last_modified_at` のタイムスタンプのみの差異は無視します。データインポートや起動時設定読み込みで生成されるタイムスタンプは一部ノードだけ更新されることがあります。

- [#17349](https://github.com/emqx/emqx/pull/17349) 応答しないターゲットクラスタへの接続でルート複製が停滞したクラスタリンクの応答性を改善し、そのようなクラスタリンクの削除がわずかに早く完了するようにしました。

- [#17382](https://github.com/emqx/emqx/pull/17382) ネットワークパーティション時に発生する可能性があるグローバルチャネルレジストリの破損を修正しました。

- [#17424](https://github.com/emqx/emqx/pull/17424) ネットワークパーティション後の Mnesia 自動修復で同一クライアント ID の重複または古いグローバルセッションレジストリエントリが残るリークを修正しました。

  破棄およびテイクオーバーキック RPC ハンドラーは対象プロセスが生存しない場合にレジストリ行を削除し、接続パスの登録スロットルはトゥームストーン行（ローカルチャネル状態なし）を認識して再利用可能にしました。

- [#17432](https://github.com/emqx/emqx/pull/17432) クラスタリンク API の同時リクエストが成功または未検出のいずれかを返さず、汎用エラー応答を返す問題を修正しました。

- [#17469](https://github.com/emqx/emqx/pull/17469) アクティブなクラスタリンクの有効化・無効化時に以下のような警告が出る問題を修正しました。

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

- [#17586](https://github.com/emqx/emqx/pull/17586) グローバルセッションレジストリの古いエントリを定期的に削除するようにしました。以前はセッション所有プロセスが正常に登録解除されず、同一クライアント ID が再接続しない場合にレジストリ行が永続的に残ることがありました。新しい制限付きバックグラウンドスイープが各コアノードで 1 秒あたり最大 500 行、10 分に 1 回以下の頻度で実行され、数百万セッション保持時でもスループットに影響しません。

#### アクセス制御

- [#16692](https://github.com/emqx/emqx/pull/16692) `emqx_crl_cache:evict/1` が内部 URL 状態を完全にクリアしない CRL キャッシュ回帰を修正しました。削除後は同じ CRL URL が次回使用時に正しく再登録され、リフレッシュタイマーが復元され、接続毎の HTTP フェッチ繰り返しを回避します。

- [#16780](https://github.com/emqx/emqx/pull/16780) 認可ソース検証で `type` フィールドが欠落したリクエストが内部エラーを引き起こす問題を修正しました。

  現在は明確な `BAD_REQUEST` 検証エラーを返します。

- [#16805](https://github.com/emqx/emqx/pull/16805) 動的 ACL 判定のため認可キャッシュ保存をオプトアウト可能にする認可フック結果をサポートしました。

- [#16865](https://github.com/emqx/emqx/pull/16865) `mqtt.client_attrs_init` 式で既存の `cn` と `dn` 変数に加え、`cert_common_name` と `cert_subject` のエイリアスを追加しました。

- [#16868](https://github.com/emqx/emqx/pull/16868) REST API 認証エラーメッセージを改善し、プログラムクライアントがベアラートークンのログインを繰り返すのではなく API キー（Basic 認証）を使うよう誘導する文言を追加しました。エラー応答に `api_key.bootstrap_file` 設定オプションと永続 API キー作成用の `POST /api_key` エンドポイントを明記しています。

- [#16939](https://github.com/emqx/emqx/pull/16939) 組み込みデータベース認証器が、デフォルトのブートストラップファイルパスが設定されていてもファイルが存在しない場合に警告ログを出さないように修正しました。

- [#17045](https://github.com/emqx/emqx/pull/17045) パスワードなしの CONNECT パケットでパスワードベース認証バックエンドが接続を即拒否せず、認証チェーンを継続するように修正しました。

  以前はパスワードなし接続で最初のパスワードベース認証器（組み込みデータベース、MySQL、PostgreSQL、MongoDB、Redis、LDAP）がエラーを返し、後続認証器が試されませんでした。

- [#17100](https://github.com/emqx/emqx/pull/17100) OIDC SSO ログインで、ID プロバイダーが `+json` 構造化構文サフィックスを持つ `Content-Type`（例：`application/jwk-set+json; charset=utf-8`）の JWKS レスポンスを返すと `provider_not_ready` で失敗する問題を修正しました。これらのレスポンスは有効な JWKS コンテンツとして受け入れられます。

- [#17122](https://github.com/emqx/emqx/pull/17122) URL エンコードされたユーザー名（例：メールアドレス）を持つ SSO ユーザーの Dashboard RBAC チェックを修正し、`force_mfa` 無効時にビューアの自己サービス MFA 無効化リクエストが正しく動作するようにしました。

- [#17140](https://github.com/emqx/emqx/pull/17140) HTTP 経由で取得した証明書失効リスト（CRL）が DER エンコードされた場合に無音で失敗する問題を修正しました。

  以前は PEM エンコードのみをデコードし、DER ボディは空リストとしてキャッシュされ、`enable_crl_check = true` リスナーの TLS ハンドシェイクが `bad_crls, no_relevant_crls` で失敗し、原因を示すログが出ませんでした。

  現在は PEM と DER の両方をデコードし、どちらでもない場合は URL を含む警告ログを出します。

- [#17171](https://github.com/emqx/emqx/pull/17171) 名前空間管理者が自身の MFA を有効・無効化できない RBAC 問題を修正しました。

  名前空間管理者は引き続き他の Dashboard ユーザーの MFA 管理は制限されます。

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard で作成される REST API キーは、API キー名由来ではなくランダムに生成されるようになりました。

- [#17223](https://github.com/emqx/emqx/pull/17223) TCP パススループロキシ（例：GCP TCP Proxy NLB、AWS NLB）を SSL リスナーの前に置いた場合にクライアント証明書が欠落する問題を修正しました。TLS ハンドシェイクは成功し証明書は存在していましたが、認証やルールイベントに渡されていませんでした。CN、Subject、PEM 全体に依存する関数、ACL ルール、認証バックエンドはこの配置でも正しく動作します。

- [#17428](https://github.com/emqx/emqx/pull/17428) Kanidm で観測された `.well-known/openid-configuration` レスポンスに `Cache-Control: max-age=0` などが含まれる場合に OIDC SSO がクラッシュし、OpenID プロバイダー検出が完了しない問題を修正しました。クラッシュにより OIDC スーパーバイザーが再起動予算を使い果たし、設定再保存なしに回復不能でした。キャッシュ制御パーサーを寛容にし、ワーカーが致命的クラッシュしなくなり、スーパーバイザーは 1 分以内に複数回の再起動を許可し、一時的失敗をクリーンにリトライします。

#### ゲートウェイ

- [#16603](https://github.com/emqx/emqx/pull/16603) DTLS 接続モードでの CoAP ゲートウェイを修正しました。
- [#16670](https://github.com/emqx/emqx/pull/16670) NATS ゲートウェイが最大パブリッシュペイロードを強制し、`echo` オプション（ローカル配信なし）を尊重し、パブリッシュ／サブスクライブのサブジェクト処理と関連エラーメッセージを改善しました。
- [#17141](https://github.com/emqx/emqx/pull/17141) CoAP 接続モードのトークンテイクオーバーを修正し、再接続する UDP/DTLS クライアントが有効なトークンで再開できるようにし、不正なトークン／クライアント ID 組み合わせは拒否します。CoAP テイクオーバー接続フック実行前に必要な接続情報フィールドが存在することを保証しました。

- [#17258](https://github.com/emqx/emqx/pull/17258) MQTT-SN ゲートウェイで、同一セッションに対し 2 回目の CONNECT パケットを送信したクライアントの接続プロセスがクラッシュする問題を修正しました。ゲートウェイは DISCONNECT で応答し、セッションを正常に閉じます。

- [#17287](https://github.com/emqx/emqx/pull/17287) MQTT-SN クライアントが接続または Will 状態外のパケット（接続セットアップ中の `DISCONNECT`、Will ハンドシェイク完了前の `REGISTER`、Will トピック未設定時の `WILLMSGUPD`）を受信してクラッシュする問題を修正しました。

- [#17581](https://github.com/emqx/emqx/pull/17581) JT/T 808 ゲートウェイで認証時に受け入れた電話番号を接続識別子として使用し、認証コード不一致の認証試行や異なる電話番号を持つアップリンクフレームを拒否するように修正しました。

#### マルチテナンシー

- [#17118](https://github.com/emqx/emqx/pull/17118) マルチテナンシーのリストエンドポイント（`/mt/ns_list`、`/mt/ns_list_details`、`/mt/managed_ns_list`、`/mt/managed_ns_list_details`、`/mt/ns/{ns}/client_list`）のページネーションを改善しました。

  - RFC 8288 準拠の `Link: <?...>; rel="next"` レスポンスヘッダーを追加。次ページが存在する場合はクエリのみの URI 参照を含み、存在しない場合はヘッダーが省略されます。これにより、完全ページ（`len(results) == limit`）と「データなし」の境界が明確になります。
  - 既存の排他的カーソル（`last_ns`、`last_clientid`）に加え、包括的キーセットカーソル（`first_ns`、`first_clientid`）を追加。包括的カーソルは完全一致検索（例：`?first_ns=foo&limit=1`）をサポートし、ページネーションの Link ヘッダーで維持されます。両者は同時に指定できず、両方指定時は HTTP 400 を返します。

- [#17406](https://github.com/emqx/emqx/pull/17406) 名前空間管理者が開始したトレースでキャプチャされるイベントを、その管理者の名前空間に限定しました。トレースタイプがトピック、IP アドレス、クライアント ID の場合に適用されます。ルール ID タイプのトレースは既にこの動作でした。

#### プラグイン

- [#16784](https://github.com/emqx/emqx/pull/16784) シングルノード展開時のプラグイン起動時のノイズを削減しました。

  クラスタ設定同期中にローカルノードからプラグイン設定を取得しようとするのをやめ、起動時の `config_not_found_on_node` 警告を回避します。

- [#16823](https://github.com/emqx/emqx/pull/16823) 事前インストール済みプラグインの Dashboard 管理問題を修正しました。

  ノード起動前に `plugins/` にプラグインパッケージを展開している場合、Dashboard から起動してもプラグイン設定ページで `Plugin Config Not Found` が表示されなくなりました。

- [#16842](https://github.com/emqx/emqx/pull/16842) ピアノードにプラグイン設定がまだない場合のノイズの多い警告ログを削減しました。

  以前は起動時にピアノードからプラグイン設定取得を試み、全ピアが設定を持たない場合でも警告を出していました。現在はこの無害なケースをデバッグレベルでログ出力し、本当のエラー（RPC 失敗、タイムアウト）のみ警告とします。

- [#16843](https://github.com/emqx/emqx/pull/16843) HTTP ヘッダーおよびクエリ文字列パラメータがプラグイン API ハンドラーに渡らず、空のヘッダーやパラメータになる問題を修正しました。

- [#16904](https://github.com/emqx/emqx/pull/16904) 同時に複数バージョンの同一プラグインを有効化・起動できないようにしました。新しいバージョンを有効化すると古いバージョンは自動的に無効化され、管理 API 操作は別バージョンがアクティブな場合に明確なエラーを返します。

- [#17247](https://github.com/emqx/emqx/pull/17247) プラグインの REST API コールバックがクラッシュまたはタイムアウトした際に、失敗した API メソッドとパス、設定されたタイムアウトをログに出力するようにしました。タイムアウトは警告レベルでログ出力され、正当な長時間処理が必要な場合は `plugins.api_endpoint.timeout` 設定を参照するヒントを含みます。

- [#17473](https://github.com/emqx/emqx/pull/17473) プラグインの Erlang アプリケーションが他の実行中アプリケーションに依存しているため停止できない場合のログレベルを警告から情報に下げました。プラグインアンロード時の期待される非アクション状態であり、警告を出さなくなりました。

- [#17575](https://github.com/emqx/emqx/pull/17575) `emqx_username_quota` プラグインの競合状態を修正しました。ユーザー名毎のセッションカウンターが実際のクライアントレコード数と不整合になることがあり、カウンターがゼロ以下に減算され削除された後に同時登録でインクリメントされると増分が失われていました。

#### REST API

- [#17002](https://github.com/emqx/emqx/pull/17002) `minirest` ライブラリを 1.4.12 に更新しました。このバージョンは EMQX API が `204 No Content` ステータスで不正な `content-length` ヘッダーを出力するバグを修正しています。

- [#17054](https://github.com/emqx/emqx/pull/17054) `GET /api/v5/configs?key=...` が `Accept: application/json` 設定時に不完全なデータを返す問題を修正しました。

  以前は JSON レスポンスが `key` クエリパラメータを無視し、`multi_tenancy` などのキーを含まない固定サブセットを返していました。現在は JSON レスポンスも hocon（text/plain）レスポンスと同様に `key` パラメータを尊重します。

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` が `Content-Type: application/json` でレスポンスを返すように修正しました。以前はレスポンスボディは有効な JSON でしたがヘッダーが `text/plain; charset=utf-8` で、コンテンツタイプで振り分けるクライアントが動作しませんでした。

#### 可観測性

- [#16661](https://github.com/emqx/emqx/pull/16661) 無効なトピック要求時の `topic_metrics` と `cluster_rpc` ロギングを改善しました。
- [#16674](https://github.com/emqx/emqx/pull/16674) Erlang PID をログデータフィールドとして確実に出力するようにしました。
- [#16876](https://github.com/emqx/emqx/pull/16876) ログメッセージ `msg_publish_not_allowed` を `msg_not_routed_to_subscribers` に変更しました。

- [#16879](https://github.com/emqx/emqx/pull/16879) 監査ログ DB キャッシュサイズの主要設定キーを `log.audit.cache_size` に変更し、互換性のため `log.audit.max_filter_size` も残しました。

- [#17513](https://github.com/emqx/emqx/pull/17513) Prometheus のマッチした認可の許可・拒否メトリクスを実際の認可判定を反映するよう修正しました。

#### デプロイメント

- [#16545](https://github.com/emqx/emqx/pull/16545) `node.cookie` の `#` 文字処理を修正しました。以前は `abc#d` のような場合、`#` 以降は無視され `abc` のみがクッキーとして使われていました。

  バックスラッシュ、シングルクォート、ダブルクォート、スペースを含むクッキーは拒否するバリデーションを追加しました。

- [#16620](https://github.com/emqx/emqx/pull/16620) aarch64 での CRC32C 動的ライブラリ読み込み問題を修正しました。

- [#16657](https://github.com/emqx/emqx/pull/16657) 古いノードバージョンから新しいノードバージョンに設定をインポートした際に、値が新しいコードに合わせてアップグレードされず不整合が起きる問題を修正しました。

  例として、5.10.0 以前の静的 clientid を持つ MQTT コネクター設定のユーザー名・パスワードが特定 clientid に紐付けられず、内部表現が異なるため変換が欠落していました。

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP リスナーがバインドアドレスが IPv6 の場合に自動的に IPv6 を使うようにし、`inet6 = true` の明示設定を不要にしました。

- [#17227](https://github.com/emqx/emqx/pull/17227) クラスタ設定ファイル保存エラーでファイル名と原因をログおよび HTTP 400 レスポンスに含めるようにしました。

  `cluster.hocon`（またはディレクトリ）が読み取り専用や不変、書き込み不可の場合、Dashboard や REST API での設定変更はこれまで不透明な HTTP 400 とファイル名なしの `badmatch` クラッシュログを返していました。

  現在は：

  - 実際のファイルパスと理由（`eacces`、`eperm`、`ebusy` 等）および運用者側の一般的な原因ヒントを含む `failed_to_save_conf_file` ログを出力。
  - ファイル名と理由を含む構造化 HTTP 400 ボディを返し、Dashboard で原因が分かりやすくなりました。

  一時ファイル書き込み失敗時（例：読み取り専用ディレクトリ）も正しく失敗を報告します。以前は HTTP 200 を返していました。

- [#17246](https://github.com/emqx/emqx/pull/17246) `jose` ライブラリを 1.11.10 から 1.11.12 にアップグレードし、OTP 新バージョン向けの EC および EdDSA キー修正を取り込みました。

- [#17252](https://github.com/emqx/emqx/pull/17252) 公式ダウンロードサイトのプラグインパッケージに `.sha256` チェックサムサイドカーを公開し、ダウンロードしたプラグインアーカイブの整合性検証を可能にしました。

- [#17254](https://github.com/emqx/emqx/pull/17254) コンテナ内のメモリ使用報告を改善しました。ブローカーは cgroup v2、cgroup v1、ホストの `/proc/meminfo` のうち最も制約の厳しいメモリ値を選択します（非ゼロ最小値が勝ち、使用率が大きい方が同率の場合はそちらを選択）。以前は制約の厳しい cgroup 制限があるコンテナでホストビューが誤って高い値を示したり、その逆が起きたり、制限なし cgroup では使用率がほぼ 0% と報告されることがありました。過負荷保護閾値と `Memory used` メトリクスは実際に制約している制限を反映します。

- [#17271](https://github.com/emqx/emqx/pull/17271) 公式 EMQX Docker イメージのセキュリティスキャナー指摘を解消しました。

  - ランタイムイメージビルド時に Debian セキュリティアップグレードを適用し、最新のパッチ済み `libssl3t64` を取得。
  - 未使用の `libgnutls30t64` パッケージを削除。EMQX は Erlang/OTP 経由で OpenSSL を使い、GnuTLS はリンクしません。`curl` の依存として存在していました。
  - Debian の `curl` パッケージを [stunnel/static-curl](https://github.com/stunnel/static-curl) のスタティックリンクバイナリ（OpenSSL、HTTP/2、HTTP/3 対応、RTMP・GnuTLS 非対応）に置き換え。Debian パッケージは `librtmp1` 経由で `libgnutls30t64` を再導入していましたが、スタティックバイナリはこれを回避しつつ `curl` を使ったコンテナヘルスチェックを維持します。

- [#17311](https://github.com/emqx/emqx/pull/17311) コンテナホスト名が解決できない場合の Docker 起動問題を修正しました。エントリポイントはノード名自動生成前にインターフェイス IP アドレスにフォールバックし、ノードホストが判明しない場合は明確なエラーで失敗します。

- [#17342](https://github.com/emqx/emqx/pull/17342) エクスポートされた `cluster.hocon` に部分的な `node` セクションが含まれている場合に、`node.cookie` のスキーマチェックエラーでクラスタ設定インポートが失敗する問題を修正しました。読み取り専用ルート（`node`、`rpc`）はデータインポート対象外なので、インポート前に削除して実行中ノードの値を使うようにしました。

- [#17369](https://github.com/emqx/emqx/pull/17369) Dashboard リスナーのデフォルト設定（`http.bind` とプレースホルダーの HTTPS `ssl_options`）をユーザー編集可能な `etc/emqx.conf` から配布済みの `etc/base.hocon` に移動しました。

  以前はハードコードされた `emqx.conf` ブロックが再起動時にデフォルトの自己署名証明書へのランタイム更新を静かに上書きしていました。Dashboard、REST API、`emqx_acme` プラグインの自動 HTTPS 設定によるランタイム更新は再起動後も正しく保持されます。

- [#17536](https://github.com/emqx/emqx/pull/17536) Dashboard の SSL リスナー `password` や MQTT ブリッジパスワード、クラスタリンクパスワード、Dashboard OIDC クライアントシークレット、S3 シークレットアクセスキー、AI 補完 API キー、Pulsar/RocketMQ 資格情報などのシークレット型設定フィールドのツールチップに `file://` オプションをドキュメント化しました。

  汎用シークレット型説明には既に記載されていましたが、フィールド固有の説明が上書きしていたため、ユーザーがリテラル値のみ受け入れると誤解していました。

- [#17540](https://github.com/emqx/emqx/pull/17540) SSL リスナーで `password = "file://..."` を設定し、キーファイルが暗号化されている場合に設定検証が `bad_password_or_invalid_keyfile` で失敗するバグを修正しました。`file://` 参照はランタイムだけでなく検証時にも解決されます。

## 6.0.2

*リリース日: 2026-01-16*

EMQX 6.0.2 へのアップグレード前に、破壊的変更点および既知の問題を必ずご確認ください。

### 強化点

#### セキュリティ

- [#16461](https://github.com/emqx/emqx/pull/16461) EMQX は TLS 1.3 のステートレスセッションチケットを使ったセッション再開をサポートし、サーバー側セッション状態なしでクライアントが TLS 接続を再開可能になりました。

  **設定**

  - **ノードレベル**: `node.tls_stateless_tickets_seed`

    TLS 1.3 ステートレスセッションチケット生成用の秘密鍵シード。

  - **リスナーレベル**: `listeners.ssl.<name>.ssl_options.session_tickets`

    TLS 1.3 セッション再開を有効化。サポート値：

    - `disabled`（デフォルト）
    - `stateless`
    - `stateless_with_cert`（チケットに証明書情報を含む）

  **注意**

  - セッションチケットは `node.tls_stateless_tickets_seed` が設定（空でない）され、リスナー SSL オプションで `session_tickets` が有効な場合にのみ生成されます。
  - `session_tickets` が有効でも `node.tls_stateless_tickets_seed` が空の場合、セッションチケットは生成されず、リスナー起動時にエラーログが出ます。

  この PR には TLS 1.2 セッション再開設定の修正も含まれています。以前は SSL リスナーの `reuse_sessions` オプションが無効で、EMQX は常に TLS 1.2 セッション再開を有効化しようとしていました。現在は無効化可能です。TLS 1.2 セッション再開は 6.2.0 以降デフォルトで無効になります。

#### ルールエンジン

- [#16524](https://github.com/emqx/emqx/pull/16524) ルールエンジン SQL の base64 エンコード・デコード関数を強化し、パディングなしおよび URL セーフオプションをサポートしました。

  `base64_encode` と `base64_decode` はオプション引数で以下を制御可能：

  - **`no_padding`**：パディング文字（`=`）なしでエンコード・デコード。パディングを除去したい場合やパディングなし文字列をデコードする際に有用。
  - **`urlsafe`**：URL セーフな base64 エンコード・デコード。`+` を `-`、`/` を `_` に置換し、URL でエンコード不要な文字列にします。

  これらは個別または任意の順序で組み合わせて使用可能です。

  **ルール SQL の例：**

  パディングなしでエンコード：

  ```sql
  SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"
  ```

  URL セーフ文字でエンコード：

  ```sql
  SELECT base64_encode(payload, 'urlsafe') as encoded FROM "t/#"
  ```

  両方のオプションでエンコード：

  ```sql
  SELECT base64_encode(payload, 'no_padding', 'urlsafe') as encoded FROM "t/#"
  ```

  URL セーフ base64 をデコード：

  ```sql
  SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"
  ```

  パディングなし URL セーフ base64 をデコード：

  ```sql
  SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
  ```

- [#16533](https://github.com/emqx/emqx/pull/16533) JSON データおよび JWT トークンからドット区切りキーで値を抽出する可変長式ヘルパー関数 `json_value` と `jwt_value` を追加しました。

  - `json_value` は JSON バイナリ文字列からネストしたオブジェクトをドット区切りキーで辿って値を取得します。
  - `jwt_value` は JWT のペイロードをデコードし、同様のドット区切りパスでクレーム値を抽出します。

  **例：**

  - `username` に JSON オブジェクトが含まれる場合、`json_value(username, 'shop.floor')` でネストフィールドにアクセス可能。
  - `password` にカスタムクレームを含む JWT がある場合、`jwt_value(password, 'client_attrs.unitid')` でネスト値にアクセス可能。

- [#16539](https://github.com/emqx/emqx/pull/16539) ルールエンジン関数 `spb_decode` 使用時の Sparkplug B メトリクスエイリアス追跡をサポートしました。

  デバイスやエッジノードが `NBIRTH` または `DBIRTH` メッセージをパブリッシュすると、EMQX はメッセージ内で定義されたエイリアスと名前のマッピングを記録します。後で同セッションの `NDATA` または `DDATA` メッセージに `spb_decode` を適用すると、元のメトリクス名が自動復元されデコード出力に含まれます。

  注意：フォールバックアクション実行時はマッピングが利用できません。フォールバックアクションが未デコードの `DDATA`/`NDATA` ペイロードを Sparkplug B トピックに再パブリッシュすると、メトリクスの `name` フィールドはエイリアスマッピングされません。

#### 耐久ストレージ

- [#16136](https://github.com/emqx/emqx/pull/16136) 耐久ストレージのリソース管理と性能を改善しました。

  耐久ストレージデータベースグループの概念を導入し、メンバー間でメモリテーブルサイズやディスク使用量クォータなどのリソースを共有可能にしました。

  新しいメトリクス（DB グループ毎）：

  - `emqx_ds_disk_usage`: SST ファイルの合計サイズ
  - `emqx_ds_write_buffer_memory_usage`: RocksDB メモリテーブルサイズ
  - `emqx_ds_total_trash_size`: ゴミ SST ファイルのディスク使用量

  新しいグループ設定：

  - `durable_storage.db_groups.<group>.storage_quota`: SST ファイルサイズのソフトクォータ
  - `durable_storage.db_groups.<group>.write_buffer_size`: 最大メモリテーブルサイズ
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` と `durable_storage.db_groups.<group>.rocksdb_nthreads_low`: RocksDB スレッドプールサイズ

  クォータ超過時に発生する新しいアラーム：`db_storage_quota_exceeded:<DB>` があります。詳細はドキュメントの「ストレージクォータ」セクションを参照してください。

  セッションチェックポイント間隔のデフォルトは 15 秒に変更されました。

- [#16286](https://github.com/emqx/emqx/pull/16286) デフォルトの耐久ストレージ設定を最適化し、不要な DB サブスクリプションを無効化して CPU 負荷を削減しました。

#### パフォーマンス

- [#16413](https://github.com/emqx/emqx/pull/16413) MQTT セッションプロセスの冗長な監視を減らし、サブスクリプション処理性能を改善しました。

### バグ修正

#### コア MQTT 機能

- [#16354](https://github.com/emqx/emqx/pull/16354) MQTT v5 接続でリクエストレスポンス情報プロパティ処理時の型不一致によるクラッシュを修正しました。

- [#16515](https://github.com/emqx/emqx/pull/16515) ブローカーがクライアントが通知した `Maximum-Packet-Size` を超えるメッセージを送信すると WebSocket 接続がクラッシュする問題を修正しました。

- [#16569](https://github.com/emqx/emqx/pull/16569) 高負荷時にフラッピング検出用の `emqx_flapping` プロセスがクラッシュする稀な競合状態を修正しました。

#### データ統合

- [#16265](https://github.com/emqx/emqx/pull/16265) Kafka ソースコネクターのヘルスチェックで、現在の EMQX ノードに割り当てられたパーティションのみリーダー接続を検証するようにし、不要なアイドル接続や誤警報を防止しました。

  以前はすべてのパーティションのリーダー接続を検証しており、クラスタ展開時にノードが割り当てられていないパーティションのリーダー接続がアイドル状態となり、Kafka がタイムアウトで切断し誤警報が発生していました。

- [#16542](https://github.com/emqx/emqx/pull/16542) Kafka プロデューサー接続が Kafka 過負荷時に早期切断され、過剰なリトライを引き起こす問題を修正しました。

  プロデュースリクエストのタイムアウトはメタデータリクエストタイムアウトの少なくとも 2 倍、最小 30 秒に自動設定され、メタデータリクエストが短時間設定でも不要な再接続・リトライを減らします。

- [#16352](https://github.com/emqx/emqx/pull/16352) Apache Pulsar クライアントを 2.1.2 にアップグレードしました。Pulsar プロデューサーアクションの `batch_size` が `1` の場合、単一メッセージを単一要素バッチではなくエンコードし、Key Share 戦略でコンシューマーが負荷分散可能にしました。

- [#16383](https://github.com/emqx/emqx/pull/16383) IoTDB コネクターの REST API ドライバー使用時のヘルスチェックを改善しました。

  以前はヘルスチェックでクライアント認証情報を検証していませんでしたが、軽量な no-op クエリを送信し、誤設定認証情報を早期検出可能にしました。

- [#16507](https://github.com/emqx/emqx/pull/16507) MQTT ソースがコネクター再接続後にメッセージ受信を停止する問題を修正しました。

  以前は MQTT ソースのコネクターが接続喪失から復旧してもトピック再サブスクライブが行われず、コネクター再起動まで動作停止していました。現在は再接続時に自動的に再サブスクライブします。

#### クラスタリング

- [#16269](https://github.com/emqx/emqx/pull/16269) クラスタリンクのルート複製プロトコル回復シーケンスで、リモート側が再ブートストラップを必要としているのにスキップされる問題を修正しました。

- [#16317](https://github.com/emqx/emqx/pull/16317) 複数独立クラスタリンクが存在し、一部リンクが長期間ダウンしている場合に、内部ルーティングテーブルからアクティブルートが誤って削除されるクラスタリンクのガベージコレクションロジック問題を修正しました。

- [#16465](https://github.com/emqx/emqx/pull/16465) `gen_rpc` を 3.5.1 にアップグレードしました。

  これ以前はピアノードが到達不能な場合に接続タイムアウトの長いクラッシュログが続きましたが、新バージョンは長いクラッシュログをなくし、読みやすいエラーログに変えています。頻発する `"failed_to_connect_server"` ログもスロットリングされます。

- [#16544](https://github.com/emqx/emqx/pull/16544) クラスタ自動クリーン手順の堅牢性を改善しました。以前はノード起動時に自動クリーン機能が無効化されていると、その後の設定変更で有効化されませんでした。

#### アップグレード

- [#16308](https://github.com/emqx/emqx/pull/16308) EMQX 5.3.0 未満からのアップグレード後に多要素認証（MFA）が有効化できない問題を修正しました。ログインユーザーデータベースレコードの非互換が原因でした。

#### 設定管理

- [#16397](https://github.com/emqx/emqx/pull/16397) リスナー起動前に TLS 証明書と鍵ファイルの検証を追加しました。

  EMQX は SSL リスナー設定の解析時に基本検証を行い、不正な PEM ファイルが検出されるとエラーレベルログ（例：`invalid_pem_file_ignored`、`bad_keyfile_ignored`）を出力します。これにより TLS ハンドシェイク失敗のトラブルシューティングが容易になります。

#### アクセス制御

- [#16423](https://github.com/emqx/emqx/pull/16423) 認証時に JWT の `aud`（オーディエンス）クレーム検証をサポートしました。

  `verify_claims` に `aud` が設定されている場合、JWT は有効な `aud` 値を含む必要があります。文字列と配列形式の両方をサポート：

  - `aud` が文字列の場合、設定値と完全一致する必要があります。
  - `aud` が配列の場合、少なくとも 1 要素が設定値と一致する必要があります。
  - 空文字列または空配列は検証失敗。
  - `verify_claims` に `aud` が設定されているのに JWT に `aud` がない場合も検証失敗。

- [#16459](https://github.com/emqx/emqx/pull/16459) SCRAM 認証 HTTP API のユーザー作成 API で誤ったユーザー ID を返す問題を修正しました。

#### 可観測性

- [#16417](https://github.com/emqx/emqx/pull/16417) `resource_exception` イベントのログ量を削減しました。リソース例外発生時のログはスロットリングされ、大きなタームはマスクされます。

- [#16537](https://github.com/emqx/emqx/pull/16537) `gen_rpc` の特定エラーメッセージで発生するフォーマッタークラッシュを修正しました。

  以前は `gen_rpc` が特定のエラー（送信タイムアウトなど）をログ出力すると EMQX が「FORMATTER CRASH」エラーでクラッシュしていました。現在は安全に処理します。

## 6.0.1

*リリース日: 2025-11-11*

EMQX 6.0.1 へのアップグレード前に、破壊的変更点および既知の問題を必ずご確認ください。

### 強化点

#### メッセージキュー

- [#16080](https://github.com/emqx/emqx/pull/16080) メッセージキュー機能を無効化する設定オプションを追加しました。メッセージキューを無効化するとクラスタのリソース使用量がわずかに減少します。耐久セッションも無効化すると、耐久ストレージの維持を回避し、管理オーバーヘッドを減らし性能を向上させます。

- [#16096](https://github.com/emqx/emqx/pull/16096) クライアントが存在しない `$q/` トピックにサブスクライブした際にメッセージキューを自動作成するサポートを追加しました。通常キューとラストバリューセマンティクスキューの両方で自動作成を有効化する設定オプションがあります。

- [#16097](https://github.com/emqx/emqx/pull/16097) 通常メッセージキューへのメッセージ書き込みを最適化し、トランザクション付き追加からダーティ追加関数に置き換えました。QoS 0 メッセージは非同期追加を使います。これにより通常キューへのメッセージ挿入性能が大幅に向上します。

- [#16098](https://github.com/emqx/emqx/pull/16098) システム内のメッセージキュー総数を制限する最大キュー数設定オプションを追加しました。

- [#16152](https://github.com/emqx/emqx/pull/16152) キュー毎の最大メッセージ数および合計メッセージサイズの制限を導入しました。メッセージ追加レイテンシを監視する新しいメトリクスも追加し、性能やキュー制限問題の診断に役立ちます。

#### データ統合

- [#16121](https://github.com/emqx/emqx/pull/16121) GreptimeDB インジェスタークライアントを [v0.2.3](https://github.com/GreptimeTeam/greptimedb-ingester-erl/releases/tag/v0.2.3) にアップグレードしました。複数のバグ修正と行ベース gRPC プロトコルサポートを追加しました（列ベースプロトコルは非推奨）。

  さらに CI イメージを最新の安定版 GreptimeDB に更新しました。

- [#16127](https://github.com/emqx/emqx/pull/16127) [#16121](https://github.com/emqx/emqx/pull/16121) の変更に伴い、GreptimeDB コネクターの無効な文字列値問題を修正しました。

#### パフォーマンス

- [#15949](https://github.com/emqx/emqx/pull/15949) リスナー設定の `parse_unit` のデフォルト値を `chunk` から `frame` に変更しました。ペイロードサイズがソケットバッファ（デフォルト 4 KB）を超える場合の CPU 使用率を大幅に削減します。

  **注意**：`parse_unit = frame` の場合、`PUBLISH` パケットが最大許容サイズを超えると、EMQX は `DISCONNECT` パケットを送信せず接続を切断します。

- [#16165](https://github.com/emqx/emqx/pull/16165) `GET /clients_v2` API の性能を最適化しました。クラスタに約 5 万クライアント以上存在する場合、クライアント一覧取得 API 呼び出しが非常に遅くなるかタイムアウトすることがありました。

### バグ修正

#### コア MQTT 機能

- [#15884](https://github.com/emqx/emqx/pull/15884) 稀にグローバルルーティングテーブルがクラスタを離脱したノードの情報を無期限に保持する問題を修正しました。

- [#15518](https://github.com/emqx/emqx/pull/15518) 多数の共有サブスクライバーが同時切断した際にクラスタのルーティングテーブルや共有サブスクリプション状態に不整合が蓄積される競合状態を修正しました。

#### アップグレード

- [#16047](https://github.com/emqx/emqx/pull/16047) EMQX Enterprise ベースバージョン 5.8.0 以降から 6.0 へのローリングアップグレードをサポートしました。アップグレード中にレガシー設定は自動的に 6.0 でサポートされる新形式にマイグレーションされます。特に廃止された `bridges` 設定ルートは新しい `connectors`、`sources`、`actions` ルートに変換されます。

  ただし、GCP PubSub コンシューマーと Kafka コンシューマーソースは手動変更が必要です。古い設定に `topic_mapping` フィールドが含まれる場合は削除し、以前の `topic_mapping` の各エントリに対して個別の「ソース＋ルール」ペアを手動作成してください。

#### セキュリティ

- [#16156](https://github.com/emqx/emqx/pull/16156) EMQX 5.10 と比較して一部依存関係でデフォルト設定が欠落し、RSA 署名検証失敗を引き起こす問題を修正しました。欠落したデフォルトにより以下のようなエラーログが発生していました：

  ```
  {sign_unsupported,[[{rsa_padding,rsa_pkcs1_padding}]]}, [{jose_jwa_unsupported,verify,5,[{file,"src/jwa/jose_jwa_unsupported.erl"},{line,55}]}
  ```

- [#16175](https://github.com/emqx/emqx/pull/16175) TLS 証明書の定期的なガベージコレクションで、管理された名前空間の設定で使用中の証明書ファイルが誤って削除される問題を修正しました。

#### アクセス制御

- [#16081](https://github.com/emqx/emqx/pull/16081) 拡張認証とメモリベースセッションを使うクライアントが `session_stepdown_request_exception`（`calling_self` エラー）でクラッシュする問題を修正しました。

  <details> <summary>エラーログ例</summary>

  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### クラスタリング

- [#16123](https://github.com/emqx/emqx/pull/16123) Mria レプリケーション管理コンポーネントのバグを修正し、コア・レプリカントクラスタでクラスタ参加がハングまたは不完全になる問題を解決しました。

  新規コアノード追加時にレプリカントが必要とするレプリケーション関連プロセスが起動しないことがあり、アップグレードまたは新規追加されたレプリカントが起動時にハングしました。

  Kubernetes 展開ではこれによりレディネスプローブが失敗し、コントローラーが対象レプリカント Pod を繰り返し再起動していました。

  この問題は新旧バージョンのコアおよびレプリカントノードを追加するローリングアップグレードでよく発生しました。

#### ルールエンジン

- [#16028](https://github.com/emqx/emqx/pull/16028) ルールエンジンの `jq` 関数のメモリリークを修正しました。

  例：`jq` の組み込み関数 `index`（例：`.key | index("name")`）使用時にメモリリークが発生していました。

#### データ統合

- [#16010](https://github.com/emqx/emqx/pull/16010) フォールバックアクションが非同期クエリモードで、元ルールの SQL にルール環境の `metadata` フィールドが含まれない場合に `function_clause` エラーで失敗する問題を修正しました。

  例エラーログ：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16046](https://github.com/emqx/emqx/pull/16046) 数百のアクションを持つコネクター設定の読み込みや再起動時に発生する可能性のあるメモリ不足（OOM）クラッシュを修正しました。

- [#16140](https://github.com/emqx/emqx/pull/16140) Redis クラスタフェイルオーバー時にコネクターが「接続中」状態に固まる問題を修正しました。

  以前は EMQX の Redis クラスタクライアントが通常クエリ（`GET` など）失敗時のみクラスタトポロジを更新し、定期的な `PING` 失敗では更新しませんでした。そのためフェイルオーバー後に他のコマンドが発行されないと古いトポロジを使い続け、復旧できませんでした。

  修正後は失敗した `PING` 応答もトポロジ更新をトリガーし、迅速に復旧します。

#### MQTT 耐久セッション

- [#16105](https://github.com/emqx/emqx/pull/16105) 耐久ストレージの性能最適化。特に耐久セッションを使うクライアントの `CONNACK` レイテンシを削減しました。

- [#16129](https://github.com/emqx/emqx/pull/16129) 耐久ストレージのトランザクション設定をランタイムで変更可能にしました。以前は設定変更にノード再起動が必要でした。

#### 可観測性

- [#15963](https://github.com/emqx/emqx/pull/15963) リモートシェル（`remsh`）でのループ評価中に発生する過剰な監査ログエントリを削減しました。

- [#15967](https://github.com/emqx/emqx/pull/15967) 大量監査ログのクリーンアップ中に Mnesia トランザクションがブロックされ、急激なメモリ増加を引き起こす問題を修正しました。

- [#16060](https://github.com/emqx/emqx/pull/16060) 非 ASCII 文字を含む深くネストしたデバッグレベルログのフォーマッタークラッシュを修正しました。

  <details> <summary>エラーログ例</summary>

  ```
  2025-09-29T06:55:34.120640+00:00 debug: FORMATTER CRASH: {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}}
  2025-09-29T06:55:34.120780+00:00 [debug] formatter_crashed: emqx_logger_textfmt, config: #{time_offset => [],chars_limit => unlimited,depth => 100,single_line => true,template => ["[",level,"] ",msg,"\n"],with_mfa => false,timestamp_format => auto,payload_encode => text}, log_event: #{meta => #{line => 44,pid => <0.281254.0>,time => 1759128934120640,file => "emqx_ai_completion_anthropic.erl",gl => <0.4317.0>,mfa => {emqx_ai_completion_anthropic,call_completion,3},report_cb => fun logger:format_otp_report/1,matched => <<"t/1">>,namespace => global,clientid => <<"c_emqx">>,trigger => <<"t/1">>,rule_id => <<"r1sczoo0">>,rule_trigger_ts => [1759128934120]},msg => {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}},level => debug}, reason: {error,badarg,[{erlang,iolist_to_binary,[["[",[["messages",": ",[[91,[[#...
  ```

  </details>

- [#16134](https://github.com/emqx/emqx/pull/16134) 新規ログトレース作成が一部ケースでできなくなる後方互換性問題を修正しました。

#### レートリミット

- [#16160](https://github.com/emqx/emqx/pull/16160) 個別クライアント接続のレートリミットアルゴリズムを改善しました。以前は接続直後や非アクティブ期間後にパブリッシュレート制限を一時的に超過することがありました。

  この更新によりリミッターの挙動がより予測可能かつ一貫し、接続開始時からレート制限が正しく適用されます。

## 6.0.0

*リリース日: 2025-09-30*

EMQX 6.0.0 へのアップグレード前に、破壊的変更点および既知の問題を必ずご確認ください。

### 機能ハイライト

EMQX Enterprise 6.0.0 は EMQX Enterprise バージョン 6 シリーズの最初のリリースであり、大幅なアーキテクチャ改善と新機能をもたらします。

#### メッセージキュー

ネイティブのメッセージキュー機能は、リアルタイム MQTT パブリッシュ／サブスクライブと永続的非同期キューイングを統合します。サーバーはトピックフィルターにマッチするメッセージをバッファし、サブスクライバーがオフラインでも保持します。クライアントは特別なトピック `$q/{topic}` を通じてこれらのメッセージを消費でき、信頼性の高いメッセージ配信を実現します。

メッセージキューはオフラインメッセージストレージ、ラストバリュー保持、柔軟なディスパッチ戦略をサポートし、MQTT にリアルタイムと耐久メッセージングの両方を強化します。

#### ネームスペース

ネームスペース機能は Dashboard のネームスペースレベルロールでマルチテナンシーと可観測性を改善します。ユーザーは自身のリソース（ルール、アクション、コネクターなど）に制限され、管理者やビューアなどの細粒度権限を持ちます。ロールは Dashboard、API、CLI で管理可能で、マルチテナント運用を簡素化します。

セッション数追跡も最適化され、1000 接続未満はオンデマンド更新、1000 以上は 5 秒毎に更新されます。旧バージョンからのローリングアップグレード中は一時的に不整合が見られますが、全ノード更新後に安定します。

#### MQTT 耐久セッション

耐久ストレージはセッションデータをブローカーの他のメタデータから分離し、RAM 使用量を大幅に削減し、ストレージ効率を向上させました。

新しい設定オプションで RocksDB のメモリ使用量と性能を細かく制御可能です。さらに、保存メッセージのデフォルトシリアライズスキーマを ASN.1 に更新し、効率を高めています。

#### 新しいデータ統合

- Google BigQuery
- AWS AlloyDB
- CockroachDB
- AWS Redshift

#### 統合強化

- **AWS**:
  - S3 または S3Tables データ統合で EC2 インスタンスのインスタンスメタデータサービス v2 API をサポート。手動の AWS 資格情報設定なしに S3 バケットにシームレスにアクセス可能で、IAM ロールを活用しセキュリティを向上。
  - S3 Tables アクションで Parquet フォーマットをサポート。

- **RabbitMQ**: RabbitMQ シンクでメッセージルーティングと互換性を強化するカスタムヘッダーおよびプロパティテンプレートを定義可能に。

- **Snowflake**: Snowflake アクションで Snowpipe ストリーミングアップロードモード（プレビュー機能）を追加。

- **RocketMQ**: アクションに新しい `key` と `tag` テンプレートフィールドを追加し、Produce Strategy の `key_dispatch` オプションを導入。メッセージメタデータのカスタマイズが可能。

#### Elixir サポート

すべてのパッケージが Elixir の Mix ビルドシステムを通じて Elixir サポートを含むようになり、Elixir コミュニティに開放され、IEx コンソールによる優れたツール利用が可能になりました。

#### LDAP サポート強化

LDAP 認可は JSON 形式の拡張 ACL ルールをサポートし、LDAP 認証は LDAP から直接 ACL ルールを取得し、クライアントメタデータでキャッシュして追加 LDAP クエリなしに認可を実施可能になりました。

#### トレーシング改善

最大トレース数（`trace.max_traces`）およびトレースファイルサイズ（`trace.max_file_size`）の設定可能な制限を導入しました。`max_file_size` に達するとトレースログは停止せず新しいファイルにローテーションします。

#### クラスタ管理

新設定 `cluster.description` により EMQX Dashboard にカスタムクラスタ説明を設定・表示可能になりました。

### 強化点

#### メッセージキュー

- [#15789](https://github.com/emqx/emqx/pull/15789) メッセージキューを実装しました。これは `topic_filter` で識別されるメッセージの集合で、明示的なライフサイクルを持ち、キューの寿命中にキューのトピックフィルターにマッチするパブリッシュメッセージで自動的に補充されます。クライアントは `$q/{topic}` 形式の特別なトピックにサブスクライブして協調的にキューからメッセージを消費できます。

#### コア MQTT 機能

- [#15805](https://github.com/emqx/emqx/pull/15805) シャーディングされたファンアウトメッセージ配信を処理する専用ワーカープールを導入しました。

  以前はブローカープールがサブスクリプション管理とメッセージ配信の両方を処理し、スケジューリング競合が発生していました。ファンアウト配信負荷を分離し、pub/sub 操作をより効率的に処理します。

#### アクセス制御

- [#15349](https://github.com/emqx/emqx/pull/15349) 認証・認可用外部リソース管理を最適化しました。無効化された認証器や認可器に設定されたリソースへの接続が維持される問題を解決しました。

- [#15294](https://github.com/emqx/emqx/pull/15294) LDAP 認証と認可を強化しました。LDAP 認可は JSON 形式の拡張 ACL ルールをサポートし、LDAP 認証は LDAP から ACL ルールを取得してクライアントメタデータにキャッシュし、追加 LDAP クエリなしに認可を実施可能です。

- [#15730](https://github.com/emqx/emqx/pull/15730) 認証結果に基づくクライアント ID 上書きをサポートしました。認証バックエンドが成功時に `clientid_override` 属性を返すと、元のクライアント ID を置き換えます。

  対応バックエンド：

  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis

- [#15820](https://github.com/emqx/emqx/pull/15820) 設定 `authorization.no_match` のデフォルト値を `allow` から `deny` に変更し、より安全なデフォルトにしました。

#### クラスタリング

- [#15600](https://github.com/emqx/emqx/pull/15600) クラスタに説明ラベルを追加する新設定 `cluster.description` を導入しました。`PUT /cluster` で更新可能で、`GET /cluster` API で取得できます。

#### LLM ベース MQTT データ処理

- [#15467](https://github.com/emqx/emqx/pull/15467) AI 補完プロバイダーのトランスポート設定オプションを公開しました。接続タイムアウトや最大接続数を設定可能で、高スループット時の `checkout_timeout` エラーを防ぎます。

- Flow デザイナーは [Google Gemini モデル](https://docs.mqttce.com/en/emqx/v6.0/flow-designer/gemini-node-quick-start.html) と統合をサポートします。

- [#15631](https://github.com/emqx/emqx/pull/15631) AI プロバイダーで利用可能なモデル一覧を取得する新 API エンドポイントを追加しました。

- [#15724](https://github.com/emqx/emqx/pull/15724) OpenAI の `response` API を使う AI 補完プロバイダーと補完プロファイルのために `openai_response` タイプを導入しました。

#### データ統合

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX は BigQuery とデータ統合をサポートします。

- [#15401](https://github.com/emqx/emqx/pull/15401) Snowflake アクションに Snowpipe ストリーミングアップロードモードを追加しました。

  *注：Snowpipe ストリーミングは現在 [プレビュー機能](https://docs.snowflake.com/en/release-notes/preview-features) であり、AWS ホストの Snowflake アカウントでのみ利用可能です。*

- [#15387](https://github.com/emqx/emqx/pull/15387) Kinesis プロデューサーコネクターとアクションのヘルスチェックにレート制限を追加し、AWS API クォータに準拠しクラスタ挙動を改善しました。

  - `ListStreams` と `DescribeStream` へのヘルスチェック呼び出しはコネクター毎にそれぞれ 5/s と 10/s に制限されます。
  - クラスタ内のコアノードが分散リミッターを調整し、一貫した制限を実施します。
  - ヘルスチェックがスロットルまたはタイムアウトした場合、コネクターやアクションは切断状態にせず前回の状態を保持します。

  また新設定 `resource_opts.health_check_interval_jitter` を導入し、`resource_opts.health_check_interval` に一様ランダム遅延を加え、同一コネクター下の複数アクションのヘルスチェック同時実行を減らします。

- [#15176](https://github.com/emqx/emqx/pull/15176) GreptimeDB コネクタークライアントをアップグレードし、自動作成テーブルのデフォルト TTL 設定用の新パラメータ `ttl` をサポートしました。

- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX は AWS AlloyDB、CockroachDB、AWS Redshift とデータ統合をサポートします。

- [#15635](https://github.com/emqx/emqx/pull/15635) RocketMQ アクションに新しい `key` と `tag` テンプレートフィールドを追加し、`Produce Strategy` フィールドに `key_dispatch` オプションを導入しました。

- [#15621](https://github.com/emqx/emqx/pull/15621) S3 Tables コネクターで `access_key_id` と `secret_access_key` をオプションにしました。省略時は EMQX がデプロイされた EC2 インスタンスのインスタンスメタデータサービス v2 API から取得します。

- [#15628](https://github.com/emqx/emqx/pull/15628) HStreamDB データ統合を削除しました。

- [#15544](https://github.com/emqx/emqx/pull/15544) Datalayers 統合のため Arrow Flight SQL NIF ドライバーサポートを追加しました。

- [#15637](https://github.com/emqx/emqx/pull/15637) RabbitMQ アクションでメッセージヘッダーとプロパティのテンプレート化をサポートしました。

- [#15864](https://github.com/emqx/emqx/pull/15864) 廃止された「Bridges V1」API と設定スキーマを削除しました。`/bridges/*` 以下のすべてのエンドポイントと `bridges` ルートキーの設定は利用できなくなりました。データ統合は完全に「Connectors/Actions/Sources」モデルに移行しています。

- [#15583](https://github.com/emqx/emqx/pull/15583) `brod` クライアントを 4.4.4 にアップグレードし、Kafka API のサポート範囲を拡大しました。`JoinGroups` API バージョン `v0` ～ `v1` の非推奨対応です。

#### スマートデータハブ

- [#15525](https://github.com/emqx/emqx/pull/15525) まだ使用中の内部スキーマの削除を防止しました。スキーマがスキーマ検証やメッセージ変換で参照されている場合、削除できず、ランタイムエラーや設定不整合を回避します。

#### 耐久ストレージ

- [#15463](https://github.com/emqx/emqx/pull/15463) 耐久ストレージの RAM 使用量とストレージ効率を改善しました。

  - RocksDB メモリ使用量とストレージ性能を制御する以下の設定を導入：
    - `durable_storage.messages.rocksdb.write_buffer_size`: シャード毎の RocksDB メモリテーブルサイズ
    - `durable_storage.messages.rocksdb.cache_size`: シャード毎の RocksDB ブロックキャッシュサイズ
    - `durable_storage.messages.rocksdb.max_open_files`: シャード毎の RocksDB ファイルディスクリプタ上限
    - `durable_storage.messages.layout.wildcard_thresholds`: `wildcard_optimized_v2` ストレージレイアウトのワイルドカード閾値調整

  - 保存メッセージのデフォルト `serialization_schema` を `asn1` に変更。

- [#16044](https://github.com/emqx/emqx/pull/16044) 耐久セッションの設定フィールドの一部を削除または名称変更し、旧値は非推奨にしました：

  - `durable_sessions.heartbeat_interval` は `durable_sessions.checkpoint_interval` に名称変更。
  - `durable_sessions.idle_poll_interval` と `durable_sessions.renew_streams_interval` は削除。セッションは完全にイベント駆動になりました。
  - `durable_sessions.session_gc_interval` と `durable_sessions.session_gc_batch_size` は廃止。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` ツールが現在のシステム設定を HOCON 形式でエクスポートし、パスワードやシークレットなどの機密情報を自動的にマスクするようになりました。

#### ネームスペース

- [#15841](https://github.com/emqx/emqx/pull/15841) 名前空間セッションのセッション数更新頻度を改善しました。

  - 名前空間の接続数が 1000 未満の場合、セッション数はオンデマンドで更新されます。
  - 1000 以上の場合は 5 秒毎に更新されます。

  6.0 未満からのローリングアップグレード中は内部追跡テーブルの変更により一時的にセッション数が不整合に見えることがありますが、クライアントがアップグレード済みノードに再接続するにつれて安定し、すべてのノードが 6.0 以降になると正確になります。

#### 可観測性

- [#15594](https://github.com/emqx/emqx/pull/15594) クラスタ全体でアクティブなトレースの最大数を制御する新設定 `trace.max_traces` を導入しました。この制限は `emqx ctl trace` で管理するノードローカルトレースには適用されません。

  実装も最適化し、作成されたトレース毎のアトムリークを排除しました。

- [#15556](https://github.com/emqx/emqx/pull/15556) 個別トレースの最大ファイルサイズを制限する新設定 `trace.max_file_size` を導入しました。

- [#15650](https://github.com/emqx/emqx/pull/15650) トレースログの自動ローテーションを実装しました。

  トレースファイルサイズが `trace.max_file_size` を超えると、EMQX は以降のイベントを破棄して不明瞭な警告を `stderr` に出す代わりに、古いイベントの一部を破棄し最新を保持します。

  これにより：

  - EMQX はアクティブなトレース毎に複数のトレースログファイルを保持します。トレースディレクトリのレイアウトが変更されました。
  - トレース API もこれに対応し、ログストリーム API はストリームが遅延した場合などに新しいエラーを返す可能性があります。

- [#15904](https://github.com/emqx/emqx/pull/15904) トレース設定の表示・更新をトレース API 経由でサポートしました。

#### パフォーマンス

- [#15451](https://github.com/emqx/emqx/pull/15451) TCP リスナー向けに実験的な `socket` バックエンドを導入しました。メッセージ処理レイテンシの改善と計算リソース使用量の削減を目指します。`tcp_backend` リスナーオプションで有効化可能です。

#### ビルドとツーリング

- [#15484](https://github.com/emqx/emqx/pull/15484) ビルドシステムを Elixir の Mix に切り替え、すべてのパッケージでネイティブな Elixir サポートを含むようにしました。これにより開発者ツールが向上し、必要に応じて Elixir 依存関係と統合可能で、より強力な EMQX コンソールとして [IEx](https://hexdocs.pm/iex/IEx.html) シェルを利用可能にします。

#### ライセンス

- [#15921](https://github.com/emqx/emqx/pull/15921) クラスタ全体の最大トランザクション毎秒（TPS）に対するライセンスアラームを導入しました。

  - 各ノードは過去 10 秒間の MQTT メッセージ送受信平均数を TPS として計算します。
  - クラスタ全体の TPS は 5 秒毎に集計されます。
  - 観測された TPS がライセンス上限を超えるとアラームが発生します。
  - より高い TPS 許容量を持つライセンスが適用されるまでアラームは継続します。

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) 環境変数 `QUICER_SKIP_NIF_LOAD=1` を設定することで QUIC スタックの NIF ロードを無効化可能にしました。

### バグ修正

#### コア MQTT 機能

- [#15396](https://github.com/emqx/emqx/pull/15396) 切断されたクライアントの共有サブスクリプションに対
