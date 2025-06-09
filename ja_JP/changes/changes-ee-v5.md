# EMQX Enterprise バージョン 5

## 5.9.0

*リリース日: 2025-05-02*

EMQX 5.9.0 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#14721](https://github.com/emqx/emqx/pull/14721) 遅延パブリッシュ間隔の上限を 4294967 秒（約49.7日）から 42949670 秒（約497日）に変更しました。

- [#14595](https://github.com/emqx/emqx/pull/14595) `retainer.enable` フラグを非推奨にしました。Retainer はゾーン設定の `mqtt.retain_available` フラグに基づいて自動的に開始・停止されます。

#### インストールおよびデプロイメント

- [#14930](https://github.com/emqx/emqx/pull/14930) macOS 15 (Sequoia) パッケージのリリースを開始しました。
- [#14590](https://github.com/emqx/emqx/pull/14590) 評価ライセンスで動作するノードの最大アップタイムを1ヶ月に制限しました。アップタイム制限に達すると、新規接続を拒否します。

#### ネームスペース

- [#14261](https://github.com/emqx/emqx/pull/14261) MQTT クライアント管理におけるネームスペース機能を強化しました。

  **新機能**:

  - ネームスペースクライアント認識: `tns` 属性を持つ MQTT クライアントはネームスペースクライアントとして扱われます。
  - ネームスペースインデックス: クライアントIDインデックスに MQTT クライアントのネームスペース (`tns`) を追加し、マルチテナンシーをサポートします。

  **API**:

  - ネームスペース一覧取得: ページネーション対応 API を追加  
    エンドポイント: `/api/v5/mt/ns_list`
  - ネームスペース内クライアントセッション一覧取得: ページネーション対応 API を追加  
    エンドポイント: `/api/v5/mt/:ns/client_list`
  - ネームスペース内アクティブクライアントセッション数取得: 新 API を追加  
    エンドポイント: `/api/v5/mt/:ns/client_count`

  **設定**:

  - ネームスペースごとのセッション制限: `multi_tenancy.default_max_sessions` 設定を追加し、ネームスペースごとのクライアントセッション数制限を適用可能に。

  注意:

  - 管理者ネームスペース（管理ユーザーグループ）は本プルリクエストに含まれておらず、現在開発中です。

- [#14884](https://github.com/emqx/emqx/pull/14884) ネームスペース設定管理用の HTTP API を追加しました。

- [#14840](https://github.com/emqx/emqx/pull/14840) ネームスペース機能のクライアントおよびテナントレートリミッター設定用 HTTP API エンドポイントを追加しました。

#### 認証および認可

- [#14584](https://github.com/emqx/emqx/pull/14584) ダッシュボードの 2FA（2要素認証）ログインに対応した認証アプリを追加しました。

- [#14979](https://github.com/emqx/emqx/pull/14979) 認証および認可テンプレートで `zone` と `listener` のサポートを追加しました。ACL ルールの `who` マッチ条件にも `zone` と `listener` を追加。

  これにより、リスナー単位やゾーン単位のアクセス制御が可能になります。例:

  - HTTP 認証リクエストのボディテンプレートで `zone` 名を送信:  
    `{"username": "${username}", "zone": "${zone}"}`

  - `acl.conf` で SSL リスナー経由のクライアントのみ `${username}/#` のサブスクライブを許可:  
    `{allow, {listener, "ssl:default"}, subscribe, ["${username}/#"]}.`

- [#14976](https://github.com/emqx/emqx/pull/14976) 認証器の事前条件設定をサポートしました。

  クライアント情報に基づいて認証器の呼び出しを選択的に行うことで、不要な認証リクエストを回避可能です。例えば、`tcp:default` 経由のクライアントのみ HTTP 認証器を、`ssl:default` 経由のクライアントのみ Postgre 認証器を呼び出す設定が可能です。

- [#14966](https://github.com/emqx/emqx/pull/14966) デフォルトのダッシュボード管理者ユーザーの削除を可能にしました。ただし、他に管理者ユーザーが最低1人存在する必要があります。

- [#14358](https://github.com/emqx/emqx/pull/14358) LDAP 認証/認可テンプレートで使用可能な変数を他の認証/認可ソースと同様に制限し、サポートされていない変数はレンダリングしません。

- [#14610](https://github.com/emqx/emqx/pull/14610) 外部ソースから取得または組み込みデータベースに保存された認可ルールの追加フィールドを処理可能にしました。

  新たにサポートされたフィルター:

  - `username_re`: ユーザー名による正規表現フィルター
  - `clientid_re`: クライアントIDによる正規表現フィルター
  - `ipaddr`: IPアドレス/マスクによるフィルター

  すべてのフィルターが一致した場合にのみルールが適用されます。

- [#14329](https://github.com/emqx/emqx/pull/14329) 認証および認可で外部リクエストのテンプレートに `${peerport}` 変数を利用可能にしました。

- [#14286](https://github.com/emqx/emqx/pull/14286) 認証および認可のノードレベルキャッシュを実装しました。

  外部サービス呼び出しが必要な認証/認可方式の負荷軽減に有効です。HTTP、LDAP、MongoDB、MySQL、PostgreSQL、Redis のバックエンドで利用可能です。

#### REST API

- [#14254](https://github.com/emqx/emqx/pull/14254) `/status` HTTP エンドポイントでクラスター名を返すようにしました。
- [#14972](https://github.com/emqx/emqx/pull/14972) プラグイン設定の個別ダウンロード/アップロード用 API メソッドを実装しました。
- [#15013](https://github.com/emqx/emqx/pull/15013) ルールエンジン HTTP API のルール情報に `action_details` フィールドを追加。各ルールが参照するアクションのタイプ、名前、状態を含みます。
- [#14735](https://github.com/emqx/emqx/pull/14735) ルールエンジン HTTP API のルール情報に `last_modified_at` フィールドを追加しました。

#### クラスタリング

- [#14766](https://github.com/emqx/emqx/pull/14766) Durable Storage データ複製を担当するノードがクラスタから離脱するのを防ぐため、`emqx ctl cluster leave` コマンドに保護機能を追加しました。

- [#14040](https://github.com/emqx/emqx/pull/14040) ノードリバランス時の内部 RPC 呼び出しにタイムアウトを追加。応答しないノードがある場合のハングを防止します。

- [#14892](https://github.com/emqx/emqx/pull/14892) クラスター負荷リバランスを強化:

  - core/replicant クラスターの負荷不均衡を修正。特定条件下で replicant からの全トランザクションが単一の core ノードに送られる問題を解消。
  - replicant ノードのリバランス用 CLI コマンドを追加:
    - `emqx_ctl cluster core rebalance plan`
    - `emqx_ctl cluster core rebalance status`
    - `emqx_ctl cluster core rebalance confirm`
    - `emqx_ctl cluster core rebalance abort`

- [#14907](https://github.com/emqx/emqx/pull/14907) ノード避難の安定性を改善。以前はデッドループに陥り手動介入が必要になることがありました。

#### データ統合

- [#14118](https://github.com/emqx/emqx/pull/14118) Mysql アクションで `ON DUPLICATE KEY UPDATE` をサポート。

  例:

  ```
  INSERT INTO t1 (a,b,c) VALUES (${id},${clientid},${qos}) ON DUPLICATE KEY UPDATE a=a;
  ```

  ただし、`ON DUPLICATE KEY UPDATE` 句内ではプレースホルダー `${var}` はサポートされません。

- [#14629](https://github.com/emqx/emqx/pull/14629) S3 および Azure Blob Storage アクションで [JSON Lines](https://jsonlines.org/) コンテナタイプをサポート。

- [#14642](https://github.com/emqx/emqx/pull/14642) ローカルディスクに JSON Lines 形式でイベントをログ出力する新しいコネクターおよびアクションタイプを追加。

- [#14996](https://github.com/emqx/emqx/pull/14996) RabbitMQ アクションでデフォルトエクスチェンジの使用をサポート。

- [#14901](https://github.com/emqx/emqx/pull/14901) Schema Registry に新しいスキーマタイプ `external_http` を追加。外部 HTTP サーバーでペイロードに対する任意の操作を行い、その結果をルールで利用可能にします。

- [#14722](https://github.com/emqx/emqx/pull/14722) MQTT コネクターに新オプション `connect_timeout` を追加。接続確立待機の最大秒数を制御し、低い値は接続問題のフィードバック時間短縮に寄与します。

- [#14615](https://github.com/emqx/emqx/pull/14615) `ehttpc` HTTP ドライバーを利用する統合で `max_inactive` パラメータの設定をサポート。

- [#14459](https://github.com/emqx/emqx/pull/14459) フォールバックアクションをサポート。

  メッセージがバッファオーバーフローや TTL 到達などで正常に処理されなかった場合にトリガーされます。すべてのデータ統合アクションで設定可能。

  Kafka アクションの設定例:

  ```
  actions.kafka_producer.my_action {
    fallback_actions = [
      {kind = reference, type = mqtt, name = mqtt_fallback_publisher},
      {kind = republish, args = {topic = "fallback/action/republish"}}
    ]
    # ...
  }
  ```

- [#14582](https://github.com/emqx/emqx/pull/14582) 内部 JSON 表現のシリアライズ前後の不要な前処理・後処理を回避。

#### 管理

- [#14845](https://github.com/emqx/emqx/pull/14845) ゲートウェイ設定やリスナー変更時に既存リスナーの不要な再起動を回避。

- [#14773](https://github.com/emqx/emqx/pull/14773) レートリミット機能を改善（ゾーンやリスナーに設定可能な `bytes_rate`、`messages_rate`、`max_conn_rate`）。

  - アルゴリズムを簡素化し、クライアントへのバックプレッシャーはかけずメッセージをドロップ。QoS1/QoS2 メッセージには適切な理由コードを返します。
  - ランタイムでの再設定を可能に。
  - ウィンドウサイズと各ウィンドウの制限を明示的に指定可能。例: `messages_rate = "300/5m"` は5分間に300メッセージ、`messages_rate = "10/10s"` は10秒間に10メッセージ。
  - バーストレートは `messages_burst`、`bytes_burst`、`max_conn_burst` オプションで指定可能。例: `messages_burst = 1000/h` は1時間あたり追加で1000メッセージを許容。

- [#14341](https://github.com/emqx/emqx/pull/14341) ネームスペース機能でネームスペースごとのセッション数制限をサポート。`client.authenticate` フックポイントの `quota_exceeded` エラー理由をサポート。

- [#14679](https://github.com/emqx/emqx/pull/14679) OnMessagePublish コールバックで `User-Property` パラメータを渡すために `exhook.proto` を v3 に更新。

- [#14963](https://github.com/emqx/emqx/pull/14963) プラグインにヘルスステータスを提供する `on_health_check/1` コールバックを追加。HTTP API と CLI でヘルスステータスを公開。

#### ダッシュボード

- [#14750](https://github.com/emqx/emqx/pull/14750) ダッシュボードの「共有サブスクリプション」表示の不具合を修正。古い値が表示される問題を解消。

- [#14638](https://github.com/emqx/emqx/pull/14638) ダッシュボードのデフォルトパスワード設定にファイルシークレット（例: `file://...`）を使用可能に。

- [#14255](https://github.com/emqx/emqx/pull/14255) ダッシュボードユーザーのパスワード有効期限機能を追加。

- [#15014](https://github.com/emqx/emqx/pull/15014) ダッシュボードのセキュリティ強化。複数回のログイン失敗後、一定期間ログインをブロック。試行回数とロック時間は設定可能。

- [#15132](https://github.com/emqx/emqx/pull/15132) SAML SSO 統合でのレスポンス Content-Type 修正。ACS からのレスポンスが誤って `application/xml` となっていた問題を `application/x-www-form-urlencoded` に修正し、一部 ID プロバイダーとの互換性を向上。

#### 可観測性

- [#14794](https://github.com/emqx/emqx/pull/14794) Log Trace の HTTP API インターフェースに `payload_limit` パラメータを追加。以前はペイロードが1024バイトを超えると切り捨てられていましたが、設定可能になりました。

- [#14876](https://github.com/emqx/emqx/pull/14876) ルールエンジンのエンドツーエンドトレーシングを強化。以下のトリガーを含みます:

  - クライアントがパブリッシュしたメッセージによるルールトリガー
  - クライアントイベントおよびアラートイベントによるルールトリガー
  - ソーストリガーのルール
  - ルールによって実行されるアクション

  制限事項: フォールバックアクションのトレーシングは未対応。

- [#14723](https://github.com/emqx/emqx/pull/14723) Prometheus Push Gateway の設定に `method` オプションを追加。デフォルトは `post` から `put` に変更。

  `put` メソッドは Pushgateway 内の同ジョブのメトリクスを置き換え、不要なメトリクスの残存を防ぎます。

- [#14636](https://github.com/emqx/emqx/pull/14636) `packets.publish.dropped` メトリクスを廃止し、より意味のある2つの新メトリクスに置き換え:

  - `messages.dropped.quota_exceeded`: クライアントが設定されたレート制限を超えた場合にトリガー（例: QoS 0 メッセージ数超過）
  - `messages.dropped.receive_maximum`: QoS 2 メッセージの受信最大数制限に達した場合にトリガー

- [#14540](https://github.com/emqx/emqx/pull/14540) 認証および認可のレイテンシ計測を設定可能に。メトリクスは Prometheus ヒストグラムとして公開。

- [#14264](https://github.com/emqx/emqx/pull/14264) crash_dump ファイルにタイムスタンプを追加し、次回クラッシュダンプで上書きされないように。

- [#15119](https://github.com/emqx/emqx/pull/15119) セッションレジストリテーブルサイズのハイウォーターマークメトリクスを追加。ダッシュボードの概要ページでピークセッション数を監視可能。

- [#15117](https://github.com/emqx/emqx/pull/15117) cinfo 認証式評価失敗の警告ログを簡潔化し、クラッシュ誤認を減らしました。

  旧ログ例:

  ```
  2025-04-25T13:15:59.993395+00:00 [warning] tag: AUTHN, clientid: mqttx_a50058aa, msg: authenticator_error, peername: 127.0.0.1:60842, 
  reason: {case_clause,{error,#{error => #{reason => var_unbound,var_name => <<"cert_common_name">>},
  cause => "clientinfo_auth_expression_evaluation_error"}}}, 
  stacktrace: [{emqx_authn_cinfo,do_check,2,[{file,"emqx_authn_cinfo.erl"},{line,94}]},{emqx_authn_cinfo,check,2,[{file,"emqx_authn_cinfo.erl"},{line,82}]},{emqx_authn_chains,authenticate_with_provider,2,...
  ```

  新ログ例:

  ```
  2025-04-25T15:46:50.748732+02:00 [warning] clientid: client1, 
  msg: clientinfo_auth_expression_evaluation_error, 
  peername: 127.0.0.1:53919, 
  reason: #{reason => var_unbound,var_name => <<"cert_common_name">>}
  ```

#### CLI

- [#14691](https://github.com/emqx/emqx/pull/14691) `emqx ctl data export` コマンドでエクスポートするデータのフィルタリングを可能に。`cluster.hocon` のルートキーやテーブルセットを指定可能で、`POST /data/export` と同様の動作。

#### 設定ファイル

- [#14647](https://github.com/emqx/emqx/pull/14647) `cluster.hocon` のバックアップを設定可能な間隔で作成。設定更新ごとにバックアップを作成するのではなく、複数の変更をまとめてバックアップし、バックアップ数を削減。

#### プラグインおよび拡張

- [#14957](https://github.com/emqx/emqx/pull/14957) プラグイン設定更新処理を強化:

  - プラグインの `on_config_changed` コールバックの戻り値を尊重し、停止中のプラグインでも設定変更時に適切に呼び出すように。
  - `on_config_changed` コールバックの結果を尊重する新しいプラグイン設定更新メソッドを導入。

#### ゲートウェイ

- [#14017](https://github.com/emqx/emqx/pull/14017) GB/T 32960 ゲートウェイでカスタマイズされた InfoReport データメッセージタイプの解析をサポート。

#### MQTT over QUIC

- [#14431](https://github.com/emqx/emqx/pull/14431) QUIC スタックを新しいもの（quicer 0.2.3）に切り替え:

  - msquic 2.3.8 + パッチ適用
  - リソース管理の強化
  - リスナーの動的設定変更対応準備

#### システムアップグレード

- [#14639](https://github.com/emqx/emqx/pull/14639) EMQX を Erlang/OTP 27 でリリース。

### バグ修正

#### コア MQTT 機能

- [#14707](https://github.com/emqx/emqx/pull/14707) `strict_mode` で QoS 2 かつ DUP フラグ付きの PUBLISH パケットが誤って無効と判定される問題を修正。

- [#14192](https://github.com/emqx/emqx/pull/14192) 認証/認可期限切れで切断されたクライアントが遺言メッセージを送信できない問題を修正。以前は期限切れ直後のため認可ルールを通過できなかった。

- [#14122](https://github.com/emqx/emqx/pull/14122) QoS 2 と QoS 1 の PUBLISH メッセージに対する `PUBACK` と `PUBREC`/`PUBCOMP` の処理を修正。誤ったパケット識別子を持つパケットを受け入れていた問題を修正し、該当クライアントは切断されます。

- [#15106](https://github.com/emqx/emqx/pull/15106) `GET api/v5/clients_v2` API で重複した `clientid` が返されるバグを修正。`chaninfo` イベントの誤復活によるクライアントデータ重複を防止。

- [#14906](https://github.com/emqx/emqx/pull/14906) Mria を 0.8.12.1 に更新し、予期しない終了シグナルによる警告を解消。

  ```
  2025-01-10T20:00:00+00:00 [warning] clientid: C1, msg: emqx_session_mem_unknown_message, message: {'EXIT',<0.123456.0>,normal}
  ```

- [#15084](https://github.com/emqx/emqx/pull/15084) クライアント属性 `zone` と `listener` を文字列関数の入力として利用可能に。以前は内部的にアトムであったため、`regex_match` 等で例外が発生していた。

#### インストール

- [#14624](https://github.com/emqx/emqx/pull/14624) macOS リリースパッケージの OpenSSL 動的リンク問題を修正。

  quicer アプリケーションがシステムインストールの OpenSSL に動的リンクしていたため、EMQX ZIP パッケージが起動しない問題を解消。macOS 用 OTP も動的リンクを無効化しているため、同様に無効化。

#### REST API

- [#14771](https://github.com/emqx/emqx/pull/14771) `GET /clients_v2` HTTP API が要求した制限を超える結果を返す問題を修正。

  注意: ローリングアップグレード中は全クライアントがリストされない場合があります。旧コアノードにリクエストを送ることで全クライアントを取得可能。

- [#14182](https://github.com/emqx/emqx/pull/14182) `POST /publish` HTTP API で遅延メッセージをパブリッシュした際に、202 レスポンスと理由コード 16（"no matching subscribers"）が返されていた問題を修正。メッセージID付きの 200 レスポンスを返すように。

#### MQTT Durable Sessions

- [#14674](https://github.com/emqx/emqx/pull/14674) EMQX Durable Storage が作成する RocksDB 情報ログファイルの数とサイズを制限。

- [#14498](https://github.com/emqx/emqx/pull/14498) Durable Sessions のパフォーマンス改善:

  - アイドル状態の Durable Sessions が CPU を消費しなくなりました。
  - QoS アップグレード機能の修正。サブスクライバーがサブスクリプションの QoS より高いメッセージを受け取らなくなりました。

- [#14933](https://github.com/emqx/emqx/pull/14933) DS Raft バックエンドの Durable Storage が、長期間クラスタから離脱したストレージサイトに割り当てられる稀なエッジケースを解消。

#### 認証および認可

- [#14777](https://github.com/emqx/emqx/pull/14777) JWT 認証の設定更新を修正。外部 JWKS エンドポイント構成で一部フィールドが正しく更新されなかった問題を修正。

- [#14556](https://github.com/emqx/emqx/pull/14556) ノード起動・シャットダウン時に発生する稀な誤認証を修正。

- [#15059](https://github.com/emqx/emqx/pull/15059) Redis 認証設定の無効値更新時の反応を修正。

  以前は認証器がクラッシュし認証適用が停止していたが、現在は適切なエラーを返し更新を拒否。

- [#14303](https://github.com/emqx/emqx/pull/14303) `scram:http` 認証の問題を修正。HTTP コネクターへのリクエストが不正で認証失敗していた。

#### クラスタリング

- [#14778](https://github.com/emqx/emqx/pull/14778) `data/certs` または `data/authz` ディレクトリに壊れたシンボリックリンクがある場合、別ノードが参加に失敗する問題を修正。

- [#14936](https://github.com/emqx/emqx/pull/14936) 稀にグローバルルーティングテーブルが長期間クラスタから離脱したノードの情報を保持し続ける問題を修正。

- [#14977](https://github.com/emqx/emqx/pull/14977) `emqx ctl conf cluster_sync status` コマンドのノード表示順序を修正。新旧設定のノード名が逆に表示されていた。

#### クラスターリンク

- [#15067](https://github.com/emqx/emqx/pull/15067) クラスターリンクのルートレプリケーションに関する複数の問題を修正。

  - クラスターリンクの誤設定時にレプリケーションが不規則な再接続ループに陥る問題。
  - 存在しない MQTT クライアント接続のクローズ試行時にクラッシュする問題。
  - 共有サブスクリプションがルーティングテーブルに存在する場合のレプリケーションブートストラップのクラッシュ。

#### ルールエンジン

- [#14849](https://github.com/emqx/emqx/pull/14849) `POST /rule_test` レスポンスから内部フィールド `event_type` を削除。実際のイベントには存在しないため混乱を避けるため。

- [#15056](https://github.com/emqx/emqx/pull/15056) ペイロードが JSON 配列の MQTT メッセージで、`foreach` 文内で明示的にデコードする必要がなくなりました。

#### Smart Data Hub

- [#14988](https://github.com/emqx/emqx/pull/14988) バックアップ復元時にスキーマレジストリより前にスキーマ検証やメッセージ変換設定がインポートされてしまい検証エラーになる問題を修正。

#### データ統合

- [#14716](https://github.com/emqx/emqx/pull/14716) アクション/ソースの追加・削除を設定変更外で非同期に実行。タイムアウトによる設定とリソース状態の乖離を防止。

- [#14519](https://github.com/emqx/emqx/pull/14519) ノード再起動時に一部ソースのメトリクスが欠落し警告ログが発生する問題を修正。

- [#14992](https://github.com/emqx/emqx/pull/14992) コネクターの接続性テスト時の稀なリソースリークを修正。

- [#15000](https://github.com/emqx/emqx/pull/15000) CLI または HTTP API での設定読み込み時にコネクター、アクション、ソースが不安定になる問題を修正。

- [#15010](https://github.com/emqx/emqx/pull/15010) コネクター無効化にかかる時間を短縮。なお、アクションを持つコネクターや不健康な場合は依然として時間がかかることがあります。

- [#15051](https://github.com/emqx/emqx/pull/15051) TDengine コネクターのパラメータ検証を追加し、ドライバーバージョンを更新してエラーメッセージを明確化。

- [#15012](https://github.com/emqx/emqx/pull/15012) RabbitMQ アクションの `publish_confirmation_timeout` パラメータが1000倍されていた問題を修正。

- [#14989](https://github.com/emqx/emqx/pull/14989) Kinesis コネクションおよびアクションの起動時およびヘルスチェック時の API 呼び出し回数を削減。

- [#14767](https://github.com/emqx/emqx/pull/14767) Kafka プロデューサーがパーティション数減少によるトピック再作成をスムーズに処理。以前は失われたパーティションのプロデューサーが残り大量のエラーログを出力していた。

- [#14121](https://github.com/emqx/emqx/pull/14121) Kafka コンシューマーコネクターの `health_check_topic` 設定を廃止。実際には使用されていなかった。

- [#15116](https://github.com/emqx/emqx/pull/15116) Kafka コネクターのヘルスチェックで `topic_authorization_failed` を有効な応答として許容。ACL 制御された Kafka サービスとの互換性を向上。

#### 管理

- [#14931](https://github.com/emqx/emqx/pull/14931) `mqtt.max_qos_allowed` 設定を SUBACK パケットの付与 QoS と理由コードに使用。以前は理由コードがサブスクライブ QoS に固定されていた。

- [#14975](https://github.com/emqx/emqx/pull/14975) 一部 TLS リスナーオプションのオンザフライ更新が無効になる問題を修正。無効化・有効化のサイクルが不要に。

- [#15037](https://github.com/emqx/emqx/pull/15037) 動的作成ゾーンのレートリミットが適用されない問題を修正。

#### 設定ファイル

- [#15087](https://github.com/emqx/emqx/pull/15087) `hocon` ライブラリの問題を修正。文字列の1行フィールドが末尾に単独のバックスラッシュを含む場合に設定ファイルの解析に失敗していた。

#### プラグインおよび拡張

- [#15073](https://github.com/emqx/emqx/pull/15073) `exhook` 設定のサーバー URL バリデータを追加。無効な URL はエラーとなり保存を防止。インポート時の問題を回避。

- [#14774](https://github.com/emqx/emqx/pull/14774) プラグイン関連の問題を修正。設定ファイルが存在しない状態でプラグイン起動時にクラスターノードからプラグイン設定ファイルを取得できない問題を解決。

- [#14826](https://github.com/emqx/emqx/pull/14826) Exhook サーバーの "IGNORE" 応答が効果を持たない問題を修正。

- [#15018](https://github.com/emqx/emqx/pull/15018) CLI から無効な `exhook` 設定をインポートしようとした際の `badarg` クラッシュを修正。

- [#15108](https://github.com/emqx/emqx/pull/15108) ExHook に組み込みの gRPC ヘルスチェック機構を追加。外部フックサーバーの実際の可用性を正確に反映。長時間停止後のステータスの陳腐化問題を解決。設定で自動再接続をサポート。

#### MQTT over QUIC

- [#14775](https://github.com/emqx/emqx/pull/14775) QUIC リスナーの設定リロード後にゾーン設定が適用されない問題を修正。

## 5.8.6

*リリース日: 2025-03-25*

EMQX 5.8.6 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

- [#14869](https://github.com/emqx/emqx/pull/14869) `$events/client_disconnected` イベントペイロードに `connected_at` タイムスタンプフィールドを追加。切断されたクライアントの元の接続セッション時間を追跡可能にし、古い切断イベントが新しい接続状態を上書きするのを防止。

- [#14855](https://github.com/emqx/emqx/pull/14855) JT/T 808 ゲートウェイに `ignore_unsupported_frames` 設定を追加。ゲートウェイが解析できないメッセージ送信時の切断を防止。

- [#14858](https://github.com/emqx/emqx/pull/14858) EMQX が TDengine Cloud とデータ統合をサポート。TDengine Cloud の認証に必要な追加トークンパラメータを TDengine コネクターで対応。

### バグ修正

#### コア MQTT 機能

- [#14815](https://github.com/emqx/emqx/pull/14815) QoS 2 メッセージのパケットID解放を修正。クライアントが最大保留 QoS 2 メッセージ数の PUBREL 送信に失敗し切断した場合、Max Awaiting PUBREL Timeout 超過後もパケットIDが解放されなかった問題を修正。

#### インストールおよびデプロイメント

- [#14797](https://github.com/emqx/emqx/pull/14797) macOS リリースパッケージの OpenSSL 動的リンクによる起動問題を修正（バックポート #14624）。

#### 認証

- [#14847](https://github.com/emqx/emqx/pull/14847) ワイルドカード HTTPS エンドポイントの JWKS 認証失敗を修正。

- [#14786](https://github.com/emqx/emqx/pull/14786) 外部 JWKS エンドポイント使用時の JWT 認証設定更新を修正。

#### REST API

- [#14834](https://github.com/emqx/emqx/pull/14834) バックアップファイルダウンロード時の `Content-Type` ヘッダー誤りを修正。`application/json` から `application/octet-stream` に修正。

- [#14863](https://github.com/emqx/emqx/pull/14863) `cluster/:node/invite_async` REST API のダウンノードをコーディネーターに使用する問題を修正。

#### ルールエンジン

- [#14824](https://github.com/emqx/emqx/pull/14824) SQL ルールテスターでアラームイベントの `details` キー処理により HTTP 500 エラーが発生する問題を修正。

#### データ統合

- [#14796](https://github.com/emqx/emqx/pull/14796) Pulsar プロデューサーのインフライト状態リークを修正。パフォーマンス改善も含む。

- [#14902](https://github.com/emqx/emqx/pull/14902) SQL Server アクションの接続失敗時のエラー処理を改善。`IMC0x` SQLSTATE エラーを回復可能とし、メッセージ損失を防止。

#### 可観測性

- [#14800](https://github.com/emqx/emqx/pull/14800) `dropped_qos0_msg` 警告ログのスロットリングを追加。

- [#14793](https://github.com/emqx/emqx/pull/14793) MQTT 接続の `protocol_error` トレースログを追加。接続中の不正な MQTT パケット送信時の詳細ログを強化。

- [#14813](https://github.com/emqx/emqx/pull/14813) WebSocket クライアントへの送信メッセージがエンドツーエンドトレーシングに含まれない問題を修正。

- [#14880](https://github.com/emqx/emqx/pull/14880) SQL Server コネクターのヘルスチェック失敗ログを詳細化。

#### プラグイン

- [#14802](https://github.com/emqx/emqx/pull/14802) プラグイン用新 CLI コマンドを追加:

  ```bash
   emqx ctl plugins allow NAME-VSN
  ```

  HTTP API やダッシュボード経由でプラグインをインストールする前に、このコマンドで明示的に許可する必要があります。

#### ゲートウェイ

- [#14756](https://github.com/emqx/emqx/pull/14756) 匿名認証有効時の JT/T 808 ゲートウェイ登録応答にデフォルト認証コード `anonymous` を付与。空の認証コードを解析できないクライアント問題を回避。

## 5.8.5

*リリース日: 2025-02-25*

EMQX 5.8.5 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#14454](https://github.com/emqx/emqx/pull/14454) Retainer に `max_publish_rate` オプションを追加。ノードごとの保持メッセージの最大パブリッシュレートを制御。超過メッセージは配信されるが保持されない。

- [#14456](https://github.com/emqx/emqx/pull/14456) Linux 向けに SYN フラッド攻撃からリスナーを保護する簡易ファイアウォールスクリプト `bin/emqx_fw` を追加。

- [#14496](https://github.com/emqx/emqx/pull/14496) `POST /data/export` API の `root_keys` パラメータに対する追加検証を実装。無効なルートキーはエラーを返す。

#### アクセス制御

- [#14494](https://github.com/emqx/emqx/pull/14494) MongoDB 認可で複雑なクエリをサポート。

  - セレクターフィルターでトップレベルの `$orderby` 演算子を追加。
  - ページネーション用に `skip` と `limit` オプションを追加。

- [#14570](https://github.com/emqx/emqx/pull/14570) HTTP 認証・認可設定で HTTP ヘッダーにプレースホルダーを使用可能に。

- [#14665](https://github.com/emqx/emqx/pull/14665) ACL ルールの事前条件にクライアント属性をサポート。属性に基づく細かなアクセス制御が可能に。

  例: `"type"="internal"` のクライアントに全トピックのパブリッシュ/サブスクライブを許可:

  `{allow, {client_attr, "type", "internal"}, all, ["#"]}.`

  `"type"` 属性が `"external-"` で始まるクライアントのパブリッシュを拒否:

  `{deny, {client_attr, "type", {re, "external-.*"}}, publish, ["#"]}.`

#### ルールエンジン

- [#14627](https://github.com/emqx/emqx/pull/14627) システムアラームの有効化・無効化時にトリガーされるイベント `$events/sys/alarm_activated` と `$events/sys/alarm_deactivated` を追加。

#### データ統合

- [#14404](https://github.com/emqx/emqx/pull/14404) MQTT コネクターで静的クライアントID指定をサポート。

- [#14450](https://github.com/emqx/emqx/pull/14450) MQTT ソースに `no-local` フラグ設定を追加。クライアント自身がパブリッシュしたメッセージを受信しないように設定可能。

- [#14507](https://github.com/emqx/emqx/pull/14507) 軽量な HTTP API `GET /actions_summary` と `GET /sources_summary` を追加。アクション・ソースの概要を高速かつリソース効率よく取得可能。

- [#14524](https://github.com/emqx/emqx/pull/14524) Couchbase コネクターのヘルスチェック失敗時の詳細エラーメッセージを追加。

- [#14572](https://github.com/emqx/emqx/pull/14572) Kafka、Azure Event Hub、Confluent プロデューサーの `parameters.buffer.memory_overload_protection` のデフォルトを `true` に変更。長時間 Kafka サービスがダウンしている場合のメモリ過負荷を防止。

- [#14626](https://github.com/emqx/emqx/pull/14626) Kafka と Pulsar アクションのパーティションごとのバッファサイズのデフォルトを 256 MB に変更。

#### 可観測性

- [#14437](https://github.com/emqx/emqx/pull/14437) Prometheus に新しいゲージ `emqx_vm_mnesia_tm_mailbox_size` と `emqx_vm_broker_pool_max_mailbox_size` を追加。EMQX 内部プロセスのメールボックスサイズを監視し、高水準を超えるとアラームを発生。

- [#14645](https://github.com/emqx/emqx/pull/14645) CRL 初回取得時のデバッグ・警告ログを追加。

- [#14656](https://github.com/emqx/emqx/pull/14656) Prometheus プッシュでより多くのメトリクスをサポートし、クラスタ名をジョブラベルの変数名として利用可能に。

- [#14479](https://github.com/emqx/emqx/pull/14479) OpenTelemetry 統合のエンドツーエンドトレーシングで認証・認可バックエンドの詳細トレースを追加。

- [#14644](https://github.com/emqx/emqx/pull/14644) OpenTelemetry 統合のエンドツーエンドトレーシングでクライアント提供の traceparent をサポート。

- [#14657](https://github.com/emqx/emqx/pull/14657) エンドツーエンドトレーシングのホワイトリストエントリを `broker.publish` スパンにも適用。メッセージ配信スパンのトレースが可能に。

- [#14589](https://github.com/emqx/emqx/pull/14589) および [#14689](https://github.com/emqx/emqx/pull/14689) メッセージ変換・検証のログメッセージをスロットリング。

#### MQTT over QUIC

- [#14583](https://github.com/emqx/emqx/pull/14583) QUIC リスナーで TLS 秘密鍵を `SSLKEYLOGFILE` 環境変数にダンプ可能に。Wireshark 等で QUIC トラフィックの復号が可能。

- [#14597](https://github.com/emqx/emqx/pull/14597) 接続終了時の非同期ストリーム読み取り中止を実装。セッションの「奪取」「破棄」「キック」時のブロッキング遅延を軽減。

### バグ修正

#### コア MQTT 機能

- [#14405](https://github.com/emqx/emqx/pull/14405) `mqtt.max_packet_size` の `256MB` 設定をプロトコル上限の `268435455` バイトに変換。以前は1バイト超過していたため、互換性維持のために自動変換。

- [#14508](https://github.com/emqx/emqx/pull/14508) 多数クライアントの再接続時の EMQX パフォーマンスを改善。

- [#14608](https://github.com/emqx/emqx/pull/14608) MQTT セッションメッセージキューの FIFO セマンティクスを強制。キュー満杯時は最古メッセージから削除。

- [#14609](https://github.com/emqx/emqx/pull/14609) オーバーロード保護の高メモリ閾値を `sysmon.os.sysmem_high_watermark` に基づいて動的に更新。

- [#14654](https://github.com/emqx/emqx/pull/14654) 最大セッション制限に達しても、以前のセッションが有効な限りクライアントは再接続可能に。

- [#14588](https://github.com/emqx/emqx/pull/14588) コンテナ環境（AWS EKS 等）でのメモリ使用量報告の精度を向上。

#### ライセンス

- [#14568](https://github.com/emqx/emqx/pull/14568) 切断済みセッションもライセンス制限に含めるよう強化。セッション保持有効時に制限を超えると新規オフライン保持セッションを拒否。

#### 認証

- [#14585](https://github.com/emqx/emqx/pull/14585) パスワードハッシュ比較を大文字小文字区別しないよう修正。外部システムとの連携時の認証失敗を防止。

#### ゲートウェイ

- [#14484](https://github.com/emqx/emqx/pull/14484) Exproto ゲートウェイでサーバーエンドポイントにホスト名を使用できない問題を修正。

- [#14489](https://github.com/emqx/emqx/pull/14489) クラスタ内でゲートウェイが有効化されていないノードの `api/v5/gateways` へのアクセスで 500 エラーになる問題を修正。適切なレスポンスを返すように。

- [#14501](https://github.com/emqx/emqx/pull/14501) ゲートウェイクライアントクエリ HTTP API が常に `keepalive=0` を返す問題を修正。正しいキープアライブ値を返し、ゲートウェイが設定されたアイドルタイムアウトを遵守。

- [#14503](https://github.com/emqx/emqx/pull/14503) ゲートウェイにリスナーが存在しない場合、404 エラーではなく空リストを返すように修正。

- [#14511](https://github.com/emqx/emqx/pull/14511) クライアント認証失敗時の Stomp ゲートウェイの不要なログ出力を削減。

- [#14653](https://github.com/emqx/emqx/pull/14653) Stomp ゲートウェイのキープアライブ動作を修正。ハートビートパケットがチェックタイマー直後に届いても接続が維持されるよう許容。

#### ルールエンジン

- [#14622](https://github.com/emqx/emqx/pull/14622) 32 件以上の変換・検証がトピックにマッチした場合のメッセージ変換・スキーマ検証の実行順序問題を修正。

#### データ統合

- [#14518](https://github.com/emqx/emqx/pull/14518) 設定読み込み時にコネクターを非同期起動するよう修正。起動遅延による設定インポートタイムアウトを防止。

- [#14545](https://github.com/emqx/emqx/pull/14545) RabbitMQ アクションが RabbitMQ の応答なし状態で削除できない問題を修正。

- [#14550](https://github.com/emqx/emqx/pull/14550) MQTT コネクターの接続プール内クライアントが一部切断時に自動再接続できない問題を修正。

- [#14555](https://github.com/emqx/emqx/pull/14555) MQTT ソースで共有トピックのサブスクライブ解除が正しく行われない問題を修正。

- [#14650](https://github.com/emqx/emqx/pull/14650) Redis クラスターモードのマスター・スレーブフェイルオーバー後に `no_connection` エラーから復旧できない問題を修正（`eredis_cluster` ライブラリを 0.8.8 に更新）。

- [#14671](https://github.com/emqx/emqx/pull/14671) MQTT アクションで TCP 接続切断時のレースコンディションによる送信失敗・再試行問題を修正。

- [#14695](https://github.com/emqx/emqx/pull/14695) コネクター更新時の HTTP API エラーメッセージを改善。

- [#14697](https://github.com/emqx/emqx/pull/14697) 同名のソースとアクションが同じコネクターを使用し、ルール依存関係がある場合にアクションやソースを削除できない問題を修正。

- [#14427](https://github.com/emqx/emqx/pull/14427) GCP PubSub プロデューサーコネクターのヘルスチェック失敗時のエラーメッセージを詳細化。

- [#14451](https://github.com/emqx/emqx/pull/14451) PostgreSQL アクションでタイムスタンプ列の無効入力による大規模クラッシュレポートを修正。エラーメッセージを簡潔化。

- [#14552](https://github.com/emqx/emqx/pull/14552) Kafka プロデューサーでバッファオーバーフロー後の `unexpected_id` クラッシュを修正（EMQX Enterprise 5.8.1 で発生）。

- [#14560](https://github.com/emqx/emqx/pull/14560) 複雑な SQL テンプレートによる Oracle アクションのヘルスチェック失敗を修正。

- [#14563](https://github.com/emqx/emqx/pull/14563) Kafka と Pulsar プロデューサーでバッファオーバーフローやリクエスト期限切れによるメッセージドロップ時にルールの失敗カウンターが正しく増加しない問題を修正。

- [#14567](https://github.com/emqx/emqx/pull/14567) S3 コネクター無効化・削除後に HTTP プールが停止しない問題を修正。

- [#14631](https://github.com/emqx/emqx/pull/14631) Kafka、Azure Event Hub、Confluent プロデューサーアクションのメモリオーバーロード保護を強化。高水準に達するとバッファデータを積極的に破棄。

- [#14705](https://github.com/emqx/emqx/pull/14705) Kafka コネクターの接続性チェックを改善。認証が必要だが認証情報やヘルスチェックトピックが未設定の場合に誤って正常判定していた問題を修正。

#### クラスタリング

- [#14536](https://github.com/emqx/emqx/pull/14536) クラスタ管理操作の稀なレースコンディションを修正。`mria:join/1` 操作のグローバルロックを強化し、同時参加による競合を防止。

- [#14548](https://github.com/emqx/emqx/pull/14548) ノード再起動時に新規ノードがクラスタに参加中だとクラッシュする問題を修正。`** FATAL ** Failed to merge schema: {aborted,function_clause}` エラーを解消。

- [#14662](https://github.com/emqx/emqx/pull/14662) 内部データベースを消去されたコアノード群に再参加した replicant ノードが一部 RPC 操作に参加できない問題を修正。

#### 管理

- [#14543](https://github.com/emqx/emqx/pull/14543) WS、WSS、ゲートウェイリスナー経由のクライアント接続時に一部 ExHooks がクラッシュする内部互換性問題を修正。

#### 可観測性

- [#14544](https://github.com/emqx/emqx/pull/14544) TCP/TLS リスナー無効化時に Prometheus メトリクス収集プロセスがクラッシュする問題を修正。

- [#14466](https://github.com/emqx/emqx/pull/14466) トレースイベントスイッチが `100%` に設定されている場合に効果がない問題を修正。

- [#14666](https://github.com/emqx/emqx/pull/14666) `opentelemetry.traces.max_queue_size` 設定を REST API とダッシュボードから設定可能に。以前は設定ファイルや環境変数のみ。

## 5.8.4

*リリース日: 2024-12-26*

EMQX 5.8.4 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#13739](https://github.com/emqx/emqx/pull/13739) クラスター全体のモニタリング（統計）データをクリアする機能を追加。`api/v5/monitor` エンドポイントに `DELETE` リクエストを送信可能。

- [#14247](https://github.com/emqx/emqx/pull/14247) クライアントメタデータに `tns` 属性があればログに記録。ただしクライアントIDに既に `tns` プレフィックスがあれば重複記録を回避。

- [#14353](https://github.com/emqx/emqx/pull/14353) セッションリバランスおよび避難処理の堅牢性を強化。クラスタリングエラー時にデッドループに陥る問題を修正。

#### ルールエンジン

- [#14369](https://github.com/emqx/emqx/pull/14369) ルールエンジンにサイズ関連関数を追加:

  - `is_empty`: マップまたは配列が空なら `true` を返す。
  - `map_size`: マップのサイズを返す。

#### データ統合

- [#14110](https://github.com/emqx/emqx/pull/14110) Pulsar ドライバーのメトリクス報告を追加。キューイングメッセージ数、インフライトメッセージ数、ドロップメッセージ数を報告。

- [#14410](https://github.com/emqx/emqx/pull/14410) EMQX が [Aliyun Tablestore](https://cn.aliyun.com/product/ots) とのデータ統合をサポート。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB データ統合の実装をリファクタリングし、バッチ性能を向上。

#### 設定ファイル

- [#14269](https://github.com/emqx/emqx/pull/14269) 新たに `etc/base.hocon` 設定ファイルを追加。設定管理と明確化を強化。

  従来、`emqx.conf` は最上位の設定層であり、UI/API/CLI で変更可能だがノード再起動で上書きされる問題があった。

  新たな設定優先順位（上位から）:

  1. 環境変数
  2. `etc/emqx.conf`
  3. `data/configs/cluster.hocon`
  4. `etc/base.hocon`

  `etc/base.hocon` は基盤設定層として機能し、ノード起動後も変更可能だが一貫性と適切な上書きを保証。

#### 可観測性

- [#14360](https://github.com/emqx/emqx/pull/14360) Prometheus メトリクスにリスナーシャットダウン数を追加。シャットダウン理由別にカウント。例:

  ```
  emqx_client_disconnected_reason{node="emqx@127.0.0.1",reason="takenover"} 1 
  emqx_client_disconnected_reason{node="emqx@127.0.0.1",reason="kicked"} 1
  ```

  現状は TCP と TLS リスナーのみ対応。

### バグ修正

#### コア MQTT 機能

- [#14248](https://github.com/emqx/emqx/pull/14248) クラスタノード間の断続的な接続問題を修正。クラスタ全体のルーティングテーブル状態の部分的消失を防止。

- [#14272](https://github.com/emqx/emqx/pull/14272) CLI 経由で読み込んだ `auto_subscribe` 設定が成功メッセージを表示するが反映されない問題を修正。

- [#14424](https://github.com/emqx/emqx/pull/14424) 排他サブスクリプションに関するメンバーシップメッセージが `unexpected_info` 警告として誤ってログ出力される問題を修正。

#### REST API

- [#14317](https://github.com/emqx/emqx/pull/14317) ページネーション計算時に HTTP API が空ページを返す問題を修正。

#### データ統合

- [#14318](https://github.com/emqx/emqx/pull/14318) HTTP コネクターの状態初期化問題を修正。HTTP アクションが再起動中にトラフィックを処理するとクラッシュしていた。

- [#14319](https://github.com/emqx/emqx/pull/14319) リソース管理の内部状態機械をリファクタリングし、複数のレースコンディションバグを解消。

- [#14429](https://github.com/emqx/emqx/pull/14429) コネクターが無効化されている場合のルールアクションメトリクス処理を修正。失敗カウンターが2回増加していた問題を修正。

- [#14291](https://github.com/emqx/emqx/pull/14291) Pulsar プロデューサードライバーをアップグレードし、`Redirect` タイプの `LookupType` 応答処理を修正。

- [#14345](https://github.com/emqx/emqx/pull/14345) 既存の有効な Kafka コンシューマーソースを持つノードの起動/再起動時のクラッシュログを修正。

- [#14362](https://github.com/emqx/emqx/pull/14362) MySQL ドライバーを更新し、クラッシュおよびエラーからの回復を改善。

- [#14375](https://github.com/emqx/emqx/pull/14375) Kafka コンシューマーソースのドライラン結果に詳細なエラー情報を追加。

#### CLI

- [#14357](https://github.com/emqx/emqx/pull/14357) `bin/emqx help` コマンドの表示問題を修正。

#### 設定ファイル

- [#14371](https://github.com/emqx/emqx/pull/14371) `undefined` や `null` がリテラル文字列 `"undefined"` や `"null"` としてレンダリングされる問題を修正。空文字列として表示。

- [#14376](https://github.com/emqx/emqx/pull/14376) 存在しないログファイルディレクトリの設定インポート時にデフォルトログディレクトリにフォールバック。

#### 可観測性

- [#14267](https://github.com/emqx/emqx/pull/14267) シークレット文字列がファイルパス（例: `file:///path/to/the/secret`）の場合、ログや HTTP レスポンスでのマスキングを回避。

- 永続ライセンスの `emqx_license_expiry_at` Prometheus 値取得時の `function_clause` エラーを解消。

#### ゲートウェイ

- [#14445](https://github.com/emqx/emqx/pull/14445) JT/T 808 クライアントが無効な下流制御メッセージを受信した際の接続クラッシュを修正。

## 5.8.3

*リリース日: 2024-12-05*

EMQX 5.8.3 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#14219](https://github.com/emqx/emqx/pull/14219) 接続レートリミッターを強化し、システムのレジリエンスを向上。

  - 高接続率時のシステム安定性と応答性を改善。従来は接続率制限超過時にリスナーが新規接続を無視し、短期間に多数の接続/再接続があると回復不能状態になる可能性があった。現在は保留中の接続を受け入れ、制限超過時に即座に切断し、リソース負荷を軽減。

  - 新リスナーオプション `nolinger` を追加。`true` 設定時はソケットクローズ時に即座に TCP-RST を送信し、SYN フラッド攻撃を軽減。

  - MQTT リスナーの `max_connection` 設定をシステム制限（OS の ulimit や `node.process_limit`）で上限設定。`infinity` やシステム上限超過値は自動調整。

  - SSL リスナーの `ssl_options` を変更前に検証。無効な SSL オプション（非対応 TLS バージョン等）を受け入れず、ノード起動失敗や Dashboard/API からの設定適用失敗時に 400 エラーを返す。

#### 認証および認可

- [#14147](https://github.com/emqx/emqx/pull/14147) LDAP 拡張マッチフィルターで `memberOf` 構文をサポート。例: `(&(objectClass=class)(memberOf:1.2.840.113556.1.4.1941:=CN=GroupName,OU=emqx,DC=WL,DC=com))`

#### データ統合

- [#14166](https://github.com/emqx/emqx/pull/14166) RabbitMQ プロデューサーで `exchange` と `routing_key` をテンプレート値として設定可能に。ペイロードに基づく動的ルーティングを実現。

- [#14176](https://github.com/emqx/emqx/pull/14176) ルールエンジンで RabbitMQ ソースアクションのメタデータ（`queue`、`exchange`、`routing_key`）を利用可能に。

- [#14218](https://github.com/emqx/emqx/pull/14218) S3 互換ストレージで vhost スタイルのバケットアクセスとリダイレクト処理を改善。S3 ブリッジとファイル転送バックエンドで利用可能。

#### 設定

- [#14195](https://github.com/emqx/emqx/pull/14195) クライアントIDオーバーライドをサポート。

#### MQTT over QUIC

- [#14283](https://github.com/emqx/emqx/pull/14283) QUIC トランスポートを改善し、`quicer` を 0.1.9 にアップグレード。

### バグ修正

#### コア MQTT 機能

- [#14201](https://github.com/emqx/emqx/pull/14201) WebSocket 接続がレートリミットに達した際の `check_gc` 警告を抑制。

- [#14215](https://github.com/emqx/emqx/pull/14215) Retainer が無効時に REST/CLI から呼び出すと例外が発生する問題を修正。

- [#14223](https://github.com/emqx/emqx/pull/14223) WebSocket のクローズ理由をアトムで返すよう修正し、クラッシュを防止。

- [#14260](https://github.com/emqx/emqx/pull/14260) CONNECT パケットがアイドルタイムアウト前に完全受信されない場合の稀なレースコンディションによる接続プロセスクラッシュを修正。

- [#14268](https://github.com/emqx/emqx/pull/14268) 同上の WebSocket 接続プロセスクラッシュを修正。

- [#14266](https://github.com/emqx/emqx/pull/14266) `emqtt` を 1.13.0 から 1.13.5 にアップデート。

#### Durable Sessions

- [#14160](https://github.com/emqx/emqx/pull/14160) MQTT 仕様に準拠し、Durable Session のサブスクリプションで `$` で始まるトピックのマッチングを適切に処理。

- [#14229](https://github.com/emqx/emqx/pull/14229) Durable Storage の Raft/RocksDB バックエンドで内部データベースの整合性とレプリカ収束に関わる問題を修正。

- [#14298](https://github.com/emqx/emqx/pull/14298) DS Raft/RocksDB バックエンドの一時的なリモートシャード障害に対するフォールトトレランスを強化。

#### REST API

- [#14117](https://github.com/emqx/emqx/pull/14117) REST API ドキュメントの `Users` エンドポイントで誤って `Basic` 認証をサポートと記載されていた問題を修正。

#### 認証

- [#14314](https://github.com/emqx/emqx/pull/14314) `scram:http` 認証の不具合を修正。

- [#14305](https://github.com/emqx/emqx/pull/14305) NIST Secure Hash Standard 非準拠のハッシュアルゴリズム（MD4、MD5、RIPEMD-160）を認証から削除。

#### ルールエンジン

- [#14217](https://github.com/emqx/emqx/pull/14217) スキーマレジストリエンドポイントの例設定の誤りを修正。

#### データ統合

- [#14172](https://github.com/emqx/emqx/pull/14172) HTTP API でコネクターの接続テストがタイムアウトした場合にリソースが残る問題を修正。

- [#14178](https://github.com/emqx/emqx/pull/14178) クラスタ内の異なるノードでルールを同時削除すると設定同期が停止する問題を修正。

- [#14226](https://github.com/emqx/emqx/pull/14226) 高負荷時にリソースメトリクスを失い再起動が必要になる問題を緩和。リソース再起動時にメトリクス再生成を試行。ホットパスメトリクスの警告ログをスロットリング。

- [#14265](https://github.com/emqx/emqx/pull/14265) MQTT ソースアクションがサブスクライブに失敗した場合のコネクター停止時の `badkey` エラーを修正。

- [#14296](https://github.com/emqx/emqx/pull/14296) 遅延起動する `ecpool_worker` による `ecpool_sup` ブロックを防止。

- [#14126](https://github.com/emqx/emqx/pull/14126) Oracle 統合のプリペアドステートメント問題を修正。無効なステートメントが古いバージョンに戻る問題を解消。

- [#14181](https://github.com/emqx/emqx/pull/14181) Kafka と Pulsar プロデューサーが破損した COMMIT ファイルに対して耐性を向上。クラッシュを回避。

#### 設定

- [#14180](https://github.com/emqx/emqx/pull/14180) variform 式で `undefined` または `null` にバインドされた変数が `'undefined'` と返される問題を修正。空文字列を返すように。

- [#14289](https://github.com/emqx/emqx/pull/14289) 異なる環境間での設定インポート時にログファイルパスの環境変数展開によるクラッシュを防止。古い絶対パスは新環境で存在しなければ環境変数に変換。

- [#14313](https://github.com/emqx/emqx/pull/14313) レプリカノードで REST API ブートストラップ API キーファイルを読み込む際のハングを修正。コアノードのみで読み込み。

#### ゲートウェイ

- [#14276](https://github.com/emqx/emqx/pull/14276) JT/T808 メッセージ解析失敗時のエラーログを詳細化。

#### 拡張

- [#14243](https://github.com/emqx/emqx/pull/14243) 一部ゲートウェイで `client.connect` フックがトリガーされない問題を修正。

#### MQTT over QUIC

- [#14258](https://github.com/emqx/emqx/pull/14258) QUIC 接続のシャットダウンタイムアウトを短縮。以前は 5 秒で、クライアントが応答しない場合に警告ログやダッシュボードのタイムアウトを引き起こしていた。現在はキック時 1 秒、その他 3 秒に短縮。

## 5.8.2

*リリース日: 2024-11-12*

EMQX 5.8.2 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#14059](https://github.com/emqx/emqx/pull/14059) Retainer の保持メッセージの有効期限間隔上限を設定可能に。ストレージ不足時に早期ガベージコレクションを促進。

- [#14072](https://github.com/emqx/emqx/pull/14072) 仮想マシンの表示可能文字範囲を Unicode に更新。バイナリデータの可読性が向上。

#### MQTT Durable Sessions

- [#14130](https://github.com/emqx/emqx/pull/14130) アイドル状態の Durable Sessions の CPU 使用率を削減。ストリーム検出をイベント駆動化し、遅延通知を短縮。

#### REST API

- [#13889](https://github.com/emqx/emqx/pull/13889) `/api/v5/monitor_current` と `/api/v5/metrics` API のパフォーマンスを向上。クラスタノードへのクエリを並列化し、レイテンシを低減。`/api/v5/monitor_current` に `node` パラメータを追加し、特定ノードのみのデータ取得が可能に。

#### EMQX クラスタリング

- [#13903](https://github.com/emqx/emqx/pull/13903) replicant ノードが同一リリースバージョンのコアノードを見つけられない場合のログを追加。

#### セキュリティ

- [#13923](https://github.com/emqx/emqx/pull/13923) 認証・認可・マウントポイントテンプレートで `zone` をサポート。`${zone}` プレースホルダーを直接使用可能に。

- [#14102](https://github.com/emqx/emqx/pull/14102) 秘密ファイルからの SSL プライベートキーパスフレーズをサポート。

#### データ統合

- [#14065](https://github.com/emqx/emqx/pull/14065) データ統合のキューイングバイト数メトリクスを追加。Pulsar プロデューサーアクションは未対応。

- [#14044](https://github.com/emqx/emqx/pull/14044) IoTDB Thrift ドライバーで複数アドレスのサポートを追加。接続失敗時に次のアドレスにフォールバック。

- [#14048](https://github.com/emqx/emqx/pull/14048) Kafka/Confluent/Azure Event Hub プロデューサーアクションの不明トピック検出時の不健康判定を廃止。メッセージをキューイング継続。

- [#14079](https://github.com/emqx/emqx/pull/14079) Kafka コンシューマーソースの最大待機時間設定を追加。

#### 可観測性

- [#14096](https://github.com/emqx/emqx/pull/14096) ノードごとの設定同期状態を示す `emqx_conf_sync_txid` メトリクスを Prometheus に公開。

#### MQTT over QUIC

- [#13814](https://github.com/emqx/emqx/pull/13814) MQTT over QUIC マルチストリームの接続スコープキープアライブを実装。制御ストリームがアイドルでも他のストリームがアクティブなら接続を維持。

- [#13984](https://github.com/emqx/emqx/pull/13984) Quicer NIF ライブラリをシステムの `libcrypto` にリンク。セキュリティ・パフォーマンス・互換性を向上。

- [#14112](https://github.com/emqx/emqx/pull/14112) QUIC リスナーで `ssl_options.hibernate_after` をサポートし、メモリ使用量を削減。

### バグ修正

#### コア MQTT 機能

- [#13931](https://github.com/emqx/emqx/pull/13931) `gen_rpc` ライブラリを 3.4.1 に更新。クライアントソケット初期化エラーがサーバーノードクラッシュに至る問題を修正。

- [#13969](https://github.com/emqx/emqx/pull/13969) 期限切れ保持メッセージの定期クリーンアップを最適化。

- [#14068](https://github.com/emqx/emqx/pull/14068) ゲートウェイ実装モジュールに `handle_frame_error/2` コールバックを追加し、メッセージ解析エラーを処理。

- [#14037](https://github.com/emqx/emqx/pull/14037) 新規ノードが既存クラスタに参加する際の内部データベースブートストラップを改善。

- [#14116](https://github.com/emqx/emqx/pull/14116) Retainer のデフォルト設定生成問題を修正。

#### MQTT Durable Sessions

- [#14042](https://github.com/emqx/emqx/pull/14042) サブスクリプションパラメータ更新後の Durable Session クラッシュを修正。

- [#14052](https://github.com/emqx/emqx/pull/14052) cgroups 使用時のメモリ使用量報告を修正。

- [#14055](https://github.com/emqx/emqx/pull/14055) `/clients_v2` API でオフラインクライアントのフィルターが正しく適用されない問題を修正。

- [#14151](https://github.com/emqx/emqx/pull/14151) `/clients_v2` API の `conn_state` フィルター処理を修正。

- [#14057](https://github.com/emqx/emqx/pull/14057) セッション永続化有効時の Messages DS データベース起動互換性問題を修正。

#### REST API

- [#14023](https://github.com/emqx/emqx/pull/14023) `/monitor` HTTP API の値が指定時間窓で実際より大きく見える問題を修正。

#### EMQX クラスタリング

- [#13996](https://github.com/emqx/emqx/pull/13996) `emqx conf fix` 実行時の断続的クラッシュを修正。

#### セキュリティ

- [#13922](https://github.com/emqx/emqx/pull/13922) CRL キャッシュのキーに完全な配布ポイント URL を使用。パスのみ使用していた問題を修正。

- [#13924](https://github.com/emqx/emqx/pull/13924) JWT 認証失敗時に JWK キーがデバッグログに漏洩する問題を修正。

- [#13998](https://github.com/emqx/emqx/pull/13998) OIDC 設定不備時の SSO 機能クラッシュを修正。

#### データ統合

- [#13916](https://github.com/emqx/emqx/pull/13916) ルールの `failed` 親メトリクスが更新されない問題を修正。

- [#14001](https://github.com/emqx/emqx/pull/14001) リソースが短時間切断後に誤って接続済みと判定されるレースコンディションを修正。

- [#13913](https://github.com/emqx/emqx/pull/13913) タイムアウト時にアクション・ソース HTTP API が 500 を返す問題を修正。

- [#14101](https://github.com/emqx/emqx/pull/14101) 同名のソースとアクションが同じコネクターを使う場合の削除失敗問題を修正。

- [#14005](https://github.com/emqx/emqx/pull/14005) IoTDB Thrift ドライバーの SSL 有効時の不具合を修正。

- [#14125](https://github.com/emqx/emqx/pull/14125) IoTDB で非対応の `async` モード指定時にエラーログを出力。

- [#14008](https://github.com/emqx/emqx/pull/14008) 集約モードのアクションでアップロード時にバッチがスキップされるレースコンディションを修正。

- [#14015](https://github.com/emqx/emqx/pull/14015) Kafka/Confluent/Azure Event Hub プロデューサーアクションのディスクバッファリング後の再起動時にメッセージが送信されない問題を修正。

- [#14069](https://github.com/emqx/emqx/pull/14069) Cassandra 統合のプリペアドステートメント問題を修正。

- [#14079](https://github.com/emqx/emqx/pull/14079) Kafka コンシューマーで同一リーダーを持つ複数パーティションの遅延問題を修正。

- [#14106](https://github.com/emqx/emqx/pull/14106) Kafka コンシューマーコネクターで同一トピックの重複ソースを禁止。

- [#14120](https://github.com/emqx/emqx/pull/14120) Pulsar コネクターのヘルスチェック時のタイムアウトログを改善。

#### 可観測性

- [#13909](https://github.com/emqx/emqx/pull/13909) ペイロードが UTF-8 Unicode で表示できない場合のログフォーマットを修正。

- [#14061](https://github.com/emqx/emqx/pull/14061) `emqx_cm:request_stepdown/3` 失敗時のログ情報を改善。

- [#14070](https://github.com/emqx/emqx/pull/14070) コネクターの状態をログから除外し、`emqx_resource:list_instances_verbose/0` で参照可能に。

- [#14099](https://github.com/emqx/emqx/pull/14099) MQTT メッセージの UTF-8 文字列検証失敗時のエラーログを削除。

- [#14091](https://github.com/emqx/emqx/pull/14091) サポートされていない書き込み構文での `function_clause` ログを削除。

#### 監査ログ

- [#14152](https://github.com/emqx/emqx/pull/14152) 監査ログの長大なコンテンツを切り詰めて保存。

#### クラスターリンク

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクのトピックフィルター重複による不整合を修正。各フィルターを個別に処理。

- [#13929](https://github.com/emqx/emqx/pull/13929) クラスターリンクが停止し手動再起動が必要になる問題を修正。

## 5.8.1

*リリース日: 2024-10-14*

EMQX 5.8.1 へのアップグレード前に、破壊的変更および既知の問題を必ずご確認ください。

### 重要な変更

- [#13956](https://github.com/emqx/emqx/pull/13956) `gen_rpc` ライブラリを 3.4.1 に更新。RPC チャンネル確立中の強制シャットダウンでクラスターピアノードがクラッシュする問題を修正。

### 強化点

#### コア MQTT 機能

- [#13525](https://github.com/emqx/emqx/pull/13525) `shared_subscription_initial_sticky_pick` 設定を追加。`shared_subscription_strategy` が `sticky` の場合の初期選択戦略を指定可能に。

- [#13942](https://github.com/emqx/emqx/pull/13942) HTTP クライアントが最新リクエストの10秒後にアクティビティがなければ自動再接続。HTTP 認証、認可、Webhook、GCP PubSub、S3、InfluxDB、Couchbase、IoTDB、Snowflake コネクターに影響。

#### 認証および認可

- [#13863](https://github.com/emqx/emqx/pull/13863) 生の ACL ルールのトピック名テンプレートで `${cert_common_name}` プレースホルダーをサポート。

- [#13864](https://github.com/emqx/emqx/pull/13864) LDAP クエリフィルターで `memberOf` 構文をサポート。

- [#13810](https://github.com/emqx/emqx/pull/13810) クライアント情報認証を追加。Variform 式でクライアント属性をチェックし、マッチ時に認証結果を返す軽量認証方式。

- [#13792](https://github.com/emqx/emqx/pull/13792) 禁止クライアント API `GET /banned` でクエリ文字列によるフィルターをサポート。

#### ルールエンジン

- [#13773](https://github.com/emqx/emqx/pull/13773) 無効化されたルールアクションは `out_of_service` 警告をトリガーしないように。代わりに `discarded` ログとカウンターを使用。

- [#13804](https://github.com/emqx/emqx/pull/13804) Confluent Schema Registry を外部プロバイダーとしてサポート。

#### データ統合

- [#13716](https://github.com/emqx/emqx/pull/13716) IoTDB コネクターに Thrift ドライバーを追加。

- [#13745](https://github.com/emqx/emqx/pull/13745) Snowflake データ統合をサポート。

- [#13783](https://github.com/emqx/emqx/pull/13783) Kafka プロデューサーの非同期モード時の RAM 使用量を削減。

- [#13861](https://github.com/emqx/emqx/pull/13861) 一部データ統合アクションに `undefined_vars_as_null` 設定を追加。SQL テンプレートの未定義変数を `NULL` として扱う。

#### MQTT over QUIC

- [#13814](https://github.com/emqx/emqx/pull/13814) MQTT over QUIC マルチストリームの接続スコープキープアライブを実装。

- [#13897](https://github.com/emqx/emqx/pull/13897) gRPC クライアントの遅延起動問題を修正。

- [#11796](https://github.com/emqx/emqx/pull/11796) RPC スキーマを修正し、クライアントとサーバーが同じトランスポートドライバーを使用するように。

- [#11798](https://github.com/emqx/emqx/pull/11798) `./bin/emqx data import [FILE]` 実行後にノードが起動できない問題を修正。

- [#11813](https://github.com/emqx/emqx/pull/11813) RPC クライアント SSL ポートがサーバーポートと一致するように修正。

- [#11819](https://github.com/emqx/emqx/pull/11819) OpenTelemetry ライブラリを v1.3.1-emqx にアップデートし、無効なメトリクスタイムスタンプを修正。

- [#11861](https://github.com/emqx/emqx/pull/11861) リモートコンソールシェルの過剰な警告ログを修正。

- [#11722](https://github.com/emqx/emqx/pull/11722) `sync` クエリモードの Kafka プロデューサーブリッジでバッファリングされない問題を修正。

- [#11724](https://github.com/emqx/emqx/pull/11724) Kafka 送信メッセージのルールメトリクスが誤って失敗としてカウントされる問題を修正。

- [#11728](https://github.com/emqx/emqx/pull/11728) LDAP フィルター文字列パーサーを改善。特殊文字の自動エスケープと `dn` フィルター値のバグ修正。

- [#11733](https://github.com/emqx/emqx/pull/11733) セッション奪取やチャネル追放時のクラッシュを修正。

- [#11750](https://github.com/emqx/emqx/pull/11750) HTTP 認証と HTTP ブリッジのリクエストボディのログ出力を抑制。

- [#11760](https://github.com/emqx/emqx/pull/11760) Cassandra ブリッジのクエリを簡素化し、サーバーログの警告を削減。

- [#11886](https://github.com/emqx/emqx/pull/11886) プラグインの後方互換性を修正。誤ったフックポイント名の登録を許可し、警告を出すように。

- [#11897](https://github.com/emqx/emqx/pull/11897) クラスターノードの同時起動時の設定同期ループレースコンディションを修正。

## 5.8.0

*リリース日: 2024-08-28*

アップグレード前に [5.8 の既知の問題](./known-issues-5.8.md) をご確認ください。

### 強化点

#### クラスターリンク

- [#13126](https://github.com/emqx/emqx/pull/13126) 複数の独立した EMQX クラスターを接続し相互通信可能にするクラスターリンク機能を導入。地理的に分散したクライアント間の効率的なメッセージ交換を実現。

#### コア MQTT 機能

- [#13009](https://github.com/emqx/emqx/pull/13009) レート制限によるメッセージ受信一時停止のログレベルを `debug` から `warning` に変更し、ログスロットリングを追加。

#### 認証および認可

- [#13350](https://github.com/emqx/emqx/pull/13350) クライアント接続時のサーバー名（`peersni`）を取得しクライアント情報に保存可能に。

- [#12418](https://github.com/emqx/emqx/pull/12418) JWT 認証のクレーム検証をオブジェクトのリストでサポート。テンプレート式による任意表現を可能に。

- [#13229](https://github.com/emqx/emqx/pull/13229) 認証テンプレートで `${cert_pem}` プレースホルダーをサポート。

- [#13324](https://github.com/emqx/emqx/pull/13324) EMQX ダッシュボードが OIDC プロトコル対応 ID サービス（例: Okta）と連携し OIDC ベースの SSO を可能に。

- [#13534](https://github.com/emqx/emqx/pull/13534) スーパーユーザーが認可チェックをバイパスした際のトレースログを追加。

- [#13601](https://github.com/emqx/emqx/pull/13601) GSSAPI メカニズム（SASL-GSSAPI Kerberos V5）を用いた Kerberos 認証を追加。

#### データ統合

- [#13144](https://github.com/emqx/emqx/pull/13144) `data_bridge_buffer_overflow` ログレベルを `info` から `warning` に変更しスロットリングを追加。

- [#13492](https://github.com/emqx/emqx/pull/13492) `GET /connectors` と `GET /connectors/:id` API に依存するアクション・ソースの一覧を含めるよう拡張。

- [#13505](https://github.com/emqx/emqx/pull/13505) HTTP API でデータ統合アクション・ソースの ID によるルールフィルタリングを追加。

- [#13506](https://github.com/emqx/emqx/pull/13506) ルールエンジンイベントに `peername` フィールド（`IP:PORT` 形式）を追加。

- [#13516](https://github.com/emqx/emqx/pull/13516) `republish` アクションに `direct_dispatch` 引数を追加。`true` 設定でメッセージを直接サブスクライバーに送信し、再帰的ルール発火を防止。

- [#13573](https://github.com/emqx/emqx/pull/13573) ルール SQL コンテキストに `client_attrs` を追加。クライアント属性を SQL 文で利用可能に。

- [#13640](https://github.com/emqx/emqx/pull/13640) ルール SQL に `coalesce/2` と `coalesce_ne/2` 関数を追加。NULL 値処理を簡素化。

- [#12959](https://github.com/emqx/emqx/pull/12959) Kafka プロデューサーコネクターにヘルスチェック用の専用トピック設定を追加。

- [#12961](https://github.com/emqx/emqx/pull/12961) Kafka コンシューマーソースでグループIDの事前カスタマイズを追加。

- [#13069](https://github.com/emqx/emqx/pull/13069) Azure Blob Storage とのデータ統合をサポート。

- [#13199](https://github.com/emqx/emqx/pull/13199) メッセージ変換機能を実装。Avro から JSON へのデコードやトピックの前置き文字列追加などを簡単な variform 構文で実現。

- [#13415](https://github.com/emqx/emqx/pull/13415) Couchbase とのデータ統合をサポート。

- [#13463](https://github.com/emqx/emqx/pull/13463) GCP PubSub プロデューサーの HTTP ステータス 502/503 受信時に自動リトライを追加。

- [#13546](https://github.com/emqx/emqx/pull/13546) Pulsar プロデューサーアクションのクエリモード設定を追加。

- [#13650](https://github.com/emqx/emqx/pull/13650) DataLayers とのデータ統合をサポート。

#### 運用

- [#13202](https://github.com/emqx/emqx/pull/13202) クラスター設定不整合を修正する `emqx ctl conf cluster_sync fix` コマンドを追加。

- [#13250](https://github.com/emqx/emqx/pull/13250) `cluster.discovery_strategy` に `singleton` を追加。クラスタリングを事実上無効化し、他ノードとの接続を拒否。

- [#13370](https://github.com/emqx/emqx/pull/13370) Durable Storage の新しいストレージレイアウト `wildcard_optimized` を追加。遅延がなく、メッセージをより効率的にシリアライズ。

- [#13524](https://github.com/emqx/emqx/pull/13524) 独占トピック管理用の CLI インターフェース `emqx ctl exclusive` を追加。

- [#13597](https://github.com/emqx/emqx/pull/13597) プラグイン用の証明書ファイル管理ラッパー関数を追加。証明書の誤削除を防止。

- [#13626](https://github.com/emqx/emqx/pull/13626) リスナーの有効/無効を切り替える `emqx ctl listeners enable <Identifier> <Bool>` コマンドを追加。

- [#13493](https://github.com/emqx/emqx/pull/13493) RPC ライブラリ `gen_rpc` を 3.4.0 にアップグレード。RPC サーバーのソケットオプションを `true` から `active-100` に変更し、負荷時にバックプレッシャーを導入。

- [#13665](https://github.com/emqx/emqx/pull/13665) Prometheus エンドポイントにルールによるすべてのアクション数を示すメトリクス `emqx_actions_count` を追加。

- [#13434](https://github.com/emqx/emqx/pull/13434) `rpc` 設定を簡素化。`rpc.server_port` を追加し、`rpc.tcp_server_port` と `rpc.ssl_server_port` を置換。`rpc.tcp_client_num` は `rpc.client_num` に名称変更。旧設定は互換性のためエイリアスとして残す。

### バグ修正

#### コア MQTT 機能

- [#12944](https://github.com/emqx/emqx/pull/12944) `strict_mode=false` で UTF-8 以外のクライアントID接続時のクラッシュを修正。

- [#13006](https://github.com/emqx/emqx/pull/13006) ネットワーク問題等で遅延・奪取されたセッションメッセージが禁止クライアントIDルールを回避する問題を修正。

#### 認証および認可

- [#13024](https://github.com/emqx/emqx/pull/13024) HTTP 認証のエラーログを改善。`Content-Type` ヘッダー欠如時の詳細ログを追加。POST メソッドの JSON テンプレート失敗時のエラーメッセージを改善。

- [#13196](https://github.com/emqx/emqx/pull/13196) 組み込み認可データベースでクライアント/ユーザーごとの ACL ルール数をデフォルト100に制限。

- [#13584](https://github.com/emqx/emqx/pull/13584) HTTP 認可で空のヘッダーリストによるエラーを修正。

- [#13618](https://github.com/emqx/emqx/pull/13618) `authorization/sources` エンドポイントの型定義を改善しエラーメッセージを明確化。

- [#13624](https://github.com/emqx/emqx/pull/13624) 組み込み認可でルール更新時に `max_rules` 制限を超える問題を修正。

- [#13678](https://github.com/emqx/emqx/pull/13678) 認証チェーンの認証器削除を冪等操作に修正。

#### データ統合

- [#13207](https://github.com/emqx/emqx/pull/13207) `republish` ルールアクションでメッセージ配信失敗時に失敗メトリクスが正しく増加しない問題を修正。

- [#13425](https://github.com/emqx/emqx/pull/13425) MQTT コネクターのエラーログを改善。

- [#13589](https://github.com/emqx/emqx/pull/13589) HTTP API で ID に `"null"` 文字列を指定したルール作成を禁止。

- [#13414](https://github.com/emqx/emqx/pull/13414) RabbitMQ コネクターのエラーログを改善。

#### ファイル転送

- [#12514](https://github.com/emqx/emqx/pull/12514) ファイル転送コマンドの結果報告が切断時に失われる問題を修正。専用プロセスで監視し、切断後もステータス通知を保証。

#### 運用

- [#13078](https://github.com/emqx/emqx/pull/13078) EMQX 管理 API で JSON ボディの `Content-Type: application/json` ヘッダーが必須に。欠如時は 415 エラーを返す。

- [#13225](https://github.com/emqx/emqx/pull/13225) 認証・認可 API でパスワード等の機密情報を `******` にマスク。

#### ゲートウェイ

- [#13607](https://github.com/emqx/emqx/pull/13607) CoAP サブスクリプションの QoS 表示が実際の QoS と異なる問題を修正。

# 以下略（以降のバージョンも同様の形式で翻訳可能です）
