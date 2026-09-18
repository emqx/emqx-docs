# EMQX Enterprise バージョン 4

## e4.4.36

*リリース日: 2026-05-22*

### 強化点

- ノードごとのライセンス情報を Prometheus メトリクスで公開。

  `emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at` の3つの Prometheus メトリクスを追加しました。タイムスタンプは Unix エポック秒（UTC）です。ライセンスが利用できない場合、これら3つのメトリクスはすべて `0` を返します。

- RabbitMQ ルールアクションが RabbitMQ のデフォルト（名前なし）エクスチェンジをサポート。

  `exchange` パラメータは必須ではなくなり、デフォルトは空文字列 `""` です。空の場合、ルールアクションはデフォルトのダイレクトエクスチェンジを通じてメッセージをパブリッシュし、メッセージは `routing_key` と同名のキューにルーティングされます。このモードでは、デフォルトエクスチェンジはブローカーによって管理されているため、ブリッジは `exchange.declare` や `exchange.delete` を実行しません。[RabbitMQ: Default Exchange](https://www.rabbitmq.com/docs/exchanges#default-exchange) を参照してください。

- Erlang VM スケジューラのロード圧縮をデフォルトで無効化。

  スケジューラロード圧縮は `vm.args` の `+scl false` により無効化され、スケジューリングの安定性向上と負荷遷移時のメッセージレイテンシ低減に寄与します。

  ただし、一部のCPUトポロジーでは、ロード圧縮無効化により低負荷時に EMQX のCPU使用率が高くなる場合があります。特に複数NUMAノードや多数の論理CPUコアを持つシステムで顕著です。この場合、ロード圧縮を再度有効化（`+scl true`）することで低負荷時の挙動を改善できますが、負荷増加時の性能変動が大きくなる可能性があります。スケジューラ数を `+S Schedulers:SchedulerOnline` で減らす、CPUのハイパースレッディングを無効化してスケジューラスレッド数を減らす、または単一NUMAノードのCPUコアのみをバインドすることも効果的です。

- MQTT接続のヒープメモリ制限更新時のパフォーマンス最適化。

  ClientID のログトレースを有効化すると、EMQX は該当 MQTT 接続プロセスのヒープメモリ制限を調整し、過剰ログによるプロセス終了を防止します。この最適化では、設定更新時に多数の MQTT 接続プロセスで GC が発生することによる性能低下を避けるため、`persistent_term` の代わりに ETS を使用してヒープ制限設定を管理します。

- Erlang 分散通信ポートバッファに関する設定オプションを追加し、デフォルト値を増加させてクラスターの安定性を向上。

  以前のデフォルトバッファサイズは `1460B` でしたが、本リリースでは `1MB` に増加し、高レイテンシや大容量メッセージに対応し RPC レイテンシを大幅に低減、クラスターの安定性を改善します。新しい設定は以下の通りです。

  ```hocon
  node.dist_connect_options.nodelay = false
  node.dist_connect_options.sndbuf = 1MB
  node.dist_connect_options.recbuf = 1MB
  node.dist_connect_options.buffer = 1MB
  node.dist_listen_options.nodelay = false
  node.dist_listen_options.sndbuf = 1MB
  node.dist_listen_options.recbuf = 1MB
  node.dist_listen_options.buffer = 1MB
  ```

- ホットアップグレードスクリプトのタイムアウトを延長。

  デフォルトのタイムアウトを 5 分から 25 分に延長し、アップグレードのタイムアウトを回避します。ホットアップグレードは旧バージョンのスクリプトを使用するため、この新しいデフォルトタイムアウトは本バージョンから新しい EMQX バージョンへのアップグレード時にのみ有効です。

- ノード避難／リバランス機能およびクラスター分散ロック（`ekka_locker`）のマルチノード呼び出しで `rpc` を `erpc` に置換し、高負荷時の性能と安定性を向上。

- Erlang/OTP を 24.3.4.17-2 にアップグレード。

  Erlang/OTP の修正はホットアップグレードでは適用されず、EMQX 再起動後に有効となります。

### バグ修正

- 不安定なネットワーク環境下での EMQX クラスターの自己修復失敗を修正。

- Redis Sentinel 接続で、Redis データノードと Sentinel ノードに別々の認証設定を行えるよう修正。

- 一部 HTTP API の RPC 自己呼び出し問題を修正。

  修正前は、Dashboard API の `nodes/:node/monitor/metrics`（メッセージレート統計取得）が RPC の再帰ループに入り、多数の `gen_rpc` プロセスがリークする問題がありました。この問題はクラスター環境で、ノード名の IP 部分がループバックアドレス（例：`emqx@127.0.0.1`）に誤設定されている場合に発生しました。

  この変更は、`emqx_management`、Dashboard、ホット設定、トピックメトリクス、クライアントタグモジュールでのクロスノード RPC を行う多くの HTTP API に影響し、ローリングアップグレード中は古いバージョンノードが存在するため、これらの API が不正確な結果を返すか失敗する場合があります。

- 長時間のプロセスマイルボックス警告における不正確なメールボックス長ログを修正。

- `emqx_broker_helper` と `username_quota` プロセスが過度に長いプロセスマイルボックスから復旧できないリスクを修正。

- RocketMQ リソースが旧バージョンのコードモジュールを解放できない問題を修正。

  修正前はホットアップグレード完了後も RocketMQ リソースプロセスが旧コードのアンロードをブロックし、不要なプロセススキャンと性能オーバーヘッドを引き起こしていました。なお、RocketMQ リソースはホットアップグレード時に再起動されるため、少数のメッセージが失われる可能性があります。

- RocketMQ Producer のメモリリーク問題を修正。

- e4.4.34 で導入された API キー権限管理後、`management.default_application` と `management.bootstrap_apps_file` からの AppID が互換モード（権限レコードなし）として扱われる問題を修正。

  修正前はこれらの AppID が権限レコードを持たず、API リクエストごとに `AppId 'xxx' accessing '/api/v4/resources' in compatibility mode (no permission record)` の警告ログが繰り返されていました。修正後はこれらの AppID をフル権限として扱い、権限レコードなしの互換モード AppID のログレベルは `warning` から `info` に下げられました。

- SAML SSO ログイン失敗時に EMQX 内部エラー詳細がレスポンスボディに漏洩する問題を修正。

  修正前は SAML 認証成功後にアカウントプロビジョニングやセッション設定が失敗すると、内部エラー詳細がブラウザに返されていました。修正後は一般的なエラーメッセージを返し、詳細はサーバーログにのみ記録されます。

- ノード間の MQTT メッセージ転送率が非常に高い場合に `gen_rpc` が大量のワーカープロセスを生成し、システムのプロセス数制限を超過する問題を修正。

## e4.4.35

*リリース日: 2026-04-03*

### バグ修正

- ネットワーク分断後の EMQX 再起動によるルーティングテーブル不整合を修正。`ekka-0.8.1.17` へのアップグレードで解決。

## e4.4.34

*リリース日: 2026-03-25*

### 強化点

- 管理 HTTP API に API キー権限管理を追加。

  API キーはカテゴリ別の書き込み権限（`banned`、`rule_engine`、`resources`、`plugins`、`modules`）を設定可能で、`GET` リクエストは互換性のため読み取り可能です。

- ダッシュボードに MFA 認証とセッション管理を追加。

  MFA のセットアップ／チャレンジフロー、MFA 状態管理 API、JWT ベアラーセッション、ユーザーログアウトをサポート。

- ダッシュボードに SAML 2.0 SSO モジュールを追加。

  IDP メタデータ統合、ACS コールバック処理、SP メタデータエクスポート、オプションの SP 署名付き AuthnRequest、SSO ユーザー向けの `force_mfa` 設定をサポート。

- HTTP API の可観測性メトリクスを追加。

  HTTP API の成功／失敗カウンターとリクエスト時間ヒストグラムを記録し、Prometheus にエクスポート。`/api/v4/http_api_metrics` で直接カウンター参照可能。

- Helm Chart の起動挙動を改善。

  デフォルトの `podManagementPolicy` を `OrderedReady` に変更し、`k8s`／`dns` 発見モード向けに DNS 待機用の init コンテナを追加し、クラスターのブートストラップ安定性を向上。

- `ehttpc` のヘッドオブラインブロッキング処理を改善。

  ブロッキング検出時に自動的に切断・再接続し、少数の長時間リクエストによる接続全体のブロッキングを防止。HTTP ACL・認証、Webhook リソース、IoTDB リソース、SAP Event Mesh リソース、GCP PubSub リソースなどに影響。

### バグ修正

- ホットアップグレードおよびローリングアップグレード時のトレースモジュールテーブル処理を修正し、トレーステーブルコピー不整合と起動問題を回避。

- `pulsar-client-erl` を `0.7.3` にアップグレードし、Pulsar ブリッジの単一メッセージ解析を修正。

- HTTP API で ACL ファイルを空値に更新できない問題を修正。

- 高接続数シナリオでの `emqx_vm_mon` リソース使用を最適化し、メモリオーバーヘッドを削減。

- システムメモリ使用量取得による HTTP API レスポンス遅延を修正。

  以前は大量接続時に `memsup` が全 Erlang プロセスを走査して最大メモリ使用プロセスを特定していたため遅延。現在は `os_mon.memsup_system_only = true` に設定し、システムメモリ使用のみ取得。`/nodes` と `/emqx_prometheus` エンドポイントに影響。

- `eredis_cluster` を `0.7.8` にアップグレードし、Redis クラスターの再接続バックオフ挙動を改善。

- Kafka クライアントの安定性修正を含む `wolff` を `1.5.20` にアップグレード。

- ノードがクラスターに参加する際のライセンス読み込み挙動を修正し、クラスターライセンスを優先し無効または期限切れライセンスの使用を防止。

## e4.4.33

*リリース日: 2025-11-26*

### 強化点

- タグに基づくレート制限を追加。

  HTTP 認証サービスが返すタグを利用してクライアントを分類し、カテゴリ別にレート制限を適用可能。

- ACL キャッシュ機能のメモリ消費を削減。

  以前は MQTT メッセージペイロードが大きい場合、ACL キャッシュが多数の MQTT セッション数に比例して大きなメモリを消費していました。

- username_quota モジュールが指定したユーザー名の全クライアント接続をキック可能に。

- username_quota モジュールの「使用状況」ページのユーザー体験を改善。

  以前は「使用状況」ページがセッション数で自動ソートし、多数のユーザー名がある場合にページ読み込み時間が長くなっていました。現在はソートボタンを追加し、ボタンクリック時のみソートを実行。

- クラスターのノード変更時に username_quota モジュールのシステムリソース消費を削減。

  他ノードのオフライン検知時の不要なデータ同期を減らし、リソース使用を抑制。

### バグ修正

- MySQL および PostgreSQL アクションで SQL 複数行挿入構文が使えなかった問題を修正。

  ログに以下のエラーメッセージが表示されていました。

  ```
  ... Not an INSERT statement or incorrect SQL syntax
  ```

- ローリングアップグレード時に LwM2M モジュールが起動しなかった問題を修正。

  ログに以下のエラーが表示されていました。

  ```
  [error] init_module_failure, module: emqx_module_proto_lwm2m, reason: {badkey,<<"coap_max_block_size">>}, ...
  ```

- バイナリパッケージでインストールした EMQX 環境の LwM2M モジュールのデフォルト XML パスエラーを修正。

- Kafka Producer のキャッシュメッセージが Kafka サービス復旧後に送信できなかった問題を修正。

  ログに以下の警告が表示されていました。

  ```
  [warning] your-kafka-topic replayq_overflow_dropped_number_of_requests 2444
  ```

- ログトレース機能が EMQX バージョンアップ後に `emqx_trace` リモートテーブルの消失で利用不能になる問題を修正。

  旧ノードが停止される前にログトレースが有効だった場合、新ノードのログトレースモジュールが `emqx_trace` テーブルにアクセスできず失敗していました。これにより `emqx ctl cluster force-leave <node>` コマンドも失敗していました。修正により、ログトレースモジュールは起動時に自動的に `emqx_trace` テーブルを復元し、`force-leave` コマンドも正常動作します。

- レート制限のトークンバケットアルゴリズム実装の誤りを修正し、不正確な最大レートを是正。

## e4.4.32

*リリース日: 2025-07-30*

### 強化点

- HTTP AUTH/ACL モジュールで HTTP ヘッダーにプレースホルダーをサポート。

  HTTP AUTH/ACL モジュールは、HTTP リクエストヘッダー値に `%u`、`%c` などのプレースホルダーを使用可能になり、クライアント情報を動的に挿入できます。

- Erlang VM のデフォルトパラメータを最適化。

  - `+sbwt none +sbwtdcpu none +sbwtdio none`：スケジューラのビジーウェイトを無効化し CPU 消費を削減。
  - `+sbt db`：スケジューラスレッドを CPU コアにデフォルトバインド。
  - `+zdbbl 32768`：分散チャネルのバッファサイズを増加。

- 定期的なグローバル GC をデフォルトで無効化。

  `node.global_gc_interval` のデフォルト値を `Disabled` に設定。

### バグ修正

- Kafka リソースで "SCRAM_SHA_256" 認証が失敗する問題を修正。

## e4.4.31

*リリース日: 2025-07-15*

### 強化点

- Username Quota モジュールのパフォーマンス改善。

  マルチノードクラスターで Username Quota 機能が有効な場合、ノード間でユーザー名とクライアントIDの状態同期が頻繁に発生し CPU 負荷が増大。今回のバージョンでバッチ同期を導入し CPU 使用率を削減。

- 「Username 更新間隔」設定を追加。

  極端な条件下でノード間の Username Quota テーブル不整合を防ぐため、定期的に他ノードからユーザー名状態を取得しローカルテーブルを更新。デフォルトは15分、最小設定値は30秒。

- Republish アクションに「未定義プロパティ送信」オプションを追加。

  未定義の MQTT プロパティやユーザープロパティを再パブリッシュメッセージに含めるか制御。有効時は値を文字列 `"undefined"` として追加、無効時は省略。

- 高レイテンシネットワーク環境下での HTTP API 安定性を改善。

  RPC に依存する部分を `gen_rpc` にリファクタリングし、Erlang 分散 RPC チャンネルの競合を回避、ブロッキングリスクを低減。

- 内蔵データベース認証（`auth_mnesia`）のクエリ性能を最適化。

  レコード数増加によるクエリ性能劣化を解消し、高頻度・同時ログイン時の CPU 消費を低減、認証効率とシステム安定性を向上。

### バグ修正

- クラスター修復後のルーティングテーブルやクライアントグローバルレジストリの不整合を修正。

  ネットワーク分断でクラスターが重複サブグループに分割された場合、マイノリティパーティションの単純再起動では整合性が完全に回復しないことがありました。今回の修正で、マイノリティパーティションと重複グループの全ノードを再起動し、クラスター全体の整合性を回復します。

- HTTP API 呼び出し中の例外による ETS メモリリークを修正。

- ダッシュボードからゲートウェイモジュールにリスナーを追加できない問題を修正。

  プロトコルゲートウェイモジュール作成後、モジュール更新画面からリスナー追加が反映されなかった問題。対象プロトコルは CoAP、GB/T 32960、JT/T 808、LwM2M、MQTT-SN、STOMP、TCP。

## e4.4.30

*リリース日: 2025-06-20*

### 強化点

- グローバルガベージコレクションのスムーズ化。

  EMQX は全プロセスに対し定期的に GC を実施し、`node.global_gc_interval`（デフォルト15分）で間隔を制御。これは Erlang VM のデフォルト GC が極端な条件下でオフヒープバイナリメモリを適時回収できない問題を防止しますが、CPU 使用率の周期的スパイクを引き起こします。

  新機構では各サイクルでプロセスをバッチ処理で GC し、CPU 使用率の変動を低減。`node.global_gc_interval` が1分以上に設定されている場合にのみ有効。

### バグ修正

- 古いバージョンから 4.4.28 または 4.4.29 へのホットアップグレード時に一部接続が切断される問題を修正。

  切断クライアント数は当該ノードのメッセージレートに正比例。

## e4.4.29

*リリース日: 2025-03-07*

### 強化点

- ライセンスチェック性能を最適化。

  クライアント接続時のライセンス総接続数チェックにおけるノード間 RPC 呼び出しを最小化し、性能オーバーヘッドを削減。

- 大量クライアント再接続時の接続管理を改善し、ライセンス制限による接続拒否を防止。

  現接続数がライセンス上限を超えても、既に確立済みの ClientID を持つクライアントの再接続を許可。

- Wolff（Kafka ドライバー）を強化し、パーティション数減少による Kafka トピック再構築をサポート。

  Kafka はパーティション数の直接減少を許さないため、通常は新規トピック作成やデータ移行が必要。以前の Wolff はこれらに対応できず、一部プロデューサーの再接続失敗を招いていました。

- 全リスナーの `acceptors` と `max_connections` のデフォルト値を統一。

  `acceptors` はすべてのリスナーでデフォルト16、`max_connections` は 1,024,000 に統一。

- Trace モジュールの設定オプションを拡充。

  - 行の最大文字数（デフォルト 2048）
  - ログファイル最大サイズ（デフォルト 1GB）
  - クライアントプロセスの最大ヒープサイズ（デフォルト 512MB）。ログトレース有効時にデフォルトメモリ制限（64bit環境で64MB）によるプロセス終了を防止。

- クライアントプロセス終了時に Warning ログを追加。

  以前は Erlang/OTP のエラーメッセージのみで、対象クライアント特定が困難でした。現在は以下のような Warning ログも出力されます。

  ```
  [warning] [CM] Clean down, clientid: abcd_bench_pub_1, pid: <0.3540.0>, reason: killed
  ```

### バグ修正

- コンテナ環境でのホット設定機能による `acceptors` と `max_connections` の更新失敗を修正。

- Redis クラスターのマスター切替後に Redis への再接続に失敗する問題を修正。

  ネットワーク障害により新マスターへの接続が失敗すると、`eredis_cluster` が異常状態に入り再接続不能になる問題。

- ダッシュボードからのログトレースファイルダウンロード時の応答停止問題を修正。

- Pulsar ドライバーのパケット解析問題を修正。

## e4.4.28

*リリース日: 2025-01-23*

### 強化点

- EMQX クラスターの自己修復能力を強化。

  以前は単純なスプリットブレインのみ自己修復可能でしたが、以下の条件で修復可能に。

  - 1ノードが他全ノードと連絡可能なら基準ノードとし、他ノードを再起動してクラスター復元。
  - スプリットブレインが2つのサブクラスターなら、ノード数少ない方を再起動してクラスター復元。

  改善後は複雑かつ非対称な複数クラスタでも自己修復可能。

- ワイルドカードサブスクライブ／解除処理性能を最適化。

  プレフィックスツリーをノード間レプリケートが必要な `mnesia` テーブルから `ETS` テーブルに変更し、ノード間同期時間を削減。サブスクライブ処理は非同期化され、SUBACK 応答後にルーティング情報を他ノードに非同期更新。

  ただし、古いバージョンノードが混在する場合、新バージョンノードでのワイルドカードサブスクライブは古いノードからのメッセージ受信ができません。全ノードアップグレード後に自動解消。アップグレード後はルーティングテーブル経由でプレフィックスツリーが再構築されるため、ローリングアップグレード中もルーティング情報は失われません。

- ルールエンジンのマッチング性能を最適化。

  トピックプレフィックスツリーをキャッシュし、過剰なトピックスプリットを削減。ルール数が多いシナリオで効果大。

- Kafka アクションに「バッファ最大遅延時間」オプションを追加。

  パーティションごとにメッセージをバッチ書き込みする最大待機時間を制御。デフォルト `0ms` は待機なし。メモリ以外のバッファリングモードでは IOPS 削減のため最低 `5ms` 推奨。

- アラーム処理を非同期モードに変更。

  以前は同期処理で多数の `conn_congestion` アラームが MQTT 接続プロセスに影響。現在は非同期処理かつ過負荷保護を追加。過負荷時は1分間のサイレント期間に入り、アラームを破棄。

- プロセスメッセージキュー長の監視・アラームを追加。

  `vm_mon.process_long_msgq` と `vm_mon.process_alarm_top_n` 設定を追加。

  - `vm_mon.process_long_msgq`：プロセスのメールボックス長がこの値を超えるとアラーム（デフォルト80）。
  - `vm_mon.process_alarm_top_n`：アラーム時にメッセージキュー長上位Nプロセス情報を含める（デフォルト5）。

- CONNECT パケット解析失敗時のログを最適化。

  CONNECT 可変ヘッダー解析失敗で MQTT 接続切断時、`esockd` はエラーをログ出力せず、切断理由を `malformed_connect_variable_header` とマーク。

- ログを「常に非同期」モードに変更。

  以前は `log.sync_mode_qlen` のデフォルト 100 を超えると同期モードに切替。現在は 3000 に変更し、`log.drop_mode_qlen` と一致。ログキュー長が 3000 を超えるとログ破棄開始。

- スロースブスクリプションの性能を最適化。

  `ets:info(emqx_slow_subs_topk, size)` 呼び出しを回避し、性能オーバーヘッドを軽減。

- ホット設定によるリスナー更新時間を短縮。

  以前はノードごとに順次リスナー更新・再起動していたが、`erpc:multicall/4` により並列更新し時間短縮。

- 遅延起動する `ecpool_worker` による `ecpool_sup` ブロックを回避。

### バグ修正

- 永続セッションのユーザー名が username quota ページから消える問題を修正。

  修正前は MQTT クライアントの永続セッション再接続後にユーザー名が消失。

- ログスロットリングによる性能低下問題を修正。

  修正前はログトレース有効時にログスロットリングの問題で EMQX のリソース消費が大幅増加。

## 4.4.27

*リリース日: 2024-11-28*

### 強化点

- MQTT ブリッジアクションに `Message Delivery Timeout` と `Max Message Retry Count` パラメータを追加し、PUBACK 応答がないピアによるブリッジ詰まりを防止。

  - `Message Delivery Timeout`：デフォルト30秒。QoS1/QoS2 メッセージの配信時間がこれを超えると破棄。
  - `Max Message Retry Count`：デフォルト3。ACK が再送間隔内に受信できない場合、最大この回数まで再送。

- ホット設定モジュールのリスナー再起動ロジックを最適化。

  ホット設定モジュール起動時に `mnesia` テーブルのリスナー設定差分を検知してもリスナーは再起動せず、ユーザーに手動再起動を促すログを出力。

  ```
  [EMQX_HOT_CONF] There is a difference between the listener conf in the hot conf module and the one currently in use at runtime. Please restart the listener at an appropriate time to ensure the configuration is correctly applied. listener: mqtt:tcp:external, conf_in_use: #{...}, hot_conf: #{...}
  ```

- ダッシュボードのノード情報ページで `Waiting to join` ステータスを廃止。

  ノードステータスは `Running` または `Stopped` のみ表示。

- Amazon Linux 2023 インストールパッケージをサポート。

- MySQL と PostgreSQL のバッチモードアクションで `ON DUPLICATE KEY UPDATE` または `ON CONFLICT DO NOTHING` 文をサポート。

  主キー重複時の重複挿入回避に利用可能。ただし、これら文の後にプレースホルダーは使用不可。

- `ecpool` の性能を最適化。

- ダッシュボードのホット設定ページで `allow_anonymous` に `false_quick_deny` を追加。

  設定すると匿名（ユーザー名なし）クライアントを即時拒否し、認証バックエンドチェックをスキップ。

### 修正

- サーバー停電後の破損キャッシュファイルにより Kafka アクションが失敗する問題を修正。

  破損ファイル検出時に Kafka プロデューサーは回復不能メッセージを破棄。

- ホットアップグレード後にノード間で MQTT メッセージ同期できない問題を修正。

  `4.4.12` から `4.4.22` の間のバージョンにアップグレードすると発生。ログに `gen_rpc_auth` コードモジュールの未定義エラーが表示。

- Username Quota モジュール未有効時に HTTP API でユーザー情報取得すると 500 エラーになる問題を修正。

- 古いバージョンからのバックアップファイルインポート後に `emqx_mod_acl_internal` フックが重複マウントされる問題を修正。

- クラスター参加中のノードで HTTP API からアラームリスト取得時に一時的に 500 エラーが発生する問題を修正。

## 4.4.26

*リリース日: 2024-09-26*

### バグ修正

- 古いバージョンクラスターへのノード参加ができない問題を修正。

  4.4.25 で username quota テーブルが存在しないことが原因。

- username quota インターフェースで特殊文字を含むユーザー名検索ができない問題を修正。

  API が URL デコードを行っていなかったため。

- username quota インターフェースに一部ユーザー名が表示されない問題を修正。

  username quota モジュール有効化前に存在したセッションが原因でユーザーリストが不完全に。

## 4.4.25

*リリース日: 2024-09-13*

### 強化点

- MQTT ユーザー名のセッション数制限モジュールを追加。

  ダッシュボードの **Modules** → **Username Quota** で設定可能。ホワイトリストに登録したユーザー名は制限対象外。MQTT ブリッジング時はホワイトリスト利用が必須。

- MQTT 3.1.1 プロトコルで CONNECT メッセージに不正な Will QoS 値が含まれる場合のエラーログを改善。

- Redis へのメッセージ送信性能を強化。

  以前は `gen_tcp:send/2` の制限で Redis クライアントプロセスの送信キューが増大すると送信性能が低下し、Redis 認証や ACL、プラグイン、ルールエンジンの Redis リソースがボトルネックに。大量デバイス再接続時の負荷を軽減。

- SysKeeper へのメッセージ送信性能を強化。

  以前は `gen_tcp:send/2` の制限で `emqx_bridge_sysk_forward` プロセスの送信キューが増大すると送信性能が低下し、ボトルネックに。

- ログトレースの単一ログファイルサイズ上限を 512MB から 1GB に拡大。

- 内蔵 DB AUTH/ACL モジュールの ACL 機能を強化。

  - クライアントごとの ACL エントリ数制限を追加。

    パブリッシュ／サブスクライブ時の ACL 検証はクライアントの ACL エントリを走査するため、多数エントリ追加は性能低下要因。`auth.mnesia.max_acls_for_each_login` 設定またはモジュールの `Max ACLs` パラメータで制限可能。

  - ACL テーブルの格納構造とトピックマッチングロジックを最適化し、検索性能を向上。クライアントの ACL エントリ数が多いほど効果大。

  - ACL エントリ追加・削除時に ACL キャッシュをリセット。

- ルールエンジンのバッチ送信処理に過負荷保護を追加。

  バッチ送信有効時、外部 DB 応答遅延でメッセージがバッチプロセスのメッセージキューに積み上がり、メモリ制限超過リスクあり。メッセージキューサイズが `"Maximum Batch Size" * 10`（1000未満なら1000）を超えると、アクションを一定期間（デフォルト60秒）「アンロード」し、以降のメッセージは破棄。`action_olp_blocked/<RuleID>/<ActionID>` アラームを発報。

  対象アクション：

  - Cassandra へのデータ
  - ClickHouse へのデータ
  - DolphinDB へのデータ
  - InfluxDB へのデータ
  - IoTDB へのデータ
  - Lindorm へのデータ
  - MySQL へのデータ
  - Oracle Database へのデータ
  - PostgreSQL へのデータ
  - SQLServer へのデータ
  - Tablestore へのデータ
  - TDengine へのデータ
  - GCP Pubsub へのデータ

- データベースに未定義値を `NULL` として挿入する機能を追加。

  ルールエンジンの各種 DB アクションで `${var}` プレースホルダーを使った挿入文構築時、未定義変数が文字列 `undefined` として挿入されていた問題を解消。新オプション `Insert Undefined Values as NULL` を追加し、未定義時に `NULL` 挿入可能に。

  対応アクション：

  - Cassandra
  - ClickHouse
  - DolphinDB
  - MySQL
  - Oracle Database
  - PostgreSQL
  - SQLServer
  - TDengine

- ログスロットリングをサポート。

  異常時に大量の類似ログが生成されシステム負荷増大や有用ログの埋没を防止。`etc/logger.conf` の `log.throttling` 設定で時間窓と最大ログレートを指定可能。

  効率向上のため CPU コア数分のスロットラーを起動。例：`log.throttling = 50,60s` なら、各スロットラーは同一メッセージ（モジュール名＋行番号判定）を1分間に最大50回まで制限。8コアなら最大400回/分。ログ破棄時は破棄数をログ出力。

  ```
  log throttled during last 60s, dropped_msg: #{{emqx_channel,1400} => #{msg => "Client ~s (Username: '~s') login failed for ~0p", count => 33}}
  ```

  デフォルトで警告以上のログに有効、設定は `50,60s`。

- HTTP/HTTPS 管理インターフェースのタイムアウト設定を追加。

  `etc/plugins/emqx_management.conf` に以下を追加。

  - `management.listener.<Proto>.request_timeout`：TCP 接続確立後、HTTP リクエスト未受信で切断するまでの時間（デフォルト5秒）。
  - `management.listener.<Proto>.idle_timeout`：HTTP リクエスト受信後のアイドルタイムアウト（デフォルト60秒）。

  `<Proto>` は `http` または `https`。

- 「Republish」アクションの送信先トピックの妥当性検証を追加。

  UTF-8 エンコードされていないバイナリトピックの場合、アクション失敗。

- Redis-Cluster ドライバーの性能を最適化。

  Redis Cluster アクセス時のメモリ使用量を削減。影響対象は Redis 認証／ACL、Redis プラグイン、ルールエンジンの Redis アクション。

### バグ修正

- Kafka サーバーの最大メッセージサイズ制限を超えるメッセージ送信で Kafka Producer がブロックする問題を修正。

  以前は Kafka サーバーの `message.max.bytes` を超える単一メッセージ送信時、ルールエンジンの Kafka Producer 送信キューがブロックし、以降メッセージが `Max Cache Bytes` までバッファされていました。現在はルールエンジンの Kafka リソースの `Max Batch Bytes` を超える単一メッセージは破棄され、キューブロックを防止。

- 一部状況で「リソースダウン」アラームが解除できない問題を修正。

- Will メッセージトリガールールの `timestamp` フィールド値が不正確だった問題を修正。

  フィールド値はクライアント接続時間ではなくルールトリガー時間を示すべき。

- Kafka アクションで `username` を `Message Key` に設定した場合の送信失敗を修正。

  以前は MQTT クライアントがログイン時にユーザー名を提供しないと送信失敗。修正後はユーザー名なしメッセージに対し `undefined` 文字列を Message Key として使用。

- ルールエンジンがプロセス異常終了時に `$events/client_disconnected` イベントをトリガーしない問題を修正。

- DynamoDB アクションで `clientid` を `Hash Key` に設定した場合の送信失敗を修正。

- ノード再起動後に共有サブスクリプションのスティッキー戦略がランダムに劣化する可能性を修正。

## 4.4.24

*リリース日: 2024-04-16*

### 強化点

- `/load_rebalance/availability_check` を認証不要のパブリック API に変更し、ロードバランサー設定を簡素化。API メソッドは可能な限り軽量化し、ブローカー過負荷を回避。

- リバランス／避難処理の待機ヘルスチェックフェーズをよりグレースフルに。

  避難対象ノードへの接続は禁止しません。このフェーズではロードバランサーがノードを不健康と判断しているか不明なため、接続禁止は再接続失敗を多発させる恐れがあります。

- HTTP API のアイドルタイムアウト問題を改善。

  以前は 5 秒に設定されていた `idle_timeout` を 60 秒に延長。HTTP API が 5 秒間リクエストを受けないと TCP 接続を切断していました。

- 一部設定項目の説明を改善。

  - Webhook リソース・アクションの `Base URL` と `Path` パラメータ説明を改善。
  - ホット設定の WS/WSS リスナーの `idle_timeout` 説明を改善。
  - オフラインメッセージ関連アクションの `Max Returned Count` 詳細説明を改善。
  - ClickHouse リソースの英語パラメータ `Key` を `Password` に名称変更。
  - HStreamDB アクションの `PartitionKey` を `Partition Key` に名称変更。
  - Retainer モジュールの `Maximum Retained Message Size` 詳細説明を `0B` から `0` に変更。
  - Retainer モジュールの `Message Interception` を `Intercept Empty Messages` に名称変更し説明改善。

- 一部設定値の範囲・妥当性チェックを追加。

  - `mqtt.max_topic_levels`、`mqtt.max_packet_size`、`keepalive_backoff` は非負値必須。
  - SSL リスナー起動時に `verify_peer`、`fail_if_no_peer_cert`、`cacertfile` の依存関係をチェック。以前は SSL クライアント接続時のみチェック。
  - リスナー設定の `acceptors`、`max_connections`、`max_conn_rate`、`active_n` は非負値必須。
  - RabbitMQ リソースの `Heartbeat Interval` と `Automatic Reconnection Interval` は正しい時間長文字列必須。
  - GB/T 32960 と JT/T808 リスナーの重複ポートチェックを修正。
  - GB/T 32960 と JT/T808 ゲートウェイ設定の一部パラメータ値検証を修正。

- ログフォーマットを最適化し、ダッシュボードの表示問題を修正。

  - ルールエンジンのレート値が長い浮動小数点数になる問題を小数点以下2桁に修正。
  - システムリソース使用アラートの CPU 使用率を小数点以下2桁に修正。
  - ログから `mfa` などのデバッグフィールドを削除。

- ルールエンジンでユーザー定義 SQL 関数をサポート。

  例：ユーザープラグインに `emqx_rule_funcs1` モジュールの `func` 関数があれば、以下のように利用可能。

  ```SQL
  SELECT emqx_rule_funcs1.func() FROM "t/#"
  ```

  モジュール名は `emqx_rule_funcs` または `EmqxRuleFuncs` で始まる必要あり。

- Kafka コンシューマグループモジュールが PLAIN、SCRAM_SHA_256、KERBEROS 認証をサポート。

- システムリソース消費が多い HTTP API メソッドに過負荷保護を追加。

  - `GET /api/v4/clients/*` 関連 API
  - `GET /api/v4/routes`
  - `GET /api/v4/subscriptions`
  - `GET /api/v4/rules`
  - `GET /api/v4/banned`
  - `GET /api/v4/audits`
  - 内蔵認証モジュールのユーザー名・クライアントID検索 API

### バグ修正

- うるう年の日付入力時にルールエンジンの `date_to_unix_ts()` SQL 関数が誤った値を返す問題を修正。

- ノード避難時に `clean_start = true` かつ非ゼロの `Session-Expiry-Interval` 設定のセッションを避難しない問題を修正。

- Redis 認証情報が Redis に存在しない場合の例外を修正。

- ノード起動時の HTTP API アクセスで未初期化の ETS テーブルによる例外を修正。

- 一部状況で拡張プラグインがロードできない問題を修正。

  `plugins.expand_plugins_dir` で指定したディレクトリのプラグインファイルが複数回ロードされ、EMQX 起動失敗を引き起こす場合あり。

- CLI で単一クライアントの ACL キャッシュをクリアできない問題を修正。

  例：`emqx ctl acl cache-clean 'mqttx_458d5222'` が機能しなかった。

- ルールリストや内蔵認証／認可リストのあいまい検索でページネーションが不完全でデータ表示が欠落する問題を修正。

- ログトレースファイルダウンロード時に「開始待ち」状態で誤ったエラーログが出力される問題を修正。

- ホット設定機能初回使用時に SSL リスナーの `backlog` 設定が空かつ必須と表示されフォーム送信失敗する問題を修正。

  修正後は `backlog` 設定は省略可能でデフォルト 1024。

- `emqx.schema` 設定ファイルの `zone` 関連バリデータが無効になる問題を修正。

- 監査ログクエリ API のレスポンスで HTTP ステータスコードが 500 でも `operation_result` が `success` のままになる問題を修正。

- ダッシュボードで GB/T 32960 クライアントIDの重複表示を修正。

## 4.4.23

*リリース日: 2023-11-24*

### 強化点

- EMQX ノード間のメッセージ送信性能を改善。

  `gen_rpc` は EMQX 内部でノード間 MQTT メッセージ送信に使われる RPC チャンネル。今回の改善で `gen_rpc` のバックログ処理能力を最適化し、トラフィックピークからの回復を高速化。

### バグ修正

- ルールエンジンが [upstash](https://upstash.com/) Redis に接続できない問題を修正。

  修正前は Redis サービスとの TCP 接続確立後、emqx の Redis ドライバーが Inline Commands で AUTH と SELECT コマンドを送信していたが、upstash Redis は Inline Commands 非対応で接続失敗。修正後は RESP（REdis Serialization Protocol）で送信。

- 「Offline Msg to Redis」アクションと Redis リソースの一部パラメータの妥当性チェックを追加。

  - 「Redis Key TTL」パラメータ
  - Redis リソースの「Redis Database」パラメータ

## 4.4.22

*リリース日: 2023-11-01*

### 強化点

- 重要操作変更を追跡する監査ログ機能を追加。

  - ダッシュボードの **General** → **Audit Log** → **Enable** で有効化し、パラメータを変更して **Add** をクリック。
  - 有効化後、`GET` 以外のすべての HTTP リクエストと CLI 実行を記録。
  - ダッシュボードで最新5,000件を表示可能。完全ログファイルは `data/audit` ディレクトリに保存。

- ダッシュボードに RBAC ロールを追加。

  ログイン時に「管理者」または「閲覧者」のいずれかのロールを割り当て可能。

  - 管理者：ダッシュボードの全機能に無制限アクセス。
  - 閲覧者：読み取り専用アクセス。情報閲覧は可能だが変更不可。

  RBAC により適切なアクセス権限管理とセキュリティ強化。

- LwM2M ゲートウェイが Block Wise Transfer を使ったダウンリンク送信をサポート。

- 新しい SQL 関数を追加：`map_keys()`, `map_values()`, `map_to_entries()`, `join_to_string()`, `join_to_sql_values_string()`, `is_null_var()`, `is_not_null_var()`。

- MQTT ブリッジ経由で転送するメッセージの QoS レベルを指定する `Forward QoS` 設定を追加。

- MQTT メッセージの有効期限を設定ファイルで指定可能に。

  詳細は `emqx.conf` の `mqtt.message_expiry_interval` 設定を参照。

- OCSP Stapling と CRL チェックの整合性向上のためスキーマ検証を追加。

### バグ修正

- Kafka クライアント（wolff）プロデューサーのクラッシュ問題を修正。

  ルール初期化時に Kafka リソースが誤って削除され、依存ルールが失敗しエラーが連鎖して全ルールがクラッシュする問題を防止。

- GB/T 32960 ゲートウェイモジュールが `retry_interval` パラメータを解析できない問題を修正。

- GB/T 32960 クライアントが HTTP API から取得できない問題を修正。

- OCPP クライアントの認証失敗時に例外ログが出る問題を修正。

- OCPP ゲートウェイが空の ClientID を検証しない問題を修正。

- RabbitMQ ドライバーをアップグレードし、いくつかのセキュリティ脆弱性を修正。

- ルールエンジンの GCP PubSub アクションで非同期送信モード時に統計カウンターが増加しない問題を修正。

- 手動でリソース再接続時に現在ノードのリソースのみ再接続される問題を修正。

- ルール削除・再インポート後にアクションの統計カウンターがリセットされない問題を修正。

- クラスター環境でルール再起動時にアクションリソースリークが発生する問題を修正。

  ルール停止・開始時に一部ノードでアクション作成失敗すると、アクション関連プロセスがリーク。

- マルチ CPU 環境でバッチモードの一部データ統合アクションの性能低下を修正。

  4.4.5 でバッチプロセスプールのワーカー数を CPU コア数×4 に変更した結果、CPU コア数が多いマシンでワーカー数が過剰となり、各プロセスがバッチ時間内に処理するメッセージ数が少なくなり性能低下。修正後はワーカー数をハードコードせず、`batch_pool_size` 設定（デフォルト8）を追加。

  対象アクション：`data_to_cassa`、`data_to_clickhouse`、`data_to_influxdb`、`data_to_iotdb`、`data_to_lindorm`、`data_to_mysql`、`data_to_oracle`、`data_to_pgsql`、`data_to_sqlserver`、`data_to_tablestore`、`data_to_tdengine`、`data_to_gcp_pubsub`。

- MQTT ブリッジで MQTT 5.0 プロトコル使用時に QoS2 メッセージ送信が失敗する問題を修正。

- 設定ファイルにリスナー設定がない場合、ホット設定更新が失敗する問題を修正。

- LwM2M ゲートウェイプラグインの起動失敗問題を修正。

  LwM2M モジュールを先に停止し、その後プラグインを起動すると起動失敗。ログに以下が出力されていました。

  ```
  {emqx_lwm2m,{bad_return,{{emqx_lwm2m_app,start,[normal,[]]},{'EXIT',{{already_started,<0.3895.177>},[...]}}}}}
  ```

- ダッシュボードで共有サブスクリプショントピックプレフィックスが正しく表示されない問題を修正。

  例：`$share/g//t` がクライアント詳細ページで `/t` と表示されていた問題を修正。

- 設定ファイルに `peer_cert_as_username` と `peer_cert_as_clientid` の `none` オプションを追加。

  これらはクライアント証明書内容をユーザー名／ClientID として使用する設定。

- ホット設定機能有効時にリスナーが時折再起動する問題を修正。

- 実行中のルール停止時にエラーが発生する問題を修正。

  修正前はアクションが未初期化またはクリア済みの場合に以下のようなエラーログが出ていました。

  ```
  foo@x.x.x.x:54663 Rule: <<"rule:ba48182b">>; Action: data_to_kafka; Resource: <<"resource:7bacacdc">>. Continue next action, reason: {error,{badmatch,not_found}, ...
  ```

- LwM2M ゲートウェイの DTLS PSK ハンドシェイク失敗問題を修正。

- Retainer モジュールの設定に不正フィールドチェックを追加。

  `Max Retained Messages` と `Max Retained Payload Size` が非負値であることを検証。

- ホットアップデート後に TDEngine へのメッセージ送信失敗問題を修正。

- ホットアップデート後に RabbitMQ リソースが利用不能になる問題を修正。

- OCSP Stapling 無効化または TLS リスナー無効化時に HTTP リフレッシュタイマーをキャンセル。

- CRL チェック無効化または TLS リスナー無効化時に CRL リフレッシュタイマーをキャンセル。

## 4.4.21

*リリース日: 2023-10-16*

### 強化点

- Confluent データブリッジを追加。

- Kafka コンシューマグループの MQTT トピックフィールドでプレースホルダーをサポート。

  例：Kafka で消費したメッセージのキーが "a" で、設定された MQTT トピックが `topic/${key}` の場合、転送時に `topic/a` に置換。

- 「Message Republish」アクションに「MQTT Properties」と「User Properties」フィールドを追加。

  両方ともキー・バリュー形式で、キーと値にプレースホルダーを使用可能。

### バグ修正

- Kafka アクションで数値を Kafka ヘッダーとして送信できない問題を修正。

  修正前は「Kafka Headers value encode mode」が "NONE" の場合、JSON オブジェクト内の数値型が無視されていました。修正後は数値を文字列に変換して送信。

## 4.4.20

*リリース日: 2023-08-01*

### 強化点

- Kafka と HStreamDB へのデータ送信性能を改善。

  ドライバープロセス前に Erlang メッセージバッファを追加し、EMQX 内部のメッセージ送受信回数を削減。これによりメッセージレイテンシは増加するものの、スループットが大幅に向上。

  EMQX から Kafka/HStreamDB ドライバーへのメッセージはまずバッファに入り、`message_accumulation_size` に達するか `message_accumulation_interval` が経過するとバッチ送信。`message_accumulation_size = 0` でバッファリング機能を無効化。

- SQL Server リソースに `auto_reconnect` オプションを追加。

  以前は EMQX と SQL Server 間の接続断時に自動再接続できなかったが、追加により自動再接続可能に。`auto_reconnect = false` で無効化可能。

- RabbitMQ リソースに TLS 接続サポートを追加。

- GCP PubSub アクションで属性定義とオーダリングキーを設定可能に。

### バグ修正

- ルールエンジンの `mongo_date()` 関数がダッシュボードの SQL テストページでテストできない問題を修正。

- ホットアップグレード後、ルールエンジンの RabbitMQ アクションでメッセージ送信に失敗する問題を修正。

## 4.4.19

*リリース日: 2023-06-27*

### 強化点

- MQTT/TCP と MQTT/SSL リスナーで TCP キープアライブをサポート [#10854](https://github.com/emqx/emqx/pull/10854)。

  新設定 `zone.<zone-name>.tcp_keepalive = Idle,Interval,Probes` で TCP レイヤのキープアライブを有効化し、時間パラメータを指定可能。Linux と MacOS のみ有効。

- Proxy Protocol 関連のエラーログを改善 [emqx/esockd#177](https://github.com/emqx/esockd/pull/177)。

  改善前ログ例：

  ```
  2023-04-20T14:56:51.671735+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {invalid_proxy_info,<<"f\n">>}, offender: [{pid,<0.3192.0>},{name,connection},{mfargs,{...}}]

  2023-04-20T14:57:01.348275+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {proxy_proto_timeout,5000}, offender: [{pid,<0.3194.0>},{name,connection},{mfargs,{...}}]
  ```

  改善後ログ例：

  ```
  2023-04-20T18:07:06.180134+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but received invalid proxy_protocol header, raw_bytes=<<"f\n">>

  2023-04-20T18:10:17.205436+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but timed out while waiting for proxy_protocol header
  ```

- TLS リスナーで部分証明書チェーン検証を有効化する新機能を追加 [#10553](https://github.com/emqx/emqx/pull/10553)。

  詳細は `listeners.conf` の `listener.ssl.external.partial_chain` を参照。

- TLS リスナーでクライアント証明書の拡張キー使用法検証を有効化する新機能を追加 [#10669](https://github.com/emqx/emqx/pull/10669)。

  詳細は `listeners.conf` の `listener.ssl.external.verify_peer_ext_key_usage` を参照。

- HTTP API `/api/v4/nodes` のレスポンスに `live_connections` フィールドを追加 [#10859](https://github.com/emqx/emqx/pull/10859)。

  これまでは `connections` フィールドが切断済みでも永続セッションがあればカウントしていたが、`live_connections` は切断されていない MQTT 接続中のクライアント数を示す。

- ルールエンジンにランダム関数を3つ追加 [#11113](https://github.com/emqx/emqx/pull/11113)。

  - `random()`：0以上1未満の乱数生成。
  - `uuid_v4()`：ハイフン付きのランダム UUID（バージョン4）文字列生成。
  - `uuid_v4_no_hyphen()`：ハイフンなしのランダム UUID（バージョン4）文字列生成。

- `mqtt.max_clientid_len` 設定パラメータに数値範囲検証（23-65535）を追加 [#11096](https://github.com/emqx/emqx/pull/11096)。

- プラグイン `emqx_gcp_device` を追加。

  Google IoT Core からの移行を簡素化：

  - Google IoT Core のデバイス設定と認証データのインポートを可能に。
  - Google IoT Core 互換の MQTT 認証を実装。
  - デバイス設定と認証データ管理用 API エンドポイントを提供。

- RabbitMQ アクションで動的ルーティングキーをサポート。

  ルーティングキーに `${key}` 形式の動的変数を使用可能。

- DynamoDB リソースのデフォルトポートを追加。

  以前はポート指定なしの URL でリソース作成が失敗していたが、ポート未指定時は HTTP 80 または HTTPS 443 をデフォルト使用。

### バグ修正

- ルールエンジンのトークンバケットアルゴリズム実装を修正し、実際の最大レートが設定値より常にやや高い問題を修正。

- ルールエンジンの `Data to InfluxDB` アクションがホットアップグレード後に失敗する問題を修正。

- ルールエンジンの `Data to Cassandra` アクションでバッチ送信時に過負荷保護が正しく動作しない問題を修正。

- ルールエンジンの `Data to Kafka` アクションでメッセージキーに `username` を指定し、クライアントにユーザー名がない場合に送信失敗する問題を修正。

- ルールエンジンの `$events/client_disconnected` イベントがプロセス異常終了時にトリガーされない問題を修正。

- DynamoDB アクションで `clientid` をハッシュキーに指定した場合の送信失敗を修正。

- ノード再起動後に共有サブスクリプションのスティッキー戦略がランダムに劣化する問題を修正。

---

（以下、同様の形式で以降のバージョンも翻訳されていますが、文字数制限のためここまでとします。）
