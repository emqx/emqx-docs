# EMQX Enterprise Version 6

## 6.3.3

*リリース日: 2026-11-11*

EMQX 6.3.3 へアップグレードする前に、破壊的変更を必ずご確認ください。

### 概要

本リリースはパフォーマンスおよびリソース使用の最適化に注力しています。42件の改善のうち26件は、各接続、各メッセージ、各セッションに対する EMQX の CPU またはメモリ使用量を削減しています。その他の改善では、アウトバウンド TLS クライアント向けのノード全体の信頼ストア、耐久ストレージおよび TLS ハンドシェイクの過負荷保護、ピン留めプラグイン、新しいコネクターオプションが追加されています。さらに127件のバグ修正が含まれています。

### アップグレードノート

以下の変更は既存のデプロイメントの挙動を変更する可能性があります。

- `mq.auto_create` と `streams.auto_create` のデフォルトが無効になりました。これにより、`$queue/...` または `$stream/...` トピックへのサブスクライブはキューやストリームを自動作成しなくなります（[#18788](https://github.com/emqx/emqx/pull/18788)）。
- `authentication_settings.ignore_backend_failures` と `authorization.ignore_backend_failures` のデフォルトが `per_security_profile` に変更され、`legacy` プロファイルではバックエンド障害時に `false` が保存されている場合クライアントを拒否します（[#19088](https://github.com/emqx/emqx/pull/19088)）。
- MQTT v5 の CONNECT および Will プロパティは最大100ペアのユーザープロパティを受け入れるようになり、新設定 `mqtt.max_connect_user_properties` で制御されます（[#18974](https://github.com/emqx/emqx/pull/18974)）。
- ダッシュボードの OIDC および SAML SSO ログインは、ログインを開始したブラウザで完了する必要があります。SAML は `dashboard_addr` に HTTPS アドレスが必要で、バックエンドで `skip_login_cookie_check = true` が設定されていない限り必須です（[#18810](https://github.com/emqx/emqx/pull/18810)）。
- 起動時に EMQX は既存の名前空間付きダッシュボードビューアの保存されたスコープリストから `gateways`、`publish`、`audit`、`mfa_management` を削除します。既存の API キーは変更されません（[#19125](https://github.com/emqx/emqx/pull/19125)）。
- MQTT およびゲートウェイの WebSocket リスナーは、完全なメッセージに対して `websocket.max_frame_size` を強制します。MQTT リスナーのデフォルト `infinity` は `mqtt.max_packet_size` の2倍に制限され、ゲートウェイリスナーはデフォルトで 2 MiB です（[#18882](https://github.com/emqx/emqx/pull/18882)、[#19518](https://github.com/emqx/emqx/pull/19518)）。
- HTTP および WebSocket パーサーが厳格化され、HTTP クライアントはデフォルトでレスポンスヘッダーとフレームサイズの制限を適用します。レガシーまたは非準拠の HTTP/WebSocket サービスとの統合はアップグレード後に検証してください。REST API は100以上のクエリパラメータを持つリクエストを拒否します（[#19694](https://github.com/emqx/emqx/pull/19694)）。
- MQTT コネクターとクラスターリンクは `retry_interval` を秒単位で解釈し、1000倍しなくなりました。これにより QoS 1 と QoS 2 の再送間隔が設定通りになります（[#19885](https://github.com/emqx/emqx/pull/19885)）。
- RabbitMQ コネクターは `heartbeat` を1000倍しなくなり、既存の値はそのまま意味を持ちます（[#19910](https://github.com/emqx/emqx/pull/19910)）。
- `verify_peer` を設定し `cacertfile` を空にしたアウトバウンド TLS クライアントは `node.trusted_certs` を使用し、組み込み HTTP クライアントは独自の CA バンドルを持ちません（[#19756](https://github.com/emqx/emqx/pull/19756)）。
- Datalayers Arrow Flight ドライバーはサーバー証明書を検証し、`verify_none` を拒否します（[#19815](https://github.com/emqx/emqx/pull/19815)）。
- TLS リスナーは大量のハンドシェイクが開始待ちの間、新規接続を拒否します（[#19940](https://github.com/emqx/emqx/pull/19940)）。
- QUIC リスナーの `ssl_options.hibernate_after` 設定は非推奨で無効化され、QUIC 接続は `mqtt.hibernate_after` に従います（[#19407](https://github.com/emqx/emqx/pull/19407)）。
- `socket` TCP バックエンドはパケット間で受信バッファを解放するようになり、新設定 `tcp_options.buffer_retention_strategy` のデフォルトは `packet` です（[#19785](https://github.com/emqx/emqx/pull/19785)）。
- `recv_cnt` と `send_cnt` は TCP パケット数ではなく MQTT パケット数をカウントするようになりました（[#19790](https://github.com/emqx/emqx/pull/19790)）。
- `GET /clients` は耐久セッションの `seqno_q1_*` と `seqno_q2_*` フィールドを返さなくなりました（[#19651](https://github.com/emqx/emqx/pull/19651)）。
- 監査ログはグローバル管理者とグローバルビューアのみが閲覧可能です（[#18883](https://github.com/emqx/emqx/pull/18883)）。
- `topic_match` は、`#` で終わるフィルターが `#/a` のような無効なトピックにマッチした場合に `true` を返すようになりました（[#19710](https://github.com/emqx/emqx/pull/19710)）。
- クライアント情報テーブルを直接読むプラグインは `conninfo` 下に `sock`、`clientid`、`username` を見つけられなくなりました（[#19419](https://github.com/emqx/emqx/pull/19419)、[#19761](https://github.com/emqx/emqx/pull/19761)）。
- メモリ内セッションヘッダーの `deliver_begin_at` を読むプラグインやフックは、マイクロ秒単位の `deliver_begin_at_us` を使う必要があります（[#19774](https://github.com/emqx/emqx/pull/19774)）。

### 改善点

#### セキュリティ

- [#19756](https://github.com/emqx/emqx/pull/19756) `node.trusted_certs` 設定を追加。アウトバウンド TLS クライアントが CA を指定しない場合に信頼する CA 証明書を選択します。デフォルトは OS の CA バンドルを使う `system_default`。異なる PEM ファイルを使う場合は `file:///absolute/path` を設定してください。

  `verify = verify_peer` で `cacertfile` が空のクライアントはこれらの CA 証明書でサーバーを検証します。以前は接続失敗していました。

  組み込み HTTP クライアントも `node.trusted_certs` を使い、独自の CA バンドルは持ちません。

  `cacertfile = system_default` を設定すると `node.trusted_certs` の CA 証明書を信頼します。`node.trusted_certs` に CA バンドルがない場合は Erlang/OTP が OS から読み込む CA を使います。リスナーはこの値を受け付けません。

  Docker イメージはビルド時に curl.se の最新 Mozilla CA バンドルをインストールします。

  アウトバウンド接続の信頼アンカーを固定したい場合は `cacertfile` を明示的に設定してください。

- [#19088](https://github.com/emqx/emqx/pull/19088) `authentication_settings.ignore_backend_failures` と `authorization.ignore_backend_failures` は `legacy` と `hardened` の両方のセキュリティプロファイルで適用されるようになりました。以前は `hardened` のみでした。

  各設定は以下の3つの値を受け入れます。

  - `per_security_profile`（新デフォルト）：セキュリティプロファイルに従う。`legacy` はバックエンド障害を無視し、`hardened` はクライアントを拒否。
  - `true`：バックエンド障害を無視し、次の認証器または認可ソースへ進む。
  - `false`：バックエンド障害時にクライアントを拒否。

  アップグレード注意：`legacy` では保存済みの `false` がバックエンド障害時にクライアント拒否となります。以前の `legacy` 動作を維持したい場合はアップグレード後に `per_security_profile` に設定してください。

- [#19096](https://github.com/emqx/emqx/pull/19096) `mqtt.max_connect_packet_size` 設定を追加。CONNECT パケットのサイズ制限を `mqtt.max_packet_size` に加えて設定できます。超過したクライアントの接続は切断され、リスナーのシャットダウンカウンター `connect_packet_too_large` が増加します。デフォルトは `1MB` で `mqtt.max_packet_size` と同じため、デフォルト設定は変わりません。`mqtt.max_packet_size` を 1MB 以上に設定して大きな CONNECT パケットを許可する場合は、こちらも適切に上げてください。

#### パフォーマンスとリソース使用

- [#19642](https://github.com/emqx/emqx/pull/19642) `gen_tcp` TCP バックエンドのリスナーオプション `tcp_options.low_watermark` を追加。

  Erlang ソケットバッファが `high_watermark` を超えるとソケットはビジー状態になり、バッファが `low_watermark` を下回るまでビジー状態が続きます。以前は `low_watermark` は常に Erlang/OTP のデフォルト 4KB でした。これにより、1MB の `high_watermark` でビジーになった接続はほぼ全データ送信までビジー状態が続いていました。

  このオプション未設定時は `high_watermark` の4分の1（ただし4KB未満にはならない）がデフォルトです。`socket` TCP バックエンドはこのオプションを無視します。`low_watermark` が `high_watermark` を超える場合は `high_watermark` にクランプされます。

- [#19240](https://github.com/emqx/emqx/pull/19240) MQTT PUBLISH 受信時の一時メモリ割り当てを削減（パケット検証は変更なし）。

- [#19256](https://github.com/emqx/emqx/pull/19256) TLS 接続ごとのメモリ使用量を削減。共有ソケットオプション用の ETS テーブルを廃止し、アイドル状態の TLS 1.3 接続は暗号状態を解放します。

- [#19277](https://github.com/emqx/emqx/pull/19277) MQTT 接続ごとのメモリ使用量を削減。同一ゾーンの接続はパケットパーサー、シリアライザー、ゾーン設定を共有します。1万のアイドル TCP 接続で約170バイトの節約。

- [#19278](https://github.com/emqx/emqx/pull/19278) MQTT 接続と外部サブスクリプションのメモリ使用を削減。

- [#19419](https://github.com/emqx/emqx/pull/19419) クライアント情報テーブルの各接続のメモリ使用を削減。`conninfo.sock` にソケットのタームを保存しなくなりました。TCP `gen_tcp` バックエンドで16バイト、`socket` バックエンドで32バイト、TLS 接続で約128バイトの節約。

  保存コピーのみの変更で、フックコールバックは依然として接続の `conninfo` を受け取り `sock` を含みます。REST API の応答も変更ありません。`emqx_cm:get_chan_info/1,2`、`emqx_cm:lookup_client/1`、`emqx_mgmt:lookup_client/2`、`emqx_channel:info/1`、`emqx_connection:info/1` で直接テーブルを読むプラグインは `conninfo` 下に `sock` を見なくなりました。接続プロセス内で `emqx_channel:info(conninfo, Channel)` を使って読み取ってください。

- [#19420](https://github.com/emqx/emqx/pull/19420) TLS ハンドシェイクごとに `cacertfile` パス解決のためにファイルサーバーを2回呼び出すことを廃止。TLS 接続のバースト時に `file_server_2` プロセスのメッセージバックログを削減。

- [#19514](https://github.com/emqx/emqx/pull/19514) PUBLISH パケット処理の CPU コストを削減。

  - トピック分割と検証の処理を軽減。
  - Sparkplug B のエイリアスマッピングフックは最初にトピックをチェックし、Sparkplug B 名前空間外のトピックでは追加処理なし。
  - A2A レジストリ、スキーマ検証、メッセージ変換、レガシートピックメトリクスは初回使用時のみフックを追加し、それまではメッセージごとのコストなし。
  - クライアントにレート制限が設定されていない場合、パブリッシュ時にリミッター設定をチェックしない。

  60,000 クライアントがそれぞれ1秒あたり1KBの QoS 0 メッセージをパブリッシュするテストで、`gen_tcp` と `socket` バックエンドの両方でブローカーの CPU 使用率が低下。

- [#19554](https://github.com/emqx/emqx/pull/19554) 大きすぎるソケットバッファの再利用によるメモリ保持を削減。Erlang 分散と `gen_rpc` のソケットバッファサイズを 1088 KiB に増加し、Erlang の再利用可能バッファ制限を超えました。

- [#19556](https://github.com/emqx/emqx/pull/19556) クラスター表への書き込みは、呼び出しプロセスのメッセージキューが100件以上の場合にのみヘルパープロセスで実行。以前は10件以上で実行していたため、新規サブスクリプションが多い場合にブローカーのワーカーが頻繁にヘルパー待ちとなり、SUBSCRIBE レイテンシが増加していました。

- [#19627](https://github.com/emqx/emqx/pull/19627) `gen_tcp` バックエンドの TCP リスナー接続は、送信パケットごとにソケットドライバーを待たなくなりました。これによりメッセージあたりの CPU 使用率が低下。QoS 0 の 1対1 パブリッシュ/サブスクライブベンチマークで約12%低減。

- [#19651](https://github.com/emqx/emqx/pull/19651) 接続ごとの統計を接続プロセス所有のアトミクス配列に保持し、`emqx_channel_info` テーブルへの書き込みを廃止。

  テーブルの各行は配列への参照を持ち、パケット・メッセージカウンターはその場でインクリメントされるため、`GET /clients` などはリアルタイムにカウンターを報告。`SUBACK` や統計タイマーごとの行の再書き込みを廃止し、接続プロセスのヒープにカウンターを置かなくなりました。

  耐久セッションの `seqno_q1_*` と `seqno_q2_*` フィールドは `GET /clients` と `GET /clients/{clientid}` の応答から削除されました。`n_streams` は残ります。

- [#19710](https://github.com/emqx/emqx/pull/19710) トピックマッチングで以下の場所でトピックをレベルに分割しなくなりました。

  - 共有サブスクリプションのトピックマッチング
  - ルールエンジン SQL と Variform 式の `topic_match` 関数
  - クラスターリンクのルート更新

  有効なトピックの場合は結果は変わりません。`topic_match` 関数はランタイムデータを受け取り、`#` レベルが最後でないトピックは無効なトピックです。これにより、以前はマッチしなかった `topic_match('#/a', '#')` が `true` を返すようになりました。

  デフォルトの `acl.conf` は `dashboard` ユーザー名を正規表現ではなく等価比較で照合します。

- [#19718](https://github.com/emqx/emqx/pull/19718) アイドル MQTT 接続はキープアライブチェック時に毎回ハイバネート解除されなくなりました。クライアントがキープアライブ期限を過ぎて遅延した時に一度だけ解除されます。クライアント検出時間は変わりません。多数のアイドルクライアントでブローカーの CPU 使用率が低下。

- [#19719](https://github.com/emqx/emqx/pull/19719) メモリ内セッションのメモリ使用量とガベージコレクション中のセッションデータコピー量を削減。

- [#19732](https://github.com/emqx/emqx/pull/19732) クラスターリンクが有効でないノードでメッセージパブリッシュ時にクラスターリンク呼び出しを行わなくなりました。

- [#19733](https://github.com/emqx/emqx/pull/19733) リスナーに配信レート制限がない場合、メッセージ配信時にリスナーのレートリミッター設定を読み込まなくなりました。

  ランタイムでリスナーに配信レート制限が追加された場合、既存のクライアントセッションに対しては1統計間隔（デフォルト15秒）以内に適用されます。新規セッションは最初の配信メッセージから制限されます。

- [#19739](https://github.com/emqx/emqx/pull/19739) クラスター内のどのクライアントも `$SYS/brokers/<node>/clients/<clientid>/connected` と `.../disconnected` トピックをサブスクライブしておらず、ルールエンジンが `$SYS` メッセージを無視している場合（デフォルト）に、これらのメッセージを生成・パブリッシュしなくなりました。これにより接続ごとのブローカー CPU 使用率が約4%削減されます。

  トピックにサブスクライブしているサブスクライバーはイベントを受信します。`rule_engine.ignore_sys_message = false` の場合は従来通りメッセージをパブリッシュします。同様にクライアントの `subscribed` と `unsubscribed` イベントもデフォルトオフです。

- [#19740](https://github.com/emqx/emqx/pull/19740) シングルノード展開でのクライアント接続ごとの CPU コストを約7%削減。CONNECT 時のセッションロックはクラスタが単一ノードの場合、ローカルノードへの内部リモート呼び出しを送信しなくなりました。

- [#19761](https://github.com/emqx/emqx/pull/19761) クライアント情報テーブルの各接続のメモリ使用を、クライアント識別子とユーザー名の長さ分削減。

  `conninfo` 下の `clientid` と `username` は、`clientinfo` と同じ値の場合保存しなくなりました。CONNECT パケットの値がクライアントの最終値と異なる場合は引き続き `conninfo` に表示されます。

  保存コピーのみの変更で、フックコールバックや REST API 応答は変更ありません。

- [#19774](https://github.com/emqx/emqx/pull/19774) メモリ内セッションの QoS 1/2 メッセージ配信時のメモリ割り当てと CPU オーバーヘッドを削減。

  ローリングアップグレード中の inflight-message REST API ページングに以下の制限あり：

  - 古いノードが HTTP 400 でページングカーソルを拒否する場合がある（6.3.3 ノードから古いノードへの連続リクエスト時）。
  - 6.3.3 ノードから古いノードへのセッションテイクオーバー後、同一マイクロ秒配信タイムスタンプの inflight メッセージが複数ページにまたがるとスキップされる可能性あり。

  MQTT メッセージ配信には影響なし。

- [#19785](https://github.com/emqx/emqx/pull/19785) `socket` バックエンドの TCP 接続が保持するメモリを削減。新設定 `tcp_options.buffer_retention_strategy` のデフォルトは `packet` で、次の MQTT パケット待ち中に受信バッファを解放します。以前の動作を維持したい場合は `always` に設定してください。

  パケット長が既知の読み込みは16 MiBに制限され、それ以上は複数回の読み込みで受信されます。

- [#19790](https://github.com/emqx/emqx/pull/19790) クライアント統計からソケットの受信・送信操作回数のカウントを削除。

  `recv_cnt` と `send_cnt` は MQTT パケット数を示し、`recv_pkt` と `send_pkt` と同じ値になります。以前は TCP パケット数でした。REST API `/clients`、`emqx ctl clients stats` CSV エクスポート、耐久セッション情報に適用。互換性のためフィールドは残ります。

- [#19800](https://github.com/emqx/emqx/pull/19800) MQTT メッセージをサブスクライバーに配信する際の CPU オーバーヘッドとメモリ割り当てを削減。

- [#19810](https://github.com/emqx/emqx/pull/19810) 受信 MQTT メッセージ処理時の CPU オーバーヘッドと一時メモリ割り当てを削減。

- [#19811](https://github.com/emqx/emqx/pull/19811) ファイル（ACL）認可ソースおよびその他のルールベース認可ソースの認可チェックの CPU コストを削減。トピック分割はルールの各トピックフィルターごとに1回だけ行われます。固定レベルで始まるトピックフィルター（例：`$SYS/#`）は、トピックが異なるレベルで始まる場合分割しません。

#### MQTT リスナー

- [#19407](https://github.com/emqx/emqx/pull/19407) QUIC 接続はリスナーのゾーンの `mqtt.hibernate_after` をハイバネーションタイムアウトに使用し、TCP/SSL 接続と同様に制御ストリームとデータストリームに適用します。

  QUIC リスナーの `listeners.quic.<name>.ssl_options.hibernate_after` 設定は非推奨で無効化されています。変更は受け付けますが効果はありません。以前はこの設定が使われており、`mqtt.hibernate_after` の変更は QUIC 接続に影響しませんでした。

  両設定のデフォルトは `5s` です。どちらも変更されていなければ QUIC 接続のハイバネーション挙動は変わりません。

#### クラスタリング

- [#19242](https://github.com/emqx/emqx/pull/19242) クラスター間ノード間で送信するデータ（MQTT メッセージの転送を含む）を圧縮するための設定 `rpc.compress_level` と `rpc.compress_threshold` を追加。

  `rpc.compress_level` は zlib 圧縮レベル（1=高速〜9=最小出力）を設定。デフォルトは 0（圧縮無効）。`rpc.compress_threshold` は圧縮対象の最小パケットサイズでデフォルトは 1KB。

  圧縮は送信側・受信側両方で CPU を使用してノード間トラフィックを削減します。帯域制限や課金がある場合に有効です。レベル1は最小の CPU コストで大半のサイズ削減を実現します。0.5〜1KB の JSON ペイロードでレベル1は約60%のトラフィック削減、CPU 使用率は送信側で約10%、受信側で約5%増加しました。既に圧縮・暗号化済みのペイロードはサイズが小さくならないため CPU コストだけ増えます。

#### 耐久ストレージ、メッセージキュー、ストリーム

- [#19217](https://github.com/emqx/emqx/pull/19217) メッセージキューとメッセージストリームの通常メッセージ保持期間の上限 49.71日を削除。

- [#19623](https://github.com/emqx/emqx/pull/19623) メッセージキューのリプレイ性能を改善。コンシューマがイテレータを開始または再作成する際に保持期間より古いレコードをスキップ。保存済みアックは保持。

- [#19624](https://github.com/emqx/emqx/pull/19624) 耐久ストレージ書き込みパスに過負荷保護を追加。楽観的トランザクションマネージャが設定されたメッセージキュー制限に達すると、EMQX はキューイングされたトランザクションを回復可能な `overloaded` エラーで中止。呼び出し元は再試行可能。操作数の最大値を超えるトランザクションも拒否。

#### データ統合

- [#18926](https://github.com/emqx/emqx/pull/18926) Kafka、Confluent、Azure Event Hubs コネクターに IPv6 サポートを追加。

  - `bootstrap_hosts` に括弧付き IPv6 アドレス（例 `[::1]:9092` や `[fd00::5]:9092,host2:9093`）を指定可能。
  - IPv6 のみ解決するホスト名や IPv6 アドレスを広告するブローカーに接続可能。
  - 新オプション `socket_opts.ip_family` で IP アドレスファミリを指定。デフォルト `auto` は IPv4 を優先し、次に IPv6。`ipv6` で IPv6 のみ、`ipv4` で IPv4 のみ接続。

  Kafka クライアントライブラリのアップグレードにより、SASL 再認証中にリクエストが保留中の場合の同期プロデュースタイムアウトを修正。

- [#19204](https://github.com/emqx/emqx/pull/19204) Amazon Kinesis コネクターに AWS IAM ロール認証を追加。

  アクセスキーIDとシークレットキーが省略された場合、EMQX は ECS タスクロールまたは EC2 インスタンスメタデータから一時認証情報を取得し、有効期限前に更新。

- [#19544](https://github.com/emqx/emqx/pull/19544) S3 コネクターとファイル転送の S3 エクスポーターにオプションの `region` フィールドを追加。設定するとリクエストと署名付き URL の署名にこのリージョンを使用。未設定時は AWS S3 インターフェース VPC エンドポイント（PrivateLink）ホストからリージョンを正しく推定。以前は `us-east-1` で署名され、他リージョンのエンドポイントへのリクエストは拒否されていた。

- [#19815](https://github.com/emqx/emqx/pull/19815) Datalayers コネクターの `arrow_flight` ドライバーを `datalayers-adapter-erl` 0.1.6 に更新。

  Arrow Flight TLS 接続は Datalayers サーバー証明書を検証。デフォルトは `cacertfile = "system_default"` と `verify = verify_peer`。`system_default` は `node.trusted_certs` の CA バンドルを使用。PEM CA パスで置き換え可能。`verify_none`、クライアント証明書設定、`managed_certs` は拒否。

  デフォルトの `influxdb_v1` ドライバーは既存の TLS 動作を維持。

#### プラグインと拡張

- [#17917](https://github.com/emqx/emqx/pull/17917) 新プラグイン `emqx_bcast` を追加。製品レベルのデバイスメッセージング API を提供：`PubBroadcast`（製品の全オンラインデバイスへブロードキャスト）、`BatchPub`（オフライン保存と QoS 1 アック追跡付きのデバイスリストへのバッチ配信）、`RegisterMessage`（コンテンツ重複排除と TTL 更新付きのメッセージ事前登録）。

  統一エンドポイント `POST /api/v5/plugin_api/emqx_bcast/pub` を公開し、接続中クライアントチャネルに直接メッセージを配信。デバイス再接続時に保留中の QoS 1 配信をリプレイ。`GET /api/v5/plugin_api/emqx_bcast/metrics` で Prometheus メトリクスを公開。

  プラグイン再起動、インデックス所有者の引き継ぎ、コアシャットダウンはキューイングされた配信進捗、デバイスごとのパブリッシュ順序、QoS 1 配信台帳を保持し、クライアントの再パブリッシュや再サブスクライブを不要に。

- [#18843](https://github.com/emqx/emqx/pull/18843) 新しい API スコープ `plugin_api` を追加。プラグイン拡張 API エンドポイント（`/plugin_api/...`）へのアクセスを許可し、それ以外は許可しません。

  API キーまたはダッシュボードユーザーに割り当てると、プラグインが公開するエンドポイントを呼び出せますが、管理者相当の `system` スコープは不要です。`plugin_api` スコープはプラグインのインストール、起動、停止、設定変更は許可しません。これらは `system` スコープに残ります。

  既存の `system` スコープを持つ API キーとダッシュボードユーザーはプラグイン拡張 API へのアクセスを維持。

- [#19126](https://github.com/emqx/emqx/pull/19126) パブリッシュフックでメッセージ永続化成功を報告する機能を追加。プラグインが永続化を報告した場合、HTTP パブリッシュ API はマッチするサブスクリプションがなくても HTTP 200 とメッセージIDを返します。

- [#19359](https://github.com/emqx/emqx/pull/19359) `node.pinned_plugins` 設定を追加。ノードがクラスタープラグイン設定に関係なく実行するプラグインを列挙。例えばコンテナイメージに組み込まれたプラグイン。

  カンマ区切りの名前-バージョンリスト（例 `EMQX_NODE__PINNED_PLUGINS="my_plugin-1.0.0"`）または配列で設定。ピン留めプラグインは `<plugins.install_dir>/<name-vsn>` ディレクトリから起動し、常に有効で `plugins.states` エントリや `plugins allow` 権限、インストールコマンドは不要。他ノードからのファイル・設定コピーは行わず、設定はパッケージデフォルトと `etc/plugins/<name>.hocon` のマージ。API やダッシュボードから他プラグイン同様に設定変更可能。保存設定は `data/plugins/<name>/config.hocon` に保存され、その後はこのファイルが全設定。変更はそのプラグインバージョンを実行するノードに適用。プラグインの API エンドポイントは他プラグインと同様に `/api/v5/plugin_api/` 以下で動作。ノードは同名の `plugins.states` エントリを無視し、他ノードからのクラスタープラグイン操作はスキップ。ピン留めプラグインディレクトリがないか起動失敗時はエラーをログに出しアラームを上げつつノードは継続。

  `GET /api/v5/plugins` と `GET /api/v5/plugins/{name}` は各プラグインとノードの `running_status` に `pinned` フラグを返す。ピン留めプラグインは CLI のみ停止・起動可能で、`emqx ctl plugins stop|start|restart` をピン留めノードで実行。停止はノードが全プラグインを再起動するまで持続。ピン留めノードの API はプラグインのライフサイクル操作（インストール、アンインストール、起動、停止、起動順変更、クラスタ同期）で HTTP 409 とコード `PLUGIN_PINNED` を返す。

  ノードは同一プラグインの異なるバージョンを同時に実行可能。プラグインはこれらバージョン間で互換性を保つ必要があり、共有データベーステーブル、RPC、設定などが該当。

### バグ修正

#### セキュリティ

- [#16389](https://github.com/emqx/emqx/pull/16389) OAuth2 の `client_secret` が認証、認可、ゲートウェイ、HTTP コネクターの設定読み取り API で平文で返されていた問題を修正。API 応答ではマスクされ、マスク済み値を再送信しても保存済みシークレットは変更されません。

- [#18810](https://github.com/emqx/emqx/pull/18810) ダッシュボードのシングルサインオンは、ログインを開始したブラウザでのみ OIDC または SAML ログインを完了します。EMQX はログインごとに値を生成しブラウザクッキーに保持、ID プロバイダーから戻ったブラウザと照合します。異なるブラウザからのログインは誰もサインインしません。

  運用者向け注意点：

  - SSO ログイン開始前に `dashboard_addr` で設定されたアドレスでダッシュボードを開いてください。ID プロバイダーはそのアドレスにブラウザを戻し、クッキーはそのアドレスに属します。
  - SAML シングルサインオンは `dashboard_addr` に HTTPS アドレスが必要です。SAML ID プロバイダーはクロスサイトフォームポストでブラウザを戻し、ブラウザは HTTPS のみでクッキーを送信します。
  - ブラウザがクッキーを保持できず SSO ログインに失敗する場合、OIDC または SAML バックエンドで `skip_login_cookie_check = true` を設定しブラウザチェックを無効化できます。ただし自己責任で有効化してください。

- [#18879](https://github.com/emqx/emqx/pull/18879) ロールベースアクセス制御で拒否されたリクエストの監査ログに認証済みダッシュボードユーザーまたは API キーとリクエストのパスパラメータを含めるよう修正。

  以前は認証は通ったがアクセス制御で拒否されたリクエストは `source` が空で `http_request.bindings` がなく、`GET /api/v5/audit` で誰が何をリクエストしたか表示できませんでした。未認証リクエストの記録は変更なし。

- [#18883](https://github.com/emqx/emqx/pull/18883) 監査ログの閲覧をグローバルユーザーに制限。監査ログは全ネームスペースの操作を記録するため、`GET /api/v5/audit` はグローバル管理者とグローバルビューアのみが読めます。ネームスペース所属のダッシュボードユーザーと API キーは閲覧不可。

- [#18964](https://github.com/emqx/emqx/pull/18964) Dashboard が送信する OAuth2 設定を SCRAM HTTP 認証バックエンドが拒否していた問題を修正。OAuth2 有効の `SCRAM` + `HTTP Server` 認証器作成が成功し、ユーザー検索リクエストに `Bearer` 認可ヘッダーでアクセストークンを送信。

- [#19036](https://github.com/emqx/emqx/pull/19036) ゲートウェイログの認証情報漏洩を修正。起動中にゲートウェイ接続が切断された場合（例：DTLS ハンドシェイク失敗後）、スーパーバイザーのオフェンダーレポートに接続引数全体が含まれ、`clientinfo_override.password` とゲートウェイ認証設定が露出していた。これらはログに出力されなくなりました。

- [#19037](https://github.com/emqx/emqx/pull/19037) ゲートウェイのデバッグログに生のデータグラムを含めなくなりました。`received_data`、`received_udp_proxy_data`、`send_data` はバイトサイズのみログ出力。エンコード済みパケットに認証情報が含まれる可能性があるため。解析済みパケットはプロトコルレベルでマスクしてログ出力。

  CoAP ゲートウェイは `POST /mqtt/connection` で発行されるセッショントークンと短縮クエリアライアス（`t`、`p`）の認証情報を解析済みパケットログと拒否リクエストログでマスク。

- [#19098](https://github.com/emqx/emqx/pull/19098) `GET /api/v5/schema_registry`、`GET /api/v5/schema_registry/:name`、`GET /api/v5/opentelemetry` の設定応答を強化し、認証情報を含む値をマスク。対応する作成・更新エンドポイントはマスク済み値を受け入れ、保存済み値を上書きしません。

- [#19125](https://github.com/emqx/emqx/pull/19125) 名前空間付きダッシュボードユーザーと API キーのデフォルトスコープの不整合を修正。

  - 明示的スコープリストなしの名前空間管理者はグローバル管理者のスコープを取得していた。ユーザー作成・更新時の `"scopes": "unset"` やスコープリストを変更しなかった更新時に発生。名前空間管理者のデフォルトに修正。
  - スコープなしで作成された名前空間ビューアは名前空間管理者より多くのスコープ（`gateways`、`publish`、`audit`）を持っていた。名前空間ビューアと名前空間ビューア API キーは、ログイン専用スコープを除く名前空間管理者と同じ管理スコープに変更。名前空間ビューアに `gateways`、`publish`、`audit`、`mfa_management` を付与不可に。
  - 起動時に EMQX は既存の名前空間ビューアの保存スコープリストから `gateways`、`publish`、`audit`、`mfa_management` を削除。ビューアはゲートウェイエンドポイントの読み取りアクセスを失う。`publish` と `audit` は既に名前空間ユーザーに拒否済み。既存 API キーの保存スコープリストは変更なし。

- [#19142](https://github.com/emqx/emqx/pull/19142) Gateway lookup CLI コマンドが認証情報と `clientinfo_override.password` を露出していた問題を修正。機密ゲートウェイ設定値は出力からマスク。

- [#19175](https://github.com/emqx/emqx/pull/19175) RPC の安全でない認証フォールバックを削除。

  クラスター内ノードは常にチャレンジレスポンスハンドシェイクでバックプレーン（RPC）接続を認証。失敗時に Erlang cookie を送信する旧フォールバックは削除。

  `rpc.insecure_fallback` 設定キーは無効化。EMQX 5.3.0 未満とのクラスタリング用に存在していたが、設定ファイルに残っていても無視される。

#### MQTT プロトコルとリスナー

- [#18882](https://github.com/emqx/emqx/pull/18882) MQTT とゲートウェイリスナーの WebSocket メッセージサイズ制限をプロトコル設定に合わせて調整。

  - MQTT WebSocket リスナーでは `websocket.max_frame_size = infinity`（デフォルト）はゾーンの `mqtt.max_packet_size` の2倍を意味します。クライアントが複数 MQTT パケットを1 WebSocket メッセージで送る場合は明示的な値を設定してください。
  - ゲートウェイ WebSocket リスナーでは `websocket.max_frame_size` が有効になり、デフォルトは 2097152（2 MiB）、`infinity` はこのデフォルトを選択します。
  - `websocket.max_frame_size` は最大 268435455 まで設定可能。

  制限はフラグメント結合後、圧縮解除後の WebSocket メッセージに適用。超過時は WebSocket ステータス 1009 で接続を閉じます。

- [#19174](https://github.com/emqx/emqx/pull/19174) `parse_unit = frame`（デフォルト）で TCP/SSL リスナーはトランスポートレベルで `mqtt.max_connect_packet_size` を適用。最初のパケットが制限を超えると固定ヘッダーで拒否し、本文はバッファリングしません。以前はトランスポートが `mqtt.max_packet_size` までバッファリングしてからチェックしていました。クライアント接続後は制限は `mqtt.max_packet_size` に戻ります。両設定はデフォルトで 1MB なのでデフォルト設定は変わりません。`mqtt.max_packet_size` を 1MB 以上に設定して大きな CONNECT パケットを許可する場合は、こちらも適切に上げてください。

- [#19239](https://github.com/emqx/emqx/pull/19239) WebSocket リスナーの `Sec-WebSocket-Protocol` ヘッダーが解析不能な場合の応答を修正。ヘッダーなしのリクエストと同様に処理されます（`fail_if_no_subprotocol` 有効時は 400 応答、無効時はサブプロトコルなしでアップグレード）。ゲートウェイ WebSocket と OCPP ゲートウェイも同様。以前はリクエストプロセスが終了し、クライアントに空の 400 応答を返し、ログにクラッシュレポートが出ていました。

- [#19374](https://github.com/emqx/emqx/pull/19374) MQTT 5 セッション有効期限が0のとき、異常な WebSocket 切断で保留中の Will メッセージがパブリッシュされなかった問題を修正。

- [#19511](https://github.com/emqx/emqx/pull/19511) クライアントが同一接続で DISCONNECT 後に送信する MQTT パケットをブローカーが無視するよう修正。修正前は DISCONNECT 直後の CONNECT で `session_stepdown_request_exception` エラーがログに出て、Clean Session=false のクライアントのセッションが破棄されていました。

- [#19518](https://github.com/emqx/emqx/pull/19518) MQTT とゲートウェイリスナーの WebSocket メッセージサイズ制限を強化し、圧縮メッセージも対象に。

- [#19522](https://github.com/emqx/emqx/pull/19522) メッセージキューやヒープが大きすぎるため `force_shutdown` ポリシーで切断されたクライアント接続はエラーレベルのスーパーバイザーレポートを生成しなくなりました。リスナーのシャットダウンカウンターは `mailbox_overflow` または `proc_heap_too_large` にカウント。切断は警告レベルでログ出力。

- [#19531](https://github.com/emqx/emqx/pull/19531) TLS アラートで確立済み接続を閉じた場合はエラーレベルのスーパーバイザーレポートを生成しません。各アラートは種類別のリスナーシャットダウンカウンター（例：`user_canceled`、`bad_record_mac`）を増加させ、通知レベルでログ出力。TLS スタックが認識しないアラートコードは `unsupported/unknown_alert` カウンターにまとめられます。以前エラーレポートを生成していた無効なプロキシプロトコルヘッダーなどのソケットエラーは `sock_error` として通知レベルでログ出力。カウンターは `emqx ctl listeners` と `emqx_client_disconnected_reason` Prometheus メトリクスで確認可能。

- [#19574](https://github.com/emqx/emqx/pull/19574) クライアントIDに MQTT ワイルドカードが含まれる `$SYS` クライアントイベントトピックでのパブリッシュフックエラーを修正。クライアントID中の `#`、`+`、`%` はトピック名で `%23`、`%2B`、`%25` にエンコード。イベントペイロード中のクライアントIDは変更なし。

- [#19632](https://github.com/emqx/emqx/pull/19632) WSS リスナーの TLS 接続処理を改善するため ranch ライブラリを 2.2.0-emqx-4 にアップグレード。

- [#19640](https://github.com/emqx/emqx/pull/19640) WebSocket 上の MQTT パケット順序を修正。完全なパケットの後に不完全または無効なパケットが続く場合の処理と、無効パケット処理時の接続プロセスクラッシュを防止。

  ノード移動やリバランス後のセッション期限切れ時の不要なエラーログを修正。

- [#19649](https://github.com/emqx/emqx/pull/19649) TLS リスナーで `ssl_options.cacertfile` が空文字列の場合の `cert_file_not_found` エラーログを修正。

- [#19674](https://github.com/emqx/emqx/pull/19674) WSS リスナーで `ssl_options.handshake_timeout` が無効だった問題を修正。WSS リスナーは常に 15 秒のタイムアウトを使っていましたが、設定値を使用するようになりました。

- [#19737](https://github.com/emqx/emqx/pull/19737) 共有サブスクライバーが、同じトピックフィルターの他のサブスクライバーがサブスクライブ解除または切断した後にメッセージ受信を停止する問題を修正。

- [#19940](https://github.com/emqx/emqx/pull/19940) TLS 接続試行の大規模バーストが TLS リスナーの接続受け入れ停止を引き起こす問題を修正。クライアントは接続タイムアウトとなり、ノードはメモリを増やし続け、リスナーは回復しませんでした。通常は再起動が必要でした。

  EMQX はバースト中に一部の新規 TLS 接続を即時切断します。リスナーは他の接続を受け入れ続け、拒否されたクライアントは再試行で成功します。拒否された接続は `too_many_handshakes` 理由でカウントされ、`emqx ctl listeners` と `emqx_client_disconnected_reason` メトリクスで確認可能。

#### クラスタリング

- [#19136](https://github.com/emqx/emqx/pull/19136) クラスターリンクがピアクラスターへの接続を失った際にフライト中のメッセージを破棄する問題を修正。接続が失われた、または確立されていない（例：接続タイムアウト、DNS 解決失敗、トランスポートエラー）場合は回復可能とみなし、リクエスト期限切れまでメッセージを再試行。

- [#19139](https://github.com/emqx/emqx/pull/19139) クラスターリンクのヘルスチェックはリモートブローカーへの接続をエンドツーエンドで検証します。MQTT `PINGREQ` を送り `PINGRESP` を待ちます。以前はローカルクライアント状態のみを検査していました。

  パケットが無応答の場合、リンクは即座に `disconnected` と報告され、バッファワーカーはメッセージをキューイングしリソースを再起動します。以前はクライアントのキープアライブ（1〜2倍の `keepalive`、デフォルト60秒）が無応答を検知していましたが、継続トラフィックで検知されない場合がありました。

  リンクのメッセージ転送クライアントはクライアントキープアライブ設定を持ちません。

- [#19556](https://github.com/emqx/emqx/pull/19556) etcd ベースのクラスターディスカバリが一時的な etcd 障害に耐えるよう修正。ノードはバックオフ付きで etcd に再接続し、接続回復後に自身の登録キーを復元します。以前は etcd が到達不能になるとディスカバリが失敗し、etcd 復旧後もノードは登録解除されたままでした。

#### データ統合

- [#18945](https://github.com/emqx/emqx/pull/18945) RocketMQ アクションが存在しないトピックを扱う際の問題を修正。

  ネームサーバーにルートがなく、ブローカーがトピックを自動作成しない場合（例：Alibaba Cloud コンソールで名前空間ごとにトピック作成）、すべてのメッセージが `case_clause` エラーで破棄され、メッセージ全体がログに書き込まれていました。修正後：

  - プレースホルダーを含まないトピックのアクションはヘルスチェックで存在を確認し、存在しなければトピック名と名前空間を含むメッセージで `disconnected` と報告。メッセージはバッファに保持され再試行され、トピック作成後の最初のヘルスチェックで復旧。
  - テンプレート化トピックが存在しないメッセージは読みやすい `topic_not_found` エラーで破棄され、ログにペイロードは含まれません。
  - 名前空間が設定されている場合、デフォルトの自動作成トピック `TBW102` は `<namespace>%TBW102` とリクエストされていましたが、存在しないため名前空間内で自動作成できませんでした。デフォルトトピックは Java クライアント同様に素の名前でリクエストされます。

  また、プロデューサーが稼働していない間に2分ごとに上がり下がりしていたコネクターアラームを修正。ネームサーバーは120秒後にアイドル接続を切断し、クライアントはすぐ再接続するようになりました。

- [#18987](https://github.com/emqx/emqx/pull/18987) 一部のコネクター、アクション、ソースが `resource_opts.health_check_timeout` をヘルスチェックのタイムアウトに使っていなかった問題を修正。

  対象は Cassandra、DynamoDB、GCP PubSub Consumer、IoTDB（Thrift ドライバー）、Snowflake Aggregated、SQL Server、TDengine、MySQL、Doris、Postgres コネクター。

- [#19055](https://github.com/emqx/emqx/pull/19055) 初回ヘルスチェックで詳細な接続失敗理由を報告しなかったコネクターを修正。以前は「resource down: disconnected」のみ報告。対象は PostgreSQL 系、MySQL、Redis、Cassandra、TDengine、SQL Server、IoTDB、Couchbase、DynamoDB、Snowflake Aggregated、Syskeeper。

- [#19099](https://github.com/emqx/emqx/pull/19099) ルールエンジンの一部 SQL 関数を改善し、各ルールが同一名前空間のルールで書き込まれたデータのみアクセス可能に。グローバル名前空間のルールは引き続きグローバルデータ空間を共有。

- [#19108](https://github.com/emqx/emqx/pull/19108) 高負荷時に GCP PubSub プロデューサーと HTTP アクションがリモートサーバー切断で `{error,closed}` を回復不能エラーとして報告していた問題を修正。これらは回復可能エラーとして扱われます。

- [#19109](https://github.com/emqx/emqx/pull/19109) コネクターとアクション/ソースのヘルスチェック失敗時の詳細情報を改善。

  対象は Disk log、GreptimeDB、HTTP、InfluxDB、Kafka Consumer、Kafka Producer 系、OpenTS、Pulsar Producer、Redis、RocketMQ、Syskeeper、Tablestore。

- [#19117](https://github.com/emqx/emqx/pull/19117) リモートブローカーが到達不能な場合、`mqtt` アクションのバッファに蓄積されたメッセージが失われなくなりました。接続レベルの失敗（例：`ssl_closed`、`econnrefused`、キープアライブやソケットタイムアウト、DNS エラー）は再試行可能となり、`request_ttl` 期限切れまでバッファに保持されます。フォールバックアクション設定時は期限切れでフォールバックがトリガーされます。

- [#19122](https://github.com/emqx/emqx/pull/19122) `mqtt` コネクターのヘルスチェックはリモートブローカーへの接続をエンドツーエンドで検証します。MQTT `PINGREQ` を送り `PINGRESP` を待ちます。タイムアウトは `resource_opts.health_check_timeout` の半分に制限。以前はローカルクライアント状態のみを検査していました。

  リモートブローカーが TCP 接続を開いたままパケットを破棄する場合、プローブが応答しないと即座に `disconnected` と報告され、リソースが再作成され、アクションのバッファワーカーは配信不能な接続にメッセージを渡さずキューイングします。クライアントのキープアライブ（1〜2倍の `keepalive`、デフォルト160秒）が無応答を検知する以前の挙動は廃止。

  コネクター状態理由はピアによる接続切断をリスナー容量問題として説明しなくなりました。

- [#19186](https://github.com/emqx/emqx/pull/19186) S3 サーバーが非成功 HTTP ステータスでボディなし応答した場合、コネクター/アクションの状態やログにクラッシュログが出ていた問題を修正。

- [#19222](https://github.com/emqx/emqx/pull/19222) Pulsar コネクターのヘルスチェックで内部呼び出しに常に5秒タイムアウトを使っていた問題を修正。設定の `resource_opts.health_check_timeout`（デフォルト10秒）を使うようになり、調整可能に。

- [#19249](https://github.com/emqx/emqx/pull/19249) 一部コネクター/アクション/ソースがヘルスチェックのタイムアウトに `resource_opts.health_check_timeout` を使っていなかった問題を修正。

  対象は MongoDB コネクター、Redis コネクター。

- [#19261](https://github.com/emqx/emqx/pull/19261) HTTP ベースのコネクター（HTTP Server、GCP PubSub、BigQuery、Couchbase、Snowflake）および HTTP 認証・認可の接続で TCP キープアライブを有効化。

  これらの接続は30秒キープアライブで開かれますが以前は効果なし。Linux/macOS ではアイドル接続は30秒後にキープアライブプローブを送り、その後5秒ごとにプローブを送り、3回応答なしで切断。その他の OS では OS のタイマーを使用。

- [#19287](https://github.com/emqx/emqx/pull/19287) GreptimeDB アクションで書き込み構文に合わない値（例：`i` サフィックスのない浮動小数点値）が原因の失敗を修正。無効メッセージはバッファワーカーをブロックせず失敗としてカウント。バッチ内の有効メッセージは書き込み継続。

  整数・符号なし整数値とタイムスタンプは書き込み前にサポート範囲で検証。`i` や `u` サフィックスなしの整数は InfluxDB ラインプロトコルに従い `float64` として書き込み。

- [#19363](https://github.com/emqx/emqx/pull/19363) 一部コネクター/アクション/ソースがヘルスチェックのタイムアウトに `resource_opts.health_check_timeout` を使っていなかった問題を修正。

  対象は Postgres アクションとコネクター。

- [#19364](https://github.com/emqx/emqx/pull/19364) Postgres アクションのヘルスチェックでテーブル存在検証トランザクションが次のヘルスチェックまでアクティブでロックを保持していた問題を修正。検証終了時にトランザクションを閉じるように。

- [#19386](https://github.com/emqx/emqx/pull/19386) 一部コネクター/アクション/ソースがヘルスチェックのタイムアウトに `resource_opts.health_check_timeout` を使っていなかった問題を修正。

  対象は Couchbase コネクター。

- [#19412](https://github.com/emqx/emqx/pull/19412) Sparkplug B の `NDATA` と `DDATA` のメトリックエイリアスマッピングがトピックマウントポイント使用時に正しく動作しなかった問題を修正。

- [#19521](https://github.com/emqx/emqx/pull/19521) HTTP アクションが HTTP 502、504 応答や一時的な DNS 解決失敗（`nxdomain`）でメッセージを破棄していた問題を修正。これらは再試行可能エラーとなり、アクションバッファがリトライを試みます。

  同様に HTTP アクションの同期・非同期クエリモードで `parameters.max_retries` を無視していた問題を修正。パラメータはアクションクエリ内のリトライ回数を制御し、アクションバッファのリトライとは独立。通常の接続シャットダウン競合の無料リトライは従来通り。

- [#19533](https://github.com/emqx/emqx/pull/19533) OAuth2 クライアント認証を使う HTTP コネクターが HTTP 401 応答後に拒否されたキャッシュトークンを送り続けていた問題を修正。コネクターはトークンを更新し1回リトライ。複数リクエスト間でトークン更新を共有し、設定済みの HTTP リトライ予算はリセットしません。失敗したトークン更新はバッファアクションで再試行可能。HTTP 認証・認可も同様。

- [#19619](https://github.com/emqx/emqx/pull/19619) HTTP アクションは非同期クエリ内で回復可能な HTTP 応答を即時リトライしなくなりました。両コールバックはエラーをアクションバッファに返し、リトライは `resume_interval` と `request_ttl` で制御されます。

- [#19647](https://github.com/emqx/emqx/pull/19647) S3 アクションの Direct Upload モードが設定のカスタム HTTP ヘッダーを無視していた問題を修正。

- [#19650](https://github.com/emqx/emqx/pull/19650) アクションの基盤接続が再接続する際に再接続コールバック関数が蓄積する可能性のあるリークを修正。

  対象アクション：

  - Oracle
  - Postgres 系（TimescaleDB、MatrixDB、CockroachDB、AlloyDB、Redshift）
  - Cassandra
  - Datalayers Arrow Flight

  Oracle アクションの削除または SQL テンプレート変更後に再接続時に古い SQL が送信される問題を修正。再接続コールバックは削除され、Oracle コネクターはプールワーカーを自動再接続し、再接続ごとに SQL を再検証。

- [#19663](https://github.com/emqx/emqx/pull/19663) MongoDB 認証、認可、データ統合が `overflow_ttl` と `overflow_check_period` 設定を無視していた問題を修正。クエリのバースト時に余分な MongoDB 接続がクエリごとに閉じられていました。EMQX はアイドルタイムアウトまで接続を再利用します。

- [#19664](https://github.com/emqx/emqx/pull/19664) RocketMQ アクションで、以前のインスタンスのプロデューサーオプションが再作成後も残存するリークを修正。

- [#19669](https://github.com/emqx/emqx/pull/19669) QuasarDB コネクタープールがコネクター停止・削除後も動作し続ける問題を修正。

- [#19670](https://github.com/emqx/emqx/pull/19670) 2つ目の Tablestore アクションをコネクターに追加すると既存アクションのクエリが `channel_not_found` で失敗する問題を修正。

- [#19690](https://github.com/emqx/emqx/pull/19690) 応答しない EMQX Tables バックエンドがノード全体のファイル・ログ操作をブロックし、ノードが応答しなくなる問題を修正。

- [#19703](https://github.com/emqx/emqx/pull/19703) SparkplugB メトリックエイリアスマッピングで、`DBIRTH` メッセージ前に `NDATA` メッセージをパブリッシュするとデバイスのメトリックエイリアスマッピングがノードのマッピングの一部を継承する問題を修正。

- [#19778](https://github.com/emqx/emqx/pull/19778) 複数のコネクターで TLS オプション設定を追加：

  - ClickHouse
  - OpenTSDB
  - TDengine
  - Tablestore

  Tablestore は設定スキーマで TLS オプションを受け入れていましたがドライバーに渡していませんでした。修正済み。

- [#19779](https://github.com/emqx/emqx/pull/19779) GCP ベースのコネクター（GCP Pub/Sub Producer/Consumer、BigQuery）の WIF 認証で初期トークン取得時の TLS オプション指定を追加。チェーン内の他のトークンは常に `node.trusted_certs` の CA バンドルで Google エンドポイント証明書を検証。初期ステップのみ任意のエンドポイント向けに異なる TLS オプションを指定可能。

#### バグ修正（続き）

- [#19841](https://github.com/emqx/emqx/pull/19841) `resource_opts` 設定なしで S3 アクションを作成すると HTTP 500 エラーになる問題を修正。

- [#19845](https://github.com/emqx/emqx/pull/19845) MongoDB アクションの冗長なヘルスチェックを削除。以前は各アクションがコネクターのチェックを繰り返し、チェック数が増えて接続プールの競合や `mongodb_connector_pool_checkout_timeout` エラーを引き起こしていました。

- [#19885](https://github.com/emqx/emqx/pull/19885) MQTT コネクターとクラスターリンク MQTT クライアントの `retry_interval` 処理を修正。値が誤って1000倍されており、QoS 1 と QoS 2 の再送が設定より大幅に遅延していました。

- [#19888](https://github.com/emqx/emqx/pull/19888) コネクターのチャネル自動追加リトライが成功後も繰り返される可能性のある競合状態を修正。

  RabbitMQ コネクターで以下のログが繰り返される可能性があります。

  ```
  tag: CONNECTOR/RABBITMQ, msg: add_channel_failed, reason: already_exists
  ```

#### ゲートウェイ

- [#19027](https://github.com/emqx/emqx/pull/19027) GB/T 32960 ゲートウェイで、ログイン時に認証された VIN と異なるヘッダー VIN のフレームを拒否し、既に確立済み接続での車両ログインを拒否。以前は1つの接続が異なる車両としてテレメトリをパブリッシュでき、VIN を切り替えても前の車両のセッション状態を保持していました。

- [#19059](https://github.com/emqx/emqx/pull/19059) STOMP、NATS、JT/T 808 ゲートウェイでフレームサイズ制限の不整合を修正。STOMP トランザクションの使用制限と非正のトランザクションタイムアウトを拒否。LwM2M の再登録が異なるクライアントIDのセッションを再利用しないよう修正。NATS ゲートウェイは制御ラインに `max_control_line` を適用し、メッセージデータと INFO.max_payload は `max_payload_size` で制限。

- [#19176](https://github.com/emqx/emqx/pull/19176) NATS ゲートウェイのサブジェクト変換を修正。NATS サブジェクトは MQTT ワイルドカードや共有サブスクリプションの意味を持てず、曖昧なトークン境界を持つ MQTT トピックは NATS サブスクライバーに配信されません。

  `/`、`+`、`#` を含む、または `$share.`、`$queue.`、`$exclusive.` で始まるサブジェクトは拒否。`PUB` と `HPUB` のリプライサブジェクトも同様。NATS `>` サブスクリプションと JWT 権限は少なくとも1つのトークンが必要。サポートされないサブジェクトは接続認証時に拒否。

  キューグループ名に MQTT セパレータやワイルドカードを含むものは拒否され、結果のサブスクリプションフィルターは要求された NATS サブジェクトと整合的でなければなりません。

  JWT パブリッシュ・サブスクライブ権限は NATS サブジェクト意味論で直接照合。ワイルドカード拒否ルールは `$` で始まるサブジェクトもカバーし、MQTT ワイルドカード除外による JWT 制限回避を防止。

  パブリッシュ JWT 権限はメッセージ受信前の元のフレームサブジェクトでチェックされ、EMQX ACL 認可は受信後の MQTT トピックに適用。

- [#19179](https://github.com/emqx/emqx/pull/19179) CoAP と LwM2M のブロック単位交換数とセッションごとの保持ペイロードバイト数を制限。ブロック1アップロードは交換制限到達時に 4.29、バイト制限到達時に 4.13 を返す。ブロック通知と LwM2M ダウンリンクは一時的にブロックされてもキューに保持され、容量復帰時に再開。

  失敗した LwM2M 転送は次のコマンド送信前にブロック単位リソースを解放。アクティブ転送はコマンドがキューにない場合や受信デバイスメッセージで期限切れ状態が削除されてもタイムアウトを報告。キューモードデバイスは次のアクティビティまでコマンドをキャッシュ。LwM2M トランスポートタイマーは再送とコマンドタイムアウトを管理。状態遷移は不要なアックタイマーをキャンセル。

- [#19546](https://github.com/emqx/emqx/pull/19546) LwM2M セッションごとに最大1,000件の保留ダウンリンクコマンドを制限。キューが満杯の場合、新規コマンドは MQTT で 4.29 応答を返し FIFO 順序で受理済みコマンドは保持。保留数と固定制限は mqueue 統計で報告。これにより同一パス・タイプの繰り返しコマンドがコマンド記録削除キューを無限に増やすのを防止。

  LwM2M 受信ウィンドウは単調時間を使い、時計調整でダウンリンクコマンドがスリープ中デバイスに送信されるのを防止。

- [#19582](https://github.com/emqx/emqx/pull/19582) MQTT-SN 接続拒否理由をフックとログに正しく報告。未知の戻りコードは `unknown_error` とし、`rejected_invaild_topic_id` は `rejected_invalid_topic_id` に修正。誤記の理由名を使うコンシューマは修正名を使う必要あり。クライアントに送信される戻りコードは変更なし。

- [#19676](https://github.com/emqx/emqx/pull/19676) CoAP Observe クライアントはリクエストごとに異なるトークンで後続の Block2 ブロックを取得可能に。Observe トークンを再利用するクライアントも引き続きサポート。観測の再登録はトークンとサブスクリプションオプションを置換。保留中通知とキャッシュ済みブロックは現在の観測ライフサイクルに従う。管理 API でサブスクリプションオプションを更新するとクライアントの観測は維持。

- [#19693](https://github.com/emqx/emqx/pull/19693) CoAP ゲートウェイは有効なトピック名でないトピックへのパブリッシュと有効なトピックフィルターでないトピックへのサブスクライブを `4.00 Bad Request` で拒否。例：`a/#` や `+/a` へのパブリッシュは以前は受け入れていた。

- [#19741](https://github.com/emqx/emqx/pull/19741) STOMP ゲートウェイで、クライアントが `receipt` ヘッダー付きでトランザクションをコミットした際に `RECEIPT` を送信するよう修正。`COMMIT` にこのヘッダーがある場合、トランザクション内のコマンドのレシートはコマンド順に届き、その後に `COMMIT` レシートが届く。

- [#19742](https://github.com/emqx/emqx/pull/19742) ゲートウェイの WebSocket キープアライブチェックはバイト数ではなくプロトコルパケット数をカウントし、TCP/UDP 接続と整合。

- [#19746](https://github.com/emqx/emqx/pull/19746) CoAP と LwM2M ゲートウェイはブロック単位の `max_body_size` と `exchange_lifetime` に正の値を要求。無効な設定は保存せず拒否。アップグレード前にゼロまたは負の値を正の値に置き換えてください。

- [#19747](https://github.com/emqx/emqx/pull/19747) CoAP と LwM2M ゲートウェイはブロック単位の `max_total_size` に正の値を要求。無効な設定は保存せず拒否。アップグレード前にゼロまたは負の値を正のバイト数に置き換えてください。

#### ダッシュボードと REST API

- [#19003](https://github.com/emqx/emqx/pull/19003) `PUT /api/v5/api_key/:name` を強化し、リクエストボディに存在するフィールドのみ更新するように。以前は部分更新で言及されていないフィールドも書き換えられ、管理者が意図せず API キーの権限を変更する恐れがありました。呼び出しには管理者権限が必要です。

- [#19085](https://github.com/emqx/emqx/pull/19085) 不正な `Authorization` ヘッダーの REST API 応答を修正。該当リクエストは `AUTHORIZATION_HEADER_ERROR` コード付きの 401 応答を返します。以前はリクエストプロセスが終了し、クライアントに空の 400 応答を返し、ログにクラッシュレポートが出ていました。

- [#19116](https://github.com/emqx/emqx/pull/19116) REST API でテキストでないデータ（例：PEM 形式が期待されるフィールドに DER 証明書）が送信された場合のエラーメッセージを改善。無効な UTF-8 であることとサイズを報告し、以前のようにバイト列の長大なリストを表示したり、型が違うと誤って報告したりしません。

  任意値を受け入れる設定フィールドは変換エラーを保存しなくなりました。以前は内部エラーが値の代わりに保存されていました。

- [#19228](https://github.com/emqx/emqx/pull/19228) 認証済み呼び出し元の一部が API ドキュメントエンドポイント（`/api-docs/swagger.json`、`/api-spec.json`、`/api-spec.md`、`/api-spec/:tag`）を読めなかった問題を修正。

  - 明示的なスコープリストを持つダッシュボードユーザーと API キーは、保持するスコープに関わらずこれらのエンドポイントを読めます。
  - ビューワーロールまたは名前空間管理者ロールのダッシュボードユーザーと API キーも読めます。

  認証なしリクエストは引き続き 401 応答。

- [#19435](https://github.com/emqx/emqx/pull/19435) REST API ハンドラーが内部エラーで失敗した際、HTTP 500 応答と警告ログに失敗した関数の引数（リクエストヘッダーとボディ）を含めていた問題を修正。`authorization` ヘッダーの値（ベアラートークンや API キーとシークレット）とリクエストボディのフィールド値は構造のみ保持し、文字列値は長さに置き換え。

- [#19448](https://github.com/emqx/emqx/pull/19448) protobuf バンドル REST API で、フォームフィールドが誤った形式で送信された場合（`bundle` がテキストフィールド、`name`、`root_proto_file`、`description` がアップロードファイル）、HTTP 400 と `BAD_FORM_DATA` コード、フィールド名を含むメッセージを返すよう修正。以前は HTTP 500 を返していました（`description` は内部バリデーションメッセージ付きの 400）。

- [#19503](https://github.com/emqx/emqx/pull/19503) JSON ボディで不正な UTF-8 や不対の Unicode サロゲートエスケープがある場合、スキーマ検証前に固定の HTTP 400 エラーを返すよう修正。誤った設定エラーや過大なエラー応答を防止。絵文字を含む有効な Unicode は引き続きサポート。

- [#19520](https://github.com/emqx/emqx/pull/19520) ダッシュボード LDAP SSO バックエンドの繰り返し削除で、以降の削除リクエストや再作成が内部エラーで失敗する問題を修正。

  バックエンド削除後の削除リクエストは一貫して HTTP 404 を返し、バックエンドは正常に再作成可能。

- [#19565](https://github.com/emqx/emqx/pull/19565) 不正な `Authorization` ヘッダーは 400 ではなく 401 `AUTHORIZATION_HEADER_ERROR` を返し、内容はエラーログに書き込まれなくなりました。

  HTTP 仕様で解析できない認証情報（`:` 区切りのない Basic 認証や不正文字を含む Bearer トークン）はリクエストプロセスが終了し、クラッシュレポートにヘッダー値がエラーレベルで含まれていました。これによりフォーマットミスのクライアントが認証情報をログに書き込み、未認証呼び出し元が任意のログエントリを追加できていました。

#### プラグインと拡張

- [#18957](https://github.com/emqx/emqx/pull/18957) ダッシュボードからのプラグインパッケージアップロードで、パッケージ tarball がプラグインインストールディレクトリに存在するが展開されていない場合に `ALREADY_INSTALLED` と応答してインストールを拒否していた問題を修正。アップロードはまずインストール許可リストを通過し、許可されていなければ `403 FORBIDDEN` と `emqx ctl plugins allow` の指示を返します。中断または失敗したインストールの残骸（`release.json` が読めないディレクトリや展開されていないマニフェスト）はインストール済みと誤認されず削除され、アップロードされたパッケージはクリーンなディレクトリに展開されます。アプリケーションがロード済みのインストールは削除されず、`plugin_is_in_use` で拒否され、先に停止が必要です。拒否または失敗したアップロードは置き換えたパッケージファイルを削除しないため、ローカルのインストール済みパッケージのコピーは残り、修復に使えます。置き換え対象のパッケージファイルが読めない場合はアップロードを拒否し、プラグインが完全にインストール済みのノードはファイルを保持します。

- [#19034](https://github.com/emqx/emqx/pull/19034) プラグインパッケージのインストールとロードを強化。

- [#19035](https://github.com/emqx/emqx/pull/19035) プラグインパッケージのインストールと同期を強化。

- [#19115](https://github.com/emqx/emqx/pull/19115) プラグイン設定読み取りエンドポイント（`GET /api/v5/plugins/:name/config` と `GET /api/v5/plugins/:name/config/download`）をグローバル管理者に限定。

- [#19195](https://github.com/emqx/emqx/pull/19195) プラグインパッケージ名のバリデーションを強化。

- [#19339](https://github.com/emqx/emqx/pull/19339) EMQX が既に提供するアプリケーションをバンドルするプラグインパッケージのインストールを修正。

  例えば `emqx_acme` プラグインは `jose` をバンドルしますが、以前は `plugin_app_loaded_outside_package` で拒否されていました。パッケージはインストールされ、プラグインは EMQX がロード済みのアプリケーションを使用します。バージョンが異なる場合は `plugin_app_already_loaded` 警告をログに出します。プラグイン停止やアンインストールは EMQX のコピーを停止・アンロードしません。

  `mix emqx.plugin` でビルドされたプラグインパッケージは EMQX が提供する依存関係をバンドルしなくなりました。

- [#19358](https://github.com/emqx/emqx/pull/19358) プラグインのクラスタ同期（`POST /api/v5/plugins/cluster_sync`）が保持対象のプラグインバージョンを停止していた問題を修正。指定バージョンはすべてのノードでインストール・有効・実行され、他のバージョンのみ削除されます。

- [#19373](https://github.com/emqx/emqx/pull/19373) ノード起動時にクラスタ設定のプラグイン状態を書き換えていた問題を修正。`plugins.states` が1ノードでのみ設定されている場合、その値が全ノードに公開され、ノード起動は他ノードの適用完了待ちで遅延していました。

- [#19391](https://github.com/emqx/emqx/pull/19391) プラグインアップロード拒否時のエラーメッセージを改善。既に別バージョンの同プラグインがロード済みの場合、ロード済みバージョン名を表示しアンインストールまたはノード再起動を促します。以前は同一アプリケーションをバンドルする別プラグインの場合のみ説明し、対象プラグイン名は示しませんでした。

- [#19433](https://github.com/emqx/emqx/pull/19433) プラグイン設定アップロードとインストール REST API で、リクエストボディが期待される単一アップロードファイルでない場合に HTTP 400 と `BAD_FORM_DATA` コード、期待されるフォームを示すメッセージを返すよう修正。以前は HTTP 500 を返していました。

#### 運用と可観測性

- [#18886](https://github.com/emqx/emqx/pull/18886) バックアップ同期の間隔とタイムアウトの無効値を拒否し、成功した HTTP 同期での TLS 証明書パスエラーの誤検知を抑制。

- [#19040](https://github.com/emqx/emqx/pull/19040) 古い EMQX バージョン（6.1 以前）から名前空間付き設定をインポートする際にリミッター設定（`delivery_bytes`、`delivery_messages`、`subscribes`）が存在しないため失敗する問題を修正。

- [#19120](https://github.com/emqx/emqx/pull/19120) `emqx ctl conf load --merge` と `PUT /api/v5/configs?mode=merge` で省略されたフィールドをデフォルトにリセットしなくなりました。以前は1フィールドだけのロードで他のフィールドがリセットされていました。省略されたフィールドは保存値を維持し、既存の保存設定を繰り返す必要がなくなりました。置換モードは変更なし。

- [#19148](https://github.com/emqx/emqx/pull/19148) `license_expiry` アラームに、ライセンスが期限切れ間近か既に期限切れかを示すメッセージと期限日を追加。以前はアラーム名のみで、30日前警告でも期限切れと誤解されていました。アラーム詳細に `days_left` フィールドを追加し、アラームがアクティブな間は詳細とメッセージを更新。

- [#19163](https://github.com/emqx/emqx/pull/19163) `emqx ctl conf load --namespace <ns> --merge` でロードした設定が名前空間の保存設定にマージされるよう修正。以前はグローバル設定にマージされていました。

- [#19166](https://github.com/emqx/emqx/pull/19166) クライアント接続・切断中に `emqx ctl clients stats` コマンドがエラーで停止し、部分的な CSV ファイルを残す問題を修正。

#### 耐久ストレージ、メッセージキュー、ストリーム

- [#19008](https://github.com/emqx/emqx/pull/19008) シャードメタデータ読み込み失敗時にメッセージキューのガベージコレクションが耐久ストレージ世代を継続的に作成し、ETS テーブルとメモリ使用が過剰になる問題を修正。

- [#19010](https://github.com/emqx/emqx/pull/19010) シャードメタデータ読み込み失敗時にストリームのガベージコレクションが耐久ストレージ世代を継続的に作成し、ETS テーブルとメモリ使用が過剰になる問題を修正。

- [#19211](https://github.com/emqx/emqx/pull/19211) ラストバリュキューとメッセージ数・バイト数制限付き通常キューの期限切れメッセージ削除に失敗する問題を修正。

- [#19216](https://github.com/emqx/emqx/pull/19216) ラストバリューストリームとメッセージ数・バイト数制限付き通常ストリームの期限切れメッセージ削除に失敗する問題を修正。

- [#19259](https://github.com/emqx/emqx/pull/19259) メッセージキューのコンシューマが、ノード再起動や全サブスクライバー切断後の再接続時にストレージシャードから新規メッセージ配信を停止する問題を修正。シャードの前世代のすべてのメッセージを既に読み込みアック済みの場合に発生。

- [#19264](https://github.com/emqx/emqx/pull/19264) 耐久ストレージサブスクリプションを失い復元中のストリームからのメッセージをサブスクライブがアックした際のメッセージキューコンシューマのクラッシュを修正。

- [#19304](https://github.com/emqx/emqx/pull/19304) 耐久セッションのクリーンアップと期限切れ処理の複数問題を修正：

  - セッションテイクオーバー中の新規接続が古いセッションデータを読み込まないように。
  - セッション削除は一方向にして半閉じセッションの復元・再利用を防止。
  - RocksDB のカラムファミリハンドルをスラブ削除後に解放し、ノード再起動なしでディスク空き容量を回復可能に。
  - 耐久セッション期限切れタイマーのリトライを追加。
  - チャンネルクッキーで期限切れタイマーを保護し、古いタイマーがテイクオーバー後にセッションを削除しないように。ネットワーク分断時も含む。
  - 切断済み耐久セッションの `awaiting_rel_cnt` カウンターを修正。

- [#19551](https://github.com/emqx/emqx/pull/19551) 遅延トランザクション開始通知がトランザクション停止中に到着した際の競合を修正。これにより耐久ストレージシャードがクラッシュする可能性がありました。

- [#19572](https://github.com/emqx/emqx/pull/19572) 耐久セッションが状態をデータベースにコミットできずハングする問題を修正。

- [#19621](https://github.com/emqx/emqx/pull/19621) 耐久セッションが過負荷時に堅牢性を向上。コミットタイムアウトを回復可能とみなし、チェックポイントタイマーにジッターを加えて同時コミットのスパイクを軽減。

- [#19652](https://github.com/emqx/emqx/pull/19652) 非同期状態チェックポイント中の回復可能な障害が耐久セッションを不整合状態にする問題を修正。

- [#19668](https://github.com/emqx/emqx/pull/19668) ストリームや保持メッセージを消費するクライアントがメッセージ受信を停止したり、接続プロセスクラッシュで切断される稀なケースを修正。セッションテイクオーバー後、No Local やメッセージ期限切れ、配信レート制限到達時に発生。

  配信レート制限到達時、ストリームとメッセージキューのメッセージはセッションメッセージキューではなくソースで待機。配信レート制限でドロップされた QoS 0 メッセージは `delivery.dropped` メトリクスにカウントされ、`delivery.dropped` イベントをトリガー。

- [#19753](https://github.com/emqx/emqx/pull/19753) `DELETE /api/v5/clients/{clientid}` がオフライン耐久セッションを削除するよう修正。以前は HTTP 204 を返すだけで削除していませんでした。

## 6.3.2

*リリース日: 2026-10-16*

### 改善点

#### コア MQTT 機能

- [#19438](https://github.com/emqx/emqx/pull/19438) 混雑していない接続では、メモリ内セッションが QoS 1/2 メッセージのアック待ちでも、キューイングされた QoS 0 メッセージの排出を楽観的に継続します。混雑解消後や既存セッション再開後も排出されます。トピック優先度が有効な場合は早期に停止することがあります。

#### パフォーマンス

本リリースは MQTT 接続、セッション、ノードローカルクライアントデータのメモリ使用を削減し、複数のブローカーパスの CPU オーバーヘッドを低減し、サブスクリプション検索と起動性能を改善します。

- **接続とセッションのメモリ**

  - [#19092](https://github.com/emqx/emqx/pull/19092) 接続は `mqtt.hibernate_after`（デフォルト5秒間無通信）後にハイバネートします。ハイバネートはプロセスヒープを使用中データに縮小し、接続あたり約5KBを解放。以前は TCP と TLS 接続は `mqtt.idle_timeout`（デフォルト15秒）後にのみハイバネートし、WebSocket 接続は最初のパケット後も起きたままでした。`mqtt.hibernate_after = infinity` で接続を起こしたままにできます。クライアントがこの間隔に近い間隔で送信する場合は値を上げてください。ハイバネートはガベージコレクションを1回発生させ、起床時にヒープが再成長します。

  - [#19181](https://github.com/emqx/emqx/pull/19181) ゾーンとリスナーでメッセージ・バイト・サブスクライブレート制限が設定されていない接続はレートリミッター状態を割り当てません。後から制限を設定しても既存接続に適用されます。

  - [#19262](https://github.com/emqx/emqx/pull/19262) キューイングメッセージがないセッション（パブリッシュ専用クライアントや配信に追いつくサブスクライバー）は 48 バイト節約。

  - [#19563](https://github.com/emqx/emqx/pull/19563) Erlang/OTP を 28.4.1-7 に、`emqx-builder` を 6.2-3 にアップグレード。TLS 接続は共有ソケットオプション用の ETS テーブル不要でメモリ使用削減。TLS ハンドシェイクは絶対パスの `cacertfile` 解決で Erlang ファイルサーバーを2回呼び出さなくなり、TLS 接続バースト時のファイルサーバーバックログを防止。

- **ノードローカルクライアントデータ**

  - [#19306](https://github.com/emqx/emqx/pull/19306) チャネル情報テーブルはクライアントの遺言メッセージ、CONNECT プロパティ、サブスクリプションリストを保存しません。SUBACK と UNSUBACK 後のテーブル書き換えコストとメモリ使用を削減。

  - [#19263](https://github.com/emqx/emqx/pull/19263) テーブルは `sockinfo` マップと `conninfo` に既にある `peerhost`、`peerport`、`peername`、`sockport` を削除し、接続あたり約100バイト節約。

  - [#19303](https://github.com/emqx/emqx/pull/19303) 値が0のクライアント統計を保存しなくなり、アイドルクライアントあたり約145バイト節約。

  - [#19245](https://github.com/emqx/emqx/pull/19245) TCP、TLS、WebSocket でサブスクライブするクライアントはサブスクライバーIDテーブルに行を持たなくなり、クライアントあたり約110バイト、100万クライアントで約110MB節約。

  REST API 応答、CLI 出力、フックコールバック引数は変更なし。`emqx_cm:get_chan_info/1,2`、`emqx_cm:lookup_client/1`、`emqx_mgmt:lookup_client/2`、`emqx_channel:info/1`、`emqx_connection:info/1` で直接テーブルを読むプラグインは `will_msg`、`conninfo.conn_props`、`session.subscriptions` を見なくなりました。クライアントのサブスクリプションは `emqx_broker:subscriptions/1`、CONNECT プロパティは `client.connected` フック、単一属性は `emqx_channel:info/2` を使ってください。

- **CPU とレイテンシ**

  - [#19527](https://github.com/emqx/emqx/pull/19527) メッセージ処理とクライアントライフサイクルでフックを名前で直接呼び出すように。

  - [#19516](https://github.com/emqx/emqx/pull/19516)、[#19536](https://github.com/emqx/emqx/pull/19536) メトリクスの更新と読み込みでルックアップ回数を削減。受信・送信 MQTT パケットメトリクスは名前空間ごとにカウンター参照を再利用。

  - [#19528](https://github.com/emqx/emqx/pull/19528) `tcp_backend = socket` の TCP リスナーで、1KB 以下のパケットは受信システムコール回数を削減。QoS 0 PUBLISH パケットあたりの CPU 使用率が約7%低下。大きなパケットは同等か低下。

  - [#19532](https://github.com/emqx/emqx/pull/19532) SUBACK と UNSUBACK 後のクライアントテーブル書き込みを1回にまとめ、高頻度の新規サブスクライブ時のレイテンシを低減。

  - [#19555](https://github.com/emqx/emqx/pull/19555) `mria` を 1.0.2 にアップグレード。クラスターテーブル書き込みは呼び出しプロセスのメッセージキューが100件以上ある場合のみヘルパープロセスで実行。高頻度の新規サブスクリプション時の SUBSCRIBE レイテンシを低減。

  - [#19545](https://github.com/emqx/emqx/pull/19545) リテイナー有効時の SUBSCRIBE 処理の CPU 使用を削減。ワイルドカードなしのトピックフィルターのサブスクライブ時、リテインメッセージがなければ以降のリテイン処理を省略。

- **サブスクリプション検索と起動**

  - [#19245](https://github.com/emqx/emqx/pull/19245) `emqx ctl subscriptions show <ClientId>` はノード上の全サブスクリプションをスキャンしなくなりました。

  - [#19183](https://github.com/emqx/emqx/pull/19183) リリースパッケージに含まれる `nodetool` ヘルパーが事前コンパイル済みになりました。

### バグ修正

#### コア MQTT 機能

- [#19489](https://github.com/emqx/emqx/pull/19489) MQTT TCP リスナーのデフォルト `tcp_backend` を `gen_tcp` に戻しました。

  6.3.0 で Unix/Linux で `socket` がデフォルトになりましたが、接続あたりのメモリ使用が多いため、対応中は `gen_tcp` に戻します。メッセージトラフィックなしの接続のみで測定されたコストです。メッセージトラフィックありのテストは未実施。将来的に v7 で再度 `socket` がデフォルトになります。

  `socket` バックエンドは変更なく、Unix/Linux で `tcp_backend = socket` と明示すれば利用可能。明示的に設定したリスナーは影響なし。デフォルトに依存するリスナーは `gen_tcp` に戻り、アップグレード時に再起動され接続が切断されます。

- [#19298](https://github.com/emqx/emqx/pull/19298) 6.3.0 で導入された `socket` バックエンドの TCP リスナーでの大規模 QoS 1 メッセージバースト時の CPU 使用増加と遅延問題を修正。送信メッセージのバッチ処理でバーストスループットも改善。

- [#19326](https://github.com/emqx/emqx/pull/19326) メッセージ配信でフルインフライトウィンドウを接続混雑信号と扱わなくしました。QoS 0 メッセージはインフライトウィンドウが満杯でも配信可能になり、不必要なキューイングやメッセージドロップを減らします。QoS 1/2 のメッセージ順序も配信レート制限時に修正。キューイングメッセージが送信前に配信クォータを消費しないように。

- [#19351](https://github.com/emqx/emqx/pull/19351) `esockd` を 5.17.4 から 5.17.8 にアップグレードし、以下のリスナー問題を修正：

  - `socket` バックエンドで小さい `tcp_options.recbuf` で高 MTU リンクの TCP スループット低下を修正。
  - PROXY Protocol v2 ヘッダーのゼロ長プロキシ情報が MQTT データを消費する問題を修正し、切断された IPv4/IPv6 アドレス情報を拒否。
  - `socket` バックエンドで不完全な PROXY Protocol v1 ヘッダーを拒否。
  - リスナーオプションのマージとソケットチューニングを修正し、未初期化カウンター更新時のアクセプタークラッシュを防止。

- [#19630](https://github.com/emqx/emqx/pull/19630) `gen_tcp` バックエンドのリスナーで、読み込みを停止したサブスクライバーの QoS 0 配信遅延を修正。サブスクライバーが読み込み再開後、配信が `mqtt.idle_timeout`（デフォルト15秒）まで遅延することがありました。小さい TCP 受信バッファのクライアントは接続中ずっと QoS 0 メッセージの一部しか受信できませんでした。混雑接続は送信キューをタイマーでチェックし、サブスクライバーの読み込み再開後に即時配信再開。

- [#19144](https://github.com/emqx/emqx/pull/19144) セッションレジストリのクリーンアップ処理がクライアント切断時に停止していた問題を修正。切断されたクライアントをスキップし、古い登録を削除し続けます。

- [#19316](https://github.com/emqx/emqx/pull/19316) リミッター登録が再起動やノードシャットダウン時にクリーンアップされず、`persistent_term` にバケットエントリがリークしていた問題を修正。

#### ゲートウェイ

- [#19422](https://github.com/emqx/emqx/pull/19422) MQTT-SN セッションテイクオーバーが混在バージョンクラスタで失敗する問題を修正。6.3.0、6.3.1、6.3.2 へのローリングアップグレード時に異なるバージョンノード間で再接続やウェイクアップが失敗またはクラッシュし、セッション再開と保留メッセージ配信ができませんでした。

- [#19459](https://github.com/emqx/emqx/pull/19459) MQTT-SN ゲートウェイが送信パブリッシュパケットから `DUP` と `RETAIN` フラグを省略していた問題を修正。クライアントは再送メッセージとサブスクライブ時の保持メッセージを識別可能に。

- [#19393](https://github.com/emqx/emqx/pull/19393) セッション再開したゲートウェイクライアントへの QoS 1/2 配信でエラーログが大量発生する問題を修正。

  メッセージキューやストリーム使用時、配信でフックコールバック内例外が発生し、EMQX は `hook_callback_exception` をエラーレベルでログ出力。配信自体は影響なし。セッション再開するゲートウェイチャネル（例：MQTT-SN スリープからのウェイクアップ）はセッションライフサイクルフックを実行せず、コールバックが必要とするチャネルメタデータが設定されていませんでした。

#### アクセス制御

- [#19282](https://github.com/emqx/emqx/pull/19282) HTTP 認証で `hostname_resolution = dynamic` の場合に、`Content-Type` ヘッダー名が小文字でない応答（例：`Content-Type: application/json`）を拒否していた問題を修正。該当応答は受け入れます。

#### データ統合

- [#19313](https://github.com/emqx/emqx/pull/19313) HTTP ベースコネクター（HTTP Server、GCP PubSub Producer、Couchbase、Snowflake）の応答待ちリクエストがある接続は、`resource_opts.request_ttl` と `max_inactive` の合計待機後に再接続していました。`request_ttl` が大きいと遅延し、`infinity` だと再接続されませんでした。

  接続はリクエストが待機中で、60秒間リクエスト送信がないか、`max_inactive` がそれより長い場合に再接続されます。デフォルトの `request_ttl` 45秒、`max_inactive` 10秒は影響なし。

  `request_ttl` が約50秒以上の設定に影響あり。60秒以上かかる応答は再接続で中断される可能性があります。長時間応答が予想される場合は `max_inactive` を最長応答時間以上に設定してください。

- [#19249](https://github.com/emqx/emqx/pull/19249) 一部コネクター/アクション/ソースがヘルスチェックのタイムアウトに `resource_opts.health_check_timeout` を使っていなかった問題を修正。

  対象は MongoDB コネクター、Redis コネクター。

- [#19261](https://github.com/emqx/emqx/pull/19261) HTTP ベースコネクター（HTTP Server、GCP PubSub、BigQuery、Couchbase、Snowflake）および HTTP 認証・認可の接続で TCP キープアライブを有効化。

  これらの接続は30秒キープアライブで開かれますが以前は効果なし。Linux/macOS ではアイドル接続は30秒後にキープアライブプローブを送り、その後5秒ごとにプローブを送り、3回応答なしで切断。その他の OS では OS のタイマーを使用。

- [#19287](https://github.com/emqx/emqx/pull/19287) GreptimeDB アクションで書き込み構文に合わない値（例：`i` サフィックスのない浮動小数点値）が原因の失敗を修正。無効メッセージはバッファワーカーをブロックせず失敗としてカウント。バッチ内の有効メッセージは書き込み継続。

  整数・符号なし整数値とタイムスタンプは書き込み前にサポート範囲で検証。整数は `i` や `u` サフィックスなしの場合は InfluxDB ラインプロトコルに従い `float64` として書き込み。

- [#19363](https://github.com/emqx/emqx/pull/19363) 一部コネクター/アクション/ソースがヘルスチェックのタイムアウトに `resource_opts.health_check_timeout` を使っていなかった問題を修正。

  対象は Postgres アクションとコネクター。

- [#19364](https://github.com/emqx/emqx/pull/19364) Postgres アクションのヘルスチェックでテーブル存在検証トランザクションが次のヘルスチェックまでアクティブでロックを保持していた問題を修正。検証終了時にトランザクションを閉じるように。

- [#19386](https://github.com/emqx/emqx/pull/19386) 一部コネクター/アクション/ソースがヘルスチェックのタイムアウトに `resource_opts.health_check_timeout` を使っていなかった問題を修正。

  対象は Couchbase コネクター。

- [#19412](https://github.com/emqx/emqx/pull/19412) Sparkplug B の `NDATA` と `DDATA` のメトリックエイリアスマッピングがトピックマウントポイント使用時に正しく動作しなかった問題を修正。

- [#19521](https://github.com/emqx/emqx/pull/19521) HTTP アクションが HTTP 502、504 応答や一時的な DNS 解決失敗（`nxdomain`）でメッセージを破棄していた問題を修正。これらは再試行可能エラーとなり、アクションバッファがリトライを試みます。

  同様に HTTP アクションの同期・非同期クエリモードで `parameters.max_retries` を無視していた問題を修正。パラメータはアクションクエリ内のリトライ回数を制御し、アクションバッファのリトライとは独立。通常の接続シャットダウン競合の無料リトライは従来通り。

- [#19533](https://github.com/emqx/emqx/pull/19533) OAuth2 クライアント認証を使う HTTP コネクターが HTTP 401 応答後に拒否されたキャッシュトークンを送り続けていた問題を修正。コネクターはトークンを更新し1回リトライ。複数リクエスト間でトークン更新を共有し、設定済みの HTTP リトライ予算はリセットしません。失敗したトークン更新はバッファアクションで再試行可能。HTTP 認証・認可も同様。

- [#19619](https://github.com/emqx/emqx/pull/19619) HTTP アクションは非同期クエリ内で回復可能な HTTP 応答を即時リトライしなくなりました。両コールバックはエラーをアクションバッファに返し、リトライは `resume_interval` と `request_ttl` で制御されます。

- [#19647](https://github.com/emqx/emqx/pull/19647) S3 アクションの Direct Upload モードが設定のカスタム HTTP ヘッダーを無視していた問題を修正。

- [#19650](https://github.com/emqx/emqx/pull/19650) アクションの基盤接続が再接続する際に再接続コールバック関数が蓄積する可能性のあるリークを修正。

  対象アクション：

  - Oracle
  - Postgres 系（TimescaleDB、MatrixDB、CockroachDB、AlloyDB、Redshift）
  - Cassandra
  - Datalayers Arrow Flight

  Oracle アクションの削除または SQL テンプレート変更後に再接続時に古い SQL が送信される問題を修正。再接続コールバックは削除され、Oracle コネクターはプールワーカーを自動再接続し、再接続ごとに SQL を再検証。

- [#19663](https://github.com/emqx/emqx/pull/19663) MongoDB 認証、認可、データ統合が `overflow_ttl` と `overflow_check_period` 設定を無視していた問題を修正。クエリのバースト時に余分な MongoDB 接続がクエリごとに閉じられていました。EMQX はアイドルタイムアウトまで接続を再利用します。

- [#19664](https://github.com/emqx/emqx/pull/19664) RocketMQ アクションで、以前のインスタンスのプロデューサーオプションが再作成後も残存するリークを修正。

- [#19669](https://github.com/emqx/emqx/pull/19669) QuasarDB コネクタープールがコネクター停止・削除後も動作し続ける問題を修正。

- [#19670](https://github.com/emqx/emqx/pull/19670) 2つ目の Tablestore アクションをコネクターに追加すると既存アクションのクエリが `channel_not_found` で失敗する問題を修正。

- [#19690](https://github.com/emqx/emqx/pull/19690) 応答しない EMQX Tables バックエンドがノード全体のファイル・ログ操作をブロックし、ノードが応答しなくなる問題を修正。

- [#19703](https://github.com/emqx/emqx/pull/19703) SparkplugB メトリックエイリアスマッピングで、`DBIRTH` メッセージ前に `NDATA` メッセージをパブリッシュするとデバイスのメトリックエイリアスマッピングがノードのマッピングの一部を継承する問題を修正。

- [#19778](https://github.com/emqx/emqx/pull/19778) 複数のコネクターで TLS オプション設定を追加：

  - ClickHouse
  - OpenTSDB
  - TDengine
  - Tablestore

  Tablestore は設定スキーマで TLS オプションを受け入れていましたがドライバーに渡していませんでした。修正済み。

- [#19779](https://github.com/emqx/emqx/pull/19779) GCP ベースのコネクター（GCP Pub/Sub Producer/Consumer、BigQuery）の WIF 認証で初期トークン取得時の TLS オプション指定を追加。チェーン内の他のトークンは常に `node.trusted_certs` の CA バンドルで Google エンドポイント証明書を検証。初期ステップのみ任意のエンドポイント向けに異なる TLS オプションを指定可能。

- [#19841](https://github.com/emqx/emqx/pull/19841) `resource_opts` 設定なしで S3 アクションを作成すると HTTP 500 エラーになる問題を修正。

- [#19845](https://github.com/emqx/emqx/pull/19845) MongoDB アクションの冗長なヘルスチェックを削除。以前は各アクションがコネクターのチェックを繰り返し、チェック数が増えて接続プールの競合や `mongodb_connector_pool_checkout_timeout` エラーを引き起こしていました。

- [#19885](https://github.com/emqx/emqx/pull/19885) MQTT コネクターとクラスターリンク MQTT クライアントの `retry_interval` 処理を修正。値が誤って1000倍されており、QoS 1 と QoS 2 の再送が設定より大幅に遅延していました。

- [#19888](https://github.com/emqx/emqx/pull/19888) コネクターのチャネル自動追加リトライが成功後も繰り返される可能性のある競合状態を修正。

  RabbitMQ コネクターで以下のログが繰り返される可能性があります。

  ```
  tag: CONNECTOR/RABBITMQ, msg: add_channel_failed, reason: already_exists
  ```

#### ゲートウェイ

- [#19027](https://github.com/emqx/emqx/pull/19027) GB/T 32960 ゲートウェイで、ログイン時に認証された VIN と異なるヘッダー VIN のフレームを拒否し、既に確立済み接続での車両ログインを拒否。以前は1つの接続が異なる車両としてテレメトリをパブリッシュでき、VIN を切り替えても前の車両のセッション状態を保持していました。

- [#19059](https://github.com/emqx/emqx/pull/19059) STOMP、NATS、JT/T 808 ゲートウェイでフレームサイズ制限の不整合を修正。STOMP トランザクションの使用制限と非正のトランザクションタイムアウトを拒否。LwM2M の再登録が異なるクライアントIDのセッションを再利用しないよう修正。NATS ゲートウェイは制御ラインに `max_control_line` を適用し、メッセージデータと INFO.max_payload は `max_payload_size` で制限。

- [#19176](https://github.com/emqx/emqx/pull/19176) NATS ゲートウェイのサブジェクト変換を修正。NATS サブジェクトは MQTT ワイルドカードや共有サブスクリプションの意味を持てず、曖昧なトークン境界を持つ MQTT トピックは NATS サブスクライバーに配信されません。

  `/`、`+`、`#` を含む、または `$share.`、`$queue.`、`$exclusive.` で始まるサブジェクトは拒否。`PUB` と `HPUB` のリプライサブジェクトも同様。NATS `>` サブスクリプションと JWT 権限は少なくとも1つのトークンが必要。サポートされないサブジェクトは接続認証時に拒否。

  キューグループ名に MQTT セパレータやワイルドカードを含むものは拒否され、結果のサブスクリプションフィルターは要求された NATS サブジェクトと整合的でなければなりません。

  JWT パブリッシュ・サブスクライブ権限は NATS サブジェクト意味論で直接照合。ワイルドカード拒否ルールは `$` で始まるサブジェクトもカバーし、MQTT ワイルドカード除外による JWT 制限回避を防止。

  パブリッシュ JWT 権限はメッセージ受信前の元のフレームサブジェクトでチェックされ、EMQX ACL 認可は受信後の MQTT トピックに適用。

- [#19179](https://github.com/emqx/emqx/pull/19179) CoAP と LwM2M のブロック単位交換数とセッションごとの保持ペイロードバイト数を制限。ブロック1アップロードは交換制限到達時に 4.29、バイト制限到達時に 4.13 を返す。ブロック通知と LwM2M ダウンリンクは一時的にブロックされてもキューに保持され、容量復帰時に再開。

  失敗した LwM2M 転送は次のコマンド送信前にブロック単位リソースを解放。アクティブ転送はコマンドがキューにない場合や受信デバイスメッセージで期限切れ状態が削除されてもタイムアウトを報告。キューモードデバイスは次のアクティビティまでコマンドをキャッシュ。LwM2M トランスポートタイマーは再送とコマンドタイムアウトを管理。状態遷移は不要なアックタイマーをキャンセル。

- [#19546](https://github.com/emqx/emqx/pull/19546) LwM2M セッションごとに最大1,000件の保留ダウンリンクコマンドを制限。キューが満杯の場合、新規コマンドは MQTT で 4.29 応答を返し FIFO 順序で受理済みコマンドは保持。保留数と固定制限は mqueue 統計で報告。これにより同一パス・タイプの繰り返しコマンドがコマンド記録削除キューを無限に増やすのを防止。

  LwM2M 受信ウィンドウは単調時間を使い、時計調整でダウンリンクコマンドがスリープ中デバイスに送信されるのを防止。

- [#19582](https://github.com/emqx/emqx/pull/19582) MQTT-SN 接続拒否理由をフックとログに正しく報告。未知の戻りコードは `unknown_error` とし、`rejected_invaild_topic_id` は `rejected_invalid_topic_id` に修正。誤記の理由名を使うコンシューマは修正名を使う必要あり。クライアントに送信される戻りコードは変更なし。

- [#19676](https://github.com/emqx/emqx/pull/19676) CoAP Observe クライアントはリクエストごとに異なるトークンで後続の Block2 ブロックを取得可能に。Observe トークンを再利用するクライアントも引き続きサポート。観測の再登録はトークンとサブスクリプションオプションを置換。保留中通知とキャッシュ済みブロックは現在の観測ライフサイクルに従う。管理 API でサブスクリプションオプションを更新するとクライアントの観測は維持。

- [#19693](https://github.com/emqx/emqx/pull/19693) CoAP ゲートウェイは有効なトピック名でないトピックへのパブリッシュと有効なトピックフィルターでないトピックへのサブスクライブを `4.00 Bad Request` で拒否。例：`a/#` や `+/a` へのパブリッシュは以前は受け入れていた。

- [#19741](https://github.com/emqx/emqx/pull/19741) STOMP ゲートウェイで、クライアントが `receipt` ヘッダー付きでトランザクションをコミットした際に `RECEIPT` を送信するよう修正。`COMMIT` にこのヘッダーがある場合、トランザクション内のコマンドのレシートはコマンド順に届き、その後に `COMMIT` レシートが届く。

- [#19742](https://github.com/emqx/emqx/pull/19742) ゲートウェイの WebSocket キープアライブチェックはバイト数ではなくプロトコルパケット数をカウントし、TCP/UDP 接続と整合。

- [#19746](https://github.com/emqx/emqx/pull/19746) CoAP と LwM2M ゲートウェイはブロック単位の `max_body_size` と `exchange_lifetime` に正の値を要求。無効な設定は保存せず拒否。アップグレード前にゼロまたは負の値を正の値に置き換えてください。

- [#19747](https://github.com/emqx/emqx/pull/19747) CoAP と LwM2M ゲートウェイはブロック単位の `max_total_size` に正の値を要求。無効な設定は保存せず拒否。アップグレード前にゼロまたは負の値を正のバイト数に置き換えてください。

#### ダッシュボードと REST API

- [#19003](https://github.com/emqx/emqx/pull/19003) `PUT /api/v5/api_key/:name` を強化し、リクエストボディに存在するフィールドのみ更新するように。以前は部分更新で言及されていないフィールドも書き換えられ、管理者が意図せず API キーの権限を変更する恐れがありました。呼び出しには管理者権限が必要です。

- [#19085](https://github.com/emqx/emqx/pull/19085) 不正な `Authorization` ヘッダーの REST API 応答を修正。該当リクエストは `AUTHORIZATION_HEADER_ERROR` コード付きの 401 応答を返します。以前はリクエストプロセスが終了し、クライアントに空の 400 応答を返し、ログにクラッシュレポートが出ていました。

- [#19116](https://github.com/emqx/emqx/pull/19116) REST API でテキストでないデータ（例：PEM 形式が期待されるフィールドに DER 証明書）が送信された場合のエラーメッセージを改善。無効な UTF-8 であることとサイズを報告し、以前のようにバイト列の長大なリストを表示したり、型が違うと誤って報告したりしません。

  任意値を受け入れる設定フィールドは変換エラーを保存しなくなりました。以前は内部エラーが値の代わりに保存されていました。

- [#19228](https://github.com/emqx/emqx/pull/19228) 認証済み呼び出し元の一部が API ドキュメントエンドポイント（`/api-docs/swagger.json`、`/api-spec.json`、`/api-spec.md`、`/api-spec/:tag`）を読めなかった問題を修正。

  - 明示的なスコープリストを持つダッシュボードユーザーと API キーは、保持するスコープに関わらずこれらのエンドポイントを読めます。
  - ビューワーロールまたは名前空間管理者ロールのダッシュボードユーザーと API キーも読めます。

  認証なしリクエストは引き続き 401 応答。

- [#19435](https://github.com/emqx/emqx/pull/19435) REST API ハンドラーが内部エラーで失敗した際、HTTP 500 応答と警告ログに失敗した関数の引数（リクエストヘッダーとボディ）を含めていた問題を修正。`authorization` ヘッダーの値（ベアラートークンや API キーとシークレット）とリクエストボディのフィールド値は構造のみ保持し、文字列値は長さに置き換え。

- [#19448](https://github.com/emqx/emqx/pull/19448) protobuf バンドル REST API で、フォームフィールドが誤った形式で送信された場合（`bundle` がテキストフィールド、`name`、`root_proto_file`、`description` がアップロードファイル）、HTTP 400 と `BAD_FORM_DATA` コード、フィールド名を含むメッセージを返すよう修正。以前は HTTP 500 を返していました（`description` は内部バリデーションメッセージ付きの 400）。

- [#19503](https://github.com/emqx/emqx/pull/19503) JSON ボディで不正な UTF-8 や不対の Unicode サロゲートエスケープがある場合、スキーマ検証前に固定の HTTP 400 エラーを返すよう修正。誤った設定エラーや過大なエラー応答を防止。絵文字を含む有効な Unicode は引き続きサポート。

- [#19520](https://github.com/emqx/emqx/pull/19520) ダッシュボード LDAP SSO バックエンドの繰り返し削除で、以降の削除リクエストや再作成が内部エラーで失敗する問題を修正。

  バックエンド削除後の削除リクエストは一貫して HTTP 404 を返し、バックエンドは正常に再作成可能。

- [#19565](https://github.com/emqx/emqx/pull/19565) 不正な `Authorization` ヘッダーは 400 ではなく 401 `AUTHORIZATION_HEADER_ERROR` を返し、内容はエラーログに書き込まれなくなりました。

  HTTP 仕様で解析できない認証情報（`:` 区切りのない Basic 認証や不正文字を含む Bearer トークン）はリクエストプロセスが終了し、クラッシュレポートにヘッダー値がエラーレベルで含まれていました。これによりフォーマットミスのクライアントが認証情報をログに書き込み、未認証呼び出し元が任意のログエントリを追加できていました。

#### プラグインと拡張

- [#18957](https://github.com/emqx/emqx/pull/18957) ダッシュボードからのプラグインパッケージアップロードで、パッケージ tarball がプラグインインストールディレクトリに存在するが展開されていない場合に `ALREADY_INSTALLED` と応答してインストールを拒否していた問題を修正。アップロードはまずインストール許可リストを通過し、許可されていなければ `403 FORBIDDEN` と `emqx ctl plugins allow` の指示を返します。中断または失敗したインストールの残骸（`release.json` が読めないディレクトリや展開されていないマニフェスト）はインストール済みと誤認されず削除され、アップロードされたパッケージはクリーンなディレクトリに展開されます。アプリケーションがロード済みのインストールは削除されず、`plugin_is_in_use` で拒否され、先に停止が必要です。拒否または失敗したアップロードは置き換えたパッケージファイルを削除しないため、ローカルのインストール済みパッケージのコピーは残り、修復に使えます。置き換え対象のパッケージファイルが読めない場合はアップロードを拒否し、プラグインが完全にインストール済みのノードはファイルを保持します。

- [#19034](https://github.com/emqx/emqx/pull/19034) プラグインパッケージのインストールとロードを強化。

- [#19035](https://github.com/emqx/emqx/pull/19035) プラグインパッケージのインストールと同期を強化。

- [#19115](https://github.com/emqx/emqx/pull/19115) プラグイン設定読み取りエンドポイント（`GET /api/v5/plugins/:name/config` と `GET /api/v5/plugins/:name/config/download`）をグローバル管理者に限定。

- [#19195](https://github.com/emqx/emqx/pull/19195) プラグインパッケージ名のバリデーションを強化。

#### 運用と可観測性

- [#18886](https://github.com/emqx/emqx/pull/18886) バックアップ同期の間隔とタイムアウトの無効値を拒否し、成功した HTTP 同期での TLS 証明書パスエラーの誤検知を抑制。

- [#19040](https://github.com/emqx/emqx/pull/19040) 古い EMQX バージョン（6.1 以前）から名前空間付き設定をインポートする際にリミッター設定（`delivery_bytes`、`delivery_messages`、`subscribes`）が存在しないため失敗する問題を修正。

- [#19120](https://github.com/emqx/emqx/pull/19120) `emqx ctl conf load --merge` と `PUT /api/v5/configs?mode=merge` で省略されたフィールドをデフォルトにリセットしなくなりました。以前は1フィールドだけのロードで他のフィールドがリセットされていました。省略されたフィールドは保存値を維持し、既存の保存設定を繰り返す必要がなくなりました。置換モードは変更なし。

- [#19163](https://github.com/emqx/emqx/pull/19163) `emqx ctl conf load --namespace <ns> --merge` でロードした設定が名前空間の保存設定にマージされるよう修正。以前はグローバル設定にマージされていました。

- [#19166](https://github.com/emqx/emqx/pull/19166) クライアント接続・切断中に `emqx ctl clients stats` コマンドがエラーで停止し、部分的な CSV ファイルを残す問題を修正。

## 6.3.1

*リリース日: 2026-09-17*

EMQX 6.3.1 へアップグレードする前に、破壊的変更を必ずご確認ください。

### 改善点

#### アクセス制御

- [#18777](https://github.com/emqx/emqx/pull/18777) ダッシュボード JSON API ログインに SCRAM-SHA-256 チャレンジレスポンス認証をオプションで追加し、組み込み API Spec ログインページのデフォルトを SCRAM に変更。既存のパスワード認証はデフォルトで有効。

  `dashboard.password_login = scram_only` の場合、`POST /api/v5/login` でユーザー名とパスワードを使うスクリプトやサードパーティクライアントは SCRAM フローに移行するか API キーを使う必要があります。

#### パッケージング

- [#18977](https://github.com/emqx/emqx/pull/18977) 公式 EMQX Enterprise Docker リリースイメージを Hardened Debian 13 (Trixie) に切り替え。ランタイムイメージに `apt` や `dpkg` などのパッケージマネージャは含まれません。

  リリースイメージはビルド由来情報とソフトウェア部品表（SBOM）証明を含み、Docker Scout が Hardened ベースイメージの脆弱性評価（VEX ステートメント）を適用可能。

#### パフォーマンス

- [#18806](https://github.com/emqx/emqx/pull/18806) MQTT 接続のメモリ使用を削減。

  接続時にレートリミッター状態を割り当てていましたが、実際にパブリッシュやサブスクライブするまで割り当てを遅延。接続のみのクライアントは約2KBのメモリ節約。設定済みのレート制限は従来通り動作。

- [#18807](https://github.com/emqx/emqx/pull/18807) MQTT セッションのメモリ使用を削減。

  配信レート制限が設定されていない場合は配信レートリミッター状態を作成時に割り当てず、最初の配信時に割り当て。約0.5KB節約。設定済みの配信レート制限は従来通り動
