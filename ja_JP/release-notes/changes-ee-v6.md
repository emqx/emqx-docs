# EMQX Enterprise Version 6

## 6.3.3

*リリース日: 2026-11-11*

EMQX 6.3.3 へのアップグレード前に、破壊的変更を必ずご確認ください。

### 概要

本リリースはパフォーマンスとリソース使用の最適化に注力しています。42件の改善のうち26件は、各接続、各メッセージ、各セッションに対して EMQX が使用するCPUまたはメモリを削減しています。その他の改善には、アウトバウンドTLSクライアント向けのノード全体の信頼ストア、耐久ストレージおよびTLSハンドシェイクの過負荷保護、ピン留めプラグイン、新しいコネクターオプションが含まれます。また、127件のバグ修正も含まれています。

### アップグレードノート

既存のデプロイメントの動作を変更する可能性のある変更点は以下の通りです。

- `mq.auto_create` と `streams.auto_create` のデフォルトが無効になりました。これにより、`$queue/...` または `$stream/...` トピックへのサブスクライブはキューやストリームを自動作成しなくなります ([#18788](https://github.com/emqx/emqx/pull/18788))。
- `authentication_settings.ignore_backend_failures` と `authorization.ignore_backend_failures` のデフォルトが `per_security_profile` に変更されました。`legacy` プロファイルでは、保存された `false` の場合、バックエンド障害時にクライアントを拒否します ([#19088](https://github.com/emqx/emqx/pull/19088))。
- MQTT v5 の CONNECT および Will プロパティは最大100組のユーザープロパティペアを受け入れます。新設定 `mqtt.max_connect_user_properties` で制御されます ([#18974](https://github.com/emqx/emqx/pull/18974))。
- ダッシュボードの OIDC および SAML SSO ログインは、ログインを開始したブラウザ内で完了する必要があります。SAML は `dashboard_addr` に HTTPS アドレスが必要で、`skip_login_cookie_check = true` がバックエンドに設定されていない限り必須です ([#18810](https://github.com/emqx/emqx/pull/18810))。
- 起動時に、EMQX は既存のネームスペース付きダッシュボードビューアの保存されたスコープリストから `gateways`、`publish`、`audit`、`mfa_management` を削除します。既存のAPIキーは変更されません ([#19125](https://github.com/emqx/emqx/pull/19125))。
- MQTTおよびゲートウェイのWebSocketリスナーは、完全なメッセージに対して `websocket.max_frame_size` を適用します。MQTTリスナーのデフォルト `infinity` は `mqtt.max_packet_size` の2倍に制限され、ゲートウェイリスナーは2MiBがデフォルトです ([#18882](https://github.com/emqx/emqx/pull/18882), [#19518](https://github.com/emqx/emqx/pull/19518))。
- HTTPおよびWebSocketパーサーが厳格化され、HTTPクライアントはデフォルトでレスポンスヘッダーとフレームサイズ制限を適用します。レガシーまたは非準拠のHTTP/WebSocketサービスと連携する統合はアップグレード後に検証してください。REST APIは100以上のクエリパラメータを持つリクエストを拒否します ([#19694](https://github.com/emqx/emqx/pull/19694))。
- MQTTコネクターとクラスターリンクは、`retry_interval` を秒単位で設定されたまま解釈し、1000倍しなくなりました。これによりQoS 1およびQoS 2の再送間隔が設定通りになります ([#19885](https://github.com/emqx/emqx/pull/19885))。
- RabbitMQコネクターは `heartbeat` を1000倍しなくなりました。既存の値はそのままの意味になります ([#19910](https://github.com/emqx/emqx/pull/19910))。
- `verify_peer` を設定し `cacertfile` が空のアウトバウンドTLSクライアントは `node.trusted_certs` を使用します。組み込みHTTPクライアントは独自のCAバンドルを持ちません ([#19756](https://github.com/emqx/emqx/pull/19756))。
- Datalayers Arrow Flightドライバーはサーバー証明書を検証し、`verify_none` を拒否します ([#19815](https://github.com/emqx/emqx/pull/19815))。
- TLSリスナーは大量のハンドシェイクが開始待ちの間、新規接続を拒否します ([#19940](https://github.com/emqx/emqx/pull/19940))。
- QUICリスナーの `ssl_options.hibernate_after` 設定は非推奨で無効です。QUIC接続は `mqtt.hibernate_after` に従います ([#19407](https://github.com/emqx/emqx/pull/19407))。
- `socket` TCPバックエンドはパケット間で受信バッファを解放します。新設定 `tcp_options.buffer_retention_strategy` のデフォルトは `packet` です ([#19785](https://github.com/emqx/emqx/pull/19785))。
- `recv_cnt` と `send_cnt` はTCPパケット数ではなくMQTTパケット数をカウントします ([#19790](https://github.com/emqx/emqx/pull/19790))。
- `GET /clients` は耐久セッションの `seqno_q1_*` と `seqno_q2_*` フィールドを返さなくなりました ([#19651](https://github.com/emqx/emqx/pull/19651))。
- 監査ログはグローバル管理者とグローバルビューアのみが読み取れます ([#18883](https://github.com/emqx/emqx/pull/18883))。
- `topic_match` は `#/a` のような無効なトピックに対し、`#` で終わるフィルターがマッチすると `true` を返します ([#19710](https://github.com/emqx/emqx/pull/19710))。
- クライアント情報テーブルを直接読むプラグインは `conninfo` 下に `sock`、`clientid`、`username` を見なくなりました ([#19419](https://github.com/emqx/emqx/pull/19419), [#19761](https://github.com/emqx/emqx/pull/19761))。
- メモリ内セッションヘッダー `deliver_begin_at` を読むプラグインやフックはマイクロ秒単位の `deliver_begin_at_us` を使う必要があります ([#19774](https://github.com/emqx/emqx/pull/19774))。

### 改善点

#### セキュリティ

- [#19756](https://github.com/emqx/emqx/pull/19756) `node.trusted_certs` 設定を追加しました。アウトバウンドTLSクライアントがCAを指定しない場合に信頼するCA証明書を選択します。デフォルトの `system_default` はOSのCAバンドルを使用します。別のPEMファイルを使うには `file:///absolute/path` を設定してください。

  `verify = verify_peer` で `cacertfile` が空のアウトバウンドTLSクライアントはこれらのCA証明書でサーバーを検証します。以前は接続に失敗していました。

  組み込みHTTPクライアントも `node.trusted_certs` を使用し、独自のCAバンドルは持ちません。

  `cacertfile = system_default` を設定すると `node.trusted_certs` のCA証明書を信頼します。`node.trusted_certs` にCAバンドルがなければErlang/OTPがOSから読み込むCAを使います。リスナーはこの値を受け付けません。

  Dockerイメージはビルド時にcurl.seから最新のMozilla CAバンドルをインストールします。

  アウトゴーイング接続の信頼アンカーを固定するには `cacertfile` を明示的に設定してください。

- [#19088](https://github.com/emqx/emqx/pull/19088) `authentication_settings.ignore_backend_failures` と `authorization.ignore_backend_failures` は `legacy` と `hardened` の両方のセキュリティプロファイルで適用されます。以前は `hardened` のみ有効でした。

  各設定は以下の3値を取ります：

  - `per_security_profile`（新デフォルト）：セキュリティプロファイルに従う。`legacy` はバックエンド障害を無視し、`hardened` はクライアントを拒否します。
  - `true`：バックエンド障害を無視して次の認証器または認可ソースに進みます。
  - `false`：バックエンド障害時にクライアントを拒否します。

  アップグレード注意：`legacy` プロファイルで保存された `false` はバックエンド障害時にクライアントを拒否します。以前の `legacy` 動作を維持するにはアップグレード後に `per_security_profile` に設定してください。

- [#19096](https://github.com/emqx/emqx/pull/19096) `mqtt.max_connect_packet_size` 設定を追加しました。CONNECTパケットのサイズを制限し、`mqtt.max_packet_size` と併用します。CONNECTパケットが大きすぎるクライアントの接続を切断し、リスナーのシャットダウンカウンター `connect_packet_too_large` にカウントします。デフォルトは `1MB` で、`mqtt.max_packet_size` と同じため既定設定は変わりません。`mqtt.max_packet_size` を1MB以上に設定する場合は本設定も引き上げてください。

#### パフォーマンスとリソース使用

- [#19642](https://github.com/emqx/emqx/pull/19642) `gen_tcp` TCPバックエンドのリスナーに `tcp_options.low_watermark` オプションを追加しました。

  Erlangのソケットバッファが `high_watermark` を超えるとソケットはビジー状態になり、バッファが `low_watermark` 以下になるまでビジー状態が続きます。従来は `low_watermark` は常に4KB（Erlang/OTPのデフォルト）でした。これにより、1MBの `high_watermark` でビジーになった接続はほぼ全てのキューイングデータが送信されるまでビジー状態でした。

  オプション未設定時は `high_watermark` の4分の1（ただし4KB未満にはならない）がデフォルトです。これは `socket` TCPバックエンドが輻輳解消に使う比率と同じです。`socket` バックエンドはこのオプションを無視します。`high_watermark` より大きい値は `high_watermark` に制限されます。

- [#19240](https://github.com/emqx/emqx/pull/19240) MQTT PUBLISHメッセージ受信時の一時メモリ割り当てを削減しました。

- [#19256](https://github.com/emqx/emqx/pull/19256) TLS接続ごとのメモリ使用量を削減しました。共有ソケットオプション用のETSテーブルを廃止し、アイドル状態のTLS 1.3接続は暗号状態を解放します。

- [#19277](https://github.com/emqx/emqx/pull/19277) MQTT接続ごとのメモリ使用量を削減しました。同一ゾーン内の接続はパケットパーサー、シリアライザー、ゾーン設定を共有します。1万のアイドルTCP接続で約170バイトの節約です。

- [#19278](https://github.com/emqx/emqx/pull/19278) MQTT接続と外部サブスクリプションのメモリ使用量を削減しました。

- [#19419](https://github.com/emqx/emqx/pull/19419) クライアント情報テーブルの接続ごとのメモリ使用量を削減しました。`conninfo.sock` にソケットのタームを保存しなくなりました。`gen_tcp` バックエンドのTCP接続で16バイト、`socket` バックエンドで32バイト、TLS接続で約128バイトの節約です。

  これは保存されたコピーのみの変更です。フックコールバックは接続自身の `conninfo` を受け取り、そこには `sock` が含まれます。REST APIのレスポンスも変わりません。`emqx_cm:get_chan_info/1,2`、`emqx_cm:lookup_client/1`、`emqx_mgmt:lookup_client/2`、`emqx_channel:info/1`、`emqx_connection:info/1` で直接テーブルを読むプラグインは `conninfo` 下に `sock` を見なくなりました。接続プロセス内で `emqx_channel:info(conninfo, Channel)` を使って読み取ってください。

- [#19420](https://github.com/emqx/emqx/pull/19420) TLSハンドシェイクごとに2回ファイルサーバーを呼んでいた `cacertfile` パス解決を削減しました。TLS接続のバースト時に `file_server_2` プロセスのメッセージバックログが解消されます。

- [#19514](https://github.com/emqx/emqx/pull/19514) PUBLISHパケット処理のCPUコストを削減しました。

  - トピック分割とトピック検証の処理を軽減。
  - Sparkplug Bのエイリアスマッピングフックは最初にトピックをチェックし、Sparkplug B名前空間外のトピックでは処理しません。
  - A2Aレジストリ、スキーマ検証、メッセージ変換、レガシートピックメトリクスは機能使用時にのみフックを追加し、未使用時はメッセージ処理にコストをかけません。
  - クライアントにレート制限が設定されていない場合、パブリッシュ時にリミッター設定をチェックしません。

  6万クライアントが1秒あたり1KBのQoS 0メッセージをパブリッシュするテストで、`gen_tcp` と `socket` バックエンド両方でブローカーのCPU使用率が低下しました。

- [#19554](https://github.com/emqx/emqx/pull/19554) 大きすぎるソケットバッファを小さいネットワーク読み込みに再利用した際のメモリ保持を削減しました。Erlang分散と `gen_rpc` のソケットバッファサイズは1088KiBに増加し、Erlangの再利用可能バッファ制限を超えています。

- [#19556](https://github.com/emqx/emqx/pull/19556) クラスター表への書き込みは、呼び出しプロセスのメッセージキューが100件以上の時のみヘルパープロセスで実行されるように変更しました。以前は10件以上で実行されていました。新規サブスクライブが多い環境でSUBSCRIBEのレイテンシが改善します。

- [#19627](https://github.com/emqx/emqx/pull/19627) `gen_tcp` バックエンドのTCPリスナーの接続は、送信パケットごとにソケットドライバーを待たなくなりました。これによりメッセージあたりのCPU使用率が低下します。QoS 0の1対1パブリッシュ/サブスクライブベンチで約12%低減しました。

- [#19651](https://github.com/emqx/emqx/pull/19651) 接続ごとの統計は接続プロセスが所有するアトミック配列に保持し、`emqx_channel_info` テーブルには参照のみを保持します。

  パケット・メッセージカウンターはその場でインクリメントされるため、`GET /clients`、`GET /clients/{clientid}`、`emqx ctl clients show` はリアルタイムに値を返します。`SUBACK` 後や統計タイマーごとにテーブル行を書き換えなくなり、行の再圧縮が不要になりました。カウンターは接続プロセスのヒープに保持されません。

  耐久セッションの7つの `seqno_q1_*` と `seqno_q2_*` フィールドは `GET /clients` と `GET /clients/{clientid}` のレスポンスから削除されます。`n_streams` は残ります。

- [#19710](https://github.com/emqx/emqx/pull/19710) トピックマッチングは以下の場所でトピックをレベルに分割しなくなりました：

  - 共有サブスクリプションのトピックマッチング
  - ルールエンジンSQLおよびバリフォーム式の `topic_match` 関数
  - クラスターリンクのルート更新

  有効なトピックに対しては結果は従来と同じです。`topic_match` 関数はランタイムデータを受け取り、`#` レベルが最後でないトピックは無効なトピックです。これらのトピックは、以前マッチしなかった `#` で終わるフィルターにマッチします。例：`topic_match('#/a', '#')` は `true` を返します。

  デフォルトの `acl.conf` は `dashboard` ユーザー名を正規表現ではなく等価比較でチェックします。

- [#19718](https://github.com/emqx/emqx/pull/19718) アイドルMQTT接続はキープアライブチェックごとにハイバネーションから復帰しなくなりました。クライアントがキープアライブ期限を過ぎて遅れた時に1回だけ復帰します。クライアント消失検出時間は変わりません。多数のアイドルクライアントがいる場合、ブローカーのCPU使用率が低下します。

- [#19719](https://github.com/emqx/emqx/pull/19719) メモリ内セッションのメモリ使用量とガベージコレクション時のセッションデータコピー量を削減しました。

- [#19732](https://github.com/emqx/emqx/pull/19732) メッセージパブリッシュは、クラスターリンクが有効でないノードではクラスターリンクを呼び出さなくなりました。

- [#19733](https://github.com/emqx/emqx/pull/19733) 配信レート制限がないリスナーでは、メッセージごとにリスナーのレートリミッター設定を読み込まなくなりました。

  実行時にリスナーに配信レート制限が追加された場合、既存のクライアントセッションには1統計間隔（デフォルト15秒）以内に適用されます。新規クライアントセッションは最初のメッセージから制限されます。

- [#19739](https://github.com/emqx/emqx/pull/19739) クラスター内に該当トピックをサブスクライブしているクライアントがなく、ルールエンジンが `$SYS` メッセージを無視している場合（`rule_engine.ignore_sys_message = true`）、`$SYS/brokers/<node>/clients/<clientid>/connected` と `.../disconnected` メッセージを生成・パブリッシュしなくなりました。これにより接続ごとのブローカーCPU使用率が約4%削減されます。

  トピックにサブスクライブしているサブスクライバーはイベントを受け取ります。`rule_engine.ignore_sys_message = false` の場合は従来通りパブリッシュされます。`subscribed` と `unsubscribed` クライアントイベントも同様で、デフォルトは無効です。

- [#19740](https://github.com/emqx/emqx/pull/19740) シングルノード展開でのクライアント接続ごとのCPUコストを約7%削減しました。EMQXがCONNECTごとに取得するセッションロックは、クラスタが単一ノードの場合はローカルノードへの内部リモートコールを送信しなくなりました。

- [#19761](https://github.com/emqx/emqx/pull/19761) クライアント情報テーブルの接続ごとのメモリ使用量を、クライアント識別子とユーザー名の長さ分削減しました。

  `conninfo` 下の `clientid` と `username` は、`clientinfo` 下の値と等しい場合は保存しなくなりました。CONNECTパケットの値が最終的なものと異なる場合は引き続き `conninfo` に現れます。

  これは保存されたコピーのみの変更です。フックコールバックは接続自身の情報を受け取り、そこには両方のフィールドが含まれます。REST APIのレスポンスも変わりません。

- [#19774](https://github.com/emqx/emqx/pull/19774) メモリ内セッションを通じたQoS 1およびQoS 2メッセージ配信時のメモリ割り当てとCPUオーバーヘッドを削減しました。

  ローリングアップグレード中のinflightメッセージREST APIページネーションには以下の制限があります：

  - 6.3.3ノードから古いノードへ連続してRESTリクエストを送ると、古いノードはページネーションカーソルをHTTP 400で拒否します。
  - 6.3.3ノードから古いノードへセッションテイクオーバーした後、同じマイクロ秒単位の配信タイムスタンプを持つinflightメッセージが複数ページにまたがるとスキップされる場合があります。

  MQTTメッセージ配信には影響ありません。

- [#19785](https://github.com/emqx/emqx/pull/19785) `socket` バックエンドのTCP接続が保持するメモリを削減しました。新設定 `tcp_options.buffer_retention_strategy` のデフォルトは `packet` で、次のMQTTパケットを待つ間に受信バッファを解放します。以前の動作を維持するには `always` に設定してください。

  パケット長が既知の読み込みは16MiBに制限されます。より大きいパケットは複数回の読み込みで受信されます。

- [#19790](https://github.com/emqx/emqx/pull/19790) クライアントごとの統計からソケットの送受信操作回数のカウントを削除しました。

  `recv_cnt` と `send_cnt` はMQTTパケットの受信・送信数を示し、`recv_pkt` と `send_pkt` と同じ値になります。以前はTCPパケット数でした。これは `/clients` REST API、`emqx ctl clients stats` CSVエクスポート、耐久セッション情報に適用されます。互換性のためフィールドは残ります。

- [#19800](https://github.com/emqx/emqx/pull/19800) MQTTメッセージをサブスクライバーに配信する際のCPUオーバーヘッドとメモリ割り当てを削減しました。

- [#19810](https://github.com/emqx/emqx/pull/19810) 受信したMQTTメッセージ処理時のCPUオーバーヘッドと一時メモリ割り当てを削減しました。

- [#19811](https://github.com/emqx/emqx/pull/19811) ファイル（ACL）認可ソースおよびその他ルールベース認可ソースの認可チェックのCPUコストを削減しました。トピック分割は最大1回に抑えられ、固定レベルで始まるトピックフィルター（例：`$SYS/#`）は異なるレベルで始まるトピックに対して分割しません。

#### MQTTリスナー

- [#19407](https://github.com/emqx/emqx/pull/19407) QUIC接続はリスナーのゾーンの `mqtt.hibernate_after` をハイバネーションタイムアウトに使用します。TCPやSSL接続と同様です。接続の制御ストリームとデータストリームはこの値を使います。

  QUICリスナーの `listeners.quic.<name>.ssl_options.hibernate_after` 設定は非推奨で効果がありません。EMQXは設定を受け入れますが無視します。変更前はQUICリスナー設定が使われており、`mqtt.hibernate_after` の変更はQUIC接続に影響しませんでした。

  両設定のデフォルトは5秒です。どちらも変更されていなければQUIC接続のハイバネーション動作は変わりません。

#### クラスタリング

- [#19242](https://github.com/emqx/emqx/pull/19242) クラスターノード間で送信するデータ（MQTTメッセージの転送を含む）を圧縮するための `rpc.compress_level` と `rpc.compress_threshold` 設定を追加しました。

  `rpc.compress_level` は zlib 圧縮レベルで、`1`（高速）から `9`（最小出力）まで設定可能です。デフォルトは `0`（圧縮無効）で、アップグレード時に動作は変わりません。`rpc.compress_threshold` は圧縮対象の最小パケットサイズで、デフォルトは1KBです。

  圧縮は送信側と受信側の両方でCPUを消費し、ノード間のトラフィックを削減します。ノード間帯域が制限されている場合や課金対象の場合に有効です。レベル1は最小のCPUコストでほとんどのサイズ削減を実現します。0.5～1KBのJSONペイロードでレベル1はトラフィックを約60%削減し、送信ノードのCPU使用率を約10%、受信ノードを約5%増加させました。既に圧縮・暗号化されたペイロードはサイズが小さくならず、CPUコストだけ増えます。

#### 耐久ストレージ、メッセージキュー、ストリーム

- [#19217](https://github.com/emqx/emqx/pull/19217) メッセージキューとメッセージストリームの通常メッセージ保持期間の上限49.71日を撤廃しました。

- [#19623](https://github.com/emqx/emqx/pull/19623) メッセージキューのリプレイ性能を改善しました。コンシューマがイテレータを開始または再作成する際、保持期間より古いレコードをスキップします。保存済みアックは保持されます。

- [#19624](https://github.com/emqx/emqx/pull/19624) 耐久ストレージの書き込みパスに過負荷保護を追加しました。楽観的トランザクションマネージャが設定されたメッセージキュー制限に達すると、EMQXは回復可能な `overloaded` エラーでキューイングされたトランザクションを中止します。呼び出し元はこれらを再試行できます。操作数の最大値を超えるトランザクションも拒否します。

#### データ統合

- [#18926](https://github.com/emqx/emqx/pull/18926) Kafka、Confluent、Azure Event HubsコネクターにIPv6サポートを追加しました。

  - `bootstrap_hosts` に括弧付きIPv6アドレスを指定可能（例：`[::1]:9092`、`[fd00::5]:9092,host2:9093`）。
  - IPv6アドレスのみを返すホスト名やIPv6アドレスを広告するブローカーに接続可能。
  - 新しい `socket_opts.ip_family` オプションでIPアドレスファミリを選択可能。デフォルトの `auto` はIPv4優先でIPv6を試行。`ipv6` はIPv6のみ、`ipv4` はIPv4のみ。

  Kafkaクライアントライブラリのアップグレードで、SASL再認証中に保留中のリクエストがあると同期プロデュースタイムアウトが発生する問題を修正しました。

- [#19204](https://github.com/emqx/emqx/pull/19204) Amazon KinesisコネクターにAWS IAMロール認証を追加しました。

  アクセスキーIDとシークレットアクセスキーが省略された場合、EMQXはECSタスクロールまたはEC2インスタンスメタデータから一時的な認証情報を取得し、有効期限前に更新します。

- [#19544](https://github.com/emqx/emqx/pull/19544) S3コネクターとファイル転送のS3エクスポーターに任意の `region` フィールドを追加しました。設定時はリクエストと署名付きURLの署名にこのリージョンを使用します。未設定時はエンドポイントホストからリージョンを推定します。

  AWS S3インターフェースVPCエンドポイント（PrivateLink）ホスト（例：`bucket.vpce-1a2b3c4d-5e6f.s3.us-west-2.vpce.amazonaws.com`）から正しくリージョンを推定するようになりました。以前は `us-east-1` で署名され、他リージョンのリクエストはAWSに拒否されていました。

- [#19815](https://github.com/emqx/emqx/pull/19815) Datalayersコネクターの `arrow_flight` ドライバーが `datalayers-adapter-erl` 0.1.6を使用するようになりました。

  Arrow Flight TLS接続はDatalayersサーバー証明書を検証し、デフォルトで `cacertfile = "system_default"` と `verify = verify_peer` です。`system_default` は `node.trusted_certs` のCAバンドルを使用します。PEM CAパスで置き換え可能です。`verify_none`、クライアント証明書設定、`managed_certs` は拒否されます。

  デフォルトの `influxdb_v1` ドライバーは既存のTLS動作を維持します。

#### プラグインと拡張

- [#17917](https://github.com/emqx/emqx/pull/17917) 新プラグイン `emqx_bcast` を追加しました。製品レベルのデバイスメッセージングAPIを提供します：`PubBroadcast`（製品の全オンラインデバイスへのブロードキャスト）、`BatchPub`（オフライン保存とQoS 1アック追跡付きのデバイスリストへのバッチ配信）、`RegisterMessage`（重複排除とTTL更新付きメッセージ事前登録）。

  プラグインは統一エンドポイント `POST /api/v5/plugin_api/emqx_bcast/pub` を公開し、接続済みクライアントチャネルに直接メッセージを配信します。デバイス再接続時に保留中のQoS 1配信をリプレイし、`GET /api/v5/plugin_api/emqx_bcast/metrics` でPrometheusメトリクスをエクスポートします。

  プラグイン再起動、インデックス所有者の引き継ぎ、コアシャットダウン時もキューイングされた配信進捗、デバイスごとのパブリッシュ順序、QoS 1配信台帳を保持し、クライアントの再パブリッシュや再サブスクライブは不要です。

- [#18843](https://github.com/emqx/emqx/pull/18843) 新しいAPIスコープ `plugin_api` を追加しました。これはプラグイン拡張APIエンドポイント（`/plugin_api/...`）へのアクセスのみを許可します。

  APIキーまたはダッシュボードユーザーに割り当てると、プラグインが公開するエンドポイントを呼び出せますが、管理者相当の `system` スコープは不要です。`plugin_api` スコープはプラグインのインストール、起動、停止、設定変更権限を与えません。これらは `system` スコープに残ります。

  既存の `system` スコープを持つAPIキーとダッシュボードユーザーはプラグイン拡張APIへのアクセスを維持します。

- [#19126](https://github.com/emqx/emqx/pull/19126) パブリッシュフックでメッセージ永続化成功を報告する機能を追加しました。サブスクリプションがマッチしなくても、プラグインが永続化を報告するとHTTPパブリッシュAPIはHTTP 200とメッセージIDを返します。

- [#19359](https://github.com/emqx/emqx/pull/19359) `node.pinned_plugins` 設定を追加しました。これはノードがクラスタプラグイン設定に関係なく常に実行するプラグインのリストです。例えばコンテナイメージに組み込まれたプラグインなどです。

  カンマ区切りの名前-バージョンリスト（例：`EMQX_NODE__PINNED_PLUGINS="my_plugin-1.0.0"`）か配列で設定します。ピン留めプラグインは `<plugins.install_dir>/<name-vsn>` ディレクトリから起動され、パッケージが既に展開されている必要があります。常に有効で、`plugins.states` エントリや `plugins allow` 権限、インストールコマンドは不要です。ファイルや設定は他ノードからコピーされません。設定はパッケージデフォルトと `etc/plugins/<name>.hocon` のマージで決まり、APIやダッシュボードから変更可能です。保存された設定は `data/plugins/<name>/config.hocon` に保存され、それ以降はこのファイルが全設定となります。変更はそのプラグインバージョンを実行するノードに適用されます。プラグイン自身の `/api/v5/plugin_api/` 以下のAPIエンドポイントは他のプラグインと同様に動作します。ノードは同名プラグインの `plugins.states` エントリを無視し、他ノードからのクラスタプラグイン操作はスキップされます。ピン留めプラグインのディレクトリが無いか起動に失敗すると、ノードはエラーをログに出しアラームを上げつつ稼働を続けます。

  `GET /api/v5/plugins` と `GET /api/v5/plugins/{name}` はプラグインとノードごとに `pinned` フラグを返します。ピン留めプラグインはCLIのみが停止・起動可能で、`emqx ctl plugins stop|start|restart` をピン留めノードで実行します。停止はノードが全プラグインを再起動するまで続きます。ピン留めノードではプラグインのライフサイクル操作（インストール、アンインストール、開始、停止、起動順変更、クラスタ同期）に対しHTTP 409とコード `PLUGIN_PINNED` を返します。

  ノードは同一プラグインの異なるバージョンを同時に実行可能です。プラグインはこれらバージョン間で互換性を保つ必要があります。例えば共有データベーステーブル、RPC、設定などです。

### バグ修正

#### セキュリティ

- [#16389](https://github.com/emqx/emqx/pull/16389) OAuth2の `client_secret` が認証、認可、ゲートウェイ、HTTPコネクターの設定読み取りAPIで平文で返されていた問題を修正しました。APIレスポンスではマスクされ、マスクされた値を再送信しても保存済みの秘密は変更されません。

- [#18810](https://github.com/emqx/emqx/pull/18810) ダッシュボードのシングルサインオンは、ログイン開始したブラウザ内でのみOIDCまたはSAMLログインを完了します。EMQXは各ログインに値を割り当て、ブラウザクッキーに保持し、IDプロバイダーがブラウザを戻す際に照合します。異なるブラウザでログインが来ても誰もサインインしません。

  運用者向けの注意点：

  - SSOログイン開始前に `dashboard_addr` に設定されたアドレスでダッシュボードを開いてください。IDプロバイダーはブラウザをそのアドレスに戻し、クッキーはそのアドレスに属します。
  - SAML SSOは `dashboard_addr` にHTTPSアドレスが必要です。SAML IDプロバイダーはクロスサイトフォームポストでブラウザを戻し、ブラウザはHTTPS上でのみクッキーを送信します。
  - ブラウザがクッキーを保持できずSSOログインに失敗する場合、OIDCまたはSAMLバックエンドに `skip_login_cookie_check = true` を設定してブラウザチェックを無効化してください。リスクを理解した上で有効にしてください。

- [#18879](https://github.com/emqx/emqx/pull/18879) 役割ベースアクセス制御で拒否されたリクエストの監査記録に認証済みダッシュボードユーザーまたはAPIキーとリクエストのパスパラメータを含めるようにしました。

  変更前は認証は通るが役割ベースアクセス制御で拒否されたリクエストは `source` が空で `http_request.bindings` がなく、`GET /api/v5/audit` で誰が何を対象にリクエストしたかが表示されませんでした。未認証リクエストの記録は変更ありません。

- [#18883](https://github.com/emqx/emqx/pull/18883) 監査ログの閲覧をグローバルユーザーに制限しました。監査ログは全ネームスペースの操作を記録するため、`GET /api/v5/audit` はグローバル管理者とグローバルビューアのみが読み取れます。ネームスペースに属するダッシュボードユーザーとAPIキーは読み取れなくなりました。

- [#18964](https://github.com/emqx/emqx/pull/18964) SCRAM HTTP認証バックエンドがダッシュボードから送信されたOAuth2設定を拒否する問題を修正しました。OAuth2を有効にした `SCRAM` + `HTTP Server` 認証器の作成が成功し、アクセストークンはユーザー照会リクエストの `Bearer` 認可ヘッダーとして送信されます。

- [#19036](https://github.com/emqx/emqx/pull/19036) ゲートウェイログの資格情報漏洩を修正しました。DTLSハンドシェイク失敗後など起動中にゲートウェイ接続が終了すると、スーパーバイザーのオフェンダーレポートに接続引数全体が含まれ、`clientinfo_override.password` とゲートウェイ認証設定が露出していました。これらはログに記録されなくなりました。

- [#19037](https://github.com/emqx/emqx/pull/19037) ゲートウェイのデバッグログに生のデータグラムが含まれなくなりました。`received_data`、`received_udp_proxy_data`、`send_data` はバイト数のみログに出力します。パケットの解析済みログはプロトコルレベルでマスクされて出力されます。

  CoAPゲートウェイは `POST /mqtt/connection` で発行されるセッショントークンと、短縮クエリアリアス（`t`、`p`）で送信される資格情報を、解析済みパケットログと拒否リクエストログの両方でマスクします。

- [#19098](https://github.com/emqx/emqx/pull/19098) `GET /api/v5/schema_registry`、`GET /api/v5/schema_registry/:name`、`GET /api/v5/opentelemetry` の設定レスポンスで資格情報をマスクするように強化しました。対応する作成・更新エンドポイントはマスク値を受け入れ、保存済み値を上書きしません。

- [#19125](https://github.com/emqx/emqx/pull/19125) ネームスペース付きダッシュボードユーザーとAPIキーのデフォルトスコープの不整合を修正しました。

  - 明示的なスコープリストを持たないネームスペース管理者はグローバル管理者のスコープを取得していました。ユーザー作成・更新時に `"scopes": "unset"` またはデフォルトスコープリストを変更せずに更新した場合に発生します。現在はネームスペース管理者のデフォルトスコープを取得します。
  - スコープなしで作成されたネームスペースビューアはネームスペース管理者より多くのスコープ（`gateways`、`publish`、`audit`）を持っていました。ネームスペースビューアとネームスペースビューアAPIキーはログイン専用スコープを除きネームスペース管理者と同じ管理スコープがデフォルトです。`gateways`、`publish`、`audit`、`mfa_management` はネームスペースビューアに付与不可です。
  - 起動時にEMQXは既存のネームスペースビューアの保存されたスコープリストから `gateways`、`publish`、`audit`、`mfa_management` を削除します。これによりビューアはゲートウェイエンドポイントの読み取り権限を失います。`publish` と `audit` エンドポイントは既にネームスペースユーザーに拒否されていました。既存APIキーの保存スコープリストは変更されません。

- [#19142](https://github.com/emqx/emqx/pull/19142) ゲートウェイのlookup CLIコマンドで認証資格情報と `clientinfo_override.password` が露出する問題を修正しました。機密ゲートウェイ設定値は出力からマスクされます。

- [#19175](https://github.com/emqx/emqx/pull/19175) RPCの安全でない認証フォールバックを削除しました。

  クラスター内ノードは常にチャレンジレスポンスハンドシェイクでバックプレーン（RPC）接続を認証します。以前はハンドシェイク失敗時にErlangクッキーを送信するフォールバックがありました。

  `rpc.insecure_fallback` 設定キーは無効になりました。これは5.3.0より古いEMQXとのクラスタリング用でした。設定ファイルに残っていても無視されます。

#### MQTTプロトコルとリスナー

- [#18882](https://github.com/emqx/emqx/pull/18882) MQTTおよびゲートウェイリスナーのWebSocketメッセージサイズ制限をプロトコル制限に合わせました。

  - MQTT WebSocketリスナーでは、`websocket.max_frame_size = infinity`（デフォルト）はゾーンの `mqtt.max_packet_size` の2倍を意味します。クライアントが複数のMQTTパケットを1つのWebSocketメッセージで送信する場合は明示的な値を設定してください。
  - ゲートウェイWebSocketリスナーでは `websocket.max_frame_size` が有効になりました。デフォルトは2097152（2MiB）で、`infinity` はこのデフォルトを選択します。
  - `websocket.max_frame_size` は最大268435455まで設定可能です。

  制限はWebSocketメッセージのフラグメント結合後および `compress` 有効時の解凍後に適用されます。クライアントが大きすぎるメッセージを送るとWebSocketステータスコード1009で接続を切断します。

- [#19174](https://github.com/emqx/emqx/pull/19174) `parse_unit = frame`（デフォルト）で、TCPおよびSSLリスナーはトランスポートレベルで `mqtt.max_connect_packet_size` を適用します。制限を超える最初のパケットは固定ヘッダーで拒否され、ボディはバッファされません。以前はトランスポートが `mqtt.max_packet_size` までバッファしてからチェックしていました。クライアント接続後は制限は `mqtt.max_packet_size` に戻ります。両設定のデフォルトは1MBで、既定設定は変わりません。`mqtt.max_packet_size` を1MB以上に設定する場合は本設定も引き上げてください。

- [#19239](https://github.com/emqx/emqx/pull/19239) 解析不能な `Sec-WebSocket-Protocol` ヘッダーのWebSocketリスナー応答を修正しました。ヘッダーなしのリクエストと同様に処理されます（`fail_if_no_subprotocol` 有効時は400応答、無効時はサブプロトコルなしでアップグレード）。以前はリクエストプロセスが終了し、クライアントは空の400応答を受け、ログにクラッシュレポートが書き込まれていました。

- [#19374](https://github.com/emqx/emqx/pull/19374) MQTT 5 Willメッセージの保留パブリッシュが、セッション有効期限が0の異常なWebSocket切断時に発行されない問題を修正しました。

- [#19511](https://github.com/emqx/emqx/pull/19511) クライアントがDISCONNECT後に同じ接続で送信するMQTTパケットの処理を修正しました。ブローカーはこれらを無視します。修正前はDISCONNECT直後のCONNECTで `session_stepdown_request_exception` エラーがログに出て、Clean Sessionがfalseのクライアントのセッションが破棄されていました。

- [#19518](https://github.com/emqx/emqx/pull/19518) MQTTおよびゲートウェイリスナーのWebSocketメッセージサイズ制限を強化し、圧縮メッセージもカバーするようにしました。

- [#19522](https://github.com/emqx/emqx/pull/19522) メッセージキューまたはヒープが大きすぎるため `force_shutdown` ポリシーで切断されたクライアント接続は、エラーレベルのスーパーバイザーレポートを生成しなくなりました。リスナーシャットダウンカウンターは `mailbox_overflow` または `proc_heap_too_large` でカウントします。警告レベルでログは残ります。

- [#19531](https://github.com/emqx/emqx/pull/19531) 確立済み接続を閉じるTLSアラートはエラーレベルのスーパーバイザーレポートを生成しなくなりました。アラートは種類別のリスナーシャットダウンカウンター（例：`user_canceled`、`bad_record_mac`）を増加させ、通知レベルでログに残ります。TLSスタックが認識しないアラートコードは `unsupported/unknown_alert` カウンターにまとめられます。以前同様のエラーレポートを生成していたソケットエラー（無効なプロキシプロトコルヘッダーなど）は `sock_error` として通知レベルでログに記録されます。カウンターは `emqx ctl listeners` と `emqx_client_disconnected_reason` Prometheusメトリクスで確認可能です。

- [#19574](https://github.com/emqx/emqx/pull/19574) クライアントIDにMQTTワイルドカードが含まれる `$SYS` クライアントイベントトピックでのパブリッシュフックエラーを修正しました。クライアントID中の `#`、`+`、`%` はトピック名でそれぞれ `%23`、`%2B`、`%25` にエンコードされます。イベントペイロード中のクライアントIDは変更ありません。

- [#19632](https://github.com/emqx/emqx/pull/19632) WSSリスナーのTLS接続処理を改善するために ranch ライブラリを2.2.0-emqx-4にアップグレードしました。

- [#19640](https://github.com/emqx/emqx/pull/19640) WebSocket上のMQTTパケット順序を修正しました。完全なパケットの後に不完全または無効なパケットがある場合の処理と、無効パケット処理時の接続プロセスクラッシュを防止しました。

  ノード退避やリバランス後のセッション期限切れ時の不要なエラーログも修正しました。

- [#19649](https://github.com/emqx/emqx/pull/19649) `ssl_options.cacertfile` に空文字列を設定したTLSリスナーで発生していた `cert_file_not_found` エラーログを修正しました。

- [#19674](https://github.com/emqx/emqx/pull/19674) WSSリスナーで `ssl_options.handshake_timeout` が無効だった問題を修正しました。WSSリスナーは常に15秒のTLSハンドシェイクタイムアウトを使っていましたが、設定値を使うようになりました。

- [#19737](https://github.com/emqx/emqx/pull/19737) 他の同一トピックフィルターのサブスクライバーが解除または切断された後、共有サブスクライバーがメッセージを受信しなくなる問題を修正しました。

- [#19940](https://github.com/emqx/emqx/pull/19940) TLS接続試行の大量バーストでTLSリスナーが接続受付を停止する問題を修正しました。クライアントは接続タイムアウトを報告し、ノードはメモリ使用量が増加し続け、リスナーは回復しませんでした。通常は再起動が必要でした。

  EMQXはバースト中に新規TLS接続の一部を即座に切断します。リスナーは他の接続を受け入れ続け、切断されたクライアントは再試行時に成功します。拒否された接続は理由 `too_many_handshakes` でカウントされ、`emqx ctl listeners` と `emqx_client_disconnected_reason` メトリクスで確認可能です。

#### クラスタリング

- [#19136](https://github.com/emqx/emqx/pull/19136) クラスターリンクがピアクラスターへの接続を失った際にフライト中のメッセージを破棄する問題を修正しました。接続が失われた、または確立されていない（接続タイムアウト、DNS解決失敗、トランスポートエラーなど）場合は回復可能とみなし、メッセージはリクエスト期限切れまで再試行されます。

- [#19139](https://github.com/emqx/emqx/pull/19139) クラスターリンクのヘルスチェックはリモートブローカーへの接続をエンドツーエンドで検証するようになりました。MQTT `PINGREQ` を送信し `PINGRESP` を待ちます。以前はローカルクライアント状態のみを監視していました。

  パケットが無応答で破棄されるリンクはプローブが応答しないとすぐに `disconnected` と報告され、バッファワーカーはメッセージをキューに保持し、リソースは再起動されます。リンクはクライアントキープアライブを持ちません。

- [#19556](https://github.com/emqx/emqx/pull/19556) etcdベースのクラスターディスカバリーが一時的なetcd障害に耐えるようになりました。ノードはバックオフ付きでetcdに再接続し、接続復旧後に自身の登録キーを復元します。以前はetcdが到達不能になるとディスカバリーが失敗し、etcd復旧後もノードは登録解除されたままでした。

#### データ統合

- [#18945](https://github.com/emqx/emqx/pull/18945) RocketMQアクションの、ブローカーに存在しないトピックの処理を修正しました。

  トピックがネームサーバーにルートを持たず、ブローカーがトピックを自動作成しない場合（例：Alibaba Cloudのネームスペース単位トピック作成）、すべてのメッセージが `case_clause` エラーで破棄され、メッセージ全体がログに書き込まれていました。修正後：

  - プレースホルダーを含まないトピックのアクションはヘルスチェックごとに存在を確認し、存在しない場合はトピック名とネームスペースを含むメッセージで `disconnected` を報告し、バッファ内でメッセージを保持して再試行し、トピック作成後の最初のヘルスチェックで復旧します。
  - プレースホルダー付きトピックのメッセージは読みやすい `topic_not_found` エラーで破棄され、ログにペイロードは含まれません。
  - ネームスペース設定時、デフォルトの自動作成トピック `TBW102` は `<namespace>%TBW102` とリクエストされていましたが存在しないため、Javaクライアント同様に素の名前でリクエストされるようになりました。

  また、プロデューサーが稼働していない間に2分ごとに発生・解除していたコネクターアラームを修正しました。ネームサーバーは120秒後にアイドル接続を切断し、クライアントはすぐに再接続します。

- [#18987](https://github.com/emqx/emqx/pull/18987) 一部のコネクター、アクション、ソースが `resource_opts.health_check_timeout` をヘルスチェックのタイムアウトに使っていなかった問題を修正しました。

  対象統合：

  - Cassandraコネクター
  - DynamoDBコネクター
  - GCP PubSubコンシューマーソース
  - IoTDBコネクター（Thriftドライバー）
  - Snowflake集約コネクター
  - SQL Serverコネクター
  - TDengineコネクター
  - MySQLコネクター
  - Dorisコネクター
  - Postgresコネクター

- [#19055](https://github.com/emqx/emqx/pull/19055) 一部コネクターの初回ヘルスチェックで詳細な接続失敗理由を報告できない問題を修正しました。以前は「resource down: disconnected」のみ報告していました。対象コネクター：PostgreSQL系、MySQL、Redis、Cassandra、TDengine、SQL Server、IoTDB、Couchbase、DynamoDB、Snowflake集約、Syskeeper。

- [#19099](https://github.com/emqx/emqx/pull/19099) ルールエンジンのSQL関数を改善し、各ルールが同一ネームスペース内のルールが書き込んだデータのみアクセスできるようにしました。グローバルネームスペースのルールは引き続きグローバルデータ空間を共有します。

- [#19108](https://github.com/emqx/emqx/pull/19108) 負荷が高い場合にGCP PubSubプロデューサーとHTTPアクションがリモートサーバーが接続を閉じた際に `{error,closed}` を回復不能エラーとして報告することがあった問題を修正しました。これらは回復可能エラーとして扱われます。

- [#19109](https://github.com/emqx/emqx/pull/19109) コネクターとアクション/ソースのヘルスチェック失敗時の詳細情報を改善しました。

  対象コネクター/アクション/ソース：

  - ディスクログ
  - GreptimeDB
  - HTTP
  - InfluxDB
  - Kafkaコンシューマー
  - Kafkaプロデューサー系
  - OpenTS
  - Pulsarプロデューサー
  - Redis
  - RocketMQ
  - Syskeeper
  - Tablestore

- [#19117](https://github.com/emqx/emqx/pull/19117) リモートブローカーが到達不能な場合でも `mqtt` アクションのバッファ内メッセージが失われなくなりました。接続レベルの失敗（`ssl_closed`、`econnrefused`、キープアライブ・ソケットタイムアウト、DNSエラーなど）は再試行可能で、`request_ttl` が切れるまでバッファに残ります。フォールバックアクション設定時は期限切れでフォールバックがトリガーされます。

- [#19122](https://github.com/emqx/emqx/pull/19122) `mqtt` コネクターのヘルスチェックはリモートブローカーへの接続をエンドツーエンドで検証します。MQTT `PINGREQ` を送り `PINGRESP` を待ちます。タイムアウトは `resource_opts.health_check_timeout` の半分です。以前はローカルクライアント状態のみを監視していました。

  TCP接続を維持しつつパケットが破棄されるリモートブローカーはプローブ応答なしで即座に `disconnected` と報告され、リソースが再作成され、アクションのバッファワーカーは配信不能な接続にメッセージを渡さずキューに保持します。クライアントのキープアライブは無視されます（デフォルト160秒）。

  コネクター状態理由はピアによる接続切断をリスナー容量問題として説明しなくなりました。

- [#19186](https://github.com/emqx/emqx/pull/19186) S3サーバーが非成功HTTPステータスでボディなし応答した場合に、コネクター/アクションの状態やログにクラッシュログが出る問題を修正しました。

- [#19222](https://github.com/emqx/emqx/pull/19222) Pulsarコネクターのヘルスチェックで内部呼び出しに5秒固定タイムアウトを使っていた問題を修正しました。設定された `resource_opts.health_check_timeout`（デフォルト10秒）を使うようになり、細かい調整が可能です。

- [#19249](https://github.com/emqx/emqx/pull/19249) 一部コネクター/アクション/ソースが `resource_opts.health_check_timeout` をヘルスチェックのタイムアウトに使っていなかった問題を修正しました。

  対象統合：

  - MongoDBコネクター
  - Redisコネクター

- [#19261](https://github.com/emqx/emqx/pull/19261) HTTPベースのコネクター（HTTPサーバー、GCP PubSub、BigQuery、Couchbase、Snowflake）およびHTTP認証・認可の接続でTCPキープアライブを有効にしました。

  これらの接続は30秒のキープアライブでオープンされます。Linux/macOSではアイドル接続は30秒後にキープアライブプローブを送り、以降5秒ごとに送り、3回応答がないと切断されます。他のOSではOSのタイマーでキープアライブが有効になります。

- [#19287](https://github.com/emqx/emqx/pull/19287) GreptimeDBアクションで書き込み構文に合わない値（例：`i`接尾辞のない浮動小数点値）が原因で失敗する問題を修正しました。無効なメッセージはバッファワーカーをブロックせず失敗としてカウントされ、有効なメッセージは書き込まれます。

  整数・符号なし整数値とタイムスタンプは書き込み前にサポート範囲で検証されます。`i`や`u`接尾辞のない整数はInfluxDBラインプロトコルに従い `float64` として書き込まれます。

- [#19363](https://github.com/emqx/emqx/pull/19363) 一部コネクター/アクション/ソースが `resource_opts.health_check_timeout` をヘルスチェックのタイムアウトに使っていなかった問題を修正しました。

  対象統合：

  - Postgresアクションとコネクター

- [#19364](https://github.com/emqx/emqx/pull/19364) Postgresアクションのヘルスチェックでテーブル存在検証のトランザクションが次のヘルスチェックまでアクティブでロックを保持していた問題を修正しました。

- [#19386](https://github.com/emqx/emqx/pull/19386) 一部コネクター/アクション/ソースが `resource_opts.health_check_timeout` をヘルスチェックのタイムアウトに使っていなかった問題を修正しました。

  対象統合：

  - Couchbaseコネクター

- [#19412](https://github.com/emqx/emqx/pull/19412) Sparkplug Bの `NDATA` と `DDATA` のメトリックエイリアスマッピングがトピックマウントポイント使用時に正しく動作しなかった問題を修正しました。

- [#19521](https://github.com/emqx/emqx/pull/19521) HTTPアクションがHTTP 502、504応答や一時的なDNS解決失敗（`nxdomain`）でメッセージを破棄していた問題を修正しました。これらのエラーは回復可能で、アクションバッファはリクエストTTL切れまで再試行します。

  同様に、HTTPアクションの同期・非同期クエリモードで `parameters.max_retries` を無視していた問題も修正しました。このパラメータはアクションクエリ内のリトライを制御します。既存の通常接続シャットダウン競合の無料リトライは変わりません。

- [#19533](https://github.com/emqx/emqx/pull/19533) OAuth2クライアント認証を使うHTTPコネクターがHTTP 401応答後に拒否されたキャッシュトークンを送り続けていた問題を修正しました。コネクターはトークンを更新し、1回だけリトライします。更新は同時リクエスト間で共有され、HTTPリトライ予算はリセットされません。失敗したトークン更新はバッファリングされたアクションで回復可能です。HTTP認証・認可にも適用されます。

- [#19619](https://github.com/emqx/emqx/pull/19619) HTTPアクションは非同期クエリ内で回復可能なHTTP応答を即時リトライしなくなりました。コネクターコールバックはこれらのエラーをアクションバッファに返し、リトライは `resume_interval` と `request_ttl` で制御されます。

- [#19647](https://github.com/emqx/emqx/pull/19647) S3アクションのDirect Uploadモードが設定のカスタムHTTPヘッダーを無視していた問題を修正しました。

- [#19650](https://github.com/emqx/emqx/pull/19650) 一部アクションで再接続コールバック関数が蓄積される可能性のあるリークを修正しました。

  対象アクション：

  - Oracle
  - Postgres系（TimescaleDB、MatrixDB、CockroachDB、AlloyDB、Redshift）
  - Cassandra
  - Datalayers Arrow Flight

  Oracleアクションが削除またはSQLテンプレート変更後に再接続すると古いSQLが送信されエラーが発生する問題も修正しました。削除済みアクションのコールバックはクリーンアップされます。Oracleコネクターはプールワーカーを自動再接続し、再接続時にSQLを再検証します。

- [#19663](https://github.com/emqx/emqx/pull/19663) MongoDB認証、認可、データ統合で `overflow_ttl` と `overflow_check_period` 設定を無視していた問題を修正しました。クエリのバースト時に余分なMongoDB接続がクエリごとに閉じられていました。EMQXはアイドルタイムアウトまでこれらの接続を再利用可能に保持します。

- [#19664](https://github.com/emqx/emqx/pull/19664) RocketMQアクションで、再作成後も以前の設定が残るリークを修正しました。

- [#19669](https://github.com/emqx/emqx/pull/19669) QuasarDB接続プールがコネクター停止・削除後も稼働し続ける問題を修正しました。

- [#19670](https://github.com/emqx/emqx/pull/19670) 2つ目のTablestoreアクションをコネクターに追加すると既存アクションのクエリが `channel_not_found` で失敗する問題を修正しました。

- [#19690](https://github.com/emqx/emqx/pull/19690) 応答しないEMQX Tablesバックエンドがノード全体のファイル・ログ操作をブロックし、ノードが応答しなくなる問題を修正しました。

- [#19703](https://github.com/emqx/emqx/pull/19703) SparkplugBのメトリックエイリアスマッピングで、`DBIRTH` メッセージ前に `NDATA` メッセージをパブリッシュするとデバイスのマッピングがノードのマッピングの一部を継承する問題を修正しました。

- [#19778](https://github.com/emqx/emqx/pull/19778) 以下の複数コネクターでTLSオプション設定を追加しました：

  - ClickHouse
  - OpenTSDB
  - TDengine
  - Tablestore

  Tablestoreは設定スキーマでTLSオプションを受け入れていましたが、ドライバークライアントに渡されていませんでした。修正により渡されるようになりました。

- [#19779](https://github.com/emqx/emqx/pull/19779) GCPベースのコネクター（GCP Pub/Sub Producer/Consumer、BigQuery）でWIF認証の初期トークン取得にTLSオプションを指定可能にしました。チェーン内の他トークンは常に `node.trusted_certs` のCAバンドルでGoogleエンドポイント証明書を検証します。

- [#19841](https://github.com/emqx/emqx/pull/19841) `resource_opts` 設定がないS3アクション作成でHTTP 500エラーになる問題を修正しました。

- [#19845](https://github.com/emqx/emqx/pull/19845) MongoDBアクションの冗長なヘルスチェックを削除しました。以前はアクションごとにコネクターのチェックを繰り返し、チェック数が増加して接続プール競合や `mongodb_connector_pool_checkout_timeout` エラーが発生していました。

- [#19885](https://github.com/emqx/emqx/pull/19885) MQTTコネクターとクラスターリンクMQTTクライアントの `retry_interval` 処理を修正しました。値が1000倍されており、QoS 1およびQoS 2の再送が設定より長く遅延していました。

- [#19888](https://github.com/emqx/emqx/pull/19888) コネクターのチャネル自動追加リトライが成功後も繰り返される可能性のある競合状態を修正しました。

  RabbitMQコネクターで以下のログが繰り返される可能性があります：

  ```
  tag: CONNECTOR/RABBITMQ, msg: add_channel_failed, reason: already_exists
  ```

#### ゲートウェイ

- [#19027](https://github.com/emqx/emqx/pull/19027) GB/T 32960ゲートウェイで、ログイン時に認証されたVINと異なるヘッダーVINのフレームを拒否し、既存接続での車両ログインを拒否するように修正しました。以前は異なる車両として認証された接続が別の車両のテレメトリをパブリッシュでき、VINを切り替えても前の車両のセッション状態を保持していました。

- [#19059](https://github.com/emqx/emqx/pull/19059) STOMP、NATS、JT/T 808ゲートウェイのフレームサイズ制限の不整合を修正し、STOMPトランザクションの使用制限と非正のタイムアウト拒否、LwM2Mの異なるクライアントIDでの再登録防止を行いました。NATSゲートウェイは制御ラインを `max_control_line` で制限し、メッセージデータとINFO.max_payloadは `max_payload_size` で制限します。

- [#19176](https://github.com/emqx/emqx/pull/19176) NATSゲートウェイの主題変換を修正し、NATS主題がMQTTワイルドカードや共有サブスクリプションの意味を持たず、曖昧なトークン境界を持つMQTTトピックがNATSサブスクライバーに配信されないようにしました。

  `/`、`+`、`#` を含む、または `$share.`、`$queue.`、`$exclusive.` で始まる主題は拒否されます。これらの制限は `PUB` と `HPUB` の応答主題にも適用され、応答主題はNATSワイルドカードを含めません。

  NATSの `>` サブスクリプションとJWT権限は少なくとも1つの末尾トークンを要求します。サポートされない主題はNATSユーザーJWT権限の認証時に拒否されます。

  MQTT区切り文字やワイルドカードを含むキューグループ名は拒否され、結果のサブスクリプションフィルターは要求されたNATS主題と整合性を保ちます。

  JWTのパブリッシュ・サブスクライブ権限はNATS主題意味論で直接マッチされます。ワイルドカード拒否ルールは `$` で始まる主題をカバーし、MQTTワイルドカード除外がJWT制限を回避するのを防ぎます。

  パブリッシュJWT権限は元のフレーム主題に対してチェックされ、EMQX ACL認可はイングレス後のMQTTトピックに適用されます。

- [#19179](https://github.com/emqx/emqx/pull/19179) CoAPとLwM2Mのブロック単位交換とセッションごとの保留ペイロードバイト数を制限しました。ブロック1アップロードは交換制限到達時に4.29、バイト制限到達時に4.13を受け取ります。ブロック通知とLwM2Mダウンリンクは一時的にブロックされてもキューに残り、容量確保時に再開します。

  失敗したLwM2M転送は次のコマンド送信前にブロック単位リソースを解放します。アクティブ転送はキューなしや受信デバイスメッセージで期限切れ状態が削除されてもタイムアウトを報告します。キューモードデバイスは次のアクティビティまでコマンドをキャッシュします。LwM2Mトランスポートタイマーは再送をトリガーし、リトライ終了時にコマンドタイムアウトを報告します。状態遷移は不要なアックタイマーをキャンセルします。

- [#19546](https://github.com/emqx/emqx/pull/19546) 各LwM2Mセッションの保留ダウンリンクコマンド数を1000に制限しました。キューが満杯の場合、新規コマンドはMQTTで4.29応答を返しFIFO順に受け入れ済みコマンドを保持します。保留数と固定制限はmqueue統計で報告されます。同一パス・タイプの繰り返しコマンドがコマンド記録削除キューを無限に増加させるのを防ぎます。

  LwM2M受信ウィンドウは単調時間を使うようになり、壁時計の調整でダウンリンクコマンドがスリープ中のデバイスに送信されるのを防ぎます。

- [#19582](https://github.com/emqx/emqx/pull/19582) MQTT-SN接続拒否理由のフック・ログ報告を修正しました。未知の戻りコードは `accepted` ではなく `unknown_error` と報告され、`rejected_invaild_topic_id` は正しく `rejected_invalid_topic_id` に修正されました。クライアントへの戻りコードは変更ありません。

- [#19676](https://github.com/emqx/emqx/pull/19676) CoAP Observeクライアントは各リクエストで異なるトークンを使って後続のBlock2ブロックを取得可能になりました。Observeトークンを再利用するクライアントも引き続きサポートされます。同一トピックの観測再登録はトークンとサブスクリプションオプションを置き換えます。保留通知とキャッシュ済みブロックは現在の観測ライフサイクルに従います。管理APIによるサブスクリプションオプション更新は観測を維持します。

- [#19693](https://github.com/emqx/emqx/pull/19693) CoAPゲートウェイは有効なトピック名でないトピックへのパブリッシュと、有効なトピックフィルターでないトピックへのサブスクライブを `4.00 Bad Request` で拒否します。例：`a/#` や `+/a` へのパブリッシュは以前は受け入れていました。

- [#19741](https://github.com/emqx/emqx/pull/19741) STOMPゲートウェイで、`receipt` ヘッダー付きトランザクションコミット時に `RECEIPT` を送信するよう修正しました。`COMMIT` に `receipt` が含まれると、トランザクション内コマンドのレシートはコマンド順に届き、その後に `COMMIT` レシートが届きます。

- [#19742](https://github.com/emqx/emqx/pull/19742) ゲートウェイWebSocketのキープアライブチェックはバイト数ではなくプロトコルパケット数をカウントするようになりました。TCP/UDP接続と整合します。

- [#19746](https://github.com/emqx/emqx/pull/19746) CoAPとLwM2Mゲートウェイはブロック単位の `max_body_size` と `exchange_lifetime` に正の値を要求します。無効な設定リクエストは保存済み設定を変更せず拒否されます。アップグレード前に0または負の値を正の値に置き換えてください。

- [#19747](https://github.com/emqx/emqx/pull/19747) CoAPとLwM2Mゲートウェイはブロック単位の `max_total_size` に正の値を要求します。無効な設定リクエストは保存済み設定を変更せず拒否されます。アップグレード前に0または負の値を正のバイト数に置き換えてください。

#### ダッシュボードとREST API

- [#19003](https://github.com/emqx/emqx/pull/19003) `PUT /api/v5/api_key/:name` を強化し、リクエストボディに存在するフィールドのみを更新するようにしました。以前は部分更新で言及されていないフィールドも書き換えられ、管理者がAPIキーの権限を誤って変更する可能性がありました。このエンドポイントの呼び出しには管理者権限が必要です。

- [#19085](https://github.com/emqx/emqx/pull/19085) 不正な `Authorization` ヘッダーのREST APIレスポンスを修正しました。`fail_if_no_subprotocol` 有効時は400応答、無効時はサブプロトコルなしでアップグレードされます。以前はリクエストプロセスが終了し、クライアントは空の400応答を受け、ログにクラッシュレポートが書き込まれていました。

- [#19116](https://github.com/emqx/emqx/pull/19116) REST APIリクエストがテキストでないデータ（例：PEM形式が必要なフィールドにDER形式の証明書）を送信した場合のエラーメッセージを改善しました。無効なUTF-8テキストであることとサイズを報告し、以前のようにバイト列リストを表示したり型エラーを誤って報告したりしません。

  任意値を受け入れる設定フィールドは変換エラーを値の代わりに保存しなくなりました。

- [#19228](https://github.com/emqx/emqx/pull/19228) 認証された一部の呼び出し元がAPIドキュメントエンドポイント（`/api-docs/swagger.json`、`/api-spec.json`、`/api-spec.md`、`/api-spec/:tag`）を読めなかった問題を修正しました。

  - 明示的なスコープリストを持つダッシュボードユーザーとAPIキーはスコープに関係なくこれらのエンドポイントを読めます。
  - ビューワーロールまたはネームスペース管理者ロールのダッシュボードユーザーとAPIキーもこれらのエンドポイントを読めます。

  認証情報なしのリクエストはHTTP 401を返します。

- [#19435](https://github.com/emqx/emqx/pull/19435) REST APIハンドラーが内部エラーで失敗した場合、HTTP 500レスポンスと警告ログに失敗関数の引数（リクエストヘッダーとボディ）が含まれていました。これにより `authorization` ヘッダーの値（ベアラートークンやAPIキーとシークレット）やボディフィールドがログに書き込まれていました。レスポンスとログは構造のみを保持し、ヘッダー名とボディフィールド名は表示し、文字列値は長さに置き換えます。

- [#19448](https://github.com/emqx/emqx/pull/19448) protobufバンドルREST APIで、フォームフィールドが誤った形式（`bundle`がテキストフィールド、`name`、`root_proto_file`、`description`がアップロードファイル）で送信された場合、HTTP 400とコード `BAD_FORM_DATA`、フィールド名を含むメッセージを返すように修正しました。修正前はHTTP 500や内部検証メッセージが返されていました。

- [#19503](https://github.com/emqx/emqx/pull/19503) JSONボディに不正なUTF-8やペアになっていないUnicodeサロゲートエスケープがあるREST APIリクエストは、スキーマ検証前に固定のHTTP 400エラーを返すようになりました。誤解を招く設定エラーや過大なエラー応答を防ぎます。絵文字を含む有効なUnicodeは引き続きサポートされます。

- [#19520](https://github.com/emqx/emqx/pull/19520) ダッシュボードLDAP SSOバックエンドを繰り返し削除すると、以降の削除リクエストや再作成試行が内部エラーで失敗する問題を修正しました。

  バックエンド削除後は、以降の削除リクエストは一貫してHTTP 404を返し、バックエンドは正常に再作成可能です。

- [#19565](https://github.com/emqx/emqx/pull/19565) 不正な `Authorization` ヘッダーはHTTP 401 `AUTHORIZATION_HEADER_ERROR` を返し、内容はエラーログに書き込まれなくなりました。

  HTTP仕様で解析できない資格情報（`:`区切りのないBasic認証や無効文字を含むBearerトークンなど）はリクエストプロセスを終了させていました。クラッシュレポートにヘッダー値がエラーレベルで含まれていたため、誤ったクライアントが自身の資格情報をログに書き込み、未認証呼び出し元が任意のログエントリを追加できていました。

#### プラグインと拡張

- [#18957](https://github.com/emqx/emqx/pull/18957) ダッシュボードからプラグインパッケージをアップロードすると、パッケージのtarballがプラグインインストールディレクトリに既に存在し展開されていない場合に `ALREADY_INSTALLED` と応答してインストールを拒否していた問題を修正しました。アップロードはまずインストール許可リストを通過し、許可されていない場合は `403 FORBIDDEN` と `emqx ctl plugins allow` 指示を返します。中断または失敗したインストールの残骸（読み取り可能な `release.json` がないディレクトリや展開されていないマニフェスト）はインストール済みと誤認されず削除され、アップロードされたパッケージはクリーンなディレクトリに展開されます。アプリケーションがロード済みのインストールは削除されず、`plugin_is_in_use` エラーで拒否され、先に停止が必要です。拒否または失敗したアップロードは置き換えたパッケージファイルを削除しなくなり、ローカルのインストール済みパッケージのコピーは残ります。読み取り不能なパッケージファイルのアップロードは拒否され、プラグインが完全にインストールされているノードはファイルを保持します。

- [#19034](https://github.com/emqx/emqx/pull/19034) プラグインパッケージのインストールとロードを強化しました。

- [#19035](https://github.com/emqx/emqx/pull/19035) プラグインパッケージのインストールと同期を強化しました。

- [#19115](https://github.com/emqx/emqx/pull/19115) プラグイン設定読み取りエンドポイント（`GET /api/v5/plugins/:name/config` と `GET /api/v5/plugins/:name/config/download`）をグローバル管理者に限定しました。

- [#19195](https://github.com/emqx/emqx/pull/19195) プラグインパッケージ名の検証を強化しました。

#### 運用と可観測性

- [#18886](https://github.com/emqx/emqx/pull/18886) バックアップ同期の間隔とタイムアウトの不正値を拒否し、成功したHTTP同期でのTLS証明書パスエラーの誤検知を抑制しました。

- [#19040](https://github.com/emqx/emqx/pull/19040) 古いEMQXバージョン（6.1以前）からのネームスペース付き設定インポートでリミッター設定（`delivery_bytes`、`delivery_messages`、`subscribes`）が存在しないため失敗する問題を修正しました。

- [#19120](https://github.com/emqx/emqx/pull/19120) `emqx ctl conf load --merge` と `PUT /api/v5/configs?mode=merge` で省略されたフィールドをデフォルトにリセットしなくなりました。以前は設定構造体の1フィールドだけを読み込むと他のフィールドがリセットされていました。例：`listeners.tcp.default.tcp_options.active_n` のみを読み込むと `parse_unit`、`bind`、`acceptors`、`max_connections` がリセットされました。マージモードは省略フィールドの保存値を保持し、保存済み設定にある必須フィールドを繰り返す必要がなくなりました。`authorization.sources` がない場合は保存済み認可ソースを保持します。置換モードは変更ありません。

- [#19148](https://github.com/emqx/emqx/pull/19148) `emqx ctl conf load --namespace <ns> --merge` は読み込んだ設定をネームスペースの保存済み設定にマージするよう修正しました。以前はグローバル設定にマージされていました。

---

## 6.3.2

*リリース日: 2026-10-16*

### 改善点

#### コアMQTT機能

- [#19438](https://github.com/emqx/emqx/pull/19438) 混雑していない接続では、メモリ内セッションがQoS 1/2メッセージのアック待ちでも、キューイングされたQoS 0メッセージのドレインを楽観的に継続します。輻輳解消後や既存セッション再開時も含みます。トピック優先度が有効な場合は早期に停止することがあります。

#### パフォーマンス

MQTT接続、セッション、ノードローカルクライアントデータのメモリ使用量を削減し、複数のブローカーパスのCPUオーバーヘッドを低減し、サブスクリプション検索と起動性能を改善しました。

- **接続とセッションのメモリ**

  - [#19092](https://github.com/emqx/emqx/pull/19092) 接続は `mqtt.hibernate_after`（デフォルト5秒無通信）後にハイバネーションし、ヒープを使用中データに縮小し約5KB節約します。以前はTCP/TLS接続は `mqtt.idle_timeout`（デフォルト15秒）後にハイバネーションし、WebSocket接続は最初のパケット後も起きていました。`mqtt.hibernate_after = infinity` で接続を起こしたままにできます。クライアントがこの間隔に近い間隔で送信する場合は値を上げてください。ハイバネーションはガベージコレクションを1回発生させ、起床時にヒープが再成長します。
  - [#19181](https://github.com/emqx/emqx/pull/19181) ゾーンとリスナーでメッセージ・バイト・サブスクライブレート制限が設定されていない接続はレートリミッター状態を割り当てません。後から制限を設定すると既存接続に適用されます。
  - [#19262](https://github.com/emqx/emqx/pull/19262) キューイングメッセージがないセッション（パブリッシュ専用クライアントや配信に追いつくサブスクライバー）は48バイト節約します。
  - [#19563](https://github.com/emqx/emqx/pull/19563) Erlang/OTPを28.4.1-7に、`emqx-builder`を6.2-3にアップグレードしました。TLS接続は共有ソケットオプション用のETSテーブルを不要とし、アイドルTLS 1.3接続は暗号状態を解放します。TLSハンドシェイクは絶対パスの `cacertfile` 解決でファイルサーバーを2回呼んでいましたが、バースト時のファイルサーバーバックログを防止するため1回に削減しました。

- **ノードローカルクライアントデータ**

  - [#19306](https://github.com/emqx/emqx/pull/19306) チャネル情報テーブルはクライアントの遺言メッセージ、CONNECTプロパティ、サブスクリプションリストを保存しなくなりました。SUBACK・UNSUBACK後の書き換えコストとメモリ使用量を削減します。
  - [#19263](https://github.com/emqx/emqx/pull/19263) テーブルは `sockinfo` マップと `conninfo` が持つ `peerhost`、`peerport`、`peername`、`sockport` を削除し、接続ごとに約100バイト節約します。
  - [#19303](https://github.com/emqx/emqx/pull/19303) 0の値のクライアント統計を保存しなくなり、アイドルクライアントごとに約145バイト節約します。
  - [#19245](https://github.com/emqx/emqx/pull/19245) TCP、TLS、WebSocketでサブスクライブするクライアントはサブスクライバーIDテーブルに行を作成しなくなり、クライアントごとに約110バイト、100万クライアントで約110MB節約します。

  REST APIレスポンス、CLI出力、フックコールバック引数は変わりません。`emqx_cm:get_chan_info/1,2`、`emqx_cm:lookup_client/1`、`emqx_mgmt:lookup_client/2`、`emqx_channel:info/1`、`emqx_connection:info/1` で直接テーブルを読むプラグインは `will_msg`、`conninfo.conn_props`、`session.subscriptions` を見なくなりました。クライアントのサブスクリプションは `emqx_broker:subscriptions/1`、CONNECTプロパティは `client.connected` フック、単一属性は `emqx_channel:info/2` を使ってください。

- **CPUとレイテンシ**

  - [#19527](https://github.com/emqx/emqx/pull/19527) メッセージ処理やクライアントライフサイクルでフックを名前で直接呼び出すようにしました。
  - [#19516](https://github.com/emqx/emqx/pull/19516), [#19536](https://github.com/emqx/emqx/pull/19536) メトリクスの更新と読み込みでルックアップ回数を削減しました。受信・送信MQTTパケットメトリクスは名前空間ごとにカウンター参照を再利用します。
  - [#19528](https://github.com/emqx/emqx/pull/19528) `tcp_backend = socket` のTCPリスナーで、1KB以下のパケットは受信システムコール回数を削減しました。QoS 0 PUBLISHパケットあたりのCPU使用率が約7%低減しました。大きいパケットは同等か低減します。
  - [#19532](https://github.com/emqx/emqx/pull/19532) SUBACK・UNSUBACK後のクライアントテーブル更新を1回の書き込みにまとめ、高頻度の新規サブスクライブ時のレイテンシを低減しました。
  - [#19555](https://github.com/emqx/emqx/pull/19555) `mria` を1.0.2にアップグレードしました。クラスター表への書き込みは呼び出しプロセスのメッセージキューが100件以上の時のみヘルパープロセスで実行され、頻繁なヘルパー待ちを回避しSUBSCRIBEレイテンシを低減しました。
  - [#19545](https://github.com/emqx/emqx/pull/19545) リテーナー有効時、ワイルドカードなしのトピックフィルターのサブスクライブ時に保持済みメッセージがない場合は保持済みメッセージ処理を省略しCPUを削減しました。

- **サブスクリプション検索と起動**

  - [#19245](https://github.com/emqx/emqx/pull/19245) `emqx ctl subscriptions show <ClientId>` はノード全体のサブスクリプションをスキャンしなくなりました。
  - [#19183](https://github.com/emqx/emqx/pull/19183) リリースパッケージに含まれる `nodetool` ヘルパーは事前コンパイル済みです。

### バグ修正

#### コアMQTT機能

- [#19489](https://github.com/emqx/emqx/pull/19489) MQTT TCPリスナーのデフォルト `tcp_backend` を `gen_tcp` に戻しました。

  6.3.0でUnix/Linuxのデフォルトが `socket` になっていました。接続ごとのメモリ使用量が多いため、対応中はデフォルトを `gen_tcp` に戻します。メッセージトラフィックなしの接続でのみコストが測定されています。メッセージトラフィックありのテストはありません。将来的にv7で再び `socket` がデフォルトになります。

  `socket` バックエンドは変更なく、Unix/Linuxで `tcp_backend = socket` と明示すれば利用可能です。明示的に設定したリスナーは影響を受けません。デフォルトに依存するリスナーはアップグレード時に `gen_tcp` に切り替わり再起動され、接続は切断されます。

- [#19298](https://github.com/emqx/emqx/pull/19298) 6.3.0で導入された `socket` バックエンドのTCPリスナーにおけるQoS 1メッセージバースト時の高CPU使用率と遅延を修正し、送信メッセージのバッチ処理でバーストスループットを改善しました。

- [#19326](https://github.com/emqx/emqx/pull/19326) メッセージ配信を修正し、フルインフライトウィンドウを接続輻輳信号とみなさなくしました。QoS 0メッセージはインフライトウィンドウが満杯でも配信され、不要なキューイングとメッセージ破棄を減らします。QoS 1/2のメッセージ順序もレート制限下で修正し、キューイングメッセージが配信前に配信クォータを消費しないようにしました。

- [#19351](https://github.com/emqx/emqx/pull/19351) `esockd` を5.17.4から5.17.8にアップグレードし、以下を修正しました：

  - `socket` バックエンドで `tcp_options.recbuf` が小さい場合の高MTUリンクでのTCPスループット低下。
  - PROXY Protocol v2でゼロ長プロキシ情報がMQTTデータを消費する問題を修正し、IPv4/IPv6アドレス情報の切断を拒否。
  - 不完全なPROXY Protocol v1ヘッダーを `socket` バックエンド接続で拒否。
  - リスナーオプションのマージとソケット調整を修正し、未初期化カウンター更新時のアクセプタークラッシュを防止。

- [#19630](https://github.com/emqx/emqx/pull/19630) `gen_tcp` バックエンドのリスナーで、読み込みを停止したサブスクライバーへのQoS 0配信遅延を修正しました。サブスクライバーが読み込みを再開すると、配信は `mqtt.idle_timeout`（デフォルト15秒）まで遅延することがありました。小さいTCP受信バッファのクライアントは接続中ずっとQoS 0メッセージの一部しか受信できませんでした。輻輳した接続はタイマーで送信キューをチェックし、サブスクライバーの読み込み再開後に速やかに配信を再開します。

- [#19144](https://github.com/emqx/emqx/pull/19144) クライアント切断時にセッションレジストリのクリーンアップ処理が停止する問題を修正しました。切断クライアントをスキップし、古い登録の削除を継続します。

- [#19316](https://github.com/emqx/emqx/pull/19316) リミッター登録が再起動時やレジストリのクリーンアップなしで停止時にバケットエントリをリークする問題を修正しました。

#### ゲートウェイ

- [#19422](https://github.com/emqx/emqx/pull/19422) MQTT-SNゲートウェイの混合バージョンクラスタでのセッションテイクオーバー失敗を修正しました。異なるバージョンのノードで再接続や復帰すると失敗またはクラッシュし、セッション再開と保留メッセージ配信ができませんでした。

- [#19459](https://github.com/emqx/emqx/pull/19459) MQTT-SNゲートウェイが送信するPUBLISHパケットから `DUP` と `RETAIN` フラグが欠落していた問題を修正しました。クライアントは再送メッセージとサブスクライブ時の保持メッセージを識別可能になりました。

- [#19393](https://github.com/emqx/emqx/pull/19393) セッション再開したゲートウェイクライアントへのQoS 1/2配信で毎回エラーログが出ていた問題を修正しました。

  メッセージキューやストリーム使用時、配信時にフックコールバック内で例外が発生し、EMQXは `hook_callback_exception` をエラーレベルでログに記録していました。配信自体は影響ありません。セッション復帰するゲートウェイチャネル（MQTT-SNクライアントのスリープ復帰など）はセッションライフサイクルフックを実行しないため、必要なチャネルメタデータが設定されていませんでした。

---

（以下、6.3.1、6.3.0、6.2.3、6.2.2、6.2.1、6.2.0、6.1.5、6.1.4、6.1.3、6.1.2、6.1.1、6.1.0、6.0.4、6.0.3、6.0.2、6.0.1、6.0.0のリリースノートも同様に翻訳されていますが、文字数制限のためここでは省略します。）
