# EMQX Enterprise Version 6

## 6.3.2

*リリース日: 2026-10-16*

### 改善点

#### Core MQTT 機能

- [#19438](https://github.com/emqx/emqx/pull/19438) 混雑していない接続において、インメモリセッションは、以前の QoS 1/2 メッセージがアックを待っている場合でも、キューに溜まった QoS 0 メッセージのドレインを楽観的に継続するようになりました。これは混雑が解消された後や既存のセッションが再開した後のドレインも含みます。トピック優先度が有効な場合は、QoS 0 メッセージのドレインが早期に停止する可能性があります。

#### パフォーマンス

本リリースでは、MQTT 接続、セッション、ノードローカルクライアントデータのメモリ使用量を削減し、複数のブローカーパスにおける CPU オーバーヘッドを低減、サブスクリプションのルックアップおよび起動パフォーマンスを改善しました。

- **接続およびセッションのメモリ**
  - [#19092](https://github.com/emqx/emqx/pull/19092) 接続は `mqtt.hibernate_after`（デフォルトはトラフィックなしで5秒）経過後にハイバネートします。ハイバネートによりプロセスヒープが使用中のデータに縮小され、接続ごとに約5KBのメモリを解放します。以前は TCP/TLS 接続は `mqtt.idle_timeout`（デフォルト15秒）後にのみハイバネートし、WebSocket 接続は最初のパケット後も起きたままでした。`mqtt.hibernate_after = infinity` に設定すると接続を起きたままにできますが、クライアントがほぼ同じ間隔で送信する場合は値を上げてください。ハイバネートごとにガベージコレクションが発生し、起床時にヒープが再び増加するためです。
  - [#19181](https://github.com/emqx/emqx/pull/19181) ゾーンとリスナーでメッセージ、バイト、サブスクライブのレート制限が設定されていない接続は、レートリミッター状態を割り当てなくなりました。後から制限が設定されると既存接続にも適用されます。
  - [#19262](https://github.com/emqx/emqx/pull/19262) パブリッシュ専用クライアントや配信に追いついているサブスクライバーなど、キューにメッセージがないセッションは48バイトのメモリを節約します。
  - [#19563](https://github.com/emqx/emqx/pull/19563) Erlang/OTP を 28.4.1-7 に、`emqx-builder` を 6.2-3 にアップグレードしました。TLS 接続は共有ソケットオプションで接続ごとの ETS テーブルが不要になりメモリ使用量が減少し、アイドル TLS 1.3 接続は暗号状態を解放します。TLS ハンドシェイク時に絶対パスの `cacertfile` 解決で Erlang ファイルサーバーを2回呼び出すことがなくなり、TLS 接続バースト時のファイルサーバーのバックログを防止します。

- **ノードローカルクライアントデータ**
  - [#19306](https://github.com/emqx/emqx/pull/19306) channel-info テーブルはクライアントの遺言メッセージ、CONNECT プロパティ、サブスクリプションリストを保持しなくなりました。これにより、CONNECT パケットやサブスクリプション数に応じてエントリが増大しなくなり、メモリ使用量と SUBACK/UNSUBACK 後のテーブル書き換えコストが減少します。
  - [#19263](https://github.com/emqx/emqx/pull/19263) テーブルは `sockinfo` マップと `conninfo` が保持する `peerhost`, `peerport`, `peername`, `sockport` を削除し、接続ごとに約100バイト節約します。
  - [#19303](https://github.com/emqx/emqx/pull/19303) 値が0のクライアント統計情報を保持しなくなり、アイドルクライアントごとに約145バイト節約します。
  - [#19245](https://github.com/emqx/emqx/pull/19245) TCP、TLS、WebSocket でサブスクライブするクライアントは subscriber-id テーブルに行を持たなくなり、クライアントごとに約110バイト、100万クライアントで約110MB節約します。

  REST API レスポンス、CLI 出力、フックコールバック引数は変更ありません。`emqx_cm:get_chan_info/1,2`、`emqx_cm:lookup_client/1`、`emqx_mgmt:lookup_client/2`、`emqx_channel:info/1`、`emqx_connection:info/1` を通じて直接クライアント情報テーブルを読むプラグインは、返されるマップに `will_msg`、`conninfo.conn_props`、`session.subscriptions` を含みません。クライアントのサブスクリプションは `emqx_broker:subscriptions/1`、CONNECT プロパティは `client.connected` フック、単一属性の読み取りは `emqx_channel:info/2` を使用してください。

- **CPU とレイテンシ**
  - [#19527](https://github.com/emqx/emqx/pull/19527) メッセージ処理やクライアントライフサイクル操作時のフックは名前で直接呼び出されるようになりました。
  - [#19516](https://github.com/emqx/emqx/pull/19516) [#19536](https://github.com/emqx/emqx/pull/19536) メトリックの更新と読み取りに必要なルックアップ数を削減しました。受信・送信した MQTT パケットのメトリック更新は、各名前空間更新グループ内の全カウンターでカウンター参照を再利用します。
  - [#19528](https://github.com/emqx/emqx/pull/19528) `tcp_backend = socket` の TCP リスナーでは、1KB までのパケットで受信ごとのシステムコール回数が減少し、QoS 0 PUBLISH パケットあたりのブローカー CPU 使用率が約7%低下しました。大きなパケットでは同等か低下しています。
  - [#19532](https://github.com/emqx/emqx/pull/19532) SUBACK と UNSUBACK 後のクライアントテーブル更新を1回の書き込みにまとめ、高頻度の新規サブスクライブ接続時の SUBSCRIBE レイテンシを低減しました。
  - [#19555](https://github.com/emqx/emqx/pull/19555) `mria` を 1.0.2 にアップグレード。クラスタテーブルへの書き込みは、呼び出しプロセスのキューに100メッセージ以上ある場合のみヘルパープロセスで実行されるようになり、頻繁なヘルパープロセス待ちを回避して新規サブスクライブの高負荷時のレイテンシを低減しました。
  - [#19545](https://github.com/emqx/emqx/pull/19545) リテイナー有効時、ワイルドカードなしトピックフィルターのサブスクライブでリテインメッセージのチェックを最適化し、該当メッセージがなければ以降のリテイン処理を省略して CPU 使用率を低減しました。

- **サブスクリプションルックアップと起動**
  - [#19245](https://github.com/emqx/emqx/pull/19245) `emqx ctl subscriptions show <ClientId>` コマンドはノード上の全サブスクリプションをスキャンしなくなりました。
  - [#19183](https://github.com/emqx/emqx/pull/19183) リリースパッケージに含まれる `nodetool` ヘルパーを事前コンパイルしました。

### バグ修正

#### Core MQTT 機能

- [#19489](https://github.com/emqx/emqx/pull/19489) MQTT TCP リスナーのデフォルト `tcp_backend` を `gen_tcp` に戻しました。

  6.3.0 で Unix/Linux でのデフォルトが `socket` になりましたが、接続ごとのメモリ使用量が増加するため、対応が完了するまでデフォルトを `gen_tcp` に戻します。メッセージトラフィックなしの接続でのみコストが測定されており、トラフィックありのテストでは未確認です。`socket` は v7 で再びデフォルトになる予定です。

  `socket` バックエンドは変更なく、Unix/Linux で `tcp_backend = socket` と設定すれば利用可能です。明示的に設定したリスナーは影響を受けません。デフォルトに依存するリスナーは `gen_tcp` に切り替わり、アップグレード時に再起動されてアクティブな接続は切断されます。

- [#19298](https://github.com/emqx/emqx/pull/19298) 6.3.0 で導入されたパフォーマンス低下を修正。`socket` バックエンドの TCP リスナーで大規模な QoS 1 メッセージバースト時に高い CPU 使用率と遅いメッセージ配信が発生していました。アウトゴーイングメッセージのバッチ処理でバーストスループットも改善しました。

- [#19326](https://github.com/emqx/emqx/pull/19326) フルインフライトウィンドウを接続混雑シグナルとして扱わないようにメッセージ配信を改良。QoS 0 メッセージはインフライトウィンドウが満杯でも配信可能になり、不要なキューイングやメッセージドロップを減らしつつ、トピック・QoS レベルごとの順序は保持されます。

  また、配信レート制限下での QoS 1/2 メッセージの順序入れ替わりを修正し、送信前のキューイングメッセージが配信クォータを消費しないようにしました。

- [#19351](https://github.com/emqx/emqx/pull/19351) `esockd` を 5.17.4 から 5.17.8 にアップグレードし、以下のリスナー問題を修正しました：

  - `socket` バックエンドで小さい `tcp_options.recbuf` 値を使うと高 MTU リンクで TCP スループットが著しく低下する問題を修正。
  - PROXY Protocol v2 のゼロ長プロキシ情報が MQTT データを消費し、切断された IPv4/IPv6 アドレス情報をクラッシュせずに拒否する問題を修正。
  - 不完全な PROXY Protocol v1 ヘッダーを `socket` バックエンド接続で拒否するように。
  - リスナーオプションのマージとソケットチューニングを修正し、未初期化カウンター更新時のアクセプタークラッシュを防止。

- [#19144](https://github.com/emqx/emqx/pull/19144) クライアント切断中にセッションレジストリクリーンアップ処理が停止する問題を修正。切断されたクライアントをスキップして古い登録を継続的に削除します。

- [#19316](https://github.com/emqx/emqx/pull/19316) リミッター登録が再起動されたりノードがクリーンアップなしでシャットダウンされた際に、`persistent_term` にレートリミッターのバケットエントリがリークする問題を修正。

#### ゲートウェイ

- [#19422](https://github.com/emqx/emqx/pull/19422) MQTT-SN セッションテイクオーバーが混在バージョンクラスターで失敗する問題を修正。異なるバージョンのノードで再接続やウェイクアップするとクライアントチャネルがクラッシュし、セッション再開や保留メッセージ配信が阻害されていました。

- [#19459](https://github.com/emqx/emqx/pull/19459) MQTT-SN ゲートウェイが送信する PUBLISH パケットから `DUP` と `RETAIN` フラグが抜けていた問題を修正。クライアントは再送メッセージやサブスクライブ時のリテインメッセージを識別可能になりました。

- [#19393](https://github.com/emqx/emqx/pull/19393) セッションを再開したゲートウェイクライアントへの QoS 1/2 配信時に毎回エラーログが出る問題を修正。

  メッセージキューやストリーム使用時に、フックコールバック内で例外が発生し、`hook_callback_exception` がエラーレベルでログに記録されていました。配信自体には影響ありません。セッション再開時のゲートウェイチャネルはセッションライフサイクルフックを実行しないため、コールバックに必要なチャネルメタデータが設定されていませんでした。

#### アクセス制御

- [#19282](https://github.com/emqx/emqx/pull/19282) `hostname_resolution = dynamic` の HTTP 認証で、`Content-Type` ヘッダー名が小文字でないレスポンスを拒否していた問題を修正。例えば `Content-Type: application/json` は許容されます。以前は認証バックエンドが `unsupported_content_type` を返し、クライアント接続ができませんでした。

#### データ統合

- [#19313](https://github.com/emqx/emqx/pull/19313) HTTP ベースコネクター（HTTP Server、GCP PubSub Producer、Couchbase、Snowflake）で応答なし接続検出を改善。

  修正前は、応答待ちリクエストがある接続は `resource_opts.request_ttl`（リクエストタイムアウト）＋ `max_inactive` 経過後に再接続されていました。`request_ttl` が大きいと再接続まで長時間かかり、`infinity` の場合は再接続されませんでした。

  修正後は、リクエスト待ちがあり送信リクエストが60秒間ない場合、または `max_inactive` がそれより長い場合は `max_inactive` 経過後に再接続されます。デフォルト設定（`request_ttl` 45秒、`max_inactive` 10秒）は影響を受けません。

  これにより、`request_ttl` が約50秒以上のコネクターで挙動が変わります。60秒以上かかる応答は再接続で中断される可能性があります。リモートサービスが60秒以上かかる場合は `max_inactive` を最長応答時間以上に設定してください。

#### オブザーバビリティ

- [#19561](https://github.com/emqx/emqx/pull/19561) ログハンドラが `payload_encode = hidden` を設定した場合のログフォーマッタクラッシュを修正。以前は Cluster Linking の `recv_data` イベントの `data` フィールドなど、タームフィールドを含むデバッグログが `FORMATTER CRASH` レポートに置き換わっていました。現在は `Encoded(hidden)=******` としてマスクされ、通常通りログが書き込まれます。

#### ノードとクラスター

- [#19444](https://github.com/emqx/emqx/pull/19444) CPU クォータ下（例: `docker run --cpus`、Kubernetes CPU 制限）での Erlang スケジューラ数を修正。

  6.3.0 以降、EMQX はホストの CPU コア数分のスケジューラを起動していましたが、コンテナが使用可能なコア数が少ない場合でも同様でした。スケジューラが CPU クォータを奪い合い、同じメッセージレートで約30%多く CPU を使用していました。現在は CPU クォータに応じたスケジューラ数を起動します。明示的に設定する場合は `node.schedulers` を使用してください。

- [#19160](https://github.com/emqx/emqx/pull/19160) コアノードがクラスターに参加・離脱する際の `mria` アプリケーションのクラッシュを修正。

  ```text
  Reason: {badarg,[{ets,select,[gproc,[{{{{n,l,{mria_rlog_replica,route_shard_m,'$1'}},n},'$2','_'},[],[{{'$1','$2'}}]]],[{error_info,#{cause => id...
  ```

  この修正で、`mria` アプリケーションの `gproc` 依存をクラスターノード変更時に再起動されるビジネスレベルアプリケーションのリストから除外しました。

- [#19578](https://github.com/emqx/emqx/pull/19578) ローリングアップグレード時に避難されたセッションのインポート中に発生するクラッシュを修正。セッションが旧バージョンのノード由来でリスナーメタデータが欠落している場合に発生していました。

- [#19580](https://github.com/emqx/emqx/pull/19580) ローリングアップグレード中にノード避難が完了しない可能性のある問題を修正。

  - **ソースノードバージョン:** EMQX 5.8～5.10、6.0.0～6.0.3、6.1.0～6.1.4、6.2.0～6.2.3、6.3.0
  - **宛先ノードバージョン:** EMQX 6.0.4、6.1.5、6.3.1
  - **影響:** 宛先ノードがセッション強制退去呼び出しに応答せず、ソースノードがクライアントセッションを保持し続けて無限に再試行していました。
  - **解決:** EMQX ノードは呼び出しに再び応答し、ノード避難が完了するようになりました。

#### パッケージング

- [#19141](https://github.com/emqx/emqx/pull/19141) Docker イメージの `/etc/passwd` に `root` エントリを復元し、`docker run --user root` が再び動作するようにしました。6.3.1 でイメージがハードニングされたベースに移行した際に削除され、数値形式の `--user 0:0` のみが動作していました。

## 6.3.1

*リリース日: 2026-09-17*

EMQX 6.3.1 へのアップグレード前に破壊的変更を必ずご確認ください。

### 改善点

#### アクセス制御

- [#18777](https://github.com/emqx/emqx/pull/18777) Dashboard JSON API ログインに SCRAM-SHA-256 チャレンジレスポンス認証をオプションで追加し、埋め込み API Spec ログインページのデフォルトを SCRAM に切り替えました。既存のパスワードログインはデフォルトで有効のままです。

  `dashboard.password_login = scram_only` の場合、ユーザー名とパスワードで `POST /api/v5/login` を使うスクリプトやサードパーティクライアントは SCRAM チャレンジレスポンスフローに移行するか API キーを使用する必要があります。

#### パッケージング

- [#18977](https://github.com/emqx/emqx/pull/18977) 公式 EMQX Enterprise Docker リリースイメージを Docker Hardened Debian 13 (Trixie) に切り替えました。ランタイムイメージからは `apt` や `dpkg` などのパッケージマネージャーが除外されています。

  リリースイメージにはビルド由来情報とソフトウェア部品表（SBOM）証明書が含まれ、Docker Scout によるハードニングベースイメージの脆弱性評価（VEX ステートメント）適用が可能になりました。

#### パフォーマンス

- [#18806](https://github.com/emqx/emqx/pull/18806) MQTT 接続のメモリ使用量を削減。

  接続時にレートリミッター状態を割り当てていましたが、レート制限が設定されていない場合でも割り当てていました。現在はクライアントが初めてパブリッシュまたはサブスクライブした際に割り当てるため、接続のみの状態では約2KBのメモリ節約になります。設定済みのレート制限は従来通り動作します。

- [#18807](https://github.com/emqx/emqx/pull/18807) MQTT セッションのメモリ使用量を削減。

  配信レート制限が設定されていない場合でも配信レートリミッター状態を割り当てていましたが、現在は配信レート制限が設定された後の最初の配信時に割り当てます。セッションごとに約0.5KB節約されます。設定済みの配信レート制限は従来通り動作します。

- [#18808](https://github.com/emqx/emqx/pull/18808) パブリッシュまたはサブスクライブする MQTT 接続のメモリ使用量を削減。

  接続ごとのレートリミッター状態は、実際に設定されたレート制限に対してのみ割り当てられます。デフォルト設定（レート制限なし）ではアクティブ接続ごとに約1.6KB節約されます。設定済みのレート制限は従来通り動作し、ランタイム追加も対応します。

- [#18809](https://github.com/emqx/emqx/pull/18809) レート制限が設定されている MQTT 接続のメモリ使用量を削減。

  全接続で共有されるレートリミッターの管理情報は接続ごとにコピーされなくなり、レート制限有効時に接続ごとに約0.5KB節約されます。レート制限の動作は変更ありません。

- [#18875](https://github.com/emqx/emqx/pull/18875) マルチテナントネームスペースに属する MQTT 接続のメモリ使用量を削減。

  テナントクライアントのレートリミッター状態は、クライアントが初めてパブリッシュ、サブスクライブ、またはメッセージ受信した際に割り当てられ、実際に設定された制限に対してのみ割り当てられます。通常の接続と同様の動作です。設定済みのテナント・クライアントレート制限は従来通り動作します。ネームスペースのリミッター設定が削除された場合、そのクライアントに対するリミッター検査は操作拒否を継続します。

#### バグ修正

#### Core MQTT 機能

- [#18734](https://github.com/emqx/emqx/pull/18734) `tcp_options.buffer = 0` がデフォルトの `socket` バックエンドで新規接続が失敗する TCP リスナーの問題を修正。

  `tcp_options.active_n` は 1..1000 に制限され、`tcp_options.buffer` は MQTT 固定ヘッダー最大長の5バイト以上に制限されます。範囲外の値は受け入れられますが静かに調整されます。

- [#18789](https://github.com/emqx/emqx/pull/18789) MQTT とゲートウェイリスナーの作成で、64バイトを超える名前や制限付き名前形式に合わない名前を明確な `BAD_REQUEST` 応答で拒否するようにしました。既存の長い名前のリスナーは編集可能なままです。

- [#18811](https://github.com/emqx/emqx/pull/18811) 6.3.0 のリグレッションを修正し、トピックエイリアスを受け入れない MQTT v5 クライアントにトピックエイリアスを送信しなくなりました。

  `Topic Alias Maximum` プロパティを省略または0に設定したクライアントに対し、SCRAM などの強化認証で認証された場合でも `Topic-Alias` プロパティ付き PUBLISH パケットが送信されていました。

- [#18828](https://github.com/emqx/emqx/pull/18828) MQTT リスナーは、最初のパケットが CONNECT でない場合、パケット本文を解析せずに接続を閉じるようになりました。

#### アクセス制御

- [#18681](https://github.com/emqx/emqx/pull/18681) API キーを拒否するエンドポイント（例: `POST /api/v5/logout`）で、ベアラートークン拒否や認可ヘッダー欠落時に API キー使用を示唆する誤った認証エラーメッセージを修正。`api_key.bootstrap_file` 設定キー名も表示しません。

- [#18724](https://github.com/emqx/emqx/pull/18724) Dashboard SSO OIDC で、発行者 URL スキームと TLS オプションが矛盾する設定（`https` で TLS 無効、`http` で TLS 有効）を設定時に拒否し、実行時の不明瞭な TLS エラーや TLS オプション無視を防止しました。

- [#18737](https://github.com/emqx/emqx/pull/18737) 認証・認可リクエストテンプレートで `${peerport}` が空文字列になる問題を修正。

  また、`${peername}` をクライアント情報に追加。例: `192.168.0.1:51544`。

- [#18852](https://github.com/emqx/emqx/pull/18852) 6.2 で動作中のノードがローリングアップグレード中にブートストラップ API キーを拒否する問題を修正。

  6.3 ノード起動時にクラスタ共有 API キーの秘密ハッシュを書き換え、6.2 ノードが検証できず拒否していました。ブートストラップファイルローダーは、秘密が同じ場合は保存済みハッシュを保持し、ロールやスコープなどのメタデータのみ更新します。

  秘密を変更するとハッシュは置き換えられます。クラスタ全体が 6.3 になってからブートストラップ秘密をローテーションしてください。

- [#18904](https://github.com/emqx/emqx/pull/18904) Dashboard SSO バックエンドのリソースが開始タイムアウトまで `connecting` 状態のままの場合に、リソースの作成・更新が内部エラーで失敗する問題を修正。

  バックエンドはリソース ID で追跡され、開始タイムアウト後にクリーンアップされます。

#### マルチテナンシー

- [#18774](https://github.com/emqx/emqx/pull/18774) リソースクリーンアップ中に削除済みマルチテナンシーネームスペースにクライアントが接続できる問題を修正。

#### データ統合

- [#18763](https://github.com/emqx/emqx/pull/18763) TDengine アクションのエラー報告を修正。アクションが見つからない場合、アクション ID ではなくコネクター ID を名前に含めていました。

#### 監視

- [#18664](https://github.com/emqx/emqx/pull/18664) ネームスペース付きリクエストの監査ログに対象ネームスペースを記録。以前は異なるネームスペースの同名トピックメトリクスコレクションのリセットが区別できませんでした。クエリパラメータも記録されます。

- [#18677](https://github.com/emqx/emqx/pull/18677) データバックアップリクエストの監査ログに対象ネームスペースを記録。以前は異なるネームスペースのバックアップ操作が区別できませんでした。クエリパラメータも記録されます。

- [#18678](https://github.com/emqx/emqx/pull/18678) A2A レジストリリクエストの監査ログに対象ネームスペースを記録。以前は異なるネームスペースのエージェントカード操作が区別できませんでした。クエリパラメータも記録されます。

- [#18684](https://github.com/emqx/emqx/pull/18684) OpenTelemetry トレース有効時に MQTT 5 クライアントの CONNECT パケットに Will User-Property が含まれる場合の接続クラッシュを修正。

- [#18686](https://github.com/emqx/emqx/pull/18686) 認可、認証、コネクター、ブリッジ、ルールエンジン、トレースリクエストの監査ログに対象ネームスペースを記録。以前はネームスペース間で区別できませんでした。クエリパラメータも記録されます。

- [#18754](https://github.com/emqx/emqx/pull/18754) ヘルプのみを表示する CLI コマンドで監査ログを作成しないようにしました。

  例: `emqx ctl api_keys` はサブコマンドなしで使用すると使用法を表示しますが、以前は空引数で操作が行われたように監査ログが記録されていました。実際に操作を行うコマンドは監査されます。

- [#18853](https://github.com/emqx/emqx/pull/18853) SSO MFA 設定・検証認証情報が監査ログにマスクされずに保存される問題を修正。

  SSO MFA リクエストボディは `username` と `backend` のみ監査記録に残し、一時トークン、TOTP コード、未知の認証情報フィールドは監査ファイル・監査 DB に書き込む前にマスクされます。

- [#18961](https://github.com/emqx/emqx/pull/18961) `emqx ctl plugins ...` コマンドの監査ログで全引数が `******` でマスクされるリグレッションを修正。`plugins` 引数は機密情報でないためそのまま記録します。`plugins install --cluster` オプションは CLI リファクタリングで誤って削除されていたため復活しました。

#### 管理

- [#18600](https://github.com/emqx/emqx/pull/18600) Durable セッション有効時のクライアントリスト API フィルタリングを修正。

  `GET /api/v5/clients` は切断済み durable セッションをすべて結果に追加しており、`conn_state`, `username`, `clientid` などのクエリフィルターを無視していました。例: `conn_state=connected` でも切断済み durable セッションを返していました。現在は切断済み durable セッションにもフィルターが適用されます。

  フィルター適用時に切断済み durable セッションが存在するとき、レスポンスは誤った数を返す代わりに `meta.count` フィールドを省略します。`conn_state=connected` フィルター時は正確な `meta.count` を返します。

- [#18666](https://github.com/emqx/emqx/pull/18666) `GET /api/v5/topics` は durable 共有サブスクリプションの完全な共有トピック（例: `$share/group/a/1`）を返すようになりました。

  以前はトピックのみ（例: `a/1`）を返しており、通常の durable サブスクリプションと区別できませんでした。

- [#18817](https://github.com/emqx/emqx/pull/18817) リクエストボディに `enable` フィールドがない場合、`PUT /api/v5/telemetry/status` が Erlang スタックトレース付きで `500 INTERNAL_ERROR` を返す問題を修正。

  現在は検証メッセージ付きで `400 BAD_REQUEST` を返します。API ドキュメントは `enable` を必須とし、デフォルト値を表示しません。エンドポイントは以前からデフォルトを適用していません。

- [#18859](https://github.com/emqx/emqx/pull/18859) 設定更新時に HTTP ヘッダー名の大文字小文字だけを変更すると保存済み値が消える問題を修正。

- [#18860](https://github.com/emqx/emqx/pull/18860) クライアントメッセージページネーションで最終ページに継続位置を返す問題を修正。

  `GET /clients/{clientid}/mqueue_messages` と `GET /clients/{clientid}/inflight_messages` は、メッセージがなくなると `meta.position` に `end_of_data` を返し、API クライアントは追加の空ページをリクエストせずに停止できます。

#### デプロイメントとセキュリティ

- [#18836](https://github.com/emqx/emqx/pull/18836) クラスター設定同期のデバッグログに成功した設定変更結果を含めるのをやめました。

  結果には HTTP 認証ヘッダーのテンプレートなどのコンパイル済みランタイム状態が含まれ、ログのマスク対象外の秘密情報を含んでいました。

## 6.3.0

*リリース日: 2026-09-03*

EMQX 6.3.0 へのアップグレード前に破壊的変更と既知の問題を必ずご確認ください。

### 改善点

#### Core MQTT 機能

- [#16694](https://github.com/emqx/emqx/pull/16694) QUIC 接続からピア証明書を抽出する機能を追加し、QUIC mTLS リスナーで `peer_cert_as_username` を有効にできるようにしました。

- [#17307](https://github.com/emqx/emqx/pull/17307) リスナーの SUBSCRIBE パケットに対するクライアント単位レートリミッターを追加。デフォルトは無効。有限レート設定時、制限超過パケットは Quota Exceeded 理由コード付き SUBACK で応答し処理しません。ネームスペースごとに独立したレートを設定でき、リスナーレベルのレートを上書きします。

- [#17546](https://github.com/emqx/emqx/pull/17546) [#18477](https://github.com/emqx/emqx/pull/18477) MQTT 5.0 クライアントが `Session-Expiry-Interval` プロパティで要求できるセッション有効期限の上限を設定する `mqtt.max_session_expiry_interval` を追加。クライアントが上限を超える値を要求した場合、サーバーは黙って制限値に丸めて CONNACK に反映します。デフォルトは `infinity`（制限なし）で従来動作を維持。MQTT 3.1.1/3.1 クライアントには影響ありません。

  この制限は DISCONNECT パケットのセッション有効期限にも適用され、クライアントが切断時に期限を延長できなくなります。

- [#17603](https://github.com/emqx/emqx/pull/17603) TLS クライアント証明書の Subject Alternative Name を MQTT クライアント属性に抽出する機能を追加。`mqtt.client_attrs_init` で `cert_san.dns`、`cert_san.ip`、`cert_san.email`、`cert_san.uri` を使用可能。

- [#17854](https://github.com/emqx/emqx/pull/17854) MQTT TCP リスナーのデフォルト `tcp_backend` を `socket` に変更し、メッセージレイテンシとリソース使用を改善。`tcp_backend = gen_tcp` で従来のバックエンドも利用可能。

- [#17870](https://github.com/emqx/emqx/pull/17870) 遅いまたは混雑したサブスクライバー向けのインメモリセッション配信動作を改善。

  - EMQX は接続送信キューの混雑を追跡し、必要に応じて QoS 0 配信をセッションメッセージキュー経由に切り替えます。
  - セッションメッセージキューは容量到達時に古い QoS 0 メッセージを優先的に削除し、QoS 1/2 配信の進行を助けます。
  - 配信レート制限到達時の配信順序を修正し、後続メッセージが先行キューイングメッセージを追い越さないようにしました。
  - ソケットバック接続の `send_timeout` 処理を改善し、ソケットキューがウォーターマークを超えた後にのみタイムアウトを設定します。

#### アクセス制御

- [#17145](https://github.com/emqx/emqx/pull/17145) 認可ソースが Variform ベースの前提条件をサポート。前提条件が `true` の場合にのみ認可ソースが呼び出され、クライアント属性、アクション、トピックなどのコンテキストに応じて異なる認可バックエンドを選択可能。

- [#17487](https://github.com/emqx/emqx/pull/17487) Dashboard 管理者パスワードと API キー秘密のハッシュ強化。

  Dashboard 管理者パスワードは PBKDF2-HMAC-SHA256（60万回反復）と16バイトランダムソルトでハッシュ。API キー秘密は同じ自己記述形式で16バイトランダムソルトを使い反復なし。従来の単一 SHA-256 + 2バイトソルト方式は両者で置き換え。

  既存ハッシュは認証に使用可能。ユーザーがパスワードを変更するか API キーを再作成すると新形式に書き換え。

- [#17671](https://github.com/emqx/emqx/pull/17671) 認証・認可拒否ログにバックエンド識別情報を追加。

  認証拒否時は拒否を出した認証器 ID とプロバイダーを警告ログで出力。認可拒否時は認可ソース種別、モジュール、トピック、アクションを警告ログで出力（従来はクライアントトレースでのみ可視）。

  複数認証器や認可ソースがある環境で、クライアントトレースなしにどのバックエンドが決定を出したか判別可能。新ログは認証器・認可ソースごとにスロットリング。

- [#18130](https://github.com/emqx/emqx/pull/18130) HTTP 認証器・認可器の URL ホストにテンプレート変数をサポート。例: `https://${client_attrs.tns}.auth.example.com/authn`。外部ルーティング層なしでテナント別認証・認可エンドポイントを実現。

  `hostname_resolution` 設定でホストの扱いを制御。デフォルト `static` は従来動作で、設定時に固定ホスト名を解決し接続プールを作成。`dynamic`（テンプレートホスト時必須）は毎リクエストでホスト名を解決し、リクエストごとに接続を作成。TLS オプションを適用。`pool_size` はアイドル接続数上限。パイプラインは無効。

  テンプレートホストの場合、`allowed_hosts` 設定に許可ホスト名リスト（完全一致または `*.suffix` ワイルドカード）を指定。レンダリング結果がリスト外ならリクエストを送らずチェック失敗。リテラルホストは従来通り接続プールを利用。

- [#18239](https://github.com/emqx/emqx/pull/18239) [#18371](https://github.com/emqx/emqx/pull/18371) フラッピング検出がクライアント ID に加え、ユーザー名と接続元 IP アドレスにも対応。

  各次元は独自の検出ウィンドウ、接続試行閾値、禁止期間を持ち、`flapping_detect.by_clientid`、`by_username`、`by_peerhost` で設定（ユーザー名・IP 検出はデフォルト無効）。クライアント ID 次元は従来の平坦な `flapping_detect` 設定と互換性あり。検出閾値超過時は接続試行を拒否し、既接続クライアントは影響なし。禁止エントリは `/banned` REST API で確認・削除可能。エントリはタイプ（`clientid`、`username`、`peerhost`）と検出元を持ち、タイプでフィルター可能。

  ゾーン・次元ごとにカウンターを分離し、異なる検出ウィンドウのゾーン間でカウンターが破棄されません。

  新メトリクス: `flapping.detected.clientid`、`flapping.detected.username`、`flapping.detected.peerhost`（次元別フラッピングトリガー数）、`client.banned`（禁止エントリによる接続拒否数）。

#### マルチテナンシー

- [#17454](https://github.com/emqx/emqx/pull/17454) `/api/v5/prometheus/data_integration` の Prometheus データをリクエスト元のアクターのネームスペースに限定。グローバル管理者は全ネームスペースのデータを閲覧・フィルタ可能。

#### データ統合

- [#17129](https://github.com/emqx/emqx/pull/17129) GCP ベースコネクター（GCP PubSub Producer、Consumer、BigQuery）にアタッチドサービスアカウント認証を追加。GCP VM にサービスアカウントがアタッチされている場合、内部メタデータエンドポイントからトークンを取得可能。

- [#17222](https://github.com/emqx/emqx/pull/17222) Bigtable へのデータ追加をサポートするデータ統合を追加。

- [#17547](https://github.com/emqx/emqx/pull/17547) Kafka Producer/Consumer コネクターに AWS IAM Roles Anywhere を追加。認証ヘルパープロセスの HTTP エンドポイントを設定可能。

  認証ヘルパープロセスは EMQX からアクセス可能で稼働している必要があります。詳細は [AWS IAM Roles Anywhere credential helper documentation](https://docs.aws.amazon.com/rolesanywhere/latest/userguide/credential-helper.html#credential-helper-serve) を参照。

- [#17783](https://github.com/emqx/emqx/pull/17783) PostgreSQL 系コネクターに `application_name` オプションを追加。デフォルトは `emqx`。PostgreSQL のアクティビティビューやログでコネクターセッションを識別可能。

- [#18119](https://github.com/emqx/emqx/pull/18119) Disk Log コネクターに時間ベースのファイルローテーションを追加。

  新しいオプション `rotation` を追加:

  - `rotation.period`: `none`（デフォルト）、`day`、`hour`。`day` または `hour` 設定時は期間境界で別ログセットを開始し、ファイル名に期間の日付スタンプを付与（例: `mqtt-trace-2026062400.log.1`）。サイズベースローテーションは期間内で適用。
  - `rotation.retention_period`: 過去期間のファイル保持期間（例: `30d`）。期間外ファイルは期間ローテーションごとに自動削除。デフォルトは `infinity`。
  - `rotation.timezone`: 期間境界のタイムゾーン。`UTC`（デフォルト）、`local`、`+02:00` などの固定オフセット。

  デフォルト動作（`rotation` 未設定または `none`）は変更なし。

- [#18319](https://github.com/emqx/emqx/pull/18319) BigQuery アクションにアクション固有のプロジェクト ID 指定を追加。

- [#18624](https://github.com/emqx/emqx/pull/18624) `emqx ctl actions show` と `emqx ctl actions status` コマンドを追加。ローカルノードのアクションステータスを JSON で表示。REST API 認証やネットワーク呼び出し不要。

  `status` は `{"<type>:<name>": "<status>"}` の JSON 配列をコンパクトに表示。`show` は `GET /api/v5/actions/{id}` と同様の情報を表示（コネクター秘密はマスク）。両コマンドは `--name <type:name>` でアクション選択、`--ns <namespace>` でネームスペース選択可能。デフォルトはグローバルネームスペースの全アクション。

  ローカルノードの準備状況確認に適します。クラスタ集約された REST API の `status` フィールドはローカルノードの準備状況を示しません。

#### オブザーバビリティ

- [#17493](https://github.com/emqx/emqx/pull/17493) セッションバッファのオブザーバビリティを追加。クライアント API は `total_payload_bytes` を報告し、`sysmon.session.total_payload_bytes_high_watermark` はペイロードバイト閾値超過セッションで警告を抑制付きで発行。`emqx ctl session-top` はクラスタートップセッションを `total_payload_bytes` または `mqueue_length` でエクスポート可能。

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus メトリック収集を `prometheus.erl` 6.1.2 に更新し、パフォーマンスとスケーラビリティを改善。

  `vm_dist`、`vm_statistics`、`vm_system_info`、`vm_memory` コレクターはデフォルトで有効。廃止された `emqx_vm_process_messages_in_queues` メトリックは Prometheus 出力から削除。

- [#17607](https://github.com/emqx/emqx/pull/17607) [#17998](https://github.com/emqx/emqx/pull/17998) 名前付きコレクション、ワイルドカードトピックフィルター、ネームスペース所有権、REST CRUD、Prometheus スクレイプエンドポイントを備えた v2 トピックメトリクスを追加。

  - `/api/v5/mqtt/topic_metrics2/:name` でユーザー指定名のトピックメトリクスコレクションを登録可能。ワイルドカード（例: `t/#`, `sensor/+/temp`）をトピックフィルターに使用可能。1つのパブリッシュメッセージが複数コレクションにマッチ可能。
  - コレクションはネームスペーススコープ。ネームスペース管理者が作成したコレクションは `client_attrs.tns` が一致するパブリッシャーのみカウント。グローバル管理者が作成したコレクションは全パブリッシャーをカウント。ネームスペース管理者は自分のコレクションのみ閲覧・変更可能。グローバル管理者は `ns` クエリパラメータで個別コレクションにアクセス可能。名前空間外の `ns` 指定は `403 Forbidden`。`ns` 省略時はアクター自身のネームスペース。
  - カウンターは `/api/v5/prometheus/topic_metrics` で Prometheus 形式で公開。ラベルは `name`、`topic_filter`、`namespace`。レートは Prometheus の `rate()` で計算可能。
  - v1 API（`/api/v5/mqtt/topic_metrics` と `/api/v5/mqtt/topic_metrics/:topic`）は変更なく動作継続。Swagger 仕様で非推奨とされ、統合は v2 を推奨。

- [#18148](https://github.com/emqx/emqx/pull/18148) OpenTelemetry 経由の Dynatrace 統合を追加。対応信号はトレースとログ。OAuth2 トークンで認証。

#### デプロイメントとセキュリティ

- [#17381](https://github.com/emqx/emqx/pull/17381) OpenAPI 仕様エンドポイントをデフォルトで認証必須に。対象は `GET /api-docs/swagger.json`、`GET /api-spec.json`、`GET /api-spec.md`、`GET /api-spec/:tag[/:name]`。

  未認証リクエストは 401 を返し、`WWW-Authenticate` ヘッダーと最小限の有効な OpenAPI ドキュメント（または Markdown）を返します。ドキュメントにはサポートされるセキュリティスキームと公開ブートストラップエンドポイント（`POST /api/v5/login`、`GET /api/v5/status`）が含まれ、Dashboard の匿名アクセスでの API 表示を制限しつつ認証方法を案内します。

  Dashboard の `api-spec.html` エクスプローラーは匿名で読み込み、既存のセッション Cookie またはトークンで仕様を取得します。

- [#17407](https://github.com/emqx/emqx/pull/17407) [#17808](https://github.com/emqx/emqx/pull/17808) Feature Gates を追加。

  `EMQX_FEATURES` 環境変数で起動時に利用する機能セットを制限可能。無効なプリセットや機能名はノード起動を阻止。依存機能は自動有効化。

  プリセット:

  - `FULL`: デフォルト。全機能を有効化。
  - `ESSENTIAL`: 最小機能セット。コア MQTT ブローカーと認証・認可のみ。

  カスタムリストで選択可能な機能:

  - `dashboard`: Dashboard UI（SSO、RBAC含む）、REST API
  - `data_integration`: コネクター、アクション、ソース、ルールエンジン
  - `message_transformation`: メッセージ変換
  - `schema_validation`: スキーマ検証
  - `schema_registry`: スキーマレジストリ
  - `gateways`: ゲートウェイプロトコル
  - `cluster_link`: クラスターリンク
  - `multi_tenancy`: マルチテナンシーとネームスペース
  - `ai`: AI 機能（A2A レジストリ、AI 補完）
  - `metrics`: Prometheus メトリックエクスポート
  - `mqtt_extensions`: MQTT 拡張（遅延パブリッシュ、トピック書き換え、自動サブスクライブ、遅延サブスクリプション、メッセージキュー、ストリーム）
  - `plugins`: サードパーティプラグイン管理フレームワーク
  - `file_transfer`: MQTT ファイル転送拡張
  - `exhook`: 外部 gRPC フック
  - `opentelemetry`: OpenTelemetry エクスポーター

  自動有効化される依存関係:

  | 機能 | 自動有効化される依存機能 |
  | --- | --- |
  | `data_integration` | `schema_registry` |
  | `message_transformation` | `schema_registry` |
  | `schema_validation` | `schema_registry` |
  | `ai` | `schema_registry` |
  | `metrics` | `dashboard` |
  | `opentelemetry` | `dashboard` |

  他の機能に依存関係はありません。

- [#17768](https://github.com/emqx/emqx/pull/17768) `file://` URL 形式で `node.cookie` をファイルから読み込む機能を追加。

  オペレーターは `node.cookie = "file:///path/to/cookie"`（または環境変数 `EMQX_NODE__COOKIE` に `file://` URL）を設定可能。クラスター秘密を設定ファイルに平文で保存せずに済みます。参照先は通常ファイルまたは FIFO（名前付きパイプ）で、ノード起動時に一度読み込みます。FIFO 使用時は起動時にオーケストレーターが書き込む必要があります。`emqx ctl` などのコマンドは既存ノードからクッキーを取得するため、ファイルを再読み込みしません。

  解決されたクッキーは Erlang VM に直接渡され、`data/configs/vm.*.args` に書き込まれなくなり、起動時のディスクへの秘密保存を防止します。

- [#17803](https://github.com/emqx/emqx/pull/17803) `EMQX_FEATURES=ESSENTIAL` で起動した場合、Erlang コード読み込みモードをデフォルトで `interactive` に変更。無効化された機能の `.beam` ファイルは起動時に全て読み込まれず、必要時にオンデマンドで読み込まれます。これにより、ESSENTIAL モードのノードの常駐メモリフットプリントが大幅に削減されます。`CODE_LOADING_MODE` で上書き可能。

- [#18451](https://github.com/emqx/emqx/pull/18451) 起動時環境変数を `etc/emqx.env`（RPM/DEB は `/etc/emqx/emqx.env`、Docker は `/opt/emqx/etc/emqx.env`）から読み込む機能を追加。

  ファイルには `EMQX_FEATURES` と `EMQX_SECURITY_PROFILE` のデフォルトコメントと説明が含まれます。`emqx.conf` 解析前に読み込まれるため、`emqx.conf` 内で設定できません。`emqx` コマンドは毎回このファイルをソースし、サービス起動、フォアグラウンド起動、`emqx ctl` で同じ値を使用します。パッケージアップグレードで編集内容は維持されます。

- [#18452](https://github.com/emqx/emqx/pull/18452) `GET /nodes` と `GET /nodes/{node}` のノード情報に `security_profile` と `feature_preset` を追加。

  `security_profile` は `legacy` または `hardened`。`feature_preset` は `full`、`essential`、`custom`。ノード起動時に固定され、クラスター内で異なる設定のノードがある場合に一覧で確認可能。停止ノードはこれらのフィールドを返しません。

- [#18453](https://github.com/emqx/emqx/pull/18453) `security_profile_divergence` アラームを追加。

  `hardened` セキュリティプロファイルのノードはクラスター内の他ノードのプロファイルを定期チェックし、`legacy` プロファイルのノードがある場合にアラームを発報。`legacy` ノードはチェックしません。古い EMQX リリースは `legacy` とみなします。アラームメッセージに `legacy` ノード名を含み、アラーム詳細はアラーム中のノードリストを保持。すべてのノードが `hardened` になるか最後の `legacy` ノードが離脱するとアラームは自動解除。

  ローリングアップグレード中は一時的に発生する想定。長期間アクティブな場合は新設定で再起動されていないノードが存在します。

- [#18471](https://github.com/emqx/emqx/pull/18471) `node_dump` 診断スクリプトに起動時環境ファイル `etc/emqx.env` の `EMQX_FEATURES` と `EMQX_SECURITY_PROFILE` 設定を追加（ファイルが存在する場合）。他の変数は収集しません。

- [#18557](https://github.com/emqx/emqx/pull/18557) [#18609](https://github.com/emqx/emqx/pull/18609) `node.default_listener_address` 設定を追加。MQTT リスナー、ゲートウェイリスナー、Dashboard HTTP リスナーの `bind` に明示的なアドレスがない場合のバインドアドレスを指定可能。値は `loopback`（127.0.0.1）、`nodename`（Erlang ノード名のホスト部のアドレス）、`all`（0.0.0.0）、IPv4/IPv6 リテラルアドレス、または起動時に解決するホスト名。未設定時はセキュリティプロファイルが従来通り決定。明示的な `IP:port` バインドが優先。

  公式 Docker イメージは `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all` を設定し、セキュリティプロファイルに関係なくデフォルトリスナーがコンテナ公開ポート経由で到達可能。

  リスナービューは `resolved_address` を追加し、ノード上で実際にバインドされた IP アドレスを表示。`bind` は設定値（ポート含む）を表示し続け、`resolved_address` はセキュリティプロファイルや `node.default_listener_address` 適用後の IP アドレス（ポート除く）を表示。`resolved_address_from` は値の由来を示す。

  両フィールドはノードローカル。`GET /api/v5/listeners/:id` はリクエスト処理ノードの値を返し、`GET /api/v5/listeners` はノードごとに `node_status` 配下で返す。同一クラスター内でノードごとに異なる値になる場合があります（例: `node.default_listener_address=nodename`）。

  `emqx ctl listeners` は既存の `listen_on` と並べて両フィールドを表示。

- [#18628](https://github.com/emqx/emqx/pull/18628) データバックアップのエクスポートにエクスポート元ノードのセキュリティプロファイルを記録。

  `legacy` プロファイルでエクスポートされたバックアップを `hardened` プロファイルのノードに復元すると、以下の動作差異が発生する可能性があります：

  - MQTT、ゲートウェイ、Dashboard HTTP リスナーの裸ポートバインドが全インターフェースからループバックに変わる
  - 空または無効化された認証チェーンが全クライアント拒否に変わる
  - 復元された Dashboard アカウントはデフォルトパスワードでログイン不可になる
  - 以前は無視されていた認証・認可バックエンド失敗が操作拒否になる

  このようなバックアップの復元には CLI フラグ `--allow-security-profile-mismatch` または API パラメータ `allow_security_profile_mismatch` が必要で、オペレーターが差異を確認可能に。変更前のバックアップは `legacy` と同等に扱われます。`legacy` ノードへの復元は影響なし。

#### プラグイン

- [#18455](https://github.com/emqx/emqx/pull/18455) `hardened` セキュリティプロファイル下で `emqx ctl plugins allow <Name-Vsn>` は `sha256:<hex>` 引数を必須化。プラグインパッケージを指定された SHA-256 ダイジェストに紐付け、バイトが一致しないアップロードは拒否。引数なしは拒否し、必要なコマンドを表示。クラスターピアから送られたダイジェストなしの許可も拒否。

  `legacy` プロファイルは変更なしで引数は任意。

#### パッケージング

- [#17335](https://github.com/emqx/emqx/pull/17335) RPM/DEB インストール後に `/opt/emqx` ディレクトリを作成し、パッケージで使用される散在する FHS パスを指す便利なシンボリックリンク（`bin`、`data`、`etc`、`lib`、`log`、`plugins`、`releases`、`erts-*`）を配置。オペレーターは公式 Docker イメージと同じ `/opt/emqx/...` パスを使用可能。

#### パフォーマンス

- [#17583](https://github.com/emqx/emqx/pull/17583) JSON エンコード・デコードのパフォーマンスを改善。

  Erlang/OTP 標準に準拠した浮動小数点数のフォーマットに変更され、以前と若干異なる場合があります（例: 科学記数法の使用開始が早くなる）。

- [#18033](https://github.com/emqx/emqx/pull/18033) ESSENTIAL 機能モード（および Dashboard/管理クライアントの client-info API 無効時）で、`GET /clients` エンドポイントのみに利用される接続ごとの統計報告を停止し、高接続数時のオーバーヘッドを削減。

- [#18424](https://github.com/emqx/emqx/pull/18424) プラグイン設定スキーマ（`config_schema.avsc`）をプラグイン設定検証時に毎回インストール済みプラグインパッケージから読み込み、メモリに保持しなくなりました。

  インストール済みプラグインのスキーマファイルが欠落または読み込み不可の場合は設定検証でファイルエラーを報告。

- [#18688](https://github.com/emqx/emqx/pull/18688) ダーティ I/O スケジューラースレッド数（`+SDio`）を `node.dirty_io_schedulers` で設定可能に。デフォルト `auto` は `node.schedulers` が2以上のノードで従来の8、2以下の小規模ノード（例: 2 vCPU コンテナ）で4を使用。小規模ノードの起動時メモリフットプリントを削減しつつ、ブロッキング I/O オーバーラップに十分なスレッド数を確保。

### バグ修正

#### Core MQTT 機能

- [#18010](https://github.com/emqx/emqx/pull/18010) クライアントから送信された不正な MQTT パケットをブローカーエラーとしてログに記録しなくなりました。

  不正入力はリスナーの接続シャットダウンカウンターにカウントされ、ポートスキャンやプロトコルファザー、誤動作クライアントのアラートノイズを削減。パケット固有の詳細を持つ解析エラーは単一の `frame_error` カウンターを共有し、新規カウンター名を生成しません。

  解析エラーの詳細（問題のバイト列など）はトレースで報告されます。クライアント ID、IP アドレス、トピックでトレースを開始して確認可能。

- [#18027](https://github.com/emqx/emqx/pull/18027) 共有サブスクリプションが無効な場合、共有サブスクライブを試みるクライアントを切断するように共有サブスクリプション処理を変更。

  `mqtt.shared_subscription = false` でクライアントが `$share/...` または `$queue/...` を含む SUBSCRIBE を送信すると、MQTT 仕様に従い接続を閉じます。MQTT 5.0 クライアントには理由コード `0x9E`（Shared Subscriptions not supported）付き DISCONNECT パケットを送信。MQTT 3.1/3.1.1 クライアントは単に接続を閉じます。

  以前は失敗理由コード付き SUBACK を返し接続は維持されていました。

- [#18116](https://github.com/emqx/emqx/pull/18116) `strict_mode` 有効時（デフォルト）、MQTT v5 パケットで非繰り返しプロパティが複数回含まれる場合（例: CONNECT パケットに `Session-Expiry-Interval` が2つ以上）はプロトコルエラーとして拒否。最後の値を黙って使う動作から変更。繰り返し許可の `User-Property` は影響なし。

  従来の寛容な動作はリスナー設定で `strict_mode = false` により復元可能。

- [#18438](https://github.com/emqx/emqx/pull/18438) 送信パケットサイズチェックのオフバイワンを修正。

  クライアントの `Maximum Packet Size` と同じサイズのパケットは破棄され `frame_is_too_large` とログに記録されていました。MQTT 5.0 仕様では制限超過パケットのみ破棄すべきであり、同サイズは送信されます。制限未満のパケットは影響なし。

- [#18470](https://github.com/emqx/emqx/pull/18470) 元のパケットがパブリッシャー切断前に受信されていなかった場合に、再送された QoS 2 PUBLISH パケットをアックするが配信しない問題を修正。

  [#16721](https://github.com/emqx/emqx/pull/16721) で導入された QoS 2 重複処理の変更を部分的に元に戻しました。PUBREL 待ち状態が期限切れ後、再送された QoS 2 PUBLISH は新規 QoS 2 交換として扱われ、購読者に複数回配信される可能性があります。EMQX 7.0.0 以降は PUBREL 待ち状態期限切れをデフォルトで無効にし、再配信を防止予定。

- [#18487](https://github.com/emqx/emqx/pull/18487) 不明なパケット識別子を持つ PUBACK、PUBREC、PUBREL、PUBCOMP パケットのログ量を削減。これらはデバッグレベルでログされ、クライアントトレースで確認可能。

- [#18523](https://github.com/emqx/emqx/pull/18523) ノードシャットダウン時の停止順序を修正し、アプリケーション停止前に MQTT リスナーを停止。

  以前はリスナーがクライアントトラフィックを受け付け続け、パブリッシュパスのアプリケーションが停止済みであったため、ルールエンジンなどで `hook_callback_exception` エラーが多発。リスナーを先に停止することでアプリケーション停止中にトラフィック処理を防止。

  ノードはシャットダウン開始時点で `GET /status` で非稼働と報告し、ロードバランサーが新規接続を停止。

- [#18585](https://github.com/emqx/emqx/pull/18585) MQTT 仕様に従い、同一クライアント ID で新接続がセッションを引き継がない場合は古いセッションを終了するように修正。

  MQTT 5.0 クライアントの Session Expiry Interval 0、MQTT 3.1.1 クライアントの Clean Session 1 が対象。以前は新接続が古いセッションのサブスクリプションやキューイングメッセージを引き継ぎ、Will Delay Interval > 0 の遺言メッセージが黙って破棄されていました。現在は新接続が新規セッション（CONNACK Session Present 0）を開始し、古い接続は理由コード 0x8E（Session taken over）付き DISCONNECT を受け、遺言メッセージがあればテイクオーバー時にパブリッシュされます。

#### アクセス制御

- [#18246](https://github.com/emqx/emqx/pull/18246) ハードニングセキュリティプロファイルで遅延メッセージ認可を追加。

  - 遅延メッセージは再生時に再認可されます。MQTT とゲートウェイクライアントの遅延メッセージは制限付き認可コンテキストを持ち、現在のパブリッシュ認可ルールと禁止レコードをチェック。スケジュール時に認可済みでも再生時に破棄される場合があります。
  - ハードニングプロファイルでは、アップグレード前に作成された認可コンテキストを持たない保留遅延メッセージは再生時に破棄されます。レガシープロファイルは再生します。
  - 複数ゲートウェイでのマウントポイント処理を修正。ゲートウェイは論理トピック（マウント解除済み）を認可に渡します。`authorization.include_mountpoint = false` なら論理トピックをチェック。`true` なら認可チェック前にマウントポイントを1回適用。パブリッシュ・サブスクライブ時は常にマウントポイントを1回適用。
  - GBT 32960、JT/T 808、LwM2M、NATS、STOMP ゲートウェイはマウント済みトピックをパブリッシュ認可に渡さなくなりました。重複マウントによる認可チェックを防止。
  - GBT 32960 の `dnstream`、JT/T 808 の `proto.dn_topic`、LwM2M のコマンド自動サブスクライブは認可前にマウントポイントを適用しません。
  - JT/T 808 の `proto.up_topic` と `proto.dn_topic` はゲートウェイマウントポイント相対に変更。デフォルトは `jt808/${clientid}/${phone}/up` と `jt808/${clientid}/${phone}/dn` から `${phone}/up` と `${phone}/dn` に変更。
  - MQTT-SN のアイドル QoS -1 パブリッシュと遺言メッセージは設定されたマウントポイントを適用。以前は適用されていませんでした。
  - NATS パブリッシュ認可は NATS サブジェクトから変換した MQTT トピックに対してマウントポイント適用前にチェック。NATS JWT 権限と EMQX 認可はマウント済みトピックをチェックしません。
  - 遅延メッセージの重複処理を修正。ブリッジ、リテイナー、スキーマバリデーターなど他システムは遅延メッセージを再生時のみ処理。プラグインなどの直接内部パブリッシャーは遅延メッセージスケジューリングに `message.ingress` フックを呼び出す必要があります。`emqx:publish/1` 直接呼び出しや管理 API の `$delayed/...` パブリッシュはメッセージイングレスをバイパスし、遅延メッセージをスケジューリングしません。

- [#18458](https://github.com/emqx/emqx/pull/18458) `oidcc` を 3.2.3 にアップグレード。

  プロバイダー設定ワーカーがキャッシュ更新中に忙しい場合の Dashboard SSO（OIDC）ログインタイムアウト（`INTERNAL_ERROR: exit,{timeout,{gen_server,call,[...]}}`）を修正。

- [#18576](https://github.com/emqx/emqx/pull/18576) OIDC SSO 設定 API (`GET /api/v5/sso/oidc`) はクライアント JWKS 未設定時に `client_jwks` を `none` で返すように修正。CLI 出力と一致。以前は未設定でもマスクされた `******` を返していました。設定済み JWKS はマスクのまま。

- [#18580](https://github.com/emqx/emqx/pull/18580) `bin/node_dump` スクリプトが生成する `conf.hocon` ファイルで機密設定値をマスク。

  設定スキーマで機密指定された `dashboard.default_password` や `license.key` などの値は `******` として書き込み。修正前は固定のキー名リストのみマスクされ、これらは平文で書き込まれていました。

#### マルチテナンシー

- [#18423](https://github.com/emqx/emqx/pull/18423) ネームスペース管理者によるデータバックアップインポートは自身のネームスペース設定のみ適用。認証、認可、ExHook、リスナーなどのクラスタ全体設定は警告付きでスキップされ、グローバル設定に書き込まれません。

- [#18466](https://github.com/emqx/emqx/pull/18466) 特殊文字を含むネームスペース名のバックアップファイル一覧表示を修正。

  以前は `*`、`?`、`{}`、`[]` などを含むネームスペースでバックアップファイルが存在しても一覧が空でした。ネームスペース名をリテラルディレクトリ名として扱います。

- [#18539](https://github.com/emqx/emqx/pull/18539) ネームスペースが変わって再接続した永続セッションをマルチテナンシークライアント一覧が追従しない問題を修正。

  以前は `clean_start=false` で既存セッションを再開した際、古いネームスペースにクライアントが残り、新しいネームスペースの一覧に含まれませんでした。クライアント一覧とネームスペースごとのクライアント数は常に接続時のネームスペースを反映し、永続セッション再開後にクライアントが消える問題も修正。

#### ゲートウェイ

- [#17796](https://github.com/emqx/emqx/pull/17796) UDP ソースポートが最近切断されたデバイスで使われていた場合に MQTT-SN ゲートウェイがクラッシュする問題を修正。古いチャネルを正常に退避し、新規接続を新規セッションとして処理。

- [#17805](https://github.com/emqx/emqx/pull/17805) 不正な設定やポート競合でロードが途中中断された後、ゲートウェイの再ロードが `already_started` エラーで失敗する問題を修正。失敗時のロックプロセスを自動回収し、次回ロードやオペレーター再試行時にクリーンな状態から開始。

- [#17815](https://github.com/emqx/emqx/pull/17815) UDP ソースタプルが変わるか再利用される場合の MQTT-SN UDP セッションルーティングを修正。

  MQTT-SN UDP リスナーはパケットから解析した ClientId で `esockd_udp_proxy` を通じてルーティングし、スリープ中セッションが異なる UDP ソースタプルから再開可能にし、再利用された UDP ソースタプルが別 ClientId のパケットを古いセッションに届けるのを防止。

- [#17888](https://github.com/emqx/emqx/pull/17888) LwM2M ゲートウェイが MQTT 登録・更新レポートに `password`、`secret`、`private_key`、`access_token` などの機密クエリフィールドを含める問題を修正。

- [#18051](https://github.com/emqx/emqx/pull/18051) CoAP デバッグログの URI クエリ値漏洩を修正。

#### プラグイン

- [#17861](https://github.com/emqx/emqx/pull/17861) ノード起動や再参加時にクラスタープラグイン設定に存在しないローカルプラグインパッケージを削除しないことで、以前のプラグイン起動動作を復元。

- [#17884](https://github.com/emqx/emqx/pull/17884) クラスタープラグイン設定に存在せずローカルで実行もされていない古いプラグインパッケージをプラグイン管理 HTTP API が無視するよう修正。

  これらの古いパッケージはプラグイン一覧、詳細、設定、スキーマレスポンスに表示されず、操作 API で操作できず、HTTP インストール API での再インストールを妨げません。設定済みのプリインストールプラグインは表示され続け、ドキュメント通りのプリインストールワークフローに従います。

  プラグインパッケージがアンパックされているが `plugins.states` で有効・無効いずれでもない場合、起動時と HTTP API アクセス時にエラーをログ出力。

- [#17932](https://github.com/emqx/emqx/pull/17932) CLI でプラグインインストール時の `failed_to_get_plugin_config_from_cluster` 警告ログを抑制。

  `emqx ctl plugins install` は HTTP API と同様に `fresh_install` モードでインストールし、クラスタ設定検索をスキップ。これによりクラスタ内全ノードでの繰り返し警告を回避。

  `--cluster` フラグを追加し、指定時は稼働中全ノードに一括配布・インストール。

- [#18018](https://github.com/emqx/emqx/pull/18018) プラグインパッケージインストール時にパッケージのアプリケーション宣言、設定スキーマ、デフォルト設定の検証前にコードをロードしていた問題を修正。

- [#18153](https://github.com/emqx/emqx/pull/18153) ルート JSON 値の型が不正な場合にプラグイン設定 API が `500 INTERNAL_ERROR` ではなく読みやすい検証エラーを返すよう修正。

#### オブザーバビリティ

- [#17886](https://github.com/emqx/emqx/pull/17886) パブリッシュクォータ超過パケットメトリックを Prometheus で `emqx_packets_publish_quota_exceeded` として公開。

- [#18114](https://github.com/emqx/emqx/pull/18114) ノードがクラスター参加中に Dashboard メトリクス API (`GET /api/v5/monitor_current`、`GET /api/v5/monitor`) が `500 INTERNAL_ERROR` を返す問題を修正。

  参加中ノードのアプリケーション再起動中にメトリクスサンプリングが失敗しても、残りの到達可能ノードの集計を返し、警告ログを出力してリクエスト全体の失敗を防止。

  また、`DELETE /api/v5/monitor` の成功時に毎回出ていた不要な `clear_monitor_metrics_rpc_errors` 警告を修正。

#### ファイル転送

- [#18069](https://github.com/emqx/emqx/pull/18069) ファイル転送ファイル一覧 API (`GET /api/v5/file_transfer/files`) が非 ASCII 文字（例: 中国語）を含むファイル名で 500 エラーになる問題を修正。

#### デプロイメント

- [#17877](https://github.com/emqx/emqx/pull/17877) `emqx-enterprise` Helm チャートのノードホスト名に `svc.cluster.local` をハードコードしていた問題を修正。DNS ドメインが `cluster.local` でない Kubernetes クラスターで名前解決できず Erlang 分散が開始できずクラスター形成に失敗していました。ホスト名はチャートの `clusterDomain` 値に従います。

## 6.2.3

*リリース日: 2026-08-21*

EMQX 6.2.3 へのアップグレード前に破壊的変更と既知の問題を必ずご確認ください。

### 改善点

#### アクセス制御

- [#17813](https://github.com/emqx/emqx/pull/17813) Dashboard ユーザーと API キーのエンドポイントで、管理者相当の権限を持つスコープ（`system`、`user_management`、`api_key_management`、`sso_management`）とその他のスコープが混在するスコープリストを拒否する検証を追加。これら4つの特権スコープは管理者相当の権限を持つため、制限付きスコープと組み合わせても意味のある制限になりません。管理者相当のスコープのみ、またはそれ以外のみを使用してください。

  既存の混在スコープを持つレコードは更新まで動作継続。更新時は分割が必要。

- [#17980](https://github.com/emqx/emqx/pull/17980) ハードニングセキュリティプロファイルで、サーバー起動のサブスクライブに対してトピック検証、認可、MQTT 機能チェック、クライアントサブスクライブフックを適用。

- [#18002](https://github.com/emqx/emqx/pull/18002) ハードニングセキュリティプロファイルで SAML レスポンスとアサーションの署名検証をデフォルトで有効化。

- [#18296](https://github.com/emqx/emqx/pull/18296) `is_jwt(value)` 関数を認証器の `precondition` 式（および Variform 式全般）に追加。値が構造的に JWT（JWS コンパクト形式）である場合にのみ真を返し、署名は検証しません。JWT でないパスワードを提示するクライアントを JWT 認証器からスキップ可能。特にハードニングプロファイルで非 JWT 資格情報が JWT 認証器で拒否される問題を回避。

#### マルチテナンシー

- [#17732](https://github.com/emqx/emqx/pull/17732) API キー作成・更新エンドポイントに `namespace` フィールドを追加。オペレーターは `role` 文字列にネームスペースを埋め込む必要がなくなりました（既存の `ns:<namespace>::<role>` 形式は引き続き有効）。両方指定した場合は一致が必要。

- [#17855](https://github.com/emqx/emqx/pull/17855) ネームスペーススコープ付き Dashboard 管理者は自身のネームスペース内の API キーを作成、一覧、参照、更新、削除可能に。グローバル API キーや他ネームスペースのキーは作成不可で、他ネームスペースのキーは非表示。

#### データ統合

- [#17933](https://github.com/emqx/emqx/pull/17933) RabbitMQ コネクターで複数ノードの `servers` リスト（例: `rmq1:5672,rmq2:5672`）をサポート。接続時フェイルオーバーとプール開始オフセットのローテーションを提供。`servers` 未設定時は従来の `server`/`port` を使用。

- [#17944](https://github.com/emqx/emqx/pull/17944) HTTP コネクターと HTTP 認証・認可に OAuth2 クライアントクレデンシャル認証を追加。設定時、EMQX はトークンエンドポイントからアクセストークンを取得・更新し、アウトバウンドリクエストに Bearer 認証ヘッダーとして追加。

  コネクターのヘルスチェックはトークン取得不能時に `disconnected` を報告。OAuth2 有効かつ `Authorization` ヘッダー指定は拒否。

  EMQX はトークンリクエストボディにクライアント ID とシークレットをフォームフィールドで送信。HTTP Basic `Authorization` ヘッダーでの送信は非対応。

- [#18014](https://github.com/emqx/emqx/pull/18014) Datalayers Arrow Flight コネクターで準備済みステートメントの自動再構築を追加。サーバーが準備済みステートメントを失った場合（例: 再起動後）、クライアントが自動で再作成し書き込み操作を再試行、書き込み失敗を回避。

- [#18042](https://github.com/emqx/emqx/pull/18042) DynamoDB コネクターに AWS IAM ロール認証を追加。

  アクセスキー ID とシークレットアクセスキーが両方省略された場合、EMQX は ECS タスクロールまたは EC2 インスタンスメタデータから一時認証情報を取得し、有効期限前に更新。

- [#18081](https://github.com/emqx/emqx/pull/18081) Snowflake Streaming アクションのレジリエンスを改善。チャネル内部状態が同期外れになる特定のエラー時に失敗した行をリトライし、手動介入なしにチャネル再オープンを試行。

- [#18085](https://github.com/emqx/emqx/pull/18085) Kafka、Confluent、Azure Event Hubs プロデューサーに以下の設定を追加。

  - `max_batch_age`（アクション）: プロデューサーバッファ内でこの期間を超えたメッセージを破棄し、`dropped.expired` メトリックを増加。デフォルトは `infinity`（破棄なし）。
  - `max_retries`（アクション）: 失敗リトライ回数上限。上限超過でバッチを破棄し、`failed` メトリックを増加。リトライカウンターは Kafka のエラーコード応答時のみ増加し、接続喪失後の再送はカウントしない。デフォルトは `infinity`（無限リトライ）。
  - `reconnect_delay`（アクション）: 接続喪失後の再接続遅延。デフォルトは `2s`（従来は固定）。
  - `request_timeout`（コネクター）: Kafka 応答待ち時間。デフォルトは `30s`。

  Kafka クライアントライブラリ（wolff）を 4.2.1 にアップグレードし、メモリモードバッファの `max_linger_time` サポートを復活。小規模バッチは最大 `max_linger_time` 待機してメッセージを集約し、送信リクエスト頻度を削減。満杯バッチは遅延なしで送信。

- [#18110](https://github.com/emqx/emqx/pull/18110) スキーマレジストリで JSON Schema draft 2019-09 と 2020-12 をサポート。

- [#18137](https://github.com/emqx/emqx/pull/18137) GCP Pub/Sub プロデューサー・コンシューマーが完全修飾トピックパス（`projects/<project-id>/topics/<topic-name>`）を受け入れ可能に。サービスアカウントのプロジェクトとは異なる GCP プロジェクトのトピックにパブリッシュ・コンシューム可能。ベアトピック名は従来通りサービスアカウントのプロジェクトで解決。コンシューマーは購読をサービスアカウントのプロジェクトに作成し、トピック参照のみ別プロジェクトを指す。

#### ルールエンジン

- [#18253](https://github.com/emqx/emqx/pull/18253) 2つの SQL 関数 `map_to_range(value, min, max)` と `hash_to_range(value, min, max)` を追加。値またはそのハッシュを指定範囲内の整数にマッピング。シャーディングやバケット分割に便利。例: `hash_to_range(nth(2, tokens(topic, '/')), 0, 3)` でトピックの2番目トークンからシャードインデックスを算出。

- [#18306](https://github.com/emqx/emqx/pull/18306) LZ4 Frame 圧縮・解凍のルール関数 `lz4_compress` と `lz4_uncompress` を追加。

#### プラグイン

- [#18012](https://github.com/emqx/emqx/pull/18012) EMQX REST API 経由の同期 MQTT リクエスト/レスポンスフローを提供する `emqx_sync_request` プラグインを追加。ノードローカル CLI 診断でリクエストカウンターと現在の保留状態を確認可能。

#### パッケージング

- [#18037](https://github.com/emqx/emqx/pull/18037) Enterprise Linux 10（Red Hat Enterprise Linux 10、Rocky Linux 10、互換ディストリビューション）用パッケージを追加。

- [#18127](https://github.com/emqx/emqx/pull/18127) macOS 26（Tahoe）用パッケージのリリースを開始。

### バグ修正

#### Core MQTT 機能

- [#17895](https://github.com/emqx/emqx/pull/17895) 管理証明書バンドルからファイルベース証明書に切り替える TLS リスナーで、参照先バンドルが既に削除されていても成功するよう修正。

- [#17911](https://github.com/emqx/emqx/pull/17911) Erlang/OTP `ssl` アプリケーションが対応している場合、DTLS リスナーで `ECDHE-PSK-CHACHA20-POLY1305` 暗号スイートを検証可能に。

- [#18102](https://github.com/emqx/emqx/pull/18102) 配信レート制限が有効な場合に MQTT クライアントが QoS 1/2 メッセージを順不同で受信する問題を修正。EMQX はブロックされたメッセージが送信可能になるまで後続メッセージをキューに保持。

- [#18108](https://github.com/emqx/emqx/pull/18108) 参照中の管理証明書バンドルやそのファイルを削除しようとすると、参照設定を列挙した明確なエラーで失敗。`force_delete` クエリパラメータはチェックをバイパスしなくなり、API スキーマから削除。

  さらに、証明書バンドルがディスクに存在しないリスナーがある場合、Prometheus 統計エンドポイントが完全に失敗しなくなり、該当リスナーは証明書有効期限メトリックから除外され、警告ログを出力。

- [#18111](https://github.com/emqx/emqx/pull/18111) `mqtt.strict_mode` 有効時、MQTT v3.1 CONNECT パケットでパスワードフラグがセットされユーザーフラグがセットされていない場合は拒否。MQTT v3.1.1 と同様の仕様準拠。MQTT v3.1 仕様ではユーザーなしパスワードは無効。

  接続ログの可読性も改善。CONNECT パケットトレースはパスワード未指定時に `Password=undefined` と表示し、空パスワードと区別。ログの `peername` フィールドは常に `10.0.0.1:54123` のような文字列で表示。

#### ルールエンジン

- [#17957](https://github.com/emqx/emqx/pull/17957) `rule_engine.limit_selects_in_namespace = true` 時にグローバルネームスペースの複数ルールイベント（例: `$events/client/connack`）がトリガーされない問題を修正。

- [#18049](https://github.com/emqx/emqx/pull/18049) `rule_engine.limit_selects_in_namespace = true` 設定時にアラーム有効化/無効化トリガーのグローバルルールが発火しない問題を修正。

#### データ統合

- [#17859](https://github.com/emqx/emqx/pull/17859) MQTT コネクターが IPv6 ブローカーに接続できるよう修正。

  以前は `[::1]:1883` のような IPv6 リテラルが保存時に `bad_host_port` バリデーションエラーとなり、IPv6 のみ解決するホスト名は IPv4 で接続しようとしてホスト解決失敗していました。

  サーバーアドレスパーサーは括弧付き IPv6 リテラル（例: `[::1]`, `[::1]:1883`, `mqtt://[::1]:1883`）を受け入れ、MQTT コネクターは接続時に IPv6 プローブを有効化し、IPv6 専用ブローカーに到達可能に。

  MQTT コネクターとクラスターリンクの `server` アドレスは公式 MQTT URI スキーム `mqtt`（プレーン TCP）と `mqtts`（TLS）を受け入れ。例: `mqtt://broker:1883`、`mqtts://broker:8883`。スキームなしの `host:port` も受け入れ。その他のスキームは `unsupported_scheme` バリデーションエラーで拒否。

- [#17947](https://github.com/emqx/emqx/pull/17947) HTTP コネクター更新時にアクションバッファーワーカーがブロックされ、メッセージが次のリトライまでキューに残る問題を修正。

- [#17955](https://github.com/emqx/emqx/pull/17955) GreptimeDB 非同期バッチが低書き込みレート時のヘルスチェック後にフラッシュされない問題を修正。

- [#17961](https://github.com/emqx/emqx/pull/17961) Kafka や Pulsar コネクターがヘルスチェックタイムアウトで `disconnected` 状態に遷移し、内部キューが再作成される問題を修正。`connecting` 状態に遷移するように。

#### クラスタリング

- [#18826](https://github.com/emqx/emqx/pull/18826) バックアップインポート前にクラスター内全ノードが同一バージョンであることを確認。

  ローリングアップグレード中のインポートは、未アップグレードノードが異なる解釈をする呼び出しで適用される可能性がありました。インポートは開始前に停止し、アップグレード待ちノードを列挙してクラスター状態を維持します。

- [#18862](https://github.com/emqx/emqx/pull/18862) `emqx_router_tool:scan_missing_routes/1` と `emqx_router_tool:reconcile_missing_routes/1` に渡すオプションの検証を追加。

  不正な `chunk` や `sleep_ms` 値はスキャンスロットリングを無効化し、オペレーターがスロットリングされていると思い込む問題がありました。ツールは不正な値や未知のオプションキー（例: 誤字の `chunks`）を拒否し、問題のあるオプション名を含むエラーを発生させます。

- [#18899](https://github.com/emqx/emqx/pull/18899) ローリングアップグレード中に混在バージョンクラスターで `GET /api/v5/listeners` と `GET /api/v5/listeners_status` が `500 INTERNAL_ERROR` を返すリグレッションを修正。

#### Durable Storage

- [#18822](https://github.com/emqx/emqx/pull/18822) ノード再起動やリーダー変更後に Durable Storage シャードが復旧できない稀な問題を修正。EMQX は追加の一時的 DS Raft リーダーエラーを再試行可能とみなし、不完全な RocksDB スキーマ初期化がシャード復旧を妨げなくなりました。

#### ゲートウェイ

- [#18755](https://github.com/emqx/emqx/pull/18755) Dashboard から既存の NATS ゲートウェイ設定を保存すると、認証資格情報がフォームに表示されるマスク値に置き換わり、NATS クライアントが認証失敗する問題を修正。

  認証資格情報は、認証設定を変更せずに他のゲートウェイ設定を更新した場合に保持されます。

  NATS ゲートウェイ認証設定は重複する認証方式や資格情報（重複 NKey や JWT アカウントエントリ）を拒否し、あいまいな認証動作を防止。

- [#18776](https://github.com/emqx/emqx/pull/18776) MQTT-SN はスリープクライアントのスリープ期間満了時に設定済みの Will メッセージをパブリッシュし、正常切断時には Will メッセージをパブリッシュしなくなりました。

- [#18842](https://github.com/emqx/emqx/pull/18842) ゲートウェイプロトコルはクライアント ID オーバーライドをサポートしません。認証バックエンドが返す `clientid_override` を無視し、警告をログに出力。

  ゲートウェイ接続のマウントポイントテンプレートは認証結果統合後の共有ゲートウェイ認証フローで評価されます。

- [#18855](https://github.com/emqx/emqx/pull/18855) 証明書検証済みクライアント証明書で認証された DTLS クライアントの MQTT-SN セッションウェイクアップ認可を修正。

  証明書に紐づくセッションは同一のピア証明書を提示した新しい関連付けからのみ再開可能。クライアント証明書なし、異なる証明書、再発行証明書は拒否。

  証明書なしの平文 UDP と DTLS クライアントは従来の ClientId のみの PINGREQ ウェイクアップ動作を維持。

#### プラグイン

- [#18891](https://github.com/emqx/emqx/pull/18891) EMQX 6.x で Sync Request プラグインのインストール・起動が `missing_i18n_ref` エラーで失敗する問題を修正。プラグインは正常に起動し、API エンドポイントが利用可能に。

#### オブザーバビリティ

- [#18664](https://github.com/emqx/emqx/pull/18664) ネームスペース付きリクエストの監査ログに対象ネームスペースを記録。以前は同名の異なるネームスペースのトピックメトリクスコレクションのリセットが区別できませんでした。クエリパラメータも記録されます。

- [#18677](https://github.com/emqx/emqx/pull/18677) データバックアップリクエストの監査ログに対象ネームスペースを記録。以前は異なるネームスペースのバックアップ操作が区別できませんでした。クエリパラメータも記録されます。

- [#18678](https://github.com/emqx/emqx/pull/18678) A2A レジストリリクエストの監査ログに対象ネームスペースを記録。以前は異なるネームスペースのエージェントカード操作が区別できませんでした。クエリパラメータも記録されます。

- [#18684](https://github.com/emqx/emqx/pull/18684) OpenTelemetry トレース有効時に MQTT 5 クライアントの CONNECT パケットに Will User-Property が含まれる場合の接続クラッシュを修正。

- [#18686](https://github.com/emqx/emqx/pull/18686) 認可、認証、コネクター、ブリッジ、ルールエンジン、トレースリクエストの監査ログに対象ネームスペースを記録。以前はネームスペース間で区別できませんでした。クエリパラメータも記録されます。

- [#18754](https://github.com/emqx/emqx/pull/18754) ヘルプのみを表示する CLI コマンドで監査ログを作成しないようにしました。

  例: `emqx ctl api_keys` はサブコマンドなしで使用すると使用法を表示しますが、以前は空引数で操作が行われたように監査ログが記録されていました。実際に操作を行うコマンドは監査されます。

- [#18853](https://github.com/emqx/emqx/pull/18853) SSO MFA 設定・検証認証情報が監査ログにマスクされずに保存される問題を修正。

  SSO MFA リクエストボディは `username` と `backend` のみ監査記録に残し、一時トークン、TOTP コード、未知の認証情報フィールドは監査ファイル・監査 DB に書き込む前にマスクされます。

- [#18961](https://github.com/emqx/emqx/pull/18961) `emqx ctl plugins ...` コマンドの監査ログで全引数が `******` でマスクされるリグレッションを修正。`plugins` 引数は機密情報でないためそのまま記録します。`plugins install --cluster` オプションは CLI リファクタリングで誤って削除されていたため復活しました。

#### 管理

- [#18600](https://github.com/emqx/emqx/pull/18600) Durable セッション有効時のクライアントリスト API フィルタリングを修正。

  `GET /api/v5/clients` は切断済み durable セッションをすべて結果に追加しており、`conn_state`, `username`, `clientid` などのクエリフィルターを無視していました。例: `conn_state=connected` でも切断済み durable セッションを返していました。現在は切断済み durable セッションにもフィルターが適用されます。

  フィルター適用時に切断済み durable セッションが存在するとき、レスポンスは誤った数を返す代わりに `meta.count` フィールドを省略します。`conn_state=connected` フィルター時は正確な `meta.count` を返します。

- [#18666](https://github.com/emqx/emqx/pull/18666) `GET /api/v5/topics` は durable 共有サブスクリプションの完全な共有トピック（例: `$share/group/a/1`）を返すようになりました。

  以前はトピックのみ（例: `a/1`）を返しており、通常の durable サブスクリプションと区別できませんでした。

- [#18817](https://github.com/emqx/emqx/pull/18817) リクエストボディに `enable` フィールドがない場合、`PUT /api/v5/telemetry/status` が Erlang スタックトレース付きで `500 INTERNAL_ERROR` を返す問題を修正。

  現在は検証メッセージ付きで `400 BAD_REQUEST` を返します。API ドキュメントは `enable` を必須とし、デフォルト値を表示しません。エンドポイントは以前からデフォルトを適用していません。

- [#18859](https://github.com/emqx/emqx/pull/18859) 設定更新時に HTTP ヘッダー名の大文字小文字だけを変更すると保存済み値が消える問題を修正。

- [#18860](https://github.com/emqx/emqx/pull/18860) クライアントメッセージページネーションで最終ページに継続位置を返す問題を修正。

  `GET /clients/{clientid}/mqueue_messages` と `GET /clients/{clientid}/inflight_messages` は、メッセージがなくなると `meta.position` に `end_of_data` を返し、API クライアントは追加の空ページをリクエストせずに停止できます。

#### デプロイメントとセキュリティ

- [#18836](https://github.com/emqx/emqx/pull/18836) クラスター設定同期のデバッグログに成功した設定変更結果を含めるのをやめました。

  結果には HTTP 認証ヘッダーのテンプレートなどのコンパイル済みランタイム状態が含まれ、ログのマスク対象外の秘密情報を含んでいました。

---

（以下、6.3.0 より前のバージョンの詳細は省略します）
