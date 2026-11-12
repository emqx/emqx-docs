# EMQX 6.3 の互換性のない変更点

## 6.3.3

- [#18788](https://github.com/emqx/emqx/pull/18788) セキュリティ強化のため、`mq.auto_create` と `streams.auto_create` の自動作成をデフォルトで無効化しました。

  以前は、存在しないキューやストリームに対して `$queue/...` トピックをサブスクライブするとラストバリューメッセージキューが、`$stream/...` トピックをサブスクライブするとラストバリューメッセージストリームが作成されていました。現在は両方ともデフォルトで `false` となり、サブスクライブ時に暗黙的に作成されることはありません。

  以前の動作を維持するには、自動作成を明示的に有効化してください。

  ```
  mq.auto_create.lastvalue = {}
  streams.auto_create.lastvalue = {}
  ```

  通常のキューやストリームを自動作成する場合は `lastvalue` の代わりに `regular` を設定します。両方を同時に有効にすることはできません。既存のメッセージキューやストリームには影響しません。

- [#18974](https://github.com/emqx/emqx/pull/18974) MQTT v5 の CONNECT プロパティおよび Will プロパティで受け入れる User Property ペアの数を制限する `mqtt.max_connect_user_properties` を追加しました。デフォルトは 100 で、制限を無効にするには `infinity` に設定します。

- [#19774](https://github.com/emqx/emqx/pull/19774) インメモリセッションの内部メッセージヘッダー `deliver_begin_at` を `deliver_begin_at_us` に名称変更し、タイムスタンプの単位をミリ秒からマイクロ秒に変更しました。

  このヘッダーを参照するプラグインやフックハンドラーは新しい名前と精度に対応してください。

- [#19910](https://github.com/emqx/emqx/pull/19910) RabbitMQ コネクターの `heartbeat` 値が RabbitMQ クライアントに渡される前にミリ秒に変換されていた問題を修正しました。RabbitMQ クライアントは秒単位で解釈するため、リモート AMQP サーバーが `0` のハートビートを提案した場合、アイドル接続がタイムアウトして再接続を繰り返すことがありました。EMQX は現在、設定された値を秒単位で渡します。

## 6.3.1

- [#18465](https://github.com/emqx/emqx/pull/18465) ClickHouse、TDengine、SQL Server のアクションおよびバッチ挿入が有効な MySQL アクションにおけるテンプレート化された `INSERT` 文の検証と安全なレンダリングを改善しました。

  以前は、手動で入力されたテンプレートの構文エラーや補間の問題により、不正な SQL が生成されることがありました。

  現在は、アクション作成時に EMQX が SQL 文を完全に解析し、不正な SQL は拒否されます。レンダリング時には適切なエスケープが強制されます。安定した予測可能な動作を提供するため、テンプレートで使用できる SQL 機能を制限しています。特に、SQL コメントはサポートされません。サポートされる構文は定数値、文字列と文字列補間、算術演算、関数、条件式および条件演算子です。

  MySQL は `ON DUPLICATE KEY UPDATE` を、ClickHouse は `FORMAT Values` と `FORMAT JSONCompactEachRow` を、TDengine は `INSERT ... USING ... TAGS` とテーブル識別子の補間をサポートします。

  MySQL テンプレートの一貫したレンダリングを提供するため、MySQL ブリッジはすべての接続で `ANSI_QUOTES` と `NO_BACKSLASH_ESCAPES` モードを無条件に無効化し、それに応じて文を扱います。

  ClickHouse ブリッジは SQL テンプレートからバッチ値の区切り文字を推測し、設定された `batch_value_separator` の値は無視されます。

- [#18630](https://github.com/emqx/emqx/pull/18630) ネームスペース付き管理者 API キーは、ネームスペース付きロールが保持できないスコープ（例：`gateways` や `audit`）で作成、更新、ブートストラップできなくなりました。これは既存のダッシュボードユーザールールに合わせた変更です。

  既にそのようなスコープが付与されているネームスペース付き API キーは、ローテーションされるまで有効です。該当するキーはローテーションしてください。

- [#18824](https://github.com/emqx/emqx/pull/18824) `emqx ctl listeners` の出力におけるフィールド名の誤字を修正しました。

  リスナーの有効フラグが `enbale` と表示されていましたが、`enable` に修正されました。この出力を解析するスクリプトは修正が必要です。

## 6.3.0

- [#17185](https://github.com/emqx/emqx/pull/17185) MQTT パーサーがデフォルトで厳格モードで動作するようになりました。以前の寛容な動作に戻すには、`mqtt.strict_mode = false`（グローバルまたはゾーン単位）を設定してください。

  厳格モードでは、ブローカーは受信した MQTT パケットをプロトコル仕様に照らして検証し、不正なパケットを送信したクライアントを切断します。厳格モードでのみ適用される検証は以下の通りです。

  - **固定ヘッダーのフラグ**：非 PUBLISH パケットの予約された DUP/QoS/RETAIN ビットはゼロでなければならず、PUBREL/SUBSCRIBE/UNSUBSCRIBE は QoS=1 を使用する必要があります（`bad_frame_header`）。
  - **CONNECT の予約ビット**はゼロでなければなりません（`reserved_connect_flag`）。
  - **CONNECT の Will フラグ整合性**：Will Flag=0 の場合は Will QoS=0 かつ Will Retain=0、Will Flag=1 の場合は Will QoS が {0,1,2} のいずれかである必要があります（`invalid_will_qos`、`invalid_will_retain`）。
  - **CONNECT のパスワード/ユーザ名フラグ（MQTT 3.1.1 のみ）**：Username Flag=0 の場合は Password Flag も 0 でなければなりません（`invalid_password_flag`）。MQTT 5.0 ではこの制約は解除されています。
  - **UTF-8 文字列**（プロトコル名、クライアント ID、トピック、ユーザ名、パスワード、Will トピック、MQTT 5 の文字列プロパティ）は有効な UTF-8 であり、制御文字 U+0000–U+001F および U+007F–U+009F を含んではいけません（`utf8_string_invalid`）。
  - **パケット識別子**は必要な場合にゼロであってはいけません（PUBLISH QoS>0、PUBACK/REC/REL/COMP、SUBSCRIBE/SUBACK、UNSUBSCRIBE/UNSUBACK）（`bad_packet_id`）。

  これらの検証に違反したクライアントが接続した場合、ブローカーは `info` レベルで `msg=frame_parse_error` と構造化された `reason`（例：`cause=invalid_password_flag`、`proto_ver`、`received_prefix`）をログに記録します。MQTT 5.0 接続では、切断前に CONNACK/DISCONNECT で理由コード `0x81 Malformed Packet` を返します。MQTT 3.1/3.1.1 では理由コードなしで静かに切断されます。

- [#17215](https://github.com/emqx/emqx/pull/17215) EMQX リリースパッケージから Swagger UI のアセットを削除し、tarball サイズを約 11 MB 削減しました。

  `/api-docs/swagger.json` は引き続き完全な OpenAPI 3 JSON 仕様を提供するため、外部の Swagger UI で URL から読み込む構成は動作し続けます。従来の `/api-docs` URL は HTTP 308 リダイレクトで 6.3.0 で導入されたインツリーの仕様エクスプローラー `/api-spec.html` に転送されます。その他の `/api-docs/*` サブパス（埋め込み Swagger UI アセット）は提供されず 404 を返します。

- [#17267](https://github.com/emqx/emqx/pull/17267) `node.max_ports` のデフォルト値が `auto` に変更されました。これは Erlang VM のポート制限（`+Q`）を論理 CPU コア数に応じてスケールさせます。8 コアまではコアあたり 65536 ポート、それ以上は従来の固定値 1048576 ポートとなります。明示的な整数値も引き続き受け付けます。

  以前のバージョンからアップグレードしたノードでは、`max_ports` のデフォルトが固定の 1048576 だったため、8 コア以下のホストは起動時に小さいポートテーブルになります。`cores * 65536` を超える接続を受け入れるセットアップは、アップグレード前に `node.max_ports` を明示的に設定し（ノード再起動も必要）、対応してください。

  非公開の `node.process_limit` 設定はオーバーライドとして復活しました。`max_ports` の 2 倍より大きい値に設定すると尊重され、小さい値は無視されます。これによりプロセステーブルがポートテーブルより小さくなることはありません。

  新しい `node.schedulers` 設定（デフォルト `auto`）は Erlang スケジューラー数（`+S`）を制御します。`auto` では VM が実際に利用可能な論理プロセッサ数（Linux の `sched_getaffinity`）を上限とし、`--cpuset-cpus` や Kubernetes の CPU リクエストで制限されたコンテナが並列実行できないスケジューラー OS スレッドを生成しなくなります。正の整数を設定すると自動検出値を上書きします。

- [#17437](https://github.com/emqx/emqx/pull/17437) Prometheus スクレイプエンドポイント（`/api/v5/prometheus/*`）はデフォルトで認証が必須になりました。以前の認証なし動作に戻すには `prometheus.enable_basic_auth = false` を明示的に設定してください。認証なしでスクレイプしている環境は、スクレイパーに認証情報を設定するか、設定項目を変更する必要があります。推奨構成は `monitoring` スコープを持つ専用 API キーを作成し、スクレイパーで Bearer 認証を利用する方法です。

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus の VM および Mnesia コレクターのメトリクス名を `prometheus.erl` 6.x の promtool 準拠名に変更しました。

  変更されたメトリクス名：

  - `erlang_mnesia_failed_transactions` -> `erlang_mnesia_failed_transactions_total`
  - `erlang_mnesia_committed_transactions` -> `erlang_mnesia_committed_transactions_total`
  - `erlang_mnesia_logged_transactions` -> `erlang_mnesia_logged_transactions_total`
  - `erlang_mnesia_restarted_transactions` -> `erlang_mnesia_restarted_transactions_total`
  - `erlang_vm_memory_atom_bytes_total` -> `erlang_vm_memory_atom_bytes`
  - `erlang_vm_memory_bytes_total` -> `erlang_vm_memory_bytes`
  - `erlang_vm_memory_processes_bytes_total` -> `erlang_vm_memory_processes_bytes`
  - `erlang_vm_memory_system_bytes_total` -> `erlang_vm_memory_system_bytes`
  - `erlang_vm_statistics_context_switches` -> `erlang_vm_statistics_context_switches_total`
  - `erlang_vm_statistics_garbage_collection_number_of_gcs` -> `erlang_vm_statistics_garbage_collection_number_of_gcs_total`
  - `erlang_vm_statistics_garbage_collection_words_reclaimed` -> `erlang_vm_statistics_garbage_collection_words_reclaimed_total`
  - `erlang_vm_statistics_garbage_collection_bytes_reclaimed` -> `erlang_vm_statistics_garbage_collection_bytes_reclaimed_total`
  - `erlang_vm_statistics_runtime_milliseconds` -> `erlang_vm_statistics_runtime_seconds_total`
  - `erlang_vm_statistics_wallclock_time_milliseconds` -> `erlang_vm_statistics_wallclock_time_seconds_total`
  - `erlang_vm_port_count` -> `erlang_vm_ports`
  - `erlang_vm_process_count` -> `erlang_vm_processes`
  - `erlang_vm_atom_count` -> `erlang_vm_atoms`

- [#17596](https://github.com/emqx/emqx/pull/17596) 認可ルールのトピックフィルターテンプレートにおける `/`、`+`、`#` 記号の補間を禁止する認可オプションを追加しました。新しい設定は以下の通りです。

  ```hocon
  authorization.topic_template_allow {
    plus = false,
    hash = false,
    slash = false
  }
  ```

  `false` に設定すると、対応する記号をトピックテンプレートに補間された値に使用できなくなります。例えば、`plus = false` の場合、ユーザ名 `bad+user` は `{allow, all, publish, ["userspace/${username}"]}` のようなルールで禁止されます。結果は有効なセキュリティプロファイルによって異なり、レガシープロファイルではルールがマッチせず、ハードニングプロファイルではアクションが拒否されます。

- [#17677](https://github.com/emqx/emqx/pull/17677) Prometheus REST API の JSON 出力フォーマットのサポートを廃止しました。

  `/api/v5/prometheus` 以下のエンドポイント（`stats`、`auth`、`data_integration`、`schema_validation`、`message_transformation`）は Prometheus テキストフォーマットのみを返します。`Accept: application/json` を送信したリクエストは `400 Bad Request`（"only prometheus format is supported"）で拒否されます。以前はメトリクスの JSON 表現を返していました。

- [#17626](https://github.com/emqx/emqx/pull/17626) [#18123](https://github.com/emqx/emqx/pull/18123) 新しい設定 `multi_tenancy.deny_namespaces` を追加しました。ここに指定されたネームスペース名は、管理者ネームスペース（ダッシュボードロール、API キー、多重テナント管理 API）やクライアントの `client_attrs.tns` として使用できません。`client_attrs.tns` が拒否された名前に解決されるクライアントは拒否されます。

  これは破壊的変更です。デフォルト値 `["global", "undefined", "null", "none"]` は以前は許容されていた名前を拒否します。これらの名前は内部のセントネルと衝突し、ログやダッシュボードの出力が曖昧になるためです。既存の該当ネームスペースは移行されません。アップグレード前に名前を変更するか、制限を解除するには `multi_tenancy.deny_namespaces` を空リストに設定してください。

  さらに、`multi_tenancy.post_auth_tns_expression` が設定されていて空または評価失敗した場合、事前認証の `client_attrs.tns` が拒否された名前であればクライアントは拒否されます。これは式が非空の値を返した場合の処理と一貫しています。

- [#18228](https://github.com/emqx/emqx/pull/18228) デフォルトの認可ルールファイル（`acl.conf`）は、`127.0.0.1` から接続するクライアントに対してすべてのトピック（`$SYS/#` や `#` を含む）へのパブリッシュ／サブスクライブを無条件に許可しなくなりました。

  ローカルホストからのクライアントは他のクライアントと同様にルールで認可され、最終的には `authorization.no_match` 設定に従います。特に、デフォルトルールでは `$SYS/#`、ワイルドカードフィルター `#` および `+/#` へのサブスクライブはローカルホストクライアントに対して拒否されます。セキュリティプロファイルに関係なく適用されます。

  組み込みのローカルホスト許可に依存していた場合は、`acl.conf` に明示的なルールを追加してください。以前のルールはコメントとしてファイルに残されています。

  ```erlang
  %% {allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
  ```

  これは新規インストールおよびカスタマイズしていない `acl.conf` に適用されます。既存のカスタマイズ済みファイルはアップグレードで変更されません。

- [#18244](https://github.com/emqx/emqx/pull/18244) ExProto ゲートウェイを削除しました。

- [#18271](https://github.com/emqx/emqx/pull/18271) [#18329](https://github.com/emqx/emqx/pull/18329) MQTT およびゲートウェイ WebSocket リスナーは、デフォルトで転送ヘッダーからクライアントのアドレスとポートを読み取らなくなりました。`proxy_address_header` と `proxy_port_header` のデフォルト値が `x-forwarded-for` / `x-forwarded-port` から空文字に変更され、常にソケットの送信元アドレスとポートが使用されます。ロードバランサーやリバースプロキシの背後で転送ヘッダーに依存している場合は、ヘッダー名を明示的に設定してください（例：`proxy_address_header` を `x-forwarded-for` に設定）。空文字を設定すると転送ヘッダーの参照が無効になります。

  この変更はゲートウェイ WebSocket リスナーの転送ヘッダー参照の不具合も修正します。以前は設定されたヘッダー名がリクエストヘッダーと照合されず、転送ヘッダーが存在してもソケット送信元アドレスとポートが使用されていました。

- [#18377](https://github.com/emqx/emqx/pull/18377) 管理対象ネームスペース名の作成時にバリデーションを行うようになりました。名前は ASCII の英数字と `.`, `-`, `_` のみを含み、長さは 1～255 バイトでなければなりません。`.` と `..` は許可されません。既存のネームスペースには影響しません。

- [#18390](https://github.com/emqx/emqx/pull/18390) `mqtt.clientid_override` 式は失敗した場合にクライアントが送信した Client ID にフォールバックしなくなりました。

  `mqtt.clientid_override` が設定されている場合、式がエラーを発生させる（例えばクライアントが提供していない属性を参照する）か空文字列を返すと、EMQX は CONNACK 理由コード 0x85（Client Identifier not valid、MQTT 3.1/3.1.1 はリターンコード 2）で接続を拒否します。以前はこのようなクライアントは元の Client ID で接続され、オーバーライドは無効のままでした。

  アップグレード前に、接続するすべてのクライアントが設定された式を空でない文字列にレンダリングできることを確認してください。アップグレード前は式をレンダリングできなかったクライアントは元の Client ID で接続されていましたが、アップグレード後は式またはクライアントデータを修正するまで接続が拒否されます。

- [#18419](https://github.com/emqx/emqx/pull/18419) Google Cloud IoT Core の移行互換機能（GCP デバイス認証およびデバイス管理 API）を削除しました。

- [#18515](https://github.com/emqx/emqx/pull/18515) Azure Blob Storage アクションの `blob` テンプレートフィールドを、Aggregated S3 アクションの `key` フィールドと同じスキーマ検証に更新しました。サポートされていないテンプレートバインディングは拒否されます。

- [#18528](https://github.com/emqx/emqx/pull/18528) OpenTelemetry 統合のエクスポーターエンドポイントが明示的なスキームとポートを持つ URL であることを要求する検証を追加しました。サポートされるスキームは `http` と `https` です。

- [#18627](https://github.com/emqx/emqx/pull/18627) ダッシュボードの SAML SSO はすべてのセキュリティプロファイルでデフォルトで IdP 署名を検証するようになりました。

  以前はセキュリティプロファイルに依存しており、ハードニングプロファイルは署名を検証しましたが、レガシープロファイル（v7.0 までのデフォルト）は検証せず、署名されていない偽造の SAMLResponse を受け入れてダッシュボードセッションを発行していました。

  署名されていない IdP を意図的に使用する場合は、`sso.saml.idp_signs_envelopes = false` と `sso.saml.idp_signs_assertions = false` を明示的に設定してください。IdP が署名するがメタデータに証明書が含まれていない場合、SAML バックエンドは `missing_idp_certificate` エラーで起動に失敗します。
