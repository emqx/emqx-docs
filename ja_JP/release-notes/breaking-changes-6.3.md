# EMQX 6.3 の互換性のない変更点

## 6.3.1

- [#18465](https://github.com/emqx/emqx/pull/18465) ClickHouse、TDengine、SQL Server アクションのテンプレート化された `INSERT` ステートメントおよびバッチ挿入が有効な MySQL アクションにおける検証と安全なレンダリングを改善しました。

  以前は、手動で入力されたテンプレート自体の構文エラーや補間の問題により、SQL テンプレートのレンダリングが不正な SQL を生成することが頻繁にありました。

  現在、EMQX はアクション作成時に SQL ステートメントを完全に解析し、不正な SQL を拒否します。レンダリング時には正しいエスケープを強制します。安定かつ予測可能な動作を提供するために、EMQX はテンプレートで使用できる SQL 機能を制限しています。特に、SQL コメントはサポートされていません。サポートされる構文は定数値、文字列と文字列補間、算術、関数、条件式、および条件演算子です。

  MySQL は `ON DUPLICATE KEY UPDATE` をサポートし、ClickHouse は `FORMAT Values` と `FORMAT JSONCompactEachRow` をサポートし、TDengine は `INSERT ... USING ... TAGS` とテーブル識別子の補間をサポートします。

  MySQL テンプレートの一貫したレンダリングを提供するために、MySQL ブリッジはすべての接続で `ANSI_QUOTES` と `NO_BACKSLASH_ESCAPES` モードを無条件に無効化し、それに応じてステートメントを処理します。

  ClickHouse ブリッジは SQL テンプレートからバッチ値の区切り文字を推測し、設定された `batch_value_separator` の値を無視するようになりました。

- [#18630](https://github.com/emqx/emqx/pull/18630) ネームスペース付き管理者 API キーは、ネームスペース付きロールが保持できないスコープ（例：`gateways` や `audit`）で作成、更新、またはブートストラップできなくなりました。これは既存のダッシュボードユーザールールと一致します。

  既にそのようなスコープが付与されたネームスペース付き API キーは、ローテーションされるまで有効なため、速やかにローテーションしてください。

- [#18824](https://github.com/emqx/emqx/pull/18824) `emqx ctl listeners` の出力におけるフィールド名の誤字を修正しました。

  コマンドはリスナーの有効フラグを `enbale` と表示していましたが、現在は `enable` と表示します。この出力を解析するスクリプトは修正が必要です。

## 6.3.0

- [#17185](https://github.com/emqx/emqx/pull/17185) MQTT パーサーがデフォルトで厳格モードで動作するようになりました。以前の寛容な動作を復元するには、`mqtt.strict_mode = false`（グローバルまたはゾーン単位）を設定してください。

  厳格モードでは、ブローカーは受信した MQTT パケットをプロトコル仕様に照らして検証し、不正なパケットを送信したクライアントを切断します。厳格モードでのみ適用される検証は以下の通りです：

  - **固定ヘッダーフラグ**：非 PUBLISH パケットでは予約された DUP/QoS/RETAIN ビットはゼロでなければならず、PUBREL/SUBSCRIBE/UNSUBSCRIBE は QoS=1 を使用する必要があります（`bad_frame_header`）。
  - **CONNECT の予約ビット** はゼロでなければなりません（`reserved_connect_flag`）。
  - **CONNECT の Will フラグ整合性**：Will Flag=0 の場合は Will QoS=0 かつ Will Retain=0、Will Flag=1 の場合は Will QoS が {0,1,2} のいずれかである必要があります（`invalid_will_qos`, `invalid_will_retain`）。
  - **CONNECT のパスワード/ユーザ名フラグ（MQTT 3.1.1 のみ）**：Username Flag=0 の場合は Password Flag も 0 でなければなりません（`invalid_password_flag`）。MQTT 5.0 ではこの制約は解除されています。
  - **UTF-8 文字列**（プロトコル名、クライアント ID、トピック、ユーザ名、パスワード、Will トピック、MQTT 5 の文字列プロパティ）は有効な UTF-8 であり、制御文字 U+0000–U+001F および U+007F–U+009F を含んではいけません（`utf8_string_invalid`）。
  - **パケット識別子** は必要な箇所でゼロ以外でなければなりません（PUBLISH QoS>0、PUBACK/REC/REL/COMP、SUBSCRIBE/SUBACK、UNSUBSCRIBE/UNSUBACK）（`bad_packet_id`）。

  これらの検証に違反した場合、ブローカーは `msg=frame_parse_error` の `info` レベルログを出力し、トラブルシューティング用に構造化された `reason`（例：`cause=invalid_password_flag`、`proto_ver`、`received_prefix`）を付与します。MQTT 5.0 接続では、切断前に CONNACK/DISCONNECT で理由コード `0x81 Malformed Packet` を返します。MQTT 3.1/3.1.1 では理由コードなしで静かに切断されます（これらのバージョンには不正パケット用の CONNACK 理由コードが定義されていません）。

- [#17215](https://github.com/emqx/emqx/pull/17215) EMQX リリースパッケージからバンドルされた Swagger UI アセットを削除し、tarball サイズを約 11 MB 削減しました。

  `/api-docs/swagger.json` は引き続き完全な OpenAPI 3 JSON 仕様を提供するため、URL で読み込む外部 Swagger UI は動作を維持します。レガシーの `/api-docs` URL は HTTP 308 リダイレクトで 6.3.0 で導入されたインツリーの仕様エクスプローラー `/api-spec.html` に転送します。その他の `/api-docs/*` サブパス（埋め込み Swagger UI アセット）は提供されず 404 を返します。

- [#17267](https://github.com/emqx/emqx/pull/17267) `node.max_ports` 設定のデフォルトが `auto` に変更されました。これは Erlang VM のポート制限（`+Q`）を論理 CPU コア数に応じてスケールさせます：8 コアまではコアあたり 65536 ポート、それ以上は従来の固定値 1048576 ポートです。明示的な整数値は引き続き受け付けます。

  これは以前のバージョンからアップグレードしたノードに対する動作変更です。8 コア以下のホストはより小さいポートテーブルで起動します。`cores * 65536` を超える接続を受け入れるセットアップは、アップグレード前に `node.max_ports` を明示的に設定し（ノード再起動が必要）、対応してください。

  非公開の `node.process_limit` 設定はオーバーライドとして復活しました：派生制限（`2 * max_ports`）より大きい値を設定すると尊重され、小さい値は無視されてプロセステーブルがポートテーブルより小さくなることはありません。

  新しい `node.schedulers` 設定（デフォルト `auto`）は Erlang スケジューラ数（`+S`）を制御します。`auto` では VM が実際に利用可能な論理プロセッサ数（Linux の `sched_getaffinity`）で上限を設定し、`--cpuset-cpus` や Kubernetes の CPU リクエストで制限されたコンテナが並列実行できないスケジューラ OS スレッドを生成しなくなります。正の整数を設定すると自動検出値を上書きします。

- [#17437](https://github.com/emqx/emqx/pull/17437) Prometheus スクレイプエンドポイント（`/api/v5/prometheus/*`）はデフォルトで認証が必要になりました。以前の認証なしの動作を復元するには `prometheus.enable_basic_auth = false` を明示的に設定してください。認証情報なしでこれらのエンドポイントをスクレイプするデプロイメントは、スクレイパー側で認証情報を設定するか、設定フィールドを変更する必要があります。推奨される設定は、`monitoring` スコープを持つ専用 API キーを作成し、スクレイパーで Bearer 認証を使用することです。

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus VM および Mnesia コレクタのメトリック名が `prometheus.erl` 6.x の promtool 準拠名に変更されました。

  変更されたメトリック名：

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

- [#17596](https://github.com/emqx/emqx/pull/17596) 認可ルールにおけるトピックフィルターテンプレートへの `/`、`+`、`#` シンボルの補間を禁止する認可オプションを追加しました。新しいオプションは以下の通りです：

  ```hocon
  authorization.topic_template_allow {
    plus = false,
    hash = false,
    slash = false
  }
  ```

  `false` に設定すると、対応するシンボルをトピックテンプレートに補間された値に使用できなくなります。例えば、`plus = false` の場合、ユーザ名 `bad+user` は `{allow, all, publish, ["userspace/${username}"]}` のようなルールで禁止されます。結果はアクティブなセキュリティプロファイルによって異なり、レガシープロファイルではルールがマッチせず、ハードニングプロファイルではアクションが拒否されます。

- [#17677](https://github.com/emqx/emqx/pull/17677) Prometheus REST API の JSON 出力フォーマットのサポートを廃止しました。

  `/api/v5/prometheus` 以下のエンドポイント（`stats`、`auth`、`data_integration`、`schema_validation`、`message_transformation`）は Prometheus テキストフォーマットのみを出力します。`Accept: application/json` を送信したリクエストは `400 Bad Request`（「only prometheus format is supported」）で拒否されます。以前はメトリックの JSON 表現を返していました。

- [#17626](https://github.com/emqx/emqx/pull/17626) [#18123](https://github.com/emqx/emqx/pull/18123) 新しい設定 `multi_tenancy.deny_namespaces` を追加しました。これは管理者ネームスペース（ダッシュボードロール、API キー、多重テナント管理 API）やクライアント単位の `client_attrs.tns` として使用できないネームスペース名を保持します。`client_attrs.tns` が拒否された名前に解決されるクライアントは拒否されます。

  これは破壊的変更です。デフォルト値 `["global", "undefined", "null", "none"]` は以前は許可されていた名前を拒否します。これらの名前は内部のセントネルと衝突し、ログやダッシュボード出力で曖昧さを生じます。既存のこれらの名前のネームスペースは移行されません。アップグレード前に名前を変更するか、制限を解除するには `multi_tenancy.deny_namespaces` を空リストに設定してください。

  さらに、`multi_tenancy.post_auth_tns_expression` が設定され、空値または評価失敗した場合、事前認証の `client_attrs.tns` が拒否された名前の場合も拒否されるようになり、式が非空値の場合の処理と一貫性が保たれます。

- [#18228](https://github.com/emqx/emqx/pull/18228) デフォルトの認可ルールファイル（`acl.conf`）は、`127.0.0.1` から接続するクライアントに対してすべてのトピック（`$SYS/#` や `#` を含む）への無条件のパブリッシュ／サブスクライブアクセスを付与しなくなりました。

  localhost からのクライアントは他のクライアントと同じルールで認可され、最終的には `authorization.no_match` 設定によって制御されます。特に、デフォルトルールでは localhost クライアントの `$SYS/#` およびワイルドカードフィルター `#` と `+/#` へのサブスクライブは拒否されます。セキュリティプロファイルに関わらず適用されます。

  組み込みの localhost 許可に依存していたデプロイメントは、`acl.conf` に明示的なルールを追加する必要があります。以前のルールはコメントとしてファイルに残されており、再有効化が容易です：

  ```erlang
  %% {allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
  ```

  注意：これは新規インストールおよびカスタマイズしていない `acl.conf` に適用されます。既存のカスタマイズ済み `acl.conf` はアップグレードで変更されません。

- [#18244](https://github.com/emqx/emqx/pull/18244) ExProto ゲートウェイを削除しました。

- [#18271](https://github.com/emqx/emqx/pull/18271) [#18329](https://github.com/emqx/emqx/pull/18329) MQTT およびゲートウェイの WebSocket リスナーは、デフォルトで転送ヘッダーからクライアントのアドレスとポートを読み取らなくなりました。`proxy_address_header` と `proxy_port_header` のデフォルト値は `x-forwarded-for` / `x-forwarded-port` から空文字に変更され、常にソケットの送信元アドレスとポートを使用します。ロードバランサーやリバースプロキシの背後にあるデプロイメントで転送ヘッダーに依存している場合は、ヘッダー名を明示的に設定してください（例：`proxy_address_header` を `x-forwarded-for` に設定）。空文字のヘッダー名を設定すると転送ヘッダーの参照が無効になります。

  この変更はゲートウェイ WebSocket リスナーの転送ヘッダー参照の不具合も修正しています。以前は設定されたヘッダー名がリクエストヘッダーと一致しなかったため、転送ヘッダーが存在してもソケットの送信元アドレスとポートが使用されていました。

- [#18377](https://github.com/emqx/emqx/pull/18377) 管理対象ネームスペース名は作成時に検証されるようになりました。名前は ASCII の英数字と `.`, `-`, `_` のみを含み、長さは 1～255 バイトでなければなりません。`.` と `..` は許可されません。既存のネームスペースには影響しません。

- [#18390](https://github.com/emqx/emqx/pull/18390) `mqtt.clientid_override` 式は失敗した場合にクライアントから提供された Client ID にフォールバックしなくなりました。

  `mqtt.clientid_override` が設定され、式がエラーを発生させる（例：クライアントが提供していない属性を参照する）か空文字列を返す場合、EMQX は CONNACK 理由コード 0x85（Client Identifier not valid、MQTT 3.1/3.1.1 クライアントはリターンコード 2）で接続を拒否します。以前はそのようなクライアントは元の Client ID で接続を維持し、オーバーライドは黙って適用されませんでした。

  アップグレード前に、接続するすべてのクライアントが設定された式を空でない文字列にレンダリングできることを確認してください。アップグレード前は式をレンダリングできなかったクライアントは元の Client ID で接続していましたが、アップグレード後は式またはクライアントデータが修正されるまで拒否されます。

- [#18419](https://github.com/emqx/emqx/pull/18419) Google Cloud IoT Core の移行互換機能（GCP デバイス認証機能およびデバイス管理 API）を削除しました。

- [#18515](https://github.com/emqx/emqx/pull/18515) Azure Blob Storage アクションの `blob` テンプレートフィールドを、Aggregated S3 アクションの `key` フィールドと同じスキーマ検証に更新しました。サポートされていないテンプレートバインディングは拒否されます。

- [#18528](https://github.com/emqx/emqx/pull/18528) OpenTelemetry 統合のエクスポーターエンドポイントが、明示的なスキームとポートを持つ URL であることを要求する検証を追加しました。サポートされるスキームは `http` と `https` です。

- [#18627](https://github.com/emqx/emqx/pull/18627) ダッシュボードの SAML SSO はすべてのセキュリティプロファイルでデフォルトで IdP 署名を検証するようになりました。

  以前はセキュリティプロファイルに従っており、ハードニングプロファイルは署名を検証しましたが、レガシープロファイル（v7.0 までのデフォルト）は検証せず、署名されていない偽造の SAMLResponse を受け入れてダッシュボードセッションを発行していました。

  署名なしの IdP を意図的に運用する場合は、`sso.saml.idp_signs_envelopes = false` および `sso.saml.idp_signs_assertions = false` を明示的に設定してください。IdP が署名するがメタデータに証明書がない場合、SAML バックエンドは `missing_idp_certificate` エラーで起動に失敗します。
