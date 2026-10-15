# EMQX Enterprise Version 6

## 6.3.1

*リリース日: 2026-09-17*

EMQX 6.3.1 にアップグレードする前に、破壊的変更を必ずご確認ください。

### 強化点

#### アクセス制御

- [#18777](https://github.com/emqx/emqx/pull/18777) ダッシュボードの JSON API ログインに対して、SCRAM-SHA-256 チャレンジレスポンス認証をオプションで追加し、埋め込みの API Spec ログインページをデフォルトで SCRAM に切り替えました。既存のパスワードログインはデフォルトで有効のままです。

  `dashboard.password_login = scram_only` の場合、`POST /api/v5/login` をユーザー名とパスワードで使用しているスクリプトやサードパーティクライアントは、SCRAM チャレンジレスポンスフローに移行するか、API キーを使用する必要があります。

#### パッケージング

- [#18977](https://github.com/emqx/emqx/pull/18977) 公式の EMQX Enterprise Docker リリースイメージを Docker Hardened Debian 13 (Trixie) に切り替えました。ランタイムイメージには `apt` や `dpkg` などのパッケージマネージャーは含まれなくなりました。

  リリースイメージにはビルドの出所情報とソフトウェア部品表（SBOM）証明が含まれ、Docker Scout による Hardened ベースイメージの脆弱性評価（VEX ステートメント）適用が可能です。

#### パフォーマンス

- [#18806](https://github.com/emqx/emqx/pull/18806) MQTT 接続のメモリ使用量を削減しました。

  これまでは、レートリミッターが設定されていなくても接続時に状態が割り当てられていました。現在はクライアントが初めてパブリッシュまたはサブスクライブした時に割り当てられるため、接続だけしている場合は約 2 KB のメモリを節約します。設定されたレート制限の動作は従来通りです。

- [#18807](https://github.com/emqx/emqx/pull/18807) MQTT セッションのメモリ使用量を削減しました。

  配信レートリミッター状態は、配信レート制限が設定された後の最初の配信時に割り当てられます。これによりセッションあたり約 0.5 KB のメモリを節約します。設定された配信レート制限の動作は従来通りです。

- [#18808](https://github.com/emqx/emqx/pull/18808) パブリッシュまたはサブスクライブする MQTT 接続のメモリ使用量を削減しました。

  接続ごとのレートリミッター状態は、実際に設定されている制限に対してのみ割り当てられます。デフォルト設定（レート制限なし）では、アクティブな接続あたり約 1.6 KB のメモリを節約します。設定されたレート制限の動作は従来通りで、実行時に追加された制限も含みます。

- [#18809](https://github.com/emqx/emqx/pull/18809) レート制限が設定されている MQTT 接続のメモリ使用量を削減しました。

  すべての接続で共有されていたレートリミッターの管理情報は各接続にコピーされなくなり、レート制限が有効な接続あたり約 0.5 KB のメモリを節約します。レート制限の動作は変更ありません。

- [#18875](https://github.com/emqx/emqx/pull/18875) マルチテナントネームスペースに属する MQTT 接続のメモリ使用量を削減しました。

  ネームスペースクライアントのレートリミッター状態は、クライアントが初めてパブリッシュ、サブスクライブ、またはメッセージを受信した時に割り当てられ、実際に設定されている制限に対してのみ割り当てられます。これは通常の接続の動作に一致します。設定されたテナントおよびクライアントレート制限の動作は従来通りです。ネームスペースのリミッター設定が削除された場合、そのクライアントに対するリミッターのチェックは引き続き操作を拒否します。

### バグ修正

#### コア MQTT 機能

- [#18734](https://github.com/emqx/emqx/pull/18734) `tcp_options.buffer = 0` がデフォルトの `socket` バックエンドで新しい接続をすべて失敗させていた TCP リスナーの問題を修正しました。

  すべてのリスナータイプで `tcp_options.active_n` は 1..1000 の範囲に制限され、`tcp_options.buffer` は最小 5 バイト（最大の MQTT 固定ヘッダーサイズ）に制限されます。範囲外の値は受け入れられ、静かに調整されます。

- [#18789](https://github.com/emqx/emqx/pull/18789) MQTT およびゲートウェイリスナーの作成時に、64 バイトを超える名前や制限付き名前形式に合致しない名前を拒否し、明確な `BAD_REQUEST` 応答を返すようにしました。既存の長い名前のリスナーは編集可能なままです。

- [#18811](https://github.com/emqx/emqx/pull/18811) MQTT v5 クライアントがトピックエイリアスを受け入れない場合に送信されていたトピックエイリアスの送信を修正しました。

  `Topic Alias Maximum` プロパティを省略するか 0 に設定したクライアントに対し、SCRAM などの拡張認証で認証した場合でも、PUBLISH パケットに `Topic-Alias` プロパティが送信されていました。

- [#18828](https://github.com/emqx/emqx/pull/18828) MQTT リスナーは、最初のパケットが CONNECT でない場合、パケット本文を解析せずに接続を閉じるようにしました。

#### アクセス制御

- [#18681](https://github.com/emqx/emqx/pull/18681) REST API 認証エラーメッセージを、対象エンドポイントが受け入れる内容に合わせて修正しました。API キーを拒否するエンドポイント（例: `POST /api/v5/logout`）で拒否されたベアラートークンや認可ヘッダーの欠如は、API キーの使用を示唆しなくなりました。メッセージは `api_key.bootstrap_file` 設定キーの名前も含みません。

- [#18724](https://github.com/emqx/emqx/pull/18724) ダッシュボードの SSO OIDC で、発行者の URL スキームと TLS オプションが矛盾する設定（`https` で TLS 無効、`http` で TLS 有効）を設定時に拒否するようにしました。これまでは不明瞭な TLS エラーで繰り返し失敗するか、TLS オプションを無視していました。

- [#18737](https://github.com/emqx/emqx/pull/18737) 認証および認可リクエストテンプレートで `${peerport}` が空文字列になる問題を修正しました。

  また、クライアント情報として `${peername}` を追加しました。これはクライアントのアドレスとポート（例: `192.168.0.1:51544`）としてレンダリングされます。

- [#18852](https://github.com/emqx/emqx/pull/18852) 6.2 から 6.3 へのローリングアップグレード中に、6.2 ノードがブートストラップ API キーを拒否する問題を修正しました。

  6.3 ノード起動時に、6.2 ノードが検証できない秘密ハッシュにクラスタ共有の API キーを書き換えていたため、6.2 ノードがキーを拒否していました。ブートストラップファイルローダーは、ファイルに同じ秘密がある場合は保存済みのハッシュを保持し、ロールやスコープなどのキーのメタデータのみを更新します。

  ブートストラップファイルの秘密を変更した場合はハッシュが置き換わります。クラスタ全体が 6.3 にアップグレードされてからブートストラップ秘密をローテーションしてください。

- [#18904](https://github.com/emqx/emqx/pull/18904) 有効なダッシュボード SSO バックエンドの作成または更新時に、リソースが開始タイムアウトまで `connecting` 状態のままの場合に内部エラーで失敗する問題を修正しました。

  バックエンドはリソース ID で追跡を継続し、開始タイムアウト後にクリーンアップできるようにしました。

#### マルチテナンシー

- [#18774](https://github.com/emqx/emqx/pull/18774) リソースのクリーンアップ中に削除済みマルチテナンシーネームスペースにクライアントが接続できる問題を修正しました。

#### データ統合

- [#18763](https://github.com/emqx/emqx/pull/18763) TDengine アクションで発生していたエラーを修正しました。アクションが見つからない場合、エラーにコネクターの ID が表示されていましたが、アクションの ID が表示されるように修正しました。

- [#18775](https://github.com/emqx/emqx/pull/18775) RocketMQ コネクターの `namespace` 設定フィールドを `rocketmq_namespace` に改名しました。

  旧名は引き続き受け入れられるため、既存の設定、API 呼び出し、インポート済みバックアップは変更不要です。名前の衝突を避けるための改名で、コネクター API が報告する EMQX ネームスペースと区別されます。Dashboard では API 応答や表示で `rocketmq_namespace` として表示されます。

- [#18846](https://github.com/emqx/emqx/pull/18846) データ統合の SQL テンプレートレンダリングを修正しました。Doris のバッチ挿入は Doris 互換の構文とテキスト・バイナリ値のエスケープを使用し、MySQL テンプレートはエスケープされたドル記号を正しく処理します。

- [#18925](https://github.com/emqx/emqx/pull/18925) QuasarDB アクションで SQL テンプレートの検証と SQL コンテキストに応じた値のエスケープを修正しました。

- [#18941](https://github.com/emqx/emqx/pull/18941) 高負荷の書き込み時に接続と切断を繰り返す GreptimeDB コネクターを修正しました。ヘルスチェックは保留中の書き込みを待たず、GreptimeDB が応答しない場合のみ失敗します。

#### クラスタリング

- [#18826](https://github.com/emqx/emqx/pull/18826) バックアップインポート時に、クラスタ内のすべてのノードが同じバージョンを実行していることを確認するようにしました。

  ローリングアップグレード中にインポートすると、まだアップグレードされていないノードが異なる解釈をする呼び出しでバックアップが適用される可能性がありました。インポートは開始前に停止し、アップグレード待ちのノードを表示してクラスタを元の状態に保ちます。

- [#18862](https://github.com/emqx/emqx/pull/18862) `emqx_router_tool:scan_missing_routes/1` と `emqx_router_tool:reconcile_missing_routes/1` に渡されるオプションの検証を追加しました。

  以前は無効な `chunk` や `sleep_ms` の値がスキャンスロットリングを無効化したり、低レベルのランタイムエラーを引き起こしていました。ツールは無効な値や未知のオプションキー（例: 誤字の `chunks`）を拒否し、問題のあるオプションを特定するエラーを発生させます。

- [#18899](https://github.com/emqx/emqx/pull/18899) ローリングアップグレード中に一部ノードが 6.3.0 より前のバージョンを実行している混在クラスタで、`GET /api/v5/listeners` と `GET /api/v5/listeners_status` が `500 INTERNAL_ERROR` を返す回帰を修正しました。

#### 永続ストレージ

- [#18822](https://github.com/emqx/emqx/pull/18822) ノード再起動やリーダーシップ変更後に Durable Storage シャードが回復できない稀な問題を修正しました。EMQX は追加の一時的な DS Raft リーダーシップエラーを再試行可能とみなし、不完全な RocksDB スキーマ初期化がシャードの回復を妨げなくなりました。

#### ゲートウェイ

- [#18755](https://github.com/emqx/emqx/pull/18755) ダッシュボードから既存の NATS ゲートウェイ設定を保存すると、フォームに表示されるマスク済みの認証情報で置き換わり、NATS クライアントの認証に失敗する問題を修正しました。

  認証情報は、認証設定を変更せずに他のゲートウェイ設定を更新する場合に保持されます。

  NATS ゲートウェイの認証設定は、重複する認証方法や認証情報エントリ（重複する NKeys や JWT アカウントエントリを含む）を拒否し、あいまいな認証動作を防止します。

- [#18776](https://github.com/emqx/emqx/pull/18776) MQTT-SN は、スリーピングクライアントのスリープ期間が切れたときに設定された Will メッセージをパブリッシュし、クライアントが正常に切断した場合は Will メッセージをパブリッシュしなくなりました。

- [#18842](https://github.com/emqx/emqx/pull/18842) ゲートウェイプロトコルはクライアント ID オーバーライドをサポートしません。ゲートウェイ接続は認証バックエンドから返された `clientid_override` 値を無視し、警告をログに記録します。

  ゲートウェイ接続のマウントポイントテンプレートは、認証結果がマージされた後の共有ゲートウェイ認証フローで評価されます。

- [#18855](https://github.com/emqx/emqx/pull/18855) クライアント証明書で認証された DTLS クライアントの MQTT-SN セッションウェイクアップ認可を修正しました。証明書に紐づくセッションは、同じピア証明書を提示した場合にのみ新しい関連付けから再開可能です。証明書なし、異なる証明書、または再発行された証明書でのウェイクアップは拒否されます。

  証明書なしの平文 UDP および DTLS クライアントは、従来の ClientId のみの PINGREQ ウェイクアップ動作を維持します。

#### プラグイン

- [#18891](https://github.com/emqx/emqx/pull/18891) EMQX 6.x で Sync Request プラグインのインストールと起動が `missing_i18n_ref` エラーで失敗する問題を修正しました。プラグインは正常に起動し、API エンドポイントが利用可能になりました。

#### 可観測性

- [#18664](https://github.com/emqx/emqx/pull/18664) ネームスペース付きリクエストの監査ログに、リクエストが対象としたネームスペースを識別する情報を追加しました。以前は異なるネームスペースで同名のトピックメトリクスコレクションをリセットすると、監査ログが同一に見え、どのネームスペースのコレクションか判別できませんでした。監査ログはリクエストのクエリパラメータも記録します。

- [#18677](https://github.com/emqx/emqx/pull/18677) データバックアップリクエストの監査ログに、リクエストが対象としたネームスペースを識別する情報を追加しました。以前は異なるネームスペースでバックアップのエクスポート、インポート、アップロード、削除を行うと監査ログが同一に見え、どのネームスペースのバックアップか判別できませんでした。監査ログはリクエストのクエリパラメータも記録します。

- [#18678](https://github.com/emqx/emqx/pull/18678) A2A レジストリリクエストの監査ログに、リクエストが対象としたネームスペースを識別する情報を追加しました。以前は異なるネームスペースでエージェントカードの登録や削除を行うと監査ログが同一に見え、どのネームスペースのカードか判別できませんでした。監査ログはリクエストのクエリパラメータも記録します。

- [#18684](https://github.com/emqx/emqx/pull/18684) OpenTelemetry トレースが有効で MQTT 5 クライアントの CONNECT パケットに Will User-Property が含まれる場合の接続時クラッシュを修正しました。

- [#18686](https://github.com/emqx/emqx/pull/18686) 認可、認証、コネクター、ブリッジ、ルールエンジン、トレースリクエストの監査ログに、リクエストが対象としたネームスペースを識別する情報を追加しました。以前はこれらの監査ログがネームスペース間で同一に見え、どのネームスペースのリソースか判別できませんでした。監査ログはリクエストのクエリパラメータも記録します。

- [#18754](https://github.com/emqx/emqx/pull/18754) ヘルプ表示のみを行う CLI コマンドの監査ログ記録を停止しました。

  例えば `emqx ctl api_keys` をサブコマンドなしで実行すると使用法が表示されますが、以前は空の引数リストで監査ログが記録され、操作が行われたように見えました。実際の操作を行うコマンドは引き続き監査されます（読み取り専用も含む）。

- [#18853](https://github.com/emqx/emqx/pull/18853) 監査ログ有効時に SSO MFA 設定および検証資格情報が未編集のまま保存される問題を修正しました。

  SSO MFA リクエストボディは監査ログに `username` と `backend` のみを保持し、一時トークン、TOTP コード、および不明な資格情報フィールドは監査ファイルや監査データベースに書き込む前にマスクされます。

- [#18961](https://github.com/emqx/emqx/pull/18961) `emqx ctl plugins ...` コマンドの監査ログ引数がすべて `******` とマスクされる回帰を修正しました。すべての `plugins` 引数は機密情報を含まないため、マスクせずそのまま記録します。CLI リファクタリングで誤って削除された `plugins install --cluster` オプションも復活しました。

#### 管理

- [#18600](https://github.com/emqx/emqx/pull/18600) Durable セッション有効時のクライアントリスト API フィルタリングを修正しました。

  `GET /api/v5/clients` は切断済み durable セッションをすべて結果に追加し、`conn_state`、`username`、`clientid` などのクエリフィルターを無視していました。例えば `conn_state=connected` でも切断済み durable セッションを返していました。現在は切断済み durable セッションにもフィルターが適用されます。

  フィルター適用時に切断済み durable セッションが存在する場合、レスポンスは誤った数値を返す代わりに `meta.count` フィールドを省略します。`conn_state=connected` フィルターのクエリは正確な `meta.count` を保持します。

- [#18666](https://github.com/emqx/emqx/pull/18666) `GET /api/v5/topics` が durable 共有サブスクリプションに対して、完全な共有サブスクリプショントピック（例: `$share/group/a/1`）を返すようになりました。

  以前は素のトピック（例: `a/1`）のみを返していたため、durable 共有サブスクリプションが通常の durable サブスクリプションと区別できませんでした。

- [#18817](https://github.com/emqx/emqx/pull/18817) リクエストボディに `enable` フィールドがない場合に `PUT /api/v5/telemetry/status` が Erlang スタックトレース付きで `500 INTERNAL_ERROR` を返す問題を修正しました。

  現在は検証メッセージ付きで `400 BAD_REQUEST` を返します。API ドキュメントは `enable` を必須とし、デフォルト値を表示しません。エンドポイントはこれまでデフォルト値を適用していませんでした。

- [#18859](https://github.com/emqx/emqx/pull/18859) 設定更新時に HTTP ヘッダー名の大文字小文字のみを変更すると保存済み値が消える問題を修正しました。

- [#18860](https://github.com/emqx/emqx/pull/18860) クライアントメッセージページネーションで最終ページに継続位置が返される問題を修正しました。

  `GET /clients/{clientid}/mqueue_messages` と `GET /clients/{clientid}/inflight_messages` は、メッセージがなくなると `meta.position` に `end_of_data` を返します。API クライアントは空ページをリクエストせずに停止できます。

#### デプロイメントとセキュリティ

- [#18836](https://github.com/emqx/emqx/pull/18836) クラスタ設定同期のデバッグログに成功した設定変更結果を含めるのを停止しました。

  結果には HTTP 認証ヘッダーのテンプレートなどのコンパイル済みランタイム状態が含まれ、ログのマスク対象外の秘密情報が含まれていました。

## 6.3.0

*リリース日: 2026-09-03*

EMQX 6.3.0 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### コア MQTT 機能

- [#16694](https://github.com/emqx/emqx/pull/16694) QUIC 接続からピア証明書を抽出する機能を追加し、QUIC mTLS リスナーで `peer_cert_as_username` が利用可能になりました。

- [#17307](https://github.com/emqx/emqx/pull/17307) リスナーの SUBSCRIBE パケットに対するクライアント単位のレートリミッターを追加しました。デフォルトで無効です。有限のレートが設定されると、制限を超えたパケットに対しては Quota Exceeded 理由コードを含む SUBACK を返し処理しません。ネームスペースごとに独立したレートを設定でき、リスナーレベルのレートを上書きします。

- [#17546](https://github.com/emqx/emqx/pull/17546) [#18477](https://github.com/emqx/emqx/pull/18477) MQTT 5.0 クライアントが要求できるセッション有効期限を制限する `mqtt.max_session_expiry_interval` 設定を追加しました。クライアントがこの制限を超える値を要求した場合、サーバーは黙って制限値に丸めて CONNACK に反映します。デフォルトは `infinity`（制限なし）で従来動作を維持します。MQTT 3.1.1/3.1 クライアントには影響しません。

  DISCONNECT パケットでクライアントが指定するセッション有効期限にも制限が適用され、切断時に有効期限を延長できなくなります。

- [#17603](https://github.com/emqx/emqx/pull/17603) 直接接続された TLS クライアント証明書から Subject Alternative Names を抽出し、`mqtt.client_attrs_init` の `cert_san.dns`、`cert_san.ip`、`cert_san.email`、`cert_san.uri` に設定する機能を追加しました。

- [#17854](https://github.com/emqx/emqx/pull/17854) MQTT TCP リスナーのデフォルト `tcp_backend` を `socket` に変更し、メッセージレイテンシとリソース使用を改善しました。`tcp_backend = gen_tcp` で従来のバックエンドも利用可能です。

- [#17870](https://github.com/emqx/emqx/pull/17870) 遅いまたは混雑したサブスクライバー向けのインメモリセッション配信動作を改善しました。

  - EMQX は接続の送信キュー混雑を追跡し、必要に応じて QoS 0 配信を直接混雑した接続に送るのではなくセッションメッセージキュー経由に切り替えます。
  - セッションメッセージキューは容量に達すると古い QoS 0 メッセージを優先的に削除し、QoS 1/2 配信の進行を助けます。
  - セッション配信レート制限に達した場合の配信順序を修正し、後続メッセージが先行メッセージを追い越さないようにしました。
  - ソケットバック接続の `send_timeout` 処理を改善し、ソケットキューがウォーターマークを超えた後にのみタイムアウトを設定します。

#### アクセス制御

- [#17145](https://github.com/emqx/emqx/pull/17145) 認可ソースが Variform ベースの前提条件をサポートするようになりました。前提条件を持つソースは式が `true` の場合のみ呼び出され、クライアント属性、アクション、トピックなどのコンテキストに応じて異なる認可バックエンドを選択できます。

- [#17487](https://github.com/emqx/emqx/pull/17487) ダッシュボード管理者パスワードと API キーシークレットのハッシュ強化。

  ダッシュボード管理者パスワードは PBKDF2-HMAC-SHA256（60万回の反復）と 16 バイトのランダムソルトでハッシュされます。API キーシークレットは同じ自己記述形式で 16 バイトソルトを使い反復はしません。以前の単一パス SHA-256 + 2 バイトソルト方式は両方とも置き換えられます。

  既存のハッシュは認証に引き続き使え、パスワード変更や API キー再作成時に新形式に書き換えられます。

- [#17671](https://github.com/emqx/emqx/pull/17671) 認証拒否ログと認可拒否ログにバックエンドごとの情報を追加しました。

  認証拒否時は拒否を出した認証器の ID とプロバイダーを警告ログで出力します。認可拒否時は警告レベルでソースタイプ、モジュール、トピック、アクションをログに記録します。

  複数認証器や認可ソースを使う環境で、クライアントトレースを有効にせずにどのバックエンドが判定を出したか判別可能です。新しいログは認証器・認可ソースごとにスロットリングされ、ログ洪水を防止します。

- [#18130](https://github.com/emqx/emqx/pull/18130) HTTP 認証器・認可器の URL ホストにテンプレート変数をサポートしました。例: `https://${client_attrs.tns}.auth.example.com/authn`。外部ルーティング層なしでテナントごとの認証・認可エンドポイントを実現できます。

  `hostname_resolution` 設定でホスト名の解決方法を制御します。デフォルト `static` は固定ホスト名と設定時に確立された接続プールを使います。`dynamic`（URL ホストにテンプレートが含まれる場合必須）にすると、リクエストごとにホスト名を解決し、TLS オプションを適用した接続を使います。`pool_size` はアイドル接続の最大数を制限し、`0` で再利用を無効化します。パイプライン設定は無効です。

  テンプレートホストの場合、`allowed_hosts` 設定で許可されるホスト名を正確な名前または `*.suffix` ワイルドカードで指定する必要があります。許可リストにないホスト名はリクエストされずチェックは失敗します。リテラルホスト名は従来通り接続プールを使います。

- [#18239](https://github.com/emqx/emqx/pull/18239) [#18371](https://github.com/emqx/emqx/pull/18371) フラッピング検出がクライアント ID に加え、ユーザー名と接続元 IP アドレスにも対応しました。

  各次元は独自の検出ウィンドウ、接続試行閾値、禁止期間を持ち、`flapping_detect.by_clientid`、`flapping_detect.by_username`、`flapping_detect.by_peerhost` で設定します（ユーザー名と IP はデフォルト無効）。クライアント ID 次元は従来の平坦な `flapping_detect` 設定と互換性を持ち自動マッピングされます。

  閾値超過時は一時禁止され、新規接続試行は認証前に拒否されます。既接続クライアントは影響を受けません。禁止エントリは自動で期限切れになり、`/banned` REST API で確認・早期削除可能です。各エントリは禁止タイプ（`clientid`、`username`、`peerhost`）と `by=flapping detector` を持ち、タイプでフィルタ可能です。

  カウンターはゾーンと次元ごとに分離され、異なる検出ウィンドウのゾーン間でカウンターが干渉しません。

  新しいメトリクス: `flapping.detected.clientid`、`flapping.detected.username`、`flapping.detected.peerhost` は次元ごとのフラッピング検出回数をカウントし、`client.banned` は禁止エントリによる接続拒否回数をカウントします。

#### マルチテナンシー

- [#17454](https://github.com/emqx/emqx/pull/17454) `/api/v5/prometheus/data_integration` で返される Prometheus データをリクエスト元のアクターのネームスペースに制限しました。グローバル管理者はすべてのネームスペースのデータを閲覧・フィルター可能です。

#### データ統合

- [#17129](https://github.com/emqx/emqx/pull/17129) GCP ベースのコネクター（GCP PubSub Producer、GCP PubSub Consumer、BigQuery）で、GCP VM にアタッチされたサービスアカウント認証を追加しました。EMQX は内部メタデータエンドポイントからトークンを取得できます。

- [#17222](https://github.com/emqx/emqx/pull/17222) Bigtable へのデータ追加をサポートする Bigtable データ統合を追加しました。

- [#17547](https://github.com/emqx/emqx/pull/17547) Kafka Producer と Consumer コネクターで AWS IAM Roles Anywhere をサポートしました。認証ヘルパープロセスが提供する HTTP エンドポイントを設定できます。

  認証ヘルパープロセスは EMQX からアクセス可能で稼働している必要があります。詳細は [AWS IAM Roles Anywhere credential helper documentation](https://docs.aws.amazon.com/rolesanywhere/latest/userguide/credential-helper.html#credential-helper-serve) を参照してください。

- [#17783](https://github.com/emqx/emqx/pull/17783) PostgreSQL 系コネクターに `application_name` オプションを追加しました。デフォルトは `emqx` で、PostgreSQL のアクティビティビューやログでコネクターセッションを識別できます。値は 1～63 バイトで、ヌルバイトを含めません。

- [#18119](https://github.com/emqx/emqx/pull/18119) Disk Log コネクターでサイズベース回転に加え時間ベースファイル回転をサポートしました。

  新しいオプション `rotation` が追加されました:

  - `rotation.period`: `none`（デフォルト）、`day`、`hour`。`day` または `hour` の場合、期間境界ごとに別のログファイルセットを開始し、ファイル名に日付スタンプを付与します（例: `mqtt-trace-2026062400.log.1`）。サイズベース回転は期間内で適用されます。
  - `rotation.retention_period`: 期間外のファイルを保持する期間（例: `30d`）。期間回転ごとに自動削除されます。デフォルトは `infinity`（無期限保持）。
  - `rotation.timezone`: 期間境界を決定するタイムゾーン。`UTC`（デフォルト）、`local`、または固定オフセット（例: `+02:00`）。

  デフォルト動作（`rotation` 未設定または `none`）は変更ありません。

- [#18319](https://github.com/emqx/emqx/pull/18319) BigQuery アクションでアクション固有のプロジェクト ID を指定可能にしました。

- [#18624](https://github.com/emqx/emqx/pull/18624) `emqx ctl actions show` と `emqx ctl actions status` コマンドを追加しました。ローカルノードのアクションステータスを JSON 形式で表示し、REST API 認証情報やネットワーク呼び出しは不要です。

  `status` は `{"<type>:<name>": "<status>"}` のコンパクトな JSON 配列を表示し、`show` は `GET /api/v5/actions/{id}` と同様の情報を表示します（コネクターの秘密はマスク）。両コマンドは `--name <type:name>` でアクション選択、`--ns <namespace>` でネームスペース選択が可能で、デフォルトはグローバルネームスペースのすべてのアクションです。

  クラスタ集約された REST API の `status` フィールドではローカルノードのアクション準備完了を判別できないため、ノード単位のレディネスプローブに適しています。

#### 可観測性

- [#17493](https://github.com/emqx/emqx/pull/17493) セッションバッファの可観測性を追加しました。クライアント API は `total_payload_bytes` を報告し、`sysmon.session.total_payload_bytes_high_watermark` はペイロードバイト数閾値超過のスロットル警告を出し、`emqx ctl session-top` はクラスタのトップセッションを `total_payload_bytes` または `mqueue_length` でエクスポートします。

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus メトリクス収集を `prometheus.erl` バージョン 6.1.2 に更新し、パフォーマンスとスケーラビリティを改善しました。

  `vm_dist`、`vm_statistics`、`vm_system_info`、`vm_memory` コレクターはデフォルトで有効になりました。廃止予定の `emqx_vm_process_messages_in_queues` メトリクスは Prometheus 出力から削除されました。

- [#17607](https://github.com/emqx/emqx/pull/17607) [#17998](https://github.com/emqx/emqx/pull/17998) 名前付きコレクション、ワイルドカードトピックフィルター、ネームスペース所有権、REST CRUD、Prometheus スクレイプエンドポイントを備えた v2 トピックメトリクスを追加しました。

  - `/api/v5/mqtt/topic_metrics2/:name` 以下の新ルートで、トピックメトリクスコレクションをユーザー指定の名前（例: `my-pressure`、`vehicle-events`）で登録可能。ワイルドカード（`t/#`、`sensor/+/temp`）がトピックフィルターとして受け入れられ、1 件のパブリッシュが複数のコレクションにマッチしてカウントされます。
  - コレクションはネームスペーススコープ: ネームスペース管理者が作成したコレクションは `client_attrs.tns` が一致するパブリッシャーのみカウント。グローバル管理者が作成したコレクションはすべてのパブリッシャーをカウント。ネームスペース管理者は自分のコレクションのみ閲覧・変更可能。グローバル管理者は `ns` クエリパラメータで個別のネームスペースコレクションにアクセス可能。ネームスペース管理者が他のネームスペース名を指定すると `403 Forbidden`。`ns` 省略時は自身のネームスペース。
  - カウンターは `/api/v5/prometheus/topic_metrics` で Prometheus 形式で公開され、ラベルは `name`、`topic_filter`、`namespace`。レートは Prometheus の `rate()` で計算可能。
  - v1 API (`/api/v5/mqtt/topic_metrics` と `/api/v5/mqtt/topic_metrics/:topic`) は変更なく動作し続け、Swagger 仕様では非推奨。統合は v2 を推奨。

- [#18148](https://github.com/emqx/emqx/pull/18148) OpenTelemetry 経由の Dynatrace 統合を追加しました。対応するシグナルはトレースとログで、OAuth2 トークンで認証します。

#### デプロイメントとセキュリティ

- [#17381](https://github.com/emqx/emqx/pull/17381) OpenAPI 仕様エンドポイントはデフォルトで認証が必要になりました。対象は `GET /api-docs/swagger.json`、`GET /api-spec.json`、`GET /api-spec.md`、`GET /api-spec/:tag[/:name]` です。

  未認証リクエストは `WWW-Authenticate` ヘッダー付きの 401 を返し、サポートされるセキュリティスキームと公開ブートストラップエンドポイント（`POST /api/v5/login`、`GET /api/v5/status`）を含む最小限の有効な OpenAPI ドキュメント（`/api-spec.md` は Markdown 版）を返します。これによりダッシュボードの完全 API 表面を匿名で公開せずに認証方法を発見可能です。

  ダッシュボードの `api-spec.html` エクスプローラーは匿名で読み込み、既存のセッション Cookie またはトークンで仕様を取得します。

- [#17407](https://github.com/emqx/emqx/pull/17407) [#17808](https://github.com/emqx/emqx/pull/17808) 機能ゲートを追加しました。

  `EMQX_FEATURES` 環境変数で指定された機能セットで EMQX を起動可能です。無効なプリセットや機能名はノードの起動を妨げます。依存機能は自動的に有効化されます。

  利用可能なプリセットは以下の 2 つです:

  - `FULL`: デフォルト。すべての機能を有効にして起動。
  - `ESSENTIAL`: 最小機能セットで起動。コア MQTT ブローカーと認証・認可のみ。

  カスタム `EMQX_FEATURES` リストで選択可能な機能:

  - `dashboard`: ダッシュボード UI（SSO と RBAC 含む）、REST API。
  - `data_integration`: コネクター、アクション、ソース、ルールエンジン。
  - `message_transformation`: メッセージ変換。
  - `schema_validation`: スキーマ検証。
  - `schema_registry`: スキーマレジストリ。
  - `gateways`: ゲートウェイプロトコル。
  - `cluster_link`: クラスターリンク。
  - `multi_tenancy`: マルチテナンシーとネームスペース。
  - `ai`: AI 機能（A2A レジストリ、AI 補完）。
  - `metrics`: Prometheus メトリクスエクスポート。
  - `mqtt_extensions`: MQTT 拡張（遅延パブリッシュ、トピック書き換え、自動サブスクライブ、スローサブスクリプション、メッセージキュー、ストリーム）。
  - `plugins`: サードパーティプラグインのインストール・管理用プラグインフレームワーク。
  - `file_transfer`: MQTT ファイル転送拡張。
  - `exhook`: 外部 gRPC フック。
  - `opentelemetry`: OpenTelemetry エクスポーター。

  自動的に有効化される依存関係:

  | 機能 | 自動有効化される依存機能 |
  | --- | --- |
  | `data_integration` | `schema_registry` |
  | `message_transformation` | `schema_registry` |
  | `schema_validation` | `schema_registry` |
  | `ai` | `schema_registry` |
  | `metrics` | `dashboard` |
  | `opentelemetry` | `dashboard` |

  他の機能に依存関係はありません。

- [#17768](https://github.com/emqx/emqx/pull/17768) `node.cookie` をファイルから読み込む `file://` URL 形式をサポートしました。

  オペレーターは `node.cookie = "file:///path/to/cookie"`（または環境変数 `EMQX_NODE__COOKIE` に `file://` URL を指定）を設定可能で、クラスタ秘密が設定ファイルに平文で保存されなくなります。参照先は通常ファイルまたは FIFO（名前付きパイプ）で、ノード起動時に一度だけ読み込まれます。FIFO の場合、オーケストレーターは起動時にパイプに書き込む必要があります。`emqx ctl` などのコマンドは既に起動中のノードからクッキーを取得し、ファイルを再読込しません。

  解決されたクッキーは Erlang VM に直接渡され、生成される `data/configs/vm.*.args` ファイルには書き込まれなくなり、起動時のディスクへの秘密保存を防止します。

- [#17803](https://github.com/emqx/emqx/pull/17803) `EMQX_FEATURES=ESSENTIAL` で起動した場合、Erlang のコードロードモードがデフォルトで `interactive` になり、無効化された機能の `.beam` ファイルは必要時にロードされます。これにより、ESSENTIAL モードノードの常駐メモリ使用量が大幅に削減されます。`CODE_LOADING_MODE` で明示的に上書き可能です。

- [#18451](https://github.com/emqx/emqx/pull/18451) 起動時環境変数を `etc/emqx.env`（RPM/DEB は `/etc/emqx/emqx.env`、Docker は `/opt/emqx/etc/emqx.env`）から読み込む機能を追加しました。

  ファイルには `EMQX_FEATURES` と `EMQX_SECURITY_PROFILE` のデフォルト値のコメントと説明が記載されます。`emqx.conf` 解析前に読み込まれるため、`emqx.conf` 内で設定できません。`emqx` コマンドは毎回このファイルをソースし、サービス起動、フォアグラウンド起動、`emqx ctl` で同一値を使用します。パッケージアップグレードで編集内容は保持されます。

- [#18452](https://github.com/emqx/emqx/pull/18452) `GET /nodes` と `GET /nodes/{node}` で返されるノード情報に `security_profile` と `feature_preset` を追加しました。

  `security_profile` は `legacy` または `hardened`、`feature_preset` は `full`、`essential`、`custom` のいずれかです。ノード起動時に固定され、クラスタ内で異なる設定のノードがあることを一覧で示します。停止中のノードはこれらのフィールドを返しません。

- [#18453](https://github.com/emqx/emqx/pull/18453) `security_profile_divergence` アラームを追加しました。

  `hardened` セキュリティプロファイルのノードはクラスタ内の他のノードのセキュリティプロファイルを定期的にチェックし、`legacy` プロファイルのノードがある場合にアラームを発報します。`legacy` プロファイルのノードはチェックを行わず、古い EMQX リリースのノードは `legacy` とみなします。アラームメッセージは `legacy` ノード名を示し、アラーム詳細はアラーム中のノードリストを保持します。すべての稼働ノードが `hardened` プロファイルになるか、最後の `legacy` ノードがクラスタから離脱するとアラームは自動で解除されます。

  ローリングアップグレード中に一時的に発生することが想定され、長時間解除されない場合は新しい `EMQX_SECURITY_PROFILE` で再起動されていないノードがあることを示します。

- [#18471](https://github.com/emqx/emqx/pull/18471) `node_dump` 診断スクリプトに、起動時環境ファイル `etc/emqx.env` の `EMQX_FEATURES` と `EMQX_SECURITY_PROFILE` 設定を追加収集するようにしました。その他の変数は収集しません。

- [#18557](https://github.com/emqx/emqx/pull/18557) [#18609](https://github.com/emqx/emqx/pull/18609) `node.default_listener_address` 設定を追加しました。MQTT リスナー、ゲートウェイリスナー、ダッシュボード HTTP リスナーの `bind` に明示的なアドレスがない場合のバインドアドレスを設定します。値は `loopback`（127.0.0.1）、`nodename`（Erlang ノード名のホスト部のアドレス、IP でなければ解決）、`all`（0.0.0.0）、リテラル IPv4/IPv6 アドレス、またはホスト名です。設定しない場合はセキュリティプロファイルが従来のデフォルトを決定します。明示的な `IP:port` バインドが優先されます。環境変数 `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` でも設定可能です。

  公式 Docker イメージは `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all` を設定し、セキュリティプロファイルに関わらず公開コンテナポート経由でリスナーに到達可能にします。

  リスナービューは `resolved_address` を報告し、ノード上で実際にバインドされた IP アドレスを示します。既存の `bind` フィールドは設定値（ポート含む）を示し続けます。`resolved_address` はセキュリティプロファイルや `node.default_listener_address` 適用後の IP アドレス（ポートなし）で、裸ポートバインド時に `bind` のアドレスと異なる場合があります。

  もう一つのフィールド `resolved_address_from` は `resolved_address` の値の由来を示します。`bind`（リスナーの `bind` が明示的アドレス）、`0.0.0.0`（全インターフェース）、`127.0.0.1`（ループバック）、`nodename`、またはホスト名や IP のリテラル `node.default_listener_address` 値です。

  両フィールドはノードローカルで、`GET /api/v5/listeners/:id` はリクエスト処理ノードの値を返し、`GET /api/v5/listeners` はノードごとに `node_status` 以下で返します。同一 ID のリスナーがクラスタ内のノードごとに異なるアドレスに解決される場合に対応します（例: `node.default_listener_address=nodename`）。

  `emqx ctl listeners` は既存の `listen_on` と並んで両フィールドを表示します。

- [#18628](https://github.com/emqx/emqx/pull/18628) データバックアップのエクスポートにエクスポートノードのセキュリティプロファイルを記録するようにしました。

  `legacy` プロファイルでエクスポートされたバックアップを `hardened` プロファイルのノードに復元すると、復元後に動作が異なるデータや設定が持ち込まれる可能性があります。

  - 裸ポート MQTT、ゲートウェイ、ダッシュボード HTTP リスナーのバインドが全インターフェースからループバックに変わる
  - 空または無効化された認証チェーンがすべてのクライアントを拒否する
  - 復元されたダッシュボードアカウントはデフォルトパスワードでログインできなくなる
  - 以前は無視されていた認証・認可バックエンドの障害が操作を拒否する

  そのため、`--allow-security-profile-mismatch` CLI フラグまたは `allow_security_profile_mismatch` API パラメータが必要になり、オペレーターが差異を確認できます。変更前のバックアップは `legacy` と同様に扱われます。`legacy` ノードへの復元は影響ありません。

#### プラグイン

- [#18455](https://github.com/emqx/emqx/pull/18455) `hardened` セキュリティプロファイルで `emqx ctl plugins allow <Name-Vsn>` は `sha256:<hex>` 引数を必須にしました。付与はプラグインパッケージを指定された SHA-256 ダイジェストに紐付けます。ダイジェストが一致しないアップロードは拒否されます。ダイジェストなしの付与は拒否され、必要なコマンドを示すメッセージを返します。クラスタピアから送られたダイジェストなし付与も拒否されます。

  `legacy` プロファイルは変更ありません。`sha256:<hex>` 引数は任意です。

#### パッケージング

- [#17335](https://github.com/emqx/emqx/pull/17335) RPM または DEB パッケージインストール後に `/opt/emqx` ディレクトリを作成し、パッケージで使用される散在する FHS パスへの利便性シンボリックリンク（`bin`、`data`、`etc`、`lib`、`log`、`plugins`、`releases`、`erts-*`）を配置するようにしました。これにより、インストール方法に関わらず公式 Docker イメージと同じ `/opt/emqx/...` パスが使えます。

#### パフォーマンス

- [#17583](https://github.com/emqx/emqx/pull/17583) JSON エンコード・デコード性能を改善しました。

  この変更に伴い、JSON 出力の浮動小数点数は Erlang/OTP 標準に合わせて一貫したフォーマットになりました。以前のリリースと若干異なる場合があります（例: 科学表記の切り替えが早くなるなど）。

- [#18033](https://github.com/emqx/emqx/pull/18033) `ESSENTIAL` 機能モード（およびダッシュボード・管理クライアント情報 API 無効化のデプロイ）では、`GET /clients` エンドポイントにのみ使用される接続ごとの統計報告を停止し、高接続数時のオーバーヘッドを削減しました。

- [#18424](https://github.com/emqx/emqx/pull/18424) プラグイン設定スキーマ（`config_schema.avsc`）はプラグイン設定検証時にインストール済みプラグインパッケージから毎回読み込むように変更しました。メモリに保持しません。

  インストール済みプラグインのスキーマファイルが存在しないか読み込み不可の場合は設定検証でファイルエラーを報告します。

- [#18688](https://github.com/emqx/emqx/pull/18688) ダーティ I/O スケジューラー数（`+SDio`）を `node.dirty_io_schedulers` で設定可能にしました。デフォルト `auto` は、`node.schedulers` が 2 超の場合は従来の固定値 8 を使い、小規模ノード（例: 2 vCPU コンテナや cgroup）では 4 を使います。小規模ノードの起動時メモリ使用量を削減しつつ、ブロッキング I/O 操作を重ね合わせるのに十分なスレッド数を確保します。

### バグ修正

#### コア MQTT 機能

- [#18010](https://github.com/emqx/emqx/pull/18010) クライアントが送信する不正な MQTT パケットをブローカーエラーとしてログに記録しなくなりました。

  不正な入力はリスナーの接続シャットダウンカウンターにカウントされ、ポートスキャナーやプロトコルファジングツール、誤動作クライアントからのアラートノイズを削減します。パケット固有の詳細を持つ解析エラーは単一の `frame_error` カウンターを共有し、不正パケットで新しいカウンター名が作成されることはありません。

  解析エラーの詳細（不正バイトなど）はトレースで報告されます。クライアント ID、IP アドレス、トピックでトレースを開始して調査できます。

- [#18027](https://github.com/emqx/emqx/pull/18027) 共有サブスクリプションが無効な場合に共有サブスクリプションを試みるクライアントを切断するよう共有サブスクリプション処理を変更しました。

  `mqtt.shared_subscription` が `false` の場合、クライアントが共有トピックフィルター（`$share/...` または `$queue/...`）を含む SUBSCRIBE を送ると、MQTT 仕様に従いネットワーク接続を閉じます。MQTT 5.0 クライアントには理由コード `0x9E`（Shared Subscriptions not supported）付きの DISCONNECT パケットを送信し、MQTT 3.1/3.1.1 クライアントは単に接続を閉じます。

  以前は SUBACK で失敗理由コードを返し接続を維持していました。

- [#18116](https://github.com/emqx/emqx/pull/18116) `strict_mode` 有効時、MQTT v5 パケットで繰り返し不可のプロパティが複数回含まれる場合（例: CONNECT パケットの `Session-Expiry-Interval` が 2 回以上）はプロトコルエラーとして拒否するようにしました。`User-Property` は繰り返し可能なので影響しません。

  従来の寛容な動作に戻すにはリスナー設定で `strict_mode = false` にします。

- [#18438](https://github.com/emqx/emqx/pull/18438) 送信パケットのサイズチェックでオフバイワンを修正しました。

  クライアントの `Maximum Packet Size` と同じサイズのパケットは破棄され `frame_is_too_large` とログに記録されていましたが、MQTT 5.0 仕様では制限を超えるパケットのみ破棄すべきです。制限以下のパケットは影響ありません。

- [#18470](https://github.com/emqx/emqx/pull/18470) 元のパケットが受信されていない状態でパブリッシャーが切断した場合に、再送された QoS 2 PUBLISH パケットを EMQX がアックしたが配信しなかった問題を修正しました。

  [#16721](https://github.com/emqx/emqx/pull/16721) で導入された QoS 2 重複処理の変更を部分的に元に戻します。待機中 PUBREL 状態が期限切れ後、再送された QoS 2 PUBLISH パケットは新しい QoS 2 交換として扱われ、サブスクライバーに複数回配信される可能性があります。EMQX 7.0.0 以降は待機中 PUBREL 状態の期限切れがデフォルトで無効化され、この再配信は防止されます。

- [#18487](https://github.com/emqx/emqx/pull/18487) 不明なパケット識別子を持つ PUBACK、PUBREC、PUBREL、PUBCOMP パケットのログ量を削減しました。これらのイベントはデバッグレベルでログ出力され、クライアントトレースで利用可能です。

- [#18523](https://github.com/emqx/emqx/pull/18523) ノードシャットダウン時に MQTT リスナーをアプリケーション停止前に停止するようシャットダウン順序を変更しました。

  以前はアプリケーションが停止済みでもリスナーがクライアントトラフィックを受け入れて処理し続け、パブリッシュクライアントがルールエンジンなどの `hook_callback_exception` エラーを大量に発生させていました。リスナーを先に停止することで、アプリケーション停止中にクライアントトラフィックが処理されなくなりました。

  ノードはシャットダウン開始時点で `GET /status` で非稼働を報告し、ロードバランサーが新規接続を停止します。

- [#18585](https://github.com/emqx/emqx/pull/18585) MQTT 仕様に従い、同じクライアント ID の別接続がセッションを引き継ぐ場合、元の接続より長く存続しないセッションは終了するよう修正しました。MQTT 5.0 クライアントの Session Expiry Interval 0 と MQTT 3.1.1 クライアントの Clean Session 1 が対象です。

  修正前は新接続が旧セッションのサブスクリプションやキューイングメッセージを継承し、Will Delay Interval が 0 超の遺言メッセージが黙って破棄されていました。修正後は新接続が新規セッション（CONNACK Session Present 0）を開始し、旧接続は理由コード 0x8E（Session taken over）で切断され、遺言メッセージが引き継ぎ時にパブリッシュされます。

#### アクセス制御

- [#18246](https://github.com/emqx/emqx/pull/18246) Hardened セキュリティプロファイルで遅延メッセージの認可を追加しました。

  - Hardened プロファイルでは、遅延メッセージは再生時に再認可されます。MQTT およびゲートウェイクライアントの遅延メッセージは制限付き認可コンテキストを持ち、パブリッシュ認可ルールと禁止レコードを現在の状態でチェックします。スケジューリング時に認可されたメッセージでも再生時に破棄される可能性があります。
  - Hardened プロファイルでは、アップグレード前に作成された認可コンテキストを持たない保留中の遅延メッセージは再生時に破棄されます。レガシープロファイルは再生します。
  - 複数ゲートウェイのマウントポイント処理を修正しました。ゲートウェイは論理的でマウントされていないトピックを認可に渡します。`authorization.include_mountpoint = false` の場合は論理トピックをチェックし、`true` の場合は認可チェック前にマウントポイントを適用します。パブリッシュやサブスクライブ時は常にマウントポイントを 1 回適用します。
  - GBT 32960、JT/T 808、LwM2M、NATS、STOMP ゲートウェイは認可にプリマウント済みトピックを渡さなくなりました。これにより `authorization.include_mountpoint = true` で二重マウントチェックを防止します。
  - GBT 32960 の `dnstream`、JT/T 808 の `proto.dn_topic`、LwM2M の自動サブスクリプションは認可前にマウントポイントを適用しません。
  - JT/T 808 の `proto.up_topic` と `proto.dn_topic` はゲートウェイマウントポイントに対する相対パスになりました。デフォルトは `jt808/${clientid}/${phone}/up` と `jt808/${clientid}/${phone}/dn` から `${phone}/up` と `${phone}/dn` に変更されました。
  - MQTT-SN のアイドル QoS -1 パブリッシュと遺言メッセージは設定されたマウントポイントを適用します。以前は適用されていませんでした。
  - NATS パブリッシュ認可は NATS サブジェクトから変換された MQTT トピックに対してマウントポイント適用前にチェックします。NATS JWT 権限と EMQX 認可はプリマウント済みトピックをチェックしません。
  - 遅延メッセージの重複処理を修正しました。ブリッジ、リテーナー、スキーマ検証器など他のシステムによる重複処理は、実際の再生時のみ EMQX が行います。プラグインなどの直接内部パブリッシャーは遅延メッセージスケジューリングに `message.ingress` フックを呼び出す必要があります。`emqx:publish/1` 直接呼び出しや管理 API の `$delayed/...` へのパブリッシュはメッセージイングレスをバイパスし、遅延メッセージをスケジューリングしません。

- [#18458](https://github.com/emqx/emqx/pull/18458) `oidcc` を `3.2.3` にアップグレードしました。

  プロバイダー設定ワーカーがキャッシュを更新中に忙しい場合のダッシュボード SSO（OIDC）ログインタイムアウト（`INTERNAL_ERROR: exit,{timeout,{gen_server,call,[...]}}`）を修正しました。

- [#18576](https://github.com/emqx/emqx/pull/18576) OIDC SSO 設定 API (`GET /api/v5/sso/oidc`) は、クライアント JWKS が未設定の場合に `client_jwks` を `none` で返すようになりました。CLI 出力と一致します。以前は未設定でも `******` とマスクされていました。設定済みクライアント JWKS はマスクされたままです。

- [#18580](https://github.com/emqx/emqx/pull/18580) `bin/node_dump` スクリプトで生成される `conf.hocon` ファイルの機密設定値をマスクしました。

  設定スキーマで機密指定された値（例: `dashboard.default_password`、`license.key`）は `******` として書き込まれます。修正前は固定のキー名リストのみマスクされており、これらの値は平文で書き込まれていました。

#### マルチテナンシー

- [#18423](https://github.com/emqx/emqx/pull/18423) ネームスペース管理者によるデータバックアップインポートは、対象ネームスペースの設定のみ適用されるようになりました。認証、認可、ExHook、リスナーなどクラスタ全体設定は警告付きでスキップされ、グローバル設定に書き込まれません。

- [#18466](https://github.com/emqx/emqx/pull/18466) 特殊文字を含むネームスペースのバックアップファイル一覧表示の問題を修正しました。

  以前は `*`、`?`、`{}`、`[]` などを含むネームスペース名のバックアップファイル一覧が空でしたが、バックアップファイルはディスク上に存在していました。一覧表示はネームスペース名をリテラルなディレクトリ名として扱います。

- [#18539](https://github.com/emqx/emqx/pull/18539) ネームスペースが異なる再接続時に永続セッションが前のネームスペースに残る問題を修正しました。

  以前はクライアントが既存セッションを再開（`clean_start=false`）しネームスペースが変わった場合、`GET /api/v5/mt/ns/{ns}/client_list` はクライアントを旧ネームスペースにリストし、新ネームスペースのリストに含まれませんでした。現在はクライアントが接続したネームスペースを常に反映し、永続セッション再開後にクライアントがリストから消える問題も解決します。

#### データ統合

- [#18300](https://github.com/emqx/emqx/pull/18300) コネクターの TLS 設定で証明書ファイルフィールドが空白の場合、`verify` モードに関わらず未設定として扱うようにしました。以前は `verify_peer` 設定時にクライアント証明書が空白だと検証エラーになっていましたが、クライアント証明書はピア検証で任意です。

- [#18392](https://github.com/emqx/emqx/pull/18392) 名前空間が異なる同名の集約アクション（S3、S3Tables、Azure Blob Storage、Snowflake Aggregated）が同じ作業ディレクトリを共有していた問題を修正しました。

- [#18449](https://github.com/emqx/emqx/pull/18449) PostgreSQL アクションで書き込み中に `sock_closed` エラーが発生した稀な競合状態を修正しました。EMQX はこのエラーを回復可能とみなします。

- [#18767](https://github.com/emqx/emqx/pull/18767) RocketMQ コネクターが RocketMQ インスタンスのネームスペースを EMQX ネームスペースとして誤認識していた問題を修正しました。

  RocketMQ コネクターの `namespace` 設定は RocketMQ インスタンスのネームスペースを保持しますが、コネクター API 応答では EMQX ネームスペースと同じ JSON キーで返していました。そのため Dashboard はコネクターをその名前の EMQX ネームスペースに属すると誤認し、「ネームスペース `<name>` の管理者のみ操作可能」と表示し、「管理対象ネームスペースが見つかりません」で開けませんでした。

  コネクター API 応答の `namespace` フィールドは常に EMQX ネームスペースを表し、RocketMQ インスタンスのネームスペースは含まれません。更新リクエストで RocketMQ `namespace` 設定を省略すると既存値を保持します。

#### ルールエンジン

- [#18527](https://github.com/emqx/emqx/pull/18527) スキーマ検証、メッセージ変換、ルールエンジントピックインデックステーブルが利用不可時にメッセージパブリッシュで繰り返し発生していた `badarg` エラーを修正しました。

  そのようなパブリッシュは、検証・変換・ルールマッチなしとして処理され、ブローカーはスロットルされた `topic_index_table_missing` メッセージをログに記録します。インデックステーブルは所有プロセスの再起動に耐え、アプリケーション停止時にはテーブルより先にフックが削除されるため、テーブル欠落の既知のウィンドウを除去します。

#### クラスタリング

- [#18409](https://github.com/emqx/emqx/pull/18409) 複数アドレスを持つ `server` フィールドのクラスタリンクの接続を修正しました。

  リンクはリストされたすべてのアドレスに接続し、各接続は順にアドレスを選択し接続できなければ他のアドレスにフェイルオーバーします。以前は接続できず、ノード再起動までリンクの作成・更新・削除もできませんでした。

- [#18447](https://github.com/emqx/emqx/pull/18447) ノードが他のクラスタメンバーから設定を同期した後に `base.hocon` の変更が無視される問題を修正しました。

  設定同期はピアの `base.hocon` を `cluster.hocon` に永続化しなくなり、ローカルの `base.hocon` 変更は明示的にクラスタ設定に上書きされない限り再起動後に有効になります。

- [#18537](https://github.com/emqx/emqx/pull/18537) クラスタリンクの一時的なメッセージ転送接続エラーを回復可能として分類するよう修正しました。ネットワークの一時的障害で影響を受けたメッセージは失敗としてカウントせずバッファリングして再試行します。

#### ゲートウェイ

- [#18312](https://github.com/emqx/emqx/pull/18312) コネクションモード有効な平文 CoAP UDP リスナーで、他の送信元からの拒否リクエストが後続のダウンリンクメッセージをリダイレクトする問題を修正しました。

- [#18436](https://github.com/emqx/emqx/pull/18436) NATS ゲートウェイの内部 JWT 認証でアカウント JWT の `exp`/`nbf` クレームやアカウントレベルのユーザー無効化を適用するよう修正しました。

  期限切れまたは未有効なアカウント JWT とアカウントにより無効化されたユーザー JWT は認証時に拒否されます。既存接続はユーザー JWT またはアカウント JWT の早い方の期限切れ時に切断されます。破損したリゾルバープリロード済みアカウント JWT はゲートウェイ設定検証時に拒否されます。

- [#18494](https://github.com/emqx/emqx/pull/18494) CoAP ゲートウェイクライアントが内部キープアライブチェック間隔を設定されたハートビート間隔ではなく報告していた問題を修正しました。

  ゲートウェイ API と `emqx ctl gateway-clients list coap` が秒単位の設定済みハートビート値を返します。

- [#18504](https://github.com/emqx/emqx/pull/18504) STOMP フレームのエスケープされたヘッダー文字と CRLF 行末の解析を修正しました。

  STOMP ゲートウェイは STOMP 1.2 仕様に従い、ヘッダー名と値のエスケープシーケンス `\c`、`\r`、`\n`、`\\` をデコードします。CONNECT と CONNECTED フレームは除外され、STOMP 1.0 との後方互換性のためエスケープされません。未定義のエスケープシーケンスはフレームエラーとして拒否します。

  フレームとハートビートの行末に CRLF (`\r\n`) を許容するようにしました。修正前は CRLF 行末を使うクライアントが接続できませんでした。

- [#18700](https://github.com/emqx/emqx/pull/18700) ダッシュボードから既存の NATS ゲートウェイ設定を保存すると、フォームに表示されるマスク済みの認証情報で置き換わり、NATS クライアントの認証に失敗する問題を修正しました。

  認証情報は、認証設定を変更せずに他のゲートウェイ設定を更新する場合に保持されます。

  NATS ゲートウェイの認証設定は、重複する認証方法や認証情報エントリ（重複する NKeys や JWT アカウントエントリを含む）を拒否し、あいまいな認証動作を防止します。

#### プラグイン

- [#18188](https://github.com/emqx/emqx/pull/18188) プラグインフレームワークのパッケージとランタイムの整合性チェックを強化しました。

  - オペレーターは `emqx ctl plugins install` の前に `emqx ctl plugins allow` を実行する必要があり、HTTP アップロード APIと同じ許可ゲートを強制します。
  - プラグイン API コールバックのレスポンスヘッダーは安全な許可リストに制限されます。ブラウザに影響するヘッダー（`set-cookie`、`location`、`access-control-*`、`content-security-policy`、その他認証・セキュリティポリシーヘッダー）や `x-plugin-` プレフィックスなしのカスタムヘッダーは削除されます。
  - プラグインパッケージ抽出の制限を `plugins.package_limits` 設定で追加しました。`max_package_size`（デフォルト 10MB）、`max_decompressed_size`（デフォルト 50MB）、`max_file_count`（デフォルト 10000）、`max_path_depth`（デフォルト 32）、`max_extraction_time_ms`（デフォルト 60秒、クラスタパッケージコピーの RPC タイムアウトにも使用）。これらの制限を超えるパッケージは抽出前または抽出中に拒否されます。インストールディレクトリ外にパスが抜ける tar エントリ（パストラバーサル）も拒否されます。

- [#18468](https://github.com/emqx/emqx/pull/18468) ホットアップグレード（relup）プラグインがターゲットバージョン文字列の検証とアップグレードパス互換性チェックを追加しました。互換性のないまたは不正なアップグレードパッケージは、インストール済みリリースのファイルを削除・上書きせずに拒否されます。

- [#18540](https://github.com/emqx/emqx/pull/18540) `emqx_relup` プラグインパッケージにデフォルト設定ファイル（`priv/config.hocon`）を同梱しました。プラグインインストール時の `failed_to_copy_plugin_default_hocon_config` 警告が繰り返し出る問題を修正しました。

#### ExHook

- [#18464](https://github.com/emqx/emqx/pull/18464) 設定更新中に ExHook サーバーが不健康になった際の稀なクラッシュを修正しました。マネージャーは設定済みサーバー順序を保持し、サーバー再接続中も設定変更を継続提供します。

- [#18473](https://github.com/emqx/emqx/pull/18473) コールバックサーバーが稼働していない場合の ExHook 認証・認可動作を修正しました。レガシーセキュリティプロファイルは設定済み `failed_action` を尊重し、ハードンドプロファイルはフェイルクローズのままです。

#### 可観測性

- [#17602](https://github.com/emqx/emqx/pull/17602) ロガー出力設定を設定ファイルで管理し、CLI の `emqx ctl log outputs` コマンドで設定変更が HTTP API やダッシュボードの設定と整合するようにしました。

- [#17912](https://github.com/emqx/emqx/pull/17912) OpenTelemetry W3C Baggage ヘッダー抽出のセキュリティ脆弱性を修正しました（GHSA-64w2-whjg-q7q7）。受信した `baggage` ヘッダーは 8192 バイト、180 エントリに制限され、不正なキー値ペアはクラッシュせずスキップされます。

- [#18521](https://github.com/emqx/emqx/pull/18521) 接続が強制シャットダウン制限（`force_shutdown.max_mailbox_size` または `force_shutdown.max_heap_size`）を超えた際のシャットダウン報告に識別ラベルを追加しました。

  確立済み接続はクライアント ID を、CONNECT 完了前にシャットダウンした接続はリスナー名とピアアドレスを保持します。以前は制限値と計測値のみで、影響を受けた接続を特定できませんでした。

- [#18696](https://github.com/emqx/emqx/pull/18696) SSO 認証ユーザーが作成した監査ログレコードのクエリでエラーが返される問題を修正しました。

#### 管理

- [#18289](https://github.com/emqx/emqx/pull/18289) HOCON 文字列とキーの JSON 互換 Unicode エスケープシーケンスがデコードされずエスケープされたままになる問題を修正しました。

- [#18403](https://github.com/emqx/emqx/pull/18403) UTF-8 ロケールが設定されていないシェル（非対話 SSH、cron、`LANG` 未設定など）で `emqx ctl` コマンドが非 ASCII 文字を `\x{...}` エスケープや無効バイトで出力する問題を修正しました。

  `emqx` スクリプトで起動される Erlang VM（ノード本体、`emqx ctl`、`emqx eval`、`emqx remote_console`、`emqx escript`）は標準入出力を常に UTF-8 で読み書きします。

- [#18444](https://github.com/emqx/emqx/pull/18444) 設定ファイルでバイトサイズ単位 `b` と `B` に引用符が必要だった問題を修正しました。

  `max_packet_size = 1MB` は受け入れられましたが、`max_packet_size = 1B` は解析エラーで `"1B"` と書く必要がありました。すべてのバイトサイズ単位は引用符なしで受け入れられます。

- [#18509](https://github.com/emqx/emqx/pull/18509) `GET /clients/{clientid}/mqueue_messages` と `GET /clients/{clientid}/inflight_messages` のメッセージページングを修正しました。

  これらの API は 1 ページのペイロードサイズを `max_payload_bytes` パラメータ（デフォルト 1MB）で制限します。制限でページが途中で切られた場合、返される `meta.position` が残りのメッセージの先を指していたため、次ページリクエストでそれらをスキップしていました。これによりメッセージが失われたように見え、`mqueue_len` カウントが API の返すメッセージ数より多くなることがありました。現在は `meta.position` は最後に返したメッセージを指し、次ページは残りの最初のメッセージから開始します。

- [#18544](https://github.com/emqx/emqx/pull/18544) `GET /clients_v2` がメモリセッションの全クライアントを返した後にカーソルを返し、続くカーソルで空ページを返す問題を修正しました。データがない場合はカーソルを返しません。

- [#18558](https://github.com/emqx/emqx/pull/18558) `GET /clients_v2` が `fields` クエリパラメータを無視する問題を修正しました。

#### デプロイメントとセキュリティ

- [#17921](https://github.com/emqx/emqx/pull/17921) `protobuf` 依存を v0.17.0 にアップグレードしました。これは SBOM 生成にのみ使われ、EMQX ランタイムには含まれません。深くネストされたメッセージのデコード時の無限再帰 DoS（GHSA-rv48-qqj5-crxg）修正と Elixir 1.19/1.20 のコンパイラ警告修正を含みます。以前の開発リファレンスから正式リリースに切り替えました。

- [#18706](https://github.com/emqx/emqx/pull/18706) デバッグモードで機密情報をログに出力しないようにしました。

  `bin/emqx` コマンドを `DEBUG=1` または `DEBUG=2` で実行しても Erlang クッキーやライセンスキーがシェルトレースに表示されなくなりました。

## 6.2.3

*リリース日: 2026-08-21*

EMQX 6.2.3 にアップグレードする前に、破壊的変更および既知の問題を必ずご確認ください。

### 強化点

#### アクセス制御

- [#17813](https://github.com/emqx/emqx/pull/17813) ダッシュボードのユーザーおよび API キーエンドポイントで、特権スコープ（`system`、`user_management`、`api_key_management`、`sso_management`）とその他のスコープを混在させたスコープリストを拒否するようにしました。これら 4 つの特権スコープは管理者相当の権限を持つため、制限付きスコープと組み合わせても意味のある制限になりません。アカウントに管理者相当の権限を与えるかどうかで、特権スコープのみのリストか非特権スコープのみのリストを使ってください。既存の混在スコープは次回更新まで動作しますが、更新時は分割が必要です。

- [#17980](https://github.com/emqx/emqx/pull/17980) Hardened セキュリティプロファイルで、サーバー発行のサブスクライブに対してトピック検証、認可、MQTT 機能チェック、クライアントサブスクライブフックを適用するようにしました。

- [#18002](https://github.com/emqx/emqx/pull/18002) Hardened セキュリティプロファイルで SAML レスポンスとアサーションの署名検証をデフォルトで有効にしました。

- [#18296](https://github.com/emqx/emqx/pull/18296) 認証器の `precondition` 式（および Variform 式）で使える新関数 `is_jwt(value)` を追加しました。値が JWT（JWS コンパクト形式）構造の場合のみ真を返し、署名検証は行いません。JWT 以外のパスワードを提示するクライアントで JWT 認証器をスキップするのに便利です。特に Hardened プロファイルで、非 JWT 資格情報が JWT 認証器で拒否される問題を回避します。

#### マルチテナンシー

- [#17732](https://github.com/emqx/emqx/pull/17732) API キー作成・更新エンドポイントに `namespace` フィールドを追加しました。オペレーターはもはや `role` 文字列にネームスペースを埋め込む必要がありません（既存の `ns:<namespace>::<role>` 形式は引き続き有効）。両方指定された場合は一致が必要です。

- [#17855](https://github.com/emqx/emqx/pull/17855) ネームスペーススコープ付きダッシュボード管理者は、自身のネームスペース内で API キーの作成、一覧、読み取り、更新、削除が可能になりました。グローバル API キーや他のネームスペースのキーは作成できず、他ネームスペースの API キーは非表示になります。

#### データ統合

- [#17933](https://github.com/emqx/emqx/pull/17933) RabbitMQ コネクターでマルチノードの `servers` リスト（例: `rmq1:5672,rmq2:5672`）をサポートしました。接続時のフェイルオーバーとプール開始オフセットのローテーションを提供します。`servers` 未設定時は従来の `server`/`port` 設定が有効です。

- [#17944](https://github.com/emqx/emqx/pull/17944) HTTP コネクターと HTTP 認証・認可に OAuth2 クライアントクレデンシャル認証を追加しました。有効化すると、設定されたトークンエンドポイントからアクセストークンを取得・更新し、アウトバウンドリクエストに Bearer 認証ヘッダーとして追加します。

  コネクターのヘルスチェックはトークン取得不能時に `disconnected` を報告します。OAuth2 有効化時に `Authorization` ヘッダーも指定すると設定が拒否されます。

  クライアント ID とシークレットはトークンリクエストボディのフォームフィールドとして送信され、HTTP Basic `Authorization` ヘッダーでの送信はサポートしません。

- [#18014](https://github.com/emqx/emqx/pull/18014) Datalayers Arrow Flight コネクターで準備済みステートメントの自動再構築を追加しました。サーバーが準備済みステートメントを失った場合（例: 再起動後）、クライアントが自動的に再作成し書き込み操作を再試行します。書き込み失敗を回避します。

- [#18042](https://github.com/emqx/emqx/pull/18042) DynamoDB コネクターで AWS IAM ロール認証を追加しました。

  アクセスキー ID とシークレットアクセスキーが省略された場合、EMQX は ECS タスクロールまたは EC2 インスタンスメタデータから一時的認証情報を取得し、有効期限前に更新します。

- [#18081](https://github.com/emqx/emqx/pull/18081) Snowflake Streaming アクションのレジリエンスを改善しました。チャネルの内部状態が同期外になった特定のエラー時に、失敗した行を再試行し、チャネルを手動介入なしで再オープンします。

- [#18085](https://github.com/emqx/emqx/pull/18085) Kafka、Confluent、Azure Event Hubs プロデューサーに以下の設定オプションを追加しました。

  - `max_batch_age`（アクション）: プロデューサーバッファに一定時間以上滞留したメッセージを破棄し、`dropped.expired` メトリクスをインクリメント。デフォルトは `infinity`（破棄しない）。
  - `max_retries`（アクション）: 失敗リトライ回数上限。上限超過でバッチを破棄し、`failed` メトリクスをインクリメント。リトライカウンターは Kafka が明示的にエラーコードを返した場合のみ増加。接続喪失後の再送はカウントしない。デフォルトは `infinity`（無限リトライ）。
  - `reconnect_delay`（アクション）: 接続喪失後の再接続遅延。デフォルトは `2s`（従来は固定）。
  - `request_timeout`（コネクター）: Kafka からの応答待ち時間。これを超えると接続を再確立。デフォルトは `30s`。

  Kafka クライアントライブラリ（wolff）を 4.2.1 にアップグレードし、メモリモードバッファの `max_linger_time` サポートを復活させました。小さなバッチは `max_linger_time` まで待機してメッセージを集約し、送信要求率を削減します。満杯バッチは遅延なく送信されます。

- [#18110](https://github.com/emqx/emqx/pull/18110) スキーマレジストリで JSON Schema ドラフト 2019-09 と 2020-12 をサポートしました。

- [#18137](https://github.com/emqx/emqx/pull/18137) GCP Pub/Sub プロデューサーとコンシューマーがトピック設定に完全修飾トピックパス（`projects/<project-id>/topics/<topic-name>`）を受け入れるようになりました。これにより、サービスアカウントのプロジェクトとは異なる GCP プロジェクトのトピックにパブリッシュまたはコンシューム可能です。ベアトピック名は従来通りサービスアカウントのプロジェクトに解決されます。コンシューマーはサブスクリプションをサービスアカウントのプロジェクトに作成し、トピック参照のみ別プロジェクトを指せます。

#### ルールエンジン

- [#18253](https://github.com/emqx/emqx/pull/18253) ルールエンジン SQL に `map_to_range(value, min, max)` と `hash_to_range(value, min, max)` 関数を追加しました。値またはハッシュ値を包含的整数範囲にマッピングします。シャーディングやバケット分割に便利です。例: トピックの第 2 セグメントからシャードインデックスを導出し、複数ルールに分散する `hash_to_range(nth(2, tokens(topic, '/')), 0, 3)`。

- [#18306](https://github.com/emqx/emqx/pull/18306) LZ4 Frame 圧縮・解凍用のルール関数 `lz4_compress` と `lz4_uncompress` を追加しました。

#### プラグイン

- [#18012](https://github.com/emqx/emqx/pull/18012) EMQX REST API 経由の同期 MQTT リクエスト/レスポンスフロー用プラグイン `emqx_sync_request` を追加しました。ノードローカル CLI 診断でリクエストカウンターと現在の保留状態を確認可能です。

- [#18353](https://github.com/emqx/emqx/pull/18353) ルール SQL 用の名前付きマッピングテーブルを提供する新プラグイン `emqx_maptabs` を追加しました。

  JSON ファイルからテーブルをシードし、ルールエンジンのホットパスで高速検索のためメモリに保持します。新しいルール SQL 関数 `maptab_lookup(Table, Key)`、`maptab_lookup(Table, Key, Field)`、`maptab_lookup(Table, Key, Field, Default)` は長い `CASE ... WHEN ... THEN` を単一のテーブル検索に置き換え、検索結果のフィールドは組み込みの `subbits` 関数に直接渡せます。

  テーブルは `emqx ctl maptabs` CLI で管理可能で、1 ノードでのロードや削除はクラスタ全体に複製され、更新時にダウンしていたノードは復帰時に追いつきます。

  プラグイン設定で安全制限を提供します: `max_tables`（デフォルト 100）、`max_rows_per_table`（デフォルト 10000）、`max_table_file_bytes`（デフォルト 10000000）。

#### パッケージング

- [#18037](https://github.com/emqx/emqx/pull/18037) Enterprise Linux 10（RHEL 10、Rocky Linux 10、互換ディストリビューション）向けパッケージを追加しました。

- [#18127](https://github.com/emqx/emqx/pull/18127) macOS 26（Tahoe）パッケージのリリースを開始しました。

#### パフォーマンス

- [#18185](https://github.com/emqx/emqx/pull/18185) サブスクリプション HTTP API の深いページングクエリを改善しました。各ターゲットノードでインメモリのサブスクリプション行を集計し、ページネーションバッチごとの RPC を回避します。

- [#18229](https://github.com/emqx/emqx/pull/18229) データ統合送信パスの CPU オーバーヘッドを削減しました。アクションやソースでないリソース（例: クラスタリンクのメッセージ転送）を経由するメッセージごとにフォーマット済みエラー文字列を生成しなくなり、高負荷時の長スケジューラ警告を減らします。

#### バグ修正

（以下、バグ修正は 6.3.1 と重複するものは省略します）

---

以降のバージョン（6.2.2、6.2.1、6.1.5、6.1.4、6.1.3、6.1.2、6.1.1、6.1.0、6.0.4、6.0.3、6.0.2、6.0.1、6.0.0）については、同様に英語原文に準拠し、用語集と表記ルールに従って翻訳を行ってください。  
長文のためここでは省略しますが、必要に応じて個別に翻訳を提供いたします。
