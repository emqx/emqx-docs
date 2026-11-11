# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティレイヤーごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、主要なトポロジー変更後、および定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ 1: インフラストラクチャとOS

- ノードが通常または悪意のある接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げます。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化します。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化されたリスナーを優先し、`1883` のような平文リスナーは内部または一時的なユースケースに制限します。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- ノード間通信ポートはセキュリティグループやファイアウォールルールで制限します。クラスター内部で使用されるポートマッピングについては [Cluster Security](../cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXを配置する場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーにのみ [Proxy Protocol](../cluster/lb.md) を有効にします。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制します。そうしないと、ポートに直接到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できてしまいます。
- EMQX 6.3.0以降では、WebSocketリスナー（`ws` または `wss`）が信頼できるプロキシから転送ヘッダーを取得する必要がない限り、`proxy_address_header` と `proxy_port_header` は空のデフォルトのままにしてください。
- これらのオプションは適切なパスで設定します：
  - MQTTリスナー：`listeners.{type}.{name}.websocket`
  - OCPPおよびNATSゲートウェイリスナー：`gateway.<gateway-name>.listeners.{type}.{name}.websocket`
- クライアントがリスナーに直接到達可能な場合、任意の設定済みヘッダーを送信できます。これらのヘッダーは、プロキシがクライアント提供値を上書きする場合のみ信頼してください。値の追加だけではなりすましを防止できません。ヘッダーが存在しないか無効な場合、EMQXは対応するTCPピアアドレスまたはポートを使用します。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners) を参照してください。

## フェーズ 2: Erlang とクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用します。[Set Node Cookie](../cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- 秘密情報として扱われるフィールドは可能な限りインライン値ではなく `file://` 参照として保存します。SSLキーのパスフレーズ、ブリッジやコネクタのパスワード、APIキーなどの秘密情報は `file:///path/to/secret` と設定し、EMQXが起動時およびリロード時にファイルから読み込むようにします。これにより、プレーンテキストの秘密情報が設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理に含まれず、共有やエクスポート時の漏洩リスクが低減します。[Load Secrets from a File](../configuration/secret-from-file.md) を参照してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ 3: トランスポートセキュリティ

- 本番のMQTTリスナーでは、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼できるCAまたは社内PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのID検証にクライアント証明書を用いる場合は相互TLSを有効にします。この場合、TLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制し（`verify = verify_peer`、`fail_if_no_peer_cert = true`）、管理するCAバンドルを使用してください。これがないと、クライアントは攻撃者が選んだCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 環境で証明書失効が重要な場合は、[CRLチェック](../network/crl.md) や [OCSPスタップリング](../network/ocsp.md) を評価してください。
- HTTP認証者、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ 4: MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有のユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDに紐付けてください。例えば、JWTの `clientid` クレームを検証したり、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングしたり、HTTP認証者で不一致を拒否したり、[Client-Info](./authn/cinfo.md) ルールと組み合わせたりします。これをしないと：
  - 漏洩した認証情報で攻撃者が無制限にセッションを作成でき、長い[Session Expiry Interval](../../get-started/messaging/mqtt-concepts.md)を持つため、アイドル状態の永続セッションが蓄積し、ブローカーのメモリを枯渇させる恐れがあります。
  - 攻撃者が有効な認証情報を持ち、被害者のクライアントIDを知っていれば、そのセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開するため、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード `0x8E`（`Session taken over`）の `DISCONNECT` パケットを送信します。
  - `Clean Start = 0` の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを引き継ぎます。EMQXはサブスクリプション作成時に認可を行い、再開時の継承サブスクリプションは認可を再評価しません。そのため攻撃者は自身の認可ルールで拒否されるメッセージも受信できます。

  クライアントIDを認証済みIDに紐付けることで、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXはパブリッシュごとに現在のIDで認可を行うためです。
- X.509、JWT、SCRAM、セキュアなデータベースに基づくパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、プレーンテキストではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- EMQXが使用するデータベースアカウントには認証・認可に必要なデータへの読み取り専用アクセスのみを付与してください。EMQXはSQLバックエンドで準備済みステートメントの `SELECT` 文を実行し、MongoDBバックエンドで `find` クエリを発行し、Redis認証・認可用の `cmd` は `HGET` と `HMGET` または `HMGET` と `HGETALL` のみを受け付けます。書き込みや管理権限があると、攻撃者がその認証情報を入手した場合や誤ったカスタムクエリで認証データや認可ルールを変更される恐れがあります。テーブル作成やレコード管理は別の管理用アカウントで行ってください。[Authentication](./authn/authn.md) と [Authorization](./authz/authz.md) を参照してください。
- トピック権限は可能な限り厳密に定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md) を参照してください。
- 認可トピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合、補間値にこれらの文字が含まれる必要がない限り、`authorization.topic_template_allow.plus`、`authorization.topic_template_allow.hash`、`authorization.topic_template_allow.slash` を `false` に設定してください。EMQX 6.3.0以降のデフォルト設定で、クライアント由来の値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれても、ルールのトピックフィルターが広がるのを防ぎます。例えば、クライアントIDが `+` または `tenantA/+` の場合に `clients/${clientid}/data` に代入すると、クライアントの割り当てられたトピックサブツリー外へのアクセスが許可されてしまう可能性があります。
- 追加の安全策として、認可トピックテンプレートでクライアントID値を使用する前に、[Client-Info](./authn/cinfo.md) ルールやJWTクレームパターンで厳格なフォーマット検証を行ってください。あるいはHTTP認証者で非準拠値を拒否する設定も可能です。組み込みの検証やセキュリティプロファイルの動作については [Topic Placeholders](./authz/authz.md#topic-placeholders) を参照してください。
- HTTP認証、HTTP認可、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、秘密情報をEMQXが機密として認識するフィールドやヘッダーに格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が `******` とマスクされます。マスキングはフィールド名やヘッダー名で制御されます。HTTPヘッダーの認証情報は標準の `Authorization`（または `Proxy-Authorization`）ヘッダーを使用してください。EMQXはこれらを常にマスクします。その他の設定フィールドの秘密情報は、`password`、`token`、`secret`、`secret_key`、`jwt` など認識される機密キー名を使用してください。`x-custom-secret` のような非標準カスタムヘッダーや非慣例的なフィールド名は認識されず、`debug` レベルログやエラーメッセージにプレーンテキストで表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切な場合はデフォルト拒否の姿勢を採用し、ルールを `{deny, all}` で終わらせたり、`authorization.no_match = deny` を設定したりしてください。[Use ACL File](./authz/file.md) を参照してください。
- 信頼できないまたは公開ネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは `ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みると、EMQXは接続を切断します。これに [flapping detection](./flapping-detect.md) を組み合わせると、繰り返し接続し認可拒否を引き起こすクライアントを自動的に禁止できます。`deny_action` はグローバル設定であり、拒否された操作を試みる正当なクライアントも切断されるため、クライアントが通常認可されたトピックのみを利用する場合に適用してください。再接続の嵐時に誤って禁止されないよう、flapping検出の閾値を調整してください。[Authorization](./authz/authz.md) を参照してください。
- 認可キャッシュ設定や認可者の順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 必要に応じて [Banned Clients](./blacklist.md) と [Flapping Detect](./flapping-detect.md) を使用して悪質または不安定なクライアントを制御してください。
- [Message Queue](../../develop/message-queue/message-queue-concept.md) または [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) を有効にしている場合は、`$queue/` と `$stream/` ネームスペース（廃止予定の `$q/` と `$s/` プレフィックスも含む）に対して別個の認可ルールを定義してください。EMQXはプレフィックス付きのサブスクリプショントピックフィルター全体を認可し、`$queue/<name>/` や `$stream/<name>/` の後の `<topic_filter>` 部分を個別に認可しません。`#` や `+/#` のルールは `$` で始まるフィルターにはマッチしません。自動作成が有効な場合、この `<topic_filter>` 部分を制限してください。これは新規キューやストリームが受信・保存するパブリッシュメッセージを決定するためです。[Message Queue Security Considerations](../../develop/message-queue/message-queue-concept.md#security-considerations) と [MQTT Streams Security Considerations](../../develop/mqtt-stream/mqtt-stream-concept.md#security-considerations) を参照してください。
- Cluster Linkingを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のCluster LinkingクライアントIDに制限し、その他すべてに対して拒否してください。[Secure Cluster Linking](../../develop/cluster-linking/security.md) を参照してください。

## フェーズ 5: 管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、可能な限りダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合は、ホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボードの認証情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../api.md) と [System](../dashboard/system.md#api-key) を参照してください。
- `POST /api/v5/publish` と `POST /api/v5/publish/bulk` の呼び出し権限は、信頼できるサービスにのみ付与してください。これらのAPIは管理APIのロールとスコープでアクセスを制御し、MQTTクライアントのACLを評価しません。REST経由でのパブリッシュを許可された呼び出し元は、ブローカー全体へのパブリッシュ権限を持ち、トピックの認可に関してはMQTTスーパーユーザーと同等です。`publish` スコープはトピック名を制限しません。アプリケーションでトピック単位の制限が必要な場合は、認可ルールを設定したMQTTクライアントを使用してください。[REST API](../api.md) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md) の利用を検討し、可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、リストア手順をリハーサルしてください。EMQXデータディレクトリ外に保存されている証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効化し、異常検知やインシデント対応のためにログやメトリクスをオブザーバビリティスタックに集約してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後はこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
