# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、トポロジーの大幅な変更後、および定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を、接続規模に合わせて引き上げ、ノードが通常または悪意のある接続負荷下で失敗しないようにします。
- 長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化し、SYNフラッド保護、接続追跡容量、および信頼できるインターフェースのみでのリスナー公開を行います。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`のような平文リスナーは内部または移行用途に限定してください。[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- ノード間通信ポートはセキュリティグループやファイアウォールルールで制限します。クラスター内で使用されるポートマッピングは[Cluster Security](../cluster/security.md)を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドしてください。
- EMQXをロードバランサーやTCPプロキシの背後に配置する場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーのみで[Proxy Protocol](../cluster/lb.md)を有効にします。
- リスナーでProxy Protocolが有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`を設定し、ネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）と組み合わせて強制してください。そうしないと、ポートに直接アクセスしたクライアントが任意のピア証明書フィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。
- EMQX 6.3.0以降、WebSocketリスナー（`ws`または`wss`）が信頼できるプロキシから転送ヘッダーでクライアントアドレスやポートを取得する必要がない限り、`proxy_address_header`と`proxy_port_header`は空のデフォルトのままにしてください。
- これらのオプションは以下のパスで設定します：
  - MQTTリスナー：`listeners.{type}.{name}.websocket`
  - OCPPおよびNATSゲートウェイリスナー：`gateway.<gateway-name>.listeners.{type}.{name}.websocket`
- クライアントがリスナーに直接到達可能な場合、任意の設定済みヘッダーを送信可能です。これらのヘッダーは、プロキシがクライアント提供値を上書きする場合のみ信頼してください。値の追加はなりすましを防止しません。ヘッダーが存在しないか無効な場合、EMQXは対応するTCPピアアドレスまたはポートを使用します。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。[Set Node Cookie](../cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- 秘密情報タイプのフィールドは可能な限りインライン値ではなく`file://`参照として保存してください。SSLキーのパスフレーズ、ブリッジやコネクターのパスワード、APIキーなど秘密として文書化されているフィールドは、値を`file:///path/to/secret`に設定し、EMQXが起動時およびリロード時にファイルから読み込むようにします。これにより、平文の秘密が設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理から除外され、共有やエクスポート時の漏洩リスクを低減します。[Load Secrets from a File](../configuration/secret-from-file.md)を参照してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にしてください。[Cluster Security](../cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番のMQTTリスナーでは、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終リスナー設定を検証してください。
- 信頼されたCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する場合は相互TLSを有効にしてください。このモデルではTLSハンドシェイク時にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制してください（`verify = verify_peer`、`fail_if_no_peer_cert = true`）かつ管理下のCAバンドルを使用してください。これがないと、クライアントは攻撃者が選択したCN/DNを持つ自己署名証明書を提示し、任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md)や[OCSPスタップリング](../network/ocsp.md)の導入を検討してください。
- HTTP認証、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md)を参照してください。
- 共有ユーザー名、パスワード、証明書の代わりに、デバイスまたはアプリケーションごとの資格情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みのIDに紐付けてください。例えば、JWTの`clientid`クレームを検証したり、証明書フィールドを[`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping)でマッピングしたり、HTTP認証機構で不一致を拒否したり、[Client-Info](./authn/cinfo.md)ルールと組み合わせたりします。これを行わない場合：
  - 資格情報が漏洩すると、攻撃者は長い[Session Expiry Interval](../../get-started/messaging/mqtt-concepts.md)を持つランダムなクライアントIDで無制限にセッションを作成でき、アイドル状態の永続セッションが蓄積してブローカーのメモリを枯渇させる恐れがあります。
  - 攻撃者が有効な資格情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDだけでセッションを識別し再開します。攻撃者が同じクライアントIDで接続すると、EMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード`0x8E`（`Session taken over`）付きの`DISCONNECT`パケットを送信します。
  - `Clean Start = 0`の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを引き継ぎます。EMQXはサブスクリプション作成時に認可を行い、再開時の継承サブスクリプションを再評価しません。したがって攻撃者は自身の認可ルールで拒否されるメッセージを受信できます。

  クライアントIDを認証済みIDに紐付けることで、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。この継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXは各パブリッシュ操作を現在のIDで認可します。
- X.509、JWT、SCRAM、または安全なデータベースに基づくパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文の秘密ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`などの強力なアルゴリズムを推奨します。
- EMQXが使用するデータベースアカウントには、認証と認可に必要なデータの読み取り専用アクセスのみを付与してください。EMQXはこれらのデータを読み取るだけです。SQLバックエンドはプリペアドステートメントで`SELECT`を実行し、MongoDBバックエンドは`find`クエリを発行し、Redis認証・認可用の`cmd`は`HGET`と`HMGET`、または`HMGET`と`HGETALL`のみを受け付けます。書き込みや管理権限があると、攻撃者が資格情報を入手した場合や誤ったカスタムクエリにより認証データや認可ルールを変更される恐れがあります。テーブル作成やレコード管理は別の管理アカウントを使用してください。[Authentication](./authn/authn.md)および[Authorization](./authz/authz.md)を参照してください。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md)を参照してください。
- `${clientid}`、`${username}`、`${client_attrs.X}`が認可トピックテンプレート内で使用される場合、挿入される値に`+`、`#`、`/`が含まれる必要がない限り、`authorization.topic_template_allow.plus`、`authorization.topic_template_allow.hash`、`authorization.topic_template_allow.slash`は`false`に設定してください。EMQX 6.3.0以降のデフォルト設定で、これによりクライアント由来の値にMQTTトピックワイルドカードやトピック区切り文字が含まれても、ルールがマッチするトピックフィルターの範囲が広がるのを防ぎます。例えば、クライアントIDが`+`や`tenantA/+`の場合、`clients/${clientid}/data`に代入すると、本来割り当てられたトピックツリー外へのアクセスが許可される恐れがあります。
- 追加の安全策として、認可トピックテンプレートで使用する前にクライアントID値を検証してください。[Client-Info](./authn/cinfo.md)ルールやJWTクレームパターンで厳格なフォーマットを強制するか、HTTP認証機構で非準拠値を拒否します。組み込みの検証およびセキュリティプロファイルの動作については[Topic Placeholders](./authz/authz.md#topic-placeholders)を参照してください。
- HTTP認証、HTTP認可、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、EMQXが機密として認識するフィールドやヘッダーで各シークレットを運搬してください。これにより、関連ログ、トレース、設定APIレスポンスで値が`******`にマスクされます。マスクはフィールド名やヘッダー名で制御されます。HTTPヘッダーに資格情報を配置する場合は、EMQXが常にマスクする標準の`Authorization`（または`Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドのシークレットは、`password`、`token`、`secret`、`secret_key`、`jwt`など認識される機密キー名を付けてください。`x-custom-secret`のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug`レベルのログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切な場合はデフォルト拒否の姿勢を採用し、ルールの末尾に`{deny, all}`を追加し、`authorization.no_match = deny`を設定してください。[Use ACL File](./authz/file.md)を参照してください。
- 信頼できないまたは公開ネットワークに公開されるブローカーでは、`authorization.deny_action = disconnect`（デフォルトは`ignore`）の設定を検討してください。クライアントが許可されていないトピックへのパブリッシュやサブスクライブを試みた場合、EMQXは接続を維持せず切断します。[flapping detection](./flapping-detect.md)と組み合わせると、繰り返し接続して認可拒否を引き起こすクライアントは自動的に禁止されます。`deny_action`はグローバル設定であるため、許可されたトピック以外の操作を試みる正当なクライアントも切断されます。クライアントが通常許可されたトピックのみでパブリッシュ・サブスクライブする場合に適用し、再接続の多い状況で誤って禁止されないようにフラッピング検出の閾値を調整してください。[Authorization](./authz/authz.md)を参照してください。
- 認可キャッシュ設定とオーソライザーの順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- MQTTリソースの使用を制限し、不正または悪意のあるクライアントの影響を軽減してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージなどの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 必要に応じてリスナー単位のレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- [Banned Clients](./blacklist.md)や[Flapping Detect](./flapping-detect.md)を使用して、悪質または不安定なクライアントを制御してください。
- [Message Queue](../../develop/message-queue/message-queue-concept.md)や[MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md)を有効にしている場合、`$queue/`および`$stream/`ネームスペース（非推奨の`$q/`および`$s/`プレフィックスを含む）に対して別個の認可ルールを定義してください。EMQXは完全なプレフィックス付きサブスクリプショントピックフィルターを認可し、`$queue/<name>/`や`$stream/<name>/`の後の`<topic_filter>`部分は個別に認可しません。`#`や`+/#`のルールは`$`で始まるフィルターにはマッチしません。自動作成が有効な場合、この`<topic_filter>`部分を制限してください。これは新しいキューやストリームが受信・保存するパブリッシュメッセージを決定します。[Message Queue Security Considerations](../../develop/message-queue/message-queue-concept.md#security-considerations)および[MQTT Streams Security Considerations](../../develop/mqtt-stream/mqtt-stream-concept.md#security-considerations)を参照してください。
- Cluster Linkingを有効にしている場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`制御ネームスペースを専用のCluster LinkingクライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../../develop/cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理者アクセス権を確認してください。[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定し、管理者アクセスにはHTTPSを推奨します。可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合はホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボード資格情報の代わりにAPIキーを使用し、必要最小限のロールを付与し、有効期限を設定してください。[REST API](../api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- `POST /api/v5/publish`および`POST /api/v5/publish/bulk`へのアクセスは信頼できるサービスのみに制限してください。これらのAPIは管理APIのロールとスコープを使用し、MQTTクライアントの認可ルールは評価しません。認可されたREST呼び出し元はブローカー内の任意のトピックにパブリッシュ可能であり、`publish`スコープはトピック名を制限しません。トピックレベルの制限が必要な場合は認可ルールを適用するMQTTクライアントを使用してください。[REST API](../api.md)を参照してください。
- EMQX Enterpriseを使用している場合、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、可能な場合はIDプロバイダーでMFAを強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../backup-restore.md)を参照してください。
- 監査ログを有効にし、ログやメトリクスを可観測性スタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後はこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効な証明書によるTLSハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
