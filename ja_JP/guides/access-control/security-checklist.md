# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、大規模なトポロジー変更後、および定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または悪意のある接続圧力下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を接続規模に合わせて引き上げてください。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化してください。
- クライアントが実際に必要とするリスナーのみを公開してください。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`のような平文リスナーは内部または移行用ケースに制限してください。[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- クラスター内で使用されるポートマッピングについては、[Cluster Security](../cluster/security.md)を参照し、ノード間のポートをセキュリティグループやファイアウォールルールで制限してください。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドしてください。
- EMQXをロードバランサーやTCPプロキシの背後にデプロイする場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーにのみ[Proxy Protocol](../cluster/lb.md)を有効にしてください。
- リスナーでProxy Protocolが有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーにのみ公開してください。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制します。そうしないと、直接ポートに到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できてしまいます。
- WebSocketリスナー（`ws`または`wss`）が`x-forwarded-for`ヘッダーを書き換える信頼できるプロキシの背後にない場合、`listeners.{type}.{name}.websocket.proxy_address_header = ""`（および`websocket.proxy_port_header = ""`）を設定し、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査ログが実際のTCPピアアドレスを使用するようにしてください。ヘッダーが有効な場合、派生元IPはクライアント提供の値であり、信頼できるプロキシがヘッダーを書き換えない限り保護されません。インバウンドヘッダーに追記するだけのプロキシは保護になりません。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。[Set Node Cookie](../cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- シークレット型フィールドは可能な限りインライン値ではなく`file://`参照として保存してください。SSLキーのパスフレーズ、ブリッジおよびコネクターのパスワード、APIキーなど、シークレットとしてドキュメント化されているフィールドは、値を`file:///path/to/secret`に設定し、EMQXが起動時およびリロード時にファイルから読み込むようにしてください。これにより、平文のシークレットが設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理から排除され、設定共有やエクスポート時の漏洩リスクが低減されます。[Load Secrets from a File](../configuration/secret-from-file.md)を参照してください。
- クラスター間通信が信頼度の低いネットワークやパブリッククラウド境界を越える場合は、クラスターリングポートを内部に限定し、TLSを有効にしてください。[Cluster Security](../cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番環境のMQTTリスナーでは、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼されたCAまたは社内PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する場合は相互TLSを有効にしてください。この場合、TLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書フィールドをMQTTのユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドルで強制してください。これがないと、クライアントは攻撃者が選んだCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md)や[OCSPステープリング](../network/ocsp.md)を評価してください。
- HTTP認証、データベース、その他の統合先へのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md)を参照してください。
- 共有ユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例として、JWTの`clientid`クレームを検証する、証明書フィールドを[`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping)でマッピングする、HTTP認証機構で不一致を拒否する、または認証機構を[Client-Info](./authn/cinfo.md)ルールと組み合わせる方法があります。バインドしない場合：
  - 漏洩した認証情報により、攻撃者が長い[Session Expiry Interval](../../get-started/messaging/mqtt-concepts.md)を持つ無制限のセッションをランダムなクライアントIDで作成でき、アイドル状態のパーシステントセッションが蓄積されてブローカーのメモリを枯渇させる恐れがあります。
  - 攻撃者が有効な認証情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開するため、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード`0x8E`（`Session taken over`）の`DISCONNECT`パケットを送信します。
  - `Clean Start = 0`の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを引き継ぎます。EMQXはサブスクリプション作成時に認可を行い、再開時の継承サブスクリプションを認証済みIDに対して再評価しません。したがって攻撃者は自身の認可ルールで拒否されるメッセージを受信できます。

  クライアントIDを認証済みIDにバインドすることで、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXは各パブリッシュ操作を現在のIDに対して認可するためです。
- X.509、JWT、SCRAM、または安全なデータベースに裏付けられたパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`などの強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md)を参照してください。
- ACLトピックテンプレート内で`${clientid}`、`${username}`、`${client_attrs.X}`を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、それらのID値がMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）を含まないことを検証してください。検証されていないIDが`clients/${clientid}/data`のようなテンプレートに代入されると、クライアントIDが`+`の場合はワイルドカードパターンに展開されて他のクライアントのサブトピックにアクセス可能になり、`tenantA/+`や`/`を含む場合は割り当てられたサブツリーを逸脱します。上流で厳格なID形式を強制し、[Client-Info](./authn/cinfo.md)ルール、JWTクレームパターン、HTTP認証機構での拒否などを用いてください。ACLに依存して不正な代入を検出するのではなく、接続自体を拒否してください。
- HTTP認証、HTTP認可、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエストを設計する際は、EMQXが機密情報として認識するフィールドやヘッダーにシークレットを格納してください。これにより、関連ログ、トレース、設定APIレスポンスなどのマスキング処理で値が`******`と表示されます。マスキングはフィールド名やヘッダー名で制御されます。HTTPヘッダーに認証情報を置く場合は標準の`Authorization`（または`Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドでは`password`、`token`、`secret`、`secret_key`、`jwt`などの認識済みの機密キー名を使用してください。`x-custom-secret`のような非標準カスタムヘッダーや非慣例的なフィールド名は認識されず、`debug`レベルログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールを削除または調整してください。
- ファイルベースのACLを使用する場合は、適切にデフォルト拒否の姿勢を取り、ルールの末尾に`{deny, all}`を付けたり、`authorization.no_match = deny`を設定してください。[Use ACL File](./authz/file.md)を参照してください。
- 信頼できないまたはパブリックネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは`ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みた場合、EMQXは接続を切断します。これに[フラッピング検出](./flapping-detect.md)を組み合わせると、繰り返し再接続して認可拒否を引き起こすクライアントを自動的に禁止できます。`deny_action`はグローバル設定であり、正当なクライアントの誤操作による切断も発生します。クライアントが通常は認可済みトピックのみを利用する場合に適用し、フラッピング検出の閾値を調整して通常の再接続ラッシュでの禁止を避けてください。[Authorization](./authz/authz.md)を参照してください。
- 認可キャッシュ設定と認可順序を見直し、ポリシー変更が期待通りに反映されることを確認してください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピック階層数、サブスクリプション数、インフライトウィンドウ、キューイングメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 接続およびパブリッシュのバーストを制限するために、必要に応じてリスナーレベルのレート制御を適用してください。[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- 必要に応じて[禁止クライアント](./blacklist.md)および[フラッピング検出](./flapping-detect.md)を使用して悪質または不安定なクライアントを制御してください。
- [Message Queue](../../develop/message-queue/message-queue-concept.md)または[MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md)を有効にしている場合は、`$queue/`および`$stream/`ネームスペース（非推奨の`$q/`および`$s/`プレフィックスを含む）に対して別個の認可ルールを定義してください。EMQXはプレフィックス付きの完全なサブスクリプショントピックフィルターを認可し、`$queue/<name>/`や`$stream/<name>/`の後の`<topic_filter>`部分を個別に認可しません。`#`や`+/#`のルールは`$`で始まるフィルターにマッチしません。自動作成が有効な場合は、この`<topic_filter>`部分を制限してください。これは新しいキューやストリームが受信・保存するパブリッシュメッセージを決定します。[Message Queue Security Considerations](../../develop/message-queue/message-queue-concept.md#security-considerations)および[MQTT Streams Security Considerations](../../develop/mqtt-stream/mqtt-stream-concept.md#security-considerations)を参照してください。
- Cluster Linkingを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`制御ネームスペースを専用のCluster LinkingクライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../../develop/cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、ダッシュボードリスナーは可能な限りlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続は対象外です。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合は、ホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報の代わりにAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、利用可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。EMQXデータディレクトリ外に保存されている証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../backup-restore.md)を参照してください。
- 監査ログを有効にし、異常検知やインシデント対応のためにログとメトリクスを可観測性スタックに集約してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後にこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可外トピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
