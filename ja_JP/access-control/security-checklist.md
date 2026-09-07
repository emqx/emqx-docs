# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、大規模なトポロジー変更後、および定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または悪意のある接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を接続規模に合わせて引き上げてください。
- SYNフラッド防御、接続追跡容量、信頼できるインターフェースのみにリスナーを公開するなど、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化してください。
- クライアントが実際に必要とするリスナーのみを公開してください。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`などの平文リスナーは内部または移行用ケースに限定してください。詳細は[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- クラスター内で使用されるポートマッピングについては、[Cluster Security](../deploy/cluster/security.md)を参照し、ノード間ポートをセキュリティグループやファイアウォールルールで制限してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドしてください。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーにのみ[Proxy Protocol](../deploy/cluster/lb.md)を有効にしてください。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開してください。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制します。そうしないと、直接ポートに到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。
- WebSocketリスナー（`ws`または`wss`）が`x-forwarded-for`ヘッダーを書き換える信頼できるプロキシの背後にない場合、`listeners.{type}.{name}.websocket.proxy_address_header = ""`（および`websocket.proxy_port_header = ""`）を設定し、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査ログが実際のTCPピアアドレスを使用するようにしてください。ヘッダーが有効な場合、派生元IPは信頼できるプロキシが上書きしない限りクライアント提供の値となります。インバウンドヘッダーに追記するだけのプロキシは保護しません。詳細は[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。詳細は[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の機密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- 可能な限り、シークレット型フィールドはインライン値ではなく`file://`参照として保存してください。SSLキーのパスフレーズ、ブリッジおよびコネクターのパスワード、APIキーなどのシークレットとして文書化されているフィールドは、値を`file:///path/to/secret`に設定し、EMQXが起動時およびリロード時にファイルから読み込むようにしてください。これにより、平文のシークレットが設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理から除外され、共有やエクスポート時の漏洩リスクが低減されます。詳細は[Load Secrets from a File](../configuration/secret-from-file.md)を参照してください。
- クラスター間通信のポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はTLSを有効にしてください。詳細は[Cluster Security](../deploy/cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。詳細は[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、最終的なリスナー設定をステージング環境で検証してください。
- 信頼されたCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- クライアント証明書によるデバイスID検証が必要な場合は相互TLSを有効にしてください。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。詳細は[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制してください（`verify = verify_peer`、`fail_if_no_peer_cert = true`）かつ管理下のCAバンドルを使用してください。これがないと、クライアントは自己署名証明書を提示して攻撃者が選んだCN/DNで任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。詳細は[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md)や[OCSPステープリング](../network/ocsp.md)の評価を行ってください。
- HTTP認証、データベース、その他の統合先へのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、EMQXは認証が有効でない場合すべてのクライアントの接続を許可します。詳細は[Authentication](./authn/authn.md)を参照してください。
- 共有のユーザー名、パスワード、証明書ではなく、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例えば、JWTの`clientid`クレームを検証したり、証明書フィールドを[`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping)でマッピングしたり、HTTP認証機構で不一致を拒否したり、[Client-Info](./authn/cinfo.md)ルールと組み合わせたりします。これをしない場合：
  - 資格情報が漏洩すると、攻撃者はランダムなクライアントIDで無制限にセッションを作成でき、長い[Session Expiry Interval](../messaging/mqtt-concepts.md)によりアイドル状態の永続セッションが蓄積されてブローカーのメモリを枯渇させる恐れがあります。
  - 攻撃者が有効な資格情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開し、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード`0x8E`（`Session taken over`）付きの`DISCONNECT`パケットを送信します。
  - `Clean Start = 0`の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを継承します。EMQXはサブスクリプション作成時に認可し、再開時の継承サブスクリプションは再評価しません。攻撃者は自身の認可ルールで拒否されるメッセージを受信可能になります。

  クライアントIDを認証済みIDにバインドすると、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。なお、この継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXは現在のIDに対して各パブリッシュ操作を認可します。
- X.509、JWT、SCRAM、または安全なデータベースに基づくパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`などの強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。詳細は[Authorization](./authz/authz.md)を参照してください。
- ACLトピックテンプレート内で`${clientid}`、`${username}`、`${client_attrs.X}`を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、これらのID値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないよう検証してください。検証されていないIDが`clients/${clientid}/data`のようなテンプレートに代入され、クライアントIDが`+`の場合はワイルドカードパターンに展開されて他クライアントのサブトピックにアクセス可能になったり、`tenantA/+`や`/`を含む場合は割り当てられたサブツリーから逸脱したアクセスが可能になります。上流で[Client-Info](./authn/cinfo.md)ルール、JWTクレームパターン、HTTP認証機構での拒否など厳格なID形式を強制し、ACLに頼らず接続を拒否してください。
- HTTP認証（./authn/http.md）、HTTP認可（./authz/http.md）、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、EMQXが機密として認識するフィールドまたはヘッダーにシークレットを保持してください。これにより、関連ログ、トレース、設定APIレスポンスのマスキング処理で値が`******`と表示されます。マスキングはフィールド名やヘッダー名で制御され、HTTPヘッダーの認証情報は標準の`Authorization`（または`Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドでは`password`、`token`、`secret`、`secret_key`、`jwt`などの認識されるキー名を使用してください。`x-custom-secret`のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug`レベルログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールを削除または調整してください。
- ファイルベースのACLでは、適切に`{deny, all}`でルールを終了させ、`authorization.no_match = deny`を設定するなど、デフォルト拒否の姿勢を推奨します。詳細は[Use ACL File](./authz/file.md)を参照してください。
- 信頼できないまたはパブリックネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは`ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みた場合、EMQXは接続を切断します。これに[フラッピング検出](./flapping-detect.md)を組み合わせると、繰り返し再接続して認可拒否を引き起こすクライアントは自動的に禁止されます。`deny_action`はグローバル設定であり、正当なクライアントが誤って拒否操作を試みた場合も切断されるため、クライアントが通常認可済みトピックのみを利用する場合に適用してください。フラッピング検出の閾値は通常の再接続ラッシュ時に誤禁止しないよう調整してください。詳細は[Authorization](./authz/authz.md)を参照してください。
- 認可キャッシュ設定と認可順序をレビューし、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージ数などの制限を検討してください。詳細は[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。詳細は[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- 悪質または不安定なクライアントを制御するために[禁止クライアント](./blacklist.md)および[フラッピング検出](./flapping-detect.md)を活用してください。
- [Message Queue](../message-queue/message-queue-concept.md)または[MQTT Streams](../mqtt-stream/mqtt-stream-concept.md)を有効にしている場合は、`$queue/`および`$stream/`ネームスペース（非推奨の`$q/`および`$s/`プレフィックスを含む）に対して別個の認可ルールを定義してください。EMQXは完全なプレフィックス付きサブスクリプショントピックフィルターを認可し、`$queue/<name>/`や`$stream/<name>/`の後の`<topic_filter>`部分を個別に認可しません。`#`や`+/#`のルールは`$`で始まるフィルターにはマッチしません。自動作成が有効な場合、この`<topic_filter>`部分を制限してください。これは新しいキューやストリームが受信・保存するパブリッシュメッセージを決定します。詳細は[Message Queue Security Considerations](../message-queue/message-queue-concept.md#security-considerations)および[MQTT Streams Security Considerations](../mqtt-stream/mqtt-stream-concept.md#security-considerations)を参照してください。
- クラスターリンクを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、その他すべてに対して拒否してください。詳細は[Secure Cluster Linking](../cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を持つユーザーを見直してください。詳細は[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、ダッシュボードリスナーは可能な限りlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。詳細は[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合はホストレベルのイグレス制御を追加してください。詳細は[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボードの資格情報ではなくAPIキーを使用し、必要最小限のロールを付与し、有効期限を設定してください。詳細は[REST API](../admin/api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、復元手順のリハーサルを行ってください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。詳細は[Backup and Restore](../operations/backup-restore.md)を参照してください。
- 監査ログを有効にし、ログやメトリクスを可観測性スタックに集約して異常検知やインシデント対応に活用してください。詳細は[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、および[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後にこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
