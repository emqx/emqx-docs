# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前にレビューするためのものです。セキュリティレイヤーごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、大規模なトポロジー変更後、定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を、接続規模に合わせて引き上げ、ノードが通常または悪意ある接続負荷で失敗しないようにします。
- 長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化します。SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開を含みます。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化リスナーを推奨し、`1883` のような平文リスナーは内部または一時的なユースケースに制限します。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限します。[Cluster Security](../cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーにのみ [Proxy Protocol](../cluster/lb.md) を有効にします。
- Proxy Protocolをリスナーで有効にした場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせて強制します。そうしないと、ポートに直接到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。
- EMQX 6.3.0以降、WebSocketリスナー（`ws` または `wss`）が信頼できるプロキシから転送ヘッダーでクライアントアドレスやポートを取得する必要がない限り、`proxy_address_header` と `proxy_port_header` は空のデフォルトのままにしてください。
- これらのオプションは以下のパスで設定します：
  - MQTTリスナー：`listeners.{type}.{name}.websocket`
  - OCPPおよびNATSゲートウェイリスナー：`gateway.<gateway-name>.listeners.{type}.{name}.websocket`
- クライアントがリスナーに直接アクセス可能な場合、任意の設定済みヘッダーを送信できます。これらのヘッダーは、プロキシがクライアント提供値を上書きする場合にのみ信頼してください。値の追加はなりすましを防ぎません。ヘッダーが存在しないか無効な場合、EMQXは対応するTCPピアアドレスまたはポートを使用します。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners) を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用します。[Set Node Cookie](../cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- 可能な限り、秘密情報はインライン値ではなく `file://` 参照として保存します。SSLキーのパスフレーズ、ブリッジやコネクターのパスワード、APIキーなどの秘密として文書化されているフィールドは、値を `file:///path/to/secret` に設定し、EMQXが起動時およびリロード時にファイルから読み込むようにします。これにより、平文の秘密情報が設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理に含まれず、共有やエクスポート時の漏洩リスクが低減されます。[Load Secrets from a File](../configuration/secret-from-file.md) を参照してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番のMQTTリスナーでは、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終的なリスナー設定を検証します。
- 信頼されたCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する必要がある場合は相互TLSを有効にします。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制し（`verify = verify_peer`、`fail_if_no_peer_cert = true`）、管理するCAバンドルを使用してください。これがないと、クライアントは攻撃者が選んだCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 環境で証明書失効が重要な場合は、[CRLチェック](../network/crl.md) または [OCSPスタップリング](../network/ocsp.md) を評価してください。
- HTTP認証者、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有ユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みのIDにバインドしてください。例えば、JWTの `clientid` クレームを検証したり、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングしたり、HTTP認証者が不一致を拒否したり、認証者と [Client-Info](./authn/cinfo.md) ルールを組み合わせたりします。これを行わない場合：
  - 漏洩した認証情報により攻撃者が無制限のセッションをランダムなクライアントIDで作成し、長い [Session Expiry Interval](../../get-started/messaging/mqtt-concepts.md) を持つアイドル状態のパーシステントセッションが蓄積され、ブローカーのメモリを枯渇させる可能性があります。
  - 攻撃者が有効な認証情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開するため、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード `0x8E`（`Session taken over`）の `DISCONNECT` パケットを送信します。
  - `Clean Start = 0` の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを継承します。EMQXはサブスクリプション作成時に認可を行い、継承されたサブスクリプションを再評価しません。そのため攻撃者は自身の認可ルールで拒否されるメッセージを受信できます。

  クライアントIDを認証済みIDにバインドすることで、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。この継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXはパブリッシュ操作ごとに現在のIDに対して認可を行うためです。
- X.509、JWT、SCRAM、パスワードベース認証（安全なデータベースバックエンド）など、信頼モデルに合った認証機構を選択してください。
- パスワードベース認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- EMQXが使用するデータベースアカウントには、認証と認可に必要なデータへの読み取り専用アクセスのみを付与してください。EMQXはSQLバックエンドで `SELECT` ステートメント（プリペアドステートメントとして実行）、MongoDBバックエンドで `find` クエリ、Redis認証・認可で `HGET` と `HMGET`（または `HMGET` と `HGETALL`）のみを使用します。書き込みや管理権限があると、攻撃者が認証情報を入手した場合や誤ったカスタムクエリで認証データや認可ルールを変更される恐れがあります。テーブル作成やレコード管理は別の管理アカウントで行ってください。[Authentication](./authn/authn.md) と [Authorization](./authz/authz.md) を参照してください。
- トピック権限は可能な限り厳密に定義し、ワイルドカードの使用は慎重に見直してください。[Authorization](./authz/authz.md) を参照してください。
- 認可トピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合、補間される値にこれらの文字が含まれる必要がない限り、`authorization.topic_template_allow.plus`、`authorization.topic_template_allow.hash`、`authorization.topic_template_allow.slash` は `false` のままにしてください。EMQX 6.3.0以降、これらのデフォルトにより、クライアント由来の値にMQTTトピックワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれても、ルールがマッチするトピックフィルターの範囲が広がるのを防ぎます。例えば、クライアントIDに `+` や `tenantA/+` を代入して `clients/${clientid}/data` にすると、クライアント割り当て済みトピックサブツリー外へのアクセスが許可される可能性があります。
- 追加の安全対策として、認可トピックテンプレートで使用する前にクライアントID値の検証を行ってください。[Client-Info](./authn/cinfo.md) ルールやJWTクレームパターンで厳格な形式を強制するか、HTTP認証者で非準拠値を拒否することも可能です。組み込みの検証とセキュリティプロファイルの動作については [Topic Placeholders](./authz/authz.md#topic-placeholders) を参照してください。
- HTTP認証、HTTP認可、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエストを設計する際は、各シークレットをEMQXが機密情報として認識するフィールドまたはヘッダーに格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が `******` にマスクされます。マスクはフィールド名やヘッダー名で制御されます。HTTPヘッダーの認証情報は標準の `Authorization`（または `Proxy-Authorization`）ヘッダーを使用してください。EMQXはこれらを常にマスクします。その他の設定フィールドのシークレットは、`password`、`token`、`secret`、`secret_key`、`jwt` のような認識される機密キー名を使用してください。`x-custom-secret` のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug` レベルのログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールを削除または調整してください。
- ファイルベースのACLでは、適切な場合はデフォルト拒否の姿勢を採用してください。例えば、ルールの末尾に `{deny, all}` を付けたり、`authorization.no_match = deny` を設定したりします。[Use ACL File](./authz/file.md) を参照してください。
- 信頼できないまたはパブリックネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは `ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みた場合、接続を維持せずに切断します。[Flapping Detect](./flapping-detect.md) と組み合わせると、認可拒否を繰り返すクライアントは自動的に禁止されます。`deny_action` はグローバル設定のため、正当なクライアントも拒否操作を試みると切断されます。通常は認可済みトピックのみでパブリッシュ・サブスクライブするクライアントに適用してください。再接続の嵐で誤って禁止されないよう、フラッピング検出の閾値を調整してください。[Authorization](./authz/authz.md) を参照してください。
- 認可キャッシュ設定と認可者の順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意あるクライアントの影響を軽減するため、MQTTリソース使用量を制限してください。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージ数などの制限を見直してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルでレート制限を適用し、接続やパブリッシュのバーストを制御してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 必要に応じて [Banned Clients](./blacklist.md) と [Flapping Detect](./flapping-detect.md) を使用し、悪質または不安定なクライアントを制御してください。
- [Message Queue](../../develop/message-queue/message-queue-concept.md) または [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) を有効にしている場合、`$queue/` と `$stream/` ネームスペース（非推奨の `$q/` と `$s/` プレフィックスも含む）に対して別個の認可ルールを定義してください。EMQXはプレフィックス付きのサブスクリプショントピックフィルター全体を認可し、`$queue/<name>/` や `$stream/<name>/` の後の `<topic_filter>` 部分を個別に認可しません。`#` または `+/#` のルールは `$` で始まるフィルターにはマッチしません。自動作成が有効な場合、この `<topic_filter>` 部分を制限してください。これは新しいキューやストリームが受信・保存するパブリッシュメッセージを決定するためです。[Message Queue Security Considerations](../../develop/message-queue/message-queue-concept.md#security-considerations) と [MQTT Streams Security Considerations](../../develop/mqtt-stream/mqtt-stream-concept.md#security-considerations) を参照してください。
- クラスターリンクを有効にしている場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` 制御ネームスペースを専用のクラスターリンククライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../../develop/cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権限を見直してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワークのみに配置し、管理者アクセスにはHTTPSを推奨します。可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証します。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合はホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) と [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボード資格情報の代わりにAPIキーを使用し、必要最小限のロールを付与し、可能な場合は有効期限を設定してください。[REST API](../api.md) と [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) を検討し、利用可能な場合はIDプロバイダーでMFAを強制してください。
- 定期的なバックアップをスケジュールし、リストア手順をリハーサルしてください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../backup-restore.md) を参照してください。
- 可能な場合は監査ログを有効にし、異常検知やインシデント対応のためにログやメトリクスをオブザーバビリティスタックに集約してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後にこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブ拒否など、想定される失敗モードを本番切り替え前に検証してください。
