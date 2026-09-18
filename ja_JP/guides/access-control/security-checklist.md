# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初期展開時、主要なトポロジー変更後、および定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を、接続規模に合わせて引き上げ、ノードが通常または攻撃的な接続負荷下で失敗しないようにします。
- 長時間維持されるMQTTトラフィックに対して、TCPスタックとファイアウォールの設定を強化します。これにはSYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開が含まれます。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化リスナーを優先し、`1883` のような平文リスナーは内部または移行用途に限定してください。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- ノード間のポートはセキュリティグループやファイアウォールルールで制限します。クラスター内で使用されるポートマッピングについては [Cluster Security](../cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- EMQXをロードバランサーやTCPプロキシの背後に配置する場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーのみに [Proxy Protocol](../cluster/lb.md) を有効にします。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーにのみ公開します。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制します。そうしないと、ポートに直接到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDをなりすます可能性があります。
- EMQX 6.3.0以降では、WebSocketリスナー（`ws` または `wss`）が信頼できるプロキシから転送ヘッダーを上書きしてクライアントアドレスやポートを取得する必要がない限り、`proxy_address_header` と `proxy_port_header` は空のデフォルトのままにしてください。
- これらのオプションは適切なパスで設定します：
  - MQTTリスナー：`listeners.{type}.{name}.websocket`
  - OCPPおよびNATSゲートウェイリスナー：`gateway.<gateway-name>.listeners.{type}.{name}.websocket`
- クライアントがリスナーに直接到達できる場合、任意の設定済みヘッダーを送信可能です。これらのヘッダーは、プロキシがクライアント送信値を上書きする場合のみ信頼してください。値の追加はなりすまし防止になりません。ヘッダーが存在しないか無効な場合、EMQXは対応するTCPピアアドレスまたはポートを使用します。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners) を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用します。[Set Node Cookie](../cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- 秘密情報タイプのフィールドは可能な限りインライン値ではなく `file://` 参照として保存します。SSLキーのパスフレーズ、ブリッジやコネクターのパスワード、APIキーなど秘密として文書化されているフィールドは、値を `file:///path/to/secret` に設定し、EMQXが起動時およびリロード時にファイルから秘密を読み込むようにします。これにより、平文の秘密が設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理に含まれることを防ぎ、設定共有やエクスポート時の漏洩リスクを低減します。[Load Secrets from a File](../configuration/secret-from-file.md) を参照してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番環境のMQTTリスナーには、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、最終的なリスナー設定をステージング環境で検証してください。
- 信頼されたCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する場合は相互TLSを有効にします。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドルで強制してください。これがないと、クライアントは攻撃者が選んだCN/DNの自己署名証明書を提示し、任意のIDをなりすますことが可能です。空のユーザー名の場合の追加保護として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md) または [OCSPスタップリング](../network/ocsp.md) の評価を行ってください。
- HTTP認証者、データベース、その他の統合先へのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有ユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例えば、JWTの `clientid` クレームを検証する、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングする、HTTP認証者が不一致を拒否する、または認証者と [Client-Info](./authn/cinfo.md) ルールを組み合わせる方法があります。これをしないと以下のリスクがあります：
  - 認証情報が漏洩すると、攻撃者がランダムなクライアントIDで無制限にセッションを作成でき、長い [Session Expiry Interval](../../get-started/messaging/mqtt-concepts.md) によりアイドル状態の永続セッションが蓄積されてブローカーのメモリを枯渇させる可能性があります。
  - 攻撃者が有効な認証情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開するため、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード `0x8E`（`Session taken over`）付きの `DISCONNECT` パケットを送信します。
  - `Clean Start = 0` の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを引き継ぎます。EMQXはサブスクリプション作成時に認可を行い、再開時の継承サブスクリプションは認証済みIDに対して再評価しません。したがって攻撃者は自身の認可ルールで拒否されるメッセージを受信可能です。

  クライアントIDを認証済みIDにバインドすると、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXは各パブリッシュ操作を現在のIDに対して認可します。
- X.509、JWT、SCRAM、または安全なデータベースをバックエンドとするパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く設定し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md) を参照してください。
- 認可トピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合、補間される値にこれらの文字が含まれる必要がない限り、`authorization.topic_template_allow.plus`、`authorization.topic_template_allow.hash`、`authorization.topic_template_allow.slash` は `false` に設定したままにしてください。EMQX 6.3.0以降、これらのデフォルト設定によりクライアント由来の値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれても、ルールのトピックフィルターが広がるのを防ぎます。例えば、クライアントIDが `+` や `tenantA/+` の場合、`clients/${clientid}/data` に代入するとクライアントの割り当てトピックサブツリー外へのアクセスを許可してしまう可能性があります。
- 追加の保護として、認可トピックテンプレートで使用する前にクライアントID値の検証を行ってください。厳格な形式を [Client-Info](./authn/cinfo.md) ルールやJWTクレームパターンで強制するか、HTTP認証者で非準拠値を拒否する設定も可能です。組み込みの検証とセキュリティプロファイルの動作については [Topic Placeholders](./authz/authz.md#topic-placeholders) を参照してください。
- HTTP認証（./authn/http.md）、HTTP認可（./authz/http.md）、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、秘密情報をEMQXが機密として認識するフィールドやヘッダーに格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が `******` とマスクされます。マスクはフィールド名やヘッダー名で制御されます。HTTPヘッダーの認証情報は標準の `Authorization`（または `Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドは `password`、`token`、`secret`、`secret_key`、`jwt` のような認識済みの機密キー名を使います。`x-custom-secret` のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug` レベルのログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLを使用する場合は、適切にデフォルト拒否の姿勢を取ってください。例えばルールを `{deny, all}` で終わらせ、`authorization.no_match = deny` を設定します。[Use ACL File](./authz/file.md) を参照してください。
- 信頼できないまたはパブリックネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは `ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みた場合、EMQXは接続を切断します。これにより [flapping detection](./flapping-detect.md) と組み合わせて、繰り返し再接続して認可拒否を引き起こすクライアントを自動的に禁止できます。`deny_action` はグローバル設定であり、拒否された操作を試みる正当なクライアントも切断されるため、クライアントが通常認可済みトピックのみを扱う場合に適用してください。再接続の嵐で誤検知しないようフラッピング検知の閾値を調整してください。[Authorization](./authz/authz.md) を参照してください。
- 認可キャッシュ設定と認可者の順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- MQTTリソース使用を制限し、不正または悪意あるクライアントの影響を軽減してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルのレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 必要に応じて [Banned Clients](./blacklist.md) と [Flapping Detect](./flapping-detect.md) を使用し、悪質または不安定なクライアントを制御してください。
- [Message Queue](../../develop/message-queue/message-queue-concept.md) または [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) を有効にしている場合、`$queue/` と `$stream/` ネームスペース（廃止予定の `$q/` と `$s/` プレフィックスを含む）に対して個別の認可ルールを定義してください。EMQXは完全なプレフィックス付きサブスクリプショントピックフィルターを認可し、`$queue/<name>/` または `$stream/<name>/` の後の `<topic_filter>` 部分は個別に認可しません。`#` または `+/#` のルールは `$` で始まるフィルターにはマッチしません。自動作成が有効な場合、この `<topic_filter>` 部分を制限してください。これは新しいキューやストリームが受信・保存するパブリッシュメッセージを決定するためです。[Message Queue Security Considerations](../../develop/message-queue/message-queue-concept.md#security-considerations) および [MQTT Streams Security Considerations](../../develop/mqtt-stream/mqtt-stream-concept.md#security-considerations) を参照してください。
- Cluster Linkingを有効にしている場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のCluster LinkingクライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../../develop/cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番使用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、可能な限りダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合はホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) の導入を検討し、利用可能な場合はIDプロバイダーでMFAを強制してください。
- 定期的なバックアップをスケジュールし、リストア手順のリハーサルを行ってください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効にし、ログやメトリクスをオブザーバビリティスタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後はこのチェックリストを再実行してください。
- 匿名クライアント拒否、無効証明書によるTLSハンドシェイク失敗、許可外トピックへのパブリッシュやサブスクライブ拒否など、想定される失敗モードを本番切り替え前に検証してください。
