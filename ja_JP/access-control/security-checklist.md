# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前にレビューするためのものです。セキュリティレイヤーごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、大規模なトポロジー変更後、および定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を、接続規模に合わせて引き上げ、ノードが通常または悪意のある接続負荷で失敗しないようにします。
- 長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化し、SYNフラッド防御、接続追跡容量、信頼できるインターフェースのみでのリスナー公開を含みます。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`のような平文リスナーは内部または移行用ケースに制限してください。[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限します。[Cluster Security](../deploy/cluster/security.md)を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- EMQXをロードバランサーやTCPプロキシの背後にデプロイする場合、実際のクライアントIPアドレスやクライアント証明書の詳細が必要なリスナーでのみ[Proxy Protocol](../deploy/cluster/lb.md)を有効にします。
- リスナーでProxy Protocolが有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`を設定し、ネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）と組み合わせて強制してください。そうしないと、ポートに直接到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。
- EMQX 6.3.0以降では、WebSocketリスナー（`ws`または`wss`）が信頼できるプロキシから転送ヘッダーでクライアントアドレスやポートを取得する必要がない限り、`proxy_address_header`および`proxy_port_header`は空のデフォルトのままにしてください。
- これらのオプションは適切なパスで設定します：
  - MQTTリスナー：`listeners.{type}.{name}.websocket`
  - OCPPおよびNATSゲートウェイリスナー：`gateway.<gateway-name>.listeners.{type}.{name}.websocket`
- クライアントがリスナーに直接到達可能な場合、任意の設定済みヘッダーを送信できます。これらのヘッダーは、プロキシがクライアント提供値を上書きする場合にのみ信頼してください。値の追加はなりすましを防止しません。ヘッダーが存在しないか無効な場合、EMQXは対応するTCPピアアドレスまたはポートを使用します。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを変更し、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- シークレット型フィールドは可能な限りインライン値ではなく`file://`参照として保存してください。SSLキーのパスフレーズ、ブリッジやコネクターのパスワード、APIキーなどのシークレットとして文書化されたフィールドは、値を`file:///path/to/secret`に設定し、EMQXが起動時およびリロード時にファイルから読み込むようにします。これにより、平文のシークレットが設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理に含まれず、共有やエクスポート時の漏洩リスクを低減します。[Load Secrets from a File](../configuration/secret-from-file.md)を参照してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../deploy/cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番MQTTリスナーでは、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、最終的なリスナー設定をステージング環境で検証してください。
- 信頼できるCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する場合は相互TLSを有効にします。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書フィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制してください（`verify = verify_peer`、`fail_if_no_peer_cert = true`）かつ管理するCAバンドルを使用してください。これがないと、クライアントは攻撃者が選択したCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 環境で証明書失効が重要な場合は、[CRLチェック](../network/crl.md)や[OCSPスタップリング](../network/ocsp.md)を評価してください。
- HTTP認証者、データベース、その他の統合先など外部リソースへのアウトバウンド接続にもTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に少なくとも1つの認証機構を設定してください。認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md)を参照してください。
- 共有ユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例えば、JWTの`clientid`クレームを検証したり、証明書フィールドを[`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping)でマッピングしたり、HTTP認証者で不一致を拒否したり、[Client-Info](./authn/cinfo.md)ルールと組み合わせたりします。これをしない場合：
  - 漏洩した認証情報により、攻撃者がランダムなクライアントIDで無制限にセッションを作成し、長い[Session Expiry Interval](../messaging/mqtt-concepts.md)を設定すると、アイドル状態の永続セッションが蓄積し、ブローカーのメモリを枯渇させる恐れがあります。
  - 攻撃者が有効な認証情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開するため、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントでは、EMQXは理由コード`0x8E`（`Session taken over`）の`DISCONNECT`パケットを送信します。
  - `Clean Start = 0`の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを継承します。EMQXはサブスクリプション作成時に認可し、継承されたサブスクリプションを再評価しないため、攻撃者は自身の認可ルールで拒否されるメッセージを受信可能です。

  クライアントIDを認証済みIDにバインドすると、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。パブリッシュは現在のIDに対して都度認可されるため、この継承サブスクリプションのリスクはありません。
- X.509、JWT、SCRAM、パスワード認証（安全なデータベースに基づく）など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`などの強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く設定し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md)を参照してください。
- `${clientid}`、`${username}`、`${client_attrs.X}`が認可トピックテンプレート内で使用される場合、EMQX 6.3.0以降は、`authorization.topic_template_allow.plus`、`authorization.topic_template_allow.hash`、`authorization.topic_template_allow.slash`を、補間値にそれらの文字が含まれる必要がない限り`false`に設定してください。これにより、クライアント由来の値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれても、ルールがマッチするトピックフィルターの範囲が広がることを防ぎます。例えば、クライアントIDに`+`や`tenantA/+`を代入すると、`clients/${clientid}/data`がクライアントの割当トピックサブツリー外へのアクセスを許可する恐れがあります。
- 追加の対策として、認可トピックテンプレートで使用する前にクライアントIDなどの値を検証してください。[Client-Info](./authn/cinfo.md)ルールやJWTクレームパターンで厳格なフォーマットを強制するか、HTTP認証者で非準拠値を拒否してください。組み込みの検証およびセキュリティプロファイルの動作は[Topic Placeholders](./authz/authz.md#topic-placeholders)を参照してください。
- HTTP認証、HTTP認可、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、EMQXが機密情報として認識するフィールドやヘッダーにシークレットを格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が`******`にマスクされます。マスクはフィールド名やヘッダー名で制御されます。HTTPヘッダーに配置する認証情報は標準の`Authorization`（または`Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドは`password`、`token`、`secret`、`secret_key`、`jwt`など認識されるキー名を付けてください。`x-custom-secret`のような非標準カスタムヘッダーや非慣習的なフィールド名は認識されず、`debug`レベルのログやエラーメッセージで平文が表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切な場合はデフォルト拒否の姿勢を取り、ルールを`{deny, all}`で終わらせ、`authorization.no_match = deny`を設定してください。[Use ACL File](./authz/file.md)を参照してください。
- 信頼できないまたはパブリックネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは`ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みた場合、EMQXは接続を切断します。これに[flapping detection](./flapping-detect.md)を組み合わせると、繰り返し再接続して認可拒否を引き起こすクライアントを自動的に禁止できます。`deny_action`はグローバル設定のため、誤って拒否された正当なクライアントも切断されます。通常、クライアントが許可されたトピックのみでパブリッシュ・サブスクライブする場合に適用してください。再接続の嵐時に誤禁止しないようにフラッピング検出の閾値を調整してください。[Authorization](./authz/authz.md)を参照してください。
- 認可キャッシュ設定や認可順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意あるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- 悪質または不安定なクライアントを抑制するために、[Banned Clients](./blacklist.md)および[Flapping Detect](./flapping-detect.md)を活用してください。
- クラスターリンクが有効な場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権限を見直してください。[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定し、管理者アクセスにはHTTPSを推奨します。可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更可能な場合や完全なアウトバウンドネットワーク境界が必要な場合は、ホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、利用可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md)を参照してください。
- 監査ログを有効にし、ログやメトリクスをオブザーバビリティスタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後は、このチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可外トピックでのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
