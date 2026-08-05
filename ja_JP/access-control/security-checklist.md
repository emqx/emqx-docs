# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初期展開時、大規模なトポロジー変更後、および定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または攻撃的な接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げてください。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィックに対してTCPスタックとファイアウォールの強化を行ってください。
- クライアントが実際に必要とするリスナーのみを公開してください。信頼できないネットワークでは、`8883` や `8084` のような暗号化されたリスナーを優先し、`1883` のような平文リスナーは内部または一時的なユースケースに制限してください。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限してください。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドしてください。
- EMQXをロードバランサーやTCPプロキシの背後にデプロイする場合、実際のクライアントIPアドレスやクライアント証明書の詳細が必要なリスナーに対してのみ [Proxy Protocol](../deploy/cluster/lb.md) を有効にしてください。
- リスナーでProxy Protocolが有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーにのみ公開してください。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制します。そうしないと、ポートに直接到達したクライアントが任意のピア証明書フィールドを含むPROXY v2フレームを作成し、任意のIDを偽装できます。
- WebSocketリスナー（`ws` または `wss`）が `x-forwarded-for` ヘッダーを書き換える信頼できるプロキシの背後にない場合、`listeners.{type}.{name}.websocket.proxy_address_header = ""`（および `websocket.proxy_port_header = ""`）を設定し、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査ログが実際のTCPピアアドレスを使用するようにしてください。ヘッダーが有効な場合、派生元IPはクライアント提供のものであり、信頼できるプロキシがヘッダーを書き換えない限り保護されません。プロキシがインバウンドヘッダーに追記するだけでは保護されません。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners) を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にしてください。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、リスナー設定をステージング環境で検証してください。
- 信頼できるCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する場合は相互TLSを有効にしてください。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書フィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドルで強制してください。これがないと、クライアントは攻撃者が選んだCN/DNを持つ自己署名証明書を提示し、任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 環境で証明書失効が重要な場合は、[CRLチェック](../network/crl.md) または [OCSPスタップリング](../network/ocsp.md) を評価してください。
- HTTP認証、データベース、その他の統合先など外部リソースへのアウトバウンド接続にTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に、少なくとも1つの認証機構を設定してください。デフォルトでは、EMQXは認証が有効でない場合、すべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有ユーザー名、パスワード、証明書の代わりに、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例えば、JWTの `clientid` クレームを検証したり、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングしたり、HTTP認証機構で不一致を拒否したり、認証機構を [Client-Info](./authn/cinfo.md) ルールと組み合わせたりします。これがない場合：
  - 資格情報漏洩により、攻撃者が無制限のセッションをランダムなクライアントIDで長い [Session Expiry Interval](../messaging/mqtt-concepts.md) 付きで作成し、アイドル状態の永続セッションが蓄積してブローカーのメモリを枯渇させる可能性があります。
  - 攻撃者が有効な資格情報を持ち、被害者のクライアントIDを知っていれば、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開し、攻撃者が同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード `0x8E`（`Session taken over`）付きの `DISCONNECT` パケットを送信します。
  - `Clean Start = 0` の場合、攻撃者は被害者のセッションを再開し、その既存のサブスクリプションを継承します。EMQXはサブスクリプション作成時に認可を行い、再開時の継承サブスクリプションを再評価しません。したがって攻撃者は自身の認可ルールで拒否されるメッセージも受信できます。

  クライアントIDを認証済みIDにバインドすることで、接続時に認証機構がID不一致を拒否し、この乗っ取りを防ぎます。この継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXは各パブリッシュ操作を現在のIDで認可します。
- X.509、JWT、SCRAM、または安全なデータベースに裏付けられたパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文の秘密情報ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重に見直してください。[Authorization](./authz/authz.md) を参照してください。
- ACLトピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、それらのID値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないように検証してください。未検証のIDが `clients/${clientid}/data` のようなテンプレートに代入されると、クライアントIDが `+` の場合はワイルドカードパターンに展開され（他のすべてのクライアントのサブトピックへのアクセスを許可）、`tenantA/+` や `/` を含む場合は割り当てられたサブツリーから逸脱します。上流で厳格なIDフォーマットを強制し、例えば [Client-Info](./authn/cinfo.md) ルール、JWTクレームパターン、HTTP認証機構での拒否などを用いてください。ACLに頼るのではなく、接続を拒否してください。
- HTTP認証（./authn/http.md）、HTTP認可（./authz/http.md）、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、EMQXが機密として認識するフィールドやヘッダーに各シークレットを格納してください。これにより、関連ログ、トレース、設定APIレスポンスのマスキング処理で値が `******` と表示されます。マスキングはフィールド名やヘッダー名で制御されます。HTTPヘッダーに配置する資格情報は標準の `Authorization`（または `Proxy-Authorization`）ヘッダーを使用してください。EMQXは常にこれらをマスキングします。その他の設定フィールドは `password`、`token`、`secret`、`secret_key`、`jwt` のような認識済みの機密キー名を付けてください。`x-custom-secret` のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug` レベルのログやエラーメッセージに平文で表示される可能性があります。
- 認可に依存する前に、許容的なデフォルトルールを削除または調整してください。
- ファイルベースのACLでは、適切な場合に `{deny, all}` でルールを終了し、`authorization.no_match = deny` を設定するなど、デフォルト拒否の姿勢を取ってください。[Use ACL File](./authz/file.md) を参照してください。
- 認可キャッシュ設定と認可順序を見直し、ポリシー変更が期待どおりに反映されるようにしてください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージなどの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルのレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- [Banned Clients](./blacklist.md) と [Flapping Detect](./flapping-detect.md) を使用して、悪質または不安定なクライアントを制御してください。
- クラスターリンクが有効な場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、その他すべてに拒否してください。[Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続をカバーしません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合は、ホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報の代わりにAPIキーを使用し、必要最小限のロールを付与し、可能な場合は有効期限を設定してください。[REST API](../admin/api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) を検討し、IDプロバイダーでMFAを利用可能な場合は強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。EMQXデータディレクトリ外に保存された証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログを有効にし、異常検知やインシデント対応のためにログとメトリクスをオブザーバビリティスタックに集約してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後にこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックでのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
