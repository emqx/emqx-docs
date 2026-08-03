# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、主要なトポロジー変更後、定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または悪意ある接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げてください。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィックに対してTCPスタックとファイアウォールの強化を行ってください。
- クライアントが実際に必要とするリスナーのみを公開してください。信頼できないネットワーク上では、`8883` や `8084` のような暗号化されたリスナーを優先し、`1883` のような平文リスナーは内部または移行用ユースケースに制限してください。詳細は [Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限してください。詳細は [Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドしてください。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーでのみ [Proxy Protocol](../deploy/cluster/lb.md) を有効にしてください。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開してください。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせて強制してください。そうしないと、直接ポートにアクセスしたクライアントが任意のピア証明書フィールドを持つPROXY v2フレームを作成し、任意のIDを偽装する可能性があります。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーの秘密を使用してください。詳細は [Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にしてください。詳細は [Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。詳細は [Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼されたCAまたは内部PKIによって発行された証明書を使用し、有効期限前にローテーションしてください。
- クライアント証明書によってデバイスのIDを検証する必要がある場合は、相互TLSを有効にしてください。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。詳細は [X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書フィールドをMQTTのユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドルで強制してください。これがないと、クライアントは攻撃者が選択したCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。詳細は [Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md) または [OCSPスタップリング](../network/ocsp.md) の評価を行ってください。
- HTTP認証サーバー、データベース、その他の統合先など外部リソースへのアウトバウンド接続にTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。詳細は [Authentication](./authn/authn.md) を参照してください。
- 共有のユーザー名、パスワード、証明書ではなく、デバイス単位またはアプリケーション単位の資格情報を優先してください。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例えば、JWTの `clientid` クレームを検証したり、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングしたり、HTTP認証機が不一致を拒否したり、認証機と [Client-Info](./authn/cinfo.md) ルールを組み合わせたりします。これがないと、漏洩した資格情報で攻撃者がランダムなクライアントIDで無制限にセッションを作成し、長い [Session Expiry Interval](../messaging/mqtt-concepts.md) によりアイドル状態の永続セッションが蓄積されてブローカーのメモリを枯渇させる恐れがあります。
- X.509、JWT、SCRAM、または安全なデータベースに基づくパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文の秘密ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。詳細は [Authorization](./authz/authz.md) を参照してください。
- ACLトピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` が使用されている場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、それらのID値がMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）を含まないように検証してください。検証されていないIDが `clients/${clientid}/data` のようなテンプレートに代入されると、クライアントIDが `+` の場合はすべての他クライアントのサブトピックへのアクセスを許可するワイルドカードパターンに展開されたり、`tenantA/+` や `/` を含む場合は割り当てられたサブツリーから逸脱したアクセスが可能になります。上流で厳密なIDフォーマットを [Client-Info](./authn/cinfo.md) ルール、JWTクレームパターン、HTTP認証機の拒否などで強制し、ACLに任せず接続を拒否してください。
- 本番環境で認可に依存する前に、許容的なデフォルトルールを削除または調整してください。
- ファイルベースのACLでは、適切な場合は `{deny, all}` でルールを終了し、`authorization.no_match = deny` を設定するなど、デフォルト拒否の姿勢を取ってください。詳細は [Use ACL File](./authz/file.md) を参照してください。
- 認可キャッシュ設定や認可順序を確認し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意あるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージなどの制限を見直してください。詳細は [MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。詳細は [Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 必要に応じて [Banned Clients](./blacklist.md) や [Flapping Detect](./flapping-detect.md) を使用して悪質または不安定なクライアントを制御してください。
- クラスターリンクが有効な場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、他のすべてのクライアントからは拒否してください。詳細は [Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番使用前にデフォルトのダッシュボードパスワードを変更し、管理者アクセス権を持つユーザーを確認してください。詳細は [System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク上に限定してください。管理者アクセスにはHTTPSを推奨し、可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。詳細は [Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証してください。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続をカバーしません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合は、ホストレベルのイグレス制御を追加してください。詳細は [Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボードの資格情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な場合は有効期限を設定してください。詳細は [REST API](../admin/api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) を検討し、利用可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。EMQXデータディレクトリ外に保存された証明書やACLファイルは別途バックアップが必要です。詳細は [Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効にし、ログやメトリクスを可観測性スタックに集約して異常検知やインシデント対応に役立ててください。詳細は [Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、および [Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後には、このチェックリストを再実行してください。
- 匿名クライアントの拒否、無効な証明書によるTLSハンドシェイク失敗、許可されていないトピックでのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
