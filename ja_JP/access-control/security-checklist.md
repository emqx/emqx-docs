# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、大規模なトポロジー変更後、定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または悪意のある接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を接続規模に合わせて引き上げます。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化します。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`などの平文リスナーは内部または移行用途に限定してください。[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限してください。[Cluster Security](../deploy/cluster/security.md)を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーのみに[Proxy Protocol](../deploy/cluster/lb.md)を有効にします。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開してください。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`を設定し、ネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケットなど）と組み合わせて強制します。そうしないと、ポートに直接アクセスしたクライアントが任意のピア証明書フィールドを持つPROXY v2フレームを作成し、任意のIDをなりすますことが可能です。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の機密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../deploy/cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番のMQTTリスナーでは、トラフィックが信頼できないネットワークを越える場合はTLSを使用してください。[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートは無効化し、最終的なリスナー設定をステージング環境で検証してください。
- 信頼されたCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- クライアント証明書によるデバイス認証が必要な場合は相互TLSを有効にします。このモデルでは、TLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制しなければなりません（`verify = verify_peer`、`fail_if_no_peer_cert = true`）かつ管理下のCAバンドルを使用してください。これを行わないと、クライアントは自己署名証明書を提示して攻撃者が選択したCN/DNで任意のIDをなりすますことが可能です。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md)や[OCSPスタップリング](../network/ocsp.md)の導入を検討してください。
- HTTP認証、データベース、その他の統合先へのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、EMQXは認証が有効でない場合すべてのクライアントの接続を許可します。[Authentication](./authn/authn.md)を参照してください。
- 共有のユーザー名、パスワード、証明書ではなく、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みのIDにバインドしてください。例えば、JWTの`clientid`クレームの検証、証明書フィールドの`peer_cert_as_clientid`によるマッピング、HTTP認証機構での不一致拒否、[Client-Info](./authn/cinfo.md)ルールとの組み合わせなどです。これがないと、漏洩した資格情報で攻撃者が無制限にセッションを作成し、長い[Session Expiry Interval](../messaging/mqtt-concepts.md)を持つランダムなクライアントIDでアイドル状態の永続セッションが蓄積され、ブローカーのメモリを枯渇させる恐れがあります。
- X.509、JWT、SCRAM、パスワードベース認証（安全なデータベースバックエンド）など、信頼モデルに合った認証機構を選択してください。
- パスワードベース認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`などの強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md)を参照してください。
- ACLトピックテンプレート内で`${clientid}`、`${username}`、`${client_attrs.X}`を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、これらのID値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないよう検証してください。検証されていないIDが`clients/${clientid}/data`のようなテンプレートに挿入されると、クライアントIDが`+`の場合は他のすべてのクライアントのサブトピックへのアクセスを許可するワイルドカードパターンに展開され、`tenantA/+`や`/`を含む場合は割り当てられたサブツリーを逸脱します。上流で厳格なIDフォーマットを強制し、例えば[Client-Info](./authn/cinfo.md)ルール、JWTクレームパターン、HTTP認証機構での拒否を行い、ACLに頼らず接続自体を拒否してください。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切な場合に`{deny, all}`でルールを終了し、`authorization.no_match = deny`を設定するなど、デフォルト拒否の姿勢を採用してください。[Use ACL File](./authz/file.md)を参照してください。
- 認可キャッシュ設定とオーソライザの順序を確認し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意のあるクライアントの影響を軽減するため、パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングメッセージ数などのMQTTリソース使用制限を検討してください。[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- 悪質または不安定なクライアントを抑制するために、[Banned Clients](./blacklist.md)や[Flapping Detect](./flapping-detect.md)を活用してください。
- クラスターリンクが有効な場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理者アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定し、管理者アクセスにはHTTPSを推奨します。可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター、ブリッジ、アクションのアウトバウンドターゲットを設定更新時に検証してください。これは委任管理者がルールエンジンリソースを作成・変更できる場合に特に重要です。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボードの資格情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、IDプロバイダーでMFAを利用可能な場合は強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md)を参照してください。
- 監査ログが利用可能な場合は有効化し、ログやメトリクスをオブザーバビリティスタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後には必ずこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効な証明書によるTLSハンドシェイク失敗、許可外トピックでのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
