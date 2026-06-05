# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、主要なトポロジー変更後、および定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を接続規模に合わせて引き上げ、ノードが通常または悪意ある接続負荷下で失敗しないようにします。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみにリスナーを公開するなど、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化します。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`のような平文リスナーは内部または移行用途に制限してください。[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- クラスター内で使用されるポートマッピングについては、[Cluster Security](../deploy/cluster/security.md)を参照のうえ、セキュリティグループやファイアウォールルールでノード間ポートを制限します。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書の詳細が必要なリスナーでのみ[Proxy Protocol](../deploy/cluster/lb.md)を有効にします。
- Proxy Protocolをリスナーで有効にした場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`を設定し、ネットワークレベル（ファイアウォール、プライベートネットワーク、Unixソケット）でも制御してください。これを怠ると、ポートに直接アクセスしたクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーの秘密を使用します。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、およびその他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../deploy/cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーでTLSを使用してください。[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼されたCAまたは内部PKIによって発行された証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する場合は相互TLSを有効にします。このモデルではTLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドルで強制してください。これがないと、クライアントは攻撃者が選んだCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md)や[OCSPステープリング](../network/ocsp.md)を評価してください。
- HTTP認証、データベース、その他の統合先など外部リソースへのアウトバウンド接続にもTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、EMQXは認証が有効でない場合、すべてのクライアントの接続を許可します。[Authentication](./authn/authn.md)を参照してください。
- 共有のユーザー名、パスワード、証明書の代わりに、デバイスまたはアプリケーションごとの資格情報を推奨します。
- 信頼モデルに合った認証方式を選択してください。X.509、JWT、SCRAM、または安全なデータベースに基づくパスワード認証などがあります。
- パスワード認証を使用する場合は、平文の秘密情報ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md)を参照してください。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLを使用する場合は、適切な箇所でデフォルト拒否の姿勢を取り、ルールの末尾に`{deny, all}`を付けたり、`authorization.no_match = deny`を設定してください。[Use ACL File](./authz/file.md)を参照してください。
- 認可キャッシュ設定とオーソライザーの順序を確認し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意あるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージなどの制限を見直してください。[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- 悪質または不安定なクライアントを制御するために、[Banned Clients](./blacklist.md)や[Flapping Detect](./flapping-detect.md)を活用してください。
- Cluster Linkingを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`コントロールネームスペースを専用のCluster Linking ClientIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理者アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワークのみに限定し、管理者アクセスにはHTTPSを推奨します。可能であれば、ダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター、ブリッジ、アクションのアウトバウンドターゲットを設定更新時に検証してください。これは委任管理者がルールエンジンリソースを作成・変更できる場合に特に重要です。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボードの資格情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、利用可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、リストア手順をリハーサルしてください。EMQXデータディレクトリ外に保存された証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md)を参照してください。
- 可能な場合は監査ログを有効にし、異常検知やインシデント対応のためにログやメトリクスをオブザーバビリティスタックに集約してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後には、このチェックリストを再度実行してください。
- 本番切り替え前に、匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックでのパブリッシュやサブスクライブの拒否など、想定される失敗モードを検証してください。
