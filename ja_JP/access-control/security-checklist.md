# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前にレビューするためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初期展開時、大規模なトポロジー変更後、定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または攻撃的な接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げます。
- SYNフラッド防御、コネクショントラッキング容量、信頼できるインターフェースのみにリスナーを公開するなど、長時間維持されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化します。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化リスナーを優先し、`1883` のような平文リスナーは内部または移行用のユースケースに限定してください。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限します。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書の詳細が必要なリスナーにのみ [Proxy Protocol](../deploy/cluster/lb.md) を有効にします。
- Proxy Protocolがリスナーで有効になっている場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開してください。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制します。そうしないと、ポートに直接アクセスしたクライアントが任意のピア証明書フィールドを持つPROXY v2フレームを作成し、任意のIDを偽装する可能性があります。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用します。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートは無効化し、最終的なリスナー設定をステージング環境で検証してから展開してください。
- 信頼できるCAまたは内部PKIから発行された証明書を使用し、有効期限前にローテーションしてください。
- クライアント証明書によるデバイス認証が必要な場合は相互TLSを有効にします。このモデルではTLSハンドシェイク時にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書フィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドルで強制してください。これがないと、クライアントは自己署名証明書を使って攻撃者が選んだCN/DNで任意のIDを偽装できます。空のユーザー名の場合の追加保護として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 環境で証明書失効が重要な場合は、[CRLチェック](../network/crl.md) または [OCSPスタップリング](../network/ocsp.md) を検討してください。
- HTTP認証、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- パブリックリスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有ユーザー名、パスワード、証明書の代わりに、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- X.509、JWT、SCRAM、または安全なデータベースに裏付けられたパスワード認証など、信頼モデルに合った認証方式を選択してください。
- パスワード認証を使用する場合は、平文の秘密情報ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限はできるだけ狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md) を参照してください。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLを使用する場合は、適切にデフォルト拒否の姿勢を取り、ルールの最後に `{deny, all}` を付けるか、`authorization.no_match = deny` を設定してください。[Use ACL File](./authz/file.md) を参照してください。
- ポリシー変更が期待通りに反映されるよう、認可キャッシュ設定とオーソライザーの順序を確認してください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージなどの制限を見直してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 接続やパブリッシュのバーストを制限するために必要に応じてリスナーレベルのレート制御を適用してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 悪質または不安定なクライアントを制御するために、[Banned Clients](./blacklist.md) および [Flapping Detect](./flapping-detect.md) を活用してください。
- Cluster Linkingを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のCluster LinkingクライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、可能であればダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター、ブリッジ、アクションのアウトバウンドターゲットを設定更新時に検証してください。これは委任管理者がルールエンジンリソースを作成・変更できる場合に特に重要です。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボードの資格情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) の利用を検討し、可能な場合はIDプロバイダーでMFAを強制してください。
- 定期的なバックアップをスケジュールし、復元手順のリハーサルを行ってください。EMQXデータディレクトリ外に保存された証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効化し、ログとメトリクスをオブザーバビリティスタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書のローテーション、リスナーの変更、ロードバランサーの更新、クラスター拡張、バックアップポリシーの変更、認証・認可チェーンの変更などがあった場合は、このチェックリストを再実行してください。
- 本番切り替え前に、匿名クライアントの拒否、無効な証明書によるTLSハンドシェイク失敗、許可されていないトピックでのパブリッシュやサブスクライブの拒否など、期待される失敗モードを検証してください。
