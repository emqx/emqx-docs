# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初期展開時、大規模なトポロジー変更後、および定期的なセキュリティレビューの際にご活用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または悪意のある接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げます。
- SYNフラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィックに対してTCPスタックとファイアウォールの設定を強化します。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化リスナーを優先し、`1883` のような平文リスナーは内部または移行用ケースに限定してください。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- セキュリティグループやファイアウォールルールでノード間ポートを制限します。クラスター内部で使用されるポートマッピングについては [Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーにのみ [Proxy Protocol](../deploy/cluster/lb.md) を有効にします。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせて強制してください。そうしないと、ポートに直接到達したクライアントが任意のピア証明書フィールドを持つPROXY v2フレームを作成し、任意のIDを偽装する恐れがあります。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用します。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- クラスタリングポートは内部に限定し、トラフィックが信頼性の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼されたCAまたは内部PKIによって発行された証明書を使用し、有効期限前にローテーションしてください。
- デバイスのIDをクライアント証明書で検証する必要がある場合は相互TLSを有効にします。このモデルでは、TLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書フィールドをMQTTのユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）をCAバンドル付きで強制してください。これがないと、クライアントは攻撃者が選択したCN/DNを持つ自己署名証明書を提示して任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md) または [OCSPスタップリング](../network/ocsp.md) の評価を行ってください。
- HTTP認証者、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有のユーザー名、パスワード、証明書ではなく、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例として、JWTの `clientid` クレームを検証する、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングする、HTTP認証者で不一致を拒否する、または認証者を [Client-Info](./authn/cinfo.md) ルールと組み合わせる方法があります。これがないと、漏洩した資格情報により攻撃者が無制限のセッションを任意のクライアントIDで作成し、長い [Session Expiry Interval](../messaging/mqtt-concepts.md) によってアイドル状態の永続セッションが蓄積され、ブローカーのメモリが枯渇する恐れがあります。
- X.509、JWT、SCRAM、または安全なデータベースに裏付けられたパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文の秘密情報ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重に見直してください。[Authorization](./authz/authz.md) を参照してください。
- ACLトピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、MQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）を含まないようにこれらのID値を検証してください。未検証のIDが `clients/${clientid}/data` のようなテンプレートに代入されると、クライアントIDが `+` の場合はワイルドカードパターンに展開されて他のすべてのクライアントのサブトピックへのアクセスを許可したり、`tenantA/+` や `/` を含む場合は割り当てられたサブツリーから逸脱したアクセスが可能になります。上流で厳格なID形式を強制し、例えば [Client-Info](./authn/cinfo.md) ルール、JWTクレームパターン、HTTP認証者での拒否などを用いてください。ACLに頼って不正な置換を検出するのではなく、接続を拒否してください。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切な場合に `{deny, all}` でルールを終了し、`authorization.no_match = deny` を設定するなど、デフォルト拒否の姿勢を取ってください。[Use ACL File](./authz/file.md) を参照してください。
- 認可キャッシュ設定や認可者の順序を見直し、ポリシー変更が期待通りに反映されることを確認してください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージ数などの制限を見直してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルのレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 必要に応じて [Banned Clients](./blacklist.md) や [Flapping Detect](./flapping-detect.md) を使用して悪質または不安定なクライアントを制御してください。
- クラスターリンクが有効な場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を持つユーザーを確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、可能な限りダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター、ブリッジ、アクションのアウトバウンドターゲットを設定更新時に検証してください。委任管理者がルールエンジンリソースを作成・変更できる場合に特に重要です。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボードの資格情報ではなくAPIキーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) の利用を検討し、利用可能な場合はIDプロバイダーでMFAを強制してください。
- 定期的なバックアップをスケジュールし、リストア手順をリハーサルしてください。EMQXデータディレクトリ外に保存された証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効にし、ログとメトリクスを可観測性スタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後にこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効な証明書によるTLSハンドシェイク失敗、許可されていないトピックでのパブリッシュやサブスクライブの拒否など、期待される失敗モードを本番切り替え前に検証してください。
