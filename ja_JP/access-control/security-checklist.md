# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、主要なトポロジー変更後、および定期的なセキュリティレビューの際にご利用ください。

## フェーズ1：インフラストラクチャとOS

- ノードが通常または悪意のある接続負荷下で失敗しないように、オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げてください。
- SYNフラッド保護、コネクショントラッキング容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの設定を強化してください。
- クライアントが実際に必要とするリスナーのみを公開してください。信頼できないネットワークでは、`8883` や `8084` のような暗号化リスナーを優先し、`1883` のような平文リスナーは内部または移行用途に限定してください。[Listener Configuration](../configuration/listener.md) と [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- セキュリティグループやファイアウォールルールでノード間ポートを制限してください。クラスター内で使用されるポートマッピングについては [Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合は、Erlang分散トラフィックをプライベートネットワークインターフェースのみにバインドしてください。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書の詳細が必要なリスナーにのみ [Proxy Protocol](../deploy/cluster/lb.md) を有効にしてください。
- リスナーでProxy Protocolを有効にした場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーにのみ公開してください。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせてこれを強制してください。そうしないと、ポートに直接到達したクライアントが任意のピア証明書フィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用してください。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の機密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護してください。
- クラスター間通信に使用するポートは内部に限定し、トラフィックが信頼できないネットワークやパブリッククラウド境界を越える場合はTLSを有効にしてください。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク変更、デプロイトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効にし、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼されたCAまたは内部PKIが発行した証明書を使用し、有効期限前にローテーションしてください。
- クライアント証明書によるデバイス認証が必要な場合は相互TLSを有効にしてください。このモデルではTLSハンドシェイク時にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書フィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制してください（`verify = verify_peer`、`fail_if_no_peer_cert = true`）かつ管理下のCAバンドルを使用してください。これがないと、クライアントは自己署名証明書を使って攻撃者が選んだCN/DNで任意のIDを偽装できます。空ユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md) や [OCSPスタップリング](../network/ocsp.md) の導入を検討してください。
- HTTP認証器、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、EMQXは認証が有効でない場合すべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有ユーザー名、パスワード、証明書ではなく、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例として、JWTの `clientid` クレーム検証、証明書フィールドの `peer_cert_as_clientid` マッピング、HTTP認証器による不一致拒否、[Client-Info](./authn/cinfo.md) ルールとの組み合わせなどがあります。これがないと、資格情報漏洩時に攻撃者が無制限にランダムなクライアントIDで多数のセッションを作成し、長い [Session Expiry Interval](../messaging/mqtt-concepts.md) によりアイドル状態の永続セッションが蓄積されてブローカーのメモリを枯渇させる恐れがあります。
- X.509、JWT、SCRAM、パスワード認証（安全なデータベースバックエンド）など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md) を参照してください。
- ACLトピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、これらのID値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないよう検証してください。未検証のIDが `clients/${clientid}/data` のようなテンプレートに代入されると、クライアントIDが `+` の場合はワイルドカードパターンに展開されて他クライアントのサブトピックにアクセス可能になり、`tenantA/+` や `/` を含む場合は割り当てられたサブツリーから逸脱します。上流で厳格なIDフォーマットを強制し、[Client-Info](./authn/cinfo.md) ルール、JWTクレームパターン、HTTP認証器による拒否などで対応してください。ACLに任せず、接続自体を拒否してください。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切な場合はデフォルト拒否の姿勢をとり、ルールを `{deny, all}` で終端し、`authorization.no_match = deny` を設定してください。[Use ACL File](./authz/file.md) を参照してください。
- 認可キャッシュ設定や認可順序をレビューし、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意あるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルでレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- [Banned Clients](./blacklist.md) や [Flapping Detect](./flapping-detect.md) を使用して、悪質または不安定なクライアントを制御してください。
- クラスターリンクを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のクラスターリンククライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ5：管理と保守

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理者アクセス権を確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定してください。管理者アクセスにはHTTPSを推奨し、可能な限りダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター、ブリッジ、アクションのアウトバウンドターゲットを設定更新時に検証してください。これは委任管理者がルールエンジンリソースを作成・変更できる場合に特に重要です。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) と [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報の代わりにAPIキーを使用し、最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md) と [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) の導入を検討し、可能な場合はIDプロバイダーで多要素認証（MFA）を強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。EMQXデータディレクトリ外に保存されている証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効にし、異常検知やインシデント対応のためにログとメトリクスをオブザーバビリティスタックに集中管理してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後は、このチェックリストを再実行してください。
- 匿名クライアントの拒否、無効な証明書によるTLSハンドシェイク失敗、許可されていないトピックでのパブリッシュやサブスクライブ拒否など、想定される失敗モードを本番切り替え前に検証してください。
