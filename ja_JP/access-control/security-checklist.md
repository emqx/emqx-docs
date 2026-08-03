# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティレイヤーごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、トポロジーの大幅な変更後、定期的なセキュリティレビューの際にご活用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を、接続規模に合わせて引き上げ、ノードが通常または悪意ある接続負荷下で失敗しないようにします。
- SYNフラッド保護、接続トラッキング容量、信頼できるインターフェースのみにリスナーを公開するなど、長時間接続されるMQTTトラフィック向けにTCPスタックとファイアウォールの強化を行います。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化されたリスナーを優先し、`1883` のような平文リスナーは内部または移行用ケースに限定してください。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- クラスター内で使用されるポートマッピングについては、セキュリティグループやファイアウォールルールでノード間ポートを制限します。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- ロードバランサーやTCPプロキシの背後にEMQXをデプロイする場合、実際のクライアントIPアドレスやクライアント証明書の詳細が必要なリスナーにのみ [Proxy Protocol](../deploy/cluster/lb.md) を有効にします。
- Proxy Protocolをリスナーで有効にした場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせて強制してください。そうしないと、ポートに直接到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを偽造し、任意のIDをなりすますことが可能になります。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーの秘密を使用します。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジーの変更後は、ファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番MQTTリスナーにTLSを使用してください。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートは無効化し、ステージング環境で最終的なリスナー設定を検証してください。
- 信頼されたCAまたは内部PKIによって発行された証明書を使用し、有効期限前にローテーションしてください。
- クライアント証明書によるデバイスID検証が必要な場合は相互TLSを有効にします。この場合、TLSハンドシェイク中にクライアント証明書のチェーンと存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制する必要があります（`verify = verify_peer`、`fail_if_no_peer_cert = true`）、かつ管理下のCAバンドルを使用してください。これがないと、クライアントは自己署名証明書を提示し、攻撃者が選んだCN/DNで任意のIDをなりすますことが可能です。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md) や [OCSPスタップリング](../network/ocsp.md) を評価してください。
- HTTP認証者、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にしてください。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、認証が有効でない場合、EMQXはすべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有のユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDに紐付けてください。例えば、JWTの `clientid` クレーム検証、証明書フィールドの [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) マッピング、HTTP認証者による不一致拒否、または認証者と [Client-Info](./authn/cinfo.md) ルールの組み合わせなどです。これがないと、漏洩した認証情報で攻撃者が無制限のセッションをランダムなクライアントIDで作成し、長い [Session Expiry Interval](../messaging/mqtt-concepts.md) によりアイドル状態の永続セッションが蓄積され、ブローカーのメモリを枯渇させる恐れがあります。
- X.509、JWT、SCRAM、または安全なデータベースに基づくパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md) を参照してください。
- ACLトピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、これらのID値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないように検証してください。検証されていないIDが `clients/${clientid}/data` のようなテンプレートに代入されると、クライアントIDが `+` の場合はワイルドカードパターンに展開されて他のクライアントのサブトピックすべてにアクセス可能となり、`tenantA/+` や `/` を含む場合は割り当てられたサブツリーから逸脱します。上流で厳格なIDフォーマットを強制し、例えば [Client-Info](./authn/cinfo.md) ルール、JWTクレームパターン、HTTP認証者での拒否などを利用してください。不正な代入をACLに頼るのではなく、接続を拒否してください。
- HTTP認証（./authn/http.md）、HTTP認可（./authz/http.md）、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、秘密情報をEMQXが機密として認識するフィールドやヘッダーに格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が `******` とマスクされます。マスクはフィールド名やヘッダー名で判別され、HTTPヘッダーの場合は標準の `Authorization`（または `Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドでは、`password`、`token`、`secret`、`secret_key`、`jwt` のような認識済みの機密キー名を使用してください。`x-custom-secret` のような非標準カスタムヘッダーや慣例外のフィールド名は認識されず、`debug` レベルのログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切に拒否デフォルトの姿勢を取り、ルールを `{deny, all}` で終わらせたり、`authorization.no_match = deny` を設定してください。[Use ACL File](./authz/file.md) を参照してください。
- 認可キャッシュ設定や認可者の順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用を制限してください。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナーレベルのレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- 悪質または不安定なクライアントを抑制するために、[Banned Clients](./blacklist.md) と [Flapping Detect](./flapping-detect.md) を活用してください。
- Cluster Linkingを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用のCluster Linking ClientIDに制限し、その他すべてに対して拒否してください。[Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を確認してください。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定し、管理者アクセスにはHTTPSを推奨します。可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証します。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合は、ホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報ではなくAPIキーを使用し、最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) を検討し、IDプロバイダーでMFAを利用可能な場合は強制してください。
- 定期的なバックアップをスケジュールし、復元手順のリハーサルを行ってください。EMQXデータディレクトリ外に保存されている証明書やACLファイルは別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログが利用可能な場合は有効化し、異常検知やインシデント対応のためにログやメトリクスをオブザーバビリティスタックに集中管理してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後は、このチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
