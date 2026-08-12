# セキュリティチェックリスト

このチェックリストは、EMQX のデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、主要なトポロジー変更後、および定期的なセキュリティレビューの際にご利用ください。

## フェーズ 1: インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの `LimitNOFILE` 設定を接続規模に合わせて引き上げ、ノードが通常または悪意のある接続負荷下で失敗しないようにします。
- 長時間接続される MQTT トラフィック向けに TCP スタックとファイアウォールの設定を強化し、SYN フラッド保護、接続追跡容量、信頼できるインターフェースのみでのリスナー公開を行います。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883` や `8084` のような暗号化されたリスナーを優先し、`1883` のような平文リスナーは内部または移行用途に限定します。[Listener Configuration](../configuration/listener.md) および [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md) を参照してください。
- ノード間通信ポートはセキュリティグループやファイアウォールルールで制限します。クラスター内で使用されるポートマッピングは [Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノードに複数のインターフェースがある場合、Erlang 分散トラフィックはプライベートネットワークインターフェースにのみバインドします。
- ロードバランサーや TCP プロキシの背後に EMQX をデプロイする場合、クライアントの実IPアドレスやクライアント証明書情報が必要なリスナーのみに [Proxy Protocol](../deploy/cluster/lb.md) を有効にします。
- Proxy Protocol を有効にしたリスナーのアドレスとポートは、指定されたプロキシまたはロードバランサーにのみ公開します。EMQX 側では `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unix ソケット）を組み合わせて強制します。これを怠ると、ポートに直接アクセスしたクライアントが任意の peer-cert フィールドを持つ PROXY v2 フレームを作成し、任意のIDをなりすます可能性があります。
- WebSocket リスナー（`ws` または `wss`）が `x-forwarded-for` ヘッダーを書き換える信頼できるプロキシの背後にない場合、`listeners.{type}.{name}.websocket.proxy_address_header = ""`（および `websocket.proxy_port_header = ""`）に設定し、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査ログが実際の TCP ピアアドレスを使用するようにします。ヘッダーが有効な場合、派生元IPはクライアント提供の値であり、信頼できるプロキシがヘッダーを書き換えない限り保護されません。詳細は [Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners) をご覧ください。

## フェーズ 2: Erlang とクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーの秘密を使用します。[Set Node Cookie](../deploy/cluster/security.md#set-node-cookie) を参照してください。
- `emqx.conf`、ACL ファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- 可能な限り、シークレット型フィールドはインライン値ではなく `file://` 参照として保存します。SSL キーのパスフレーズ、ブリッジやコネクターのパスワード、API キーなど、シークレットとして文書化されているフィールドには、値を `file:///path/to/secret` に設定します。EMQX は起動時および設定リロードのたびにそのファイルからシークレットを読み込みます。これにより、平文のシークレットが設定ファイル、API リクエストボディ、設定バックアップ、バージョン管理に残らず、設定を共有またはエクスポートする際の漏えいリスクを低減できます。[ファイルからシークレットを読み込む](../configuration/secret-from-file.md) を参照してください。
- クラスター間通信ポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合は TLS を有効にします。[Cluster Security](../deploy/cluster/security.md) を参照してください。
- ノード追加、ネットワーク移動、デプロイメントトポロジーの変更後はファイアウォールルール、証明書、クラスター参加制御を再確認します。

## フェーズ 3: トランスポートセキュリティ

- トラフィックが信頼できないネットワークを越える場合は、本番 MQTT リスナーに TLS を使用します。[Network and TLS](../network/overview.md) を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、ステージング環境で最終リスナー設定を検証します。
- 信頼された CA または内部 PKI によって発行された証明書を使用し、有効期限前にローテーションします。
- デバイスの識別をクライアント証明書で検証する場合は相互 TLS を有効にします。このモデルでは TLS ハンドシェイク時にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md) を参照してください。
- ピア証明書のフィールドを MQTT のユーザー名またはクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ず mTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`）を CA バンドル付きで強制します。これがないと、クライアントが攻撃者が選択した CN/DN を持つ自己署名証明書を提示し、任意のIDをなりすます可能性があります。空ユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous` を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping) を参照してください。
- 証明書失効が重要な環境では、[CRL チェック](../network/crl.md) または [OCSP ステープリング](../network/ocsp.md) の評価を行います。
- HTTP 認証者、データベース、その他の統合先など外部リソースへのアウトバウンド接続には TLS を有効にします。

## フェーズ 4: MQTT アクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定します。デフォルトでは、EMQX は認証が有効でない場合すべてのクライアントの接続を許可します。[Authentication](./authn/authn.md) を参照してください。
- 共有のユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の資格情報を推奨します。
- 認証機構が許す場合は、MQTT クライアントIDを認証済みのIDにバインドします。たとえば、JWT の `clientid` クレームを検証したり、証明書フィールドを [`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping) でマッピングしたり、HTTP 認証者が不一致を拒否したり、認証者と [Client-Info](./authn/cinfo.md) ルールを組み合わせたりします。これを行わない場合：
  - 資格情報が漏えいすると、攻撃者は長い [Session Expiry Interval](../messaging/mqtt-concepts.md) を持つ無制限のセッションをランダムなクライアントIDで作成でき、アイドル状態のパーシステントセッションが蓄積してブローカーのメモリを枯渇させる可能性があります。
  - 攻撃者が有効な資格情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTT はクライアントIDのみでセッションを識別・再開するため、攻撃者が同じクライアントIDで接続すると EMQX は被害者を切断します。MQTT 5.0 クライアントの場合、EMQX は理由コード `0x8E`（`Session taken over`）付きの `DISCONNECT` パケットを送信します。
  - `Clean Start = 0` の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを引き継ぎます。EMQX はサブスクリプション作成時に認可を行い、再開時の継承サブスクリプションを再評価しません。したがって、攻撃者は自身の認可ルールでは拒否されるメッセージを受信できます。

  クライアントIDを認証済みIDにバインドすると、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。この継承サブスクリプションのリスクはパブリッシュには影響しません。EMQX はパブリッシュごとに現在のIDで認可を行うためです。
- X.509、JWT、SCRAM、パスワード認証（安全なデータベースバックエンド付き）など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt` や `pbkdf2` のような強力なアルゴリズムを推奨します。
- トピック権限は可能な限り狭く定義し、ワイルドカードの使用は慎重にレビューしてください。[Authorization](./authz/authz.md) を参照してください。
- ACL トピックテンプレート内で `${clientid}`、`${username}`、`${client_attrs.X}` を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders) 参照）、それらのID値に MQTT トピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないよう検証してください。検証されていない ID が `clients/${clientid}/data` のようなテンプレートに代入されると、クライアントIDが `+` の場合はワイルドカードパターンとなり他クライアントのサブトピックへのアクセスを許可し、`tenantA/+` や `/` を含む場合は割り当てられたサブツリーから逸脱します。厳格な ID 形式は、[Client-Info](./authn/cinfo.md) ルール、JWT クレームパターン、HTTP 認証者での拒否など上流で強制し、ACL による不正な代入検出に頼らず接続を拒否してください。
- HTTP 認証（./authn/http.md）、HTTP 認可（./authz/http.md）、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、EMQX が機密情報として認識するフィールドやヘッダーにシークレットを格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が `******` にマスクされます。マスクはフィールド名やヘッダー名で制御されます。HTTP ヘッダーに資格情報を置く場合は標準の `Authorization`（または `Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドは `password`、`token`、`secret`、`secret_key`、`jwt` など認識されるキー名を付けてください。`x-custom-secret` のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug` レベルログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースの ACL では、適切な場合はデフォルト拒否の姿勢を採用し、ルールの末尾に `{deny, all}` を付けたり、`authorization.no_match = deny` を設定したりします。[Use ACL File](./authz/file.md) を参照してください。
- 信頼できないまたは公開ネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは `ignore`）の設定を検討してください。クライアントが認可されていないトピックへのパブリッシュやサブスクライブを試みた場合、EMQX は接続を切断します。これに [flapping detection](./flapping-detect.md) を組み合わせると、繰り返し接続して認可拒否を引き起こすクライアントを自動的に禁止できます。`deny_action` はグローバル設定であり、拒否された操作を試みる正当なクライアントも切断されるため、通常は認可されたトピックのみでパブリッシュ・サブスクライブするクライアントに適用してください。フラッピング検出の閾値は通常の再接続ラッシュ時に誤禁止しないよう調整してください。[Authorization](./authz/authz.md) を参照してください。
- 認可キャッシュ設定と認可順序を見直し、ポリシー変更が期待通りに反映されるようにします。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTT リソース使用を制限します。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングメッセージ数などの制限を確認してください。[MQTT Configuration](../configuration/mqtt.md) を参照してください。
- 必要に応じてリスナー単位のレート制御を適用し、接続やパブリッシュのバーストを制限します。[Rate Limiter Configuration](../configuration/limiter.md) を参照してください。
- [Banned Clients](./blacklist.md) と [Flapping Detect](./flapping-detect.md) を利用して、悪質または不安定なクライアントを制御します。
- Cluster Linking を有効にしている場合、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/` コントロールネームスペースを専用の Cluster Linking クライアントIDに制限し、それ以外は拒否します。[Secure Cluster Linking](../cluster-linking/security.md) を参照してください。

## フェーズ 5: 管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を確認します。[System](../dashboard/system.md) を参照してください。
- ダッシュボードは信頼できるネットワーク内に限定し、管理者アクセスには HTTPS を推奨します。可能な場合はダッシュボードリスナーを localhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md) を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security** で SSRF 保護を有効にし、コネクター設定のテスト、作成、更新時に HTTP および MQTT コネクターのターゲットを検証します。EMQX 6.0.4 以降、このポリシーは他のコネクタータイプやランタイム接続をカバーしません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合はホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security) および [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。
- 管理 API を公開する場合は、ダッシュボード認証情報の代わりに API キーを使用し、必要最小限のロールを付与し、可能な限り有効期限を設定してください。[REST API](../admin/api.md) および [System](../dashboard/system.md#api-key) を参照してください。
- EMQX Enterprise を利用している場合は、管理ユーザー向けに [Single Sign-On (SSO)](../dashboard/sso.md) を検討し、利用可能な場合は ID プロバイダーで MFA を強制してください。
- 定期的なバックアップをスケジュールし、復元手順をリハーサルしてください。証明書や ACL ファイルが EMQX データディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../operations/backup-restore.md) を参照してください。
- 監査ログを有効にし、ログやメトリクスをオブザーバビリティスタックに集約して異常検知やインシデント対応に活用してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md) を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後はこのチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書による TLS ハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
