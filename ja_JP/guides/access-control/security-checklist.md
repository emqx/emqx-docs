# セキュリティチェックリスト

このチェックリストは、EMQXのデプロイメントを本番トラフィックに公開する前に確認するためのものです。セキュリティ層ごとに整理されており、オペレーティングシステムからダッシュボードまでの全経路を検証できます。初回展開時、主要なトポロジー変更後、および定期的なセキュリティレビューの一環としてご利用ください。

## フェーズ1：インフラストラクチャとOS

- オペレーティングシステムのファイルディスクリプタ制限およびサービスレベルの`LimitNOFILE`設定を、接続規模に合わせて引き上げ、ノードが通常または悪意のある接続負荷下で失敗しないようにします。
- SYNフラッド保護、接続トラッキング容量、信頼できるインターフェースのみでのリスナー公開など、長時間接続されるMQTTトラフィックに対してTCPスタックとファイアウォールの強化を行います。
- クライアントが実際に必要とするリスナーのみを公開します。信頼できないネットワークでは、`8883`や`8084`などの暗号化リスナーを優先し、`1883`のような平文リスナーは内部または移行用ケースに制限します。[Listener Configuration](../configuration/listener.md)および[Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md)を参照してください。
- ノード間ポートはセキュリティグループやファイアウォールルールで制限します。クラスター内で使用されるポートマッピングについては[Cluster Security](../cluster/security.md)を参照してください。
- ノードに複数のインターフェースがある場合、Erlang分散トラフィックはプライベートネットワークインターフェースのみにバインドします。
- EMQXをロードバランサーやTCPプロキシの背後に配置する場合、実際のクライアントIPアドレスやクライアント証明書情報が必要なリスナーのみで[Proxy Protocol](../cluster/lb.md)を有効にします。
- Proxy Protocolがリスナーで有効な場合、そのアドレスとポートは指定されたプロキシまたはロードバランサーのみに公開します。EMQXでは`listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]`とネットワークレベルの制御（ファイアウォール、プライベートネットワーク、Unixソケット）を組み合わせて強制します。そうしないと、ポートに直接到達したクライアントが任意のpeer-certフィールドを持つPROXY v2フレームを作成し、任意のIDを偽装できます。
- WebSocketリスナー（`ws`または`wss`）が`x-forwarded-for`ヘッダーを書き換える信頼できるプロキシの背後にない場合、`listeners.{type}.{name}.websocket.proxy_address_header = ""`（および`websocket.proxy_port_header = ""`）に設定し、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査ログが実際のTCPピアアドレスを使用するようにします。ヘッダーが有効な場合、派生元IPはクライアント提供であり、信頼できるプロキシがヘッダーを書き換えない限り保護されません。インバウンドヘッダーに追記するだけのプロキシは保護しません。[Forwarded Client Address](../configuration/listener.md#forwarded-client-address-websocket-listeners)を参照してください。

## フェーズ2：Erlangとクラスター

- クラスター内のすべてのノードでデフォルトのノードクッキーを置き換え、すべてのメンバーで同じ高エントロピーのシークレットを使用します。[Set Node Cookie](../cluster/security.md#set-node-cookie)を参照してください。
- `emqx.conf`、ACLファイル、証明書、秘密鍵、その他の秘密情報は厳格なファイル権限と安全なシークレット管理プロセスで保護します。
- 秘密情報のフィールドは可能な限りインライン値ではなく`file://`参照として保存します。SSLキーのパスフレーズ、ブリッジやコネクターのパスワード、APIキーなど秘密として文書化されているフィールドは、`file:///path/to/secret`の形式に設定し、EMQXが起動時およびリロード時にファイルから読み込むようにします。これにより、平文の秘密情報が設定ファイル、APIリクエストボディ、設定バックアップ、バージョン管理に含まれず、共有やエクスポート時の漏洩リスクを低減します。[Load Secrets from a File](../configuration/secret-from-file.md)を参照してください。
- クラスタリングポートは内部に限定し、トラフィックが信頼度の低いネットワークやパブリッククラウド境界を越える場合はノード間通信にTLSを有効にします。[Cluster Security](../cluster/security.md)を参照してください。
- ノード追加、ネットワーク移動、デプロイトポロジー変更後はファイアウォールルール、証明書、クラスター参加制御を再確認してください。

## フェーズ3：トランスポートセキュリティ

- 本番MQTTリスナーではトラフィックが信頼できないネットワークを越える場合はTLSを使用します。[Network and TLS](../network/overview.md)を参照してください。
- 組織のセキュリティ基準に従い、レガシープロトコルバージョンや弱い暗号スイートを無効化し、最終的なリスナー設定をステージング環境で検証してから展開してください。
- 信頼できるCAまたは社内PKIが発行した証明書を使用し、有効期限前にローテーションします。
- クライアント証明書でデバイスIDを検証する場合は相互TLSを有効にします。この場合、TLSハンドシェイク中にクライアント証明書チェーンと証明書の存在を検証します。[X.509 Certificate Authentication](./authn/x509.md)を参照してください。
- ピア証明書のフィールドをMQTTのユーザー名やクライアントIDにマッピングする場合（`peer_cert_as_username` / `peer_cert_as_clientid`）、リスナーは必ずmTLSを強制します（`verify = verify_peer`、`fail_if_no_peer_cert = true`）かつ管理するCAバンドルを使用してください。これがないと、クライアントが攻撃者が選択したCN/DNを持つ自己署名証明書を提示し、任意のIDを偽装できます。空のユーザー名の場合の追加対策として、`listeners.{type}.{name}.enable_authn = quick_deny_anonymous`を設定してください。[Certificate Information Mapping](./authn/x509.md#certificate-information-mapping)を参照してください。
- 証明書失効が重要な環境では、[CRLチェック](../network/crl.md)や[OCSPスタップリング](../network/ocsp.md)を評価してください。
- HTTP認証、データベース、その他の統合先など外部リソースへのアウトバウンド接続にはTLSを有効にします。

## フェーズ4：MQTTアクセス制御とリソース保護

- 公開リスナーを公開する前に少なくとも1つの認証機構を設定してください。デフォルトでは、EMQXは認証が有効でない場合、すべてのクライアントの接続を許可します。[Authentication](./authn/authn.md)を参照してください。
- 共有ユーザー名、パスワード、証明書よりも、デバイス単位またはアプリケーション単位の認証情報を推奨します。
- 認証機構が許す場合は、MQTTクライアントIDを認証済みIDにバインドしてください。例えば、JWTの`clientid`クレームを検証したり、証明書フィールドを[`peer_cert_as_clientid`](./authn/x509.md#certificate-information-mapping)でマッピングしたり、HTTP認証機構で不一致を拒否したり、[Client-Info](./authn/cinfo.md)ルールと組み合わせたりします。これをしないと以下のリスクがあります。
  - 認証情報が漏洩すると、攻撃者が無制限にセッションを作成し、長い[Session Expiry Interval](../../get-started/messaging/mqtt-concepts.md)でアイドル状態のパーシステントセッションが蓄積し、ブローカーのメモリを枯渇させる恐れがあります。
  - 攻撃者が有効な認証情報を持ち、被害者のクライアントIDを知っている場合、被害者のセッションを乗っ取れます。MQTTはクライアントIDのみでセッションを識別・再開するため、同じクライアントIDで接続するとEMQXは被害者を切断します。MQTT 5.0クライアントの場合、EMQXは理由コード`0x8E`（`Session taken over`）付きの`DISCONNECT`パケットを送信します。
  - `Clean Start = 0`の場合、攻撃者は被害者のセッションを再開し、既存のサブスクリプションを継承します。EMQXはサブスクリプション作成時に認可し、再開時の継承サブスクリプションは再評価しません。攻撃者は自身の認可ルールで拒否されるメッセージを受信できます。

  クライアントIDを認証済みIDにバインドすることで、接続時に認証機構がID不一致を拒否し、この乗っ取りを防止します。継承サブスクリプションのリスクはパブリッシュには影響しません。EMQXは各パブリッシュ操作を現在のIDで認可します。
- X.509、JWT、SCRAM、または安全なデータベースに基づくパスワード認証など、信頼モデルに合った認証機構を選択してください。
- パスワード認証を使用する場合は、平文ではなくソルト付きパスワードハッシュを保存し、`bcrypt`や`pbkdf2`などの強力なアルゴリズムを推奨します。
- EMQXが使用するデータベースアカウントには、認証と認可に必要なデータのみ読み取り専用アクセスを付与してください。EMQXはこのデータを読み取るだけです。SQLバックエンドはプリペアドステートメントで`SELECT`文を実行し、MongoDBバックエンドは`find`クエリを発行し、Redis認証・認可用`cmd`は`HGET`と`HMGET`（または`HMGET`と`HGETALL`）のみを受け付けます。アカウントに書き込みや管理権限があると、攻撃者が認証情報を入手した場合や誤ったカスタムクエリにより認証データや認可ルールを変更される恐れがあります。テーブル作成やレコード管理には別の管理アカウントを使用してください。[Authentication](./authn/authn.md)および[Authorization](./authz/authz.md)を参照してください。
- トピック権限は可能な限り狭く設定し、ワイルドカードの使用は慎重に見直してください。[Authorization](./authz/authz.md)を参照してください。
- ACLトピックテンプレート内で`${clientid}`、`${username}`、`${client_attrs.X}`を使用する場合（[Authorization Placeholders](./authz/authz.md#authorization-placeholders)参照）、これらのID値にMQTTトピックのワイルドカード（`+`、`#`）やトピック区切り文字（`/`）が含まれないよう検証してください。検証されていないIDが`clients/${clientid}/data`のようなテンプレートに代入され、クライアントIDが`+`の場合はすべての他クライアントのサブトピックへのアクセスを許可するワイルドカードパターンに展開され、`tenantA/+`や`/`を含む場合は割り当てられたサブツリーから逸脱します。上流で[Client-Info](./authn/cinfo.md)ルール、JWTクレームパターン、HTTP認証機構で拒否するなど厳格なID形式を強制し、ACLに依存せず接続を拒否してください。
- HTTP認証、HTTP認可、データ統合コネクター、ブリッジ、アクションなど外部サービスへのアウトバウンドリクエスト設計時は、EMQXが機密情報として認識するフィールドやヘッダーに秘密情報を格納してください。これにより、関連ログ、トレース、設定APIレスポンスで値が`******`にマスクされます。マスクはフィールド名やヘッダー名で制御されます。HTTPヘッダーの認証情報は標準の`Authorization`（または`Proxy-Authorization`）ヘッダーを使用してください。その他の設定フィールドは`password`、`token`、`secret`、`secret_key`、`jwt`など認識される機密キー名を付けてください。`x-custom-secret`のような非標準カスタムヘッダーや慣習外のフィールド名は認識されず、`debug`レベルログやエラーメッセージに平文で表示される可能性があります。
- 本番環境で認可に依存する前に、許容的なデフォルトルールは削除または調整してください。
- ファイルベースのACLでは、適切に拒否デフォルトの姿勢を採用し、ルールを`{deny, all}`で終わらせたり、`authorization.no_match = deny`を設定したりしてください。[Use ACL File](./authz/file.md)を参照してください。
- 信頼できないまたはパブリックネットワークに公開するブローカーでは、`authorization.deny_action = disconnect`（デフォルトは`ignore`）の設定を検討してください。クライアントが認可されていないトピックにパブリッシュやサブスクライブを試みると、EMQXは接続を切断します。これに[フラッピング検出](./flapping-detect.md)を組み合わせると、繰り返し再接続し認可拒否を引き起こすクライアントは自動的に禁止されます。`deny_action`はグローバル設定のため、正当なクライアントが拒否された操作を試みた場合も切断されます。クライアントが通常、認可済みトピックのみでパブリッシュ・サブスクライブする場合に適用してください。フラッピング検出の閾値は通常の再接続ストームで禁止されないよう調整してください。[Authorization](./authz/authz.md)を参照してください。
- 認可キャッシュ設定と認可順序を見直し、ポリシー変更が期待通りに反映されるようにしてください。
- 不正または悪意のあるクライアントの影響を軽減するため、MQTTリソース使用量を制限してください。パケットサイズ、トピックレベル数、サブスクリプション数、インフライトウィンドウ、キューイングされたメッセージ数などの制限を見直してください。[MQTT Configuration](../configuration/mqtt.md)を参照してください。
- 必要に応じてリスナーレベルのレート制御を適用し、接続やパブリッシュのバーストを制限してください。[Rate Limiter Configuration](../configuration/limiter.md)を参照してください。
- [Banned Clients](./blacklist.md)および[Flapping Detect](./flapping-detect.md)を使用して、悪質または不安定なクライアントを制御してください。
- [Message Queue](../../develop/message-queue/message-queue-concept.md)または[MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md)を有効にしている場合は、`$queue/`および`$stream/`ネームスペース（廃止予定の`$q/`および`$s/`プレフィックスを含む）に対して別個の認可ルールを定義してください。EMQXはプレフィックス付きのサブスクリプショントピックフィルター全体を認可し、`$queue/<name>/`や`$stream/<name>/`の後の`<topic_filter>`部分は個別に認可しません。`#`や`+/#`のルールは`$`で始まるフィルターにはマッチしません。自動作成が有効な場合、この`<topic_filter>`部分を制限してください。これは新しいキューやストリームが受信・保存するパブリッシュメッセージを決定するためです。[Message Queue Security Considerations](../../develop/message-queue/message-queue-concept.md#security-considerations)および[MQTT Streams Security Considerations](../../develop/mqtt-stream/mqtt-stream-concept.md#security-considerations)を参照してください。
- Cluster Linkingを有効にしている場合は、ピア接続を受け入れるリスナーで認証を強制し、`$LINK/`制御ネームスペースを専用のCluster LinkingクライアントIDに制限し、それ以外は拒否してください。[Secure Cluster Linking](../../develop/cluster-linking/security.md)を参照してください。

## フェーズ5：管理とメンテナンス

- 本番利用前にデフォルトのダッシュボードパスワードを変更し、管理アクセス権を見直してください。[System](../dashboard/system.md)を参照してください。
- ダッシュボードは信頼できるネットワークのみに配置し、管理者アクセスにはHTTPSを推奨します。可能な場合はダッシュボードリスナーをlocalhost、プライベートインターフェース、または保護された管理ネットワークにバインドしてください。[Dashboard Configuration](../configuration/dashboard.md)を参照してください。
- **Management** -> **Cluster Settings** -> **Rule Engine Security**でSSRF保護を有効にし、コネクター設定のテスト、作成、更新時にHTTPおよびMQTTコネクターのターゲットを検証します。EMQX 6.0.4以降、このポリシーは他のコネクタータイプやランタイム接続には適用されません。委任管理者がルールエンジンリソースを作成・変更できる場合や完全なアウトバウンドネットワーク境界が必要な場合はホストレベルのイグレス制御を追加してください。[Rule Engine Security](../dashboard/cluster_settings.md#rule-engine-security)および[Mitigate SSRF with Rule Engine Policy and Firewall Rules](../cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。
- 管理APIを公開する場合は、ダッシュボード認証情報の代わりにAPIキーを使用し、必要最小限のロールを付与し、有効期限を設定してください。[REST API](../api.md)および[System](../dashboard/system.md#api-key)を参照してください。
- EMQX Enterpriseを使用している場合は、管理ユーザー向けに[シングルサインオン（SSO）](../dashboard/sso.md)を検討し、利用可能な場合はIDプロバイダーでMFAを強制してください。
- 定期的なバックアップをスケジュールし、リストア手順をリハーサルしてください。証明書やACLファイルがEMQXデータディレクトリ外に保存されている場合は別途バックアップが必要です。[Backup and Restore](../backup-restore.md)を参照してください。
- 可能な場合は監査ログを有効にし、異常検知やインシデント対応のためにログやメトリクスをオブザーバビリティスタックに集約してください。[Audit Log](../dashboard/audit-log.md)、[Logs Configuration](../configuration/logs.md)、[Logs and Observability](../observability/overview.md)を参照してください。

## 変更後の再検証

- 証明書ローテーション、リスナー変更、ロードバランサー更新、クラスター拡張、バックアップポリシー変更、認証・認可チェーンの変更後は、このチェックリストを再実行してください。
- 匿名クライアントの拒否、無効証明書によるTLSハンドシェイク失敗、許可されていないトピックへのパブリッシュやサブスクライブの拒否など、想定される失敗モードを本番切り替え前に検証してください。
