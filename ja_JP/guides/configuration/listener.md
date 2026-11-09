# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受信するために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。  
これは、`emqx.conf` に設定した場合、ダッシュボードでの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。TCP/IPネットワーク上でクライアントとEMQX間の接続を確立・管理する上で重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの `etc` フォルダ内にある `base.hocon` ファイルに `listeners.tcp` の設定項目を追加します。

例えば、ポート `1883` でTCPリスナーを有効にし、リスナーが許可する最大同時接続数を1,024,000に設定する場合は以下のように記述します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default` はリスナーを有効にする設定で、`default` はリスナー名です。任意のリスナー名に変更可能です。  
- `bind` はリスナーのIPアドレスとポートを設定します。ここでは任意のIPアドレスからのポート `1883` への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数を設定します。デフォルト値は `infinity`（無制限）です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続の着信を待ち受けるネットワークサービスです。EMQXではクライアントとEMQX間のネットワークトラフィックを暗号化し、安全に通信するために使用されます。

EMQXでSSLリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.ssl` の設定項目を追加します。

例えば、ポート `8883` でSSLリスナーを有効にし、最大同時接続数を1,024,000に設定する場合は以下のように記述します。

```bash
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 1024000
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    verify = verify_none
    fail_if_no_peer_cert = false
  }
}
```

ここで、

- `listeners.ssl.default` はリスナーを有効にする設定です。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート `8883` への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `ssl_options` はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。  
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナー用のSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。  
  - `verify`: クライアント証明書の真正性を検証する場合は `verify_peer`、検証しない場合は `verify_none` を設定します。  
  - `fail_if_no_peer_cert`: `true` に設定するとクライアントが証明書を送信しない（空の証明書を送る）場合に接続を失敗させます。`false` の場合はクライアントが無効な証明書を送信した場合のみ失敗し、空の証明書は有効とみなされます。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket経由でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使ってEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要については、[MQTT over WebSocket](../../develop/connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.ws` の設定項目を追加します。

例えば、ポート `8083` でWebSocketリスナーを有効にし、最大同時接続数を1,024,000に設定する場合は以下のように記述します。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default` はリスナーを有効にする設定です。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート `8083` への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、SSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間で交換されるデータを暗号化するWebSocketリスナーです。EMQXにおいては、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策です。

EMQXでセキュアWebSocketリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.wss` の設定項目を追加します。

例えば、ポート `8084` でセキュアWebSocketリスナーを有効にし、最大同時接続数を1,024,000に設定する場合は以下のように記述します。

```bash
listeners.wss.default {
  bind = "0.0.0.0:8084"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
  ssl_options {
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
  }
}
```

ここで、

- `listeners.wss.default` はリスナーを有効にする設定です。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート `8084` への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。  
- `ssl_options` はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。  
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナー用のSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 転送元クライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスをどのように判断するかを制御する2つのオプションがあります。

- `websocket.proxy_address_header`（デフォルト: `x-forwarded-for`）  
- `websocket.proxy_port_header`（デフォルト: `x-forwarded-port`）

設定されたヘッダーがWebSocketアップグレード要求に存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用し、実際のTCPピアのアドレスの代わりに扱います。  
この派生されたアドレスは、IPベースの認可ルール、禁止されたクライアント、フラッピング検出、監査およびトレースログでクライアントの送信元IPとして認識されます。

::: warning 信頼できるプロキシの背後でのみ転送元アドレスヘッダーを信頼してください

ヘッダー値はクライアントの見かけ上の送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重すべきです。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合、任意のクライアントがヘッダーを送信し任意の送信元IPを偽装できます。常に実際のTCPピアアドレスを使うには、`proxy_address_header = ""` と `proxy_port_header = ""` に設定してください。  
- プロキシが `X-Forwarded-For` ヘッダーを上書きや削除せずに追記する場合（多くのプロキシのデフォルト動作、例: NGINXの `$proxy_add_x_forwarded_for`）、EMQXが読む左端のエントリは依然としてクライアントが提供したものなので、送信元IPは偽装可能です。プロキシを設定して観測したアドレスでヘッダーを上書きするか、[PROXYプロトコル](../cluster/lb.md)を使用するか、オプションを空文字列に設定してください。  
- 未使用のヘッダー名を指定してこの機能を無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列のみがクライアントが送信できない値です。

リスナーで `proxy_protocol = true` が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。  
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーと設定ゾーンのリンク

EMQXの各リスナーはゾーンに関連付けられており、デフォルトでは論理ゾーン `default` に設定されています。

リスナーが特定のゾーンにリンクされると、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[ゾーンオーバーライド](./configuration.md#zone-override)セクションをご参照ください。

## マウントポイント

各リスナーは `mountpoint` を設定可能で、これはEMQXがそのリスナー経由で接続するクライアントが使用するトピックに追加するトピックプレフィックスです。  
このプレフィックスは `PUBLISH` パケット、`SUBSCRIBE` および `UNSUBSCRIBE` リクエスト、Willメッセージのトピックに追加され、クライアントに配信されるメッセージのトピックからは削除されます。  
マウントポイントはクライアントに対して透過的であり、マルチテナント環境などでクライアントグループ間のトピックスペースを分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは `${clientid}`、`${username}`、`${zone}`、`${client_attrs.NAME}` のプレースホルダーをサポートしています。  
例えば、`mountpoint = "${username}/"` と設定すると、ユーザー名 `u1` のクライアントが `sensors/#` をサブスクライブした場合、内部的には `u1/sensors/#` としてサブスクライブされます。

::: warning マウントポイント追加前に認可がチェックされます

EMQXはリスナーのマウントポイントを追加する前のトピックに対して認可ルールを評価します。マウントポイントは認可チェック後に追加されます。  
これはパブリッシュ、サブスクライブ操作およびWillメッセージに適用されます。

マウントポイント付きリスナーに対しては、マウントされていないトピックに対して認可ルールを書いてください。  
例えば `department-a/secret/#` に対する拒否ルールは、そのリスナー経由で `secret/#` にパブリッシュまたはサブスクライブするクライアントにはマッチしません。  
:::

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な `$` プレフィックスで始まるトピックのパブリッシュやサブスクライブでトリガーされます。  
EMQXはこれらのプレフィックスとマッチする前にマウントポイントプレフィックスを追加します。  
例えば、マウントポイントが `mp/` のリスナー経由でクライアントが `$delayed/10/t` にパブリッシュした場合、ブローカーは `mp/$delayed/10/t` と受け取るため、もはや `$delayed/` で始まりません。  
この機能はサイレントにバイパスされ、EMQXはメッセージを通常のマウントされたリテラルトピックにルーティングし、クライアントにエラーは報告されません。

::: warning 互換性の制限

以下の機能を使用するクライアントがいるリスナーにはマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [Delayed Publish](../../get-started/messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [File Transfer](../../develop/file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [Message Queue](../../develop/message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [Cluster Linking](../../develop/cluster-linking/introduction.md) | `$LINK/` |
| [Dynamic Keep Alive Adjustment](./mqtt.md#dynamic-keep-alive-adjustment) | `$SETOPTS/` |
| [A2A over MQTT](../../develop/emqx-ai/a2a-over-mqtt/overview.md) | `$a2a/` |

Cluster Linkingの場合、リンクされたクラスターからの接続を受け入れるリスナーにはマウントポイントを設定してはいけません。  
A2A over MQTTの場合、ちょうど1トピックレベルのマウントポイント（例: `acme/`）は動作します。EMQXは `$a2a` トピックのネームスペースプレフィックスとして解析します。  
:::

[共有サブスクリプション](../../get-started/messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[排他サブスクリプション](../../get-started/messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。  
EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内部のトピックフィルターにのみ追加されます。  
例えば、マウントポイント `mp/` のリスナー経由で `$share/g/t` にサブスクライブすると、共有サブスクリプショングループ `g` はトピック `mp/t` に参加します。
