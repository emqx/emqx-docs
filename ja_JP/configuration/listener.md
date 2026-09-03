# リスナーの設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受信するために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf`ではなく`base.hocon`の使用を推奨します。  
これは、`emqx.conf`で設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQXの再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。クライアントとEMQX間のTCP/IPネットワーク上での接続確立および管理において重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内にある`base.hocon`ファイルに`listeners.tcp`の設定項目を追加します。

例えば、ポート`1883`でTCPリスナーを有効化し、最大1,024,000の同時接続を許可するには、以下のように設定します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default`はリスナーを有効化する設定で、`default`はリスナー名です。任意の名前に変更可能です。  
- `bind`はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`への全ての着信トラフィックを待ち受けます。  
- `max_connections`はリスナーが許可する最大同時接続数を設定します。デフォルト値は`infinity`です。

## SSLリスナーの設定

SSLリスナーは着信するSSL（Secure Sockets Layer）接続を待ち受けるネットワークサービスです。EMQXでは、クライアントとEMQX間のネットワークトラフィックを暗号化し、安全に通信するために使用されます。

EMQXでSSLリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ssl`の設定項目を追加します。

例えば、ポート`8883`でSSLリスナーを有効化し、最大1,024,000の同時接続を許可するには以下のように設定します。

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

- `listeners.ssl.default`はリスナーを有効化する設定です。  
- `bind`はリスナーのIPアドレスとポートを設定し、任意のIPアドレスからのポート`8883`への全ての着信トラフィックを待ち受けます。  
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。  
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。  
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。  
  - `verify`: クライアント証明書の真正性を検証する場合は`verify_peer`、しない場合は`verify_none`を設定します。  
  - `fail_if_no_peer_cert`: `true`の場合、クライアントが証明書を送信しない（空の証明書を送信）とサーバーは接続を拒否します。`false`の場合は、クライアントが無効な証明書を送信した場合のみ拒否します（空の証明書は有効とみなされます）。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket上でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使用してEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンについては、[MQTT over WebSocket](../connect-emqx/mqtt-over-websocket.md)をご参照ください。

EMQXでWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ws`の設定項目を追加します。

例えば、ポート`8083`でWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可するには以下のように設定します。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default`はリスナーを有効化する設定です。  
- `bind`はリスナーのIPアドレスとポートを設定し、任意のIPアドレスからのポート`8083`への全ての着信トラフィックを待ち受けます。  
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。  
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、WebSocketクライアントとブローカー間で交換されるデータをSSLまたはTLSプロトコルで暗号化するWebSocketリスナーです。EMQXでは、セキュアWebSocketリスナーはWebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策です。

EMQXでセキュアWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.wss`の設定項目を追加します。

例えば、ポート`8084`でセキュアWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可するには以下のように設定します。

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

- `listeners.wss.default`はリスナーを有効化する設定です。  
- `bind`はリスナーのIPアドレスとポートを設定し、任意のIPアドレスからのポート`8084`への全ての着信トラフィックを待ち受けます。  
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。  
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。  
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。  
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 転送されたクライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスをどのように判定するかを制御する2つのオプションがあります。

- `websocket.proxy_address_header`: クライアントIPアドレスを含むHTTPヘッダーを指定します。  
- `websocket.proxy_port_header`: クライアントポートを含むHTTPヘッダーを指定します。

EMQX 6.3.0以降、両オプションのデフォルトは`""`です。空文字の場合、EMQXは対応するTCPピアのアドレスまたはポートを使用します。信頼できるプロキシから値を取得するには、明示的にヘッダー名（例: `x-forwarded-for`や`x-forwarded-port`）を設定してください。

設定されたヘッダーがWebSocketアップグレードリクエストに存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用します。これはIPベースの認可ルール、禁止クライアント、フラッピング検出、監査およびトレースログでクライアントの送信元IPとして扱われます。設定されたヘッダー名は大文字・小文字を区別しません。

::: warning 信頼できるプロキシの背後でのみ転送アドレスヘッダーを信頼してください

ヘッダー値はEMQXが使用するクライアント送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重してください。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合は、`proxy_address_header`と`proxy_port_header`を空にして、常に実際のTCPピアアドレスを使用してください。  
- プロキシが`X-Forwarded-For`ヘッダーに上書きではなく追記している場合（多くのプロキシのデフォルト動作、例：NGINXの`$proxy_add_x_forwarded_for`）、EMQXが読む左端のエントリは依然としてクライアントが提供したものであり、送信元IPを偽装される可能性があります。プロキシを設定して観測したアドレスでヘッダーを上書きするか、[PROXYプロトコル](../deploy/cluster/lb.md)を使用するか、オプションを空文字に設定してください。  
- 未使用のヘッダー名を指定してこの仕組みを無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字のみがクライアントが絶対に送信できない値です。

リスナーで`proxy_protocol = true`が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。  
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーと設定ゾーンのリンク

EMQXの各リスナーはゾーンに関連付けられており、デフォルトでは`default`という論理ゾーンに設定されています。

リスナーが特定のゾーンにリンクされると、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[ゾーンオーバーライド](./configuration.md#zone-override)セクションをご参照ください。

## マウントポイント

各リスナーは`mountpoint`を設定可能で、これはリスナー経由で接続するクライアントが使用するトピックにEMQXが付加するトピックプレフィックスです。  
このプレフィックスは`PUBLISH`パケット、`SUBSCRIBE`および`UNSUBSCRIBE`リクエスト、Willメッセージのトピックに付加され、クライアントに配信されるメッセージのトピックからは除去されます。  
マウントポイントはクライアントには透過的であり、マルチテナント環境などでクライアントグループ間のトピックスペースを分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは`${clientid}`、`${username}`、`${zone}`、`${client_attrs.NAME}`のプレースホルダーをサポートします。  
例えば、`mountpoint = "${username}/"`と設定した場合、ユーザー名が`u1`のクライアントが`sensors/#`をサブスクライブすると、内部的には`u1/sensors/#`としてサブスクリプションが作成されます。

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な`$`プレフィックスで始まるトピックのパブリッシュやサブスクライブによってトリガーされます。EMQXはマウントポイントのプレフィックスを追加した後にこれらのプレフィックスとマッチングを行います。  
例えば、マウントポイントが`mp/`のリスナー経由でクライアントが`$delayed/10/t`にパブリッシュすると、ブローカーは`mp/$delayed/10/t`として受信し、もはや`$delayed/`で始まらないため、機能は無効化されます。EMQXはメッセージを通常のメッセージとしてマウントされたリテラルトピックにルーティングし、クライアントにはエラーを報告しません。

::: warning 互換性の制限  
以下の機能を使用するクライアントが接続するリスナーにはマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [Delayed Publish](../messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [File Transfer](../file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [Message Queue](../message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [Cluster Linking](../cluster-linking/introduction.md) | `$LINK/` |
| [Dynamic Keep Alive Adjustment](./mqtt.md#dynamic-keep-alive-adjustment) | `$SETOPTS/` |
| [A2A over MQTT](../emqx-ai/a2a-over-mqtt/overview.md) | `$a2a/` |

Cluster Linkingの場合、リンクされたクラスターからの接続を受け入れるリスナーにはマウントポイントを設定してはいけません。  
A2A over MQTTの場合、ちょうど1トピックレベル（例：`acme/`）のマウントポイントは動作します。EMQXは`$a2a`トピックの名前空間プレフィックスとして解釈します。  
:::

[共有サブスクリプション](../messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[排他サブスクリプション](../messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析し、マウントポイントは内側のトピックフィルターにのみ追加されます。  
例えば、マウントポイント`mp/`のリスナー経由で`$share/g/t`をサブスクライブすると、共有サブスクリプショングループ`g`はトピック`mp/t`に参加します。
