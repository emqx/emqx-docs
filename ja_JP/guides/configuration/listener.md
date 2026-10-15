# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受信するために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定した場合、ダッシュボードを通じて行った変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。TCP/IPネットワーク上でクライアントとEMQX間の接続を確立・管理する上で重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの `etc` フォルダ内にある `base.hocon` ファイルに `listeners.tcp` の設定項目を追加します。

例えば、ポート `1883` でTCPリスナーを有効にし、最大1,024,000の同時接続を許可するには、以下のように設定します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default` はリスナーを有効化する設定で、`default` はリスナー名です。任意の名前に変更可能です。  
- `bind` はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数を設定します。デフォルト値は `infinity` です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続の着信を待ち受けるネットワークサービスです。EMQXではクライアントとEMQX間の通信を暗号化し、ネットワークトラフィックのセキュリティを確保するために使用されます。

EMQXでSSLリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.ssl` の設定項目を追加します。

例えば、ポート `8883` でSSLリスナーを有効にし、最大1,024,000の同時接続を許可する設定例は以下の通りです。

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

- `listeners.ssl.default` はリスナーを有効化します。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8883`への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `ssl_options` はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。  
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。  
  - `verify`: クライアント証明書の真正性を検証する場合は `verify_peer`、しない場合は `verify_none` に設定します。  
  - `fail_if_no_peer_cert`: `true` に設定すると、クライアントが証明書を送信しない（空の証明書を送信）場合にサーバーは失敗します。`false` の場合は、無効な証明書を送信した場合のみ失敗します（空の証明書は有効とみなされます）。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket上でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使用してEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シナリオの概要については、[MQTT over WebSocket](../../develop/connect-emqx/mqtt-over-websocket.md)をご参照ください。

EMQXでWebSocketリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.ws` の設定項目を追加します。

例えば、ポート `8083` でWebSocketリスナーを有効にし、最大1,024,000の同時接続を許可する設定例は以下の通りです。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default` はリスナーを有効化します。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8083`への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、WebSocketクライアントとブローカー間で交換されるデータをSSLまたはTLSプロトコルで暗号化するWebSocketリスナーです。EMQXでは、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となります。

EMQXでセキュアWebSocketリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.wss` の設定項目を追加します。

例えば、ポート `8084` でセキュアWebSocketリスナーを有効にし、最大1,024,000の同時接続を許可する設定例は以下の通りです。

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

- `listeners.wss.default` はリスナーを有効化します。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8084`への全ての着信トラフィックを待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。  
- `ssl_options` はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。  
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 転送されたクライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスをどのように判定するかを制御する2つのオプションがあります。

- `websocket.proxy_address_header` （デフォルト: `x-forwarded-for`）  
- `websocket.proxy_port_header` （デフォルト: `x-forwarded-port`）

設定されたヘッダーがWebSocketアップグレード要求に存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用し、実際のTCPピアのアドレスではなくなります。  
この派生されたアドレスは、IPベースの認可ルール、禁止されたクライアント、フラッピング検出、および監査・トレースログでクライアントの送信元IPとして認識されます。

::: warning 信頼できるプロキシの背後でのみ転送アドレスヘッダーを信頼してください

ヘッダー値はクライアントの見かけ上の送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重すべきです。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合、任意のクライアントがヘッダーを送信し、自身の見かけ上の送信元IPを選択可能です。常に実際のTCPピアアドレスを使用するには、`proxy_address_header = ""` および `proxy_port_header = ""` に設定してください。  
- プロキシが存在するが、`X-Forwarded-For` ヘッダーを上書きや削除せずに追記する場合（多くのプロキシのデフォルト動作、例: NGINXの `$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリは依然としてクライアントが提供したものなので、送信元IPを偽装可能です。プロキシを設定して観測したアドレスでヘッダーを上書きするか、[PROXYプロトコル](../cluster/lb.md)を使用するか、オプションを空文字列に設定してください。  
- 未使用のヘッダー名を指定してこの機能を無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列のみがクライアントが送信できない値です。

リスナーに `proxy_protocol = true` が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。  
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーを設定ゾーンにリンクする

EMQXの各リスナーはゾーンに関連付けられており、デフォルトでは `default` という論理ゾーンに設定されています。

リスナーが特定のゾーンにリンクされると、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[ゾーンオーバーライド](./configuration.md#zone-override)セクションをご参照ください。

## マウントポイント

各リスナーは `mountpoint` を設定可能で、これはEMQXがそのリスナー経由で接続したクライアントが使用するトピックに付加するトピックプレフィックスです。  
このプレフィックスは、`PUBLISH` パケット、`SUBSCRIBE` および `UNSUBSCRIBE` リクエスト、Willメッセージのトピックに追加され、クライアントに配信されるメッセージのトピックからは削除されます。  
マウントポイントはクライアントには透過的であり、マルチテナント環境などでクライアントグループ間のトピックスペースを分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは `${clientid}`, `${username}`, `${zone}`, `${client_attrs.NAME}` のプレースホルダーをサポートしています。  
例えば、`mountpoint = "${username}/"` と設定すると、ユーザー名 `u1` のクライアントが `sensors/#` をサブスクライブした場合、内部的には `u1/sensors/#` としてサブスクリプションが作成されます。

::: warning マウントポイント追加前に認可がチェックされます
EMQXはリスナーのマウントポイントを追加する前のトピックに対して認可ルールを評価します。  
このルールはパブリッシュ、サブスクライブ操作およびWillメッセージに適用されます。

マウントポイント付きリスナーの場合、認可ルールはマウントされていないトピックに対して記述してください。  
例えば、`department-a/secret/#` に対する拒否ルールは、そのリスナー経由で `secret/#` にパブリッシュまたはサブスクライブするクライアントには適用されません。
:::

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特殊な `$` プレフィックスで始まるトピックのパブリッシュやサブスクライブによってトリガーされます。  
EMQXはこれらのプレフィックスとマッチングする前にマウントポイントプレフィックスを追加します。  
例えば、マウントポイントが `mp/` のリスナー経由でクライアントが `$delayed/10/t` にパブリッシュすると、ブローカーは `mp/$delayed/10/t` を受け取り、もはや `$delayed/` で始まらないため、機能は無効化されます。  
EMQXはこのメッセージを通常のマウントされたリテラルトピックとしてルーティングし、クライアントにエラーは報告されません。

::: warning 互換性の制限
以下の機能を使用するクライアントが接続するリスナーにマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [遅延パブリッシュ](../../get-started/messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [ファイル転送](../../develop/file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [メッセージキュー](../../develop/message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [クラスターリンク](../../develop/cluster-linking/introduction.md) | `$LINK/` |
| [動的キープアライブ調整](./mqtt.md#dynamic-keep-alive-adjustment) | `$SETOPTS/` |
| [A2A over MQTT](../../develop/emqx-ai/a2a-over-mqtt/overview.md) | `$a2a/` |

クラスターリンクの場合、リンクされたクラスターからの接続を受け入れるリスナーにはマウントポイントを設定してはいけません。  
A2A over MQTTの場合、1トピックレベルのマウントポイント（例: `acme/`）は動作します。EMQXは `$a2a` トピックに対して名前空間プレフィックスとして解析します。
:::

[共有サブスクリプション](../../get-started/messaging/mqtt-shared-subscription.md) (`$share/{group}/`) および [排他サブスクリプション](../../get-started/messaging/mqtt-exclusive-subscription.md) (`$exclusive/`) は例外で、マウントポイントと共に動作します。  
EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内側のトピックフィルターにのみ追加されます。  
例えば、マウントポイントが `mp/` のリスナー経由で `$share/g/t` にサブスクライブすると、トピック `mp/t` の共有サブスクリプショングループ `g` に参加します。
