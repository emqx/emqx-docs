# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受信するために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューの **Management** -> **Listeners** をクリックして設定することもできます。  
設定ファイルからリスナーを設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードからの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

::: tip

EMQXはより詳細なカスタマイズニーズに対応するため、さらに多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## TCPリスナーの設定

TCPリスナーは、特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。TCP/IPネットワーク上でクライアントとEMQX間の接続を確立・管理する上で重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの `etc` フォルダ内にある `base.hocon` ファイルに `listeners.tcp` の設定項目を追加します。

例えば、ポート `1883` でTCPリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default` はリスナーを有効化する設定で、`default` はリスナー名です。任意のリスナー名に変更可能です。  
- `bind` はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`への全ての着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数を設定します。デフォルトは `infinity`（無制限）です。

## SSLリスナーの設定

SSLリスナーは、SSL（Secure Sockets Layer）接続の着信を待ち受けるネットワークサービスです。EMQXでは、クライアントとEMQX間の通信を暗号化し、ネットワークトラフィックを保護するために使用されます。

EMQXでSSLリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.ssl` の設定項目を追加します。

例えば、ポート `8883` でSSLリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

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
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8883`への全ての着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `ssl_options` はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます：  
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。  
  - `verify`: クライアント証明書の真正性を検証する場合は `verify_peer`、しない場合は `verify_none` を設定します。  
  - `fail_if_no_peer_cert`: `true` に設定すると、クライアントが証明書を送信しない（空の証明書を送信）場合に接続を失敗させます。`false` の場合は、無効な証明書を送信した場合のみ失敗し、空の証明書は有効とみなします。

## WebSocketリスナーの設定

WebSocketリスナーは、WebSocket経由でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使ってEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要については、[MQTT over WebSocket](../connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.ws` の設定項目を追加します。

例えば、ポート `8083` でWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default` はリスナーを有効化します。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8083`への全ての着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、SSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間のデータを暗号化するWebSocketリスナーです。EMQXでは、セキュアWebSocketリスナーはWebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策です。

EMQXでセキュアWebSocketリスナーを設定するには、`etc` フォルダ内の `base.hocon` ファイルに `listeners.wss` の設定項目を追加します。

例えば、ポート `8084` でセキュアWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

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
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8084`への全ての着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。  
- `ssl_options` はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます：  
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 転送されたクライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合に、EMQXがクライアントの送信元アドレスをどのように判定するかを制御する2つのオプションがあります。

- `websocket.proxy_address_header`（デフォルト: `x-forwarded-for`）  
- `websocket.proxy_port_header`（デフォルト: `x-forwarded-port`）

設定されたヘッダーがWebSocketアップグレードリクエストに存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用し、実際のTCPピアのアドレスではなくなります。  
この派生されたアドレスは、IPベースの認可ルール、禁止されたクライアント、フラッピング検知、監査およびトレースログでクライアントの送信元IPとして扱われます。

::: warning 信頼できるプロキシの背後でのみ転送アドレスヘッダーを信用してください

ヘッダー値はクライアントの見かけ上の送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重すべきです。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合、任意のクライアントがヘッダーを送信して任意の送信元IPを偽装可能です。常に実際のTCPピアアドレスを使用するには、`proxy_address_header = ""` および `proxy_port_header = ""` に設定してください。  
- プロキシが存在しても、`X-Forwarded-For` ヘッダーを上書きや削除せずに追記する場合（多くのプロキシのデフォルト動作、例: NGINXの `$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリは依然としてクライアントが送信したものとなり、送信元IPを偽装可能です。プロキシを設定してヘッダーを観測したアドレスで上書きするか、[PROXYプロトコル](../deploy/cluster/lb.md)を使用するか、オプションを空文字列に設定してください。  
- 未使用のヘッダー名を指定してこの機能を無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列のみがクライアントが絶対に送信できない値です。

リスナーで `proxy_protocol = true` が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。  
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーを設定ゾーンにリンクする

EMQXの各リスナーはゾーンに関連付けられており、デフォルトでは `default` という論理ゾーンに設定されています。

リスナーが特定のゾーンにリンクされると、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[ゾーンオーバーライド](./configuration.md#zone-override)セクションをご参照ください。

## マウントポイント

各リスナーは `mountpoint` を設定可能で、これはリスナー経由で接続するクライアントが使用するトピックにEMQXが付加するトピックプレフィックスです。  
このプレフィックスは `PUBLISH` パケット、`SUBSCRIBE` および `UNSUBSCRIBE` リクエスト、Willメッセージのトピックに付加され、クライアントに配信されるメッセージのトピックからは除去されます。  
マウントポイントはクライアントには透過的であり、マルチテナント環境などでクライアントグループ間のトピックスペースを分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは `${clientid}`、`${username}`、`${zone}`、`${client_attrs.NAME}` のプレースホルダーをサポートしています。  
例えば、`mountpoint = "${username}/"` と設定すると、ユーザー名 `u1` のクライアントが `sensors/#` をサブスクライブした場合、内部的には `u1/sensors/#` としてサブスクライブされます。

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な `$` プレフィックスで始まるトピックのパブリッシュやサブスクライブによりトリガーされます。  
EMQXはこれらのプレフィックスとマッチングする前にマウントポイントのプレフィックスを付加します。  
例えば、マウントポイント `mp/` のリスナー経由でクライアントが `$delayed/10/t` にパブリッシュすると、ブローカーは `mp/$delayed/10/t` として受信し、もはや `$delayed/` で始まらないため、該当機能は無効化されます。  
EMQXはメッセージを通常のリテラルトピックとしてルーティングし、クライアントにはエラーは報告されません。

::: warning 互換性制限
以下の機能を使用するクライアントが接続するリスナーに対しては、マウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [遅延パブリッシュ](../messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [ファイル転送](../file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [クラスターリンク](../cluster-linking/introduction.md) | `$LINK/` |

クラスターリンクの場合、リンクされたクラスターからの接続を受け付けるリスナーにはマウントポイントを設定してはいけません。
:::

[共有サブスクリプション](../messaging/mqtt-shared-subscription.md) (`$share/{group}/`) および [排他サブスクリプション](../messaging/mqtt-exclusive-subscription.md) (`$exclusive/`) は例外で、マウントポイントと共に動作します。  
EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内部のトピックフィルターのみに付加されます。  
例えば、マウントポイント `mp/` のリスナー経由で `$share/g/t` をサブスクライブすると、共有サブスクリプショングループ `g` はトピック `mp/t` に参加します。
