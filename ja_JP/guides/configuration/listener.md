# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受け取るために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf`ではなく`base.hocon`を使用することを推奨します。  
これは、`emqx.conf`に設定を記述すると、ダッシュボードからの変更が一時的なものとなり、EMQX再起動時に失われてしまうためです。

:::

::: tip

EMQXはカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## リスナー名の要件

EMQX 6.0.4以降、新規作成されるMQTTリスナーの名前は以下の要件を満たす必要があります。

- 名前は1〜64バイトの長さであること
- 名前はASCIIの英字または数字で始まること
- 名前はASCIIの英字、数字、ハイフン（`-`）、アンダースコア（`_`）のみを含むこと

これらの要件を満たさないリスナーの作成リクエストはEMQXにより拒否されます。アップグレード前から存在し、64バイトを超える名前のMQTTリスナーは、設定の更新や削除は可能ですが、名前の変更はできません。

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。TCP/IPネットワーク上でクライアントとEMQX間の接続を確立・管理する上で重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内にある`base.hocon`ファイルに`listeners.tcp`の設定項目を追加します。

例えば、ポート`1883`でTCPリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように記述します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default`はリスナーを有効化する設定で、`default`はリスナー名です。任意のリスナー名に変更可能です。
- `bind`はリスナーのIPアドレスとポートを設定します。ここでは任意のIPアドレスからのポート`1883`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数を設定します。デフォルト値は`infinity`です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続の着信を待ち受けるネットワークサービスです。EMQXではクライアントとEMQX間のネットワークトラフィックを暗号化して保護するために使用されます。

EMQXでSSLリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ssl`の設定項目を追加します。

例えば、ポート`8883`でSSLリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように記述します。

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
- `bind`はリスナーのIPアドレスとポートで、ここでは任意のIPアドレスからのポート`8883`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼済みCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。
  - `verify`: クライアント証明書の真正性を検証する場合は`verify_peer`、検証しない場合は`verify_none`を設定します。
  - `fail_if_no_peer_cert`: `true`に設定するとクライアントが証明書を送信しない（空の証明書を送信）場合に接続を失敗させます。`false`の場合はクライアントが無効な証明書を送信した場合のみ失敗とみなします（空の証明書は有効とみなされます）。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket上でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使ってEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要は[MQTT over WebSocket](../../develop/connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ws`の設定項目を追加します。

例えば、ポート`8083`でWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように記述します。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default`はリスナーを有効化する設定です。
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8083`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、SSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間のデータを暗号化するWebSocketリスナーです。EMQXでは、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となります。

EMQXでセキュアWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.wss`の設定項目を追加します。

例えば、ポート`8084`でセキュアWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように記述します。

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
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8084`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼済みCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 転送されたクライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシまたはロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスをどのように判定するかを制御する2つのオプションがあります。

- `websocket.proxy_address_header`（デフォルト: `x-forwarded-for`）
- `websocket.proxy_port_header`（デフォルト: `x-forwarded-port`）

設定されたヘッダーがWebSocketアップグレードリクエストに存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用します。これにより、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査およびトレースログがクライアントの送信元IPとして認識するアドレスが決まります。

::: warning 信頼できるプロキシの背後でのみ転送アドレスヘッダーを信頼してください

ヘッダー値はクライアントの見かけ上の送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重すべきです。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合、任意のクライアントがヘッダーを送信して任意の送信元IPを偽装できます。常に実際のTCPピアアドレスを使用するには、`proxy_address_header = ""`および`proxy_port_header = ""`に設定してください。
- プロキシが`X-Forwarded-For`ヘッダーを上書きや削除ではなく**追記**している場合（多くのプロキシのデフォルト動作、例: NGINXの`$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリは依然としてクライアント由来のため、送信元IPを偽装可能です。プロキシを設定して観測したアドレスでヘッダーを上書きするか、[PROXYプロトコル](../cluster/lb.md)を使用するか、オプションを空文字列に設定してください。
- 未使用のヘッダー名を指定してこの仕組みを無効化しようとしないでください。クライアントは任意の名前でヘッダーを送信可能です。空文字列のみがクライアントが絶対に送信できない値です。

リスナーで`proxy_protocol = true`が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーと設定ゾーンのリンク

EMQXの各リスナーはゾーンに紐づけられており、デフォルトでは`default`という論理ゾーンに設定されています。

リスナーが特定のゾーンにリンクされている場合、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[ゾーンオーバーライド](./configuration.md#zone-override)セクションをご覧ください。

## マウントポイント

各リスナーは`mountpoint`を設定可能で、これはEMQXがそのリスナー経由で接続するクライアントのトピックに付加するトピックプレフィックスです。  
このプレフィックスは`PUBLISH`パケット、`SUBSCRIBE`および`UNSUBSCRIBE`リクエスト、Willメッセージのトピックに追加され、クライアントに配信されるメッセージのトピックからは削除されます。  
マウントポイントはクライアントに対して透過的であり、マルチテナント環境などでクライアントグループ間のトピック空間を分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは`${clientid}`、`${username}`、`${zone}`、`${client_attrs.NAME}`のプレースホルダーをサポートしています。  
例えば、`mountpoint = "${username}/"`と設定した場合、ユーザー名`u1`のクライアントが`sensors/#`をサブスクライブすると、内部的には`u1/sensors/#`としてサブスクライブが作成されます。

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な`$`プレフィックスで始まるトピックのパブリッシュやサブスクライブをトリガーします。EMQXはマウントポイントのプレフィックスをこれらのプレフィックスのマッチング前に追加します。  
例えば、マウントポイント`mp/`のリスナー経由でクライアントが`$delayed/10/t`にパブリッシュすると、ブローカーは`mp/$delayed/10/t`として受信し、もはや`$delayed/`で始まらないため機能は無効化されます。EMQXはこのメッセージを通常のリテラルトピックとしてルーティングし、クライアントにエラーは通知されません。

::: warning 互換性の制限
以下の機能を使用するクライアントが接続するリスナーにはマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [Delayed Publish](../../get-started/messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [File Transfer](../../develop/file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [Message Queue](../../develop/message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [Cluster Linking](../../develop/cluster-linking/introduction.md) | `$LINK/` |

Cluster Linkingの場合、リンクされたクラスターからの接続を受け入れるリスナーにはマウントポイントを設定してはいけません。
:::

[共有サブスクリプション](../../get-started/messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[排他サブスクリプション](../../get-started/messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。  
EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内部のトピックフィルターにのみ追加されます。  
例えば、マウントポイント`mp/`のリスナー経由で`$share/g/t`をサブスクライブすると、トピック`mp/t`の共有サブスクリプショングループ`g`に参加します。
