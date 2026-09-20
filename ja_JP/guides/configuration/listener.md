# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受信するために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **管理** -> **リスナー** をクリックして設定することもできます。  
設定ファイルからリスナーを設定する場合は、`emqx.conf`ではなく`base.hocon`を使用することを推奨します。  
これは、`emqx.conf`で設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQXの再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## リスナー名の要件

EMQX 6.0.4以降、新規作成されるMQTTリスナーの名前は以下の要件を満たす必要があります。

- 名前は1〜64バイトの長さであること
- 名前はASCIIの英字または数字で始まること
- 名前はASCIIの英字、数字、ハイフン（`-`）、アンダースコア（`_`）のみを含むこと

これらの要件を満たさない場合、EMQXはリスナー作成要求を拒否します。アップグレード前に存在していた64バイトを超える名前のMQTTリスナーについては、設定の更新や削除は可能ですが、名前の変更はできません。

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。TCP/IPネットワーク上でクライアントとEMQX間の接続を確立および管理する上で重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内の`base.hocon`ファイルに`listeners.tcp`の設定項目を追加します。

例えば、ポート`1883`でTCPリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default`はリスナーを有効化する設定で、`default`はリスナー名です。任意のリスナー名に変更可能です。
- `bind`はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数を設定します。デフォルト値は`infinity`です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続の着信を待ち受けるネットワークサービスです。EMQXではクライアントとEMQX間のネットワークトラフィックを暗号化し、通信の安全性を確保するために使用されます。

EMQXでSSLリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内の`base.hocon`ファイルに`listeners.ssl`の設定項目を追加します。

例えば、ポート`8883`でSSLリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

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
- `bind`はリスナーのIPアドレスとポートを設定し、任意のIPアドレスからのポート`8883`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルト値は`infinity`です。
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナー用のSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。
  - `verify`: クライアント証明書の真正性を検証する場合は`verify_peer`、しない場合は`verify_none`を設定します。
  - `fail_if_no_peer_cert`: `true`に設定すると、クライアントが証明書を送信しない（空の証明書を送信する）場合に接続を失敗させます。`false`の場合は、クライアントが無効な証明書を送信した場合のみ失敗します（空の証明書は有効とみなされます）。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket経由でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使用してEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要については、[MQTT over WebSocket](../../develop/connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内の`base.hocon`ファイルに`listeners.ws`の設定項目を追加します。

例えば、ポート`8083`でWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default`はリスナーを有効化する設定です。
- `bind`はリスナーのIPアドレスとポートを設定し、任意のIPアドレスからのポート`8083`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルト値は`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、SSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間で交換されるデータを暗号化するWebSocketリスナーです。EMQXにおいて、セキュアWebSocketリスナーはWebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となります。

EMQXでセキュアWebSocketリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内の`base.hocon`ファイルに`listeners.wss`の設定項目を追加します。

例えば、ポート`8084`でセキュアWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

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
- `bind`はリスナーのIPアドレスとポートを設定し、任意のIPアドレスからのポート`8084`へのすべての着信トラフィックを待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルト値は`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナー用のSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合は、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 転送されたクライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスをどのように判定するかを制御する2つのオプションがあります。

- `websocket.proxy_address_header`（デフォルト: `x-forwarded-for`）
- `websocket.proxy_port_header`（デフォルト: `x-forwarded-port`）

設定されたヘッダーがWebSocketアップグレード要求に存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用し、実際のTCPピアのアドレスではなくなります。導出されたアドレスは、IPベースの認可ルール、禁止されたクライアント、フラッピング検知、監査およびトレースログでクライアントの送信元IPとして扱われます。

::: warning 信頼できるプロキシの背後でのみ転送アドレスヘッダーを信用してください

ヘッダー値はクライアントの見かけ上の送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重すべきです。

- リスナーがクライアントから直接到達可能（プロキシなし）の場合、任意のクライアントがヘッダーを送信して任意の送信元IPを偽装できます。常に実際のTCPピアアドレスを使用するには、`proxy_address_header = ""`および`proxy_port_header = ""`に設定してください。
- プロキシが存在しても、`X-Forwarded-For`ヘッダーを上書きや削除ではなく**追記**している場合（多くのプロキシのデフォルト動作、例: NGINXの`$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリは依然としてクライアントが提供したものであり、送信元IPは偽装可能です。プロキシを設定して観測したアドレスでヘッダーを上書きするか、[PROXYプロトコル](../cluster/lb.md)を使用するか、オプションを空文字列に設定してください。
- 未使用のヘッダー名を指定してこの機能を無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列のみがクライアントが送信不可能な値です。

`proxy_protocol = true`がリスナーに設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーと設定ゾーンのリンク

EMQXの各リスナーはゾーンに関連付けられており、デフォルトでは`default`という論理ゾーンに設定されています。

リスナーが特定のゾーンにリンクされている場合、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[ゾーンオーバーライド](./configuration.md#zone-override)セクションをご参照ください。

## マウントポイント

各リスナーは`mountpoint`を設定できます。これは、リスナー経由で接続するクライアントが使用するトピックにEMQXが付加するトピックプレフィックスです。プレフィックスは`PUBLISH`パケット、`SUBSCRIBE`および`UNSUBSCRIBE`リクエスト、Willメッセージのトピックに付加され、クライアントに配信されるメッセージのトピックからは除去されます。マウントポイントはクライアントに対して透過的であり、マルチテナント環境などでクライアントグループ間のトピック空間を分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは`${clientid}`、`${username}`、`${zone}`、`${client_attrs.NAME}`のプレースホルダーをサポートします。例えば、`mountpoint = "${username}/"`の場合、ユーザー名`u1`のクライアントが`sensors/#`をサブスクライブすると、内部的には`u1/sensors/#`としてサブスクライブされます。

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な`$`プレフィックスで始まるトピックのパブリッシュやサブスクライブによってトリガーされます。EMQXはマウントポイントプレフィックスをこれらのプレフィックスのマッチング前に付加します。例えば、マウントポイント`mp/`のリスナー経由でクライアントが`$delayed/10/t`にパブリッシュすると、ブローカーは`mp/$delayed/10/t`として受信し、もはや`$delayed/`で始まらないため機能は無効化されます。EMQXはメッセージを通常のリテラルトピックとしてルーティングし、クライアントにエラーは報告されません。

::: warning 互換性の制限
以下の機能を利用するクライアントが接続するリスナーにはマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [遅延パブリッシュ](../../get-started/messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [ファイル転送](../../develop/file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [クラスターリンク](../../develop/cluster-linking/introduction.md) | `$LINK/` |

クラスターリンクの場合、リンクされたクラスターからの接続を受け入れるリスナーにはマウントポイントを設定してはいけません。
:::

[共有サブスクリプション](../../get-started/messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[排他サブスクリプション](../../get-started/messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内部のトピックフィルターにのみ付加されます。例えば、マウントポイント`mp/`のリスナー経由で`$share/g/t`をサブスクライブすると、共有サブスクリプショングループ`g`はトピック`mp/t`で参加します。
