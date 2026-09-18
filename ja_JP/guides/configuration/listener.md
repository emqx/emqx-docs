# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受け取るために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf`ではなく`base.hocon`を使用することを推奨します。  
これは、`emqx.conf`で設定した場合、ダッシュボードからの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに対応するため、さらに多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## リスナー名の要件

EMQX 6.3.1以降、新規作成されるMQTTリスナーの名前は以下の要件を満たす必要があります。

- 名前は1〜64バイトの長さであること
- 名前はASCIIの英字または数字で始まること
- 名前はASCIIの英字、数字、ハイフン（`-`）、アンダースコア（`_`）のみを含むこと

これらの要件を満たさない名前でのリスナー作成リクエストはEMQXによって拒否されます。アップグレード前に存在し、64バイトを超える名前のMQTTリスナーは、設定の更新や削除は可能ですが、名前の変更はできません。

## EMQXがリスナーアドレスを決定する方法

リスナーアドレスは、EMQXがクライアント接続を受け付けるローカルのネットワークインターフェースとポートを決定します。

リスナーの`bind`設定は、`"0.0.0.0:1883"`のような明示的なIPアドレスとポート、または`1883`のようなポートのみを指定できます。EMQX 6.3.0以降、ノードレベルの`node.default_listener_address`設定が、ポートのみを指定したリスナーのアドレス選択を制御します。

EMQXは以下の順序でアドレスを選択します。

1. `bind`にIPアドレスが含まれている場合、そのアドレスを使用します。`node.default_listener_address`やセキュリティプロファイルはこれを上書きしません。
2. `bind`がポートのみで、かつ`node.default_listener_address`が設定されている場合、その設定で選択されたアドレスを使用します。
3. それ以外の場合、MQTTリスナーはセキュリティプロファイルのデフォルトを使用します。`legacy`ではすべてのネットワークインターフェース、`hardened`ではループバックアドレス（ローカルホストからのみアクセス可能）です。

設定された`bind`値は変更されません。例えば、`bind = 1883`は実行時に特定のIPアドレスが使用されてもポートのみの値のままです。

以下のTCP、SSL、WebSocketの設定例は明示的なIPアドレスを使用しているため、デフォルトリスナーアドレス設定の影響を受けません。

対応する値や起動時の挙動については[Default Listener Address](../access-control/security-profile.md#default-listener-address)を参照してください。公式Dockerイメージは独自のデフォルトを設定しているため、[Listener Addresses in Docker](../../get-started/deploy/install-docker.md#listener-addresses-in-docker)もご覧ください。

## TCPリスナーの設定

TCPリスナーは、特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。TCP/IPネットワーク上でクライアントとEMQX間の接続確立および管理に重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内の`base.hocon`ファイルに`listeners.tcp`の設定項目を追加します。

例えば、ポート`1883`でTCPリスナーを有効化し、最大1,024,000の同時接続を許可する設定は以下の通りです。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default`はリスナーを有効化する設定で、`default`はリスナー名です。任意の名前に変更可能です。
- `bind`はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`へのすべての着信を待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。

## SSLリスナーの設定

SSLリスナーは、着信するSSL（Secure Sockets Layer）接続を待ち受けるネットワークサービスです。EMQXでは、クライアントとEMQX間の通信を暗号化し、ネットワークトラフィックを保護するために使用されます。

EMQXでSSLリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ssl`の設定項目を追加します。

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
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスのポート`8883`からの着信を待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。
  - `verify`: クライアント証明書の真正性を検証する場合は`verify_peer`、そうでなければ`verify_none`を設定します。
  - `fail_if_no_peer_cert`: `true`に設定すると、クライアントが証明書を送信しない（空の証明書を送る）場合に接続を失敗させます。`false`の場合は、無効な証明書を送信した場合のみ失敗します（空の証明書は有効とみなされます）。

## WebSocketリスナーの設定

WebSocketリスナーは、WebSocket経由でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使ってEMQXに接続し、リアルタイムでデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要は[MQTT over WebSocket](../../develop/connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ws`の設定項目を追加します。

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
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスのポート`8083`からの着信を待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、SSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間で交換されるデータを暗号化するWebSocketリスナーです。EMQXでは、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となります。

EMQXでセキュアWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.wss`の設定項目を追加します。

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
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスのポート`8084`からの着信を待ち受けます。
- `max_connections`はリスナーが許可する最大同時接続数で、デフォルトは`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。
- `ssl_options`はリスナーのSSL/TLS設定オプションで、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 各ノードで異なるアドレスを使用する

ダッシュボード、REST API、CLIを通じて行われたリスナー設定の変更はクラスター全体に複製されます。`bind`に特定ノードのIPアドレスを指定すると、そのIPアドレスが他のノードのローカルネットワークインターフェースに設定されていない限り、他ノードでバインドできません。各ノードで異なるアドレスを使用するには、リスナーの`bind`をポートのみとし、ノードごとにデフォルトアドレスを個別に設定してください。

リスナー設定は`base.hocon`で行い、ノードレベルのデフォルトリスナーアドレスは`emqx.conf`または環境変数で設定します。例えば、各ノードのErlangノード名のホスト部分を使用する場合は以下のようにします。

1. ダッシュボードでTCPリスナーの`bind`を`1883`に設定するか、各ノードの`etc/base.hocon`に以下を設定します。

   ```hocon
   listeners.tcp.default.bind = 1883
   ```

   もし優先度の高い設定ソースで既に明示的な`bind`アドレスが設定されている場合は、その設定ソースを更新してください。詳細は[Config Override Rules](./configuration.md#config-override-rules)を参照してください。

2. 各ノードの`emqx.conf`に以下を追加します。

   ```hocon
   node.default_listener_address = "nodename"
   ```

   Docker環境では、`docker run`に`-e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=nodename`を渡すか、Docker Composeの`environment`セクションに`EMQX_NODE__DEFAULT_LISTENER_ADDRESS: nodename`を設定します。これは公式イメージの`all`デフォルトを上書きします。

   EMQXはノード名の`@`以降のホスト部分を使用し、ノード起動時にホスト名を解決します。解決できないホスト名はノード起動を妨げるため、各ノードで利用可能なアドレスに解決されることを確認してください。

3. 各ノードを再起動して`node.default_listener_address`を適用します。この設定はポートのみ指定のMQTTリスナー、ゲートウェイリスナー、ダッシュボードHTTPリスナーに影響します。明示的なIPアドレス指定は変更されません。

ノードの環境変数に`EMQX_NODE__DEFAULT_LISTENER_ADDRESS`を設定することも可能で、環境変数は`emqx.conf`より優先されます。

## リスナーアドレス情報の確認

EMQX 6.3.0以降、リスナーの設定された`bind`を変更せずに解決済みアドレスとその情報源を確認できます。CLIまたはREST APIでノードに問い合わせます。

### CLIでノードを問い合わせる

対象ノードで以下のコマンドを実行します。

```bash
emqx ctl listeners
```

`listen_on`は設定された`bind`、`resolved_address`は解決されたIPアドレス、`resolved_address_from`はアドレスの情報源を示します。`running`でリスナーの稼働状況も確認可能です。停止中のリスナーも解決済みアドレスを報告する場合があります。各フィールドの意味は[Listener Address Information](../cli.md#listener-address-information)を参照してください。空の`resolved_address`の意味も記載されています。

### REST APIでリスナーを問い合わせる

REST APIでリスナーを確認するには、`GET /api/v5/listeners/:id`を使用します。例：`GET /api/v5/listeners/tcp:default`。レスポンスはリクエストを処理したノードのアドレス情報を返します。必要に応じて[API認証](../api.md#authentication)を行ってください。

`bind`フィールドは設定値（ポート含む）を保持し、`resolved_address`と`resolved_address_from`は読み取り専用情報です。アドレスを変更するには`bind`または`node.default_listener_address`を変更し、これらのレスポンスフィールドを編集しないでください。

これらの問い合わせはMQTTリスナーに対応します。ゲートウェイリスナーは[ゲートウェイリスナーの問い合わせ](../../develop/gateway/gateway.md#gateway-listeners)をご利用ください。

## 転送されたクライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスを決定する方法を制御する2つのオプションがあります。

- `websocket.proxy_address_header`: クライアントIPアドレスを含むHTTPヘッダー名を指定します。
- `websocket.proxy_port_header`: クライアントポートを含むHTTPヘッダー名を指定します。

EMQX 6.3.0以降、両オプションのデフォルトは空文字列`""`です。空の場合、EMQXは対応するTCPピアのアドレスまたはポートを使用します。信頼できるプロキシから値を取得するには、`x-forwarded-for`や`x-forwarded-port`などのヘッダー名を明示的に設定してください。

設定されたヘッダーがWebSocketアップグレードリクエストに存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用します。これにより、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査・トレースログがクライアントの送信元IPとして認識します。ヘッダー名の比較は大文字小文字を区別しません。

::: warning 信頼できるプロキシの背後でのみ転送アドレスヘッダーを信頼してください

ヘッダー値はEMQXが使用するクライアント送信元IPを決定するため、信頼できるプロキシが設定した場合のみ尊重してください。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合は、`proxy_address_header`と`proxy_port_header`を空にしてEMQXが常に実際のTCPピアアドレスを使用するようにしてください。
- プロキシが存在しても、受信した`X-Forwarded-For`ヘッダーに追記（append）するだけで上書きや削除をしない場合（多くのプロキシのデフォルト動作、例：NGINXの`$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリは依然としてクライアントが供給したものであり、送信元IPの偽装が可能です。プロキシを設定してヘッダーを観測したアドレスで上書きするか、[PROXYプロトコル](../cluster/lb.md)を使用するか、オプションを空文字列に設定してください。
- 未使用のヘッダー名を指定してこの機能を無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列だけがクライアントが絶対に送信できない値です。

リスナーで`proxy_protocol = true`が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーを設定ゾーンにリンクする

EMQXの各リスナーはゾーンに関連付けられており、デフォルトでは論理ゾーン`default`に設定されています。

リスナーが特定のゾーンにリンクされると、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[Zone Override](./configuration.md#zone-override)セクションを参照してください。

## マウントポイント

各リスナーは`mountpoint`を設定できます。これは、リスナー経由で接続するクライアントが使用するトピックにEMQXが付加するトピックプレフィックスです。`PUBLISH`パケット、`SUBSCRIBE`および`UNSUBSCRIBE`リクエスト、Willメッセージのトピックにプレフィックスが追加され、クライアントに配信されるメッセージのトピックからはプレフィックスが除去されます。マウントポイントはクライアントに透過的であり、マルチテナント環境などでクライアントグループ間のトピック空間を分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは`${clientid}`, `${username}`, `${zone}`, `${client_attrs.NAME}`のプレースホルダーをサポートします。例えば、`mountpoint = "${username}/"`の場合、ユーザー名`u1`のクライアントが`sensors/#`をサブスクライブすると、内部的には`u1/sensors/#`としてサブスクライブされます。

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な`$`プレフィックスで始まるトピックのパブリッシュやサブスクライブによってトリガーされます。EMQXはマウントポイントプレフィックスをこれらのプレフィックスのマッチング前に追加します。例えば、マウントポイント`mp/`のリスナー経由でクライアントが`$delayed/10/t`にパブリッシュすると、ブローカーは`mp/$delayed/10/t`として受け取り、もはや`$delayed/`で始まらないため機能は無効化されます。EMQXはこのメッセージを通常のマウントされたリテラルトピックとしてルーティングし、クライアントにエラーは通知されません。

::: warning 互換性制限
以下の機能を使用するクライアントが接続するリスナーにはマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [Delayed Publish](../../get-started/messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [File Transfer](../../develop/file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [Message Queue](../../develop/message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../../develop/mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [Cluster Linking](../../develop/cluster-linking/introduction.md) | `$LINK/` |
| [Dynamic Keep Alive Adjustment](./mqtt.md#dynamic-keep-alive-adjustment) | `$SETOPTS/` |
| [A2A over MQTT](../../develop/emqx-ai/a2a-over-mqtt/overview.md) | `$a2a/` |

Cluster Linkingでは、リンクされたクラスターからの接続を受け入れるリスナーにマウントポイントを設定してはいけません。A2A over MQTTでは、ちょうど1トピックレベルのマウントポイント（例：`acme/`）は動作します。EMQXは`$a2a`トピックに対して名前空間プレフィックスとして解析します。
:::

[共有サブスクリプション](../../get-started/messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[排他サブスクリプション](../../get-started/messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内部のトピックフィルターにのみ追加されます。例えば、マウントポイント`mp/`のリスナー経由で`$share/g/t`をサブスクライブすると、トピック`mp/t`の共有サブスクリプショングループ`g`に参加します。
