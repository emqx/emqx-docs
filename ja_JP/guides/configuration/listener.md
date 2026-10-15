# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受け取るために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf`ではなく`base.hocon`を使用することを推奨します。  
これは、`emqx.conf`で設定した場合、ダッシュボード経由での変更は一時的なものとなり、EMQXの再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに対応するため、より多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::

## リスナー名の要件

新規作成するMQTTリスナーの名前は以下の要件を満たす必要があります。

- 名前は1〜64バイトの長さであること
- 名前はASCIIの英字または数字で始まること
- 名前はASCIIの英字、数字、ハイフン（`-`）、アンダースコア（`_`）のみを含むこと

これらの要件を満たさない場合、EMQXはリスナー作成要求を拒否します。アップグレード前に存在し、64バイトを超える名前のMQTTリスナーについては、設定の更新や削除は可能ですが、名前の変更はできません。

## EMQXがリスナーアドレスを決定する方法

リスナーアドレスは、EMQXがクライアント接続を受け付けるローカルのネットワークインターフェースとポートを決定します。

リスナーの`bind`設定は、`"0.0.0.0:1883"`のような明示的なIPアドレスとポート、または`1883`のようなポートのみを受け付けます。EMQX 6.3.0以降では、ノードレベルの`node.default_listener_address`設定が、ポートのみを指定したリスナーのアドレス選択を制御します。

EMQXは以下の順序でアドレスを選択します。

1. `bind`にIPアドレスが含まれている場合、EMQXはそのアドレスを使用します。`node.default_listener_address`やセキュリティプロファイルはこれを上書きしません。
2. `bind`がポートのみで、かつ`node.default_listener_address`が設定されている場合、EMQXはその設定で選択されたローカルノードのアドレスを使用します。
3. それ以外の場合、MQTTリスナーはセキュリティプロファイルのデフォルトを使用します。`legacy`ではすべてのネットワークインターフェース、`hardened`ではループバックアドレス（ローカルホストのみアクセス可能）です。

設定された`bind`値は変更されません。例えば、`bind = 1883`はランタイムで特定のIPアドレスを使用してもポートのみの値のままです。

以下のTCP、SSL、WebSocketの設定例は明示的なIPアドレスを使用しているため、デフォルトリスナーアドレス設定の影響を受けません。

対応する値や起動時の挙動については[Default Listener Address](../access-control/security-profile.md#default-listener-address)を参照してください。公式Dockerイメージは独自のデフォルトを設定しているため、[Listener Addresses in Docker](../../get-started/deploy/install-docker.md#listener-addresses-in-docker)もご覧ください。

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートでTCP接続の受け入れを行うネットワークサービスです。クライアントとEMQX間のTCP/IPネットワーク上の接続確立および管理に重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダー内の`base.hocon`ファイルに`listeners.tcp`の設定項目を追加します。

例えば、ポート`1883`でTCPリスナーを有効化し、最大1,024,000の同時接続を許可する場合、以下のように設定します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default`はリスナーを有効化する設定で、`default`はリスナー名です。任意の名前に変更可能です。
- `bind`はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`への全ての着信トラフィックを受け入れます。
- `max_connections`はリスナーが許可する最大同時接続数を設定します。デフォルトは`infinity`です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続の受け入れを行うネットワークサービスです。EMQXではクライアントとEMQX間の通信を暗号化し、ネットワークトラフィックのセキュリティを確保します。

EMQXでSSLリスナーを設定するには、`etc`フォルダー内の`base.hocon`ファイルに`listeners.ssl`の設定項目を追加します。

例えば、ポート`8883`でSSLリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように設定します。

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

- `listeners.ssl.default`はリスナーを有効化します。
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8883`への全着信を受け入れます。
- `max_connections`は最大同時接続数で、デフォルトは`infinity`です。
- `ssl_options`はリスナーのSSL/TLS設定で、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。
  - `verify`: クライアント証明書の真正性を検証する場合は`verify_peer`、検証しない場合は`verify_none`を設定します。
  - `fail_if_no_peer_cert`: `true`の場合、クライアントが証明書を送信しない（空の証明書）と接続を失敗させます。`false`の場合は、無効な証明書を送信した場合のみ失敗します（空の証明書は有効とみなされます）。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket経由でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを用いてEMQXに接続し、リアルタイムにデータを交換できます。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要は[MQTT over WebSocket](../../develop/connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、`etc`フォルダー内の`base.hocon`ファイルに`listeners.ws`の設定項目を追加します。

例えば、ポート`8083`でWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように設定します。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default`はリスナーを有効化します。
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8083`への全着信を受け入れます。
- `max_connections`は最大同時接続数で、デフォルトは`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーはSSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間のデータを暗号化するWebSocketリスナーです。EMQXでは、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となっています。

EMQXでセキュアWebSocketリスナーを設定するには、`etc`フォルダー内の`base.hocon`ファイルに`listeners.wss`の設定項目を追加します。

例えば、ポート`8084`でセキュアWebSocketリスナーを有効化し、最大1,024,000の同時接続を許可する場合は以下のように設定します。

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

- `listeners.wss.default`はリスナーを有効化します。
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8084`への全着信を受け入れます。
- `max_connections`は最大同時接続数で、デフォルトは`infinity`です。
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。
- `ssl_options`はリスナーのSSL/TLS設定で、以下のプロパティを持ちます。
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA証明書を含むPEMファイル。
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 各ノードで異なるアドレスを使用する

ダッシュボード、REST API、CLIを通じて行われたリスナー設定の変更はクラスター全体に複製されます。`bind`に特定のノードのIPアドレスを指定すると、そのIPアドレスが他ノードのローカルネットワークインターフェースに設定されていない限り、他ノードではバインドできません。  
各ノードで異なるアドレスを使用するには、リスナーの`bind`をポートのみの設定にして、ノードごとにデフォルトアドレスを別途設定してください。

リスナー設定は`base.hocon`で行い、ノードレベルのデフォルトリスナーアドレスは`emqx.conf`または環境変数で設定します。例えば、各ノードのErlangノード名のホスト部分を使用する場合は以下の手順です。

1. ダッシュボードでTCPリスナーの`bind`を`1883`に設定するか、各ノードの`etc/base.hocon`に以下を設定します。

   ```hocon
   listeners.tcp.default.bind = 1883
   ```

   すでに優先度の高い設定ソースで明示的なバインドアドレスが設定されている場合は、その設定を更新してください。詳細は[Config Override Rules](./configuration.md#config-override-rules)を参照してください。

2. 各ノードの`emqx.conf`に以下を追加します。

   ```hocon
   node.default_listener_address = "nodename"
   ```

   Docker環境では、`docker run`に`-e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=nodename`を渡すか、Docker Composeの`environment`セクションに`EMQX_NODE__DEFAULT_LISTENER_ADDRESS: nodename`を設定します。これにより、公式イメージの`all`デフォルト（`emqx.conf`より優先）を上書きします。

   EMQXはノード名の`@`以降のホスト部分を使用し、ホスト名の場合はノード起動時に解決します。ノードで利用可能なアドレスに解決されることを確認してください。解決できないホスト名はノードの起動を妨げます。

3. 各ノードを再起動して`node.default_listener_address`を適用します。この設定はポートのみのバインドを持つMQTTリスナー、ゲートウェイリスナー、およびそのノードのダッシュボードHTTPリスナーに影響します。明示的なIPアドレスを持つリスナーのバインドは変更されません。

ノードの環境変数に`EMQX_NODE__DEFAULT_LISTENER_ADDRESS`を設定することも可能です。環境変数は`emqx.conf`より優先されます。

## リスナーアドレス情報の確認

EMQX 6.3.0以降、リスナーの設定された`bind`を変更せずに解決済みアドレスとその情報源を確認できます。CLIまたはREST APIでノードを照会してください。

### CLIでノードを照会する

確認したいノード上で以下のコマンドを実行します。

```bash
emqx ctl listeners
```

`listen_on`は設定されたバインド、`resolved_address`は解決済みIPアドレス、`resolved_address_from`はアドレスの情報源を示します。`running`でリスナーが稼働中かも確認可能です。停止中のリスナーでも解決済みアドレスを報告する場合があります。フィールドの意味は[Listener Address Information](../cli.md#listener-address-information)を参照してください。空の`resolved_address`の意味も記載されています。

### REST APIでリスナーを照会する

REST APIでリスナーを確認するには、`GET /api/v5/listeners/:id`を使用します。例：`GET /api/v5/listeners/tcp:default`。レスポンスはリクエストを処理したノードのアドレスを返します。必要に応じて[API認証](../api.md#authentication)を利用してください。

`bind`フィールドは設定値（ポート含む）を保持します。`resolved_address`と`resolved_address_from`は読み取り専用の情報であり、アドレスを変更するには`bind`または`node.default_listener_address`を変更してください。レスポンスのこれらのフィールドを直接編集しても反映されません。

これらの照会はMQTTリスナーを対象としています。ゲートウェイリスナーは[ゲートウェイリスナー照会](../../develop/gateway/gateway.md#gateway-listeners)を参照してください。

## 転送元クライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーには、リスナーがプロキシやロードバランサーの背後にある場合にEMQXがクライアントの送信元アドレスを決定する方法を制御する2つのオプションがあります。

- `websocket.proxy_address_header`: クライアントIPアドレスを含むHTTPヘッダー名を指定します。
- `websocket.proxy_port_header`: クライアントポートを含むHTTPヘッダー名を指定します。

EMQX 6.3.0以降、両オプションのデフォルトは空文字列（`""`）です。空の場合、EMQXは対応するTCPピアのアドレスまたはポートを使用します。信頼できるプロキシから値を取得するには、`x-forwarded-for`や`x-forwarded-port`などのヘッダー名を明示的に設定してください。

設定されたヘッダーがWebSocketアップグレード要求に存在する場合、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用します。これにより、IPベースの認可ルール、禁止クライアント、フラッピング検出、監査およびトレースログはこの派生アドレスをクライアントの送信元IPとして認識します。設定されたヘッダー名は大文字・小文字を区別せずにマッチします。

::: warning 信頼できるプロキシの背後でのみ転送元アドレスヘッダーを信頼してください

ヘッダー値はEMQXが使用するクライアント送信元IPを決定するため、以下の場合のみ信頼してください。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合は、`proxy_address_header`と`proxy_port_header`を空にして、常に実際のTCPピアアドレスを使用させてください。
- プロキシが`X-Forwarded-For`ヘッダーを上書きせずに追記する場合（多くのプロキシのデフォルト動作、例: NGINXの`$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリはクライアント由来のままなので、送信元IPを偽装される可能性があります。プロキシを設定してヘッダーを上書きさせるか、[PROXYプロトコル](../cluster/lb.md)を使用するか、オプションを空文字列に設定してください。
- 未使用のヘッダー名を指定してこの機能を無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列のみがクライアントが送信できない値です。

リスナーで`proxy_protocol = true`が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーを設定ゾーンにリンクする

EMQXの各リスナーはゾーンに紐付けられており、デフォルトでは`default`という論理ゾーンに設定されています。

リスナーが特定のゾーンにリンクされている場合、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[Zone Override](./configuration.md#zone-override)セクションを参照してください。

## マウントポイント

各リスナーは`mountpoint`を設定可能で、これはEMQXがそのリスナー経由で接続するクライアントのトピックに付加するトピックプレフィックスです。  
このプレフィックスは`PUBLISH`パケット、`SUBSCRIBE`および`UNSUBSCRIBE`リクエスト、Willメッセージのトピックに付加され、クライアントに配信されるメッセージのトピックからは取り除かれます。  
マウントポイントはクライアントには透過的であり、マルチテナント環境などでクライアントグループ間のトピックスペースを分離するために一般的に使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは`${clientid}`, `${username}`, `${zone}`, `${client_attrs.NAME}`のプレースホルダーをサポートします。例えば、`mountpoint = "${username}/"`の場合、ユーザー名`u1`のクライアントが`sensors/#`をサブスクライブすると、内部的には`u1/sensors/#`として扱われます。

::: warning マウントポイントは認可チェックの前に追加されません
EMQXは認可ルールをリスナーのマウントポイントを付加しないトピックに対して評価します。マウントポイントは認可チェック後に追加されます。これはパブリッシュ、サブスクライブ操作およびWillメッセージに適用されます。

マウントポイント付きリスナーに対しては、マウントポイントを付加しないトピックに対して認可ルールを書いてください。例えば、`department-a/secret/#`に対する拒否ルールは、`secret/#`をそのリスナー経由でパブリッシュまたはサブスクライブするクライアントにはマッチしません。
:::

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、`$`で始まる特別なトピックプレフィックスに対してトリガーされます。EMQXはマウントポイントプレフィックスをこれらのプレフィックスのマッチング前に追加します。例えば、マウントポイント`mp/`のリスナー経由でクライアントが`$delayed/10/t`にパブリッシュすると、ブローカーは`mp/$delayed/10/t`として受信し、`$delayed/`で始まらなくなるため、機能は無効化されます。EMQXはメッセージを通常のマウントされたリテラルトピックとしてルーティングし、クライアントにエラーは報告されません。

::: warning 互換性制限
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

Cluster Linkingでは、リンク先クラスターからの接続を受け入れるリスナーにマウントポイントを設定してはいけません。A2A over MQTTでは、1トピックレベルのマウントポイント（例: `acme/`）は動作します。EMQXは`$a2a`トピックに対して名前空間プレフィックスとして解析します。
:::

[Shared subscriptions](../../get-started/messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[exclusive subscriptions](../../get-started/messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析するため、マウントポイントは内部のトピックフィルターのみに追加されます。例えば、マウントポイント`mp/`のリスナー経由で`$share/g/t`をサブスクライブすると、`g`グループの共有サブスクリプショングループに参加し、トピックは`mp/t`となります。
