# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受け取るために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューの **Management** -> **Listeners** からも設定可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf`ではなく`base.hocon`の使用を推奨します。  
これは、`emqx.conf`で設定した場合、ダッシュボードからの変更は一時的なものとなり、EMQX再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに対応するため、より多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## EMQXがリスナーアドレスを決定する方法

リスナーアドレスは、EMQXがクライアント接続を受け付けるローカルのネットワークインターフェースとポートを決定します。

リスナーの`bind`設定は、`"0.0.0.0:1883"`のような明示的なIPアドレスとポート、または`1883`のようなポートのみを受け入れます。EMQX 6.3.0以降では、ノードレベルの`node.default_listener_address`設定が、ポートのみを指定したリスナーのアドレス決定を制御します。

EMQXは以下の順序でアドレスを選択します。

1. `bind`にIPアドレスが含まれている場合、EMQXはそのアドレスを使用します。`node.default_listener_address`やセキュリティプロファイルはこれを上書きしません。  
2. `bind`がポートのみで`node.default_listener_address`が設定されている場合、EMQXはその設定で選択されたローカルノードのアドレスを使用します。  
3. それ以外の場合、MQTTリスナーはセキュリティプロファイルのデフォルトを使用します。`legacy`では全ネットワークインターフェース、`hardened`ではループバックアドレスです。ループバックアドレスはローカルホストからのみアクセス可能です。

設定された`bind`値は変更されません。例えば、`bind = 1883`は実行時に特定のIPアドレスが使われてもポートのみの値のままです。

以下のTCP、SSL、WebSocketの設定例は明示的なIPアドレスを使用しているため、デフォルトリスナーアドレス設定の影響を受けません。

サポートされる値や起動時の挙動については[Default Listener Address](../access-control/security-profile.md#default-listener-address)を参照してください。公式Dockerイメージは独自のデフォルトを設定しているため、[Listener Addresses in Docker](../deploy/install-docker.md#listener-addresses-in-docker)もご確認ください。

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートでのTCP接続を待ち受けるネットワークサービスです。クライアントとEMQX間のTCP/IPネットワーク上の接続確立と管理に重要な役割を果たします。

EMQXでTCPリスナーを設定するには、EMQXインストールディレクトリの`etc`フォルダ内の`base.hocon`ファイルに`listeners.tcp`の設定項目を追加します。

例えば、ポート`1883`でTCPリスナーを有効化し、リスナーが許可する最大同時接続数を1,024,000に設定する場合、以下のように記述します。

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = 1024000
}
```

ここで、

- `listeners.tcp.default`はリスナーを有効化する設定で、`default`はリスナー名です。任意の名前に変更可能です。  
- `bind`はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート`1883`への全ての着信トラフィックを待ち受けます。  
- `max_connections`はリスナーが許可する最大同時接続数を設定します。デフォルトは`infinity`です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続を待ち受けるネットワークサービスです。EMQXではクライアントとEMQX間の通信を暗号化して保護するために使用されます。

EMQXでSSLリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ssl`の設定項目を追加します。

例えば、ポート`8883`でSSLリスナーを有効化し、最大同時接続数を1,024,000に設定する場合は以下のようにします。

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
- `bind`はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート`8883`への全ての着信トラフィックを待ち受けます。  
- `max_connections`は最大同時接続数で、デフォルトは`infinity`です。  
- `ssl_options`はリスナーのSSL/TLS設定で、以下のプロパティがあります。  
  - `cacertfile`: クライアント証明書の真正性を検証するために使用する信頼されたCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナー用のSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。  
  - `verify`: クライアント証明書の真正性を検証する場合は`verify_peer`、しない場合は`verify_none`を設定します。  
  - `fail_if_no_peer_cert`: `true`の場合、クライアントが証明書を送信しない（空の証明書を送る）とサーバーは接続を拒否します。`false`の場合は無効な証明書を送信した場合のみ拒否し、空の証明書は有効とみなします。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocketを介してメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使ってEMQXに接続し、リアルタイムでデータ交換が可能です。

MQTT over WebSocketの仕組みや典型的な利用シナリオについては[MQTT over WebSocket](../connect-emqx/mqtt-over-websocket.md)をご覧ください。

EMQXでWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.ws`の設定項目を追加します。

例えば、ポート`8083`でWebSocketリスナーを有効化し、最大同時接続数を1,024,000に設定する場合は以下のようにします。

```bash
listeners.ws.default {
  bind = "0.0.0.0:8083"
  max_connections = 1024000
  websocket.mqtt_path = "/mqtt"
}
```

ここで、

- `listeners.ws.default`はリスナーを有効化します。  
- `bind`はIPアドレスとポートで、任意のIPアドレスからのポート`8083`への全ての着信トラフィックを待ち受けます。  
- `max_connections`は最大同時接続数で、デフォルトは`infinity`です。  
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、SSLまたはTLSプロトコルを使用してWebSocketクライアントとブローカー間のデータを暗号化するWebSocketリスナーです。EMQXでは、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となっています。

EMQXでセキュアWebSocketリスナーを設定するには、`etc`フォルダ内の`base.hocon`ファイルに`listeners.wss`の設定項目を追加します。

例えば、ポート`8084`でセキュアWebSocketリスナーを有効化し、最大同時接続数を1,024,000に設定する場合は以下のようにします。

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
- `bind`はIPアドレスとポートで、任意のIPアドレスからのポート`8084`への全ての着信トラフィックを待ち受けます。  
- `max_connections`は最大同時接続数で、デフォルトは`infinity`です。  
- `websocket.mqtt_path`はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは`/mqtt`です。  
- `ssl_options`はSSL/TLS設定で、以下のプロパティがあります。  
  - `cacertfile`: クライアント証明書の真正性を検証するための信頼されたCA証明書を含むPEMファイル。  
  - `certfile`: リスナー用のSSL/TLS証明書チェーンを含むPEMファイル。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

## 各ノードで異なるアドレスを使用する

ダッシュボード、REST API、CLIを通じて行ったリスナー設定の変更はクラスター全体に複製されます。`bind`に特定ノードのIPアドレスを設定すると、そのIPアドレスが他ノードのローカルネットワークインターフェースに設定されていない限り、他ノードではバインドできません。各ノードで異なるアドレスを使用するには、リスナーの`bind`をポートのみとし、ノードごとにデフォルトアドレスを別途設定してください。

リスナー設定は`base.hocon`で行い、ノードレベルのデフォルトリスナーアドレスは`emqx.conf`または環境変数で設定します。例えば、各ノードのErlangノード名のホスト部を使用する場合は以下の手順です。

1. ダッシュボードでTCPリスナーの`bind`を`1883`に設定するか、各ノードの`etc/base.hocon`に以下を設定します。

   ```hocon
   listeners.tcp.default.bind = 1883
   ```

   もし優先度の高い設定ソースで既に明示的なバインドアドレスが設定されている場合は、その設定ソースを更新してください。詳細は[Config Override Rules](./configuration.md#config-override-rules)を参照してください。

2. 各ノードの`emqx.conf`に以下を追加します。

   ```hocon
   node.default_listener_address = "nodename"
   ```

   Docker環境の場合は、`docker run`に`-e EMQX_NODE__DEFAULT_LISTENER_ADDRESS=nodename`を渡すか、Docker Composeの`environment`セクションに`EMQX_NODE__DEFAULT_LISTENER_ADDRESS: nodename`を設定してください。これは公式イメージの`all`デフォルトを上書きします。

   EMQXはノード名の`@`以降のホスト部を使用し、ノード起動時にホスト名を解決します。ノードで利用可能なアドレスに解決されることを確認してください。解決できないホスト名はノードの起動を妨げます。

3. 各ノードを再起動して`node.default_listener_address`を適用します。この設定はポートのみのバインドに対してMQTTリスナー、ゲートウェイリスナー、ダッシュボードHTTPリスナーに影響します。明示的なIPアドレスを指定したリスナーのバインドは変更されません。

環境変数`EMQX_NODE__DEFAULT_LISTENER_ADDRESS`をノードの環境に設定することも可能で、環境変数は`emqx.conf`より優先されます。

## リスナーアドレス情報の確認

EMQX 6.3.0以降、リスナーの設定済み`bind`を変更せずに解決済みアドレスとその情報源を確認できます。CLIまたはREST APIでノードをクエリしてください。

### CLIでノードをクエリする

確認したいノード上で以下のコマンドを実行します。

```bash
emqx ctl listeners
```

`listen_on`は設定されたバインド、`resolved_address`は解決済みIP、`resolved_address_from`はアドレスの情報源を示します。`running`でリスナーが稼働中かも確認可能です。停止中のリスナーでも解決済みアドレスを報告する場合があります。各フィールドの意味は[Listener Address Information](../admin/cli.md#listener-address-information)を参照してください。`resolved_address`が空の場合の意味も記載されています。

### REST APIでリスナーをクエリする

REST APIでリスナーを確認するには、`GET /api/v5/listeners/:id`を使用します。例：`GET /api/v5/listeners/tcp:default`。レスポンスはリクエストを処理したノードのアドレスを報告します。必要に応じて[API認証](../admin/api.md#authentication)を行ってください。

`bind`フィールドは設定値（ポート含む）を保持し、`resolved_address`と`resolved_address_from`は読み取り専用の情報です。アドレスを変更するには`bind`または`node.default_listener_address`を変更してください。これらのレスポンスフィールドを編集しても反映されません。

これらのクエリはMQTTリスナーに対応しています。ゲートウェイリスナーは[ゲートウェイリスナーのクエリ](../gateway/gateway.md#listener)を参照してください。

## 転送元クライアントアドレス（WebSocketリスナー）

WebSocketおよびセキュアWebSocketリスナーは、リスナーがプロキシやロードバランサーの背後にある場合にクライアントの送信元アドレスを決定する方法を制御する2つのオプションを持ちます。

- `websocket.proxy_address_header`: クライアントIPアドレスを含むHTTPヘッダー名を指定します。  
- `websocket.proxy_port_header`: クライアントポートを含むHTTPヘッダー名を指定します。

EMQX 6.3.0以降、両オプションのデフォルトは空文字列`""`です。空の場合はEMQXは対応するTCPピアアドレスまたはポートを使用します。信頼できるプロキシから値を取得するには、`x-forwarded-for`や`x-forwarded-port`などのヘッダー名を明示的に設定してください。

設定されたヘッダーがWebSocketアップグレードリクエストに存在すると、EMQXはヘッダー値の最初（左端）のエントリをクライアントの送信元IPアドレス（またはポート）として使用し、実際のTCPピアのアドレスではなくなります。派生したアドレスはIPベースの認可ルール、禁止クライアント、フラッピング検出、監査・トレースログでクライアントの送信元IPとして扱われます。設定されたヘッダー名は大文字・小文字を区別しません。

::: warning 信頼できるプロキシの背後でのみ転送元アドレスヘッダーを信用してください

ヘッダー値はEMQXが使用するクライアント送信元IPを決定するため、信頼できるプロキシが設定した場合にのみ尊重すべきです。

- リスナーがクライアントから直接アクセス可能（プロキシなし）の場合は、`proxy_address_header`と`proxy_port_header`を空にして、常に実際のTCPピアアドレスを使用してください。  
- プロキシが存在しても、受信した`X-Forwarded-For`ヘッダーに追記する（上書きや削除しない）場合（多くのプロキシのデフォルト動作、例：NGINXの`$proxy_add_x_forwarded_for`）、EMQXが読み取る左端のエントリはクライアントが送信したもののままであり、送信元IPを偽装される可能性があります。プロキシを設定してヘッダーを観測したアドレスで上書きするか、[PROXYプロトコル](../deploy/cluster/lb.md)を使用するか、オプションを空文字列に設定してください。  
- 未使用のヘッダー名を指定してこの仕組みを無効化しようとしないでください。クライアントは任意の名前のヘッダーを送信可能であり、空文字列のみがクライアントが絶対に送信できない値です。

リスナーで`proxy_protocol = true`が設定されている場合、クライアントアドレスはPROXYプロトコルのハンドシェイクから取得され、これらのヘッダーは参照されません。  
:::

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーを設定ゾーンに紐付ける

EMQXの各リスナーはゾーンに紐付けられており、デフォルトでは`default`という論理ゾーンに設定されています。

リスナーが特定のゾーンに紐付けられると、そのリスナーに接続するMQTTクライアントはそのゾーンの設定を継承します。

詳細は設定ドキュメントの[Zone Override](./configuration.md#zone-override)セクションを参照してください。

## マウントポイント

各リスナーは`mountpoint`を設定できます。これは、リスナー経由で接続するクライアントが使用するトピックにEMQXが追加するトピックプレフィックスです。プレフィックスは`PUBLISH`パケット、`SUBSCRIBE`および`UNSUBSCRIBE`リクエスト、Willメッセージのトピックに追加され、クライアントに配信されるメッセージのトピックからは削除されます。マウントポイントはクライアントには透過的であり、マルチテナント環境などでクライアントグループ間のトピック空間を分離するためによく使用されます。

```bash
listeners.tcp.demo {
    bind = "0.0.0.0:1883"
    mountpoint = "department-a/"
}
```

マウントポイントは`${clientid}`, `${username}`, `${zone}`, `${client_attrs.NAME}`のプレースホルダーをサポートします。例えば、`mountpoint = "${username}/"`の場合、ユーザー名`u1`のクライアントが`sensors/#`をサブスクライブすると、内部的には`u1/sensors/#`としてサブスクライブされます。

### トピックプレフィックス拡張機能との非互換性

EMQXのいくつかの機能は、特別な`$`プレフィックスで始まるトピックのパブリッシュやサブスクライブによってトリガーされます。EMQXはマウントポイントのプレフィックスをこれらのプレフィックスのマッチング前に追加します。例えば、マウントポイント`mp/`のリスナー経由でクライアントが`$delayed/10/t`にパブリッシュすると、ブローカーは`mp/$delayed/10/t`として受け取り、もはや`$delayed/`で始まらないため機能は無効化されます。EMQXはメッセージを通常のマウントされたリテラルトピックとしてルーティングし、クライアントにエラーは報告されません。

::: warning 互換性の制限
以下の機能を使用するクライアントが接続するリスナーにはマウントポイントを設定しないでください。

| 機能 | トピックプレフィックス |
| --- | --- |
| [遅延パブリッシュ](../messaging/mqtt-delayed-publish.md) | `$delayed/` |
| [ファイル転送](../file-transfer/introduction.md) | `$file/`, `$file-async/`, `$file-response/` |
| [メッセージキュー](../message-queue/message-queue-concept.md) | `$queue/` |
| [MQTT Streams](../mqtt-stream/mqtt-stream-concept.md) | `$stream/` |
| [クラスターリンク](../cluster-linking/introduction.md) | `$LINK/` |
| [動的キープアライブ調整](./mqtt.md#dynamic-keep-alive-adjustment) | `$SETOPTS/` |
| [A2A over MQTT](../emqx-ai/a2a-over-mqtt/overview.md) | `$a2a/` |

クラスターリンクの場合、リンク先クラスターからの接続を受け入れるリスナーにはマウントポイントを設定してはいけません。A2A over MQTTの場合、ちょうど1トピックレベル（例：`acme/`）のマウントポイントは動作します。EMQXは`$a2a`トピックの名前空間プレフィックスとして解析します。
:::

[共有サブスクリプション](../messaging/mqtt-shared-subscription.md)（`$share/{group}/`）および[排他サブスクリプション](../messaging/mqtt-exclusive-subscription.md)（`$exclusive/`）は例外で、マウントポイントと共に動作します。EMQXはこれらのサブスクリプションプレフィックスをマウントポイント適用前に解析し、マウントポイントは内部のトピックフィルターにのみ追加されます。例えば、マウントポイント`mp/`のリスナー経由で`$share/g/t`をサブスクライブすると、共有サブスクリプショングループ`g`はトピック`mp/t`に参加します。
