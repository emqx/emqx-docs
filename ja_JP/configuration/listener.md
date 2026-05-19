# リスナー設定

EMQXでは、リスナーはMQTTクライアントからのリクエストを受信するために設定されます。EMQXは以下のメッセージ転送プロトコルをサポートしています。

- TCP: ポート `1883`
- SSL: ポート `8883`
- WebSocketリスナー: `8083`
- セキュアWebSocketリスナー: `8084`

::: tip

リスナーはダッシュボードの左側ナビゲーションメニューから **Management** -> **Listeners** をクリックして設定することも可能です。  
設定ファイルからリスナーを設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。  
これは、`emqx.conf` に設定を記述すると、ダッシュボードからの変更が一時的なものとなり、EMQX再起動時に失われるためです。

:::

::: tip

EMQXはカスタマイズニーズに応じたより多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

:::

## TCPリスナーの設定

TCPリスナーは特定のネットワークポートで着信TCP接続を待ち受けるネットワークサービスです。クライアントとEMQX間のTCP/IPネットワーク上での接続確立および管理に重要な役割を果たします。

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
- `bind` はリスナーのIPアドレスとポートを設定し、ここでは任意のIPアドレスからのポート `1883` への着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数を設定します。デフォルト値は `infinity`（無制限）です。

## SSLリスナーの設定

SSLリスナーはSSL（Secure Sockets Layer）接続の着信を待ち受けるネットワークサービスです。EMQXではクライアントとEMQX間のネットワークトラフィックを暗号化し、安全に通信するために使用されます。

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

- `listeners.ssl.default` はリスナーを有効化する設定です。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート `8883` への着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `ssl_options` はリスナーのSSL/TLS設定で、以下のプロパティがあります。  
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼済みCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。  
  - `verify`: クライアント証明書の真正性を検証する場合は `verify_peer`、検証しない場合は `verify_none` を設定します。  
  - `fail_if_no_peer_cert`: `true` に設定すると、クライアントが証明書を送信しない（空の証明書を送る）場合に接続を失敗させます。`false` の場合は無効な証明書を送信した場合のみ失敗し、空の証明書は有効とみなします。

## WebSocketリスナーの設定

WebSocketリスナーはWebSocket経由でメッセージを受信・処理するネットワークサービスです。EMQXのWebSocketサポートにより、クライアントはWebSocketプロトコルを使ってEMQXに接続し、リアルタイムでデータ交換が可能になります。

MQTT over WebSocketの仕組みや典型的な利用シーンの概要は、[MQTT over WebSocket](../connect-emqx/mqtt-over-websocket.md)をご覧ください。

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

- `listeners.ws.default` はリスナーを有効化する設定です。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート `8083` への着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。

## セキュアWebSocketリスナーの設定

セキュアWebSocketリスナーは、Secure Sockets Layer（SSL）またはTransport Layer Security（TLS）プロトコルを使用してWebSocketクライアントとブローカー間のデータを暗号化するWebSocketリスナーです。EMQXでは、WebSocketクライアントとEMQX間で交換される機密データを保護する重要なセキュリティ対策となっています。

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

- `listeners.wss.default` はリスナーを有効化する設定です。  
- `bind` はリスナーのIPアドレスとポートで、任意のIPアドレスからのポート `8084` への着信を待ち受けます。  
- `max_connections` はリスナーが許可する最大同時接続数で、デフォルトは `infinity` です。  
- `websocket.mqtt_path` はWebSocketのMQTTプロトコルのパスを設定し、デフォルトは `/mqtt` です。  
- `ssl_options` はリスナーのSSL/TLS設定で、以下のプロパティがあります。  
  - `cacertfile`: クライアント証明書の真正性を検証するためにリスナーが使用する信頼済みCA（認証局）証明書を含むPEMファイル。  
  - `certfile`: リスナーのSSL/TLS証明書チェーンを含むPEMファイル。証明書がルートCAから直接発行されていない場合、中間CA証明書をリスナー証明書の後に連結してチェーンを形成します。  
  - `keyfile`: SSL/TLS証明書に対応する秘密鍵を含むPEMファイル。

<!--To add QUIC-->

<!--To add code sample for adding multiple listeners.-->

## リスナーと設定ゾーンの紐付け

EMQXの各リスナーはゾーンに紐付けられており、デフォルトでは論理ゾーン `default` に設定されています。

リスナーが特定のゾーンに紐付けられると、そのリスナーに接続するMQTTクライアントは紐付けられたゾーンの設定を継承します。

詳細は設定ドキュメントの[Zone Override](./configuration.md#zone-override)セクションをご参照ください。
