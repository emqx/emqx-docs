# IPv6

EMQXは、クライアント接続、ダッシュボード、ノード間クラスタリング、および外部サービスへのアウトバウンド接続に対してIPv6を完全にサポートしています。本ページでは、シングルスタック（IPv6のみ）からデュアルスタック構成まで、IPv6環境におけるEMQXの設定方法を説明します。

## MQTTリスナー

IPv6経由でMQTTクライアント接続を受け入れるには、リスナーをIPv6アドレスにバインドします。EMQXはIPv6のバインドアドレスを検出すると、自動的に`inet6`ソケットオプションを有効にします。

### デュアルスタック（IPv4およびIPv6）

同じポートでIPv4とIPv6の両方の接続を受け入れるには、`[::]`にバインドします。

```bash
listeners.tcp.default {
  bind = "[::]:1883"
}
```

::: tip

ほとんどのOSでは、`[::]`にバインドするとデフォルトでIPv4とIPv6の両方の接続を受け入れます（デュアルスタック）。両プロトコルをサポートする環境では最も簡単な設定です。

:::

### IPv6のみ

リスナーをIPv6接続のみに制限するには、`ipv6_v6only = true`を設定します。

```bash
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}
```

これは`IPV6_V6ONLY`ソケットオプションを設定し、IPv4マップドIPv6アドレスの受け入れを防ぎます。

### 特定のIPv6アドレスにバインド

特定のIPv6アドレスにバインドすることも可能です。

```bash
listeners.tcp.default {
  bind = "[::1]:1883"
}
```

同じ設定はSSL、WebSocket、Secure WebSocketリスナーにも適用されます。

```bash
listeners.ssl.default {
  bind = "[::]:8883"
  ssl_options {
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    cacertfile = "etc/certs/cacert.pem"
  }
}

listeners.ws.default {
  bind = "[::]:8083"
}

listeners.wss.default {
  bind = "[::]:8084"
}
```

## ダッシュボードHTTP/HTTPSリスナー

EMQXダッシュボードのHTTP/HTTPSリスナーもIPv6をサポートしています。

### IPv6バインドアドレスを使用

`bind`アドレスがIPv6の場合、EMQXはダッシュボードリスナーで自動的にIPv6を有効にします。

```bash
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

### `inet6`フラグを使用

明示的なIPアドレスなしでポートのみをバインドする場合は、IPv6を明示的に有効にできます。

```bash
dashboard.listeners.http {
  bind = 18083
  inet6 = true
}
```

| パラメーター       | 型       | デフォルト | 説明                                                                                   |
| ------------------ | -------- | ---------- | -------------------------------------------------------------------------------------- |
| `inet6`            | boolean  | `false`    | IPv6サポートを有効にします。`false`の場合、リスナーはIPv4トラフィックのみ受け入れます。   |
| `ipv6_v6only`      | boolean  | `false`    | IPv4からIPv6へのマッピングを無効にします。`inet6`が`true`の場合のみ有効です。             |

## クラスター通信

EMQXクラスター内のノードがIPv6ネットワークで通信する場合、2つのコンポーネントを設定する必要があります。クラスタ調整に使用されるErlang分散プロトコルと、ノード間のデータ転送に使用されるGen RPCチャネルです。

### Erlang分散プロトコル

ノード間通信にIPv6を使用するには、`cluster.proto_dist`を設定します。

```bash
cluster.proto_dist = inet6_tcp
```

利用可能なオプション：

| 値             | 説明                                                      |
| -------------- | --------------------------------------------------------- |
| `inet_tcp`     | IPv4上のTCP（デフォルト）                                  |
| `inet6_tcp`    | IPv6上のTCP                                               |
| `inet_tls`     | IPv4上のTLS、`etc/ssl_dist.conf`で設定                    |
| `inet6_tls`    | IPv6上のTLS、`etc/ssl_dist.conf`で設定                    |

::: warning 重要なお知らせ

IPv6ノード名（例：`emqx@::1`）を使用する場合、`cluster.proto_dist`を必ず`inet6_tcp`または`inet6_tls`に設定してください。設定しないと、「not responding to pings」などのエラーでノードが起動に失敗します。

:::

### Gen RPC

Gen RPCチャネルをIPv6用に設定します。

```bash
rpc.listen_address = "::"
rpc.ipv6_only = true
```

| パラメーター           | 型       | デフォルト   | 説明                                                                                       |
| ---------------------- | -------- | ------------ | ------------------------------------------------------------------------------------------ |
| `rpc.listen_address`   | string   | `0.0.0.0`    | RPCサーバーのIPアドレス。IPv4の場合は`0.0.0.0`、IPv6の場合は`::`を使用します。           |
| `rpc.ipv6_only`        | boolean  | `false`      | `listen_address`がIPv6の場合、`true`にするとRPCクライアントはIPv6のみを使用します。       |

## アウトバウンド接続

EMQXはHTTP認証、Webhookアクション、データベース統合などの機能のために外部サービスへアウトバウンド接続を行います。

### 自動IPv6検出

HTTPベースのコネクター（認証バックエンド、Webhookアクションなど）では、EMQXが対象ホストのIPv6対応を自動的に検出し、適切なアドレスファミリーを選択します。ほとんどの場合、手動設定は不要です。

### 手動オーバーライド

一部のコネクタータイプは設定内に`ipv6_probe`トグルを持ちます。有効（HTTPコネクターのデフォルト）にするとIPv6接続を優先します。ネットワークがIPv4のみでDNSがAレコードとAAAAレコードの両方を返す場合、接続遅延を避けるためにこのプローブを無効にできます。

```bash
# 例：HTTP認証バックエンド
authentication {
  backend = "http"
  method = "post"
  url = "http://auth-server.example.com:8080/auth"

  # 不要な場合はIPv6自動検出を無効化
  pool_size = 8
}
```

## IPv6のみの完全な例

以下はIPv6のみのデプロイメント向けの最小限の`emqx.conf`例です。

```bash
# IPv6アドレスを用いたノード名
node.name = "emqx@::1"

# IPv6によるクラスタ分散
cluster.proto_dist = inet6_tcp

# IPv6によるGen RPC
rpc.listen_address = "::"
rpc.ipv6_only = true

# IPv6上のMQTTリスナー
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}

# IPv6上のダッシュボード
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

## トラブルシューティング

### ノードがpingに応答しない

**症状**：IPv6ノード名でクラスタノードを起動すると、「not responding to pings」というエラーが発生し起動に失敗する。

**原因**：Erlang分散プロトコルのデフォルトが`inet_tcp`（IPv4）であり、IPv6ノード名には`inet6_tcp`が必要。

**対処法**：`emqx.conf`で`cluster.proto_dist = inet6_tcp`を設定してください。

### アウトバウンド接続で`enetunreach`エラー

**症状**：認証バックエンドなどへのHTTPリクエストが`enetunreach`（ネットワーク到達不能）で失敗する。

**原因**：IPv6のみのサービスにIPv4で接続しようとしている、またはその逆。

**対処法**：EMQXホストから対象サービスが正しいアドレスファミリーで到達可能か確認してください。HTTPコネクターは自動IPv6プローブで対応します。DNS名の場合はDNSが正しいレコード（IPv4はA、IPv6はAAAA）を返しているか確認してください。

### IPv6環境でダッシュボードに接続できない

**症状**：IPv6のみ環境でEMQXを稼働させるとダッシュボードにアクセスできない。

**原因**：ダッシュボードリスナーのデフォルトがIPv4（`0.0.0.0:18083`）であるため。

**対処法**：ダッシュボードをIPv6アドレスにバインドする（`bind = "[::]:18083"`）か、`inet6 = true`でIPv6を明示的に有効にしてください。
