# IPv6

EMQXは、クライアント接続、ダッシュボード、ノード間クラスタリング、および外部サービスへのアウトバウンド接続に対してIPv6を完全にサポートしています。本ページでは、シングルスタック（IPv6のみ）からデュアルスタックのデプロイメントまで、IPv6環境向けのEMQXの設定方法を説明します。

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

ほとんどのOSでは、`[::]`にバインドするとデフォルトでIPv4とIPv6の両方の接続を受け入れます（デュアルスタック）。両方のプロトコルをサポートする環境で最も簡単な設定方法です。

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

同じ設定はSSL、WebSocket、およびSecure WebSocketリスナーにも適用されます。

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

### IPv6バインドアドレスの使用

`bind`アドレスがIPv6の場合、EMQXはダッシュボードリスナーで自動的にIPv6を有効にします。

```bash
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

### `inet6`フラグの使用

明示的なIPアドレスなしでポートのみをバインドする場合は、IPv6を明示的に有効にできます。

```bash
dashboard.listeners.http {
  bind = 18083
  inet6 = true
}
```

| パラメーター       | 型       | デフォルト | 説明                                                                                  |
| ------------------ | -------- | ---------- | ------------------------------------------------------------------------------------- |
| `inet6`            | boolean  | `false`    | IPv6サポートを有効にします。`false`の場合、リスナーはIPv4トラフィックのみ受け入れます。 |
| `ipv6_v6only`      | boolean  | `false`    | IPv4からIPv6へのマッピングを無効にします。`inet6`が`true`の場合にのみ有効です。          |

## クラスター通信

EMQXクラスターのノード間通信がIPv6ネットワーク上で行われる場合、設定が必要なコンポーネントは2つあります。クラスタ調整に使用されるErlang分散プロトコルと、ノード間のデータ転送に使用されるGen RPCチャネルです。

### Erlang分散プロトコル

ノード間通信にIPv6を使用するには、`cluster.proto_dist`を設定します。

```bash
cluster.proto_dist = inet6_tcp
```

利用可能なオプション：

| 値             | 説明                                             |
| -------------- | ------------------------------------------------ |
| `inet_tcp`     | IPv4上のTCP（デフォルト）                        |
| `inet6_tcp`    | IPv6上のTCP                                      |
| `inet_tls`     | IPv4上のTLS、`etc/ssl_dist.conf`で設定          |
| `inet6_tls`    | IPv6上のTLS、`etc/ssl_dist.conf`で設定          |

::: warning 重要なお知らせ

IPv6ノード名（例：`emqx@::1`）を使用する場合は、必ず`cluster.proto_dist`を`inet6_tcp`または`inet6_tls`に設定してください。設定しないと、「pingに応答しない」などのエラーでノードが起動に失敗します。

:::

### Gen RPC

Gen RPCチャネルをIPv6用に設定します。

```bash
rpc.listen_address = "::"
rpc.ipv6_only = true
```

| パラメーター           | 型       | デフォルト     | 説明                                                                                     |
| ---------------------- | -------- | -------------- | ---------------------------------------------------------------------------------------- |
| `rpc.listen_address`   | string   | `0.0.0.0`      | RPCサーバーのIPアドレス。IPv4の場合は`0.0.0.0`、IPv6の場合は`::`を使用します。         |
| `rpc.ipv6_only`        | boolean  | `false`        | `listen_address`がIPv6の場合、これを`true`にするとRPCクライアントはIPv6のみを使用します。 |

## アウトバウンド接続

EMQXはHTTP認証、Webhookアクション、データベース統合などの機能のために外部サービスへのアウトバウンド接続を行います。

### 自動IPv6検出

HTTPベースのコネクター（認証バックエンド、Webhookアクションなど）では、EMQXがターゲットホストのIPv6対応を自動的に検出し、適切なアドレスファミリーを選択します。ほとんどの場合、手動設定は不要です。

### 手動オーバーライド

一部のコネクタータイプでは、設定に`ipv6_probe`トグルがあり、これを有効にすると（HTTPコネクターのデフォルト）、EMQXはまずIPv6接続を試みます。ネットワークがIPv4のみでDNSがAレコードとAAAAレコードの両方を返す場合、接続遅延を避けるためにプローブを無効にできます。

```bash
# 例：HTTP認証バックエンド
authentication {
  backend = "http"
  method = "post"
  url = "http://auth-server.example.com:8080/auth"

  # IPv6自動検出が不要な場合は無効化
  pool_size = 8
}
```

## IPv6のみの完全な例

以下はIPv6のみのデプロイメント向けの最小限の`emqx.conf`例です。

```bash
# IPv6アドレスを使用したノード名
node.name = "emqx@::1"

# IPv6によるクラスター分散
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

**症状**：IPv6ノード名でクラスターノードを起動すると、「pingに応答しない」と表示され起動に失敗する。

**原因**：Erlang分散プロトコルがデフォルトで`inet_tcp`（IPv4）になっている。IPv6ノード名には`inet6_tcp`が必要。

**対処法**：`emqx.conf`で`cluster.proto_dist = inet6_tcp`を設定してください。

### アウトバウンド接続で`enetunreach`エラー

**症状**：認証バックエンドなどへのHTTPリクエストが`enetunreach`（ネットワーク到達不能）で失敗する。

**原因**：IPv6のみのサービスにIPv4で接続しようとしている、またはその逆。

**対処法**：EMQXホストからターゲットサービスが正しいアドレスファミリーで到達可能か確認してください。HTTPコネクターは自動IPv6プローブで対応します。DNS名の場合は、DNSが正しいレコードタイプ（IPv4はA、IPv6はAAAA）を返しているか確認してください。

### IPv6環境でダッシュボードにアクセスできない

**症状**：EMQXがIPv6のみ環境で動作しているときにダッシュボードにアクセスできない。

**原因**：ダッシュボードリスナーがデフォルトでIPv4（`0.0.0.0:18083`）にバインドされている。

**対処法**：ダッシュボードをIPv6アドレス（`bind = "[::]:18083"`）にバインドするか、`inet6 = true`でIPv6を明示的に有効にしてください。
