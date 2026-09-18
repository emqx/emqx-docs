# IPv6

EMQXはクライアント接続、ダッシュボード、ノード間クラスタリング、および外部サービスへのアウトバウンド接続に対してIPv6を完全にサポートしています。本ページでは、シングルスタック（IPv6のみ）からデュアルスタックのデプロイメントまで、IPv6環境でのEMQXの設定方法を説明します。

## MQTTリスナー

IPv6経由でMQTTクライアント接続を受け入れるには、リスナーをIPv6アドレスにバインドします。EMQXはIPv6バインドアドレスを検出すると、自動的に`inet6`ソケットオプションを有効にします。

### デュアルスタック（IPv4およびIPv6）

同じポートでIPv4とIPv6の両方の接続を受け入れるには、`[::]`にバインドします。

```bash
listeners.tcp.default {
  bind = "[::]:1883"
}
```

::: tip

ほとんどのオペレーティングシステムでは、`[::]`にバインドするとデフォルトでIPv4およびIPv6の両方の接続を受け入れます（デュアルスタック）。両方のプロトコルをサポートする環境において最も簡単な設定方法です。

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

### IPv6バインドアドレスを使用する

`bind`アドレスがIPv6の場合、EMQXはダッシュボードリスナーで自動的にIPv6を有効にします。

```bash
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

### `inet6`フラグを使用する

明示的なIPアドレスなしでポートのみをバインドする場合は、IPv6を明示的に有効にできます。

```bash
dashboard.listeners.http {
  bind = 18083
  inet6 = true
}
```

| パラメーター      | 型       | デフォルト | 説明                                                                                   |
| ----------------- | -------- | ---------- | -------------------------------------------------------------------------------------- |
| `inet6`           | boolean  | `false`    | IPv6サポートを有効にします。`false`の場合、リスナーはIPv4トラフィックのみ受け入れます。 |
| `ipv6_v6only`     | boolean  | `false`    | IPv4からIPv6へのマッピングを無効にします。`inet6`が`true`の場合にのみ有効です。        |

## クラスター通信

EMQXクラスターのノードがIPv6ネットワーク上で通信する場合、2つのコンポーネントの設定が必要です。クラスタ調整に使用されるErlang分散プロトコルと、ノード間のデータ転送に使用されるGen RPCチャネルです。

### Erlang分散プロトコル

ノード間通信でIPv6を使用するには、`cluster.proto_dist`を設定します。

```bash
cluster.proto_dist = inet6_tcp
```

利用可能なオプション：

| 値             | 説明                                                    |
| -------------- | ------------------------------------------------------- |
| `inet_tcp`     | IPv4上のTCP（デフォルト）                               |
| `inet6_tcp`    | IPv6上のTCP                                             |
| `inet_tls`     | IPv4上のTLS、`etc/ssl_dist.conf`で設定                 |
| `inet6_tls`    | IPv6上のTLS、`etc/ssl_dist.conf`で設定                 |

::: warning 重要なお知らせ

IPv6ノード名（例：`emqx@::1`）を使用する場合は、必ず`cluster.proto_dist`を`inet6_tcp`または`inet6_tls`に設定してください。そうしないと、「not responding to pings」などのエラーでノードが起動に失敗します。

:::

### Gen RPC

Gen RPCチャネルをIPv6用に設定します。

```bash
rpc.listen_address = "::"
rpc.ipv6_only = true
```

| パラメーター           | 型       | デフォルト   | 説明                                                                                  |
| ---------------------- | -------- | ------------ | ------------------------------------------------------------------------------------- |
| `rpc.listen_address`   | string   | `0.0.0.0`    | RPCサーバーのIPアドレス。IPv4の場合は`0.0.0.0`、IPv6の場合は`::`を使用します。      |
| `rpc.ipv6_only`        | boolean  | `false`     | `listen_address`がIPv6の場合、`true`に設定するとRPCクライアントがIPv6のみを使用します。 |

## アウトバウンド接続

EMQXはHTTP認証、Webhookアクション、データベース統合などの機能のために外部サービスへのアウトバウンド接続を行います。

### 自動IPv6検出

EMQXはさまざまなアウトバウンド接続タイプでIPv6をサポートしています。

- HTTPベースのコネクター（認証バックエンド、Webhookアクションなど）では、EMQXがターゲットホストのIPv6対応を自動的に検出し、適切なアドレスファミリーを選択します。ほとんどの場合、手動設定は不要です。
- MQTTコネクターでは、IPv6のみのブローカーやホスト名がIPv6の`AAAA`レコードのみを返すブローカーにも接続可能です。MQTTブローカーのアドレスは、`[::1]:1883`や`mqtt://[::1]:1883`のような角括弧付きIPv6リテラルも使用できます。

### 手動オーバーライド

一部のコネクタータイプは設定で`ipv6_probe`トグルを提供しています。有効（HTTPコネクターのデフォルト）にすると、EMQXはまずIPv6接続を試みます。ネットワークがIPv4のみでDNSがAレコードとAAAAレコードの両方を返す場合、接続遅延を避けるためにこの検出を無効にできます。

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

## 完全なIPv6のみの例

以下はIPv6のみのデプロイメント向けの最小限の`emqx.conf`例です。

```bash
# IPv6アドレスを使用したノード名
node.name = "emqx@::1"

# IPv6によるクラスター分散
cluster.proto_dist = inet6_tcp

# IPv6によるGen RPC
rpc.listen_address = "::"
rpc.ipv6_only = true

# IPv6でのMQTTリスナー
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}

# IPv6でのダッシュボード
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

## トラブルシューティング

### ノードがpingに応答しない

**症状**：IPv6ノード名でクラスターのノードを起動すると、「not responding to pings」というエラーが発生し、起動に失敗する。

**原因**：Erlang分散プロトコルのデフォルトが`inet_tcp`（IPv4）であるため。IPv6ノード名には`inet6_tcp`が必要。

**対処法**：`emqx.conf`で`cluster.proto_dist = inet6_tcp`を設定してください。

### アウトバウンド接続での`enetunreach`エラー

**症状**：HTTPリクエスト（認証バックエンドなど）が`enetunreach`（ネットワーク到達不能）で失敗する。

**原因**：IPv6のみのサービスにIPv4で接続しようとしている、またはその逆。

**対処法**：EMQXホストからターゲットサービスが正しいアドレスファミリーで到達可能か確認してください。HTTPコネクターは自動IPv6検出を行います。DNS名を使用している場合は、DNSが正しいレコードタイプ（IPv4はA、IPv6はAAAA）を返しているか確認してください。

### IPv6環境でダッシュボードにアクセスできない

**症状**：IPv6のみ環境でEMQXを稼働させているときにダッシュボードにアクセスできない。

**原因**：ダッシュボードリスナーのデフォルトがIPv4（`0.0.0.0:18083`）になっているため。

**対処法**：ダッシュボードをIPv6アドレスにバインドする（`bind = "[::]:18083"`）か、`inet6 = true`でIPv6を明示的に有効にしてください。
