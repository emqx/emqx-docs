# IPv6

<<<<<<< HEAD
EMQXは、クライアント接続、ダッシュボード、ノード間クラスタリング、および外部サービスへのアウトバウンド接続に対してIPv6を完全にサポートしています。本ページでは、シングルスタック（IPv6のみ）からデュアルスタック展開まで、IPv6環境でのEMQXの設定方法を説明します。

## MQTTリスナー

IPv6経由でMQTTクライアント接続を受け入れるには、リスナーをIPv6アドレスにバインドします。EMQXはIPv6のバインドアドレスを検出すると、自動的に`inet6`ソケットオプションを有効にします。
=======
EMQXは、クライアント接続、ダッシュボード、ノード間クラスタリング、および外部サービスへのアウトバウンド接続に対してIPv6を完全にサポートしています。本ページでは、シングルスタック（IPv6のみ）からデュアルスタック展開まで、IPv6環境におけるEMQXの設定方法を説明します。

## MQTTリスナー

IPv6経由でMQTTクライアント接続を受け入れるには、リスナーをIPv6アドレスにバインドします。EMQXはIPv6バインドアドレスを検出すると、自動的に`inet6`ソケットオプションを有効にします。
>>>>>>> origin/release-6.1

### デュアルスタック（IPv4およびIPv6）

同じポートでIPv4とIPv6の両方の接続を受け入れるには、`[::]`にバインドします。

```bash
listeners.tcp.default {
  bind = "[::]:1883"
}
```

::: tip

<<<<<<< HEAD
ほとんどのOSでは、`[::]`にバインドするとデフォルトでIPv4とIPv6の両方の接続を受け入れます（デュアルスタック）。両方のプロトコルをサポートする環境で最も簡単な設定方法です。
=======
ほとんどのOSでは、`[::]`へのバインドはデフォルトでIPv4とIPv6の両方の接続を受け入れます（デュアルスタック）。両方のプロトコルをサポートする環境において最も簡単な設定です。
>>>>>>> origin/release-6.1

:::

### IPv6のみ

リスナーをIPv6接続のみに制限するには、`ipv6_v6only = true`を設定します。

```bash
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}
```

<<<<<<< HEAD
これは`IPV6_V6ONLY`ソケットオプションを設定し、IPv4マップされたIPv6アドレスの受け入れを防ぎます。
=======
これは`IPV6_V6ONLY`ソケットオプションを設定し、IPv4マップドIPv6アドレスの受け入れを防ぎます。
>>>>>>> origin/release-6.1

### 特定のIPv6アドレスにバインド

特定のIPv6アドレスにバインドすることも可能です。

```bash
listeners.tcp.default {
  bind = "[::1]:1883"
}
```

<<<<<<< HEAD
同じ設定はSSL、WebSocket、およびSecure WebSocketリスナーにも適用されます。
=======
同様の設定はSSL、WebSocket、Secure WebSocketリスナーにも適用されます。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
## ダッシュボード HTTP/HTTPSリスナー
=======
## ダッシュボードHTTP/HTTPSリスナー
>>>>>>> origin/release-6.1

EMQXダッシュボードのHTTP/HTTPSリスナーもIPv6をサポートしています。

### IPv6バインドアドレスの使用

<<<<<<< HEAD
`bind`アドレスがIPv6の場合、EMQXはダッシュボードリスナーで自動的にIPv6を有効にします。
=======
`bind`アドレスがIPv6の場合、EMQXは自動的にダッシュボードリスナーでIPv6を有効にします。
>>>>>>> origin/release-6.1

```bash
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

### `inet6`フラグの使用

<<<<<<< HEAD
明示的なIPアドレスなしでポートのみをバインドする場合は、IPv6を明示的に有効にできます。
=======
明示的なIPアドレスなしでポートのみを指定する場合は、IPv6を明示的に有効にできます。
>>>>>>> origin/release-6.1

```bash
dashboard.listeners.http {
  bind = 18083
  inet6 = true
}
```

<<<<<<< HEAD
| パラメーター       | 型       | デフォルト | 説明                                                                                 |
| ------------------ | -------- | ---------- | ------------------------------------------------------------------------------------ |
| `inet6`            | boolean  | `false`    | IPv6サポートを有効にします。`false`の場合、リスナーはIPv4トラフィックのみを受け入れます。 |
| `ipv6_v6only`      | boolean  | `false`    | IPv4からIPv6へのマッピングを無効にします。`inet6`が`true`の場合にのみ有効です。       |

## クラスター通信

EMQXクラスターのノードがIPv6ネットワーク上で通信する場合、設定が必要なコンポーネントは2つあります。クラスタ調整に使われるErlang分散プロトコルと、ノード間のデータ転送に使われるGen RPCチャネルです。

### Erlang分散プロトコル

ノード間通信にIPv6を使用するには、`cluster.proto_dist`を設定します。
=======
| パラメーター       | 型       | デフォルト | 説明                                                                                   |
| ------------------ | -------- | ---------- | -------------------------------------------------------------------------------------- |
| `inet6`            | boolean  | `false`    | IPv6サポートを有効にします。`false`の場合、リスナーはIPv4トラフィックのみを受け入れます。  |
| `ipv6_v6only`      | boolean  | `false`    | IPv4からIPv6へのマッピングを無効にします。`inet6`が`true`の場合のみ有効です。           |

## クラスター通信

EMQXクラスターのノード間通信がIPv6ネットワーク上で行われる場合、2つのコンポーネントの設定が必要です。1つはクラスター調整に使用されるErlang分散プロトコル、もう1つはノード間のデータ転送に使用されるGen RPCチャネルです。

### Erlang分散プロトコル

ノード間通信でIPv6を使用するには、`cluster.proto_dist`を設定します。
>>>>>>> origin/release-6.1

```bash
cluster.proto_dist = inet6_tcp
```

利用可能なオプション：

<<<<<<< HEAD
| 値             | 説明                                               |
| -------------- | -------------------------------------------------- |
| `inet_tcp`     | IPv4上のTCP（デフォルト）                          |
| `inet6_tcp`    | IPv6上のTCP                                        |
| `inet_tls`     | IPv4上のTLS、`etc/ssl_dist.conf`で設定             |
| `inet6_tls`    | IPv6上のTLS、`etc/ssl_dist.conf`で設定             |
=======
| 値             | 説明                                         |
| -------------- | -------------------------------------------- |
| `inet_tcp`     | IPv4上のTCP（デフォルト）                     |
| `inet6_tcp`    | IPv6上のTCP                                   |
| `inet_tls`     | IPv4上のTLS、`etc/ssl_dist.conf`で設定       |
| `inet6_tls`    | IPv6上のTLS、`etc/ssl_dist.conf`で設定       |
>>>>>>> origin/release-6.1

::: warning 重要なお知らせ

IPv6ノード名（例：`emqx@::1`）を使用する場合、`cluster.proto_dist`を必ず`inet6_tcp`または`inet6_tls`に設定してください。そうしないと、「not responding to pings」などのエラーでノードが起動に失敗します。

:::

### Gen RPC

Gen RPCチャネルをIPv6用に設定します。

```bash
rpc.listen_address = "::"
rpc.ipv6_only = true
```

<<<<<<< HEAD
| パラメーター           | 型       | デフォルト   | 説明                                                                                      |
| ---------------------- | -------- | ------------ | ----------------------------------------------------------------------------------------- |
| `rpc.listen_address`   | string   | `0.0.0.0`    | RPCサーバーのIPアドレス。IPv4の場合は`0.0.0.0`、IPv6の場合は`::`を使用します。          |
| `rpc.ipv6_only`        | boolean  | `false`      | `listen_address`がIPv6の場合、これを`true`にするとRPCクライアントがIPv6のみを使用します。 |

## アウトバウンド接続

EMQXはHTTP認証、Webhookアクション、データベース連携などの機能のために外部サービスへのアウトバウンド接続を行います。

### 自動IPv6検出

HTTPベースのコネクター（認証バックエンド、Webhookアクションなど）では、EMQXがターゲットホストがIPv6をサポートしているか自動的に検出し、適切なアドレスファミリーを選択します。ほとんどの場合、手動設定は不要です。

### 手動オーバーライド

一部のコネクタータイプは設定で`ipv6_probe`トグルを提供しています。有効（HTTPコネクターのデフォルト）にすると、EMQXはまずIPv6接続を試みます。ネットワークがIPv4のみでDNSがAレコードとAAAAレコードの両方を返す場合、接続遅延を避けるためにプローブを無効にできます。
=======
| パラメーター          | 型       | デフォルト    | 説明                                                                                   |
| --------------------- | -------- | ------------ | -------------------------------------------------------------------------------------- |
| `rpc.listen_address`  | string   | `0.0.0.0`    | RPCサーバーのIPアドレス。IPv4の場合は`0.0.0.0`、IPv6の場合は`::`を使用します。        |
| `rpc.ipv6_only`       | boolean  | `false`      | `listen_address`がIPv6の場合、これを`true`に設定するとRPCクライアントはIPv6のみを使用します。 |

## アウトバウンド接続

EMQXはHTTP認証、Webhookアクション、データベース統合などの機能のために外部サービスへのアウトバウンド接続を行います。

### 自動IPv6検出

HTTPベースのコネクター（認証バックエンド、Webhookアクションなど）では、EMQXが対象ホストのIPv6対応を自動的に検出し、適切なアドレスファミリーを選択します。ほとんどの場合、手動設定は不要です。

### 手動オーバーライド

一部のコネクタータイプは設定に`ipv6_probe`トグルを提供しています。有効（HTTPコネクターのデフォルト）にすると、EMQXはまずIPv6接続を試みます。ネットワークがIPv4のみでDNSがAレコードとAAAAレコードの両方を返す場合、接続遅延を避けるためにプローブを無効にできます。
>>>>>>> origin/release-6.1

```bash
# 例：HTTP認証バックエンド
authentication {
  backend = "http"
  method = "post"
  url = "http://auth-server.example.com:8080/auth"

<<<<<<< HEAD
  # IPv6自動検出を不要な場合は無効化
=======
  # 不要な場合はIPv6自動検出を無効化
>>>>>>> origin/release-6.1
  pool_size = 8
}
```

## IPv6のみの完全な例

<<<<<<< HEAD
以下はIPv6のみの展開における最小限の`emqx.conf`例です。
=======
以下はIPv6のみ展開の最小限の`emqx.conf`例です。
>>>>>>> origin/release-6.1

```bash
# IPv6アドレスを使用したノード名
node.name = "emqx@::1"

# IPv6によるクラスター分散
cluster.proto_dist = inet6_tcp

# IPv6によるGen RPC
rpc.listen_address = "::"
rpc.ipv6_only = true

<<<<<<< HEAD
# IPv6上のMQTTリスナー
=======
# IPv6のMQTTリスナー
>>>>>>> origin/release-6.1
listeners.tcp.default {
  bind = "[::]:1883"
  ipv6_v6only = true
}

<<<<<<< HEAD
# IPv6上のダッシュボード
=======
# IPv6のダッシュボード
>>>>>>> origin/release-6.1
dashboard.listeners.http {
  bind = "[::]:18083"
}
```

## トラブルシューティング

### ノードがpingに応答しない

<<<<<<< HEAD
**症状**：IPv6ノード名でクラスターを起動すると、「not responding to pings」と表示されて起動に失敗する。

**原因**：Erlang分散プロトコルがデフォルトの`inet_tcp`（IPv4）になっている。IPv6ノード名には`inet6_tcp`が必要。
=======
**症状**：IPv6ノード名でクラスターを起動すると、「not responding to pings」というエラーが表示され起動に失敗する。

**原因**：Erlang分散プロトコルがデフォルトで`inet_tcp`（IPv4）になっている。IPv6ノード名には`inet6_tcp`が必要。
>>>>>>> origin/release-6.1

**対処法**：`emqx.conf`で`cluster.proto_dist = inet6_tcp`を設定してください。

### アウトバウンド接続での`enetunreach`エラー

<<<<<<< HEAD
**症状**：認証バックエンドなどへのHTTPリクエストが`enetunreach`（ネットワーク到達不能）で失敗する。

**原因**：IPv6のみのサービスにIPv4で接続しようとしている、またはその逆。

**対処法**：EMQXホストからターゲットサービスが正しいアドレスファミリーで到達可能か確認してください。HTTPコネクターは自動IPv6プローブで対応します。DNS名の場合は、DNSが正しいレコードタイプ（IPv4ならA、IPv6ならAAAA）を返しているか確認してください。

### IPv6環境でダッシュボードにアクセスできない

**症状**：EMQXがIPv6のみ環境で稼働しているとき、ダッシュボードにアクセスできない。
=======
**症状**：認証バックエンドなどへのアウトバウンドHTTPリクエストが`enetunreach`（ネットワーク到達不能）で失敗する。

**原因**：IPv6のみのサービスにIPv4で接続しようとしている、またはその逆。

**対処法**：EMQXホストから対象サービスが正しいアドレスファミリーで到達可能か確認してください。HTTPコネクターは自動IPv6プローブで対応します。DNS名の場合は、DNSが正しいレコードタイプ（IPv4はA、IPv6はAAAA）を返しているか確認してください。

### IPv6環境でダッシュボードにアクセスできない

**症状**：IPv6のみ環境でEMQXを実行しているときにダッシュボードにアクセスできない。
>>>>>>> origin/release-6.1

**原因**：ダッシュボードリスナーがデフォルトでIPv4（`0.0.0.0:18083`）にバインドされている。

**対処法**：ダッシュボードをIPv6アドレスにバインドする（`bind = "[::]:18083"`）か、`inet6 = true`でIPv6を明示的に有効にしてください。
