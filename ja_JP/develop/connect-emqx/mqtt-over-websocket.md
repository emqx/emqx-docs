# MQTT over WebSocket

EMQX Enterprise は、組み込みの WebSocket リスナーを通じてネイティブに MQTT over WebSocket をサポートしています。これにより、MQTT クライアントは生の TCP または TLS 接続の代わりに WebSocket を介してブローカーと通信できます。

MQTT over WebSocket は機能的に MQTT over TCP/TLS と同一です。唯一の違いはトランスポート層であり、MQTT パケットが WebSocket フレーム内にカプセル化され、HTTP または HTTPS 上で送信される点です。

MQTT over WebSocket を使用するクライアントは、TCP、TLS、または QUIC を介して同じブローカーやクラスターに接続している他の MQTT クライアントとシームレスに相互運用できます。MQTT プロトコル自体は変更されていないため、異なるトランスポートで接続されたクライアントは同じトピックネームスペースとルーティング動作を共有します。

この機能は、直接の TCP 接続が利用できない、または制限されている環境（例えば、ウェブブラウザや特定の企業ネットワーク）で特に有用です。

## Protocol Stack

MQTT over WebSocket を使用する場合のプロトコルスタックは以下の通りです：

```
MQTT
WebSocket
HTTP / HTTPS
TCP
IP
```

MQTT プロトコル自体は変更されません。すべての MQTT 制御パケット（CONNECT、PUBLISH、SUBSCRIBE など）は WebSocket フレーム内でそのまま送信されます。

## Features

EMQX Enterprise の MQTT over WebSocket は以下を提供します：

- MQTT v3.1、v3.1.1、v5.0 との完全な互換性
- QoS 0、1、2 のサポート
- 保持メッセージおよび遺言メッセージ
- 永続セッションおよびオフラインメッセージ
- 共有サブスクリプション
- 認証および認可機構（ユーザー名/パスワード、JWT、OAuth など）
- WSS による TLS 暗号化

すべての MQTT セマンティクスおよびブローカー側の処理は、標準の TCP または TLS 接続と同一です。

## Configure WebSocket Listeners

MQTT over WebSocket を利用するには、WebSocket リスナーを有効にする必要があります。

EMQX Enterprise では以下のリスナーを提供しています：

- WebSocket リスナー（WS）
- セキュア WebSocket リスナー（WSS）

これらのリスナーは以下の方法で設定可能です：

- EMQX ダッシュボード：**Management** -> **Listeners**
- 設定ファイル
- REST API

詳細な設定手順は以下を参照してください：

- [Configure WebSocket Listener](../../guides/configuration/listener.md#configure-websocket-listener)
- [Configure Secure WebSocket Listener](../../guides/configuration/listener.md#configure-secure-websocket-listener)

## Get Started

MQTT over WebSocket を使用するには：

1. WebSocket（WS）またはセキュア WebSocket（WSS）リスナーを有効にします。
2. クライアントを対応するエンドポイントに `ws://` または `wss://` で接続します。
3. MQTT over WebSocket をサポートする MQTT クライアントライブラリを使用します。
4. クライアントを WebSocket トランスポートで動作するよう設定します。

クライアントの視点では、MQTT の動作は変わりません。クライアントは標準の MQTT 制御パケットを送信し、それが WebSocket フレーム内で透過的に運ばれます。

ブラウザベースのアプリケーションでは、MQTT over WebSocket は JavaScript MQTT クライアントライブラリと組み合わせてよく使用されます。

ステップバイステップの例については、[Connect via JavaScript SDK](./javascript.md) をご覧ください。

### Example: Browser Client

以下の例は、ブラウザクライアントがセキュア WebSocket 接続を介して MQTT トピックにパブリッシュおよびサブスクライブする方法を示しています：

```javascript
import mqtt from "mqtt";

const client = mqtt.connect("wss://broker.example.com:8084/mqtt", {
  clientId: "web-client-1",
  username: "username",
  password: "password"
});

client.on("connect", () => {
  console.log("Connected over WebSockets");

  client.subscribe("test/topic", () => {
    client.publish("test/topic", "Hello from browser");
  });
});

client.on("message", (topic, message) => {
  console.log(topic, message.toString());
});
```

WebSocket を介したパブリッシュおよびサブスクライブは、MQTT/TCP または MQTT/TLS とまったく同じ動作をします。

## Typical Use Cases

MQTT over WebSocket は以下の用途に推奨されます：

- ブラウザベースのアプリケーション
- ウェブダッシュボードやフロントエンドシステム
- ポート 80 または 443 のみが許可されている環境
- 厳格なファイアウォールやプロキシポリシーがある企業ネットワーク

TCP が利用可能なバックエンドサービスやデバイス接続では、最適なパフォーマンスのために通常は MQTT over TCP/TLS が推奨されます。

## Performance Considerations

MQTT over TCP/TLS と比較して、MQTT over WebSocket は以下の影響があります：

- 追加の HTTP および WebSocket フレーミングによるオーバーヘッド
- わずかに増加したレイテンシ
- 若干低下するスループット

これらの差異は通常、ブラウザやウェブアプリケーションでは無視できる程度ですが、高スループットやレイテンシに敏感なシナリオでは考慮が必要です。
