# JavaScript SDKによる接続

[MQTT.js](https://github.com/mqttjs/MQTT.js) は、JavaScriptで実装されたMQTTプロトコルのクライアント機能を持つモジュールで、ブラウザおよびNode.js環境で使用できます。

JavaScriptのシングルスレッドの特性により、MQTT.jsは完全非同期のMQTTクライアントです。MQTT.jsはMQTTおよび[MQTT over WebSocket](./mqtt-over-websocket.md)をサポートしています。各動作環境での対応は以下の通りです：

- ブラウザ環境：MQTT over WebSocket（WeChatアプレット、Alipayアプレットなどのカスタマイズされたブラウザ環境を含む）
- Node.js環境：MQTT、MQTT over WebSocket

一部の接続パラメータは環境によって異なりますが、それ以外のAPIは共通です。

npmを使ったインストール：

```bash
npm i mqtt
```

CDNを使ったインストール（ブラウザ）：

```html
<script src="https://unpkg.com/mqtt/dist/mqtt.min.js"></script>
<script>
    // グローバルにmqtt変数を初期化
    console.log(mqtt)
</script>
```

Node.jsがインストールされた環境では、`npm i mqtt -g`コマンドでMQTT.jsをグローバルに利用できます。

```bash
npm i mqtt -g

mqtt help

> MQTT.jsコマンドラインインターフェース、利用可能なコマンド：

  * publish     ブローカーにメッセージをパブリッシュする
  * subscribe   ブローカーからの更新をサブスクライブする
  * version     現在のMQTT.jsのバージョン
  * help        コマンドのヘルプ

> 詳細は 'mqtt help [command]' を実行してください。
```

## MQTT.js 使用例

以下は、JavaScriptでMQTT.jsを使ってEMQXに接続し、メッセージの送受信を行う完全なコード例です：

```javascript
// const mqtt = require('mqtt')
import mqtt from 'mqtt'

// 接続オプション
const options = {
  		clean: true, // セッションを保持しない
      connectTimeout: 4000, // タイムアウト時間（ミリ秒）
      // 認証情報
      clientId: 'emqx_test',
      username: 'emqx_test',
      password: 'emqx_test',
}

// 接続文字列、プロトコルで接続方法を指定
// ws 暗号化されていないWebSocket接続
// wss 暗号化されたWebSocket接続
// mqtt 暗号化されていないTCP接続
// mqtts 暗号化されたTCP接続
// wxs WeChatアプレット接続
// alis Alipayアプレット接続
const connectUrl = 'wss://broker.emqx.io:8084/mqtt'
const client = mqtt.connect(connectUrl, options)

client.on('reconnect', (error) => {
    console.log('再接続中:', error)
})

client.on('error', (error) => {
    console.log('接続失敗:', error)
})

client.on('message', (topic, message) => {
  console.log('メッセージ受信：', topic, message.toString())
})
```

## MQTT.jsのMQTT 5.0サポート

現在、MQTT.jsはMQTT 5.0を完全にサポートしています。
