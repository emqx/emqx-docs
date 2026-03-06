# JavaScript SDK を使った接続

[MQTT.js](https://github.com/mqttjs/MQTT.js) は、JavaScript で実装された MQTT プロトコルクライアント機能のモジュールであり、ブラウザおよび Node.js 環境で利用できます。

JavaScript のシングルスレッド特性により、MQTT.js は完全に非同期の MQTT クライアントです。MQTT.js は MQTT と MQTT over WebSocket をサポートしており、各環境での対応状況は以下の通りです。

- ブラウザ環境：MQTT over WebSocket（WeChat アプレット、Alipay アプレットなどのカスタマイズされたブラウザ環境を含む）
- Node.js 環境：MQTT、MQTT over WebSocket

接続パラメータの一部は環境によって異なりますが、その他の API は共通です。

npm を使ったインストール方法：

```bash
npm i mqtt
```

CDN を使ったブラウザでのインストール方法：

```html
<script src="https://unpkg.com/mqtt/dist/mqtt.min.js"></script>
<script>
    // グローバルに mqtt 変数を初期化
    console.log(mqtt)
</script>
```

Node.js がインストールされている環境では、`npm i mqtt -g` コマンドで MQTT.js をグローバルに利用できます。

```bash
npm i mqtt -g

mqtt help

> MQTT.js コマンドラインインターフェース、利用可能なコマンドは以下の通りです：

  * publish     ブローカーへメッセージをパブリッシュする
  * subscribe   ブローカーからの更新をサブスクライブする
  * version     現在の MQTT.js バージョンを表示
  * help        コマンドのヘルプを表示

> 詳細は 'mqtt help [command]' を実行してください。
```

## MQTT.js 使用例

以下は、JavaScript で MQTT.js を使って EMQX に接続し、メッセージの送受信を行う完全なコード例です。

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
// ws 暗号化されていない WebSocket 接続
// wss 暗号化された WebSocket 接続
// mqtt 暗号化されていない TCP 接続
// mqtts 暗号化された TCP 接続
// wxs WeChat アプレット接続
// alis Alipay アプレット接続
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

## MQTT.js の MQTT 5.0 サポート

現在、MQTT.js は MQTT 5.0 を完全にサポートしています。
