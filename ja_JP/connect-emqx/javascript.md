# JavaScript SDKによる接続

<<<<<<< HEAD
[MQTT.js](https://www.npmjs.com/package/mqtt) は、JavaScriptで実装されたMQTTプロトコルクライアント機能を持つモジュールであり、ブラウザおよびNode.js環境で使用できます。

JavaScriptのシングルスレッドの特性により、MQTT.jsは完全に非同期のMQTTクライアントです。MQTT.jsはMQTTおよび[MQTT over WebSocket](./mqtt-over-websocket.md)をサポートしています。各種動作環境でのサポート状況は以下の通りです。

- ブラウザ環境：MQTT over WebSocket（WeChatアプレット、Alipayアプレットなどのカスタマイズされたブラウザ環境を含む）
- Node.js環境：MQTT、MQTT over WebSocket

一部の接続パラメーターは環境によって異なりますが、その他のAPIは共通です。
=======
[MQTT.js](https://www.npmjs.com/package/mqtt)は、JavaScriptで実装されたMQTTプロトコルクライアント機能を持つモジュールで、ブラウザおよびNode.js環境で使用できます。

JavaScriptのシングルスレッド特性により、MQTT.jsは完全に非同期のMQTTクライアントです。MQTT.jsはMQTTおよび[MQTT over WebSocket](./mqtt-over-websocket.md)をサポートしています。各動作環境でのサポート状況は以下の通りです。

- ブラウザ環境：MQTT over WebSocket（WeChatミニプログラム、Alipayミニプログラムなどのカスタマイズされたブラウザ環境を含む）
- Node.js環境：MQTT、MQTT over WebSocket

接続パラメータの一部は環境によって異なりますが、それ以外のAPIは共通です。
>>>>>>> origin/release-5.9

npmによるインストール：

```bash
npm i mqtt
```

CDNによるインストール（ブラウザ）：

```html
<script src="https://unpkg.com/mqtt/dist/mqtt.min.js"></script>
<script>
    // グローバルにmqtt変数を初期化
    console.log(mqtt)
</script>
```

<<<<<<< HEAD
Node.jsがインストールされた環境では、`npm i mqtt -g` コマンドでMQTT.jsをグローバルに使用できます。
=======
Node.jsがインストールされている環境では、`npm i mqtt -g`コマンドでMQTT.jsをグローバルに使用できます。
>>>>>>> origin/release-5.9

```bash
npm i mqtt -g

mqtt help

> MQTT.js コマンドラインインターフェース、利用可能なコマンド:

  * publish     ブローカーにメッセージをパブリッシュする
  * subscribe   ブローカーからの更新をサブスクライブする
<<<<<<< HEAD
  * version     現在のMQTT.jsのバージョンを表示
  * help        コマンドのヘルプを表示
=======
  * version     現在のMQTT.jsのバージョン
  * help        コマンドのヘルプ
>>>>>>> origin/release-5.9

> コマンドの詳細は 'mqtt help [command]' を実行してください。
```

## MQTT.js 使用例

<<<<<<< HEAD
以下はJavaScript言語でMQTT.jsを用いてEMQXに接続し、メッセージを送受信する完全なコード例です。
=======
以下は、JavaScriptでMQTT.jsを使ってEMQXに接続し、メッセージの送受信を行う完全なコード例です。
>>>>>>> origin/release-5.9

```javascript
// const mqtt = require('mqtt')
import mqtt from 'mqtt'

// 接続オプション
const options = {
  		clean: true, // セッションを保持しない
<<<<<<< HEAD
      connectTimeout: 4000, // タイムアウト期間
=======
      connectTimeout: 4000, // タイムアウト時間
>>>>>>> origin/release-5.9
      // 認証情報
      clientId: 'emqx_test',
      username: 'emqx_test',
      password: 'emqx_test',
}

<<<<<<< HEAD
// 接続文字列、プロトコルで接続方式を指定
=======
// 接続URL、プロトコルで接続方法を指定
>>>>>>> origin/release-5.9
// ws 暗号化されていないWebSocket接続
// wss 暗号化されたWebSocket接続
// mqtt 暗号化されていないTCP接続
// mqtts 暗号化されたTCP接続
<<<<<<< HEAD
// wxs WeChatアプレット接続
// alis Alipayアプレット接続
=======
// wxs WeChatミニプログラム接続
// alis Alipayミニプログラム接続
>>>>>>> origin/release-5.9
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

<<<<<<< HEAD
## MQTT.jsのMQTT 5.0対応状況
=======
## MQTT.js MQTT 5.0対応状況
>>>>>>> origin/release-5.9

現在、MQTT.jsはMQTT 5.0を完全にサポートしています。
