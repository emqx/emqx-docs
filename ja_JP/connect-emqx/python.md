# Python SDKでの接続

[Eclipse Paho Python](https://github.com/eclipse/paho.mqtt.python) は、Eclipse PahoプロジェクトのPython言語クライアントライブラリであり、MQTTブローカーに接続してメッセージをパブリッシュし、トピックをサブスクライブしてパブリッシュされたメッセージを受信できます。

PyPiパッケージ管理ツールを使ってインストールします：

```bash
pip install paho-mqtt
```

## Paho Python 使用例

以下の例は、EMQXに接続してメッセージを送受信するPaho Pythonの完全なコードです：

```python
import paho.mqtt.client as mqtt

# 接続成功時のコールバック
def on_connect(client, userdata, flags, rc):
    print('Connected with result code '+str(rc))
    client.subscribe('testtopic/#')

# メッセージ受信時のコールバック
def on_message(client, userdata, msg):
    print(msg.topic+" "+str(msg.payload))

client = mqtt.Client()

# コールバック関数を指定
client.on_connect = on_connect
client.on_message = on_message

# 接続を確立
client.connect('broker.emqx.io', 1883, 60)
# メッセージをパブリッシュ
client.publish('emqtt',payload='Hello World',qos=0)

client.loop_forever()
```

## Paho Python MQTT 5.0対応状況

現時点で、Paho PythonはまだMQTT 5.0に対応中であり、完全なサポートはされていません。
