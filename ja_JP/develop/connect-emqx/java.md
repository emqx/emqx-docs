# Java SDKで接続する

[Eclipse Paho Java Client](https://www.eclipse.org/paho/clients/java/) は、Javaで書かれたMQTTクライアントライブラリ（MQTT Java Client）で、JVMやAndroidなどのJava互換プラットフォームで利用できます。

Eclipse Paho Java Clientは、非同期および同期のAPIとしてMqttAsyncClientとMqttClientを提供しています。

## MavenでPaho Javaをインストールする

Paho Javaクライアントライブラリは、パッケージ管理ツールのMavenを使って簡単にインストールできます。最新バージョンは以下のように指定します。

```xml
<dependency>
  <groupId>org.eclipse.paho</groupId>
	<artifactId>org.eclipse.paho.client.mqttv3</artifactId>
	<version>1.2.2</version>
</dependency>
```

## Paho Java使用例

Java環境では、Paho Javaは比較的安定して広く使われているMQTTクライアントライブラリです。以下の例は、Java言語でEMQXに接続し、メッセージの送受信を行うPaho Javaの完全なコードです。

**App.java**

```java
package io.emqx;

import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;


public class App {
    public static void main(String[] args) {
        String subTopic = "testtopic/#";
        String pubTopic = "testtopic/1";
        String content = "Hello World";
        int qos = 2;
        String broker = "tcp://broker.emqx.io:1883";
        String clientId = "emqx_test";
        MemoryPersistence persistence = new MemoryPersistence();

        try {
            MqttClient client = new MqttClient(broker, clientId, persistence);

            // MQTT接続オプション
            MqttConnectOptions connOpts = new MqttConnectOptions();
            connOpts.setUserName("emqx_test");
            connOpts.setPassword("emqx_test_password".toCharArray());
            // セッションを保持しない
            connOpts.setCleanSession(true);

            // コールバックを設定
            client.setCallback(new PushCallback());

            // 接続を確立
            System.out.println("ブローカーに接続中: " + broker);
            client.connect(connOpts);

            System.out.println("接続完了");
            System.out.println("メッセージをパブリッシュ: " + content);

            // サブスクライブ
            client.subscribe(subTopic);

            // メッセージパブリッシュに必要なパラメータ
            MqttMessage message = new MqttMessage(content.getBytes());
            message.setQos(qos);
            client.publish(pubTopic, message);
            System.out.println("メッセージをパブリッシュしました");

            client.disconnect();
            System.out.println("切断しました");
            client.close();
            System.exit(0);
        } catch (MqttException me) {
            System.out.println("理由 " + me.getReasonCode());
            System.out.println("メッセージ " + me.getMessage());
            System.out.println("ローカライズメッセージ " + me.getLocalizedMessage());
            System.out.println("原因 " + me.getCause());
            System.out.println("例外 " + me);
            me.printStackTrace();
        }
    }
}

```

**コールバックメッセージ処理クラス OnMessageCallback.java**

```java
package io.emqx;

import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttMessage;

public class OnMessageCallback implements MqttCallback {
    public void connectionLost(Throwable cause) {
        // 接続が切断された後、通常ここで再接続を行う
        System.out.println("切断されました。再接続できます。");
    }

    public void messageArrived(String topic, MqttMessage message) throws Exception {
        // サブスクライブ後に受信したメッセージはここで処理される
        System.out.println("受信したメッセージのトピック:" + topic);
        System.out.println("受信したメッセージのQoS:" + message.getQos());
        System.out.println("受信したメッセージの内容:" + new String(message.getPayload()));
    }

    public void deliveryComplete(IMqttDeliveryToken token) {
        System.out.println("配信完了---------" + token.isComplete());
    }
}
```

## Paho Java MQTT 5.0対応状況

現時点で、Paho JavaはMQTT 5.0に対応中であり、まだ完全にはサポートされていません。
