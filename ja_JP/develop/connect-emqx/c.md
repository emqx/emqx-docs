# C SDKによる接続

[Eclipse Paho C](https://www.eclipse.org/paho/clients/c/) および [Eclipse Paho Embedded C](https://www.eclipse.org/paho/clients/c/embedded/) は、Eclipse Pahoプロジェクトに属するC言語のクライアントライブラリ（MQTT Cクライアント）であり、ANSI Cで書かれたフル機能のMQTTクライアントです。

Eclipse Paho Embedded CはデスクトップOSでも利用可能ですが、主に [mbed](http://mbed.org/)、[Arduino](http://www.arduino.cc/) 、および [FreeRTOS](http://freertos.org/) といった組み込み環境向けに設計されています。

クライアントは同期／非同期のAPIを持ち、それぞれ `MQTTClient` と `MQTTAsync` で始まります：

- 同期APIはよりシンプルで使いやすく設計されており、いくつかの呼び出しは処理完了までブロックされるため、プログラミングが容易です。
- 非同期APIは `API-waitForCompletion` の呼び出しブロックが1つだけで、コールバックを通じて通知されるため、メインスレッド以外の環境により適しています。

## Paho C 使用例

C言語に関連する2つのMQTTクライアントライブラリの比較、ダウンロード、使用方法の詳細については、プロジェクトのホームページをご参照ください。以下の例は、EMQXに接続し、メッセージを送受信するPaho Cの完全なC言語コードです：

```c
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "MQTTClient.h"

#define ADDRESS     "tcp://broker.emqx.io:1883"
#define CLIENTID    "emqx_test"
#define TOPIC       "testtopic/1"
#define PAYLOAD     "Hello World!"
#define QOS         1
#define TIMEOUT     10000L

int main(int argc, char* argv[])
{
    MQTTClient client;
    MQTTClient_connectOptions conn_opts = MQTTClient_connectOptions_initializer;
    MQTTClient_message pubmsg = MQTTClient_message_initializer;
    MQTTClient_deliveryToken token;
    int rc;

    MQTTClient_create(&client, ADDRESS, CLIENTID,
        MQTTCLIENT_PERSISTENCE_NONE, NULL);

    // MQTT接続パラメータ
    conn_opts.keepAliveInterval = 20;
    conn_opts.cleansession = 1;

    if ((rc = MQTTClient_connect(client, &conn_opts)) != MQTTCLIENT_SUCCESS)
    {
        printf("接続に失敗しました。戻りコード %d\n", rc);
        exit(-1);
    }

    // メッセージのパブリッシュ
    pubmsg.payload = PAYLOAD;
    pubmsg.payloadlen = strlen(PAYLOAD);
    pubmsg.qos = QOS;
    pubmsg.retained = 0;
    MQTTClient_publishMessage(client, TOPIC, &pubmsg, &token);
    printf("%d秒以内にメッセージ %s をトピック %s にクライアントID %s でパブリッシュします\n",
            (int)(TIMEOUT/1000), PAYLOAD, TOPIC, CLIENTID);
    rc = MQTTClient_waitForCompletion(client, token, TIMEOUT);
    printf("デリバリートークン %d のメッセージが配信されました\n", token);

    // 切断
    MQTTClient_disconnect(client, 10000);
    MQTTClient_destroy(&client);
    return rc;
}
```

## Paho C MQTT 5.0対応状況

Paho Cは現在、MQTT 5.0を完全にサポートしています。
