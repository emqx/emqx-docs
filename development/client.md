---
# 标题
title: 客户端库
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 客户端库

本页选取各个编程语言中热门客户端库进行介绍说明，提供连接、发布、订阅、取消订阅基本功能代码示例。

- [C](./mqtt.md#eclipse-paho-c-与-eclipse-paho-embedded-c)
- [Java](./mqtt.md#eclipse-paho-java-client)
- [Go](./mqtt.md#eclipse-paho-mqtt-go-clien)
- [Erlang](./mqtt.md#emqtt--emq-提供的-erlang-mqtt-客户端库)
- [JavaScript](./mqtt.md#mqttjs-browser--nodejs-通用客户端)
- [Python](./mqtt.md#eclipse-paho-python)


MQTT 社区收录了更多 MQTT 客户端库，你可以[点击此处](https://github.com/mqtt/mqtt.github.io/wiki/libraries)查看。


## 客户端生命周期

MQTT 客户端整个生命周期的行为可以概括为：建立连接、订阅主题、接收消息并处理、向指定主题发布消息、取消订阅、断开连接。

标准的客户端库在每个环节都暴露出相应的方法，不同库在相同环节所需方法参数含义大致相同，具体选用哪些参数、启用哪些功能特性需要用户深入了解 MQTT 协议特性并结合实际应用场景而定。

以一个客户端连接并发布、处理消息为例，每个环节大致需要进行的步骤：

- **建立连接**：
  
  - 指定 MQTT Broker 基本信息接入地址与端口
  - 指定传输类型是 TCP 还是 MQTT over WebSocket
  - 如果启用 TLS 需要选择协议版本并携带相应的的证书
  - Broker 启用了认证鉴权则客户端需要携带相应的 MQTT Username Password 信息
  - 配置客户端参数如 keepalive 时长、clean session 回话保留标志位、MQTT 协议版本、遗嘱消息（LWT）等
  
- **订阅主题**：连接建立成功后可以订阅主题，需要指定主题信息

  - 指定主题过滤器 Topic，订阅的时候支持主题通配符 `+` 与 `#` 的使用
  - 指定 QoS，根据客户端库和 Broker 的实现可选  Qos 0 1 2，注意部分 Broker 与云服务提供商不支持部分 QoS 级别，如 AWS IoT 、阿里云 IoT 套件、Azure IoT Hub 均不支持 QoS 2 级别消息
  - 订阅主题可能因为网络问题、Broker 端 ACL 规则限制而失败

- **接收消息并处理**：

  - 一般是在连接时指定处理函数，依据客户端库与平台的网络编程模型不同此部分处理方式略有不同

- **发布消息**：向指定主题发布消息

  - 指定目标主题，注意该主题不能包含通配符 `+` 或 `#`，若主题中包含通配符可能会导致消息发布失败、客户端断开等情况（视 Broker 与客户端库实现方式）
  - 指定消息 QoS 级别，同样存在不同 Broker 与平台支持的 QoS 级别不同，如 Azure IoT Hub 发布 QoS 2 的消息将断开客户端连接
  - 指定消息体内容，消息体内容大小不能超出 Broker 设置最大消息大小
  - 指定消息 Retain 保留消息标志位

- **取消订阅**：

  - 指定目标主题即可

- **断开连接**：

  - 客户端主动断开连接，服务器端将发布遗嘱消息（LWT）


## Eclipse Paho C 与 Eclipse Paho Embedded C

[Eclipse Paho C](https://www.eclipse.org/paho/clients/c/) 与 [Eclipse Paho Embedded C](https://www.eclipse.org/paho/clients/c/embedded/) 均为 Eclipse Paho 项目下的客户端库，均为使用 ANSI  C 编写的功能齐全的 MQTT 客户端。

Eclipse Paho Embedded C 可以在桌面操作系统上使用，但主要针对 [mbed](http://mbed.org/)，[Arduino](http://www.arduino.cc/)和 [FreeRTOS](http://freertos.org/) 等嵌入式环境。

该客户端有同步/异步两种 API ，分别以 MQTTClient 和 MQTTAsync 开头：

- 同步 API 旨在更简单，更有用，某些调用将阻塞直到操作完成为止，使用编程上更加容易；
- 异步 API 中只有一个调用块 `API-waitForCompletion` ，通过回调进行结果通知，更适用于非主线程的环境。

两个库的下载、使用详细说明请移步至项目主页查看，本文使用 Eclipse Paho C，直接提供样例代码如下:

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
  
    // 连接参数
    conn_opts.keepAliveInterval = 20;
    conn_opts.cleansession = 1;

    if ((rc = MQTTClient_connect(client, &conn_opts)) != MQTTCLIENT_SUCCESS)
    {
        printf("Failed to connect, return code %d\n", rc);
        exit(-1);
    }
  
    // 发布消息
    pubmsg.payload = PAYLOAD;
    pubmsg.payloadlen = strlen(PAYLOAD);
    pubmsg.qos = QOS;
    pubmsg.retained = 0;
    MQTTClient_publishMessage(client, TOPIC, &pubmsg, &token);
    printf("Waiting for up to %d seconds for publication of %s\n"
            "on topic %s for client with ClientID: %s\n",
            (int)(TIMEOUT/1000), PAYLOAD, TOPIC, CLIENTID);
    rc = MQTTClient_waitForCompletion(client, token, TIMEOUT);
    printf("Message with delivery token %d delivered\n", token);
  
    // 断开连接
    MQTTClient_disconnect(client, 10000);
    MQTTClient_destroy(&client);
    return rc;
}
```



## Eclipse Paho Java Client

[Eclipse Paho Java Client](https://www.eclipse.org/paho/clients/java/) 是用 Java 编写的 MQTT 客户端库，可用于 JVM 或其他 Java 兼容平台（例如Android）。

Eclipse Paho Java Client 提供了MqttAsyncClient 和 MqttClient 异步和同步 API。

**通过 Maven 安装：**

```xml
<dependency>
  <groupId>org.eclipse.paho</groupId>
	<artifactId>org.eclipse.paho.client.mqttv3</artifactId>
	<version>1.2.2</version>
</dependency>
```

连接样例代码如下：

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

            // 连接选项
            MqttConnectOptions connOpts = new MqttConnectOptions();
            connOpts.setUserName("emqx_test");
            connOpts.setPassword("emqx_test_password".toCharArray());
            // 保留会话
            connOpts.setCleanSession(true);

            // 设置回调
            client.setCallback(new PushCallback());

            // 建立连接
            System.out.println("Connecting to broker: " + broker);
            client.connect(connOpts);

            System.out.println("Connected");
            System.out.println("Publishing message: " + content);

            // 订阅
            client.subscribe(subTopic);

            // 消息发布所需参数
            MqttMessage message = new MqttMessage(content.getBytes());
            message.setQos(qos);
            client.publish(pubTopic, message);
            System.out.println("Message published");

            client.disconnect();
            System.out.println("Disconnected");
            client.close();
            System.exit(0);
        } catch (MqttException me) {
            System.out.println("reason " + me.getReasonCode());
            System.out.println("msg " + me.getMessage());
            System.out.println("loc " + me.getLocalizedMessage());
            System.out.println("cause " + me.getCause());
            System.out.println("excep " + me);
            me.printStackTrace();
        }
    }
}

```

**回调消息处理类 OnMessageCallback.java**

```java
package io.emqx;

import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttMessage;

public class OnMessageCallback implements MqttCallback {
    public void connectionLost(Throwable cause) {
        // 连接丢失后，一般在这里面进行重连
        System.out.println("连接断开，可以做重连");
    }

    public void messageArrived(String topic, MqttMessage message) throws Exception {
        // subscribe后得到的消息会执行到这里面
        System.out.println("接收消息主题:" + topic);
        System.out.println("接收消息Qos:" + message.getQos());
        System.out.println("接收消息内容:" + new String(message.getPayload()));
    }

    public void deliveryComplete(IMqttDeliveryToken token) {
        System.out.println("deliveryComplete---------" + token.isComplete());
    }
}
```





## Eclipse Paho MQTT Go client

[Eclipse Paho MQTT Go Client](https://github.com/eclipse/paho.mqtt.golang) 为 Eclipse Paho 项目下的 Go 语言版客户端库，该库能够连接到 MQTT Broker 以发布消息，订阅主题并接收已发布的消息，支持完全异步的操作模式。

客户端依赖于 Google 的 [proxy](https://godoc.org/golang.org/x/net/proxy) 和 [websockets](https://godoc.org/github.com/gorilla/websocket) 软件包，通过以下命令完成安装：

```bash
go get github.com/eclipse/paho.mqtt.golang
```

连接样例代码如下：

```go
package main

import (
	"fmt"
	"log"
	"os"
	"time"

	"github.com/eclipse/paho.mqtt.golang"
)

var f mqtt.MessageHandler = func(client mqtt.Client, msg mqtt.Message) {
	fmt.Printf("TOPIC: %s\n", msg.Topic())
	fmt.Printf("MSG: %s\n", msg.Payload())
}

func main() {
	mqtt.DEBUG = log.New(os.Stdout, "", 0)
	mqtt.ERROR = log.New(os.Stdout, "", 0)
	opts := mqtt.NewClientOptions().AddBroker("tcp://broker.emqx.io:1883").SetClientID("emqx_test_client")
	
	opts.SetKeepAlive(60 * time.Second)
	// 设置消息回调处理函数
	opts.SetDefaultPublishHandler(f)
	opts.SetPingTimeout(1 * time.Second)

	c := mqtt.NewClient(opts)
	if token := c.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	// 订阅主题
	if token := c.Subscribe("testtopic/#", 0, nil); token.Wait() && token.Error() != nil {
		fmt.Println(token.Error())
		os.Exit(1)
	}
	
	// 发布消息
	token := c.Publish("testtopic/1", 0, false, "Hello World")
	token.Wait()

	time.Sleep(6 * time.Second)

	// 取消订阅
	if token := c.Unsubscribe("testtopic/#"); token.Wait() && token.Error() != nil {
		fmt.Println(token.Error())
		os.Exit(1)
	}
  
  // 断开连接
	c.Disconnect(250)
	time.Sleep(1 * time.Second)
}
```



## emqtt Erlang MQTT 客户端

[emqtt](https://github.com/emqx/emqtt) 是开源 MQTT Broker EMQ X 官方 EMQ 提供的客户端库，适用于 Erlang 语言。

Erlang 生态有多个 MQTT Broker 实现，如通过插件支持 MQTT 的 RabbitMQ ，VerenMQ、EMQ X 等。但是 MQTT 客户端库几乎没有选择的余地，MQTT 社区收录的 Erlang 客户端库中 [emqtt](https://github.com/emqx/emqtt) 是最佳选择。

emqtt 完全由 Erlang 实现，完成整支持 MQTT v3.1.1 和 MQTT v5.0 协议版本，支持 SSL 单双向认证与 WebSocket 连接。另一款 MQTT 基准测试工具 [emqtt_bench](https://github.com/emqx/emqtt-bench) 就基于该客户端库构建。

emqtt 使用方式如下：

```erlang
ClientId = <<"test">>.
{ok, ConnPid} = emqtt:start_link([{clientid, ClientId}]).
{ok, _Props} = emqtt:connect(ConnPid).
Topic = <<"guide/#">>.
QoS = 1.
{ok, _Props, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, QoS}).
{ok, _PktId} = emqtt:publish(ConnPid, <<"guide/1">>, <<"Hello World!">>, QoS).
%% If the qos of publish packet is 0, `publish` function would not return packetid.
ok = emqtt:publish(ConnPid, <<"guide/2">>, <<"Hello World!">>, 0).

%% Recursively get messages from mail box.
Y = fun (Proc) -> ((fun (F) -> F(F) end)((fun(ProcGen) -> Proc(fun() -> (ProcGen(ProcGen))() end) end))) end.
Rec = fun(Receive) -> fun()-> receive {publish, Msg} -> io:format("Msg: ~p~n", [Msg]), Receive(); _Other -> Receive() after 5 -> ok end end end.
(Y(Rec))().

%% If you don't like y combinator, you can also try named function to recursively get messages in erlang shell.
Receive = fun Rec() -> receive {publish, Msg} -> io:format("Msg: ~p~n", [Msg]), Rec(); _Other -> Rec() after 5 -> ok end end.
Receive().

{ok, _Props, _ReasonCode} = emqtt:unsubscribe(ConnPid, <<"guide/#">>).

ok = emqtt:disconnect(ConnPid).
```



## MQTT.js Browser & Node.js 通用 MQTT 客户端

[MQTT.js](https://www.npmjs.com/package/mqtt) 是 JavaScript 编写的，实现了 MQTT 协议客户端功能的模块，可以在浏览器 和 Node.js环境中使用。

由于 JavaScript 单线程特性，MQTT.js 是全异步 MQTT 客户端，MQTT.js 支持 MQTT 与 MQTT over WebSocket，在不同运行环境支持的度如下：

- 浏览器环境：MQTT over WebSocket（包括微信小程序、支付宝小程序等定制浏览器环境）
- Node.js 环境：MQTT、MQTT over WebSocket

不同环境里除了少部分连接参数不同，其他 API 均是相同的。

使用 npm 安装：

```bash
npm i mqtt
```

使用 CDN 安装（浏览器）：

```html
<script src="https://unpkg.com/mqtt/dist/mqtt.min.js"></script>
<script>
    // 将在全局初始化一个 mqtt 变量
    console.log(mqtt)
</script>
```

在安装 Node.js 的环境里，可以通过 `npm i mqtt -g` 命令全局安装以命令行的形式使用 MQTT.js。

```bash
npm i mqtt -g

mqtt help

> MQTT.js command line interface, available commands are:

  * publish     publish a message to the broker
  * subscribe   subscribe for updates from the broker
  * version     the current MQTT.js version
  * help        help about commands

> Launch 'mqtt help [command]' to know more about the commands.
```

样例代码：

```javascript
// const mqtt = require('mqtt')
import mqtt from 'mqtt'

// 连接选项
const options = {
  		clean: true, // 保留回话
      connectTimeout: 4000, // 超时时间
      // 认证信息
      clientId: 'emqx_test',
      username: 'emqx_test',
      password: 'emqx_test',
}

// 连接字符串, 通过协议指定使用的连接方式
// ws 未加密 WebSocket 连接
// wss 加密 WebSocket 连接
// mqtt 未加密 TCP 连接
// mqtts 加密 TCP 连接
// wxs 微信小程序连接
// alis 支付宝小程序连接
const connectUrl = 'wss://broker.emqx.io:8084/mqtt'
const client = mqtt.connect(connectUrl, options)

client.on('reconnect', (error) => {
    console.log('正在重连:', error)
})

client.on('error', (error) => {
    console.log('连接失败:', error)
})

client.on('message', (topic, message) => {
  console.log('收到消息：', topic, message.toString())
})
```

## Eclipse Paho Python

[Eclipse Paho Python](https://github.com/eclipse/paho.mqtt.python) 为 Eclipse Paho 项目下的 Python 语言版客户端库，该库能够连接到 MQTT Broker 以发布消息，订阅主题并接收已发布的消息。

使用 PyPi 包管理工具安装：

```bash
pip install paho-mqtt
```

代码样例：

```python

import paho.mqtt.client as mqtt


# 连接成功回调
def on_connect(client, userdata, flags, rc):
    print('Connected with result code '+str(rc))
    client.subscribe('testtopic/#')

# 消息接收回调
def on_message(client, userdata, msg):
    print(msg.topic+" "+str(msg.payload))

client = mqtt.Client()

# 指定回调函数
client.on_connect = on_connect
client.on_message = on_message

# 建立连接
client.connect('broker.emqx.io', 1883, 60)
# 发布消息
client.publish('emqtt',payload='Hello World',qos=0)

client.loop_forever()
```