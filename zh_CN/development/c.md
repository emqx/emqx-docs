---
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
ref:
---


# MQTT C 客户端库

[Eclipse Paho C](https://www.eclipse.org/paho/clients/c/) 与 [Eclipse Paho Embedded C](https://www.eclipse.org/paho/clients/c/embedded/) 均为 Eclipse Paho 项目下的 C 语言客户端库（MQTT C Client），均为使用 ANSI C 编写的功能齐全的 MQTT 客户端。

Eclipse Paho Embedded C 可以在桌面操作系统上使用，但主要针对 [mbed](http://mbed.org/)，[Arduino](http://www.arduino.cc/)和 [FreeRTOS](http://freertos.org/) 等嵌入式环境。

该客户端有同步/异步两种 API ，分别以 MQTTClient 和 MQTTAsync 开头：

- 同步 API 旨在更简单，更有用，某些调用将阻塞直到操作完成为止，使用编程上更加容易；
- 异步 API 中只有一个调用块 `API-waitForCompletion` ，通过回调进行结果通知，更适用于非主线程的环境。

## Paho C 使用示例

MQTT C 语言相关两个客户端库的比较、下载、使用方式等详细说明请移步至项目主页查看，本示例包含 C 语言的 Paho C 连接 EMQ X Broker，并进行消息收发完整代码：

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
  
    // MQTT 连接参数
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

## Paho C MQTT 5.0 支持

目前 Paho C 已经完整支持 MQTT 5.0。