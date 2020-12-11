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


# MQTT Python 客户端库


[Eclipse Paho Python](https://github.com/eclipse/paho.mqtt.python) 为 Eclipse Paho 项目下的 Python 语言版客户端库，该库能够连接到 MQTT Broker 以发布消息，订阅主题并接收已发布的消息。

使用 PyPi 包管理工具安装：

```bash
pip install paho-mqtt
```

## Paho Python 使用示例

本示例包含 Python 语言的 Paho Python 连接 EMQ X Broker，并进行消息收发完整代码：


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


## Paho Python MQTT 5.0 支持

目前 Paho Python 还在适配 MQTT 5.0，尚未全面支持。
