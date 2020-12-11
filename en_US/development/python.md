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


# MQTT Python client library

[Eclipse Paho Python](https://github.com/eclipse/paho.mqtt.python) is a Python language client library under the Eclipse Paho project, which can connect to MQTT Broker to publish messages, subscribe to topics and receive Published message.

Install using the PyPi package management tool:

```bash
pip install paho-mqtt
```

## Paho Python usage example

This example contains the complete code of Paho Python in Python connecting to EMQ X Broker, sending and receiving messages:


```python
import paho.mqtt.client as mqtt

#Connection success callback
def on_connect(client, userdata, flags, rc):
    print('Connected with result code '+str(rc))
    client.subscribe('testtopic/#')

# Message receiving callback
def on_message(client, userdata, msg):
    print(msg.topic+" "+str(msg.payload))

client = mqtt.Client()

# Specify callback function
client.on_connect = on_connect
client.on_message = on_message

# Establish a connection
client.connect('broker.emqx.io', 1883, 60)
# Publish a message
client.publish('emqtt',payload='Hello World',qos=0)

client.loop_forever()
```


## Paho Python MQTT 5.0 support

Currently, Paho Python is still adapting to MQTT 5.0 and has not yet been fully supported it.
