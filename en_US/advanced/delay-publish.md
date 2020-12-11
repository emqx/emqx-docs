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

# Delayed publish

The delay-publish function of EMQ X Broker can implement the function of delaying the PUBLISH packet publishing according to the time interval configured by the user. When a client publishes a message to EMQ X Broker with the special topic prefix  `$delayed/{DelayInteval}` , the delay-publish function is triggered.

The specific format of the delay-publish topic is as follows:

```bash
$delayed/{DelayInterval}/{TopicName}
```

- `$delayed`: Messages prefixed with `$delay` will be treated as messages that need to be delayed. The delay interval is determined by the content of the next topic level.
- `{DelayInterval}`: Specify the time interval for delaying the publish of this MQTT message with the unit of second. The maximum allowed interval is 4294967 seconds. If `{DelayInterval}` cannot be parsed as an integer number, EMQ X Broker will discard the message and the client will not receive any information.
- `{TopicName}`: The topic name of the MQTT message.

E.g:

- `$delayed/15/x/y`: Publish MQTT message to the topic `x/y` after 15 seconds
- `$delayed/60/a/b`: Publish MQTT message to the topic `a/b` after 1 minute
- `$delayed/3600/$SYS/topic`: Publish MQTT message to the topic  `$SYS/topic` after 1 hour

This feature is provided by the `emqx-delay-publish` plugin, which is disabled by default. You need to enable the plugin to use this feature. For the method of enabling the plugin, please refer to [Plugins](./plugins.md). If you need to use this feature for a long time, then it is recommended that you set the plugin to start by default, see  [directory structure](getting-started/directory.md) for more information.