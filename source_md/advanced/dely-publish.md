---
# 标题
title: 延迟发布
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

# 延迟发布

EMQ X 可以实现延迟发布消息的功能。

想要实现这个功能需要打开 `emqx_delayed_publish` 插件, 使用 `emqx_ctl plugins load emqx_delayed_publish` 命令开启插件。

插件开启后客户端可以将需要延迟发送的消息发布到 EMQ X。使用特殊样式标识这边是一个需要延迟发布消息。格式是:

```
$delayed/{DelayInterval}/{TopicName}
```

- `$delayed`: 是一个字符串，它将主题名称标记为延迟主题。
- `{DelayInterval}`: 指定MQTT消息的延迟秒的延迟间隔，允许的最大间隔是4294967。
- `{TopicName}`: MQTT消息的主题名称。

例如:

- `$delayed/15/x/y`: 15秒后将MQTT消息发布到主题 `x/y`。
- `$delayed/60//a/b`: 1分钟后将MQTT消息发布到 `/a/b`。
- `$delayed/3600/$SYS/topic`: 1小时后将MQTT消息发布到 `$SYS/topic`。
