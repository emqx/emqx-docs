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


# MQTT JavaScript 客户端库

[MQTT.js](https://www.npmjs.com/package/mqtt) 是 JavaScript 编写的，实现了 MQTT 协议客户端功能的模块，可以在浏览器 和 Node.js 环境中使用。

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

## MQTT.js 使用示例

本示例包含 JavaScrip 语言的 MQTT.js 连接 EMQ X Broker，并进行消息收发完整代码：

```javascript
// const mqtt = require('mqtt')
import mqtt from 'mqtt'

// 连接选项
const options = {
      clean: true, // true: 清除会话, false: 保留会话
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


## MQTT.js MQTT 5.0 支持

目前 MQTT.js 已经完整支持 MQTT 5.0。
