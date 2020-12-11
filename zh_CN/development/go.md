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


# MQTT Go 客户端库

[Eclipse Paho MQTT Go Client](https://github.com/eclipse/paho.mqtt.golang) 为 Eclipse Paho 项目下的 Go 语言版客户端库，该库能够连接到 MQTT Broker 以发布消息，订阅主题并接收已发布的消息，支持完全异步的操作模式。

客户端依赖于 Google 的 [proxy](https://godoc.org/golang.org/x/net/proxy) 和 [websockets](https://godoc.org/github.com/gorilla/websocket) 软件包，通过以下命令完成安装：

```bash
go get github.com/eclipse/paho.mqtt.golang
```

## MQTT Go 使用示例

本示例包含 Go 语言的 Paho MQTT 连接 EMQ X Broker，并进行消息收发完整代码：

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




## Paho Golang MQTT 5.0 支持

目前 Paho Golang 还在适配 MQTT 5.0，尚未全面支持。