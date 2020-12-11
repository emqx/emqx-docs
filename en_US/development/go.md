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


# MQTT Go Client library

[Eclipse Paho MQTT Go Client](https://github.com/eclipse/paho.mqtt.golang) is the Go language client library under the Eclipse Paho project, which can connect to the MQTT Broker to publish messages, subscribe to topics and receive the published message. It supports asynchronous operation mode completely.

The client depends on Google's software Package of [proxy](https://godoc.org/golang.org/x/net/proxy) and [websockets](https://godoc.org/github.com/gorilla/websocket), which can be installed with the following command:

```bash
go get github.com/eclipse/paho.mqtt.golang
```

## MQTT Go usage example

This example contains the complete code for Paho MQTT in Go language connecting to EMQ X Broker, sending and receiving messages:

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
	// Set the message callback handler
	opts.SetDefaultPublishHandler(f)
	opts.SetPingTimeout(1 * time.Second)

	c := mqtt.NewClient(opts)
	if token := c.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	// Subscribe to a topic
	if token := c.Subscribe("testtopic/#", 0, nil); token.Wait() && token.Error() != nil {
		fmt.Println(token.Error())
		os.Exit(1)
	}
	
	// Publish a message
	token := c.Publish("testtopic/1", 0, false, "Hello World")
	token.Wait()

	time.Sleep(6 * time.Second)

	// Unscribe
	if token := c.Unsubscribe("testtopic/#"); token.Wait() && token.Error() != nil {
		fmt.Println(token.Error())
		os.Exit(1)
	}
  
  // Disconnect
	c.Disconnect(250)
	time.Sleep(1 * time.Second)
}
```




## Paho Golang MQTT 5.0 support

Currently, Paho Golang is still adapting to MQTT 5.0 and has not yet fully supported it.