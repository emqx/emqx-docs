# Go SDK を使った接続

[Eclipse Paho MQTT Go Client](https://github.com/eclipse/paho.mqtt.golang) は、Eclipse Paho プロジェクトの Go 言語用クライアントライブラリで、MQTT ブローカーに接続してメッセージをパブリッシュ／サブスクライブし、パブリッシュされたメッセージを受信できます。完全な非同期動作モードをサポートしています。

このクライアントは Google のソフトウェアパッケージである [proxy](https://godoc.org/golang.org/x/net/proxy) と [websockets](https://godoc.org/github.com/gorilla/websocket) に依存しており、以下のコマンドでインストールできます。

```bash
go get github.com/eclipse/paho.mqtt.golang
```

## MQTT Go 使用例

以下は、Go 言語で Paho MQTT を使って EMQX に接続し、メッセージの送受信を行う完全なコード例です。

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
	// メッセージコールバックハンドラーを設定
	opts.SetDefaultPublishHandler(f)
	opts.SetPingTimeout(1 * time.Second)

	c := mqtt.NewClient(opts)
	if token := c.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	// トピックをサブスクライブ
	if token := c.Subscribe("testtopic/#", 0, nil); token.Wait() && token.Error() != nil {
		fmt.Println(token.Error())
		os.Exit(1)
	}
	
	// メッセージをパブリッシュ
	token := c.Publish("testtopic/1", 0, false, "Hello World")
	token.Wait()

	time.Sleep(6 * time.Second)

	// サブスクライブ解除
	if token := c.Unsubscribe("testtopic/#"); token.Wait() && token.Error() != nil {
		fmt.Println(token.Error())
		os.Exit(1)
	}
  
	// 切断
	c.Disconnect(250)
	time.Sleep(1 * time.Second)
}
```

## Paho Golang の MQTT 5.0 サポート状況

現時点で、Paho Golang はまだ MQTT 5.0 に完全対応しておらず、適応中です。
