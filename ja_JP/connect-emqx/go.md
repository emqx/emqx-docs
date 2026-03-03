# Go SDKによる接続

[Eclipse Paho MQTT Go Client](https://github.com/eclipse/paho.mqtt.golang) は、Eclipse PahoプロジェクトのGo言語向けクライアントライブラリであり、MQTTブローカーに接続してメッセージをパブリッシュ、トピックをサブスクライブし、パブリッシュされたメッセージを受信できます。完全な非同期動作モードをサポートしています。

このクライアントはGoogleのソフトウェアパッケージである[proxy](https://godoc.org/golang.org/x/net/proxy)と[websockets](https://godoc.org/github.com/gorilla/websocket)に依存しており、以下のコマンドでインストール可能です。

```bash
go get github.com/eclipse/paho.mqtt.golang
```

## MQTT Go 使用例

以下の例は、Go言語でPaho MQTTを使ってEMQXに接続し、メッセージの送受信を行う完全なコードです。

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

## Paho Golang MQTT 5.0対応状況

現時点でPaho GolangはMQTT 5.0に対応中であり、まだ完全なサポートは実装されていません。
