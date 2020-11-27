# MQTT 桥接

EMQ X 桥接转发 MQTT 消息到 MQTT Broker，支持桥接至常见 MQTT 云服务:

![image](./assets/bridge_mqtt.png)

mqtt bridge 桥接插件配置文件: etc/plugins/emqx_bridge_mqtt.conf。

## 配置 MQTT 桥接的 Broker 地址

```bash
## 桥接地址： 使用节点名则用于 rpc 桥接，使用 host:port 用于 mqtt 连接
bridge.mqtt.aws.address = 127.0.0.1:1883

## 桥接的协议版本
## 枚举值: mqttv3 | mqttv4 | mqttv5
bridge.mqtt.aws.proto_ver = mqttv4

## mqtt 连接是否启用桥接模式
bridge.mqtt.aws.bridge_mode = true

## mqtt 客户端的 client_id
bridge.mqtt.aws.client_id = bridge_aws

## mqtt 客户端的 clean_start 字段
## 注: 有些 MQTT Broker 需要将 clean_start 值设成 `true`
bridge.mqtt.aws.clean_start = true

## mqtt 客户端的 username 字段
bridge.mqtt.aws.username = user

## mqtt 客户端的 password 字段
bridge.mqtt.aws.password = passwd

## mqtt 客户端是否使用 ssl 来连接远程服务器
bridge.mqtt.aws.ssl = off

## 客户端 SSL 连接的 CA 证书 (PEM格式)
bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem

## 客户端 SSL 连接的 SSL 证书
bridge.mqtt.aws.certfile = etc/certs/client-cert.pem

## 客户端 SSL 连接的密钥文件
bridge.mqtt.aws.keyfile = etc/certs/client-key.pem

## SSL 加密算法
bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384

## TLS PSK 的加密算法
## 注意 'listener.ssl.external.ciphers' 和 'listener.ssl.external.psk_ciphers' 不能同时配置
##
## See 'https://tools.ietf.org/html/rfc4279#section-2'.
bridge.mqtt.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

## 客户端的心跳间隔
bridge.mqtt.aws.keepalive = 60s

## 支持的 TLS 版本
bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

## 配置 MQTT 桥接转发和订阅主题

```bash
## 桥接的 mountpoint(挂载点)
bridge.mqtt.aws.mountpoint = bridge/aws/${node}/

## 转发消息的主题
bridge.mqtt.aws.forwards = topic1/#,topic2/#

## 用于桥接的订阅主题
bridge.mqtt.aws.subscription.1.topic = cmd/topic1

## 用于桥接的订阅 qos
bridge.mqtt.aws.subscription.1.qos = 1

## 用于桥接的订阅主题
bridge.mqtt.aws.subscription.2.topic = cmd/topic2

## 用于桥接的订阅 qos
bridge.mqtt.aws.subscription.2.qos = 1
```

## MQTT 桥接转发和订阅主题说明

挂载点 Mountpoint: mountpoint 用于在转发消息时加上主题前缀，该配置选项须配合 forwards 使用，转发主题为
sensor1/hello 的消息, 到达远程节点时主题为 bridge/aws/emqx1@192.168.1.1/sensor1/hello。

转发主题 Forwards: 转发到本地 EMQX 指定 forwards 主题上的消息都会被转发到远程 MQTT Broker 上。

订阅主题 Subscription: 本地 EMQX 通过订阅远程 MQTT Broker 的主题来将远程 MQTT Broker
上的消息同步到本地。

## 启用 bridge_mqtt 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_mqtt
```

## 桥接 CLI 命令

```bash
$ cd emqx1/ && ./bin/emqx_ctl bridges
bridges list                                    # List bridges
bridges start <Name>                            # Start a bridge
bridges stop <Name>                             # Stop a bridge
bridges forwards <Name>                         # Show a bridge forward topic
bridges add-forward <Name> <Topic>              # Add bridge forward topic
bridges del-forward <Name> <Topic>              # Delete bridge forward topic
bridges subscriptions <Name>                    # Show a bridge subscriptions topic
bridges add-subscription <Name> <Topic> <Qos>   # Add bridge subscriptions topic
```

## 列出全部 bridge 状态

```bash
$ ./bin/emqx_ctl bridges list
name: emqx     status: Stopped
```

## 启动指定 bridge

```bash
$ ./bin/emqx_ctl bridges start emqx
Start bridge successfully.
```

## 停止指定 bridge

```bash
$ ./bin/emqx_ctl bridges stop emqx
Stop bridge successfully.
```

## 列出指定 bridge 的转发主题

```bash
$ ./bin/emqx_ctl bridges forwards emqx
topic:   topic1/#
topic:   topic2/#
```

## 添加指定 bridge 的转发主题

```bash
$ ./bin/emqx_ctl bridges add-forwards emqx topic3/#
Add-forward topic successfully.
```

## 删除指定 bridge 的转发主题

```bash
$ ./bin/emqx_ctl bridges del-forwards emqx topic3/#
Del-forward topic successfully.
```

## 列出指定 bridge 的订阅

```bash
$ ./bin/emqx_ctl bridges subscriptions emqx
topic: cmd/topic1, qos: 1
topic: cmd/topic2, qos: 1
```

## 添加指定 bridge 的订阅主题

```bash
$ ./bin/emqx_ctl bridges add-subscription emqx cmd/topic3 1
Add-subscription topic successfully.
```

## 删除指定 bridge 的订阅主题

```bash
$ ./bin/emqx_ctl bridges del-subscription emqx cmd/topic3
Del-subscription topic successfully.
```