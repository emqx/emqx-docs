# 上下线通知
EMQ X 的上下线系统消息通知功能在客户端连接成功或者客户端断开连接，自动发送一条系统主题的消息, EMQ X 默认开启上下线通知模块。

## 上下线消息通知
通过 dashboard 页面可以开启 上下线通知 控制模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/)，并登陆，点击左侧的 “模块” 选项卡，选择添加

![image-20200927213049265](./assets/modules.png)

选择 上下线通知 模块

![image-20200927213049265](./assets/mod_presence1.png)

上下线通知不需要配置任何参数，直接点击添加后，模块添加完成

![image-20200927213049265](./assets/mod_presence2.png)

## 上下线消息通知格式

`$SYS` 主题前缀: `$SYS/brokers/${node}/clients/`

| 主题 (Topic)              | 说明                                     |
| ------------------------ | ---------------------------------------- |
| ${clientid}/connected    | 上线事件。当任意客户端上线时，EMQ X 就会发布该主题的消息 |
| ${clientid}/disconnected | 下线事件。当任意客户端下线时，EMQ X 就会发布该主题的消息 |

`connected` 事件消息的 Payload 解析成 JSON 格式如下:

```bash
{
    "username":"undefined",
    "ts":1582687922392,
    "sockport":1883,
    "proto_ver":5,
    "proto_name":"MQTT",
    "keepalive":300,
    "ipaddress":"127.0.0.1",
    "expiry_interval":0,
    "connected_at":1582687922392,
    "connack":0,
    "clientid":"emqtt-8348fe27a87976ad4db3",
    "clean_start":true
}
```

`disconnected` 事件消息的 Payload 解析成 JSON 格式如下:

```bash
{
    "username":"undefined",
    "ts":1582688032203,
    "reason":"tcp_closed",
    "disconnected_at":1582687922392,
    "clientid":"emqtt-8348fe27a87976ad4db3"
}
```