# 上下线通知
EMQ X 的上下线系统消息通知功能在客户端连接成功或者客户端断开连接，自动发送一条系统主题的消息, EMQ X 默认开启上下线通知模块。

## 创建模块

上下线通知模块默认启动，可以通过dashboard页面进行启动和停止。

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/modules)，点击左侧的 “模块” 选项卡：

![image-20200927213049265](./assets/modules.png)

## 上下线消息通知格式

`$SYS` 主题前缀: `$SYS/brokers/${node}/clients/`

| 主题 (Topic)              | 说明                                     |
| ------------------------ | ---------------------------------------- |
| ${clientid}/connected    | 上线事件。当任意客户端上线时，EMQ X 就会发布该主题的消息 |
| ${clientid}/disconnected | 下线事件。当任意客户端下线时，EMQ X 就会发布该主题的消息 |

`connected` 事件消息的 Payload 解析成 JSON 格式如下:

```bash
{
    "username": "foo",
    "ts": 1625572213873,
    "sockport": 1883,
    "proto_ver": 4,
    "proto_name": "MQTT",
    "keepalive": 60,
    "ipaddress": "127.0.0.1",
    "expiry_interval": 0,
    "connected_at": 1625572213873,
    "connack": 0,
    "clientid": "emqtt-8348fe27a87976ad4db3",
    "clean_start": true
}
```

`disconnected` 事件消息的 Payload 解析成 JSON 格式如下:

```bash
{
    "username": "foo",
    "ts": 1625572213873,
    "sockport": 1883,
    "reason": "tcp_closed",
    "proto_ver": 4,
    "proto_name": "MQTT",
    "ipaddress": "127.0.0.1",
    "disconnected_at": 1625572213873,
    "clientid": "emqtt-8348fe27a87976ad4db3"
}
```
