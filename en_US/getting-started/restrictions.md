# Operating limitations 

## Background 

As well as following protocols, if there are no limitations on connection numbers or data transmission, users may experience reduced MQTT broker performance, such as a slow network connection or operation response, prolonged message latency, message accumulation, or even message discards, and other issues.

EMQX has therefore specified the following quotas and usage limits for some of our major features.

## Reference list

The operating limitations can be classified into: 

- Broker resource limitation: Theoretically, there is no maximum limit, the actual limit varies with the server specification.
- Hard coding or protocol limitation: EMQX has set these limitations to comply with the protocols or to ensure stable performance. Note: In some projects, EMQX has modified the default setting (as specified in the protocol) to a more reasonable value. You can change the setting with our configuration file. 

| **Limitation**       | **Description**              | **Quota**               |
| -------------------- | ---------------------------- | ----------------------- |
| 设备连接             | 最大并发连接设备数           | 不限                    |
| 最大设备建立连接速度 | 不限                         |                         |
| 最大客户端 ID 长度   | 65535                        |                         |
| 设备订阅             | 最大订阅数                   | 不限                    |
| 最大订阅速度         | 不限                         |                         |
| 单个设备订阅数       | 不限                         |                         |
| 单个设备订阅速度     | 不限                         |                         |
| 网络流量             | 最大带宽                     | 不限                    |
| 单个设备带宽         | 不限                         |                         |
| MQTT 消息            | 单条消息大小                 | 默认 1024KB，最大 256MB |
| 最大 QoS             | 2                            |                         |
| MQTT 心跳时长        | 支持设置的最大心跳时长       | 65535 秒                |
| MQTT 主题            | 主题数量                     | 不限                    |
| 主题层级             | 65535                        |                         |
| 主题长度             | 不限                         |                         |
| 支持的主题别名数量   | 65535                        |                         |
| MQTT 保留消息        | 单条消息大小                 | 默认 1204KB，最大 256MB |
| 保留消息总数         | 不限                         |                         |
| 保留消息总大小       | 不限                         |                         |
| MQTT 5.0 协议        | 最多可添加用户自定义属性个数 | 65535                   |
| MQTT 扩展            | 主题重写规则数量             | 30                      |
| 代理订阅规则数量     | 30                           |                         |
| 延迟发布消息数量     | 不限                         |                         |
| 延迟发布最大时长     | 4294967 秒                   |                         |
| 规则引擎             | 规则数量                     | 不限                    |
| 规则执行超时         | 不限                         |                         |
| 单个规则目的地数量   | 不限                         |                         |
| 数据桥接             | 数据桥接数量                 | 不限                    |
| REST API             | 分页最大大小                 | 10000                   |
| API 密钥             | API 密钥数量                 | 1024                    |
| Dashboard            | Dashboard 用户数量           | 不限                    |
