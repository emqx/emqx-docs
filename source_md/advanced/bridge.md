---
# 标题
title: 消息桥接
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

# 消息桥接


目前 EMQ X 支持的桥接方式有:

  - RPC 桥接：RPC 桥接只能在 EMQ X Broker 间使用，且不支持订阅远程节点的主题去同步数据
  - MQTT 桥接：MQTT 桥接同时支持转发和通过订阅主题来实现数据同步两种方式

其概念如下图所示:

![image](./_static/images/bridge.png)

此外 *EMQ X* 消息服务器支持多节点桥接模式互联:

![image](_static/images/bridges_3.png)

在 EMQ X 中，通过修改 `etc/plugins/emqx_bridge_mqtt.conf` 来配置 bridge。EMQ X
根据不同的 name 来区分不同的 bridge。例如:

    ## Bridge address: node name for local bridge, host:port for remote.
    bridge.mqtt.aws.address = 127.0.0.1:1883

该项配置声明了一个名为 `aws` 的 bridge 并指定以 MQTT 的方式桥接到 `127.0.0.1:1883` 这台 MQTT 服务器

在需要创建多个 bridge 时，可以先复制其全部的配置项，在通过使用不同的 name 来标示（比如
bridge.mqtt.$name.address 其中 $name 指代的为 bridge 的名称）

接下来两个小节，表述了如何创建 RPC/MQTT 方式的桥接，并创建一条转发传感器 (sensor) 主题消息的转发规则。假设在两台主机上启动了两个
EMQ X 节点：

|       |                     |         |
| ----- | ------------------- | ------- |
| 名称    | 节点                  | MQTT 端口 |
| emqx1 | <emqx1@192.168.1.1> | 1883    |
| emqx2 | <emqx2@192.168.1.2> | 1883    |

### EMQ X 节点 RPC 桥接配置

以下是 RPC 桥接的基本配置，最简单的 RPC 桥接只需要配置以下三项就可以了:

    ## 桥接地址： 使用节点名（nodename@host）则用于 RPC 桥接，使用 host:port 用于 MQTT 连接
    bridge.mqtt.emqx2.address = emqx2@192.168.1.2
    
    ## 转发消息的主题
    bridge.mqtt.emqx2.forwards = sensor1/#,sensor2/#
    
    ## 桥接的 mountpoint (挂载点)
    bridge.mqtt.emqx2.mountpoint = bridge/emqx2/${node}/

forwards 用于指定桥接的主题。所有发到 forwards 指定主题上的消息都会被转发到远程节点上。

mountpoint 用于在转发消息时加上主题前缀。例如，以上配置中，主题为 sensor1/hello 的消息，EMQ X
将其转发到对端节点时，会将主题变为
bridge/emqx2/emqx1@192.168.1.1/sensor1/hello。

RPC 桥接的特点：

1.  RPC 桥接只能将本地的消息转发到远程桥接节点上，无法将远程桥接节点的消息同步到本地节点上；
2.  RPC 桥接只能将两个 EMQ X 桥接在一起，无法桥接 EMQ X 到其他的 MQTT Broker 上；
3.  RPC 桥接不涉及 MQTT 协议编解码，效率高于 MQTT 桥接。

### EMQ X 节点 MQTT 桥接配置

EMQ X 可以通过 MQTT Bridge 去订阅远程 MQTT Broker 的主题，再将远程 MQTT Broker 的消息同步到本地。

EMQ X 的 MQTT Bridge 原理：作为 MQTT 客户端连接到远程的 MQTT Broker，因此在 MQTT Bridge
的配置中，需要设置 MQTT 客户端连接时所需要的字段：

    ## 桥接地址
    bridge.mqtt.emqx2.address = 192.168.1.2:1883
    
    ## 桥接的协议版本
    ## 枚举值: mqttv3 | mqttv4 | mqttv5
    bridge.mqtt.emqx2.proto_ver = mqttv4
    
    ## 客户端的 clientid
    bridge.mqtt.emqx2.clientid = bridge_emq
    
    ## 客户端的 clean_start 字段
    ## 注：有些 MQTT Broker 需要将 clean_start 值设成 `true`
    bridge.mqtt.emqx2.clean_start = true
    
    ## 客户端的 username 字段
    bridge.mqtt.emqx2.username = user
    
    ## 客户端的 password 字段
    bridge.mqtt.emqx2.password = passwd
    
    ## 客户端是否使用 ssl 来连接远程服务器
    bridge.mqtt.emqx2.ssl = off
    
    ## 客户端 SSL 连接的 CA 证书 (PEM 格式)
    bridge.mqtt.emqx2.cacertfile = etc/certs/cacert.pem
    
    ## 客户端 SSL 连接的 SSL 证书
    bridge.mqtt.emqx2.certfile = etc/certs/client-cert.pem
    
    ## 客户端 SSL 连接的密钥文件
    bridge.mqtt.emqx2.keyfile = etc/certs/client-key.pem
    
    ## SSL 加密方式
    bridge.mqtt.emqx2.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384
    
    ## TLS PSK 的加密套件
    ## 注意 'listener.ssl.external.ciphers' 和 'listener.ssl.external.psk_ciphers' 不能同时配置
    ##
    ## See 'https://tools.ietf.org/html/rfc4279#section-2'.
    ## bridge.mqtt.emqx2.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA
    
    ## 客户端的心跳间隔
    bridge.mqtt.emqx2.keepalive = 60s
    
    ## 支持的 TLS 版本
    bridge.mqtt.emqx2.tls_versions = tlsv1.2,tlsv1.1,tlsv1
    
    ## 需要被转发的消息的主题
    bridge.mqtt.emqx2.forwards = sensor1/#,sensor2/#
    
    ## 挂载点 (mountpoint)
    bridge.mqtt.emqx2.mountpoint = bridge/emqx2/${node}/
    
    ## 订阅对端的主题
    bridge.mqtt.emqx2.subscription.1.topic = cmd/topic1
    
    ## 订阅对端主题的 QoS
    bridge.mqtt.emqx2.subscription.1.qos = 1
    
    ## 桥接的重连间隔
    ## 默认: 30 秒
    bridge.mqtt.emqx2.reconnect_interval = 30s
    
    ## QoS1/QoS2 消息的重传间隔
    bridge.mqtt.emqx2.retry_interval = 20s
    
    ## Inflight 大小.
    bridge.mqtt.emqx2.max_inflight_batches = 32

### EMQ X 桥接缓存配置

EMQ X 的 Bridge 拥有消息缓存机制，缓存机制同时适用于 RPC 桥接和 MQTT 桥接，当 Bridge
断开（如网络连接不稳定的情况）时，可将 forwards
主题的消息缓存到本地的消息队列上。等到桥接恢复时，再把消息重新转发到远程节点上。关于缓存队列的配置如下：

    ## emqx_bridge 内部用于 batch 的消息数量
    bridge.mqtt.emqx2.queue.batch_count_limit = 32
    
    ## emqx_bridge 内部用于 batch 的消息字节数
    bridge.mqtt.emqx2.queue.batch_bytes_limit = 1000MB
    
    ## 放置 replayq 队列的路径，如果没有在配置中指定该项，那么 replayq
    ## 将会以 `mem-only` 的模式运行，消息不会缓存到磁盘上。
    bridge.mqtt.emqx2.queue.replayq_dir = data/emqx_emqx2_bridge/
    
    ## Replayq 数据段大小
    bridge.mqtt.emqx2.queue.replayq_seg_bytes = 10MB

`bridge.mqtt.emqx2.queue.replayq_dir` 是用于指定 bridge 存储队列的路径的配置参数。

`bridge.mqtt.emqx2.queue.replayq_seg_bytes`
是用于指定缓存在磁盘上的消息队列的最大单个文件的大小，如果消息队列大小超出指定值的话，会创建新的文件来存储消息队列。

### EMQ X 桥接的命令行使用

启动 emqx\_bridge\_mqtt 插件:

``` sourceCode bash
$ cd emqx1/ && ./bin/emqx_ctl plugins load emqx_bridge_mqtt
ok
```

桥接 CLI 命令：

``` sourceCode bash
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

列出全部 bridge 状态

``` sourceCode bash
$ ./bin/emqx_ctl bridges list
name: emqx     status: Stopped
```

启动指定 bridge

``` sourceCode bash
$ ./bin/emqx_ctl bridges start emqx
Start bridge successfully.
```

停止指定 bridge

``` sourceCode bash
$ ./bin/emqx_ctl bridges stop emqx
Stop bridge successfully.
```

列出指定 bridge 的转发主题

``` sourceCode bash
$ ./bin/emqx_ctl bridges forwards emqx
topic:   topic1/#
topic:   topic2/#
```

添加指定 bridge 的转发主题

``` sourceCode bash
$ ./bin/emqx_ctl bridges add-forwards emqx 'topic3/#'
Add-forward topic successfully.
```

删除指定 bridge 的转发主题

``` sourceCode bash
$ ./bin/emqx_ctl bridges del-forwards emqx 'topic3/#'
Del-forward topic successfully.
```

列出指定 bridge 的订阅

``` sourceCode bash
$ ./bin/emqx_ctl bridges subscriptions emqx
topic: cmd/topic1, qos: 1
topic: cmd/topic2, qos: 1
```

添加指定 bridge 的订阅主题

``` sourceCode bash
$ ./bin/emqx_ctl bridges add-subscription emqx 'cmd/topic3' 1
Add-subscription topic successfully.
```

删除指定 bridge 的订阅主题

``` sourceCode bash
$ ./bin/emqx_ctl bridges del-subscription emqx 'cmd/topic3'
Del-subscription topic successfully.
```

注：如果有创建多个 Bridge 的需求，需要复制默认的 Bridge 配置，再拷贝到 emqx\_bridge\_mqtt.conf
中，根据需求重命名 bridge.mqtt.${name}.config 中的 name 即可。