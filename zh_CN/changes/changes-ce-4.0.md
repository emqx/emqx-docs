# v4.0

## 4.0.13

*发布日期: 2021-04-13*

EMQX 4.0.13 现已发布，主要包含以下改动

### emqx-management

**Bug fixes:**

- 修复数据导入导出时的问题

  Github PR: [emqx-management#319](https://github.com/emqx/emqx-management/pull/319), [emqx-management#321](https://github.com/emqx/emqx-management/pull/321)

## 4.0.11

*发布日期: 2021-04-10*

EMQX 4.0.11 现已发布，主要包含以下改动

### emqx-management

**Bug fixes:**

- 导出数据时, 对 `emqx_auth_clientid` 的密码进行 base64 编码

  Github PR: [emqx-management#316](https://github.com/emqx/emqx-management/pull/316)

## 4.0.9

*发布日期: 2021-03-12*

EMQX 4.0.9 现已发布，主要修复了 MQTT 消息解析的问题。

## 4.1-beta.1

*发布日期: 2020-04-26*

EMQX 4.1-beta.1 现已发布，主要包含以下改动：
  
### emqx

**错误修复:**

- 修复 flapping 检查没有删除过期数据的问题

  Github PR: [emqx/emqx#3406](https://github.com/emqx/emqx/pull/3406)
  
- 修复内置 ACL 模块重新加载时没有清除 ACL 缓存的问题

  Github PR: [emqx/emqx#3409](https://github.com/emqx/emqx/pull/3409)
  
### emqx-management

**错误修复:**

- 修复错误的时间戳单位

  Github PR: [emqx/emqx-management#203](https://github.com/emqx/emqx-management/pull/203)

### emqx-bridge-mqtt

**错误修复:**

- 修复没有发送 PINREQ 报文导致连接断开的问题

  Github PR: [emqx/emqx-bridge-mqtt#68](https://github.com/emqx/emqx-bridge-mqtt/pull/68)

### emqx-statsd

**错误修复:**

- 修复没有获取 EMQX Broker 指标的问题

  Github PR: [emqx/emqx-statsd#55](https://github.com/emqx/emqx-statsd/pull/55)

### emqx-sasl

**功能增强:**

- 支持服务端认证

  Github PR: [emqx/emqx-sasl#3](https://github.com/emqx/emqx-sasl/pull/3)
  
**错误修复:**

- 修复 SCRAM-SHA-1 认证算法不可用的问题

  Github PR: [emqx/emqx-sasl#2](https://github.com/emqx/emqx-sasl/pull/2)
  
### emqx-auth-jwt

**功能增强:**

- 支持配置 jwerl 签名格式

  Github PR: [emqx/emqx-auth-jwt#117](https://github.com/emqx/emqx-auth-jwt/pull/117)
  
### emqx-extension-hook

**功能增强:**

- 增加 Java 支持

  Github PR: [emqx/emqx-extension-hook#2](https://github.com/emqx/emqx-extension-hook/pull/2)
  
### emqx-dashboard

**功能增强:**

- 支持内部模块管理

  Github PR: [emqx/emqx-dasboard#225](https://github.com/emqx/emqx-dasboard/pull/225)
  
- 支持规则编辑

  Github PR: [emqx/emqx-dasboard#227](https://github.com/emqx/emqx-dasboard/pull/227), [emqx/emqx-dasboard#230](https://github.com/emqx/emqx-dasboard/pull/230)

## 4.0.6

*发布日期: 2020-04-22*

EMQX 4.0.6 现已发布，主要包含以下改动：
  
### emqx

**错误修复:**

- 修复 flapping 检查没有删除过期数据的问题

  Github PR: [emqx/emqx#3407](https://github.com/emqx/emqx/pull/3407)
  
- 修复使用 WebSocket 时 Proxy Protocol 功能无法使用的问题

  Github PR: [emqx/emqx#3372](https://github.com/emqx/emqx/pull/3372)
  
### emqx-bridge-mqtt

**错误修复:**

- 修复默认情况不会发送心跳包的问题

  Github PR: [emqx/emqx-bridge-mqtt#67](https://github.com/emqx/emqx-bridge-mqtt/pull/67)

### emqx-rule-engine

**错误修复:**

- 修复规则引擎时间戳的错误类型

  Github Commit: [emqx/emqx-rule-engine#27ca37](https://github.com/emqx/emqx-rule-engine/commit/27ca3768602c107af71ea6b20f4518bb0f70404d)

- 修复规则引擎测试 SQL 语句功能

  Github Commit: [emqx/emqx-rule-engine#33fcba](https://github.com/emqx/emqx-rule-engine/commit/33fcba394e59fef495e2fe54883297c8d3d893e5)


## 4.1-alpha.3

*发布日期: 2020-04-17*

EMQX 4.1-alpha.3 现已发布，主要包含以下改动：
  
### emqx-coap

**功能增强:**

- 支持 IPv6

  Github PR: [emqx/emqx-coap#167](https://github.com/emqx/emqx-coap/pull/167)
  
### emqx-auth-mnesia

**功能增强:**

- 增加基于 Mnesia 数据库的认证插件

  Github PR: [emqx/emqx-auth-mnesia#1](https://github.com/emqx/emqx-auth-mnesia/pull/1)
  
### emqx-sasl

**功能增强:**

- 支持 SCRAM-SHA-1 认证算法

  Github Repository: [emqx/emqx-sasl](https://github.com/emqx/emqx-sasl)

### emqx-management

**功能增强:**

- 支持返回所有的 Topic Metrics

  Github PR: [emqx/emqx-management#197](https://github.com/emqx/emqx-management/pull/197)
  
### emqx-lwm2m

**功能增强:**

- 支持 IPv6 和同时监听多个端口

  Github PR: [emqx/emqx-lwm2m#78](https://github.com/emqx/emqx-lwm2m/pull/78)

## 4.1-alpha.2

*发布日期: 2020-04-11*

EMQX 4.1-alpha.2 现已发布.

### emqx

**功能增强:**

- 支持使用代理协议时获取网络地址与端口

  Github PR: [emqx/emqx#3372](https://github.com/emqx/emqx/pull/3372)
  
- 支持 MQTT AUTH 报文（尚未支持认证算法）

  Github PR: [emqx/emqx#3374](https://github.com/emqx/emqx/pull/3374)

### emqx-management (plugin)

**功能增强:**

- 优化主题统计指标的 HTTP APIs

  Github PR: [emqx/emqx-management#189](https://github.com/emqx/emqx-management/pull/189)
  
- 支持跨版本数据迁移

  Github PR: [emqx/emqx-management#190](https://github.com/emqx/emqx-management/pull/190)
  
- 支持订阅的模糊搜索

  Github PR: [emqx/emqx-management#191](https://github.com/emqx/emqx-management/pull/191)

- 为内部模块增加 HTTP APIs 和 CLIs

  Github PR: [emqx/emqx-management#193](https://github.com/emqx/emqx-management/pull/193)

### emqx-rel (build project)

**错误修复:**

- 修复 `etc/emqx.conf` 为只读文件时 emqx 无法启动的问题

  Github issue: [emqx/emqx-rel#479](https://github.com/emqx/emqx-rel/issues/479)
  Github PR: [emqx/emqx-rel#480](https://github.com/emqx/emqx-rel/pull/480)

### emqx-dashboard (plugin)

**错误修复:**
  
- 修复无法删除用户的问题

  Github PR: [emqx/emqx-dashboard#219](https://github.com/emqx/emqx-dashboard/pull/219)

### emqx-extension-hook (plugin)

**功能增强:**

- 支持多语言扩展，目前仅支持 Python

  Github Repository: [emqx/emqx-extension-hook](https://github.com/emqx/emqx-extension-hook)

## 4.1-alpha.1

*发布日期: 2020-03-27*

EMQX 4.1-alpha.1 现已发布，主要包括以下改动:

### emqx

**功能增强:**

- 支持主题指标统计

  Github PR: [emqx/emqx#3341](https://github.com/emqx/emqx/pull/3341)
  
- 插件启动时加载最新配置

  Github PR: [emqx/emqx#3335](https://github.com/emqx/emqx/pull/3335)
  
- 支持消息转发时使用主题别名

  Github PR: [emqx/emqx#3344](https://github.com/emqx/emqx/pull/3344)
  
- 延迟发布功能现通过内部模块提供

  Github PR: [emqx/emqx#3323](https://github.com/emqx/emqx/pull/3323)
  
- 移除插件信息中的版本字段

  Github PR: [emqx/emqx#3335](https://github.com/emqx/emqx/pull/3335)
  
- 代理订阅支持设置所有订阅选项

  Github PR: [emqx/emqx#3307](https://github.com/emqx/emqx/pull/3307)
  
### emqx-management

**功能增强:**

- 为主题指标通知增加 HTTP APIs

  Github PR: [emqx/emqx-management#183](https://github.com/emqx/emqx-management/pull/183)
  
- 支持模糊查询和多条件查询

  Github PR: [emqx/emqx-management#182](https://github.com/emqx/emqx-management/pull/182)
  
### emqx-dashboard

**功能增强:**

- 支持通过 Dashboard 添加简单的认证信息

  Github PR: [emqx/emqx-dashboard#182](https://github.com/emqx/emqx-dashboard/pull/182)

## 4.0.5

*发布日期: 2020-03-17*

EMQX 4.0.5 现已发布。此版本主要进行了错误修复。

emqx
----

**错误修复:**

- 修复 GC 策略

  Github PR: [emqx/emqx#3317](https://github.com/emqx/emqx/pull/3317)
  
- 修复了 `Maximum-QoS` 属性的值设置错误的问题

  Github issue: [emqx/emqx#3304](https://github.com/emqx/emqx/issues/3304), [emqx/emqx#3315](https://github.com/emqx/emqx/issues/3315)
  Github PR: [emqx/emqx#3321](https://github.com/emqx/emqx/pull/3321)
  
- 修复了 EMQX 运行在 Docker 环境中时 CPU 占用率每隔 15 秒异常升高的问题

 Github issue: [emqx/emqx#3274](https://github.com/emqx/emqx/pull/3274)
  Github PR: [emqx/emqx-rel#462](https://github.com/emqx/emqx-rel/pull/462)
  
- 修复配置文件中 node.* 配置项不生效的问题

  Github issue: [emqx/emqx#3302](https://github.com/emqx/emqx/pull/3302)
  Github PR: [emqx/emqx-rel#463](https://github.com/emqx/emqx-rel/pull/463)

emqx-rule-engine (plugin)
------------------------

**错误修复:**

- 修复规则引擎不支持 Payload 为 utf-8 字符串的问题

  Github issue: [emqx/emqx#3287](https://github.com/emqx/emqx/issues/3287)
  Github PR: [emqx/emqx#3299](https://github.com/emqx/emqx/pull/3299)

emqx-sn (plugin)
----------------

**错误修复:**

- 修复 MQTT-SN 订阅丢失的问题

  Github issue: [emqx/emqx#3275](https://github.com/emqx/emqx/issues/3275)
  Github PR: [emqx/emqx-sn#156](https://github.com/emqx/emqx-sn/pull/156)


## 4.0.4

*发布日期: 2019-03-06*

EMQX 4.0.4 现已发布。此版本主要进行了错误修复。

### emqx

**错误修复:**

  - 修复 `acl_deny_action` 配置项不生效的问题
    
    Github issue:
    [emqx/emqx\#3266](https://github.com/emqx/emqx/issues/3266)
    
    Github PR: [emqx/emqx\#3286](https://github.com/emqx/emqx/pull/3286)

  - 修复 `mountpoint` 配置项的错误类型
    
    Github issue:
    [emqx/emqx\#3271](https://github.com/emqx/emqx/issues/3271)
    
    Github PR: [emqx/emqx\#3272](https://github.com/emqx/emqx/pull/3272)

  - 修复 `peer_cert_as_username` 配置项不生效的问题
    
    Github issue:
    [emqx/emqx\#3281](https://github.com/emqx/emqx/issues/3281)
    
    Github PR: [emqx/emqx\#3291](https://github.com/emqx/emqx/pull/3291)

  - 修复连接正常关闭后仍打印错误日志的问题
    
    Github PR: [emqx/emqx\#3290](https://github.com/emqx/emqx/pull/3290)

### emqx-dashboard (plugin)

**错误修复:**

  - 修复 Dashboard 节点下拉列表中显示空白的问题
    
    Github issue:
    [emqx/emqx\#3278](https://github.com/emqx/emqx/issues/3278)
    
    Github PR:
    [emqx/emqx-dashboard\#206](https://github.com/emqx/emqx-dashboard/pull/206)

### emqx-retainer (plugin)

**错误修复:**

  - 保留消息达到最大存储数量后的行为由无法存储任何保留消息更正为可以替换已存在主题的保留消息
    
    Github PR:
    [emqx/emqx-retainer\#136](https://github.com/emqx/emqx-retainer/pull/136)

## 4.0.3

*发布日期: 2019-02-21*

EMQX 4.0.3 现已发布。此版本主要进行了错误修复。

### emqx

**功能增强:**

  - 添加允许客户端绕过认证插件登录的选项
    
    Github PR: [emqx/emqx\#3253](https://github.com/emqx/emqx/pull/3253)

**错误修复:**

  - 修复某些竞争条件下会打印不必要的错误日志的问题
    
    Github PR: [emqx/emqx\#3246](https://github.com/emqx/emqx/pull/3253)

### emqx-management (plugin)

**错误修复:**

  - 移除不再使用的字段和函数以及修复字段值异常的问题
    
    Github PR:
    [emqx/emqx-management\#176](https://github.com/emqx/emqx-management/pull/176)

  - 修复集群环境下无法获取客户端列表的问题
    
    Github PR:
    [emqx/emqx-management\#173](https://github.com/emqx/emqx-management/pull/173)

  - 修复 HTTPS 监听选项
    
    Github PR:
    [emqx/emqx-management\#172](https://github.com/emqx/emqx-management/pull/172)

  - 修复应用列表的返回格式
    
    Github PR:
    [emqx/emqx-management\#169](https://github.com/emqx/emqx-management/pull/169)

## 4.0.2

*发布日期: 2019-02-07*

EMQX 4.0.2 现已发布。此版本主要进行了错误修复和性能优化。

### emqx

**功能增强:**

  - 提升 Json 编解码性能
    
    Github PR:
    [emqx/emqx\#3213](https://github.com/emqx/emqx/pull/3213),
    [emqx/emqx\#3230](https://github.com/emqx/emqx/pull/3230),
    [emqx/emqx\#3235](https://github.com/emqx/emqx/pull/3235)

  - 压缩生成的项目大小
    
    Github PR: [emqx/emqx\#3214](https://github.com/emqx/emqx/pull/3214)

**错误修复:**

  - 修复某些情况下没有发送 DISCONNECT 报文的问题
    
    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

  - 修复收到相同 PacketID 的 PUBLISH 报文时会断开连接的问题
    
    Github PR: [emqx/emqx\#3233](https://github.com/emqx/emqx/pull/3233)

### emqx-stomp (plugin)

**错误修复:**

  - 修复最大连接数限制不生效的问题
    
    Github PR:
    [emqx/emqx-stomp\#93](https://github.com/emqx/emqx-stomp/pull/93)

### emqx-auth-redis (plugin)

**错误修复:**

  - 修复内部模块启动失败的问题
    
    Github PR:
    [emqx/emqx-auth-redis\#151](https://github.com/emqx/emqx-auth-redis/pull/151)

### cowboy (dependency)

**错误修复:**

  - 修复 Websocket 连接某些情况下不会发送遗嘱消息的问题
    
    Github Commit:
    [emqx/cowboy\#3b6bda](https://github.com/emqx/cowboy/commit/3b6bdaf4f2e3c5b793a0c3cada2c3b74c3d5e885)

## 4.0.1

*发布日期: 2019-01-17*

EMQX 4.0.1 现已发布。此版本主要进行了错误修复和性能优化。

### emqx

**功能增强:**

  - force\_shutdown\_policy 默认关闭
    
    Github PR: [emqx/emqx\#3184](https://github.com/emqx/emqx/pull/3184)

  - 支持定时全局 GC 并提供配置项
    
    Github PR: [emqx/emqx\#3190](https://github.com/emqx/emqx/pull/3190)

  - 优化 `force_gc_policy` 的默认配置
    
    Github PR:
    [emqx/emqx\#3192](https://github.com/emqx/emqx/pull/3192),
    [emqx/emqx\#3201](https://github.com/emqx/emqx/pull/3201)

  - 优化 Erlang VM 参数配置
    
    Github PR:
    [emqx/emqx\#3195](https://github.com/emqx/emqx/pull/3195),
    [emqx/emqx\#3197](https://github.com/emqx/emqx/pull/3197)

**错误修复:**

  - 修复使用错误的单位导致黑名单功能异常的问题
    
    Github PR: [emqx/emqx\#3188](https://github.com/emqx/emqx/pull/3188)

  - 修复对 `Retain As Publish` 标志位的处理并且在桥接模式下保持 `Retain` 标识位的值
    
    Github PR: [emqx/emqx\#3189](https://github.com/emqx/emqx/pull/3189)

  - 修复无法使用多个 Websocket 监听端口的问题
    
    Github PR: [emqx/emqx\#3196](https://github.com/emqx/emqx/pull/3196)

  - 修复会话 takeover 时 EMQX 可能不发送 DISCONNECT 报文的问题
    
    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

### emqx-rule-engine

**功能增强:**

  - 提供更多操作数组的 SQL 函数
    
    Github PR:
    [emqx/emqx-rule-engine\#136](https://github.com/emqx/emqx-rule-engine/pull/136)

  - 减少未配置任何规则时对性能的影响
    
    Github PR:
    [emqx/emqx-rule-engine\#138](https://github.com/emqx/emqx-rule-engine/pull/138)

### emqx-web-hook

**错误修复:**

  - 修复参数不匹配导致的崩溃问题
    
    Github PR:
    [emqx/emqx-web-hook\#167](https://github.com/emqx/emqx-web-hook/pull/167)

## 4.0.0

*发布日期: 2019-01-10*

EMQX 4.0.0 正式版现已发布。在这个版本中，我们通过重构 channel 和 session
显著地改进了吞吐性能，通过添加更多的钩子和统计指标增强了可扩展性，重新设计了规则引擎的
SQL，并优化 Edge 版本的性能表现。

#### 常规

**功能增强:**

  - 架构优化，大幅提高消息吞吐性能，降低了 CPU 与内存占用
  - 改进 MQTT 5.0 报文处理流程
  - 规则引擎支持全新的 SQL 语句
  - 调整 metrics 命名并增加更多的 metrics
  - 调整钩子参数并增加更多的钩子
  - emqtt 提供发布与订阅的命令行接口

**错误修复:**

  - 修复了 SSL 握手失败导致崩溃的问题
  - 修复 `max_subscriptions` 配置不生效的问题
  - 修复跨集群转发消息失序的问题
  - 修复命令行接口无法获取单个主题的多条路由信息的问题

#### REST API

**功能增强:**

  - 支持 IPv6
  - REST API 默认监听端口由 8080 改为 8081，减少被其他应用占用的情况
  - 移除所有 sessions 相关的接口
  - connections 调整为 clients，并提供原先 sessions 的功能
  - 支持订阅查询接口返回共享订阅的真实主题
  - 支持配置默认的 AppID 与 AppSecret
  - 发布消息的 REST API 支持使用 base64 编码的 payload

**错误修复:**

  - 修复转码后的 URI 没有被正确处理的问题

#### 认证

**功能增强:**

  - HTTP 认证插件支持用户配置自定义的 HTTP 请求头部
  - clientid 与 username 认证插件重新支持用户通过配置文件配置默认的 clientid 与 username

## 4.0-rc.4

*发布日期: 2019-12-31*

EMQX 4.0-rc.4 版本现已发布，其中包括以下更改:

### emqx

**功能增强:**

  - 增加了更多的钩子
    
    Github PR: [emqx/emqx\#3138](https://github.com/emqx/emqx/pull/3138)

  - 增加了更多的 metrics
    
    Github PR:
    [emqx/emqx\#3139](https://github.com/emqx/emqx/pull/3139),
    [emqx/emqx\#3141](https://github.com/emqx/emqx/pull/3141)

**错误修复:**

  - 修复定时器超时消息可能匹配失败的问题
    
    Github PR: [emqx/emqx\#3145](https://github.com/emqx/emqx/pull/3145)

### emqx-bridge-mqtt

**错误修复:**

  - 修复 keepalive 配置项使用了错误的单位的问题
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#43](https://github.com/emqx/emqx-bridge-mqtt/pull/43)

### emqx-management

**功能增强:**

  - 支持配置默认的 AppID 与 AppSecret
    
    Github PR:
    [emqx/emqx-management\#153](https://github.com/emqx/emqx-management/pull/153)

  - 发布消息的 HTTP API 现以支持 base64 编码后的 payload
    
    Github PR:
    [emqx/emqx-management\#154](https://github.com/emqx/emqx-management/pull/154)

### emqx-auth-http

**功能增强:**

  - 支持用户自行配置 HTTP 请求头
    
    Github PR:
    [emqx/emqx-auth-http\#170](https://github.com/emqx/emqx-auth-http/pull/170)

## 4.0-rc.3

*发布日期: 2019-12-21*

EMQX 4.0-rc.3 版本现已发布，其中包括以下更改:

### emqx

**功能增强:**

  - 添加更多的 Metrics; 并删除 `channel.gc`, `messages.qos2.expired`,
    `messages.qos2.dropped`, `auth.mqtt.anonymous` 等
    
    Github PR: [emqx/emqx\#3128](https://github.com/emqx/emqx/pull/3128)

  - 日志格式支持配置行号
    
    Github PR: [emqx/emqx\#3117](https://github.com/emqx/emqx/pull/3117)

  - 为 emqx\_connection 增加更多的测试用例
    
    Github PR: [emqx/emqx\#3116](https://github.com/emqx/emqx/pull/3116)

  - 修复 MQTT/WS 消息乱序的 BUG
    
    Github PR: [emqx/emqx\#3115](https://github.com/emqx/emqx/pull/3115)

### emqx-dashboard (plugin)

**进行了以下更改:**

  - 优化 SQL 编辑器使用体验:
    
    Github PR:
    [emqx/emqx-dashboard\#176](https://github.com/emqx/emqx-dashboard/pull/176),
    [emqx/emqx-dashboard\#177](https://github.com/emqx/emqx-dashboard/pull/177)

  - 优化 Overview 页面显示
    
    Github PR:
    [emqx/emqx-dashboard\#179](https://github.com/emqx/emqx-dashboard/pull/179)

### emqx-management (plugin)

**进行了以下更改:**

  - 支持返回共享订阅的真实主题
    
    Github PR:
    [emqx/emqx-management\#151](https://github.com/emqx/emqx-management/pull/151)

**错误修复:**

  - 修复无法获取单个主题的多条路由信息的问题
    
    Github PR:
    [emqx/emqx-management\#150](https://github.com/emqx/emqx-management/pull/150)

### emqx-coap (plugin)

**错误修复:**

  - 修复停止插件后，无法正常启动的问题
    
    Github PR:
    [emqx/emqx-coap\#151](https://github.com/emqx/emqx-coap/pull/151)

### emqx-delayed-publish (plugin)

**进行了以下更改:**

  - 新增 `messages.delayed` Metrics 计数
    
    Github PR:
    [emqx/emqx-delayed-publish\#55](https://github.com/emqx/emqx-delayed-publish/pull/55)

### emqx-statsd (plugin)

**进行了以下更改:**

  - 对新的 Metrics 进行适配
    
    Github PR:
    [emqx/emqx-statsd\#43](https://github.com/emqx/emqx-statsd/pull/43)

### emqx-bridge-mqtt (plugin)

**进行了以下修复:**

  - 修正 Keepalive 单位为秒
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#43](https://github.com/emqx/emqx-bridge-mqtt/pull/43)

### emqx-auth-http (plugin)

**进行了以下更改:**

  - 支持 '%p' 占位符，以获取客户端所连接的端口
    
    Github PR:
    [emqx/emqx-auth-http\#167](https://github.com/emqx/emqx-auth-http/pull/167)

### All of Authentication Plugins

**进行了以下更改:**

  - 重命名认证成功或失败的计数 Metrics 的前缀为 `client.auth.`; 重命名 ACL 检查成功或失败的 Metrics
    前缀为 `client.acl.`
    
    Github PR:
    [emqx/emqx-auth-username\#132](https://github.com/emqx/emqx-auth-username/pull/132),
    [emqx/emqx-auth-clientid\#127](https://github.com/emqx/emqx-auth-clientid/pull/127),
    [emqx/emqx-auth-http\#168](https://github.com/emqx/emqx-auth-http/pull/168),
    [emqx/emqx-auth-jwt\#107](https://github.com/emqx/emqx-auth-jwt/pull/107),
    [emqx/emqx-auth-ldap\#96](https://github.com/emqx/emqx-auth-ldap/pull/96),
    [emqx/emqx-auth-mongo\#197](https://github.com/emqx/emqx-auth-mongo/pull/197),
    [emqx/emqx-auth-mysql\#193](https://github.com/emqx/emqx-auth-mysql/pull/193),
    [emqx/emqx-auth-pgsql\#174](https://github.com/emqx/emqx-auth-pgsql/pull/174),
    [emqx/emqx-auth-redis\#144](https://github.com/emqx/emqx-auth-redis/pull/144)

## 4.0-rc.2

*发布日期: 2019-12-16*

EMQX 4.0-rc.2 版本现已发布，其中包括以下更改:

### emqx

**功能增强:**

  - 为更多模块增加测试用例，提升原有测试用例的测试覆盖率
    
    Github PR:
    [emqx/emqx\#3091](https://github.com/emqx/emqx/pull/3091),
    [emqx/emqx\#3095](https://github.com/emqx/emqx/pull/3095),
    [emqx/emqx\#3096](https://github.com/emqx/emqx/pull/3096),
    [emqx/emqx\#3100](https://github.com/emqx/emqx/pull/3100),
    [emqx/emqx\#3106](https://github.com/emqx/emqx/pull/3106),
    [emqx/emqx\#3107](https://github.com/emqx/emqx/pull/3107)

  - Get the timestamp uniformly by `erlang:system_time`
    
    Github PR:
    [emqx/emqx\#3088](https://github.com/emqx/emqx/pull/3088),
    [emqx/emqx\#3089](https://github.com/emqx/emqx/pull/3089)

  - 移除 `sessions.persistent.count` 与 `sessions.persistent.max` 计数
    
    Github PR: [emqx/emqx\#3111](https://github.com/emqx/emqx/pull/3111)

  - WebSocket 支持会话机制
    
    Github PR:
    [emqx/emqx\#3106](https://github.com/emqx/emqx/pull/3106),
    [emqx/cowboy\#1](https://github.com/emqx/cowboy/pull/1),
    [emqx/cowboy\#3](https://github.com/emqx/cowboy/pull/3)

### emqx-retainer (plugin)

**错误修复:**

  - 存在大量保留消息时 EMQX 不能及时向客户端回复 SUBACK
    
    Github PR:
    [emqx/emqx-retainer\#126](https://github.com/emqx/emqx-retainer/pull/126)

### emqx-dashboard (plugin)

**进行了以下更改:**

  - 客户端列表增加 IP 字段，不需要进入详情才能查看
    
    Github PR:
    [emqx/emqx-dashboard\#172](https://github.com/emqx/emqx-dashboard/pull/172)

## 4.0-rc.1

*发布日期: 2019-12-07*

EMQX 4.0-rc.1 版本发布。此版本主要优化了内部模块和 MQTT 报文处理流程。

### emqx

功能增强:

  - 优化 MQTT 报文优化逻辑
    
    Github PR:
    [emqx/emqx\#3079](https://github.com/emqx/emqx/pull/3079),
    [emqx/emqx\#3082](https://github.com/emqx/emqx/pull/3082),
    [emqx/emqx\#3083](https://github.com/emqx/emqx/pull/3083)

### emqx-auth-username (plugin)

功能增强:

  - 重新支持通过配置文件配置默认的 `username`
    
    Github PR:
    [emqx/emqx-auth-username\#126](https://github.com/emqx/emqx-auth-username/pull/126)

### emqx-auth-clientid (plugin)

功能增强:

  - 重新支持通过配置文件配置默认的 `clientid`
    
    Github PR:
    [emqx/emqx-auth-clientid\#122](https://github.com/emqx/emqx-auth-clientid/pull/122)

### emqx-management (plugin)

功能增强:

  - HTTP API 服务器默认监听端口由 8080 改为 8081
    
    Github PR:
    [emqx/emqx-management\#144](https://github.com/emqx/emqx-management/pull/144)

## 4.0-beta.4

*发布日期: 2019-11-18*

EMQX 4.0-beta.4 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

功能增强:

  - 被检测到 flapping 的客户端会被 banned
    
    Github PR: [emqx/emqx\#3033](https://github.com/emqx/emqx/pull/3033)

  - 优化 emqx\_vm 模块并更新测试用例
    
    Github PR: [emqx/emqx\#3034](https://github.com/emqx/emqx/pull/3034)

### emqx-management (plugin)

功能增强:

  - 更新 banned API
    
    Github PR:
    [emqx/emqx-management\#141](https://github.com/emqx/emqx-management/pull/141)

错误修复:

  - 修复一些错误的返回值
    
    Github PR:
    [emqx/emqx-management\#142](https://github.com/emqx/emqx-management/pull/142)

### minirest (plugin)

错误修复:

  - 添加错误处理并增加日志
    
    Github PR:
    [emqx/minirest\#20](https://github.com/emqx/minirest/pull/20)

### esockd (dependency)

功能增强:

  - 调整部分接口并增加测试用例
    
    Github PR:
    [emqx/esockd\#124](https://github.com/emqx/esockd/pull/124)

### ekka (dependency)

功能增强:

  - 调整部分接口并增加测试用例
    
    Github PR: [emqx/ekka\#67](https://github.com/emqx/ekka/pull/67)

## 4.0-beta.3

*发布日期: 2019-11-01*

EMQX 4.0-beta.3 版本发布。此版本主要针对错误修复以及测试覆盖率提升。

错误修复:

  - 修复跨集群转发时消息失序的问题
    
    Github PR: [emqx/emqx\#3000](https://github.com/emqx/emqx/pull/3000)

### emqx-management (plugin)

功能增强:

  - REST API 支持 IPv6
    
    Github PR:
    [emqx/emqx-management\#135](https://github.com/emqx/emqx-management/pull/135)

错误修复:

  - 修复转码后的 URI 没有被正确处理的问题
    
    Github PR:
    [emqx/emqx-management\#137](https://github.com/emqx/emqx-management/pull/137)

### emqx-dashboard (plugin)

功能增强:

  - 支持使用 IPv6 访问 Dashbaord
    
    Github PR:
    [emqx/emqx-dashboard\#162](https://github.com/emqx/emqx-dashboard/pull/162)

### emqx-delayed-publish (plugin)

错误修复:

  - 修复插件在集群环境下只能在一个节点中开启的问题
    
    Github PR:
    [emqx/emqx-delay-publish\#50](https://github.com/emqx/emqx-delay-publish/pull/50)

  - 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献
    
    Github PR:
    [emqx/emqx-delay-publish\#49](https://github.com/emqx/emqx-delay-publish/pull/49)
    
    Github Issue:
    [emqx/emqx-delay-publish\#15](https://github.com/emqx/emqx-delay-publish/issues/15)

## 4.0-beta.2

*发布日期: 2019-10-12*

<div id="release_4.0-beta.1">

EMQX 4.0-beta.2 版本发布。此版本主要针对错误修复以及继续优化内部模块设计。

</div>

错误修复:

  - 修复 SSL 握手失败导致崩溃的问题
    
    Github PR: [emqx/emqx\#2963](https://github.com/emqx/emqx/pull/2963)

  - 检查 PUBLISH 报文的主题层级
    
    Github PR: [emqx/emqx\#2964](https://github.com/emqx/emqx/pull/2964)

### emqtt (plugin)

功能增强:

  - 提供命令行接口
    
    Github PR: [emqx/emqtt\#91](https://github.com/emqx/emqtt/pull/91)

### emqx-sn (plugin)

错误修复:

  - 适配 MQTT-SN 插件到 4.0 版本
    
    Github PR:
    [emqx/emqx-sn\#145](https://github.com/emqx/emqx-sn/pull/145)

### emqx-coap (plugin)

错误修复:

  - 适配 CoAP 插件到 4.0 版本
    
    Github Commit:
    [emqx/emqx-coap\#c7c175](https://github.com/emqx/emqx-coap/commit/c7c17540c1248dcdd402b41323c23a211e8292fc),
    [emqx/emqx-coap\#9b8ede](https://github.com/emqx/emqx-coap/commit/9b8ede093cfc3b7211663520e496c579c11611f6)

## 4.0-beta.1

*发布日期: 2019-09-30*

EMQX 4.0-beta.1 版本发布。此版本主要针对内部模块进行重新设计，实现吞吐大幅度提升。
