# v4.1

## 4.1.4

*发布日期: 2020-08-28*

EMQX 4.1.4 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复主题指标功能导致内存异常增长的问题

  Github PR: [emqx#3680](https://github.com/emqx/emqx/pull/3680)

### emqx-bridge-mqtt

**功能增强:**

- clientid 配置项支持 `${node}` 占位符，优化集群下的使用体验

  Github PR: [emqx-bridge-mqtt#99](https://github.com/emqx/emqx-bridge-mqtt/pull/99)

### emqx-management

**错误修复:**

- 修复数据迁移功能在 Windows 下不可用的问题

  Github PR: [emqx-management#262](https://github.com/emqx/emqx-management/pull/262)

### emqx-lua-hook

**错误修复:**

- 修复无法获取 Username 字段的问题

  Github PR: [emqx-lua-hook#115](https://github.com/emqx/emqx-lua-hook/pull/115)

## 4.2-rc.1

*发布日期: 2020-08-22*

EMQX 4.2-rc.1 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 允许一个 WebSocket 消息中包含多个 MQTT 报文以提供效率，也可以配置为仅可包含一个 MQTT 报文以兼容部分客户端

  Github Issue: [emqx#3324](https://github.com/emqx/emqx/issues/3324)

  Github PR: [emqx#3673](https://github.com/emqx/emqx/pull/3673)

- 允许独立配置发布、订阅的主题重写规则

  Github PR: [emqx#3676](https://github.com/emqx/emqx/pull/3676)

**错误修复:**

- 修复主题指标功能导致内存异常增长的问题

  Github PR: [emqx#3679](https://github.com/emqx/emqx/pull/3679)

### emqx-rel

**错误修复:**

- 修复热升级后丢失旧配置的问题

  Github PR: [emqx-rel#578](https://github.com/emqx/emqx-rel/pull/578)

### emqx-bridge-mqtt

**功能增强:**

- 为 clientid 配置项提供 `${node}` 占位符

  Github PR: [emqx-bridge-mqtt#100](https://github.com/emqx/emqx-bridge-mqtt/pull/100)

### emqx-telemetry

**功能增强:**

- 遥测功能正式上线，此功能被用于帮助我们改进产品且默认开启，此功能不会收集任何个人身份信息，用户可以通过开放的 API 查询我们上报的数据

  Github PR: [emqx-telemetry#1](https://github.com/emqx/emqx-telemetry/pull/1)

### emqx-exproto

**错误修复:**

- 修复一些问题

  Github PR: [emqx-exproto#11](https://github.com/emqx/emqx-exproto/pull/11)

### emqx-management

**错误修复:**

- 修复 RPM/Deb 包无法导出数据的问题

  Github PR: [emqx-management#257](https://github.com/emqx/emqx-management/pull/257)

## 4.2-beta.1

*发布日期: 2020-08-14*

EMQX 4.2-beta.1 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 支持使用配额限制转发的消息数量

  Github PR: [emqx#3656](https://github.com/emqx/emqx/pull/3656)

- 支持数据遥测功能并默认关闭

  Github PR: [emqx#3653](https://github.com/emqx/emqx/pull/3653)

**错误修复:**

- 修复 Websocket 不支持 IPv6 的问题

  Github PR: [emqx#3654](https://github.com/emqx/emqx/pull/3654)

### emqx-rel

**功能增强:**

- 支持修订版本间的热升级

  Github PR: [emqx-rel#571](https://github.com/emqx/emqx-rel/pull/571)

### emqx-rule-engine

**功能增强:**

- 支持全新的 SQL 语法

  Github PR: [emqx-rule-engine#168](https://github.com/emqx/emqx-rule-engine/pull/168)

- 支持用户设置规则 ID 与资源 ID

  Github PR: [emqx-rule-engine#169](https://github.com/emqx/emqx-rule-engine/pull/169)

### emqx-exproto

**错误修复:**

- 修复了一些问题

  Github PR: [emqx-exproto#9](https://github.com/emqx/emqx-exproto/pull/9)

### emqx-web-hook

**功能增强:**

- 支持 HTTPS

  Github PR: [emqx-web-hook#209](https://github.com/emqx/emqx-web-hook/pull/209)

### emqx-auth-clientid

**错误修复:**

- 修复查找的认证信息不存在引发的错误

  Github PR: [emqx-auth-clientid#145](https://github.com/emqx/emqx-auth-clientid/pull/145)

### emqx-auth-http

**错误修复:**

- 默认关闭超级用户认证请求

  Github PR: [emqx-auth-http#195](https://github.com/emqx/emqx-auth-http/pull/195)

### emqx-bridge-mqtt

**功能增强:**

- 默认关闭桥接模式

  Github PR: [emqx-bridge-mqtt#95](https://github.com/emqx/emqx-bridge-mqtt/pull/95)

### emqx-management

**功能增强:**

- 为遥测功能添加 HTTP API 与 CLI，提供遥测数据查询接口

  Github PR: [emqx-management#253](https://github.com/emqx/emqx-management/pull/253)

## 4.1.3

*发布日期: 2020-08-04*

EMQX 4.1.3 现已发布，主要包含以下改动:

### emqx-management

**错误修复:**

- 为 PUBLISH API 的 payload 字段增加类型检查

  Github PR: [emqx/emqx-management#250](https://github.com/emqx/emqx-management/pull/250)

### emqx-retainer

**错误修复:**

- 修复订阅主题同时包含 '+' 和 '#' 不会下发保留消息的问题

  Github PR: [emqx/emqx-retainer#146](https://github.com/emqx/emqx-retainer/pull/146)

## 4.2-alpha.3

*发布日期: 2020-07-31*

EMQX 4.2-alpha.3 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 支持全局速率限制

  Github PR: [emqx/emqx#3613](https://github.com/emqx/emqx/pull/3613)

- 重新设计的告警

  Github PR: [emqx/emqx#3632](https://github.com/emqx/emqx/pull/3632)

**错误修复:**

- 修复没有使用主题别名替代主题的问题

  Github PR: [emqx/emqx#3617](https://github.com/emqx/emqx/pull/3617)

### emqx-auth-ldap

**功能增强:**

- 支持 IPv6

  Github PR: [emqx/emqx-auth-ldap#114](https://github.com/emqx/emqx-auth-ldap/pull/114)

### emqx-retainer

**错误修复:**

- 修复订阅主题同时包含 '+' 和 '#' 不会下发保留消息的问题

  Github PR: [emqx/emqx-retainer#147](https://github.com/emqx/emqx-retainer/pull/147)

### emqx-management

**功能增强:**

- 重新设计的告警 API

  Github PR: [emqx/emqx-management#244](https://github.com/emqx/emqx-management/pull/244)

### emqx-dashboard

**功能增强:**

- 增加告警页面

  Github PR: [emqx/emqx-dashboard#245](https://github.com/emqx/emqx-dashboard/pull/245)

### emqx-extension-hook

**功能增强:**

- 不再支持 Python 2

  Github PR: [emqx/emqx-extension-hook#11](https://github.com/emqx/emqx-extension-hook/pull/11)

### emqx-exproto

**功能增强:**

- 支持 Java 驱动

  Github PR: [emqx/emqx-exproto#5](https://github.com/emqx/emqx-exproto/pull/5)

## 4.1.2

*发布日期: 2020-07-23*

EMQX 4.1.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复没有使用主题别名替代主题的问题

  Github PR: [emqx/emqx#3616](https://github.com/emqx/emqx/pull/3616)

- 修复某些操作占用过多 CPU 的问题

  Github PR: [emqx/emqx#3581](https://github.com/emqx/emqx/pull/3581)

### emqx-rel

**错误修复:**

- 修复以容器方式运行 emqx 时日志写满所有日志文件后控制台不再输出日志的问题

  Github PR: [emqx/emqx-rel#559](https://github.com/emqx/emqx-rel/pull/559)

## 4.2-alpha.2

*发布日期: 2020-07-17*

EMQX 4.2-alpha.2 现已发布，主要包含以下改动:

### emqx-statsd

**功能增强:**

- 更名为 `emqx-prometheus`

  Github Repository: [emqx/emqx-prometheus](https://github.com/emqx/emqx-prometheus)

### emqx-bridge-mqtt

**功能增强:**

- 支持在规则引擎中创建订阅资源

  Github PR: [emqx/emqx-bridge-mqtt#78](https://github.com/emqx/emqx-bridge-mqtt/pull/78)

### emqx-lwm2m

**错误修复:**

- 修复没有正确获取版本号的问题

  Github PR: [emqx/emqx-lwm2m#82](https://github.com/emqx/emqx-lwm2m/pull/82)

### emqx-retainer

**功能增强:**

- 增强性能

  Github PR: [emqx/emqx-retainer#140](https://github.com/emqx/emqx-retainer/pull/140)

### emqx-lua-hook

**错误修复:**

- 修复没有正确卸载 Lua 脚本和命令行接口的问题

  Github PR: [emqx/emqx-lua-hook#105](https://github.com/emqx/emqx-lua-hook/pull/105)

### emqx-web-hook

**功能增强:**

- 支持配置自定义的 HTTP 请求头部

  Github PR: [emqx/emqx-web-hook#200](https://github.com/emqx/emqx-web-hook/pull/200)

### emqx-auth-mysql

**功能增强:**

- 支持 IPv6

  Github PR: [emqx/emqx-auth-mysql#228](https://github.com/emqx/emqx-auth-mysql/pull/228)

### emqx-exproto

**功能增强:**

- 支持使用多种编程语言开发任何定制协议

  Github Repository: [emqx/emqx-exproto](https://github.com/emqx/emqx-exproto)

## 4.1.1

*发布日期: 2020-07-03*

EMQX 4.1.1 现已发布，主要包含以下改动:

### emqx-retainer

**错误修复:**

- 修复性能问题

  Github PR: [emqx/emqx-retainer#141](https://github.com/emqx/emqx-retainer/pull/141)

### emqx-bridge-mqtt

**错误修复:**

- 将挂载点改为可选配置

  Github PR: [emqx/emqx-bridge-mqtt#84](https://github.com/emqx/emqx-bridge-mqtt/pull/84)

### emqx-rel

**错误修复:**

- 屏蔽 docker 运行时输出的控制台日志中的敏感信息

  Github Issue: [emqx/emqx-rel#524](https://github.com/emqx/emqx-rel/pull/524)

  Github PR: [emqx/emqx-rel#542](https://github.com/emqx/emqx-rel/pull/542)

  Thanks: [emqx/emqx-rel#525](https://github.com/emqx/emqx-rel/pull/525) - [daadu](https://github.com/daadu)

### emqx-lua-hook

**错误修复:**

- 修复插件卸载时没有卸载脚本和 CLI 的问题

  Github PR: [emqx/emqx-lua-hook#106](https://github.com/emqx/emqx-lua-hook/pull/106)

## 4.2-alpha.1

*发布日期: 2020-06-20*

EMQX 4.2-alpha.2 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 支持 `Response Information`

  Github PR: [emqx/emqx#3533](https://github.com/emqx/emqx/pull/3533)

**错误修复:**

- 修复连接属性为空时导致连接进程崩溃的问题

  Github PR: [emqx/emqx#3525](https://github.com/emqx/emqx/pull/3525)

### emqx-rule-engine

**功能增强:**

- 为规则引擎事件增加 MQTT 属性字段以及与规则相关的元数据

  Github PR: [emqx/emqx-rule-engine#163](https://github.com/emqx/emqx-rule-engine/pull/163)

### emqx-rel

**功能增强:**

- 支持 CentOS 8

  Github PR: [emqx/emqx-rel#526](https://github.com/emqx/emqx-rel/pull/526)

- 支持 Ubuntu 20.04

  Github PR: [emqx/emqx-rel#521](https://github.com/emqx/emqx-rel/pull/521)

### esockd

**错误修复:**

- 修复 `max_conn_rate` 配置的错误类型

  Github PR: [emqx/esockd#161](https://github.com/emqx/esockd/pull/130)

### gen_coap

**功能增强:**

- 使用 esockd 替换 gen_udp

  Github PR: [emqx/gen_coap#12](https://github.com/emqx/gen_coap/pull/12)

### gen_rpc

**错误修复:**

- 修复 acceptor 在某些情况下发生崩溃的问题

  Github PR: [emqx/gen_rpc#9](https://github.com/emqx/gen_rpc/pull/9)

## 4.1.0

*发布日期: 2020-06-04*

EMQX 4.1.0 现已发布，主要包含以下改动：

**功能增强:**

  - 支持多语言扩展并提供 SDK，已支持语言：Python, Java
  - 支持基于主题的指标统计
  - 支持插件启动时加载最新配置
  - 支持消息转发时使用主题别名
  - 代理订阅支持配置所有订阅选项
  - 支持客户端列表的模糊查询和多条件查询
  - 支持订阅列表的模糊查询
  - 支持通过 Dashboard 添加简单的认证信息
  - 支持跨版本数据迁移
  - 支持 MQTT AUTH 报文，目前仅支持 SCRAM-SHA-1 认证机制，支持用户自行扩展
  - 支持使用代理协议时获取网络地址与端口
  - 增加基于 Mnesia 数据库的认证插件（在后续版本中完全替代 `emqx-auth-clientid` 与 `emqx-auth-username` 插件）
  - 支持编辑规则引擎中的规则
  - 通过 Docker 运行 EMQX 时支持注释配置项
  - LwM2M 网关插件支持 IPv6 和同时监听多个端口
  - CoAP 网关插件支持 IPv6
  - JWT 认证插件支持配置 jwerl 签名格式

**错误修复:**

  - 修复 `etc/emqx.conf` 为只读文件时 EMQX 无法启动的问题
  - 修复连接进程在某些情况下出错崩溃的问题
  - 修复浏览器不支持当前 SSL/TLS 证书的问题
  - 修复 MQTT 桥接插件默认情况下不会发送心跳包的问题
  - 修复异常登录检测功能没有删除过期数据导致内存增长的问题
  - 修复内置 ACL 模块重新加载时没有清除 ACL 缓存的问题
  - 修复 WebHook 插件中 `client.disconnected` 事件在某些情况下出错的问题
  - 修复 MQTT-SN 网关插件不支持指定监听 IP 地址的问题并支持 IPv6

## 4.1-rc.2

*发布日期: 2020-05-23*

EMQX 4.1-rc.2 现已发布，主要包含以下改动：

### emqx

**错误修复:**

- 修复客户端在发送 CONNECT 报文前发送其他报文导致崩溃的问题

  Github PR: [emqx/emqx#3476](https://github.com/emqx/emqx/pull/3476)

### emqx-auth-mnesia

**功能增强:**

- 支持全局 ACL 规则

  Github PR: [emqx/emqx-auth-mnesia#13](https://github.com/emqx/emqx-auth-mnesia/pull/13)

### emqx-rule-engine

**错误修复:**

- 修复资源不可用时无法导入规则的问题，以及修复导入规则后某些情况下无法启用的问题

  Github Commit: [emqx-rule-engine#582de5](https://github.com/emqx/emqx-rule-engine/commit/582de5363229ce513d02919bb41c9289a1e3729f)

### emqx-rel

**功能增强:**

- 通过 Docker 运行 EMQX 时支持注释配置项

  Github PR: [emqx/emqx-rel#508](https://github.com/emqx/emqx-rel/pull/508)

### emqx-extension-java-sdk

**功能增强:**

- 为多语言扩展增加 Java SDK

  Github Repository: [emqx/emqx-extension-java-sdk](https://github.com/emqx/emqx-extension-java-sdk)

### emqx-extension-python-sdk

**功能增强:**

- 为多语言扩展增加 Python SDK

  Github Repository: [emqx/emqx-extension-python-sdk](https://github.com/emqx/emqx-extension-python-sdk)

## 4.1-rc.1

*发布日期: 2020-05-15*

EMQX 4.1-rc.1 现已发布，主要包含以下改动：

### emqx

**错误修复:**

- 修复浏览器不支持当前 SSL/TLS 证书的问题

  Github PR: [emqx/emqx#3447](https://github.com/emqx/emqx/pull/3447)

- 修复连接进程在某些情况下出错崩溃的问题

  Github PR: [emqx/emqx#3459](https://github.com/emqx/emqx/pull/3459)

### emqx-auth-mnesia

**错误修复:**

- 修复配置的用户无法通过认证的问题

  Github PR: [emqx/emqx-auth-mnesia#6](https://github.com/emqx/emqx-auth-mnesia/pull/6)

- 修复没有正确处理错误的问题

  Github PR: [emqx/emqx-auth-mnesia#9](https://github.com/emqx/emqx-auth-mnesia/pull/9)

### emqx-reloader

**错误修复:**

- 修复某些情况下不会重新加载模块代码的问题

  Github PR: [emqx/emqx-reloader#73](https://github.com/emqx/emqx-reloader/pull/73)

### emqx-sn

**错误修复:**

- 修复不支持指定监听 IP 地址的问题并支持 IPv6

  Github PR: [emqx/emqx-sn#158](https://github.com/emqx/emqx-sn/pull/158)

### emqx-web-hook

**错误修复:**

- 修复 `client.disconnected` 事件在某些情况下出错的问题

  Github PR: [emqx/emqx-web-hook#188](https://github.com/emqx/emqx-web-hook/pull/188)

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
