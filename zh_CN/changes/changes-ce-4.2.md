# v4.2

## 4.2.14

*发布日期: 2021-08-02*

EMQX 4.2.14 现已发布，主要包含以下改动:

**错误修复:**

- 修复热升级不可用的问题

## 4.2.13

*发布日期: 2021-06-28*

EMQX 4.2.13 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复同一客户端建立多个共享订阅时可能在取消订阅后出现消息丢失的问题

  GitHub PR: [emqx#5104](https://github.com/emqx/emqx/pull/5104)

### emqx-auth-http

**错误修复:**

- 修复请求超时后打印 crash 日志的问题

  GitHub PR: [emqx-auth-http#263](https://github.com/emqx/emqx-auth-http/pull/263)

- 支持查询字符串参数

  GitHub PR: [emqx-auth-http#264](https://github.com/emqx/emqx-auth-http/pull/264)

### emqx-web-hook

**错误修复:**

- 支持查询字符串参数

  GitHub PR: [emqx-web-hook#284](https://github.com/emqx/emqx-web-hook/pull/284)

## 4.2.12

*发布日期: 2021-05-07*

EMQX 4.2.12 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复了一个因等待 Mnesia 表超时导致 emqx 过早启动的问题

  GitHub PR: [emqx#4724](https://github.com/emqx/emqx/pull/4724)

**性能优化:**

- 优化了大批量客户端同时订阅和取消订阅的性能

  GitHub PR: [emqx#4732](https://github.com/emqx/emqx/pull/4732)
  GitHub PR: [emqx#4738](https://github.com/emqx/emqx/pull/4738)

## 4.2.11

*发布日期: 2021-04-16*

EMQX 4.2.11 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 WebSocket 连接无法使用对端证书作为用户名的问题

  Github PR: [emqx#4574](https://github.com/emqx/emqx/pull/4574)

### emqx-management

**错误修复:**

- 修复认证数据导出导入问题

  Github PR: [emqx-management#320](https://github.com/emqx/emqx-management/pull/320)

## 4.2.10

*发布日期: 2021-04-12*

EMQX 4.2.10 现已发布，主要包含以下改动:

### emqx-management

**错误修复:**

- 导出数据时, 对 `emqx_auth_clientid` 的密码进行 base64 编码

  Github PR: [emqx-management#314](https://github.com/emqx/emqx-management/pull/314)

- MQTT 桥接飞行窗口中的消息ID引用的错误

  Github PR: [emqx-bridge-mqtt#132](https://github.com/emqx/emqx-bridge-mqtt/pull/132)

## 4.2.9

*发布日期: 2021-03-24*

EMQX 4.2.9 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 MQTT 报文接收计数问题

  Github PR: [emqx#4425](https://github.com/emqx/emqx/pull/4425)

- 修复心跳报文的处理

  Github Issue: [emqx#4370](https://github.com/emqx/emqx/issues/4370)
  Github PR: [emqx#4425](https://github.com/emqx/emqx/pull/4425)

### emqx-auth-mnesia

**错误修复:**

- 修复了数据库存储问题与 CLI 问题

  Github PR: [emqx-auth-mnesia#54](https://github.com/emqx/emqx-auth-mnesia/pull/54)

- 修复 `password_hash` 配置项不生效的问题

  Github PR: [emqx-auth-mnesia#56](https://github.com/emqx/emqx-auth-mnesia/pull/56)

## 4.2.8

*发布日期: 2021-03-10*

EMQX 4.2.8 现已发布，主要修复了 MQTT 消息解析的问题。

## 4.2.7

*发布日期: 2021-01-29*

EMQX 4.2.7 现已发布，主要包含以下改动:

### emqx-auth-http

**错误修复:**

- 修复 HTTP 长连接在达到 Keepalive 超时时长或最大请求数时被断开导致请求丢失的情况

  Github PR: [emqx-auth-http#245](https://github.com/emqx/emqx-auth-http/pull/245)

### emqx-web-hook

**错误修复:**

- 修复 HTTP 长连接在达到 Keepalive 超时时长或最大请求数时被断开导致请求丢失的情况

  Github PR: [emqx-web-hook#272](https://github.com/emqx/emqx-web-hook/pull/272)

- 修复 SSL 证书配置问题

  Github PR: [emqx-web-hook#264](https://github.com/emqx/emqx-web-hook/pull/264)

### emqx-auth-redis

**错误修复:**

- 修复 Redis 重连失败的问题

  Github PR: [emqx-auth-redis#195](https://github.com/emqx/emqx-auth-redis/pull/195)

## 4.2.6

*发布日期: 2021-01-16*

EMQX 4.2.6 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 remsh 使用的端口错误问题

  Github PR: [emqx#4016](https://github.com/emqx/emqx/pull/4016)

### emqx-auth-http

**功能增强:**

- HTTP 请求头部中的 host 字段使用用户配置的原始 URL

  Github PR: [emqx-auth-http#240](https://github.com/emqx/emqx-auth-http/pull/240)

**错误修复:**

- 修复 GET 请求不可用的问题

  Github PR: [emqx-auth-http#238](https://github.com/emqx/emqx-auth-http/pull/238)

### emqx-web-hook

**功能增强:**

- HTTP 请求头部中的 host 字段使用用户配置的原始 URL

  Github PR: [emqx-web-hook#256](https://github.com/emqx/emqx-web-hook/pull/256)

- URL 中未携带端口时使用默认端口

  Github PR: [emqx-web-hook#253](https://github.com/emqx/emqx-web-hook/pull/253)

**错误修复:**

- 修复 SSL 配置项解析错误问题

  Github PR: [emqx-web-hook#252](https://github.com/emqx/emqx-web-hook/pull/252)

- 修复 GET 请求不可用的问题

  Github PR: [emqx-web-hook#254](https://github.com/emqx/emqx-web-hook/pull/254)

### emqx-management

**错误修复:**

- 告警增加持续时间字段，修复前端可能由于时间不一致导致计算出错的问题

  Github PR: [emqx-management#304](https://github.com/emqx/emqx-management/pull/304)

### emqx-dashboard

**功能增强:**

- 改进告警持续时间显示

  Github PR: [emqx-dashboard#271](https://github.com/emqx/emqx-dashboard/pull/271)

### ehttpc

**错误修复:**

- 修复 gen_server:call/3 超时导致回复被送至调用进程的消息队列的问题

  Github PR: [ehttpc#2](https://github.com/emqx/ehttpc/pull/2)

## 4.2.5

*发布日期: 2020-12-23*

EMQX 4.2.5 现已发布，主要包含以下改动:

### emqx-auth-http

- 修复 HTTP 请求头部中错误的字段名

  Github PR: [emqx-auth-http#229](https://github.com/emqx/emqx-auth-http/pull/229)

### emqx-web-hook

- 更新底层 HTTP 客户端驱动，解决驱动卡死导致的连接进程失去响应的问题

  Github PR: [emqx-web-hook#240](https://github.com/emqx/emqx-web-hook/pull/240)

## 4.2.4

*发布日期: 2020-12-11*

EMQX 4.2.4 现已发布，主要包含以下改动:

### emqx

- 支持配置 SSL/TLS 证书链长度和密钥文件密码

  Github PR: [emqx#3901](https://github.com/emqx/emqx/pull/3901)

### emqx-auth-http

- 更新底层 HTTP 客户端驱动，解决驱动卡死导致的连接进程失去响应的问题

  Github PR: [emqx-auth-http#213](https://github.com/emqx/emqx-auth-http/pull/213)

### emqx-auth-mongo

- 修复没有匹配查询结果导致的类型错误问题

  Github PR: [emqx-auth-mongo#240](https://github.com/emqx/emqx-auth-mongo/pull/240)

### emqx-auth-redis

- 修复 redis 驱动在 cluster 模式下启动失败时未报错的问题

  Github PR: [emqx-auth-redis#187](https://github.com/emqx/emqx-auth-redis/pull/187)

### emqx-rel

- Helm chart 支持通过密钥拉取私有镜像

  Github PR: [emqx-rel#626](https://github.com/emqx/emqx-rel/pull/626)

- 增强安全性

  Github PR: [emqx-rel#612](https://github.com/emqx/emqx-rel/pull/612)

## 4.2.3

*发布日期: 2020-11-13*

EMQX 4.2.3 现已发布，主要包含以下改动:

### emqx-web-hook

- 支持在请求的 URL 路径中插入变量

  Github PR: [emqx-web-hook#225](https://github.com/emqx/emqx-web-hook/pull/225)
  Github Issue: [emqx-web-hook#224](https://github.com/emqx/emqx-web-hook/issues/224)

- 支持设置 Content Type

  Github PR: [emqx-web-hook#230](https://github.com/emqx/emqx-web-hook/pull/230)

### emqx-rel

- 修复 LC_ALL 不等于 en_US.UTF-8 时编译失败的问题

  Github PR: [emqx-rel#605](https://github.com/emqx/emqx-rel/pull/605)
  Github Issue: [emqx-rel#604](https://github.com/emqx/emqx-rel/issues/604)

- 支持在 emqx 容器启动前创建和运行其他服务

  Github PR: [emqx-rel#608](https://github.com/emqx/emqx-rel/pull/608)

- 允许为容器设置可变数量的配置项

  Github PR: [emqx-rel#609](https://github.com/emqx/emqx-rel/pull/609)

### emqx-sn

- 修复尝试发布遗嘱消息将导致崩溃的问题

  Github PR: [emqx-sn#169](https://github.com/emqx/emqx-sn/pull/169)

- 修复为 from 字段设置了不恰当的值的问题

  Github PR: [emqx-sn#170](https://github.com/emqx/emqx-sn/pull/170)

### emqx-rule-engine

- 保持向后兼容

  Github PR: [emqx-rule-engine#189](https://github.com/emqx/emqx-rule-engine/pull/189)

- 修复 `messages.received` 指标的统计

  Github PR: [emqx-rule-engine#193](https://github.com/emqx/emqx-rule-engine/pull/193)

### emqx-management

- 修复 `messages.received` 指标的统计

  Github PR: [emqx-management#284](https://github.com/emqx/emqx-management/pull/284)

- 使数据导入导出功能能够在集群环境下使用

  Github PR: [emqx-management#288](https://github.com/emqx/emqx-management/pull/288)

### emqx-redis

- 支持 Redis 6.0 的 SSL/TLS 单双向认证

  Github PR: [emqx-auth-redis#180](https://github.com/emqx/emqx-auth-redis/pull/180)

## 4.2.2

*发布日期: 2020-10-24*

EMQX 4.2.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复主题统计速率计算不准确的问题

  Github PR: [emqx#3784](https://github.com/emqx/emqx/pull/3784)

### emqx-web-hook

**功能增强:**

- 支持 `GET` 和 `DELETE` 方法

  Github PR: [emqx-web-hook#220](https://github.com/emqx/emqx-web-hook/pull/220)

- 增加 `node` 和 `disconnected_at` 字段

  Github PR: [emqx-web-hook#215](https://github.com/emqx/emqx-web-hook/pull/215)

### emqx-auth-pgsql

**错误修复:**

- 修复 `%a` 占位符不生效的问题

  Github PR: [emqx-auth-pgsql#208](https://github.com/emqx/emqx-auth-pgsql/pull/208)

### emqx-auth-mysql

**错误修复:**

- 修复 `%a` 占位符不生效的问题

  Github PR: [emqx-auth-mysql#245](https://github.com/emqx/emqx-auth-mysql/pull/245)

## 4.2.1

*发布日期: 2020-09-29*

EMQX 4.2.1 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复没有正确处理 No Local 逻辑导致消息堆积在飞行窗口和消息队列的问题

  Github PR: [emqx#3741](https://github.com/emqx/emqx/pull/3741)
  Github Issue: [emqx#3738](https://github.com/emqx/emqx/issues/3738)

### emqx-bridge-mqtt

**错误修复:**

- 修复规则引擎 MQTT 订阅无法接收消息的问题

  Github PR: [emqx-bridge-mqtt#108](https://github.com/emqx/emqx-bridge-mqtt/pull/108)

### emqx-dashboard

**错误修复:**

- 修复超长 Client ID 的显示错误问题

  Github PR: [emqx-dashboard#262](https://github.com/emqx/emqx-dashboard/pull/262)

### emqx-auth-mongo

**错误修复:**

- 修复 ACL 查询语句有多个匹配结果时仅处理了第一个匹配的问题

  Github PR: [emqx-auth-mongo#231](https://github.com/emqx/emqx-auth-mongo/pull/231)

## 4.2.0

*发布日期: 2020-09-05*

EMQX 4.2.0 现已发布，主要包含以下改动:

**功能:**

- 支持使用第三方语言编写扩展插件接入其他非 MQTT 协议，目前已支持 Java 和 Python 两种编程语言。访问 [Read Me](https://github.com/emqx/emqx-exproto/blob/master/README.md) 获取更多相关信息
- 支持修订版本间的热升级
- 新增遥测功能，收集有关 EMQX Broker 使用情况的信息以帮助我们改进产品，此功能默认开启，支持手动关闭。访问 [EMQX Telemetry](https://docs.emqx.io/broker/latest/en/advanced/telemetry.html) 获取更多遥测相关信息。
- 支持配额形式的消息流控

**增强:**

- 规则引擎支持为 MQTT 桥接创建订阅
- 规则引擎支持功能更加强大的 SQL 语法
- MySQL、PostgreSQL 等插件全面支持 IPv6、SSL/TLS
- 支持 CentOS 8、Ubuntu 20.04 操作系统和 ARM64 系统架构
- Webhook 支持配置自定义的 HTTP 头部
- 更加友好的告警机制，为开发者提供 HTTP API
- 优化保留消息性能

**调整:**

- 后续版本不再支持 Debian 8、Ubuntu 14.04 和 Raspbian 8 操作系统
- `emqx-statsd` 插件正式更名为 `emqx-prometheus`
- 发布与订阅支持独立配置主题重写规则
- 允许用户配置是否允许 WebSocket 消息包含多个 MQTT 报文，以兼容部分客户端
- 调整 RPC 端口发现策略
- ***不兼容改动：*** `emqx-auth-mnesia` 插件提供的 API 端口调整为 `api/v4/mqtt_user` 与 `api/v4/mqtt_acl`
- `emqx-auth-http` 插件默认关闭超级用户认证请求
- `emqx-bridge-mqtt` 默认关闭桥接模式

**错误修复:**

- 修复主题指标功能导致内存异常增长的问题
- 修复 LwM2M 插件没有正确获取协议版本的问题
- 修复一台机器上运行多个 emqx 实例时命令行接口无法使用的问题
- 修复 Websocket 连接不支持 IPv6 的问题

## 4.2-rc.2

*发布日期: 2020-08-28*

EMQX 4.2-rc.2 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 调整 RPC 端口发现策略

  Github PR: [emqx#3696](https://github.com/emqx/emqx/pull/3696)

### emqx-rel

**错误修复:**

- 修复一台机器上运行多个 emqx 实例时命令行接口无法使用的问题

  Github PR: [emqx-rel#583](https://github.com/emqx/emqx-rel/pull/583)

### emqx-management

**功能增强:**

- 提供 Log Handler 的启动停止命令

  Github PR: [emqx-management#259](https://github.com/emqx/emqx-management/pull/259)

### emqx-telemetry

**错误修复:**

- 修复遥测功能在集群下无法使用的问题

  Github PR: [emqx-telemetry#3](https://github.com/emqx/emqx-telemetry/pull/3)

### emqx-auth-mnesia

**功能增强:**

- *不兼容改动：* API 端口调整为 `api/v4/mqtt_user` 与 `api/v4/mqtt_acl`

  Github PR: [emqx-auth-mnesia#31](https://github.com/emqx/emqx-auth-mnesia/pull/31)


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
