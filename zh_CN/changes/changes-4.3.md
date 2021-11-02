---
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
ref:
---

# 版本发布

## 4.3.9 版本

*发布日期: 2021-11-02*

EMQ X 4.3.9 现已发布，主要包含以下改动:

**错误修复（重要）:**

- 修复 WebHook TLS 不可用的问题

  Github PR: [emqx#5696](https://github.com/emqx/emqx/pull/5696)

- 修复 MongoDB 资源不支持域名的问题

  Github PR: [emqx#6035](https://github.com/emqx/emqx/pull/6035)

- 修复基于内置数据库的 ACL 的性能问题

  Github PR: [emqx#5885](https://github.com/emqx/emqx/pull/5885)

- 修复基于内置数据库的认证错误转码 HTTP 请求参数的问题

  Github PR: [emqx#5674](https://github.com/emqx/emqx/pull/5674)

- 修复集群环境下禁用规则时资源没有正确清理的问题

  Github PR: [emqx#5731](https://github.com/emqx/emqx/pull/5731)

- 修复使用不同优先级转发消息时低优先级队列可能无法得到处理的问题

  Github PR: [emqx#5666](https://github.com/emqx/emqx/pull/5666)

- 修复 Stomp 客户端无法在 Dashboard 页面展示的问题，并支持在页面中进行订阅和取消订阅操作

  Github PR: [emqx#6040](https://github.com/emqx/emqx/pull/6040)

- 修复 STOMP 客户端的 ID 在集群中可能不唯一的问题

  Github PR: [emqx#6040](https://github.com/emqx/emqx/pull/6040)

- 修复 STOMP 网关的认证问题

  Github PR: [emqx#6040](https://github.com/emqx/emqx/pull/6040)

**错误修复（次要）:**

- 修复包含 “\” 字符的 Client ID 无法进行模糊搜索的问题

  Github PR: [emqx#5978](https://github.com/emqx/emqx/pull/5978)

- 修复可变字节整数可能大于 4 字节的问题

  Github PR: [emqx#5826](https://github.com/emqx/emqx/pull/5826)

**功能增强:**

- 改进客户端踢除机制

  Github PR: [emqx#6030](https://github.com/emqx/emqx/pull/6030)

- 为 LwM2M 网关添加新加密套件的支持

  Github PR: [emqx#5970](https://github.com/emqx/emqx/pull/5970)

- 默认为 HTTP 认证插件关闭超级用户请求

  Github PR: [emqx#5567](https://github.com/emqx/emqx/pull/5567)

## 4.3.8 版本

*发布日期: 2021-09-06*

EMQ X 4.3.8 现已发布，主要包含以下改动:

**错误修复:**

- 修复规则引擎规则导入失败的问题

  Github PR: [emqx#5512](https://github.com/emqx/emqx/pull/5512)

- 修复规则引擎 Webhook 动作中 Path 字段无法使用的问题

  Github PR: [emqx#5468](https://github.com/emqx/emqx/pull/5468)

- 修复 Force Shutdown 机制在进程挂起时无法生效的问题

  Github PR: [emqx#5460](https://github.com/emqx/emqx/pull/5460)

- 修复某些情况下 k8s 部署 EMQ X 集群无法正确重启的问题

  Github PR: [emqx#5646](https://github.com/emqx/emqx/pull/5646), [emqx#5428](https://github.com/emqx/emqx/pull/5428)

- 修复 exproto 跨节点进程间调用的错误

  Github PR: [emqx#5436](https://github.com/emqx/emqx/pull/5436)

**功能增强:**

- 为 exhook 增加自动重连机制以及请求超时的相关配置项，增强可靠性

  Github PR: [emqx#5447](https://github.com/emqx/emqx/pull/5447)

- 为 exproto 增加断连重试机制

  Github PR: [emqx#5436](https://github.com/emqx/emqx/pull/5436)

> 注: 此版本开始 CentoOS 7 要求使用 openssl 1.1.1，openssl 升级安装办法见：[FAQ - OpenSSL 版本不正确](https://docs.emqx.cn/broker/v4.3/faq/error.html#openssl-%E7%89%88%E6%9C%AC%E4%B8%8D%E6%AD%A3%E7%A1%AE)

## 4.3.7 版本

*发布日期: 2021-08-09*

EMQ X 4.3.7 现已发布，主要包含以下改动:

**错误修复:**

- 修复当前 HTTP KeepAlive 行为可能导致某些服务器断开连接的问题

  Github PR: [emqx#5395](https://github.com/emqx/emqx/pull/5395)

- 修复命令行接口无法打印某些字符的问题

  Github PR: [emqx#5411](https://github.com/emqx/emqx/pull/5411)

- 修复 LwM2M 网关下发整型数字时编码错误的问题

  Github PR: [emqx#5425](https://github.com/emqx/emqx/pull/5425)

## 4.3.6 版本

*发布日期: 2021-07-28*

EMQ X 4.3.6 现已发布，主要包含以下改动:

**功能改进:**

- 支持关闭 HTTP Pipelining

  Github PR: [emqx#5279](https://github.com/emqx/emqx/pull/5279)

- ACL 支持 IP 地址列表

  Github PR: [emqx#5328](https://github.com/emqx/emqx/pull/5328)

## 4.3.5 版本

*发布日期: 2021-06-28*

EMQ X 4.3.5 现已发布，主要包含以下改动:

**错误修复:**

- 修复同一客户端建立多个共享订阅时可能在取消订阅后出现消息丢失的问题

  Github PR: [emqx#5098](https://github.com/emqx/emqx/pull/5098)

## 4.3.4 版本

*发布日期: 2021-06-23*

EMQ X 4.3.4 现已发布，主要包含以下改动:

**错误修复:**

- 修复 CoAP 网关无法解析某些 URI 的问题

  Github Issue: [emqx#5062](https://github.com/emqx/emqx/issues/5062)
  Github PR: [emqx#5059](https://github.com/emqx/emqx/pull/5059)

- 修复多语言扩展钩子可能启动失败的问题

  Github PR: [emqx#5004](https://github.com/emqx/emqx/pull/5004)

- 修复规则引擎删除资源时如果存在依赖该资源的规则会导致 Crash 的问题

  Github PR: [emqx#4996](https://github.com/emqx/emqx/pull/4996)

- 修复 HTTP 认证与 Webhook 不支持 Query String 的问题

  Github PR: [emqx#4981](https://github.com/emqx/emqx/pull/4981)

- 保证默认配置下节点间报文的转发顺序

  Github PR: [emqx#4979](https://github.com/emqx/emqx/pull/4979)

## 4.3.3 版本

*发布日期: 2021-06-05*

EMQ X 4.3.3 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 数据转储支持从 HTTP 请求中获取导入数据

  Github PR: [emqx#4900](https://github.com/emqx/emqx/pull/4900)

**错误修复:**

- 修复没有配置 JWKS 端点导致崩溃的问题

  Github PR: [emqx#4916](https://github.com/emqx/emqx/pull/4916)

- 修复 MQTT-SN 在集群环境下的订阅

  Github PR: [emqx#4915](https://github.com/emqx/emqx/pull/4915)

- 修复 Webhook 无法使用 TLS 的问题

  Github PR: [emqx#4908](https://github.com/emqx/emqx/pull/4908)

- 修复客户端多条件查询时参数错误可能导致崩溃的问题

  Github PR: [emqx#4916](https://github.com/emqx/emqx/pull/4916)

- 修复内存占用计算错误的问题

  Github PR: [emqx#4891](https://github.com/emqx/emqx/pull/4891)

## 4.3.2 版本

*发布日期: 2021-05-27*

EMQ X 4.3.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复主题指标监控无法在集群环境使用的问题

  Github PR: [emqx#4870](https://github.com/emqx/emqx/pull/4870)

- 修复报文解析时的一些问题

  Github PR: [emqx#4858](https://github.com/emqx/emqx/pull/4858)

- 修复 MQTT-SN 睡眠模式与 KeepAlive 机制的冲突问题

  Github PR: [emqx#4842](https://github.com/emqx/emqx/pull/4842)

- 修复客户端大量离线时可能出现崩溃的问题

  Github Issue: [emqx#4823](https://github.com/emqx/emqx/issues/4823)
  Github PR: [emqx#4824](https://github.com/emqx/emqx/pull/4824)

- 规则引擎刷新资源失败时将资源标记为不可用

  Github PR: [emqx#4821](https://github.com/emqx/emqx/pull/4821)

## 4.3.1 版本

*发布日期: 2021-05-14*

EMQ X 4.3.1 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复路由压缩之后性能消耗随主题层级数量指数级增长的问题

  Github PR: [emqx#4800](https://github.com/emqx/emqx/pull/4800)

- 修复处理大报文的性能问题

  Github Issue: [emqx#4787](https://github.com/emqx/emqx/issues/4787)
  Github PR: [emqx#4802](https://github.com/emqx/emqx/pull/4802)

- 修复新增的共享订阅策略不可用的问题

  Github Issue: [emqx#4808](https://github.com/emqx/emqx/issues/4808)
  Github PR: [emqx#4809](https://github.com/emqx/emqx/pull/4809)

- 修复了保留消息和延迟发布消息统计指标的错误实现

  Github PR: [emqx#4778](https://github.com/emqx/emqx/pull/4778), [emqx#4778](https://github.com/emqx/emqx/pull/4799)

- 确保 JSON 日志之间的换行

  Github PR: [emqx#4778](https://github.com/emqx/emqx/pull/4771)

## 4.3.0 版本

*发布日期: 2021-05-06*

EMQ X 4.3.0 现已发布，主要包含以下改动:

### 功能与改进

#### 构建

- 支持 Erlang/OTP 23
- 新安装包仅支持 macOS 10.14 及以上版本
- 项目调整为 umbrella 结构
- 支持使用 Elixir 编译插件

#### 性能改进

- 多语言扩展功能底层实现方式由 erlport 改为 gRPC
- 支持路由表压缩，减少内存占用，增强订阅性能，发布性能会略受影响，因此提供了关闭选项
- 优化通配符订阅性能
- 改进大量客户端离线时的处理性能

#### 安全性

- 保护 EMQ X Broker 免受跨站点 WebSocket 劫持攻击
- SSL 支持 `verify` 与 `server_name_indication` 配置项
- SSL 支持证书链最大长度以及私钥文件密码配置项
- JWT 认证支持 JWKS

#### 其他

- 规则引擎新增更新资源逻辑
- 规则引擎 SQL 函数支持 unix 时间戳与 rfc3339 格式时间之间的转换
- 保持对 EMQ X Broker 启动后连接失败的资源进行重试
- Websocket 监听器支持从 subprotocols 列表中选择支持的 subprotocol
- WebSocket 连接支持获取真实 IP 与 Port
- 支持 MySQL 8.0 的默认认证方法 caching_sha2_password
- 共享订阅分发策略配置为 `round_robin` 时随机选择起始点
- 共享订阅支持按源主题的 Hash 分发消息
- 支持 Mnesia 认证信息的导入导出
- 允许使用 Base64 编码的客户端证书或者客户端证书的 MD5 值作为用户名或者 Client ID
- 支持重启监听器
- 仅在正式版本中启用数据遥测功能
- 支持清除所有 ACL 缓存
- 支持 observer_cli
- Prometheus 支持集群指标
- Redis 哨兵模式支持 SSL 连接
- 支持单行日志输出，并支持 rfc3339 时间格式
- `emqx_auth_clientid` 与 `emqx_auth_usernmae` 合并为 `emqx_auth_mnesia`。请参考 [文档](https://docs.emqx.io/en/broker/v4.3/advanced/data-import-and-export.html) 将数据到旧版本导出，并导入到 4.3 中
- Docker 默认输出日志到控制台，设置 EMQX_LOG__TO=file 使日志输出到文件
- 支持输出 Json 格式的日志
- 支持 IPv6 自动探测
- 所有发行版都支持环境变量覆盖配置文件（以前仅适用于 Docker）
- 开源版支持 Dashboard 上传证书文件（以前仅适用于企业版）


### 错误修复

#### MQTT 协议

- 修复 MQTTT 心跳报文的处理
- 修复 MQTT 报文接收计数问题
- 限制飞行窗口的最大长度为 65535
- 修复 Server Keep Alive 生效情况下 Dashboard 中 Keep Alive 字段的值未同步的问题

#### 网关

- 修复 CoAP 连接中 ACL 配置不生效的问题
- 修复使用相同 ClientID 的 CoAP 客户端可以同时接入的问题
- 修复 MQTT-SN 睡眠模式不可用的问题
- 修复 MQTT-SN 网关在睡眠模式下会丢弃 DISCONNECT 报文的问题
- 修复 LwM2M 网关将数字编码、解码为无符号整型的问题

#### 资源

- 修复 MySQL 认证 SSL/TLS 连接功能不可用的问题
- 修复 Redis 重连失败问题

#### 其他修复

- 修复 ekka_locker 在极端条件下内存可能无限增长的问题
- 修复 MQTT 桥接功能中 `max_inflight_size` 配置项不生效的问题
- 修复 MQTT 桥接飞行窗口的问题
- 修复 MQTT 桥接功能中指标统计错误和 `retry_interval` 字段进行了多次单位转换的问题
- 修复告警持续时间计算错误的问题
- 修复过长的 Client ID 无法追踪的问题
- 修复查询客户端信息可能出现崩溃的问题
- 修复主题重写与 ACL 在发布订阅时执行顺序不一致的问题
- 修复 WebSocket 连接无法使用对端证书作为用户名的问题
- 修复认证数据无法导入的问题
- 修复 Docker 中 EMQ X 可能启动失败的问题
- OOM 时快速杀死连接进程
- 修复 Clean Session 为 false 的 MQTT-SN 连接在非正常断开时没有发布遗嘱消息的问题

## 4.3-rc.5 版本

*发布日期: 2021-04-26*

EMQ X 4.3-rc.5 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 优化通配符订阅性能

  Github Issue: [emqx#2985](https://github.com/emqx/emqx/issues/2985)
  Github PR: [emqx#4645](https://github.com/emqx/emqx/pull/4645)

- 支持单行日志输出，并支持 rfc3339 时间格式

  Github PR: [emqx#4656](https://github.com/emqx/emqx/pull/4656)

- 支持路由表压缩，减少内存占用，增强订阅性能，发布性能会略受影响，因此提供了关闭选项

  Github PR: [emqx#4628](https://github.com/emqx/emqx/pull/4628)

- 规则引擎 SQL 函数支持 unix 时间戳与 rfc3339 格式时间之间的转换

  Github PR: [emqx#4639](https://github.com/emqx/emqx/pull/4639)

**错误修复:**

- 修复 Docker 中 EMQ X 可能启动失败的问题

  Github PR: [emqx#4670](https://github.com/emqx/emqx/pull/4670), [emqx#4675](https://github.com/emqx/emqx/pull/4675), [emqx#4657](https://github.com/emqx/emqx/pull/4657)

- 规则引擎资源未初始化成功时将相应规则状态设为不可用

  Github Issue: [emqx#4642](https://github.com/emqx/emqx/issues/4642)
  Github PR: [emqx#4643](https://github.com/emqx/emqx/pull/4643)

- 修复在 EMQ X 未完全启动时上报遥测数据导致的问题

  Github PR: [emqx#4627](https://github.com/emqx/emqx/pull/4627)

- 修复启动 emqx-exhook 插件必须配置 HTTPS 证书的问题

  Github PR: [emqx#4678](https://github.com/emqx/emqx/pull/4678)

## 4.3-rc.4 版本

*发布日期: 2021-04-16*

EMQ X 4.3-rc.4 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- Redis 哨兵模式支持 SSL 连接

  Github PR: [emqx#4553](https://github.com/emqx/emqx/pull/4553)

- WebSocket 连接支持获取真实 IP 与 Port

  Github PR: [emqx#4558](https://github.com/emqx/emqx/pull/4558)

- Prometheus 支持集群指标

  Github Issue: [emqx#4548](https://github.com/emqx/emqx/pull/4548)
  Github PR: [emqx#4572](https://github.com/emqx/emqx/pull/4572)

**错误修复:**

- 修复 MQTT 桥接飞行窗口的问题

  Github Issue: [emqx#3629](https://github.com/emqx/emqx/issues/3629)
  Github PR: [emqx#4513](https://github.com/emqx/emqx/pull/4513), [emqx#4526](https://github.com/emqx/emqx/pull/4526)
  
- 修复多语言扩展钩子无法处理返回的 false 值的问题

  Github PR: [emqx#4542](https://github.com/emqx/emqx/pull/4542)

- 默认启动模块，避免集群后内置模块无法正常工作

  Github PR: [emqx#4547](https://github.com/emqx/emqx/pull/4547)

- 修复认证数据无法导入的问题

  Github PR: [emqx#4582](https://github.com/emqx/emqx/pull/4582), [emqx#4528](https://github.com/emqx/emqx/pull/4528)

- 修复 WebSocket 连接无法使用对端证书作为用户名的问题

  Github PR: [emqx#4563](https://github.com/emqx/emqx/pull/4563)

- 修复 MQTT-SN 网关在睡眠模式下会丢弃 DISCONNECT 报文的问题

  Github Issue: [emqx#4506](https://github.com/emqx/emqx/issues/4506)
  Github PR: [emqx#4515](https://github.com/emqx/emqx/pull/4515)

- 修复 LwM2M 网关将数字编码、解码为无符号整型的问题

  Github Issue: [emqx#4499](https://github.com/emqx/emqx/issues/4499)
  Github PR: [emqx#4500](https://github.com/emqx/emqx/pull/4500)

- 修复部分 HTTP API 不可用的问题

  Github Issue: [emqx#4472](https://github.com/emqx/emqx/issues/4472)
  Github PR: [emqx#4503](https://github.com/emqx/emqx/pull/4503)

## 4.3-rc.3 版本

*发布日期: 2021-03-30*

EMQ X 4.3-rc.3 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 限制飞行窗口的最大长度为 65535

  Github PR: [emqx#4436](https://github.com/emqx/emqx/pull/4436)

- 修复 Server Keep Alive 生效情况下 Dashboard 中 Keep Alive 字段的值未同步的问题

  Github PR: [emqx#4444](https://github.com/emqx/emqx/pull/4444)

- OOM 时快速杀死连接进程

  Github PR: [emqx#4451](https://github.com/emqx/emqx/pull/4451)

- 修复 `emqx start` 报超时但服务实际已启动的问题

  Github PR: [emqx#4449](https://github.com/emqx/emqx/pull/4449)

- 修复 MQTT-SN 睡眠模式不可用的问题

  Github PR: [emqx#4435](https://github.com/emqx/emqx/pull/4435)

## 4.3-rc.2 版本

*发布日期: 2021-03-26*

EMQ X 4.3-rc.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 emqx 和 emqx_ctl 命令在某些情况下不可用的问题

  Github PR: [emqx#4430](https://github.com/emqx/emqx/pull/4430)

## 4.3-rc.1 版本

*发布日期: 2021-03-23*

EMQ X 4.3-rc.1 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 支持 observer_cli

  Github PR: [emqx#4323](https://github.com/emqx/emqx/pull/4323)

- 支持清除所有 ACL 缓存

  Github PR: [emqx#4361](https://github.com/emqx/emqx/pull/4361)

- SSL 支持 `verify` 与 `server_name_indication` 配置项

  Github PR: [emqx#4349](https://github.com/emqx/emqx/pull/4349)

**错误修复:**

- 修复主题重写与 ACL 执行顺序导致的问题

  Github Issue: [emqx#4200](https://github.com/emqx/emqx/issues/4200)
  Github PR: [emqx#4331](https://github.com/emqx/emqx/pull/4331)

- 修复 MQTT 报文接收计数问题

  Github PR: [emqx#4371](https://github.com/emqx/emqx/pull/4371)

- 修复心跳报文的处理

  Github Issue: [emqx#4370](https://github.com/emqx/emqx/issues/4370)
  Github PR: [emqx#4371](https://github.com/emqx/emqx/pull/4371)

- 修复由于默认的 SSL Ciphers 中包含了 OTP 22 不支持的 Ciphers 导致使用 OTP 22 编译后启动失败的问题

  Github PR: [emqx#4377](https://github.com/emqx/emqx/pull/4377)

## 4.3-beta.1 版本

*发布日期: 2021-03-03*

EMQ X 4.3-beta.1 现已发布，主要包含以下改动:

### emqx

**功能增强:**

- 减少开启规则引擎插件时的性能损耗

  Github PR: [emqx#4160](https://github.com/emqx/emqx/pull/4160)

- 仅在正式版本中启用数据遥测功能

  Github PR: [emqx#4163](https://github.com/emqx/emqx/pull/4163)

- 支持重启监听器

  Github PR: [emqx#4188](https://github.com/emqx/emqx/pull/4188), [emqx#4190](https://github.com/emqx/emqx/pull/4190)

- 禁用规则的同时销毁动作占用的资源

  Github PR: [emqx#4232](https://github.com/emqx/emqx/pull/4232)

- 共享订阅分发策略配置为 `round_robin` 时随机选择起始点

  Github PR: [emqx#4232](https://github.com/emqx/emqx/pull/4232)

- 允许使用 Base64 编码的客户端证书或者客户端证书的 MD5 值作为用户名或者 Client ID

  Github PR: [emqx#4194](https://github.com/emqx/emqx/pull/4194)

- 保持对 EMQ X Broker 启动后连接失败的资源进行重试

  Github PR: [emqx#4125](https://github.com/emqx/emqx/pull/4125)

**错误修复:**

- 修复过长的 Client ID 无法追踪的问题

  Github PR: [emqx#4163](https://github.com/emqx/emqx/pull/4163)

- 修复查询客户端信息可能出现崩溃的问题

  Github PR: [emqx#4124](https://github.com/emqx/emqx/pull/4124)

## 4.3-alpha.1 版本

*发布日期: 2021-01-29*

EMQ X 4.3-alpha.1 现已发布，主要包含以下改动:

*功能*

- 支持 Erlang/OTP 23
- 新安装包仅支持 macOS 10.14 及以上版本
- 规则引擎新增更新资源逻辑
- 增强 Webhook 与 HTTP 认证性能
- 多语言扩展功能底层实现方式由 erlport 改为 gRPC
- 保护 EMQ X Broker 免受跨站点 WebSocket 劫持攻击
- 项目调整为 umbrella 结构
- 解决集群环境下节点必须按首次启动顺序启动，否则需要等待前置节点启动的问题
- Websocket 监听器支持从 subprotocols 列表中选择支持的 subprotocol
- 支持 MySQL 8.0 的默认认证方法 caching_sha2_password
- JWT 认证支持 JWKS
- 支持配置证书链最大长度以及私钥文件密码
- 支持 Mnesia 认证信息的导入导出
- 共享订阅支持按源主题的 Hash 分发消息

*BUG*

- 修复 ekka_locker 在极端条件下内存可能无限增长的问题
- 修复 MQTT 桥接功能中 `max_inflight_size` 配置项不生效的问题
- 修复 CoAP 连接中 ACL 配置不生效的问题
- 修复使用相同 ClientID 的 CoAP 客户端可以同时接入的问题
- 修复告警持续时间计算错误的问题
- 修复 MySQL 认证 SSL/TLS 连接功能不可用的问题
- 修复 MQTT 桥接功能中指标统计错误和 `retry_interval` 字段进行了多次单位转换的问题
- 修复 Redis 重连失败问题
