# 版本发布 (Changes)

## 4.0.0 版本

* 发布日期: 2019-01-10*

EMQ X 4.0.0 正式版现已发布。在这个版本中，我们通过重构 channel 和 session
显著地改进了吞吐性能，通过添加更多的钩子和统计指标增强了可扩展性，重新设计了规则引擎的
SQL，并优化 Edge 版本的性能表现。

### 常规

** 进行了以下修改:**

  - 架构优化，大幅提高消息吞吐性能，降低了 CPU 与内存占用
  - 改进 MQTT 5.0 报文处理流程
  - 规则引擎支持全新的 SQL 语句
  - 调整 metrics 命名并增加更多的 metrics
  - 调整钩子参数并增加更多的钩子
  - emqtt 提供发布与订阅的命令行接口

** 解决了以下问题:**

  - 修复了 SSL 握手失败导致崩溃的问题
  - 修复 `max_subscriptions` 配置不生效的问题
  - 修复跨集群转发消息失序的问题
  - 修复命令行接口无法获取单个主题的多条路由信息的问题

### REST API

** 进行了以下修改:**

  - 支持 IPv6
  - REST API 默认监听端口由 8080 改为 8081，减少被其他应用占用的情况
  - 移除所有 sessions 相关的接口
  - connections 调整为 clients，并提供原先 sessions 的功能
  - 支持订阅查询接口返回共享订阅的真实主题
  - 支持配置默认的 AppID 与 AppSecret
  - 发布消息的 REST API 支持使用 base64 编码的 payload

** 解决了以下问题:**

  - 修复转码后的 URI 没有被正确处理的问题

### 认证

** 进行了以下修改:**

  - HTTP 认证插件支持用户配置自定义的 HTTP 请求头部
  - clientid 与 username 认证插件重新支持用户通过配置文件配置默认的 clientid 与 username

## 4.0-rc.4 版本

* 发布日期: 2019-12-31*

EMQ X 4.0-rc.4 版本现已发布，其中包括以下更改:

#### emqx

** 进行了以下修改:**

  - 增加了更多的钩子
    
    Github PR: [emqx/emqx\#3138](https://github.com/emqx/emqx/pull/3138)

  - 增加了更多的 metrics
    
    Github PR:
    [emqx/emqx\#3139](https://github.com/emqx/emqx/pull/3139),
    [emqx/emqx\#3141](https://github.com/emqx/emqx/pull/3141)

** 修复了以下问题:**

  - 修复定时器超时消息可能匹配失败的问题
    
    Github PR: [emqx/emqx\#3145](https://github.com/emqx/emqx/pull/3145)

#### emqx-bridge-mqtt

** 修复了以下问题:**

  - 修复 keepalive 配置项使用了错误的单位的问题
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#43](https://github.com/emqx/emqx-bridge-mqtt/pull/43)

#### emqx-management

** 进行了以下修改:**

  - 支持配置默认的 AppID 与 AppSecret
    
    Github PR:
    [emqx/emqx-management\#153](https://github.com/emqx/emqx-management/pull/153)

  - 发布消息的 HTTP API 现以支持 base64 编码后的 payload
    
    Github PR:
    [emqx/emqx-management\#154](https://github.com/emqx/emqx-management/pull/154)

#### emqx-auth-http

** 进行了以下修改:**

  - 支持用户自行配置 HTTP 请求头
    
    Github PR:
    [emqx/emqx-auth-http\#170](https://github.com/emqx/emqx-auth-http/pull/170)

## 4.0-rc.3 版本

* 发布日期: 2019-12-21*

EMQ X 4.0-rc.3 版本现已发布，其中包括以下更改:

#### emqx

** 进行了以下修改:**

  - 添加更多的 Metrics; 并删除 `channel.gc`, `messages.qos2.expired`,
    `messages.qos2.dropped`, `auth.mqtt.anonymous` 等
    
    Github PR: [emqx/emqx\#3128](https://github.com/emqx/emqx/pull/3128)

  - 日志格式支持配置行号
    
    Github PR: [emqx/emqx\#3117](https://github.com/emqx/emqx/pull/3117)

  - 为 emqx\_connection 增加更多的测试用例
    
    Github PR: [emqx/emqx\#3116](https://github.com/emqx/emqx/pull/3116)

  - 修复 MQTT/WS 消息乱序的 BUG
    
    Github PR: [emqx/emqx\#3115](https://github.com/emqx/emqx/pull/3115)

#### emqx-dashboard (plugin)

** 进行了以下更改:**

  - 优化 SQL 编辑器使用体验:
    
    Github PR:
    [emqx/emqx-dashboard\#176](https://github.com/emqx/emqx-dashboard/pull/176),
    [emqx/emqx-dashboard\#177](https://github.com/emqx/emqx-dashboard/pull/177)

  - 优化 Overview 页面显示
    
    Github PR:
    [emqx/emqx-dashboard\#179](https://github.com/emqx/emqx-dashboard/pull/179)

#### emqx-management (plugin)

** 进行了以下更改:**

  - 支持返回共享订阅的真实主题
    
    Github PR:
    [emqx/emqx-management\#151](https://github.com/emqx/emqx-management/pull/151)

** 修复了以下问题:**

  - 修复无法获取单个主题的多条路由信息的问题
    
    Github PR:
    [emqx/emqx-management\#150](https://github.com/emqx/emqx-management/pull/150)

#### emqx-coap (plugin)

** 修复了以下问题:**

  - 修复停止插件后，无法正常启动的问题
    
    Github PR:
    [emqx/emqx-coap\#151](https://github.com/emqx/emqx-coap/pull/151)

#### emqx-delayed-publish (plugin)

** 进行了以下更改:**

  - 新增 `messages.delayed` Metrics 计数
    
    Github PR:
    [emqx/emqx-delayed-publish\#55](https://github.com/emqx/emqx-delayed-publish/pull/55)

#### emqx-statsd (plugin)

** 进行了以下更改:**

  - 对新的 Metrics 进行适配
    
    Github PR:
    [emqx/emqx-statsd\#43](https://github.com/emqx/emqx-statsd/pull/43)

#### emqx-bridge-mqtt (plugin)

** 进行了以下修复:**

  - 修正 Keepalive 单位为秒
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#43](https://github.com/emqx/emqx-bridge-mqtt/pull/43)

#### emqx-auth-http (plugin)

** 进行了以下更改:**

  - 支持 '% p' 占位符，以获取客户端所连接的端口
    
    Github PR:
    [emqx/emqx-auth-http\#167](https://github.com/emqx/emqx-auth-http/pull/167)

#### All of Authentication Plugins

** 进行了以下更改:**

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

## 4.0-rc.2 版本

* 发布日期: 2019-12-16*

EMQ X 4.0-rc.2 版本现已发布，其中包括以下更改:

#### emqx

** 进行了以下修改:**

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

#### emqx-retainer (plugin)

** 解决了以下问题:**

  - 存在大量保留消息时 EMQ X 不能及时向客户端回复 SUBACK
    
    Github PR:
    [emqx/emqx-retainer\#126](https://github.com/emqx/emqx-retainer/pull/126)

#### emqx-dashboard (plugin)

** 进行了以下更改:**

  - 客户端列表增加 IP 字段，不需要进入详情才能查看
    
    Github PR:
    [emqx/emqx-dashboard\#172](https://github.com/emqx/emqx-dashboard/pull/172)

## 4.0-rc.1 版本

* 发布日期: 2019-12-07*

EMQ X 4.0-rc.1 版本发布。此版本主要优化了内部模块和 MQTT 报文处理流程。

#### emqx

功能增强:

  - 优化 MQTT 报文优化逻辑
    
    Github PR:
    [emqx/emqx\#3079](https://github.com/emqx/emqx/pull/3079),
    [emqx/emqx\#3082](https://github.com/emqx/emqx/pull/3082),
    [emqx/emqx\#3083](https://github.com/emqx/emqx/pull/3083)

#### emqx-auth-username (plugin)

功能增强:

  - 重新支持通过配置文件配置默认的 `username`
    
    Github PR:
    [emqx/emqx-auth-username\#126](https://github.com/emqx/emqx-auth-username/pull/126)

#### emqx-auth-clientid (plugin)

功能增强:

  - 重新支持通过配置文件配置默认的 `clientid`
    
    Github PR:
    [emqx/emqx-auth-clientid\#122](https://github.com/emqx/emqx-auth-clientid/pull/122)

#### emqx-management (plugin)

功能增强:

  - HTTP API 服务器默认监听端口由 8080 改为 8081
    
    Github PR:
    [emqx/emqx-management\#144](https://github.com/emqx/emqx-management/pull/144)

## 3.2.7 版本

* 发布日期: 2019-12-03*

EMQ X 3.2.7 版本发布。此版本主要重新支持了通过配置文件配置默认的 `username` 和 `clientid`。

#### emqx-auth-username (plugin)

功能增强:

  - 重新支持了通过配置文件配置默认的 `username`
    
    Github PR:
    [emqx/emqx-auth-username\#127](https://github.com/emqx/emqx-auth-username/pull/127)

#### emqx-auth-clientid (plugin)

功能增强:

  - 重新支持了通过配置文件配置默认的 `clientid`
    
    Github PR:
    [emqx/emqx-auth-clientid\#123](https://github.com/emqx/emqx-auth-clientid/pull/123)

## 3.2.6 版本

* 发布日期: 2019-11-23*

EMQ X 3.2.6 版本发布。此版本主要关注功能改进和错误修复。

#### emqx (major)

错误修复:

  - 修复通过 `gen_rpc` 向远程节点转发消息时可能失序的问题
    
    Github PR: [emqx/emqx\#3049](https://github.com/emqx/emqx/pull/3049)

  - 修复认证插件崩溃会导致 `emqx` 崩溃的问题
    
    Github PR: [emqx/emqx\#3048](https://github.com/emqx/emqx/pull/3048)

## 4.0-beta.4 版本

* 发布日期: 2019-11-18*

EMQ X 4.0-beta.4 版本发布。此版本主要关注功能改进和错误修复。

#### emqx (major)

功能增强:

  - 被检测到 flapping 的客户端会被 banned
    
    Github PR: [emqx/emqx\#3033](https://github.com/emqx/emqx/pull/3033)

  - 优化 emqx\_vm 模块并更新测试用例
    
    Github PR: [emqx/emqx\#3034](https://github.com/emqx/emqx/pull/3034)

#### emqx-management (plugin)

功能增强:

  - 更新 banned API
    
    Github PR:
    [emqx/emqx-management\#141](https://github.com/emqx/emqx-management/pull/141)

错误修复:

  - 修复一些错误的返回值
    
    Github PR:
    [emqx/emqx-management\#142](https://github.com/emqx/emqx-management/pull/142)

#### minirest (plugin)

错误修复:

  - 添加错误处理并增加日志
    
    Github PR:
    [emqx/minirest\#20](https://github.com/emqx/minirest/pull/20)

#### esockd (dependency)

功能增强:

  - 调整部分接口并增加测试用例
    
    Github PR:
    [emqx/esockd\#124](https://github.com/emqx/esockd/pull/124)

#### ekka (dependency)

功能增强:

  - 调整部分接口并增加测试用例
    
    Github PR: [emqx/ekka\#67](https://github.com/emqx/ekka/pull/67)

## 3.2.5 版本

* 发布日期: 2019-11-15*

EMQ X 3.2.5 版本发布。此版本主要进行了错误修复。

#### emqx-rule-engine (plugin)

错误修复:

  - 支持 SQL 关键字: FOREACH/DO/INCASE
    
    Github Commit:
    [emqx/emqx-rule-engine\#a962e3](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

  - 支持 SQL 关键字: CASE/WHEN
    
    Github Commit:
    [emqx/emqx-rule-engine\#40e68e](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

  - 支持在 SQL 的 WHERE 子句中比较原子与二进制
    
    Github Commit:
    [emqx/emqx-rule-engine\#b240cc](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

  - 修复 select 和 foreach 中的列验证失败
    
    Github Commit:
    [emqx/emqx-rule-engine\#6a1267](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

  - 修复重建规则时出现竞争的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#af8967](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

  - 修复重发消息时没有确证设置标志的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#60e45c](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)

#### minirest (plugin)

错误修复:

  - 修复日志没有记录错误数据的问题
    
    Github PR:
    [emqx/minirest\#20](https://github.com/emqx/minirest/pull/20)

#### emqx-web-hook (plugin)

错误修复:

  - 修复错误的匹配
    
    Github Commit:
    [emqx/emqx-web-hook\#3dd041](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)

## 4.0-beta.3 版本

* 发布日期: 2019-11-01*

EMQ X 4.0-beta.3 版本发布。此版本主要针对错误修复以及测试覆盖率提升。

错误修复:

  - 修复跨集群转发时消息失序的问题
    
    Github PR: [emqx/emqx\#3000](https://github.com/emqx/emqx/pull/3000)

#### emqx-management (plugin)

功能增强:

  - REST API 支持 IPv6
    
    Github PR:
    [emqx/emqx-management\#135](https://github.com/emqx/emqx-management/pull/135)

错误修复:

  - 修复转码后的 URI 没有被正确处理的问题
    
    Github PR:
    [emqx/emqx-management\#137](https://github.com/emqx/emqx-management/pull/137)

#### emqx-dashboard (plugin)

功能增强:

  - 支持使用 IPv6 访问 Dashbaord
    
    Github PR:
    [emqx/emqx-dashboard\#162](https://github.com/emqx/emqx-dashboard/pull/162)

#### emqx-delayed-publish (plugin)

错误修复:

  - 修复插件在集群环境下只能在一个节点中开启的问题
    
    Github PR:
    [emqx/emqx-delay-publish\#50](https://github.com/emqx/emqx-delay-publish/pull/50)

  - 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献
    
    Github PR:
    [emqx/emqx-delay-publish\#49](https://github.com/emqx/emqx-delay-publish/pull/49)
    
    Github Issue:
    [emqx/emqx-delay-publish\#15](https://github.com/emqx/emqx-delay-publish/issues/15)

## 3.2.4 版本

* 发布日期: 2019-10-28*

EMQ X 3.2.4 版本发布。此版本主要为 Dashbaord 和 REST API 添加了 IPv6 支持，并修复了一些错误。

错误修复:

  - 修复 max\_subscriptions 配置不生效的问题
    
    Github PR: [emqx/emqx\#2922](https://github.com/emqx/emqx/pull/2922)
    
    Github Issue:
    [emqx/emqx\#2908](https://github.com/emqx/emqx/issues/2908)

#### emqx-auth-mysql (plugin)

错误修复:

  - 使用占位符时更安全地取值
    
    Github PR:
    [emqx/emqx-auth-mysql\#180](https://github.com/emqx/emqx-auth-mysql/pull/180)
    
    Github Issue:
    [emqx/emqx\#2937](https://github.com/emqx/emqx/issues/2937)

#### emqx-dashboard (plugin)

功能增强:

  - 支持使用 IPv6 访问 Dashbaord
    
    Github PR:
    [emqx/emqx-dashboard\#161](https://github.com/emqx/emqx-dashboard/pull/161)

#### emqx-management (plugin)

功能增强:

  - REST API 支持 IPv6
    
    Github PR:
    [emqx/emqx-management\#134](https://github.com/emqx/emqx-management/pull/134)

#### emqx-delay-publish (plugin)

错误修复:

  - 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献
    
    Github PR:
    [emqx/emqx-delay-publish\#48](https://github.com/emqx/emqx-delay-publish/pull/48)
    
    Github Issue:
    [emqx/emqx-delay-publish\#15](https://github.com/emqx/emqx-delay-publish/issues/15)

#### emqx-rule-engine (plugin)

功能增强:

  - 优化规则引擎中 JSON Payload 解析语句
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

## 4.0-beta.2 版本

* 发布日期: 2019-10-12*

<div id="release_4.0-beta.1">

EMQ X 4.0-beta.2 版本发布。此版本主要针对错误修复以及继续优化内部模块设计。

</div>

错误修复:

  - 修复 SSL 握手失败导致崩溃的问题
    
    Github PR: [emqx/emqx\#2963](https://github.com/emqx/emqx/pull/2963)

  - 检查 PUBLISH 报文的主题层级
    
    Github PR: [emqx/emqx\#2964](https://github.com/emqx/emqx/pull/2964)

#### emqtt (plugin)

功能增强:

  - 提供命令行接口
    
    Github PR: [emqx/emqtt\#91](https://github.com/emqx/emqtt/pull/91)

#### emqx-sn (plugin)

错误修复:

  - 适配 MQTT-SN 插件到 4.0 版本
    
    Github PR:
    [emqx/emqx-sn\#145](https://github.com/emqx/emqx-sn/pull/145)

#### emqx-coap (plugin)

错误修复:

  - 适配 CoAP 插件到 4.0 版本
    
    Github Commit:
    [emqx/emqx-coap\#c7c175](https://github.com/emqx/emqx-coap/commit/c7c17540c1248dcdd402b41323c23a211e8292fc),
    [emqx/emqx-coap\#9b8ede](https://github.com/emqx/emqx-coap/commit/9b8ede093cfc3b7211663520e496c579c11611f6)

## 4.0-beta.1 版本

* 发布日期: 2019-09-30*

EMQ X 4.0-beta.1 版本发布。此版本主要针对内部模块进行重新设计，实现吞吐大幅度提升。

## 3.2.3 版本

* 发布日期: 2019-09-16*

EMQ X 3.2.3 版本改动主要为错误修复。

错误修复:

  - 修复 emqx 容器运行时 CPU 占用率告警异常的问题
    
    GitHub Commit:
    [emqx/emqx\#9cdaa7](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

  - 修复消息过期机制不生效的问题
    
    Github Commit:
    [emqx/emqx\#31671f](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

  - 修复占位符在 mountpoint 中不生效的问题
    
    Github Commit:
    [emqx/emqx\#58ba22](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)

#### emqx-dashboard (plugin)

错误修复:

  - 修复 SSL 无法使用的问题
    
    Github Commit:
    [emqx/emqx-dashboard\#272a42](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)

## 3.2.2 版本

* 发布日期: 2019-08-03*

EMQ X 3.2.2 版本改动主要为错误修复。

功能增强:

  - 扩展 `gen_rpc` 配置
    
    Github PR: [emqx/emqx\#2732](https://github.com/emqx/emqx/pull/2732)

#### emqx-rule-engine (plugin)

错误修复:

  - 修复测试 URL 连通性的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#88](https://github.com/emqx/emqx-rule-engine/pull/88)

#### emqx-dashboard (plugin)

功能增强:

  - 增加帮助页面

#### ekka (dependency)

错误修复:

  - 修复释放锁可能导致崩溃的问题
    
    Github PR: [emqx/ekka\#60](https://github.com/emqx/ekka/pull/60)

## 3.2.1 版本

* 发布日期: 2019-07-20*

EMQ X 3.2.1 版本改动主要包括错误修复与性能增强。

功能增强:

  - 优化 `gen_rpc` 的调用
    
    Github PR: [emqx/emqx\#2694](https://github.com/emqx/emqx/pull/2694)

  - 支持使用 hostname 自动发现 k8s 集群
    
    Github PR: [emqx/emqx\#2699](https://github.com/emqx/emqx/pull/2699)

  - 将默认 uptime 心跳时间改为 30s
    
    Github PR: [emqx/emqx\#2696](https://github.com/emqx/emqx/pull/2696)

错误修复:

  - 修复 WebSocket 非正常下线时出现 crash 的问题
    
    Github PR: [emqx/emqx\#2697](https://github.com/emqx/emqx/pull/2697)

  - 修复 Session 异常关闭时，ws\_channel 仍然在线的问题
    
    Github PR: [emqx/emqx\#2704](https://github.com/emqx/emqx/pull/2704)

#### emqx-rule-engine (plugin)

功能增强:

  - 增强 republish 动作参数
    
    Github PR:
    [emqx/emqx-rule-engine\#81](https://github.com/emqx/emqx-rule-engine/pull/81)

错误修复:

  - 修复使用 '.' 筛选 payload 字段失败的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#83](https://github.com/emqx/emqx-rule-engine/pull/83)

#### emqx-dashboard (plugin)

错误修复:

  - 修复 Dashboard 资源列表在 Safari 下渲染错误的问题
    
    Github PR:
    [emqx/emqx-dashboard\#124](https://github.com/emqx/emqx-dashboard/pull/124),
    [emqx/emqx-dashboard\#125](https://github.com/emqx/emqx-dashboard/pull/125),
    [emqx/emqx-dashboard\#126](https://github.com/emqx/emqx-dashboard/pull/126)

#### emqx-lwm2m (plugin)

功能增强:

  - 兼容 LwM2M 1.1 版本客户端登录
    
    Github Commit:
    [emqx/emqx-lwm2m\#1c03bf](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)

#### emqx-rel (build project)

功能增强:

  - 内置 rebar3 脚本
    
    Github PR:
    [emqx/emqx-rel\#394](https://github.com/emqx/emqx-rel/pull/394)

  - EMQ X Windows 服务延迟启动
    
    Github PR:
    [emqx/emqx-rel\#395](https://github.com/emqx/emqx-rel/pull/395)

## 3.2.0 版本

* 发布日期: 2019-07-12*

EMQ X 3.2.0 版本主要优化和改进了规则引擎。

#### 规则引擎

改进规则引擎功能和规则管理界面 (Dashboard)，支持更多动作。

#### 项目构建

改用 rebar3 构建项目。

#### MQTT Broker 桥接

将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件，并提升了 RPC bridge 的性能。

#### HTTP 插件

支持 HTTPs。

#### 集群 (ekka)

改善集群稳定性。

#### 其他插件和依赖

修复 Windows 服务注册问题。

## 3.2-rc.3 版本

* 发布日期: 2019-07-06*

EMQ X 3.2-rc.3 版本改动主要包括功能增强与错误修复。

错误修复:

  - 修复 [emqx/emqx:
    issue\#2635](https://github.com/emqx/emqx/issues/2635)
    
    Github PR: [emqx/emqx\#2663](https://github.com/emqx/emqx/pull/2663)

#### emqx-web-hook (plugin)

错误修复:

  - 修复 `actions.failure` 无计数的问题
    
    Github PR:
    [emqx/emqx-web-hook\#137](https://github.com/emqx/emqx-web-hook/pull/137)

#### emqx-bridge-mqtt (plugin)

功能增强:

  - 增加桥接模式选项
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#6](https://github.com/emqx/emqx-bridge-mqtt/pull/6)

  - 优化 RPC 消息的应答机制

  - 支持规则引擎下的 MQTT/RPC Bridge 缓存消息到本地磁盘队列

  - 修复规则引擎下的 RPC Bridge 无法桥接远程 EMQ X 节点的问题
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#7](https://github.com/emqx/emqx-bridge-mqtt/pull/7)

#### emqx-rule-engine (plugin)

功能增强:

  - Rule 与 Resource 的 API 支持集群
    
    Github PR:
    [emqx/emqx-rule-engine\#75](https://github.com/emqx/emqx-rule-engine/pull/75)

  - 新增返回触发事件的可用字段的 API
    
    Github PR:
    [emqx/emqx-rule-engine\#74](https://github.com/emqx/emqx-rule-engine/pull/74),
    [emqx/emqx-rule-engine\#77](https://github.com/emqx/emqx-rule-engine/pull/77)

错误修复:

  - 修复获取资源状态超时引起的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#76](https://github.com/emqx/emqx-rule-engine/pull/76)

#### emqx-dashboard (plugin)

功能增强:

  - 规则引擎各项指标细分到节点
    
    Github PR:
    [emqx/emqx-dashboard\#114](https://github.com/emqx/emqx-dashboard/pull/114)

错误修复:

  - 修复资源创建的 BUG
    
    Github PR:
    [emqx/emqx-dashboard\#114](https://github.com/emqx/emqx-dashboard/pull/114)

## 3.2-rc.2 版本

* 发布日期: 2019-06-29*

EMQ X 3.2-rc.2 版本改动主要包括错误修复。

功能增强:

  - 把默认日志级别改为 **warning**
    
    Github PR: [emqx/emqx\#2657](https://github.com/emqx/emqx/pull/2657)

  - 增加获取历史告警的接口
    
    Github PRs:
    [emqx/emqx\#2660](https://github.com/emqx/emqx/pull/2660)
    [emqx/emqx-management\#98](https://github.com/emqx/emqx-management/pull/98)

错误修复:

  - 删除残留的 Session 记录
    
    Github PR: [emqx/emqx\#2655](https://github.com/emqx/emqx/pull/2655)

  - 解决批量发送时，消息失序的问题
    
    Github PR: [emqx/emqx\#2650](https://github.com/emqx/emqx/pull/2650)
    
    感谢 [tradingtrace](https://github.com/tradingtrace) 的贡献 \!

#### emqx-rule-engine (plugin)

功能增强:

  - 新增一个动作 "do nothing"
    
    Github PR:
    [emqx/emqx-rule-engine\#70](https://github.com/emqx/emqx-rule-engine/pull/70)

  - 将 `retain` flag 的数据类型改为 integer
    
    Github RP:
    [emqx/emqx-rule-engine\#72](https://github.com/emqx/emqx-rule-engine/pull/72)

错误修复:

  - 修复 SQL 中无法使用 `timestamp` 关键字作为字段的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#71](https://github.com/emqx/emqx-rule-engine/pull/71)

#### emq-bridge-mqtt (plugin)

功能增强:

  - 将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#2](https://github.com/emqx/emqx-bridge-mqtt/pull/2)

#### emqx-rel (build project)

错误修复:

  - 解决 windows 服务注册的问题
    
    Github PR:
    [emqx/emqx-rel\#381](https://github.com/emqx/emqx-rel/pull/381)

## 3.2-rc.1 版本

* 发布日期: 2019-06-22*

EMQ X 3.2-rc.1 版本发布。此版本改动主要包括功能增强与错误修复。

功能增强:

  - 支持设置日志前缀
    
    Github PR: [emqx/emqx\#2627](https://github.com/emqx/emqx/pull/2627)

  - 提升 connect/disconnect 系统消息中客户端上下线的时间戳精度
    
    Github PR: [emqx/emqx\#2641](https://github.com/emqx/emqx/pull/2641)

  - 优化开发流程，支持 `make run`
    
    Github PR: [emqx/emqx\#2644](https://github.com/emqx/emqx/pull/2644)

错误修复:

  - 修复 flapping 模块无法正确读取配置的问题
    
    Github PR: [emqx/emqx\#2628](https://github.com/emqx/emqx/pull/2628)

  - 修复 `cpu_sup:util/0` 在 Windows 环境不可用导致崩溃的问题
    
    Github PR: [emqx/emqx\#2629](https://github.com/emqx/emqx/pull/2629)

  - 修复 [emqx/emqx:
    issue\#2619](https://github.com/emqx/emqx/issues/2619)
    
    Github PR: [emqx/emqx\#2646](https://github.com/emqx/emqx/pull/2646)

#### emqx-rule-engine (plugin)

功能增强:

  - 支持定期获取资源状态并设置告警
    
    Github PR:
    [emqx/emqx-rule-engine\#67](https://github.com/emqx/emqx-rule-engine/pull/67)

#### emqx-sn (plugin)

错误修复:

  - 修复误判 `keepalive_timeout` 的问题
    
    Github PR:
    [emqx/emqx-sn\#127](https://github.com/emqx/emqx-sn/pull/127)

  - 修复没有正确获取 `idle_timeout` 的问题
    
    Github PR:
    [emqx/emqx-sn\#128](https://github.com/emqx/emqx-sn/pull/128)

  - 修复测试用例
    
    Github PR:
    [emqx/emqx-sn\#130](https://github.com/emqx/emqx-sn/pull/130)

#### emqx-auth-jwt (plugin)

错误修复:

  - 正确读取 pubkey
    
    Github PR:
    [emqx/emqx-auth-jwt\#88](https://github.com/emqx/emqx-auth-jwt/pull/88)

#### emqx-rel (build-project)

Enhancements:

  - 使项目构建更加智能和健壮
    
    GitHub PR:
    [emqx/emqx-rel\#375](https://github.com/emqx/emqx-rel/pull/375),
    [emqx/emqx-rel\#376](https://github.com/emqx/emqx-rel/pull/376)

## 3.2-beta.3 版本

* 发布日期: 2019-06-14*

EMQ X 3.2-beta.3 版本发布。此版本改动主要包括增强规则引擎和错误修复。

错误修复:

  - 修复没有检查 `Will Retain` 的问题
    
    Github PR: [emqx/emqx\#2607](https://github.com/emqx/emqx/pull/2607)

  - 修复 [emqx/emqx:
    issue\#2591](https://github.com/emqx/emqx/issues/2591)
    
    Github PR: [emqx/emqx\#2615](https://github.com/emqx/emqx/pull/2615)

  - 默认情况下删除日志记录的字符限制
    
    Github PR: [emqx/emqx\#2617](https://github.com/emqx/emqx/pull/2617)

  - 修复无法处理分裂的 TCP 报文的问题
    
    Github PR: [emqx/emqx\#2611](https://github.com/emqx/emqx/pull/2611)

#### emqx-rule-engine (plugin)

功能增强:

  - 支持规则命中次数等 Metrics 统计
    
    Github PR:
    [emqx/emqx-rule-engine\#63](https://github.com/emqx/emqx-rule-engine/pull/63)

#### emqx-management (plugin)

错误修复:

  - 修复 CLI 无法踢掉 websocket 连接的问题
    
    Github PR:
    [emqx/emqx-management\#93](https://github.com/emqx/emqx-management/pull/93)

## 3.2-beta.2 版本

* 发布日期: 2019-06-06*

EMQ X 3.2-beta.2 版本发布。此版本改动主要包括增强规则引擎和错误修复。

错误修复:

  - 修复 [emqx/emqx:
    issue\#2553](https://github.com/emqx/emqx/issues/2553)
    
    Github PR: [emqx/emqx\#2596](https://github.com/emqx/emqx/pull/2596)

#### emqx-rule-engine (plugin)

功能增强:

  - 支持在 Dashboard 中测试 SQL 语句
    
    Github Commit:
    [emqx/emqx-rule-engine\#3e7c4c](https://github.com/emqx/emqx-rule-engine/commit/3e7c4cbe275d8f120ad8efb83fd23ee571d465db)

  - 预处理 PreparedStatement 以获得更好的性能
    
    Github Commit:
    [emqx/emqx-rule-engine\#fa3720](https://github.com/emqx/emqx-rule-engine/commit/fa37205850c6efe9af5f8ca2f230e17c7de2adb4),
    [emqx/emqx-rule-engine\#b00fad](https://github.com/emqx/emqx-rule-engine/commit/b00fad45c283fa2ec3aa57353bbe161960547461)

  - 规则引擎适配集群
    
    Github Commit:
    [emqx/emqx-rule-engine\#3da7fe](https://github.com/emqx/emqx-rule-engine/commit/3da7fed60d92c9a994c2aed5f34509c0d0d4eff4),
    [emqx/emqx-rule-engine\#4963b0](https://github.com/emqx/emqx-rule-engine/commit/4963b0ee3a6114ebe74b48876d25723137df14ad)

  - Dashboard 可以显示 Resource 状态
    
    Github Commit:
    [emqx/emqx-rule-engine\#dd9a8d](https://github.com/emqx/emqx-rule-engine/commit/dd9a8d4801f650c1ac888f7420f5497f7d0d6c73),
    [emqx/emqx-rule-engine\#d16224](https://github.com/emqx/emqx-rule-engine/commit/d162246c0b630e059c21f7b36e50154f3d7832e3),
    [emqx/emqx-rule-engine\#e4574c](https://github.com/emqx/emqx-rule-engine/commit/e4574c9554d7e7d79a8ce55a6c9e4089ee00db79)

  - 支持通过 Dashboard 重启 Resource
    
    Github Commit:
    [emqx/emqx-rule-engine\#ccbffd](https://github.com/emqx/emqx-rule-engine/commit/ccbffd7d5db514adf6cd20e8d139e73f80bc1c96)

  - 支持检查 HTTP 是否可连通
    
    Github Commit:
    [emqx/emqx-rule-engine\#3feffc](https://github.com/emqx/emqx-rule-engine/commit/3feffcd5a3f0da78725f1208594cea1b3273ec0b)

错误修复:

  - 修复删除 Resource 前检查依赖发生错误的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#3265ff](https://github.com/emqx/emqx-rule-engine/commit/3265ffe10584f0edccc084e6f78ae035ba310c07)

  - 修复 Resource 无法被销毁的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#58a1ce](https://github.com/emqx/emqx-rule-engine/commit/58a1ce45e1cf96cf05481d8ed076febef0d41976)

  - 修复 SQL 无法嵌套插入的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#64776a](https://github.com/emqx/emqx-rule-engine/commit/64776aebde1fe48c1038fba3b61f457590ab4408)

#### emqx-auth-http (plugin)

功能增强:

  - 支持 HTTPs
    
    Github PR:
    [emqx/emqx-auth-http\#133](https://github.com/emqx/emqx-auth-http/pull/133)

#### emqx-docker

错误修复:

  - 修复 [emqx/emqx-docker:
    issue\#115](https://github.com/emqx/emqx-docker/issues/115)
    
    Github Commit:
    [emqx/emqx-docker\#f3c219](https://github.com/emqx/emqx-docker/commit/f3c21978f5ffefd5d419bc78a1caf1ad71de9c91)

#### emqx-management (plugin)

错误修复:

  - 修复重新加载插件失败的问题
    
    Github PR:
    [emqx/emqx-management\#91](https://github.com/emqx/emqx-management/pull/91)

#### ekka (deps)

错误修复:

  - 修复导致 emqx\_sm\_locker 崩溃的问题
    
    Github Commit:
    [emqx/ekka\#2d5bf2](https://github.com/emqx/ekka/commit/2d5bf2a1f10d84408e4b35d3e274a49f395056c3)

## 3.2-beta.1 版本

* 发布日期: 2019-05-27*

EMQ X 3.2.beta-1 版本发布。此版本改动主要包括支持 rebar3 构建和增强规则引擎。

功能增强:

  - 支持通过 rabar3 构建项目
    
    Github PR:
    [emqx/emqx\#2475](https://github.com/emqx/emqx/pull/2475),
    [emqx/emqx\#2510](https://github.com/emqx/emqx/pull/2510),
    [emqx/emqx\#2518](https://github.com/emqx/emqx/pull/2518),
    [emqx/emqx\#2521](https://github.com/emqx/emqx/pull/2521)

  - SSL 连接支持 {active, N} 选项
    
    Github PR: [emqx/emqx\#2531](https://github.com/emqx/emqx/pull/2531)

  - 更正匿名访问行为表现
    
    Github PR: [emqx/emqx\#2355](https://github.com/emqx/emqx/pull/2355)

  - 提升 zone 的访问速度
    
    Github PR: [emqx/emqx\#2548](https://github.com/emqx/emqx/pull/2548)

错误修复:

  - 修复 emqx\_sm 中的致命错误
    
    Github PR: [emqx/emqx\#2559](https://github.com/emqx/emqx/pull/2559)

  - 修复发布 MQTT-SN、CoAP 消息时的错误
    
    Github PR: [emqx/emqx\#2556](https://github.com/emqx/emqx/pull/2556)

#### emqx-rule-engine (plugin)

功能增强:

  - 更好的规则引擎
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

#### emqx-web-hook (plugin)

功能增强:

  - 增加一个用于编码 payload 字段的选项
    
    Github PR:
    [emqx/emqx-web-hook\#119](https://github.com/emqx/emqx-web-hook/pull/119)

#### emqx-auth-http (plugin)

功能增强:

  - HTTP 请求支持更多选项
    
    Github PR:
    [emqx/emqx-auth-http\#128](https://github.com/emqx/emqx-auth-http/pull/128)

#### emqx-sn (plugin)

错误修复:

  - 修复错误的函数调用
    
    Github PR:
    [emqx/emqx-sn\#118](https://github.com/emqx/emqx-sn/pull/118)

## 3.1.2 版本

* 发布日期: 2019-06-06*

EMQ X 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

#### EMQ X Core

Bug fixes:

  - 修复 [emqx/emqx: issue
    \#2595](https://github.com/emqx/emqx/issues/2595)
    
    Github PR: [emqx/emqx\#2601](https://github.com/emqx/emqx/pull/2601)

  - 修复无法设置日志等级的问题
    
    Github PR: [emqx/emqx\#2600](https://github.com/emqx/emqx/pull/2600)

  - 修复返回值不匹配的问题
    
    Github PR: [emqx/emqx\#2560](https://github.com/emqx/emqx/pull/2560)

  - 热修复 `emqx_sn` 与 `emqx_coap` 插件
    
    Github PR: [emqx/emqx\#2556](https://github.com/emqx/emqx/pull/2556)

#### emqx-coap (plugin)

错误修复:

  - 修复无法发布消息的问题
    
    Github PR:
    [emqx/emqx-coap\#120](https://github.com/emqx/emqx-coap/pull/120)

#### ekka (deps)

错误修复:

  - 修复导致 `emqx_sm_locker` 崩溃的问题
    
    Github PR: [emqx/ekka\#54](https://github.com/emqx/ekka/pull/54)

  - 修复 k8s 无法使用 dns 集群的问题
    
    Github PR: [emqx/ekka\#53](https://github.com/emqx/ekka/pull/53)

  - 修复 etcd 集群不可用的问题
    
    Github PR: [emqx/ekka\#52](https://github.com/emqx/ekka/pull/52)

## 3.1.1 版本

* 发布日期: 2019-05-10*

EMQ X 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

功能增强:

  - 增大单条日志可打印的最大字符数量
    
    Github PR: [emqx/emqx\#2509](https://github.com/emqx/emqx/pull/2509)

  - `force_shutdown_policy` 将根据系统位数使用不同的默认值
    
    Github PR: [emqx/emqx\#2515](https://github.com/emqx/emqx/pull/2515)

错误修复:

  - 正确地配置和使用 `long_gc` 与 `long_schedule`
    
    Github PR:
    [emqx/emqx\#2504](https://github.com/emqx/emqx/pull/2504),
    [emqx/emqx\#2513](https://github.com/emqx/emqx/pull/2513)

  - 修复没有更新 `suboptions/count` 的问题
    
    Github PR: [emqx/emqx\#2507](https://github.com/emqx/emqx/pull/2507)

#### emqx-lwm2m (plugin)

错误修复:

  - 修复 mountpoint 没有生效的问题
    
    Github PR:
    [emqx/emqx-lwm2m\#34](https://github.com/emqx/emqx-lwm2m/pull/34)

  - 修复消息无法被 `emqx-web-hook` 转发的问题
    
    Github PR:
    [emqx/emqx-lwm2m\#35](https://github.com/emqx/emqx-lwm2m/pull/35)

## 3.1.0 版本

* 发布日期: 2019-04-26*

EMQ X 3.1.0 版本发布。此版本改动主要包括全面支持规则引擎、引入 storm 模块以支持 edge storm、 重构
flapping 代码。

功能改进:

  - 添加 emqx\_ct\_helpers 依赖，并重构测试用例
    
    Github PR: [emqx/emqx\#2480](https://github.com/emqx/emqx/pull/2480)

  - 重构 flapping 代码
    
    Github PR: [emqx/emqx\#2476](https://github.com/emqx/emqx/pull/2476)

#### emqx-management (plugin)

问题修复:

  - 修复 listeners acceptors 的值没有正确获取的问题
    
    Github PR:
    [emqx/emqx-management\#76](https://github.com/emqx/emqx-management/pull/76)

#### emqx-rule-engine (plugin)

功能改进:

  - 支持规则动作参数的验证
    
    Github PR:
    [emqx/emqx-rule-engine\#b28318](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

  - 删除资源时检查是否存在依赖
    
    Github PR:
    [emqx/emqx-rule-engine\#fa75b9](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

  - 从 republish 动作中移除 `from` 参数
    
    Github PR:
    [emqx/emqx-rule-engine\#8721eb](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

  - 修复了 SQL where 子句不能处理整数的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#c9c761](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)

#### emqx-storm (plugin)

功能改进:

  - 支持 edge storm
    
    Github Repository:
    [emqx/emqx-storm](https://github.com/emqx/emqx-storm)

## 3.1-rc.3 版本

* 发布日期: 2019-04-19*

EMQ X 3.1-rc.3 版本发布。此版本改动主要包括规则引擎增强、错误修复。 注意：从此版本开始，新增 OpenSUSE
安装包，并且不再提供 Debian 7 安装包。

功能改进:

  - 支持对客户端进行 flapping 检测，以及禁止异常的客户端
    
    Github PR: [emqx/emqx\#2438](https://github.com/emqx/emqx/pull/2438)

  - 支持配置日志输出长度
    
    Github PR: [emqx/emqx\#2461](https://github.com/emqx/emqx/pull/2461)

问题修复:

  - 修复 `emqx_client` 没有正确设置 CONNECT 报文 Keep Alive 字段的问题
    
    Github PR: [emqx/emqx\#2443](https://github.com/emqx/emqx/pull/2443)

#### emqx-auth-mysql (plugin)

功能改进:

  - 支持 proxysql
    
    Github PR:
    [emqx/emqx-auth-mysql\#134](https://github.com/emqx/emqx-auth-mysql/pull/134)

#### emqx-statsd (plugin)

问题修复:

  - 修复 Windows 兼容性引起的问题
    
    Github PR:
    [emqx/emqx-statsd\#24](https://github.com/emqx/emqx-statsd/pull/24)

#### emqx-web-hook (plugin)

功能改进:

  - 支持事件 actions
    
    Github Commit:
    [emqx/emqx-web-hook\#8367e0](https://github.com/emqx/emqx-web-hook/commit/8367e02f5ccafc7df9600c258348461a67c171bd)

  - 优化 webhook 资源的 specs
    
    Github Commit:
    [emqx/emqx-web-hook\#5a1345](https://github.com/emqx/emqx-web-hook/commit/5a13457d4f823fa80df1c7eab9a8e945ae6a0701)

  - 支持通过 hook 类型搜索 actions
    
    Github Commit:
    [emqx/emqx-web-hook\#fb3b1b](https://github.com/emqx/emqx-web-hook/commit/fb3b1ba98ca3f2557a51be98a06537781119132c)

#### emqx-rule-engine (plugin)

功能改进:

  - 支持通过资源类型搜索 actions
    
    Github PR:
    [emqx/emqx-rule-engine\#25](https://github.com/emqx/emqx-rule-engine/pull/25)

  - 注册资源提供者更改为加载资源提供者
    
    Github PR:
    [emqx/emqx-rule-engine\#26](https://github.com/emqx/emqx-rule-engine/pull/26)

  - 优化 actions 的输入数据
    
    Github PR:
    [emqx/emqx-rule-engine\#27](https://github.com/emqx/emqx-rule-engine/pull/27)

#### emqx-rel

问题修复:

  - 修复修改 log.rotation.size 后启动失败的问题
    
    Github PR:
    [emqx/emqx-rel\#336](https://github.com/emqx/emqx-rel/pull/336)

## 3.1-rc.2 版本

* 发布日期: 2019-04-13*

EMQ X 3.1-rc.2 版本发布。此版本改动主要包括规则引擎增强、错误修复。

功能改进:

  - 重新设计 emqx\_bridge 的 ensure\_start 与 ensure\_stop API
    
    Github PR: [emqx/emqx\#2423](https://github.com/emqx/emqx/pull/2423)

  - 提供 Handler 以扩展 emqx\_bridge
    
    Github PR: [emqx/emqx\#2414](https://github.com/emqx/emqx/pull/2414)

问题修复:

  - 修复 metrics 在某些情况下没有正确更新的问题
    
    Github PR: [emqx/emqx\#2416](https://github.com/emqx/emqx/pull/2416)

  - 修复 trace log level 无法生效时没有提示的问题
    
    Github PR: [emqx/emqx\#2408](https://github.com/emqx/emqx/pull/2408)

#### emqx-auth-http (plugin)

功能增强:

  - 支持用户的 WebServer 回传 Mountpoint
    
    Github PR:
    [emqx/emqx-auth-http\#116](https://github.com/emqx/emqx-auth-http/pull/116)

#### emqx-auth-username (plugin)

功能增强:

  - 移除在配置文件中配置默认 username 的功能
    
    Github PR:
    [emqx/emqx-auth-username\#96](https://github.com/emqx/emqx-auth-username/pull/96)

#### emqx-auth-clientid (plugin)

功能增强:

  - 移除在配置文件中配置默认 clientid 的功能
    
    Github PR:
    [emqx/emqx-auth-clientid\#81](https://github.com/emqx/emqx-auth-clientid/pull/81)

#### emqx-rule-engine (plugin)

功能增强:

  - 支持标准 POSIX CLI 格式
    
    Github PR:
    [emqx/emqx-rule-engine\#23](https://github.com/emqx/emqx-rule-engine/pull/23)

问题修复:

  - 修复 HTTP APIs 中的错误
    
    Github PR:
    [emqx/emqx-rule-engine\#21](https://github.com/emqx/emqx-rule-engine/pull/21)

#### emqx-packages (plugin)

问题修复:

  - 修复 EMQ X 在 CentOS 中开机启动失败的问题
    
    Github Commit:
    [emqx/emqx-packages\#64760523ea29ca0ad1d85b763f0e8a8e6954db9c](https://github.com/emqx/emqx-packages/commit/64760523ea29ca0ad1d85b763f0e8a8e6954db9c)

#### emqx-dashboard (plugin)

功能增强:

  - 新增 Rule-Engine 前端页面
    
    Github PR:
    [emqx/emqx-dashboard\#50](https://github.com/emqx/emqx-dashboard/pull/50)

  - 支持在集群中统一管理 Dashboard 用户
    
    Github PR:
    [emqx/emqx-dashboard\#48](https://github.com/emqx/emqx-dashboard/pull/48)

## 3.1-rc.1 版本

* 发布日期: 2019-04-04*

EMQ X 3.1-rc.1 版本发布。此版本改动主要包括规则引擎增强、错误修复、稳定性增强等。

功能改进:

  - 支持压缩 WebSocket 消息
    
    Github PR: [emqx/emqx\#2356](https://github.com/emqx/emqx/pull/2356)

  - etcd 集群支持 SSL 连接
    
    Github PR: [emqx/emqx\#2367](https://github.com/emqx/emqx/pull/2367)

  - 支持 Websocket 的 proxy protocol
    
    Github PR: [emqx/emqx\#2372](https://github.com/emqx/emqx/pull/2372)

问题修复:

  - 修复 monitor 模块中的错误逻辑
    
    Github PR: [emqx/emqx\#2353](https://github.com/emqx/emqx/pull/2353)

  - 修复 allow\_anonymous 功能不符合预期的问题
    
    Github PR: [emqx/emqx\#2355](https://github.com/emqx/emqx/pull/2355)

  - 修复 session 进程中无法一次性接收多个消息的问题
    
    Github PR: [emqx/emqx\#2373](https://github.com/emqx/emqx/pull/2373)

  - 修复 message.dropped 的 hook 在某些情况下不会被触发的问题
    
    Github PR: [emqx/emqx\#2399](https://github.com/emqx/emqx/pull/2399)

#### emqx-auth-http (plugin)

功能增强:

  - 支持从 SSL 双向连接中取出 Subject Name 与 Common Name 用于认证
    
    Github PR:
    [emqx/emqx-auth-http\#113](https://github.com/emqx/emqx-auth-http/pull/113)

#### emqx-auth-clientid (plugin)

功能增强:

  - 支持通过 REST API 操作 ClientId
    
    Github PR:
    [emqx/emqx-auth-clientid\#78](https://github.com/emqx/emqx-auth-clientid/pull/78)

#### emqx-auth-jwt (plugin)

功能增强:

  - 支持验证指定的 claims 字段
    
    Github PR:
    [emqx/emqx-auth-jwt\#69](https://github.com/emqx/emqx-auth-jwt/pull/69)

#### emqx-rule-engine (plugin)

功能增强:

  - 增强规则引擎
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

#### emqx-rel

错误修复:

  - 修复 Windows 环境下 EMQ X 需要启动两次的问题
    
    Github Commit:
    [emqx/emqx-rel\#75de3441db9bf03d489609dcbb340a74de263508](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

  - 修复 Windows 环境下 EMQ X 安装路径含有中文或空格时无法启动的问题
    
    Github Commit:
    [emqx/emqx-rel\#75de3441db9bf03d489609dcbb340a74de263508](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

## 3.1-beta.3 版本

* 发布日期: 2019-03-22*

EMQ X 3.1-beta.3 版本发布。此版本改动主要包括引入规则引擎，增强插件发现机制，和修复一些问题等。

功能改进:

  - 增强插件发现机制
    
    Github PR: [emqx/emqx\#2339](https://github.com/emqx/emqx/pull/2339)

问题修复:

  - 修复重复清除告警的错误
    
    Github PR: [emqx/emqx\#2332](https://github.com/emqx/emqx/pull/2332)

  - 修复粘包解析失败的问题
    
    Github PR: [emqx/emqx\#2333](https://github.com/emqx/emqx/pull/2333)

  - 正确设置 PUBLISH 文件中的 DUP 标识
    
    Github PR: [emqx/emqx\#2337](https://github.com/emqx/emqx/pull/2337)

#### emqx-rule-engine (plugin)

功能增强:

  - 实现规则引擎原型
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

#### emqx-lua-hook (plugin)

功能增强:

  - 增加认证与 ACL 的 hook
    
    Github PR:
    [emqx/emqx-lua-hook\#63](https://github.com/emqx/emqx-lua-hook/pull/63)

#### emqx-auth-mysql (plugin)

问题修复:

  - 修复 ACL 功能无法使用的问题
    
    Github PR:
    [emqx/emqx-auth-mysql\#130](https://github.com/emqx/emqx-auth-mysql/pull/130)

## 3.1-beta.2 版本

* 发布日期: 2019-03-16*

EMQ X 3.1-beta.2 版本发布。此版本改动主要包括重新设计 hooks, 支持 TLS/PSK 和修复 gen\_rpc
的一些问题等。

功能改进:

  - 优化 emqx hooks
    
    Github PR: [emqx/emqx\#2309](https://github.com/emqx/emqx/pull/2309)

  - 支持 TLS/DTLS PSK
    
    Github PR: [emqx/emqx\#2297](https://github.com/emqx/emqx/pull/2297)

  - 将 Request/Response 从 emqx client 分离
    
    Github PR: [emqx/emqx\#2293](https://github.com/emqx/emqx/pull/2293)

错误修复:

  - 修复某些情况下集群转发消息时 Broker 可能崩溃的问题
    
    Github issues:
    [emqx/emqx\#2290](https://github.com/emqx/emqx/issues/2290)
    
    Github PR: [emqx/emqx\#2320](https://github.com/emqx/emqx/pull/2320)

  - 在 Broker 卸载插件并退出前卸载 emqx\_alarm\_handler
    
    Github PR: [emqx/emqx\#2316](https://github.com/emqx/emqx/pull/2316)

  - 修复一个与 emqx bridge 相关的错误
    
    Github issues:
    [emqx/emqx\#2312](https://github.com/emqx/emqx/issues/2312)
    
    Github PR: [emqx/emqx\#2313](https://github.com/emqx/emqx/pull/2313)

  - 终结 inflight full error
    
    Github PR: [emqx/emqx\#2281](https://github.com/emqx/emqx/pull/2281)

#### emqx-management (plugin)

功能增强:

  - 增加默认的 secret 配置
    
    Github PR:
    [emqx/emqx-management\#58](https://github.com/emqx/emqx-management/pull/58)

  - 修复插件尚未启动时无法 reload 的问题
    
    Github PR:
    [emqx/emqx-management\#59](https://github.com/emqx/emqx-management/pull/59)

  - 插件相关的 HTTP API 由插件各自实现
    
    Github PR:
    [emqx/emqx-management\#57](https://github.com/emqx/emqx-management/pull/57)

  - 修复查询 io/max\_fds 返回 undefined 的问题
    
    Github issues:
    [emqx/emqx-management\#2222](https://github.com/emqx/emqx-management/issues/2222)
    
    Github PR:
    [emqx/emqx-management\#54](https://github.com/emqx/emqx-management/pull/54)

#### emqx-auth-jwt (plugin)

功能增强:

  - 优化 JWT 认证插件
    
    Github PR:
    [emqx/emqx-auth-jwt\#63](https://github.com/emqx/emqx-auth-jwt/pull/63)

#### emqx-auth-usernmae (plugin)

功能增强:

  - 增加 CURD HTTP API 以管理用户名密码
    
    Github PR:
    [emqx/emqx-auth-username\#82](https://github.com/emqx/emqx-auth-username/pull/82)

#### emqx-web-hook (plugin)

错误修复:

  - 修复格式化消息时的错误
    
    Github issues:
    [emqx/emqx-web-hook\#93](https://github.com/emqx/emqx-web-hook/issues/93)
    
    Github PR:
    [emqx/emqx-web-hook\#96](https://github.com/emqx/emqx-web-hook/pull/96)

#### minirest (deps)

错误修复:

  - 过滤未启动插件的 HTTP API
    
    Github PR:
    [emqx/minirest\#12](https://github.com/emqx/minirest/pull/12)

#### gen\_rpc (deps)

错误修复:

  - 修复 'gen\_rpc' 的 raw socket flags
    
    Github PR:
    [emqx/gen\_rpc\#5](https://github.com/emqx/gen_rpc/pull/5)

## 3.1-beta.1 版本

* 发布日期: 2019-02-28*

EMQ X 3.1-beta.1 版本发布。此版本主要针对功能改进，包括引入全新的 Bridge，支持消息批量发送，支持 redis 集群等。

功能改进:

  - 引入新的 Bridge 实现，支持 EMQ Broker 节点间桥接和 MQTT 协议间桥接
    
    Github PR: [emqx/emqx\#2199](https://github.com/emqx/emqx/pull/2199)

  - 支持消息批量发送
    
    Github PR: [emqx/emqx\#2253](https://github.com/emqx/emqx/pull/2253)

  - 使用 gen\_statem behaviour 改进 emqx\_connection 模块
    
    Github PR: [emqx/emqx\#2235](https://github.com/emqx/emqx/pull/2235)

  - 新增资源监控，优化告警处理
    
    Github PR: [emqx/emqx\#2266](https://github.com/emqx/emqx/pull/2266)

#### emqx-auth-redis

功能改进:

  - 支持 redis 集群
    
    Github PR:
    [emqx/emqx-auth-redis\#93](https://github.com/emqx/emqx-auth-redis/pull/93)

#### emqx-dashboard

功能改进:

  - 为 emqx\_dashboard\_cli 模块增加测试用例
    
    Github PR:
    [emqx/emqx-dashboard\#34](https://github.com/emqx/emqx-dashboard/pull/34)

#### emqx-auth-username

功能改进:

  - 增加新的 CLI 以更新 username
    
    Github PR:
    [emqx/emqx-auth-username\#74](https://github.com/emqx/emqx-auth-username/pull/74)

#### emqx-auth-clientid

功能改进:

  - 增加新的 CLI 以更新 clientid
    
    Github PR:
    [emqx/emqx-auth-clientid\#59](https://github.com/emqx/emqx-auth-clientid/pull/59)

## 3.0.1 版本

* 发布日期: 2019-01-25*

EMQ X 3.0.1 版本发布。此版本主要包含功能改进和错误修复。

功能改进:

  - 为 emqx edge 增加 +L 虚拟机参数以减少内存
    
    Github PR: [emqx/emqx\#2110](https://github.com/emqx/emqx/pull/2110)

  - 简化修改日志输出等级的命令
    
    Github PR: [emqx/emqx\#2115](https://github.com/emqx/emqx/pull/2115)

  - 重构 bridge 代码；支持 bridge 消息持久化
    
    Github PR:
    [emqx/emqx\#2160](https://github.com/emqx/emqx/pull/2160),
    [emqx/emqx\#2117](https://github.com/emqx/emqx/pull/2117),
    [emqx/emqx\#2113](https://github.com/emqx/emqx/pull/2113),
    [emqx/emqx\#2108](https://github.com/emqx/emqx/pull/2108),
    [emqx/emqx\#2053](https://github.com/emqx/emqx/pull/2053)

  - 优化路由匹配
    
    Github PR: [emqx/emqx\#2124](https://github.com/emqx/emqx/pull/2124)

  - 改进 'emqx\_client' 模块设计
    
    Github PR: [emqx/emqx\#2137](https://github.com/emqx/emqx/pull/2137)

  - 改进 'emqx\_pool' 模块的设计
    
    Github PR: [emqx/emqx\#2138](https://github.com/emqx/emqx/pull/2138)

  - 改进共享订阅调度实现
    
    Github PR: [emqx/emqx\#2144](https://github.com/emqx/emqx/pull/2144)

  - 支持重启 emqx 时重新生成配置
    
    Github PR: [emqx/emqx\#2175](https://github.com/emqx/emqx/pull/2175)

问题修复:

  - 修复对端关闭连接时崩溃的问题
    
    Github PR: [emqx/emqx\#2074](https://github.com/emqx/emqx/pull/2074)

  - 修复客户端正常断开连接时依旧发送遗嘱消息的问题
    
    Github PR: [emqx/emqx\#2156](https://github.com/emqx/emqx/pull/2156)

#### emqx-lwm2m

问题修复:

  - 移除认证功能
    
    GitHub PR:
    [emqx/emqx-lwm2m\#14](https://github.com/emqx/emqx-lwm2m/pull/14)

#### emqx-auth-username

问题修复:

  - 支持可选的加密模式
    
    GitHub PR:
    [emqx/emqx-auth-usernmae\#64](https://github.com/emqx/emqx-auth-username/pull/64)

#### emqx-auth-clientid

功能改进:

  - 支持可选的加密模式
    
    GitHub PR:
    [emqx/emqx-auth-clientid\#52](https://github.com/emqx/emqx-auth-username/pull/52)

#### emqx-management

功能改进:

  - 增加 'plugins reload \<Name\>' CLI 命令，支持重载插件时重新生成配置
    
    Github PR:
    [emqx/emqx-management\#30](https://github.com/emqx/emqx-management/pull/30)

## 3.0.0 版本

* 发布日期: 2018-12-22*

EMQ X 3.0.0 版本，重新设计了订阅的 ETS 表，通过重构模块和调节 erlang 虚拟机参数提升了 EMQ 性能

功能改进:

  - 将虚拟机参数移动到单独的 vm.args 文件
    
    Github PR:
    [emqx/emqx\#2033](https://github.com/emqx/emqx/pull/2033),
    [emqx/emqx\#2057](https://github.com/emqx/emqx/pull/2057),
    [emqx/emqx\#2070](https://github.com/emqx/emqx/pull/2070)

  - 为遗嘱消息主题增加格式校验和 ACL 检查
    
    Github PR: [emqx/emqx\#2075](https://github.com/emqx/emqx/pull/2075)

  - 增加 ACL 检查返回拒绝时是否断开客户端连接的配置选项
    
    Github PR: [emqx/emqx\#2059](https://github.com/emqx/emqx/pull/2059)

  - 重构 session 监控树
    
    Github PR: [emqx/emqx\#2077](https://github.com/emqx/emqx/pull/2077)

  - 增加 'active\_n' 选项以优化 emqx\_connection 的 CPU 占用率
    
    Github PR: [emqx/emqx\#2060](https://github.com/emqx/emqx/pull/2060)

  - 支持客户端批量下线
    
    Github PR: [emqx/emqx\#2060](https://github.com/emqx/emqx/pull/2060)

  - 增加订阅表分片机制
    
    Github PR: [emqx/emqx\#2044](https://github.com/emqx/emqx/pull/2044)

  - 重构 'emqx\_gc' 模块
    
    Github PR: [emqx/emqx\#2090](https://github.com/emqx/emqx/pull/2090)

问题修复:

  - 修复 Topic Alias Maximum 的错误实现
    
    Github PR: [emqx/emqx\#2074](https://github.com/emqx/emqx/pull/2074)

  - 修复部分情况下不会发送遗嘱消息的错误
    
    Github PR: [emqx/emqx\#2068](https://github.com/emqx/emqx/pull/2068)

#### emqx-auth-ldap

功能改进:

  - 更好的设计
    
    GitHub PR:
    [emqx/emqx-auth-ldap\#46](https://github.com/emqx/emqx-auth-ldap/pull/46)

#### emqx-lua-hook

问题修复:

  - 修复测试用例
    
    GitHub PR:
    [emqx/emqx-lua-hook\#45](https://github.com/emqx/emqx-lua-hook/pull/45)

#### emqx-management

功能改进:

  - 为 REST API 增加测试用例，并规范返回的响应格式
    
    Github PR:
    [emqx/emqx-management\#21](https://github.com/emqx/emqx-management/pull/21)

## 3.0-rc.5 版本

* 发布日期: 2018-11-30*

EMQ X 3.0-rc.5 版本发布，该版本支持 metrics 的批量提交和修复错误:

功能改进:

  - 减小依赖大小
    
    Github PR: [emqx/emqx\#1981](https://github.com/emqx/emqx/pull/1981)

  - 支持 metrics 的批量提交
    
    Github PR: [emqx/emqx\#2001](https://github.com/emqx/emqx/pull/2001)

  - 优化 mnesia/ets 的并行读写性能
    
    Github PR: [emqx/emqx\#2006](https://github.com/emqx/emqx/pull/2006)

问题修复:

  - 修复 emqx\_router 中的 'function\_clause' 错误
    
    Github PR: [emqx/emqx\#1998](https://github.com/emqx/emqx/pull/1998)

  - 启动过程中移除 simple 日志句柄
    
    Github PR: [emqx/emqx\#2000](https://github.com/emqx/emqx/pull/2000)

  - 修复 emqx\_reason\_codes 模块可能出现参数异常的问题
    
    Github PR: [emqx/emqx\#2008](https://github.com/emqx/emqx/pull/2008)

#### emqx-passwd

功能改进:

  - 支持 Rebar3
    
    GitHub PR:
    [emqx/emqx-passwd\#6](https://github.com/emqx/emqx-passwd/pull/6)

#### emqx-web-hook

功能改进:

  - 支持 Rebar3
    
    GitHub PR:
    [emqx/emqx-web-hook\#77](https://github.com/emqx/emqx-web-hook/pull/77)

问题修复:

  - 修复 emqx-web-hook 发送 HTTP 请求时未携带 username 和 clientid 的错误
    
    GitHub PR:
    [emqx/emqx-web-hook\#77](https://github.com/emqx/emqx-web-hook/pull/77)

#### emqx-dashboard

问题修复:

  - 修复火狐浏览器无法拷贝应用信息的问题
    
    Github PR:
    [emqx/emqx-dashboard\#12](https://github.com/emqx/emqx-dashboard/pull/12)

#### emqx-management

问题修复:

  - 修复 clients 的 CLI 错误
    
    Github PR:
    [emqx/emqx-management\#16](https://github.com/emqx/emqx-management/pull/16)

## 3.0-rc.4 版本

* 发布日期: 2018-11-24*

EMQ X 3.0-rc.4 版本发布，该版本改进日志功能，部分项目支持 Rebar3 构建:

功能改进:

  - 为使用 MQTT v3.1.1 的客户端提供避免 loop delivery 的功能
    
    Github PR: [emqx/emqx\#1964](https://github.com/emqx/emqx/pull/1964)

  - 支持使用 username 代替 client\_id，默认不开启
    
    Github PR: [emqx/emqx\#1961](https://github.com/emqx/emqx/pull/1961)

  - 默认日志类型为 both
    
    Github PR: [emqx/emqx\#1979](https://github.com/emqx/emqx/pull/1979)

  - 添加控制日志等级的命令行接口
    
    Github PR: [emqx/emqx\#1977](https://github.com/emqx/emqx/pull/1977)

  - 改进 log tracer 的命令行接口
    
    Github PR: [emqx/emqx\#1973](https://github.com/emqx/emqx/pull/1973)

  - 优化日志性能
    
    Github PR: [emqx/emqx\#1960](https://github.com/emqx/emqx/pull/1960)

问题修复:

  - 修复用户属性的类型验证
    
    Github PR: [emqx/emqx\#1969](https://github.com/emqx/emqx/pull/1969)

  - 修复 max\_topic\_alias 配置项的错误描述
    
    Github PR: [emqx/emqx\#1962](https://github.com/emqx/emqx/pull/1962)

  - 当 client\_id 为空时，将 proc meta-data 设置为服务端生成的 client\_id
    
    Github PR: [emqx/emqx\#1980](https://github.com/emqx/emqx/pull/1980)

#### emqx-coap

功能改进:

  - 支持 Rebar3
    
    GitHub PR:
    [emqx/emqx-coap\#89](https://github.com/emqx/emqx-coap/pull/89)

问题修复:

  - 修复 sendfun 参数错误的问题
    
    Github PR:
    [emqx/emqx-coap\#89](https://github.com/emqx/emqx-coap/pull/89)

#### emqx-management

问题修复:

  - 修复集群模式下通过 REST API 查找连接不稳定的问题
    
    Github PR:
    [emqx/emqx-management\#11](https://github.com/emqx/emqx-management/pull/11)

#### ekka

问题修复:

  - 修复分布式锁的错误判断
    
    Github PR: [emqx/ekka\#39](https://github.com/emqx/ekka/pull/39)

#### minirest

功能改进:

  - 支持 Rebar3
    
    Github PR:
    [emqx/minirest\#6](https://github.com/emqx/minirest/pull/6)

#### cuttlefish

问题修复:

  - 将 cuttlefish 的日志输出到 std\_error
    
    Github PR:
    [emqx/cuttlefish\#4](https://github.com/emqx/cuttlefish/pull/4)

#### emqx-rel

功能改进:

  - 构建时更新 cuttlefish
    
    Github PR:
    [emqx/emqx-rel\#253](https://github.com/emqx/emqx-rel/pull/253)

  - 默认不启用 delay\_publish 插件
    
    Github PR:
    [emqx/emqx-rel\#251](https://github.com/emqx/emqx-rel/pull/251)

## 3.0-rc.3 版本

* 发布日期: 2018-11-10*

EMQ X 3.0-rc.3 版本发布，该版本重构 emqx\_mqueue 代码，支持 MQTT-SN, CoAP 与 STOMP 协议:

功能改进:

  - 将 QOS$i 替换为 QOS\_$i
    
    Github PR: [emqx/emqx\#1948](https://github.com/emqx/emqx/pull/1948)

  - 更新配置文件中 ACL cache 的描述信息
    
    Github PR: [emqx/emqx\#1950](https://github.com/emqx/emqx/pull/1950)

  - 重构 emqx\_mqueue 代码
    
    Github PR: [emqx/emqx\#1926](https://github.com/emqx/emqx/pull/1926)

  - lager 替换为 OTP logger
    
    Github PR: [emqx/emqx\#1898](https://github.com/emqx/emqx/pull/1898)

问题修复:

  - 修复重复订阅时的 'badarg' 错误
    
    Github PR: [emqx/emqx\#1943](https://github.com/emqx/emqx/pull/1943)

  - 修复 emqx\_message:format 函数 'badarg' 错误
    
    Github PR: [emqx/emqx\#1954](https://github.com/emqx/emqx/pull/1954)

  - 修复 MQTT bridge 无法使用 TLS 连接的问题
    
    Github PR: [emqx/emqx\#1949](https://github.com/emqx/emqx/pull/1949)

#### emqx-stomp

功能改进:

  - 增强 receipt 报文支持，增加测试用例
    
    GitHub PR:
    [emqx/emqx-stomp\#53](https://github.com/emqx/emqx-stomp/pull/53)

#### emqx-sn

功能改进:

  - 增强对 MQTT-SN 协议的支持
    
    GitHub PR:
    [emqx/emqx-sn\#90](https://github.com/emqx/emqx-sn/pull/90)

#### emqx-lua-hook

问题修复:

  - 修复 emqx-lua-hook 无法正常使用的问题
    
    Github PR:
    [emqx/emqx-lua-hook\#41](https://github.com/emqx/emqx-lua-hook/pull/41)

#### emqx-statsd

功能改进:

  - 增加统计指标
    
    Github PR:
    [emqx/emqx-statsd\#4](https://github.com/emqx/emqx-statsd/pull/4)

#### emqx-dashboard

功能改进:

  - 增加 qos2/forward 指标
    
    Github PR:
    [emqx/emqx-dashboard\#7](https://github.com/emqx/emqx-dashboard/pull/7)

#### emqx-auth-pgsql

问题修复:

  - 修复并发量大时 emqx-auth-pgsql 出错的问题
    
    Github PR:
    [emqx/emqx-auth-pgsql\#94](https://github.com/emqx/emqx-auth-pgsql/pull/94)

## 3.0-rc.2 版本

* 发布日期: 2018-10-27*

EMQ X 3.0-rc.2 版本发布，该版本改进 Will Message 发布机制，新增支持使用 ssl 证书作为 MQTT 用户名:

功能改进:

  - 改进 Will Message 发布机制，增加取消发布处理
    
    Github PR: [emqx/emqx\#1889](https://github.com/emqx/emqx/pull/1889)

  - 新增支持使用 ssl 证书作为 MQTT 用户名
    
    Github PR: [emqx/emqx\#1913](https://github.com/emqx/emqx/pull/1913)

  - 提升代码测试覆盖率
    
    Github PR: [emqx/emqx\#1921](https://github.com/emqx/emqx/pull/1921)

问题修复:

  - 修复 emqx\_broker:subscribed 函数 'bad argument' 错误
    
    Github PR: [emqx/emqx\#1921](https://github.com/emqx/emqx/pull/1921)

## 3.0-rc.1 版本

* 发布日期: 2018-10-20*

EMQ X 3.0-rc.1 版本发布，该版本新增 request & response 以及 LwM2M 插件，修复 PUBLISH 验证问题:

功能改进:

  - 为 CONNECT & CONNACK 报文添加 request & response 支持
    
    Github PR: [emqx/emqx\#1819](https://github.com/emqx/emqx/pull/1819)

  - 为未认证的订阅添加警告信息
    
    Github PR:
    
    [emqx/emqx\#1878](https://github.com/emqx/emqx/pull/1878)

  - 增加 emqx\_hooks 的测试覆盖率，为 emqx\_mod\_sup 模块增加测试用例
    
    Github PR:
    
    [emqx/emqx\#1892](https://github.com/emqx/emqx/pull/1892)

问题修复:

  - 更新 ACL 文档链接
    
    Github PR: [emqx/emqx\#1899](https://github.com/emqx/emqx/pull/1899)

  - 修复验证 PUBLISH 报文时的匹配问题
    
    Github PR: [emqx/emqx\#1888](https://github.com/emqx/emqx/pull/1888)

  - 修复某些情况下不返回 Reason Code 给 client 的 BUG
    
    Github PR: [emqx/emqx\#1819](https://github.com/emqx/emqx/pull/1819)

  - 修复 emqx\_client 模块中的兼容性问题
    
    Github PR: [emqx/emqx\#1819](https://github.com/emqx/emqx/pull/1819)

#### emqx-lwm2m

  - 更新 LwM2M 插件以适配 EMQ X 3.0
    
    Github PR:
    [emqx/emqx-lwm2m\#3](https://github.com/emqx/emqx-lwm2m/pull/3)

## 3.0-Beta.4 版本

* 发布日期: 2018-09-29*

EMQ X 3.0-beta.4 版本发布，该版本改进连接 Shutdown 策略，改进共享订阅 sticky 策略，修复 Delayed
Publish 问题：

功能改进:

  - 为进程自定义 max\_heap\_size
    
    GitHub issues:
    [emqx/emqx\#1855](https://github.com/emqx/emqx/pull/1855)

  - 改进 Topic 别名 Maximum、连接 Receive Maximum
    
    GitHub issues:
    [emqx/emqx\#1873](https://github.com/emqx/emqx/pull/1873)

  - 修复共享订阅 sticky 策略 pick ID 方式
    
    GitHub issues:
    [emqx/emqx\#1871](https://github.com/emqx/emqx/pull/1871)

  - 为 Zone 新增 Mountpoint 配置
    
    GitHub issues:
    [emqx/emqx\#1869](https://github.com/emqx/emqx/pull/1869)

  - 修复 make app.config 错误
    
    GitHub issues:
    [emqx/emqx\#1868](https://github.com/emqx/emqx/pull/1868),

  - 修复 Hooks 回调参数错误
    
    GitHub issues:
    [emqx/emqx\#1866](https://github.com/emqx/emqx/pull/1866)

  - 改进 travis 构建支持 rebar3 xref
    
    GitHub issues:
    [emqx/emqx\#1861](https://github.com/emqx/emqx/pull/1861)

  - 升级依赖库 esockd 至 v5.4.2
    
    GitHub issues:
    [emqx/emqx\#1875](https://github.com/emqx/emqx/pull/1875)

  - 升级依赖库 erlang-bcrypt 至 0.5.1
    
    GitHub issues:
    [emqx/emqx-passwd\#3](https://github.com/emqx/emqx-passwd/pull/3)

#### emqx-delayed-publish

  - 修复消息延时发布
    
    GitHub issues:
    [emqx/emqx-delayed-publish\#5](https://github.com/emqx/emqx-delayed-publish/pull/5)

#### emqx-passwd

  - 改进 check\_pass 方式，供各类认证插件调用
    
    GitHub issues:
    [emqx/emqx-passwd\#3](https://github.com/emqx/emqx-passwd/pull/3)

#### bcrypt

  - 改进 bcrypt 验证方式
    
    GitHub issues:
    [emqx/erlang-bcrypt\#1](https://github.com/emqx/erlang-bcrypt/pull/1)

#### esockd

  - 新增 DTLS PSK 样例
    
    GitHub issues:
    [emqx/esockd\#88](https://github.com/emqx/esockd/pull/88)

  - 修复 DTLS 启动失败
    
    GitHub issues:
    [emqx/esockd\#89](https://github.com/emqx/esockd/pull/89)

  - 改进 SSL 启动方式
    
    GitHub issues:
    [emqx/esockd\#90](https://github.com/emqx/esockd/pull/90)

## 3.0-Beta.3 版本

* 发布日期: 2018-09-22*

EMQ X 3.0-beta.3 版本发布，该版本新增共享订阅派发策略功能，改进 GC 策略、桥接设计:

功能改进:

  - 修复 travis 构建
    
    GitHub issues:
    [emqx/emqx\#1818](https://github.com/emqx/emqx/pull/1818)

  - 更新模块 emqx\_mqueue.erl 文档说明
    
    GitHub issues:
    [emqx/emqx\#1815](https://github.com/emqx/emqx/pull/1815)

  - 新增共享订阅派发策略
    
    GitHub issues:
    [emqx/emqx\#1823](https://github.com/emqx/emqx/pull/1823)

  - 修复 emqx\_pool 模块参数错误
    
    GitHub issues:
    [emqx/emqx\#1827](https://github.com/emqx/emqx/pull/1827)

  - 新增强制 shutdown 策略
    
    GitHub issues:
    [emqx/emqx\#1836](https://github.com/emqx/emqx/pull/1836)

  - 改进 KeepAlive 检测算法
    
    GitHub issues:
    [emqx/emqx\#1839](https://github.com/emqx/emqx/pull/1839)

  - 修复跨节点消息投递
    
    GitHub issues:
    [emqx/emqx\#1846](https://github.com/emqx/emqx/pull/1846)

  - 改进 Bridge 设计
    
    GitHub issues:
    [emqx/emqx\#1849](https://github.com/emqx/emqx/pull/1849)

  - 改进 force\_gc\_policy 配置
    
    GitHub issues:
    [emqx/emqx\#1851](https://github.com/emqx/emqx/pull/1851)

  - 修复 Maximum-QoS 选项值
    
    GitHub issues:
    [emqx/emqx\#1852](https://github.com/emqx/emqx/pull/1852)

  - 升级依赖库 esockd 至 v5.4.1
    
    GitHub issues:
    [emqx/emqx\#1858](https://github.com/emqx/emqx/pull/1858)

问题修复:

  - 订阅 API，主题属性支持通配符
    
    GitHub issues:
    [emqx/emqx\#1706](https://github.com/emqx/emqx/issues/1706)

  - WebSocket 连接新增 Path 配置
    
    GitHub issues:
    [emqx/emqx\#1809](https://github.com/emqx/emqx/issues/1809)

  - 修复报文尺寸过大导致 block 问题
    
    GitHub issues:
    [emqx/emqx\#1811](https://github.com/emqx/emqx/issues/1811)

  - 新增函数 check\_expiry
    
    GitHub issues:
    [emqx/emqx\#1813](https://github.com/emqx/emqx/issues/1813)

  - 修复 DISCONNECT 报文 Session Expiry Interval 不起作用
    
    GitHub issues:
    [emqx/emqx\#1833](https://github.com/emqx/emqx/issues/1833)

  - 修复 DISCONNECT 报文 Max Session Expiry Interval 不起作用
    
    GitHub issues:
    [emqx/emqx\#1834](https://github.com/emqx/emqx/issues/1834)

#### emqx-management

  - 改进 Bridge CTL 命令

  - 修复函数调用 emqx\_mgmt\_cli:print () crash 问题

  - 修复 emqx\_mgmt:subscribe 函数 'function\_clause' 错误
    
    GitHub issues:
    [emqx/emqx-management\#1815](https://github.com/emqx/emqx-management/pull/7)

#### emqx-web-hook

修复加载 emqx\_web\_hook 错误

#### emqx-dashboard

  - 修复 Dashboard -\> OverView 中 disconnect 统计数据显示

  - 在 Dashboard -\> Websocket 新增 WebSocket Path 参数
    
    GitHub issues:
    [emqx/emqx-dashboard\#5](https://github.com/emqx/emqx-dashboard/pull/5)

#### emqx-retainer

  - Retained 消息新增 TTL
    
    GitHub issues:
    [emqx/emqx-retainer\#52](https://github.com/emqx/emqx-retainer/issues/52)

#### emqx-coap

  - 新增 emqx\_coap 插件
    
    GitHub issues:
    [emqx/emqx-coap\#5](https://github.com/emqx/emqx-coap/pull/86)
    [emqx/gen-coap\#5](https://github.com/emqx/gen_coap/pull/8)

#### emqx-docker

  - 优化 Dockerfile
    
    GitHub issues:
    [emqx/emqx-docker\#5](https://github.com/emqx/emqx-docker/pull/71)

#### esockd

  - 改进 esockd\_connection\_sup 模块设计
    
    GitHub issues:
    [emqx/esockd\#86](https://github.com/emqx/esockd/pull/86)

## 3.0-Beta.2 版本

* 发布日期: 2018-09-10*

EMQ X 3.0-Beta.2 版本主要包含了对 MQTT 5.0 新特性的改进，以及问题修复。

#### EMQ X Core

功能改进:

  - 支持 MQTT 5.0'subscription options'
    
    GitHub issues:
    [emqx/emqx\#1788](https://github.com/emqx/emqx/pull/1788),
    [emqx/emqx-retainer\#58](https://github.com/emqx/emqx-retainer/pull/58),
    [emqx/emqx\#1803](https://github.com/emqx/emqx/pull/1803)

  - 增加对 MQTT 5.0 'Topic-Alias' 的校验
    
    GitHub issues:
    [emqx/emqx\#1789](https://github.com/emqx/emqx/pull/1789),
    [emqx/emqx\#1802](https://github.com/emqx/emqx/pull/1802)

  - 改进 hooks 的设计
    
    GitHub issue:
    [emqx/emqx\#1790](https://github.com/emqx/emqx/pull/1790)

  - 将模块 'emqx\_mqtt\_properties' 重命名为 'emqx\_mqtt\_props'
    
    GitHub issue:
    [emqx/emqx\#1791](https://github.com/emqx/emqx/pull/1791)

  - 改进 emqx\_zone
    
    GitHub issue:
    [emqx/emqx\#1795](https://github.com/emqx/emqx/pull/1795)

问题修复:

  - 修复了 'Will Delay Interval' 属性处理错误
    
    GitHub issues:
    [emqx/emqx\#1800](https://github.com/emqx/emqx/pull/1800),
    [emqx/emqx-delayed-publish\#3](https://github.com/emqx/emqx-delayed-publish/pull/3)

  - 修复了 'Reserved' 标志位的处理错误
    
    GitHub issue:
    [emqx/emqx\#1783](https://github.com/emqx/emqx/pull/1783)

  - 为单元测试生成配置文件
    
    GitHub issue:
    [emqx/emqx\#1794](https://github.com/emqx/emqx/pull/1794)

#### emqx-management (插件)

功能改进:

  - 增加 'banned' 功能的 restful API
    
    GitHub issue:
    [emqx/emqx-management\#6](https://github.com/emqx/emqx-management/pull/6)

#### emqx-delayed-publish (插件)

功能改进:

  - 重构代码
    
    GitHub issue:
    [emqx/emqx-delayed-publish\#4](https://github.com/emqx/emqx-delayed-publish/pull/4)

#### minirest (依赖工程)

功能改进:

  - 回调函数里，同时传递 query 参数和 body 参数
    
    GitHub issue:
    [emqx/minirest\#4](https://github.com/emqx/minirest/pull/4)

#### emqx-rel (编译工程)

功能改进:

  - 编译时检查 OTP 版本
    
    GitHub issue:
    [emqx/emqx-rel\#217](https://github.com/emqx/emqx-rel/pull/217)

## 3.0-Beta.1 版本

* 发布日期: 2018-09-02* 版本别名: The Promise of Tomorrow

3.0-beta.1 版本正式发布。兼容 MQTT-3.1.1 协议的同时， 完整支持 MQTT-5.0 协议。
此外还增加了很多实用的功能特性，重构了核心组件，提升了系统的伸缩性和扩展能力。

#### 全面支持 MQTT-5.0

EMQX 3.0 版本实现了大多数的 MQTT-5.0 特性，主要的 MQTT-5.0 新特性一览:

  - 增加了新的 MQTT 控制报文类型: AUTH
    
    MQTT-5.0 里新增加了一个 AUTH 类型的报文，用来实现相对复杂的认证交互流程。

  - Session 过期机制
    
    之前版本的 "Clean session flag" 现在拆分成了两个字段: "Clean Start Flag"，"Session
    Expiry Interval"。

  - Message 过期机制
    
    MQTT-5.0 里，在发布消息时允许设置一个消息过期时间。

  - 所有的 ACK 都可携带 Reason Code
    
    MQTT-5.0 里，所有的回复报文都包含 Reason Code 字段。现在终端可以知道一个请求失败的原因了。

  - 所有的 ACK 都可携带 Reason String
    
    除了 Reason Code 之外，所有的回复报文都可包含一个 Reason String。

  - Server 端主动断开
    
    MQTT-5.0 里，Server 端可以主动断开一个连接了。

  - Payload format and content type
    
    MQTT-5.0 里发消息的时候，可以指定 Payload 类型和一个 MIME 风格的 content type。

  - Request/Response 模式
    
    增加了几个 property，来规范使用 MQTT 协议做 Request/Response 模式的交互。

  - 共享订阅
    
    EMQ X 2.x 支持单节点的共享订阅。 现在 EMQ X 3.0 支持了整个集群范围内的共享订阅。

  - 订阅 ID
    
    有了这个订阅 ID，终端可以获知某条消息是由哪个订阅来的。

  - Topic 别名
    
    Topic 现在可以有一个整型的别名，这可以降低 MQTT 由于长 Topic 导致的网络交互损耗。

  - 用户自定义的 User properties
    
    MQTT-5.0 里，多数报文都可以携带 User properties。

  - 报文大小限制
    
    EMQ X 2.x 里可以配置 Broker 端的最大报文限制，过大的报文会被丢弃，然后 Broker 会将连接断开。MQTT-5.0
    里，通过 CONNECT/CONNECT ACK 报文，客户端和 Broker 端都可以指定最大报文限制。

  - 可选的 Server 端能力通知 (TODO)
    
    Broker 端可以定义不支持的特性，并将其通知给终端。

  - 订阅选项
    
    MQTT-5.0 提供了一些订阅选项，主要是为了桥接的应用。 比如 nolocal，和 retained 消息处理相关的选项。

  - Will delay
    
    MQTT-5.0 允许指定一个时延，定义从连接断开到遗嘱消息被发送出去之前的延时。这样可以避免在短暂的网络断开和波动时发出遗嘱消息。

  - Broker 端的保活设置
    
    MQTT-5.0 里，Broker 端可以指定一个期望终端使用的保活时间。

  - Assigned ClientID
    
    MQTT-5.0 里，如果 ClientID 是 Broker 分配的，服务端需要返回这个 ClientID 给终端。

  - Server reference
    
    MQTT-5.0 里，Broker 可以指定一个另外一个 Broker 让终端使用。可以用来做连接重定向。

#### 集群架构演进

EQMX 3.0 引入了伸缩性较强的 RPC 机制，现在单集群可以支持千万级别的并发连接:

    --------               --------

>  EMQX EMQX |
> 
> | Ekka Ekka | | Mnesia Mnesia | | Kernel Kernel | -------- --------

  - 引入 Ekka 以实现集群的自动建立和自动恢复。目前支持以下几种集群建立方式:
    
      - manual: 手动加入集群；
      - static: 使用预置的节点列表自动组建集群；
      - mcast: 使用广播自动建立集群；
      - dns: 使用 DNS A 记录自动建立集群；
      - etcd: 使用 etcd 自动建立集群；
      - k8s: 使用 k8s 自动建立集群。

#### 消息速率限制

3.0 引入了消息速率限制，以增加 Broker 的可靠性。在 MQTT TCP 或 SSL 监听器配置里，可以配置:

  - 并发连接数量: max\_clients
  - 连接速率限制: max\_conn\_rate
  - 连接流量限制: rate\_limit
  - 发布速率限制: max\_publish\_rate

#### Feature improvements and Bug Fixes

  - 更新了 esockd;
  - 改用 cowboy 以提升 HTTP 连接的性能；
  - 重构了 ACL 缓存机制；
  - 增加本地和远程 MQTT 桥接功能；
  - 配置文件引入 "zone" 的概念，不同的 "zone" 可以使用不同的配置；
  - 重构了 session 模块，减少了节点间的内存拷贝，提升了节点间通信效率；
  - 改进了 OpenLDAP 的 Access Control;
  - 增加了延时发布功能；
  - 增加了支持 Prometheus 的新的监控和统计功能；
  - 改进了 hook 机制。

## 2.3.11 版本

* 发布日期: 2018-07-23*

#### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

#### emq-auth-jwt

Fix token verification with jwerl 1.0.0

#### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo\#123)

Support both clientid and username variables in all queries.
(emq-auth-mongo\#123)

## 2.3.10 版本

* 发布日期: 2018-06-27*

#### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

#### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9 版本

* 发布日期: 2018-05-20*

#### Bugfix and Enhancements

Bugfix: check params for REST publish API (\#1599)

Upgrade the mongodb library to v3.0.5

#### esockd

Bugfix: proxy protocol - set socket to binary mode (\#78)

## 2.3.8 版本

* 发布日期: 2018-05-11*

#### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq\_auth\_username (\#1588)

Bugfix: Should be an info level when change CleanSession (\#1590)

Bugfix: emqttd\_ctl crashed when emq\_auth\_usename doesn't exist
(\#1588)

#### emq-auth-mongo

Improve: Support authentication database (authSource) (\#116)

## 2.3.7 版本

* 发布日期: 2018-04-22*

#### Bugfix and Enhancements

Bugfix: fixed spec of function setstats/3 (\#1575)

Bugfix: clean dead persistent session on connect (\#1575)

Bugfix: dup flag not set when re-deliver (\#1575)

Bugfix: Upgrade the lager\_console\_backend config (\#1575)

Improve: Support set k8s namespace (\#1575)

Upgrade the ekka library to v0.2.3 (\#1575)

Improve: move PIPE\_DIR dir from /tmp/${WHOAMI}\_erl\_pipes/$NAME/to
/$RUNNER\_DATA\_DIR/${WHOAMI}\_erl\_pipes/$NAME/ (emq-relx\#188)

#### emq-auth-http

Improve: Retry 3 times when httpc:request occurred
socket\_closed\_remotely error (emq-auth-http\#70)

## 2.3.6 版本

* 发布日期: 2018-03-25*

#### Bugfix and Enhancements

Security: LWT message checking the ACL (\#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions
(\#1529)

#### emq-auth-jwt

Validate JWT token using a expired field (\#29)

## 2.3.5 版本

* 发布日期: 2018-03-03*

#### Bugfix and Enhancements

Feature: Add etc/ssl\_dist.conf file for erlang SSL distribution
(emq-relx\#178)

Feature: Add node.ssl\_dist\_optfile option and etc/ssl\_dist.conf file
(\#1512)

Feature: Support Erlang Distribution over TLS (\#1512)

Improve: Tune off the 'tune\_buffer' option for external MQTT
connections (\#1512)

#### emq-sn

Clean registered topics if mqtt-sn client send a 2nd CONNECT in
connected state (\#76)

Upgrade the esockd library to v5.2.1 (\#76)

#### emq-auth-http

Remove 'password' param from ACL and superuser requests (\#66)

## 2.3.4 版本

* 发布日期: 2018-01-29*

#### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket
(\#1335)

Feature: EMQ node.name with link local ipv6 address not responding to
ping (\#1460)

Feature: Add PROTO\_DIST\_ARG flag to support clustering via IPv6
address. (\#1460)

Bugfix: retain bit is not set when publishing to clients (when it should
be set). (\#1461)

Bugfix: Can't search topic on web dashboard (\#1473)

#### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn\#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn\#66)

## 2.3.3 版本

* 发布日期: 2018-01-08*

#### Bugfix and Enhancements

Add a full documentation for emq.conf and plugins.

Repair a dead link in README - missing emq-lwm2m. (\#1430)

Subscriber with wildcard topic does not receive retained messages with
sub topic has $ sign (\#1398)

Web Interface with NGINX Reverse Proxy not working. (\#953)

#### emq-dashboard

Add dashboard.default\_user.login, dashboard.default\_user.password
options to support configuring default admin.

#### emq-modules

The emq-modules rewrite config is not right. (\#35)

#### emq-docker

Upgrade alpine to 3.7 (\#31)

#### emq-packages

Support ARM Platform (\#12)

## 2.3.2 版本

* 发布日期: 2017-12-26*

#### Bugfix and Enhancements

Support X.509 certificate based authentication (\#1388)

Add proxy\_protocol, proxy\_protocol\_timeout options for ws/wss
listener.

Cluster discovery etcd nodes key must be created manually. (\#1402)

Will read an incorrect password at the last line of
emq\_auth\_username.conf (\#1372)

How can I use SSL/TLS certificate based client authentication? (\#794)

Upgrade the esockd library to v5.2.

#### esockd

Improve the parser of proxy protocol v2.

Add'send\_timeout', 'send\_timeout\_close' options.

Rename esockd\_transport:port\_command/2 function to async\_send/2.

Add test case for esockd\_transport:async\_send/2 function.

Add esockd\_transport:peer\_cert\_subject/1, peer\_cert\_common\_name/1
functions.

#### emq-auth-mysql

Update depends on emqtt/mysql-otp.

Fixed the issue that Cannot connect to MySQL 5.7 (\#67).

#### emq-relx

Fix mergeconf/3 appending line break error. (\#152)

#### emq-sn

Fix crash in emq\_sn\_gateway:transform () function which handles SUBACK.
(\#57)

Define macro SN\_RC\_MQTT\_FAILURE. (\#59)

#### emq-web-hook

Filter auth\_failure client for disconnected hook. (\#30)

## 2.3.1 版本

* 发布日期: 2017-12-03*

#### Bugfix and Enhancements

Remove the unnecessary transactions to optimize session management.

Should not exit arbitrarily when clientid conflicts in mnesia.

Change the default value of 'mqtt.session.enable\_stats' to 'on'.

The DUP flag should be set to 0 for all QoS0 messages. (emqttd\#1319)

Fix the 'no function clause' exception. (emqttd\#1293)

The retained flags should be propagated for bridge. (emqttd\#1293)

The management API should listen on 0.0.0.0:8080. (emqttd\#1353)

Fast close the invalid websocket in init/1 function.

erlang:demonitor/1 the reference when erasing a monitor. (emqttd\#1340)

#### emq-retainer

Don't clean the retain flag after the retained message is stored.

Add three CLIs for the retainer plugin. (emq-retainer\#38)

#### emq-dashboard

Refactor (priv/www): improve the routing page. (emq-dashboard\#185)

#### emq-modules

Turn off the subscription module by default. (emq-modules\#26)

#### emq-sn

Add an integration test case for sleeping device.

Do not send will topic if client is kicked out.

Prevent crash information in log when emq\_sn\_gateway getting timeout,
since it is a possible procedure.

#### emq-relx

Support node cookie value with = characters. (emq-relx\#146)

#### mochiweb

Improve Req:get (peername) funciton to support x-forwarded-for and
x-remote-port. (emqtt/mochiweb\#9)

## 2.3.0 版本 "Passenger's Log"

* 发布日期: 2017-11-20*

EMQ 2.3.0 版本正式发布，改进了 PubSub 设计与消息路由性能，更新 EMQ 自带的自签名 SSL 证书，改进 Dashboard
界面与 API 设计。

#### Bugfix and Enhancements

Fix the issue that Retained message is not sent for Subscribe to
existing topic. (emqttd\#1314)

Fix the issue that The DUP flag MUST be set to 0 for all QoS0
messages.(emqttd\#1319)

Improve the pubsub design and fix the race-condition issue.
(emqttd\#PR1342)

Crash on macOS High Sierra (emqttd\#1297)

#### emq-dashboard Plugin (emq-dashboard\#PR174)

Upgraded the'subscriptions' RESTful API.

Improvement of the auth failure log. (emq-dashboard\#59)

#### emq-coap Plugin (emq-coap\#PR61)

Replaced coap\_client with er\_coap\_client.

Fix: correct the output format of coap\_discover () to enable
".well-known/core".

Refactor the coap\_discover method.

#### emq-relx

Upgraded the bin/nodetool script to fix the rpcterms command.

#### emq-web-hook Plugin

Fix the emq\_web\_hook plugin getting username from client.connected
hook. (emq-web-hook\#19)

#### emq-auth-jwt Plugin (emq-auth-jwt\#PR15)

Added test cases for emq\_auth\_jwt.

Fix jwt:decode/2 functions's return type.

#### emq-auth-mongo Plugin (emq-auth-mongo\#PR92)

Updated the default MongoDB server configuration.

## 2.3-rc.2 版本

* 发布日期: 2017-10-22*

#### Bugfix

Change the default logging level of trace CLI. (emqttd\#1306)

#### emq-dashboard Plugin (emq-dashboard\#164)

Fix the 'Status' filters of plugins's management.

Fix the URL Redirection when deleting an user.

Compatible with IE,Safari,360 Browsers.

## 2.3-rc.1 版本

* 发布日期: 2017-10-12*

#### Bugfix

Fix the issue that invalid clients can publish will message.
(emqttd\#1230)

Fix Dashboard showing no stats data (emqttd\#1263)

Fix a rare occurred building failure (emqttd\#1284)

Support Persistence Logs for longer time (emqttd\#1275)

Fix for users APIs (emqttd\#1289)

Changed passwd\_hash/2 function's return type (emqttd\#1289)

#### emq-dashboard Plugin (emq-dashboard\#154)

Improved the Dashboard Interface of Monitoring/Management/Tools.

Allow switching dashboard themes.

Supoort both EN and CN languages.

## 2.3-beta.4 版本

* 发布日期: 2017-09-13*

#### Highlights

Released a new sexy dashboard.

Add more RESTful APIs for manangement and monitoring.

Configuring the broker through CLI or API without having to restart.

#### Bugfix

Job for emqttd.service failed because the control process exited with
error code. (emqttd\#1238)

Travis-CI Build Failing (emqttd\#1221)

Https listener of Dashboard plugin won't work (emqttd\#1220)

Service not starting on Debian 8 Jessie (emqttd\#1228)

#### emq-dashboard

1.  Support switching to other clustered node.
2.  Configure and reboot the plugins on the dashboard.
3.  A login page to replace the basic authentication popup window.

#### emq-coap

1.Try to clarify the relationship between coap and mqtt in EMQ.
(emq-coap\#54).

2.Fix crashes in coap concurrent test (gen-coap\#3).

## 2.3-beta.3 版本

* 发布日期: 2017-08-21*

## 2.3-beta.3 版本

* 发布日期: 2017-08-21*

#### Enhancements

Add HTTP API for hot configuration.

#### Bugfix

1.  Parse 'auth.mysql.password\_hash' error when hot configuration
    reload (emq-auth-mysql\#68)
2.  Set 'auth.pgsql.server' error when hot configuration reload
    (emq-auth-pgsql\#67)
3.  Set 'auth.redis.server' and 'auth.redis.password\_hash' error when
    hot configuration reload (emq-auth-redis\#47)
4.  Fix the issue that when deleting retained message subscribed clients
    are not notified (emqttd\#1207)
5.  Support more parameters for hot configuration reload:

<!-- end list -->

  - mqtt.websocket\_protocol\_header = on
  - mqtt.mqueue.low\_watermark = 20%
  - mqtt.mqueue.high\_watermark = 60%
  - mqtt.client.idle\_timeout = 30s
  - mqtt.client.enable\_stats = off

## 2.3-beta.2 版本

* 发布日期: 2017-08-12*

EMQ R2.3-beta.2 版本发布！该版本新增 HTTP 管理 API，支持配置 Keepalive 检测周期，支持配置参数热更新。

目前支持配置热更新的插件有:

  - emq-stomp
  - emq-coap
  - emq-sn
  - emq-lwm2m
  - emq-retainer
  - emq-recon
  - emq-web-hook
  - emq-auth-jwt
  - emq-auth-http
  - emq-auth-mongo
  - emq-auth-mysql
  - emq-auth-pgsql
  - emq-auth-redis

<div class="note">

<div class="admonition-title">

Note

</div>

为支持命令行更新配置参数，部分认证插件参数值采用 ',' 替代了空格分隔符。

</div>

#### Enhancements

1.  Introduce new HTTP management API.
2.  Add ClientId parameter for HTTP Publish API.
3.  Allow configuring keepalive backoff.
4.  Remove the fullsweep\_after option to lower CPU usage.
5.  Authorize HTTP Publish API with clientId.

#### emq-sn Plugin (emq-sn\#49)

1.  Support CONNECT message in
    connected/wait\_for\_will\_topic/wait\_for\_will\_msg states.
2.  Clean registered topic for a restarted client.
3.  Bug fix of not clearing buffered PUBLISH messages received during
    asleep state as those messages are sent to client when client wakes
    up.

#### emq-auth-ldap Plugin (emq-auth-ldap\#21)

Improve the design LDAP authentication.

#### emq-coap Plugin (emq-coap\#51)

Support CoAP PubSub Specification
(<https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt>)

## 2.3-beta.1 版本

* 发布日期: 2017-07-24*

EMQ R2.3-beta.1 版本发布！该版本正式支持集群节点自动发现与集群脑裂自动愈合，支持基于 IP
Multicast、Etcd、Kubernetes 等多种策略自动构建集群。

#### 节点发现与自动集群

EMQ R2.3 版本支持多种策略的节点自动发现与集群:

| 策略     | 说明               |
| ------ | ---------------- |
| static | 静态节点列表自动集群       |
| mcast  | UDP 组播方式自动集群      |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群       |
| k8s    | Kubernetes 服务自动集群 |

#### 集群脑裂与自动愈合

EMQ R2.3 版本正式支持集群脑裂自动愈合 (Network Partition Autoheal):

``` sourceCode properties
cluster.autoheal = on
```

集群脑裂自动恢复流程:

1.  节点收到 Mnesia 库的 'inconsistent\_database' 事件 3 秒后进行集群脑裂确认；
2.  节点确认集群脑裂发生后，向 Leader 节点 (集群中最早启动节点) 上报脑裂消息；
3.  Leader 节点延迟一段时间后，在全部节点在线状态下创建脑裂视图 (SplitView)；
4.  Leader 节点在多数派 (Majority) 分区选择集群自愈的 Coordinator 节点；
5.  Coordinator 节点重启少数派 (minority) 分区节点恢复集群。

#### 节点宕机与自动清除

EMQ R2.3 版本支持从集群自动删除宕机节点 (Autoclean):

``` sourceCode properties
cluster.autoclean = 5m
```

#### LWM2M 协议支持

EMQ R2.3
版本正式支持 LWM2M 协议网关，实现了 LWM2M 协议的大部分功能。MQTT 客户端可以通过 EMQ-LWM2M 访问支持 LWM2M 的设备。设备也可以往 EMQ-LWM2M 上报 notification，为 EMQ 后端的服务采集数据。

LWM2M 是由 Open Mobile
Alliance (OMA) 定义的一套适用于物联网的协议，它提供了设备管理和通讯的功能。LWM2M 使用 CoAP 作为底层的传输协议，承载在 UDP 或者 SMS 上

#### JSON Web Token 认证支持

EMQ R2.3 版本支持基于 JWT (JSON Web Token) 的 MQTT 客户端认证。

#### Retainer 插件

Retainer 插件支持 'disc\_only' 模式存储 retained 消息。

#### Debian 9 安装包

EMQ R2.3 支持 Debian 9 系统安装包。

#### Erlang/OTP R20

EMQ R2.3 版本兼容 Erlang/OTP R20，全部程序包基于 Erlang/OTP R20 构建。

## 2.2 正式版 "Nostalgia"

* 发布日期: 2017-07-08*

* 版本别名: Nostalgia*

EMQ-2.2.0 版本正式发布！EMQ R2.2 版本完整支持 CoAP (RFC 7252)、MQTT-SN 协议，支持 Web Hook、Lua
Hook、Proxy Protocol V2，支持 Elixir 语言插件开发。

Feature: Add 'listeners restart/stop' CLI command (emqttd\#1135)

Bugfix: Exit Code from emqttd\_ctl (emqttd\#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd\#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard\#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed
(emq-coap\#43)

## 2.2-rc.2 版本

* 发布日期: 2017-07-03*

<div class="warning">

<div class="admonition-title">

Warning

</div>

2.2-rc.2 版本源码编译需要 Erlang/OTP R19.3+

</div>

#### 问题与改进

Compatible with Erlang/OTP R20 (emq-relx\#77)

CoAP gateway plugin supports coap-style publish & subscribe pattern.
(emq\_coap\#33)

MQTT-SN gateway plugin supports sleeping device (emq\_sn\#32)

Upgrade esockd and mochiweb libraries to support restarting a listener

## 2.2-rc.1 版本

* 发布日期: 2017-06-14*

#### 问题与改进

Add a new listener for HTTP REST API (emqttd\#1094)

Fix the race condition issue caused by unregister\_session/1
(emqttd\#1096)

Fix the issue that we cannot remove a down node from the cluster
(emqttd\#1100)

Passed org.eclipse.paho.mqtt\_sn.testing/interoperability tests
(emq\_sn\#29)

Fix the issue that send http request and return non-200 status code, but
AUTH/ACL result is denied (emq-auth-http\#33)

Fix the issue that fail to stop listener (emq\_stomp\#24)

Support using systemctl to manage emqttd service on CentOS

## 2.2-beta.3 版本

* 发布日期: 2017-05-27*

#### 问题与改进

Call emit\_stats when force GC (emqttd\#1071)

Update the default value of 'mqtt.mqueue.max\_length' to 1000
(emqttd\#1074)

Update emq-auth-mongo READEME (emq-auth-mongo\#66)

Update default password field (emq-auth-mongo\#67)

Upgrade the mongodb library to v3.0.3

Remove ‘check password===undefined && userName\!== undefined’
(emq-dashboard\#120)

#### emq\_auth\_redis 插件

认证支持 HGET 查询

#### emq\_auth\_mongo 插件

支持 mongodb 集群、Replica Set

#### 文档更新

更新 Windows 源码编译安装

## 2.2-beta.2 版本

* 发布日期: 2017-05-20*

#### 问题与改进

Add a 'websocket\_protocol\_header' option to handle WebSocket
connection from WeChat (emqttd\#1060)

Assign username and password to MQTT-SN's CONNECT message (emqttd\#1041)

Allow for Content-Type:application/json in HTTP Publish API
(emqttd\#1045)

emqttd\_http.erl:data conversion (emqttd\#1059)

Seperate emq\_sn from emqttd (emq-sn\#24)

Check St0's type, making it easier to debug crash problems
(emq-lua-hook\#6)

Fix error: load xxx.lua (emq-lua-hook\#8)

Leave luerl alone as a rebar project (emq-lue-hook\#9)

Display websocket data in reverse order (emq-dashboard\#118)

priv/www/assets/js/dashboard.js:Fixed a typo (emq-dashboard\#118)

#### Update README

Update README of emq-auth-pgsql: add the'ssl\_opts' configuration
(emq-auth-pgsql\#56)

Update README of emq-auth-mysql: fix the 'passwd\_hash' typo
(emq-auth-mysql\#54)

Update README of emq-auth-mongo: change 'aclquery' to 'acl\_query'
(emq-auth-mongo\#63)

#### Elixir Plugin

Add a new plugin
[emq-elixir-plugin](https://github.com/emqtt/emq-elixir-plugin) to
support Elixir language.

## 2.2-beta.1 版本

* 发布日期: 2017-05-05*

*EMQ* 2.2-beta.1 版本正式发布！EMQ2.2 版本发布主要新功能包括:

1.  支持 MQTT 协议多监听器配置，支持 HAProxy 的 Proxy Protocol V1/V2
2.  新增 Web Hook 插件 (emq-web-hook)、Lua Hook 插件 (emq-lua-hook)

#### MQTT 协议监听器配置

一个 EMQ 节点可配置多个 MQTT 协议监听端口，例如下述配置 external, internal 监听器，分别用于设备连接与内部通信:

    -------

>   - \-- Ex，支持 Web Hook、Lua Hook、ernal TCP 1883 --\> | |
>     
>     EMQ | -- Internal TCP 2883 --\> Service
> 
>   - \-- External SSL 8883--\> | |
>     
>     -----

EMQ 2.2 版本 etc/emq.conf 监听器配置方式:

    listener.tcp.${name}= 127.0.0.1:2883
    
    listener.tcp.${name}.acceptors = 16
    
    listener.tcp.${name}.max_clients = 102400

#### Proxy Protocol V1/2 支持

EMQ 集群通常部署在负载均衡器 (LB) 后面，典型架构:

    -----
    |   |
    | L | --TCP 1883--> EMQ

>   - \--SSL 8883--\> | | |
>     
>     B | --TCP 1883--\> EMQ  
>       |
>     
>     -----

HAProxy、NGINX 等常用的负载均衡器 (LB)，一般通过 Proxy Protocol 协议传递 TCP 连接源地址、源端口给 EMQ。

EMQ 2.2 版本的监听器开启 Proxy Protocol 支持:

    ## Proxy Protocol V1/2
    ## listener.tcp.${name}.proxy_protocol = on
    ## listener.tcp.${name}.proxy_protocol_timeout = 3s

#### Web Hook 插件

新增 WebHook 插件: [emq-web-hook](https://github.com/emqtt/emq-web-hook)
，支持在 MQTT 客户端上下线、消息发布订阅时触发 WebHook 回调。

#### Lua Hook 插件

新增 Lua Hook 插件: [emq-lua-hook](https://github.com/emqtt/emq-lua-hook)
，支持 Lua 脚本注册 EMQ 扩展钩子来开发插件。

#### 改进认证链设计

EMQ 2.2 版本改进认证链设计，当前认证模块返回 ignore (例如用户名不存在等情况下)，认证请求将继续转发后面认证模块:

    -------------           ------------           -------------

>   - Client --\> | Redis 认证 | -ignore-\> | HTTP 认证 | -ignore-\> | MySQL 认证
>     |
>     
>       - \------------- ------------ ------------- | | |  
>         |/ |/ |/
>     
>     allow | deny allow | deny allow | deny

#### 支持 bcrypt 密码 Hash

EMQ 2.2 版本支持 bcrypt 密码 Hash 方式，例如 Redis 认证插件配置:

    auth.redis.password_hash = bcrypt

#### etc/emq.conf 配置变更

'mqtt.queue.*' 配置变更为 'mqtt.mqueue.*'

#### emq-dashboard

WebSocket 页面支持 Unsubscribe

## 2.1.2 版本

* 发布日期: 2017-04-21*

Fix emqttd\_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd\#1000)

Fix crash caused by duplicated PUBREC packet (emqttd\#1004)

Unload the'session.created' and'session.teminated' hooks
(emq-plugin-template)

## 2.1.1 版本

* 发布日期: 2017-04-14*

Localhost:8083/status returns 404 when AWS LB check the health of EMQ
(emqttd\#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard\#105)

Fix mqtt-sn Gateway not working (emq-sn\#12)

Upgrade emq-sn Plugin (emq-sn\#11)

Upgrade emq-coap Plugin (emq-coap\#21)

## 2.1.0 版本

* 发布日期: 2017-04-07*

The stable release of 2.1 version.

Trouble with auth.mysql.acl\_query (emq-auth-mysql\#38)

Filter the empty fields in ACL table (emq-auth-mysql\#39)

## 2.1.0-rc.2 版本

* 发布日期: 2017-03-31*

Support pbkdf2 hash (emq-auth-mongo\#46)

Kickout the conflict WebSocket connection (emqttd\#963)

Correct licence in app.src (emqttd\#958)

SSL options to connect to pgsql (emq-auth-pgsql\#41)

## 2.1.0-rc.1 版本

* 发布日期: 2017-03-24*

EMQ fails to start if run under a different linux user than that which
first ran it (emqttd\#842)

Depend on emqtt/pbkdf2 to fix the building errors of Travis CI
(emqttd\#957)

Depend on goldrush and emqtt/pbkdf2 to resolve the building errors
(emqttd\#956)

Fix 'rebar command not found' (emq-relx\#33)

Compile error in v2.1.0-beta.2 (emq-relx\#32)

Support salt with passwords (emq-auth-mongo\#11)

Change the default storage\_type to 'ram' (emq-retainer\#13)

## 2.1.0-beta.2 版本

* 发布日期: 2017-03-13*

Cannot find AwaitingAck (emqttd\#597)

EMQ V2.1 crash when public with QoS = 2 (emqttd\#919)

Support pbkdf2 hash (emqttd\#940)

Add src/emqttd.app.src to be compatible with rebar3 (emqttd\#920)

Add more test cases (emqttd\#944)

CRASH REPORT Process \<0.1498.0\> with 0 neighbours crashed with reason:
{ssl\_error,{tls\_alert,"certificate unknown"}} in
esockd\_connection:upgrade (emqttd\#915)

'auth.redis.password\_hash = plain' by default (emq-auth-redis\#20)

## 2.1.0-beta.1 版本

* 发布日期: 2017-02-24*

*EMQ* 2.1.0-beta.1 版本发布。

<div class="warning">

<div class="admonition-title">

Warning

</div>

2.1.x 版本源码编译需要 Erlang/OTP R19+

</div>

EMQ 正式采用 [Semantic Versioning 2.0.0](http://semver.org)
规范创建发布版本号，按 'Tick-Tock' 方式按月发布迭代版本。奇数版本问题修复与性能改进，偶数版本架构改进和新功能布。

#### GC 优化

1.  WebSocket、Client、Session 进程空置一段时间后自动 Hibernate 与 GC。
2.  新增 'mqtt.conn.force\_gc\_count' 配置，Client、Session 进程处理一定数量消息后强制 GC。
3.  大幅降低 WebSocket、Client、Session 进程 fullsweep\_after 设置，强制进程深度 GC。

#### API 改进

Hooks API 支持注册带 Tag 的回调函数，解决相同模块函数多次 Hook 注册问题。

#### 问题修复

emqttd\#916: Add 'mqtt\_msg\_from ()' type

emq-auth-http\#15: ACL endpoint isnt called

## 2.1-beta 版本

* 发布日期: 2017-02-18*

EMQ v2.1-beta 版本正式发布，改进 Session/Inflight 窗口设计，一个定时器负责全部 Inflight
QoS1/2 消息重传，大幅降低高消息吞吐情况下的 CPU 占用。

#### Client, Session 统计信息

支持对单个 Client、Session 进程进行统计，etc/emq.conf 配置文件中设置 'enable\_stats' 开启:

    mqtt.client.enable_stats = 60s
    
    mqtt.session.enable_stats = 60s

#### 新增 missed 统计指标

EMQ 收到客户端 PUBACK、PUBREC、PUBREL、PUBCOMP 报文，但在 Inflight 窗口无法找到对应消息时，计入 missed 统计指标:

    packets/puback/missed
    
    packets/pubrec/missed
    
    packets/pubrel/missed
    
    packets/pubcomp/missed

#### Syslog 日志集成

支持输出 EMQ 日志到 Syslog，etc/emq.config 配置项:

    ## Syslog. Enum: on, off
    log.syslog = on
    
    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

#### Tune QoS 支持

支持订阅端升级 QoS，etc/emq.conf 配置项:

    mqtt.session.upgrade_qos = on

#### 'acl reload' 管理命令

Reload acl.conf without restarting emqttd service (\#885)

#### 配置项变更

1.  变更 mqtt.client\_idle\_timeout 为 mqtt.client.idle\_timeout
2.  新增 mqtt.client.enable\_stats 配置项
3.  新增 mqtt.session.upgrade\_qos 配置项
4.  删除 mqtt.session.collect\_interval 配置项
5.  新增 mqtt.session.enable\_stats 配置项
6.  变更 mqtt.session.expired\_after 为 mqtt.session.expiry\_interval

#### 合并扩展模块到 emq\_modules 项目

合并 emq\_mod\_presence, emq\_mod\_subscription,
emq\_mod\_rewrite 到 emq\_modules 项目

变更 emq\_mod\_retainer 为 emq\_retainer 项目

#### Dashboard 插件

Overview 页面增加 missed 相关统计指标。 Client 页面增加 SendMsg、RecvMsg 统计指标。
Session 页面增加 DeliverMsg、EnqueueMsg 指标。

#### recon 插件

变更 recon.gc\_interval 配置项类型为 duration

#### reloader 插件

变更 reloader.interval 配置项类型为 duration

## 2.0.7 版本

* 发布日期: 2017-01-20*

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB
Packages.

emq-auth-http\#9: Update the priv/emq\_auth\_http.schema,
cuttlefish:unset () if no super\_req/acl\_req config exists

emq-auth-mongo\#31: cuttlefish:unset () if no ACL/super config exists

emq-dashboard\#91: Fix the exception caused by binary payload

emq-relx\#21: Improve the bin\\emqttd.cmd batch script for windows

emqttd\#873: Documentation: installing-from-source

emqttd\#870: Documentation: The word in Documents is wrong

emqttd\#864: Hook 'client.unsubscribe' need to handle'stop'

emqttd\#856: Support variables in etc/emq.conf: {{ runner\_etc\_dir }},
{{ runner\_etc\_dir }}, {{ runner\_data\_dir }}

## 2.0.6 版本

* 发布日期: 2017-01-08*

Upgrade the [esockd](https://github.com/emqtt/esockd) library to v4.1.1

esockd\#41: Fast close the TCP socket if ssl:ssl\_accept failed

emq-relx\#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo\#31: Mongodb ACL Cannot work?

## 2.0.5 版本

* 发布日期: 2016-12-24*

emq-auth-http\#9: Disable ACL support

emq-auth-mongo\#29: Disable ACL support

emq-auth-mongo\#30: {datatype, flag}

## 2.0.4 版本

* 发布日期: 2016-12-16*

emqttd\#822: Test cases for SSL connections

emqttd\#818: trap\_exit to link WebSocket process

emqttd\#799: Can't publish via HTTPS

## 2.0.3 版本

* 发布日期: 2016-12-12*

emqttd\#796: Unable to forbidden tcp lisener

emqttd\#814: Cannot remove a 'DOWN' node from the cluster

emqttd\#813: Change parameters order

emqttd\#795: Fix metrics of websocket connections

emq-dashboard\#88: Rename the default topic from “/World” to “world”

emq-dashboard\#86: Lookup all online clients

emq-dashboard\#85: Comment the default listener port

emq-mod-retainer\#3: Retained messages get lost after EMQTT broker
restart.

## 2.0.2 版本

* 发布日期: 2016-12-05*

emqttd\#787: Stop plugins before the broker stopped, clean routes when a
node down

emqttd\#790: Unable to start emqttd service if username/password
contains special characters

emq-auth-clientid\#4: Improve the configuration of
emq\_auth\_clientid.conf to resolve emqttd\#790

emq-auth-username\#4: Improve the configuration of
emq\_auth\_username.conf to resolve emqttd\#790

## 2.0.1 版本

* 发布日期: 2016-11-30*

emqttd\#781: 更新项目 README 到 2.0 版本

emq\_dashboard\#84: 显示节点集群状态

emq\_dashboard\#79: 集群节点采用 disc\_copies 存储 mqtt\_admin 表

emq\_auth\_clientid: 集群节点采用 disc\_copies 存储 mqtt\_auth\_clientid 表

emq\_auth\_username: 集群节点采用 disc\_copies 存储 mqtt\_auth\_username 表

emq\_mod\_subscription\#3:
删除 emq\_mod\_subscription 表与 module.subscription.backend 配置

emq\_plugin\_template\#5: 插件停止时注销认证 / ACL 模块

## 2.0 正式版 "西湖以西"

* 发布日期: 2016-11-24*

* 版本别名：西湖以西 (West of West Lake)*

EMQ-2.0 版本正式发布！EMQ-1.0 版本产品环境下已支持 900K 并发连接，EMQ-2.0 版本重构了整个项目架构并正式支持共享订阅功能:

1.  支持共享订阅 (Shared Subscription) 与本地订阅 (Local
    Subscription)，解决 MQTT 协议负载平衡消费问题；
2.  支持 CoAP (RFC 7252)、MQTT-SN 协议和网关，支持 CoAP、MQTT-SN 客户端与 MQTT 客户端互通；
3.  重构配置文件格式与加载方式，支持用户友好的 'K = V' 文件格式，支持操作系统环境变量；
4.  增加了扩展钩子和大量的认证插件，支持与大部分数据库或 NoSQL 的认证集成；
5.  支持全平台编译部署，Linux/Unix/Windows 以及 ARM 平台网关，支持 Docker 镜像制作。

#### 共享订阅 (Shared Subscription)

共享订阅 (Shared Subscription) 支持在多订阅者间采用分组负载平衡方式派发消息:

    ---------
    |       | --Msg1--> Subscriber1

>   - Publisher--Msg1,Msg2,Msg3--\>| EMQ | --Msg2--\> Subscriber2
>     
>           | --Msg3--\> Subscriber3
>     
>     -----

使用方式：订阅者在主题 (Topic) 前增加 '$queue' 或 '$share/\<group\>/' 前缀。

#### 本地订阅 (Local Subscription)

本地订阅 (Local Subscription) 只在本节点创建订阅与路由表，不会在集群节点间广播全局路由，非常适合物联网数据采集应用。

使用方式：订阅者在主题 (Topic) 前增加 '$local/' 前缀。

#### erlang.mk 与 relx

2.0 版本分离 [emqttd](https://github.com/emqtt/emqttd) 主项目和发布项目
[emq-relx](https://github.com/emqtt/emq-relx), 采用
[erlang.mk](https://erlang.mk) 和 [relx](https://github.com/erlware/relx)
编译发布工具替换 1.x 版本使用的 rebar，项目可以跨平台在 Linux/Unix/Windows 系统下编译。

#### CoAP 协议支持

2.0 版本支持 CoAP 协议 (RFC7252)，支持 CoAP 网关与 MQTT 客户端互通。

CoAP 插件: <https://github.com/emqtt/emq_coap>

#### MQTT-SN 协议支持

2.0 版本支持 MQTT-SN 协议，支持 MQTT-SN 网关与 MQTT 客户端互通。

MQTT-SN 插件: <https://github.com/emqtt/emq_sn>

#### 'K = V' 格式配置文件

2.0 版本支持用户友好的 'K = V' 格式配置文件 etc/emq.conf:

    node.name = emqttd@127.0.0.1
    
    ...
    
    mqtt.listener.tcp = 1883
    
    ...

#### 操作系统环境变量

2.0 版本支持操作系统环境变量。启动时通过环境变量设置 EMQ 节点名称、安全 Cookie 以及 TCP 端口号:

    EMQ_NODE_NAME=emqttd@127.0.0.1
    EMQ_NODE_COOKIE=emq_dist_cookie
    EMQ_MAX_PORTS=65536
    EMQ_TCP_PORT=1883
    EMQ_SSL_PORT=8883
    EMQ_HTTP_PORT=8083
    EMQ_HTTPS_PORT=8084

#### Docker 镜像支持

EMQ-2.0 版本支持 Docker 镜像制作，Dockerfile 开源在:
<https://github.com/emqtt/emq_docker>

#### Windows 平台支持

2.0 版本完整支持 Windows 平台的编译、发布与运行，支持 Windows 平台下的 'emqttd\_ctl' 控制命令，支持在 Windows 节点间的集群。

#### 问题与改进

\#764: add mqtt.cache\_acl option

\#667: Configuring emqttd from environment variables

\#722: mqtt/superuser calls two times emqtt\_auth\_http

\#754: "-heart" option for EMQ 2.0

\#741: emq\_auth\_redis cannot use hostname as server
address

#### 扩展插件

2.0 版本发布的认证与扩展插件列表:

| 插件                                                                      | 说明                    |
| ----------------------------------------------------------------------- | --------------------- |
| [emq\_dashboard](https://github.com/emqtt/emqttd_dashboard)             | Web 控制台插件 (默认加载)        |
| [emq\_auth\_clientid](https://github.com/emqtt/emq_auth_clientid)       | ClientId 认证插件          |
| [emq\_auth\_username](https://github.com/emqtt/emq_auth_username)       | 用户名、密码认证插件            |
| [emq\_auth\_ldap](https://github.com/emqtt/emq_auth_ldap)               | LDAP 认证 / 访问控制           |
| [emq\_auth\_http](https://github.com/emqtt/emq_auth_http)               | HTTP 认证 / 访问控制           |
| [emq\_auth\_mysql](https://github.com/emqtt/emq_auth_mysql)             | MySQL 认证 / 访问控制          |
| [emq\_auth\_pgsql](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL 认证 / 访问控制     |
| [emq\_auth\_redis](https://github.com/emqtt/emq_auth_redis)             | Redis 认证 / 访问控制          |
| [emq\_auth\_mongo](https://github.com/emqtt/emq_auth_mongo)             | MongoDB 认证 / 访问控制        |
| [emq\_mod\_rewrite](https://github.com/emqtt/emq_mod_rewrite)           | 重写主题 (Topic) 插件         |
| [emq\_mod\_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块          |
| [emq\_mod\_presence](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布          |
| [emq\_mod\_subscription](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅           |
| [emq\_coap](https://github.com/emqtt/emq_coap)                          | CoAP 协议支持              |
| [emq\_sn](https://github.com/emqtt/emq_sn)                              | MQTT-SN 协议支持           |
| [emq\_stomp](https://github.com/emqtt/emq_stomp)                        | Stomp 协议支持             |
| [emq\_sockjs](https://github.com/emqtt/emq_sockjs)                      | Stomp over SockJS 协议支持 |
| [emq\_recon](https://github.com/emqtt/emq_recon)                        | Recon 性能调试             |
| [emq\_reloader](https://github.com/emqtt/emq_reloader)                  | Reloader 代码热加载插件       |
| [emq\_plugin\_template](https://github.com/emqtt/emq_plugin_template)   | 插件开发模版                |

## 2.0-rc.3 版本

## 2.0-rc.3 版本

* 发布日期:
2016-11-01*

1.  将 Presence、Retainer、Subscription 三个扩展模块改为独立插件:

|                                                                         |              |
| ----------------------------------------------------------------------- | ------------ |
| [emq\_mod\_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块 |
| [emq\_mod\_presence](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布 |
| [emq\_mod\_subscription](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅  |

2.  更新 EMQ 自带的自签名 SSL 证书，修复 SSL 双向认证配置文件错误
3.  Bugfix: Fixed a typo (\#716)
4.  Bugfix: emqttd\_http can not use emq\_auth\_http? \#739
5.  Bugfix: emq\_auth\_redis cannot use hostname as server address
    (\#741)
6.  升级 Redis, MySQL, Postgre, MongoDB 插件，支持主机名或域名配置

## 2.0-rc.2 版本

* 发布日期: 2016-10-19*

1.  集成 cuttlefish 库，支持 'K = V' 通用配置文件格式，重构 EMQ 与全部插件配置文件:
    
        node.name = emqttd@127.0.0.1
        
        ...
        
        mqtt.listener.tcp = 1883
        
        ...

2.  支持操作系统环境变量。启动时通过环境变量设置 EMQ 节点名称、Cookie 以及 TCP 端口号:
    
        EMQ_NODE_NAME
        EMQ_NODE_COOKIE
        EMQ_MAX_PORTS
        EMQ_TCP_PORT
        EMQ_SSL_PORT
        EMQ_HTTP_PORT
        EMQ_HTTPS_PORT

3.  重构认证模块、ACL 模块与扩展模块，更新全部插件项目名称以及配置文件。

TODO: issues closed.

## 2.0-rc.1 版本

* 发布日期: 2016-10-03*

1.  超级用户认证成功后，发布订阅时不进行 ACL 鉴权 (\#696)

2.  MQTT 客户端认证失败后，EMQ 服务器主动关闭 TCP 连接 (\#707)

3.  改进插件管理设计，新增插件无需修改 rel/sys.config 配置

4.  改进全部插件 Makefile 的 emqttd 依赖:
    
        BUILD_DEPS = emqttd
        dep_emqttd = git https://github.com/emqtt/emqttd emq20

5.  重新设计 Redis 插件的 ACL 鉴权模块

## 2.0-beta.3 版本

* 发布日期: 2016-09-18*

#### 共享订阅 (Shared Subscription)

Shared Suscriptions (\#639, \#416):

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

#### 本地订阅 (Local Subscription)

Local Subscriptions that will not create global routes:

    mosquitto_sub -t '$local/topic'

#### 问题修复

Error on Loading emqttd\_auth\_http (\#691)

Remove 'emqttd' application from dependencies (emqttd\_coap PR\#3)

## 2.0-beta.2 版本

* 发布日期: 2016-09-10*

#### CoAP 协议支持

CoAP 协议支持插件 (Beta): <https://github.com/emqtt/emqttd_coap>

#### API Breaking Changes

'$u', '$c' variables in emqttd.conf and modules/acl.conf changed to
'% u', '% c'

Improve the design of mqtt retained message, replace emqttd\_retainer
with emqttd\_mod\_retainer.

Add'session.subscribed', 'session.unsubscribed' hooks, remove
'client.subscribe.after' hook

Tab 'retained\_message' -\> 'mqtt\_retained'

#### Bugfix

\[2.0 beta1\] FORMAT ERROR: "~s PUBLISH to ~s: ~p" (PR \#671)

Fixing issues in cluster mode. (PR \#681)

Fixing issues with unsubscribe hook (PR \#673)

## 2.0-beta.1 版本

* 发布日期: 2016-08-30*

* 版本别名：西湖以西 (West of West Lake)*

EMQ 2.0-beta1 预览版本 (Preview Release) 发布。EMQ
2.0 版本改进了项目结构、发布方式、Git 分支结构以及配置文件格式，以奠定 EMQ 消息服务器项目长期演进基础。

<div class="note">

<div class="admonition-title">

Note

</div>

1.x 版本产品部署用户请勿升级到该版本，2.0 正式版本发布前会有 API 变更。

</div>

#### 项目简称 - EMQ

项目简称变更为 EMQ (Erlang/Enterprise/Elastic MQTT
Broker)，E 含义 Erlang/OTP 平台、企业 (Enterprise)、弹性 (Elastic)。

#### 项目发布方式

2.0 版本后采用预览版 (Preview Release) + 候选版本 (Release
Candidate) 版本方式迭代发布，2.0 版本将陆续发布 beta1, beta2,
beta3, rc1, rc2 等迭代，直到 2.0 正式版本发布。

#### 应用与发布

2.0 版本后 [emqttd](https://github.com/emqtt/emqttd)
项目只包括消息服务器应用源码，分离发布 (rel) 为独立项目:
[emqttd\_relx](https://github.com/emqtt/emqttd-relx)
，以解决 1.0 版本的插件 (plugins) 与 emqttd 应用编译依赖问题。

源码编译请 clone [emqttd\_relx](https://github.com/emqtt/emqttd-relx):

    git clone https://github.com/emqtt/emqttd-relx.git
    
    cd emqttd-relx && make
    
    cd _rel/emqttd && ./bin/emqttd console

#### erlang.mk 与 relx

2.0 版本发布项目 [emqttd\_relx](https://github.com/emqtt/emqttd-relx) 采用
[erlang.mk](https://erlang.mk) 和 [relx](https://github.com/erlware/relx)
编译发布工具替换 1.x 版本使用的 rebar。原因: <https://erlang.mk/guide/why.html>

#### Git 分支结构

|             |            |
| ----------- | ---------- |
| stable      | 1.x 稳定版本分支 |
| master      | 2.x 主版本分支  |
| emq10       | 1.x 版本开发分支 |
| emq20       | 2.x 版本开发分支 |
| emq30       | 3.x 版本开发分支 |
| issue\#{id} | Issue 修复分支  |

etc/emqttd.conf 配置文件 ---------=-------------

2.0 版本改进项目配置文件格式，采用 rebar.config、relx.config 类似格式，提高配置文件的可读性和可编辑性。

etc/emqttd.conf 配置示例:

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.
    
    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.
    
    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

#### MQTT-SN 协议支持

2.0-beta1 版本正式发布 [emqttd\_sn](http://github.com/emqtt/emqttd_sn)
项目支持 MQTT-SN 协议，插件加载方式启用 emqttd\_sn 项目，MQTT-SN 默认 UDP 端口: 1884:

    ./bin/emqttd_ctl plugins load emqttd_sn

#### 改进插件架构

2.0
版本从 emqttd 项目删除 plugins / 目录，插件作为一个普通的 Erlang 应用，直接依赖 (deps) 方式在编译到 lib 目录，插件配置文件统一放置在 etc/plugins/ 目录中:

    ▾ emqttd-relx/
      ▾ etc/
        ▸ modules/
        ▾ plugins/
            emqtt_coap.conf
            emqttd.conf
            emqttd_auth_http.conf
            emqttd_auth_mongo.conf
            emqttd_auth_mysql.conf
            emqttd_auth_pgsql.conf
            emqttd_auth_redis.conf
            emqttd_coap.conf
            emqttd_dashboard.conf
            emqttd_plugin_template.conf
            emqttd_recon.conf
            emqttd_reloader.conf
            emqttd_sn.conf
            emqttd_stomp.conf

#### 2.0 版本项目文档

2.0 版本中文文档: <http://emqtt.com/docs/v2/index.html> 或
<http://docs.emqtt.cn/zh_CN/emq20>

2.0 版本英文文档: <https://developer.emqx.io/docs/emq/v2/en/index.html> 或
<http://docs.emqtt.com/>

#### 发布订阅流程

![image](./_static/images/publish.png)

## 1.1.3 版本

* 发布日期: 2016-08-19*

Support './bin/emqttd\_ctl users list' CLI (\#621)

Cannot publish payloads with a size of the order 64K using WebSockets
(\#643)

Optimize the procedures that retrieve the Broker version and Borker
description in the tick timer (PR\#627)

Fix SSL certfile, keyfile config (\#651)

## 1.1.2 版本

* 发布日期: 2016-06-30*

Upgrade mysql-otp driver to 1.2.0 (\#564, \#523, \#586, \#596)

Fix WebSocket Client Leak (PR \#612)

java.io.EOFException using paho java client (\#551)

Send message from paho java client to javascript client (\#552)

Compatible with the Qos0 PUBREL packet (\#575)

Empty clientId with non-clean session accepted (\#599)

Update docs to fix typos (\#601, \#607)

## 1.1.1 版本

* 发布日期: 2016-06-04*

Compatible with the Qos0 PUBREL packet (\#575)

phpMqtt Client Compatibility (\#572)

java.io.EOFException using paho java client (\#551)

## 1.1 版本

* 发布日期:
2016-06-01*

1.1 版本升级 eSockd 库到 4.0，支持 IPv6 与监听特定 IP 地址。新增 MongoDB 认证插件、HTTP 认证插件与 Reloader 插件。升级 MySQL、PostgreSQL、Redis 认证插件，采用参数化查询避免 SQL 注入，并支持超级用户 (superuser) 认证。

#### 问题与改进

Allow human-friendly IP addresses (PR\#395)

File operation error: emfile (\#445)

emqttd\_plugin\_mongo not found in emqttd (\#489)

emqttd\_plugin\_mongo Error While Loading in emqttd (\#505)

Feature request: HTTP Authentication (\#541)

Compatible with the Qos0 PUBREL packet (\#575)

Bugfix: function\_clause exception occurs when registering a duplicated
authentication module (\#542)

Bugfix: ./emqttd\_top msg\_q result: {"init terminating in
do\_boot",{undef,\[{etop,start,\[\],\[\]},{init,start\_it,1,\[\]},{init,start\_em,1,\[\]}\]}}
(\#557)

#### Dashboard 插件

WebSocket 连接页面支持 Clean Session, Qos, Retained 参数设置 (emqttd\_dashboard\#52)

升级 eSockd 库到 4.0 版本，Overview 页面显示 OTP 版本 (emqttd\_dashboard\#61)

Changing dashboard credentials for username authentication
(emqttd\_dashboard\#56)

新增 './bin/emqttd\_ctl admins' 管理命令，支持通过命令行重新设置 admin 密码

#### HTTP 认证插件

支持通过 HTTP API 认证 / 鉴权 MQTT 客户端: <https://github.com/emqtt/emqttd_auth_http>

#### MongoDB 认证插件

升级 Erlang Mongodb 驱动到 v1.0.0 (emqttd\_plugin\_mongo\#1)

支持超级用户认证

支持基于 MongoDB 的 ACL (emqttd\_plugin\_mongo\#3)

#### MySQL 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

#### Postgre 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

#### Redis 认证插件

支持超级用户认证

支持 ClientId 认证 / ACL (emqttd\_plugin\_redis\#4)

#### Reloader 插件

开发调试代码热升级插件: <https://github.com/emqtt/emqttd_reloader>

## 1.0.2 版本

* 发布日期: 2016-05-04*

Issue\#534 - './bin/emqttd\_ctl vm' - add 'port/count', 'port/limit'
statistics

Issue\#535 - emqttd\_client should be terminated properly even if
exception happened when sending data

PR\#519 - The erlang '-name' requires the fully qualified host name

emqttd\_reloader plugin - help reload modified modules during
development.

## 1.0.1 版本

* 发布日期: 2016-04-16*

PR\#515 - Fix '$queue' pubsub, add 'pubsub\_queue' test and update docs

## 1.0 (七英里) 版本

* 发布日期: 2016-04-13*

* 版本别名：七英里 (The Seven Mile Journey)*

经过两年开发，五十个版本迭代，我们正式发布 1.0 (七英里) 版本，和完整的中英文项目文档。

1.0 版本基本实现了设计目标：稳定承载来自移动互联网或物联网终端的大量并发 MQTT 连接，并实现在大数量的终端间快速低延时的 MQTT 消息路由。

1.  完整支持 MQTT V3.1.1 协议，扩展支持 WebSocket、Stomp 或私有 TCP 等多协议。
2.  稳定承载大规模的并发 MQTT 客户端连接，单服务器节点支持 50 万到 100 万连接。
3.  分布式节点集群或桥接，快速低延时的消息路由，单集群支持 1000 万规模的路由。
4.  支持消息服务器内扩展，支持定制多种认证方式，插件方式存储消息到后端数据库。

#### 问题与改进

1.0 版本主要发布完整项目文档，相比 0.17.1 版本很少代码变更:

Possible race condition using emqttd\_cm (\#486)

Improve the design of retained message expiration (\#503)

Should not expire the retained messages from $SYS/\# topics (\#500)

#### 项目文档

1.0 版本中文文档: <http://emqtt.com/docs/> 或 <http://docs.emqtt.cn>

1.0 版本英文文档: <https://developer.emqx.io/docs/emq/v1/en/index.html> 或
<http://docs.emqtt.com/>

#### 官方站点

中文站点: <http://emqtt.com>

英文站点: <https://www.emqx.io/>

#### 致谢

爱立信与 Erlang/OTP 语言平台团队 (<http://www.erlang.org/>)\!

贡献者 (GitHub 帐户): @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng
@Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

公司：开源中国，鲁能电力，太极计算机，电信天翼云直播，研色科技，杭州华思

乐队：七英里 (The Seven Mile Journey)，腰乐队，万能青年旅店

## 0.17.1-beta 版本

* 发布日期: 2016-03-22*

#### Enhancements

Time unit of session 'expired\_after' changed to minute. (\#479)

#### Dashboard

Code Review and improve the design of Dashboard.

## 0.17.0-beta 版本

* 发布日期: 2016-03-15*

#### Highlights

Installation and Configuration Guide released on <http://docs.emqtt.com>

Improve and Consolidate the design of Hook, Server, PubSub and Router

Upgrade the \[Web
Dashboard\](<https://github.com/emqtt/emqttd_dashboard>) to support
pagination

Bridge emqttd broker to another emqttd broker & emqttd to mosquitto
bridge (\#438)

#### Enhancements

emqttd\_ctl: better error message (\#450)

./bin/emqttd\_ctl: add 'routes' command

`` ` routes list # List all routes routes show <Topic> # Show a
route``\`

Add 'backend\_subscription' table and support static subscriptions
(emqttd\_backend)

Add 'retained\_message' table and refactor emqttd\_retainer module
(emqttd\_backend)

A New Hook and Callback Design (emqttd\_hook)

Add PubSub, Hooks APIs to emqttd module (emqttd)

Move start\_listeners/0, stop\_listeners/0 APIs to emqttd\_app module
(emqttd\_app)

#### Tests

Add 100+ common test cases.

#### Plugins

Upgrade Dashboard, Redis, Stomp and Template Plugins

## 0.16.0-beta 版本

* 发布日期: 2016-02-16*

#### Highlights

Licensed under the Apache License, Version 2.0 Now.

Improve the design of cluster, support to join or leave the cluster
(\#449):

`` ` $ ./bin/emqttd_ctl cluster cluster join <Node> #Join the cluster
cluster leave #Leave the cluster cluster remove <Node> #Remove the node
from cluster cluster status #Cluster status``\`

Improve the design of Trie and Route, only the wildcard topics stored in
Trie.

Common Test to replace EUnit.

#### Enhancements

mqtt\_message record: add'sender' field (\#440)

refactor the emqttd, emqttd\_time, emqttd\_opts, emqttd\_node modules.

#### Bugfix

noproc error when call to gen\_server2:call (false,
{add\_route,Topic,\<0.685.0\>}, infinity) (\#446)

\#\#\#\# Plugins

Changed the license of all plugins.

## 0.15.0-beta 版本

* 发布日期: 2016-01-31*

#### Highlights

Optimize for Push Application, 500K+ Subscribers to a Topic.

Optimization for Route ETS insertion (\#427)

Priority Message Queue for Persistent Session (\#432)

Add Redis, MongoDB Plugins (\#417)

#### Enhancements

Username/Password Authentication: Support to configure default users
(\#428)

Improve CLI Commands: pubsub, bridges, trace (\#429)

emqttd\_mod\_subscription: fix client\_connected/3

emqttd\_auth\_mod: add passwd\_hash/2 function

priority\_queue: add plen/2, out/2 functions

#### Bugfix

Fix dequeue/1 of emqttd\_bridge...

Add emqttd:seed\_now/0 function

#### Plugins

emqttd\_plubin\_mysql: Changed mysql driver to mysql-otp

emqttd\_plugin\_pgsql: Integrate with ecpool

emqttd\_plugin\_redis: First release

emqttd\_plugin\_mongo: First release

## 0.14.1-beta 版本

* 发布日期: 2015-12-28*

Bugfix: emqttd\_ws\_client.erl: Unexpected Info:
{'EXIT',\<0.27792.18\>,{shutdown,destroy}} (\#413)

Improve: fix spec errors found by dialyzer

## 0.14.0-beta 版本

* 发布日期: 2015-12-18*

#### Highlights

Scaling to 1.3 Million Concurrent MQTT Connections on a 12 Core, 32G
CentOS server.

New PubSub, Router Design (\#402). Prepare for scaling to 10 millions on
one cluster.

#### Enhancements

Improve the gproc\_pool usage with a general emqttd\_pool\_sup

Improve the design of emqttd\_pubsub, add a new emqttd\_router module

Improve the design of the whole supervisor tree

Route aging mechanism to remove the topics that have no subscriptions

Improve the dashboard, mysql, pgsql, stomp, sockjs plugins

Add 'topics', 'subscriptions' admin commands

Avoid using mnesia table index and mnesia:index\_read API to lower CPU
usage

Subscribe timeout exception (\#366)

Long Delay on Multiple Topic Subscription (\#365)

Subscriptions persistence (\#344)

emqttd\_ctl: 'subscriptions' command to force clients to subscribe some
topics (\#361)

#### Bugfix

emqttd\_sm: spec of lookup\_session/1 is not right BUG (\#411)

Observer application should be removed from reltool.config for 'wx' app
is not available (\#410)

#### Benchmark

1.3 million concurrent MQTT connections on a 12 Core, 32G CentOS Server,
consume about 15G Memory and 200% CPU.

## 0.13.1-beta 版本

* 发布日期: 2015-11-28*

Bugfix: Plugin pathes error under windows (\#387)

Improve: Too many error logs "\[error\] Session ..... Unexpected EXIT:
client\_pid=\<0.14137.35\>, exit\_pid=\<0.30829.22\>, reason=nop..."
(\#383)

Improve: Define QOS0/1/2, Pooler Error (PR\#382)

Improve: High CPU load when 400K unstable mobile connections (\#377)

BugFix: emqttd\_plugin\_pgsql - error using same query with latest
update plugin (pgsql\#5)

## 0.13.0-beta 版本

* 发布日期: 2015-11-08*

#### Highlights

Rate Limiting based on \[Token
Bucket\](<https://en.wikipedia.org/wiki/Token_bucket>) and \[Leaky
Bucket\](<https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter>)
Algorithm

Upgrade eSockd and MochiWeb libraries to support Parameterized
Connection Module

Improve emqttd\_client to support fully asynchronous socket networking

#### Enhancements

Protocol Compliant - Session Present Flag (\#163)

Compilation fails if repo is cloned with a different name (\#348)

emqttd\_client: replace gen\_tcp:send with port\_command (\#358)

TCP sndbuf, recbuf, buffer tuning (\#359)

emqttd\_client.erl to handle 'inet\_async', 'inet\_reply' properly
(\#360)

Refator the \[client/session management
design\](<https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md>)

#### Bugfix

Cannot kick transient client out when clientId collision (\#357)

Fix the order of emqttd\_app:start\_server/1 (\#367)

emqttd\_<session:subscribe/2> will crash (\#374)

#### Benchmark

\[benchmark for 0.13.0
release\](<https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release>)

3.1G memory and 50+ CPU/core:

``` sourceCode bash
Connections: 250K
Subscribers: 250K
Topics:      50K
Qos1 Messages/Sec In:  4K
Qos1 Messages/Sec Out: 20K
Traffic In (bps):  12M+
Traffic Out (bps): 56M+
```

## 0.12.3-beta 版本

* 发布日期: 2015-10-22*

Bugfix: emqttd\_sysmon crasher for 'undefined' process\_info (\#350)

Bugfix: emqttd\_client: catch parser exception (\#353)

## 0.12.2-beta 版本

* 发布日期: 2015-10-16*

Bugfix: Retained messages should not be expired if
'broker.retained.expired\_after = 0' (\#346)

## 0.12.1-beta 版本

* 发布日期: 2015-10-15*

Highlight: Release for Bugfix and Code Refactor.

Feature: Retained message expiration (\#182)

Improve: '$SYS/\#' publish will not match '\#' or '+/\#' (\#68)

Improve: Add more metrics and ignore '$SYS/\#' publish (\#266)

Improve: emqttd\_sm should be optimized for clustered nodes may be
crashed (\#282)

Improve: Refactor emqttd\_sysmon and suppress 'monitor' messages (\#328)

Task: benchmark for 0.12.0 release (\#225)

Benchmark: About 900K concurrent connections established on a 20Core,
32G CentOS server.

## 0.12.0-beta 版本

* 发布日期: 2015-10-08*

#### Highlights

Enhance the **emqttd\_ctl** module to allow plugins to register new
commands (\#256)

Add \[emqttd\_recon plugin\](<https://github.com/emqtt/emqttd_recon>) to
debug/optimize the broker (\#235)

Add **'./bin/emqttd\_ctl broker pubsub'** command to check the status of
core pubsub processes

Add **'./bin/emqttd\_top'** command (like etop) to show the top 'msg\_q',
'reductions', 'memory' or 'runtime' processes

'rel/files/emqttd.config.production' for production deployment (default)

'rel/files/emqttd.config.development' for development deployment

#### Enhancements

Qos1/2 messages will not be dropped under unstable mobile network
(\#264)

**emqttd\_<session:subscribe/2>, emqttd\_<session:unsubscribe/2>** APIs
should be asynchronous (\#292)

**etc/emqttd.config**: 'idle\_timeout' option to close the idle
client (socket connected but no 'CONNECT' frame received)

**etc/emqttd.config**: 'unack\_retry\_interval' option for redelivering
Qos1/2 messages

How to monitor large 'message\_queue\_len' (\#283)

#### Bugfix

Behaviour emqttd\_auth\_mod is missing init callback (\#318)

#### Benchmark

Write a new \[benchmark
tool\](<https://github.com/emqtt/emqtt_benchmark>) to benchmark this
release

Hw requirements - 5K users, 25-50 msgs/sec, QoS=1 (\#209)

Supported Number of Connections Greatly Reduced When Clients are
Subscribing (\#324)

## 0.11.0-beta 版本

* 发布日期: 2015-09-25*

Highlight: Rebar to manage plugin dependencies.

Highlight: \[Stomp\](<https://github.com/emqtt/emqttd_stomp>) and
\[SockJS\](<https://github.com/emqtt/emqttd_sockjs>) Plugins\!

Improve: add rel/files/emqttd.config.development|production.

Improve: rel/reltool.config.script to release deps of plugin.

Improve: persist mnesia schema on slave nodes.

Improve: use timer:seconds/1 api.

Improve: The binary release will be compiled with R18.1 now.

Bugfix: issue\#306 - emqttd\_cm should unregister the duplicated client

Bugfix: issue\#310 - usage of emqttd\_ctl error: 'session list' should
be'sessions list'

Bugfix: issue\#311 - './bin/emqttd\_ctl sessions list' error

Bugfix: issue\#312 - unsubcribe will lead to crash if
emqttd\_plugin\_template plugin loaded

## 0.10.4-beta 版本

* 发布日期: 2015-09-18*

Optimize session management and upgrade eSockd library to 2.7.1

\[Benchmark for 0.10.4
release\](<https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release>)

Improve: issue\#294 - \[error\] failed to start connection on
0.0.0.0:1883 - enotconn

Improve: issue\#297 - How do I allow user with some pattern to access
topic with some pattern?

Bugfix: issue\#291 - "./bin/emqttd attach ..." cannot work

Bugfix: issue\#284 - Should not use erlang:list\_to\_atom/1 in
emqttd\_vm.erl

## 0.10.3-beta 版本

* 发布日期: 2015-08-30*

Bugfix: issue\#271 - add emqttd\_ws\_client:subscribe/2 function

Bugfix: issue\#269 - bin/emqttd Syntax error on ubuntu

Improve: issue\#265 - client under unstable mobile network generate a
lot of logs

## 0.10.2-beta 版本

* 发布日期: 2015-08-26*

Improve: issue\#257 - After the node name changed, the broker cannot
restart for mnesia schema error.

## 0.10.1-beta 版本

* 发布日期: 2015-08-25*

Bugfix: issue\#259 - when clustered the emqttd\_dashboard port is close,
and the 'emqttd' application cannot stop normally.

Feature: issue\#262 - Add '<http://host:8083/mqtt/status>' Page for
health check

## 0.10.0-beta 版本

* 发布日期: 2015-08-20*

\[Web Dashboard\](<https://github.com/emqtt/emqttd_dashboard>) and
\[MySQL\](<https://github.com/emqtt/emqttd_plugin_mysql>),
\[PostgreSQL\](<https://github.com/emqtt/emqttd_plugin_pgsql>)
Authentication/ACL Plugins\!

Highlight: Web Dashboard to monitor Statistics, Metrics, Clients,
Sessions and Topics of the broker.

Highlight: JSON/HTTP API to query all clients connected to broker.

Highlight: A new \[Plugin
Design\](<https://github.com/emqtt/emqttd/wiki/Plugin%20Design>) and a
\[Template project\](<https://github.com/emqtt/emqttd_plugin_template>)
for plugin development.

Highlight: Authentication/ACL with MySQL, PostreSQl databases (\#194,
\#172)

Feature: Session Statistics including inflight\_queue, message\_queue,
message\_dropped, awaiting\_rel, awaiting\_ack, awaiting\_comp (\#213)

Feature: Cookie based authentication for MQTT over websocket connections
(\#231)

Feature: Get all clients connected to the broker (\#228, \#230, \#148,
\#129)

Feature: "./bin/emqttd\_ctl clients show ClientId" to query client
status (\#226)

Feature: "./bin/emqttd\_ctl clients kick ClientId" to kick out a client

Feature: "./bin/emqttd\_ctl sessions list" to show all sessions

Feature: "./bin/emqttd\_ctl sessions show ClientId" to show a session

Feature: Erlang VM metrics monitor with Web Dashboard (\#59)

Improve: Too many "inflight queue is full\!" log when session is
overloaded (\#247)

Improve: There are two many "MQueue (~s) drop ~s" logs if the message
queue of session is small (\#244)

Improve: gen\_server2 (from RabbitMQ) to improve emqttd\_session,
emqttd\_pubsub

Improve: Makefile to build plugins

Bugfix: emqttd\_broker:unhook/2 cannot work (\#238)

Bugfix: emqttd plugin cannot include\_lib ("emqttd/include/emqttd.hrl")
(\#233)

Bugfix: Too many 'Session ~s cannot find PUBACK' logs (\#212)

Bugfix: emqttd\_pooler cannot work

## 0.9.3-alpha 版本

* 发布日期: 2015-07-25*

Wiki: \[Bridge\](<https://github.com/emqtt/emqttd/wiki/Bridge>)

Improve: emqttd\_protocol.hrl to define 'QOS\_I'

Improve: emqttd\_pubsub to add subscribe/2 API

Improve: ./bin/emqttd\_ctl to support new bridges command

Bugfix: issue \#206 - Cannot bridge two nodes

## 0.9.2-alpha 版本

* 发布日期: 2015-07-18*

Improve: issue \#196 - Add New Hook 'client.subscribe.after'

## 0.9.1-alpha 版本

* 发布日期: 2015-07-10*

Bugfix: issue \#189 - MQTT over WebSocket (SSL) cannot work?

Bugfix: issue \#193 - 'client.ack' hook should be renamed to
'message.acked', and called by emqttd\_broker:foreach\_hooks

## 0.9.0-alpha 版本

* 发布日期: 2015-07-09*

\[Session, Queue, Inflight Window, Hooks, Global MessageId and More
Protocol
Compliant\](<https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha>)
Now\!

Feature: Session/Queue/Inflight Window Design (\#145).

Feature: Support to resume a persistent session on other clustered node.

Feature: Support alarm management.

Feature: emqttd\_guid to generate global unique message id.

Feature: Hooks for message pub/ack.

Feature: Protocol compliant - message ordering, timeout and retry.

Improve: Every client will start\_link a session process, whether or not
the client is persistent.

Improve: etc/emqttd.config to support more session, queue configuration.

Improve: issue \#179 - Max offline message queue {max\_queue, 100}
meaning.

Improve: issue \#180 - Should change project structure for other
projects maybe depend on 'emqttd'. Merge emqtt, emqttd apps.

Improve: issue \#185 - PacketId and MessageId: the broker should
generate global unique message id.

Improve: issue \#187 - etc/emqttd.config to support https listener

Improve: issue \#186 - emqttd\_cm to store client details

Improve: issue \#174 - add 'from' field to mqtt\_message record.

Improve: issue \#170 - $SYS Topics should support alarms.

Improve: issue \#169 - Add More
\[Hooks\](<https://github.com/emqtt/emqttd/wiki/Hooks-Design>)

Improve: issue \#167 - Inflight window to assure message ordering.

Improve: issue \#166 - Message delivery timeout and retry.

Improve: issue \#143 - Qos1, Qos2 PubSub message timeout.

Improve: issue \#122 - Labeling message with unique id. emqttd\_guid
module to generate global unique msgid.

Improve: emqttd\_bridge to support pending message queue, and fix the
wrong Qos design.

Improve: mqtt\_message record to add 'msgid', 'from' and'sys' fields.

Change: Add emqttd\_mqueue, emqttd\_guid, emqttd\_alarm modules.

Bugfix: issue \#184 - emqttd\_stats:setstats is not right.

Bugfix: Closed issues \#181, \#119.

Tests: fix the parser, acl test cases.

## 0.8.6-beta 版本

* 发布日期: 2015-06-17*

Bugfix: issue \#175 - publish Will message when websocket is closed
without 'DISCONNECT' packet

## 0.8.5-beta 版本

* 发布日期: 2015-06-10*

Bugfix: issue \#53 - client will receive duplicate messages when
overlapping subscription

## 0.8.4-beta 版本

* 发布日期: 2015-06-08*

Bugfix: issue \#165 - duplicated message when publish 'retained' message
to persistent client

## 0.8.3-beta 版本

* 发布日期: 2015-06-05*

Bugfix: issue \#158 - should queue:in new message after old one dropped

Bugfix: issue \#155 - emqtt\_parser.erl: parse\_topics/3 should reverse
topics

Bugfix: issue \#149 - Forget to merge plugins/emqttd\_auth\_mysql from
'dev' branch to 'master' in 0.8.x release

## 0.8.2-alpha 版本

* 发布日期: 2015-06-01*

Bugfix: issue \#147 - WebSocket client cannot subscribe queue
'$Q/queue/${clientId}'

Bugfix: issue \#146 - emqttd\_auth\_ldap: fill (Username, UserDn) is not
right

## 0.8.1-alpha 版本

* 发布日期: 2015-05-28*

Client \[Presence\](<https://github.com/emqtt/emqttd/wiki/Presence>)
Support and \[$SYS
Topics\](<https://github.com/emqtt/emqttd/wiki/$SYS-Topics>)
Redesigned\!

Bugfix: issue \#138 - when client disconnected normally, broker will not
publish disconnected $SYS message

Bugfix: fix websocket url in emqttd/priv/www/websocket.html

Improve: etc/emqttd.config to allow websocket connections from any hosts

Improve: rel/reltool.config to exclude unnecessary apps.

## 0.8.0-alpha 版本

* 发布日期: 2015-05-25*

\[Hooks\](<https://github.com/emqtt/emqttd/wiki/Hooks%20Design>),
Modules and
\[Plugins\](<https://github.com/emqtt/emqttd/wiki/Plugin%20Design>) to
extend the broker Now\!

Plugin: emqttd\_auth\_mysql - MySQL authentication plugin (issues \#116,
\#120)

Plugin: emqttd\_auth\_ldap - LDAP authentication plugin

Feature: emqttd\_broker to support Hooks API

Feature: issue \#111 - Support 'Forced Subscriptions' by
emqttd\_mod\_autosub module

Feature: issue \#126 - Support 'Rewrite rules' by emqttd\_mod\_rewrite
module

Improve: Support hooks, modules to extend the broker

Improve: issue \#76 - dialyzer check

Improve: 'Get Started', 'User Guide', 'Developer Guide' Wiki

Improve: emqtt\_topic to add join/1, feed\_var/3, is\_queue/1

Improve: emqttd\_pooler to execute common tasks

Improve: add emqttd\_sm\_sup module, and use 'hash' gproc\_pool to
manage sessions

Tests: add more test cases for 'emqttd' app

## 0.7.1-alpha 版本

* 发布日期: 2015-05-04*

Add doc/design/\* and merge doc/\* to github Wiki

Bugfix: issue \#121 - emqttd cluster issuse

Bugfix: issue \#123 - emqttd:unload\_all\_plugins/0 cannot unload any
plugin

Bugfix: fix errors found by dialyzer

## 0.7.0-alpha 版本

* 发布日期: 2015-05-02*

\[MQTT over
WebSocket (SSL)\](<https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket>)
Now\!

\[Plugin
Achitecture\](<https://github.com/emqtt/emqttd/wiki/Plugin%20Design>)
based on OTP application

\[Trace MQTT Packets or
Messages\](<https://github.com/emqtt/emqttd/wiki/Trace%20Design>) to log
files

Feature: issue \#40, \#115 - WebSocket/SSL Support

Feature: issue \#49, \#105 - Plugin Architecture Support

Feature: issue \#93 - Trace API Design

Improve: issue \#109 - emqttd\_broker should add subscribe, notify API

Improve: update README.md to add 'Goals', 'Contributors' chapters

Change: rename etc/app.config to etc/emqttd.config

Change: etc/emqttd.config changed

Bugfix: critical issue \#54 - error when resume session\!

Bugfix: issue \#118 - error report when UNSUBSCRIBE with no topics

Bugfix: issue \#117 - sys\_interval = 0 config cannot work

Bugfix: issue \#112 - Makefile to support build plugins

Bugfix: issue \#96 - "make clean" cannot work

## 0.6.2-alpha 版本

* 发布日期: 2015-04-24*

Bugfix: critical issue \#54, \#104, \#106 - error when resume session

Improve: add emqttd\_cm\_sup module, and use 'hash' gproc\_pool to
register/unregister client ids

Improve: kick old client out when session is duplicated.

Improve: move mnesia dir config from etc/app.config to etc/vm.args

## 0.6.1-alpha 版本

* 发布日期: 2015-04-20*

Integrate with \[gproc library\](<https://github.com/uwiger/gproc>) to
support pool

Feature: issues\#91 - should use worker\_pool to handle some async work?

Feature: issues\#95 - Topic filters in ACL rule should support 'eq' tag

Improve: issues\#84 - emqttd\_pubsub is redesigned again to protect
mnesia transaction

Improve: issues\#74 - ACL Support and update \[ACL Design
Wiki\](<https://github.com/emqtt/emqttd/wiki/ACL-Design>)

## 0.6.0-alpha 版本

* 发布日期: 2015-04-17*

ACL Support Now: \[ACL-Design
Wiki\](<https://github.com/emqtt/emqttd/wiki/ACL-Design>)

Authentication with username, clientid Now: \[Authentication
Wiki\](<https://github.com/emqtt/emqttd/wiki/Authentication>)

Seperate common MQTT library to 'emqtt' application

Redesign message pubsub, route and retain modules

Redesign mnesia database cluster

Feature: issues\#47 - authentication, authorization support

Feature: issues\#92 - merge emqttd\_acl and emqttd\_auth to
emqttd\_access\_control

Feature: emqttd\_acl\_mod, emqttd\_auth\_mod behaviour to extend ACL,
authentication

Feature: issues\#85 - lager:info to log subscribe, unsubscribe actions

Feature: issues\#77 - authentication with clientid, ipaddress

Improve: issues\#90 - fix lager\_file\_backend log format, and rotate 10
log files

Improve: issues\#88 - use '-mneisa\_create', '-mnesia\_replicate'
attributes to init mneisa

Improve: issues\#87 - record mqtt\_user and mqtt\_client is duplicated

Improve: issues\#81 - redesign nodes cluster to support disc\_copies
mnesia tables

Improve: issues\#80 - redesign emqttd\_cm to handle more concurrent
connections

Improve: issues\#70 - how to handle connection flood? Now could support
2K+ CONNECT/sec

Change: redesign mnesia tables: message, topic, subscriber, trie,
trie\_node

Bugfix: issues\#83 - emqttd\_broker stats cannot work

Bugfix: issues\#75 - careless about function name when emqttd\_pubsub
handle getstats message

## 0.5.5-beta 版本

* 发布日期: 2015-04-09*

Bugfix: issue \#75 - careless about function name when emqttd\_pubsub
handle getstats message.

Bugfix: issue \#79 - cannot find topic\_subscriber table after cluster
with other nodes.

## 0.5.4-alpha 版本

* 发布日期: 2015-03-22*

Benchmark this release on a ubuntu/14.04 server with 8 cores, 32G memory
from QingCloud.com: :

    200K Connections,
    30K Messages/Sec,
    20Mbps In/Out Traffic,
    200K Topics,
    200K Subscribers,
    Consumed 7G memory, 40% CPU/core

Benchmark code: <https://github.com/emqtt/emqttd_benchmark>

Change: rewrite emqttd\_pubsub to handle more concurrent subscribe
requests.

Change: ./bin/emqttd\_ctl add'stats', 'metrics' commands.

Bugfix: issue \#71, \#72

## 0.5.3-alpha 版本

* 发布日期: 2015-03-19*

Bugfix: issues\#72 - emqttd\_cm, emqtt\_sm ets:match\_delete/2 with
wrong pattern

## 0.5.2-alpha 版本

* 发布日期: 2015-03-18*

Change: upgrade esockd to 2.1.0-alpha, do not tune socket buffer for
mqtt connection.

## 0.5.1-alpha 版本

* 发布日期: 2015-03-13*

Change: upgrade esockd to v1.2.0-beta, rename 'acceptor\_pool' to
'acceptors'

## 0.5.0-alpha 版本

* 发布日期: 2015-03-12*

RENAME 'emqtt' to 'emqttd'\!

Support \[Broker
Bridge\](<https://github.com/emqtt/emqttd/wiki/Bridge-Design>) Now\!

Change: rename project from 'emqtt' to 'emqttd'

Change: lager:debug to dump RECV/SENT packets

Feature: emqttd\_bridge, emqttd\_bridge\_sup to support broker bridge

Feature: emqtt\_event to publish client connected/disconnected message
to $SYS topics

Feature: ./bin/emqttd\_ctl add more commands: listeners, broker,
bridges, start\_bridge, stop\_bridge...

Feature: issue\#57 - support to configure max packet size

Feature: issue\#68 - if sys\_interval = 0, emqttd\_broker will not
publish messages to $SYS/brokers/\#

Bugfix: issue\#67 - subscribe '\#' to receive all messages

Bugfix: issue\#64 - emqtt\_app start/2: should wait\_for\_databases

Test: emqttd\_topic\_tests add more '\_match\_test'

## 0.4.0-alpha 版本

* 发布日期: 2015-03-10*

Support \[$SYS Topics of
Broker\](<https://github.com/emqtt/emqttd/wiki/$SYS-Topics-of-Broker>)
Now\!

Feature: emqtt\_broker to publish version, uptime, datetime to
$SYS/brokers/\# topics

Feature: emqtt\_broker to publish count of clients, sessions, suscribers
to $SYS/brokers/\# topics

Feature: emqtt\_metrics to publish bytes, packets, messages metrics to
$SYS/brokers/\# topics

Feature: add include/emqtt\_systop.hrl

Change: emqtt\_cm to count current clients

Change: emqtt\_sm to count current sessions

Change: emqtt\_pubsub to count current topics and suscribers

Change: emqtt\_pubsub to add create/1 API

Change: emqtt\_pubsub dispatch/2 to return number of subscribers

Change: emqtt\_pubsub to count 'dropped' messages

Change: emqtt\_opts to add merge/2 function

Test: add emqtt\_serialiser\_tests.erl

## 0.3.4-beta 版本

* 发布日期: 2015-03-08*

Bugfix: emqtt\_serialiser.erl cannot serialise UNSUBACK packets

## 0.3.3-beta 版本

* 发布日期: 2015-03-07*

Bugfix: emqtt\_serialiser.erl cannot serialise PINGRESP issue\#60

## 0.3.2-beta 版本

* 发布日期: 2015-03-05*

Improve: merge emqttc serialiser, parser, packet

Add: emqtt\_opts to merge socket options

## 0.3.1-beta 版本

* 发布日期: 2015-03-02*

Feature: SSL Socket Support

Feature: issue\#44 HTTP API should add Qos parameter

Bugfix: issue\#52 emqtt\_session crash

Bugfix: issue\#53 sslsocket keepalive error

Upgrade: esockd to v0.2.0

Upgrade: mochiweb to v3.0.0

## 0.3.0-beta 版本

* 发布日期: 2015-01-19*

Feature: HTTP POST API to support 'qos', 'retain' parameters

Feature: $SYS system topics support

Change: Rewrite emqtt\_topic.erl, use '', '\#', '+' to replace
\<\<""\>\>, \<\<"\#"\>\>, \<\<"+"\>\>

Change: fix emqtt\_pubsub.erl to match '\#', '+'

Tests: emqtt\_topic\_tests.erl add more test cases

## 0.3.0-alpha 版本

* 发布日期: 2015-01-08*

NOTICE: Full MQTT 3.1.1 support now\!

Feature: Passed org.eclipse.paho.mqtt.testing/interoperability tests

Feature: Qos0, Qos1 and Qos2 publish and suscribe

Feature: session (clean\_sess=false) management and offline messages

Feature: redeliver awaiting puback/pubrec messages (doc: Chapter 4.4)

Feature: retain messages, add emqtt\_server module

Feature: MQTT 3.1.1 null client\_id support

Bugfix: keepalive timeout to send will message

Improve: overlapping subscription support

Improve: add emqtt\_packet:dump to dump packets

Test: passed org.eclipse.paho.mqtt.testing/interoperability

Test: simple cluster test

Closed Issues: \#22, \#24, \#27, \#28, \#29, \#30, \#31, \#32, \#33,
\#34, \#36, \#37, \#38, \#39, \#41, \#42, \#43

## 0.2.1-beta 版本

* 发布日期: 2015-01-08*

pull request 26: Use binaries for topic paths and fix wildcard topics

emqtt\_pubsub.erl: fix wildcard topic match bug caused by binary topic
in 0.2.0

Makefile: deps -\> get-deps

rebar.config: fix mochiweb git url

tag emqtt release accoding to \[Semantic
Versioning\](<http://semver.org/>)

max clientId length is 1024 now.

## 0.2.0 版本

* 发布日期: 2014-12-07*

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5 版本

* 发布日期: 2013-01-05*

Bugfix: remove QOS\_1 match when handle PUBREL request

Bugfix: reverse word in emqtt\_topic:words/1 function

## 0.1.4 版本

* 发布日期: 2013-01-04*

Bugfix: fix "mosquitto\_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3 版本

* 发布日期: 2013-01-04*

Feature: support QOS2 PUBREC, PUBREL,PUBCOMP messages

Bugfix: fix emqtt\_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2 版本

* 发布日期: 2012-12-27*

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp\_listener.erl

## 0.1.1 版本

* 发布日期: 2012-09-24*

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0 版本

* 发布日期: 2012-09-21*

The first public release.





# 分布集群 (Clustering)

## Erlang/OTP 分布式编程

Erlang/OTP
最初是爱立信为开发电信设备系统设计的编程语言平台，电信设备 (路由器、接入网关、...) 典型设计是通过背板连接主控板卡与多块业务板卡的分布式系统。

Erlang/OTP 语言平台的分布式程序，由分布互联的 Erlang 运行系统组成，每个 Erlang
运行系统被称为节点 (Node)，节点 (Node) 间通过 TCP 互联，消息传递的方式通信:

![image](_static/images/cluster_1.png)

### 节点 (Node)

Erlang 节点由唯一的节点名称标识，节点间通过名称进行通信寻址。 例如在本机启动四个 Erlang 节点，节点名称分别为:

``` sourceCode shell
erl -name node1@127.0.0.1
erl -name node2@127.0.0.1
erl -name node3@127.0.0.1
erl -name node4@127.0.0.1
```

<node1@127.0.0.1> 控制台下建立与其他节点的连接:

    (node1@127.0.0.1) 1> net_kernel:connect_node ('node2@127.0.0.1').
    true
    (node1@127.0.0.1) 2> net_kernel:connect_node ('node3@127.0.0.1').
    true
    (node1@127.0.0.1) 3> net_kernel:connect_node ('node4@127.0.0.1').
    true
    (node1@127.0.0.1) 4> nodes ().
    ['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1']

### epmd

epmd (Erlang Port Mapper Daemon) - Erlang 端口映射服务程序，在 Erlang
节点运行主机上自启动，负责映射节点名称到通信 TCP 端口号:

    (node1@127.0.0.1) 6> net_adm:names ().
    {ok,[{"node1",62740},
         {"node2",62746},
         {"node3",62877},
         {"node4",62895}]}

### 安全

Erlang 节点间通过一个相同的 cookie 进行互连认证。

Erlang 节点 Cookie 设置:

    1. $HOME/.erlang.cookie 文件
    
    2. erl -setcookie <Cookie>

本节内容来自: <http://erlang.org/doc/reference_manual/distributed.html>

### 连接

Erlang 集群节点可通过 TCPv4, TCPv6 或 TLS 方式连接，EMQ X 支持在 `etc/emqx.conf`
中配置连接方式:

``` sourceCode properties
## Specify the erlang distributed protocol.
##
## Value: Enum
##  - inet_tcp: the default; handles TCP streams with IPv4 addressing.
##  - inet6_tcp: handles TCP with IPv6 addressing.
##  - inet_tls: using TLS for Erlang Distribution.
##
## vm.args: -proto_dist inet_tcp
node.proto_dist = inet_tcp

## Specify SSL Options in the file if using SSL for Erlang Distribution.
##
## Value: File
##
## vm.args: -ssl_dist_optfile <File>
## node.ssl_dist_optfile = {{ platform_etc_dir }}/ssl_dist.conf
```

## EMQ X 分布集群设计

*EMQ X* 消息服务器集群基于 Erlang/OTP 分布式设计，集群原理可简述为下述两条规则:

1.  MQTT 客户端订阅主题时，所在节点订阅成功后广播通知其他节点：某个主题 (Topic) 被本节点订阅。
2.  MQTT 客户端发布消息时，所在节点会根据消息主题 (Topic)，检索订阅并路由消息到相关节点。

EMQ X 消息服务器同一集群的所有节点，都会复制一份主题 (Topic) -\> 节点 (Node) 映射的路由表，例如:

    topic1 -> node1, node2
    topic2 -> node3
    topic3 -> node2, node4

### 主题树 (Topic Trie) 与路由表 (Route Table)

EMQ X 消息服务器每个集群节点，都保存一份主题树 (Topic Trie) 和路由表。

例如下述主题订阅关系:

<table style="width:83%;">
<colgroup>
<col style="width: 23%" />
<col style="width: 19%" />
<col style="width: 40%" />
</colgroup>
<tbody>
<tr class="odd">
<td > 客户端 </td>
<td > 节点 </td>
<td><blockquote>
<p > 订阅主题 </p>
</blockquote></td>
</tr>
<tr class="even">
<td>client1</td>
<td>node1</td>
<td>t/+/x, t/+/y</td>
</tr>
<tr class="odd">
<td>client2</td>
<td>node2</td>
<td>t/#</td>
</tr>
<tr class="even">
<td>client3</td>
<td>node3</td>
<td>t/+/x, t/a</td>
</tr>
</tbody>
</table>

最终会生成如下主题树 (Topic Trie) 和路由表 (Route Table):

![image](_static/images/cluster_2.png)

### 订阅 (Subscription) 与消息派发

客户端的主题订阅 (Subscription) 关系，只保存在客户端所在节点，用于本节点内派发消息到客户端。

例如 client1 向主题 't/a' 发布消息，消息在节点间的路由与派发流程:

    title: Message Route and Deliver
    
    client1 -> node1: Publish [t/a]
        node1 --> node2: Route [t/#]
            node2 --> client2: Deliver [t/#]
        node1 --> node3: Route [t/a]
            node3 --> client3: Deliver [t/a]

![image](./_static/images/design_9.png)

## 手动配置管理集群

假设部署两台服务器 s1.emqx.io, s2.emqx.io 上部署集群:

<table style="width:88%;">
<colgroup>
<col style="width: 31%" />
<col style="width: 25%" />
<col style="width: 30%" />
</colgroup>
<tbody>
<tr class="odd">
<td > 节点名 </td>
<td > 主机名 (FQDN)</td>
<td><blockquote>
<p>IP 地址 </p>
</blockquote></td>
</tr>
<tr class="even">
<td><a href="mailto:emqx@s1.emqx.io">emqx@s1.emqx.io</a> 或 <a href="mailto:emqx@192.168.0.10">emqx@192.168.0.10</a></td>
<td>s1.emqx.io</td>
<td>192.168.0.10</td>
</tr>
<tr class="odd">
<td><a href="mailto:emqx@s2.emqx.io">emqx@s2.emqx.io</a> 或 <a href="mailto:emqx@192.168.0.20">emqx@192.168.0.20</a></td>
<td>s2.emqx.io</td>
<td>192.168.0.20</td>
</tr>
</tbody>
</table>

<div class="warning">

<div class="admonition-title">

Warning

</div>

节点名格式: <Name@Host>, Host 必须是 IP 地址或 FQDN (主机名。域名)

</div>

### <emqx@s1.emqx.io> 节点设置

emqx/etc/emqx.conf:

    node.name = emqx@s1.emqx.io
    
    或
    
    node.name = emqx@192.168.0.10

也可通过环境变量:

    export EMQX_NODE_NAME=emqx@s1.emqx.io && ./bin/emqx start

<div class="warning">

<div class="admonition-title">

Warning

</div>

节点启动加入集群后，节点名称不能变更。

</div>

### <emqx@s2.emqx.io> 节点设置

emqx/etc/emqx.conf:

    node.name = emqx@s2.emqx.io
    
    或
    
    node.name = emqx@192.168.0.20

### 节点加入集群

启动两台节点后，emqx@s2.emqx.io 上执行:

    $ ./bin/emqx_ctl cluster join emqx@s1.emqx.io
    
    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]

或，emq@s1.emqx.io 上执行:

    $ ./bin/emqx_ctl cluster join emqx@s2.emqx.io
    
    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]

任意节点上查询集群状态:

    $ ./bin/emqx_ctl cluster status
    
    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]

### 节点退出集群

节点退出集群，两种方式:

1.  leave: 本节点退出集群
2.  force-leave: 从集群删除其他节点

<emqx@s2.emqx.io> 主动退出集群:

    $ ./bin/emqx_ctl cluster leave

或 <emqx@s1.emqx.io> 节点上，从集群删除 <emqx@s2.emqx.io> 节点:

    $ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io

## 节点发现与自动集群

EMQ X 支持基于 Ekka 库的集群自动发现 (Autocluster)。Ekka 是为 Erlang/OTP 应用开发的集群管理库，支持
Erlang 节点自动发现 (Discovery)、自动集群 (Autocluster)、脑裂自动愈合 (Network Partition
Autoheal)、自动删除宕机节点 (Autoclean)。

EMQ X 支持多种策略创建集群:

| 策略     | 说明                |
| ------ | ----------------- |
| manual | 手动命令创建集群          |
| static | 静态节点列表自动集群        |
| mcast  | UDP 组播方式自动集群      |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### manual 手动创建集群

默认配置为手动创建集群，节点通过 ./bin/emqx\_ctl join \<Node\> 命令加入:

``` sourceCode properties
cluster.discovery = manual
```

### 基于 static 节点列表自动集群

配置固定的节点列表，自动发现并创建集群:

``` sourceCode properties
cluster.discovery = static

cluster.static.seeds = emq1@127.0.0.1,ekka2@127.0.0.1
```

### 基于 mcast 组播自动集群

基于 UDP 组播自动发现并创建集群:

``` sourceCode properties
cluster.discovery = mcast

cluster.mcast.addr = 239.192.0.1

cluster.mcast.ports = 4369,4370

cluster.mcast.iface = 0.0.0.0

cluster.mcast.ttl = 255

cluster.mcast.loop = on
```

### 基于 DNS A 记录自动集群

基于 DNS A 记录自动发现并创建集群:

``` sourceCode properties
cluster.discovery = dns

cluster.dns.name = localhost

cluster.dns.app  = ekka
```

### 基于 etcd 自动集群

基于 [etcd](https://coreos.com/etcd/) 自动发现并创建集群:

``` sourceCode properties
cluster.discovery = etcd

cluster.etcd.server = http://127.0.0.1:2379

cluster.etcd.prefix = emqcl

cluster.etcd.node_ttl = 1m
```

### 基于 Kubernetes 自动集群

[Kubernetes](https://kubernetes.io/) 下自动发现并创建集群:

``` sourceCode properties
cluster.discovery = k8s

cluster.k8s.apiserver = http://10.110.111.204:8080

cluster.k8s.service_name = ekka

## Address Type: ip | dns
cluster.k8s.address_type = ip

## The Erlang application name
cluster.k8s.app_name = ekka
```

## 集群脑裂与自动愈合

*EMQ X* 支持集群脑裂自动恢复 (Network Partition Autoheal):

``` sourceCode properties
cluster.autoheal = on
```

集群脑裂自动恢复流程:

1.  节点收到 Mnesia 的 `inconsistent_database` 事件 3 秒后进行集群脑裂确认；
2.  节点确认集群脑裂发生后，向 Leader 节点 (集群中最早启动节点) 上报脑裂消息；
3.  Leader 节点延迟一段时间后，在全部节点在线状态下创建脑裂视图 (SplitView)；
4.  Leader 节点在多数派 (majority) 分区选择集群自愈的 Coordinator 节点；
5.  Coordinator 节点重启少数派 (minority) 分区节点恢复集群。

## 集群节点自动清除

*EMQ X* 支持从集群自动删除宕机节点 (Autoclean):

``` sourceCode properties
cluster.autoclean = 5m
```

## 跨节点会话 (Session)

*EMQ X* 集群模式下，MQTT 连接的持久会话 (Session) 跨节点。

例如负载均衡的两台集群节点: node1 与 node2，同一 MQTT 客户端先连接 node1，node1
节点会创建持久会话；客户端断线重连到 node2 时，MQTT 的连接在
node2 节点，持久会话仍在 node1 节点:

![image](_static/images/cluster_3.png)

## 防火墙设置

如果集群节点间存在防火墙，防火墙需要开启 4369 端口和一个 TCP 端口段。4369 由 epmd 端口映射服务使用，TCP
端口段用于节点间建立连接与通信。

防火墙设置后，EMQ X 需要配置相同的端口段，emqx/etc/emqx.conf 文件:

    ## Distributed node port range
    node.dist_listen_min = 6369
    node.dist_listen_max = 7369

<div id="cluster_hash">

</div>





# CoAP 协议介绍

<div class="todo">

CoAP 协议介绍文档 10 篇

</div>

<div class="toctree">

</div>

> overview





# 管理命令 (Commands)

*EMQ X* 消息服务器提供了 `./bin/emqx_ctl` 的管理命令行。

## status 命令

查询 *EMQ X* 消息服务器运行状态:

    $ ./bin/emqx_ctl status
    
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running

## mgmt 命令

mgmt 命令查询应用程序。

|                                  |                     |
| -------------------------------- | ------------------- |
| mgmt list                        | 列出应用程序列表            |
| mgmt insert \<AppId\> \<Name\>   | 添加 REST API 的应用程序   |
| mgmt update \<AppId\> \<status\> | 更新 REST API 的应用程序   |
| mgmt lookup \<AppId\>            | 获取 REST API 的应用程序详情 |
| mgmt delete \<AppId\>            | 删除 REST API 的应用程序   |

### mgmt list

列出应用程序列表:

    $ ./bin/emqx_ctl mgmt list
    app_id: 901abdba8eb8c, secret: MjgzMzQ5MjM1MzUzMTc4MjgyMjE3NzU4ODcwMDg0NjQ4OTG, name: hello, desc: , status: true, expired: undefined

### mgmt insert \<AppId\> \<Name\>

添加 REST API 的应用程序:

    $ ./bin/emqx_ctl mgmt insert dbcb6e023370b world
    AppSecret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA

### mgmt update \<AppId\> \<status\>

更新 REST API 的应用程序:

    $ ./bin/emqx_ctl mgmt update dbcb6e023370b stop
    update successfully.

### mgmt lookup \<AppId\>

获取 REST API 的应用程序详情:

    $ ./bin/emqx_ctl mgmt lookup dbcb6e023370b
    app_id: dbcb6e023370b
    secret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA
    name: world
    desc: Application user
    status: stop
    expired: undefined

### mgmt delete \<AppId\>

删除 REST API 的应用程序:

    $ ./bin/emqx_ctl mgmt delete dbcb6e023370b
    ok

## broker 命令

broker
命令查询服务器基本信息，启动时间，统计数据与性能数据。

|                |                                                                        |
| -------------- | ---------------------------------------------------------------------- |
| broker         | 查询 EMQ X 消息服务器描述、版本、启动时间                                               |
| broker stats   | 查询连接 (Connection)、会话 (Session)、主题 (Topic)、 订阅 (Subscription)、路由 (Route) 统计信息 |
| broker metrics | 查询 MQTT 报文 (Packet)、消息 (Message) 收发统计                                     |

查询 *EMQ X* 消息服务器基本信息包括版本、启动时间等:

    $ ./bin/emqx_ctl broker
    
    sysdescr  : EMQ X Broker
    version   : v4.0.0
    uptime    : 25 seconds
    datetime  : 2019-12-19 14:34:19

### broker stats

查询服务器客户端连接 (Connections)、会话 (Sessions)、主题 (Topics)、订阅 (Subscriptions)、路由 (Routes) 统计:

    $ ./bin/emqx_ctl broker stats
    
    actions.count                 : 5
    actions.max                   : 5
    channels.count                : 0
    channels.max                  : 0
    connections.count             : 0
    connections.max               : 0
    resources.count               : 0
    resources.max                 : 0
    retained.count                : 3
    retained.max                  : 3
    routes.count                  : 0
    routes.max                    : 0
    rules.count                   : 0
    rules.max                     : 0
    sessions.count                : 0
    sessions.max                  : 0
    suboptions.count              : 0
    suboptions.max                : 0
    subscribers.count             : 0
    subscribers.max               : 0
    subscriptions.count           : 0
    subscriptions.max             : 0
    subscriptions.shared.count    : 0
    subscriptions.shared.max      : 0
    topics.count                  : 0
    topics.max                    : 0

### broker metrics

查询服务器流量 (Bytes)、MQTT 报文 (Packets)、消息 (Messages) 收发统计:

    $ ./bin/emqx_ctl broker metrics
    
    actions.success               : 0
    bytes.received                : 0
    bytes.sent                    : 0
    client.auth.anonymous         : 0
    client.authenticate           : 0
    client.check_acl              : 0
    client.connack                : 0
    client.connect                : 0
    client.connected              : 0
    client.disconnected           : 0
    client.subscribe              : 0
    client.unsubscribe            : 0
    delivery.dropped              : 0
    delivery.dropped.expired      : 0
    delivery.dropped.no_local     : 0
    delivery.dropped.qos0_msg     : 0
    delivery.dropped.queue_full   : 0
    delivery.dropped.too_large    : 0
    messages.acked                : 0
    messages.delayed              : 0
    messages.delivered            : 0
    messages.dropped              : 0
    messages.dropped.expired      : 0
    messages.dropped.no_subscriber: 0
    messages.forward              : 0
    messages.publish              : 0
    messages.qos0.received        : 0
    messages.qos0.sent            : 0
    messages.qos1.received        : 0
    messages.qos1.sent            : 0
    messages.qos2.received        : 0
    messages.qos2.sent            : 0
    messages.received             : 0
    messages.retained             : 3
    messages.sent                 : 0
    packets.auth.received         : 0
    packets.auth.sent             : 0
    packets.connack.auth_error    : 0
    packets.connack.error         : 0
    packets.connack.sent          : 0
    packets.connect.received      : 0
    packets.disconnect.received   : 0
    packets.disconnect.sent       : 0
    packets.pingreq.received      : 0
    packets.pingresp.sent         : 0
    packets.puback.inuse          : 0
    packets.puback.missed         : 0
    packets.puback.received       : 0
    packets.puback.sent           : 0
    packets.pubcomp.inuse         : 0
    packets.pubcomp.missed        : 0
    packets.pubcomp.received      : 0
    packets.pubcomp.sent          : 0
    packets.publish.auth_error    : 0
    packets.publish.dropped       : 0
    packets.publish.error         : 0
    packets.publish.received      : 0
    packets.publish.sent          : 0
    packets.pubrec.inuse          : 0
    packets.pubrec.missed         : 0
    packets.pubrec.received       : 0
    packets.pubrec.sent           : 0
    packets.pubrel.missed         : 0
    packets.pubrel.received       : 0
    packets.pubrel.sent           : 0
    packets.received              : 0
    packets.sent                  : 0
    packets.suback.sent           : 0
    packets.subscribe.auth_error  : 0
    packets.subscribe.error       : 0
    packets.subscribe.received    : 0
    packets.unsuback.sent         : 0
    packets.unsubscribe.error     : 0
    packets.unsubscribe.received  : 0
    rules.matched                 : 0
    session.created               : 0
    session.discarded             : 0
    session.resumed               : 0
    session.takeovered            : 0
    session.terminated            : 0

## cluster 命令

cluster 命令集群多个 *EMQ X* 消息服务器节点 (进程):

|                              |         |
| ---------------------------- | ------- |
| cluster join \<Node\>        | 加入集群    |
| cluster leave                | 离开集群    |
| cluster force-leave \<Node\> | 从集群删除节点 |
| cluster status               | 查询集群状态  |

cluster 命令集群本机两个 *EMQ X* 节点示例:

|       |                   |         |
| ----- | ----------------- | ------- |
| 目录    | 节点名               | MQTT 端口 |
| emqx1 | <emqx1@127.0.0.1> | 1883    |
| emqx2 | <emqx2@127.0.0.1> | 2883    |

启动 emqx1 :

    $ cd emqx1 && ./bin/emqx start

启动 emqx2 :

    $ cd emqx2 && ./bin/emqx start

emqx2 节点与 emqx1 集群，emqx2 目录下:

    $ ./bin/emqx_ctl cluster join emqx1@127.0.0.1
    
    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx1@127.0.0.1','emqx2@127.0.0.1']}]

任意节点目录下查询集群状态:

    $ ./bin/emqx_ctl cluster status
    
    Cluster status: [{running_nodes,['emqx2@127.0.0.1','emqx1@127.0.0.1']}]

集群消息路由测试:

    # emqx1 节点上订阅 x
    $ mosquitto_sub -t x -q 1 -p 1883
    
    # emqx2 节点上向 x 发布消息
    $ mosquitto_pub -t x -q 1 -p 2883 -m hello

emqx2 节点离开集群:

    $ cd emqx2 && ./bin/emqx_ctl cluster leave

emqx1 节点下删除 emqx2:

    $ cd emqx1 && ./bin/emqx_ctl cluster force-leave emqx2@127.0.0.1

<div class="note">

<div class="admonition-title">

Note

</div>

不支持一个已经在 A 集群中的节点加入另外一个集群，因为这会导致两个集群数据不一致

</div>

## acl 命令

重新加载 acl 配置文件:

    $ ./bin/emqx_ctl acl reload

## clients 命令

clients 命令查询连接的 MQTT 客户端。

|                           |                   |
| ------------------------- | ----------------- |
| clients list              | 查询全部客户端连接         |
| clients show \<ClientId\> | 根据 ClientId 查询客户端 |
| clients kick \<ClientId\> | 根据 ClientId 踢出客户端 |

### clients list

查询全部客户端连接:

    $ ./bin/emqx_ctl clients list
    
    Client (mosqsub/43832-airlee.lo, username=test1, peername=127.0.0.1:62135, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=0, inflight=0, awaiting_rel=0, delivered_msgs=0, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1576477947, connected_at=1576477947)
    Client (mosqsub/44011-airlee.lo, username=test2, peername=127.0.0.1:64961, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=0, inflight=0, awaiting_rel=0, delivered_msgs=0, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1576477950, connected_at=1576477950)
    ...

返回 Client 对象的属性:

|                           |                           |
| ------------------------- | ------------------------- |
| username                  | 用户名                       |
| peername                  | 客户端 IP 与端口                |
| clean\_start              | MQTT Clean Start          |
| keepalive                 | MQTT KeepAlive            |
| session\_expiry\_interval | 会话过期间隔                    |
| subscriptions             | 当前订阅数量                    |
| inflight                  | 当前正在下发的消息数                |
| awaiting\_rel             | 等待客户端发送 PUBREL 的 QoS2 消息数 |
| delivered\_msgs           | EMQ X 向此客户端转发的消息数量 (包含重传)  |
| enqueued\_msgs            | 消息队列当前长度                  |
| dropped\_msgs             | 消息队列达到最大长度后丢弃的消息数量        |
| connected                 | 是否在线                      |
| created\_at               | 会话创建时间                    |
| connected\_at             | 客户端连接时间                   |

### clients show \<ClientId\>

根据 ClientId 查询客户端:

    $ ./bin/emqx_ctl clients show "mosqsub/43832-airlee.lo"
    
    Client (mosqsub/43832-airlee.lo, username=test1, peername=127.0.0.1:62747, clean_start=false, keepalive=60, session_expiry_interval=7200, subscriptions=0, inflight=0, awaiting_rel=0, delivered_msgs=0, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1576479557, connected_at=1576479557)

### clients kick \<ClientId\>

根据 ClientId 踢出客户端:

    $ ./bin/emqx_ctl clients kick "clientid"

## routes 命令

routes 命令查询路由表。

|                       |               |
| --------------------- | ------------- |
| routes list           | 查询全部路由        |
| routes show \<Topic\> | 根据 Topic 查询路由 |

### routes list

查询全部路由:

    $ ./bin/emqx_ctl routes list
    
    t2/# -> emqx2@127.0.0.1
    t/+/x -> emqx2@127.0.0.1,emqx@127.0.0.1

### routes show \<Topic\>

根据 Topic 查询一条路由:

    $ ./bin/emqx_ctl routes show t/+/x
    
    t/+/x -> emqx2@127.0.0.1,emqx@127.0.0.1

## subscriptions 命令

subscriptions 命令查询消息服务器的订阅 (Subscription) 表。

|                                                  |                   |
| ------------------------------------------------ | ----------------- |
| subscriptions list                               | 查询全部订阅            |
| subscriptions show \<ClientId\>                  | 查询某个 ClientId 的订阅 |
| subscriptions add \<ClientId\> \<Topic\> \<QoS\> | 手动添加静态订阅          |
| subscriptions del \<ClientId\> \<Topic\>         | 手动删除静态订阅          |

### subscriptions list

查询全部订阅:

    $ ./bin/emqx_ctl subscriptions list
    
    mosqsub/91042-airlee.lo -> t/y:1
    mosqsub/90475-airlee.lo -> t/+/x:2

### subscriptions show \<ClientId\>

查询某个 Client 的订阅:

    $ ./bin/emqx_ctl subscriptions show 'mosqsub/90475-airlee.lo'
    
    mosqsub/90475-airlee.lo -> t/+/x:2

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

手动添加订阅关系:

    $ ./bin/emqx_ctl subscriptions add 'mosqsub/90475-airlee.lo' '/world' 1
    
    ok

### subscriptions del \<ClientId\> \<Topic\>

手动删除订阅关系:

    $ ./bin/emqx_ctl subscriptions del 'mosqsub/90475-airlee.lo' '/world'
    
    ok

## plugins 命令

plugins 命令用于加载、卸载、查询插件应用。 *EMQ X* 消息服务器通过插件扩展认证、定制功能，插件置于 plugins/ 目录下。

|                           |                |
| ------------------------- | -------------- |
| plugins list              | 列出全部插件 (Plugin) |
| plugins load \<Plugin\>   | 加载插件 (Plugin)   |
| plugins unload \<Plugin\> | 卸载插件 (Plugin)   |
| plugins reload \<Plugin\> | 重载插件 (Plugin)   |

<div class="note">

<div class="admonition-title">

Note

</div>

当修改完成某插件的配置文件时，若需要立即生效则需要执行 `reload` 命令。因为 `unload/load` 命令不会编译新的配置文件

</div>

### plugins list

列出全部插件:

    $ ./bin/emqx_ctl plugins list
    
    Plugin (emqx_auth_clientid, version=v4.0.0, description=EMQ X Authentication with ClientId/Password, active=false)
    Plugin (emqx_auth_http, version=v4.0.0, description=EMQ X Authentication/ACL with HTTP API, active=false)
    Plugin (emqx_auth_jwt, version=v4.0.0, description=EMQ X Authentication with JWT, active=false)
    Plugin (emqx_auth_ldap, version=v4.0.0, description=EMQ X Authentication/ACL with LDAP, active=false)
    Plugin (emqx_auth_mongo, version=v4.0.0, description=EMQ X Authentication/ACL with MongoDB, active=false)
    Plugin (emqx_auth_mysql, version=v4.0.0, description=EMQ X Authentication/ACL with MySQL, active=false)
    Plugin (emqx_auth_pgsql, version=v4.0.0, description=EMQ X Authentication/ACL with PostgreSQL, active=false)
    Plugin (emqx_auth_redis, version=v4.0.0, description=EMQ X Authentication/ACL with Redis, active=false)
    Plugin (emqx_auth_username, version=v4.0.0, description=EMQ X Authentication with Username and Password, active=false)
    Plugin (emqx_bridge_mqtt, version=v4.0.0, description=EMQ X Bridge to MQTT Broker, active=false)
    Plugin (emqx_coap, version=v4.0.0, description=EMQ X CoAP Gateway, active=false)
    Plugin (emqx_dashboard, version=v4.0.0, description=EMQ X Web Dashboard, active=true)
    Plugin (emqx_delayed_publish, version=v4.0.0, description=EMQ X Delayed Publish, active=false)
    Plugin (emqx_lua_hook, version=v4.0.0, description=EMQ X Lua Hooks, active=false)
    Plugin (emqx_lwm2m, version=v4.0.0, description=EMQ X LwM2M Gateway, active=false)
    Plugin (emqx_management, version=v4.0.0, description=EMQ X Management API and CLI, active=true)
    Plugin (emqx_plugin_template, version=v4.0.0, description=EMQ X Plugin Template, active=false)
    Plugin (emqx_psk_file, version=v4.0.0, description=EMQX PSK Plugin from File, active=false)
    Plugin (emqx_recon, version=v4.0.0, description=EMQ X Recon Plugin, active=true)
    Plugin (emqx_reloader, version=v4.0.0, description=EMQ X Reloader Plugin, active=false)
    Plugin (emqx_retainer, version=v4.0.0, description=EMQ X Retainer, active=true)
    Plugin (emqx_rule_engine, version=v4.0.0, description=EMQ X Rule Engine, active=true)
    Plugin (emqx_sn, version=v4.0.0, description=EMQ X MQTT SN Plugin, active=false)
    Plugin (emqx_statsd, version=v4.0.0, description=Statsd for EMQ X, active=false)
    Plugin (emqx_stomp, version=v4.0.0, description=EMQ X Stomp Protocol Plugin, active=false)
    Plugin (emqx_web_hook, version=v4.0.0, description=EMQ X Webhook Plugin, active=false)

插件属性:

|             |       |
| ----------- | ----- |
| version     | 插件版本  |
| description | 插件描述  |
| active      | 是否已加载 |

### plugins load \<Plugin\>

加载插件:

    $ ./bin/emqx_ctl plugins load emqx_lua_hook
    
    Plugin emqx_lua_hook loaded successfully.

### plugins unload \<Plugin\>

卸载插件:

    $ ./bin/emqx_ctl plugins unload emqx_lua_hook
    
    Plugin emqx_lua_hook unloaded successfully.

### plugins reload \<Plugin\>

重载插件:

    $ ./bin/emqx_ctl plugins reload emqx_lua_hook
    
    Plugin emqx_lua_hook reloaded successfully.

## vm 命令

vm 命令用于查询 Erlang 虚拟机负载、内存、进程、IO 信息。

|            |                   |
| ---------- | ----------------- |
| vm         | 等同于 vm all        |
| vm all     | 查询 VM 全部信息        |
| vm load    | 查询 VM 负载          |
| vm memory  | 查询 VM 内存          |
| vm process | 查询 VM Erlang 进程数量 |
| vm io      | 查询 VM io 最大文件句柄   |
| vm ports   | 查询 VM 的端口         |

### vm all

查询 VM 全部信息，包括负载、内存、Erlang 进程数量等:

    cpu/load1               : 4.22
    cpu/load5               : 3.29
    cpu/load15              : 3.16
    memory/total            : 99995208
    memory/processes        : 38998248
    memory/processes_used   : 38938520
    memory/system           : 60996960
    memory/atom             : 1189073
    memory/atom_used        : 1173808
    memory/binary           : 100336
    memory/code             : 25439961
    memory/ets              : 7161128
    process/limit           : 2097152
    process/count           : 315
    io/max_fds              : 10240
    io/active_fds           : 0
    ports/count             : 18
    ports/limit             : 1048576

### vm load

查询 VM 负载:

    $ ./bin/emqx_ctl vm load
    
    cpu/load1               : 2.21
    cpu/load5               : 2.60
    cpu/load15              : 2.36

### vm memory

查询 VM 内存:

    $ ./bin/emqx_ctl vm memory
    
    memory/total            : 23967736
    memory/processes        : 3594216
    memory/processes_used   : 3593112
    memory/system           : 20373520
    memory/atom             : 512601
    memory/atom_used        : 491955
    memory/binary           : 51432
    memory/code             : 13401565
    memory/ets              : 1082848

### vm process

查询 Erlang 进程数量:

    $ ./bin/emqx_ctl vm process
    
    process/limit           : 2097152
    process/count           : 314

### vm io

查询 IO 最大句柄数:

    $ ./bin/emqx_ctl vm io
    
    io/max_fds              : 10240
    io/active_fds           : 0

### vm ports

查询 VM 的端口:

    $ ./bin/emqx_ctl vm ports
    
    ports/count           : 18
    ports/limit           : 1048576

## mnesia 命令

查询 mnesia 数据库系统状态。

## log 命令

log 命令用于设置日志等级。访问 [Documentation of
logger](http://erlang.org/doc/apps/kernel/logger_chapter.html)
以获取详细信息

|                                                |                          |
| ---------------------------------------------- | ------------------------ |
| log set-level \<Level\>                        | 设置主日志等级和所有 Handlers 日志等级 |
| log primary-level                              | 查看主日志等级                  |
| log primary-lelvel \<Level\>                   | 设置主日志等级                  |
| log handlers list                              | 查看当前安装的所有 Hanlders       |
| log handlers set-level \<HandlerId\> \<Level\> | 设置指定 Hanlder 的日志等级       |

### log set-level \<Level\>

设置主日志等级和所有 Handlers 日志等级:

    $ ./bin/emqx_ctl log set-level debug
    
    debug

### log primary-level

查看主日志等级:

    $ ./bin/emqx_ctl log primary-level
    
    debug

### log primary-level \<Level\>

设置主日志等级:

    $ ./bin/emqx_ctl log primary-level info
    
    info

### log handlers list

查看当前安装的所有 Hanlders:

    $ ./bin/emqx_ctl log handlers list
    
    LogHandler (id=emqx_logger_handler, level=debug, destination=unknown)
    LogHandler (id=file, level=debug, destination=log/emqx.log)
    LogHandler (id=default, level=debug, destination=console)

### log handlers set-level \<HandlerId\> \<Level\>

设置指定 Hanlder 的日志等级:

    $ ./bin/emqx_ctl log handlers set-level emqx_logger_handler error
    
    error

## trace 命令

trace 命令用于追踪某个客户端或
Topic，打印日志信息到文件。

|                                                        |                           |
| ------------------------------------------------------ | ------------------------- |
| trace list                                             | 查询全部开启的追踪                 |
| trace start client \<ClientId\> \<File\> \[\<Level\>\] | 开启 Client 追踪，存储指定等级的日志到文件 |
| trace stop client \<ClientId\>                         | 关闭 Client 追踪              |
| trace start topic \<Topic\> \<File\> \[\<Level\>\]     | 开启 Topic 追踪，存储指定等级的日志到文件  |
| trace stop topic \<Topic\>                             | 关闭 Topic 追踪               |

<div class="note">

<div class="admonition-title">

Note

</div>

使用 trace 之前，需要将主日志等级 (primary logger level) 设置成足够低的值。为提高系统运行性能，默认的主日志等级是
error。

</div>

### trace start client \<ClientId\> \<File\> \[\<Level\>\]

开启 Client 追踪:

    $ ./bin/emqx_ctl log primary-level debug
    
    debug
    
    $ ./bin/emqx_ctl trace start client clientid log/clientid_trace.log
    
    trace clientid clientid successfully
    
    $ ./bin/emqx_ctl trace start client clientid2 log/clientid2_trace.log error
    
    trace clientid clientid2 successfully

### trace stop client \<ClientId\>

关闭 Client 追踪:

    $ ./bin/emqx_ctl trace stop client clientid
    
    stop tracing clientid clientid successfully

### trace start topic \<Topic\> \<File\> \[\<Level\>\]

开启 Topic 追踪:

    $ ./bin/emqx_ctl log primary-level debug
    
    debug
    
    $ ./bin/emqx_ctl trace start topic topic log/topic_trace.log
    
    trace topic topic successfully
    
    $ ./bin/emqx_ctl trace start topic topic2 log/topic2_trace.log error
    
    trace topic topic2 successfully

### trace stop topic \<Topic\>

关闭 Topic 追踪:

    $ ./bin/emqx_ctl trace topic topic off
    
    stop tracing topic topic successfully

### trace list

查询全部开启的追踪:

    $ ./bin/emqx_ctl trace list
    
    Trace (clientid=clientid2, level=error, destination="log/clientid2_trace.log")
    Trace (topic=topic2, level=error, destination="log/topic2_trace.log")

## listeners

listeners 命令用于查询开启的 TCP 服务监听器

|                                   |                 |
| --------------------------------- | --------------- |
| listeners                         | 查询开启的 TCP 服务监听器 |
| listeners stop \<Proto\> \<Port\> | 停止监听端口          |

### listeners list

查询开启的 TCP 服务监听器:

    $ ./bin/emqx_ctl listeners
    
    listener on mqtt:ssl:8883
      acceptors       : 16
      max_conns       : 102400
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:tcp:0.0.0.0:1883
      acceptors       : 8
      max_conns       : 1024000
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:tcp:127.0.0.1:11883
      acceptors       : 4
      max_conns       : 1024000
      current_conn    : 2
      shutdown_count  : []
    listener on http:dashboard:18083
      acceptors       : 2
      max_conns       : 512
      current_conn    : 0
      shutdown_count  : []
    listener on http:management:8081
      acceptors       : 2
      max_conns       : 512
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:ws:8083
      acceptors       : 2
      max_conns       : 102400
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:wss:8084
      acceptors       : 2
      max_conns       : 16
      current_conn    : 0
      shutdown_count  : []

listener 参数说明:

|                 |                |
| --------------- | -------------- |
| acceptors       | TCP Acceptor 池 |
| max\_conns      | 最大允许连接数        |
| current\_conns  | 当前连接数          |
| shutdown\_count | Socket 关闭原因统计  |

### listeners stop \<Proto\> \<Port\>

停止监听端口:

    $ ./bin/emqx_ctl listeners stop mqtt:tcp 0.0.0.0:1883
    
    Stop mqtt:tcp listener on 0.0.0.0:1883 successfully.

## 规则引擎 (rule engine) 命令

## rules 命令

|                                                                         |                |
| ----------------------------------------------------------------------- | -------------- |
| rules list                                                              | List all rules |
| rules show \<RuleId\>                                                   | Show a rule    |
| rules create \<name\> \<hook\> \<sql\> \<actions\> \[-d \[\<descr\>\]\] | Create a rule  |
| rules delete \<RuleId\>                                                 | Delete a rule  |

### rules create

创建一个新的规则:

    ## 创建一个测试规则，简单打印所有发送到 't/a' 主题的消息内容
    $ ./bin/emqx_ctl rules create \
      'test1' \
      'message.publish' \
      'select * from "t/a"' \
      '[{"name":"built_in:inspect_action", "params": {"a": 1}}]' \
      -d 'Rule for debug'
    
    Rule test1:1556242324634254201 created

<div class="note">

<div class="admonition-title">

Note

</div>

一个规则由系统生成的规则 ID 标识，所以如果用相同的名字重复添加规则，会生成多个 ID 不同的规则。

</div>

### rules list

列出当前所有的规则:

    $ ./bin/emqx_ctl rules list
    
    rule (id='test1:1556242324634254201', name='test1', for='message.publish', rawsql='select * from "t/a"', actions=[{"name":"built_in:inspect_action","params":{"a":1}}], enabled='true', description='Rule for debug')

### rules show

查询规则:

    ## 查询 RuleID 为 'test1:1556242324634254201' 的规则
    $ ./bin/emqx_ctl rules show 'test1:1556242324634254201'
    
    rule (id='test1:1556242324634254201', name='test1', for='message.publish', rawsql='select * from "t/a"', actions=[{"name":"built_in:inspect_action","params":{"a":1}}], enabled='true', description='Rule for debug')

### rules delete

删除规则:

    ## 删除 RuleID 为 'test1:1556242324634254201' 的规则
    $ ./bin/emqx_ctl rules delete 'test1:1556242324634254201'
    
    ok

## rule-actions 命令

|                                                           |                    |
| --------------------------------------------------------- | ------------------ |
| rule-actions list \[-t \[\<type\>\]\] \[-k \[\<hook\>\]\] | List all actions   |
| rule-actions show \<ActionId\>                            | Show a rule action |

<div class="note">

<div class="admonition-title">

Note

</div>

动作可以由 emqx 内置 (称为系统内置动作)，或者由 emqx 插件编写，但不能通过 CLI/API 添加或删除。

</div>

### rule-actions show

查询动作:

    ## 查询名为 'built_in:inspect_action' 动作
    $ ./bin/emqx_ctl rule-actions show 'built_in:inspect_action'
    
    action (name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

### rule-actions list

列出符合条件的动作:

    ## 列出当前所有的动作
    $ ./bin/emqx_ctl rule-actions list
    
    action (name='built_in:republish_action', app='emqx_rule_engine', for='message.publish', type='built_in', params=#{target_topic => #{description => <<"Repubilsh the message to which topic">>,format => topic,required => true,title => <<"To Which Topic">>,type => string}}, description='Republish a MQTT message to a another topic')
    action (name='web_hook:event_action', app='emqx_web_hook', for='$events', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string},template => #{description => <<"The payload template to be filled with variables before sending messages">>,required => false,schema => #{},title => <<"Payload Template">>,type => object}}, description='Forward Events to Web Server')
    action (name='web_hook:publish_action', app='emqx_web_hook', for='message.publish', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string}}, description='Forward Messages to Web Server')
    action (name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')
    
    ## 列出所有资源类型为 web_hook 的动作
    $ ./bin/emqx_ctl rule-actions list -t web_hook
    
    action (name='web_hook:event_action', app='emqx_web_hook', for='$events', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string},template => #{description => <<"The payload template to be filled with variables before sending messages">>,required => false,schema => #{},title => <<"Payload Template">>,type => object}}, description='Forward Events to Web Server')
    action (name='web_hook:publish_action', app='emqx_web_hook', for='message.publish', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string}}, description='Forward Messages to Web Server')
    
    ## 列出所有 Hook 类型匹配 'client.connected' 的动作
    $ ./bin/emqx_ctl rule-actions list -k 'client.connected'
    
    action (name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

## resources 命令

|                                                                                         |                    |
| --------------------------------------------------------------------------------------- | ------------------ |
| emqx\_ctl resources create \<name\> \<type\> \[-c \[\<config\>\]\] \[-d \[\<descr\>\]\] | Create a resource  |
| resources list \[-t \<ResourceType\>\]                                                  | List all resources |
| resources show \<ResourceId\>                                                           | Show a resource    |
| resources delete \<ResourceId\>                                                         | Delete a resource  |

### resources create

创建一个新的资源:

    $ ./bin/emqx_ctl resources create 'webhook1' 'web_hook' -c '{"url": "http://host-name/chats"}' -d 'forward msgs to host-name/chats'
    
    Resource web_hook:webhook1 created

### resources list

列出当前所有的资源:

    $ ./bin/emqx_ctl resources list
    
    resource (id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

### resources list by type

列出当前所有的资源:

    $ ./bin/emqx_ctl resources list --type 'debug_resource_type'
    
    resource (id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

### resources show

查询资源:

    $ ./bin/emqx_ctl resources show 'web_hook:webhook1'
    
    resource (id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

### resources delete

删除资源:

    $ ./bin/emqx_ctl resources delete 'web_hook:webhook1'
    
    ok

## resource-types 命令

|                              |                         |
| ---------------------------- | ----------------------- |
| resource-types list          | List all resource-types |
| resource-types show \<Type\> | Show a resource-type    |

<div class="note">

<div class="admonition-title">

Note

</div>

资源类型可以由 emqx 内置 (称为系统内置资源类型)，或者由 emqx 插件编写，但不能通过 CLI/API 添加或删除。

</div>

### resource-types list

列出当前所有的资源类型:

    ./bin/emqx_ctl resource-types list
    
    resource_type (name='built_in', provider='emqx_rule_engine', params=#{}, on_create={emqx_rule_actions,on_resource_create}, description='The built in resource type for debug purpose')
    resource_type (name='web_hook', provider='emqx_web_hook', params=#{headers => #{default => #{},description => <<"Request Header">>,schema => #{},title => <<"Request Header">>,type => object},method => #{default => <<"POST">>,description => <<"Request Method">>,enum => [<<"PUT">>,<<"POST">>],title => <<"Request Method">>,type => string},url => #{description => <<"Request URL">>,format => url,required => true,title => <<"Request URL">>,type => string}}, on_create={emqx_web_hook_actions,on_resource_create}, description='WebHook Resource')

### resource-types show

查询资源类型:

    $ ./bin/emqx_ctl resource-types show built_in
    
    resource_type (name='built_in', provider='emqx_rule_engine', params=#{}, on_create={emqx_rule_actions,on_resource_create}, description='The built in resource type for debug purpose')

## recon 命令

|                        |                                                    |
| ---------------------- | -------------------------------------------------- |
| recon memory           | recon\_alloc:memory/2                              |
| recon allocated        | recon\_alloc:memory (allocated\_types, current/max) |
| recon bin\_leak        | recon:bin\_leak (100)                               |
| recon node\_stats      | recon:node\_stats (10, 1000)                        |
| recon remote\_load Mod | recon:remote\_load (Mod)                            |

访问 [Documentation for recon](http://ferd.github.io/recon/) 以获取详细信息。

### recon memory

recon\_alloc:memory/2:

    $ ./bin/emqx_ctl recon memory
    
    usage/current       : 0.810331960305788
    usage/max           : 0.7992495929358717
    used/current        : 84922296
    used/max            : 122519208
    allocated/current   : 104345600
    allocated/max       : 153292800
    unused/current      : 19631520
    unused/max          : 30773592

### recon allocated

recon\_alloc:memory (allocated\_types, current/max):

    $ ./bin/emqx_ctl recon allocated
    
    binary_alloc/current: 425984
    driver_alloc/current: 425984
    eheap_alloc/current : 4063232
    ets_alloc/current   : 3833856
    fix_alloc/current   : 1474560
    ll_alloc/current    : 90439680
    sl_alloc/current    : 163840
    std_alloc/current   : 2260992
    temp_alloc/current  : 655360
    binary_alloc/max    : 4907008
    driver_alloc/max    : 425984
    eheap_alloc/max     : 25538560
    ets_alloc/max       : 5931008
    fix_alloc/max       : 1736704
    ll_alloc/max        : 90439680
    sl_alloc/max        : 20348928
    std_alloc/max       : 2260992
    temp_alloc/max      : 1703936

### recon bin\_leak

recon:bin\_leak (100):

    $ ./bin/emqx_ctl recon bin_leak
    
    {<10623.1352.0>,-3,
     [cowboy_clock,
      {current_function,{gen_server,loop,7}},
      {initial_call,{proc_lib,init_p,5}}]}
    {<10623.3865.0>,0,
     [{current_function,{recon_lib,proc_attrs,2}},
      {initial_call,{erlang,apply,2}}]}
    {<10623.3863.0>,0,
     [{current_function,{dist_util,con_loop,2}},
      {initial_call,{inet_tcp_dist,do_accept,7}}]}
      ...

### recon node\_stats

recon:node\_stats (10, 1000):

    $ ./bin/emqx_ctl recon node_stats
    
    {[{process_count,302},
      {run_queue,0},
      {memory_total,88925536},
      {memory_procs,27999296},
      {memory_atoms,1182843},
      {memory_bin,24536},
      {memory_ets,7163216}],
     [{bytes_in,62},
      {bytes_out,458},
      {gc_count,4},
      {gc_words_reclaimed,3803},
      {reductions,3036},
      {scheduler_usage,[{1,9.473889959272245e-4},
                        {2,5.085983030767205e-5},
                        {3,5.3851477624711046e-5},
                        {4,7.579021269127057e-5},
                        {5,0.0},
                        {6,0.0},
                        {7,0.0},
                        {8,0.0}]}]}
    ...

### recon remote\_load Mod

recon:remote\_load (Mod):

    $ ./bin/emqx_ctl recon remote_load

## retainer 命令

|                 |             |
| --------------- | ----------- |
| retainer info   | 显示保留消息的数量   |
| retainer topics | 显示保留消息的所有主题 |
| retainer clean  | 清除所有保留的消息   |

### retainer info

显示保留消息的数量:

    $ ./bin/emqx_ctl retainer info
    
    retained/total: 3

### retainer topics

显示保留消息的所有主题:

    $ ./bin/emqx_ctl retainer topics
    
    $SYS/brokers/emqx@127.0.0.1/version
    $SYS/brokers/emqx@127.0.0.1/sysdescr
    $SYS/brokers

### retainer clean

清除所有保留的消息:

    $ ./bin/emqx_ctl retainer clean
    
    Cleaned 3 retained messages

## admins 命令

Dashboard 插件会自动注册 admins 命令，用于创建、删除管理员账号，重置管理员密码。

|                                               |             |
| --------------------------------------------- | ----------- |
| admins add \<Username\> \<Password\> \<Tags\> | 创建 admin 账号 |
| admins passwd \<Username\> \<Password\>       | 重置 admin 密码 |
| admins del \<Username\>                       | 删除 admin 账号 |

### admins add \<Username\> \<Password\> \<Tags\>

创建 admin 账户:

    $ ./bin/emqx_ctl admins add root public test
    
    ok

### admins passwd \<Username\> \<Password\>

重置 admin 账户密码:

    $ ./bin/emqx_ctl admins passwd root private
    
    ok

### admins del \<Username\>

删除 admin 账户:

    $ ./bin/emqx_ctl admins del root
    
    ok

## luahook 命令

|                            |                                                         |
| -------------------------- | ------------------------------------------------------- |
| luahook load \<Script\>    | 加载 lua 脚本                                               |
| luahook unload \<Script\>  | 卸载 lua 脚本                                               |
| luahook reload \<Script\>  | 重新加载 lua 脚本                                             |
| luahook enable \<Script\>  | 将名为 \<Script\>.x 的 lua 脚本重命名为 \<Script\> 并加载            |
| luahook disable \<Script\> | 卸载名为 \<Script\> 的 lua 脚本并重命名为 \<Script\>.x，以避免下次启动时自动加载 |

### luahook load \<Script\>

加载 lua 脚本:

    $ ./bin/emqx_ctl luahook load test.lua
    
    Load "test.lua" successfully

### luahook unload \<Script\>

卸载 lua 脚本:

    $ ./bin/emqx_ctl luahook unload test.lua
    
    Unload "test.lua" successfully

### luahook reload \<Script\>

重新加载 lua 脚本:

    $ ./bin/emqx_ctl luahook reload test.lua
    
    Reload "test.lua" successfully

### luahook enable \<Script\>

将名为 \<Script\>.x 的 lua 脚本重命名为 \<Script\> 并加载:

    $ ./bin/emqx_ctl luahook enable test.lua
    
    Enable "test.lua" successfully

### luahook disable \<Script\>

卸载名为 \<Script\> 的 lua 脚本并重命名为 \<Script\>.x，以避免下次启动时自动加载:

    $ ./bin/emqx_ctl luahook disable test.lua
    
    Disable "test.lua" successfully





# 配置说明 (Configuration)

## EMQ X 配置文件

*EMQ X* 消息服务器通过 etc/ 目录下配置文件进行设置，主要配置文件包括:

|                     |                   |
| ------------------- | ----------------- |
| 配置文件                | 说明                |
| etc/emqx.conf       | EMQ X 消息服务器配置文件   |
| etc/acl.conf        | EMQ X 默认 ACL 规则配置文件 |
| etc/plugins/\*.conf | EMQ X 各类插件配置文件    |

## EMQ X 配置变更历史

为方便用户与插件开发者使用，\*EMQ X\* 配置文件经过四次调整。

1.  EMQ X 1.x 版本采用 Erlang 原生配置文件格式 etc/emqttd.config:

<!-- end list -->

``` sourceCode erlang
{emqttd, [
  %% Authentication and Authorization
  {access, [
    %% Authetication. Anonymous Default
    {auth, [
        %% Authentication with username, password
        %{username, []},

        %% Authentication with clientid
        %{clientid, [{password, no}, {file, "etc/clients.config"}]},
```

Erlang 的原生配置格式多层级嵌套，对非 Erlang 开发者的用户很不友好。

2.  EMQ X 2.0-beta.x 版本简化了原生 Erlang 配置文件，采用类似 rebar.config 或 relx.config
    格式:

<!-- end list -->

``` sourceCode erlang
%% Max ClientId Length Allowed.
{mqtt_max_clientid_len, 512}.

%% Max Packet Size Allowed, 64K by default.
{mqtt_max_packet_size, 65536}.

%% Client Idle Timeout.
{mqtt_client_idle_timeout, 30}. % Second
```

简化后的 Erlang 原生配置格式方便用户配置，但插件开发者不得不依赖 gen\_conf 库，而不是通过
appliaton:get\_env 读取配置参数。

3.  EMQ X 2.0-rc.2 正式版集成了 cuttlefish 库，采用了类似 sysctl 的 k = v
    通用格式，并在系统启动时翻译成 Erlang 原生配置格式:

<!-- end list -->

``` sourceCode properties
## Node name
node.name = emq@127.0.0.1

## Max ClientId Length Allowed.
mqtt.max_clientid_len = 1024
```

4.  EMQ X 3.0-beta.1 测试版正式更名 emqttd 为 emqx ，配置名称与配置信息进行相关变化:

<!-- end list -->

``` sourceCode properties
## Profile
etc/emq.config  ==> etc/emqx.config

## Node name
原先:
node.name = emq@127.0.0.1
现在:
node.name = emqx@127.0.0.1
```

EMQ X
    启动时配置文件处理流程:

    ----------------------                                          3.0/schema/*.schema      -------------------
    | etc/emqx.conf      |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------

## EMQ X 环境变量

|                    |                                       |
| ------------------ | ------------------------------------- |
| EMQX\_NODE\_NAME   | Erlang 节点名称，例如: <emqx@127.0.0.1>      |
| EMQX\_NODE\_COOKIE | Erlang 分布式节点通信 Cookie                 |
| EMQX\_MAX\_PORTS   | Erlang 虚拟机最大允许打开文件 Socket 数           |
| EMQX\_TCP\_PORT    | MQTT/TCP 监听端口，默认: 1883                |
| EMQX\_SSL\_PORT    | MQTT/SSL 监听端口，默认: 8883                |
| EMQX\_WS\_PORT     | MQTT/WebSocket 监听端口，默认: 8083          |
| EMQX\_WSS\_PORT    | MQTT/WebSocket with SSL 监听端口，默认: 8084 |

## EMQ X 集群设置

集群名称：

``` sourceCode properties
cluster.name = emqxcl
```

指定 Erlang 分布式协议:

``` sourceCode properties
cluster.proto_dist = inet_tcp
```

集群发现策略：

``` sourceCode properties
cluster.discovery = manual
```

启用集群自愈：

``` sourceCode properties
cluster.autoheal = on
```

宕机节点自动清除周期：

``` sourceCode properties
cluster.autoclean = 5m
```

## EMQ X 集群自动发现

EMQ X 版本支持多种策略的节点自动发现与集群:

| 策略     | 说明                |
| ------ | ----------------- |
| manual | 手工命令创建集群          |
| static | 静态节点列表自动集群        |
| mcast  | UDP 组播方式自动集群      |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

**manual 手动创建集群 **

默认配置为手动创建集群，节点通过 ./bin/emqx\_ctl join \<Node\> 命令加入:

``` sourceCode properties
cluster.discovery = manual
```

** 基于 static 节点列表自动集群 **

集群发现策略为 static:

``` sourceCode properties
cluster.discovery = static
```

静态节点列表:

``` sourceCode properties
cluster.static.seeds = emqx1@127.0.0.1,emqx2@127.0.0.1
```

** 基于 mcast 组播自动集群 **

集群发现策略为 mcast:

``` sourceCode properties
cluster.discovery = mcast
```

IP 组播地址:

``` sourceCode properties
cluster.mcast.addr = 239.192.0.1
```

组播端口范围:

``` sourceCode properties
cluster.mcast.ports = 4369,4370
```

网卡地址:

``` sourceCode properties
cluster.mcast.iface = 0.0.0.0
```

组播 TTL:

``` sourceCode properties
cluster.mcast.ttl = 255
```

是否循环发送组播报文:

``` sourceCode properties
cluster.mcast.loop = on
```

** 基于 DNS A 记录自动集群 **

集群发现策略为 dns:

``` sourceCode properties
cluster.discovery = dns
```

dns 名字:

``` sourceCode properties
cluster.dns.name = localhost
```

用于和 IP 地址一起构建节点名字的应用名字:

``` sourceCode properties
cluster.dns.app  = emqx
```

** 基于 etcd 自动集群 **

集群发现策略为 etcd:

``` sourceCode properties
cluster.discovery = etcd
```

etcd 服务器列表，以 `,` 进行分隔:

``` sourceCode properties
cluster.etcd.server = http://127.0.0.1:2379
```

用于 etcd 中节点路径的前缀，集群中的每个节点都会在 etcd 创建以下路径:
v2/keys/\<prefix\>/\<cluster.name\>/\<node.name\>:

``` sourceCode properties
cluster.etcd.prefix = emqxcl
```

etcd 中节点的 TTL:

``` sourceCode properties
cluster.etcd.node_ttl = 1m
```

包含客户端私有 PEM 编码密钥文件的路径:

``` sourceCode properties
cluster.etcd.ssl.keyfile = etc/certs/client-key.pem
```

包含客户端证书文件的路径:

``` sourceCode properties
cluster.etcd.ssl.certfile = etc/certs/client.pem
```

包含 PEM 编码的 CA 证书文件的路径:

``` sourceCode properties
cluster.etcd.ssl.cacertfile = etc/certs/ca.pem
```

** 基于 Kubernetes 自动集群 **

集群发现策略为 k8s:

``` sourceCode properties
cluster.discovery = k8s
```

Kubernetes API 服务器列表，以 `,` 进行分隔:

``` sourceCode properties
cluster.k8s.apiserver = http://10.110.111.204:8080
```

帮助查找集群中的 EMQ X 节点的服务名称:

``` sourceCode properties
cluster.k8s.service_name = emqx
```

用于从 k8s 服务中提取 host 的地址类型:

``` sourceCode properties
cluster.k8s.address_type = ip
```

EMQ X 的节点名称:

``` sourceCode properties
cluster.k8s.app_name = emqx
```

Kubernetes 的命名空间:

``` sourceCode properties
cluster.k8s.namespace = default
```

## EMQ X 节点与 Cookie

Erlang 节点名称:

``` sourceCode properties
node.name = emqx@127.0.0.1
```

Erlang 分布式节点间通信 Cookie:

``` sourceCode properties
node.cookie = emqxsecretcookie
```

<div class="note">

<div class="admonition-title">

Note

</div>

Erlang/OTP 平台应用多由分布的 Erlang 节点 (进程) 组成，每个 Erlang 节点 (进程) 需指配一个节点名，用于节点间通信互访。
所有互相通信的 Erlang 节点 (进程) 间通过一个共用的 Cookie 进行安全认证。

</div>

## EMQ X 节点连接方式

*EMQ X* 节点基于 Erlang/OTP 平台的 IPv4, IPv6 或 TLS 协议连接:

``` sourceCode properties
## 指定 Erlang 分布式通信协议: inet_tcp | inet6_tcp | inet_tls
cluster.proto_dist = inet_tcp

## 指定 Erlang 分布式通信 SSL 的参数配置
## node.ssl_dist_optfile = etc/ssl_dist.conf
```

## Erlang 虚拟机参数

Erlang 运行时系统的心跳监控功能。注释此行以禁用心跳监控，或将值设置为 `on` 启用:

``` sourceCode properties
node.heartbeat = on
```

异步线程池中的线程数，有效范围为 0-1024:

``` sourceCode properties
node.async_threads = 32
```

Erlang 虚拟机允许的最大进程数，一个 MQTT 连接会消耗 2 个 Erlang 进程:

``` sourceCode properties
node.process_limit = 2048000
```

Erlang 虚拟机允许的最大 Port 数量，一个 MQTT 连接消耗 1 个 Port:

``` sourceCode properties
node.max_ports = 1024000
```

分配缓冲区繁忙限制:

``` sourceCode properties
node.dist_buffer_size = 8MB
```

ETS 表的最大数量。注意，mnesia 和 SSL 将创建临时 ETS 表:

``` sourceCode properties
node.max_ets_tables = 256000
```

调整 GC 以更频繁地运行:

``` sourceCode properties
node.fullsweep_after = 1000
```

崩溃转储日志文件位置:

``` sourceCode properties
node.crash_dump = log/crash.dump
```

Erlang 分布式使用 TLS 时存储 SSL/TLS 选项的文件:

``` sourceCode properties
node.ssl_dist_optfile = etc/ssl_dist.conf
```

分布式节点的滴答时间:

``` sourceCode properties
node.dist_net_ticktime = 60
```

Erlang 分布式节点间通信使用 TCP 连接的端口范围:

``` sourceCode properties
node.dist_listen_min = 6396
node.dist_listen_max = 6396
```

## RPC 参数配置

RPC 模式 (sync | async):

``` sourceCode properties
rpc.mode = async
```

RPC async 模式的最大批量消息数:

``` sourceCode properties
rpc.async_batch_size = 256
```

RPC 本地监听的 TCP 端口:

``` sourceCode properties
rpc.tcp_server_port = 5369
```

RPC 对端监听的 TCP 端口:

``` sourceCode properties
rpc.tcp_client_port = 5369
```

RPC 的 TCP 连接个数:

``` sourceCode properties
rpc.tcp_client_num = 32
```

RPC 连接超时时间:

``` sourceCode properties
rpc.connect_timeout = 5s
```

RPC 发送超时时间:

``` sourceCode properties
rpc.send_timeout = 5s
```

认证超时时间:

``` sourceCode properties
rpc.authentication_timeout = 5s
```

同步调用超时时间:

``` sourceCode properties
rpc.call_receive_timeout = 15s
```

socket 空闲时最大保持连接时间:

``` sourceCode properties
rpc.socket_keepalive_idle = 900s
```

socket 保活探测间隔:

``` sourceCode properties
rpc.socket_keepalive_interval = 75s
```

关闭连接前心跳探测最大失败次数:

``` sourceCode properties
rpc.socket_keepalive_count = 9
```

RPC 的 TCP 发送缓存大小:

``` sourceCode properties
rpc.socket_sndbuf = 1MB
```

RPC 的 TCP 发送缓存大小:

``` sourceCode properties
rpc.socket_recbuf = 1MB
```

RPC 的 Socket (用户态) 缓存大小:

``` sourceCode properties
rpc.socket_buffer = 1MB
```

## 日志参数配置

日志输出位置，可设置写到终端或写到文件:

``` sourceCode properties
log.to = both
```

设置日志级别:

``` sourceCode properties
log.level = warning
```

设置 primary logger level，以及所有到文件和终端的 logger handlers 的日志级别。

设置日志文件的存储路径:

``` sourceCode properties
log.dir = log
```

设置存储 “log.level” 日志的文件名:

``` sourceCode properties
log.file = emqx.log
```

设置每个日志文件的最大大小:

``` sourceCode properties
log.rotation.size = 10MB
```

设置循环日志记录的最大文件数量:

``` sourceCode properties
log.rotation.count = 5
```

可以通过配置额外的 file logger handlers，将某个级别的日志写到单独的文件，配置格式为 log.$level.file =
$filename.

例如，下面的配置将所有的大于等于 info 级别的日志额外写到 info.log 文件中:

``` sourceCode properties
log.info.file = info.log
```

## 匿名认证与 ACL 文件

是否允许客户端以匿名身份通过验证:

``` sourceCode properties
allow_anonymous = true
```

*EMQ X* 支持基于内置 ACL 以及 MySQL、 PostgreSQL 等插件的 ACL。

设置所有 ACL 规则都不能匹配时是否允许访问:

``` sourceCode properties
acl_nomatch = allow
```

设置存储 ACL 规则的默认文件:

``` sourceCode properties
acl_file = etc/acl.conf
```

设置是否允许 ACL 缓存:

``` sourceCode properties
enable_acl_cache = on
```

设置每个客户端 ACL 最大缓存数量:

``` sourceCode properties
acl_cache_max_size = 32
```

设置 ACL 缓存的有效时间:

``` sourceCode properties
acl_cache_ttl = 1m
```

etc/acl.conf 访问控制规则定义:

    允许 | 拒绝  用户 | IP 地址 | ClientID  发布 | 订阅  主题列表

访问控制规则采用 Erlang 元组格式，访问控制模块逐条匹配规则:

![image](_static/images/config_1.png)

etc/acl.conf 默认访问规则设置:

允许 `dashboard` 用户订阅 `$SYS/#`:

``` sourceCode erlang
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
```

允许本机用户发布订阅全部主题:

``` sourceCode erlang
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.
```

拒绝除本机用户以外的其他用户订阅 `$SYS/#` 与 `#` 主题:

``` sourceCode erlang
{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.
```

允许上述规则以外的任何情形:

``` sourceCode erlang
{allow, all}.
```

<div class="note">

<div class="admonition-title">

Note

</div>

默认规则只允许本机用户订阅 $SYS/\# 与 \#。

</div>

*EMQ X* 消息服务器接收到 MQTT 客户端发布 (Publish) 或订阅 (Subscribe) 请求时，会逐条匹配 ACL
规则，直到匹配成功返回 allow 或 deny。

设置 flapping 的检测策略:

``` sourceCode properties
## flapping_detect_policy = <Threshold>, <Duration>, <Banned Interval>
## <Threshold>: 指定 MQTT 客户端在 <Duration> 时间内能够重复连接的最大次数
## <Duration>: 指定 flapping 检测的时间窗口
## <Banned Interval>: 指定 MQTT 客户端超出连接限制后被禁止登录的时长
flapping_detect_policy = 30, 1m, 5m
```

## MQTT 协议参数配置

MQTT 最大报文尺寸:

``` sourceCode properties
mqtt.max_packet_size = 1MB
```

ClientId 最大长度:

``` sourceCode properties
mqtt.max_clientid_len = 65535
```

Topic 最大层级，0 表示没有限制:

``` sourceCode properties
mqtt.max_topic_levels = 0
```

允许的最大 QoS:

``` sourceCode properties
mqtt.max_qos_allowed = 2
```

Topic Alias 最大数量，0 表示不支持 Topic Alias:

``` sourceCode properties
mqtt.max_topic_alias = 0
```

是否支持 MQTT 保留消息:

``` sourceCode properties
mqtt.retain_available = true
```

是否支持 MQTT 通配符订阅:

``` sourceCode properties
mqtt.wildcard_subscription = true
```

是否支持 MQTT 共享订阅:

``` sourceCode properties
mqtt.shared_subscription = true
```

是否允许消息的 loop deliver:

``` sourceCode properties
mqtt.ignore_loop_deliver = false
```

此配置主要为 MQTT v3.1.1 使用，以实现 MQTT 5 中 No Local 的功能。

是否使用严格模式解析 MQTT 报文:

``` sourceCode properties
mqtt.strict_mode = false
```

## MQTT Zones 参数配置

EMQ X 使用 Zone 来管理配置组。一个 Zone 定义了一组配置项 (比如最大连接数等)，Listener 可以指定使用某个
Zone，以使用该 Zone 下的所有配置。多个 Listener 可以共享同一个 Zone。

Listener 使用配置的匹配规则如下，其优先级 Zone \> Global \> Default:

    ---------              ----------              -----------

>   - Listeners -------\> | Zone | --nomatch--\> | Global |
>     --nomatch--\> | Default |
>     
>       - \--------- ---------- ----------- | | |
>         
>           - match match match  
>             |/ |/ |/
> 
> > Zone Configs Global Configs Default Configs

*EMQ X* 支持 `zone.$name.xxx` 替换成相应的 `$name` 的，这里的 `zone.external.xxx` 和
`zone.internal.xxx` 中的 `$name` 都可以换成相应的名称，也可以新增自定义 `name` 的
`zone.$name.xxx`。

### External Zone 参数设置

TCP 连接建立后等待 MQTT CONNECT 报文的最长时间:

``` sourceCode properties
zone.external.idle_timeout = 15s
```

发布消息速率限制:

``` sourceCode properties
## zone.external.publish_limit = 10,100
```

开启 ACL 检查:

``` sourceCode properties
zone.external.enable_acl = on
```

开启黑名单检查:

``` sourceCode properties
zone.external.enable_ban = on
```

是否统计每个连接的信息:

``` sourceCode properties
zone.external.enable_stats = on
```

指定没有通过 ACL 检查时的动作:

``` sourceCode properties
## 可选值: ignore | disconnect
zone.external.acl_deny_action = ignore
```

设置连接 / 会话进程在接收多少消息或字节后强制进行 GC:

``` sourceCode properties
zone.external.force_gc_policy = 1000|1MB
```

设置连接 / 会话进程可使用的最大消息队列长度和堆大小，超出限制时将强制关闭进程:

``` sourceCode properties
## zone.external.force_shutdown_policy = 8000|800MB
```

MQTT 最大报文尺寸:

``` sourceCode properties
## zone.external.max_packet_size = 64KB
```

ClientId 最大长度:

``` sourceCode properties
## zone.external.max_clientid_len = 1024
```

Topic 最大层级，0 表示没有限制:

``` sourceCode properties
## zone.external.max_topic_levels = 7
```

允许的最大 QoS:

``` sourceCode properties
## zone.external.max_qos_allowed = 2
```

Topic Alias 最大数量，0 表示不支持 Topic Alias:

``` sourceCode properties
## zone.external.max_topic_alias = 0
```

是否支持 MQTT 保留消息:

``` sourceCode properties
## zone.external.retain_available = true
```

是否支持 MQTT 通配符订阅:

``` sourceCode properties
## zone.external.wildcard_subscription = false
```

是否支持 MQTT 共享订阅:

``` sourceCode properties
## zone.external.shared_subscription = false
```

服务器允许的保持连接时间，注释此行表示保持连接时间由客户端决定:

``` sourceCode properties
## zone.external.server_keepalive = 0
```

Keepalive \* backoff \* 2 为实际的保持连接时间:

``` sourceCode properties
zone.external.keepalive_backoff = 0.75
```

允许的最大主题订阅数量，0 表示没有限制:

``` sourceCode properties
zone.external.max_subscriptions = 0
```

是否允许 QoS 升级:

``` sourceCode properties
zone.external.upgrade_qos = off
```

飞行窗口的最大大小:

``` sourceCode properties
zone.external.max_inflight = 32
```

QoS1/2 消息的重传间隔:

``` sourceCode properties
zone.external.retry_interval = 30s
```

等待 PUBREL 的 QoS2 消息最大数量 (Client -\> Broker)，0 表示没有限制:

``` sourceCode properties
zone.external.max_awaiting_rel = 100
```

QoS2 消息 (Client -\> Broker) 被删除前等待 PUBREL 的最大时间

``` sourceCode properties
zone.external.await_rel_timeout = 300s
```

MQTT v3.1.1 连接中使用的默认会话过期时间:

``` sourceCode properties
zone.external.session_expiry_interval = 2h
```

消息队列最大长度:

``` sourceCode properties
zone.external.max_mqueue_len = 1000
```

主题优先级:

``` sourceCode properties
## zone.external.mqueue_priorities = topic/1=10,topic/2=8
```

消息队列是否存储 QoS0 消息:

``` sourceCode properties
zone.external.mqueue_store_qos0 = true
```

是否开启 flapping 检测:

``` sourceCode properties
zone.external.enable_flapping_detect = off
```

挂载点:

``` sourceCode properties
## zone.external.mountpoint = devicebound/
```

是否使用用户名作为客户端标识符:

``` sourceCode properties
zone.external.use_username_as_clientid = false
```

是否接收自己发布的消息:

``` sourceCode properties
zone.external.ignore_loop_deliver = false
```

是否使用严格模式解析 MQTT 报文:

``` sourceCode properties
zone.external.strict_mode = false
```

### Internal Zone 参数设置

允许匿名访问:

``` sourceCode properties
zone.internal.allow_anonymous = true
```

是否统计每个连接的信息:

``` sourceCode properties
zone.internal.enable_stats = on
```

关闭 ACL 检查:

``` sourceCode properties
zone.internal.enable_acl = off
```

指定没有通过 ACL 检查时的动作:

``` sourceCode properties
## 可选值: ignore | disconnect
zone.internal.acl_deny_action = ignore
```

是否支持 MQTT 通配符订阅:

``` sourceCode properties
## zone.internal.wildcard_subscription = true
```

是否支持 MQTT 共享订阅:

``` sourceCode properties
## zone.internal.shared_subscription = true
```

允许的最大主题订阅数量，0 表示没有限制:

``` sourceCode properties
zone.internal.max_subscriptions = 0
```

飞行窗口的最大大小:

``` sourceCode properties
zone.internal.max_inflight = 32
```

等待 PUBREL 的 QoS2 消息最大数量 (Client -\> Broker)，0 表示没有限制:

``` sourceCode properties
zone.internal.max_awaiting_rel = 100
```

消息队列最大长度:

``` sourceCode properties
zone.internal.max_mqueue_len = 1000
```

消息队列是否存储 QoS0 消息:

``` sourceCode properties
zone.internal.mqueue_store_qos0 = true
```

是否开启 flapping 检测:

``` sourceCode properties
zone.internal.enable_flapping_detect = off
```

挂载点:

``` sourceCode properties
## zone.internal.mountpoint = devicebound/
```

是否使用用户名作为客户端标识符:

``` sourceCode properties
zone.internal.use_username_as_clientid = false
```

是否接收自己发布的消息:

``` sourceCode properties
zone.internal.ignore_loop_deliver = false
```

是否使用严格模式解析 MQTT 报文:

``` sourceCode properties
zone.internal.strict_mode = false
```

## MQTT Listeners 参数说明

*EMQ X* 消息服务器支持 MQTT、MQTT/SSL、MQTT/WS 协议服务端，可通过
listener.tcp|ssl|ws|wss|.\* 设置端口、最大允许连接数等参数。

*EMQ X* 消息服务器默认开启的 TCP 服务端口包括:

|      |                            |
| ---- | -------------------------- |
| 1883 | MQTT TCP 协议端口              |
| 8883 | MQTT/TCP SSL 端口            |
| 8083 | MQTT/WebSocket 端口          |
| 8084 | MQTT/WebSocket with SSL 端口 |

Listener 参数说明:

|                                       |                             |
| ------------------------------------- | --------------------------- |
| listener.tcp.${name}.acceptors        | TCP Acceptor 池              |
| listener.tcp.${name}.max\_connections | 最大允许 TCP 连接数                |
| listener.tcp.${name}.max\_conn\_rate  | 连接限制配置，例如连接 1000 / 秒: "1000"   |
| listener.tcp.${name}.zone             | 监听属于哪一个 Zone                |
| listener.tcp.${name}.rate\_limit      | 连接速率配置，例如限速 10B / 秒: "100,200" |

## MQTT/TCP 监听器 - 1883

*EMQ X* 版本支持配置多个 MQTT 协议监听器，例如配置名为 external、internal 两个监听器:

TCP 监听器:

``` sourceCode properties
listener.tcp.external = 0.0.0.0:1883
```

接收池大小:

``` sourceCode properties
listener.tcp.external.acceptors = 8
```

最大并发连接数:

``` sourceCode properties
listener.tcp.external.max_connections = 1024000
```

每秒最大创建连接数:

``` sourceCode properties
listener.tcp.external.max_conn_rate = 1000
```

监听器使用的 Zone:

``` sourceCode properties
listener.tcp.external.zone = external
```

TCP 数据接收速率限制:

``` sourceCode properties
## 限制每 10s 内只能接收 100KB 数据
## listener.tcp.external.rate_limit = 100KB,10s
```

访问控制规则:

``` sourceCode properties
listener.tcp.external.access.1 = allow all
```

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

``` sourceCode properties
## listener.tcp.external.proxy_protocol = on
```

代理协议的超时时间:

``` sourceCode properties
## listener.tcp.external.proxy_protocol_timeout = 3s
```

启用基于 X.509 证书的身份验证选项。EMQ X 将使用证书的公共名称作为 MQTT 用户名:

``` sourceCode properties
## listener.tcp.external.peer_cert_as_username = cn
```

挂起连接的队列的最大长度:

``` sourceCode properties
listener.tcp.external.backlog = 1024
```

TCP 发送超时时间:

``` sourceCode properties
listener.tcp.external.send_timeout = 15s
```

发送超时时是否关闭 TCP 连接:

``` sourceCode properties
listener.tcp.external.send_timeout_close = on
```

用于 MQTT 连接的 TCP 接收缓冲区 (os 内核):

``` sourceCode properties
#listener.tcp.external.recbuf = 2KB
```

用于 MQTT 连接的 TCP 发送缓冲区 (os 内核):

``` sourceCode properties
#listener.tcp.external.sndbuf = 2KB
```

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆， 它们对应于内核套接字缓冲区。建议使用
val (buffer) \>= max (val (sndbuf)，val (recbuf)) 来避免不必要的复制带来的性能问题。当设置 sndbuf
或 recbuf 值时，val (buffer) 自动设置为上述最大值:

``` sourceCode properties
#listener.tcp.external.buffer = 2KB
```

是否设置 buffer = max (sndbuf, recbuf):

``` sourceCode properties
## listener.tcp.external.tune_buffer = off
```

是否设置 TCP\_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

``` sourceCode properties
listener.tcp.external.nodelay = true
```

是否设置 SO\_REUSEADDR 标志:

``` sourceCode properties
listener.tcp.external.reuseaddr = true
```

## MQTT/SSL 监听器 - 8883

SSL 监听端口:

``` sourceCode properties
listener.ssl.external = 8883
```

接收池大小:

``` sourceCode properties
listener.ssl.external.acceptors = 16
```

最大并发连接数:

``` sourceCode properties
listener.ssl.external.max_connections = 102400
```

每秒最大创建连接数:

``` sourceCode properties
listener.ssl.external.max_conn_rate = 500
```

指定 SSL 的 {active, N} 选项:

``` sourceCode properties
listener.ssl.external.active_n = 100
```

监听器使用的 Zone:

``` sourceCode properties
listener.ssl.external.zone = external
```

TCP 数据接收速率限制:

``` sourceCode properties
## listener.ssl.external.rate_limit = 100KB,10s
```

访问控制规则:

``` sourceCode properties
listener.ssl.external.access.1 = allow all
```

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

``` sourceCode properties
## listener.ssl.external.proxy_protocol = on
```

代理协议的超时时间:

``` sourceCode properties
## listener.ssl.external.proxy_protocol_timeout = 3s
```

TLS 版本，防止 POODLE 攻击:

``` sourceCode properties
## listener.ssl.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

TLS 握手超时时间:

``` sourceCode properties
listener.ssl.external.handshake_timeout = 15s
```

包含用户私钥的文件的路径:

``` sourceCode properties
listener.ssl.external.keyfile = etc/certs/key.pem
```

包含用户证书的文件的路径:

``` sourceCode properties
listener.ssl.external.certfile = etc/certs/cert.pem
```

包含 CA 证书的文件的路径:

``` sourceCode properties
## listener.ssl.external.cacertfile = etc/certs/cacert.pem
```

包含 dh-params 的文件的路径:

``` sourceCode properties
## listener.ssl.external.dhfile = etc/certs/dh-params.pem
```

配置 verify 模式，服务器只在 verify\_peer 模式下执行 x509 路径验证，并向客户端发送一个证书请求:

``` sourceCode properties
## listener.ssl.external.verify = verify_peer
```

服务器为 verify\_peer 模式时，如果客户端没有要发送的证书，服务器是否返回失败:

``` sourceCode properties
## listener.ssl.external.fail_if_no_peer_cert = true
```

SSL cipher
suites:

``` sourceCode properties
listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA
```

SSL PSK cipher
suites:

``` sourceCode properties
## listener.ssl.external.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA
```

是否启动更安全的 renegotiation 机制:

``` sourceCode properties
## listener.ssl.external.secure_renegotiate = off
```

是否允许客户端重用一个已存在的会话:

``` sourceCode properties
## listener.ssl.external.reuse_sessions = on
```

是否强制根据服务器指定的顺序而不是客户端指定的顺序设置密码:

``` sourceCode properties
## listener.ssl.external.honor_cipher_order = on
```

使用客户端证书中的 CN、EN 或 CRT 字段作为用户名。注意，“verify” 应该设置为 “verify\_peer”:

``` sourceCode properties
## listener.ssl.external.peer_cert_as_username = cn
```

挂起连接的队列的最大长度:

``` sourceCode properties
## listener.ssl.external.backlog = 1024
```

TCP 发送超时时间:

``` sourceCode properties
## listener.ssl.external.send_timeout = 15s
```

发送超时时是否关闭 TCP 连接:

``` sourceCode properties
## listener.ssl.external.send_timeout_close = on
```

用于 MQTT 连接的 TCP 接收缓冲区 (os 内核):

``` sourceCode properties
#listener.ssl.external.recbuf = 4KB
```

用于 MQTT 连接的 TCP 发送缓冲区 (os 内核):

``` sourceCode properties
## listener.ssl.external.sndbuf = 4KB
```

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆， 它们对应于内核套接字缓冲区。建议使用
val (buffer) \>= max (val (sndbuf)，val (recbuf)) 来避免不必要的复制带来的性能问题。当设置 sndbuf
或 recbuf 值时，val (buffer) 自动设置为上述最大值:

``` sourceCode properties
## listener.ssl.external.buffer = 4KB
```

是否设置 buffer = max (sndbuf, recbuf):

``` sourceCode properties
## listener.ssl.external.tune_buffer = off
```

是否设置 TCP\_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

``` sourceCode properties
## listener.ssl.external.nodelay = true
```

是否设置 SO\_REUSEADDR 标志:

``` sourceCode properties
listener.ssl.external.reuseaddr = true
```

## MQTT/WebSocket 监听器 - 8083

MQTT/WebSocket 监听端口:

``` sourceCode properties
listener.ws.external = 8083
```

MQTT/WebSocket 端点路径:

``` sourceCode properties
listener.ws.external.mqtt_path = /mqtt
```

接收池大小:

``` sourceCode properties
listener.ws.external.acceptors = 4
```

最大并发连接数:

``` sourceCode properties
listener.ws.external.max_connections = 102400
```

每秒最大创建连接数:

``` sourceCode properties
listener.ws.external.max_conn_rate = 1000
```

TCP 数据接收速率限制:

``` sourceCode properties
## listener.ws.external.rate_limit = 100KB,10s
```

监听器使用的 Zone:

``` sourceCode properties
listener.ws.external.zone = external
```

访问控制规则:

``` sourceCode properties
listener.ws.external.access.1 = allow all
```

是否验证协议头是否有效:

``` sourceCode properties
listener.ws.external.verify_protocol_header = on
```

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-For 来识别原始 IP:

``` sourceCode properties
## listener.ws.external.proxy_address_header = X-Forwarded-For
```

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-Port 来识别原始端口:

``` sourceCode properties
## listener.ws.external.proxy_port_header = X-Forwarded-Port
```

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

``` sourceCode properties
## listener.ws.external.proxy_protocol = on
```

代理协议超时时间:

``` sourceCode properties
## listener.ws.external.proxy_protocol_timeout = 3s
```

挂起连接的队列的最大长度:

``` sourceCode properties
listener.ws.external.backlog = 1024
```

TCP 发送超时时间:

``` sourceCode properties
listener.ws.external.send_timeout = 15s
```

发送超时时是否关闭 TCP 连接:

``` sourceCode properties
listener.ws.external.send_timeout_close = on
```

用于 MQTT 连接的 TCP 接收缓冲区 (os 内核):

``` sourceCode properties
## listener.ws.external.recbuf = 2KB
```

用于 MQTT 连接的 TCP 发送缓冲区 (os 内核):

``` sourceCode properties
## listener.ws.external.sndbuf = 2KB
```

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆， 它们对应于内核套接字缓冲区。建议使用
val (buffer) \>= max (val (sndbuf)，val (recbuf)) 来避免不必要的复制带来的性能问题。当设置 sndbuf
或 recbuf 值时，val (buffer) 自动设置为上述最大值:

``` sourceCode properties
## listener.ws.external.buffer = 2KB
```

是否设置 buffer = max (sndbuf, recbuf):

``` sourceCode properties
## listener.ws.external.tune_buffer = off
```

是否设置 TCP\_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

``` sourceCode properties
listener.ws.external.nodelay = true
```

是否压缩 Websocket 消息:

``` sourceCode properties
## listener.ws.external.compress = true
```

Websocket deflate 选项:

``` sourceCode properties
## listener.ws.external.deflate_opts.level = default
## listener.ws.external.deflate_opts.mem_level = 8
## listener.ws.external.deflate_opts.strategy = default
## listener.ws.external.deflate_opts.server_context_takeover = takeover
## listener.ws.external.deflate_opts.client_context_takeover = takeover
## listener.ws.external.deflate_opts.server_max_window_bits = 15
## listener.ws.external.deflate_opts.client_max_window_bits = 15
```

最大空闲时间:

``` sourceCode properties
## listener.ws.external.idle_timeout = 60s
```

最大报文大小，0 表示没有限制:

``` sourceCode properties
## listener.ws.external.max_frame_size = 0
```

## MQTT/WebSocket with SSL 监听器 - 8084

MQTT/WebSocket with SSL 监听端口:

``` sourceCode properties
listener.wss.external = 8084
```

接收池大小:

``` sourceCode properties
listener.wss.external.acceptors = 4
```

最大并发连接数:

``` sourceCode properties
listener.wss.external.max_connections = 16
```

每秒最大创建连接数:

``` sourceCode properties
listener.wss.external.max_conn_rate = 1000
```

TCP 数据接收速率限制:

``` sourceCode properties
## listener.wss.external.rate_limit = 100KB,10s
```

监听器使用的 Zone:

``` sourceCode properties
listener.wss.external.zone = external
```

访问控制规则:

``` sourceCode properties
listener.wss.external.access.1 = allow all
```

是否验证协议头是否有效:

``` sourceCode properties
listener.wss.external.verify_protocol_header = on
```

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-For 来识别原始 IP:

``` sourceCode properties
## listener.wss.external.proxy_address_header = X-Forwarded-For
```

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-Port 来识别原始端口:

``` sourceCode properties
## listener.wss.external.proxy_port_header = X-Forwarded-Port
```

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

``` sourceCode properties
## listener.wss.external.proxy_protocol = on
```

代理协议超时时间:

``` sourceCode properties
## listener.wss.external.proxy_protocol_timeout = 3s
```

TLS 版本，防止 POODLE 攻击:

``` sourceCode properties
## listener.wss.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

包含用户私钥的文件的路径:

``` sourceCode properties
listener.wss.external.keyfile = etc/certs/key.pem
```

包含用户证书的文件的路径:

``` sourceCode properties
listener.wss.external.certfile = etc/certs/cert.pem
```

包含 CA 证书的文件的路径:

``` sourceCode properties
## listener.wss.external.cacertfile = etc/certs/cacert.pem
```

包含 dh-params 的文件的路径:

``` sourceCode properties
## listener.ssl.external.dhfile = etc/certs/dh-params.pem
```

配置 verify 模式，服务器只在 verify\_peer 模式下执行 x509 路径验证，并向客户端发送一个证书请求:

``` sourceCode properties
## listener.wss.external.verify = verify_peer
```

服务器为 verify\_peer 模式时，如果客户端没有要发送的证书，服务器是否返回失败:

``` sourceCode properties
## listener.wss.external.fail_if_no_peer_cert = true
```

SSL cipher
suites:

``` sourceCode properties
## listener.wss.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA
```

是否启动更安全的 renegotiation 机制:

``` sourceCode properties
## listener.wss.external.secure_renegotiate = off
```

是否允许客户端重用一个已存在的会话:

``` sourceCode properties
## listener.wss.external.reuse_sessions = on
```

是否强制根据服务器指定的顺序而不是客户端指定的顺序设置密码:

``` sourceCode properties
## listener.wss.external.honor_cipher_order = on
```

使用客户端证书中的 CN、EN 或 CRT 字段作为用户名。注意，“verify” 应该设置为 “verify\_peer”:

``` sourceCode properties
## listener.wss.external.peer_cert_as_username = cn
```

挂起连接的队列的最大长度:

``` sourceCode properties
listener.wss.external.backlog = 1024
```

TCP 发送超时时间:

``` sourceCode properties
listener.wss.external.send_timeout = 15s
```

发送超时时是否关闭 TCP 连接:

``` sourceCode properties
listener.wss.external.send_timeout_close = on
```

用于 MQTT 连接的 TCP 接收缓冲区 (os 内核):

``` sourceCode properties
## listener.wss.external.recbuf = 4KB
```

用于 MQTT 连接的 TCP 发送缓冲区 (os 内核):

``` sourceCode properties
## listener.wss.external.sndbuf = 4KB
```

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆， 它们对应于内核套接字缓冲区。建议使用
val (buffer) \>= max (val (sndbuf)，val (recbuf)) 来避免不必要的复制带来的性能问题。当设置 sndbuf
或 recbuf 值时，val (buffer) 自动设置为上述最大值:

``` sourceCode properties
## listener.wss.external.buffer = 4KB
```

是否设置 TCP\_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

``` sourceCode properties
## listener.wss.external.nodelay = true
```

是否压缩 Websocket 消息:

``` sourceCode properties
## listener.wss.external.compress = true
```

Websocket deflate 选项:

``` sourceCode properties
## listener.wss.external.deflate_opts.level = default
## listener.wss.external.deflate_opts.mem_level = 8
## listener.wss.external.deflate_opts.strategy = default
## listener.wss.external.deflate_opts.server_context_takeover = takeover
## listener.wss.external.deflate_opts.client_context_takeover = takeover
## listener.wss.external.deflate_opts.server_max_window_bits = 15
## listener.wss.external.deflate_opts.client_max_window_bits = 15
```

最大空闲时间:

``` sourceCode properties
## listener.wss.external.idle_timeout = 60s
```

最大报文大小，0 表示没有限制:

``` sourceCode properties
## listener.wss.external.max_frame_size = 0
```

## Modules 模块

*EMQ X* 支持模块扩展，默认三个模块，分别为上下线消息状态发布模块、代理订阅模块、主题 (Topic) 重写模块。

### 上下线消息状态发布模块

是否启动上下线消息状态发布模块:

``` sourceCode properties
module.presence = on
```

上下线消息状态发布模块发布 MQTT 消息时使用的 QoS:

``` sourceCode properties
module.presence.qos = 1
```

### 代理订阅模块

是否启动代理订阅模块:

``` sourceCode properties
module.subscription = off
```

客户端连接时自动订阅的主题与 QoS:

``` sourceCode properties
## Subscribe the Topics's qos
## module.subscription.1.topic = $client/% c
## module.subscription.1.qos = 0
## module.subscription.2.topic = $user/% u
## module.subscription.2.qos = 1
```

### 主题重写模块

是否启动主题重写模块:

``` sourceCode properties
module.rewrite = off
```

主题重写规则:

``` sourceCode properties
## module.rewrite.rule.1 = x/# ^x/y/(.+)$ z/y/$1
## module.rewrite.rule.2 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2
```

## 扩展插件配置文件

存放插件配置文件的目录:

``` sourceCode properties
plugins.etc_dir = etc/plugins/
```

存储启动时需要自动加载的插件列表的文件的路径:

``` sourceCode properties
plugins.loaded_file = data/loaded_plugins
```

*EMQ X* 插件配置文件，默认在 etc/plugins/ 目录，可修改 plugins.etc\_dir 来调整目录。

## Broker 参数设置

系统消息的发布间隔:

``` sourceCode properties
broker.sys_interval = 1m
```

系统心跳的发布间隔:

``` sourceCode properties
broker.sys_heartbeat = 30s
```

是否全局注册会话:

``` sourceCode properties
broker.enable_session_registry = on
```

会话锁策略:

``` sourceCode properties
broker.session_locking_strategy = quorum
```

共享订阅的分发策略:

``` sourceCode properties
broker.shared_subscription_strategy = random
```

共享分发时是否需要 ACK:

``` sourceCode properties
broker.shared_dispatch_ack_enabled = false
```

是否开启路由批量清理功能:

``` sourceCode properties
broker.route_batch_clean = off
```

## Erlang 虚拟机监控设置

是否开启 long\_gc 监控以及垃圾回收持续多久时会触发 long\_gc 事件，设置为 0 表示不监控此事件:

``` sourceCode properties
sysmon.long_gc = 0
```

系统中的进程或端口不间断地运行多久时会触发 long\_schedule 事件，设置为 0 表示不监控此事件:

``` sourceCode properties
sysmon.long_schedule = 240
```

垃圾回收导致分配的堆大小为多大时将触发 large\_heap 事件:

``` sourceCode properties
sysmon.large_heap = 8MB
```

系统中的进程因为发送到繁忙端口而挂起时是否触发 busy\_port 事件:

``` sourceCode properties
sysmon.busy_port = false
```

是否监控 Erlang 分布式端口繁忙事件:

``` sourceCode properties
sysmon.busy_dist_port = true
```

cpu 占用率的检查周期:

``` sourceCode properties
os_mon.cpu_check_interval = 60s
```

cpu 占用率高于多少时产生告警:

``` sourceCode properties
os_mon.cpu_high_watermark = 80%
```

cpu 占用率低于多少时清除告警:

``` sourceCode properties
os_mon.cpu_low_watermark = 60%
```

内存占用率的检查周期:

``` sourceCode properties
os_mon.mem_check_interval = 60s
```

系统内存占用率高于多少时产生告警:

``` sourceCode properties
os_mon.sysmem_high_watermark = 70%
```

单个进程内存占用率高于多少时产生告警:

``` sourceCode properties
os_mon.procmem_high_watermark = 5%
```

进程数量的检查周期:

``` sourceCode properties
vm_mon.check_interval = 30s
```

当前进程数量与进程数量最大限制的比率达到多少时产生告警:

``` sourceCode properties
vm_mon.process_high_watermark = 80%
```

当前进程数量与进程数量最大限制的比率达到多少时清除告警:

``` sourceCode properties
vm_mon.process_low_watermark = 60%
```





# 部署架构 (Deployment)

EMQ X 消息服务器集群可作为物联网接入服务 (IoT Hub)，部署在青云、AWS、阿里等公有云或企业私有云平台。

典型部署结构:

![image](_static/images/deploy_1.png)

## LB (负载均衡)

LB (负载均衡器) 负责分发设备的 MQTT 连接与消息到 EMQ X 集群，LB 提高 EMQ X 集群可用性、实现负载平衡以及动态扩容。

部署架构推荐在 LB 终结 SSL 连接。设备与 LB 之间 TLS 安全连接，LB 与 EMQ X 之间普通 TCP 连接。这种部署模式下
EMQ X 单集群可轻松支持 100 万设备。

公有云厂商 LB
产品:

| 云计算厂商                            | 是否支持 TLS 终结 | LB 产品介绍                                              |
| -------------------------------- | ----------- | ---------------------------------------------------- |
| [青云](https://qingcloud.com)      | 是           | <https://docs.qingcloud.com/guide/loadbalancer.html> |
| [AWS](https://aws.amazon.com)    | 是           | <https://aws.amazon.com/cn/elasticloadbalancing/>    |
| [阿里云](https://www.aliyun.com)    | 否           | <https://www.aliyun.com/product/slb>                 |
| [UCloud](https://ucloud.cn)      | 未知          | <https://ucloud.cn/site/product/ulb.html>            |
| [QCloud](https://www.qcloud.com) | 未知          | <https://www.qcloud.com/product/clb>                 |

私有部署 LB
服务器:

| 开源 LB                              | 是否支持 TLS 终结 | 方案介绍                                                    |
| ---------------------------------- | ----------- | ------------------------------------------------------- |
| [HAProxy](https://www.haproxy.org) | 是           | <https://www.haproxy.com/solutions/load-balancing.html> |
| [NGINX](https://www.nginx.com)     | PLUS 产品支持   | <https://www.nginx.com/solutions/load-balancing/>       |

国内公有云部署推荐青云 (EMQ X 合作伙伴)，国外部署推荐 AWS 。私有部署推荐使用 HAProxy 作为 LB。

## EMQ X 集群

EMQ X 节点集群部署在 LB 之后，建议部署在 VPC 或私有网络内。公有云厂商青云、AWS、UCloud、QCloud 均支持 VPC
网络。

EMQ X 默认开启的 MQTT 服务 TCP 端口:

|       |                       |
| ----- | --------------------- |
| 1883  | MQTT 协议端口             |
| 8883  | MQTT/SSL 端口           |
| 8083  | MQTT/WebSocket 端口     |
| 8084  | MQTT/WebSocket/SSL 端口 |
| 8080  | 管理 API 端口              |
| 18083 | Dashboard 端口          |

防火墙根据使用的 MQTT 接入方式，开启上述端口的访问权限。

EMQ X 节点集群使用的 TCP 端口:

|      |           |
| ---- | --------- |
| 4369 | 集群节点发现端口  |
| 5369 | 集群节点 PRC 通道 |
| 6369 | 集群节点控制通道  |

集群节点间如有防护墙，需开启上述 TCP 端口互访权限。

## 青云 (QingCloud) 部署

1.  创建 VPC 网络。

2.  VPC 网络内创建 EMQ X 集群 ' 私有网络 '，例如: 192.168.0.0/24

3.  私有网络内创建两台 EMQ X 主机，例如:
    
    > 
    > 
    > |       |             |
    > | ----- | ----------- |
    > | emqx1 | 192.168.0.2 |
    > | emqx2 | 192.168.0.3 |
    > 

4.  安装并集群 EMQ X 主机，具体配置请参考安装集群章节。

5.  创建 LB (负载均衡器) 并指定公网 IP 地址。

6.  在 LB 上创建 MQTT TCP 监听器:

![image](_static/images/deploy_2.png)

或创建 SSL 监听器，并终结 SSL 在 LB :

![image](_static/images/deploy_3.png)

7.  MQTT 客户端连接 LB 公网地址测试。

## 亚马逊 (AWS) 部署

1.  创建 VPC 网络。

2.  VPC 网络内创建 EMQ X 集群 ' 私有网络 '，例如: 192.168.0.0/24

3.  私有网络内创建两台 EMQ X 主机，指定上面创建的 VPC 网络，例如:
    
    > 
    > 
    > |       |             |
    > | ----- | ----------- |
    > | emqx1 | 192.168.0.2 |
    > | emqx2 | 192.168.0.3 |
    > 

4.  在安全组中，开放 MQTT 服务的 TCP 端口，比如 1883, 8883。

5.  安装并集群 EMQ X 主机，具体配置请参考安装集群章节。

6.  创建 ELB (Classic 负载均衡器)，指定 VPC 网络，并指定公网 IP 地址。

7.  在 ELB 上创建 MQTT TCP 监听器:

![image](_static/images/deploy_4.png)

或创建 SSL 监听器，并终结 SSL 在 LB :

![image](_static/images/deploy_5.png)

8.  MQTT 客户端连接 LB 公网地址测试。

## 阿里云部署

<div class="todo">

阿里云 LB 支持终结 SSL?

</div>

## 私有网络部署

### EMQ X 集群直连

EMQ X 集群直接挂 DNS 轮询，设备通过域名或者 IP 地址列表访问:

1.  部署 EMQ X 集群，具体参考 \`程序包安装 \` 与 \`集群配置 \` 文档。
2.  EMQ X 节点防火墙开启外部 MQTT 访问端口，例如 1883, 8883。
3.  设备通过 IP 地址列表或域名访问 EMQ X 集群。

<div class="note">

<div class="admonition-title">

Note

</div>

产品部署不推荐这种部署方式。

</div>

### HAProxy -\> EMQ X 集群

HAProxy 作为 LB 部署 EMQ X 集群，并终结 SSL 连接:

1.  创建 EMQ X 集群节点，例如:

| 节点    | IP 地址       |
| ----- | ----------- |
| emqx1 | 192.168.0.2 |
| emqx2 | 192.168.0.3 |

2.  配置 /etc/haproxy/haproxy.cfg，示例:
    
        listen mqtt-ssl
            bind *:8883 ssl crt /etc/ssl/emqx/emq.pem no-sslv3
            mode tcp
            maxconn 50000
            timeout client 600s
            default_backend emqx_cluster
        
        backend emqx_cluster
            mode tcp
            balance source
            timeout server 50s
            timeout check 5000
            server emqx1 192.168.0.2:1883 check inter 10000 fall 2 rise 5 weight 1
            server emqx2 192.168.0.3:1883 check inter 10000 fall 2 rise 5 weight 1

### NGINX Plus -\> EMQ X 集群

NGINX Plus 产品作为 EMQ X 集群 LB，并终结 SSL 连接:

1.  注册 NGINX Plus 试用版，Ubuntu 下安装: <https://cs.nginx.com/repo_setup>
2.  创建 EMQ X 节点集群，例如:

| 节点    | IP 地址       |
| ----- | ----------- |
| emqx1 | 192.168.0.2 |
| emqx2 | 192.168.0.3 |

3.  配置 /etc/nginx/nginx.conf，示例:
    
        stream {
            # Example configuration for TCP load balancing
        
            upstream stream_backend {
                zone tcp_servers 64k;
                hash $remote_addr;
                server 192.168.0.2:1883 max_fails=2 fail_timeout=30s;
                server 192.168.0.3:1883 max_fails=2 fail_timeout=30s;
            }
        
            server {
                listen 8883 ssl;
                status_zone tcp_server;
                proxy_pass stream_backend;
                proxy_buffer_size 4k;
                ssl_handshake_timeout 15s;
                ssl_certificate     /etc/emqx/certs/cert.pem;
                ssl_certificate_key /etc/emqx/certs/key.pem;
            }
        }





# 架构设计 (Design)

## 前言

*EMQ X* 消息服务器在设计上，首先分离了前端协议 (FrontEnd) 与后端集成 (Backend)，其次分离了消息路由平面 (Flow
Plane) 与监控管理平面 (Monitor/Control Plane):

![image](_static/images/design_1.png)

### 100 万连接

多核服务器和现代操作系统内核层面，可以很轻松支持 100 万 TCP 连接，核心问题是应用层面如何处理业务瓶颈。

EMQ X 消息服务器在业务和应用层面，解决了单节点承载 100 万连接的各类瓶颈问题。连接测试的操作系统内核、TCP 协议栈、Erlang
虚拟机参数参见: <http://docs.emqtt.cn/zh_CN/latest/tune.html>

### 全异步架构

EMQ X 消息服务器是基于 Erlang/OTP 平台的全异步的架构：异步 TCP
连接处理、异步主题 (Topic) 订阅、异步消息发布。只有在资源负载限制部分采用同步设计，比如
TCP 连接创建和 Mnesia 数据库事务执行。

EMQ X 3.0 版本中，一条 MQTT 消息从发布者 (Publisher) 到订阅者 (Subscriber)，在 EMQ X
消息服务器内部异步流过一系列 Erlang 进程 Mailbox:

![image](_static/images/design_2.png)

### 消息持久化

EMQ X 开源产品不支持服务器内部消息持久化，这是一个架构设计选择。首先，EMQ X
解决的核心问题是连接与路由；其次，我们认为内置持久化是个错误设计。

传统内置消息持久化的 MQ 服务器，比如广泛使用的 JMS 服务器
ActiveMQ，几乎每个大版本都在重新设计持久化部分。内置消息持久化在设计上有两个问题:

1.  如何权衡内存与磁盘的使用？消息路由是基于内存的，而消息存储是基于磁盘的。
2.  多服务器分布集群架构下，如何放置 Queue 如何复制 Queue 的消息？

Kafka 在上述问题上，做出了正确的设计：一个完全基于磁盘分布式 Commit Log 的消息服务器。

EMQ X 在设计上分离消息路由与消息存储职责后，数据复制容灾备份甚至应用集成，可以在数据层面灵活实现。

EMQ X 企业版产品中，可以通过规则引擎或插件的方式，持久化消息到
Redis、MongoDB、Cassandra、MySQL、PostgreSQL 等数据库，以及 RabbitMQ、Kafka
等消息队列。

## 系统架构

### 概念模型

EMQ X 消息服务器概念上更像一台网络路由器 (Router) 或交换机 (Switch)，而不是传统的企业级消息队列 (MQ)。相比网络路由器按
IP 地址或 MPLS 标签路由报文，EMQ X 按主题树 (Topic Trie) 发布订阅模式在集群节点间路由 MQTT 消息:

![image](./_static/images/design_3.png)

### 设计原则

1.  EMQ X 消息服务器核心解决的问题：处理海量的并发 MQTT 连接与路由消息。
2.  充分利用 Erlang/OTP 平台软实时、低延时、高并发、分布容错的优势。
3.  连接 (Connection)、会话 (Session)、路由 (Router)、集群 (Cluster) 分层。
4.  消息路由平面 (Flow Plane) 与控制管理平面 (Control Plane) 分离。
5.  支持后端数据库或 NoSQL 实现数据持久化、容灾备份与应用集成。

### 系统分层

1.  连接层 (Connection Layer)：负责 TCP 连接处理、 MQTT 协议编解码。
2.  会话层 (Session Layer)：处理 MQTT 协议发布订阅消息交互流程。
3.  路由层 (Route Layer)：节点内路由派发 MQTT 消息。
4.  分布层 (Distributed Layer)：分布节点间路由 MQTT 消息。
5.  认证与访问控制 (ACL)：连接层支持可扩展的认证与访问控制模块。
6.  钩子 (Hooks) 与插件 (Plugins)：系统每层提供可扩展的钩子，支持插件方式扩展服务器。

## 连接层设计

连接层处理服务端 Socket 连接与 MQTT 协议编解码：

1.  基于 [eSockd](https://github.com/emqx/esockd) 框架的异步 TCP 服务端
2.  TCP Acceptor 池与异步 TCP Accept
3.  TCP/SSL, WebSocket/SSL 连接支持
4.  最大并发连接数限制
5.  基于 IP 地址 (CIDR) 访问控制
6.  基于 Leaky Bucket 的流控
7.  MQTT 协议编解码
8.  MQTT 协议心跳检测
9.  MQTT 协议报文处理

## 会话层设计

会话层处理 MQTT 协议发布订阅 (Publish/Subscribe) 业务交互流程：

1.  缓存 MQTT 客户端的全部订阅 (Subscription)，并终结订阅 QoS
2.  处理 Qos0/1/2 消息接收与下发，消息超时重传与离线消息保存
3.  飞行窗口 (Inflight Window)，下发消息吞吐控制与顺序保证
4.  保存服务器发送到客户端的，已发送未确认的 Qos1/2 消息
5.  缓存客户端发送到服务端，未接收到 PUBREL 的 QoS2 消息
6.  客户端离线时，保存持久会话的离线 Qos1/2 消息

### 消息队列与飞行窗口

会话层通过一个内存消息队列和飞行窗口处理下发消息:

![image](_static/images/design_4.png)

飞行窗口 (Inflight Window) 保存当前正在发送未确认的 Qos1/2 消息。窗口值越大，吞吐越高；窗口值越小，消息顺序越严格。

当客户端离线或者飞行窗口 (Inflight Window) 满时，消息缓存到队列。如果消息队列满，先丢弃 Qos0 消息或最早进入队列的消息。

### 报文 ID 与消息 ID

MQTT 协议定义了一个 16bits 的报文 ID (PacketId)，用于客户端到服务器的报文收发与确认。MQTT
发布报文 (PUBLISH) 进入消息服务器后，转换为一个消息对象并分配 128bits 消息 ID (MessageId)。

全局唯一时间序列消息 ID 结构:

![image](_static/images/design_5.png)

1.  64bits 时间戳: erlang:system\_time if Erlang \>= R18, otherwise
    os:timestamp
2.  Erlang 节点 ID: 编码为 2 字节
3.  Erlang 进程 PID: 编码为 4 字节
4.  进程内部序列号: 2 字节的进程内部序列号

端到端消息发布订阅 (Pub/Sub) 过程中，发布报文 ID 与报文 QoS 终结在会话层，由唯一 ID 标识的 MQTT 消息对象在节点间路由:

![image](_static/images/design_6.png)

## 路由层设计

路由层维护订阅者 (subscriber) 与订阅关系表 (subscription)，并在本节点发布订阅模式派发 (Dispatch) 消息:

![image](./_static/images/design_7.png)

消息派发到会话 (Session) 后，由会话负责按不同 QoS 送达消息。

## 分布层设计

分布层维护全局主题树 (Topic Trie) 与路由表 (Route Table)。主题树由通配主题构成，路由表映射主题到节点:

![image](./_static/images/design_8.png)

分布层通过匹配主题树 (Topic Trie) 和查找路由表 (Route Table)，在集群的节点间转发路由 MQTT 消息:

![image](./_static/images/design_9.png)

## 钩子 (Hook) 设计

### 钩子 (Hook) 定义

*EMQ X* 在客户端生命周期和会话生命周期，对连接、订阅、消息收发位置设计了扩展钩子 (Hook):

| 钩子                   | 说明          |
| -------------------- | ----------- |
| client.connect       | 收到客户端连接报文   |
| client.connack       | 下发连接应答      |
| client.connected     | 客户端上线       |
| client.disconnected  | 客户端连接断开     |
| client.authenticate  | 连接认证        |
| client.check\_acl    | ACL 校验      |
| client.subscribe     | 客户端订阅主题     |
| client.unsubscribe   | 客户端取消订阅主题   |
| session.created      | 会话创建        |
| session.subscribed   | 会话订阅主题后     |
| session.unsubscribed | 会话取消订阅主题后   |
| session.resumed      | 会话恢复        |
| session.discarded    | 会话被删除       |
| session.takeovered   | 会话被其它节点接管   |
| session.terminated   | 会话终止        |
| message.publish      | MQTT 消息发布   |
| message.delivered    | MQTT 消息进行投递 |
| message.acked        | MQTT 消息回执   |
| message.dropped      | MQTT 消息丢弃   |

钩子 (Hook)
采用职责链设计模式 ([Chain-of-responsibility\_pattern](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern))，扩展模块或插件向钩子注册回调函数，系统在客户端上下线、主题订阅或消息发布确认时，触发钩子顺序执行回调函数:

![image](./_static/images/design_10.png)

不同钩子的回调函数输入参数不同，用户可参考插件模版的
[emqx\_plugin\_template](https://github.com/emqx/emqx_plugin_template/blob/master/src/emqx_plugin_template.erl)
模块，每个回调函数应该返回:

| 返回             | 说明         |
| -------------- | ---------- |
| ok             | 继续执行       |
| {ok, NewAcc}   | 返回累积参数继续执行 |
| stop           | 停止执行       |
| {stop, NewAcc} | 返回累积参数停止执行 |

### 钩子 (Hook) 实现

emqx 模块封装了 Hook
接口:

``` sourceCode erlang
-spec (hook (emqx_hooks:hookpoint (), emqx_hooks:action ()) -> ok | {error, already_exists}).
hook (HookPoint, Action) ->
    emqx_hooks:add (HookPoint, Action).

-spec (hook (emqx_hooks:hookpoint (), emqx_hooks:action (), emqx_hooks:filter () | integer ())
    -> ok | {error, already_exists}).
hook (HookPoint, Action, Priority) when is_integer (Priority) ->
    emqx_hooks:add (HookPoint, Action, Priority);
hook (HookPoint, Action, Filter) when is_function (Filter); is_tuple (Filter) ->
    emqx_hooks:add (HookPoint, Action, Filter);
hook (HookPoint, Action, InitArgs) when is_list (InitArgs) ->
    emqx_hooks:add (HookPoint, Action, InitArgs).

-spec (hook (emqx_hooks:hookpoint (), emqx_hooks:action (), emqx_hooks:filter (), integer ())
    -> ok | {error, already_exists}).
hook (HookPoint, Action, Filter, Priority) ->
    emqx_hooks:add (HookPoint, Action, Filter, Priority).

-spec (unhook (emqx_hooks:hookpoint (), emqx_hooks:action ()) -> ok).
unhook (HookPoint, Action) ->
    emqx_hooks:del (HookPoint, Action).

-spec (run_hook (emqx_hooks:hookpoint (), list (any ())) -> ok | stop).
run_hook (HookPoint, Args) ->
    emqx_hooks:run (HookPoint, Args).

-spec (run_fold_hook (emqx_hooks:hookpoint (), list (any ()), any ()) -> any ()).
run_fold_hook (HookPoint, Args, Acc) ->
    emqx_hooks:run_fold (HookPoint, Args, Acc).
```

### 钩子 (Hook) 使用

[emqx\_plugin\_template](https://github.com/emqx/emqx_plugin_template/blob/master/src/emqx_plugin_template.erl)
提供了全部钩子的使用示例，例如端到端的消息处理回调:

``` sourceCode erlang
-module (emqx_plugin_template).

-export ([load/1, unload/0]).

-export ([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

load (Env) ->
    emqx:hook ('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
    emqx:hook ('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
    emqx:hook ('message.acked', fun ?MODULE:on_message_acked/3, [Env]).

on_message_publish (Message, _Env) ->
    io:format ("publish ~s~n", [emqx_message:format (Message)]),
    {ok, Message}.

on_message_delivered (ClientInfo, Message, _Env) ->
    io:format ("deliver to client ~s: ~s~n", [ClientInfo, emqx_message:format (Message)]),
    {ok, Message}.

on_message_acked (ClientInfo, Message, _Env) ->
    io:format ("client ~s acked: ~s~n", [ClientInfo, emqx_message:format (Message)]),
    {ok, Message}.

unload () ->
    emqx:unhook ('message.publish', fun ?MODULE:on_message_publish/2),
    emqx:unhook ('message.acked', fun ?MODULE:on_message_acked/3),
    emqx:unhook ('message.delivered', fun ?MODULE:on_message_delivered/3).
```

## 认证与访问控制设计

*EMQ X* 消息服务器支持可扩展的认证与访问控制，通过挂载 `client.authenticate` and
`client.check_acl` 两个钩子实现。

### 编写鉴权钩子回调函数

挂载回调函数到 `client.authenticate` 钩子:

``` sourceCode erlang
Env = some_input_params,
emqx:hook ('client.authenticate', fun ?MODULE:on_client_authenticate/3, [Env]).
```

钩子回调函数必须接受三个 *EMQ X* 回调的参数 `ClientInfo` `AuthResult` 和 `Env` 并且返回一个新的
`AuthResult`.

``` sourceCode erlang
on_client_authenticate (ClientInfo = #{password := Password}, AuthResult, Env) ->
    {ok, AuthResult#{result => success}}.
```

`ClientInfo` 结构体是一个包含该客户端信息的 map，包括 clientid, username, password 等；具体可查看
`emqx_types.erl` 模块的定义。 `AuthResult` 结构体是一个用于返回认证结果的 map:

``` sourceCode erlang
#{is_superuser => IsSuperUser,  %% 布尔值；是否为超级用户
  result => Result,             %% 枚举值；success 表示认证成功
  anonymous => Anonymous        %% 布尔值；是否为匿名用户
 }
```

### 编写 ACL 钩子回调函数

挂载回调函数到 `client.check_acl` 钩子:

``` sourceCode erlang
Env = some_input_params,
emqx:hook ('client.check_acl', fun ?MODULE:on_client_check_acl/5, [Env]).
```

回调函数须接受 `ClientInfo`, `AccessType`, `Topic`, `ACLResult`, `Env` 这几个参数，
然后返回一个新的
ACLResult:

``` sourceCode erlang
on_client_check_acl (#{client_id := ClientId}, AccessType, Topic, ACLResult, Env) ->
    {ok, allow}.
```

AccessType: 枚举值；`publish` 或 `subscribe` Topic: binary 类型；
PUBLISH/SUBSCRIBE 报文中的主题字段 ACLResult: 枚举值； `allow` 或 `deny`

`emqx_mod_acl_internal` 模块实现了基于 etc/acl.conf 文件的 ACL 机制，etc/acl.conf
文件的默认内容：

``` sourceCode erlang
%%%-----------------------------------------------------------------------------
%%%
%%% -type who () :: all | binary () |
%%%                {ipaddr, esockd_access:cidr ()} |
%%%                {client, binary ()} |
%%%                {user, binary ()}.
%%%
%%% -type access () :: subscribe | publish | pubsub.
%%%
%%% -type topic () :: binary ().
%%%
%%% -type rule () :: {allow, all} |
%%%                 {allow, who (), access (), list (topic ())} |
%%%                 {deny, all} |
%%%                 {deny, who (), access (), list (topic ())}.
%%%
%%%-----------------------------------------------------------------------------

{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```

由 emqx 提供的 Auth/ACL 插件:

|                      |                       |
| -------------------- | --------------------- |
| Plugin               | Authentication        |
| emqx\_auth\_username | Username and Password |
| emqx\_auth\_clientid | ClientID and Password |
| emqx\_auth\_ldap     | LDAP                  |
| emqx\_auth\_http     | HTTP API              |
| emqx\_auth\_mysql    | MySQL                 |
| emqx\_auth\_pgsql    | PostgreSQL            |
| emqx\_auth\_redis    | Redis                 |
| emqx\_auth\_mongo    | MongoDB               |
| emqx\_auth\_jwt      | JWT                   |

## 插件 (Plugin) 设计

插件是一个可以被动态加载的普通 Erlang
应用 (Application)。插件主要通过钩子 (Hook) 机制扩展服务器功能，或通过注册扩展模块方式集成认证访问控制。

emqx\_plugins 模块实现插件机制，提供加载卸载插件 API :

    -module (emqx_plugins).
    
    -export ([load/1, unload/1]).
    
    %% @doc Load a Plugin
    load (PluginName :: atom ()) -> ok | {error, any ()}.
    
    %% @doc UnLoad a Plugin
    unload (PluginName :: atom ()) -> ok | {error, any ()}.

用户可通过 ./bin/emqx\_ctl 命令行加载卸载插件:

    ./bin/emqx_ctl plugins load <plugin name>
    
    ./bin/emqx_ctl plugins unload <plugin name>

开发者请参考模版插件: <http://github.com/emqx/emqx_plugin_template>

## Mnesia/ETS 表设计

| Table                      | Type   | Description   |
| -------------------------- | ------ | ------------- |
| emqx\_conn                 | ets    | 连接表           |
| emqx\_metrics              | ets    | 统计表           |
| emqx\_session              | ets    | 会话表           |
| emqx\_hooks                | ets    | 钩子表           |
| emqx\_subscriber           | ets    | 订阅者表          |
| emqx\_subscription         | ets    | 订阅表           |
| emqx\_admin                | mnesia | Dashboard 用户表 |
| emqx\_retainer             | mnesia | Retained 消息表  |
| emqx\_shared\_subscription | mnesia | 共享订阅表         |
| emqx\_session\_registry    | mnesia | 全局会话注册表       |
| emqx\_alarm\_history       | mnesia | 告警历史表         |
| emqx\_alarm                | mnesia | 告警表           |
| emqx\_banned               | mnesia | 禁止登陆表         |
| emqx\_route                | mnesia | 路由表           |
| emqx\_trie                 | mnesia | Trie 表        |
| emqx\_trie\_node           | mnesia | Trie Node 表   |
| mqtt\_app                  | mnesia | App 表         |

## Erlang 设计相关

1.  使用 Pool, Pool, Pool... 推荐 GProc 库: <https://github.com/uwiger/gproc>
2.  异步，异步，异步消息... 连接层到路由层异步消息，同步请求用于负载保护
3.  避免进程 Mailbox 累积消息
4.  消息流经的 Socket 连接、会话进程必须 Hibernate，主动回收 binary 句柄
5.  多使用 Binary 数据，避免进程间内存复制
6.  使用 ETS, ETS, ETS... Message Passing vs. ETS
7.  避免 ETS 表非键值字段 select, match
8.  避免大量数据 ETS 读写，每次 ETS 读写会复制内存，可使用 lookup\_element, update\_counter
9.  适当开启 ETS 表 {write\_concurrency, true}
10. 保护 Mnesia 数据库事务，尽量减少事务数量，避免事务过载 (overload)
11. 避免对 Mnesia 数据表非索引、或非键值字段 match, select





# Erlang/OTP 平台介绍

*EMQ*
消息服务器完全基于 Erlang/OTP 语言平台开发，Erlang/OTP 是非常出色的软实时 (Soft-Realtime) 低延时 (Low-Latency) 的语言平台，非常适合消息服务器的开发:

1.  细粒度垃圾回收 (Garbage Collection)
2.  基于 Reduction 计数的抢占式公平进程调度
3.  容错恢复与分布支持

## Erlang/OTP 历史

爱立信、Joe Armstrong，1988 年起近 30 年历史

Erlang 语言最初设计目的是开发电信设备与系统

1998 年开源，2006 多核 CPU 支持，互联网、即时消息、云计算应用

C++、Java、C\# 面向对象系列截然不同的设计思路

以消息为主的移动互联网、物联网最佳服务端平台

## Erlang/OTP 平台特点

高并发 (Concurrency, Muti Core, Threads, Massive Processes)

低延时 (Low-Latency)

软实时 (Soft real-time)

容错 (Fault-tolerant, monitor, link, supervisor tree)

分布 (Distributed nodes, mnesia)

水平伸缩 (Scalable)

热升级 (Hot Upgrade)

## Erlang 虚拟机

类似操作系统: CPU Cores, Schedulers, Threads, Massive Processes

跨平台 (Linux, FreeBSD, AIX, HP-UX, 树莓派…Windows)

出色的内存管理：process heap, binary, ets…

细粒度垃圾回收 (Fine-grained Garbage Collected)

轻量进程、公平调度

## Erlang 编程语言 (1)

函数编程 (Functional Programming)

模式匹配 (Pattern Match)

轻量进程 (Lightweight Processes)

消息传递 (Message Passing)

递归 (Recursion)、尾递归 (Tail Recursion)

Actor-Oriented, Object-Oriented?

Atom, Pid, Tuple, List, Binary, Port…

List, Binary Comprehension:

    List, Binary Comprehension
    << <<(serialise_utf (Topic))/binary, ?RESERVED:6, Qos:2>> || {Topic, Qos} <- Topics >>;
    
    routes (Topics, Pid) ->
        lists:unzip ([{{Topic, Pid}, {Pid, Topic}} || Topic <- Topics]).

Binary Match 解析网络协议

闭包 (Closure) 与高阶函数 (HigherOrder Functions)

参数化模块 (Parameterized Module):

    new (Sock, SockFun, Opts) ->
       {?MODULE, [Sock, SockFun, parse_opt (Opts)]}.

## Erlang/OTP 平台

OTP (Open Telecom Platform) 行为 (Behaviours):

    gen_server (客户端服务器)
    gen_fsm (有限状态自动机)
    gen_event (事件通知)

  - 监控 (Supervisor)::  
    Supervisor Restart Strategies one\_for\_all one\_for\_one
    rest\_for\_one simple\_one\_for\_one

应用 (Applications):

    TREE IMAGE

发布 (Releases):

    ERTS + Boot 脚本 + Applications  => Binary Package





# 常见问题 (FAQ)

## Q1. 无法订阅 $SYS / 开头的系统主题？

$SYS/\# 系统主题默认只允许本机订阅，访问控制规则设置在 etc/acl.config:

    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

1.  emqttd 消息服务器适用于什么项目？
    
    物联网、移动互联网.
    
    不适合做企业内部 MQ

2.  使用 emqttd 服务器，需要了解 Erlang 语言吗？

## 性能测试

1.  TCP 连接测试，是否配置了重连。如果不配置重连，是否可用认为实际连接数可能并没有达到测试设置的最大连接数。

答复：TCP 连接测试有重连，SSL 双向测试取消了重连。实际连接数是通过 EMQ 的 stats、listener 指标统计，与是否重连没关系。EMQ 本次测试并未测试最高的连接数，只是例行测试 100 万连接的 CPU、内存占用情况。1.0 版本最高测试到 130 万连接，ZAKER 新闻客户端产品环境下 90 万。

2.  连接测试中的响应时间，是指创建连接到连接成功的时间吗？

答复：响应时间为 TCP 连接建立，发送 CONNECT 报文，接收到 CONNACK 报文。

3.  吞吐量测试中，没有丢包数量的统计。
请问下，是否有这个结果的统计？

答复：吞吐测试在青云北京三区主要测试 EMQ 每秒处理消息数量，没有丢包率的指标。在 EMQ 处理能力之内，QoS0 消息内网一般不会丢包，QoS1/2 消息支持回执与重传可以避免丢包。

4.  吞吐量测试中，topic 的数量是怎么设计的呢？

答复：吞吐测试是先创建 10 万线背景连接和 20 万 Topic。

5.  吞吐量测试中，测试分别统计的 fan-in 和 fan-out，fan-in 测试的时候，没有 sub。不知道我理解得对不对。有没有两个值都同时统计的测试结果呢？

答复：共享订阅测试是双向。因为大部分应用场景下 PUB 消费需要用共享订阅平衡负载。

我们的应用场景中，流量更多是从多 publisher 到少量的 subscriber

答复：共享订阅或 Fastlane 订阅，专门处理数据采集类的多 PUB 少 SUB 场景。





# 开始使用 (Get Started)

## *EMQ X* 消息服务器简介

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP
平台开发的开源物联网 MQTT 消息服务器。Erlang/OTP
是出色的软实时 (Soft-Realtime)、低延时 (Low-Latency)、分布式 (Distributed)
的语言平台。MQTT 是轻量的 (Lightweight)、发布订阅模式 (PubSub) 的物联网消息协议。

*EMQ X* 面向海量的 ** 移动 / 物联网 / 车载 ** 等终端接入，并实现在海量物理网设备间快速低延时的消息路由:

1.  稳定承载大规模的 MQTT 客户端连接，单服务器节点支持百万连接。
2.  分布式节点集群，快速低延时的消息路由，单集群支持千万规模的路由。
3.  消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。
4.  完整物联网协议支持，MQTT、MQTT-SN、CoAP、LwM2M、私有 TCP/UDP 协议支持。

## MQTT 发布订阅模式简述

MQTT 是基于 ** 发布 (Publish)/ 订阅 (Subscribe)** 模式来进行通信及数据交换的，与 HTTP 的
** 请求 (Request)/ 应答 (Response)** 的模式有本质的不同。

** 订阅者 (Subscriber)** 会向 ** 消息服务器 (Broker)** 订阅一个 ** 主题 (Topic)**
。成功订阅后，消息服务器会将该主题下的消息转发给所有的订阅者。

主题 (Topic) 以 '/' 为分隔符区分不同的层级。包含通配符 '+' 或 '\#' 的主题又称为 ** 主题过滤器 (Topic
Filters)**; 不含通配符的称为 ** 主题名 (Topic Names)** 例如:

    sensor/1/temperature
    
    chat/room/subject
    
    presence/user/feng
    
    sensor/1/#
    
    sensor/+/temperature
    
    uber/drivers/joe/inbox

<div class="note">

<div class="admonition-title">

Note

</div>

注: '+' 通配一个层级，'\#' 通配多个层级 (必须在末尾)。

</div>

<div class="note">

<div class="admonition-title">

Note

</div>

注：发布者 (Publisher) 只能向 ' 主题名 ' 发布消息，订阅者 (Subscriber) 则可以通过订阅 ' 主题过滤器 '
来通配多个主题名称。

</div>

## *EMQ X* 消息服务器功能列表

  - 完整的 MQTT V3.1/V3.1.1 及 V5.0 协议规范支持
  - QoS0, QoS1, QoS2 消息支持
  - 持久会话与离线消息支持
  - Retained 消息支持
  - Last Will 消息支持
  - TCP/SSL 连接支持
  - MQTT/WebSocket/SSL 支持
  - HTTP 消息发布接口支持
  - $SYS/\# 系统主题支持
  - 客户端在线状态查询与订阅支持
  - 客户端 ID 或 IP 地址认证支持
  - 用户名密码认证支持
  - LDAP 认证
  - Redis、MySQL、PostgreSQL、MongoDB、HTTP 认证集成
  - 浏览器 Cookie 认证
  - 基于客户端 ID、IP 地址、用户名的访问控制 (ACL)
  - 多服务器节点集群 (Cluster)
  - 支持 manual、mcast、dns、etcd、k8s 等多种集群发现方式
  - 网络分区自动愈合
  - 消息速率限制
  - 连接速率限制
  - 按分区配置节点
  - 多服务器节点桥接 (Bridge)
  - MQTT Broker 桥接支持
  - Stomp 协议支持
  - MQTT-SN 协议支持
  - CoAP 协议支持
  - Stomp/SockJS 支持
  - 延时 Publish ($delay/topic)
  - Flapping 检测
  - 黑名单支持
  - 共享订阅 ($share/\<group\>/topic)
  - TLS/PSK 支持
  - 规则引擎支持

## 五分钟下载启动 EMQ

*EMQ X* 的每个版本都会发布 CentOS、Ubuntu、Debian、FreeBSD、macOS、Windows、openSUSE
平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.io/downloads/broker?osType=Linux>

程序包下载后，可直接解压启动运行，例如 Mac 平台:

``` sourceCode bash
unzip emqx-macosx-v4.0.0.zip && cd emqx

# 启动 emqx
./bin/emqx start

# 检查运行状态
./bin/emqx_ctl status

# 停止 emqx
./bin/emqx stop
```

*EMQ X* 启动后，MQTT 客户端可通过 1883 端口接入系统。运行日志输出在 log/ 目录。

*EMQ X* 默认加载 Dashboard 插件，启动 Web 管理控制台。用户可通过 Web
控制台，查看服务器运行状态、统计数据、连接 (Connections)、会话 (Sessions)、主题 (Topics)、订阅 (Subscriptions)、插件 (Plugins) 等。

控制台地址: <http://127.0.0.1:18083，默认用户名 >: admin，密码：public

![image](./_static/images/dashboard.png)

## 开源 MQTT 客户端项目

GitHub:
<https://github.com/emqtt>

|                                                              |                    |
| ------------------------------------------------------------ | ------------------ |
| [emqttc](https://github.com/emqtt/emqttc)                    | Erlang MQTT 客户端库   |
| [CocoaMQTT](https://github.com/emqtt/CocoaMQTT)              | Swift 语言 MQTT 客户端库 |
| [QMQTT](https://github.com/emqtt/qmqtt)                      | QT 框架 MQTT 客户端库    |
| [emqtt\_benchmark](https://github.com/emqtt/emqtt_benchmark) | MQTT 连接测试工具        |

Eclipse Paho: <https://www.eclipse.org/paho/>

MQTT.org: <https://github.com/mqtt/mqtt.github.io/wiki/libraries>





# 用户指南 (User Guide)

## 程序启动

下载地址: <https://www.emqx.io/downloads/broker?osType=Linux>

程序包下载后，可直接解压启动运行，例如 macOS 平台:

``` sourceCode bash
unzip emqx-macosx-v4.0.0.zip && cd emqx

# 启动 emqx
./bin/emqx start

# 检查运行状态
./bin/emqx_ctl status
```

*EMQ X* 消息服务器默认占用的 TCP 端口包括:

|       |                   |
| ----- | ----------------- |
| 1883  | MQTT 协议端口         |
| 8883  | MQTT/SSL 端口       |
| 8083  | MQTT/WebSocket 端口 |
| 8081  | HTTP API 端口       |
| 18083 | Dashboard 管理控制台端口 |

## MQTT 发布订阅

MQTT 是为移动互联网、物联网设计的轻量发布订阅模式的消息服务器，目前支持 MQTT
[v3.1.1](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html) 和
[v5.0](http://docs.oasis-open.org/mqtt/mqtt/v5.0/mqtt-v5.0.html):

![image](./_static/images/guide_1.png)

*EMQ X* 启动后，任何设备或终端可通过 MQTT 协议连接到服务器，通过 ** 发布 (Publish)/ 订阅 (Subscribe)**
进行交换消息。

MQTT 客户端库: <https://github.com/mqtt/mqtt.github.io/wiki/libraries>

例如，mosquitto\_sub/pub 命令行发布订阅消息:

    mosquitto_sub -h 127.0.0.1 -p 1883 -t topic -q 2
    mosquitto_pub -h 127.0.0.1 -p 1883 -t topic -q 1 -m "Hello, MQTT!"

## 认证 / 访问控制

**EMQ X** 消息服务器 * 连接认证 * 和 * 访问控制 * 由一系列的认证插件 (Plugins) 提供，他们的命名都符合
`emqx_auth_<name>` 的规则。

在 EMQ X 中，这两个功能分别是指：

1.  ** 连接认证 **: *EMQ X* 校验每个连接上的客户端是否具有接入系统的权限，若没有则会断开该连接
2.  ** 访问控制 **: *EMQ X* 校验客户端每个 * 发布 (Publish)/ 订阅 (Subscribe)* 的权限，以 * 允许 / 拒绝 *
    相应操作

### 认证 (Authentication)

*EMQ X* 消息服务器认证由一系列认证插件 (Plugins) 提供，系统支持按用户名密码、ClientID 或匿名认证。

系统默认开启匿名认证 (Anonymous)，通过加载认证插件可开启的多个认证模块组成认证链:

![image](_static/images/guide_2.png)

** 开启匿名认证 **

etc/emqx.conf 配置启用匿名认证:

``` sourceCode properties
允许匿名访问
## Value: true | false
allow_anonymous = true
```

### 访问控制 (ACL)

*EMQ X* 消息服务器通过 ACL (Access Control List) 实现 MQTT 客户端访问控制。

ACL 访问控制规则定义:

    允许 (Allow)| 拒绝 (Deny) 谁 (Who) 订阅 (Subscribe)| 发布 (Publish) 主题列表 (Topics)

MQTT 客户端发起订阅 / 发布请求时，EMQ X 消息服务器的访问控制模块会逐条匹配 ACL 规则，直到匹配成功为止:

![image](_static/images/guide_3.png)

** 默认访问控制设置 **

*EMQ X* 消息服务器默认访问控制，在 etc/emqx.conf 中设置:

``` sourceCode properties
## 设置所有 ACL 规则都不能匹配时是否允许访问
## Value: allow | deny
acl_nomatch = allow

## 设置存储 ACL 规则的默认文件
## Value: File Name
acl_file = etc/acl.conf
```

ACL 规则定义在 etc/acl.conf，EMQ X 启动时加载到内存:

``` sourceCode erlang
%% 允许 'dashboard' 用户订阅 '$SYS/#'
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

%% 允许本机用户发布订阅全部主题
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

%% 拒绝除本机用户以外的其他用户订阅 '$SYS/#' 与 '#' 主题
{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

%% 允许上述规则以外的任何情形
{allow, all}.
```

EMQ X
提供的认证插件包括:

| 插件                                                                 | 说明               |
| ------------------------------------------------------------------ | ---------------- |
| [emqx\_auth\_clientid](https://github.com/emqx/emqx-auth-clientid) | ClientId 认证 / 鉴权插件 |
| [emqx\_auth\_username](https://github.com/emqx/emqx-auth-username) | 用户名密码认证 / 鉴权插件     |
| [emqx\_auth\_jwt](https://github.com/emqx/emqx-auth-jwt)           | JWT 认证 / 鉴权插件      |
| [emqx\_auth\_ldap](https://github.com/emqx/emqx-auth-ldap)         | LDAP 认证 / 鉴权插件     |
| [emqx\_auth\_http](https://github.com/emqx/emqx-auth-http)         | HTTP 认证 / 鉴权插件     |
| [emqx\_auth\_mysql](https://github.com/emqx/emqx-auth-mysql)       | MySQ L 认证 / 鉴权插件    |
| [emqx\_auth\_pgsql](https://github.com/emqx/emqx-auth-pgsql)       | Postgre 认证 / 鉴权插件  |
| [emqx\_auth\_redis](https://github.com/emqx/emqx-auth-redis)       | Redis 认证 / 鉴权插件    |
| [emqx\_auth\_mongo](https://github.com/emqx/emqx-auth-mongo)       | MongoDB 认证 / 鉴权插件  |

其中，关于每个认证插件的配置及用法，可参考 [扩展插件
(Plugins)](https://docs.emqx.io/broker/v3/cn/plugins.html) 关于认证部分。

<div class="note">

<div class="admonition-title">

Note

</div>

auth 插件可以同时启动多个。每次检查的时候，按照优先级从高到低依次检查，同一优先级的，先启动的插件先检查。

</div>

此外 *EMQ X* 还支持使用 **PSK (Pre-shared Key)** 的方式来控制客户端的接入，但它并不是使用的上述的
* 连接认证 * 链的方式，而是在 SSL 握手期间进行验证。详情参考 [Pre-shared
Key](https://en.wikipedia.org/wiki/Pre-shared_key) 和
[emqx\_psk\_file](https://github.com/emqx/emqx-psk-file)

## 共享订阅 (Shared Subscription)

*EMQ X* R3.0 版本开始支持集群级别的共享订阅功能。共享订阅 (Shared Subscription) 支持多种消息派发策略:

![image](./_static/images/guide_4.png)

共享订阅支持两种使用方式:

<table style="width:86%;">
<colgroup>
<col style="width: 25%" />
<col style="width: 61%" />
</colgroup>
<tbody>
<tr class="odd">
<td><blockquote>
<p > 订阅前缀 </p>
</blockquote></td>
<td > 使用示例 </td>
</tr>
<tr class="even">
<td>$queue/</td>
<td>mosquitto_sub -t '$queue/topic'</td>
</tr>
<tr class="odd">
<td>$share/&lt;group&gt;/</td>
<td>mosquitto_sub -t '$share/group/topic'</td>
</tr>
</tbody>
</table>

示例:

    mosquitto_sub -t '$share/group/topic'
    
    mosquitto_pub -t 'topic' -m msg -q 2

*EMQ X* 通过 etc/emqx.conf 中的 broker.shared\_subscription\_strategy
字段配置共享消息的派发策略。

目前支持按以下几种策略派发消息：

| 策略           | 说明              |
| ------------ | --------------- |
| random       | 在所有共享订阅者中随机     |
| round\_robin | 按订阅顺序           |
| sticky       | 使用上次派发的订阅者      |
| hash         | 根据发送者的 ClientId |

<div class="note">

<div class="admonition-title">

Note

</div>

当所有的订阅者都不在线时，仍会挑选一个订阅者，并存至其 Session 的消息队列中

</div>

## 节点桥接 (Bridge)

### EMQ X 节点间桥接

** 桥接 ** 的概念是 EMQ X 支持将自身某类主题的消息通过某种方式转发到另一个 MQTT Broker。

** 桥接 ** 与 ** 集群 ** 的不同在于：桥接不会复制主题树与路由表，只根据桥接规则转发 MQTT 消息。

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

## HTTP 发布接口

*EMQ X* 消息服务器提供了一个 HTTP 发布接口，应用服务器或 Web 服务器可通过该接口发布 MQTT 消息:

    HTTP POST http://host:8080/api/v3/mqtt/publish

Web 服务器例如 PHP/Java/Python/NodeJS 或 Ruby on Rails，可通过 HTTP POST 请求发布 MQTT
消息:

``` sourceCode bash
curl -v --basic -u user:passwd -H "Content-Type: application/json" -d \
'{"qos":1, "retain": false, "topic":"world", "payload":"test" , "clientid": "C_1492145414740"}' \-k http://localhost:8080/api/v3/mqtt/publish
```

HTTP 接口参数:

| 参数       | 说明                   |
| -------- | -------------------- |
| clientid | MQTT 客户端 ID          |
| qos      | QoS: 0 | 1 | 2       |
| retain   | Retain: true | false |
| topic    | 主题 (Topic)            |
| payload  | 消息载荷                 |

<div class="note">

<div class="admonition-title">

Note

</div>

HTTP 发布接口采用
[Basic](https://en.wikipedia.org/wiki/Basic_access_authentication)
认证。上例中的 `user` 和 `password` 是来自于 Dashboard 下的 Applications
内的 AppId 和密码

</div>

## MQTT WebSocket 连接

*EMQ X* 还支持 WebSocket 连接，Web 浏览器可直接通过 WebSocket 连接至服务器:

|                         |                            |
| ----------------------- | -------------------------- |
| WebSocket URI:          | ws (s)://host:8083/mqtt     |
| Sec-WebSocket-Protocol: | 'mqttv3.1' or 'mqttv3.1.1' |

Dashboard 插件提供了一个 MQTT WebSocket 连接的测试页面:

    http://127.0.0.1:18083/#/websocket

## $SYS - 系统主题

*EMQ X* 消息服务器周期性发布自身运行状态、消息统计、客户端上下线事件到 以 `$SYS/` 开头系统主题。

$SYS 主题路径以 `$SYS/brokers/{node}/` 开头。 `{node}` 是指产生该 事件 / 消息 所在的节点名称，例如:

    $SYS/brokers/emqx@127.0.0.1/version
    
    $SYS/brokers/emqx@127.0.0.1/uptime

<div class="note">

<div class="admonition-title">

Note

</div>

默认只允许 localhost 的 MQTT 客户端订阅 $SYS 主题，可通过 etc/acl.config 修改访问控制规则。

</div>

$SYS 系统消息发布周期，通过 etc/emqx.conf 配置:

``` sourceCode properties
## System interval of publishing $SYS messages.
##
## Value: Duration
## Default: 1m, 1 minute
broker.sys_interval = 1m
```

### 集群状态信息

| 主题                            | 说明            |
| ----------------------------- | ------------- |
| $SYS/brokers                  | 集群节点列表        |
| $SYS/brokers/${node}/version  | EMQ X 服务器版本   |
| $SYS/brokers/${node}/uptime   | EMQ X 服务器启动时间 |
| $SYS/brokers/${node}/datetime | EMQ X 服务器时间   |
| $SYS/brokers/${node}/sysdescr | EMQ X 服务器描述   |

### 客户端上下线事件

$SYS 主题前缀: $SYS/brokers/${node}/clients/

| 主题 (Topic)                | 说明                   |
| ------------------------ | -------------------- |
| ${clientid}/connected    | 上线事件。当某客户端上线时，会发布该消息 |
| ${clientid}/disconnected | 下线事件。当某客户端离线时，会发布该消息 |

'connected' 事件消息的 Payload 可解析成 JSON 格式:

``` sourceCode json
{
    "clientid":"id1",
    "username":"u",
    "ipaddress":"127.0.0.1",
    "connack":0,
    "ts":1554047291,
    "proto_ver":3,
    "proto_name":"MQIsdp",
    "clean_start":true,
    "keepalive":60
}
```

'disconnected' 事件消息的 Payload 可解析成 JSON 格式:

``` sourceCode json
{
    "clientid":"id1",
    "username":"u",
    "reason":"normal",
    "ts":1554047291
}
```

### 系统统计 (Statistics)

系统主题前缀: $SYS/brokers/${node}/stats/

#### 客户端统计

|                   |         |
| ----------------- | ------- |
| 主题 (Topic)         | 说明      |
| connections/count | 当前客户端总数 |
| connections/max   | 最大客户端数量 |

#### 会话统计

|                |        |
| -------------- | ------ |
| 主题 (Topic)      | 说明     |
| sessions/count | 当前会话总数 |
| sessions/max   | 最大会话数量 |

#### 订阅统计

|                            |          |
| -------------------------- | -------- |
| 主题 (Topic)                  | 说明       |
| suboptions/count           | 当前订阅选项个数 |
| suboptions/max             | 最大订阅选项总数 |
| subscribers/max            | 最大订阅者总数  |
| subscribers/count          | 当前订阅者数量  |
| subscriptions/max          | 最大订阅数量   |
| subscriptions/count        | 当前订阅总数   |
| subscriptions/shared/count | 当前共享订阅个数 |
| subscriptions/shared/max   | 当前共享订阅总数 |

#### 主题统计

|              |             |
| ------------ | ----------- |
| 主题 (Topic)    | 说明          |
| topics/count | 当前 Topic 总数 |
| topics/max   | 最大 Topic 数量 |

#### 路由统计

|              |              |
| ------------ | ------------ |
| 主题 (Topic)    | 说明           |
| routes/count | 当前 Routes 总数 |
| routes/max   | 最大 Routes 数量 |

<div class="note">

<div class="admonition-title">

Note

</div>

`topics/count` 和 `topics/max` 与 `routes/count` 和 `routes/max` 数值上是相等的。

</div>

### 收发流量 / 报文 / 消息统计

系统主题 (Topic) 前缀: $SYS/brokers/${node}/metrics/

#### 收发流量统计

|                |         |
| -------------- | ------- |
| 主题 (Topic)      | 说明      |
| bytes/received | 累计接收字节数 |
| bytes/sent     | 累计发送字节数 |

#### MQTT 报文收发统计

|                               |                          |
| ----------------------------- | ------------------------ |
| 主题 (Topic)                     | 说明                       |
| packets/received              | 累计接收 MQTT 报文             |
| packets/sent                  | 累计发送 MQTT 报文             |
| packets/connect/received      | 累计接收 CONNECT 报文          |
| packets/connack/sent          | 累计发送 CONNACK 报文          |
| packets/publish/received      | 累计接收 PUBLISH 报文          |
| packets/publish/sent          | 累计发送 PUBLISH 报文          |
| packets/publish/error         | 累计处理 PUBLISH 错误          |
| packets/publish/auth\_error   | 累计拒绝 PUBLISH 报文          |
| packets/publish/dropped       | 累计丢弃 PUBLISH 报文          |
| packets/puback/received       | 累计接收 PUBACK 报文           |
| packets/puback/sent           | 累计发送 PUBACK 报文           |
| packets/puback/inuse          | 累计丢弃 PUBACK 报文 (ID 已被使用) |
| packets/puback/missed         | 累计丢弃 PUBACK 报文           |
| packets/pubrec/received       | 累计接收 PUBREC 报文           |
| packets/pubrec/sent           | 累计发送 PUBREC 报文           |
| packets/pubrec/inuse          | 累计丢弃 PUBREC 报文 (ID 已被使用) |
| packets/pubrec/missed         | 累计丢失 PUBREC 报文           |
| packets/pubrel/received       | 累计接收 PUBREL 报文           |
| packets/pubrel/sent           | 累计发送 PUBREL 报文           |
| packets/pubrel/missed         | 累计丢失 PUBREL 报文           |
| packets/pubcomp/received      | 累计接收 PUBCOMP 报文          |
| packets/pubcomp/sent          | 累计发送 PUBCOMP 报文          |
| packets/pubcomp/missed        | 累计丢失 PUBCOMP 报文          |
| packets/subscribe/received    | 累计接收 SUBSCRIBE 报文        |
| packets/subscribe/error       | 累计处理 SUBSCRIBE 失败        |
| packets/subscribe/auth\_error | 累计拒绝 SUBSCRIBE 报文        |
| packets/suback/sent           | 累计发送 SUBACK 报文           |
| packets/unsubscribe/received  | 累计接收 UNSUBSCRIBE 报文      |
| packets/unsubscribe/error     | 累计接收 UNSUBSCRIBE 报文      |
| packets/unsuback/sent         | 累计发送 UNSUBACK 报文         |
| packets/pingreq/received      | 累计接收 PINGREQ 报文          |
| packets/pingresp/sent         | 累计发送 PINGRESP 报文         |
| packets/disconnect/received   | 累计接收 DISCONNECT 报文       |
| packets/disconnect/sent       | 累计接收 DISCONNECT 报文       |
| packets/auth/received         | 累计接收 AUTH 报文             |
| packets/auth/sent             | 累计发送 AUTH 报文             |

#### MQTT 消息收发统计

|                                 |                   |
| ------------------------------- | ----------------- |
| 主题 (Topic)                       | 说明                |
| messages/received               | 累计接收消息            |
| messages/sent                   | 累计发送消息            |
| messages/qos0/received          | 累计接受 QoS0 消息      |
| messages/qos0/sent              | 累计发送 QoS0 消息      |
| messages/qos1/received          | 累计接受 QoS1 消息      |
| messages/qos1/sent              | 累计发送 QoS1 消息      |
| messages/qos2/received          | 累计接受 QoS2 消息      |
| messages/qos2/sent              | 累计发送 QoS2 消息      |
| messages/publish                | 累计发送 PUBLUSH 消息   |
| messages/dropped                | 丢弃消息总数            |
| messages/dropped/expired        | 丢弃消息总数 (过期消息)     |
| messages/dropped/no\_subscriber | 丢弃消息总数 (无订阅者)     |
| messages/forward                | 节点转发消息总数          |
| messages/retained               | 存储的 Retained 消息总数 |
| messages/delayed                | 存储的 Delayed 消息总数  |
| messages/delivered              | 已投递消息总数           |
| messages/acked                  | 累计消息确认成功总数        |

### Alarms - 系统告警

系统主题 (Topic) 前缀: $SYS/brokers/${node}/alarms/

|           |        |
| --------- | ------ |
| 主题 (Topic) | 说明     |
| alert     | 新产生的告警 |
| clear     | 被清除的告警 |

### Sysmon - 系统监控

系统主题 (Topic) 前缀: $SYS/brokers/${node}/sysmon/

|                  |               |
| ---------------- | ------------- |
| 主题 (Topic)        | 说明            |
| long\_gc         | GC 时间过长警告     |
| long\_schedule   | 调度时间过长警告      |
| large\_heap      | Heap 内存占用警告   |
| busy\_port       | Port 忙警告      |
| busy\_dist\_port | Dist Port 忙警告 |

## 追踪

EMQ X 消息服务器支持追踪来自某个客户端 (Client)，或者发布到某个主题 (Topic) 的全部消息。

追踪来自客户端 (Client) 的消息:

``` sourceCode bash
$ ./bin/emqx_ctl log primary-level debug

$ ./bin/emqx_ctl trace start client "clientid" "trace_clientid.log" debug
```

追踪发布到主题 (Topic) 的消息:

``` sourceCode bash
$ ./bin/emqx_ctl log primary-level debug

$ ./bin/emqx_ctl trace start topic "t/#" "trace_topic.log" debug
```

查询追踪:

``` sourceCode bash
$ ./bin/emqx_ctl trace list
```

停止追踪:

``` sourceCode bash
$ ./bin/emqx_ctl trace stop client "clientid"

$ ./bin/emqx_ctl trace stop topic "topic"
```





# *EMQ X* - 百万级开源 MQTT 消息服务器

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP
语言平台开发，支持大规模连接和分布式集群，发布订阅模式的开源 MQTT 消息服务器。

<div class="note">

<div class="admonition-title">

Note

</div>

3.0 版本开始 emqttd 消息服务器自正式更名为 *EMQ X*

</div>

*EMQ X* 消息服务器完整支持 MQTT V3.1/V3.1.1/V5.0 版本协议规范，并扩展支持 MQTT-SN
、WebSocket、CoAP、LwM2M、Stomp 以及私有 TCP/UDP 协议。\*EMQ X\*
消息服务器支持单节点 100 万连接与多节点分布式集群。

*EMQ X* 消息服务器为大规模设备连接 (C1000K+)
的物联网、车联网、智能硬件、移动推送、移动消息等应用，提供完全开放源码、安装部署简便、企业级稳定可靠、可弹性扩展、易于定制开发的
MQTT 消息服务器。

<div class="note">

<div class="admonition-title">

Note

</div>

MQTT-SN、CoAP 协议已在 2.0-rc.1 版本发布，LwM2M、LoRaWan 协议在 3.0 版本发布。

</div>

![image](./_static/images/emqtt.png)

*EMQ X* 项目文档目录:

<div class="toctree">

getstarted install guide config plugins commands rest design cluster
deploy tune changes upgrade protocol

</div>

*EMQ X* 项目支持与联系:

|          |                                   |
| -------- | --------------------------------- |
| 官网:      | <https://www.emqx.io>             |
| 项目:      | <https://github.com/emqx/emqx>    |
| 微信:      | emqttd                            |
| 微博:      | <http://weibo.com/emqtt>          |
| Twitter: | @emqtt                            |
| 公司:      | 杭州映云科技有限公司                        |
| 联系:      | <hong@emqx.io> \<<hong@emqx.io>\> |

![image](./_static/images/weixin.jpg)





# 程序安装 (Installation)

*EMQ X* 消息服务器可跨平台运行在 Linux、FreeBSD、macOS、Windows 或 openSUSE 服务器上。

<div class="note">

<div class="admonition-title">

Note

</div>

产品部署建议 Linux 服务器，不推荐 Windows 服务器。

</div>

## *EMQ X* 程序包下载

*EMQ X* 消息服务器每个版本会发布 CentOS、Ubuntu、Debian、FreeBSD、macOS、Windows
、openSUSE 平台程序包与 Docker 镜像。

下载地址: <https://www.emqx.io/downloads>

## CentOS

  - CentOS6.X
  - CentOS7.X

### 使用储存库安装 EMQ X

1.  安装所需要的依赖包
    
    ``` sourceCode console
    $ sudo yum install -y yum-utils device-mapper-persistent-data lvm2
    ```

2.  使用以下命令设置稳定存储库，以 CentOS7
    为例
    
    ``` sourceCode console
    $ sudo yum-config-manager --add-repo https://repos.emqx.io/emqx-ce/redhat/centos/7/emqx-ce.repo
    ```

3.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo yum install emqx
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    如果提示接受 GPG 密钥，请确认密钥符合 fc84 1ba6 3775 5ca8 487b 1e3c c0b4 0946 3e64
    0d53，如果符合，则接受该指纹。
    
    </div>

4.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ yum list emqx --showduplicates | sort -r
        
        emqx.x86_64                     4.0.0-1.el7                        emqx-stable
        emqx.x86_64                     3.0.1-1.el7                        emqx-stable
        emqx.x86_64                     3.0.0-1.el7                        emqx-stable
        ```
    
    2.  根据第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo yum install emqx-4.0.0
        ```

5.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

6.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：`/var/lib/emqx`

### 使用 rpm 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 CentOS
    版本，然后下载要安装的 EMQ X 版本的 rpm 包。

2.  安装 EMQ X
    
    ``` sourceCode console
    $ sudo rpm -ivh emqx-centos7-v4.0.0.x86_64.rpm
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：`/var/lib/emqx`

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Centos
    版本，然后下载要安装的 EMQ X 版本的 zip 包。

2.  解压程序包
    
    ``` sourceCode console
    $ unzip emqx-centos7-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Ubuntu

  - Bionic 18.04 (LTS)
  - Xenial 16.04 (LTS)
  - Trusty 14.04 (LTS)
  - Precise 12.04 (LTS)

### 使用储存库安装 EMQ X

1.  安装所需要的依赖包
    
    ``` sourceCode console
    $ sudo apt update && sudo apt install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common
    ```

2.  添加 EMQ X 的官方 GPG 密钥
    
    ``` sourceCode console
    $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
    ```
    
    验证密钥
    
    ``` sourceCode console
    $ sudo apt-key fingerprint 3E640D53
    
    pub   rsa2048 2019-04-10 [SC]
        FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
    uid           [ unknown] emqx team <support@emqx.io>
    ```

3.  使用以下命令设置 stable 存储库。 如果要添加 unstable 存储库，请在以下命令中的单词 stable 之后添加单词
    unstable。
    
    ``` sourceCode console
    $ sudo add-apt-repository \
        "deb [arch=amd64] https://repos.emqx.io/emqx-ce/deb/ubuntu/ \
        $(lsb_release -cs) \
        stable"
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    lsb\_release -cs 子命令返回 Ubuntu 发行版的名称，例如 xenial。 有时，在像 Linux Mint
    这样的发行版中，您可能需要将 $(lsb\_release -cs) 更改为您的父 Ubuntu 发行版。
    例如，如果您使用的是 Linux Mint Tessa，则可以使用 bionic。 EMQ X
    不对未经测试和不受支持的 Ubuntu 发行版提供任何保证。
    
    </div>

4.  更新 apt 包索引
    
    ``` sourceCode console
    $ sudo apt update
    ```

5.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo apt install emqx
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    在启用了多个 EMQ X 仓库的情况下，如果 apt install 和 apt update
    命令没有指定版本号，那么会自动安装最新版的 EMQ
    X。这对于有稳定性需求的用户来说是一个问题。
    
    </div>

6.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ sudo apt-cache madison emqx
        
        emqx |      4.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        emqx |      3.0.1 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        emqx |      3.0.0 | https://repos.emqx.io/emqx-ce/deb/ubuntu bionic/stable amd64 Packages
        ```
    
    2.  使用第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo apt install emqx=4.0.0
        ```

7.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

8.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 deb 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Ubuntu
    版本，然后下载要安装的 EMQ X 版本的 deb 包。

2.  安装 EMQ X
    
    ``` sourceCode console
    $ sudo dpkg -i emqx-ubuntu18.04-v4.0.0_amd64.deb
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Ubuntu
    版本，然后下载要安装的 EMQ X 版本的 zip 包。

2.  解压程序包
    
    ``` sourceCode console
    $ unzip emqx-ubuntu18.04-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Debian

  - Stretch (Debian 9)
  - Jessie (Debian 8)

### 使用储存库安装 EMQ X

1.  安装所需要的依赖包
    
    ``` sourceCode console
    $ sudo apt update && sudo apt install -y \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg-agent \
        software-properties-common
    ```

2.  添加 EMQ X 的官方 GPG 密钥
    
    ``` sourceCode console
    $ curl -fsSL https://repos.emqx.io/gpg.pub | sudo apt-key add -
    ```
    
    验证密钥
    
    ``` sourceCode console
    $ sudo apt-key fingerprint 3E640D53
    
    pub   rsa2048 2019-04-10 [SC]
        FC84 1BA6 3775 5CA8 487B  1E3C C0B4 0946 3E64 0D53
    uid           [ unknown] emqx team <support@emqx.io>
    ```

3.  使用以下命令设置 stable 存储库。 如果要添加 unstable 的存储库，请在以下命令中的单词 stable 之后添加单词
    unstable。
    
    ``` sourceCode console
    $ sudo add-apt-repository \
        "deb [arch=amd64] https://repos.emqx.io/emqx-ce/deb/debian/ \
        $(lsb_release -cs) \
        stable"
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    lsb\_release -cs 子命令返回 Debian 发行版的名称，例如 helium。 有时，在像 BunsenLabs
    Linux 这样的发行版中，您可能需要将 $(lsb\_release -cs) 更改为您的父 Debian 发行版。
    例如，如果您使用的是 BunsenLabs Linux Helium，则可以使用 stretch。
    EMQ X 不对未经测试和不受支持的 Debian 发行版提供任何保证。
    
    </div>

4.  更新 apt 包索引
    
    ``` sourceCode console
    $ sudo apt update
    ```

5.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo apt install emqx
    ```
    
    <div class="note">
    
    <div class="admonition-title">
    
    Note
    
    </div>
    
    在启用了多个 EMQ X 仓库的情况下，如果 apt install 和 apt update
    命令没有指定版本号，那么会自动安装最新版的 EMQ
    X。这对于有稳定性需求的用户来说是一个问题。
    
    </div>

6.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ sudo apt-cache madison emqx
        
        emqx |      4.0.0 | https://repos.emqx.io/emqx-ce/deb/debian stretch/stable amd64 Packages
        emqx |      3.0.1 | https://repos.emqx.io/emqx-ce/deb/debian stretch/stable amd64 Packages
        emqx |      3.0.0 | https://repos.emqx.io/emqx-ce/deb/debian stretch/stable amd64 Packages
        ```
    
    2.  使用第二列中的版本字符串安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo apt install emqx=4.0.0
        ```

7.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

8.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 deb 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Debian
    版本，然后下载要安装的 EMQ X 版本的 deb 包。

2.  安装 EMQ X
    
    ``` sourceCode console
    $ sudo dpkg -i emqx-debian9-v4.0.0_amd64.deb
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Debian
    版本，然后下载要安装的 EMQ X 版本的 zip 包。

2.  解压程序包
    
    ``` sourceCode console
    $ unzip emqx-debian9-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## macOS

### 使用 Homebrew 安装

1.  添加 EMQ X 的 tap
    
    ``` sourceCode console
    $ brew tap emqx/emqx
    ```

2.  安装 EMQ X
    
    ``` sourceCode console
    $ brew install emqx
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ emqx start
    emqx 4.0.0 is started successfully!
    
    $ emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases)，选择 EMQ X 版本，然后下载要安装的
    zip 包。

2.  解压压缩包
    
    ``` sourceCode console
    $ unzip emqx-macos-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Windows

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 Windows
    版本，然后下载要安装的 .zip 包。

2.  解压压缩包

3.  打开 Windows 命令行窗口，cd 到程序目录， 启动 EMQ X。
    
    ``` sourceCode console
    cd emqx/bin
    emqx start
    ```

## openSUSE

  - openSUSE leap

### 使用储存库安装 EMQ X

1.  下载 GPG 公钥并导入。
    
    ``` sourceCode console
    $ curl -L -o /tmp/gpg.pub https://repos.emqx.io/gpg.pub
    $ sudo rpmkeys --import /tmp/gpg.pub
    ```

2.  添加储存库地址
    
    ``` sourceCode console
    $ sudo zypper ar -f -c https://repos.emqx.io/emqx-ce/redhat/opensuse/leap/stable emqx
    ```

3.  安装最新版本的 EMQ X
    
    ``` sourceCode console
    $ sudo zypper in emqx
    ```

4.  安装特定版本的 EMQ X
    
    1.  查询可用版本
        
        ``` sourceCode console
        $ sudo zypper pa emqx
        
        Loading repository data...
        Reading installed packages...
        S | Repository | Name | Version  | Arch
        --+------------+------+----------+-------
          | emqx       | emqx | 4.0.0-1  | x86_64
          | emqx       | emqx | 3.0.1-1  | x86_64
          | emqx       | emqx | 3.0.0-1  | x86_64
        ```
    
    2.  使用 Version 安装特定版本，例如 4.0.0
        
        ``` sourceCode console
        $ sudo zypper in emqx-4.0.0
        ```

5.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

### 使用 rpm 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 openSUSE，然后下载要安装的
    EMQ X 版本的 rpm 包。

2.  安装 EMQ X，将下面的路径更改为您下载 EMQ X 软件包的路径。
    
    ``` sourceCode console
    $ sudo rpm -ivh emqx-opensuse-v4.0.0.x86_64.rpm
    ```

3.  启动 EMQ X
    
      - 直接启动
        
        ``` sourceCode console
        $ emqx start
        emqx 4.0.0 is started successfully!
        
        $ emqx_ctl status
        Node 'emqx@127.0.0.1' is started
        emqx v4.0.0 is running
        ```
    
      - systemctl 启动
        
        ``` sourceCode console
        $ sudo systemctl start emqx
        ```
    
      - service 启动
        
        ``` sourceCode console
        $ sudo service emqx start
        ```

4.  配置文件路径
    
      - 配置文件路径：`/etc/emqx`
      - 日志文件路径：`/var/log/emqx`
      - 数据文件路径：\`/var/lib/emqx

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 openSUSE，然后下载要安装的
    EMQ X 版本的 zip 包。

2.  解压压缩包
    
    ``` sourceCode console
    $ unzip emqx-opensuse-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## FreeBSD

  - FreeBSD 12

### 使用 zip 包安装 EMQ X

1.  通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux) 或
    [github](https://github.com/emqx/emqx/releases) 选择 FreeBSD，然后下载要安装的
    EMQ X 版本的 zip 包。

2.  解压压缩包
    
    ``` sourceCode console
    $ unzip emqx-freebsd12-v4.0.0.zip
    ```

3.  启动 EMQ X
    
    ``` sourceCode console
    $ ./bin/emqx start
    emqx 4.0.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running
    ```

## Docker

1.  获取 docker 镜像
    
      - 通过 [Docker Hub](https://hub.docker.com/r/emqx/emqx) 获取
        
        ``` sourceCode console
        $ docker pull emqx/emqx:v4.0.0
        ```
    
      - 通过 [emqx.io](https://www.emqx.io/downloads/broker?osType=Linux)
        或 [github](https://github.com/emqx/emqx/releases) 手动下载 docker
        镜像，并手动加载
        
        ``` sourceCode console
        $ wget -O emqx-docker.zip https://www.emqx.io/downloads/v3/latest/emqx-docker.zip
        $ unzip emqx-docker.zip
        $ docker load < emqx-docker-v4.0.0
        ```

2.  启动 docker
    容器
    
    ``` sourceCode console
    $ docker run -d --name emqx31 -p 1883:1883 -p 8083:8083 -p 8883:8883 -p 8084:8084 -p 18083:18083 emqx/emqx:v4.0.0
    ```

更多关于 EMQ X Docker 的信息请查看 [Docker
Hub](https://hub.docker.com/r/emqx/emqx) 或 [EMQ X
Docker](https://github.com/emqx/emqx-docker)

## 源码编译安装

### 环境要求

*EMQ X* 消息服务器基于 Erlang/OTP 平台开发，项目托管的 GitHub 管理维护，源码编译依赖 Erlang 环境和 git
客户端。

<div class="note">

<div class="admonition-title">

Note

</div>

EMQ X 依赖 Erlang R21.2+ 版本

</div>

Erlang 安装: <http://www.erlang.org/>

Git 客户端: <http://www.git-scm.com/>

### 编译安装 EMQ X

1.  获取源码
    
    ``` sourceCode bash
    $ git clone -b v3.2.0 https://github.com/emqx/emqx-rel.git
    ```

2.  设置环境变量
    
    ``` sourceCode bash
    $ export EMQX_DEPS_DEFAULT_VSN=v3.2.0
    ```

3.  编译
    
    ``` sourceCode bash
    $ cd emqx-rel && make
    ```

4.  启动 EMQ X
    
    ``` sourceCode bash
    $ cd emqx-rel/_rel/emqx
    $ ./bin/emqx start
    emqx v3.2.0 is started successfully!
    
    $ ./bin/emqx_ctl status
    Node 'emqx@127.0.0.1' is started
    emqx 3.2.0 is running
    ```

## Windows 源码编译安装

Erlang 安装: <http://www.erlang.org/>

scoop 安装: <https://scoop.sh/>

scoop 安装完成后，使用 scoop 来安装 Git、 Make、curl、erlang

``` sourceCode bash
scoop install git make curl erlang
```

编译环境准备完成

rebar3 安装:

``` sourceCode bash
git clone https://github.com/erlang/rebar3.git

cd rebar3

bootstrap
```

rebar3 编译完成后，在 windows 环境变量 PATH 中添加 rebar3 的路径

构建环境准备之后，clone 代码开始构建 emqx

``` sourceCode bash
git clone  -b v3.2.0 https://github.com/emqx/emqx-rel.git

cd emqx-relx && make

cd _build/emqx/rel/emqx && ./bin/emqx start
```

控制台启动编译的 EMQ 程序包

``` sourceCode bash
cd _build/emqx/rel/emqx
emqx console
```

注册 windows 服务

``` sourceCode bash
cd _build/emqx/rel/emqx
emqx install
```





# 扩展插件 (Plugins)

*EMQ X* 消息服务器通过模块注册和钩子 (Hooks) 机制，支持用户开发扩展插件定制服务器认证鉴权与业务功能。

*EMQ X*
官方提供的插件包括：

| 插件                                                                       | 配置文件                                    | 说明                  |
| ------------------------------------------------------------------------ | --------------------------------------- | ------------------- |
| [emqx\_dashboard](https://github.com/emqx/emqx-dashboard) +              | etc/plugins/emqx\_dashbord.conf         | Web 控制台插件 (默认加载)     |
| [emqx\_management](https://github.com/emqx/emqx-management) +            | etc/plugins/emqx\_management.conf       | HTTP API 与 CLI 管理插件 |
| [emqx\_auth\_clientid](https://github.com/emqx/emqx-auth-clientid) +     | etc/plugins/emqx\_auth\_clientid.conf   | ClientId 认证插件       |
| [emqx\_auth\_username](https://github.com/emqx/emqx-auth-username) +     | etc/plugins/emqx\_auth\_username.conf   | 用户名、密码认证插件          |
| [emqx\_auth\_jwt](https://github.com/emqx/emqx-auth-jwt) +               | etc/plugins/emqx\_auth\_jwt.conf        | JWT 认证 / 访问控制         |
| [emqx\_auth\_ldap](https://github.com/emqx/emqx-auth-ldap) +             | etc/plugins/emqx\_auth\_ldap.conf       | LDAP 认证 / 访问控制        |
| [emqx\_auth\_http](https://github.com/emqx/emqx-auth-http) +             | etc/plugins/emqx\_auth\_http.conf       | HTTP 认证 / 访问控制        |
| [emqx\_auth\_mongo](https://github.com/emqx/emqx-auth-mongo) +           | etc/plugins/emqx\_auth\_mongo.conf      | MongoDB 认证 / 访问控制     |
| [emqx\_auth\_mysql](https://github.com/emqx/emqx-auth-mysql) +           | etc/plugins/emqx\_auth\_mysql.conf      | MySQL 认证 / 访问控制       |
| [emqx\_auth\_pgsql](https://github.com/emqx/emqx-auth-pgsql) +           | etc/plugins/emqx\_auth\_pgsql.conf      | PostgreSQL 认证 / 访问控制  |
| [emqx\_auth\_redis](https://github.com/emqx/emqx-auth-redis) +           | etc/plugins/emqx\_auth\_redis.conf      | Redis 认证 / 访问控制       |
| [emqx\_psk\_file](https://github.com/emqx/emqx-psk-file) +               | etc/plugins/emqx\_psk\_file.conf        | PSK 支持              |
| [emqx\_web\_hook](https://github.com/emqx/emqx-web-hook) +               | etc/plugins/emqx\_web\_hook.conf        | Web Hook 插件         |
| [emqx\_lua\_hook](https://github.com/emqx/emqx-lua-hook) +               | etc/plugins/emqx\_lua\_hook.conf        | Lua Hook 插件         |
| [emqx\_retainer](https://github.com/emqx/emqx-retainer) +                | etc/plugins/emqx\_retainer.conf         | Retain 消息存储模块       |
| [emqx\_rule\_engine](https://github.com/emqx/emqx-rule-engine) +         | etc/plugins/emqx\_rule\_engine.conf     | 规则引擎                |
| [emqx\_bridge\_mqtt](https://github.com/emqx/emqx-bridge-mqtt) +         | etc/plugins/emqx\_bridge\_mqtt.conf     | MQTT 消息桥接插件         |
| [emqx\_delayed\_publish](https://github.com/emqx/emqx-delayed-publish) + | etc/plugins/emqx\_delayed\_publish.conf | 客户端延时发布消息支持         |
| [emqx\_coap](https://github.com/emqx/emqx-coap) +                        | etc/plugins/emqx\_coap.conf             | CoAP 协议支持           |
| [emqx\_lwm2m](https://github.com/emqx/emqx-lwm2m) +                      | etc/plugins/emqx\_lwm2m.conf            | LwM2M 协议支持          |
| [emqx\_sn](https://github.com/emqx/emqx-sn) +                            | etc/plugins/emqx\_sn.conf               | MQTT-SN 协议支持        |
| [emqx\_stomp](https://github.com/emqx/emqx-stomp) +                      | etc/plugins/emqx\_stomp.conf            | Stomp 协议支持          |
| [emqx\_recon](https://github.com/emqx/emqx-recon) +                      | etc/plugins/emqx\_recon.conf            | Recon 性能调试          |
| [emqx\_reloader](https://github.com/emqx/emqx-reloader) +                | etc/plugins/emqx\_reloader.conf         | Reloader 代码热加载插件    |
| [emqx\_plugin\_template](https://github.com/emqx/emqx-plugin-template) + | etc/plugins/emqx\_plugin\_template.conf | 插件开发模版              |

其中插件的加载有四种方式：

1.  默认加载
2.  命令行启停插件
3.  使用 Dashboard 启停插件
4.  调用管理 API 启停插件

** 开启默认加载 **

如需在系统启动时就默认启动某插件，则直接在 `data/loaded_plugins` 配置入需要启动的插件，例如默认开启的加载的插件有：

``` sourceCode erlang
emqx_management.
emqx_rule_engine.
emqx_recon.
emqx_retainer.
emqx_dashboard.
```

** 命令行启停插件 **

在运行过程中，我们可以通过 CLI 命令的方式查看可用的插件列表、和启停某插件：

``` sourceCode bash
## 显示所有可用的插件列表
./bin/emqx_ctl plugins list

## 加载某插件
./bin/emqx_ctl plugins load emqx_auth_username

## 卸载某插件
./bin/emqx_ctl plugins unload emqx_auth_username

## 重新加载某插件
./bin/emqx_ctl plugins reload emqx_auth_username
```

** 使用 Dashboard 启停插件 **

如果 *EMQ X* 开启了 Dashbord 的插件 (默认开启) 还可以直接通过访问
`http://localhost:18083/plugins` 中的插件管理页面启停、或者配置插件。

## Dashboard 插件

[emqx\_dashboard](https://github.com/emqx/emqx-dashboard) 是 *EMQ X*
消息服务器的 Web 管理控制台，该插件默认开启。当 *EMQ X* 启动成功后，可访问
`http://localhost:18083` 进行查看，默认用户名 / 密码: admin/public。

Dashboard 中可查询 *EMQ X*
消息服务器基本信息、统计数据、负载情况，查询当前客户端列表 (Connections)、会话 (Sessions)、路由表 (Topics)、订阅关系 (Subscriptions)
等详细信息。

![image](./_static/images/dashboard.png)

除此之外，Dashboard 默认提供了一系列的 REST API 供前端调用。其详情可以参考 `Dashboard -> HTTP API`
部分。

### Dashboard 插件设置

etc/plugins/emqx\_dashboard.conf:

``` sourceCode properties
## Dashboard 默认用户名 / 密码
dashboard.default_user.login = admin
dashboard.default_user.password = public

## Dashboard HTTP 服务端口配置
dashboard.listener.http = 18083
dashboard.listener.http.acceptors = 2
dashboard.listener.http.max_clients = 512

## Dashboard HTTPS 服务端口配置
## dashboard.listener.https = 18084
## dashboard.listener.https.acceptors = 2
## dashboard.listener.https.max_clients = 512
## dashboard.listener.https.handshake_timeout = 15s
## dashboard.listener.https.certfile = etc/certs/cert.pem
## dashboard.listener.https.keyfile = etc/certs/key.pem
## dashboard.listener.https.cacertfile = etc/certs/cacert.pem
## dashboard.listener.https.verify = verify_peer
## dashboard.listener.https.fail_if_no_peer_cert = true
```

## HTTP API 与 CLI 管理插件

[emqx\_management](https://github.com/emqx/emqx-management) 是 *EMQ X*
消息服务器的 HTTP API 与 CLI 管理插件，该插件默认开启。当 *EMQ X* 启动成功后，用户即可通过该插件提供的
HTTP API 与 CLI 进行查询当前客户端列表等操作，详见 <span data-role="ref">rest\_api</span>
与 <span data-role="ref">commands</span>。

### HTTP API 与 CLI 管理设置

etc/plugins/emqx\_management.conf:

``` sourceCode properties
## 最多返回多少条数据，用于分页机制
management.max_row_limit = 10000

## 默认的应用 secret
# management.application.default_secret = public

## Management HTTP 服务器端口配置
management.listener.http = 8080
management.listener.http.acceptors = 2
management.listener.http.max_clients = 512
management.listener.http.backlog = 512
management.listener.http.send_timeout = 15s
management.listener.http.send_timeout_close = on

## Management HTTPS 服务器端口配置
## management.listener.https = 8081
## management.listener.https.acceptors = 2
## management.listener.https.max_clients = 512
## management.listener.https.backlog = 512
## management.listener.https.send_timeout = 15s
## management.listener.https.send_timeout_close = on
## management.listener.https.certfile = etc/certs/cert.pem
## management.listener.https.keyfile = etc/certs/key.pem
## management.listener.https.cacertfile = etc/certs/cacert.pem
## management.listener.https.verify = verify_peer
## management.listener.https.fail_if_no_peer_cert = true
```

## ClientID 认证插件

[emqx\_auth\_clientid](https://github.com/emqx/emqx-auth-clientid) 目前只支持
** 连接认证 **，通过 `clientid` 和 `password` 认证客户端。此插件在存储密码时会按照配置的 hash
算法将明文加密后存入。

### ClientID 认证配置

etc/plugins/emqx\_auth\_clientid.conf:

``` sourceCode properties
## Default usernames Examples
##auth.client.1.clientid = id
##auth.client.1.password = passwd
##auth.client.2.clientid = dev:devid
##auth.client.2.password = passwd2
##auth.client.3.clientid = app:appid
##auth.client.3.password = passwd3
##auth.client.4.clientid = client~!@#$%^&*()_+
##auth.client.4.password = passwd~!@#$%^&*()_+

## 密码加密方式
## 枚举值: plain | md5 | sha | sha256
auth.client.password_hash = sha256
```

## Username 认证插件

[emqx\_auth\_username](https://github.com/emqx/emqx-auth-username) 目前只支持
** 连接认证 **，通过 `username` 和 `password` 认证客户端。此插件在存储密码时会按照配置的 hash
算法将明文加密后存入。

### 用户名认证配置

etc/plugins/emqx\_auth\_username.conf:

``` sourceCode properties
## Default usernames Examples:
##auth.user.1.username = admin
##auth.user.1.password = public
##auth.user.2.username = feng@emqtt.io
##auth.user.2.password = public
##auth.user.3.username = name~!@#$%^&*()_+
##auth.user.3.password = pwsswd~!@#$%^&*()_+

## 密码加密方式
## 枚举值: plain | md5 | sha | sha256
auth.user.password_hash = sha256
```

## JWT 认证插件

[emqx\_auth\_jwt](https://github.com/emqx/emqx-auth-jwt) 支持基于
[JWT](https://jwt.io) 的方式，对连接的客户端进行认证，只支持 ** 连接认证 ** 功能。它会解析并校验 Token
的合理性和时效性、满足则允许连接。

### JWT 认证配置

etc/plugins/emqx\_auth\_jwt.conf:

``` sourceCode properties
## HMAC Hash 算法密钥
auth.jwt.secret = emqxsecret

## RSA 或 ECDSA 算法的公钥
## auth.jwt.pubkey = etc/certs/jwt_public_key.pem

## JWT 串的来源
## 枚举值: username | password
auth.jwt.from = password
```

## LDAP 认证 / 访问控制插件

[emqx\_auth\_ldap](https://github.com/emqx/emqx-auth-ldap) 支持访问
[LDAP](https://ldap.com) 实现 ** 连接认证 **、\** 访问控制 *\* 功能。

### LDAP 认证插件配置

etc/plugins/emqx\_auth\_ldap.conf:

``` sourceCode properties
auth.ldap.servers = 127.0.0.1

auth.ldap.port = 389

auth.ldap.pool = 8

auth.ldap.bind_dn = cn=root,dc=emqx,dc=io

auth.ldap.bind_password = public

auth.ldap.timeout = 30s

auth.ldap.device_dn = ou=device,dc=emqx,dc=io

auth.ldap.match_objectclass = mqttUser

auth.ldap.username.attributetype = uid

auth.ldap.password.attributetype = userPassword

auth.ldap.ssl = false

## auth.ldap.ssl.certfile = etc/certs/cert.pem

## auth.ldap.ssl.keyfile = etc/certs/key.pem

## auth.ldap.ssl.cacertfile = etc/certs/cacert.pem

## auth.ldap.ssl.verify = verify_peer

## auth.ldap.ssl.fail_if_no_peer_cert = true
```

## HTTP 认证 / 访问控制插件

[emqx\_auth\_http](https://github.com/emqx/emqx-auth-http) 插件实现 ** 连接认证 **
与 ** 访问控制 ** 的功能。它会将每个请求发送到指定的 HTTP 服务，通过其返回值来判断是否具有操作权限。

该插件总共支持三个请求分别为：

1.  **auth.http.auth\_req**: 连接认证
2.  **auth.http.super\_req**: 判断是否为超级用户
3.  **auth.http.acl\_req**: 访问控制权限查询

每个请求的参数都支持使用真实的客户端的 Username, IP 地址等进行自定义。

<div class="note">

<div class="admonition-title">

Note

</div>

其中在 3.1 版本中新增的 % C % d 的支持。

</div>

### HTTP 认证插件配置

etc/plugins/emqx\_auth\_http.conf:

``` sourceCode properties
## http 请求超时时间，0 为不设置超时
## auth.http.request.timeout = 0

## http 建立 tcp 连接的超时时间，默认与 'request.timeout' 一致
## auth.http.request.connect_timout = 0

## http 请求最大重试次数
auth.http.request.retry_times = 3

## http 重试间隔
auth.http.request.retry_interval = 1s

## 重试间隔的退避指数，实际值 = `interval * backoff ^ times`
auth.http.request.retry_backoff = 2.0

## https 证书配置
## auth.http.ssl.cacertfile = {{ platform_etc_dir }}/certs/ca.pem
## auth.http.ssl.certfile = {{ platform_etc_dir }}/certs/client-cert.pem
## auth.http.ssl.keyfile = {{ platform_etc_dir }}/certs/client-key.pem

## 占位符:
##  - % u: username
##  - % c: clientid
##  - % a: ipaddress
##  - % P: password
##  - % C: common name of client TLS cert
##  - % d: subject of client TLS cert
auth.http.auth_req = http://127.0.0.1:8080/mqtt/auth

## AUTH 请求的 HTTP 方法和参数配置
auth.http.auth_req.method = post
auth.http.auth_req.params = clientid=% c,username=% u,password=% P

auth.http.super_req = http://127.0.0.1:8080/mqtt/superuser
auth.http.super_req.method = post
auth.http.super_req.params = clientid=% c,username=% u

## 占位符:
##  - % A: 1 | 2, 1 = sub, 2 = pub
##  - % u: username
##  - % c: clientid
##  - % a: ipaddress
##  - % t: topic
auth.http.acl_req = http://127.0.0.1:8080/mqtt/acl
auth.http.acl_req.method = get
auth.http.acl_req.params = access=% A,username=% u,clientid=% c,ipaddr=% a,topic=% t
```

### HTTP API 返回值处理

** 连接认证 **：

``` sourceCode bash
## 认证成功
HTTP Status Code: 200

## 忽略此次认证
HTTP Status Code: 200
Body: ignore

## 认证失败
HTTP Status Code: Except 200
```

** 超级用户 **：

``` sourceCode bash
## 确认为超级用户
HTTP Status Code: 200

## 非超级用户
HTTP Status Code: Except 200
```

** 访问控制 **：

``` sourceCode bash
## 允许 Publish/Subscribe：
HTTP Status Code: 200

## 忽略此次鉴权:
HTTP Status Code: 200
Body: ignore

## 拒绝该次 Publish/Subscribe:
HTTP Status Code: Except 200
```

## MySQL 认证 / 访问控制插件

[emqx\_auth\_mysql](https://github.com/emqx/emqx-auth-mysql) 支持访问 MySQL
实现 ** 连接认证 **、\** 访问控制 *\* 功能。要实现这些功能，我们需要在 MySQL 中创建两张表，其格式如下：

### MQTT 用户表

``` sourceCode sql
CREATE TABLE `mqtt_user` (
  `id` int (11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar (100) DEFAULT NULL,
  `password` varchar (100) DEFAULT NULL,
  `salt` varchar (35) DEFAULT NULL,
  `is_superuser` tinyint (1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
```

<div class="note">

<div class="admonition-title">

Note

</div>

插件同样支持使用自定义结构的表，通过 `auth_query` 配置查询语句即可。

</div>

### MQTT 访问控制表

``` sourceCode sql
CREATE TABLE `mqtt_acl` (
  `id` int (11) unsigned NOT NULL AUTO_INCREMENT,
  `allow` int (1) DEFAULT NULL COMMENT '0: deny, 1: allow',
  `ipaddr` varchar (60) DEFAULT NULL COMMENT 'IpAddress',
  `username` varchar (100) DEFAULT NULL COMMENT 'Username',
  `clientid` varchar (100) DEFAULT NULL COMMENT 'ClientId',
  `access` int (2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar (100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

INSERT INTO `mqtt_acl` (`id`, `allow`, `ipaddr`, `username`, `clientid`, `access`, `topic`)
VALUES
    (1,1,NULL,'$all',NULL,2,'#'),
    (2,0,NULL,'$all',NULL,1,'$SYS/#'),
    (3,0,NULL,'$all',NULL,1,'eq #'),
    (5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
    (6,1,'127.0.0.1',NULL,NULL,2,'#'),
    (7,1,NULL,'dashboard',NULL,1,'$SYS/#');
```

### 配置 MySQL 认证鉴权插件

etc/plugins/emqx\_auth\_mysql.conf:

``` sourceCode properties
## Mysql 服务器地址
auth.mysql.server = 127.0.0.1:3306

## Mysql 连接池大小
auth.mysql.pool = 8

## Mysql 连接用户名
## auth.mysql.username =

## Mysql 连接密码
## auth.mysql.password =

## Mysql 认证用户表名
auth.mysql.database = mqtt

## Mysql 查询超时时间
auth.mysql.query_timeout = 5s

## 可用占位符:
##  - % u: username
##  - % c: clientid
##  - % C: common name of client TLS cert
##  - % d: subject of client TLS cert
## 注：该条 SQL 必须且仅需查询 `password` 字段
auth.mysql.auth_query = select password from mqtt_user where username = '% u' limit 1

## 密码加密方式: plain, md5, sha, sha256, pbkdf2
auth.mysql.password_hash = sha256

## 超级用户查询语句
auth.mysql.super_query = select is_superuser from mqtt_user where username = '% u' limit 1

## ACL 查询语句
## 注：可以增加 'ORDER BY' 子句以控制 ACL 规则的生效顺序
auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '% a' or username = '% u' or username = '$all' or clientid = '% c'
```

此外，为防止密码域过于简单而带来安全的隐患问题，该插件还支持密码加盐操作：

``` sourceCode properties
## 加盐密文格式
## auth.mysql.password_hash = salt,sha256
## auth.mysql.password_hash = salt,bcrypt
## auth.mysql.password_hash = sha256,salt

## pbkdf2 带 macfun 格式
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
## auth.mysql.password_hash = pbkdf2,sha256,1000,20
```

<div class="note">

<div class="admonition-title">

Note

</div>

3.1 版本新增 % C % d 支持。

</div>

## Postgres 认证插件

[emqx\_auth\_pgsql](https://github.com/emqx/emqx-auth-pgsql) 通过访问
Postgres 实现 ** 连接认证 **、\** 访问控制 *\* 功能。同样需要定义两张表如下：

### Postgres MQTT 用户表

``` sourceCode sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying (100),
  password character varying (100),
  salt character varying (40)
);
```

### Postgres MQTT 访问控制表

``` sourceCode sql
CREATE TABLE mqtt_acl (
  id SERIAL primary key,
  allow integer,
  ipaddr character varying (60),
  username character varying (100),
  clientid character varying (100),
  access  integer,
  topic character varying (100)
);

INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
VALUES
    (1,1,NULL,'$all',NULL,2,'#'),
    (2,0,NULL,'$all',NULL,1,'$SYS/#'),
    (3,0,NULL,'$all',NULL,1,'eq #'),
    (5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
    (6,1,'127.0.0.1',NULL,NULL,2,'#'),
    (7,1,NULL,'dashboard',NULL,1,'$SYS/#');
```

### 配置 Postgres 认证鉴权插件

etc/plugins/emqx\_auth\_pgsql.conf:

``` sourceCode properties
## PostgreSQL 服务地址
auth.pgsql.server = 127.0.0.1:5432

## PostgreSQL 连接池大小
auth.pgsql.pool = 8

auth.pgsql.username = root

## auth.pgsql.password =

auth.pgsql.database = mqtt

auth.pgsql.encoding = utf8

## 连接认证查询 SQL
## 占位符:
##  - % u: username
##  - % c: clientid
##  - % C: common name of client TLS cert
##  - % d: subject of client TLS cert
auth.pgsql.auth_query = select password from mqtt_user where username = '% u' limit 1

## 加密方式: plain | md5 | sha | sha256 | bcrypt
auth.pgsql.password_hash = sha256

## 超级用户查询语句 (占位符与认证一致)
auth.pgsql.super_query = select is_superuser from mqtt_user where username = '% u' limit 1

## ACL 查询语句
##
## 占位符:
##  - % a: ipaddress
##  - % u: username
##  - % c: clientid
## 注：可以增加 'ORDER BY' 子句以控制 ACL 规则的生效顺序
auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '% a' or username = '% u' or username = '$all' or clientid = '% c'
```

同样的 password\_hash 可以配置为更为安全的模式：

``` sourceCode properties
## 加盐加密格式
## auth.pgsql.password_hash = salt,sha256
## auth.pgsql.password_hash = sha256,salt
## auth.pgsql.password_hash = salt,bcrypt

## pbkdf2 macfun 格式
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
## auth.pgsql.password_hash = pbkdf2,sha256,1000,20
```

开启以下配置，则可支持 TLS 连接到 Postgres：

``` sourceCode properties
## 是否开启 SSL
auth.pgsql.ssl = false

## 证书配置
## auth.pgsql.ssl_opts.keyfile =
## auth.pgsql.ssl_opts.certfile =
## auth.pgsql.ssl_opts.cacertfile =
```

<div class="note">

<div class="admonition-title">

Note

</div>

3.1 版本新增 % C % d 支持。

</div>

## Redis 认证 / 访问控制插件

[emqx\_auth\_redis](https://github.com/emqx/emqx-auth-redis) 通过访问 Redis
数据以实现 ** 连接认证 ** 和 ** 访问控制 ** 的功能。

### 配置 Redis 认证插件

etc/plugins/emqx\_auth\_redis.conf:

``` sourceCode properties
## Redis 服务集群类型
## 枚举值: single | sentinel | cluster
auth.redis.type = single

## Redis 服务器地址
##
## Single Redis Server: 127.0.0.1:6379, localhost:6379
## Redis Sentinel: 127.0.0.1:26379,127.0.0.2:26379,127.0.0.3:26379
## Redis Cluster: 127.0.0.1:6379,127.0.0.2:6379,127.0.0.3:6379
auth.redis.server = 127.0.0.1:6379

## Redis sentinel 名称
## auth.redis.sentinel = mymaster

## Redis 连接池大小
auth.redis.pool = 8

## Redis database 序号
auth.redis.database = 0

## Redis password.
## auth.redis.password =

## Redis 查询超时时间
auth.redis.query_timeout = 5s

## 认证查询指令
## 占位符:
##  - % u: username
##  - % c: clientid
##  - % C: common name of client TLS cert
##  - % d: subject of client TLS cert
auth.redis.auth_cmd = HMGET mqtt_user:% u password

## 密码加密方式.
## 枚举: plain | md5 | sha | sha256 | bcrypt
auth.redis.password_hash = plain

## 超级用户查询指令 (占位符与认证一致)
auth.redis.super_cmd = HGET mqtt_user:% u is_superuser

## ACL 查询指令
## 占位符:
##  - % u: username
##  - % c: clientid
auth.redis.acl_cmd = HGETALL mqtt_acl:% u
```

同样，该插件支持更安全的密码格式：

``` sourceCode properties
## 加盐密文格式
## auth.redis.password_hash = salt,sha256
## auth.redis.password_hash = sha256,salt
## auth.redis.password_hash = salt,bcrypt

## pbkdf2 macfun 格式
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
## auth.redis.password_hash = pbkdf2,sha256,1000,20
```

<div class="note">

<div class="admonition-title">

Note

</div>

3.1 版本新增 % C % d 支持。

</div>

### Redis 用户 Hash

默认基于用户 Hash 认证：

``` sourceCode 
HSET mqtt_user:<username> is_superuser 1
HSET mqtt_user:<username> password "passwd"
HSET mqtt_user:<username> salt "salt"
```

### Redis ACL 规则 Hash

默认采用 Hash 存储 ACL 规则：

``` sourceCode 
HSET mqtt_acl:<username> topic1 1
HSET mqtt_acl:<username> topic2 2
HSET mqtt_acl:<username> topic3 3
```

<div class="note">

<div class="admonition-title">

Note

</div>

1: subscribe, 2: publish, 3: pubsub

</div>

## MongoDB 认证 / 访问控制插件

[emqx\_auth\_mongo](https://github.com/emqx/emqx-auth-mongo) 通过访问
MongoDB 实现 ** 连接认证 ** 和 ** 访问控制 ** 功能。

### 配置 MongoDB 认证插件

etc/plugins/emqx\_auth\_mongo.conf:

``` sourceCode properties
## MongoDB 拓扑类型
## 枚举: single | unknown | sharded | rs
auth.mongo.type = single

## rs 模式下的 `set name`
## auth.mongo.rs_set_name =

## MongoDB 服务地址
auth.mongo.server = 127.0.0.1:27017

## MongoDB 连接池大小
auth.mongo.pool = 8

## 连接认证信息
## auth.mongo.login =
## auth.mongo.password =
## auth.mongo.auth_source = admin

## 认证数据表名
auth.mongo.database = mqtt

## 查询超时时间
auth.mongo.query_timeout = 5s

## 认证查询配置
auth.mongo.auth_query.collection = mqtt_user
auth.mongo.auth_query.password_field = password
auth.mongo.auth_query.password_hash = sha256

## 连接认证查询字段列表
## 占位符:
##  - % u: username
##  - % c: clientid
##  - % C: common name of client TLS cert
##  - % d: subject of client TLS cert
auth.mongo.auth_query.selector = username=% u

## 超级用户查询
auth.mongo.super_query = on
auth.mongo.super_query.collection = mqtt_user
auth.mongo.super_query.super_field = is_superuser
auth.mongo.super_query.selector = username=% u

## ACL 查询配置
auth.mongo.acl_query = on
auth.mongo.acl_query.collection = mqtt_acl

auth.mongo.acl_query.selector = username=% u
```

<div class="note">

<div class="admonition-title">

Note

</div>

3.1 版本新增 % C % d 支持。

</div>

### MongoDB 数据库

``` sourceCode javascript
use mqtt
db.createCollection ("mqtt_user")
db.createCollection ("mqtt_acl")
db.mqtt_user.ensureIndex ({"username":1})
```

<div class="note">

<div class="admonition-title">

Note

</div>

数据库、集合名称可自定义。

</div>

### MongoDB 用户集合

``` sourceCode javascript
{
    username: "user",
    password: "password hash",
    is_superuser: boolean (true, false),
    created: "datetime"
}
```

示例：

``` sourceCode 
db.mqtt_user.insert ({username: "test", password: "password hash", is_superuser: false})
db.mqtt_user:insert ({username: "root", is_superuser: true})
```

### MongoDB ACL 集合

``` sourceCode javascript
{
    username: "username",
    clientid: "clientid",
    publish: ["topic1", "topic2", ...],
    subscribe: ["subtop1", "subtop2", ...],
    pubsub: ["topic/#", "topic1", ...]
}
```

示例：

``` sourceCode 
db.mqtt_acl.insert ({username: "test", publish: ["t/1", "t/2"], subscribe: ["user/% u", "client/% c"]})
db.mqtt_acl.insert ({username: "admin", pubsub: ["#"]})
```

## PSK 认证插件

[emqx\_psk\_file](https://github.com/emqx/emqx-psk-file) 插件主要提供了 PSK
支持。其目的是用于在客户端建立 TLS/DTLS 连接时，通过 PSK 方式实现 ** 连接认证 ** 的功能。

### 配置 PSK 认证插件

etc/plugins/emqx\_psk\_file.conf:

``` sourceCode properties
psk.file.path = etc/psk.txt
```

## WebHook 插件

[emqx\_web\_hook](https://github.com/emqx/emqx-web-hook) 插件可以将所有 *EMQ X*
的事件及消息都发送到指定的 HTTP 服务器。

### 配置 WebHook 插件

etc/plugins/emqx\_web\_hook.conf:

``` sourceCode properties
## 回调的 Web Server 地址
web.hook.api.url = http://127.0.0.1:8080

## 编码 Payload 字段
## 枚举值: undefined | base64 | base62
## 默认值: undefined (不进行编码)
## web.hook.encode_payload = base64

## 消息、事件配置
web.hook.rule.client.connected.1     = {"action": "on_client_connected"}
web.hook.rule.client.disconnected.1  = {"action": "on_client_disconnected"}
web.hook.rule.client.subscribe.1     = {"action": "on_client_subscribe"}
web.hook.rule.client.unsubscribe.1   = {"action": "on_client_unsubscribe"}
web.hook.rule.session.created.1      = {"action": "on_session_created"}
web.hook.rule.session.subscribed.1   = {"action": "on_session_subscribed"}
web.hook.rule.session.unsubscribed.1 = {"action": "on_session_unsubscribed"}
web.hook.rule.session.terminated.1   = {"action": "on_session_terminated"}
web.hook.rule.message.publish.1      = {"action": "on_message_publish"}
web.hook.rule.message.deliver.1      = {"action": "on_message_deliver"}
web.hook.rule.message.acked.1        = {"action": "on_message_acked"}
```

## Lua 插件

[emqx\_lua\_hook](https://github.com/emqx/emqx-lua-hook)
插件将所有的事件和消息都发送到指定的 Lua 函数上。其具体使用参见其
README。

## Retainer 插件

[emqx\_retainer](https://github.com/emqx/emqx-retainer) 该插件设置为默认启动，为
*EMQ X* 提供 Retained 类型的消息支持。它会将所有主题的 Retained
消息存储在集群的数据库中，并待有客户端订阅该主题的时候将该消息投递出去。

### 配置 Retainer 插件

etc/plugins/emqx\_retainer.conf:

``` sourceCode properties
## retained 消息存储方式
##  - ram: 仅内存
##  - disc: 内存和磁盘
##  - disc_only: 仅磁盘
retainer.storage_type = ram

## 最大存储数 (0 表示未限制)
retainer.max_retained_messages = 0

## 单条最大可存储消息大小
retainer.max_payload_size = 1MB

## 过期时间，0 表示永不过期
## 单位: h 小时；m 分钟；s 秒。如 60m 表示 60 分钟
retainer.expiry_interval = 0
```

## MQTT 消息桥接插件

** 桥接 ** 的概念是 EMQ X 支持将自身某类主题的消息通过某种方式转发到另一个 MQTT Broker。

** 桥接 ** 与 ** 集群 ** 的不同在于：桥接不会复制主题树与路由表，只根据桥接规则转发 MQTT 消息。

目前 MQTT 消息插件支持的桥接方式有:

  - RPC 桥接：RPC 桥接只能在 EMQ X Broker 间使用，且不支持订阅远程节点的主题去同步数据
  - MQTT 桥接：MQTT 桥接同时支持转发和通过订阅主题来实现数据同步两种方式

在 EMQ X 中，通过修改 `etc/plugins/emqx_bridge_mqtt.conf` 来配置 bridge。EMQ X
根据不同的 name 来区分不同的 bridge。例如:

    ## 桥接地址： 使用节点名（nodename@host）则用于 RPC 桥接，使用 host:port 用于 MQTT 连接
    bridge.mqtt.aws.address = 127.0.0.1:1883

该项配置声明了一个名为 `aws` 的 bridge 并指定以 MQTT 的方式桥接到 `127.0.0.1:1883` 这台 MQTT 服务器

在需要创建多个 bridge 时，可以先复制其全部的配置项，在通过使用不同的 name 来标示（比如
bridge.mqtt.$name.address 其中 $name 指代的为 bridge 的名称）

### 配置 MQTT 消息桥接插件

etc/plugins/emqx\_bridge\_mqtt.conf

``` sourceCode properties
## 桥接地址： 使用节点名（nodename@host）则用于 RPC 桥接，使用 host:port 用于 MQTT 连接
bridge.mqtt.aws.address = 192.168.1.2:1883

## 桥接的协议版本
## 枚举值: mqttv3 | mqttv4 | mqttv5
bridge.mqtt.aws.proto_ver = mqttv4

## 客户端的 clientid
bridge.mqtt.aws.clientid = bridge_emq

## 客户端的 clean_start 字段
## 注：有些 MQTT Broker 需要将 clean_start 值设成 `true`
bridge.mqtt.aws.clean_start = true

## 客户端的 username 字段
bridge.mqtt.aws.username = user

## 客户端的 password 字段
bridge.mqtt.aws.password = passwd

## 客户端是否使用 ssl 来连接远程服务器
bridge.mqtt.aws.ssl = off

## 客户端 SSL 连接的 CA 证书 (PEM 格式)
bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem

## 客户端 SSL 连接的 SSL 证书
bridge.mqtt.aws.certfile = etc/certs/client-cert.pem

## 客户端 SSL 连接的密钥文件
bridge.mqtt.aws.keyfile = etc/certs/client-key.pem

## SSL 加密方式
bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384

## TLS PSK 的加密套件
## 注意 'listener.ssl.external.ciphers' 和 'listener.ssl.external.psk_ciphers' 不能同时配置
##
## See 'https://tools.ietf.org/html/rfc4279#section-2'.
## bridge.mqtt.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

## 客户端的心跳间隔
bridge.mqtt.aws.keepalive = 60s

## 支持的 TLS 版本
bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1

## 需要被转发的消息的主题
bridge.mqtt.aws.forwards = sensor1/#,sensor2/#

## 挂载点 (mountpoint)
bridge.mqtt.aws.mountpoint = bridge/emqx2/${node}/

## 订阅对端的主题
bridge.mqtt.aws.subscription.1.topic = cmd/topic1

## 订阅对端主题的 QoS
bridge.mqtt.aws.subscription.1.qos = 1

## 桥接的重连间隔
## 默认: 30 秒
bridge.mqtt.aws.reconnect_interval = 30s

## QoS1/QoS2 消息的重传间隔
bridge.mqtt.aws.retry_interval = 20s

## Inflight 大小.
bridge.mqtt.aws.max_inflight_batches = 32

## emqx_bridge 内部用于 batch 的消息数量
bridge.mqtt.aws.queue.batch_count_limit = 32

## emqx_bridge 内部用于 batch 的消息字节数
bridge.mqtt.aws.queue.batch_bytes_limit = 1000MB

## 放置 replayq 队列的路径，如果没有在配置中指定该项，那么 replayq
## 将会以 `mem-only` 的模式运行，消息不会缓存到磁盘上。
bridge.mqtt.aws.queue.replayq_dir = data/emqx_aws_bridge/

## Replayq 数据段大小
bridge.mqtt.aws.queue.replayq_seg_bytes = 10MB
```

## Delayed Publish 插件

[emqx\_delayed\_publish](https://github.com/emqx/emqx-delayed-publish)
提供了延迟发送消息的功能。当客户端使用特殊主题前缀 `$delayed/<seconds>/` 发布消息到 *EMQ X*
时，\*EMQ X\* 将在 `<seconds>` 秒后发布该主题消息。

## CoAP 协议插件

[emqx\_coap](https://github.com/emqx/emqx-coap) 提供对 CoAP 协议 (RFC
7252) 的支持。

### 配置 CoAP 协议插件

etc/plugins/emqx\_coap.conf:

``` sourceCode properties
coap.port = 5683

coap.keepalive = 120s

coap.enable_stats = off
```

若开启以下配置，则可以支持 DTLS：

``` sourceCode properties
## DTLS 监听端口
coap.dtls.port = 5684

coap.dtls.keyfile = {{ platform_etc_dir }}/certs/key.pem

coap.dtls.certfile = {{ platform_etc_dir }}/certs/cert.pem

## 双向认证相关
## coap.dtls.verify = verify_peer
## coap.dtls.cacertfile = {{ platform_etc_dir }}/certs/cacert.pem
## coap.dtls.fail_if_no_peer_cert = false
```

### 测试 CoAP 插件

我们可以通过安装 [libcoap](https://github.com/obgm/libcoap) 来测试 *EMQ X* 对 CoAP
协议的支持情况。

``` sourceCode bash
yum install libcoap

% coap client publish message
coap-client -m put -e "qos=0&retain=0&message=payload&topic=hello" coap://localhost/mqtt
```

## LwM2M 协议插件

[emqx\_lwm2m](https://github.com/emqx/emqx-lwm2m) 提供对 LwM2M 协议的支持。

### 配置 LwM2M 插件

etc/plugins/emqx\_lwm2m.conf:

``` sourceCode properties
## LwM2M 监听端口
lwm2m.port = 5683

## Lifetime 限制
lwm2m.lifetime_min = 1s
lwm2m.lifetime_max = 86400s

## Q Mode 模式下 `time window` 长度，单位秒。
## 超过该 window 的消息都将被缓存
#lwm2m.qmode_time_window = 22

## LwM2M 是否部署在 coaproxy 后
#lwm2m.lb = coaproxy

## 设备上线后，主动 observe 所有的 objects
#lwm2m.auto_observe = off

# 主题挂载点
# Placeholders supported:
#    '% e': Endpoint Name
#    '% a': IP Address
lwm2m.mountpoint = lwm2m/% e/

## client register 成功后主动向 EMQ X 订阅的主题
## 占位符:
##    '% e': Endpoint Name
##    '% a': IP Address
lwm2m.topics.command = dn/#

## client 应答消息 (response) 到 EMQ X 的主题
lwm2m.topics.response = up/resp

## client 通知类消息 (noify message) 到 EMQ X 的主题
lwm2m.topics.notify = up/notify

## client 注册类消息 (register message) 到 EMQ X 的主题
lwm2m.topics.register = up/resp

# client 更新类消息 (update message) 到 EMQ X 的主题
lwm2m.topics.update = % e/up/resp

# Object 定义的 xml 文件位置
lwm2m.xml_dir =  etc/lwm2m_xml
```

同样可以通过以下配置打开 DTLS 支持：

``` sourceCode properties
# DTLS 证书配置
lwm2m.certfile = etc/certs/cert.pem
lwm2m.keyfile = etc/certs/key.pem
```

## MQTT-SN 协议插件

[emqx\_sn](https://github.com/emqx/emqx-sn) 插件提供对
[MQTT-SN](https://github.com/emqx/emqx-sn) 协议的支持。

### 配置 MQTT-SN 协议插件

etc/plugins/emqx\_sn.conf:

``` sourceCode properties
mqtt.sn.port = 1884
```

## Stomp 协议插件

[emqx\_stomp](https://github.com/emqx/emqx-stomp) 提供对 Stomp
协议的支持。支持客户端通过 Stomp 1.0/1.1/1.2 协议连接 EMQ
X，发布订阅 MQTT 消息。

### 配置 Stomp 插件

<div class="note">

<div class="admonition-title">

Note

</div>

Stomp 协议端口: 61613

</div>

etc/plugins/emqx\_stomp.conf:

``` sourceCode properties
stomp.default_user.login = guest

stomp.default_user.passcode = guest

stomp.allow_anonymous = true

stomp.frame.max_headers = 10

stomp.frame.max_header_length = 1024

stomp.frame.max_body_length = 8192

stomp.listener = 61613

stomp.listener.acceptors = 4

stomp.listener.max_clients = 512
```

## Recon 性能调试插件

[emqx\_recon](https://github.com/emqx/emqx-recon) 插件集成了 recon
性能调测库，可用于查看当前系统的一些状态信息，例如：

``` sourceCode bash
./bin/emqx_ctl recon

recon memory                 #recon_alloc:memory/2
recon allocated              #recon_alloc:memory (allocated_types, current|max)
recon bin_leak               #recon:bin_leak (100)
recon node_stats             #recon:node_stats (10, 1000)
recon remote_load Mod        #recon:remote_load (Mod)
```

### 配置 Recon 插件

etc/plugins/emqx\_recon.conf:

``` sourceCode properties
%% Garbage Collection: 10 minutes
recon.gc_interval = 600
```

## Reloader 热加载插件

[emqx\_reloader](https://github.com/emqx/emqx-reloader)
用于开发调试的代码热升级插件。加载该插件后 *EMQ X*
会根据配置的时间间隔自动热升级更新代码。

同时，也提供了 CLI 命令来指定 reload 某一个模块：

``` sourceCode bash
./bin/emqx_ctl reload <Module>
```

<div class="note">

<div class="admonition-title">

Note

</div>

产品部署环境不建议使用该插件。

</div>

### 配置 Reloader 插件

etc/plugins/emqx\_reloader.conf:

``` sourceCode properties
reloader.interval = 60

reloader.logfile = log/reloader.log
```

## 插件开发模版

[emqx\_plugin\_template](https://github.com/emqx/emqx-plugin-template)
是一个 *EMQ X* 插件模板，在功能上并无任何意义。

开发者需要自定义插件时，可以查看该插件的代码和结构，以更快地开发一个标准的 *EMQ X* 插件。插件实际是一个普通的 `Erlang
Application`，其配置文件为:`etc/${PluginName}.config`。

## EMQ X 插件开发

### 创建插件项目

参考
[emqx\_plugin\_template](https://github.com/emqx/emqx-plugin-template)
插件模版创建新的插件项目。

<div class="note">

<div class="admonition-title">

Note

</div>

在 `<plugin name>_app.erl` 文件中必须加上标签 `-emqx_plugin (?MODULE).` 以表明这是一个 EMQ
X 的插件。

</div>

### 创建认证 / 访问控制模块

认证演示模块 - emqx\_auth\_demo.erl

``` sourceCode erlang
-module (emqx_auth_demo).

-export ([ init/1
        , check/2
        , description/0
        ]).

init (Opts) -> {ok, Opts}.

check (_ClientInfo = #{clientid := ClientId, username := Username, password := Password}, _State) ->
    io:format ("Auth Demo: clientId=~p, username=~p, password=~p~n", [ClientId, Username, Password]),
    ok.

description () -> "Auth Demo Module".
```

访问控制演示模块 - emqx\_acl\_demo.erl

``` sourceCode erlang
-module (emqx_acl_demo).

-include_lib ("emqx/include/emqx.hrl").

%% ACL callbacks
-export ([ init/1
        , check_acl/5
        , reload_acl/1
        , description/0
        ]).

init (Opts) ->
    {ok, Opts}.

check_acl ({ClientInfo, PubSub, _NoMatchAction, Topic}, _State) ->
    io:format ("ACL Demo: ~p ~p ~p~n", [ClientInfo, PubSub, Topic]),
    allow.

reload_acl (_State) ->
    ok.

description () -> "ACL Demo Module".
```

注册认证、访问控制模块 - emqx\_plugin\_template\_app.erl

``` sourceCode erlang
ok = emqx:hook ('client.authenticate', fun emqx_auth_demo:check/2, []),
ok = emqx:hook ('client.check_acl', fun emqx_acl_demo:check_acl/5, []).
```

### 注册钩子 (Hooks)

通过钩子 (Hook) 处理客户端上下线、主题订阅、消息收发。

emqx\_plugin\_template.erl:

``` sourceCode erlang
load (Env) ->
    emqx:hook ('client.connect',      {?MODULE, on_client_connect, [Env]}),
    emqx:hook ('client.connack',      {?MODULE, on_client_connack, [Env]}),
    emqx:hook ('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook ('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook ('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    emqx:hook ('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
    emqx:hook ('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    emqx:hook ('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    emqx:hook ('session.created',     {?MODULE, on_session_created, [Env]}),
    emqx:hook ('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook ('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    emqx:hook ('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    emqx:hook ('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    emqx:hook ('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
    emqx:hook ('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    emqx:hook ('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook ('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    emqx:hook ('message.acked',       {?MODULE, on_message_acked, [Env]}),
    emqx:hook ('message.dropped',     {?MODULE, on_message_dropped, [Env]}).
```

所有可用钩子 (Hook) 说明:

| 钩子                   | 说明          |
| -------------------- | ----------- |
| client.connect       | 收到客户端连接报文   |
| client.connack       | 下发连接应答      |
| client.connected     | 客户端上线       |
| client.disconnected  | 客户端连接断开     |
| client.authenticate  | 连接认证        |
| client.check\_acl    | ACL 校验      |
| client.subscribe     | 客户端订阅主题     |
| client.unsubscribe   | 客户端取消订阅主题   |
| session.created      | 会话创建        |
| session.subscribed   | 会话订阅主题后     |
| session.unsubscribed | 会话取消订阅主题后   |
| session.resumed      | 会话恢复        |
| session.discarded    | 会话被删除       |
| session.takeovered   | 会话被其它节点接管   |
| session.terminated   | 会话终止        |
| message.publish      | MQTT 消息发布   |
| message.delivered    | MQTT 消息进行投递 |
| message.acked        | MQTT 消息回执   |
| message.dropped      | MQTT 消息丢弃   |

### 注册 CLI 命令

扩展命令行演示模块 - emqx\_cli\_demo.erl

``` sourceCode erlang
-module (emqx_cli_demo).

-export ([cmd/1]).

cmd (["arg1", "arg2"]) ->
    emqx_cli:print ("ok");

cmd (_) ->
    emqx_cli:usage ([{"cmd arg1 arg2", "cmd demo"}]).
```

注册命令行模块 - emqx\_plugin\_template\_app.erl

``` sourceCode erlang
ok = emqx_ctl:register_command (cmd, {emqx_cli_demo, cmd}, []),
```

插件加载后，`./bin/emqx_ctl` 新增命令行：

``` sourceCode bash
./bin/emqx_ctl cmd arg1 arg2
```

### 插件配置文件

插件自带配置文件放置在 `etc/${plugin_name}.conf|config`。\*EMQ X\* 支持两种插件配置格式:

1.  Erlang 原生配置文件格式 - `${plugin_name}.config`:
    
        [
          {plugin_name, [
            {key, value}
          ]}
        ].

2.  sysctl 的 `k = v` 通用格式 - `${plugin_name}.conf`:
    
        plugin_name.key = value

<div class="note">

<div class="admonition-title">

Note

</div>

`k = v` 格式配置需要插件开发者创建 `priv/plugin_name.schema` 映射文件。

</div>

### 编译发布插件

1.  clone emqx-rel 项目：

<!-- end list -->

``` sourceCode bash
git clone https://github.com/emqx/emqx-rel.git
```

2.  rebar.config 添加依赖：

<!-- end list -->

``` sourceCode erlang
{deps,
   [ {plugin_name, {git, "url_of_plugin", {tag, "tag_of_plugin"}}}
   , ....
   ....
   ]
}
```

3.  rebar.config 中 relx 段落添加：

<!-- end list -->

``` sourceCode erlang
{relx,
    [...
    , ...
    , {release, {emqx, git_describe},
       [
         {plugin_name, load},
       ]
      }
    ]
}
```





# 协议介绍 (Protocol)

## MQTT 协议

### 概览

MQTT 是一个轻量的发布订阅模式消息传输协议，专门针对低带宽和不稳定网络环境的物联网应用设计。

MQTT 官网: <http://mqtt.org>

MQTT V3.1.1 协议规范:
<http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html>

### 特点

1.  开放消息协议，简单易实现
2.  发布订阅模式，一对多消息发布
3.  基于 TCP/IP 网络连接
4.  1 字节固定报头，2 字节心跳报文，报文结构紧凑
5.  消息 QoS 支持，可靠传输保证

### 应用

MQTT 协议广泛应用于物联网、移动互联网、智能硬件、车联网、电力能源等领域。

1.  物联网 M2M 通信，物联网大数据采集
2.  Android 消息推送，WEB 消息推送
3.  移动即时消息，例如 Facebook Messenger
4.  智能硬件、智能家具、智能电器
5.  车联网通信，电动车站桩采集
6.  智慧城市、远程医疗、远程教育
7.  电力、石油与能源等行业市场

### MQTT 基于主题 (Topic) 消息路由

MQTT 协议基于主题 (Topic) 进行消息路由，主题 (Topic) 类似 URL 路径，例如:

    chat/room/1
    
    sensor/10/temperature
    
    sensor/+/temperature
    
    $SYS/broker/metrics/packets/received
    
    $SYS/broker/metrics/#

主题 (Topic) 通过 '/' 分割层级，支持 '+', '\#' 通配符:

    '+': 表示通配一个层级，例如 a/+，匹配 a/x, a/y
    
    '#': 表示通配多个层级，例如 a/#，匹配 a/x, a/b/c/d

订阅者与发布者之间通过主题路由消息进行通信，例如采用 mosquitto 命令行发布订阅消息:

    mosquitto_sub -t a/b/+ -q 1
    
    mosquitto_pub -t a/b/c -m hello -q 1

<div class="note">

<div class="admonition-title">

Note

</div>

订阅者可以订阅含通配符主题，但发布者不允许向含通配符主题发布消息。

</div>

### MQTT V3.1.1 协议报文

#### 报文结构

|                       |
| --------------------- |
| 固定报头 (Fixed header)    |
| 可变报头 (Variable header) |
| 报文有效载荷 (Payload)       |

#### 固定报头

<table style="width:82%;">
<colgroup>
<col style="width: 15%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 8%" />
</colgroup>
<tbody>
<tr class="odd">
<td>Bit</td>
<td><blockquote>
<p>7</p>
</blockquote></td>
<td><blockquote>
<p>6</p>
</blockquote></td>
<td><blockquote>
<p>5</p>
</blockquote></td>
<td><blockquote>
<p>4</p>
</blockquote></td>
<td><blockquote>
<p>3</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="even">
<td>byte1</td>
<td><blockquote>
<p>MQT</p>
</blockquote></td>
<td>T Pack</td>
<td>et typ</td>
<td>e</td>
<td></td>
<td><blockquote>
<p>Fla</p>
</blockquote></td>
<td>gs</td>
<td></td>
</tr>
<tr class="odd">
<td>byte2...</td>
<td><blockquote>
<p>Rem</p>
</blockquote></td>
<td>aining</td>
<td>Lengt</td>
<td>h</td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
</tbody>
</table>

#### 报文类型

|             |     |          |
| ----------- | --- | -------- |
| 类型名称        | 类型值 | 报文说明     |
| CONNECT     | 1   | 发起连接     |
| CONNACK     | 2   | 连接回执     |
| PUBLISH     | 3   | 发布消息     |
| PUBACK      | 4   | 发布回执     |
| PUBREC      | 5   | QoS2 消息回执 |
| PUBREL      | 6   | QoS2 消息释放 |
| PUBCOMP     | 7   | QoS2 消息完成 |
| SUBSCRIBE   | 8   | 订阅主题     |
| SUBACK      | 9   | 订阅回执     |
| UNSUBSCRIBE | 10  | 取消订阅     |
| UNSUBACK    | 11  | 取消订阅回执   |
| PINGREQ     | 12  | PING 请求   |
| PINGRESP    | 13  | PING 响应   |
| DISCONNECT  | 14  | 断开连接     |

#### PUBLISH 发布消息

PUBLISH 报文承载客户端与服务器间双向的发布消息。
PUBACK 报文用于接收端确认 QoS1 报文，PUBREC/PUBREL/PUBCOMP 报文用于 QoS2 消息流程。

#### PINGREQ/PINGRESP 心跳

客户端在无报文发送时，按保活周期 (KeepAlive) 定时向服务端发送 PINGREQ 心跳报文，服务端响应 PINGRESP 报文。PINGREQ/PINGRESP 报文均 2 个字节。

### MQTT 消息 QoS

MQTT 发布消息 QoS 保证不是端到端的，是客户端与服务器之间的。订阅者收到 MQTT 消息的 QoS 级别，最终取决于发布消息的 QoS 和主题订阅的 QoS。

<table style="width:67%;">
<colgroup>
<col style="width: 22%" />
<col style="width: 22%" />
<col style="width: 22%" />
</colgroup>
<tbody>
<tr class="odd">
<td > 发布消息的 QoS</td>
<td > 主题订阅的 QoS</td>
<td > 接收消息的 QoS</td>
</tr>
<tr class="even">
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
<td><blockquote>
<p>0</p>
</blockquote></td>
</tr>
<tr class="odd">
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
<td><blockquote>
<p>1</p>
</blockquote></td>
</tr>
<tr class="even">
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
<td><blockquote>
<p>2</p>
</blockquote></td>
</tr>
</tbody>
</table>

#### Qos0 消息发布订阅

![image](./_static/images/qos0_seq.png)

#### Qos1 消息发布订阅

![image](./_static/images/qos1_seq.png)

#### Qos2 消息发布订阅

![image](./_static/images/qos2_seq.png)

### MQTT 会话 (Clean Session)

MQTT 客户端向服务器发起 CONNECT 请求时，可以通过 'Clean Session' 标志设置会话。

'Clean Session' 设置为 0，表示创建一个持久会话，在客户端断开连接时，会话仍然保持并保存离线消息，直到会话超时注销。

'Clean
Session' 设置为 1，表示创建一个新的临时会话，在客户端断开时，会话自动销毁。

### MQTT 连接保活心跳

MQTT 客户端向服务器发起 CONNECT 请求时，通过 KeepAlive 参数设置保活周期。

客户端在无报文发送时，按 KeepAlive 周期定时发送 2 字节的 PINGREQ 心跳报文，服务端收到 PINGREQ 报文后，回复 2 字节的 PINGRESP 报文。

服务端在 1.5 个心跳周期内，既没有收到客户端发布订阅报文，也没有收到 PINGREQ 心跳报文时，主动心跳超时断开客户端 TCP 连接。

<div class="note">

<div class="admonition-title">

Note

</div>

emqttd 消息服务器默认按最长 2.5 心跳周期超时设计。

</div>

### MQTT 遗愿消息 (Last Will)

MQTT 客户端向服务器端 CONNECT 请求时，可以设置是否发送遗愿消息 (Will
Message) 标志，和遗愿消息主题 (Topic) 与内容 (Payload)。

MQTT 客户端异常下线时 (客户端断开前未向服务器发送 DISCONNECT 消息)，MQTT 消息服务器会发布遗愿消息。

### MQTT 保留消息 (Retained Message)

MQTT 客户端向服务器发布 (PUBLISH) 消息时，可以设置保留消息 (Retained Message) 标志。保留消息 (Retained
Message) 会驻留在消息服务器，后来的订阅者订阅主题时仍可以接收该消息。

例如 mosquitto 命令行发布一条保留消息到主题 'a/b/c':

    mosquitto_pub -r -q 1 -t a/b/c -m 'hello'

之后连接上来的 MQTT 客户端订阅主题 'a/b/c' 时候，仍可收到该消息:

    $ mosquitto_sub -t a/b/c -q 1
    hello

保留消息 (Retained
Message) 有两种清除方式:

1.  客户端向有保留消息的主题发布一个空消息:
    
        mosquitto_pub -r -q 1 -t a/b/c -m ''

2.  消息服务器设置保留消息的超期时间。

### MQTT WebSocket 连接

MQTT 协议除支持 TCP 传输层外，还支持 WebSocket 作为传输层。通过 WebSocket 浏览器可以直连 MQTT 消息服务器，发布订阅模式与其他 MQTT 客户端通信。

MQTT 协议的 WebSocket 连接，必须采用 binary 模式，并携带子协议 Header:

    Sec-WebSocket-Protocol: mqttv3.1 或 mqttv3.1.1

### MQTT 协议客户端库

#### emqtt 客户端库

emqtt 项目组: <https://github.com/emqtt>

|                                                 |                 |
| ----------------------------------------------- | --------------- |
| [emqttc](https://github.com/emqtt/emqttc)       | Erlang MQTT 客户端库 |
| [CocoaMQTT](https://github.com/emqtt/CocoaMQTT) | Swift 语言 MQTT 客户端库 |
| [QMQTT](https://github.com/emqtt/qmqtt)         | QT 框架 MQTT 客户端库    |

#### Eclipse Paho 客户端库

Paho 官网: <http://www.eclipse.org/paho/>

#### mqtt.org 官网客户端库

mqtt.org:
    <https://github.com/mqtt/mqtt.github.io/wiki/libraries>

### MQTT 与 XMPP 协议对比

MQTT 协议设计简单轻量、路由灵活，将在移动互联网物联网消息领域，全面取代 PC 时代的 XMPP 协议:

1.  MQTT 协议一个字节固定报头，两个字节心跳报文，报文体积小编解码容易。XMPP 协议基于繁重的 XML，报文体积大且交互繁琐。
2.  MQTT 协议基于主题 (Topic) 发布订阅模式消息路由，相比 XMPP 基于 JID 的点对点消息路由更为灵活。
3.  MQTT 协议未定义报文内容格式，可以承载 JSON、二进制等不同类型报文。XMPP 协议采用 XML 承载报文，二进制必须 Base64 编码等处理。
4.  MQTT 协议支持消息收发确认和 QoS 保证，XMPP 主协议并未定义类似机制。MQTT 协议有更好的消息可靠性保证。

## MQTT-SN 协议

MQTT-SN 协议是 MQTT 的直系亲属，它使用 UDP 进行通信，标准的端口是 1884。MQTT-SN
的主要目的是为了适应受限的设备和网络，比如一些传感器，只有很小的内存和
CPU，TCP 对于这些设备来说非常奢侈。还有一些网络，比如 ZIGBEE，报文的长度在 300 字节以下，无法承载太大的数据包。所以
MQTT-SN 的数据包更小巧。

MQTT-SN 的官方标准下载地址:
<http://mqtt.org/new/wp-content/uploads/2009/06/MQTT-SN_spec_v1.2.pdf>

### MQTT-SN 和 MQTT 的区别

MQTT-SN 的信令和 MQTT 大部分都相同，比如都有 Will, 都有 Connect/Subscribe/Publish 命令.

MQTT-SN 最大的不同是，Topic 使用 TopicId 来代替，而 TopicId 是一个 16 比特的数字。每一个数字对应一个
Topic, 设备和云端需要使用 REGISTER 命令映射 TopicId 和 Topic 的对应关系。

MQTT-SN 可以随时更改 Will 的内容，甚至可以取消。而 MQTT 只允许在 CONNECT 时设定 Will 的内容，
而且不允许更改.

MQTT-SN 的网络中有网关这种设备，它负责把 MQTT-SN 转换成 MQTT，和云端的 MQTT Broker 通信. MQTT-SN
的协议支持自动发现网关的功能。

MQTT-SN 还支持设备的睡眠功能，如果设备进入睡眠状态，无法接收 UDP 数据，网关将把下行的 PUBLISH
消息缓存起来，直到设备苏醒后再传送。

### EMQX-SN 网关插件

EMQX-SN 是 EMQ X 的一个网关插件，实现了 MQTT-SN 的大部分功能，它相当于一个在云端的 MQTT-SN 网关，直接和 EMQ
X Broker 相连。

#### 配置参数

File: etc/plugins/emqx\_sn.conf:

    mqtt.sn.port = 1884
    
    mqtt.sn.advertise_duration = 900
    
    mqtt.sn.gateway_id = 1
    
    mqtt.sn.username = mqtt_sn_user
    
    mqtt.sn.password = abc

|                             |                                    |
| --------------------------- | ---------------------------------- |
| mqtt.sn.port                | 指定 MQTT-SN 监听的端口号                  |
| mqtt.sn.advertise\_duration | ADVERTISE 消息的发送间隔 (秒)               |
| mqtt.sn.gateway\_id         | 网关 ID                              |
| mqtt.sn.username            | 这是可选的参数，指定所有 MQTT-SN 连接的用户名，用于鉴权模块 |
| mqtt.sn.password            | 这也是可选的参数，和 username 一起使用           |

#### 启动 emqx-sn

``` sourceCode 
./bin/emqx_ctl plugins load emqx_sn
```

### MQTT-SN 客户端库

1.  <https://github.com/eclipse/paho.mqtt-sn.embedded-c/>
2.  <https://github.com/ty4tw/MQTT-SN>
3.  <https://github.com/njh/mqtt-sn-tools>
4.  <https://github.com/arobenko/mqtt-sn>

## LWM2M 协议

LwM2M 全称是 Lightweight Machine-To-Machine，是由 Open Mobile Alliance (OMA)
定义的一套适用于物联网的轻量级协议，它提供了设备管理和通讯的功能，尤其适用于资源有限的终端设备。协议可以在
[这里](http://www.openmobilealliance.org/wp/) 下载。

LwM2M 基于 REST 架构，使用 CoAP 作为底层的传输协议，承载在 UDP 或者 SMS
上，因而报文结构简单小巧，并且在网络资源有限及无法确保设备始终在线的环境里同样适用。

![image](./_static/images/lwm2m_protocols.png)

LwM2M 最主要的实体包括 LwM2M Server 和 LwM2M Client。

LwM2M Server 作为服务器，部署在 M2M 服务供应商处或网络服务供应商处。LwM2M 定义了两种服务器

  - 一种是 LwM2M BOOTSTRAP SERVER，emqx-lwm2m 插件并未实现该服务器的功能。
  - 一种是 LwM2M SERVER，emqx-lwm2m 实现该服务器在 UDP 上的功能，SMS 并没有实现。

LwM2M Client 作为客户端，部署在各个 LwM2M 设备上。

在 LwM2M Server 和 LwM2M Client 之间，LwM2M 协议定义了 4 个接口。

1.  引导接口 Bootstrap：向 LwM2M 客户端提供注册到 LwM2M
    服务器的必要信息，例如服务器访问信息、客户端支持的资源信息等。
2.  客户端注册接口 Client Registration：使 LwM2M 客户端与 LwM2M 服务器互联，将 LwM2M
    客户端的相关信息存储在 LwM2M 服务器上。只有完成注册后，LwM2M
    客户端与服务器端之间的通信与管理才成为可能。
3.  设备管理与服务实现接口 Device Management and Service Enablement：该接口的主控方为 LwM2M
    服务器，服务器向客户端发送指令，客户端对指令做出回应并将回应消息发送给服务器。
4.  信息上报接口 Information Reporting：允许 LwM2M
    服务器端向客户端订阅资源信息，客户端接收订阅后按照约定的模式向服务器端报告自己的资源变化情况。

![image](./_static/images/lwm2m_arch.png)

LwM2M 把设备上的服务抽象为 Object 和 Resource, 在 XML 文件中定义各种 Object 的属性和功能。可以在
[这里](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html)
找到 XML 的各种定义。

LwM2M 协议预定义了 8 种 Object 来满足基本的需求，分别是：

  - Security 安全对象
  - Server 服务器对象
  - Access Control 访问控制对象
  - Device 设备对象
  - Connectivity Monitoring 连通性监控对象
  - Firmware 固件对象
  - Location 位置对象
  - Connectivity Statistics 连通性统计对象

### EMQX-LWM2M 插件

EMQX-LWM2M 是 EMQ X 服务器的一个网关插件，实现了 LwM2M 的大部分功能。MQTT 客户端可以通过 EMQX-LWM2M
访问支持 LwM2M 的设备。设备也可以往 EMQX-LWM2M 上报 notification，为 EMQ X 后端的服务采集数据。

### MQTT 和 LwM2M 的转换

从 MQTT 客户端可以发送 Command 给 LwM2M 设备。MQTT 到 LwM2M 的命令使用如下的 topic

``` sourceCode 
"lwm2m/{?device_end_point_name}/command".
```

其中 MQTT Payload 是一个 json 格式的字符串，指定要发送的命令，更多的细节请参见 emqx-lwm2m 的文档。

LwM2M 设备的回复用如下 topic 传送

``` sourceCode 
"lwm2m/{?device_end_point_name}/response".
```

MQTT Payload 也是一个 json 格式的字符串，更多的细节请参见 emqx-lwm2m 的文档。

#### 配置参数

File: etc/emqx\_lwm2m.conf:

    lwm2m.port = 5683
    
    lwm2m.certfile = etc/certs/cert.pem
    
    lwm2m.keyfile = etc/certs/key.pem
    
    lwm2m.xml_dir =  etc/lwm2m_xml

|                |                                                  |
| -------------- | ------------------------------------------------ |
| lwm2m.port     | 指定 LwM2M 监听的端口号，为了避免和 emqx-coap 冲突，使用了非标准的 5783 端口 |
| lwm2m.certfile | DTLS 使用的证书                                       |
| lwm2m.keyfile  | DTLS 使用的秘钥                                       |
| lwm2m.xml\_dir | 存放 XML 文件的目录，这些 XML 用来定义 LwM2M Object            |

#### 启动 emqx-lwm2m

``` sourceCode 
./bin/emqx_ctl plugins load emqx_lwm2m
```

### LwM2M 的客户端库

  - <https://github.com/eclipse/wakaama>
  - <https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit>
  - <https://github.com/AVSystem/Anjay>
  - <http://www.eclipse.org/leshan/>





# 管理监控 API (REST API)

用户可以通过 REST API 查询 MQTT
客户端连接 (Connections)、会话 (Sessions)、订阅 (Subscriptions) 和路由 (Routes) 信息，还可以检索和监控服务器的性能指标和统计数据。

## URL 地址

REST API 访问 URL 地址:

    http (s)://host:8081/api/v4/

## Basic 认证

使用 REST API 必须携带 HTTP Basic 格式的认证 (Authentication) 信息。因此，需要使用 Dashboard
中来创建的 AppID 和 AppSecret 进行认证:

``` sourceCode bash
# 例如：获取当前集群状态
curl -v --basic -u <appid>:<appsecret> -k http://localhost:8081/api/v4/brokers
```

## API 信息

### 获取当前 REST API 列表

API 定义:

    GET api/v4/

请求示例:

    GET api/v4/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "name": "list_clientid",
      "method": "GET",
      "path": "/auth_clientid",
      "descr": "List available clientid in the cluster"
    },
    {
      "name": "lookup_clientid",
      "method": "GET",
      "path": "/auth_clientid/:clientid",
      "descr": "Lookup clientid in the cluster"
    },
    {
      "name": "add_clientid",
      "method": "POST",
      "path": "/auth_clientid",
      "descr": "Add clientid in the cluster"
    },
    {
      "name": "update_clientid",
      "method": "PUT",
      "path": "/auth_clientid/:clientid",
      "descr": "Update clientid in the cluster"
    },
    {
      "name": "delete_clientid",
      "method": "DELETE",
      "path": "/auth_clientid/:clientid",
      "descr": "Delete clientid in the cluster"
    },
    {
      "name": "list_username",
      "method": "GET",
      "path": "/auth_username",
      "descr": "List available username in the cluster"
    },
    {
      "name": "lookup_username",
      "method": "GET",
      "path": "/auth_username/:username",
      "descr": "Lookup username in the cluster"
    },
    {
      "name": "add_username",
      "method": "POST",
      "path": "/auth_username",
      "descr": "Add username in the cluster"
    },
    {
      "name": "update_username",
      "method": "PUT",
      "path": "/auth_username/:username",
      "descr": "Update username in the cluster"
    },
    {
      "name": "delete_username",
      "method": "DELETE",
      "path": "/auth_username/:username",
      "descr": "Delete username in the cluster"
    },
    {
      "name": "auth_user",
      "method": "POST",
      "path": "/auth",
      "descr": "Authenticate an user"
    },
    {
      "name": "create_user",
      "method": "POST",
      "path": "/users/",
      "descr": "Create an user"
    },
    {
      "name": "list_users",
      "method": "GET",
      "path": "/users/",
      "descr": "List users"
    },
    {
      "name": "update_user",
      "method": "PUT",
      "path": "/users/:name",
      "descr": "Update an user"
    },
    {
      "name": "delete_user",
      "method": "DELETE",
      "path": "/users/:name",
      "descr": "Delete an user"
    },
    {
      "name": "change_pwd",
      "method": "PUT",
      "path": "/change_pwd/:username",
      "descr": "Change password for an user"
    },
    {
      "name": "list_all_alarms",
      "method": "GET",
      "path": "/alarms/present",
      "descr": "List all alarms"
    },
    {
      "name": "list_node_alarms",
      "method": "GET",
      "path": "/alarms/present/:node",
      "descr": "List alarms of a node"
    },
    {
      "name": "list_all_alarm_history",
      "method": "GET",
      "path": "/alarms/history",
      "descr": "List all alarm history"
    },
    {
      "name": "list_node_alarm_history",
      "method": "GET",
      "path": "/alarms/history/:node",
      "descr": "List alarm history of a node"
    },
    {
      "name": "add_app",
      "method": "POST",
      "path": "/apps/",
      "descr": "Add Application"
    },
    {
      "name": "del_app",
      "method": "DELETE",
      "path": "/apps/:appid",
      "descr": "Delete Application"
    },
    {
      "name": "list_apps",
      "method": "GET",
      "path": "/apps/",
      "descr": "List Applications"
    },
    {
      "name": "lookup_app",
      "method": "GET",
      "path": "/apps/:appid",
      "descr": "Lookup Application"
    },
    {
      "name": "update_app",
      "method": "PUT",
      "path": "/apps/:appid",
      "descr": "Update Application"
    },
    {
      "name": "list_banned",
      "method": "GET",
      "path": "/banned/",
      "descr": "List banned"
    },
    {
      "name": "create_banned",
      "method": "POST",
      "path": "/banned/",
      "descr": "Create banned"
    },
    {
      "name": "delete_banned",
      "method": "DELETE",
      "path": "/banned/:who",
      "descr": "Delete banned"
    },
    {
      "name": "list_brokers",
      "method": "GET",
      "path": "/brokers/",
      "descr": "A list of brokers in the cluster"
    },
    {
      "name": "get_broker",
      "method": "GET",
      "path": "/brokers/:node",
      "descr": "Get broker info of a node"
    },
    {
      "name": "list_clients",
      "method": "GET",
      "path": "/clients/",
      "descr": "A list of clients on current node"
    },
    {
      "name": "list_node_clients",
      "method": "GET",
      "path": "nodes/:node/clients/",
      "descr": "A list of clients on specified node"
    },
    {
      "name": "lookup_client",
      "method": "GET",
      "path": "/clients/:clientid",
      "descr": "Lookup a client in the cluster"
    },
    {
      "name": "lookup_node_client",
      "method": "GET",
      "path": "nodes/:node/clients/:clientid",
      "descr": "Lookup a client on the node"
    },
    {
      "name": "lookup_client_via_username",
      "method": "GET",
      "path": "/clients/username/:username",
      "descr": "Lookup a client via username in the cluster"
    },
    {
      "name": "lookup_node_client_via_username",
      "method": "GET",
      "path": "/nodes/:node/clients/username/:username",
      "descr": "Lookup a client via username on the node"
    },
    {
      "name": "kickout_client",
      "method": "DELETE",
      "path": "/clients/:clientid",
      "descr": "Kick out the client in the cluster"
    },
    {
      "name": "clean_acl_cache",
      "method": "DELETE",
      "path": "/clients/:clientid/acl_cache",
      "descr": "Clear the ACL cache of a specified client in the cluster"
    },
    {
      "name": "list_acl_cache",
      "method": "GET",
      "path": "/clients/:clientid/acl_cache",
      "descr": "List the ACL cache of a specified client in the cluster"
    },
    {
      "name": "list_listeners",
      "method": "GET",
      "path": "/listeners/",
      "descr": "A list of listeners in the cluster"
    },
    {
      "name": "list_node_listeners",
      "method": "GET",
      "path": "/nodes/:node/listeners",
      "descr": "A list of listeners on the node"
    },
    {
      "name": "list_all_metrics",
      "method": "GET",
      "path": "/metrics/",
      "descr": "A list of metrics of all nodes in the cluster"
    },
    {
      "name": "list_node_metrics",
      "method": "GET",
      "path": "/nodes/:node/metrics/",
      "descr": "A list of metrics of a node"
    },
    {
      "name": "list_nodes",
      "method": "GET",
      "path": "/nodes/",
      "descr": "A list of nodes in the cluster"
    },
    {
      "name": "get_node",
      "method": "GET",
      "path": "/nodes/:node",
      "descr": "Lookup a node in the cluster"
    },
    {
      "name": "list_all_plugins",
      "method": "GET",
      "path": "/plugins/",
      "descr": "List all plugins in the cluster"
    },
    {
      "name": "list_node_plugins",
      "method": "GET",
      "path": "/nodes/:node/plugins/",
      "descr": "List all plugins on a node"
    },
    {
      "name": "load_node_plugin",
      "method": "PUT",
      "path": "/nodes/:node/plugins/:plugin/load",
      "descr": "Load a plugin"
    },
    {
      "name": "unload_node_plugin",
      "method": "PUT",
      "path": "/nodes/:node/plugins/:plugin/unload",
      "descr": "Unload a plugin"
    },
    {
      "name": "reload_node_plugin",
      "method": "PUT",
      "path": "/nodes/:node/plugins/:plugin/reload",
      "descr": "Reload a plugin"
    },
    {
      "name": "unload_plugin",
      "method": "PUT",
      "path": "/plugins/:plugin/unload",
      "descr": "Unload a plugin in the cluster"
    },
    {
      "name": "reload_plugin",
      "method": "PUT",
      "path": "/plugins/:plugin/reload",
      "descr": "Reload a plugin in the cluster"
    },
    {
      "name": "mqtt_subscribe",
      "method": "POST",
      "path": "/mqtt/subscribe",
      "descr": "Subscribe a topic"
    },
    {
      "name": "mqtt_publish",
      "method": "POST",
      "path": "/mqtt/publish",
      "descr": "Publish a MQTT message"
    },
    {
      "name": "mqtt_unsubscribe",
      "method": "POST",
      "path": "/mqtt/unsubscribe",
      "descr": "Unsubscribe a topic"
    },
    {
      "name": "mqtt_subscribe_batch",
      "method": "POST",
      "path": "/mqtt/subscribe_batch",
      "descr": "Batch subscribes topics"
    },
    {
      "name": "mqtt_publish_batch",
      "method": "POST",
      "path": "/mqtt/publish_batch",
      "descr": "Batch publish MQTT messages"
    },
    {
      "name": "mqtt_unsubscribe_batch",
      "method": "POST",
      "path": "/mqtt/unsubscribe_batch",
      "descr": "Batch unsubscribes topics"
    },
    {
      "name": "list_routes",
      "method": "GET",
      "path": "/routes/",
      "descr": "List routes"
    },
    {
      "name": "lookup_routes",
      "method": "GET",
      "path": "/routes/:topic",
      "descr": "Lookup routes to a topic"
    },
    {
      "name": "list_stats",
      "method": "GET",
      "path": "/stats/",
      "descr": "A list of stats of all nodes in the cluster"
    },
    {
      "name": "lookup_node_stats",
      "method": "GET",
      "path": "/nodes/:node/stats/",
      "descr": "A list of stats of a node"
    },
    {
      "name": "list_subscriptions",
      "method": "GET",
      "path": "/subscriptions/",
      "descr": "A list of subscriptions in the cluster"
    },
    {
      "name": "list_node_subscriptions",
      "method": "GET",
      "path": "/nodes/:node/subscriptions/",
      "descr": "A list of subscriptions on a node"
    },
    {
      "name": "lookup_client_subscriptions",
      "method": "GET",
      "path": "/subscriptions/:clientid",
      "descr": "A list of subscriptions of a client"
    },
    {
      "name": "lookup_client_subscriptions_with_node",
      "method": "GET",
      "path": "/nodes/:node/subscriptions/:clientid",
      "descr": "A list of subscriptions of a client on the node"
    },
    {
      "name": "create_rule",
      "method": "POST",
      "path": "/rules/",
      "descr": "Create a rule"
    },
    {
      "name": "list_rules",
      "method": "GET",
      "path": "/rules/",
      "descr": "A list of all rules"
    },
    {
      "name": "show_rule",
      "method": "GET",
      "path": "/rules/:id",
      "descr": "Show a rule"
    },
    {
      "name": "delete_rule",
      "method": "DELETE",
      "path": "/rules/:id",
      "descr": "Delete a rule"
    },
    {
      "name": "list_actions",
      "method": "GET",
      "path": "/actions/",
      "descr": "A list of all actions"
    },
    {
      "name": "show_action",
      "method": "GET",
      "path": "/actions/:name",
      "descr": "Show an action"
    },
    {
      "name": "list_resources",
      "method": "GET",
      "path": "/resources/",
      "descr": "A list of all resources"
    },
    {
      "name": "create_resource",
      "method": "POST",
      "path": "/resources/",
      "descr": "Create a resource"
    },
    {
      "name": "show_resource",
      "method": "GET",
      "path": "/resources/:id",
      "descr": "Show a resource"
    },
    {
      "name": "get_resource_status",
      "method": "GET",
      "path": "/resource_status/:id",
      "descr": "Get status of a resource"
    },
    {
      "name": "start_resource",
      "method": "POST",
      "path": "/resources/:id",
      "descr": "Start a resource"
    },
    {
      "name": "delete_resource",
      "method": "DELETE",
      "path": "/resources/:id",
      "descr": "Delete a resource"
    },
    {
      "name": "list_resource_types",
      "method": "GET",
      "path": "/resource_types/",
      "descr": "List all resource types"
    },
    {
      "name": "show_resource_type",
      "method": "GET",
      "path": "/resource_types/:name",
      "descr": "Show a resource type"
    },
    {
      "name": "list_resources_by_type",
      "method": "GET",
      "path": "/resource_types/:type/resources",
      "descr": "List all resources of a resource type"
    },
    {
      "name": "list_events",
      "method": "GET",
      "path": "/rule_events/",
      "descr": "List all events with detailed info"
    }
  ]
}
```

## 集群与节点

### 获取集群基本信息

API 定义:

    GET api/v4/brokers/

请求示例:

    GET api/v4/brokers/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "datetime": "2019-12-18 10:56:41",
      "node": "emqx@127.0.0.1",
      "node_status": "Running",
      "otp_release": "R21/10.3.2",
      "sysdescr": "EMQ X Broker",
      "uptime": "3 minutes, 59 seconds",
      "version": "v4.0.0"
    }
  ]
}
```

### 获取节点基本信息

API 定义:

    GET api/v4/brokers/${node}

请求示例:

    GET api/v4/brokers/emqx@127.0.0.1

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": {
    "datetime": "2019-12-18 10:57:40",
    "node_status": "Running",
    "otp_release": "R21/10.3.2",
    "sysdescr": "EMQ X Broker",
    "uptime": "7 minutes, 16 seconds",
    "version": "v4.0.0"
  }
}
```

### 获取集群监控数据

API 定义:

    GET api/v4/nodes/

请求示例:

    GET api/v4/nodes/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "connections": 2,
      "load1": "2.75",
      "load15": "2.87",
      "load5": "2.57",
      "max_fds": 7168,
      "memory_total": "76.45M",
      "memory_used": "59.48M",
      "name": "emqx@127.0.0.1",
      "node": "emqx@127.0.0.1",
      "node_status": "Running",
      "otp_release": "R21/10.3.2",
      "process_available": 262144,
      "process_used": 331,
      "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
      "version": "v4.0.0"
    }
  ]
}
```

### 获取节点监控数据

API 定义:

    GET api/v4/nodes/${node}

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": {
    "connections": 1,
    "load1": "2.75",
    "load15": "2.87",
    "load5": "2.57",
    "max_fds": 7168,
    "memory_total": 80162816,
    "memory_used": 62254160,
    "name": "emqx@127.0.0.1",
    "node_status": "Running",
    "otp_release": "R21/10.3.2",
    "process_available": 262144,
    "process_used": 331,
    "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
    "version": "v4.0.0"
  }
}
```

## 客户端信息 (Clients)

### 获取集群客户端信息

API 定义:

    GET api/v4/clients

请求示例:

    GET api/v4/clients?_page=1&_limit=10000

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "username": "test",
      "recv_cnt": 2,
      "node": "emqx@127.0.0.1",
      "proto_name": "MQTT",
      "mqueue_len": 0,
      "mailbox_len": 1,
      "ip_address": "127.0.0.1",
      "awaiting_rel": 0,
      "max_mqueue": 1000,
      "send_msg": 0,
      "heap_size": 2586,
      "clientid": "mosquitto_mqtt",
      "created_at": "2019-12-18 10:27:24",
      "is_bridge": false,
      "proto_ver": 4,
      "expiry_interval": 0,
      "reductions": 4751,
      "max_subscriptions": 0,
      "recv_pkt": 1,
      "subscriptions_cnt": 0,
      "send_cnt": 0,
      "connected_at": "2019-12-18 10:27:24",
      "recv_msg": 0,
      "max_inflight": 32,
      "keepalive": 60,
      "max_awaiting_rel": 100,
      "mqueue_dropped": 0,
      "recv_oct": 21,
      "zone": "external",
      "inflight": 0,
      "connected": true,
      "port": 65273,
      "send_oct": 0,
      "send_pkt": 0,
      "clean_start": true
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 1
  }
}
```

### 获取节点客户端信息

API 定义:

    GET api/v4/nodes/${node}/clients

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/clients?_page=1&_limit=10000

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "username": "test",
      "recv_cnt": 10,
      "node": "emqx@127.0.0.1",
      "proto_name": "MQTT",
      "mqueue_len": 0,
      "mailbox_len": 0,
      "ip_address": "127.0.0.1",
      "awaiting_rel": 0,
      "max_mqueue": 1000,
      "send_msg": 0,
      "heap_size": 610,
      "clientid": "mosquitto_mqtt",
      "created_at": "2019-12-18 10:27:24",
      "is_bridge": false,
      "proto_ver": 4,
      "expiry_interval": 0,
      "reductions": 11292,
      "max_subscriptions": 0,
      "recv_pkt": 1,
      "subscriptions_cnt": 0,
      "send_cnt": 9,
      "connected_at": "2019-12-18 10:27:24",
      "recv_msg": 0,
      "max_inflight": 32,
      "keepalive": 60,
      "max_awaiting_rel": 100,
      "mqueue_dropped": 0,
      "recv_oct": 37,
      "zone": "external",
      "inflight": 0,
      "connected": true,
      "port": 65273,
      "send_oct": 20,
      "send_pkt": 9,
      "clean_start": true
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 1
  }
}
```

### 获取集群指定客户端信息

API 定义:

    GET api/v4/clients/${clientid}

请求示例:

    GET api/v4/clients/mosquitto_mqtt

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "username": "test",
      "recv_cnt": 38,
      "node": "emqx@127.0.0.1",
      "proto_name": "MQTT",
      "mqueue_len": 0,
      "mailbox_len": 0,
      "ip_address": "127.0.0.1",
      "awaiting_rel": 0,
      "max_mqueue": 1000,
      "send_msg": 0,
      "heap_size": 2586,
      "clientid": "mosquitto_mqtt",
      "created_at": "2019-12-18 10:27:24",
      "is_bridge": false,
      "proto_ver": 4,
      "expiry_interval": 0,
      "reductions": 32369,
      "max_subscriptions": 0,
      "recv_pkt": 1,
      "subscriptions_cnt": 0,
      "send_cnt": 37,
      "connected_at": "2019-12-18 10:27:24",
      "recv_msg": 0,
      "max_inflight": 32,
      "keepalive": 60,
      "max_awaiting_rel": 100,
      "mqueue_dropped": 0,
      "recv_oct": 93,
      "zone": "external",
      "inflight": 0,
      "connected": true,
      "port": 65273,
      "send_oct": 76,
      "send_pkt": 37,
      "clean_start": true
    }
  ]
}
```

### 获取节点指定客户端信息

API 定义:

    GET api/v4/nodes/${node}/clients/${clientid}

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/clients/mosquitto_mqtt

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "username": "test",
      "recv_cnt": 46,
      "node": "emqx@127.0.0.1",
      "proto_name": "MQTT",
      "mqueue_len": 0,
      "mailbox_len": 0,
      "ip_address": "127.0.0.1",
      "awaiting_rel": 0,
      "max_mqueue": 1000,
      "send_msg": 0,
      "heap_size": 1598,
      "clientid": "mosquitto_mqtt",
      "created_at": "2019-12-18 10:27:24",
      "is_bridge": false,
      "proto_ver": 4,
      "expiry_interval": 0,
      "reductions": 38422,
      "max_subscriptions": 0,
      "recv_pkt": 1,
      "subscriptions_cnt": 0,
      "send_cnt": 45,
      "connected_at": "2019-12-18 10:27:24",
      "recv_msg": 0,
      "max_inflight": 32,
      "keepalive": 60,
      "max_awaiting_rel": 100,
      "mqueue_dropped": 0,
      "recv_oct": 109,
      "zone": "external",
      "inflight": 0,
      "connected": true,
      "port": 65273,
      "send_oct": 92,
      "send_pkt": 45,
      "clean_start": true
    }
  ]
}
```

### 通过用户名获取集群指定客户端信息

API 定义:

    GET api/v4/clients/username/${username}

请求示例:

    GET api/v4/clients/username/test

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "username": "test",
      "recv_cnt": 2,
      "node": "emqx@127.0.0.1",
      "proto_name": "MQTT",
      "mqueue_len": 0,
      "mailbox_len": 0,
      "ip_address": "127.0.0.1",
      "awaiting_rel": 0,
      "max_mqueue": 1000,
      "send_msg": 0,
      "heap_size": 1598,
      "clientid": "mosquitto_mqtt",
      "created_at": "2019-12-18 11:21:08",
      "is_bridge": false,
      "proto_ver": 4,
      "expiry_interval": 0,
      "reductions": 5175,
      "max_subscriptions": 0,
      "recv_pkt": 1,
      "subscriptions_cnt": 0,
      "send_cnt": 1,
      "connected_at": "2019-12-18 11:21:08",
      "recv_msg": 0,
      "max_inflight": 32,
      "keepalive": 60,
      "max_awaiting_rel": 100,
      "mqueue_dropped": 0,
      "recv_oct": 36,
      "zone": "external",
      "inflight": 0,
      "connected": true,
      "port": 49816,
      "send_oct": 4,
      "send_pkt": 1,
      "clean_start": true
    }
  ]
}
```

### 通过用户名获取节点指定客户端信息

API 定义:

    GET api/v4/nodes/${nodes}/clients/username/${username}

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/clients/username/test

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "username": "test",
      "recv_cnt": 4,
      "node": "emqx@127.0.0.1",
      "proto_name": "MQTT",
      "mqueue_len": 0,
      "mailbox_len": 0,
      "ip_address": "127.0.0.1",
      "awaiting_rel": 0,
      "max_mqueue": 1000,
      "send_msg": 0,
      "heap_size": 1598,
      "clientid": "mosquitto_mqtt",
      "created_at": "2019-12-18 11:21:08",
      "is_bridge": false,
      "proto_ver": 4,
      "expiry_interval": 0,
      "reductions": 6741,
      "max_subscriptions": 0,
      "recv_pkt": 1,
      "subscriptions_cnt": 0,
      "send_cnt": 3,
      "connected_at": "2019-12-18 11:21:08",
      "recv_msg": 0,
      "max_inflight": 32,
      "keepalive": 60,
      "max_awaiting_rel": 100,
      "mqueue_dropped": 0,
      "recv_oct": 40,
      "zone": "external",
      "inflight": 0,
      "connected": true,
      "port": 49816,
      "send_oct": 8,
      "send_pkt": 3,
      "clean_start": true
    }
  ]
}
```

### 踢掉指定客户端

API 定义:

    DELETE api/v4/clients/${clientid}

请求示例:

    DELETE api/v4/clients/mosquitto_mqtt

返回数据:

``` sourceCode json
{
  "code": 0
}
```

### 获取指定客户端 ACL 缓存

API 定义:

    GET api/v4/clients/${clientid}/acl_cache

请求示例:

    GET api/v4/clients/mosquitto_mqtt/acl_cache

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "access": "publish",
      "result": "allow",
      "topic": "mosquitto_mqtt",
      "updated_time": 1576659345830
    }
  ]
}
```

### 清除指定客户端 ACL 缓存

API 定义:

    DELETE api/v4/clients/${clientid}/acl_cache

请求示例:

    DELETE api/v4/clients/mosquitto_mqtt/acl_cache

返回数据:

``` sourceCode json
{
  "code": 0
}
```

## 订阅 (Subscriptions)

### 获取集群订阅信息

API 定义:

    GET api/v4/subscriptions

请求示例:

    GET api/v4/subscriptions?_page=1&_limit=10000

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "clientid": "mqttjs_f79fbc5a4b",
      "node": "emqx@127.0.0.1",
      "qos": 0,
      "topic": "testtopic/#"
    },
    {
      "clientid": "mosquitto_mqtt",
      "node": "emqx@127.0.0.1",
      "qos": 0,
      "topic": "t"
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 2
  }
}
```

### 获取集群指定连接订阅信息

API 定义:

    GET api/v4/subscriptions/${clientid}

请求示例:

    GET api/v4/subscriptions/mosquitto_mqtt

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "clientid": "mosquitto_mqtt",
      "node": "emqx@127.0.0.1",
      "qos": 0,
      "topic": "t"
    }
  ]
}
```

### 获取节点订阅信息

API 定义:

    GET api/v4/nodes/${node}/subscriptions

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/subscriptions?_page=1&_limit=10000

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "clientid": "mqttjs_f79fbc5a4b",
      "node": "emqx@127.0.0.1",
      "qos": 0,
      "topic": "testtopic/#"
    },
    {
      "clientid": "mosquitto_mqtt",
      "node": "emqx@127.0.0.1",
      "qos": 0,
      "topic": "t"
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 2
  }
}
```

### 获取节点指定连接订阅信息

API 定义:

    GET api/v4/nodes/${node}/subscriptions/${clientid}

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/subscriptions/mosquitto_mqtt

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "clientid": "mosquitto_mqtt",
      "node": "emqx@127.0.0.1",
      "qos": 0,
      "topic": "t"
    }
  ]
}
```

## 路由 (Routes)

### 获取集群路由表

API 定义:

    GET api/v4/routes

请求示例:

    GET api/v4/routes

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "node": "emqx@127.0.0.1",
      "topic": "testtopic/#"
    },
    {
      "node": "emqx@127.0.0.1",
      "topic": "t"
    }
  ],
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 2
  }
}
```

### 获取集群指定主题的路由信息

API 定义:

    GET api/v4/routes/${topic}

请求示例:

    GET api/v4/routes/t

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "node": "emqx@127.0.0.1",
      "topic": "t"
    }
  ]
}
```

## 发布 / 订阅 / 取消订阅

### 发布消息

API 定义:

    POST api/v4/mqtt/publish

请求参数:

``` sourceCode json
{
  "topic": "test_topic",
  "payload": "hello",
  "qos": 1,
  "retain": false,
  "clientid": "mqttjs_ab9069449e"
}
```

请求示例:

    POST api/v4/mqtt/publish

返回数据:

``` sourceCode json
{
  "code": 0
}
```

### 创建订阅

API 定义:

    POST api/v4/mqtt/subscribe

请求参数:

``` sourceCode json
{
  "topic": "test_topic",
  "qos": 1,
  "clientid": "mqttjs_ab9069449e"
}
```

请求示例:

    POST api/v4/mqtt/subscribe

返回数据:

``` sourceCode json
{
  "code": 0
}
```

### 取消订阅

API 定义:

    POST api/v4/mqtt/unsubscribe

请求参数:

``` sourceCode json
{
  "topic": "test_topic",
  "clientid": "mqttjs_ab9069449e"
}
```

请求示例:

    POST api/v4/mqtt/unsubscribe

返回数据:

``` sourceCode json
{
  "code": 0
}
```

## 插件 (Plugins)

### 获取所有节点插件列表

API 定义:

    GET api/v4/plugins

请求示例:

    GET api/v4/plugins

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "node": "emqx@127.0.0.1",
      "plugins": [
        {
          "name": "emqx_auth_clientid",
          "version": "v4.0.0",
          "description": "EMQ X Authentication with ClientId/Password",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_http",
          "version": "v4.0.0",
          "description": "EMQ X Authentication/ACL with HTTP API",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_jwt",
          "version": "v4.0.0",
          "description": "EMQ X Authentication with JWT",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_ldap",
          "version": "v4.0.0",
          "description": "EMQ X Authentication/ACL with LDAP",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_mongo",
          "version": "v4.0.0",
          "description": "EMQ X Authentication/ACL with MongoDB",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_mysql",
          "version": "v4.0.0",
          "description": "EMQ X Authentication/ACL with MySQL",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_pgsql",
          "version": "v4.0.0",
          "description": "EMQ X Authentication/ACL with PostgreSQL",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_redis",
          "version": "v4.0.0",
          "description": "EMQ X Authentication/ACL with Redis",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_auth_username",
          "version": "v4.0.0",
          "description": "EMQ X Authentication with Username and Password",
          "active": false,
          "type": "auth"
        },
        {
          "name": "emqx_bridge_mqtt",
          "version": "v4.0.0",
          "description": "EMQ X Bridge to MQTT Broker",
          "active": false,
          "type": "bridge"
        },
        {
          "name": "emqx_coap",
          "version": "v4.0.0",
          "description": "EMQ X CoAP Gateway",
          "active": false,
          "type": "protocol"
        },
        {
          "name": "emqx_dashboard",
          "version": "v4.0.0",
          "description": "EMQ X Web Dashboard",
          "active": true,
          "type": "feature"
        },
        {
          "name": "emqx_delayed_publish",
          "version": "v4.0.0",
          "description": "EMQ X Delayed Publish",
          "active": false,
          "type": "feature"
        },
        {
          "name": "emqx_lua_hook",
          "version": "v4.0.0",
          "description": "EMQ X Lua Hooks",
          "active": false,
          "type": "feature"
        },
        {
          "name": "emqx_lwm2m",
          "version": "v4.0.0",
          "description": "EMQ X LwM2M Gateway",
          "active": false,
          "type": "protocol"
        },
        {
          "name": "emqx_management",
          "version": "v4.0.0",
          "description": "EMQ X Management API and CLI",
          "active": true,
          "type": "feature"
        },
        {
          "name": "emqx_psk_file",
          "version": "v4.0.0",
          "description": "EMQX PSK Plugin from File",
          "active": false,
          "type": "feature"
        },
        {
          "name": "emqx_recon",
          "version": "v4.0.0",
          "description": "EMQ X Recon Plugin",
          "active": true,
          "type": "feature"
        },
        {
          "name": "emqx_reloader",
          "version": "v4.0.0",
          "description": "EMQ X Reloader Plugin",
          "active": false,
          "type": "feature"
        },
        {
          "name": "emqx_retainer",
          "version": "v4.0.0",
          "description": "EMQ X Retainer",
          "active": true,
          "type": "feature"
        },
        {
          "name": "emqx_rule_engine",
          "version": "v4.0.0",
          "description": "EMQ X Rule Engine",
          "active": true,
          "type": "feature"
        },
        {
          "name": "emqx_sn",
          "version": "v4.0.0",
          "description": "EMQ X MQTT-SN Plugin",
          "active": false,
          "type": "protocol"
        },
        {
          "name": "emqx_statsd",
          "version": "v4.0.0",
          "description": "Statsd for EMQ X",
          "active": false,
          "type": "feature"
        },
        {
          "name": "emqx_stomp",
          "version": "v4.0.0",
          "description": "EMQ X Stomp Protocol Plugin",
          "active": false,
          "type": "protocol"
        },
        {
          "name": "emqx_web_hook",
          "version": "v4.0.0",
          "description": "EMQ X Webhook Plugin",
          "active": false,
          "type": "feature"
        }
      ]
    }
  ]
}
```

### 获取节点插件列表

API 定义:

    GET api/v4/nodes/${node}/plugins/

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/plugins/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "name": "emqx_auth_clientid",
      "version": "develop",
      "description": "EMQ X Authentication with ClientId/Password",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_http",
      "version": "develop",
      "description": "EMQ X Authentication/ACL with HTTP API",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_jwt",
      "version": "develop",
      "description": "EMQ X Authentication with JWT",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_ldap",
      "version": "develop",
      "description": "EMQ X Authentication/ACL with LDAP",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_mongo",
      "version": "develop",
      "description": "EMQ X Authentication/ACL with MongoDB",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_mysql",
      "version": "develop",
      "description": "EMQ X Authentication/ACL with MySQL",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_pgsql",
      "version": "develop",
      "description": "EMQ X Authentication/ACL with PostgreSQL",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_redis",
      "version": "develop",
      "description": "EMQ X Authentication/ACL with Redis",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_auth_username",
      "version": "develop",
      "description": "EMQ X Authentication with Username and Password",
      "active": false,
      "type": "auth"
    },
    {
      "name": "emqx_bridge_mqtt",
      "version": "develop",
      "description": "EMQ X Bridge to MQTT Broker",
      "active": false,
      "type": "bridge"
    },
    {
      "name": "emqx_coap",
      "version": "develop",
      "description": "EMQ X CoAP Gateway",
      "active": false,
      "type": "protocol"
    },
    {
      "name": "emqx_dashboard",
      "version": "develop",
      "description": "EMQ X Web Dashboard",
      "active": true,
      "type": "feature"
    },
    {
      "name": "emqx_delayed_publish",
      "version": "develop",
      "description": "EMQ X Delayed Publish",
      "active": false,
      "type": "feature"
    },
    {
      "name": "emqx_lua_hook",
      "version": "develop",
      "description": "EMQ X Lua Hooks",
      "active": false,
      "type": "feature"
    },
    {
      "name": "emqx_lwm2m",
      "version": "develop",
      "description": "EMQ X LwM2M Gateway",
      "active": false,
      "type": "protocol"
    },
    {
      "name": "emqx_management",
      "version": "develop",
      "description": "EMQ X Management API and CLI",
      "active": true,
      "type": "feature"
    },
    {
      "name": "emqx_psk_file",
      "version": "develop",
      "description": "EMQX PSK Plugin from File",
      "active": false,
      "type": "feature"
    },
    {
      "name": "emqx_recon",
      "version": "develop",
      "description": "EMQ X Recon Plugin",
      "active": true,
      "type": "feature"
    },
    {
      "name": "emqx_reloader",
      "version": "develop",
      "description": "EMQ X Reloader Plugin",
      "active": false,
      "type": "feature"
    },
    {
      "name": "emqx_retainer",
      "version": "develop",
      "description": "EMQ X Retainer",
      "active": true,
      "type": "feature"
    },
    {
      "name": "emqx_rule_engine",
      "version": "develop",
      "description": "EMQ X Rule Engine",
      "active": true,
      "type": "feature"
    },
    {
      "name": "emqx_sn",
      "version": "develop",
      "description": "EMQ X MQTT-SN Plugin",
      "active": false,
      "type": "protocol"
    },
    {
      "name": "emqx_statsd",
      "version": "develop",
      "description": "Statsd for EMQ X",
      "active": false,
      "type": "feature"
    },
    {
      "name": "emqx_stomp",
      "version": "develop",
      "description": "EMQ X Stomp Protocol Plugin",
      "active": false,
      "type": "protocol"
    },
    {
      "name": "emqx_web_hook",
      "version": "develop",
      "description": "EMQ X Webhook Plugin",
      "active": false,
      "type": "feature"
    }
  ]
}
```

### 启用节点指定插件

API 定义:

    PUT api/v4/nodes/${node}/plugins/${plugin}/load

请求示例:

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/load

返回数据:

``` sourceCode json
{
  "code": 0
}
```

### 关闭节点指定插件

API 定义:

    PUT api/v4/nodes/${node}/plugins/${plugin}/unload

请求示例:

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/unload

返回数据:

``` sourceCode json
{
  "code": 0
}
```

### 重启节点指定插件

API 定义:

    PUT api/v4/nodes/${node}/plugins/${plugin}/reload

请求示例:

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/reload

返回数据:

``` sourceCode json
{
  "code": 0
}
```

## 监听器 (Listeners)

### 获取集群监听器列表

API 定义:

    GET api/v4/listeners/

请求示例:

    GET api/v4/listeners/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "listeners": [
        {
          "acceptors": 16,
          "current_conns": 0,
          "listen_on": "8883",
          "max_conns": 102400,
          "protocol": "mqtt:ssl",
          "shutdown_count": [ ]
        },
        {
          "acceptors": 8,
          "current_conns": 2,
          "listen_on": "0.0.0.0:1883",
          "max_conns": 1024000,
          "protocol": "mqtt:tcp",
          "shutdown_count": {
            "closed": 2,
            "kicked": 1
          }
        },
        {
          "acceptors": 4,
          "current_conns": 0,
          "listen_on": "127.0.0.1:11883",
          "max_conns": 10240000,
          "protocol": "mqtt:tcp",
          "shutdown_count": [ ]
        },
        {
          "acceptors": 4,
          "current_conns": 1,
          "listen_on": "18083",
          "max_conns": 512,
          "protocol": "http:dashboard",
          "shutdown_count": [ ]
        },
        {
          "acceptors": 2,
          "current_conns": 0,
          "listen_on": "8081",
          "max_conns": 512,
          "protocol": "http:management",
          "shutdown_count": [ ]
        },
        {
          "acceptors": 4,
          "current_conns": 0,
          "listen_on": "8083",
          "max_conns": 102400,
          "protocol": "mqtt:ws",
          "shutdown_count": [ ]
        },
        {
          "acceptors": 4,
          "current_conns": 0,
          "listen_on": "8084",
          "max_conns": 16,
          "protocol": "mqtt:wss",
          "shutdown_count": [ ]
        }
      ],
      "node": "emqx@127.0.0.1"
    }
  ]
}
```

### 获取节点监听器列表

API 定义:

    GET api/v4/nodes/${node}/listeners

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/listeners

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "acceptors": 16,
      "current_conns": 0,
      "listen_on": "8883",
      "max_conns": 102400,
      "protocol": "mqtt:ssl",
      "shutdown_count": [ ]
    },
    {
      "acceptors": 8,
      "current_conns": 2,
      "listen_on": "0.0.0.0:1883",
      "max_conns": 1024000,
      "protocol": "mqtt:tcp",
      "shutdown_count": {
        "closed": 2,
        "kicked": 1
      }
    },
    {
      "acceptors": 4,
      "current_conns": 0,
      "listen_on": "127.0.0.1:11883",
      "max_conns": 10240000,
      "protocol": "mqtt:tcp",
      "shutdown_count": [ ]
    },
    {
      "acceptors": 4,
      "current_conns": 1,
      "listen_on": "18083",
      "max_conns": 512,
      "protocol": "http:dashboard",
      "shutdown_count": [ ]
    },
    {
      "acceptors": 2,
      "current_conns": 0,
      "listen_on": "8081",
      "max_conns": 512,
      "protocol": "http:management",
      "shutdown_count": [ ]
    },
    {
      "acceptors": 4,
      "current_conns": 0,
      "listen_on": "8083",
      "max_conns": 102400,
      "protocol": "mqtt:ws",
      "shutdown_count": [ ]
    },
    {
      "acceptors": 4,
      "current_conns": 0,
      "listen_on": "8084",
      "max_conns": 16,
      "protocol": "mqtt:wss",
      "shutdown_count": [ ]
    }
  ]
}
```

## 收发报文统计

### 获取集群收发报文统计

API 定义:

    GET api/v4/metrics/

请求示例:

    GET api/v4/metrics/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "node": "emqx@127.0.0.1",
      "metrics": {
        "auth.clientid.failure": 0,
        "rules.matched": 0,
        "messages.sent": 0,
        "packets.disconnect.sent": 0,
        "bytes.sent": 8,
        "packets.disconnect.received": 0,
        "packets.pingresp.sent": 0,
        "packets.pingreq.received": 0,
        "packets.unsubscribe.received": 0,
        "packets.pubcomp.missed": 0,
        "packets.puback.missed": 0,
        "packets.pubcomp.sent": 0,
        "packets.pubcomp.received": 0,
        "packets.pubrec.missed": 0,
        "auth.mqtt.anonymous": 2,
        "packets.connack.auth_error": 0,
        "packets.pubcomp.inuse": 0,
        "actions.failure": 0,
        "packets.pubrec.inuse": 0,
        "packets.suback.sent": 0,
        "packets.puback.sent": 0,
        "messages.retained": 0,
        "messages.received": 0,
        "packets.connect.received": 2,
        "messages.forward": 0,
        "packets.pubrel.missed": 0,
        "packets.publish.received": 0,
        "packets.connack.sent": 2,
        "auth.clientid.ignore": 2,
        "packets.subscribe.received": 0,
        "packets.pubrel.received": 0,
        "packets.pubrec.received": 0,
        "packets.puback.received": 0,
        "packets.sent": 2,
        "packets.received": 2,
        "bytes.received": 34,
        "messages.expired": 0,
        "messages.dropped": 0,
        "messages.qos2.dropped": 0,
        "messages.qos2.expired": 0,
        "packets.pubrel.sent": 0,
        "packets.pubrec.sent": 0,
        "packets.publish.sent": 0,
        "actions.success": 0,
        "channel.gc": 0,
        "packets.publish.error": 0,
        "packets.unsubscribe.error": 0,
        "messages.qos2.received": 0,
        "messages.qos1.received": 0,
        "messages.qos0.received": 0,
        "packets.auth.sent": 0,
        "messages.qos2.sent": 0,
        "messages.qos1.sent": 0,
        "messages.qos0.sent": 0,
        "packets.auth.received": 0,
        "packets.unsuback.sent": 0,
        "auth.clientid.success": 0,
        "packets.connack.error": 0,
        "packets.publish.auth_error": 0,
        "packets.subscribe.error": 0,
        "packets.subscribe.auth_error": 0
      }
    }
  ]
}
```

### 获取节点收发报文统计

API 定义:

    GET api/v4/nodes/${node}/metrics/

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/metrics/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": {
    "auth.clientid.failure": 0,
    "rules.matched": 0,
    "messages.sent": 0,
    "packets.disconnect.sent": 0,
    "bytes.sent": 52,
    "packets.disconnect.received": 0,
    "packets.pingresp.sent": 22,
    "packets.pingreq.received": 0,
    "packets.unsubscribe.received": 0,
    "packets.pubcomp.missed": 0,
    "packets.puback.missed": 0,
    "packets.pubcomp.sent": 0,
    "packets.pubcomp.received": 0,
    "packets.pubrec.missed": 0,
    "auth.mqtt.anonymous": 2,
    "packets.connack.auth_error": 0,
    "packets.pubcomp.inuse": 0,
    "actions.failure": 0,
    "packets.pubrec.inuse": 0,
    "packets.suback.sent": 0,
    "packets.puback.sent": 0,
    "messages.retained": 2,
    "messages.received": 0,
    "packets.connect.received": 2,
    "messages.forward": 0,
    "packets.pubrel.missed": 0,
    "packets.publish.received": 0,
    "packets.connack.sent": 2,
    "auth.clientid.ignore": 2,
    "packets.subscribe.received": 0,
    "packets.pubrel.received": 0,
    "packets.pubrec.received": 0,
    "packets.puback.received": 0,
    "packets.sent": 24,
    "packets.received": 2,
    "bytes.received": 78,
    "messages.expired": 0,
    "messages.dropped": 0,
    "messages.qos2.dropped": 0,
    "messages.qos2.expired": 0,
    "packets.pubrel.sent": 0,
    "packets.pubrec.sent": 0,
    "packets.publish.sent": 0,
    "actions.success": 0,
    "channel.gc": 0,
    "packets.publish.error": 0,
    "packets.unsubscribe.error": 0,
    "messages.qos2.received": 0,
    "messages.qos1.received": 0,
    "messages.qos0.received": 0,
    "packets.auth.sent": 0,
    "messages.qos2.sent": 0,
    "messages.qos1.sent": 0,
    "messages.qos0.sent": 0,
    "packets.auth.received": 0,
    "packets.unsuback.sent": 0,
    "auth.clientid.success": 0,
    "packets.connack.error": 0,
    "packets.publish.auth_error": 0,
    "packets.subscribe.error": 0,
    "packets.subscribe.auth_error": 0
  }
}
```

## 连接会话统计

### 获取集群连接会话统计

API 定义:

    GET api/v4/stats

请求示例:

    GET api/v4/stats

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "node": "emqx@127.0.0.1",
      "stats": {
        "subscriptions.shared.max": 0,
        "subscriptions.max": 0,
        "subscribers.max": 0,
        "resources.max": 0,
        "topics.count": 0,
        "channels.count": 2,
        "subscriptions.count": 0,
        "suboptions.max": 0,
        "topics.max": 0,
        "connections.max": 2,
        "actions.count": 5,
        "retained.count": 0,
        "rules.count": 0,
        "routes.count": 0,
        "subscriptions.shared.count": 0,
        "suboptions.count": 0,
        "sessions.count": 2,
        "channels.max": 2,
        "actions.max": 5,
        "retained.max": 0,
        "sessions.max": 2,
        "rules.max": 0,
        "routes.max": 0,
        "resources.count": 0,
        "subscribers.count": 0,
        "connections.count": 2
      }
    }
  ]
}
```

### 获取节点连接会话统计

API 定义:

    GET api/v4/nodes/${node}/stats

请求示例:

    GET api/v4/nodes/emqx@127.0.0.1/stats

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": {
    "subscriptions.shared.max": 0,
    "subscriptions.max": 0,
    "subscribers.max": 0,
    "resources.max": 0,
    "topics.count": 0,
    "channels.count": 2,
    "subscriptions.count": 0,
    "suboptions.max": 0,
    "topics.max": 0,
    "connections.max": 2,
    "actions.count": 5,
    "retained.count": 0,
    "rules.count": 0,
    "routes.count": 0,
    "subscriptions.shared.count": 0,
    "suboptions.count": 0,
    "sessions.count": 2,
    "channels.max": 2,
    "actions.max": 5,
    "retained.max": 0,
    "sessions.max": 2,
    "rules.max": 0,
    "routes.max": 0,
    "resources.count": 0,
    "subscribers.count": 0,
    "connections.count": 2
  }
}
```

## 告警信息

### 获取集群当前告警信息

API 定义:

    GET api/v4/alarms/present

请求示例:

    GET api/v4/alarms/present

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "alarms": [],
      "node": "emqx@127.0.0.1"
    }
  ]
}
```

### 获取节点当前告警信息

API 定义:

    GET api/v4/alarms/present/${node}

请求示例:

    GET api/v4/alarms/present/emqx@127.0.0.1

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": []
}
```

### 获取集群历史告警信息

API 定义:

    GET api/v4/alarms/history

请求示例:

    GET api/v4/alarms/history

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "alarms": [
        {
          "clear_at": "2019-07-10 16:54:35",
          "desc": "82.60344181007542",
          "id": "cpu_high_watermark"
        }
      ],
      "node": "emqx@127.0.0.1"
    }
  ]
}
```

### 获取节点历史告警信息

API 定义:

    GET api/v4/alarms/present/${node}

请求示例:

    GET api/v4/alarms/present/emqx@127.0.0.1

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
    {
      "clear_at": "2019-07-10 16:54:35",
      "desc": "82.60344181007542",
      "id": "cpu_high_watermark"
    }
  ]
}
```

## 黑名单

### 获取黑名单列表

API 定义:

    GET api/v4/banned

请求示例:

    GET api/v4/banned?_page=1&_limit=10000

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [
      {
          "as": "clientid",
          "at": 1576734915,
          "by": "user",
          "reason": "banned the clientId",
          "until": 1576735035,
          "who": "mqttjs_ab9069449e"
      }
  ],
  "meta": {
      "page": 1,
      "limit": 10000,
      "count": 1
  }
}
```

### 创建黑名单

API 定义:

    POST api/v4/banned

请求参数:

``` sourceCode json
{
  "who": "mqttjs_ab9069449e",
  "as": "clientid",
  "reason": "banned the clientId",
  "until": 1576735035
}
```

请求示例:

    POST api/v4/banned/

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": {
    "who": "mqttjs_ab9069449e",
    "as": "clientid",
    "reason": "banned the clientId",
    "until": 1576735035
  }
}
```

### 删除指定黑名单

API 定义:

    DELETE api/v4/banned/${as}/${who}

请求示例:

    DELETE api/v4/banned/clientid/mqttjs_ab9069449e

返回数据:

``` sourceCode json
{
  "code": 0
}
```

## 错误信息与数据分页

### HTTP 状态码大于 500 时响应携带错误信息返回

错误示例:

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_recon/load

返回数据:

``` sourceCode json
{
  "message": "already_started"
}
```

### 分页参数与分页信息

请求示例中使用了？\_page=1&\_limit=10000 参数的接口均支持分页:

    _page: 当前页码
    _limit: 分页大小

返回数据:

``` sourceCode json
{
  "code": 0,
  "data": [],
  "meta": {
    "page": 1,
    "limit": 10000,
    "count": 0
  }
}
```

## 规则引擎 (rule engine)

### 创建规则

API 定义:

    POST api/v4/rules

参数定义:

<table>
<colgroup>
<col style="width: 13%" />
<col style="width: 62%" />
<col style="width: 23%" />
</colgroup>
<tbody>
<tr class="odd">
<td>name</td>
<td>String，规则名字 </td>
<td></td>
</tr>
<tr class="even">
<td>for</td>
<td>String，Hook 的名字，可以为: &quot;message.publish&quot;，&quot;client.connected&quot; ... 详见 <span data-role="ref">plugins</span></td>
<td></td>
</tr>
<tr class="odd">
<td>rawsql</td>
<td>String，用于筛选和转换原始数据的 SQL 语句 </td>
<td></td>
</tr>
<tr class="even">
<td>actions</td>
<td>JSON Array，动作列表 </td>
<td></td>
</tr>
<tr class="odd">
<td><ul>
<li></li>
</ul></td>
<td>name</td>
<td>String, 动作名字 </td>
</tr>
<tr class="even">
<td><ul>
<li></li>
</ul></td>
<td>params</td>
<td>JSON Object, 动作参数 </td>
</tr>
<tr class="odd">
<td>description</td>
<td>String，可选，规则描述 </td>
<td></td>
</tr>
</tbody>
</table>

请求参数示例:

``` sourceCode json
{
  "name": "test-rule",
  "for": "message.publish",
  "rawsql": "select * from \"t/a\"",
  "actions": [{
      "name": "built_in:inspect_action",
      "params": {
          "a": 1
      }
  }],
  "description": "test-rule"
}
```

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": {
      "actions": [{
          "name": "built_in:inspect_action",
          "params": {
              "$resource": "built_in:test-resource",
              "a": 1
          }
      }],
      "description": "test-rule",
      "enabled": true,
      "for": "message.publish",
      "id": "test-rule:1556263150688255821",
      "name": "test-rule",
      "rawsql": "select * from \"t/a\""
  }
}
```

### 查询规则

API 定义:

    GET api/v4/rules/${rule_id}

请求参数示例:

    GET api/v4/rules/test-rule:1556263150688255821

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": {
      "actions": [{
          "name": "built_in:inspect_action",
          "params": {
              "$resource": "built_in:test-resource",
              "a": 1
          }
      }],
      "description": "test-rule",
      "enabled": true,
      "for": "message.publish",
      "id": "test-rule:1556263150688255821",
      "name": "test-rule",
      "rawsql": "select * from \"t/a\""
  }
}
```

### 获取当前规则列表

API 定义:

    GET api/v4/rules

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "actions": [{
          "name": "built_in:inspect_action",
          "params": {
              "$resource": "built_in:test-resource",
              "a": 1
          }
      }],
      "description": "test-rule",
      "enabled": true,
      "for": "message.publish",
      "id": "test-rule:1556263150688255821",
      "name": "test-rule",
      "rawsql": "select * from \"t/a\""
  }]
}
```

### 删除规则

API 定义:

    DELETE api/v4/rules/${rule_id}

请求参数示例:

    DELETE api/v4/rules/test-rule:1556263150688255821

返回数据示例:

``` sourceCode json
{
  "code": 0
}
```

### 获取当前动作列表

API 定义:

    GET api/v4/actions?for=${hook_type}

请求参示例:

    GET api/v4/actions

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "app": "emqx_rule_engine",
      "description": "Republish a MQTT message to a another topic",
      "for": "message.publish",
      "name": "built_in:republish_action",
      "params": {
          "target_topic": {
              "description": "Repubilsh the message to which topic",
              "format": "topic",
              "required": true,
              "title": "To Which Topic",
              "type": "string"
          }
      },
      "type": "built_in"
  }, {
      "app": "emqx_web_hook",
      "description": "Forward Events to Web Server",
      "for": "$events",
      "name": "web_hook:event_action",
      "params": {
          "$resource": {
              "description": "Bind a resource to this action",
              "required": true,
              "title": "Resource ID",
              "type": "string"
          },
          "template": {
              "description": "The payload template to be filled with variables before sending messages",
              "required": false,
              "schema": {},
              "title": "Payload Template",
              "type": "object"
          }
      },
      "type": "web_hook"
  }, {
      "app": "emqx_web_hook",
      "description": "Forward Messages to Web Server",
      "for": "message.publish",
      "name": "web_hook:publish_action",
      "params": {
          "$resource": {
              "description": "Bind a resource to this action",
              "required": true,
              "title": "Resource ID",
              "type": "string"
          }
      },
      "type": "web_hook"
  }, {
      "app": "emqx_rule_engine",
      "description": "Inspect the details of action params for debug purpose",
      "for": "$any",
      "name": "built_in:inspect_action",
      "params": {},
      "type": "built_in"
  }]
}
```

请求参数示例:

    GET api/v4/actions?for=client.connected

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "app": "emqx_rule_engine",
      "description": "Inspect the details of action params for debug purpose",
      "for": "$any",
      "name": "built_in:inspect_action",
      "params": {},
      "type": "built_in"
  }]
}
```

### 查询动作

API 定义:

    GET api/v4/actions/:action_name

请求参数示例:

    GET api/v4/actions/built_in:inspect_action

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": {
      "app": "emqx_rule_engine",
      "description": "Inspect the details of action params for debug purpose",
      "for": "$any",
      "name": "built_in:inspect_action",
      "params": {},
      "type": "built_in"
  }
}
```

### 获取当前资源类型列表

API 定义:

    GET api/v4/resource_types

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "attrs": "undefined",
      "config": {
          "url": "http://host-name/chats"
      },
      "description": "forward msgs to host-name/chats",
      "id": "web_hook:webhook1",
      "name": "webhook1",
      "type": "web_hook"
  }, {
      "attrs": "undefined",
      "config": {
          "a": 1
      },
      "description": "test-resource",
      "id": "built_in:test-resource",
      "name": "test-resource",
      "type": "built_in"
  }]
}
```

### 查询资源类型

API 定义:

    GET api/v4/resource_types/${type}

请求参数示例:

    GET api/v4/resource_types/built_in

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": {
      "description": "The built in resource type for debug purpose",
      "name": "built_in",
      "params": {},
      "provider": "emqx_rule_engine"
  }
}
```

### 获取某种类型的资源

API 定义:

    GET api/v4/resource_types/${type}/resources

请求参数示例:

    GET api/v4/resource_types/built_in/resources

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "attrs": "undefined",
      "config": {
          "a": 1
      },
      "description": "test-resource",
      "id": "built_in:test-resource",
      "name": "test-resource",
      "type": "built_in"
  }]
}
```

### 获取某种类型的动作

API 定义:

    GET api/v4/resource_types/${type}/actions

请求参数示例:

    GET api/v4/resource_types/built_in/actions

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "app": "emqx_rule_engine",
      "description": "Inspect the details of action params for debug purpose",
      "for": "$any",
      "name": "built_in:inspect_action",
      "params": {},
      "type": "built_in"
  }, {
      "app": "emqx_rule_engine",
      "description": "Republish a MQTT message to a another topic",
      "for": "message.publish",
      "name": "built_in:republish_action",
      "params": {
          "target_topic": {
              "description": "Repubilsh the message to which topic",
              "format": "topic",
              "required": true,
              "title": "To Which Topic",
              "type": "string"
          }
      },
      "type": "built_in"
  }]
}
```

### 创建资源

API 定义:

    POST api/v4/resources

参数定义:

|             |                   |
| ----------- | ----------------- |
| name        | String, 资源名字      |
| type        | String, 资源类型      |
| config      | JSON Object, 资源配置 |
| description | String，可选，规则描述    |

参数示例:

    {
      "name": "test-resource",
      "type": "built_in",
      "config": {
          "a": 1
      },
      "description": "test-resource"
    }

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": {
      "attrs": "undefined",
      "config": {
          "a": 1
      },
      "description": "test-resource",
      "id": "built_in:test-resource",
      "name": "test-resource",
      "type": "built_in"
  }
}
```

### 获取资源列表

API 定义:

    GET api/v4/resources

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": [{
      "attrs": "undefined",
      "config": {
          "url": "http://host-name/chats"
      },
      "description": "forward msgs to host-name/chats",
      "id": "web_hook:webhook1",
      "name": "webhook1",
      "type": "web_hook"
  }, {
      "attrs": "undefined",
      "config": {
          "a": 1
      },
      "description": "test-resource",
      "id": "built_in:test-resource",
      "name": "test-resource",
      "type": "built_in"
  }]
}
```

### 查询资源

API 定义:

    GET api/v4/resources/:resource_id

请求参数示例:

    GET api/v4/resources/built_in:test-resource

返回数据示例:

``` sourceCode json
{
  "code": 0,
  "data": {
      "attrs": "undefined",
      "config": {
          "a": 1
      },
      "description": "test-resource",
      "id": "built_in:test-resource",
      "name": "test-resource",
      "type": "built_in"
  }
}
```

### 删除资源

API 定义:

    DELETE api/v4/resources/:resource_id

请求参数示例:

    DELETE api/v4/resources/built_in:test-resource

返回数据示例:

``` sourceCode json
{
  "code": 0
}
```





# 测试调优 (Tuning Guide)

*EMQ X* 消息服务器 1.x 版本 MQTT 连接压力测试到 130 万，在一台 8 核心、32G 内存的 CentOS 服务器上。

100 万连接测试所需的 Linux 内核参数，网络协议栈参数，Erlang 虚拟机参数， *EMQ X* 消息服务器参数设置如下:

## Linux 操作系统参数

系统全局允许分配的最大文件句柄数:

    # 2 millions system-wide
    sysctl -w fs.file-max=2097152
    sysctl -w fs.nr_open=2097152
    echo 2097152 > /proc/sys/fs/nr_open

允许当前会话 / 进程打开文件句柄数:

    ulimit -n 1048576

### /etc/sysctl.conf

持久化 'fs.file-max' 设置到 /etc/sysctl.conf 文件:

    fs.file-max = 1048576

/etc/systemd/system.conf 设置服务最大文件句柄数:

    DefaultLimitNOFILE=1048576

### /etc/security/limits.conf

/etc/security/limits.conf 持久化设置允许用户 / 进程打开文件句柄数:

    *      soft   nofile      1048576
    *      hard   nofile      1048576

## TCP 协议栈网络参数

并发连接 backlog 设置:

    sysctl -w net.core.somaxconn=32768
    sysctl -w net.ipv4.tcp_max_syn_backlog=16384
    sysctl -w net.core.netdev_max_backlog=16384

可用知名端口范围:

    sysctl -w net.ipv4.ip_local_port_range='1000 65535'

TCP Socket 读写 Buffer 设置:

    sysctl -w net.core.rmem_default=262144
    sysctl -w net.core.wmem_default=262144
    sysctl -w net.core.rmem_max=16777216
    sysctl -w net.core.wmem_max=16777216
    sysctl -w net.core.optmem_max=16777216
    
    #sysctl -w net.ipv4.tcp_mem='16777216 16777216 16777216'
    sysctl -w net.ipv4.tcp_rmem='1024 4096 16777216'
    sysctl -w net.ipv4.tcp_wmem='1024 4096 16777216'

TCP 连接追踪设置:

    sysctl -w net.nf_conntrack_max=1000000
    sysctl -w net.netfilter.nf_conntrack_max=1000000
    sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30

TIME-WAIT Socket 最大数量、回收与重用设置:

    sysctl -w net.ipv4.tcp_max_tw_buckets=1048576
    
    # 注意：不建议开启該设置，NAT 模式下可能引起连接 RST
    # sysctl -w net.ipv4.tcp_tw_recycle=1
    # sysctl -w net.ipv4.tcp_tw_reuse=1

FIN-WAIT-2 Socket 超时设置:

    sysctl -w net.ipv4.tcp_fin_timeout=15

## Erlang 虚拟机参数

优化设置 Erlang 虚拟机启动参数，配置文件 emqx/etc/emqx.conf:

``` sourceCode properties
## Erlang Process Limit
node.process_limit = 2097152

## Sets the maximum number of simultaneously existing ports for this system
node.max_ports = 1048576
```

## EMQ X 消息服务器参数

设置 TCP 监听器的 Acceptor 池大小，最大允许连接数。配置文件 emqx/etc/emqx.conf:

``` sourceCode properties
## TCP Listener
listener.tcp.external = 0.0.0.0:1883
listener.tcp.external.acceptors = 64
listener.tcp.external.max_connections = 1024000
```

## 测试客户端设置

测试客户端服务器在一个接口上，最多只能创建 65000 连接:

    sysctl -w net.ipv4.ip_local_port_range="500 65535"
    echo 1000000 > /proc/sys/fs/nr_open
    ulimit -n 100000

### emqtt\_benchmark

并发连接测试工具: <http://github.com/emqtt/emqtt_benchmark>





# 版本升级 (Upgrade)

## 升级到 3.1 版本

<div class="note">

<div class="admonition-title">

Note

</div>

3.1 版本全新设计了项目架构、配置方式与插件管理方式。2.x 与 1.x 版本升级需要重新配置部署。

</div>

升级流程:

1.  下载解压 3.1 版本到新安装目录，例如 /opt/emqx-3.1/；
2.  参考旧版本 etc/vm.args、etc/emqttd.config 或 etc/emq.conf，配置 3.1 版本的
    etc/emqx.conf；
3.  重新配置插件 etc/plugins/${your-plugin}.conf；
4.  编辑插件加载文件 data/loaded\_plugins；
5.  停止旧版本，启动新版。

## 2.0 升级到 2.0.3 版本

升级流程:

1.  下载解压 2.0.3 版本到新安装目录，例如 /opt/emqttd-2.0.3/；
2.  旧版本的 'etc/' 配置文件、'data/' 数据文件覆盖到新版目录；
3.  停止旧版本，启动新版。

## 升级到 2.0 版本

<div class="note">

<div class="admonition-title">

Note

</div>

2.0 版本全新设计了项目架构、配置方式与插件管理方式。1.x 版本升级需要重新配置部署。

</div>

升级流程:

1.  下载解压 2.0 版本到新安装目录，例如 /opt/emqttd-2.0/
2.  参考旧版本 etc/vm.args、etc/emqttd.config，配置 2.0 版本的 etc/emq.conf
3.  重新配置插件 etc/plugins/${your-plugin}.conf
4.  编辑插件加载文件 data/loaded\_plugins
5.  停止旧版本，启动新版。

## 升级到 1.1.2 版本

<div class="note">

<div class="admonition-title">

Note

</div>

1.0 以后版本可平滑升级到 1.1.2

</div>

升级流程:

1.  下载解压 1.1.2 版本到新安装目录，例如 /opt/emqttd\_112；
2.  旧版本的 'etc/' 配置文件、'data/' 数据文件覆盖到新版目录；
3.  如果有加载插件，将旧版插件配置文件覆盖到新版；
4.  停止旧版本，启动新版。

# 迁移指南

下文提供了一套从 **EMQ X 3.x** 版本迁移到最新 **EMQ X 4.0**
版本的准则。尽管我们试图减少一些重大更改，但为了兼顾性能、简化使用方式，我们在几个地方进行了修改。

**EMQ X 3.x 版本迁移 EMQ X 4.0 要花多长时间？**

EMQ X 始终保证接入协议的规范性和持续更新，版本迁移时客户端部分
** 无需做任何调整 **，这意味着您无需停止设备功能、重新烧录设备程序固件。您仅需关注插件、配置项、命令行以及
REST API 的变更。

所需时长取决于您的项目规模和变更涉及范围，中小型的项目基本一天内就可以搞定。

## 核心

### client\_id 改为 clientid

在此处变量命名上我们做了较大的变动，EMQ X 内部所有 client\_id 字符都更改为 clientid，包括：

  - REST API 的 URL、请求 / 相应数据中的字段名称
  - 源代码中的命名规范
  - 命令行 CLI

## REST API

### v3 改为 v4

REST API 由 `http (s)://host:8081/api/v3/` 变更为
`http (s)://host:8081/api/v4/`。

### 连接 (connection) 改为客户端 (clients)

将 connections 概念改为 clients，涉及节点与集群相关的 API：

  - 获取集群连接列表：`GET /connections` -\> `/clients`
  - 获取集群指定连接信息：`GET /connections/:clientid` -\> `GET
    /connections/:clientid`
  - 获取节点连接列表：`GET /nodes/:node/connections` -\> `GET
    /nodes/:node/clients`
  - 获取节点指定连接信息：`GET /nodes/:node/connections/:clientid` -\> `GET
    /nodes/:node/clients/:clientid`
  - 请求 / 相应数据中的 client\_id 字段名称均变为 clientid

同时 API 返回内容有较大变动，变动部分详见 4.0 文档。

### 移除会话 (session) 相关的 API

4.0 中引入 Channel 概念，将会话 (session) 和客户端 (client) 合二为一，4.0 版本中以下 API 已被
** 移除 **：

  - 获取集群会话列表：`GET /sessions`
  - 获取集群指定客户端会话信息：`GET /sessions/:clientid`
  - 获取节点会话列表：`GET /nodes/:node/sessions`
  - 获取节点指定客户端会话信息：`GET /nodes/:node/sessions/:clientid`

4.0 以后如需获取会话相关信息，请使用客户端相关 API。

### 移除插件配置获取与更改 API

插件配置中可能包含敏感信息，同时插件配置不支持持久化为用户使用带来了很大疑惑。考虑到安全问题与实用性问题，我们 ** 移除 ** 了插件获取与更改
API。

  - 获取插件配置信息：`GET /nodes/:node/plugins/:plugin_name`
  - 更新插件配置：`PUT /nodes/:node/plugins/:plugin_name`

我们计划在 ** 企业版 ** 中通过安全规范及配置项本地存储提供解决以上问题，重新提供插件热配置相关的 API
以，\** 目前企业版本已经支持关键配置的热配置操作 *\*。

## Dashboard

### 连接 (connection) 改为客户端 (clients)

Dashboard 中 ** 连接 (connections)** 概念改为 ** 客户端 (clients)**，原连接信息可在现 ** 客户端
(clients)** 页面查看。

### 移除 ** 会话 (sessions)** 管理页面

Dashboard 中移除 ** 会话 (sessions)** 管理页面，相关信息整合到 ** 客户端 (clients)** 页面中。

### 规则引擎

规则引擎 SQL 语法有所变动，规则创建时 Dashboard 中不再提供 ** 事件 ** 下拉选择框，SQL 语法详细变更参照本文
** 规则引擎 ** 部分。

## 规则引擎

### SQL 语法变更

4.0 版本中规则引擎 SQL 语法更加易用，3.x 版本中所有事件 **FROM** 子句后面均需要指定事件名称，4.0 以后我们引入
** 事件主题 ** 概念，默认情况下 ** 消息发布 ** 事件不再需要指定事件名称：

``` sourceCode 
## 3.x 版本
## 需要指定事件名称进行处理
SELECT * FROM "message.publish" WHERE topic =~ 't/#'

## 4.0 及以后版本
## 默认处理 message.publish 事件，FROM 后面直接筛选 MQTT 主题
## 上述 SQL 语句等价于:
SELECT * FROM 't/#'

## 其他事件通过 事件主题 进行筛选
SELECT * FROM "$events/message_acked" where topic =~ 't/#'
SELECT * FROM "$events/client_connected"
```

Dashboard 中提供了旧版 SQL 语法转换功能可以完成 SQL 升级迁移。

### 事件名称变更

4.0 版本中 ** 订阅 / 取消订阅 ** 主体变为 ** 会话 (session)**，\** 事件 *\* 在转换为 ** 事件主题 **
时，需要注意以下变更：

  - ** 终端订阅 ** 变更为 ** 会话订阅 **：`client.subscribe` -\>
    `$events/session_subscribe`
  - ** 终端取消订阅 ** 变更为 ** 会话取消订阅 **：`client.unsubscribe` -\>
    `$events/session_unsubscribe`




