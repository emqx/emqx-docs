# v0.1-v3.x 版本

## 3.2.7

*发布日期: 2019-12-03*

EMQX 3.2.7 版本发布。此版本主要重新支持了通过配置文件配置默认的 `username` 和 `clientid` 。

### emqx-auth-username (plugin)

功能增强:

- 重新支持了通过配置文件配置默认的 `username`

Github PR: [emqx/emqx-auth-username#127](https://github.com/emqx/emqx-auth-username/pull/127)

### emqx-auth-clientid (plugin)

功能增强:

- 重新支持了通过配置文件配置默认的 `clientid`

Github PR: [emqx/emqx-auth-clientid#123](https://github.com/emqx/emqx-auth-clientid/pull/123)

## 3.2.6

*发布日期: 2019-11-23*

EMQX 3.2.6 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

错误修复:

- 修复通过 `gen_rpc` 向远程节点转发消息时可能失序的问题

Github PR: [emqx/emqx#3049](https://github.com/emqx/emqx/pull/3049)

- 修复认证插件崩溃会导致 `emqx`  崩溃的问题

Github PR: [emqx/emqx#3048](https://github.com/emqx/emqx/pull/3048)

## 3.2.5

*发布日期: 2019-11-15*

EMQX 3.2.5 版本发布。此版本主要进行了错误修复。

### emqx-rule-engine (plugin)

错误修复:

- 支持 SQL 关键字: FOREACH/DO/INCASE

Github Commit: [emqx/emqx-rule-engine#a962e3](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

- 支持 SQL 关键字: CASE/WHEN

Github Commit: [emqx/emqx-rule-engine#40e68e](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

- 支持在 SQL 的 WHERE 子句中比较原子与二进制

Github Commit: [emqx/emqx-rule-engine#b240cc](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

- 修复 select 和 foreach 中的列验证失败

Github Commit: [emqx/emqx-rule-engine#6a1267](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

- 修复重建规则时出现竞争的问题

Github Commit: [emqx/emqx-rule-engine#af8967](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

- 修复重发消息时没有确证设置标志的问题

Github Commit: [emqx/emqx-rule-engine#60e45c](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)

### minirest (plugin)

错误修复:

- 修复日志没有记录错误数据的问题

Github PR: [emqx/minirest#20](https://github.com/emqx/minirest/pull/20)

### emqx-web-hook (plugin)

错误修复:

- 修复错误的匹配

Github Commit: [emqx/emqx-web-hook#3dd041](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)

## 3.2.4

*发布日期: 2019-10-28*

EMQX 3.2.4 版本发布。此版本主要为 Dashbaord 和 REST API 添加了 IPv6 支持，并修复了一些错误。

错误修复:

- 修复 max_subscriptions 配置不生效的问题

Github PR: [emqx/emqx#2922](https://github.com/emqx/emqx/pull/2922)

Github Issue: [emqx/emqx#2908](https://github.com/emqx/emqx/issues/2908)

### emqx-auth-mysql (plugin)

错误修复:

- 使用占位符时更安全地取值

Github PR: [emqx/emqx-auth-mysql#180](https://github.com/emqx/emqx-auth-mysql/pull/180)

Github Issue: [emqx/emqx#2937](https://github.com/emqx/emqx/issues/2937)

### emqx-dashboard (plugin)

功能增强:

- 支持使用 IPv6 访问 Dashbaord

Github PR: [emqx/emqx-dashboard#161](https://github.com/emqx/emqx-dashboard/pull/161)

### emqx-management (plugin)

功能增强:

- REST API 支持 IPv6

Github PR: [emqx/emqx-management#134](https://github.com/emqx/emqx-management/pull/134)

### emqx-delay-publish (plugin)

错误修复:

- 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献

Github PR: [emqx/emqx-delay-publish#48](https://github.com/emqx/emqx-delay-publish/pull/48)

Github Issue: [emqx/emqx-delay-publish#15](https://github.com/emqx/emqx-delay-publish/issues/15)

### emqx-rule-engine (plugin)

功能增强:

- 优化规则引擎中 JSON Payload 解析语句

Github Repository: [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

## 3.2.3

*发布日期: 2019-09-16*

EMQX 3.2.3 版本改动主要为错误修复。

错误修复:

- 修复 emqx 容器运行时 CPU 占用率告警异常的问题

GitHub Commit: [emqx/emqx#9cdaa7](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

- 修复消息过期机制不生效的问题

Github Commit: [emqx/emqx#31671f](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

- 修复占位符在 mountpoint 中不生效的问题

Github Commit: [emqx/emqx#58ba22](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)

### emqx-dashboard (plugin)

错误修复:

- 修复 SSL 无法使用的问题

Github Commit: [emqx/emqx-dashboard#272a42](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)

## 3.2.2

*发布日期: 2019-08-03*

EMQX 3.2.2 版本改动主要为错误修复。

功能增强:

- 扩展 `gen_rpc` 配置

Github PR: [emqx/emqx#2732](https://github.com/emqx/emqx/pull/2732)

### emqx-rule-engine (plugin)

错误修复:

- 修复测试 URL 连通性的问题

Github PR: [emqx/emqx-rule-engine#88](https://github.com/emqx/emqx-rule-engine/pull/88)

### emqx-dashboard (plugin)

功能增强:

- 增加帮助页面

### ekka (dependency)

错误修复:

- 修复释放锁可能导致崩溃的问题

Github PR: [emqx/ekka#60](https://github.com/emqx/ekka/pull/60)

## 3.2.1

*发布日期: 2019-07-20*

EMQX 3.2.1 版本改动主要包括错误修复与性能增强。

功能增强:

- 优化 `gen_rpc` 的调用

Github PR: [emqx/emqx#2694](https://github.com/emqx/emqx/pull/2694)

- 支持使用 hostname 自动发现 k8s 集群

Github PR: [emqx/emqx#2699](https://github.com/emqx/emqx/pull/2699)

- 将默认 uptime 心跳时间改为 30s

Github PR: [emqx/emqx#2696](https://github.com/emqx/emqx/pull/2696)

错误修复:

- 修复 WebSocket 非正常下线时出现 crash 的问题

Github PR: [emqx/emqx#2697](https://github.com/emqx/emqx/pull/2697)

- 修复 Session 异常关闭时，ws_channel 仍然在线的问题

Github PR: [emqx/emqx#2704](https://github.com/emqx/emqx/pull/2704)

### emqx-rule-engine (plugin)

功能增强:

- 增强 republish 动作参数

Github PR: [emqx/emqx-rule-engine#81](https://github.com/emqx/emqx-rule-engine/pull/81)

错误修复:

- 修复使用 '.' 筛选 payload 字段失败的问题

Github PR: [emqx/emqx-rule-engine#83](https://github.com/emqx/emqx-rule-engine/pull/83)

### emqx-dashboard (plugin)

错误修复:

- 修复 Dashboard 资源列表在 Safari 下渲染错误的问题

Github PR: [emqx/emqx-dashboard#124 ](https://github.com/emqx/emqx-dashboard/pull/124) , [ emqx/emqx-dashboard#125 ](https://github.com/emqx/emqx-dashboard/pull/125) , [ emqx/emqx-dashboard#126](https://github.com/emqx/emqx-dashboard/pull/126)

### emqx-lwm2m (plugin)

功能增强:

- 兼容 LwM2M 1.1 版本客户端登录

Github Commit: [emqx/emqx-lwm2m#1c03bf](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)

### emqx-rel (build project)

功能增强:

- 内置 rebar3 脚本

Github PR: [emqx/emqx-rel#394](https://github.com/emqx/emqx-rel/pull/394)

- EMQX Windows 服务延迟启动

Github PR: [emqx/emqx-rel#395](https://github.com/emqx/emqx-rel/pull/395)

## 3.2.0

*发布日期: 2019-07-12*

EMQX 3.2.0 版本主要优化和改进了规则引擎。

### 规则引擎

改进规则引擎功能和规则管理界面(Dashboard)，支持更多动作。

### 项目构建

改用 rebar3 构建项目。

### MQTT Broker 桥接

将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件，并提升了 RPC bridge 的性能。

### HTTP 插件

支持 HTTPs。

### 集群 (ekka)

改善集群稳定性。

### 其他插件和依赖

修复 Windows 服务注册问题。

## 3.1.2

*发布日期: 2019-06-06*

EMQX 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

### EMQX Core

Bug fixes:

- 修复 [emqx/emqx: issue #2595](https://github.com/emqx/emqx/issues/2595)

Github PR: [emqx/emqx#2601](https://github.com/emqx/emqx/pull/2601)

- 修复无法设置日志等级的问题

Github PR: [emqx/emqx#2600](https://github.com/emqx/emqx/pull/2600)

- 修复返回值不匹配的问题

Github PR: [emqx/emqx#2560](https://github.com/emqx/emqx/pull/2560)

- 热修复 `emqx_sn` 与 `emqx_coap` 插件

Github PR: [emqx/emqx#2556](https://github.com/emqx/emqx/pull/2556)

### emqx-coap (plugin)

错误修复:

- 修复无法发布消息的问题

Github PR: [emqx/emqx-coap#120](https://github.com/emqx/emqx-coap/pull/120)

### ekka (deps)

错误修复:

- 修复导致 `emqx_sm_locker` 崩溃的问题

Github PR: [emqx/ekka#54](https://github.com/emqx/ekka/pull/54)

- 修复 k8s 无法使用 dns 集群的问题

Github PR: [emqx/ekka#53](https://github.com/emqx/ekka/pull/53)

- 修复 etcd 集群不可用的问题

Github PR: [emqx/ekka#52](https://github.com/emqx/ekka/pull/52)

## 3.1.1

*发布日期: 2019-05-10*

EMQX 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

功能增强:

- 增大单条日志可打印的最大字符数量

Github PR: [emqx/emqx#2509](https://github.com/emqx/emqx/pull/2509)

- `force_shutdown_policy` 将根据系统位数使用不同的默认值

Github PR: [emqx/emqx#2515](https://github.com/emqx/emqx/pull/2515)

错误修复:

- 正确地配置和使用 `long_gc` 与 `long_schedule`

Github PR: [emqx/emqx#2504 ](https://github.com/emqx/emqx/pull/2504) , [ emqx/emqx#2513](https://github.com/emqx/emqx/pull/2513)

- 修复没有更新 `suboptions/count` 的问题

Github PR: [emqx/emqx#2507](https://github.com/emqx/emqx/pull/2507)

### emqx-lwm2m (plugin)

错误修复:

- 修复 mountpoint 没有生效的问题

Github PR: [emqx/emqx-lwm2m#34](https://github.com/emqx/emqx-lwm2m/pull/34)

- 修复消息无法被 `emqx-web-hook` 转发的问题

Github PR: [emqx/emqx-lwm2m#35](https://github.com/emqx/emqx-lwm2m/pull/35)

## 3.1.0

*发布日期: 2019-04-26*

EMQX 3.1.0 版本发布。此版本改动主要包括全面支持规则引擎、引入 storm 模块以支持 edge storm、 重构 flapping 代码。

功能改进:

- 添加 emqx_ct_helpers 依赖，并重构测试用例

Github PR: [emqx/emqx#2480](https://github.com/emqx/emqx/pull/2480)

- 重构 flapping 代码

Github PR: [emqx/emqx#2476](https://github.com/emqx/emqx/pull/2476)

### emqx-management (plugin)

问题修复:

- 修复 listeners acceptors 的值没有正确获取的问题

Github PR: [emqx/emqx-management#76](https://github.com/emqx/emqx-management/pull/76)

### emqx-rule-engine (plugin)

功能改进:

- 支持规则动作参数的验证

Github PR: [emqx/emqx-rule-engine#b28318](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

- 删除资源时检查是否存在依赖

Github PR: [emqx/emqx-rule-engine#fa75b9](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

- 从 republish 动作中移除 `from` 参数

Github PR: [emqx/emqx-rule-engine#8721eb](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

- 修复了 SQL where 子句不能处理整数的问题

Github PR: [emqx/emqx-rule-engine#c9c761](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)

### emqx-storm (plugin)

功能改进:

- 支持 edge storm

Github Repository: [emqx/emqx-storm](https://github.com/emqx/emqx-storm)

## 3.0.1

*发布日期: 2019-01-25*

EMQX 3.0.1 版本发布。此版本主要包含功能改进和错误修复。

功能改进:

- 为 emqx edge 增加 +L 虚拟机参数以减少内存

Github PR: [emqx/emqx#2110](https://github.com/emqx/emqx/pull/2110)

- 简化修改日志输出等级的命令

Github PR: [emqx/emqx#2115](https://github.com/emqx/emqx/pull/2115)

- 重构 bridge 代码; 支持 bridge 消息持久化

Github PR: [emqx/emqx#2160 ](https://github.com/emqx/emqx/pull/2160) , [ emqx/emqx#2117 ](https://github.com/emqx/emqx/pull/2117) , [ emqx/emqx#2113 ](https://github.com/emqx/emqx/pull/2113) , [ emqx/emqx#2108 ](https://github.com/emqx/emqx/pull/2108) , [ emqx/emqx#2053](https://github.com/emqx/emqx/pull/2053)

- 优化路由匹配

Github PR: [emqx/emqx#2124](https://github.com/emqx/emqx/pull/2124)

- 改进 'emqx_client' 模块设计

Github PR: [emqx/emqx#2137](https://github.com/emqx/emqx/pull/2137)

- 改进 'emqx_pool' 模块的设计

Github PR: [emqx/emqx#2138](https://github.com/emqx/emqx/pull/2138)

- 改进共享订阅调度实现

Github PR: [emqx/emqx#2144](https://github.com/emqx/emqx/pull/2144)

- 支持重启 emqx 时重新生成配置

Github PR: [emqx/emqx#2175](https://github.com/emqx/emqx/pull/2175)

问题修复:

- 修复对端关闭连接时崩溃的问题

Github PR: [emqx/emqx#2074](https://github.com/emqx/emqx/pull/2074)

- 修复客户端正常断开连接时依旧发送遗嘱消息的问题

Github PR: [emqx/emqx#2156](https://github.com/emqx/emqx/pull/2156)

### emqx-lwm2m

问题修复:

- 移除认证功能

GitHub PR: [emqx/emqx-lwm2m#14](https://github.com/emqx/emqx-lwm2m/pull/14)

### emqx-auth-username

问题修复:

- 支持可选的加密模式

GitHub PR: [emqx/emqx-auth-usernmae#64](https://github.com/emqx/emqx-auth-username/pull/64)

### emqx-auth-clientid

功能改进:

- 支持可选的加密模式

GitHub PR: [emqx/emqx-auth-clientid#52](https://github.com/emqx/emqx-auth-username/pull/52)

### emqx-management

功能改进:

- 增加 'plugins reload \<Name>' CLI 命令，支持重载插件时重新生成配置

Github PR: [emqx/emqx-management#30](https://github.com/emqx/emqx-management/pull/30)

## 3.0.0

*发布日期: 2018-12-22*

EMQX 3.0.0 版本，重新设计了订阅的 ETS 表，通过重构模块和调节 erlang 虚拟机参数提升了 EMQ 性能

功能改进:

- 将虚拟机参数移动到单独的 vm.args 文件

Github PR: [emqx/emqx#2033 ](https://github.com/emqx/emqx/pull/2033) , [ emqx/emqx#2057 ](https://github.com/emqx/emqx/pull/2057) , [ emqx/emqx#2070](https://github.com/emqx/emqx/pull/2070)

- 为遗嘱消息主题增加格式校验和 ACL 检查

Github PR: [emqx/emqx#2075](https://github.com/emqx/emqx/pull/2075)

- 增加 ACL 检查返回拒绝时是否断开客户端连接的配置选项

Github PR: [emqx/emqx#2059](https://github.com/emqx/emqx/pull/2059)

- 重构 session 监控树

Github PR: [emqx/emqx#2077](https://github.com/emqx/emqx/pull/2077)

- 增加 'active_n' 选项以优化 emqx_connection 的 CPU 占用率

Github PR: [emqx/emqx#2060](https://github.com/emqx/emqx/pull/2060)

- 支持客户端批量下线

Github PR: [emqx/emqx#2060](https://github.com/emqx/emqx/pull/2060)

- 增加订阅表分片机制

Github PR: [emqx/emqx#2044](https://github.com/emqx/emqx/pull/2044)

- 重构 'emqx_gc' 模块

Github PR: [emqx/emqx#2090](https://github.com/emqx/emqx/pull/2090)

问题修复:

- 修复 Topic Alias Maximum 的错误实现

Github PR: [emqx/emqx#2074](https://github.com/emqx/emqx/pull/2074)

- 修复部分情况下不会发送遗嘱消息的错误

Github PR: [emqx/emqx#2068](https://github.com/emqx/emqx/pull/2068)

### emqx-auth-ldap

功能改进:

- 更好的设计

GitHub PR: [emqx/emqx-auth-ldap#46](https://github.com/emqx/emqx-auth-ldap/pull/46)

### emqx-lua-hook

问题修复:

- 修复测试用例

GitHub PR: [emqx/emqx-lua-hook#45](https://github.com/emqx/emqx-lua-hook/pull/45)

### emqx-management

功能改进:

- 为 REST API 增加测试用例，并规范返回的响应格式

Github PR: [emqx/emqx-management#21](https://github.com/emqx/emqx-management/pull/21)

## 2.3.11

*发布日期: 2018-07-23*

### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

### emq-auth-jwt

Fix token verification with jwerl 1.0.0

### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo#123)

Support both clientid and username variables in all queries. (emq-auth-mongo#123)

## 2.3.10

*发布日期: 2018-06-27*

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9

*发布日期: 2018-05-20*

### Bugfix and Enhancements

Bugfix: check params for REST publish API (#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (#78)

## 2.3.8

*发布日期: 2018-05-11*

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq_auth_username (#1588)

Bugfix: Should be an info level when change CleanSession (#1590)

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (#116)

## 2.3.7

*发布日期: 2018-04-22*

### Bugfix and Enhancements

Bugfix: fixed spec of function setstats/3 (#1575)

Bugfix: clean dead persistent session on connect (#1575)

Bugfix: dup flag not set when re-deliver (#1575)

Bugfix: Upgrade the lager_console_backend config (#1575)

Improve: Support set k8s namespace (#1575)

Upgrade the ekka library to v0.2.3 (#1575)

Improve: move PIPE_DIR dir from /tmp/${WHOAMI}_erl_pipes/$NAME/ to /$RUNNER_DATA_DIR/${WHOAMI}\_erl_pipes/$NAME/ (emq-relx#188)

### emq-auth-http

Improve: Retry 3 times when httpc:request occurred socket_closed_remotely error (emq-auth-http#70)

## 2.3.6

*发布日期: 2018-03-25*

### Bugfix and Enhancements

Security: LWT message checking the ACL (#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529)

### emq-auth-jwt

Validate JWT token using a expired field (#29)

## 2.3.5

*发布日期: 2018-03-03*

### Bugfix and Enhancements

Feature: Add etc/ssl_dist.conf file for erlang SSL distribution (emq-relx#178)

Feature: Add node.ssl_dist_optfile option and etc/ssl_dist.conf file (#1512)

Feature: Support Erlang Distribution over TLS (#1512)

Improve: Tune off the 'tune_buffer' option for external MQTT connections (#1512)

### emq-sn

Clean registered topics if mqtt-sn client send a 2nd CONNECT in connected state (#76)

Upgrade the esockd library to v5.2.1 (#76)

### emq-auth-http

Remove 'password' param from ACL and superuser requests (#66)

## 2.3.4

*发布日期: 2018-01-29*

### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket (#1335)

Feature: EMQ node.name with link local ipv6 address not responding to ping (#1460)

Feature: Add PROTO_DIST_ARG flag to support clustering via IPv6 address. (#1460)

Bugfix: retain bit is not set when publishing to clients (when it should be set). (#1461)

Bugfix: Can't search topic on web dashboard (#1473)

### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn#66)

## 2.3.3

*发布日期: 2018-01-08*

### Bugfix and Enhancements

Add a full documentation for emq.conf and plugins.

Repair a dead link in README - missing emq-lwm2m. (#1430)

Subscriber with wildcard topic does not receive retained messages with sub topic has $ sign (#1398)

Web Interface with NGINX Reverse Proxy not working. (#953)

### emq-dashboard

Add dashboard.default_user.login , dashboard.default_user.password options to support configuring default admin.

### emq-modules

The emq-modules rewrite config is not right. (#35)

### emq-docker

Upgrade alpine to 3.7 (#31)

### emq-packages

Support ARM Platform (#12)

## 2.3.2

*发布日期: 2017-12-26*

### Bugfix and Enhancements

Support X.509 certificate based authentication (#1388)

Add proxy_protocol, proxy_protocol_timeout options for ws/wss listener.

Cluster discovery etcd nodes key must be created manually. (#1402)

Will read an incorrect password at the last line of emq_auth_username.conf (#1372)

How can I use SSL/TLS certificate based client authentication? (#794)

Upgrade the esockd library to v5.2.

### esockd

Improve the parser of proxy protocol v2.

Add 'send_timeout', 'send_timeout_close' options.

Rename esockd_transport:port_command/2 function to async_send/2.

Add test case for esockd_transport:async_send/2 function.

Add esockd_transport:peer_cert_subject/1, peer_cert_common_name/1 functions.

### emq-auth-mysql

Update depends on emqtt/mysql-otp.

Fixed the issue that Cannot connect to MySQL 5.7 (#67).

### emq-relx

Fix mergeconf/3 appending line break error. (#152)

### emq-sn

Fix crash in emq_sn_gateway:transform() function which handles SUBACK. (#57)

Define macro SN_RC_MQTT_FAILURE. (#59)

### emq-web-hook

Filter auth_failure client for disconnected hook. (#30)

## 2.3.1

*发布日期: 2017-12-03*

### Bugfix and Enhancements

Remove the unnecessary transactions to optimize session management.

Should not exit arbitrarily when clientid conflicts in mnesia.

Change the default value of 'mqtt.session.enable_stats' to 'on'.

The DUP flag should be set to 0 for all QoS0 messages. (emqttd#1319)

Fix the 'no function clause' exception. (emqttd#1293)

The retained flags should be propagated for bridge. (emqttd#1293)

The management API should listen on 0.0.0.0:8080. (emqttd#1353)

Fast close the invalid websocket in init/1 function.

erlang:demonitor/1 the reference when erasing a monitor. (emqttd#1340)

### emq-retainer

Don't clean the retain flag after the retained message is stored.

Add three CLIs for the retainer plugin. (emq-retainer#38)

### emq-dashboard

Refactor(priv/www): improve the routing page. (emq-dashboard#185)

### emq-modules

Turn off the subscription module by default. (emq-modules#26)

### emq-sn

Add an integration test case for sleeping device.

Do not send will topic if client is kicked out.

Prevent crash information in log when emq_sn_gateway getting timeout, since it is a possible procedure.

### emq-relx

Support node cookie value with = characters. (emq-relx#146)

### mochiweb

Improve Req:get(peername) funciton to support x-forwarded-for and x-remote-port . (emqtt/mochiweb#9)

## 2.3.0 "Passenger's Log"

*发布日期: 2017-11-20*

EMQ 2.3.0 版本正式发布，改进了 PubSub 设计与消息路由性能，更新 EMQ 自带的自签名 SSL 证书，改进 Dashboard 界面与 API 设计。

### Bugfix and Enhancements

Fix the issue that Retained message is not sent for Subscribe to existing topic. (emqttd#1314)

Fix the issue that The DUP flag MUST be set to 0 for all QoS0 messages.(emqttd#1319)

Improve the pubsub design and fix the race-condition issue. (emqttd#PR1342)

Crash on macOS High Sierra (emqttd#1297)

### emq-dashboard Plugin (emq-dashboard#PR174)

Upgraded the 'subscriptions' RESTful API.

Improvement of the auth failure log. (emq-dashboard#59)

### emq-coap Plugin (emq-coap#PR61)

Replaced coap_client with er_coap_client.

Fix: correct the output format of coap_discover() to enable ".well-known/core".

Refactor the coap_discover method.

### emq-relx

Upgraded the bin/nodetool script to fix the rpcterms command.

### emq-web-hook Plugin

Fix the emq_web_hook plugin getting username from client.connected hook. (emq-web-hook#19)

### emq-auth-jwt Plugin(emq-auth-jwt#PR15)

Added test cases for emq_auth_jwt.

Fix jwt:decode/2 functions's return type.

### emq-auth-mongo Plugin(emq-auth-mongo#PR92)

Updated the default MongoDB server configuration.

## 2.2 正式版 "Nostalgia"

*发布日期: 2017-07-08*

_版本别名: Nostalgia_

EMQ-2.2.0 版本正式发布！EMQ R2.2 版本完整支持 CoAP(RFC 7252)、MQTT-SN 协议，支持 Web Hook、Lua Hook、Proxy Protocol V2，支持 Elixir 语言插件开发。

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135)

Bugfix: Exit Code from emqttd_ctl (emqttd#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43)

## 2.1.2

*发布日期: 2017-04-21*

Fix emqttd_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd#1000)

Fix crash caused by duplicated PUBREC packet (emqttd#1004)

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template)

## 2.1.1

*发布日期: 2017-04-14*

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105)

Fix mqtt-sn Gateway not working (emq-sn#12)

Upgrade emq-sn Plugin (emq-sn#11)

Upgrade emq-coap Plugin (emq-coap#21)

## 2.1.0

*发布日期: 2017-04-07*

The stable release of 2.1 version.

Trouble with auth.mysql.acl_query (emq-auth-mysql#38)

Filter the empty fields in ACL table (emq-auth-mysql#39)

## 2.0.7

*发布日期: 2017-01-20*

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages.

emq-auth-http#9: Update the priv/emq_auth_http.schema, cuttlefish:unset() if no super_req/acl_req config exists

emq-auth-mongo#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard#91: Fix the exception caused by binary payload

emq-relx#21: Improve the binemqttd.cmd batch script for windows

emqttd#873: Documentation: installing-from-source

emqttd#870: Documentation: The word in Documents is wrong

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }}

## 2.0.6

*发布日期: 2017-01-08*

Upgrade the [esockd](https://github.com/emqtt/esockd) library to v4.1.1

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed

emq-relx#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo#31: Mongodb ACL Cannot work?

## 2.0.5

*发布日期: 2016-12-24*

emq-auth-http#9: Disable ACL support

emq-auth-mongo#29: Disable ACL support

emq-auth-mongo#30: {datatype, flag}

## 2.0.4

*发布日期: 2016-12-16*

emqttd#822: Test cases for SSL connections

emqttd#818: trap_exit to link WebSocket process

emqttd#799: Can't publish via HTTPS

## 2.0.3

*发布日期: 2016-12-12*

emqttd#796: Unable to forbidden tcp lisener

emqttd#814: Cannot remove a 'DOWN' node from the cluster

emqttd#813: Change parameters order

emqttd#795: Fix metrics of websocket connections

emq-dashboard#88: Rename the default topic from “/World” to “world”

emq-dashboard#86: Lookup all online clients

emq-dashboard#85: Comment the default listener port

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart.

## 2.0.2

*发布日期: 2016-12-05*

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down

emqttd#790: Unable to start emqttd service if username/password contains special characters

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790

## 2.0.1

*发布日期: 2016-11-30*

emqttd#781: 更新项目 README 到 2.0 版本

emq_dashboard#84: 显示节点集群状态

emq_dashboard#79: 集群节点采用 disc_copies 存储 mqtt_admin 表

emq_auth_clientid: 集群节点采用 disc_copies 存储 mqtt_auth_clientid 表

emq_auth_username: 集群节点采用 disc_copies 存储 mqtt_auth_username 表

emq_mod_subscription#3: 删除 emq_mod_subscription 表与 module.subscription.backend 配置

emq_plugin_template#5: 插件停止时注销认证/ACL 模块

## 2.0 正式版 "西湖以西"

*发布日期: 2016-11-24*

_版本别名: 西湖以西(West of West Lake)_

EMQ-2.0 版本正式发布！EMQ-1.0 版本产品环境下已支持 900K 并发连接，EMQ-2.0 版本重构了整个项目架构并正式支持共享订阅功能:

1. 支持共享订阅(Shared Subscription)与本地订阅(Local Subscription)，解决 MQTT 协议负载平衡消费问题；
2. 支持 CoAP(RFC 7252)、MQTT-SN 协议和网关，支持 CoAP、MQTT-SN 客户端与 MQTT 客户端互通；
3. 重构配置文件格式与加载方式，支持用户友好的'K = V'文件格式，支持操作系统环境变量；
4. 增加了扩展钩子和大量的认证插件，支持与大部分数据库或 NoSQL 的认证集成；
5. 支持全平台编译部署，Linux/Unix/Windows 以及 ARM 平台网关，支持 Docker 镜像制作。

### 共享订阅(Shared Subscription)

共享订阅(Shared Subscription)支持在多订阅者间采用分组负载平衡方式派发消息:

    ---------
    |       | --Msg1--> Subscriber1
    Publisher--Msg1,Msg2,Msg3-->|  EMQ  | --Msg2--> Subscriber2
    |       | --Msg3--> Subscriber3
    ---------

使用方式: 订阅者在主题(Topic)前增加'$queue'或'$share/\<group>/'前缀。

### 本地订阅(Local Subscription)

本地订阅(Local Subscription)只在本节点创建订阅与路由表，不会在集群节点间广播全局路由，非常适合物联网数据采集应用。

使用方式: 订阅者在主题(Topic)前增加'$local/'前缀。

### erlang.mk 与 relx

2.0 版本分离 [emqttd ](https://github.com/emqtt/emqttd) 主项目和发布项目 [ emq-relx ](https://github.com/emqtt/emq-relx) , 采用 [ erlang.mk ](https://erlang.mk) 和 [ relx](https://github.com/erlware/relx) 编译发布工具替换 1.x 版本使用的 rebar，项目可以跨平台在 Linux/Unix/Windows 系统下编译。

### CoAP 协议支持

2.0 版本支持 CoAP 协议(RFC7252)，支持 CoAP 网关与 MQTT 客户端互通。

CoAP 插件: [https://github.com/emqtt/emq_coap](https://github.com/emqtt/emq_coap)

### MQTT-SN 协议支持

2.0 版本支持 MQTT-SN 协议，支持 MQTT-SN 网关与 MQTT 客户端互通。

MQTT-SN 插件: [https://github.com/emqtt/emq_sn](https://github.com/emqtt/emq_sn)

### 'K = V'格式配置文件

2.0 版本支持用户友好的'K = V'格式配置文件 etc/emq.conf:

    node.name = emqttd@127.0.0.1

    ...

    mqtt.listener.tcp = 1883

    ...

### 操作系统环境变量

2.0 版本支持操作系统环境变量。启动时通过环境变量设置 EMQ 节点名称、安全 Cookie 以及 TCP 端口号:

    EMQ_NODE_NAME=emqttd@127.0.0.1
    EMQ_NODE_COOKIE=emq_dist_cookie
    EMQ_MAX_PORTS=65536
    EMQ_TCP_PORT=1883
    EMQ_SSL_PORT=8883
    EMQ_HTTP_PORT=8083
    EMQ_HTTPS_PORT=8084

### Docker 镜像支持

EMQ-2.0 版本支持 Docker 镜像制作，Dockerfile 开源在: [https://github.com/emqtt/emq_docker](https://github.com/emqtt/emq_docker)

### Windows 平台支持

2.0 版本完整支持 Windows 平台的编译、发布与运行，支持 Windows 平台下的'emqttd_ctl'控制命令，支持在 Windows 节点间的集群。

### 问题与改进

#764: add mqtt.cache_acl option

#667: Configuring emqttd from environment variables

#722: mqtt/superuser calls two times emqtt_auth_http

#754: "-heart" option for EMQ 2.0

#741: emq_auth_redis cannot use hostname as server address

### 扩展插件

2.0 版本发布的认证与扩展插件列表:

| 插件                                                                    | 说明                       |
| ----------------------------------------------------------------------- | -------------------------- |
| [emq_dashboard](https://github.com/emqtt/emqttd_dashboard)            | Web 控制台插件(默认加载)   |
| [emq_auth_clientid](https://github.com/emqtt/emq_auth_clientid)       | ClientId 认证插件          |
| [emq_auth_username](https://github.com/emqtt/emq_auth_username)       | 用户名、密码认证插件       |
| [emq_auth_ldap](https://github.com/emqtt/emq_auth_ldap)               | LDAP 认证/访问控制         |
| [emq_auth_http](https://github.com/emqtt/emq_auth_http)               | HTTP 认证/访问控制         |
| [emq_auth_mysql](https://github.com/emqtt/emq_auth_mysql)             | MySQL 认证/访问控制        |
| [emq_auth_pgsql](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL 认证/访问控制   |
| [emq_auth_redis](https://github.com/emqtt/emq_auth_redis)             | Redis 认证/访问控制        |
| [emq_auth_mongo](https://github.com/emqtt/emq_auth_mongo)             | MongoDB 认证/访问控制      |
| [emq_mod_rewrite](https://github.com/emqtt/emq_mod_rewrite)           | 重写主题(Topic)插件        |
| [emq_mod_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块        |
| [emq_mod_presence](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布   |
| [emq_mod_subscription](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅     |
| [emq_coap](https://github.com/emqtt/emq_coap)                         | CoAP 协议支持              |
| [emq_sn](https://github.com/emqtt/emq_sn)                             | MQTT-SN 协议支持           |
| [emq_stomp](https://github.com/emqtt/emq_stomp)                       | Stomp 协议支持             |
| [emq_sockjs](https://github.com/emqtt/emq_sockjs)                     | Stomp over SockJS 协议支持 |
| [emq_recon](https://github.com/emqtt/emq_recon)                       | Recon 性能调试             |
| [emq_reloader](https://github.com/emqtt/emq_reloader)                 | Reloader 代码热加载插件    |
| [emq_plugin_template](https://github.com/emqtt/emq_plugin_template)   | 插件开发模版               |

## 1.1.3

*发布日期: 2016-08-19*

Support './bin/emqttd_ctl users list' CLI (#621)

Cannot publish payloads with a size of the order 64K using WebSockets (#643)

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627)

Fix SSL certfile, keyfile config (#651)

## 1.1.2

*发布日期: 2016-06-30*

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596)

Fix WebSocket Client Leak (PR #612)

java.io.EOFException using paho java client (#551)

Send message from paho java client to javascript client (#552)

Compatible with the Qos0 PUBREL packet (#575)

Empty clientId with non-clean session accepted (#599)

Update docs to fix typos (#601, #607)

## 1.1.1

*发布日期: 2016-06-04*

Compatible with the Qos0 PUBREL packet (#575)

phpMqtt Client Compatibility (#572)

java.io.EOFException using paho java client (#551)

## 1.1

*发布日期: 2016-06-01*

1.1 版本升级 eSockd 库到 4.0，支持 IPv6 与监听特定 IP 地址。新增 MongoDB 认证插件、HTTP 认证插件与 Reloader 插件。升级 MySQL、PostgreSQL、Redis 认证插件，采用参数化查询避免 SQL 注入，并支持超级用户(superuser)认证。

### 问题与改进

Allow human-friendly IP addresses (PR#395)

File operation error: emfile (#445)

emqttd_plugin_mongo not found in emqttd (#489)

emqttd_plugin_mongo Error While Loading in emqttd (#505)

Feature request: HTTP Authentication (#541)

Compatible with the Qos0 PUBREL packet (#575)

Bugfix: function_clause exception occurs when registering a duplicated authentication module (#542)

Bugfix: ./emqttd_top msg_q result: {"init terminating in do_boot",{undef,[{etop,start,[],[]},{init,start_it,1,[]},{init,start_em,1,[]}]}} (#557)

### Dashboard 插件

WebSocket 连接页面支持 Clean Session, Qos, Retained 参数设置 (emqttd_dashboard#52)

升级 eSockd 库到 4.0 版本，Overview 页面显示 OTP 版本 (emqttd_dashboard#61)

Changing dashboard credentials for username authentication (emqttd_dashboard#56)

新增'./bin/emqttd_ctl admins'管理命令，支持通过命令行重新设置 admin 密码

### HTTP 认证插件

支持通过 HTTP API 认证/鉴权 MQTT 客户端: [https://github.com/emqtt/emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)

### MongoDB 认证插件

升级 Erlang Mongodb 驱动到 v1.0.0 (emqttd_plugin_mongo#1)

支持超级用户认证

支持基于 MongoDB 的 ACL (emqttd_plugin_mongo#3)

### MySQL 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

### Postgre 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

### Redis 认证插件

支持超级用户认证

支持 ClientId 认证/ACL (emqttd_plugin_redis#4)

### Reloader 插件

开发调试代码热升级插件: [https://github.com/emqtt/emqttd_reloader](https://github.com/emqtt/emqttd_reloader)

## 1.0.2

*发布日期: 2016-05-04*

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data

PR#519 - The erlang '-name' requires the fully qualified host name

emqttd_reloader plugin - help reload modified modules during development.

## 1.0.1

*发布日期: 2016-04-16*

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs

## 1.0 (七英里)

*发布日期: 2016-04-13*

_版本别名: 七英里(The Seven Mile Journey)_

经过两年开发，五十个版本迭代，我们正式发布 1.0(七英里)版本，和完整的中英文项目文档。

1.0 版本基本实现了设计目标: 稳定承载来自移动互联网或物联网终端的大量并发 MQTT 连接，并实现在大数量的终端间快速低延时的 MQTT 消息路由。

1. 完整支持 MQTT V3.1.1 协议，扩展支持 WebSocket、Stomp 或私有 TCP 等多协议。
2. 稳定承载大规模的并发 MQTT 客户端连接，单服务器节点支持 50 万到 100 万连接。
3. 分布式节点集群或桥接，快速低延时的消息路由，单集群支持 1000 万规模的路由。
4. 支持消息服务器内扩展，支持定制多种认证方式，插件方式存储消息到后端数据库。

### 问题与改进

1.0 版本主要发布完整项目文档，相比 0.17.1 版本很少代码变更:

Possible race condition using emqttd_cm (#486)

Improve the design of retained message expiration (#503)

Should not expire the retained messages from $SYS/# topics (#500)

### 项目文档

1.0 版本中文文档: [http://emqtt.com/docs/ ](http://emqtt.com/docs/) 或 [ http://docs.emqtt.cn](http://docs.emqtt.cn)

1.0 版本英文文档: [https://developer.emqx.io/docs/emq/v1/en/index.html ](https://developer.emqx.io/docs/emq/v1/en/index.html) 或 [ http://docs.emqtt.com/](http://docs.emqtt.com/)

### 官方站点

中文站点: [http://emqtt.com](http://emqtt.com)

英文站点: [https://www.emqx.io/](https://www.emqx.io/)

### 致谢

爱立信与 Erlang/OTP 语言平台团队( [http://www.erlang.org/](http://www.erlang.org/) )!

贡献者(GitHub 帐户): @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

公司: 开源中国，鲁能电力，太极计算机，电信天翼云直播，研色科技，杭州华思

乐队: 七英里(The Seven Mile Journey)，腰乐队，万能青年旅店

## 0.2.0

*发布日期: 2014-12-07*

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5

*发布日期: 2013-01-05*

Bugfix: remove QOS_1 match when handle PUBREL request

Bugfix: reverse word in emqtt_topic:words/1 function

## 0.1.4

*发布日期: 2013-01-04*

Bugfix: fix "mosquitto_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3

*发布日期: 2013-01-04*

Feature: support QOS2 PUBREC, PUBREL,PUBCOMP messages

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2

*发布日期: 2012-12-27*

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl

## 0.1.1

*发布日期: 2012-09-24*

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0

*发布日期: 2012-09-21*

The first public release.
