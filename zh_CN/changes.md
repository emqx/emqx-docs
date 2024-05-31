# 版本发布 (Changes)

## 3.2.7 版本

_发布日期: 2019-12-03_

EMQ X 3.2.7 版本发布。此版本主要重新支持了通过配置文件配置默认的 `username` 和 `clientid` 。

### emqx-auth-username (plugin)

功能增强:

- 重新支持了通过配置文件配置默认的 `username`

Github PR: [ emqx/emqx-auth-username#127 ](https://github.com/emqx/emqx-auth-username/pull/127)

### emqx-auth-clientid (plugin)

功能增强:

- 重新支持了通过配置文件配置默认的 `clientid`

Github PR: [ emqx/emqx-auth-clientid#123 ](https://github.com/emqx/emqx-auth-clientid/pull/123)

## 3.2.6 版本

_发布日期: 2019-11-23_

EMQ X 3.2.6 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

错误修复:

- 修复通过 `gen_rpc` 向远程节点转发消息时可能失序的问题

Github PR: [ emqx/emqx#3049 ](https://github.com/emqx/emqx/pull/3049)

- 修复认证插件崩溃会导致 `emqx`  崩溃的问题

Github PR: [ emqx/emqx#3048 ](https://github.com/emqx/emqx/pull/3048)

## 4.0-beta.4 版本

_发布日期: 2019-11-18_

EMQ X 4.0-beta.4 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

功能增强:

- 被检测到 flapping 的客户端会被 banned

Github PR: [ emqx/emqx#3033 ](https://github.com/emqx/emqx/pull/3033)

- 优化 emqx_vm 模块并更新测试用例

Github PR: [ emqx/emqx#3034 ](https://github.com/emqx/emqx/pull/3034)

### emqx-management (plugin)

功能增强:

- 更新 banned API

Github PR: [ emqx/emqx-management#141 ](https://github.com/emqx/emqx-management/pull/141)

错误修复:

- 修复一些错误的返回值

Github PR: [ emqx/emqx-management#142 ](https://github.com/emqx/emqx-management/pull/142)

### minirest (plugin)

错误修复:

- 添加错误处理并增加日志

Github PR: [ emqx/minirest#20 ](https://github.com/emqx/minirest/pull/20)

### esockd (dependency)

功能增强:

- 调整部分接口并增加测试用例

Github PR: [ emqx/esockd#124 ](https://github.com/emqx/esockd/pull/124)

### ekka (dependency)

功能增强:

- 调整部分接口并增加测试用例

Github PR: [ emqx/ekka#67 ](https://github.com/emqx/ekka/pull/67)

## 3.2.5 版本

_发布日期: 2019-11-15_

EMQ X 3.2.5 版本发布。此版本主要进行了错误修复。

### emqx-rule-engine (plugin)

错误修复:

- 支持 SQL 关键字: FOREACH/DO/INCASE

Github Commit: [ emqx/emqx-rule-engine#a962e3 ](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

- 支持 SQL 关键字: CASE/WHEN

Github Commit: [ emqx/emqx-rule-engine#40e68e ](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

- 支持在 SQL 的 WHERE 子句中比较原子与二进制

Github Commit: [ emqx/emqx-rule-engine#b240cc ](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

- 修复 select 和 foreach 中的列验证失败

Github Commit: [ emqx/emqx-rule-engine#6a1267 ](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

- 修复重建规则时出现竞争的问题

Github Commit: [ emqx/emqx-rule-engine#af8967 ](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

- 修复重发消息时没有确证设置标志的问题

Github Commit: [ emqx/emqx-rule-engine#60e45c ](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)

### minirest (plugin)

错误修复:

- 修复日志没有记录错误数据的问题

Github PR: [ emqx/minirest#20 ](https://github.com/emqx/minirest/pull/20)

### emqx-web-hook (plugin)

错误修复:

- 修复错误的匹配

Github Commit: [ emqx/emqx-web-hook#3dd041 ](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)

## 4.0-beta.3 版本

_发布日期: 2019-11-01_

EMQ X 4.0-beta.3 版本发布。此版本主要针对错误修复以及测试覆盖率提升。

错误修复:

- 修复跨集群转发时消息失序的问题

Github PR: [ emqx/emqx#3000 ](https://github.com/emqx/emqx/pull/3000)

### emqx-management (plugin)

功能增强:

- REST API 支持 IPv6

Github PR: [ emqx/emqx-management#135 ](https://github.com/emqx/emqx-management/pull/135)

错误修复:

- 修复转码后的 URI 没有被正确处理的问题

Github PR: [ emqx/emqx-management#137 ](https://github.com/emqx/emqx-management/pull/137)

### emqx-dashboard (plugin)

功能增强:

- 支持使用 IPv6 访问 Dashbaord

Github PR: [ emqx/emqx-dashboard#162 ](https://github.com/emqx/emqx-dashboard/pull/162)

### emqx-delayed-publish (plugin)

错误修复:

- 修复插件在集群环境下只能在一个节点中开启的问题

Github PR: [ emqx/emqx-delay-publish#50 ](https://github.com/emqx/emqx-delay-publish/pull/50)

- 修复延迟发布消息失序的问题，感谢 [ soldag ](https://github.com/soldag) 的贡献

Github PR: [ emqx/emqx-delay-publish#49 ](https://github.com/emqx/emqx-delay-publish/pull/49)

Github Issue: [ emqx/emqx-delay-publish#15 ](https://github.com/emqx/emqx-delay-publish/issues/15)

## 3.2.4 版本

_发布日期: 2019-10-28_

EMQ X 3.2.4 版本发布。此版本主要为 Dashbaord 和 REST API 添加了 IPv6 支持，并修复了一些错误。

错误修复:

- 修复 max_subscriptions 配置不生效的问题

Github PR: [ emqx/emqx#2922 ](https://github.com/emqx/emqx/pull/2922)

Github Issue: [ emqx/emqx#2908 ](https://github.com/emqx/emqx/issues/2908)

### emqx-auth-mysql (plugin)

错误修复:

- 使用占位符时更安全地取值

Github PR: [ emqx/emqx-auth-mysql#180 ](https://github.com/emqx/emqx-auth-mysql/pull/180)

Github Issue: [ emqx/emqx#2937 ](https://github.com/emqx/emqx/issues/2937)

### emqx-dashboard (plugin)

功能增强:

- 支持使用 IPv6 访问 Dashbaord

Github PR: [ emqx/emqx-dashboard#161 ](https://github.com/emqx/emqx-dashboard/pull/161)

### emqx-management (plugin)

功能增强:

- REST API 支持 IPv6

Github PR: [ emqx/emqx-management#134 ](https://github.com/emqx/emqx-management/pull/134)

### emqx-delay-publish (plugin)

错误修复:

- 修复延迟发布消息失序的问题，感谢 [ soldag ](https://github.com/soldag) 的贡献

Github PR: [ emqx/emqx-delay-publish#48 ](https://github.com/emqx/emqx-delay-publish/pull/48)

Github Issue: [ emqx/emqx-delay-publish#15 ](https://github.com/emqx/emqx-delay-publish/issues/15)

### emqx-rule-engine (plugin)

功能增强:

- 优化规则引擎中 JSON Payload 解析语句

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)

## 4.0-beta.2 版本

_发布日期: 2019-10-12_

EMQ X 4.0-beta.2 版本发布。此版本主要针对错误修复以及继续优化内部模块设计。

错误修复:

- 修复 SSL 握手失败导致崩溃的问题

Github PR: [ emqx/emqx#2963 ](https://github.com/emqx/emqx/pull/2963)

- 检查 PUBLISH 报文的主题层级

Github PR: [ emqx/emqx#2964 ](https://github.com/emqx/emqx/pull/2964)

### emqtt (plugin)

功能增强:

- 提供命令行接口

Github PR: [ emqx/emqtt#91 ](https://github.com/emqx/emqtt/pull/91)

### emqx-sn (plugin)

错误修复:

- 适配 MQTT-SN 插件到 4.0 版本

Github PR: [ emqx/emqx-sn#145 ](https://github.com/emqx/emqx-sn/pull/145)

### emqx-coap (plugin)

错误修复:

- 适配 CoAP 插件到 4.0 版本

Github Commit: [ emqx/emqx-coap#c7c175 ](https://github.com/emqx/emqx-coap/commit/c7c17540c1248dcdd402b41323c23a211e8292fc) , [ emqx/emqx-coap#9b8ede ](https://github.com/emqx/emqx-coap/commit/9b8ede093cfc3b7211663520e496c579c11611f6)

## 4.0-beta.1 版本

_发布日期: 2019-09-30_

EMQ X 4.0-beta.1 版本发布。此版本主要针对内部模块进行重新设计，实现吞吐大幅度提升。

## 3.2.3 版本

_发布日期: 2019-09-16_

EMQ X 3.2.3 版本改动主要为错误修复。

错误修复:

- 修复 emqx 容器运行时 CPU 占用率告警异常的问题

GitHub Commit: [ emqx/emqx#9cdaa7 ](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

- 修复消息过期机制不生效的问题

Github Commit: [ emqx/emqx#31671f ](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

- 修复占位符在 mountpoint 中不生效的问题

Github Commit: [ emqx/emqx#58ba22 ](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)

### emqx-dashboard (plugin)

错误修复:

- 修复 SSL 无法使用的问题

Github Commit: [ emqx/emqx-dashboard#272a42 ](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)

## 3.2.2 版本

_发布日期: 2019-08-03_

EMQ X 3.2.2 版本改动主要为错误修复。

功能增强:

- 扩展 `gen_rpc` 配置

Github PR: [ emqx/emqx#2732 ](https://github.com/emqx/emqx/pull/2732)

### emqx-rule-engine (plugin)

错误修复:

- 修复测试 URL 连通性的问题

Github PR: [ emqx/emqx-rule-engine#88 ](https://github.com/emqx/emqx-rule-engine/pull/88)

### emqx-dashboard (plugin)

功能增强:

- 增加帮助页面

### ekka (dependency)

错误修复:

- 修复释放锁可能导致崩溃的问题

Github PR: [ emqx/ekka#60 ](https://github.com/emqx/ekka/pull/60)

## 3.2.1 版本

_发布日期: 2019-07-20_

EMQ X 3.2.1 版本改动主要包括错误修复与性能增强。

功能增强:

- 优化 `gen_rpc` 的调用

Github PR: [ emqx/emqx#2694 ](https://github.com/emqx/emqx/pull/2694)

- 支持使用 hostname 自动发现 k8s 集群

Github PR: [ emqx/emqx#2699 ](https://github.com/emqx/emqx/pull/2699)

- 将默认 uptime 心跳时间改为 30s

Github PR: [ emqx/emqx#2696 ](https://github.com/emqx/emqx/pull/2696)

错误修复:

- 修复 WebSocket 非正常下线时出现 crash 的问题

Github PR: [ emqx/emqx#2697 ](https://github.com/emqx/emqx/pull/2697)

- 修复 Session 异常关闭时，ws_channel 仍然在线的问题

Github PR: [ emqx/emqx#2704 ](https://github.com/emqx/emqx/pull/2704)

### emqx-rule-engine (plugin)

功能增强:

- 增强 republish 动作参数

Github PR: [ emqx/emqx-rule-engine#81 ](https://github.com/emqx/emqx-rule-engine/pull/81)

错误修复:

- 修复使用 '.' 筛选 payload 字段失败的问题

Github PR: [ emqx/emqx-rule-engine#83 ](https://github.com/emqx/emqx-rule-engine/pull/83)

### emqx-dashboard (plugin)

错误修复:

- 修复 Dashboard 资源列表在 Safari 下渲染错误的问题

Github PR: [ emqx/emqx-dashboard#124 ](https://github.com/emqx/emqx-dashboard/pull/124) , [ emqx/emqx-dashboard#125 ](https://github.com/emqx/emqx-dashboard/pull/125) , [ emqx/emqx-dashboard#126 ](https://github.com/emqx/emqx-dashboard/pull/126)

### emqx-lwm2m (plugin)

功能增强:

- 兼容 LwM2M 1.1 版本客户端登录

Github Commit: [ emqx/emqx-lwm2m#1c03bf ](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)

### emqx-rel (build project)

功能增强:

- 内置 rebar3 脚本

Github PR: [ emqx/emqx-rel#394 ](https://github.com/emqx/emqx-rel/pull/394)

- EMQ X Windows 服务延迟启动

Github PR: [ emqx/emqx-rel#395 ](https://github.com/emqx/emqx-rel/pull/395)

## 3.2.0 版本

_发布日期: 2019-07-12_

EMQ X 3.2.0 版本主要优化和改进了规则引擎。

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

## 3.2-rc.3 版本

_发布日期: 2019-07-06_

EMQ X 3.2-rc.3 版本改动主要包括功能增强与错误修复。

错误修复:

- 修复 [ emqx/emqx: issue#2635 ](https://github.com/emqx/emqx/issues/2635)

Github PR: [ emqx/emqx#2663 ](https://github.com/emqx/emqx/pull/2663)

### emqx-web-hook (plugin)

错误修复:

- 修复 `actions.failure` 无计数的问题

Github PR: [ emqx/emqx-web-hook#137 ](https://github.com/emqx/emqx-web-hook/pull/137)

### emqx-bridge-mqtt (plugin)

功能增强:

- 增加桥接模式选项

Github PR: [ emqx/emqx-bridge-mqtt#6 ](https://github.com/emqx/emqx-bridge-mqtt/pull/6)

- 优化 RPC 消息的应答机制

- 支持规则引擎下的 MQTT/RPC Bridge 缓存消息到本地磁盘队列

- 修复规则引擎下的 RPC Bridge 无法桥接远程 EMQ X 节点的问题

Github PR: [ emqx/emqx-bridge-mqtt#7 ](https://github.com/emqx/emqx-bridge-mqtt/pull/7)

### emqx-rule-engine (plugin)

功能增强:

- Rule 与 Resource 的 API 支持集群

Github PR: [ emqx/emqx-rule-engine#75 ](https://github.com/emqx/emqx-rule-engine/pull/75)

- 新增返回触发事件的可用字段的 API

Github PR: [ emqx/emqx-rule-engine#74 ](https://github.com/emqx/emqx-rule-engine/pull/74) , [ emqx/emqx-rule-engine#77 ](https://github.com/emqx/emqx-rule-engine/pull/77)

错误修复:

- 修复获取资源状态超时引起的问题

Github PR: [ emqx/emqx-rule-engine#76 ](https://github.com/emqx/emqx-rule-engine/pull/76)

### emqx-dashboard (plugin)

功能增强:

- 规则引擎各项指标细分到节点

Github PR: [ emqx/emqx-dashboard#114 ](https://github.com/emqx/emqx-dashboard/pull/114)

错误修复:

- 修复资源创建的 BUG

Github PR: [ emqx/emqx-dashboard#114 ](https://github.com/emqx/emqx-dashboard/pull/114)

## 3.2-rc.2 版本

_发布日期: 2019-06-29_

EMQ X 3.2-rc.2 版本改动主要包括错误修复。

功能增强:

- 把默认日志级别改为 **warning**

Github PR: [ emqx/emqx#2657 ](https://github.com/emqx/emqx/pull/2657)

- 增加获取历史告警的接口

Github PRs: [ emqx/emqx#2660 ](https://github.com/emqx/emqx/pull/2660) [ emqx/emqx-management#98 ](https://github.com/emqx/emqx-management/pull/98)

错误修复:

- 删除残留的 Session 记录

Github PR: [ emqx/emqx#2655 ](https://github.com/emqx/emqx/pull/2655)

- 解决批量发送时，消息失序的问题

Github PR: [ emqx/emqx#2650 ](https://github.com/emqx/emqx/pull/2650)

感谢 [ tradingtrace ](https://github.com/tradingtrace) 的贡献!

### emqx-rule-engine (plugin)

功能增强:

- 新增一个动作 "do nothing"

Github PR: [ emqx/emqx-rule-engine#70 ](https://github.com/emqx/emqx-rule-engine/pull/70)

- 将 `retain` flag 的数据类型改为 integer

Github RP: [ emqx/emqx-rule-engine#72 ](https://github.com/emqx/emqx-rule-engine/pull/72)

错误修复:

- 修复 SQL 中无法使用 `timestamp` 关键字作为字段的问题

Github PR: [ emqx/emqx-rule-engine#71 ](https://github.com/emqx/emqx-rule-engine/pull/71)

### emq-bridge-mqtt (plugin)

功能增强:

- 将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件

Github PR: [ emqx/emqx-bridge-mqtt#2 ](https://github.com/emqx/emqx-bridge-mqtt/pull/2)

### emqx-rel (build project)

错误修复:

- 解决 windows 服务注册的问题

Github PR: [ emqx/emqx-rel#381 ](https://github.com/emqx/emqx-rel/pull/381)

## 3.2-rc.1 版本

_发布日期: 2019-06-22_

EMQ X 3.2-rc.1 版本发布。此版本改动主要包括功能增强与错误修复。

功能增强:

- 支持设置日志前缀

Github PR: [ emqx/emqx#2627 ](https://github.com/emqx/emqx/pull/2627)

- 提升 connect/disconnect 系统消息中客户端上下线的时间戳精度

Github PR: [ emqx/emqx#2641 ](https://github.com/emqx/emqx/pull/2641)

- 优化开发流程，支持 `make run`

Github PR: [ emqx/emqx#2644 ](https://github.com/emqx/emqx/pull/2644)

错误修复:

- 修复 flapping 模块无法正确读取配置的问题

Github PR: [ emqx/emqx#2628 ](https://github.com/emqx/emqx/pull/2628)

- 修复 `cpu_sup:util/0` 在 Windows 环境不可用导致崩溃的问题

Github PR: [ emqx/emqx#2629 ](https://github.com/emqx/emqx/pull/2629)

- 修复 [ emqx/emqx: issue#2619 ](https://github.com/emqx/emqx/issues/2619)

Github PR: [ emqx/emqx#2646 ](https://github.com/emqx/emqx/pull/2646)

### emqx-rule-engine (plugin)

功能增强:

- 支持定期获取资源状态并设置告警

Github PR: [ emqx/emqx-rule-engine#67 ](https://github.com/emqx/emqx-rule-engine/pull/67)

### emqx-sn (plugin)

错误修复:

- 修复误判 `keepalive_timeout` 的问题

Github PR: [ emqx/emqx-sn#127 ](https://github.com/emqx/emqx-sn/pull/127)

- 修复没有正确获取 `idle_timeout` 的问题

Github PR: [ emqx/emqx-sn#128 ](https://github.com/emqx/emqx-sn/pull/128)

- 修复测试用例

Github PR: [ emqx/emqx-sn#130 ](https://github.com/emqx/emqx-sn/pull/130)

### emqx-auth-jwt (plugin)

错误修复:

- 正确读取 pubkey

Github PR: [ emqx/emqx-auth-jwt#88 ](https://github.com/emqx/emqx-auth-jwt/pull/88)

### emqx-rel (build-project)

Enhancements:

- 使项目构建更加智能和健壮

GitHub PR: [ emqx/emqx-rel#375 ](https://github.com/emqx/emqx-rel/pull/375) , [ emqx/emqx-rel#376 ](https://github.com/emqx/emqx-rel/pull/376)

## 3.2-beta.3 版本

_发布日期: 2019-06-14_

EMQ X 3.2-beta.3 版本发布。此版本改动主要包括增强规则引擎和错误修复。

错误修复:

- 修复没有检查 `Will Retain` 的问题

Github PR: [ emqx/emqx#2607 ](https://github.com/emqx/emqx/pull/2607)

- 修复 [ emqx/emqx: issue#2591 ](https://github.com/emqx/emqx/issues/2591)

Github PR: [ emqx/emqx#2615 ](https://github.com/emqx/emqx/pull/2615)

- 默认情况下删除日志记录的字符限制

Github PR: [ emqx/emqx#2617 ](https://github.com/emqx/emqx/pull/2617)

- 修复无法处理分裂的 TCP 报文的问题

Github PR: [ emqx/emqx#2611 ](https://github.com/emqx/emqx/pull/2611)

### emqx-rule-engine (plugin)

功能增强:

- 支持规则命中次数等 Metrics 统计

Github PR: [ emqx/emqx-rule-engine#63 ](https://github.com/emqx/emqx-rule-engine/pull/63)

### emqx-management (plugin)

错误修复:

- 修复 CLI 无法踢掉 websocket 连接的问题

Github PR: [ emqx/emqx-management#93 ](https://github.com/emqx/emqx-management/pull/93)

## 3.2-beta.2 版本

_发布日期: 2019-06-06_

EMQ X 3.2-beta.2 版本发布。此版本改动主要包括增强规则引擎和错误修复。

错误修复:

- 修复 [ emqx/emqx: issue#2553 ](https://github.com/emqx/emqx/issues/2553)

Github PR: [ emqx/emqx#2596 ](https://github.com/emqx/emqx/pull/2596)

### emqx-rule-engine (plugin)

功能增强:

- 支持在 Dashboard 中测试 SQL 语句

Github Commit: [ emqx/emqx-rule-engine#3e7c4c ](https://github.com/emqx/emqx-rule-engine/commit/3e7c4cbe275d8f120ad8efb83fd23ee571d465db)

- 预处理 PreparedStatement 以获得更好的性能

Github Commit: [ emqx/emqx-rule-engine#fa3720 ](https://github.com/emqx/emqx-rule-engine/commit/fa37205850c6efe9af5f8ca2f230e17c7de2adb4) , [ emqx/emqx-rule-engine#b00fad ](https://github.com/emqx/emqx-rule-engine/commit/b00fad45c283fa2ec3aa57353bbe161960547461)

- 规则引擎适配集群

Github Commit: [ emqx/emqx-rule-engine#3da7fe ](https://github.com/emqx/emqx-rule-engine/commit/3da7fed60d92c9a994c2aed5f34509c0d0d4eff4) , [ emqx/emqx-rule-engine#4963b0 ](https://github.com/emqx/emqx-rule-engine/commit/4963b0ee3a6114ebe74b48876d25723137df14ad)

- Dashboard 可以显示 Resource 状态

Github Commit: [ emqx/emqx-rule-engine#dd9a8d ](https://github.com/emqx/emqx-rule-engine/commit/dd9a8d4801f650c1ac888f7420f5497f7d0d6c73) , [ emqx/emqx-rule-engine#d16224 ](https://github.com/emqx/emqx-rule-engine/commit/d162246c0b630e059c21f7b36e50154f3d7832e3) , [ emqx/emqx-rule-engine#e4574c ](https://github.com/emqx/emqx-rule-engine/commit/e4574c9554d7e7d79a8ce55a6c9e4089ee00db79)

- 支持通过 Dashboard 重启 Resource

Github Commit: [ emqx/emqx-rule-engine#ccbffd ](https://github.com/emqx/emqx-rule-engine/commit/ccbffd7d5db514adf6cd20e8d139e73f80bc1c96)

- 支持检查 HTTP 是否可连通

Github Commit: [ emqx/emqx-rule-engine#3feffc ](https://github.com/emqx/emqx-rule-engine/commit/3feffcd5a3f0da78725f1208594cea1b3273ec0b)

错误修复:

- 修复删除 Resource 前检查依赖发生错误的问题

Github Commit: [ emqx/emqx-rule-engine#3265ff ](https://github.com/emqx/emqx-rule-engine/commit/3265ffe10584f0edccc084e6f78ae035ba310c07)

- 修复 Resource 无法被销毁的问题

Github Commit: [ emqx/emqx-rule-engine#58a1ce ](https://github.com/emqx/emqx-rule-engine/commit/58a1ce45e1cf96cf05481d8ed076febef0d41976)

- 修复 SQL 无法嵌套插入的问题

Github Commit: [ emqx/emqx-rule-engine#64776a ](https://github.com/emqx/emqx-rule-engine/commit/64776aebde1fe48c1038fba3b61f457590ab4408)

### emqx-auth-http (plugin)

功能增强:

- 支持 HTTPs

Github PR: [ emqx/emqx-auth-http#133 ](https://github.com/emqx/emqx-auth-http/pull/133)

### emqx-docker

错误修复:

- 修复 [ emqx/emqx-docker: issue#115 ](https://github.com/emqx/emqx-docker/issues/115)

Github Commit: [ emqx/emqx-docker#f3c219 ](https://github.com/emqx/emqx-docker/commit/f3c21978f5ffefd5d419bc78a1caf1ad71de9c91)

### emqx-management (plugin)

错误修复:

- 修复重新加载插件失败的问题

Github PR: [ emqx/emqx-management#91 ](https://github.com/emqx/emqx-management/pull/91)

### ekka (deps)

错误修复:

- 修复导致 emqx_sm_locker 崩溃的问题

Github Commit: [ emqx/ekka#2d5bf2 ](https://github.com/emqx/ekka/commit/2d5bf2a1f10d84408e4b35d3e274a49f395056c3)

## 3.2-beta.1 版本

_发布日期: 2019-05-27_

EMQ X 3.2.beta-1 版本发布。此版本改动主要包括支持 rebar3 构建和增强规则引擎。

功能增强:

- 支持通过 rabar3 构建项目

Github PR: [ emqx/emqx#2475 ](https://github.com/emqx/emqx/pull/2475) , [ emqx/emqx#2510 ](https://github.com/emqx/emqx/pull/2510) , [ emqx/emqx#2518 ](https://github.com/emqx/emqx/pull/2518) , [ emqx/emqx#2521 ](https://github.com/emqx/emqx/pull/2521)

- SSL 连接支持 {active, N} 选项

Github PR: [ emqx/emqx#2531 ](https://github.com/emqx/emqx/pull/2531)

- 更正匿名访问行为表现

Github PR: [ emqx/emqx#2355 ](https://github.com/emqx/emqx/pull/2355)

- 提升 zone 的访问速度

Github PR: [ emqx/emqx#2548 ](https://github.com/emqx/emqx/pull/2548)

错误修复:

- 修复 emqx_sm 中的致命错误

Github PR: [ emqx/emqx#2559 ](https://github.com/emqx/emqx/pull/2559)

- 修复发布 MQTT-SN、CoAP 消息时的错误

Github PR: [ emqx/emqx#2556 ](https://github.com/emqx/emqx/pull/2556)

### emqx-rule-engine (plugin)

功能增强:

- 更好的规则引擎

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)

### emqx-web-hook (plugin)

功能增强:

- 增加一个用于编码 payload 字段的选项

Github PR: [ emqx/emqx-web-hook#119 ](https://github.com/emqx/emqx-web-hook/pull/119)

### emqx-auth-http (plugin)

功能增强:

- HTTP 请求支持更多选项

Github PR: [ emqx/emqx-auth-http#128 ](https://github.com/emqx/emqx-auth-http/pull/128)

### emqx-sn (plugin)

错误修复:

- 修复错误的函数调用

Github PR: [ emqx/emqx-sn#118 ](https://github.com/emqx/emqx-sn/pull/118)

## 3.1.2 版本

_发布日期: 2019-06-06_

EMQ X 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

### EMQ X Core

Bug fixes:

- 修复 [ emqx/emqx: issue #2595 ](https://github.com/emqx/emqx/issues/2595)

Github PR: [ emqx/emqx#2601 ](https://github.com/emqx/emqx/pull/2601)

- 修复无法设置日志等级的问题

Github PR: [ emqx/emqx#2600 ](https://github.com/emqx/emqx/pull/2600)

- 修复返回值不匹配的问题

Github PR: [ emqx/emqx#2560 ](https://github.com/emqx/emqx/pull/2560)

- 热修复 `emqx_sn` 与 `emqx_coap` 插件

Github PR: [ emqx/emqx#2556 ](https://github.com/emqx/emqx/pull/2556)

### emqx-coap (plugin)

错误修复:

- 修复无法发布消息的问题

Github PR: [ emqx/emqx-coap#120 ](https://github.com/emqx/emqx-coap/pull/120)

### ekka (deps)

错误修复:

- 修复导致 `emqx_sm_locker` 崩溃的问题

Github PR: [ emqx/ekka#54 ](https://github.com/emqx/ekka/pull/54)

- 修复 k8s 无法使用 dns 集群的问题

Github PR: [ emqx/ekka#53 ](https://github.com/emqx/ekka/pull/53)

- 修复 etcd 集群不可用的问题

Github PR: [ emqx/ekka#52 ](https://github.com/emqx/ekka/pull/52)

## 3.1.1 版本

_发布日期: 2019-05-10_

EMQ X 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

功能增强:

- 增大单条日志可打印的最大字符数量

Github PR: [ emqx/emqx#2509 ](https://github.com/emqx/emqx/pull/2509)

- `force_shutdown_policy` 将根据系统位数使用不同的默认值

Github PR: [ emqx/emqx#2515 ](https://github.com/emqx/emqx/pull/2515)

错误修复:

- 正确地配置和使用 `long_gc` 与 `long_schedule`

Github PR: [ emqx/emqx#2504 ](https://github.com/emqx/emqx/pull/2504) , [ emqx/emqx#2513 ](https://github.com/emqx/emqx/pull/2513)

- 修复没有更新 `suboptions/count` 的问题

Github PR: [ emqx/emqx#2507 ](https://github.com/emqx/emqx/pull/2507)

### emqx-lwm2m (plugin)

错误修复:

- 修复 mountpoint 没有生效的问题

Github PR: [ emqx/emqx-lwm2m#34 ](https://github.com/emqx/emqx-lwm2m/pull/34)

- 修复消息无法被 `emqx-web-hook` 转发的问题

Github PR: [ emqx/emqx-lwm2m#35 ](https://github.com/emqx/emqx-lwm2m/pull/35)

## 3.1.0 版本

_发布日期: 2019-04-26_

EMQ X 3.1.0 版本发布。此版本改动主要包括全面支持规则引擎、引入 storm 模块以支持 edge storm、 重构 flapping 代码。

功能改进:

- 添加 emqx_ct_helpers 依赖，并重构测试用例

Github PR: [ emqx/emqx#2480 ](https://github.com/emqx/emqx/pull/2480)

- 重构 flapping 代码

Github PR: [ emqx/emqx#2476 ](https://github.com/emqx/emqx/pull/2476)

### emqx-management (plugin)

问题修复:

- 修复 listeners acceptors 的值没有正确获取的问题

Github PR: [ emqx/emqx-management#76 ](https://github.com/emqx/emqx-management/pull/76)

### emqx-rule-engine (plugin)

功能改进:

- 支持规则动作参数的验证

Github PR: [ emqx/emqx-rule-engine#b28318 ](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

- 删除资源时检查是否存在依赖

Github PR: [ emqx/emqx-rule-engine#fa75b9 ](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

- 从 republish 动作中移除 `from` 参数

Github PR: [ emqx/emqx-rule-engine#8721eb ](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

- 修复了 SQL where 子句不能处理整数的问题

Github PR: [ emqx/emqx-rule-engine#c9c761 ](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)

### emqx-storm (plugin)

功能改进:

- 支持 edge storm

Github Repository: [ emqx/emqx-storm ](https://github.com/emqx/emqx-storm)

## 3.1-rc.3 版本

_发布日期: 2019-04-19_

EMQ X 3.1-rc.3 版本发布。此版本改动主要包括规则引擎增强、错误修复。 注意: 从此版本开始，新增 OpenSUSE 安装包，并且不再提供 Debian 7 安装包。

功能改进:

- 支持对客户端进行 flapping 检测，以及禁止异常的客户端

Github PR: [ emqx/emqx#2438 ](https://github.com/emqx/emqx/pull/2438)

- 支持配置日志输出长度

Github PR: [ emqx/emqx#2461 ](https://github.com/emqx/emqx/pull/2461)

问题修复:

- 修复 `emqx_client` 没有正确设置 CONNECT 报文 Keep Alive 字段的问题

Github PR: [ emqx/emqx#2443 ](https://github.com/emqx/emqx/pull/2443)

### emqx-auth-mysql (plugin)

功能改进:

- 支持 proxysql

Github PR: [ emqx/emqx-auth-mysql#134 ](https://github.com/emqx/emqx-auth-mysql/pull/134)

### emqx-statsd (plugin)

问题修复:

- 修复 Windows 兼容性引起的问题

Github PR: [ emqx/emqx-statsd#24 ](https://github.com/emqx/emqx-statsd/pull/24)

### emqx-web-hook (plugin)

功能改进:

- 支持事件 actions

Github Commit: [ emqx/emqx-web-hook#8367e0 ](https://github.com/emqx/emqx-web-hook/commit/8367e02f5ccafc7df9600c258348461a67c171bd)

- 优化 webhook 资源的 specs

Github Commit: [ emqx/emqx-web-hook#5a1345 ](https://github.com/emqx/emqx-web-hook/commit/5a13457d4f823fa80df1c7eab9a8e945ae6a0701)

- 支持通过 hook 类型搜索 actions

Github Commit: [ emqx/emqx-web-hook#fb3b1b ](https://github.com/emqx/emqx-web-hook/commit/fb3b1ba98ca3f2557a51be98a06537781119132c)

### emqx-rule-engine (plugin)

功能改进:

- 支持通过资源类型搜索 actions

Github PR: [ emqx/emqx-rule-engine#25 ](https://github.com/emqx/emqx-rule-engine/pull/25)

- 注册资源提供者更改为加载资源提供者

Github PR: [ emqx/emqx-rule-engine#26 ](https://github.com/emqx/emqx-rule-engine/pull/26)

- 优化 actions 的输入数据

Github PR: [ emqx/emqx-rule-engine#27 ](https://github.com/emqx/emqx-rule-engine/pull/27)

### emqx-rel

问题修复:

- 修复修改 log.rotation.size 后启动失败的问题

Github PR: [ emqx/emqx-rel#336 ](https://github.com/emqx/emqx-rel/pull/336)

## 3.1-rc.2 版本

_发布日期: 2019-04-13_

EMQ X 3.1-rc.2 版本发布。此版本改动主要包括规则引擎增强、错误修复。

功能改进:

- 重新设计 emqx_bridge 的 ensure_start 与 ensure_stop API

Github PR: [ emqx/emqx#2423 ](https://github.com/emqx/emqx/pull/2423)

- 提供 Handler 以扩展 emqx_bridge

Github PR: [ emqx/emqx#2414 ](https://github.com/emqx/emqx/pull/2414)

问题修复:

- 修复 metrics 在某些情况下没有正确更新的问题

Github PR: [ emqx/emqx#2416 ](https://github.com/emqx/emqx/pull/2416)

- 修复 trace log level 无法生效时没有提示的问题

Github PR: [ emqx/emqx#2408 ](https://github.com/emqx/emqx/pull/2408)

### emqx-auth-http (plugin)

功能增强:

- 支持用户的 WebServer 回传 Mountpoint

Github PR: [ emqx/emqx-auth-http#116 ](https://github.com/emqx/emqx-auth-http/pull/116)

### emqx-auth-username (plugin)

功能增强:

- 移除在配置文件中配置默认 username 的功能

Github PR: [ emqx/emqx-auth-username#96 ](https://github.com/emqx/emqx-auth-username/pull/96)

### emqx-auth-clientid (plugin)

功能增强:

- 移除在配置文件中配置默认 clientid 的功能

Github PR: [ emqx/emqx-auth-clientid#81 ](https://github.com/emqx/emqx-auth-clientid/pull/81)

### emqx-rule-engine (plugin)

功能增强:

- 支持标准 POSIX CLI 格式

Github PR: [ emqx/emqx-rule-engine#23 ](https://github.com/emqx/emqx-rule-engine/pull/23)

问题修复:

- 修复 HTTP APIs 中的错误

Github PR: [ emqx/emqx-rule-engine#21 ](https://github.com/emqx/emqx-rule-engine/pull/21)

### emqx-packages (plugin)

问题修复:

- 修复 EMQ X 在 CentOS 中开机启动失败的问题

Github Commit: [ emqx/emqx-packages#64760523ea29ca0ad1d85b763f0e8a8e6954db9c ](https://github.com/emqx/emqx-packages/commit/64760523ea29ca0ad1d85b763f0e8a8e6954db9c)

### emqx-dashboard (plugin)

功能增强:

- 新增 Rule-Engine 前端页面

Github PR: [ emqx/emqx-dashboard#50 ](https://github.com/emqx/emqx-dashboard/pull/50)

- 支持在集群中统一管理 Dashboard 用户

Github PR: [ emqx/emqx-dashboard#48 ](https://github.com/emqx/emqx-dashboard/pull/48)

## 3.1-rc.1 版本

_发布日期: 2019-04-04_

EMQ X 3.1-rc.1 版本发布。此版本改动主要包括规则引擎增强、错误修复、稳定性增强等。

功能改进:

- 支持压缩 WebSocket 消息

Github PR: [ emqx/emqx#2356 ](https://github.com/emqx/emqx/pull/2356)

- etcd 集群支持 SSL 连接

Github PR: [ emqx/emqx#2367 ](https://github.com/emqx/emqx/pull/2367)

- 支持 Websocket 的 proxy protocol

Github PR: [ emqx/emqx#2372 ](https://github.com/emqx/emqx/pull/2372)

问题修复:

- 修复 monitor 模块中的错误逻辑

Github PR: [ emqx/emqx#2353 ](https://github.com/emqx/emqx/pull/2353)

- 修复 allow_anonymous 功能不符合预期的问题

Github PR: [ emqx/emqx#2355 ](https://github.com/emqx/emqx/pull/2355)

- 修复 session 进程中无法一次性接收多个消息的问题

Github PR: [ emqx/emqx#2373 ](https://github.com/emqx/emqx/pull/2373)

- 修复 message.dropped 的 hook 在某些情况下不会被触发的问题

Github PR: [ emqx/emqx#2399 ](https://github.com/emqx/emqx/pull/2399)

### emqx-auth-http (plugin)

功能增强:

- 支持从 SSL 双向连接中取出 Subject Name 与 Common Name 用于认证

Github PR: [ emqx/emqx-auth-http#113 ](https://github.com/emqx/emqx-auth-http/pull/113)

### emqx-auth-clientid (plugin)

功能增强:

- 支持通过 REST API 操作 ClientId

Github PR: [ emqx/emqx-auth-clientid#78 ](https://github.com/emqx/emqx-auth-clientid/pull/78)

### emqx-auth-jwt (plugin)

功能增强:

- 支持验证指定的 claims 字段

Github PR: [ emqx/emqx-auth-jwt#69 ](https://github.com/emqx/emqx-auth-jwt/pull/69)

### emqx-rule-engine (plugin)

功能增强:

- 增强规则引擎

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)

### emqx-rel

错误修复:

- 修复 Windows 环境下 EMQ X 需要启动两次的问题

Github Commit: [ emqx/emqx-rel#75de3441db9bf03d489609dcbb340a74de263508 ](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

- 修复 Windows 环境下 EMQ X 安装路径含有中文或空格时无法启动的问题

Github Commit: [ emqx/emqx-rel#75de3441db9bf03d489609dcbb340a74de263508 ](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

## 3.1-beta.3 版本

_发布日期: 2019-03-22_

EMQ X 3.1-beta.3 版本发布。此版本改动主要包括引入规则引擎，增强插件发现机制，和修复一些问题等。

功能改进:

- 增强插件发现机制

Github PR: [ emqx/emqx#2339 ](https://github.com/emqx/emqx/pull/2339)

问题修复:

- 修复重复清除告警的错误

Github PR: [ emqx/emqx#2332 ](https://github.com/emqx/emqx/pull/2332)

- 修复粘包解析失败的问题

Github PR: [ emqx/emqx#2333 ](https://github.com/emqx/emqx/pull/2333)

- 正确设置 PUBLISH 文件中的 DUP 标识

Github PR: [ emqx/emqx#2337 ](https://github.com/emqx/emqx/pull/2337)

### emqx-rule-engine (plugin)

功能增强:

- 实现规则引擎原型

Github Repository: [ emqx/emqx-rule-engine ](https://github.com/emqx/emqx-rule-engine)

### emqx-lua-hook (plugin)

功能增强:

- 增加认证与 ACL 的 hook

Github PR: [ emqx/emqx-lua-hook#63 ](https://github.com/emqx/emqx-lua-hook/pull/63)

### emqx-auth-mysql (plugin)

问题修复:

- 修复 ACL 功能无法使用的问题

Github PR: [ emqx/emqx-auth-mysql#130 ](https://github.com/emqx/emqx-auth-mysql/pull/130)

## 3.1-beta.2 版本

_发布日期: 2019-03-16_

EMQ X 3.1-beta.2 版本发布。此版本改动主要包括重新设计 hooks, 支持 TLS/PSK 和修复 gen_rpc 的一些问题等。

功能改进:

- 优化 emqx hooks

Github PR: [ emqx/emqx#2309 ](https://github.com/emqx/emqx/pull/2309)

- 支持 TLS/DTLS PSK

Github PR: [ emqx/emqx#2297 ](https://github.com/emqx/emqx/pull/2297)

- 将 Request/Response 从 emqx client 分离

Github PR: [ emqx/emqx#2293 ](https://github.com/emqx/emqx/pull/2293)

错误修复:

- 修复某些情况下集群转发消息时 Broker 可能崩溃的问题

Github issues: [ emqx/emqx#2290 ](https://github.com/emqx/emqx/issues/2290)

Github PR: [ emqx/emqx#2320 ](https://github.com/emqx/emqx/pull/2320)

- 在 Broker 卸载插件并退出前卸载 emqx_alarm_handler

Github PR: [ emqx/emqx#2316 ](https://github.com/emqx/emqx/pull/2316)

- 修复一个与 emqx bridge 相关的错误

Github issues: [ emqx/emqx#2312 ](https://github.com/emqx/emqx/issues/2312)

Github PR: [ emqx/emqx#2313 ](https://github.com/emqx/emqx/pull/2313)

- 终结 inflight full error

Github PR: [ emqx/emqx#2281 ](https://github.com/emqx/emqx/pull/2281)

### emqx-management (plugin)

功能增强:

- 增加默认的 secret 配置

Github PR: [ emqx/emqx-management#58 ](https://github.com/emqx/emqx-management/pull/58)

- 修复插件尚未启动时无法 reload 的问题

Github PR: [ emqx/emqx-management#59 ](https://github.com/emqx/emqx-management/pull/59)

- 插件相关的 HTTP API 由插件各自实现

Github PR: [ emqx/emqx-management#57 ](https://github.com/emqx/emqx-management/pull/57)

- 修复查询 io/max_fds 返回 undefined 的问题

Github issues: [ emqx/emqx-management#2222 ](https://github.com/emqx/emqx-management/issues/2222)

Github PR: [ emqx/emqx-management#54 ](https://github.com/emqx/emqx-management/pull/54)

### emqx-auth-jwt (plugin)

功能增强:

- 优化 JWT 认证插件

Github PR: [ emqx/emqx-auth-jwt#63 ](https://github.com/emqx/emqx-auth-jwt/pull/63)

### emqx-auth-usernmae (plugin)

功能增强:

- 增加 CURD HTTP API 以管理用户名密码

Github PR: [ emqx/emqx-auth-username#82 ](https://github.com/emqx/emqx-auth-username/pull/82)

### emqx-web-hook (plugin)

错误修复:

- 修复格式化消息时的错误

Github issues: [ emqx/emqx-web-hook#93 ](https://github.com/emqx/emqx-web-hook/issues/93)

Github PR: [ emqx/emqx-web-hook#96 ](https://github.com/emqx/emqx-web-hook/pull/96)

### minirest (deps)

错误修复:

- 过滤未启动插件的 HTTP API

Github PR: [ emqx/minirest#12 ](https://github.com/emqx/minirest/pull/12)

### gen_rpc (deps)

错误修复:

- 修复 'gen_rpc' 的 raw socket flags

Github PR: [ emqx/gen_rpc#5 ](https://github.com/emqx/gen_rpc/pull/5)

## 3.1-beta.1 版本

_发布日期: 2019-02-28_

EMQ X 3.1-beta.1 版本发布。此版本主要  针对功能改进，包括引入全新的 Bridge，支持消息批量发送，支持 redis 集群等。

功能改进:

- 引入新的 Bridge 实现，支持 EMQ Broker 节点间桥接和 MQTT 协议间桥接

Github PR: [ emqx/emqx#2199 ](https://github.com/emqx/emqx/pull/2199)

- 支持消息批量发送

Github PR: [ emqx/emqx#2253 ](https://github.com/emqx/emqx/pull/2253)

- 使用 gen_statem behaviour 改进 emqx_connection 模块

Github PR: [ emqx/emqx#2235 ](https://github.com/emqx/emqx/pull/2235)

- 新增资源监控，优化告警处理

Github PR: [ emqx/emqx#2266 ](https://github.com/emqx/emqx/pull/2266)

### emqx-auth-redis

功能改进:

- 支持 redis 集群

Github PR: [ emqx/emqx-auth-redis#93 ](https://github.com/emqx/emqx-auth-redis/pull/93)

### emqx-dashboard

功能改进:

- 为 emqx_dashboard_cli  模块增加测试用例

Github PR: [ emqx/emqx-dashboard#34 ](https://github.com/emqx/emqx-dashboard/pull/34)

### emqx-auth-username

功能改进:

- 增加新的 CLI 以更新 username

Github PR: [ emqx/emqx-auth-username#74 ](https://github.com/emqx/emqx-auth-username/pull/74)

### emqx-auth-clientid

功能改进:

- 增加新的  CLI 以更新 clientid

Github PR: [ emqx/emqx-auth-clientid#59 ](https://github.com/emqx/emqx-auth-clientid/pull/59)

## 3.0.1 版本

_发布日期: 2019-01-25_

EMQ X 3.0.1 版本发布。此版本主要包含功能改进和错误修复。

功能改进:

- 为 emqx edge 增加 +L 虚拟机参数以减少内存

Github PR: [ emqx/emqx#2110 ](https://github.com/emqx/emqx/pull/2110)

- 简化修改日志输出等级的命令

Github PR: [ emqx/emqx#2115 ](https://github.com/emqx/emqx/pull/2115)

- 重构 bridge 代码; 支持 bridge 消息持久化

Github PR: [ emqx/emqx#2160 ](https://github.com/emqx/emqx/pull/2160) , [ emqx/emqx#2117 ](https://github.com/emqx/emqx/pull/2117) , [ emqx/emqx#2113 ](https://github.com/emqx/emqx/pull/2113) , [ emqx/emqx#2108 ](https://github.com/emqx/emqx/pull/2108) , [ emqx/emqx#2053 ](https://github.com/emqx/emqx/pull/2053)

- 优化路由匹配

Github PR: [ emqx/emqx#2124 ](https://github.com/emqx/emqx/pull/2124)

- 改进 'emqx_client' 模块设计

Github PR: [ emqx/emqx#2137 ](https://github.com/emqx/emqx/pull/2137)

- 改进 'emqx_pool' 模块的设计

Github PR: [ emqx/emqx#2138 ](https://github.com/emqx/emqx/pull/2138)

- 改进共享订阅调度实现

Github PR: [ emqx/emqx#2144 ](https://github.com/emqx/emqx/pull/2144)

- 支持重启 emqx 时重新生成配置

Github PR: [ emqx/emqx#2175 ](https://github.com/emqx/emqx/pull/2175)

问题修复:

- 修复对端关闭连接时崩溃的问题

Github PR: [ emqx/emqx#2074 ](https://github.com/emqx/emqx/pull/2074)

- 修复客户端正常断开连接时依旧发送遗嘱消息的问题

Github PR: [ emqx/emqx#2156 ](https://github.com/emqx/emqx/pull/2156)

### emqx-lwm2m

问题修复:

- 移除认证功能

GitHub PR: [ emqx/emqx-lwm2m#14 ](https://github.com/emqx/emqx-lwm2m/pull/14)

### emqx-auth-username

问题修复:

- 支持可选的加密模式

GitHub PR: [ emqx/emqx-auth-usernmae#64 ](https://github.com/emqx/emqx-auth-username/pull/64)

### emqx-auth-clientid

功能改进:

- 支持可选的加密模式

GitHub PR: [ emqx/emqx-auth-clientid#52 ](https://github.com/emqx/emqx-auth-username/pull/52)

### emqx-management

功能改进:

- 增加 'plugins reload \<Name>' CLI 命令，支持重载插件时重新生成配置

Github PR: [ emqx/emqx-management#30 ](https://github.com/emqx/emqx-management/pull/30)

## 3.0.0 版本

_发布日期: 2018-12-22_

EMQ X 3.0.0 版本，重新设计了订阅的 ETS 表，通过重构模块和调节 erlang 虚拟机参数提升了 EMQ 性能

功能改进:

- 将虚拟机参数移动到单独的 vm.args 文件

Github PR: [ emqx/emqx#2033 ](https://github.com/emqx/emqx/pull/2033) , [ emqx/emqx#2057 ](https://github.com/emqx/emqx/pull/2057) , [ emqx/emqx#2070 ](https://github.com/emqx/emqx/pull/2070)

- 为遗嘱消息主题增加格式校验和 ACL 检查

Github PR: [ emqx/emqx#2075 ](https://github.com/emqx/emqx/pull/2075)

- 增加 ACL 检查返回拒绝时是否断开客户端连接的配置选项

Github PR: [ emqx/emqx#2059 ](https://github.com/emqx/emqx/pull/2059)

- 重构 session 监控树

Github PR: [ emqx/emqx#2077 ](https://github.com/emqx/emqx/pull/2077)

- 增加 'active_n' 选项以优化 emqx_connection 的 CPU 占用率

Github PR: [ emqx/emqx#2060 ](https://github.com/emqx/emqx/pull/2060)

- 支持客户端批量下线

Github PR: [ emqx/emqx#2060 ](https://github.com/emqx/emqx/pull/2060)

- 增加订阅表分片机制

Github PR: [ emqx/emqx#2044 ](https://github.com/emqx/emqx/pull/2044)

- 重构 'emqx_gc' 模块

Github PR: [ emqx/emqx#2090 ](https://github.com/emqx/emqx/pull/2090)

问题修复:

- 修复 Topic Alias Maximum 的错误实现

Github PR: [ emqx/emqx#2074 ](https://github.com/emqx/emqx/pull/2074)

- 修复部分情况下不会发送遗嘱消息的错误

Github PR: [ emqx/emqx#2068 ](https://github.com/emqx/emqx/pull/2068)

### emqx-auth-ldap

功能改进:

- 更好的设计

GitHub PR: [ emqx/emqx-auth-ldap#46 ](https://github.com/emqx/emqx-auth-ldap/pull/46)

### emqx-lua-hook

问题修复:

- 修复测试用例

GitHub PR: [ emqx/emqx-lua-hook#45 ](https://github.com/emqx/emqx-lua-hook/pull/45)

### emqx-management

功能改进:

- 为 REST API 增加测试用例，并规范返回的响应格式

Github PR: [ emqx/emqx-management#21 ](https://github.com/emqx/emqx-management/pull/21)

## 3.0-rc.5 版本

_发布日期: 2018-11-30_

EMQ X 3.0-rc.5 版本发布，该版本支持 metrics 的批量提交和修复错误:

功能改进:

- 减小依赖大小

Github PR: [ emqx/emqx#1981 ](https://github.com/emqx/emqx/pull/1981)

- 支持 metrics 的批量提交

Github PR: [ emqx/emqx#2001 ](https://github.com/emqx/emqx/pull/2001)

- 优化 mnesia/ets 的并行读写性能

Github PR: [ emqx/emqx#2006 ](https://github.com/emqx/emqx/pull/2006)

问题修复:

- 修复 emqx_router 中的 'function_clause' 错误

Github PR: [ emqx/emqx#1998 ](https://github.com/emqx/emqx/pull/1998)

- 启动过程中移除 simple 日志句柄

Github PR: [ emqx/emqx#2000 ](https://github.com/emqx/emqx/pull/2000)

- 修复 emqx_reason_codes 模块可能出现参数异常的问题

Github PR: [ emqx/emqx#2008 ](https://github.com/emqx/emqx/pull/2008)

### emqx-passwd

功能改进:

- 支持 Rebar3

GitHub PR: [ emqx/emqx-passwd#6 ](https://github.com/emqx/emqx-passwd/pull/6)

### emqx-web-hook

功能改进:

- 支持 Rebar3

GitHub PR: [ emqx/emqx-web-hook#77 ](https://github.com/emqx/emqx-web-hook/pull/77)

问题修复:

- 修复 emqx-web-hook 发送 HTTP 请求时未携带 username 和 clientid 的错误

GitHub PR: [ emqx/emqx-web-hook#77 ](https://github.com/emqx/emqx-web-hook/pull/77)

### emqx-dashboard

问题修复:

- 修复火狐浏览器无法拷贝应用信息的问题

Github PR: [ emqx/emqx-dashboard#12 ](https://github.com/emqx/emqx-dashboard/pull/12)

### emqx-management

问题修复:

- 修复 clients 的 CLI 错误

Github PR: [ emqx/emqx-management#16 ](https://github.com/emqx/emqx-management/pull/16)

## 3.0-rc.4 版本

_发布日期: 2018-11-24_

EMQ X 3.0-rc.4 版本发布，该版本改进日志功能，部分项目支持 Rebar3 构建:

功能改进:

- 为使用 MQTT v3.1.1`的客户端提供避免 `loop delivery 的功能

Github PR: [ emqx/emqx#1964 ](https://github.com/emqx/emqx/pull/1964)

- 支持使用 username 代替 client_id ，默认不开启

Github PR: [ emqx/emqx#1961 ](https://github.com/emqx/emqx/pull/1961)

- 默认日志类型为 both

Github PR: [ emqx/emqx#1979 ](https://github.com/emqx/emqx/pull/1979)

- 添加控制日志等级的命令行接口

Github PR: [ emqx/emqx#1977 ](https://github.com/emqx/emqx/pull/1977)

- 改进 log tracer 的命令行接口

Github PR: [ emqx/emqx#1973 ](https://github.com/emqx/emqx/pull/1973)

- 优化日志性能

Github PR: [ emqx/emqx#1960 ](https://github.com/emqx/emqx/pull/1960)

问题修复:

- 修复用户属性的类型验证

Github PR: [ emqx/emqx#1969 ](https://github.com/emqx/emqx/pull/1969)

- 修复 max_topic_alias 配置项的错误描述

Github PR: [ emqx/emqx#1962 ](https://github.com/emqx/emqx/pull/1962)

- 当 client_id 为空时，将 proc meta-data 设置为服务端生成的 client_id

Github PR: [ emqx/emqx#1980 ](https://github.com/emqx/emqx/pull/1980)

### emqx-coap

功能改进:

- 支持 Rebar3

GitHub PR: [ emqx/emqx-coap#89 ](https://github.com/emqx/emqx-coap/pull/89)

问题修复:

- 修复 sendfun 参数错误的问题

Github PR: [ emqx/emqx-coap#89 ](https://github.com/emqx/emqx-coap/pull/89)

### emqx-management

问题修复:

- 修复集群模式下通过 REST API 查找连接不稳定的问题

Github PR: [ emqx/emqx-management#11 ](https://github.com/emqx/emqx-management/pull/11)

### ekka

问题修复:

- 修复分布式锁的错误判断

Github PR: [ emqx/ekka#39 ](https://github.com/emqx/ekka/pull/39)

### minirest

功能改进:

- 支持 Rebar3

Github PR: [ emqx/minirest#6 ](https://github.com/emqx/minirest/pull/6)

### cuttlefish

问题修复:

- 将 cuttlefish 的日志输出到 std_error

Github PR: [ emqx/cuttlefish#4 ](https://github.com/emqx/cuttlefish/pull/4)

### emqx-rel

功能改进:

- 构建时更新 cuttlefish

Github PR: [ emqx/emqx-rel#253 ](https://github.com/emqx/emqx-rel/pull/253)

- 默认不启用 delay_publish 插件

Github PR: [ emqx/emqx-rel#251 ](https://github.com/emqx/emqx-rel/pull/251)

## 3.0-rc.3 版本

_发布日期: 2018-11-10_

EMQ X 3.0-rc.3 版本发布，该版本重构 emqx_mqueue 代码，支持 MQTT-SN , CoAP 与 STOMP 协议:

功能改进:

- 将 QOS\$i 替换为 QOS\_\$i

Github PR: [ emqx/emqx#1948 ](https://github.com/emqx/emqx/pull/1948)

- 更新配置文件中 ACL cache 的描述信息

Github PR: [ emqx/emqx#1950 ](https://github.com/emqx/emqx/pull/1950)

- 重构 emqx_mqueue 代码

Github PR: [ emqx/emqx#1926 ](https://github.com/emqx/emqx/pull/1926)

- lager 替换为 OTP logger

Github PR: [ emqx/emqx#1898 ](https://github.com/emqx/emqx/pull/1898)

问题修复:

- 修复重复订阅时的 'badarg' 错误

Github PR: [ emqx/emqx#1943 ](https://github.com/emqx/emqx/pull/1943)

- 修复 emqx_message:format 函数 'badarg' 错误

Github PR: [ emqx/emqx#1954 ](https://github.com/emqx/emqx/pull/1954)

- 修复 MQTT bridge 无法使用 TLS 连接的问题

Github PR: [ emqx/emqx#1949 ](https://github.com/emqx/emqx/pull/1949)

### emqx-stomp

功能改进:

- 增强 receipt 报文支持，增加测试用例

GitHub PR: [ emqx/emqx-stomp#53 ](https://github.com/emqx/emqx-stomp/pull/53)

### emqx-sn

功能改进:

- 增强对 MQTT-SN 协议的支持

GitHub PR: [ emqx/emqx-sn#90 ](https://github.com/emqx/emqx-sn/pull/90)

### emqx-lua-hook

问题修复:

- 修复 emqx-lua-hook 无法正常使用的问题

Github PR: [ emqx/emqx-lua-hook#41 ](https://github.com/emqx/emqx-lua-hook/pull/41)

### emqx-statsd

功能改进:

- 增加统计指标

Github PR: [ emqx/emqx-statsd#4 ](https://github.com/emqx/emqx-statsd/pull/4)

### emqx-dashboard

功能改进:

- 增加 qos2/forward 指标

Github PR: [ emqx/emqx-dashboard#7 ](https://github.com/emqx/emqx-dashboard/pull/7)

### emqx-auth-pgsql

问题修复:

- 修复并发量大时 emqx-auth-pgsql 出错的问题

Github PR: [ emqx/emqx-auth-pgsql#94 ](https://github.com/emqx/emqx-auth-pgsql/pull/94)

## 3.0-rc.2 版本

_发布日期: 2018-10-27_

EMQ X 3.0-rc.2 版本发布，该版本改进 Will Message 发布机制，新增支持使用 ssl 证书作为 MQTT 用户名:

功能改进:

- 改进 Will Message 发布机制，增加取消发布处理

Github PR: [ emqx/emqx#1889 ](https://github.com/emqx/emqx/pull/1889)

- 新增支持使用 ssl 证书作为 MQTT 用户名

Github PR: [ emqx/emqx#1913 ](https://github.com/emqx/emqx/pull/1913)

- 提升代码测试覆盖率

Github PR: [ emqx/emqx#1921 ](https://github.com/emqx/emqx/pull/1921)

问题修复:

- 修复 emqx_broker:subscribed 函数 'bad argument' 错误

Github PR: [ emqx/emqx#1921 ](https://github.com/emqx/emqx/pull/1921)

## 3.0-rc.1 版本

_发布日期: 2018-10-20_

EMQ X 3.0-rc.1 版本发布，该版本新增 request & response 以及 LwM2M 插件，修复 PUBLISH 验证问题:

功能改进:

- 为 CONNECT & CONNACK 报文添加 request & response 支持

Github PR: [ emqx/emqx#1819 ](https://github.com/emqx/emqx/pull/1819)

- 为未认证的订阅添加警告信息

Github PR:

[ emqx/emqx#1878 ](https://github.com/emqx/emqx/pull/1878)

- 增加 emqx_hooks 的测试覆盖率, 为 emqx_mod_sup 模块增加测试用例

Github PR:

[ emqx/emqx#1892 ](https://github.com/emqx/emqx/pull/1892)

问题修复:

- 更新 ACL 文档链接

Github PR: [ emqx/emqx#1899 ](https://github.com/emqx/emqx/pull/1899)

- 修复验证 PUBLISH 报文时的匹配问题

Github PR: [ emqx/emqx#1888 ](https://github.com/emqx/emqx/pull/1888)

- 修复某些情况下不返回 Reason Code 给 client 的 BUG

Github PR: [ emqx/emqx#1819 ](https://github.com/emqx/emqx/pull/1819)

- 修复 emqx_client 模块中的兼容性问题

Github PR: [ emqx/emqx#1819 ](https://github.com/emqx/emqx/pull/1819)

### emqx-lwm2m

- 更新 LwM2M 插件以适配 EMQ X 3.0

Github PR: [ emqx/emqx-lwm2m#3 ](https://github.com/emqx/emqx-lwm2m/pull/3)

## 3.0-Beta.4 版本

_发布日期: 2018-09-29_

EMQ X 3.0-beta.4 版本发布，该版本改进连接 Shutdown 策略，改进共享订阅 sticky 策略，修复 Delayed Publish 问题：

功能改进:

- 为进程自定义 max_heap_size

GitHub issues: [ emqx/emqx#1855 ](https://github.com/emqx/emqx/pull/1855)

- 改进 Topic 别名 Maximum、连接 Receive Maximum

GitHub issues: [ emqx/emqx#1873 ](https://github.com/emqx/emqx/pull/1873)

- 修复共享订阅 sticky 策略 pick ID 方式

GitHub issues: [ emqx/emqx#1871 ](https://github.com/emqx/emqx/pull/1871)

- 为 Zone 新增 Mountpoint 配置

GitHub issues: [ emqx/emqx#1869 ](https://github.com/emqx/emqx/pull/1869)

- 修复 make app.config 错误

GitHub issues: [ emqx/emqx#1868 ](https://github.com/emqx/emqx/pull/1868) ,

- 修复 Hooks 回调参数错误

GitHub issues: [ emqx/emqx#1866 ](https://github.com/emqx/emqx/pull/1866)

- 改进 travis 构建支持 rebar3 xref

GitHub issues: [ emqx/emqx#1861 ](https://github.com/emqx/emqx/pull/1861)

- 升级依赖库 esockd 至 v5.4.2

GitHub issues: [ emqx/emqx#1875 ](https://github.com/emqx/emqx/pull/1875)

- 升级依赖库 erlang-bcrypt 至 0.5.1

GitHub issues: [ emqx/emqx-passwd#3 ](https://github.com/emqx/emqx-passwd/pull/3)

### emqx-delayed-publish

- 修复消息延时发布

GitHub issues: [ emqx/emqx-delayed-publish#5 ](https://github.com/emqx/emqx-delayed-publish/pull/5)

### emqx-passwd

- 改进 check_pass 方式，供各类认证插件调用

GitHub issues: [ emqx/emqx-passwd#3 ](https://github.com/emqx/emqx-passwd/pull/3)

### bcrypt

- 改进 bcrypt 验证方式

GitHub issues: [ emqx/erlang-bcrypt#1 ](https://github.com/emqx/erlang-bcrypt/pull/1)

### esockd

- 新增 DTLS PSK 样例

GitHub issues: [ emqx/esockd#88 ](https://github.com/emqx/esockd/pull/88)

- 修复 DTLS 启动失败

GitHub issues: [ emqx/esockd#89 ](https://github.com/emqx/esockd/pull/89)

- 改进 SSL 启动方式

GitHub issues: [ emqx/esockd#90 ](https://github.com/emqx/esockd/pull/90)

## 3.0-Beta.3 版本

_发布日期: 2018-09-22_

EMQ X 3.0-beta.3 版本发布，该版本新增共享订阅派发策略功能，改进 GC 策略、桥接设计:

功能改进:

- 修复 travis 构建

GitHub issues: [ emqx/emqx#1818 ](https://github.com/emqx/emqx/pull/1818)

- 更新模块 emqx_mqueue.erl 文档说明

GitHub issues: [ emqx/emqx#1815 ](https://github.com/emqx/emqx/pull/1815)

- 新增共享订阅派发策略

GitHub issues: [ emqx/emqx#1823 ](https://github.com/emqx/emqx/pull/1823)

- 修复 emqx_pool 模块参数错误

GitHub issues: [ emqx/emqx#1827 ](https://github.com/emqx/emqx/pull/1827)

- 新增强制 shutdown 策略

GitHub issues: [ emqx/emqx#1836 ](https://github.com/emqx/emqx/pull/1836)

- 改进 KeepAlive 检测算法

GitHub issues: [ emqx/emqx#1839 ](https://github.com/emqx/emqx/pull/1839)

- 修复跨节点消息投递

GitHub issues: [ emqx/emqx#1846 ](https://github.com/emqx/emqx/pull/1846)

- 改进 Bridge 设计

GitHub issues: [ emqx/emqx#1849 ](https://github.com/emqx/emqx/pull/1849)

- 改进 force_gc_policy 配置

GitHub issues: [ emqx/emqx#1851 ](https://github.com/emqx/emqx/pull/1851)

- 修复 Maximum-QoS 选项值

GitHub issues: [ emqx/emqx#1852 ](https://github.com/emqx/emqx/pull/1852)

- 升级依赖库 esockd 至 v5.4.1

GitHub issues: [ emqx/emqx#1858 ](https://github.com/emqx/emqx/pull/1858)

问题修复:

- 订阅 API，主题属性支持通配符

GitHub issues: [ emqx/emqx#1706 ](https://github.com/emqx/emqx/issues/1706)

- WebSocket 连接新增 Path 配置

GitHub issues: [ emqx/emqx#1809 ](https://github.com/emqx/emqx/issues/1809)

- 修复报文尺寸过大导致 block 问题

GitHub issues: [ emqx/emqx#1811 ](https://github.com/emqx/emqx/issues/1811)

- 新增函数 check_expiry

GitHub issues: [ emqx/emqx#1813 ](https://github.com/emqx/emqx/issues/1813)

- 修复 DISCONNECT 报文 Session Expiry Interval 不起作用

GitHub issues: [ emqx/emqx#1833 ](https://github.com/emqx/emqx/issues/1833)

- 修复 DISCONNECT 报文 Max Session Expiry Interval 不起作用

GitHub issues: [ emqx/emqx#1834 ](https://github.com/emqx/emqx/issues/1834)

### emqx-management

- 改进 Bridge CTL 命令

- 修复函数调用 emqx_mgmt_cli:print() crash 问题

- 修复 emqx_mgmt:subscribe 函数'function_clause'错误

GitHub issues: [ emqx/emqx-management#1815 ](https://github.com/emqx/emqx-management/pull/7)

### emqx-web-hook

修复加载 emqx_web_hook 错误

### emqx-dashboard

- 修复 Dashboard -> OverView 中 disconnect 统计数据显示

- 在 Dashboard -> Websocket 新增 WebSocket Path 参数

GitHub issues: [ emqx/emqx-dashboard#5 ](https://github.com/emqx/emqx-dashboard/pull/5)

### emqx-retainer

- Retained 消息新增 TTL

GitHub issues: [ emqx/emqx-retainer#52 ](https://github.com/emqx/emqx-retainer/issues/52)

### emqx-coap

- 新增 emqx_coap 插件

GitHub issues: [ emqx/emqx-coap#5 ](https://github.com/emqx/emqx-coap/pull/86) [ emqx/gen-coap#5 ](https://github.com/emqx/gen_coap/pull/8)

### emqx-docker

- 优化 Dockerfile

GitHub issues: [ emqx/emqx-docker#5 ](https://github.com/emqx/emqx-docker/pull/71)

### esockd

- 改进 esockd_connection_sup 模块设计

GitHub issues: [ emqx/esockd#86 ](https://github.com/emqx/esockd/pull/86)

## 3.0-Beta.2 版本

_发布日期: 2018-09-10_

EMQ X 3.0-Beta.2 版本主要包含了对 MQTT 5.0 新特性的改进，以及问题修复。

### EMQ X Core

功能改进:

- 支持 MQTT 5.0 'subscription options'

GitHub issues: [ emqx/emqx#1788 ](https://github.com/emqx/emqx/pull/1788) , [ emqx/emqx-retainer#58 ](https://github.com/emqx/emqx-retainer/pull/58) , [ emqx/emqx#1803 ](https://github.com/emqx/emqx/pull/1803)

- 增加对 MQTT 5.0 'Topic-Alias' 的校验

GitHub issues: [ emqx/emqx#1789 ](https://github.com/emqx/emqx/pull/1789) , [ emqx/emqx#1802 ](https://github.com/emqx/emqx/pull/1802)

- 改进 hooks 的设计

GitHub issue: [ emqx/emqx#1790 ](https://github.com/emqx/emqx/pull/1790)

- 将模块 'emqx_mqtt_properties' 重命名为 'emqx_mqtt_props'

GitHub issue: [ emqx/emqx#1791 ](https://github.com/emqx/emqx/pull/1791)

- 改进 emqx_zone

GitHub issue: [ emqx/emqx#1795 ](https://github.com/emqx/emqx/pull/1795)

问题修复:

- 修复了 'Will Delay Interval' 属性处理错误

GitHub issues: [ emqx/emqx#1800 ](https://github.com/emqx/emqx/pull/1800) , [ emqx/emqx-delayed-publish#3 ](https://github.com/emqx/emqx-delayed-publish/pull/3)

- 修复了 'Reserved' 标志位的处理错误

GitHub issue: [ emqx/emqx#1783 ](https://github.com/emqx/emqx/pull/1783)

- 为单元测试生成配置文件

GitHub issue: [ emqx/emqx#1794 ](https://github.com/emqx/emqx/pull/1794)

### emqx-management (插件)

功能改进:

- 增加 'banned' 功能的 restful API

GitHub issue: [ emqx/emqx-management#6 ](https://github.com/emqx/emqx-management/pull/6)

### emqx-delayed-publish (插件)

功能改进:

- 重构代码

GitHub issue: [ emqx/emqx-delayed-publish#4 ](https://github.com/emqx/emqx-delayed-publish/pull/4)

### minirest (依赖工程)

功能改进:

- 回调函数里，同时传递 query 参数和 body 参数

GitHub issue: [ emqx/minirest#4 ](https://github.com/emqx/minirest/pull/4)

### emqx-rel (编译工程)

功能改进:

- 编译时检查 OTP 版本

GitHub issue: [ emqx/emqx-rel#217 ](https://github.com/emqx/emqx-rel/pull/217)

## 3.0-Beta.1 版本

_发布日期: 2018-09-02_ 版本别名: The Promise of Tomorrow

3.0-beta.1 版本正式发布。兼容 MQTT-3.1.1 协议的同时， 完整支持 MQTT-5.0 协议。 此外还增加了很多实用的功能特性，重构了核心组件，提升了系统的伸缩性和扩展能力。

### 全面支持 MQTT-5.0

EMQX 3.0 版本实现了大多数的 MQTT-5.0 特性，主要的 MQTT-5.0 新特性一览:

- 增加了新的 MQTT 控制报文类型: AUTH

MQTT-5.0 里新增加了一个 AUTH 类型的报文，用来实现相对复杂的认证交互流程。

- Session 过期机制

之前版本的 "Clean session flag" 现在拆分成了两个字段: "Clean Start Flag"，"Session Expiry Interval"。

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

EMQ X 2.x 里可以配置 Broker 端的最大报文限制，过大的报文会被丢弃，然后 Broker 会将连接断开。MQTT-5.0 里，通过 CONNECT/CONNECT ACK 报文，客户端和 Broker 端都可以指定最大报文限制。

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

### 集群架构演进

EQMX 3.0 引入了伸缩性较强的 RPC 机制，现在单集群可以支持千万级别的并发连接:

    --------               --------
    |  EMQX  |<--- MQTT--->|  EMQX  |
    |--------|             |--------|
    |  Ekka  |<----RPC---->|  Ekka  |
    |--------|             |--------|
    | Mnesia |<--Cluster-->| Mnesia |
    |--------|             |--------|
    | Kernel |<----TCP---->| Kernel |
    --------               --------

引入 Ekka 以实现集群的自动建立和自动恢复。目前支持以下几种集群建立方式:

- manual: 手动加入集群;
- static: 使用预置的节点列表自动组建集群;
- mcast: 使用广播自动建立集群;
- dns: 使用 DNS A 记录自动建立集群;
- etcd: 使用 etcd 自动建立集群;
- k8s: 使用 k8s 自动建立集群。

### 消息速率限制

3.0 引入了消息速率限制，以增加 Broker 的可靠性。在 MQTT TCP 或 SSL 监听器配置里，可以配置:

- 并发连接数量: max_clients
- 连接速率限制: max_conn_rate
- 连接流量限制: rate_limit
- 发布速率限制: max_publish_rate

### Feature improvements and Bug Fixes

- 更新了 esockd;
- 改用 cowboy 以提升 HTTP 连接的性能;
- 重构了 ACL 缓存机制;
- 增加本地和远程 MQTT 桥接功能;
- 配置文件引入 "zone" 的概念，不同的 "zone" 可以使用不同的配置;
- 重构了 session 模块，减少了节点间的内存拷贝，提升了节点间通信效率;
- 改进了 OpenLDAP 的 Access Control;
- 增加了延时发布功能;
- 增加了支持 Prometheus 的新的监控和统计功能;
- 改进了 hook 机制。

## 2.3.11 版本

_发布日期: 2018-07-23_

### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

### emq-auth-jwt

Fix token verification with jwerl 1.0.0

### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo#123)

Support both clientid and username variables in all queries. (emq-auth-mongo#123)

## 2.3.10 版本

_发布日期: 2018-06-27_

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9 版本

_发布日期: 2018-05-20_

### Bugfix and Enhancements

Bugfix: check params for REST publish API (#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (#78)

## 2.3.8 版本

_发布日期: 2018-05-11_

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq_auth_username (#1588)

Bugfix: Should be an info level when change CleanSession (#1590)

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (#116)

## 2.3.7 版本

_发布日期: 2018-04-22_

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

## 2.3.6 版本

_发布日期: 2018-03-25_

### Bugfix and Enhancements

Security: LWT message checking the ACL (#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529)

### emq-auth-jwt

Validate JWT token using a expired field (#29)

## 2.3.5 版本

_发布日期: 2018-03-03_

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

## 2.3.4 版本

_发布日期: 2018-01-29_

### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket (#1335)

Feature: EMQ node.name with link local ipv6 address not responding to ping (#1460)

Feature: Add PROTO_DIST_ARG flag to support clustering via IPv6 address. (#1460)

Bugfix: retain bit is not set when publishing to clients (when it should be set). (#1461)

Bugfix: Can't search topic on web dashboard (#1473)

### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn#66)

## 2.3.3 版本

_发布日期: 2018-01-08_

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

## 2.3.2 版本

_发布日期: 2017-12-26_

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

## 2.3.1 版本

_发布日期: 2017-12-03_

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

## 2.3.0 版本 "Passenger's Log"

_发布日期: 2017-11-20_

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

## 2.3-rc.2 版本

_发布日期: 2017-10-22_

### Bugfix

Change the default logging level of trace CLI. (emqttd#1306)

### emq-dashboard Plugin (emq-dashboard#164)

Fix the 'Status' filters of plugins's management.

Fix the URL Redirection when deleting an user.

Compatible with IE,Safari,360 Browsers.

## 2.3-rc.1 版本

_发布日期: 2017-10-12_

### Bugfix

Fix the issue that invalid clients can publish will message. (emqttd#1230)

Fix Dashboard showing no stats data (emqttd#1263)

Fix a rare occurred building failure (emqttd#1284)

Support Persistence Logs for longer time (emqttd#1275)

Fix for users APIs (emqttd#1289)

Changed passwd_hash/2 function's return type (emqttd#1289)

### emq-dashboard Plugin (emq-dashboard#154)

Improved the Dashboard Interface of Monitoring/Management/Tools.

Allow switching dashboard themes.

Supoort both EN and CN languages.

## 2.3-beta.4 版本

_发布日期: 2017-09-13_

### Highlights

Released a new sexy dashboard.

Add more RESTful APIs for manangement and monitoring.

Configuring the broker through CLI or API without having to restart.

### Bugfix

Job for emqttd.service failed because the control process exited with error code. (emqttd#1238)

Travis-CI Build Failing (emqttd#1221)

Https listener of Dashboard plugin won't work (emqttd#1220)

Service not starting on Debian 8 Jessie (emqttd#1228)

### emq-dashboard

1. Support switching to other clustered node.
2. Configure and reboot the plugins on the dashboard.
3. A login page to replace the basic authentication popup window.

### emq-coap

1.Try to clarify the relationship between coap and mqtt in EMQ. (emq-coap#54).

2.Fix crashes in coap concurrent test(gen-coap#3).

## 2.3-beta.3 版本

_发布日期: 2017-08-21_

## 2.3-beta.3 版本

_发布日期: 2017-08-21_

### Enhancements

Add HTTP API for hot configuration.

### Bugfix

1. Parse 'auth.mysql.password_hash' error when hot configuration reload (emq-auth-mysql#68)
2. Set 'auth.pgsql.server' error when hot configuration reload (emq-auth-pgsql#67)
3. Set 'auth.redis.server' and 'auth.redis.password_hash' error when hot configuration reload (emq-auth-redis#47)
4. Fix the issue that when deleting retained message subscribed clients are not notified (emqttd#1207)
5. Support more parameters for hot configuration reload:

- mqtt.websocket_protocol_header = on
- mqtt.mqueue.low_watermark = 20%
- mqtt.mqueue.high_watermark = 60%
- mqtt.client.idle_timeout = 30s
- mqtt.client.enable_stats = off

## 2.3-beta.2 版本

_发布日期: 2017-08-12_

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

::: tip
为支持命令行更新配置参数，部分认证插件参数值采用','替代了空格分隔符。
:::

### Enhancements

1. Introduce new HTTP management API.
2. Add ClientId parameter for HTTP Publish API.
3. Allow configuring keepalive backoff.
4. Remove the fullsweep_after option to lower CPU usage.
5. Authorize HTTP Publish API with clientId.

### emq-sn Plugin (emq-sn#49)

1. Support CONNECT message in connected/wait_for_will_topic/wait_for_will_msg states.
2. Clean registered topic for a restarted client.
3. Bug fix of not clearing buffered PUBLISH messages received during asleep state as those messages are sent to client when client wakes up.

### emq-auth-ldap Plugin (emq-auth-ldap#21)

Improve the design LDAP authentication.

### emq-coap Plugin (emq-coap#51)

Support CoAP PubSub Specification ( [ https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt ](https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt) )

## 2.3-beta.1 版本

_发布日期: 2017-07-24_

EMQ R2.3-beta.1 版本发布！该版本正式支持集群节点自动发现与集群脑裂自动愈合，支持基于 IP Multicast、Etcd、Kubernetes 等多种策略自动构建集群。

### 节点发现与自动集群

EMQ R2.3 版本支持多种策略的节点自动发现与集群:

| 策略   | 说明                    |
| ------ | ----------------------- |
| static | 静态节点列表自动集群    |
| mcast  | UDP 组播方式自动集群    |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### 集群脑裂与自动愈合

EMQ R2.3 版本正式支持集群脑裂自动愈合(Network Partition Autoheal):

    cluster.autoheal = on

集群脑裂自动恢复流程:

1. 节点收到 Mnesia 库的'inconsistent_database'事件 3 秒后进行集群脑裂确认；
2. 节点确认集群脑裂发生后，向 Leader 节点(集群中最早启动节点)上报脑裂消息；
3. Leader 节点延迟一段时间后，在全部节点在线状态下创建脑裂视图(SplitView)；
4. Leader 节点在多数派(Majority)分区选择集群自愈的 Coordinator 节点；
5. Coordinator 节点重启少数派(minority)分区节点恢复集群。

### 节点宕机与自动清除

EMQ R2.3 版本支持从集群自动删除宕机节点(Autoclean):

    cluster.autoclean = 5m

### LWM2M 协议支持

EMQ R2.3 版本正式支持 LWM2M 协议网关，实现了 LWM2M 协议的大部分功能。MQTT 客户端可以通过 EMQ-LWM2M 访问支持 LWM2M 的设备。设备也可以往 EMQ-LWM2M 上报 notification，为 EMQ 后端的服务采集数据。

LWM2M 是由 Open Mobile Alliance(OMA)定义的一套适用于物联网的协议，它提供了设备管理和通讯的功能。LWM2M 使用 CoAP 作为底层的传输协议，承载在 UDP 或者 SMS 上

### JSON Web Token 认证支持

EMQ R2.3 版本支持基于 JWT(JSON Web Token)的 MQTT 客户端认证。

### Retainer 插件

Retainer 插件支持'disc_only'模式存储 retained 消息。

### Debian 9 安装包

EMQ R2.3 支持 Debian 9 系统安装包。

### Erlang/OTP R20

EMQ R2.3 版本兼容 Erlang/OTP R20，全部程序包基于 Erlang/OTP R20 构建。

## 2.2 正式版 "Nostalgia"

_发布日期: 2017-07-08_

_版本别名: Nostalgia_

EMQ-2.2.0 版本正式发布！EMQ R2.2 版本完整支持 CoAP(RFC 7252)、MQTT-SN 协议，支持 Web Hook、Lua Hook、Proxy Protocol V2，支持 Elixir 语言插件开发。

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135)

Bugfix: Exit Code from emqttd_ctl (emqttd#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43)

## 2.2-rc.2 版本

_发布日期: 2017-07-03_

::: warning
2.2-rc.2 版本源码编译需要 Erlang/OTP R19.3+
:::

### 问题与改进

Compatible with Erlang/OTP R20 (emq-relx#77)

CoAP gateway plugin supports coap-style publish & subscribe pattern. (emq_coap#33)

MQTT-SN gateway plugin supports sleeping device (emq_sn#32)

Upgrade esockd and mochiweb libraries to support restarting a listener

## 2.2-rc.1 版本

_发布日期: 2017-06-14_

### 问题与改进

Add a new listener for HTTP REST API (emqttd#1094)

Fix the race condition issue caused by unregister_session/1 (emqttd#1096)

Fix the issue that we cannot remove a down node from the cluster (emqttd#1100)

Passed org.eclipse.paho.mqtt_sn.testing/interoperability tests (emq_sn#29)

Fix the issue that send http request and return non-200 status code, but AUTH/ACL result is denied (emq-auth-http#33)

Fix the issue that fail to stop listener (emq_stomp#24)

Support using systemctl to manage emqttd service on CentOS

## 2.2-beta.3 版本

_发布日期: 2017-05-27_

### 问题与改进

Call emit_stats when force GC (emqttd#1071)

Update the default value of 'mqtt.mqueue.max_length' to 1000 (emqttd#1074)

Update emq-auth-mongo READEME (emq-auth-mongo#66)

Update default password field (emq-auth-mongo#67)

Upgrade the mongodb library to v3.0.3

Remove ‘check password===undefined && userName!== undefined’ (emq-dashboard#120)

### emq_auth_redis 插件

认证支持 HGET 查询

### emq_auth_mongo 插件

支持 mongodb 集群、Replica Set

### 文档更新

更新 Windows 源码编译安装

## 2.2-beta.2 版本

_发布日期: 2017-05-20_

### 问题与改进

Add a 'websocket_protocol_header' option to handle WebSocket connection from WeChat (emqttd#1060)

Assign username and password to MQTT-SN's CONNECT message (emqttd#1041)

Allow for Content-Type:application/json in HTTP Publish API (emqttd#1045)

emqttd_http.erl:data conversion (emqttd#1059)

Seperate emq_sn from emqttd (emq-sn#24)

Check St0's type, making it easier to debug crash problems (emq-lua-hook#6)

Fix error: load xxx.lua (emq-lua-hook#8)

Leave luerl alone as a rebar project (emq-lue-hook#9)

Display websocket data in reverse order (emq-dashboard#118)

priv/www/assets/js/dashboard.js:Fixed a typo (emq-dashboard#118)

### Update README

Update README of emq-auth-pgsql: add the 'ssl_opts' configuration (emq-auth-pgsql#56)

Update README of emq-auth-mysql: fix the 'passwd_hash' typo (emq-auth-mysql#54)

Update README of emq-auth-mongo: change 'aclquery' to 'acl_query' (emq-auth-mongo#63)

### Elixir Plugin

Add a new plugin [ emq-elixir-plugin ](https://github.com/emqtt/emq-elixir-plugin) to support Elixir language.

## 2.2-beta.1 版本

_发布日期: 2017-05-05_

EMQ 2.2-beta.1 版本正式发布！EMQ2.2 版本发布主要新功能包括:

1. 支持 MQTT 协议多监听器配置，支持 HAProxy 的 Proxy Protocol V1/V2
2. 新增 Web Hook 插件(emq-web-hook)、Lua Hook 插件(emq-lua-hook)

### MQTT 协议监听器配置

一个 EMQ 节点可配置多个 MQTT 协议监听端口，例如下述配置 external, internal 监听器，分别用于设备连接与内部通信:

    -------
    -- Ex，支持Web Hook、Lua Hook、ernal TCP 1883 --> |     |
    | EMQ | -- Internal TCP 2883 --> Service
    -- External SSL 8883-->  |     |
    -------

EMQ 2.2 版本 etc/emq.conf 监听器配置方式:

    listener.tcp.${name}= 127.0.0.1:2883

    listener.tcp.${name}.acceptors = 16

    listener.tcp.${name}.max_clients = 102400

### Proxy Protocol V1/2 支持

EMQ 集群通常部署在负载均衡器(LB)后面，典型架构:

    -----
    |   |
    | L | --TCP 1883--> EMQ
    --SSL 8883--> |   |                |
    | B | --TCP 1883--> EMQ
    |   |
    -----

HAProxy、NGINX 等常用的负载均衡器(LB)，一般通过 Proxy Protocol 协议传递 TCP 连接源地址、源端口给 EMQ。

EMQ 2.2 版本的监听器开启 Proxy Protocol 支持:

    ## Proxy Protocol V1/2
    ## listener.tcp.${name}.proxy_protocol = on
    ## listener.tcp.${name}.proxy_protocol_timeout = 3s

### Web Hook 插件

新增 WebHook 插件: [ emq-web-hook ](https://github.com/emqtt/emq-web-hook) ，支持在 MQTT 客户端上下线、消息发布订阅时触发 WebHook 回调。

### Lua Hook 插件

新增 Lua Hook 插件: [ emq-lua-hook ](https://github.com/emqtt/emq-lua-hook) ，支持 Lua 脚本注册 EMQ 扩展钩子来开发插件。

### 改进认证链设计

EMQ 2.2 版本改进认证链设计，当前认证模块返回 ignore(例如用户名不存在等情况下)，认证请求将继续转发后面认证模块:

    -------------           ------------           -------------
    Client --> | Redis认证 | -ignore-> | HTTP认证 | -ignore-> | MySQL认证 |
    -------------           ------------           -------------
          |                       |                       |
         \|/                     \|/                     \|/
    allow | deny            allow | deny            allow | deny

### 支持 bcrypt 密码 Hash

EMQ 2.2 版本支持 bcrypt 密码 Hash 方式，例如 Redis 认证插件配置:

    auth.redis.password_hash = bcrypt

### etc/emq.conf 配置变更

'mqtt.queue. _' 配置变更为 'mqtt.mqueue._ '

### emq-dashboard

WebSocket 页面支持 Unsubscribe

## 2.1.2 版本

_发布日期: 2017-04-21_

Fix emqttd_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd#1000)

Fix crash caused by duplicated PUBREC packet (emqttd#1004)

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template)

## 2.1.1 版本

_发布日期: 2017-04-14_

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105)

Fix mqtt-sn Gateway not working (emq-sn#12)

Upgrade emq-sn Plugin (emq-sn#11)

Upgrade emq-coap Plugin (emq-coap#21)

## 2.1.0 版本

_发布日期: 2017-04-07_

The stable release of 2.1 version.

Trouble with auth.mysql.acl_query (emq-auth-mysql#38)

Filter the empty fields in ACL table (emq-auth-mysql#39)

## 2.1.0-rc.2 版本

_发布日期: 2017-03-31_

Support pbkdf2 hash (emq-auth-mongo#46)

Kickout the conflict WebSocket connection (emqttd#963)

Correct licence in app.src (emqttd#958)

SSL options to connect to pgsql (emq-auth-pgsql#41)

## 2.1.0-rc.1 版本

_发布日期: 2017-03-24_

EMQ fails to start if run under a different linux user than that which first ran it (emqttd#842)

Depend on emqtt/pbkdf2 to fix the building errors of Travis CI (emqttd#957)

Depend on goldrush and emqtt/pbkdf2 to resolve the building errors (emqttd#956)

Fix 'rebar command not found' (emq-relx#33)

Compile error in v2.1.0-beta.2 (emq-relx#32)

Support salt with passwords (emq-auth-mongo#11)

Change the default storage_type to 'ram' (emq-retainer#13)

## 2.1.0-beta.2 版本

_发布日期: 2017-03-13_

Cannot find AwaitingAck (emqttd#597)

EMQ V2.1 crash when public with QoS = 2 (emqttd#919)

Support pbkdf2 hash (emqttd#940)

Add src/emqttd.app.src to be compatible with rebar3 (emqttd#920)

Add more test cases (emqttd#944)

CRASH REPORT Process \<0.1498.0> with 0 neighbours crashed with reason: {ssl_error,{tls_alert,"certificate unknown"}} in esockd_connection:upgrade (emqttd#915)

'auth.redis.password_hash = plain' by default (emq-auth-redis#20)

## 2.1.0-beta.1 版本

_发布日期: 2017-02-24_

EMQ 2.1.0-beta.1 版本发布。

::: warning
2.1.x 版本源码编译需要 Erlang/OTP R19+
:::

EMQ 正式采用 [ Semantic Versioning 2.0.0 ](http://semver.org) 规范创建发布版本号，按'Tick-Tock'方式按月发布迭代版本。奇数版本问题修复与性能改进，偶数版本架构改进和新功能布。

### GC 优化

1. WebSocket、Client、Session 进程空置一段时间后自动 Hibernate 与 GC。
2. 新增'mqtt.conn.force_gc_count'配置，Client、Session 进程处理一定数量消息后强制 GC。
3. 大幅降低 WebSocket、Client、Session 进程 fullsweep_after 设置，强制进程深度 GC。

### API 改进

Hooks API 支持注册带 Tag 的回调函数，解决相同模块函数多次 Hook 注册问题。

### 问题修复

emqttd#916: Add 'mqtt_msg_from()' type

emq-auth-http#15: ACL endpoint isnt called

## 2.1-beta 版本

_发布日期: 2017-02-18_

EMQ v2.1-beta 版本正式发布，改进 Session/Inflight 窗口设计，一个定时器负责全部 Inflight QoS1/2 消息重传，大幅降低高消息吞吐情况下的 CPU 占用。

### Client, Session 统计信息

支持对单个 Client、Session 进程进行统计，etc/emq.conf 配置文件中设置'enable_stats'开启:

    mqtt.client.enable_stats = 60s

    mqtt.session.enable_stats = 60s

### 新增 missed 统计指标

EMQ 收到客户端 PUBACK、PUBREC、PUBREL、PUBCOMP 报文，但在 Inflight 窗口无法找到对应消息时，计入 missed 统计指标:

    packets/puback/missed

    packets/pubrec/missed

    packets/pubrel/missed

    packets/pubcomp/missed

### Syslog 日志集成

支持输出 EMQ 日志到 Syslog，etc/emq.config 配置项:

    ## Syslog. Enum: on, off
    log.syslog = on

    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

### Tune QoS 支持

支持订阅端升级 QoS，etc/emq.conf 配置项:

    mqtt.session.upgrade_qos = on

### 'acl reload'管理命令

Reload acl.conf without restarting emqttd service (#885)

### 配置项变更

1. 变更 mqtt.client_idle_timeout 为 mqtt.client.idle_timeout
2. 新增 mqtt.client.enable_stats 配置项
3. 新增 mqtt.session.upgrade_qos 配置项
4. 删除 mqtt.session.collect_interval 配置项
5. 新增 mqtt.session.enable_stats 配置项
6. 变更 mqtt.session.expired_after 为 mqtt.session.expiry_interval

### 合并扩展模块到 emq_modules 项目

合并 emq_mod_presence, emq_mod_subscription, emq_mod_rewrite 到 emq_modules 项目

变更 emq_mod_retainer 为 emq_retainer 项目

### Dashboard 插件

Overview 页面增加 missed 相关统计指标。 Client 页面增加 SendMsg、RecvMsg 统计指标。 Session 页面增加 DeliverMsg、EnqueueMsg 指标。

### recon 插件

变更 recon.gc_interval 配置项类型为 duration

### reloader 插件

变更 reloader.interval 配置项类型为 duration

## 2.0.7 版本

_发布日期: 2017-01-20_

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages.

emq-auth-http#9: Update the priv/emq_auth_http.schema, cuttlefish:unset() if no super_req/acl_req config exists

emq-auth-mongo#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard#91: Fix the exception caused by binary payload

emq-relx#21: Improve the binemqttd.cmd batch script for windows

emqttd#873: Documentation: installing-from-source

emqttd#870: Documentation: The word in Documents is wrong

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }}

## 2.0.6 版本

_发布日期: 2017-01-08_

Upgrade the [ esockd ](https://github.com/emqtt/esockd) library to v4.1.1

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed

emq-relx#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo#31: Mongodb ACL Cannot work?

## 2.0.5 版本

_发布日期: 2016-12-24_

emq-auth-http#9: Disable ACL support

emq-auth-mongo#29: Disable ACL support

emq-auth-mongo#30: {datatype, flag}

## 2.0.4 版本

_发布日期: 2016-12-16_

emqttd#822: Test cases for SSL connections

emqttd#818: trap_exit to link WebSocket process

emqttd#799: Can't publish via HTTPS

## 2.0.3 版本

_发布日期: 2016-12-12_

emqttd#796: Unable to forbidden tcp lisener

emqttd#814: Cannot remove a 'DOWN' node from the cluster

emqttd#813: Change parameters order

emqttd#795: Fix metrics of websocket connections

emq-dashboard#88: Rename the default topic from “/World” to “world”

emq-dashboard#86: Lookup all online clients

emq-dashboard#85: Comment the default listener port

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart.

## 2.0.2 版本

_发布日期: 2016-12-05_

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down

emqttd#790: Unable to start emqttd service if username/password contains special characters

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790

## 2.0.1 版本

_发布日期: 2016-11-30_

emqttd#781: 更新项目 README 到 2.0 版本

emq_dashboard#84: 显示节点集群状态

emq_dashboard#79: 集群节点采用 disc_copies 存储 mqtt_admin 表

emq_auth_clientid: 集群节点采用 disc_copies 存储 mqtt_auth_clientid 表

emq_auth_username: 集群节点采用 disc_copies 存储 mqtt_auth_username 表

emq_mod_subscription#3: 删除 emq_mod_subscription 表与 module.subscription.backend 配置

emq_plugin_template#5: 插件停止时注销认证/ACL 模块

## 2.0 正式版 "西湖以西"

_发布日期: 2016-11-24_

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

2.0 版本分离 [ emqttd ](https://github.com/emqtt/emqttd) 主项目和发布项目 [ emq-relx ](https://github.com/emqtt/emq-relx) , 采用 [ erlang.mk ](https://erlang.mk) 和 [ relx ](https://github.com/erlware/relx) 编译发布工具替换 1.x 版本使用的 rebar，项目可以跨平台在 Linux/Unix/Windows 系统下编译。

### CoAP 协议支持

2.0 版本支持 CoAP 协议(RFC7252)，支持 CoAP 网关与 MQTT 客户端互通。

CoAP 插件: [ https://github.com/emqtt/emq_coap ](https://github.com/emqtt/emq_coap)

### MQTT-SN 协议支持

2.0 版本支持 MQTT-SN 协议，支持 MQTT-SN 网关与 MQTT 客户端互通。

MQTT-SN 插件: [ https://github.com/emqtt/emq_sn ](https://github.com/emqtt/emq_sn)

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

EMQ-2.0 版本支持 Docker 镜像制作，Dockerfile 开源在: [ https://github.com/emqtt/emq_docker ](https://github.com/emqtt/emq_docker)

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
| [ emq_dashboard ](https://github.com/emqtt/emqttd_dashboard)            | Web 控制台插件(默认加载)   |
| [ emq_auth_clientid ](https://github.com/emqtt/emq_auth_clientid)       | ClientId 认证插件          |
| [ emq_auth_username ](https://github.com/emqtt/emq_auth_username)       | 用户名、密码认证插件       |
| [ emq_auth_ldap ](https://github.com/emqtt/emq_auth_ldap)               | LDAP 认证/访问控制         |
| [ emq_auth_http ](https://github.com/emqtt/emq_auth_http)               | HTTP 认证/访问控制         |
| [ emq_auth_mysql ](https://github.com/emqtt/emq_auth_mysql)             | MySQL 认证/访问控制        |
| [ emq_auth_pgsql ](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL 认证/访问控制   |
| [ emq_auth_redis ](https://github.com/emqtt/emq_auth_redis)             | Redis 认证/访问控制        |
| [ emq_auth_mongo ](https://github.com/emqtt/emq_auth_mongo)             | MongoDB 认证/访问控制      |
| [ emq_mod_rewrite ](https://github.com/emqtt/emq_mod_rewrite)           | 重写主题(Topic)插件        |
| [ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块        |
| [ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布   |
| [ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅     |
| [ emq_coap ](https://github.com/emqtt/emq_coap)                         | CoAP 协议支持              |
| [ emq_sn ](https://github.com/emqtt/emq_sn)                             | MQTT-SN 协议支持           |
| [ emq_stomp ](https://github.com/emqtt/emq_stomp)                       | Stomp 协议支持             |
| [ emq_sockjs ](https://github.com/emqtt/emq_sockjs)                     | Stomp over SockJS 协议支持 |
| [ emq_recon ](https://github.com/emqtt/emq_recon)                       | Recon 性能调试             |
| [ emq_reloader ](https://github.com/emqtt/emq_reloader)                 | Reloader 代码热加载插件    |
| [ emq_plugin_template ](https://github.com/emqtt/emq_plugin_template)   | 插件开发模版               |

## 2.0-rc.3 版本

## 2.0-rc.3 版本

_发布日期: 2016-11-01_

1. 将 Presence、Retainer、Subscription 三个扩展模块改为独立插件:

| [ emq_mod_retainer ](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块      |
| ----------------------------------------------------------------------- | ------------------------ |
| [ emq_mod_presence ](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布 |
| [ emq_mod_subscription ](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅   |

2. 更新 EMQ 自带的自签名 SSL 证书，修复 SSL 双向认证配置文件错误
3. Bugfix: Fixed a typo (#716)
4. Bugfix: emqttd_http can not use emq_auth_http? #739
5. Bugfix: emq_auth_redis cannot use hostname as server address (#741)
6. 升级 Redis, MySQL, Postgre, MongoDB 插件，支持主机名或域名配置

## 2.0-rc.2 版本

_发布日期: 2016-10-19_

1. 集成 cuttlefish 库，支持'K = V'通用配置文件格式，重构 EMQ 与全部插件配置文件:

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

_发布日期: 2016-10-03_

1.  超级用户认证成功后，发布订阅时不进行 ACL 鉴权 (#696)

2.  MQTT 客户端认证失败后，EMQ 服务器主动关闭 TCP 连接 (#707)

3.  改进插件管理设计，新增插件无需修改 rel/sys.config 配置

4.  改进全部插件 Makefile 的 emqttd 依赖:

        BUILD_DEPS = emqttd

    dep_emqttd = git https://github.com/emqtt/emqttd emq20

5.  重新设计 Redis 插件的 ACL 鉴权模块

## 2.0-beta.3 版本

_发布日期: 2016-09-18_

### 共享订阅(Shared Subscription)

Shared Suscriptions (#639, #416):

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

### 本地订阅(Local Subscription)

Local Subscriptions that will not create global routes:

    mosquitto_sub -t '$local/topic'

### 问题修复

Error on Loading emqttd_auth_http (#691)

Remove 'emqttd' application from dependencies (emqttd_coap PR#3)

## 2.0-beta.2 版本

_发布日期: 2016-09-10_

### CoAP 协议支持

CoAP 协议支持插件(Beta): [ https://github.com/emqtt/emqttd_coap ](https://github.com/emqtt/emqttd_coap)

### API Breaking Changes

'$u', '$c' variables in emqttd.conf and modules/acl.conf changed to '%u', '%c'

Improve the design of mqtt retained message, replace emqttd_retainer with emqttd_mod_retainer.

Add 'session.subscribed', 'session.unsubscribed' hooks, remove 'client.subscribe.after' hook

Tab 'retained_message' -> 'mqtt_retained'

### Bugfix

[2.0 beta1] FORMAT ERROR: "~s PUBLISH to ~s: ~p" (PR #671)

Fixing issues in cluster mode. (PR #681)

Fixing issues with unsubscribe hook (PR #673)

## 2.0-beta.1 版本

_发布日期: 2016-08-30_

_版本别名: 西湖以西(West of West Lake)_

EMQ 2.0-beta1 预览版本(Preview Release)发布。EMQ 2.0 版本改进了项目结构、发布方式、Git 分支结构以及配置文件格式，以奠定 EMQ 消息服务器项目长期演进基础。

::: tip
1.x 版本产品部署用户请勿升级到该版本，2.0 正式版本发布前会有 API 变更。
:::

### 项目简称 - EMQ

项目简称变更为 EMQ(Erlang/Enterprise/Elastic MQTT Broker)，E 含义 Erlang/OTP 平台、企业(Enterprise)、弹性(Elastic)。

### 项目发布方式

2.0 版本后采用预览版(Preview Release) + 候选版本(Release Candidate)版本方式迭代发布，2.0 版本将陆续发布 beta1, beta2, beta3, rc1, rc2 等迭代，直到 2.0 正式版本发布。

### 应用与发布

2.0 版本后 [ emqttd ](https://github.com/emqtt/emqttd) 项目只包括消息服务器应用源码，分离发布(rel)为独立项目: [ emqttd_relx ](https://github.com/emqtt/emqttd-relx) ，以解决 1.0 版本的插件(plugins)与 emqttd 应用编译依赖问题。

源码编译请 clone [ emqttd_relx ](https://github.com/emqtt/emqttd-relx) :

    git clone https://github.com/emqtt/emqttd-relx.git

    cd emqttd-relx && make

    cd _rel/emqttd && ./bin/emqttd console

### erlang.mk 与 relx

2.0 版本发布项目 [ emqttd_relx ](https://github.com/emqtt/emqttd-relx) 采用 [ erlang.mk ](https://erlang.mk) 和 [ relx ](https://github.com/erlware/relx) 编译发布工具替换 1.x 版本使用的 rebar。原因: [ https://erlang.mk/guide/why.html ](https://erlang.mk/guide/why.html)

### Git 分支结构

| stable     | 1.x 稳定版本分支 |
| ---------- | ---------------- |
| master     | 2.x 主版本分支   |
| emq10      | 1.x 版本开发分支 |
| emq20      | 2.x 版本开发分支 |
| emq30      | 3.x 版本开发分支 |
| issue#{id} | Issue 修复分支   |

etc/emqttd.conf 配置文件 ---------=-------------

2.0 版本改进项目配置文件格式，采用 rebar.config、relx.config 类似格式，提高配置文件的可读性和可编辑性。

etc/emqttd.conf 配置示例:

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

### MQTT-SN 协议支持

2.0-beta1 版本正式发布 [ emqttd_sn ](http://github.com/emqtt/emqttd_sn) 项目支持 MQTT-SN 协议，插件加载方式启用 emqttd_sn 项目，MQTT-SN 默认 UDP 端口: 1884:

    ./bin/emqttd_ctl plugins load emqttd_sn

### 改进插件架构

2.0 版本从 emqttd 项目删除 plugins/目录，插件作为一个普通的 Erlang 应用，直接依赖(deps)方式在编译到 lib 目录，插件配置文件统一放置在 etc/plugins/目录中:

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

### 2.0 版本项目文档

2.0 版本中文文档: [ http://emqtt.com/docs/v2/index.html ](http://emqtt.com/docs/v2/index.html) 或 [ http://docs.emqtt.cn/zh_CN/emq20 ](http://docs.emqtt.cn/zh_CN/emq20)

2.0 版本英文文档: [ https://developer.emqx.io/docs/emq/v2/en/index.html ](https://developer.emqx.io/docs/emq/v2/en/index.html) 或 [ http://docs.emqtt.com/ ](http://docs.emqtt.com/)

### 发布订阅流程

![image](./_static/images/publish.png)

## 1.1.3 版本

_发布日期: 2016-08-19_

Support './bin/emqttd_ctl users list' CLI (#621)

Cannot publish payloads with a size of the order 64K using WebSockets (#643)

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627)

Fix SSL certfile, keyfile config (#651)

## 1.1.2 版本

_发布日期: 2016-06-30_

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596)

Fix WebSocket Client Leak (PR #612)

java.io.EOFException using paho java client (#551)

Send message from paho java client to javascript client (#552)

Compatible with the Qos0 PUBREL packet (#575)

Empty clientId with non-clean session accepted (#599)

Update docs to fix typos (#601, #607)

## 1.1.1 版本

_发布日期: 2016-06-04_

Compatible with the Qos0 PUBREL packet (#575)

phpMqtt Client Compatibility (#572)

java.io.EOFException using paho java client (#551)

## 1.1 版本

_发布日期: 2016-06-01_

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

支持通过 HTTP API 认证/鉴权 MQTT 客户端: [ https://github.com/emqtt/emqttd_auth_http ](https://github.com/emqtt/emqttd_auth_http)

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

开发调试代码热升级插件: [ https://github.com/emqtt/emqttd_reloader ](https://github.com/emqtt/emqttd_reloader)

## 1.0.2 版本

_发布日期: 2016-05-04_

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data

PR#519 - The erlang '-name' requires the fully qualified host name

emqttd_reloader plugin - help reload modified modules during development.

## 1.0.1 版本

_发布日期: 2016-04-16_

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs

## 1.0 (七英里) 版本

_发布日期: 2016-04-13_

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

1.0 版本中文文档: [ http://emqtt.com/docs/ ](http://emqtt.com/docs/) 或 [ http://docs.emqtt.cn ](http://docs.emqtt.cn)

1.0 版本英文文档: [ https://developer.emqx.io/docs/emq/v1/en/index.html ](https://developer.emqx.io/docs/emq/v1/en/index.html) 或 [ http://docs.emqtt.com/ ](http://docs.emqtt.com/)

### 官方站点

中文站点: [ http://emqtt.com ](http://emqtt.com)

英文站点: [ https://www.emqx.io/ ](https://www.emqx.io/)

### 致谢

爱立信与 Erlang/OTP 语言平台团队( [ http://www.erlang.org/ ](http://www.erlang.org/) )!

贡献者(GitHub 帐户): @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

公司: 开源中国，鲁能电力，太极计算机，电信天翼云直播，研色科技，杭州华思

乐队: 七英里(The Seven Mile Journey)，腰乐队，万能青年旅店

## 0.17.1-beta 版本

_发布日期: 2016-03-22_

### Enhancements

Time unit of session 'expired_after' changed to minute. (#479)

### Dashboard

Code Review and improve the design of Dashboard.

## 0.17.0-beta 版本

_发布日期: 2016-03-15_

### Highlights

Installation and Configuration Guide released on [ http://docs.emqtt.com ](http://docs.emqtt.com)

Improve and Consolidate the design of Hook, Server, PubSub and Router

Upgrade the [Web Dashboard](https://github.com/emqtt/emqttd_dashboard) to support pagination

Bridge emqttd broker to another emqttd broker & emqttd to mosquitto bridge (#438)

### Enhancements

emqttd_ctl: better error message (#450)

./bin/emqttd_ctl: add 'routes' command

` ` routes list # List all routes routes show \<Topic> # Show a route ` `

Add 'backend_subscription' table and support static subscriptions (emqttd_backend)

Add 'retained_message' table and refactor emqttd_retainer module (emqttd_backend)

A New Hook and Callback Design (emqttd_hook)

Add PubSub, Hooks APIs to emqttd module (emqttd)

Move start_listeners/0, stop_listeners/0 APIs to emqttd_app module (emqttd_app)

### Tests

Add 100+ common test cases.

### Plugins

Upgrade Dashboard, Redis, Stomp and Template Plugins

## 0.16.0-beta 版本

_发布日期: 2016-02-16_

### Highlights

Licensed under the Apache License, Version 2.0 Now.

Improve the design of cluster, support to join or leave the cluster (#449):

` ` $ ./bin/emqttd_ctl cluster cluster join \<Node> #Join the cluster cluster leave #Leave the cluster cluster remove \<Node> #Remove the node from cluster cluster status #Cluster status ` `

Improve the design of Trie and Route, only the wildcard topics stored in Trie.

Common Test to replace EUnit.

### Enhancements

mqtt_message record: add 'sender' field (#440)

refactor the emqttd, emqttd_time, emqttd_opts, emqttd_node modules.

### Bugfix

noproc error when call to gen_server2:call(false, {add_route,Topic,\<0.685.0>}, infinity) (#446)

#### Plugins

Changed the license of all plugins.

## 0.15.0-beta 版本

_发布日期: 2016-01-31_

### Highlights

Optimize for Push Application, 500K+ Subscribers to a Topic.

Optimization for Route ETS insertion (#427)

Priority Message Queue for Persistent Session (#432)

Add Redis, MongoDB Plugins (#417)

### Enhancements

Username/Password Authentication: Support to configure default users (#428)

Improve CLI Commands: pubsub, bridges, trace (#429)

emqttd_mod_subscription: fix client_connected/3

emqttd_auth_mod: add passwd_hash/2 function

priority_queue: add plen/2, out/2 functions

### Bugfix

Fix dequeue/1 of emqttd_bridge...

Add emqttd:seed_now/0 function

### Plugins

emqttd_plubin_mysql: Changed mysql driver to mysql-otp

emqttd_plugin_pgsql: Integrate with ecpool

emqttd_plugin_redis: First release

emqttd_plugin_mongo: First release

## 0.14.1-beta 版本

_发布日期: 2015-12-28_

Bugfix: emqttd_ws_client.erl: Unexpected Info: {'EXIT',\<0.27792.18>,{shutdown,destroy}} (#413)

Improve: fix spec errors found by dialyzer

## 0.14.0-beta 版本

_发布日期: 2015-12-18_

### Highlights

Scaling to 1.3 Million Concurrent MQTT Connections on a 12 Core, 32G CentOS server.

New PubSub, Router Design (#402). Prepare for scaling to 10 millions on one cluster.

### Enhancements

Improve the gproc_pool usage with a general emqttd_pool_sup

Improve the design of emqttd_pubsub, add a new emqttd_router module

Improve the design of the whole supervisor tree

Route aging mechanism to remove the topics that have no subscriptions

Improve the dashboard, mysql, pgsql, stomp, sockjs plugins

Add 'topics', 'subscriptions' admin commands

Avoid using mnesia table index and mnesia:index_read API to lower CPU usage

Subscribe timeout exception (#366)

Long Delay on Multiple Topic Subscription (#365)

Subscriptions persistence (#344)

emqttd_ctl: 'subscriptions' command to force clients to subscribe some topics (#361)

### Bugfix

emqttd_sm: spec of lookup_session/1 is not right BUG (#411)

Observer application should be removed from reltool.config for 'wx' app is not available (#410)

### Benchmark

1.3 million concurrent MQTT connections on a 12 Core, 32G CentOS Server, consume about 15G Memory and 200% CPU.

## 0.13.1-beta 版本

_发布日期: 2015-11-28_

Bugfix: Plugin pathes error under windows (#387)

Improve: Too many error logs "[error] Session ..... Unexpected EXIT: client_pid=\<0.14137.35>, exit_pid=\<0.30829.22>, reason=nop..." (#383)

Improve: Define QOS0/1/2, Pooler Error (PR#382)

Improve: High CPU load when 400K unstable mobile connections (#377)

BugFix: emqttd_plugin_pgsql - error using same query with latest update plugin (pgsql#5)

## 0.13.0-beta 版本

_发布日期: 2015-11-08_

### Highlights

Rate Limiting based on [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket) and [Leaky Bucket](https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter) Algorithm

Upgrade eSockd and MochiWeb libraries to support Parameterized Connection Module

Improve emqttd_client to support fully asynchronous socket networking

### Enhancements

Protocol Compliant - Session Present Flag (#163)

Compilation fails if repo is cloned with a different name (#348)

emqttd_client: replace gen_tcp:send with port_command (#358)

TCP sndbuf, recbuf, buffer tuning (#359)

emqttd_client.erl to handle 'inet_async', 'inet_reply' properly (#360)

Refator the [client/session management design](https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md)

### Bugfix

Cannot kick transient client out when clientId collision (#357)

Fix the order of emqttd_app:start_server/1 (#367)

emqttd_session:subscribe/2 will crash (#374)

### Benchmark

[benchmark for 0.13.0 release](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release)

3.1G memory and 50+ CPU/core:

    Connections: 250K
    Subscribers: 250K
    Topics:      50K
    Qos1 Messages/Sec In:  4K
    Qos1 Messages/Sec Out: 20K
    Traffic In(bps):  12M+
    Traffic Out(bps): 56M+

## 0.12.3-beta 版本

_发布日期: 2015-10-22_

Bugfix: emqttd_sysmon crasher for 'undefined' process_info (#350)

Bugfix: emqttd_client: catch parser exception (#353)

## 0.12.2-beta 版本

_发布日期: 2015-10-16_

Bugfix: Retained messages should not be expired if 'broker.retained.expired_after = 0' (#346)

## 0.12.1-beta 版本

_发布日期: 2015-10-15_

Highlight: Release for Bugfix and Code Refactor.

Feature: Retained message expiration (#182)

Improve: '$SYS/#' publish will not match '#' or '+/#' (#68)

Improve: Add more metrics and ignore '$SYS/#' publish (#266)

Improve: emqttd_sm should be optimized for clustered nodes may be crashed (#282)

Improve: Refactor emqttd_sysmon and suppress 'monitor' messages (#328)

Task: benchmark for 0.12.0 release (#225)

Benchmark: About 900K concurrent connections established on a 20Core, 32G CentOS server.

## 0.12.0-beta 版本

_发布日期: 2015-10-08_

### Highlights

Enhance the **emqttd_ctl** module to allow plugins to register new commands (#256)

Add [emqttd_recon plugin](https://github.com/emqtt/emqttd_recon) to debug/optimize the broker (#235)

Add **'./bin/emqttd_ctl broker pubsub'** command to check the status of core pubsub processes

Add **'./bin/emqttd_top'** command(like etop) to show the top 'msg_q', 'reductions', 'memory' or 'runtime' processes

'rel/files/emqttd.config.production' for production deployment(default)

'rel/files/emqttd.config.development' for development deployment

### Enhancements

Qos1/2 messages will not be dropped under unstable mobile network (#264)

**emqttd_session:subscribe/2, emqttd_session:unsubscribe/2** APIs should be asynchronous (#292)

**etc/emqttd.config** : 'idle_timeout' option to close the idle client(socket connected but no 'CONNECT' frame received)

**etc/emqttd.config** : 'unack_retry_interval' option for redelivering Qos1/2 messages

How to monitor large 'message_queue_len' (#283)

### Bugfix

Behaviour emqttd_auth_mod is missing init callback (#318)

### Benchmark

Write a new [benchmark tool](https://github.com/emqtt/emqtt_benchmark) to benchmark this release

Hw requirements - 5K users, 25-50 msgs/sec, QoS=1 (#209)

Supported Number of Connections Greatly Reduced When Clients are Subscribing (#324)

## 0.11.0-beta 版本

_发布日期: 2015-09-25_

Highlight: Rebar to manage plugin dependencies.

Highlight: [Stomp](https://github.com/emqtt/emqttd_stomp) and [SockJS](https://github.com/emqtt/emqttd_sockjs) Plugins!

Improve: add rel/files/emqttd.config.development|production.

Improve: rel/reltool.config.script to release deps of plugin.

Improve: persist mnesia schema on slave nodes.

Improve: use timer:seconds/1 api.

Improve: The binary release will be compiled with R18.1 now.

Bugfix: issue#306 - emqttd_cm should unregister the duplicated client

Bugfix: issue#310 - usage of emqttd_ctl error: 'session list' should be 'sessions list'

Bugfix: issue#311 - './bin/emqttd_ctl sessions list' error

Bugfix: issue#312 - unsubcribe will lead to crash if emqttd_plugin_template plugin loaded

## 0.10.4-beta 版本

_发布日期: 2015-09-18_

Optimize session management and upgrade eSockd library to 2.7.1

[Benchmark for 0.10.4 release](https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release)

Improve: issue#294 - [error] failed to start connection on 0.0.0.0:1883 - enotconn

Improve: issue#297 - How do I allow user with some pattern to access topic with some pattern?

Bugfix: issue#291 - "./bin/emqttd attach ..." cannot work

Bugfix: issue#284 - Should not use erlang:list_to_atom/1 in emqttd_vm.erl

## 0.10.3-beta 版本

_发布日期: 2015-08-30_

Bugfix: issue#271 - add emqttd_ws_client:subscribe/2 function

Bugfix: issue#269 - bin/emqttd Syntax error on ubuntu

Improve: issue#265 - client under unstable mobile network generate a lot of logs

## 0.10.2-beta 版本

_发布日期: 2015-08-26_

Improve: issue#257 - After the node name changed, the broker cannot restart for mnesia schema error.

## 0.10.1-beta 版本

_发布日期: 2015-08-25_

Bugfix: issue#259 - when clustered the emqttd_dashboard port is close, and the 'emqttd' application cannot stop normally.

Feature: issue#262 - Add ' [ http://host:8083/mqtt/status ](http://host:8083/mqtt/status) ' Page for health check

## 0.10.0-beta 版本

_发布日期: 2015-08-20_

[Web Dashboard](https://github.com/emqtt/emqttd_dashboard) and [MySQL](https://github.com/emqtt/emqttd_plugin_mysql), [PostgreSQL](https://github.com/emqtt/emqttd_plugin_pgsql) Authentication/ACL Plugins!

Highlight: Web Dashboard to monitor Statistics, Metrics, Clients, Sessions and Topics of the broker.

Highlight: JSON/HTTP API to query all clients connected to broker.

Highlight: A new [Plugin Design](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) and a [Template project](https://github.com/emqtt/emqttd_plugin_template) for plugin development.

Highlight: Authentication/ACL with MySQL, PostreSQl databases (#194, #172)

Feature: Session Statistics including inflight_queue, message_queue, message_dropped, awaiting_rel, awaiting_ack, awaiting_comp (#213)

Feature: Cookie based authentication for MQTT over websocket connections (#231)

Feature: Get all clients connected to the broker (#228, #230, #148, #129)

Feature: "./bin/emqttd_ctl clients show ClientId" to query client status (#226)

Feature: "./bin/emqttd_ctl clients kick ClientId" to kick out a client

Feature: "./bin/emqttd_ctl sessions list" to show all sessions

Feature: "./bin/emqttd_ctl sessions show ClientId" to show a session

Feature: Erlang VM metrics monitor with Web Dashboard (#59)

Improve: Too many "inflight queue is full!" log when session is overloaded (#247)

Improve: There are two many "MQueue(~s) drop ~s" logs if the message queue of session is small (#244)

Improve: gen_server2(from RabbitMQ) to improve emqttd_session, emqttd_pubsub

Improve: Makefile to build plugins

Bugfix: emqttd_broker:unhook/2 cannot work (#238)

Bugfix: emqttd plugin cannot include_lib("emqttd/include/emqttd.hrl") (#233)

Bugfix: Too many 'Session ~s cannot find PUBACK' logs (#212)

Bugfix: emqttd_pooler cannot work

## 0.9.3-alpha 版本

_发布日期: 2015-07-25_

Wiki: [Bridge](https://github.com/emqtt/emqttd/wiki/Bridge)

Improve: emqttd_protocol.hrl to define 'QOS_I'

Improve: emqttd_pubsub to add subscribe/2 API

Improve: ./bin/emqttd_ctl to support new bridges command

Bugfix: issue #206 - Cannot bridge two nodes

## 0.9.2-alpha 版本

_发布日期: 2015-07-18_

Improve: issue #196 - Add New Hook 'client.subscribe.after'

## 0.9.1-alpha 版本

_发布日期: 2015-07-10_

Bugfix: issue #189 - MQTT over WebSocket(SSL) cannot work?

Bugfix: issue #193 - 'client.ack' hook should be renamed to 'message.acked', and called by emqttd_broker:foreach_hooks

## 0.9.0-alpha 版本

_发布日期: 2015-07-09_

[Session, Queue, Inflight Window, Hooks, Global MessageId and More Protocol Compliant](https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha) Now!

Feature: Session/Queue/Inflight Window Design (#145).

Feature: Support to resume a persistent session on other clustered node.

Feature: Support alarm management.

Feature: emqttd_guid to generate global unique message id.

Feature: Hooks for message pub/ack.

Feature: Protocol compliant - message ordering, timeout and retry.

Improve: Every client will start_link a session process, whether or not the client is persistent.

Improve: etc/emqttd.config to support more session, queue configuration.

Improve: issue #179 - Max offline message queue {max_queue, 100} meaning.

Improve: issue #180 - Should change project structure for other projects maybe depend on 'emqttd'. Merge emqtt, emqttd apps.

Improve: issue #185 - PacketId and MessageId: the broker should generate global unique message id.

Improve: issue #187 - etc/emqttd.config to support https listener

Improve: issue #186 - emqttd_cm to store client details

Improve: issue #174 - add 'from' field to mqtt_message record.

Improve: issue #170 - $SYS Topics should support alarms.

Improve: issue #169 - Add More [Hooks](https://github.com/emqtt/emqttd/wiki/Hooks-Design)

Improve: issue #167 - Inflight window to assure message ordering.

Improve: issue #166 - Message delivery timeout and retry.

Improve: issue #143 - Qos1, Qos2 PubSub message timeout.

Improve: issue #122 - Labeling message with unique id. emqttd_guid module to generate global unique msgid.

Improve: emqttd_bridge to support pending message queue, and fix the wrong Qos design.

Improve: mqtt_message record to add 'msgid', 'from' and 'sys' fields.

Change: Add emqttd_mqueue, emqttd_guid, emqttd_alarm modules.

Bugfix: issue #184 - emqttd_stats:setstats is not right.

Bugfix: Closed issues #181, #119.

Tests: fix the parser, acl test cases.

## 0.8.6-beta 版本

_发布日期: 2015-06-17_

Bugfix: issue #175 - publish Will message when websocket is closed without 'DISCONNECT' packet

## 0.8.5-beta 版本

_发布日期: 2015-06-10_

Bugfix: issue #53 - client will receive duplicate messages when overlapping subscription

## 0.8.4-beta 版本

_发布日期: 2015-06-08_

Bugfix: issue #165 - duplicated message when publish 'retained' message to persistent client

## 0.8.3-beta 版本

_发布日期: 2015-06-05_

Bugfix: issue #158 - should queue:in new message after old one dropped

Bugfix: issue #155 - emqtt_parser.erl: parse_topics/3 should reverse topics

Bugfix: issue #149 - Forget to merge plugins/emqttd_auth_mysql from 'dev' branch to 'master' in 0.8.x release

## 0.8.2-alpha 版本

_发布日期: 2015-06-01_

Bugfix: issue #147 - WebSocket client cannot subscribe queue '$Q/queue/${clientId}'

Bugfix: issue #146 - emqttd_auth_ldap: fill(Username, UserDn) is not right

## 0.8.1-alpha 版本

_发布日期: 2015-05-28_

Client [Presence](https://github.com/emqtt/emqttd/wiki/Presence) Support and [$SYS Topics](https://github.com/emqtt/emqttd/wiki/$SYS-Topics) Redesigned!

Bugfix: issue #138 - when client disconnected normally, broker will not publish disconnected $SYS message

Bugfix: fix websocket url in emqttd/priv/www/websocket.html

Improve: etc/emqttd.config to allow websocket connections from any hosts

Improve: rel/reltool.config to exclude unnecessary apps.

## 0.8.0-alpha 版本

_发布日期: 2015-05-25_

[Hooks](https://github.com/emqtt/emqttd/wiki/Hooks%20Design), Modules and [Plugins](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) to extend the broker Now!

Plugin: emqttd_auth_mysql - MySQL authentication plugin (issues #116, #120)

Plugin: emqttd_auth_ldap - LDAP authentication plugin

Feature: emqttd_broker to support Hooks API

Feature: issue #111 - Support 'Forced Subscriptions' by emqttd_mod_autosub module

Feature: issue #126 - Support 'Rewrite rules' by emqttd_mod_rewrite module

Improve: Support hooks, modules to extend the broker

Improve: issue #76 - dialyzer check

Improve: 'Get Started', 'User Guide', 'Developer Guide' Wiki

Improve: emqtt_topic to add join/1, feed_var/3, is_queue/1

Improve: emqttd_pooler to execute common tasks

Improve: add emqttd_sm_sup module, and use 'hash' gproc_pool to manage sessions

Tests: add more test cases for 'emqttd' app

## 0.7.1-alpha 版本

_发布日期: 2015-05-04_

Add doc/design/_ and merge doc/_ to github Wiki

Bugfix: issue #121 - emqttd cluster issuse

Bugfix: issue #123 - emqttd:unload_all_plugins/0 cannot unload any plugin

Bugfix: fix errors found by dialyzer

## 0.7.0-alpha 版本

_发布日期: 2015-05-02_

[MQTT over WebSocket(SSL)](https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket) Now!

[Plugin Achitecture](https://github.com/emqtt/emqttd/wiki/Plugin%20Design) based on OTP application

[Trace MQTT Packets or Messages](https://github.com/emqtt/emqttd/wiki/Trace%20Design) to log files

Feature: issue #40, #115 - WebSocket/SSL Support

Feature: issue #49, #105 - Plugin Architecture Support

Feature: issue #93 - Trace API Design

Improve: issue #109 - emqttd_broker should add subscribe, notify API

Improve: update README.md to add 'Goals', 'Contributors' chapters

Change: rename etc/app.config to etc/emqttd.config

Change: etc/emqttd.config changed

Bugfix: critical issue #54 - error when resume session!

Bugfix: issue #118 - error report when UNSUBSCRIBE with no topics

Bugfix: issue #117 - sys_interval = 0 config cannot work

Bugfix: issue #112 - Makefile to support build plugins

Bugfix: issue #96 - "make clean" cannot work

## 0.6.2-alpha 版本

_发布日期: 2015-04-24_

Bugfix: critical issue #54, #104, #106 - error when resume session

Improve: add emqttd_cm_sup module, and use 'hash' gproc_pool to register/unregister client ids

Improve: kick old client out when session is duplicated.

Improve: move mnesia dir config from etc/app.config to etc/vm.args

## 0.6.1-alpha 版本

_发布日期: 2015-04-20_

Integrate with [gproc library](https://github.com/uwiger/gproc) to support pool

Feature: issues#91 - should use worker_pool to handle some async work?

Feature: issues#95 - Topic filters in ACL rule should support 'eq' tag

Improve: issues#84 - emqttd_pubsub is redesigned again to protect mnesia transaction

Improve: issues#74 - ACL Support and update [ACL Design Wiki](https://github.com/emqtt/emqttd/wiki/ACL-Design)

## 0.6.0-alpha 版本

_发布日期: 2015-04-17_

ACL Support Now: [ACL-Design Wiki](https://github.com/emqtt/emqttd/wiki/ACL-Design)

Authentication with username, clientid Now: [Authentication Wiki](https://github.com/emqtt/emqttd/wiki/Authentication)

Seperate common MQTT library to 'emqtt' application

Redesign message pubsub, route and retain modules

Redesign mnesia database cluster

Feature: issues#47 - authentication, authorization support

Feature: issues#92 - merge emqttd_acl and emqttd_auth to emqttd_access_control

Feature: emqttd_acl_mod, emqttd_auth_mod behaviour to extend ACL, authentication

Feature: issues#85 - lager:info to log subscribe, unsubscribe actions

Feature: issues#77 - authentication with clientid, ipaddress

Improve: issues#90 - fix lager_file_backend log format, and rotate 10 log files

Improve: issues#88 - use '-mneisa_create', '-mnesia_replicate' attributes to init mneisa

Improve: issues#87 - record mqtt_user and mqtt_client is duplicated

Improve: issues#81 - redesign nodes cluster to support disc_copies mnesia tables

Improve: issues#80 - redesign emqttd_cm to handle more concurrent connections

Improve: issues#70 - how to handle connection flood? Now could support 2K+ CONNECT/sec

Change: redesign mnesia tables: message, topic, subscriber, trie, trie_node

Bugfix: issues#83 - emqttd_broker stats cannot work

Bugfix: issues#75 - careless about function name when emqttd_pubsub handle getstats message

## 0.5.5-beta 版本

_发布日期: 2015-04-09_

Bugfix: issue #75 - careless about function name when emqttd_pubsub handle getstats message.

Bugfix: issue #79 - cannot find topic_subscriber table after cluster with other nodes.

## 0.5.4-alpha 版本

_发布日期: 2015-03-22_

Benchmark this release on a ubuntu/14.04 server with 8 cores, 32G memory from QingCloud.com: :

    200K Connections,
    30K Messages/Sec,
    20Mbps In/Out Traffic,
    200K Topics,
    200K Subscribers,
    Consumed 7G memory, 40% CPU/core

Benchmark code: [ https://github.com/emqtt/emqttd_benchmark ](https://github.com/emqtt/emqttd_benchmark)

Change: rewrite emqttd_pubsub to handle more concurrent subscribe requests.

Change: ./bin/emqttd_ctl add 'stats', 'metrics' commands.

Bugfix: issue #71, #72

## 0.5.3-alpha 版本

_发布日期: 2015-03-19_

Bugfix: issues#72 - emqttd_cm, emqtt_sm ets:match_delete/2 with wrong pattern

## 0.5.2-alpha 版本

_发布日期: 2015-03-18_

Change: upgrade esockd to 2.1.0-alpha, do not tune socket buffer for mqtt connection.

## 0.5.1-alpha 版本

_发布日期: 2015-03-13_

Change: upgrade esockd to v1.2.0-beta, rename 'acceptor_pool' to 'acceptors'

## 0.5.0-alpha 版本

_发布日期: 2015-03-12_

RENAME 'emqtt' to 'emqttd'!

Support [Broker Bridge](https://github.com/emqtt/emqttd/wiki/Bridge-Design) Now!

Change: rename project from 'emqtt' to 'emqttd'

Change: lager:debug to dump RECV/SENT packets

Feature: emqttd_bridge, emqttd_bridge_sup to support broker bridge

Feature: emqtt_event to publish client connected/disconnected message to $SYS topics

Feature: ./bin/emqttd_ctl add more commands: listeners, broker, bridges, start_bridge, stop_bridge...

Feature: issue#57 - support to configure max packet size

Feature: issue#68 - if sys_interval = 0, emqttd_broker will not publish messages to $SYS/brokers/#

Bugfix: issue#67 - subscribe '#' to receive all messages

Bugfix: issue#64 - emqtt_app start/2: should wait_for_databases

Test: emqttd_topic_tests add more '\_match_test'

## 0.4.0-alpha 版本

_发布日期: 2015-03-10_

Support [$SYS Topics of Broker](https://github.com/emqtt/emqttd/wiki/$SYS-Topics-of-Broker) Now!

Feature: emqtt_broker to publish version, uptime, datetime to $SYS/brokers/# topics

Feature: emqtt_broker to publish count of clients, sessions, suscribers to $SYS/brokers/# topics

Feature: emqtt_metrics to publish bytes, packets, messages metrics to $SYS/brokers/# topics

Feature: add include/emqtt_systop.hrl

Change: emqtt_cm to count current clients

Change: emqtt_sm to count current sessions

Change: emqtt_pubsub to count current topics and suscribers

Change: emqtt_pubsub to add create/1 API

Change: emqtt_pubsub dispatch/2 to return number of subscribers

Change: emqtt_pubsub to count 'dropped' messages

Change: emqtt_opts to add merge/2 function

Test: add emqtt_serialiser_tests.erl

## 0.3.4-beta 版本

_发布日期: 2015-03-08_

Bugfix: emqtt_serialiser.erl cannot serialise UNSUBACK packets

## 0.3.3-beta 版本

_发布日期: 2015-03-07_

Bugfix: emqtt_serialiser.erl cannot serialise PINGRESP issue#60

## 0.3.2-beta 版本

_发布日期: 2015-03-05_

Improve: merge emqttc serialiser, parser, packet

Add: emqtt_opts to merge socket options

## 0.3.1-beta 版本

_发布日期: 2015-03-02_

Feature: SSL Socket Support

Feature: issue#44 HTTP API should add Qos parameter

Bugfix: issue#52 emqtt_session crash

Bugfix: issue#53 sslsocket keepalive error

Upgrade: esockd to v0.2.0

Upgrade: mochiweb to v3.0.0

## 0.3.0-beta 版本

_发布日期: 2015-01-19_

Feature: HTTP POST API to support 'qos', 'retain' parameters

Feature: $SYS system topics support

Change: Rewrite emqtt_topic.erl, use '', '#', '+' to replace <<"">>, <<"#">>, <<"+">>

Change: fix emqtt_pubsub.erl to match '#', '+'

Tests: emqtt_topic_tests.erl add more test cases

## 0.3.0-alpha 版本

_发布日期: 2015-01-08_

NOTICE: Full MQTT 3.1.1 support now!

Feature: Passed org.eclipse.paho.mqtt.testing/interoperability tests

Feature: Qos0, Qos1 and Qos2 publish and suscribe

Feature: session(clean_sess=false) management and offline messages

Feature: redeliver awaiting puback/pubrec messages(doc: Chapter 4.4)

Feature: retain messages, add emqtt_server module

Feature: MQTT 3.1.1 null client_id support

Bugfix: keepalive timeout to send will message

Improve: overlapping subscription support

Improve: add emqtt_packet:dump to dump packets

Test: passed org.eclipse.paho.mqtt.testing/interoperability

Test: simple cluster test

Closed Issues: #22, #24, #27, #28, #29, #30, #31, #32, #33, #34, #36, #37, #38, #39, #41, #42, #43

## 0.2.1-beta 版本

_发布日期: 2015-01-08_

pull request 26: Use binaries for topic paths and fix wildcard topics

emqtt_pubsub.erl: fix wildcard topic match bug caused by binary topic in 0.2.0

Makefile: deps -> get-deps

rebar.config: fix mochiweb git url

tag emqtt release accoding to [Semantic Versioning](http://semver.org/)

max clientId length is 1024 now.

## 0.2.0 版本

_发布日期: 2014-12-07_

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5 版本

_发布日期: 2013-01-05_

Bugfix: remove QOS_1 match when handle PUBREL request

Bugfix: reverse word in emqtt_topic:words/1 function

## 0.1.4 版本

_发布日期: 2013-01-04_

Bugfix: fix "mosquitto_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3 版本

_发布日期: 2013-01-04_

Feature: support QOS2 PUBREC, PUBREL,PUBCOMP messages

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2 版本

_发布日期: 2012-12-27_

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl

## 0.1.1 版本

_发布日期: 2012-09-24_

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0 版本

_发布日期: 2012-09-21_

The first public release.
