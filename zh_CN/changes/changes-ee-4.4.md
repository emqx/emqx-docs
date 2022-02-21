---
# 编写日期
date: 2021-12-21 09:32:21
# 作者 Github 名称
author: tigercl
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

## 4.4.1 版本

*发布日期: 2022-02-18*

注意: 4.4.1 与 4.3.7 保持同步。

此更改集的比较基础是 4.4.0。

### 重要变更

- 慢订阅功能改进，支持统计消息传输过程中花费的时间，并记录和展示耗时较高的客户端和主题。
- 我们在 4.4.1 中修复了 License 总连接数计算的 Bug，License 将正确地检查集群的总连接数，而非错误地仅检查每个节点上的连接数。**请计划升级的用户注意此变化可能导致的客户端达到 License 限制而无法连接的可能性。**
- 规则引擎支持 Lindorm 数据库
- 支持客户端级别的消息丢弃统计指标
- 优化 Dashboard 上在线 Trace 日志显示，支持语法高亮

### 次要变更

- license 连接数预警，默认连接数达到证书允许的 80% 则告警，小于 75% 时解除告警。用户也可在 `emqx.conf` 中进行自定义：`license.connection_high_watermark_alarm` , `license.connection_low_watermark_alarm`
- license 过期预警，当有效期小于 30 天时，会告警提示
- 规则引擎支持为客户端消息异常丢失事件配置规则与动作，以增强用户在这一场景的自定义处理能力
- 改进规则引擎 SQL 匹配执行过程中的相关统计指标
- 客户端模糊搜索支持 `*`， `(`，`)` 等特殊字符
- 改进 ACL 相关统计指标，解决命中 ACL 缓存导致计数不增加的问题
- Webhook 事件通知中新增 `connected_at` 字段
- 在因持有锁太久而终止客户端状态之前记录客户端状态

### 问题修复

- 修复数据导入导出在某些情况下不可用的问题
- Module 更新机制改进，解决更新失败后 Module 不可用的问题
- 修复规则引擎在执行比较大小的语句时候未进行类型检查的问题
- 修复更新规则引擎动作后相关计数清零的问题
- 修复 Metrics 接口默认情况下不返回 client.acl.deny 等认证鉴权指标的问题
- 修复订阅查询接口未返回分页数据的问题
- 修复 STOMP 处理 TCP 粘包时解析失败的问题
- 修复客户端过滤查询时会话创建时间选项不可用的问题
- 修复重启后内存告警可能不会触发的问题
- 修复 `emqx_auth_mnesia` 插件中存在用户数据时导入数据崩溃的问题

## 4.4.0 版本

*发布日期: 2021-12-21*

EMQX 4.4.0 现已正式发布，主要包含以下改动:

### 重要变更

- 从 4.4 开始，EMQX 的发行包命名将包含 Erlang/OTP 的版本号，例如 `emqx-ee-4.4.0-otp24.1.5-3-centos7-arm64.rpm`

- **对于 Debian/Ubuntu 用户**，Debian/Ubuntu 包 (deb) 安装的 EMQX 现在可以在 systemd 上运行，这是为了利用 systemd 的监督功能来确保 EMQX 服务在崩溃后重新启动。包安装服务从 init.d 升级到 systemd 已经过验证，但仍建议您在部署到生产环境之前再次验证确认，至少确保 systemd 在您的系统中可用

- 规则引擎 InfluxDB 集成新增对 InfluxDB v2 API 的支持，规则引擎现已支持 InfluxDB 2.0 与 InfluxDB Cloud

- 规则引擎新增对 SAP Event Mesh 的支持

- 规则引擎新增对超融合时空数据库 MatrixDB 的支持

- MongoDB 集成支持 DNS SRV 和 TXT Records 解析，可以与 MongoDB Altas 无缝对接

- 新增在线 Trace 功能，用户可以在 Dashboard 上完成对客户端和主题的追踪操作，以及查看或下载追踪日志

- 新增慢订阅统计功能，用以及时发现生产环境中消息堵塞等异常情况

- 支持动态修改 MQTT Keep Alive 以适应不同能耗策略

- 集群从 4.3 到 4.4 支持滚动升级。详情请见升级指南。

- 节点间的 RPC 链接支持配置 TLS. 详情请见[集群文档](../advanced/cluster.md#节点间RPC使用TLS)

### 次要变更

- Dashboard 支持查看客户端活跃连接数

- Dashboard 支持相对路径和自定义访问路径

- Dashboard 移除选项卡导航

- 支持配置是否将整型数据以浮点类型写入 InfluxDB

- 支持配置是否转发为 Payload 为空的保留消息，以适应仍在使用 MQTT v3.1 的用户，相关配置项为 `retainer.stop_publish_clear_msg`

- 多语言钩子扩展（exhook）支持动态取消客户端消息的后续转发

- 规则引擎 SQL 支持在 FROM 子句中使用单引号，例如：`SELECT * FROM 't/#'`

- 优化内置访问控制文件模块的使用交互

- 将 `max_topic_levels` 配置项的默认值更改为 128，以前它没有限制（配置为 0），这可能是潜在的 DoS 威胁

- 改进了接收到 Proxy Protocol 报文但 `proxy_protocol` 配置项未开启时的错误日志内容

- 为网关上报消息添加额外的消息属性。来自 CoAP, LwM2M，Stomp，ExProto 等网关的消息，在转换为 EMQX 的消息时，添加例如协议名称，协议版本，用户名，客户端 IP 等字段，可用于多语言钩子扩展

- HTTP 性能优化

- 将 openssl-1.1 添加到 RPM 依赖

### 问题修复

- 修复节点间 RPC 调用堵塞导致客户端进程失去响应的问题

- 修复锁管理进程 `ekka_locker` 在杀死挂起的锁持有者后 Crash 的问题

- 修复 RocketMQ 异步写入时数据乱码问题

- 修复 RocketMQ 统计指标不准的问题

- 修复集群节点数量超过七个时 Dashboard 监控页面的显示错误

- 修复规则引擎保存数据到 MySQL 时可能出现较高失败率的问题

- 修复规则引擎 Clickhouse 离线消息功能不可用的问题

- 修复规则引擎 MongoDB 离线消息功能中 Max Returned Count 选项无法使用的问题

- 修复规则引擎 WebHook Action 中的 Path 参数无法使用规则引擎变量的问题

- 修复 MongoDB 认证模块无法使用 Replica Set 模式等问题

- 修复集群间消息转发失序问题，相关配置项为 `rpc.tcp_client_num`

- 修复内存占用计算错误的问题

- 修复部分热配置失效的问题

- 修复远程主机无法访问时的 MQTT Bridge 故障问题（连接挂起）

- 修复 HTTP Headers 可能重复的问题
