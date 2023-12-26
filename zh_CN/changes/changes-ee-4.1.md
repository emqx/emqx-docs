# v4.1

## 4.1.5

*发布日期: 2020-08-30*

EMQ X 4.1.5 is released now, it fixes a bug in MQTT message parser.

## 4.1.4

*发布日期: 2020-08-29*

EMQ X 4.1.4 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue of abnormal memory growth caused by the topic metrics feature

  Github PR: [emqx#3680](https://github.com/emqx/emqx/pull/3680)

### emqx-bridge-mqtt

**Enhancements:**

- The clientid configuration item supports `${node}` placeholders to optimize the user experience under the cluster

  Github PR: [emqx-bridge-mqtt#99](https://github.com/emqx/emqx-bridge-mqtt/pull/99)

### emqx-management

**Bug fixes:**

- Fix the issue that the data migration function is not available under Windows

  Github PR: [emqx-management#262](https://github.com/emqx/emqx-management/pull/262)

### emqx-lua-hook

**Bug fixes:**

- Fix the issue that the Username field cannot be obtained

  Github PR: [emqx-lua-hook#115](https://github.com/emqx/emqx-lua-hook/pull/115)

## 4.1.3

*发布日期: 2020-07-24*

EMQ X 4.1.3 is released now, it mainly includes the following changes:

### emqx-management

**Bug fixes:**

- Add type checking for the payload field in PUBLISH API

  Github PR: [emqx/emqx-management#250](https://github.com/emqx/emqx-management/pull/250)

### emqx-retainer

**Bug fixes:**

- Fix the issue that the retained message will not be sent when the subscription topic contains both '+' and '#'

  Github PR: [emqx/emqx-retainer#146](https://github.com/emqx/emqx-retainer/pull/146)

## 4.1.2

*发布日期: 2020-08-08*

- 修复了一些已知问题

## 4.1.2

*发布日期: 2020-07-23*

EMQ X 4.1.2 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue that the topic alias is not used to replace the topic

  Github PR: [emqx/emqx#3616](https://github.com/emqx/emqx/pull/3616)

- Fix the issue that some operations take up too much CPU

  Github PR: [emqx/emqx#3581](https://github.com/emqx/emqx/pull/3581)

### emqx-rel

**Bug fixes:**

- Fix the issue that the console no longer outputs the log after the log is filled with all log files when running emqx by docker

  Github PR: [emqx/emqx-rel#559](https://github.com/emqx/emqx-rel/pull/559)

## 4.1.1

*发布日期: 2020-08-07*

1. 规则引擎 添加 Pulsar 消费组资源
2. 规则引擎 添加 Kafka 消费组资源
3. 规则引擎 添加数据保存到 TDengine 数据库
4. 规则引擎 添加离线消息保存到 MySQL 动作
5. 规则引擎 添加离线消息保存到 PostgreSQL 动作
6. 规则引擎 添加离线消息保存到 Cassandra 动作
7. 规则引擎 添加离线消息保存到 MongoDB 动作
8. 规则引擎 添加从 MySQL 中获取订阅关系
9. 规则引擎 添加从 PostgreSQL 中获取订阅关系
10. 规则引擎 添加从 Cassandra 中获取订阅关系
11. 规则引擎 添加从 MongoDB 中获取订阅关系
12. 规则引擎 保存数据到 MongoDB 动作支持消息模板
13. 修复 HTTP Publish API 无法支持 payload 为 json 格式的问题

## 4.1.1

*发布日期: 2020-07-09*

EMQ X 4.1.1 is released now, it mainly includes the following changes:

### emqx-retainer

**Bug fixes:**

- Fix performance issues

  Github PR: [emqx/emqx-retainer#141](https://github.com/emqx/emqx-retainer/pull/141)

### emqx-bridge-mqtt

**Bug fixes:**

- Change mount point to optional configuration

  Github PR: [emqx/emqx-bridge-mqtt#84](https://github.com/emqx/emqx-bridge-mqtt/pull/84)

### emqx-rel

**Bug fixes:**

- Hiding sensitive env from docker's logging out

  Github Issue: [emqx/emqx-rel#524](https://github.com/emqx/emqx-rel/pull/524)

  Github PR: [emqx/emqx-rel#542](https://github.com/emqx/emqx-rel/pull/542)

  Thanks: [emqx/emqx-rel#525](https://github.com/emqx/emqx-rel/pull/525) - [daadu](https://github.com/daadu)

### emqx-lua-hook

**Bug fixes:**

- Fix the issue that there is no unload script and CLI when the plugin is unloaded

  Github PR: [emqx/emqx-lua-hook#106](https://github.com/emqx/emqx-lua-hook/pull/106)

## 4.1.0

*发布日期: 2020-07-18*

1. 内置预览版 license，无需在官网注册获取 licese 可以直接启动 emqx
2. 修改 license 过期策略，emqx 服务不停，但是新连接无法登录
3. 规则引擎 添加 MQTT 订阅资源
4. 规则引擎 MQTT 消息桥接支持 pool
5. 规则引擎 MQTT 消息桥接修复集群无法使用 BUG
6. 规则引擎 添加数据保存到 ClickHouse 数据库
7. InfluxDB 支持 http/https 方式连接
8. 企业版多语言开发支持北向消息处理
9. 规则引擎 添加离线消息保存到 redis 动作
10. 规则引擎 添加从 redis 中获取订阅关系

## 4.1.0

*发布日期: 2020-06-04*

EMQ X 4.1.0 is released now, it mainly includes the following changes:

**Enhancements:**

- Support multi-language extension and provide SDK, supported languages: Python, Java
- Support topic based metrics
- Supports loading the latest configuration when the plugin starts
- Support for topic aliases when messages are forwarded
- Add subscription option configuration for proxy subscriptions
- Support fuzzy query and multi condition query of client list
- Support fuzzy query of subscription list
- Support to add simple authentication information on Dashboard
- Support data migration between versions
- MQTT AUTH Packet is supported. At present, only SCRAM-SHA-1 authentication mechanism is supported and users can expand it by themselves
- Support for obtaining network addresses and ports when using the proxy protocol
- Add authentication plugin based on Mnesia database (completely replace `emqx-auth-clientid` and `emqx-auth-username` plugins in subsequent versions)
- Support for editing rules in rule engine
- Support comment configuration items when running EMQ X through Docker
- LwM2M gateway plugin supports IPv6 and listens to multiple ports at the same time
- CoAP gateway plugin supports IPv6
- JWT authentication plugin supports configuration of jwerl signature format

**Bug fixes:**

- Fix the issue that EMQ X could not start when `etc/emqx.conf` was read-only
- Fix the issue that the connection process crashes in some cases
- Fix the issue that the browser doesn't support the current SSL/TLS certificates
- Fix the issue that the MQTT bridge plugin doesn't send heartbeat packets by default
- Fix the issue that the abnormal login detection function does not delete the expired data, resulting in memory growth
- Fix the issue that the built-in ACL module did not clear the ACL cache when reloading
- Fix the issue that `client.disconnected` event in WebHook plugin goes wrong in some cases
- Fix the issue that MQTT-SN gateway plugin doesn't support specifying listening IP address and supports IPv6
