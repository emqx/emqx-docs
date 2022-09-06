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

## 4.3.19 版本

*发布日期: 2022-08-29*

### Enhancements

- 改进 LwM2M 报文解析失败时的日志
- 改进规则引擎错误日志，动作执行失败时的日志中将包含规则 ID（
- 改进 `loaded_modules` 和 `loaded_plugins` 文件不存在时的提醒日志
- Dashboard 新增修改默认密码的引导

### Bug fixes

- 修复 `client.disconnected` 在某些情况下不会触发的问题
- 修复内置数据库认证未区分客户端 ID 和用户名的认证数据的分页统计的问题
- 修复 Redis 驱动进程泄漏的问题
- 修复规则引擎 MQTT 桥接至 AWS IOT 连接超时的问题
- 修复监听器未就绪时 `GET /listener` 请求崩溃的问题
- 修复 v4.3.12 版本后规则引擎 SQL 中任意变量与空值比较总是返回 false 的问题
- 修复错误地将 `emqx_modules` 应用作为插件管理的问题
- 修复 ExHook 的执行优先级高于规则引擎时，被 ExHook Message Hook 过滤的主题将无法触发规则引擎的问题
- 修复 ExHook 管理进程因 supervisor 关闭超时而被强制杀死的问题
- 修复 ExProto `client.connect` 钩子中 Client ID 参数未定义的问题
- 修复客户端被踢除时 ExProto 不会触发断开连接事件的问题

## 4.3.18 版本

*发布日期: 2022-08-11*

### 重要变更

- 升级了使用的 OTP 版本，以解决 OTP Bug 导致的低概率出现随机进程失去响应的问题，建议仍在使用 4.3 的用户升级到此版本
- 从下一版本起，我们将停止对 macOS 10 的支持，转为提供 macOS 11 的安装包

### 功能增强

- 允许配置连接进程在 TLS 握手完成后进行垃圾回收以减少内存占用，这可以使每个 SSL 连接减少大约 35% 的内存消耗，但相应地会增加 CPU 的消耗
- 允许配置 TLS 握手日志的日志等级以便查看详细的握手过程

## 4.3.17 版本

*发布日期: 2022-07-29*

### 功能增强

- 支持对规则引擎中的规则进行搜索和分页
- 提供 CLI `./bin/emqx check_conf` 以主动检查配置是否正确
- 优化共享订阅性能

### 错误修复

- 修复热升级后一旦卸载了老版本 EMQX 将无法再次启动的问题
- 修复多语言协议扩展中对 UDP 客户端的保活检查错误导致客户端不会过期的问题
- 修复多语言协议扩展中客户端信息没有及时更新的问题
- 修复客户端指定 Clean Session 为 false 重连时，飞行窗口中的共享订阅消息会被尝试重新派发给旧会话进程的问题
- 修复 `emqx_lua_hook` 插件无法阻止消息发布的问题

## 4.3.16 版本

*发布日期: 2022-06-30*

### 功能增强

- 规则引擎消息重发布动作中的 QoS 和保留消息标识现在可以使用占位符
- 支持排他订阅，即一个主题只允许存在一个订阅者
- 现在 Dashboard 和管理 API 的 HTTPS 监听器可以使用受密码保护的私钥文件，提供了 `key_password` 配置项
- 支持在主题重写规则中使用占位符 `%u` 和 `%c`
- 支持在消息发布的 API 请求中设置 MQTT 5.0 的 Properties，例如消息过期间隔、响应主题等
- 优化规则引擎资源创建时的 UI，例如折叠部分不常用的选项等
- 为 ExHook 底层的 gRPC 连接开放了 KeepAlive、TCP_NODELAY、SO_RCVBUF 和 SO_SNDBUF 共 4 个与 TCP 相关的配置项

### 错误修复

- 修复 Linux 系统中内存计算不准确的问题，并改为计算当前系统的内存占用，而不是 EMQX 的内存占用
- 修复 ExHook 在客户端重连时旧的断开连接事件会晚于新的连接事件触发的问题
- 修复主题重写与延迟发布执行顺序不固定的问题，现在固定为优先执行主题重写
- 修复规则引擎无法编码 MQTT 5.0 用户属性的问题
- 修复客户端使用 MQTT v5.0 以下的协议版本接入时 `connack.auth_error` 计数不准确的问题
- 修复 LwM2M 和 CoAP 网关的 UDP 监听器无法绑定指定网络接口的问题
- 修复在配置文件中移除默认的 Dashboard 用户后 Dashboard 无法启动的问题
- 修复 `client.subscribe` 钩子无法拒绝订阅的问题
- 如果 ACL 规则中的占位符没有被替换，则客户端的发布或订阅操作将被拒绝

## 4.3.15 版本

*发布日期: 2022-06-01*

### 功能增强

- 为规则引擎 SQL 增加更多的时间转换函数
- 为规则引擎 SQL 增加 `float2str/2` 函数，支持指定浮点输出精度
- 支持将 JWT 用于鉴权，现在 MQTT 客户端可以使用包含发布订阅白名单的特定声明进行授权
- 改进认证相关指标使更易理解，现在 `client.authenticate = client.auth.success + client.auth.failure`
- 支持 REST API 的监听器绑定到指定的网络接口上
- 支持对使用内置数据库作为数据源的认证鉴权中的用户数据进行多条件查询和模糊查询
- 支持将消息队列长度以及丢弃消息数量作为条件查询客户端
- 支持配置日志时间格式以兼容旧版本中的时间格式
- 当 `use_username_as_clientid` 配置为 `true` 且客户端连接时未指定 `username`，现在将拒绝连接并返回 `0x85` 原因码
- App secret 从部分随机改为完全随机
- 现在不兼容版本之间的热升级将被拒绝
- 允许 EMQX 的安装路径中有空格
- 引导脚本将在遇到无效的节点名称时快速失败，并提高错误消息的可读性

### 错误修复

- 修复规则引擎 SQL 函数 `hexstr_to_bin/1` 无法处理半字节的问题
- 修复规则引擎资源删除时告警未被清除的问题
- 修复 Dashboard HTTPS 监听器的 `verify` 选项未生效的问题
- 修复共享订阅投递 QoS 1 消息过程中对端会话关闭导致消息丢失的问题
- 修复日志跟踪功能跟踪大报文时堆大小增长过快而触发连接进程强制关闭策略的问题
- 修复 MQTT-SN 客户端重传 QoS 2 消息时会被断开连接的问题
- 修复对订阅进行多条件查询时返回结果与查询条件不符的问题
- 修复规则引擎资源连接测试不工作的问题
- 修复多项 Dashboard 显示问题

## 4.3.14 版本

*发布日期: 2022-04-18*

### 功能增强

- 规则引擎支持拷贝规则以快速复用
- 规则引擎 SQL 支持 zip、gzip 等压缩和解压缩函数
- 改进规则引擎在解析 Payload 失败时的错误提示
- 优化规则引擎部分资源的连接测试
- 支持为 ExHook 设置执行优先级
- ExHook 回调接口新增 `RequestMeta meta` Protobuf 字段用于返回 EMQX 集群名称
- 为共享订阅添加 `local` 策略，这将优先向消息流入的节点下的共享订阅者发送消息。在某些场景下会提升共享消息调度的效率，尤其是在 MQTT 桥接配置为共享订阅时
- 为 TLS 新增对 `RSA-PSK-AES256-GCM-SHA384`、`RSA-PSK-AES256-CBC-SHA384`、`RSA-PSK-AES128-GCM-SHA256`、`RSA-PSK-AES128-CBC-SHA256` 四个 PSK 加密套件的支持，从默认配置中移除 `PSK-3DES-EDE-CBC-SHA` 和 `PSK-RC4-SHA` 这两个不安全的加密套件
- 打印 Mnesia `wait_for_table` 诊断日志
  - 打印 Mnesia 内部统计的检查点
  - 打印每个表加载统计的检查点，帮助定位表加载时间长的问题
- 严格模式下禁止订阅为空的主题
- 当 `loaded_modules` 和 `loaded_plugins` 文件不存在时生成默认文件

### 错误修复

- 修复 TLS 配置项 `server_name_indication` 设置为 disable 不生效的问题
- 修复 MongoDB 驱动潜在的进程泄漏问题
- 修复通过 CLI 命令修改的 Dashboard 默认用户的密码会在节点离开集群后重置的问题
- 静默 `docker-entrypoint.sh` 中的 grep 和 sed 命令的运行错误日志
- 修复 API 路径包含 ISO8859-1 转义字符时，备份文件无法被正确删除和下载
- 修复 Redis 驱动在 DNS 解析失败等情况下会引发崩溃的问题
- 修复规则引擎发送数据到 Web 服务动作中 Headers 字段配置不生效的问题
- 修复 MQTT Bridge 插件仅配置订阅主题但未配置 QoS 时无法启动的问题
- 创建规则时如果已经有使用相同 ID 的规则存在，现在规则引擎将报错而不是替换已有规则
- 修复 HTTP 驱动进程池可能无法删除的问题

## 4.3.13 版本

*发布日期: 2022-04-01*

### 重要变更

- 对于 Docker 镜像，配置目录 `/opt/emqx/etc` 已经从 VOLUME 列表中删除，这使用户可以更容易地使用更改后的配置来重建镜像。
- CentOS 7 Erlang 运行系统在 OpenSSL-1.1.1n（之前是 1.0）上重建，在 v4.3.13 之前，客户端使用某些密码套件时，EMQX 将无法成功握手并触发 `malformed_handshake_data` 异常。
- CentOS 8 Erlang 运行时系统在 RockyLinux 8 上重新构建。 `centos8` 将继续保留在包名中以保持向后兼容。

### 功能增强

- 新增命令行接口 `emqx_ctl pem_cache clean`，允许强制清除 x509 证书缓存，以在证书文件更新后立即重新加载。
- 重构 ExProto，以便匿名客户端也可以显示在 Dashboard 上。
- 桥接中的主题配置项现在可以使用 `${node}` 占位符。
- 严格模式下新增对 MQTT 报文中的 UTF-8 字符串有效性检查。设置为 `true` 时，无效的 UTF-8 字符串将导致客户端连接断开。
- MQTT-SN 网关支持会话恢复时主动同步注册主题。
- 将规则引擎浮点型数据的写入精度从为小数点后 10 位提升至 17 位。
- EMQX 将在启动时提示如何修改 Dashboard 的初始密码。

### 错误修复

- 修复 el8 安装包在 Amazon Linux 2022 上无法启动的问题，错误内容为 `errno=13 Permission denied`。
- 修复某些情况下如果连接进程阻塞，客户端无法重连的问题，现在等待超过 15 秒无响应将强制关闭旧的连接进程。
- 修复规则引擎资源不可用时查询资源请求超时的问题。
- 修复热升级运行失败后再次运行出现 `{error, eexist}` 错误的问题。
- 修复向不存在的主题别名发布消息会导致连接崩溃的问题。
- 修复通过 HTTP API 在另一个节点上查询 lwm2m 客户端列表时的 500 错误。
- 修复主题订阅的 HTTP API 在传入非法的 QoS 参数时崩溃的问题。
- 修复通过多语言协议扩展功能接入的连接进程异常退出时未释放相关资源导致连接计数不更新的问题。
- 修复 `server_keepalive` 配置项的值会被错误应用于 MQTT v3.1.1 客户端的问题。
- 修复 Stomp 客户端无法触发 `$event/client_connection` 事件消息的问题。
- 修复 EMQX 启动时系统内存告警误激活的问题。
- 修复向 MQTT-SN 客户端成功注册主题时没有重传此前因未注册主题而投递失败的消息的问题。
- 修复 `loaded_plugins` 文件中配置了重复的插件时 EMQX 启动输出错误日志的问题。
- 修复 MongoDB 相关功能在配置不正确时输出过量错误日志的问题。
- 增加对 Dashboard User 与 AppID 的格式检查，不允许出现 `/` 等特殊字符。
- 将踢除客户端时返回的 DISCONNECT 报文中的原因码更正为 `0x98`。
- 代理订阅将忽略为空的主题。

## 4.3.12 版本

*发布日期: 2022-02-11*

### 增强

- 规则引擎支持为客户端消息异常丢失事件配置规则与动作，以增强用户在这一场景的自定义处理能力
- 改进规则引擎 SQL 匹配执行过程中的相关统计指标
- 客户端模糊搜索支持 `*`， `(`，`)` 等特殊字符
- 改进 ACL 相关统计指标，解决命中 ACL 缓存导致计数不增加的问题
- Webhook 事件通知中新增 `connected_at` 字段
- 在因持有锁太久而终止客户端之前记录客户端状态

### 修复

- 修复 Metrics 接口默认情况下不返回 client.acl.deny 等认证鉴权指标的问题
- 修复订阅查询接口未返回分页数据的问题
- 修复 STOMP 处理 TCP 粘包时解析失败的问题
- 修复客户端过滤查询时会话创建时间选项不可用的问题
- 修复重启后内存告警可能不会触发的问题
- 修复 `emqx_auth_mnesia` 插件中存在用户数据时导入数据崩溃的问题

## 4.3.11 版本

*发布日期: 2021-12-17*

EMQX 4.3.11 现已发布，主要包含以下改动:

### 增强

- 支持配置是否继续投递空的保留消息，以适应仍在使用 MQTT v3.1 协议的用户

### 修复

- 修复内存占用计算错误的问题
- 修复规则引擎 Webhook Action 的 Path 参数不支持使用 ${Variable} 的问题
- 修复某些情况下停止 MQTT Bridge 插件，会持续打印连接失败日志的问题

## 4.3.10 版本

*发布日期: 2021-11-11*

EMQX 4.3.10 现已发布，主要包含以下改动:

**错误修复（重要）:**

- 修复 STOMP 网关热升级失败

  Github PR: [emqx#6110](https://github.com/emqx/emqx/pull/6110)

- 修复通过 Dashboard 修改监听器配置后 emqx 将无法启动的问题

  Github PR: [emqx#6121](https://github.com/emqx/emqx/pull/6121)

**功能增强:**

- 为 MQTT 客户端引入压力反馈

  Github PR: [emqx#6065](https://github.com/emqx/emqx/pull/6065)

## 4.3.9 版本

*发布日期: 2021-11-02*

EMQX 4.3.9 现已发布，主要包含以下改动:

**错误修复（重要）:**

- 修复集群间调用可能导致客户端进程失去响应的问题

  Github PR: [emqx#6062](https://github.com/emqx/emqx/pull/6062)

- 修复 WebHook TLS 不可用的问题

  Github PR: [emqx#5696](https://github.com/emqx/emqx/pull/5696)

- 修复 MongoDB 资源不支持域名的问题

  Github PR: [emqx#6035](https://github.com/emqx/emqx/pull/6035)

- 修复基于内置数据库的 ACL 的性能问题

  Github PR: [emqx#5885](https://github.com/emqx/emqx/pull/5885)

- 修复基于内置数据库的认证错误转码 HTTP 请求参数的问题

  Github PR: [emqx#5674](https://github.com/emqx/emqx/pull/5674)

- 修复规则引擎在集群环境下禁用规则后资源无法释放的问题

  Github PR: [emqx#5731](https://github.com/emqx/emqx/pull/5731)

- 修复 STOMP 网关若干问题

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

- 支持优先级队列的交错（以避免低优先级队列枯竭）

  Github PR: [emqx#5666](https://github.com/emqx/emqx/pull/5666)

- 默认为 HTTP 认证插件关闭超级用户请求

  Github PR: [emqx#5567](https://github.com/emqx/emqx/pull/5567)

## 4.3.8 版本

*发布日期: 2021-09-06*

EMQX 4.3.8 现已发布，主要包含以下改动:

**错误修复:**

- 修复规则引擎规则导入失败的问题

  Github PR: [emqx#5512](https://github.com/emqx/emqx/pull/5512)

- 修复规则引擎 Webhook 动作中 Path 字段无法使用的问题

  Github PR: [emqx#5468](https://github.com/emqx/emqx/pull/5468)

- 修复 Force Shutdown 机制在进程挂起时无法生效的问题

  Github PR: [emqx#5460](https://github.com/emqx/emqx/pull/5460)

- 修复某些情况下 k8s 部署 EMQX 集群无法正确重启的问题

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

EMQX 4.3.7 现已发布，主要包含以下改动:

**错误修复:**

- 修复当前 HTTP KeepAlive 行为可能导致某些服务器断开连接的问题

  Github PR: [emqx#5395](https://github.com/emqx/emqx/pull/5395)

- 修复命令行接口无法打印某些字符的问题

  Github PR: [emqx#5411](https://github.com/emqx/emqx/pull/5411)

- 修复 LwM2M 网关下发整型数字时编码错误的问题

  Github PR: [emqx#5425](https://github.com/emqx/emqx/pull/5425)

## 4.3.6 版本

*发布日期: 2021-07-28*

EMQX 4.3.6 现已发布，主要包含以下改动:

**功能改进:**

- 支持关闭 HTTP Pipelining

  Github PR: [emqx#5279](https://github.com/emqx/emqx/pull/5279)

- ACL 支持 IP 地址列表

  Github PR: [emqx#5328](https://github.com/emqx/emqx/pull/5328)

## 4.3.5 版本

*发布日期: 2021-06-28*

EMQX 4.3.5 现已发布，主要包含以下改动:

**错误修复:**

- 修复同一客户端建立多个共享订阅时可能在取消订阅后出现消息丢失的问题

  Github PR: [emqx#5098](https://github.com/emqx/emqx/pull/5098)

## 4.3.4 版本

*发布日期: 2021-06-23*

EMQX 4.3.4 现已发布，主要包含以下改动:

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

EMQX 4.3.3 现已发布，主要包含以下改动:

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

EMQX 4.3.2 现已发布，主要包含以下改动:

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

EMQX 4.3.1 现已发布，主要包含以下改动:

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

EMQX 4.3.0 现已发布，主要包含以下改动:

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

- 保护 EMQX Broker 免受跨站点 WebSocket 劫持攻击
- SSL 支持 `verify` 与 `server_name_indication` 配置项
- SSL 支持证书链最大长度以及私钥文件密码配置项
- JWT 认证支持 JWKS

#### 其他

- 规则引擎新增更新资源逻辑
- 规则引擎 SQL 函数支持 unix 时间戳与 rfc3339 格式时间之间的转换
- 保持对 EMQX Broker 启动后连接失败的资源进行重试
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
- 修复 Docker 中 EMQX 可能启动失败的问题
- OOM 时快速杀死连接进程
- 修复 Clean Session 为 false 的 MQTT-SN 连接在非正常断开时没有发布遗嘱消息的问题

## 4.3-rc.5 版本

*发布日期: 2021-04-26*

EMQX 4.3-rc.5 现已发布，主要包含以下改动:

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

- 修复 Docker 中 EMQX 可能启动失败的问题

  Github PR: [emqx#4670](https://github.com/emqx/emqx/pull/4670), [emqx#4675](https://github.com/emqx/emqx/pull/4675), [emqx#4657](https://github.com/emqx/emqx/pull/4657)

- 规则引擎资源未初始化成功时将相应规则状态设为不可用

  Github Issue: [emqx#4642](https://github.com/emqx/emqx/issues/4642)
  Github PR: [emqx#4643](https://github.com/emqx/emqx/pull/4643)

- 修复在 EMQX 未完全启动时上报遥测数据导致的问题

  Github PR: [emqx#4627](https://github.com/emqx/emqx/pull/4627)

- 修复启动 emqx-exhook 插件必须配置 HTTPS 证书的问题

  Github PR: [emqx#4678](https://github.com/emqx/emqx/pull/4678)

## 4.3-rc.4 版本

*发布日期: 2021-04-16*

EMQX 4.3-rc.4 现已发布，主要包含以下改动:

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

EMQX 4.3-rc.3 现已发布，主要包含以下改动:

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

EMQX 4.3-rc.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 emqx 和 emqx_ctl 命令在某些情况下不可用的问题

  Github PR: [emqx#4430](https://github.com/emqx/emqx/pull/4430)

## 4.3-rc.1 版本

*发布日期: 2021-03-23*

EMQX 4.3-rc.1 现已发布，主要包含以下改动:

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

EMQX 4.3-beta.1 现已发布，主要包含以下改动:

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

- 保持对 EMQX Broker 启动后连接失败的资源进行重试

  Github PR: [emqx#4125](https://github.com/emqx/emqx/pull/4125)

**错误修复:**

- 修复过长的 Client ID 无法追踪的问题

  Github PR: [emqx#4163](https://github.com/emqx/emqx/pull/4163)

- 修复查询客户端信息可能出现崩溃的问题

  Github PR: [emqx#4124](https://github.com/emqx/emqx/pull/4124)

## 4.3-alpha.1 版本

*发布日期: 2021-01-29*

EMQX 4.3-alpha.1 现已发布，主要包含以下改动:

*功能*

- 支持 Erlang/OTP 23
- 新安装包仅支持 macOS 10.14 及以上版本
- 规则引擎新增更新资源逻辑
- 增强 Webhook 与 HTTP 认证性能
- 多语言扩展功能底层实现方式由 erlport 改为 gRPC
- 保护 EMQX Broker 免受跨站点 WebSocket 劫持攻击
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
