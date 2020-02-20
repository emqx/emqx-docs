---
# 标题
title: 发布订阅 ACL
# 编写日期
date: 2020-02-19 09:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 发布订阅 ACL


**发布订阅 ACL** 即为对 **发布(PUBLISH)/订阅(SUBSCRIBE)** 操作的 **权限控制**。例如：拒绝用户名为 `Anna` 向 `open/elsa/door` 发布消息。

EMQ X Broker 默认开启 ACL 检查，并默认允许 *未命中 ACL 规则* 的发布订阅操作，具体配置在 `etc/emqx.conf` 中：

```bash
## 是否开启 ACL 检查
## 枚举值: on | off
enable_acl = on

## 若 ACL 未命中，'拒绝' 或 '允许' PUB/SUB 操作
## 枚举值：allow | deny
acl_nomatch = allow

## 当 ACL 检查失败后，执行的操作
## 枚举值：ignore | disconnect
acl_deny_action = ignore
```

> 注：在 MQTTv3.1.1 和 v3.1 协议中，Qos0 的发布操作被拒绝后服务器无任何报文错误返回，Qos1 和 Qos2 的发布操作则是无法收到 ACK 报文。这是协议设计的一个缺陷；但在 MQTTv5.0 协议上已经支持，EMQ X Broker 会发送想要的错误报文

## ACL 规则

所有的权限控制，都是由一条条的 **ACL 规则** 组成。其中一条 ACL 规则的 **逻辑格式** 可以写作为：

```bash
## Allow-Deny Who Pub-Sub Topic

"允许(Allow) / 拒绝(Deny)"  "谁(Who)"  "订阅(Subscribe) / 发布(Publish)" "主题列表(Topics)"
```

它表达了 *EMQ X Broker ‘允许/拒绝’ 某客户端 ‘发布/订阅’ 某 ‘个’ 主题*。


## ACL 检查逻辑

这些 ACL 规则仅有以下三个来源，按优先级顺序分为：

1. 缓存(ACL Cache)
2. 按序执行已开启的插件列表中提供 ACL 检查
3. 默认 ACL


即，当客户端执行 **发布/订阅** 操作时：

- 优先检查自身当前进程的 **ACL 缓存**
- 若未命中，则执行插件中提供的 **ACL 检查**
- 仍未命中，则匹配默认 ACL 规则
- 仍未命中，则检查 `acl_nomatch` 配置。以该配置的值，作为 ACL 检查的结果返回
- 其中任何一步命中，则立即返回所命中的检查结果

该执行逻辑可以表述为:

[ACL Desgin](#todo)


### ACL 缓存

EMQ X Broker 默认开启 ACL 缓存，它允许客户端在检查某条 ACL 规则成功后，便将其缓存至自己的进程数据中，以便下次直接使用提高性能。

在 `etc/emqx.conf` 中，可配置是否启用 ACL 缓存功能、缓存的最大 ACL 规则条数和缓存失效时间：

``` properties
## 是否开启 ACL 缓存
## 枚举值：on | off
enable_acl_cache = on

## The maximum count of ACL entries can be cached for a client.
##
## Value: Integer greater than 0
## Default: 32

## ACL 缓存条数
acl_cache_max_size = 32

## ACL 缓存时间
acl_cache_ttl = 1m
```

#### 清除缓存

在更新 ACL 规则后，某些客户端由于已经存在旧的 ACL 缓存，则无法立即生效。如果想要立即生效，则需要手动清除所有的 ACL 缓存：

参见 [管理 API - 清除 ACL 缓存](#todo)

### 插件中的 ACL 检查

在各类认证插件中，提供了 ACL 检查的支持。当 EMQ X Broker 在缓存中无法命中 ACL 规则后，则会调用已经加载插件里的 ACL 检查函数。

目前，仅以下插件提供 ACL 检查的功能：

| 名称                | 用途                         |
| ------------------- | ---------------------------- |
| [emqx_auth_http][]  | 利用 HTTP 服务检查 ACL       |
| [emqx_auth_mysql][] | 利用 MySQL 服务检查 ACL      |
| [emqx_auth_mongo][] | 利用 MongoDB 服务检查 ACL    |
| [emqx_auth_pgsql][] | 利用 PostgreSQL 服务检查 ACL |
| [emqx_auth_redis][] | 利用 Redis 服务检查 ACL      |
| [emqx_auth_ldap][]  | 利用 LDAP 服务检查 ACL       |


> 注：这些插件对于规则的存储方式各有差别，具体请实现请查看各个插件的使用手册

> 注：插件的 ACL 支持实现，依赖于 `client.check_acl` 钩子，关于实现细节可参考 [钩子(Hooks)](#todo)

[emqx_auth_http]:  https://github.com/emqx/emqx-auth-http  "emqx-auth-http"
[emqx_auth_mysql]: https://github.com/emqx/emqx-auth-mysql "emqx-auth-mysql"
[emqx_auth_mongo]: https://github.com/emqx/emqx-auth-mongo "emqx-auth-mongo"
[emqx_auth_pgsql]: https://github.com/emqx/emqx-auth-pgsql "emqx-auth-pgsql"
[emqx_auth_redis]: https://github.com/emqx/emqx-auth-redis "emqx-auth-redis"
[emqx_auth_ldap]:  https://github.com/emqx/emqx-auth-ldap  "emqx-auth-ldap"


### 内置 ACL

EMQ X Broker 内置有默认的 ACL 规则，在缓存和插件中未命中 ACL 后，就会走到内置的 ACL 规则检查。

内置的 ACL 规则保存在一个配置文件中，由 `etc/emqx.conf` 中的配置决定：

``` properties
## 默认的 ACL 文件路径
acl_file = etc/acl.conf
```

在 `etc/acl.conf` 文件中，其配置格式以 Erlang 配置文件语法的形式给出。

其中包括了：

``` erlang
%% 允许 "dashboard" 用户 订阅 "$SYS/#" 主题
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

%% 允许 IP 地址为 "127.0.0.1" 的用户 发布/订阅 "#SYS/#"，"#" 主题
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

%% 拒绝 "所有用户" 订阅 "$SYS/#" "#" 主题
{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

%% 允许其它任意的发布订阅操作
{allow, all}.
```
可知，内置的 ACL 主要是为了限制用户对系统主题 `$SYS/#` 和全通配主题 `#` 的操作权限

#### acl.conf 编写规则

`acl.conf` 文件中的规则按书写顺序从上往下匹配。

`acl.conf` 的语法规则包含在顶部的注释中，熟悉 Erlang 语法的可直接阅读文件顶部的注释。或参考以下的释义：

- 以 `%%` 表示行注释
- 每条规则由 4 元组组成，以 `.` 号结束

- 元组第一位：表示规则命中成功后，执行权限控制操作，可取值为：
    * `allow`：表示 `允许`
    * `deny`： 表示 `拒绝`

- 元组第二位：表示规则所生效的用户，可使用的格式为：
    * `{user, "dashboard"}`：表明规则仅对 *用户名(Username)* 为 "dashboard" 的用户生效
    * `{clientid, "dashboard"}`：表明规则仅对 *客户端标识(ClientId)* 为 "dashboard" 的用户生效
    * `{ipaddr, "127.0.0.1"}`：表明规则仅对 *源地址* 为 "127.0.0.1" 的用户生效
    * `all`：表明规则对所有的用户都生效

- 元组第三位：表示规则所控制的操作，可取值为：
    * `publish`：表明规则应用在 PUBLISH 操作上
    * `subscribe`：表明规则应用在 SUBSCRIBE 操作上
    * `pubsub`：表明规则对 PUBLISH 和 SUBSCRIBE 操作都有效

- 元组第四位：表示规则所限制的主题列表，内容以数组的格式给出，例如：
    * `"$SYS/#"`：为一个 **主题过滤器(Topic Filter)**；表示规则可命中与 `$SYS/#` 匹配的主题；如：可命中 "$SYS/#"，也可命中 "$SYS/a/b/c"
    * `{eq, "#"}`：表示全等，规则仅可命中主题为 `#`，不能命中 `/a/b/c` 等

- 除此之外还存在两条特殊的规则：
    - `{allow, all}`：允许所有操作
    - `{deny, all}`：拒绝所有操作

在修改完成后 `acl.conf` 后，并不会自动加载至 EMQ X Broker 系统。需要手动执行：

``` bash
./bin/emqx_ctl acl reload
```

> 注：`acl.conf` 中应只包含一些简单而通用的规则，使其成为系统基础的 ACL 原则。如果需要支持复杂、大量的 ACL 内容，你应该在 [认证插件](#todo) 中去实现它。

## 超级用户

**超级用户** 权限的授予来自于客户端的认证过程，参见 [认证(Autentication)](#todo)

超级用户默认具有对所有主题的 PUB/SUB 权限。所以作为超级用户，不会进行 发布订阅的权限检查。

