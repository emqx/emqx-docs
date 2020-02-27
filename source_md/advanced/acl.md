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

**发布订阅 ACL** 指对 **发布 (PUBLISH)/订阅 (SUBSCRIBE)** 操作的 **权限控制**。例如拒绝用户名为 `Anna` 向 `open/elsa/door` 发布消息。

EMQ X Broker 默认开启 ACL 检查，并默认 *未命中 ACL 规则时* 允许其完成发布订阅操作，具体配置在 `etc/emqx.conf` 中：

|  配置项            | 类型   | 可取值               | 默认值 | 说明               |
| ------------------ | ------ | -------------------- | ------ | ------------------ |
| enable_acl         | enum   | on<br>off            | on     | 是否开启 ACL 检查  |
| acl_nomatch        | enum   | allow<br>deny        | deny   | ACL 未命中时，'拒绝' 或 '允许' 发布/订阅 操作 |
| acl_deny_action    | enum   | ignore<br>disconnect | ignore | 当 ACL 检查失败后，执行的操作 |


在 MQTT v3.1 和 v3.1.1 协议中，发布操作被拒绝后服务器无任何报文错误返回，这是协议设计的一个缺陷。但在 MQTT v5.0 协议上已经支持应答一个相应的错误报文。


## ACL 规则

ACL 通常是一组规则的集合，其规则的逻辑格式为：

```bash
## Allow-Deny Who Pub-Sub Topic

"允许(Allow) / 拒绝(Deny)"  "谁(Who)"  "订阅(Subscribe) / 发布(Publish)" "主题列表(Topics)"
```

它表达了 *EMQ X Broker `允许/拒绝` 某客户端 `发布/订阅` 某个 `主题`*。


## ACL 检查逻辑

这些 ACL 规则仅有以下三个来源，按优先级顺序分为：

1. 缓存 (ACL Cache) 的 ACL 规则
2. 已启用插件中提供的 ACL 规则
3. 默认的 ACL 规则

即，当客户端发起 **发布/订阅** 操作时，EMQ X Broker 会：

- 优先检查该客户端在 EMQ X Broker 内存中的 **ACL 缓存**
- 若未命中，则执行插件中提供的 **ACL 规则**
- 仍未命中，则匹配默认的 ACL 规则
- 仍未命中，则检查 `acl_nomatch` 配置。并作为最终的 ACL 检查结果返回
- 其中任何一步命中，则立即返回所命中的检查结果


### ACL 缓存

EMQ X Broker 默认开启 ACL 缓存功能。它允许客户端在命中某条 ACL 规则后，便将其缓存至内存中，以便下次直接使用。

ACL 缓存相关的配置都在 `etc/emqx.conf` 中：

|  配置项            | 类型     | 可取值    | 默认值 | 说明         |
| ------------------ | -------- | --------- | ------ | ------------ |
| enable_acl_cache   | enum     | on<br>off | on     | 是否开启缓存 |
| acl_cache_max_size | integer  | > 0       | 32     | 最大缓存条数 |
| acl_cache_ttl      | duration | > 0       | 1m     | 最大缓存时间 |


#### 清除缓存

在更新 ACL 规则后，某些客户端由于已经存在缓存，则无法立即生效。若要立即生效，则需手动清除所有的 ACL 缓存：

参见 [管理 API - 清除 ACL 缓存](rest-api.md)

### 插件中的 ACL 规则

EMQ X Broker 发行包中的某些认证插件提供了 ACL 检查的功能。

当缓存中的 ACL 规则未命中后，则会执行这些插件提供的 ACL 检查函数。这些 ACL 检查函数会从相应的服务中去查找所能命中的 ACL 规则，例如：从 Redis，MySQL 服务等。

目前，仅以下插件提供 ACL 检查的功能：

| 名称                | 用途                                          |
| ------------------- | --------------------------------------------- |
| [emqx_auth_http][]  | 利用 HTTP 服务检查 ACL，并直接返回匹配结果    |
| [emqx_auth_mysql][] | 从 MySQL 数据库查询 ACL 规则，并执行匹配      |
| [emqx_auth_mongo][] | 从 MongoDB 数据库查询 ACL 规则，并执行匹配    |
| [emqx_auth_pgsql][] | 从 PostgreSQL 数据库查询 ACL 规则，并执行匹配 |
| [emqx_auth_redis][] | 从 Redis 数据库查询 ACL 规则，并执行匹配      |
| [emqx_auth_ldap][]  | 从 LDAP 服务查询 ACL 规则，并执行匹配         |


这些插件对于规则的存储方式各有差别，具体请实现请查看各插件的使用手册。

[emqx_auth_http]:  https://github.com/emqx/emqx-auth-http  "emqx-auth-http"
[emqx_auth_mysql]: https://github.com/emqx/emqx-auth-mysql "emqx-auth-mysql"
[emqx_auth_mongo]: https://github.com/emqx/emqx-auth-mongo "emqx-auth-mongo"
[emqx_auth_pgsql]: https://github.com/emqx/emqx-auth-pgsql "emqx-auth-pgsql"
[emqx_auth_redis]: https://github.com/emqx/emqx-auth-redis "emqx-auth-redis"
[emqx_auth_ldap]:  https://github.com/emqx/emqx-auth-ldap  "emqx-auth-ldap"


### 默认 ACL

EMQ X Broker 内置有默认的 ACL 规则，它是优先级最低规则表，在所有的 ACL 检查完成后，如果仍然未命中则检查默认的 ACL 规则。

该规则文件的路径，由 `etc/emqx.conf` 中的配置决定：

|  配置项        | 类型     | 可取值    | 默认值       | 说明              |
| -------------- | -------- | --------- | ------------ | ----------------- |
| acl_file       | string   | -         | etc/acl.conf | 默认 ACL 文件路径 |

该规则文件以 Erlang 语法的格式进行描述：

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

可知，默认的 ACL 主要是为了限制客户端对系统主题 `$SYS/#` 和全通配主题 `#` 的权限。


#### acl.conf 编写规则

`acl.conf` 文件中的规则按书写顺序从上往下匹配。

`acl.conf` 的语法规则包含在顶部的注释中，熟悉 Erlang 语法的可直接阅读文件顶部的注释。或参考以下的释义：

- 以 `%%` 表示行注释。
- 每条规则由四元组组成，以 `.` 结束。
- 元组第一位：表示规则命中成功后，执行权限控制操作，可取值为：
    * `allow`：表示 `允许`
    * `deny`： 表示 `拒绝`

- 元组第二位：表示规则所生效的用户，可使用的格式为：
    * `{user, "dashboard"}`：表明规则仅对 *用户名 (Username)* 为 "dashboard" 的用户生效
    * `{clientid, "dashboard"}`：表明规则仅对 *客户端标识 (ClientId)* 为 "dashboard" 的用户生效
    * `{ipaddr, "127.0.0.1"}`：表明规则仅对 *源地址* 为 "127.0.0.1" 的用户生效
    * `all`：表明规则对所有的用户都生效

- 元组第三位：表示规则所控制的操作，可取值为：
    * `publish`：表明规则应用在 PUBLISH 操作上
    * `subscribe`：表明规则应用在 SUBSCRIBE 操作上
    * `pubsub`：表明规则对 PUBLISH 和 SUBSCRIBE 操作都有效

- 元组第四位：表示规则所限制的主题列表，内容以数组的格式给出，例如：
    * `"$SYS/#"`：为一个 **主题过滤器 (Topic Filter)**；表示规则可命中与 `$SYS/#` 匹配的主题；如：可命中 "$SYS/#"，也可命中 "$SYS/a/b/c"
    * `{eq, "#"}`：表示字符的全等，规则仅可命中主题为 `#` 的字串，不能命中 `/a/b/c` 等

- 除此之外还存在两条特殊的规则：
    - `{allow, all}`：允许所有操作
    - `{deny, all}`：拒绝所有操作

在 `acl.conf` 修改完成后，并不会自动加载至 EMQ X Broker 系统。需要手动执行：

``` bash
./bin/emqx_ctl acl reload
```

`acl.conf` 中应只包含一些简单而通用的规则，使其成为系统基础的 ACL 原则。如果需要支持复杂、大量的 ACL 内容，你应该在 [认证插件](auth.md) 中去实现它。

## 超级用户

**超级用户** 权限的授予来自于客户端的认证过程，参见 [认证(Autentication)](auth.md)

超级用户默认具有对所有主题的 PUBLISH/SUBSCRIBE 权限。所以作为超级用户，不会进行发布订阅的权限检查。

