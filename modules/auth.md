# ACL File 访问控制

内置 ACL 通过文件设置规则，使用上足够简单轻量，适用于规则数量可预测、无变动需求或变动较小的项目。

通过dashboard页面可以开启 内置 ACL 访问控制模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200927213049265](./assets/acl_file1.png)

选择 内置 ACL 访问控制模块

![image-20200927213049265](./assets/acl_file2.png)

配置相关参数

![image-20200927213049265](./assets/acl_file3.png)

点击添加后，模块添加完成

![image-20200927213049265](./assets/acl_file4.png)

## 定义 ACL

内置 ACL 是优先级最低规则表，在所有的 ACL 检查完成后，如果仍然未命中则检查默认的 ACL 规则。

该规则文件以 Erlang 语法的格式进行描述：

```erlang
%% 允许 "dashboard" 用户 订阅 "$SYS/#" 主题

{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

%% 允许 IP 地址为 "127.0.0.1" 的用户 发布/订阅 "#SYS/#"，"#" 主题

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

%% 拒绝 "所有用户" 订阅 "$SYS/#" "#" 主题

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

%% 允许其它任意的发布订阅操作

{allow, all}.
```

1. 第一条规则允许客户端发布订阅所有主题

2. 第二条规则禁止全部客户端订阅 `$SYS/#` 与 `#` 主题

3. 第三条规则允许 ip 地址为 `127.0.0.1` 的客户端发布/订阅 `$SYS/#` 与 `#` 主题，为第二条开了特例

4. 第四条规则允许用户名为 `dashboard` 的客户端订阅 `$SYS/#` 主题，为第二条开了特例

可知，默认的 ACL 主要是为了限制客户端对系统主题 `$SYS/#` 和全通配主题 `#` 的权限。

## acl.conf 编写规则

`acl.conf` 文件中的规则按书写顺序从上往下匹配。

`acl.conf` 的语法规则包含在顶部的注释中，熟悉 Erlang 语法的可直接阅读文件顶部的注释。或参考以下的释义：

- 以 `%%` 表示行注释。

- 每条规则由四元组组成，以 `.` 结束。

- 元组第一位：表示规则命中成功后，执行权限控制操作，可取值为：

​    * `allow`：表示 `允许`

​    * `deny`： 表示 `拒绝`

- 元组第二位：表示规则所生效的用户，可使用的格式为：

​    * `{user, "dashboard"}`：表明规则仅对 *用户名 (Username)* 为 "dashboard" 的用户生效

​    * `{clientid, "dashboard"}`：表明规则仅对 *客户端标识 (ClientId)* 为 "dashboard" 的用户生效

​    * `{ipaddr, "127.0.0.1"}`：表明规则仅对 *源地址* 为 "127.0.0.1" 的用户生效

​    * `all`：表明规则对所有的用户都生效

- 元组第三位：表示规则所控制的操作，可取值为：

​    * `publish`：表明规则应用在 PUBLISH 操作上

​    * `subscribe`：表明规则应用在 SUBSCRIBE 操作上

​    * `pubsub`：表明规则对 PUBLISH 和 SUBSCRIBE 操作都有效

- 元组第四位：表示规则所限制的主题列表，内容以数组的格式给出，例如：

​    * `"$SYS/#"`：为一个 **主题过滤器 (Topic Filter)**；表示规则可命中与 `$SYS/#` 匹配的主题；如：可命中 "$SYS/#"，也可命中 "$SYS/a/b/c"

​    * `{eq, "#"}`：表示字符的全等，规则仅可命中主题为 `#` 的字串，不能命中 `/a/b/c` 等

- 除此之外还存在两条特殊的规则：

- `{allow, all}`：允许所有操作

- `{deny, all}`：拒绝所有操作

{% hint style="info" %}

acl.conf 中应只包含一些简单而通用的规则，使其成为系统基础的 ACL 原则。如果需要支持复杂、大量的 ACL 内容，你应该在认证插件中去实现它。

{% endhint %}

# LDAP 认证/访问控制

# HTTP 认证/访问控制

# PostgreSQL 认证/访问控制

# MySQL 认证/访问控制

# MongoDB 认证/访问控制

# 内置数据库 认证/访问控制

# PSK 认证

# JWT 认证

# Redis 认证/访问控制

Redis 认证/访问控制使用外部 Redis 数据库作为数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

搭建 Redis 环境，以 MaxOS X 为例:

```bash
 $ wget http://download.redis.io/releases/redis-4.0.14.tar.gz
$ tar xzf redis-4.0.14.tar.gz
$ cd redis-4.0.14
$ make && make install

# 启动 redis
$ redis-server
```

通过dashboard页面可以开启 Redis 认证/访问控制模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200927213049265](./assets/auth_redis1.png)

选择 Redis 认证/访问控制模块

![image-20200927213049265](./assets/auth_redis2.png)

配置相关参数

![image-20200927213049265](./assets/auth_redis3.png)

点击添加后，模块添加完成

![image-20200927213049265](./assets/auth_redis4.png)

##认证默认数据结构

Redis 认证默认配置下使用哈希表存储认证数据，使用 `mqtt_user:` 作为 Redis 键前缀，数据结构如下：

```
redis> hgetall mqtt_user:emqx
password public
```

默认配置下示例数据如下：

```
HMSET mqtt_user:emqx password public
```

启用 Redis 认证后，你可以通过用户名： emqx，密码：public 连接。

{% hint style="info" %} 

这是默认配置使用的数据结构，熟悉该插件的使用后你可以使用任何满足条件的数据结构进行认证。

{% endhint %}

## 加盐规则与哈希方法

```
## 不加盐，明文
plain

## 不加盐，仅做哈希处理
sha256

## salt 前缀：使用 sha256 加密 salt + 密码 拼接的字符串
salt,sha256

## salt 后缀：使用 sha256 加密 密码 + salt 拼接的字符串
sha256,salt

## pbkdf2 with macfun iterations dklen
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
pbkdf2,sha256,1000,20
```

##  认证查询命令（auth query cmd）

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证查询命令，查询出该客户端在 Redis 中的认证数据。

```
HMGET mqtt_user:%u password
```

你可以在命令中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名

- %c：Client ID

- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效

- %d：TLS 证书 subject，仅当 TLS 连接时有效

你可以根据业务需要调整认证查询命令，使用任意 [Redis 支持的命令](http://redisdoc.com/index.html)，但是任何情况下认证查询命令需要满足以下条件：

1. 查询结果中第一个数据必须为 password，EMQ X 使用该字段与客户端密码比对

2. 如果启用了加盐配置，查询结果中第二个数据必须是 salt 字段，EMQ X 使用该字段作为 salt（盐）值

## 访问控制默认数据结构

### ACL 规则数据

```
## 格式
HSET mqtt_acl:[username clientid][topic] [access]

## 结构
redis> hgetall mqtt_acl:emqx
testtopic/1 1
```

默认配置下示例数据：

```
HSET mqtt_acl:emqx # 1

HSET mqtt_acl:testtopic/2 2
```

## ACL 查询命令（acl cmd）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户命令，如果没有启用超级用户命令或客户端不是超级用户，则使用 ACL 查询命令查询出该客户端在数据库中的 ACL 规则。

```bash
HGETALL mqtt_acl:%u
```

你可以在 ACL 查询命令中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名

- %c：Client ID

你可以根据业务需要调整 ACL 查询命令，但是任何情况下 ACL 查询命令需要满足以下条件：

1. 哈希中使用 topic 作为键，access 作为值

## 超级用户查询命令（super cmd）

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户查询命令，查询客户端是否为超级用户。客户端为超级用户时将跳过 ACL 查询命令。

```bash
HGET mqtt_user:%u is_superuser
```

你可以在命令中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名

- %c：Client ID

- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效

- %d：TLS 证书 subject，仅当 TLS 连接时有效

你可以根据业务需要调整超级用户查询命令，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户查询命令需要满足以下条件：

1. 查询结果中第一个数据必须为 is_superuser 数据

{% hint style="info" %} 

如果不需要超级用户功能，注释并禁用该选项能有效提高效率

{% endhint %}