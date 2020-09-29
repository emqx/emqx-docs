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

acl.conf 中应只包含一些简单而通用的规则，使其成为系统基础的 ACL 原则。如果需要支持复杂、大量的 ACL 内容可以选择外部资源去实现它。

{% endhint %}

# LDAP 认证/访问控制

# HTTP 认证/访问控制

HTTP 认证/访问控制使用外部自建 HTTP 应用认证数据源，根据 HTTP API 返回的数据判定认证结果，能够实现复杂的认证鉴权逻辑和实现复杂的 ACL 校验逻辑。

通过dashboard页面可以开启 HTTP 认证/访问控制模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200927213049265](./assets/auth_http1.png)

选择 HTTP 认证/访问控制模块

![image-20200927213049265](./assets/auth_http2.png)

配置相关参数

![image-20200927213049265](./assets/auth_http3.png)

点击添加后，模块添加完成

![image-20200927213049265](./assets/auth_http4.png)


## HTTP 认证原理

EMQ X 在设备连接事件中使用当前客户端相关信息作为参数，向用户自定义的认证服务发起请求查询权限，通过返回的 HTTP **响应状态码** (HTTP statusCode) 来处理认证请求。

 - 认证失败：API 返回 4xx 状态码
 - 认证成功：API 返回 200 状态码
 - 忽略认证：API 返回 200 状态码且消息体 ignore

## HTTP 请求信息

HTTP API 基础请求信息，配置证书、请求头与重试规则。

## 认证请求

进行身份认证时，EMQ X 将使用当前客户端信息填充并发起用户配置的认证查询请求，查询出该客户端在 HTTP 服务器端的认证数据。

```bash

## 认证请求地址
http://127.0.0.1:8991/mqtt/auth

## HTTP 请求方法
## Value: POST | GET
POST

## 请求参数
clientid=%c,username=%u,password=%P
```

HTTP 请求方法为 GET 时，请求参数将以 URL 查询字符串的形式传递；POST 请求则将请求参数以普通表单形式提交（content-type 为 x-www-form-urlencoded）。

你可以在认证请求中使用以下占位符，请求时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：Client ID
- %a：客户端 IP 地址
- %r：客户端接入协议
- %P：明文密码
- %p：客户端端口
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

{% hint style="danger" %}
推荐使用 POST 与 PUT 方法，使用 GET 方法时明文密码可能会随 URL 被记录到传输过程中的服务器日志中。
{% endhint %}


## HTTP 访问控制原理

EMQ X 在设备发布、订阅事件中使用当前客户端相关信息作为参数，向用户自定义的认证服务发起请求权限，通过返回的 HTTP **响应状态码** (HTTP statusCode) 来处理 ACL 授权请求。

 - 无权限：API 返回 4xx 状态码
 - 授权成功：API 返回 200 状态码
 - 忽略授权：API 返回 200 状态码且消息体 ignore

## HTTP 请求信息

HTTP API 基础请求信息，配置证书、请求头与重试规则。

进行发布、订阅认证时，EMQ X 将使用当前客户端信息填充并发起用户配置的 ACL 授权查询请求，查询出该客户端在 HTTP 服务器端的授权数据。

## superuser 请求

首先查询客户端是否为超级用户，客户端为超级用户时将跳过 ACL 查询。

```bash
# etc/plugins/emqx_auth_http.conf

## 超级用户请求地址
http://127.0.0.1:8991/mqtt/superuser

## HTTP 请求方法
## Value: POST | GET
POST

## 超级用户请求参数
clientid=%c,username=%u
```

## ACL 访问控制请求

```bash

## 访问控制请求地址
http://127.0.0.1:8991/mqtt/acl

## HTTP 请求方法
## Value: POST | GET
POST

## 访问控制请求参数
access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t,mountpoint=%m

```

## 请求说明

HTTP 请求方法为 GET 时，请求参数将以 URL 查询字符串的形式传递；POST、PUT 请求则将请求参数以普通表单形式提交（content-type 为 x-www-form-urlencoded）。

你可以在认证请求中使用以下占位符，请求时 EMQ X 将自动填充为客户端信息：

- %A：操作类型，'1' 订阅；'2' 发布
- %u：客户端用户名
- %c：Client ID
- %a：客户端 IP 地址
- %r：客户端接入协议
- %m：Mountpoint
- %t：主题

{% hint style="danger" %}
推荐使用 POST 与 PUT 方法，使用 GET 方法时明文密码可能会随 URL 被记录到传输过程中的服务器日志中。
{% endhint %}

# PostgreSQL 认证/访问控制

# MySQL 认证/访问控制

# MongoDB 认证/访问控制

# 内置数据库 认证/访问控制

# PSK 认证

# JWT 认证

[JWT](https://JWT.io/) 认证是基于 Token 的鉴权机制，不依赖服务端保留客户端的认证信息或者会话信息，在持有密钥的情况下可以批量签发认证信息，是最简便的认证方式。

## 选择模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200928161310952](./assets/modules.png)

点击"选择",然后选择 JWT 认证模块

![image-20200928141305205](./assets/auth_jwt1.png)

需要配置JWT密钥，认证源，公钥（可选）等基本连接参数表。

![image-20200928141327836](./assets/auth_jwt2.png)

如果需要添加自定义验证字段，可参考如下配置：

![image-20200928141558866](./assets/auth_jwt3.png)

最后点击“添加”按钮模块即可添加成功。

![image-20200928141558866](./assets/auth_jwt4.png)

## 认证原理

客户端使用 Token 作为用户名或密码（取决于模块配置），发起连接时 EMQ X 使用配置中的密钥、证书进行解密，如果能成功解密则认证成功，否则认证失败。

默认配置下启用 JWT 认证后，你可以通过任意用户名+以下密码进行连接：

```bash
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7ImF1dGhvciI6IndpdndpdiIsInNpdGUiOiJodHRwczovL3dpdndpdi5jb20ifSwiZXhwIjoxNTgyMjU1MzYwNjQyMDAwMCwiaWF0IjoxNTgyMjU1MzYwfQ.FdyAx2fYahm6h3g47m88ttyINzptzKy_speimyUcma4
```

{% hint style="info" %}

上述JWT Token仅做测试使用，可根据自己的业务需求用相关工具生成。此处提供一个在线生成工具：https://www.jsonwebtoken.io/。

{% endhint %}

## JWT参数含义

- 密钥：JWT密钥，用来加密JWT字符串。

- 认证来源：客户端携带 JWT 的位置，用于配置客户端 JWT 字符串携带位置，可选 username 与 password。
- 公钥文件：需要上传的JWT公钥文件。
- 签名格式：默认是der格式，可选择为JWT原生格式，选项为"raw"。

# Redis 认证/访问控制

Redis 认证/访问控制使用外部 Redis 数据库作为数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

搭建 Redis 环境，以 MacOS X 为例:

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

这是默认配置使用的数据结构，熟悉该模块的使用后，你可以使用任何满足条件的数据结构进行认证。

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