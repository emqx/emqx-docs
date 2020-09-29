# ACL File 访问控制

内置 ACL 通过文件设置规则，使用上足够简单轻量，适用于规则数量可预测、无变动需求或变动较小的项目。

通过dashboard页面可以开启 内置 ACL 访问控制模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200927213049265](./assets/modules.png)

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
LDAP 认证/访问控制使用外部 OpenLDAP 服务器作为认证数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

## 配置模块

通过dashboard页面可以开启 内置 ACL 访问控制模块
打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200928161310952](./assets/modules.png)

选择 LDAP 认证/访问控制模块

![image-20200928144927769](./assets/auth_ldap1.png)

配置OpenLDAP相关参数

![image-20200928144945076](./assets/auth_ldap2.png)

最后点击“添加”按钮，模块即可添加成功：

![image-20200928145033628](./assets/auth_ldap3.png)

## LDAP Schema

需要在 LDAP schema 目录配置数据模型，默认配置下数据模型如下：

**/etc/openldap/schema/emqx.schema**

```bash
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.1.3 NAME 'isEnabled'
EQUALITY booleanMatch
SYNTAX 1.3.6.1.4.1.1466.115.121.1.7
SINGLE-VALUE
USAGE userApplications )

attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.1 NAME ( 'mqttPublishTopic' 'mpt' )
EQUALITY caseIgnoreMatch
SUBSTR caseIgnoreSubstringsMatch
SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.2 NAME ( 'mqttSubscriptionTopic' 'mst' )
EQUALITY caseIgnoreMatch
SUBSTR caseIgnoreSubstringsMatch
SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.3 NAME ( 'mqttPubSubTopic' 'mpst' )
EQUALITY caseIgnoreMatch
SUBSTR caseIgnoreSubstringsMatch
SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
USAGE userApplications )

objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
AUXILIARY
MAY ( mqttPublishTopic $ mqttSubscriptionTopic $ mqttPubSubTopic) )

objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.2 NAME 'mqttDevice'
SUP top
STRUCTURAL
MUST ( uid )
MAY ( isEnabled ) )

objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.3 NAME 'mqttSecurity'
SUP top
AUXILIARY
MAY ( userPassword $ userPKCS12 $ pwdAttribute $ pwdLockout ) )
```

编辑 ldap 的配置文件 slapd.conf 引用 Schema：

**/etc/openldap/slapd.conf**

```bash
include  /etc/openldap/schema/core.schema
include  /etc/openldap/schema/cosine.schema
include  /etc/openldap/schema/inetorgperson.schema
include  /etc/openldap/schema/ppolicy.schema
include  /etc/openldap/schema/emqx.schema

database bdb
suffix "dc=emqx,dc=io"
rootdn "cn=root,dc=emqx,dc=io"
rootpw {SSHA}eoF7NhNrejVYYyGHqnt+MdKNBh4r1w3W

directory       /etc/openldap/data
```

默认配置下示例数据如下：
```
## create emqx.io

dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

# create testdevice.emqx.io
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

dn:uid=mqttuser0001,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
objectClass: mqttDevice
objectClass: mqttSecurity
uid: mqttuser0001
isEnabled: TRUE
mqttAccountName: user1
mqttPublishTopic: mqttuser0001/pub/1
mqttSubscriptionTopic: mqttuser0001/sub/1
mqttPubSubTopic: mqttuser0001/pubsub/1
userPassword:: e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9
```

启用 LDAP 认证后，你可以通过用户名： mqttuser0001，密码：public 连接。

## LDAP 访问控制配置方式

`mqttPublishTopic` 允许发布的主题(可以配置多个)

`mqttSubscriptionTopic` 允许订阅的主题(可以配置多个)

`mqttPubSubTopic` 允许订阅/发布的主题(可以配置多个)

{% hint style="danger" %}

目前版本仅支持 OpenLDAP，不支持 Microsoft Active Directory。

{% endhint %}

# HTTP 认证/访问控制

HTTP 认证/访问控制使用外部自建 HTTP 应用认证数据源，根据 HTTP API 返回的数据判定认证结果，能够实现复杂的认证鉴权逻辑和实现复杂的 ACL 校验逻辑。

通过dashboard页面可以开启 HTTP 认证/访问控制模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200927213049265](./assets/modules.png)

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

PostgreSQL 认证/访问控制使⽤外部 PostgreSQL 数据库作为数据源，可以存储⼤量数据，同时⽅便与外部设备管理系统集成。

## 安装PostgreSQL

打开PostgreSQL官网:https://www.postgresql.org/download/, 选择自己需要的版本，这里我们选择PostgreSQL版本为macos-10.13

![image-20200928140039111](./assets/auth_pg1.png)

安装完毕以后启动PostgreSQL。

## 3.配置模块

打开 EMQ X Dashboard，点击左侧的 “模块” 选项卡

![image-20200928161310952](./assets/modules.png)

选择PostgreSQL 认证/权限控制模块

![image-20200928140145957](./assets/auth_pg2.png)

需要配置PostgreSQL的地址，用户名，密码（可选）等基本连接参数表

![image-20200928140218313](./assets/auth_pg3.png)

最后点击“添加”按钮，模块即可添加成功

![image-20200928140218313](./assets/auth_pg4.png)

### 认证表：

```sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying(100),
  password character varying(100),
  salt character varying(40)
);
```

字段说明:

- username：连接客户端的用户名，此处的值如果设置为 `$all` 表示该规则适用于所有的用户
- password：连接客户端的密码参数
- salt：密码加盐字符串
- is_superuser：是否是超级用户

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证 SQL，查询出该客户端在数据库中的认证数据。

```sql
select password from mqtt_user where username = '%u' limit 1
```

字段说明

- %u：用户名
- %c：clientid
- %P：明文密码
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

可以根据业务需要调整认证 SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下认证 SQL 需要满足以下条件：

1. 查询结果中必须包含 password 字段，EMQ X 使用该字段与客户端密码比对
2. 如果启用了加盐配置，查询结果中必须包含 salt 字段，EMQ X 使用该字段作为 salt（盐）值
3. 查询结果只能有一条，多条结果时只取第一条作为有效数据

默认配置下示例数据如下：

```sql
INSERT INTO `mqtt_user` ( `username`, `password`, `salt`)
VALUES
    ('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL);
```

启用 PostgreSQL认证后，你可以通过用户名： emqx，密码：public 连接。

{% hint style="info" %}

可以在 SQL 中使用 AS 语法为字段重命名指定 password，或者将 salt 值设为固定值。

{% endhint %}

### 访问控制表

```sql
CREATE TABLE mqtt_acl (
  id SERIAL primary key,
  allow integer,
  ipaddr character varying(60),
  username character varying(100),
  clientid character varying(100),
  access  integer,
  topic character varying(100)
);
```

字段说明：

- allow：禁止（0），允许（1）
- ipaddr：设置 IP 地址
- username：连接客户端的用户名，此处的值如果设置为 `$all` 表示该规则适用于所有的用户
- clientid：连接客户端的 clientid
- access：允许的操作：订阅（1），发布（2），订阅发布都可以（3）
- topic：控制的主题，可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c`在匹配时主题将会替换为当前客户端的 clientid

访问控制的原理是从PostgreSQL中查找跟客户端相关的条目，然后进行鉴权，默认的查询SQL如下：

```sql
select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

可以在认证 SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：clientid
- %a：客户端 IP 地址
- %P：明文密码
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

默认配置下示例数据：

```sql
-- 所有用户不可以订阅系统主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, '$all', NULL, 1, '$SYS/#');

-- 允许 10.59.1.100 上的客户端订阅系统主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, '10.59.1.100', NULL, NULL, 1, '$SYS/#');

-- 禁止客户端订阅 /smarthome/+/temperature 主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, NULL, NULL, 1, '/smarthome/+/temperature');

-- 允许客户端订阅包含自身 Client ID 的 /smarthome/${clientid}/temperature 主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, NULL, NULL, NULL, 1, '/smarthome/%c/temperature');
```

## 超级用户

超级用户可以订阅和发布任何Topic，默认SQL如下:

```sql
select is_superuser from mqtt_user where username = '%u' limit 1
```

你可以在 SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：clientid
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

你可以根据业务需要调整超级用户 SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户 SQL 需要满足以下条件：

1. 查询结果中必须包含 is_superuser 字段，is_superuser 应该显式的为 true
2. 查询结果只能有一条，多条结果时只取第一条作为有效数据

{% hint style="info" %}

如果不需要超级用户功能，注释并禁用该选项能有效提高效率

{% endhint %}

## 加密规则

```shell
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

{% hint style="info" %}

可参考:[加盐规则与哈希方法](https://docs.emqx.net/broker/latest/cn/advanced/auth.html#加盐规则与哈希方法)。

{% endhint %}

# MySQL 认证/访问控制

MySQL 认证/访问控制使⽤外部 MySQL 数据库作为数据源，可以存储⼤量数据，同时⽅便与外部设备管理系统集成。

## 安装MySQL

打开MySQL官网:https://dev.MySQL.com/downloads/MySQL/5.7.html#downloads, 选择自己需要的版本，这里我们选择MySQL版本为macos-5.7.31

![image-20200928093151808](./assets/auth_mysql1.png)

安装完毕以后启动MySQL。

## 配置模块

打开 EMQ X Dashboard，点击左侧的 “模块” 选项卡

![image-20200928161310952](./assets/modules.png)

选择“添加”然后点击“模块”菜单，选择“添加”，然后选择MySQL 认证/权限控制模块

![image-20200928094942950](./assets/auth_mysql2.png)

需要配置MySQL的地址，用户名，密码（可选）等基本连接参数表

![image-20200928095050885](./assets/auth_mysql3.png)

最后点击“添加”按钮，模块即可添加成功

![image-20200928095050885](./assets/auth_mysql4.png)

### 认证表：

```sql
CREATE TABLE `mqtt_user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(100) DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `salt` varchar(35) DEFAULT NULL,
  `is_superuser` tinyint(1) DEFAULT 0,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `mqtt_username` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

字段说明:

- username：连接客户端的用户名，此处的值如果设置为 `$all` 表示该规则适用于所有的用户
- password：连接客户端的密码参数
- salt：密码加盐字符串
- is_superuser：是否是超级用户

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证 SQL，查询出该客户端在数据库中的认证数据。

```sql
select password from mqtt_user where username = '%u' limit 1
```

字段说明

- %u：用户名
- %c：clientid
- %P：明文密码
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

可以根据业务需要调整认证 SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下认证 SQL 需要满足以下条件：

1. 查询结果中必须包含 password 字段，EMQ X 使用该字段与客户端密码比对
2. 如果启用了加盐配置，查询结果中必须包含 salt 字段，EMQ X 使用该字段作为 salt（盐）值
3. 查询结果只能有一条，多条结果时只取第一条作为有效数据

默认配置下示例数据如下：

```sql
INSERT INTO `mqtt_user` ( `username`, `password`, `salt`)
VALUES
    ('emqx', 'efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7', NULL);
```

启用 MySQL 认证后，你可以通过用户名： emqx，密码：public 连接。

{% hint style="info" %}

可以在 SQL 中使用 AS 语法为字段重命名指定 password，或者将 salt 值设为固定值。

{% endhint %}

### 访问控制表

```sql
CREATE TABLE `mqtt_acl` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `allow` int(1) DEFAULT 1 COMMENT '0: deny, 1: allow',
  `ipaddr` varchar(60) DEFAULT NULL COMMENT 'IpAddress',
  `username` varchar(100) DEFAULT NULL COMMENT 'Username',
  `clientid` varchar(100) DEFAULT NULL COMMENT 'ClientId',
  `access` int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
```

字段说明：

- allow：禁止（0），允许（1）
- ipaddr：设置 IP 地址
- username：连接客户端的用户名，此处的值如果设置为 `$all` 表示该规则适用于所有的用户
- clientid：连接客户端的clientid
- access：允许的操作：订阅（1），发布（2），订阅发布都可以（3）
- topic：控制的主题，可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c`在匹配时主题将会替换为当前客户端的 clientid

访问控制的原理是从MySQL中查找跟客户端相关的条目，然后进行鉴权，默认的查询SQL如下：

```sql
select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

可以在认证 SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：clientid
- %a：客户端 IP 地址
- %P：明文密码
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

默认配置下示例数据：

```sql
-- 所有用户不可以订阅系统主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, '$all', NULL, 1, '$SYS/#');

-- 允许 10.59.1.100 上的客户端订阅系统主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, '10.59.1.100', NULL, NULL, 1, '$SYS/#');

-- 禁止客户端订阅 /smarthome/+/temperature 主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (0, NULL, NULL, NULL, 1, '/smarthome/+/temperature');

-- 允许客户端订阅包含自身 Client ID 的 /smarthome/${clientid}/temperature 主题
INSERT INTO mqtt_acl (allow, ipaddr, username, clientid, access, topic) VALUES (1, NULL, NULL, NULL, 1, '/smarthome/%c/temperature');
```



## 超级用户

超级用户可以订阅和发布任何Topic，默认SQL如下:

```sql
select is_superuser from mqtt_user where username = '%u' limit 1
```

你可以在 SQL 中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：clientid
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

你可以根据业务需要调整超级用户 SQL，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户 SQL 需要满足以下条件：

1. 查询结果中必须包含 is_superuser 字段，is_superuser 应该显式的为 true
2. 查询结果只能有一条，多条结果时只取第一条作为有效数据

## 加密规则

```shell
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

{% hint style="info" %}

可参考:[加盐规则与哈希方法](https://docs.emqx.net/broker/latest/cn/advanced/auth.html#加盐规则与哈希方法)。

{% endhint %}

## 特殊说明

MySQL 8.0 及以后版本使用了 `caching_sha2_password` 作为默认身份验证模块，受限于客户端驱动你必须将其更改为 `MySQL_native_password` 模块：

```sql
ALTER USER 'your_username'@'your_host' IDENTIFIED WITH MySQL_native_password BY 'your_password';
```

# MongoDB 认证/访问控制

MongoDB 认证/访问控制使⽤外部 MongoBD 数据库作为数据源，可以存储⼤量数据，同时⽅便与外部设备管理系统集成。

## 安装MongoDB

打开MongoDB官网地址:https://www.mongodb.com/try/download/communit, 选择你需要的版本,这里我们用MacOS V4.4.1版本:

![image-20200928112030369](./assets/auth_mongo1.png)

安装后启动MongoDB

## 配置插件

打开 [EMQ X Dashboard](http://127.0.0.1:18083/#/rules)，点击左侧的 “模块” 选项卡，选择添加

![image-20200928161310952](static/001.png)

选择 MongoDB 认证/访问控制模块

![image-20200928114546350](./assets/auth_mongo2.png)

配置 MongoDB 相关参数

![image-20200928114832162](./assets/auth_mongo3.png)

点击添加后，模块添加完成:

![image-20200928133916267](./assets/auth_mongo4.png)


## 认证集合

```json
{
  username: "user",
  password: "password hash",
  salt: "password salt",
  is_superuser: false,
  created: "2020-02-20 12:12:14"
}
```

进行身份认证时，EMQ X 将使用当前客户端信息填充并执行用户配置的认证 Query，查询出该客户端在数据库中的认证数据。

MongoDB 支持配置集合名称、认证字段、认证占位符等等参数。
| 配置项                         | 说明 |
| ------------------------------ | -------- |
| 认证查询集合         | 认证查询的MongoDB集合 |
| 认证查询字段名 | 需要从集合里面查询出来的字段，如果需要查询多个，使用逗号分隔。例如password,salt |
| 认证条件字段 | 认证查询的条件，如果需要查询多个，使用逗号分隔。例如 username=%u,clientid=%c |

你可以在认证查询占位符中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名
- %c：clientid
- %C：TLS 证书公用名（证书的域名或子域名），仅当 TLS 连接时有效
- %d：TLS 证书 subject，仅当 TLS 连接时有效

你可以根据业务需要调整认证查询，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下认证查询需要满足以下条件：

1. 查询结果中必须包含 password 字段，EMQ X 使用该字段与客户端密码比对
2. 如果启用了加盐配置，查询结果中必须包含 salt 字段，EMQ X 使用该字段作为 salt（盐）值
3. MongoDB 使用 findOne 查询命令，确保你期望的查询结果能够出现在第一条数据中

{% hint style="info" %}

这是默认配置使用的集合结构，熟悉该插件的使用后你可以使用任何满足条件的集合进行认证。

{% endhint %}

## 访问控制集合

```json
{
    username: "username",
    clientid: "clientid",
    publish: ["topic1", "topic2", ...],
    subscribe: ["subtop1", "subtop2", ...],
    pubsub: ["topic/#", "topic1", ...]
}
```

MongoDB ACL 一条规则中定义了发布、订阅和发布/订阅的信息，在规则中的都是**允许**列表。

规则字段说明：
| 配置项                         | 说明 |
| ------------------------------ | -------- |
| 访问控制查询集合         | 访问控制查询的MongoDB集合 |
| 访问控制查询字段名 | 需要从集合里面查询出来的字段 |
| 访问控制条件字段 | 访问控制查询的条件，支持and 和 or 操作，and操作通过逗号分隔，例如：username=%u,clientid=%c, or 操作需要添加多条数据 |

## 超级用户查询

进行 ACL 鉴权时，EMQ X 将使用当前客户端信息填充并执行用户配置的超级用户查询，查询客户端是否为超级用户。客户端为超级用户时将跳过 ACL 查询。
同一个选择器的多个条件时实际查询中使用 MongoDB and 查询：

```
db.mqtt_user.find({
  "username": "wivwiv"
  "clientid": "$all"
})
```
你可以在查询条件中使用以下占位符，执行时 EMQ X 将自动填充为客户端信息：

- %u：用户名

- %c：clientid

你可以根据业务需要调整超级用户查询，如添加多个查询条件、使用数据库预处理函数，以实现更多业务相关的功能。但是任何情况下超级用户查询需要满足以下条件：
查询结果中必须包含 is_superuser 字段，is_superuser 应该显式的为 true。
MongoDB 支持配置集合名称、认证字段、认证占位符等等参数。
| 配置项                         | 说明 |
| ------------------------------ | -------- |
| 超级用户查询集合         | 超级用户查询的MongoDB集合 |
| 超级用户查询字段名 | 需要从集合里面查询出来的字段 |
| 超级用户条件字段 | 超级用户查询的条件，如果需要查询多个，使用逗号分隔。例如 username=%u,clientid=%c |

{% hint style="danger" %}

MongoDB ACL 规则需严格使用上述数据结构。 MongoDB ACL 中添加的所有规则都是 允许 规则，可以搭配 `etc/emqx.conf` 中 `acl_nomatch = deny` 使用。

{% endhint %}

## 加密规则

```shell
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

{% hint style="info" %}

可参考:[加盐规则与哈希方法](https://docs.emqx.net/broker/latest/cn/advanced/auth.html#加盐规则与哈希方法)。

{% endhint %}

# 内置数据库 认证/访问控制

内置数据库认证使用 EMQ X 内置 Mnesia 数据库存储客户端 Clientid/Username 与密码，支持通过 HTTP API 管理认证数据。

内置数据库认证不依赖外部数据源，使用上足够简单轻量。

## 开启模块

打开 [EMQ X Dashboard](http://127.0.0.1:18083)，点击左侧的 “模块” 选项卡，选择添加

![image-20200928161310952](./assets/modules.png)

点击"选择",然后选择内置数据库认证模块

![image-20200928141305205](./assets/auth_mnesia_1.png)

配置相关参数

![image-20200927213049265](./assets/auth_mnesia_2.png)

最后点击“添加”按钮模块即可添加成功。

![image-20200928141558866](./assets/auth_mnesia_3.png)

## 管理数据

内置数据库可以通过 dashboard 管理认证与访问控制数据

![image-20200928141558866](./assets/auth_mnesia_4.png)

#### 认证数据

可以通过 dashboard 对认证数据进行管理

![image-20200928141558866](./assets/auth_mnesia_5.png)

当客户端连接 EMQ X 时，内置数据库认证会获取 CONNENT 报文中的 Clientid 与 Username，然后数据库中记录的密码进行匹配，一旦匹配成功则认证成功。

内置数据库认证还支持 HTTP API 操作，详情请查看 [Mnesia 认证](https://docs.emqx.net/broker/latest/cn/advanced/auth-mnesia.html)。

#### 访问控制数据

可以通过 dashboard 对访问控制数据进行管理

![image-20200928141558866](./assets/auth_mnesia_6.png)

当客户端向 EMQ X 进行发布、订阅、取消订阅时，访问控制可以根据内置数据库中提前设置的规则允许或拒绝该次操作。

内置数据库访问控制还支持 HTTP API 操作，详情请查看 [Mnesia ACL](https://docs.emqx.net/broker/latest/cn/advanced/acl-mnesia.html#mnesia-acl)。

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