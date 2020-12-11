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

# LDAP 认证

<!-- TODO: 为测试通过，从新宇 auth 文档中取 -->

LDAP 认证使用外部 LDAP 服务器作为认证数据源，可以存储大量数据，同时方便与外部设备管理系统集成。

插件：

```bash
emqx_auth_ldap
```

::: tip 
emqx_auth_ldap 插件同时包含 ACL 功能，可通过注释禁用。
目前版本仅支持 OpenLDAP，不支持 Microsoft Active Directory。
:::



## LDAP 配置

要启用 LDAP 认证，需要在 `etc/plugins/emqx_auth_ldap.conf` 中配置以下内容：

```bash
# etc/plugins/emqx_auth_ldap.conf

auth.ldap.servers = 127.0.0.1

auth.ldap.port = 389

auth.ldap.pool = 8

## ldap 的绑定专有名称(DN)
auth.ldap.bind_dn = cn=root,dc=emqx,dc=io

## 	ldap 的绑定密码
auth.ldap.bind_password = public

## ldap 的查询超时时间
auth.ldap.timeout = 30s

## ldap 的设备专有名
auth.ldap.device_dn = ou=device,dc=emqx,dc=io

## ldap 的匹配对象类
auth.ldap.match_objectclass = mqttUser

## ldap 的用户名属性类型
auth.ldap.username.attributetype = uid

## 	ldap 的密码属性类型
auth.ldap.password.attributetype = userPassword

## TLS 配置项
## auth.ldap.ssl.certfile = etc/certs/cert.pem
## auth.ldap.ssl.keyfile = etc/certs/key.pem
## auth.ldap.ssl.cacertfile = etc/certs/cacert.pem
## auth.ldap.ssl.verify = verify_peer
## auth.ldap.ssl.fail_if_no_peer_cert = true
```


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
