---
# 标题
title: LDAP 认证
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
ref: undefined
---

# LDAP

LDAP authentication uses an external LDAP server as the authentication data source, which can store a large amount of data and facilitate integration with external device management systems.

Plugin:

```bash
emqx_auth_ldap
```

::: tip 
The emqx_auth_ldap plugin also includes ACL feature, which can be disabled via comments.
The current version only supports openldap and does not support Microsoft active directory.
:::



## LDAP Configuration

To enable LDAP authentication, you need to configure the following in `etc/plugins/emqx_auth_ldap.conf`:

```bash
# etc/plugins/emqx_auth_ldap.conf

auth.ldap.servers = 127.0.0.1

auth.ldap.port = 389

auth.ldap.pool = 8

## ldap's Binding Distinguished Name (DN)
auth.ldap.bind_dn = cn=root,dc=emqx,dc=io

## 	ldap's Binding password
auth.ldap.bind_password = public

## ldap's query timeout
auth.ldap.timeout = 30s

## ldap's device distinguished name
auth.ldap.device_dn = ou=device,dc=emqx,dc=io

## ldap's matching object class
auth.ldap.match_objectclass = mqttUser

## ldap's username attribute type
auth.ldap.username.attributetype = uid

## 	ldap's password attribute type
auth.ldap.password.attributetype = userPassword

## TLS Configuration item
## auth.ldap.ssl.certfile = etc/certs/cert.pem
## auth.ldap.ssl.keyfile = etc/certs/key.pem
## auth.ldap.ssl.cacertfile = etc/certs/cacert.pem
## auth.ldap.ssl.verify = verify_peer
## auth.ldap.ssl.fail_if_no_peer_cert = true
```


## LDAP Schema 

The data model needs to be configured in the LDAP schema directory. By default, the data model is as follows:

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

The configuration file slapd.conf was edited with reference of Schema:

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
