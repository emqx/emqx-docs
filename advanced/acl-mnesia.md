---
# 标题
title: Mnesia ACL
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

# Mnesia ACL

Mnesia ACL 使用 EMQ X 内置的 Mnesia 数据库存储 ACL 规则，可以存储数据、动态管理 ACL，方便与外部设备管理系统集成

插件：

```bash
emqx_auth_mnesia
```

## ACL规则

Mnesia ACL 默认基于 MQTT 报文中的 Username 和密码进行权限认证, 可在 `etc/plugins/emqx_auth_mnesia.conf` 中更改为使用 MQTT 报文的的 Client ID 与密码认证：

```bash
## Auth and ACL base on username or clientid.
##
## Value: username | clientid
auth.mnesia.as = username
```

### ACL 规则结构体

```json
{
	"login":"emqx",
	"topic":"testtopic/1",
	"action":"pub",
	"allow": true
}
```

规则字段说明：

- login：根据 `auth.mnesia.as` 的值匹配客户端的 Username 或 Client ID.
- topic：控制的主题，可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c` 则在匹配时主题将会替换为当前客户端的 Client ID
  - %u：用户名
  - %c：Client ID
- action：操作行为，可选值：pub | sub | pubsub
- allow：是否允许
  
Mnesia ACL 默认不设规则，你可以使用 HTTP API 管理 ACL 规则。

## 使用 HTTP API 管理 ACL 规则

#### 添加 ACL 规则

API 定义：

```bash
# Request
POST api/v4/emqx_acl
{
	"login":"emqx",
	"topic":"Topic/A",
	"action":"pub",
	"allow": true
}

# Response
{
    "data": {
        "emqx": "ok"
    },
    "code": 0
}
```

#### 批量添加 ACL 规则

API 定义：

```bash
# Request
POST api/v4/emqx_acl
[
  {
	"login":"emqx_1",
	"topic":"Topic/A",
	"action":"pub",
	"allow": true
  },
  {
    "login":"emqx_2",
    "topic":"Topic/A",
    "action":"pub",
    "allow": true
  }
]

# Response
{
    "data": {
        "emqx_2": "ok",
        "emqx_1": "ok"
    },
    "code": 0
}
```

#### 查看已经添加的 ACL 规则

API 定义：

```bash
# Request
GET api/v4/emqx_acl

# Response
{
  "meta": {
    "page": 1,
    "limit": 10,
    "count": 1
  },
  "data": [
    {
      "topic": "Topic/A",
      "login": "emqx",
      "action": "pub"
    }
  ],
  "code": 0
}
```

#### 查看指定 ACL 规则

API 定义：

```bash
# Request
GET api/v4/emqx_acl/${login}

# Response
{
    "data": {
        "topic": "Topic/A",
        "login": "emqx",
        "allow": true,
        "action": "pub"
    },
    "code": 0
}
```

#### 删除 ACL 规则

删除指定 ACL 规则：

API 定义：

```bash
# Request
# 请注意 ${topic} 需要使用 UrlEncode 编码
DELETE api/v4/emqx_acl/${login}/${topic}

# Response
{
    "code": 0
}
```