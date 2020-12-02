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


Mnesia ACL 使用 Mnesia 数据库存储 ACL 规则，可以存储数据、动态管理 ACL，方便与外部设备管理系统集成

插件：

```bash
emqx_auth_mnesia
```

## 认证规则

Mnesia 认证默认使用客户端的 Username 和密码进行认证, 可在可在 `etc/plugins/emqx_auth_mnesia.conf` 中更改为使用客户端的 Client ID 与密码认证：

```bash
## Auth as username or auth as clientid.
##
## Value: username | clientid
auth.mnesia.as = username
```

### ACL 规则数据

```
## 格式
-record(emqx_acl, {
        login,
        topic,
        action,
        allow
    }).

## 结构
#emqx_acl{
  login = emqx, 
  topic = Topic/A, 
  action = pub,  
  allow = true
  }
```

Mnesia ACL 一条规则中定义了发布、订阅或发布/订阅的信息，在规则中的都是**允许**列表。

规则字段说明：

- login：根据 `auth.mnesia.as` 的值匹配客户端的 Username 或 Client ID.
- topic：控制的主题，可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c` 则在匹配时主题将会替换为当前客户端的 Client ID
  - %u：用户名
  - %c：Client ID
- action：操作行为，可选值：pub | sub | pubsub
- allow：是否允许
  
Mnesia ACL 默认没有任何配置的ACL规则，用户可以使用 HTTP API 管理 ACL 规则



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
    "code": 0,
    "data": ["emqx","emqx_1","emqx_2"]
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