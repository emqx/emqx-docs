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
ref: undefined
---

# Mnesia ACL

Mnesia ACL uses the built-in Mnesia database of EMQ X to store ACL rules, which can store data and dynamically manage ACLs to facilitate integration with external device management systems.

Plugin:

```bash
emqx_auth_mnesia
```

## ACL rules

### ACL Rule Structure Body

```json
{
	"username":"emqx",
	"clientid":"client1",
	"topic":"testtopic/1",
	"action":"pub",
	"allow": true
}
```

Rule field description:

- username: Match the client's Username.
- clientid: Match the client's Client.
- topic: Control topics, you can use wildcards, and you can add placeholders to topics to match client information, such as `t/%c`, then the topic will be replaced with the client ID of the current client when matching
  - %u: Username
  - %c: Client ID
- action: Operation action, optional value: pub | sub | pubsub
- allow: Whether allow

`username` and `clientid` are optional fields, when both a missing, the rule applies to all clients.

Mnesia ACL does not set rules by default, and you can use the HTTP API to manage ACL rules.


## Use the HTTP API to manage ACL rules

### Add ACL rule

API definition:

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

### Add ACL rules in batch

API definition:

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

### Check the added ACL rules

API definition:

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

### Check the specified ACL rule

API definition:

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

### Delete ACL rule

Delete the specified ACL rule：

API definition:

```bash
# Request
# Please note that ${topic} needs to be encoded with UrlEncode
DELETE api/v4/emqx_acl/${login}/${topic}

# Response
{
    "code": 0
}
```
