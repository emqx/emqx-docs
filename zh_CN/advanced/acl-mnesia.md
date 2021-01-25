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

# Mnesia ACL

Mnesia ACL 使用 EMQ X 内置的 Mnesia 数据库存储 ACL 规则，可以存储数据、动态管理 ACL，方便与外部设备管理系统集成

插件：

```bash
emqx_auth_mnesia
```

## ACL 规则结构体

```json
{
	"clientid":"emqx",
	"topic":"testtopic/1",
	"action":"pub",
	"access": "allow"
}
```

规则字段说明：

- clientid / username：客户端的 Username 或 Client ID.
- topic：控制的主题，可以使用通配符，并且可以在主题中加入占位符来匹配客户端信息，例如 `t/%c` 则在匹配时主题将会替换为当前客户端的 Client ID
  - %u：用户名
  - %c：Client ID
- action：操作行为，可选值：pub | sub | pubsub
- Access：是否允许，可选值：allow | deny
  

Mnesia ACL 默认不设规则，你可以使用 HTTP API 和 `emqx_ct`l 管理 ACL 规则。

## 使用 HTTP API 管理 ACL 规则

### 添加 ACL 规则

+ Clinetid ACL：

  ```bash
  # Request
  POST api/v4/acl
  {
    "clientid":"emqx_c",
    "topic":"Topic/A",
    "action":"pub",
    "access": "allow"
  }

  # Response
  {
      "data": {
          "clientid":"emqx_c",
          "topic":"Topic/A",
          "action":"pub",
          "access": "allow"
          "result": "ok"
      },
      "code": 0
  }
  ```
+ Username ACL：

  ```bash
  # Request
  POST api/v4/acl
  {
    "username":"emqx_u",
    "topic":"Topic/A",
    "action":"pub",
    "access": "allow"
  }

  # Response
  {
      "data": {
          "username":"emqx_u",
          "topic":"Topic/A",
          "action":"pub",
          "access": "allow"
          "result": "ok"
      },
      "code": 0
  }
  ```
+ $all ACL:

  ```bash
  # Request
  POST api/v4/acl
  {
    "topic":"Topic/A",
    "action":"pub",
    "access": "allow"
  }

  # Response
  {
      "data": {
          "all": "$all",
          "topic":"Topic/A",
          "action":"pub",
          "access": "allow"
          "result": "ok"
      },
      "code": 0
  }
  ```

### 批量添加 ACL 规则

```bash
# Request
POST api/v4/acl
[
  {
    "clientid":"emqx_c_1",
    "topic":"Topic/A",
    "action":"pub",
    "access": "allow"
  },
  {
    "username":"emqx_u_1",
    "topic":"Topic/A",
    "action":"sub",
    "access": "allow"
  },
  {
    "topic":"Topic/+",
    "action":"pubsub",
    "access": "deny"
  }
]

# Response
{
    "data": [
      {
        "clientid":"emqx_c_1",
        "topic":"Topic/A",
        "action":"pub",
        "access": "allow",
        "result": "ok"
      },
      {
        "username":"emqx_u_1",
        "topic":"Topic/A",
        "action":"pub",
        "access": "allow"
        "result": "ok"
      },
      {
        "all": "$all",
        "topic":"Topic/+",
        "action":"pubsub",
        "access": "deny"
      },
    ],
    "code": 0
}
```

### 查看已经添加的 ACL 规则

+ Clinetid ACL：

  ```bash
  # Request
  GET api/v4/acl/clinetid

  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
      {
        "clientid": "emqx_c",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "clientid": "emqx_c_1",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "clientid": "emqx_c_2",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      }
    ],
    "code": 0
  }
  ```
  
+ Username ACL：

  ```bash
  # Request
  GET api/v4/acl/username

  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
      {
        "username": "emqx_u",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "username": "emqx_u_1",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "username": "emqx_u_2",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      }
    ],
    "code": 0
  }
  ```
  
+ $all ACL：

  ```bash
  # Request
  GET api/v4/acl/$all

  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
      {
        "all": "$all",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "all": "$all",
        "topic": "Topic/+",
        "action": "pubsub",
        "access": "deny"
      }
    ],
    "code": 0
  }
  ```
  
### 查看指定 ACL 规则

+ Clientid ACL

  ```bash
  # Request
  GET api/v4/acl/clientid/${clientid}

  # Response
  {
      "data": {
          "topic": "Topic/A",
          "clientid": "emqx_c",
          "allow": true,
          "action": "pub"
      },
      "code": 0
  }
  ```
+ Username ACL

  ```bash
  # Request
  GET api/v4/acl/username/${username}

  # Response
  {
      "data": {
          "topic": "Topic/A",
          "username": "emqx_u",
          "allow": true,
          "action": "pub"
      },
      "code": 0
  }
  ```
  
### 删除 ACL 规则

+ Clinet ACL

  ```bash
  # Request
  # 请注意 ${topic} 需要使用 UrlEncode 编码
  DELETE api/v4/acl/clinetid/${clientid}/topic/${topic}

  # Response
  {
      "code": 0
  }
  ```
+ Username ACL

  ```bash
  # Request
  # 请注意 ${topic} 需要使用 UrlEncode 编码
  DELETE api/v4/acl/username/${username}/topic/${topic}

  # Response
  {
      "code": 0
  }
  ```
+ $all ACL

  ```bash
  # Request
  # 请注意 ${topic} 需要使用 UrlEncode 编码
  DELETE api/v4/acl/$all/topic/${topic}

  # Response
  {
      "code": 0
  }
  ```