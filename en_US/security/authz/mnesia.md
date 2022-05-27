# Built-in Database

Mnesia ACL uses the built-in Mnesia database of EMQX to store ACL rules, which can store data and dynamically manage ACLs to facilitate integration with external device management systems.

Plugin:

```bash
emqx_auth_mnesia
```

## ACL rules

### ACL Rule Structure Body

```json
{
	"username": "emqx",
	"clientid": "client1",
	"topic": "testtopic/1",
	"action": "pub",
	"access": "allow"
}
```


Rule field description:

- username: Match the client's Username.
- clientid: Match the client's Client.
- topic: Control topics, you can use wildcards, and you can add placeholders to topics to match client information, such as `t/%c`, then the topic will be replaced with the client ID of the current client when matching
  - %u: Username
  - %c: Client ID
- action: Operation action, optional value: pub | sub | pubsub
- access: Whether to allow, deny, or ignore

`username` and `clientid` are optional fields, when both are missing the rule applies to all clients.

Mnesia ACL does not set rules by default, and you can use the HTTP API to manage ACL rules.


## Use the HTTP API to manage ACL rules

### Add ACL rule

+ Clientid ACL：

  ```bash
  # Request
  POST api/v4/acl
  {
    "clientid": "emqx_c",
    "topic": "Topic/A",
    "action": "pub",
    "access": "allow"
  }

  # Response
  {
      "data": {
          "clientid": "emqx_c",
          "topic": "Topic/A",
          "action": "pub",
          "access": "allow"
          "result": "ok"
      },
      "code":0
  }
  ```
+ Username ACL：

  ```bash
  # Request
  POST api/v4/acl
  {
    "username": "emqx_u",
    "topic": "Topic/A",
    "action": "pub",
    "access": "allow"
  }

  # Response
  {
      "data": {
          "username": "emqx_u",
          "topic": "Topic/A",
          "action": "pub",
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
    "topic": "Topic/A",
    "action": "pub",
    "access": "allow"
  }

  # Response
  {
      "data": {
          "all": "$all",
          "topic": "Topic/A",
          "action": "pub",
          "access": "allow"
          "result": "ok"
      },
      "code":0
  }
  ```

### Add ACL rules in batch

```bash
# Request
POST api/v4/acl
[
  {
    "clientid": "emqx_c_1",
    "topic": "Topic/A",
    "action": "pub",
    "access": "allow"
  },
  {
    "username": "emqx_u_1",
    "topic": "Topic/A",
    "action": "sub",
    "access": "allow"
  },
  {
    "topic": "Topic/+",
    "action": "pubsub",
    "access": "deny"
  }
]

# Response
{
    "data": [
      {
        "clientid": "emqx_c_1",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow",
        "result": "ok"
      },
      {
        "username": "emqx_u_1",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
        "result": "ok"
      },
      {
        "all": "$all",
        "topic": "Topic/+",
        "action": "pubsub",
        "access": "deny"
      },
    ],
    "code":0
}
```

### Check the added ACL rules

+ Clientid ACL：

  ```bash
  # Request
  GET api/v4/acl/clientid

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

### Check Username/Clientid specific ACL rules

+ Clientid ACL:

  ```bash
  # Request
  GET api/v4/acl/clientid/emqx_c

  # Response
  {
      "data": [
        {
          "topic": "Topic/A",
          "clientid": "emqx_c",
          "access": "allow",
          "action": "pub"
        },
        {
          "topic": "Topic/B",
          "clientid": "emqx_c",
          "access": "allow",
          "action": "pub"
        }
      ],
      "code": 0
  }
  ```
+ Username ACL:

  ```bash
  # Request
  GET api/v4/acl/username/emqx_u

  # Response
  {
      "data": [
        {
          "topic": "Topic/A",
          "username": "emqx_u",
          "access": "allow",
          "action": "pub"
        },
        {
          "topic": "Topic/B",
          "username": "emqx_u",
          "access": "allow",
          "action": "pub"
        }
      ],
      "code": 0
  }
  ```

### Delete ACL rule

+ Client ACL

  ```bash
  # Request
  # Please note that ${topic} needs to be encoded with UrlEncode
  DELETE api/v4/acl/clientid/${clientid}/topic/${topic}

  # Response
  {
      "code": 0
  }
  ```
+ Username ACL

  ```bash
  # Request
  # Please note that ${topic} needs to be encoded with UrlEncode
  DELETE api/v4/acl/username/${username}/topic/${topic}

  # Response
  {
      "code": 0
  }
  ```
+ $all ACL

  ```bash
  # Request
  # Please note that ${topic} needs to be encoded with UrlEncode
  DELETE api/v4/acl/$all/topic/${topic}

  # Response
  {
      "code": 0
  }
  ```
