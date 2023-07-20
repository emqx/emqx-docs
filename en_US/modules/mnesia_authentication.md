# Internal Database

Built-in database authentication uses EMQX built-in Mnesia database to store the client's Clientid/Username and password, and supports management of authentication data through HTTP API.

The built-in database authentication does not rely on external data sources and is simple and lightweight enough to use.

## Create Module

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules), click the "Modules" tab on the left, and choose to add:

<img src="./assets/modules.png" alt="image-20200928161310952" style="zoom:50%;" />

Click "Select", and then select the built-in database authentication module.

<img src="./assets/auth_mnesia_1.png" alt="image-20200928141305205" style="zoom:50%;" />

Configure related parameters.

<img src="./assets/auth_mnesia_2.png" alt="image-20200927213049265" style="zoom:50%;" />

Finally, click the "Add" button to add the module successfully.

![image-20200928141558866](./assets/auth_mnesia_3.png)

## Manage Data

The built-in database can manage authentication and access control data through the Dashboard.

![image-20200928141558866](./assets/auth_mnesia_4.png)

### Authentication Data

The authentication data can be managed through the Dashboard.

![image-20200928141558866](./assets/auth_mnesia_5.png)

When the client connects to EMQX, the built-in database authentication will obtain the Clientid and Username in the CONNENT message, and then match the password recorded in the database. Once the match is successful, the authentication is successful.

### Access Control Data

Access control data can be managed through the Dashboard.

![image-20200928141558866](./assets/auth_mnesia_6.png)

When the client publishes, subscribes, or unsubscribes to EMQX, the access control can allow or deny the operation according to the rules set in advance in the built-in database.

## Set with HTTP API

The built-in database authentication and access control also support HTTP API operations. 

### Authentication

EMQX leverages its internal Mnesia database for authentication, eliminating reliance on external resources. This database stores Client IDs, usernames, and passwords, which users can efficiently manage through HTTP APIs.

#### POST api/v4/auth_clientid

To create authentication rules based on Client ID.

**Parameters (json):**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| clientid | String | True     | Client ID   |
| password | String | True     | Password    |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X POST \
	-d '{"clientid": "emqx_c", "password": "emqx_p"}' \
  http://localhost:8081/api/v4/auth_clientid

## Return
{"code":0}
```

#### POST api/v4/auth_username

To create authentication rules based on Username.

**Parameters (json):**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| username | String | True     | Username    |
| password | String | True     | Password    |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X POST \
	-d '{"username": "emqx_u", "password": "emqx_p"}' \
  http://localhost:8081/api/v4/auth_username

## Return
{"code":0}
```

#### POST api/v4/auth_clientid

To create authentication rules based on Client ID in batch. 

**Path Parameters:** no

**Parameters (json):**

| Name        | Type   | Required | Description |
| ----------- | ------ | -------- | ----------- |
| [].clientid | String | True     | Client ID   |
| [].password | String | True     | Password    |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |
| data | Object  |             |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X POST \
	-d '[{"clientid": "emqx_c_1", "password": "emqx_p"}, {"clientid": "emqx_c_2", "password": "emqx_p"}]' \
  http://localhost:8081/api/v4/auth_clientid

## Return
{
    "data":{
        "emqx_c_2":"ok",
        "emqx_c_1":"ok"
    },
    "code":0
}
```

#### POST api/v4/auth_username

To create authentication rules based on Username in batch. 

**Path Parameters:** no

**Parameters (json):**

| Name        | Type   | Required | Description |
| ----------- | ------ | -------- | ----------- |
| [].username | String | True     | Username    |
| [].password | String | True     | Password    |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |
| data | Object  |             |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X POST \
	-d '[{"username": "emqx_u_1", "password": "emqx_p"}, {"username": "emqx_u_2", "password": "emqx_p"}]' \
  http://localhost:8081/api/v4/auth_username

## Return
{
  "data":{
    "emqx_u_2":"ok",
    "emqx_u_1":"ok"
  },
  "code":0
}
```

#### GET api/v4/auth_clientid

To view the added authentication data.

**Query String Parameters:**

Fuzzy query can be performed based on the clientID. 

| Name           | Type   | Required | Description                                 |
| -------------- | ------ | -------- | ------------------------------------------- |
| _like_clientid | String | False    | Fuzzy queries are supported for Client IDs. |

**Success Response Body (JSON):**

| Name               | Type    | Description |
| ------------------ | ------- | ----------- |
| code               | Integer | 0           |
| meta               | Object  | Rule object |
| data               | Object  | Rule object |
| - data.[].clientid | String  | Client ID   |

**Example**

```shell
## Return
$ curl -i \
  --basic \
  -u admin:public \
  -X GET \
  http://localhost:8081/api/v4/auth_clientid?_like_clientid=emqx

## Request
{
    "meta":{
        "page":1,
        "limit":10,
        "count":3
    },
    "data":[
        {
            "clientid":"emqx_c_1"
        },
        {
            "clientid":"emqx_c_2"
        },
        {
            "clientid":"emqx_c"
        }
    ],
    "code":0
}
```

#### GET api/v4/auth_username

To view the added authentication data. 

**Query String Parameters:**
Fuzzy query can be performed based on the clientID. 

| Name           | Type   | Required | Description                                       |
| -------------- | ------ | -------- | ------------------------------------------------- |
| _like_username | String | False    | Fuzzy queries are supported for Client user name. |

**Success Response Body (JSON):**

| Name               | Type    | Description |
| ------------------ | ------- | ----------- |
| code               | Integer | 0           |
| meta               | Object  | Rule object |
| data               | Object  | Rule object |
| - data.[].username | String  | Client ID   |

**Example**

```shell
## Return
curl -i \
  --basic \
  -u admin:public \
  -X GET \
  http://localhost:8081/api/v4/auth_username?_like_username=emqx

## Request
{
    "meta":{
        "page":1,
        "limit":10,
        "count":3
    },
    "data":[
        {
            "username":"emqx_u"
        },
        {
            "username":"emqx_u_2"
        },
        {
            "username":"emqx_u_1"
        }
    ],
    "code":0
}
```

#### GET api/v4/auth_clientid/{clientid}

To get the details of the specific resource.

**Path Parameters:**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| clientid | String | True     | Client ID   |

**Success Response Body (JSON):**

| Name            | Type    | Description                                                  |
| --------------- | ------- | ------------------------------------------------------------ |
| code            | Integer | 0                                                            |
| data            | Object  | Rule object                                                  |
| - data.clientid | String  | Client ID                                                    |
| - data.password | String  | Note: The password returned here is the password encrypted using the hash specified in the configuration file. |

#### **Example**

```shell
## Request
curl -i \
  --basic \
  -u admin:public \
  -X GET \
  http://localhost:8081/api/v4/auth_clientid/emqx_c

## Return
{
  "data":{
    "password":"bb7bb456355aaeb55a4eb26ea286314fc360138720cfca2c852d4dfb8cd834",
    "clientid":"emqx_c"
  },
  "code":0
}
```

#### GET api/v4/auth_username/{username}

To get the details of the specific resource.

**Path Parameters:**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| username | String | True     | Username    |

**Success Response Body (JSON):**

| Name            | Type    | Description                                                  |
| --------------- | ------- | ------------------------------------------------------------ |
| code            | Integer | 0                                                            |
| data            | Object  | Rule object                                                  |
| - data.username | String  | Username                                                     |
| - data.password | String  | Note: The password returned here is the password encrypted using the hash specified in the configuration file. |

#### **Example**

```shell
## Request
curl -i \
  --basic \
  -u admin:public \
  -X GET \
  http://localhost:8081/api/v4/auth_username/emqx_u

## Return
{
  "data":{
    "password":"bb7bb456355aaeb55a4eb26ea286314fc360138720cfca2c852d4dfb8cd834",
    "clientid":"emqx_u"
  },
  "code":0
}
```

#### PUT api/v4/auth_clientid/{clientid}

To updated the already added authentication data.

**Parameters (json):**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| clientid | String | True     | Client ID   |

**Parameters (json):**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| password | String | True     | Password    |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X PUT \
	-d '{"password": "emqx_new_p"}' \
  http://localhost:8081/api/v4/auth_clientid/emqx_c

## Return
{"code":0}
```

#### PUT api/v4/auth_username/{username}

To updated the already added authentication data.

**Parameters (json):**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| username | String | True     | Username    |

**Parameters (json):**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| password | String | True     | Password    |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X PUT \
	-d '{"password": "emqx_new_p"}' \
  http://localhost:8081/api/v4/auth_username/emqx_u

## Return
{"code":0}
```

#### DELETE /api/v4/auth_clientid/{clientid}

To delete an authentication rule. 

**Path Parameters:**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| clientid | String | True     | Client ID   |

**Parameters:** no

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X Delete\
  http://localhost:8081/api/v4/auth_clientid/emqx_c

## Return
{"code":0}
```

#### DELETE /api/v4/auth_username/{username}

To delete an authentication rule. 

**Path Parameters:**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| username | String | True     | Username    |

**Parameters:** no

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X Delete\
  http://localhost:8081/api/v4/auth_username/emqx_u

## Return
{"code":0}
```

### Mnesia Access Control

EMQX leverages its internal Mnesia database for storing and managing access control rules  for easy integration with external device management systems.

#### POST api/v4/acl

To add ACL rule. 

+ Clientid ACL

  **Parameters (json):**

  | Name     | Type           | Required | Description    |
  | -------- | -------------- | -------- | -------------- |
  | clientid | String         | True     | Client ID      |
  | topic    | String         | True     | Topic          |
  | action   | sub/pub/pubsub | True     | Action         |
  | access   | allow/deny     | True     | Alllow or deny |

  **Success Response Body (JSON):**

  | Name            | Type    | Description    |
  | --------------- | ------- | -------------- |
  | code            | Integer | 0              |
  | data            | Object  | Rule object    |
  | - data.clientid | String  | Clientid       |
  | - data.topic    | String  | Topic          |
  | - data.action   | String  | Action         |
  | - data.access   | String  | Alllow or deny |

  **Examples**

  ```shell
  ## Request
  curl -i \
  --basic \
  -u admin:public \
  -X POST \
  -d '{"clientid":"emqx_c", "topic":"Topic/A", "action":"pub", "access": "allow"}' \
  http://localhost:8081/api/v4/acl
  
  ## Return
  {
    "data":{
        "topic":"Topic/A",
        "result":"ok",
        "clientid":"emqx_c",
        "action":"pub",
        "access":"allow"
    },
    "code":0
  }
  ```

+ Username ACL

  **Parameters (json):**

  | Name     | Type           | Required | Description    |
  | -------- | -------------- | -------- | -------------- |
  | username | String         | True     | Username       |
  | topic    | String         | True     | Topic          |
  | action   | sub/pub/pubsub | True     | Action         |
  | access   | allow/deny     | True     | Alllow or deny |

  **Success Response Body (JSON):**

  | Name            | Type    | Description    |
  | --------------- | ------- | -------------- |
  | code            | Integer | 0              |
  | data            | Object  | Rule object    |
  | - data.username | String  | Username       |
  | - data.topic    | String  | Topic          |
  | - data.action   | String  | Action         |
  | - data.access   | String  | Alllow or deny |

  **Examples**

  ```shell
  ## Request
  curl -i \
  --basic \
  -u admin:public \
  -X POST \
  -d '{"username":"emqx_u", "topic":"Topic/A", "action":"pub", "access": "allow"}' \
  http://localhost:8081/api/v4/acl
  
  ## Return
  {
    "data":{
        "topic":"Topic/A",
        "result":"ok",
        "username":"emqx_u",
        "action":"pub",
        "access":"allow"
    },
    "code":0
  }
  ```

+ $all ACL

  **Parameters (json):**

  | Name   | Type           | Required | Description   |
  | ------ | -------------- | -------- | ------------- |
  | topic  | String         | True     | Topic         |
  | action | sub/pub/pubsub | True     | Action        |
  | access | allow/deny     | True     | Allow or deny |

  **Success Response Body (JSON):**

  | name          | type    | description   |
  | ------------- | ------- | ------------- |
  | code          | integer | 0             |
  | data          | object  | Rule object   |
  | - data.all    | string  | $all          |
  | - data.topic  | string  | Topic         |
  | - data.action | string  | Action        |
  | - data.access | string  | Allow or deny |

  **Examples**

  ```shell
  ## Request
  curl -i \
  --basic \
  -u admin:public \
  -X POST \
  -d '{"topic":"Topic/A", "action":"pub", "access": "allow"}' \
  http://localhost:8081/api/v4/acl
  
  ## Return
  {
    "data":{
        "topic":"Topic/A",
        "result":"ok",
        "all":"$all",
        "action":"pub",
        "access":"allow"
    },
    "code":0
  }
  ```

#### POST api/v4/acl

To add ACL rules in batch. 

**Parameters (json):**

| Name         | Type           | Required | Description   |
| ------------ | -------------- | -------- | ------------- |
| [0].clientid | String         | True     | Clientid      |
| [0].topic    | String         | True     | Topic         |
| [0].action   | sub/pub/pubsub | True     | Action        |
| [0].access   | allow/deny     | True     | Allow or deny |
| [1].username | String         | True     | Username      |
| [1].topic    | String         | True     | Topic         |
| [1].action   | sub/pub/pubsub | True     | Action        |
| [1].access   | allow/deny     | True     | Allow or deny |
| [2].topic    | String         | True     | Topic         |
| [2].action   | sub/pub/pubsub | True     | Action        |
| [2].access   | allow/deny     | True     | Allow or deny |

**Success Response Body (JSON):**

| name                | type    | description   |
| ------------------- | ------- | ------------- |
| code                | integer | 0             |
| data                | object  | Rule object   |
| - data.[0].clientid | string  | Client ID     |
| - data.[0].topic    | string  | Topic         |
| - data.[0].action   | string  | Action        |
| - data.[0].access   | string  | Allow or deny |
| - data.[1].username | string  | Username      |
| - data.[1].topic    | string  | Topic         |
| - data.[1].action   | string  | Action        |
| - data.[1].access   | string  | Allow or deny |
| - data.[2].all      | string  | $all          |
| - data.[2].topic    | string  | Topic         |
| - data.[2].action   | string  | Action        |
| - data.[2].access   | string  | Allow or deny |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X POST \
	-d '[
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
      ]' \
  http://localhost:8081/api/v4/auth_clientid

## Return
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

#### GET api/v4/acl/clientid

To viewed the already added ACL rules. 

**Query String Parameters:**

Support multi-condition and fuzzy query.

| Name           | Type   | Required | Description                                 |
| -------------- | ------ | -------- | ------------------------------------------- |
| access         | Enum   | False    | Allow or deny                               |
| action         | Enum   | False    | Action<br/>Options: `pub`,`sub`,`pubsub`    |
| topic          | String | False    | MQTT topic                                  |
| _like_clientid | String | False    | Fuzzy queries are supported for Client IDs. |


**Success Response Body (JSON):**

| Name               | Type    | Description                   |
| ------------------ | ------- | ----------------------------- |
| code               | Integer | 0                             |
| data               | Object  | Rule object                   |
| - data.[].clientid | String  | Clientid                      |
| - data.[].topic    | String  | Topic                         |
| - data.[].action   | Enum    | Action: `pub`, `sub`,`pubsub` |
| - data.[].access   | Enum    | Allow or deny                 |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X GET \
  http://localhost:8081/api/v4/acl/clientid

## Return
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

#### GET api/v4/acl/username

To view already added ACL rules. 
**Query String Parameters:**

Support multi-condition and fuzzy query.

| Name           | Type   | Required | Description                                       |
| -------------- | ------ | -------- | ------------------------------------------------- |
| access         | Enum   | False    | Allow or deny                                     |
| action         | Enum   | False    | Action<br/>Options: `pub`,`sub`,`pubsub`          |
| topic          | String | False    | MQTT topic                                        |
| _like_username | String | False    | Fuzzy queries are supported for client user name. |

**Success Response Body (JSON):**

| Name               | Type    | Description                   |
| ------------------ | ------- | ----------------------------- |
| code               | Integer | 0                             |
| data               | Object  | Rule object                   |
| - data.[].username | String  | Username                      |
| - data.[].topic    | String  | Topic                         |
| - data.[].action   | Enum    | Action: `pub`, `sub`,`pubsub` |
| - data.[].access   | Enum    | Allow or deny                 |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X GET \
  http://localhost:8081/api/v4/acl/username

## Return
{
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
      {
        "clientid": "emqx_u",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "clientid": "emqx_u_1",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      },
      {
        "clientid": "emqx_u_2",
        "topic": "Topic/A",
        "action": "pub",
        "access": "allow"
      }
    ],
    "code": 0
  }
```

#### GET api/v4/acl/$all

To view the already added ACL rules. 

**Success Response Body (JSON):**

| Name             | Type    | Description   |
| ---------------- | ------- | ------------- |
| code             | Integer | 0             |
| data             | Object  | Rule object   |
| - data.[].all    | String  | $all          |
| - data.[].topic  | String  | Topic         |
| - data.[].action | String  | Action        |
| - data.[].access | String  | Allow or deny |

**Examples**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X GET \
  http://localhost:8081/api/v4/acl/\$all

## Return
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

#### GET /api/v4/acl/clientid/{clientid}

To view the specific ACL rule. 

**Path Parameters:**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| clientid | String | True     | ClientID    |

**Parameters:** 无

**Success Response Body (JSON):**

| Name            | Type    | Description   |
| --------------- | ------- | ------------- |
| code            | Integer | 0             |
| data            | object  | Rule object   |
| - data.clientid | string  | ClientID      |
| - data.topic    | string  | Topic         |
| - data.action   | string  | Action        |
| - data.access   | string  | Allow or deny |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X GET \
  http://localhost:8081/api/v4/acl/clientid/emqx_c

## Return
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

#### GET /api/v4/acl/username/{username}

查看指定的 ACL 规则。

**Path Parameters:**

| Name     | Type   | Required | Description |
| -------- | ------ | -------- | ----------- |
| usernmae | String | True     | Username    |

**Parameters:** 无

**Success response body (json):**

| name            | type    | description   |
| --------------- | ------- | ------------- |
| code            | integer | 0             |
| data            | object  | Rule object   |
| - data.username | string  | Username      |
| - data.topic    | string  | Topic         |
| - data.action   | string  | Action        |
| - data.access   | string  | Allow or deny |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X GET \
  http://localhost:8081/api/v4/acl/usernmae/emqx_u

## Return
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

#### DELETE /api/v4/acl/clientid/{clientid}/topic/{topic}

To delete the specfici ACL rule.

**Path Parameters:**

| Name     | Type   | Required | Description                                |
| -------- | ------ | -------- | ------------------------------------------ |
| clientid | String | True     | ClientID                                   |
| topic    | String | True     | Topic, may need to be encoded in UrlEncode |

**Parameters:**  no

**Success response body (json):**

| name | type    | description |
| ---- | ------- | ----------- |
| code | integer | 0           |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X DELETE \
  http://localhost:8081/api/v4/acl/clientid/emqx_c/topic/Topic%2fA

## Return
{"code": 0}
```

#### DELETE /api/v4/acl/username/{usernmae}/topic/{topic}

To delete the specfici ACL rule.

**Path Parameters:**

| Name     | Type   | Required | Description                                |
| -------- | ------ | -------- | ------------------------------------------ |
| username | String | True     | Username                                   |
| topic    | String | True     | Topic, may need to be encoded in UrlEncode |

**Parameters:** no

**Success response body (json):**

| name | type    | description |
| ---- | ------- | ----------- |
| code | integer | 0           |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X DELETE \
  http://localhost:8081/api/v4/acl/username/emqx_u/topic/Topic%2fA

## Return
{"code": 0}
```

#### DELETE /api/v4/acl/all/$all/topic/{topic}

To delete the specfici ACL rule.

**Path Parameters:**

| Name  | Type   | Required | Description                                |
| ----- | ------ | -------- | ------------------------------------------ |
| topic | String | True     | Topic, may need to be encoded in UrlEncode |

**Parameters:** no

**Success response body (json):**

| name | type    | description |
| ---- | ------- | ----------- |
| code | integer | 0           |

**Examples:**

```shell
## Request
curl -i \
	--basic \
	-u admin:public \
	-X DELETE \
  http://localhost:8081/api/v4/acl/all/\$all/topic/Topic%2fA

## Return
{"code": 0}
```

## CLI

The built-in database authentication/access control also comes equipped with the administrative command-line tool `./bin/emqx_ctl`.

### clientid Commands

The clientid commands can be used to query or manange the clientid authentication.

| Commands                                   | Description                                    |
| ------------------------------------------ | ---------------------------------------------- |
| `clientid list`                            | List clientid related authentication rules.    |
| `clientid add <ClientID> <Password>`       | Add clientid related authentication rules.     |
| `clientid update <ClientID> <NewPassword>` | Update clientid related authentication rules.  |
| `clientid del <ClientID>`                  | Delete  clientid related authentication rules. |

#### `clientid list`

List clientid related authentication rules.

```bash
$ ./bin/emqx_ctl clientid list
emqx_clientid
```

### `clientid add <ClientID> <Password>`

Add clientid related authentication rules.

```bash
./bin/emqx_ctl clientid add emqx_clientid password
ok
```

### `clientid update <ClientID> <NewPassword>`

Update clientid related authentication rules.

```bash
$ ./bin/emqx_ctl clientid update emqx_clientid new_password
ok
```

### `clientid del <ClientID>`

Delete  clientid related authentication rules.

```bash
$ ./bin/emqx_ctl clientid del emqx_clientid
ok
```

### user Commands

The user commands can be used to query or manange the username authentication. 

| Commands                               | 描述                                          |
| -------------------------------------- | --------------------------------------------- |
| `user list`                            | List username related authentication rules.   |
| `user add <Username> <Password>`       | Add username related authentication rules.    |
| `user update <Username> <NewPassword>` | Update username related authentication rules. |
| `user del <Username>`                  | Delete username related authentication rules. |

#### `user list`

List username related authentication rules.

```bash
$ ./bin/emqx_ctl user list
emqx_username
```

### `user add <Username> <Password>`

Add username related authentication rules.

```bash
./bin/emqx_ctl user add emqx_username password
ok
```

### `user update <Username> <NewPassword>`

Update username related authentication rules.

```bash
$ ./bin/emqx_ctl user update emqx_username new_password
ok
```

### `user del <Username>`

Delete username related authentication rules.

```bash
$ ./bin/emqx_ctl user del emqx_username
ok
```

### acl Commands

The act commands can be used to query or manange the access control related rules.

| Commands                                                | Description                                   |
| ------------------------------------------------------- | --------------------------------------------- |
| `acl list clientid`                                     | List clientid related access control rules.   |
| `acl list username`                                     | List username related access control rules.   |
| `acl list _all`                                         | List $all access control rules.               |
| `acl show clientid <Clientid>`                          | View clientid related access control rules.   |
| `acl show username <Username>`                          | View username related access control rules.   |
| `acl aad clientid <Clientid> <Topic> <Action> <Access>` | Add clientid related access control rules.    |
| `acl add Username <Username> <Topic> <Action> <Access>` | Add username related access control rules.    |
| `acl add _all <Topic> <Action> <Access>`                | Add $all access control rules.                |
| `acl del clientid <Clientid> <Topic>`                   | Delete clientid related access control rules. |
| `acl del username <Username> <Topic>`                   | Delete username related access control rules. |
| `acl del _all <Topic>`                                  | Delete $all access control rules.             |

#### `acl list clientid`

List clientid related access control rules. 

```bash
$ ./bin/emqx_ctl acl list clientid             
Acl(clientid = <<"emqx_clientid">> topic = <<"Topic/A">> action = pub access = allow)
```

#### `acl list username`

List username related access control rules. 

```bash
$ ./bin/emqx_ctl acl list username             
Acl(username = <<"emqx_username">> topic = <<"Topic/A">> action = pub access = allow)
```

#### `acl list _all`

List $all access control rules. 

```bash
$ ./bin/emqx_ctl acl list _all 
Acl($all topic = <<"Topic/A">> action = pub access = allow)
```

#### `acl show clientid <Clientid>`

View clientid related access control rules. 

```bash
$ ./bin/emqx_ctl acl show clientid emqx_clientid
Acl(clientid = <<"emqx_clientid">> topic = <<"Topic/A">> action = pub access = allow)
```

#### `acl show username <Username>`

View username related access control rules.

```bash
$ ./bin/emqx_ctl acl show username emqx_username
Acl(username = <<"emqx_username">> topic = <<"Topic/A">> action = pub access = allow)
```

#### `acl aad clientid <Clientid> <Topic> <Action> <Access>`

Add clientid related access control rules. 

```bash
$ ./bin/emqx_ctl acl add clientid emqx_clientid Topic/A pub allow
ok
```

#### `acl aad username <Username> <Topic> <Action> <Access>`

Add username related access control rules.

```bash
$ ./bin/emqx_ctl acl add username emqx_username Topic/A pub allow
ok
```

#### `acl aad _all <Topic> <Action> <Access>`

Add $all access control rules.

```bash
$ ./bin/emqx_ctl acl add _all Topic/A pub allow
ok
```

#### `acl del clientid <Clientid> <Topic>`

Delete clientid related access control rules. 

```bash
$ ./bin/emqx_ctl acl del clientid emqx_clientid Topic/A
ok
```

#### `acl del username <Username> <Topic>`

Delete username related access control rules. 

```bash
$ ./bin/emqx_ctl acl del username emqx_username Topic/A
ok
```

#### `acl del _all <Topic`

Delete $all access control rules.

```bash
$ ./bin/emqx_ctl acl del _all Topic/A
ok
```

