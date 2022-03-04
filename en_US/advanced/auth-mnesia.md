# Mnesia  Authentication

Mnesia authentication uses the built-in Mnesia database of EMQX to store client Client ID/Username and password, and supports management of authentication data through HTTP API.

Mnesia authentication does not depend on external data sources, and it is simple and lightweight to use.

Plugin:

```bash
emqx_auth_mnesia
```

## Authentication rules

## Hash method

Mnesia authentication uses sha256 for password hash encryption by default, which can be changed in `etc/plugins/emqx_auth_mnesia.conf`:

```bash
# etc/plugins/emqx_auth_mnesia.conf

## Value: plain | md5 | sha | sha256
auth.mnesia.password_hash = sha256
```

After configuring [Hash Method](./auth.md#Password salting rules and hash methods), the newly added preset authentication data and authentication data added through the HTTP API will be stored in the EMQX built-in database in the format of hash ciphertext.



## Preset authentication data

You can preset authentication data through the configuration file and edit the configuration file: `etc/plugins/emqx_auth_mnesia.conf`

```bash
# etc/plugins/emqx_auth_mnesia.conf

## The first group of authentication data
auth.client.1.clientid = admin
auth.client.1.password = public

## The second group of authentication data
auth.user.2.username = admin
auth.user.2.password = public
```

When the plugin starts, it will read the preset authentication data and load it into the EMQX built-in database, and the authentication data on the node will be synchronized to the cluster at this stage.

<!-- TODO 补充加载规则 -->

::: tip

The preset authentication data uses a clear text password in the configuration file. For security and maintainability, this function should be avoided.

The preset authentication data cannot be modified or deleted through the API, please use it with caution.
:::

## Use the HTTP API to manage authentication data

### Add authentication data

+ Clientid

  ```bash
  # Request
  POST api/v4/auth_clientid
  {
      "clientid": "emqx_c",
      "password": "emqx_p"
  }
  # Response
  {
      "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  POST api/v4/auth_username
  {
      "username": "emqx_u",
      "password": "emqx_p"
  }

  # Response
  {
      "code": 0
  }
  ```

### Add authentication data in batch

+ Clientid

  ```bash
  # Request
  POST api/v4/auth_clientid
  [
      {
          "clientid": "emqx_c_1",
          "password": "emqx_p"
      },
      {
          "clientid": "emqx_c_2",
          "password": "emqx_p"
      }
  ]

  # Response
  {
      "data": {
          "emqx_c_2": "ok",
          "emqx_c_1": "ok"
      },
      "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  POST api/v4/auth_username
  [
      {
          "username": "emqx_u_1",
          "password": "emqx_p"
      },
      {
          "username": "emqx_u_2",
          "password": "emqx_p"
      }
  ]

  # Response
  {
      "data": {
          "emqx_c_2": "ok",
          "emqx_c_1": "ok"
      },
      "code": 0
  }
  ```

### Check the added authentication data

+ Clientid

  ```bash
  # Request
  GET api/v4/auth_clientid

  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
                "clinetid": "emqx_c",
                "clinetid": "emqx_c_1",
                "clinetid": "emqx_c_2"
            ],
    "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  GET api/v4/auth_username

  # Response
  {
    "meta": {
      "page": 1,
      "limit": 10,
      "count": 1
    },
    "data": [
                "username": "emqx_u",
                "username": "emqx_u_1",
                "username": "emqx_u_2"
            ],
    "code": 0
  }
  ```

### Change the added authentication data

+ Clientid

  ```bash
  # Request
  PUT api/v4/auth_clientid/${clientid}
  {
      "password": "emqx_new_p"
  }

  # Response
  {
      "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  PUT api/v4/auth_username/${username}
  {
      "password": "emqx_new_p"
  }

  # Response
  {
      "code": 0
  }
  ```

### Check the specified authentication data

Note that the password returned here is the password encrypted using the hash method specified in the configuration file:

+ Clientid

  ```bash
  # Request
  GET api/v4/auth_clientid/${clientid}

  # Response
  {
      "code": 0,
      "data": {
          "clientid": "emqx_c",
          "password": "091dc8753347e7dc5d348508fe6323735eecdb84fa800548870158117af8a0c0"
      }
  }
  ```

+ Username

  ```bash
  # Request
  GET api/v4/auth_username/${username}

  # Response
  {
      "code": 0,
      "data": {
          "username": "emqx_u",
          "password": "091dc8753347e7dc5d348508fe6323735eecdb84fa800548870158117af8a0c0"
      }
  }
  ```

### Delete the authentication data

+ Clinetid

  ```bash
  # Request
  DELETE api/v4/auth_clientid/${clientid}

  # Response
  {
      "code": 0
  }
  ```

+ Username

  ```bash
  # Request
  DELETE api/v4/auth_username/${username}

  # Response
  {
      "code": 0
  }
  ```
