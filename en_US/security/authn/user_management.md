# User Management API

Some [authenticators](./authn.md#authentication-sources) (with ``built_in_database` backend`) store credential data in EMQX internal database (Mnesia):

* `password_based:built_in_database`
* `scram:built_in_database`

For this authenticators users can be managed through HTTP API.
It allows to create, update, remove, list and import user credentials.

To use user-management API for an authenticator, the authenticator must be set up for the used
[chain](./authn.md#authentication-chains).

::: warning
Each authentication chain has it own set of users.
:::

## POST /authentication/{id}/import_users

Also `POST /listeners/{listener_id}/authentication/{id}/import_users"` for a concrete listener.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X POST \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users \
-d @- <<'EOF'
{
  "filename": "user-credentials.csv"
}
EOF

## Response
## 204 No Content
```

`filename` should be a node-local path of a file with credentials.

The following file formats (identified by file extention) are supported:
* CSV (`{"filename": "some-filename.csv"}`:
  ```csv
  user_id,password_hash,salt,is_superuser
  myuser3,b6c743545a7817ae8c8f624371d5f5f0373234bb0ff36b8ffbf19bce0e06ab75,de1024f462fb83910fd13151bd4bd235,true
  myuser4,ee68c985a69208b6eda8c6c9b4c7c2d2b15ee2352cdd64a903171710a99182e8,ad773b5be9dd0613fe6c2f4d8c403139,false
  ```
* JSON (`{"filename": "some-filename.json"}`:
  ```json
  [
    {
        "user_id":"myuser1",
        "password_hash":"c5e46903df45e5dc096dc74657610dbee8deaacae656df88a1788f1847390242",
        "salt": "e378187547bf2d6f0545a3f441aa4d8a",
        "is_superuser": true
    },
    {
        "user_id":"myuser2",
        "password_hash":"f4d17f300b11e522fd33f497c11b126ef1ea5149c74d2220f9a16dc876d4567b",
        "salt": "6d3f9bd5b54d94b98adbcfe10b6d181f",
        "is_superuser": false
    }
  ]
  ```

## GET /authentication/{id}/users

Also `GET /listeners/{listener_id}/authentication/{id}/users"` for a concrete listener.

List chain users.

Possible parameters are:
* `page`, `limit` — pagination parameters (pages start from 1);
* `like_username` — limit users to having the specified substring pattern in their `username`;
* `like_clientid` — limit users to having the specified substring pattern in their `clientid`;
* `is_superuser` — limit users to having the specified `is_superuser` value.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/users?page=1&limit=1&like_username=myuser3&is_superuser=true

## Response
{
  "data": [
    {
      "is_superuser": true,
      "user_id": "myuser3"
    }
  ],
  "meta": {
    "count": 1,
    "limit": 1,
    "page": 1
  }
}
```

## POST /authentication/{id}/users

Also `POST /listeners/{listener_id}/authentication/{id}/users"` for a concrete listener.

Create a user in a chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X POST \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/users \
-d @- <<'EOF'
{
  "user_id": "user2",
  "password": "secret",
  "is_superuser": false
}
EOF

## Response
## 201 Created
{
  "is_superuser": false,
  "user_id": "user2"
}
```

`user_id` denotes `username` for authenticators configured with `user_id_type = username` and `clientid` for
the ones configured with `user_id_type = clientid`.

`is_superuser` field is optional and is `false` by default.

## GET /authentication/{id}/users/{user_id}

Also `GET /listeners/{listener_id}/authentication/{id}/users/{user_id}"` for a concrete listener.

Get information about particular user credentials.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X GET \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/users/user2 \

## Response
{
  "is_superuser": false,
  "user_id": "user2"
}
```

Note:
* there is no `password` or `password_hash` field in the answer;
* `user_id` URL path parameter should be URL-encoded.

## PUT /authentication/{id}/users/{user_id}

Also `PUT /listeners/{listener_id}/authentication/{id}/users/{user_id}"` for a concrete listener.

Update a user in a chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X PUT \
-H "Content-Type: application/json" \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/users/user2 \
-d @- <<'EOF'
{
  "password": "secretnew",
  "is_superuser": false
}
EOF

## Response
{
  "is_superuser": false,
  "user_id": "user2"
}
```

Note: `user_id` URL path parameter should be URL-encoded.

## DELETE /authentication/{id}/users/{user_id}

Also `DELETE /listeners/{listener_id}/authentication/{id}/users/{user_id}"` for a concrete listener.

Delete a user from a chain.

```bash
## Request
curl -i \
--basic \
-u admin:public \
-X DELETE \
http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/users/user2

## Response
## 204 No Content
```


