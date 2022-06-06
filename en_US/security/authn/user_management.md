# User Management API

Some [authenticators](./authn.md#authentication-sources) (with `built_in_database` backend) store credential data in EMQX internal database (Mnesia):

* `password_based:built_in_database`
* `scram:built_in_database`

For these authenticators, users can be managed through REST API.
It allows to create, update, remove, and list user credentials.

To use user-management API for an authenticator, the authenticator must be set up for the used
[chain](./authn.md#authentication-chains).

::: warning
Each authentication chain has its own set of users.
:::

## API Endpoints

The endpoint for the users of the global MQTT chain is `/api/v5/authentication/{id}/users`.
Endpoint for the uses of a concrete MQTT listener chain is `/api/v5/listeners/{listener_id}/authentication/{id}/`.
Endpoint for the users of a global `gateway` protocol chain is `/api/v5/gateway/{protocol}/authentication`.
Endpoint for the uses of a `gateway` protocol listener chain is `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication`.

See [authentication API documentation](./authn.md#http-api) for identificator conventions.

## Importing Users

User import is supported for the `password_based:built_in_database` authenticator.

The endpoints for importing users into the corresponding chains are:
* `/api/v5/authentication/{id}/import_users`
* `/api/v5/listeners/{listener_id}/authentication/{id}/import_users`
* `/api/v5/gateway/{protocol}/authentication/import_users`
* `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

The accepted `filename` parameter should be a node-local path of a file with credentials.

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

