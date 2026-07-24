# Use HTTP API to Manage User Data

For authentication data stored in the built-in database, you can use the EMQX Dashboard or HTTP API to create, update, delete, list, and import user credentials.

This applies to:

- [Use built-in database for password authentication](./mnesia.md)
- [MQTT 5.0 enhanced authentication](./scram.md)

## User Management API Endpoints

User management endpoints vary depending on the authentication chain scope.

- **Global MQTT authentication chain**

  ```
  /api/v5/authentication/{id}/users
  ```

- **MQTT listener-specific authentication chain**

  ```
  /api/v5/listeners/{listener_id}/authentication/{id}/users
  ```

- **Global gateway protocol authentication chain**

  ```
  /api/v5/gateway/{protocol}/authentication/{id}/users
  ```

- **Gateway protocol listener-specific authentication chain**

  ```
  /api/v5/gateway/{protocol}/listeners/{listener_id}/authentication/{id}/users
  ```

See [REST API](../../../guides/api.md) for identifier conventions and parameter descriptions.

## Import Users

User import is supported only for the `password_based:built_in_database` authenticator.

This feature allows batch importing users into a running EMQX instance.

The following endpoints are available:

- `/api/v5/authentication/{id}/import_users`
- `/api/v5/gateway/{protocol}/authentication/import_users`
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

The request must be a `multipart/form-data` POST.

Example:

```bash
curl -v -u admin:public -X 'POST' \
    -H 'Content-Type: multipart/form-data' \
    -F 'filename=@/tmp/myusers.csv' \
    'http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users'
```

### Supported File Formats

The file format is determined by its extension. The following formats are supported:

* `.csv`

  Header-based CSV file:

  ```txt
  user_id,password_hash,salt,is_superuser
  myuser3,b6c743545a7817ae8c8f624371d5f5f0373234bb0ff36b8ffbf19bce0e06ab75,de1024f462fb83910fd13151bd4bd235,true
  myuser4,ee68c985a69208b6eda8c6c9b4c7c2d2b15ee2352cdd64a903171710a99182e8,ad773b5be9dd0613fe6c2f4d8c403139,false
  ```

  Required fields:

  - `user_id`
  - `password_hash`

  Optional fields:

  - `salt` (defaults to empty string)
  - `is_superuser` (defaults to `false`)

* `.json`

  JSON array of objects:

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

  Field requirements are the same as for CSV.
