# Password Authentication Using Built-in Database 

The built-in database (Mnesia) is used as the storage medium for client identity credentials, and there is no need to deploy additional databases, which is simple and lightweight enough to use.

## Configuration

Password authentication using built-in database is identified with `mechanism = password_based` and `backend = built_in_database`.

```
{
    mechanism = password_based
    backend = built_in_database
    enable = true

    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }

    user_id_type = username
}
```

### `user_id_type`

Possible values:

- `username`
- `clientid`

This option is used to specify the type of user ID stored in the built-in database, and also to indicate whether the authenticator should use the `Username` or `Client Identifier` in the MQTT `CONNECT` packet to retrieve the database and authenticate the client.

### `password_hash_algorithm`

`password_hash_algorithm` specifies standard [hashing options](./authn.md#password-hashing).

## Credential management

Users for password-based Mnesia authentication can be managed through [HTTP API](./user_management.md).
