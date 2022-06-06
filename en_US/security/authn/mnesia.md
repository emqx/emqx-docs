# Built-in Database

This authenticator implements the password verification algorithm and uses the built-in Mnesia database of EMQX to store client credentials (_users_).

Mnesia authentication does not depend on external data sources, and it is simple and lightweight to use.

## Configuration

Mnesia authentication is identified with `mechanism = password_based` and `backend = built_in_database`.

```hocon
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
* `username`
* `clientid`

This option specifies which MQTT `CONNECT` field to use for searching users: `username` or `clientid`.

### `password_hash_algorithm`

`password_hash_algorithm` specifies standard [hashing options](./authn.md#password-hashing).

## Credential management

Users for password-based Mnesia authentication can be managed through [HTTP API](./user_management.md).
