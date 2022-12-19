# SCRAM Authentication

This authenticator implements [SCRAM](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism) authentication and uses the built-in Mnesia database of EMQX to store client credentials (_users_).

SCRAM is a more complicated mechanism than password verification. It requires exchanging additional MQTTpackages during connection.

SCRAM authentication does not depend on external data sources, and it is simple and lightweight to use.

::: tip
SCRAM Authentication only supports MQTT v5.0 connection.
:::

## Configuration

SCRAM authentication is identified with `mechanism = scram` and `backend = built_in_database`.

```
{
    mechanism = scram
    backend = built_in_database
    enable = true

    algorithm = sha512
    iteration_count = 4096
}
```

### `algorithm`

Possible values:
* `sha256` for `SCRAM-SHA-256` method;
* `sha512` for `SCRAM-SHA-512` method.

### `iteration_count`

Iteration-count parameter for SCRAM, optional. The default value is 4096.

## Credential management

Users can be managed through [HTTP API](./user_management.md).
