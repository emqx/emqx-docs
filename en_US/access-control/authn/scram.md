# MQTT 5.0 enhanced authentication

EMQX also supports the MQTT 5.0 enhanced authentication. This authenticator implements [Salted Challenge Response Authentication Mechanism (SCRAM)](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism) authentication and uses the built-in database of EMQX to store client credentials (_users_).

SCRAM is a more complicated mechanism than password verification. It requires exchanging additional MQTT packages during connection. SCRAM authentication does not depend on external data sources, and it is simple and lightweight to use.

::: tip
SCRAM authenticator only supports MQTT 5.0 connection.
:::

## Configure with Dashboard

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **SCRAM** as **Mechanism**, and **Built-in Database** as **Backend**, this will lead us to the **Configuration** tab. 

Set **Password Hash** as **sha256** or **sha512** and click **Create** to finish the settings. 

## Configure with configuration items

Sample configuration:

```
{
    mechanism = scram
    backend = built_in_database
    enable = true

    algorithm = sha512
    iteration_count = 4096
}
```

where, 

- `algorithm`: password hash algorithm, options: `sha256` and `sha512`
- `iteration_count` (optional): Iteration-count parameter for SCRAM; Default:  4096.
