# MQTT 5.0 Enhanced Authentication - SCRAM

This authenticator implements [Salted Challenge Response Authentication Mechanism (SCRAM)](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism) authentication and uses the built-in database of EMQX to store client credentials (_users_).

SCRAM is a more complicated mechanism than password verification. It requires exchanging additional MQTT packages during connection. SCRAM authentication does not depend on external data sources, and it is simple and lightweight to use.

::: tip
SCRAM authenticator only supports MQTT 5.0 connection.
:::

## Configure with Dashboard

1. On the [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation menu.
2. Click **Create** at the top right corner, then click to select **SCRAM** as **Mechanism**, and **Built-in Database** as **Backend**. This will lead you to the **Configuration** tab.
3. Configure the following settings for the authentication backend:
   - **Password Hash**: Select the password hash algorithm: `sha256` or `sha512`.
   - **Iteration Count**: This parameter defines the number of iterations used in the SCRAM authentication process to hash the password. A higher iteration count increases security by making the hashing process more computationally expensive, thereby slowing down brute force attacks. The default value is `4096`. Adjusting this value can impact performance and security, so it should be configured based on your system's needs.
   - **Precondition**: A [Variform expression](../../configuration/configuration.md#variform-expressions) used to control whether this Built-in Database authenticator should be applied to a client connection. The expression is evaluated against attributes from the client (such as `username`, `clientid`, `listener`, etc.). The authenticator will only be invoked if the expression evaluates to the string `"true"`. Otherwise, it will be skipped. For more information about the precondition, see [Authentication Preconditions](./authn.md#authentication-preconditions).
4. Click **Create** to finish the settings.

## Configure with Configuration Items

Sample configuration:

```hcl
{
    mechanism = scram
    backend = built_in_database
    algorithm = sha512
    iteration_count = 4096
}
```

where,

- `algorithm`: password hash algorithm, options: `sha256` and `sha512`
- `iteration_count` (optional): Iteration-count parameter for SCRAM; Default: `4096`

## Authentication Flow

![scram_workflow](./assets/scram_workflow.png)

