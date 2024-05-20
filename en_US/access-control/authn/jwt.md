# JWT Authentication

[JSON Web Token (JWT)](https://jwt.io/) is a token-based authentication mechanism. It does not rely on the server to retain client authentication information or session information. EMQX supports using JWT for user authentication. 

::: tip

Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Authentication Principle

The client carries the JWT in the connection request, and EMQX uses the pre-configured secret or public key to verify the JWT signature. If the user configures a JWKS endpoint, the JWT authenticator will verify the JWT signature using the list of public keys queried from the JWKS endpoint. 

If the signature verification is successful, the JWT authenticator proceeds to check the claims. The JWT authenticator actively checks the validity of the JWT based on these claims such as `iat` (Issued At), `nbf` (Not Before), and `exp` (Expiration Time). Additional custom claims can also be specified for verification. The client is granted access only if both the signature and claims verifications are successful.

Starting from EMQX version 5.7.0, JWT authentication includes an option to disconnect clients after their JWT expires. The configuration parameter `disconnect_after_expire` is set to `true` by default. To keep clients connected even after their JWT has expired, you can set this parameter to `false`.

## Best Practice

The JWT authenticator essentially only checks the signature of the JWT, which means that the JWT authenticator does not guarantee the legitimacy of the client's identity.

The best practice is to deploy an independent authentication server. The client first accesses the authentication server, the authentication server verifies the identity of the client, and issues JWT for the legitimate client, and then the client uses the obtained JWT to connect to EMQX.

:::tip

Since the payload in the JWT is only Base64 encoded, anyone who gets the JWT can decode the payload to get the original information by Base64 decoding. Therefore, it is not recommended to store some sensitive data in the payload of JWT.

To reduce the possibility of JWT leakage and theft, it is recommended to set a reasonable validity period and also enable TLS to encrypt client connections.

:::

## Access Control List (Optional)

The Access Control List (ACL) is an optional function to control the permissions of the client after login. A private Claim `acl` is defined to carry a list of publish and subscribe permissions in the JWT.

::: tip

ACL rules returned by JWT will be checked before all Authorizers. For details, see [Authorization Check Priority](../authz/authz.md#authorization-check-priority).
:::

:::: tabs type:board-card

::: tab New Format

The new format, supported starting from v5.5.0, utilizes an ACL to specify multiple permissions, closely resembling the semantics of ACL rules and offering greater flexibility.

Unlike the old format, the new format continues to other authorization checks if a client operation does not match any rule. While the old format remains compatible, the new format is recommended for use.

The ACL includes the following fields:

| Field      | Required | Description                                                  |
| ---------- | -------- | ------------------------------------------------------------ |
| permission | Yes      | Specifies whether the current client's operation request is allowed or denied; options: `allow`, `deny` |
| action     | Yes      | The operation associated with the rule; options: `publish`, `subscribe`, `all` |
| topic      | Yes      | The topic associated with the rule, supports [topic placeholders](../authz/authz.md#topic-placeholders) |
| qos        | No       | An array specifying the QoS levels applicable to the rule, e.g., `[0, 1]`, `[1, 2]`, default is all QoS levels |
| retain     | No       | Boolean, used only for publish operations, specifies if the current rule supports retained messages, options are `true`, `false`, default allows retained messages. |

Example:

```json
{
  "exp": 1706844358,
  "username": "emqx_u",
  "acl": [
    {
      // Allows the client to publish messages to the topic t/${clientid}, e.g., t/emqx_c
      "permission": "allow",
      "action": "publish",
      "topic": "t/${clientid}"
    },
    {
      // Allows the client to subscribe to the topic t/1 with QoS 1, while QoS 0 or 2 is allowed
      "permission": "deny",
      "action": "subscribe",
      "topic": "t/1",
      "qos": [1]
    },
    {
      // Denies the client from publishing retained messages to the topic t/2, non-retained messages are allowed
      "permission": "deny",
      "action": "publish",
      "topic": "t/2",
      "retain": true
    },
    {
      // Denies the client from publishing or subscribing to the topic t/3, including all QoS levels and retained messages
      "permission": "deny",
      "action": "all",
      "topic": "t/3"
    }
  ]
}
```

:::

::: tab Old Format

The JWT permission list defines `pub`, `sub`, and `all` as three optional fields, specifying the whitelist of topics for publishing, subscribing, or both. Topics may include topic wildcards and placeholders (currently supports `${clientid}` and `${username}`). To address potential conflicts between topic content and placeholder syntax, the `eq` syntax is provided to bypass placeholder interpolation. Example:

```json
{
  "exp": 1654254601,
  "username": "emqx_u",
  "acl": {
    "pub": [
      "testpub1/${username}",
      "eq testpub2/${username}"
    ],
    "sub": [
      "testsub1/${username}",
      "testsub2/${clientid}",
      "testsub2/#"
    ],
    "all": [
      "testall1/${username}",
      "testall2/${clientid}",
      "testall3/#"
    ]
  }
}
```

In this example, `testpub1/${username}` is replaced at runtime with `testpub1/emqx_u`, whereas `eq testpub2/${username}` is processed as `testpub2/${username}` at runtime.

:::

::::

## Client Attributes

Starting from EMQX v5.7.0, you can use the optional `client_attrs` field in the JWT Payload to set [client attributes](../../client-attributes/client-attributes.md). Please note that both the keys and values must be of string type.

Example:

```json
{
  "exp": 1654254601,
  "username": "emqx_u",
  "client_attrs": {
      "role": "admin",
      "sn": "10c61f1a1f47"
  }
}
```

## Configure JWT Authentication in Dashboard

1. Select **Access Control** -> **Authentication** from the left navigation menu. 

2. On the **Authentication** page, click **Create** at the top right corner. Click to select **JWT** as **Mechanism**, and click **Next**. Skip the Backend selection and go to the **Configuration** tab. 

   <img src="./assets/authn-jwt.png" alt="JWT" style="zoom:67%;" />

3. Configure the following options:

   - **JWT From**: Determines where the JWT is located in the client connection request. Available options are `password` and `username`, corresponding to the `Password` and `Username` fields in the MQTT `CONNECT` packet for MQTT clients.

   - **Algorithm**: Specify the encryption algorithm of JWT. Optional values ​​are `hmac-based` and `public-key`. Each selection dictates different configuration requirements.

     - `hmac-based`: Uses a symmetric key for both generating and verifying the JWT's signature. Supported algorithms include HS256, HS384, and HS512. Configuration must include:
       - `Secret`: The key used to verify the signature, identical to the one used for signature generation.
       - `Secret Base64 Encode`: Configures whether the `Secret` is Base64 encoded, which determines if EMQX needs to decode the secret during signature verification.

     - `public-key`: Uses a private key to generate the JWT's signature and a public key for verification. Supported algorithms are RS256, RS384, RS512, ES256, ES384, and ES512. Configuration must include:
        - `Public Key`: Specify the public key in PEM format used to verify the signature.

   - **Disconnect After Expiration**: Configures whether to disconnect clients after their JWT expires, enabled by default.
   - **Payload**: Specify additional claims checks that the user wants to perform. Users can define multiple key-value pairs with the **Claim** and **Expacted Value** fields, where the key is used to find the corresponding claim in the JWT, so it needs to have the same name as the JWT claim to be checked, and the value is used to compare with the actual value of the claim. Currently, the placeholders supported are `${clientid}` and `${username}`. 

4. Click **Create** to complete the configuration.


EMQX also supports periodically obtaining the latest JWKS from the JWKS endpoint, which is essentially a set of public keys used to verify any JWT issued by the authorization server and signed using the RSA or ECDSA algorithm. If you want to use this feature, you need to switch to the **JWKS** configuration page.

<img src="./assets/authn-jwt-2.png" style="zoom:67%;" />

You need to configure the following JWKS-specific configuration items:

- **JWKS Server**: Specify the server endpoint address for EMQX to query JWKS, the endpoint needs to support GET requests and return a JWKS that conforms to the specification.
- **JWKS Refresh Interval**: Specify the refresh interval of JWKS, that is, the interval for EMQX to query JWKS.

Click **Create** to complete the configuration.

<!-- ## Configure with Configuration Items

You can also configuration items for the configuration. For detailed steps, see [authn-jwt:*](../../configuration/configuration-manual.html#authn-jwt:hmac-based). -->

