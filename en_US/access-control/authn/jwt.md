# JWT Authentication

[JSON Web Token (JWT)](https://jwt.io/) is a token-based authentication mechanism. It does not rely on the server to retain client authentication information or session information. EMQX supports using JWT for user authentication. 

::: tip

- Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Authentication Principle

The client carries the JWT in the connection request, and EMQX uses the pre-configured secret or public key to verify the JWT signature. If the user configures a JWKS endpoint, the JWT authenticator will verify the JWT signature using the list of public keys queried from the JWKS endpoint. 

If the signature verification is successful, the JWT authenticator proceeds to check the claims. If there are claims such as `iat`, `nbf` or `exp`, the JWT authenticator will actively check the validity of the JWT based on these claims. In addition to this, we also allow users to specify some additional claims checks. The client is finally allowed to login only if the signature verification and claims check pass together.

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
| topic      | Yes      | The topic associated with the rule, supports [topic placeholders](https://chat.openai.com/authz/authz.md#topic-placeholders) |
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

## Configure with Dashboard

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **Password-Based** as **Mechanism**, and **JWT** as **Backend**, this will lead us to the **Configuration** tab, as shown below. 

<img src="./assets/authn-jwt.png" alt="JWT" style="zoom:67%;" />

Follow the instruction below on how to configure:

**JWT From**: Specify the location of the JWT in the client connection request; option values are `password` and `username`. For MQTT clients, these are the `Password` and `Username` fields in the MQTT `CONNECT` packet.

**Algorithm**: Specify the encryption algorithm of JWT, optional values ​​are `hmac-based` and `public-key`. For different encryption methods, the JWT authenticator will have different configuration requirements.

1. If configured as `hmac-based`, indicating that JWT will use symmetric secret to generate signature and verify signature (support HS256, HS384 and HS512 algorithms), we also need to configure:
   - `Secret`: the key used to verify the signature (the same key used to generate the signature)
   - `Secret Base64 Encode`: indicating whether the `Secret` is Base64 encrypted, that is, whether EMQX needs to perform Base64 decryption on the secret when verifying the signature.

2. If configured as `public-key`, indicating that JWT uses the private key to generate the signature, and needs to use the public key to verify the signature (supports RS256, RS384, RS512, ES256, ES384 and ES512 algorithms), we also need to configure:
   - `Public Key`: specifying the public key in PEM format used to verify the signature

**Payload**: Specify additional claims checks that the user wants to perform. Users can define multiple key-value pairs with the **Claim** and **Expacted Value** fields, where the key is used to find the corresponding claim in the JWT, so it needs to have the same name as the JWT claim to be checked, and the value is used to compare with the actual value of the claim. Currently the placeholders supported are `${clientid}` and `${username}`, 

EMQX also supports periodically obtaining the latest JWKS from the JWKS endpoint, which is essentially a set of public keys that will be used to verify any JWT issued by the authorization server and signed using the RSA or ECDSA algorithm. If we want to use this feature, we first need to switch to the **JWKS** configuration page.

![](./assets/authn-jwt-2.png)

So as shown above, we now have two brand-new configuration items:

1. `JWKS Server`: Specify the server endpoint address for EMQX to query JWKS, the endpoint needs to support GET requests and return a JWKS that conforms to the specification.
2. `JWKS Refresh Interval`: Specify the refresh interval of JWKS, that is, the interval for EMQX to query JWKS.

## <!--Configure with Configuration Items-->

<!--You can also configuration items for the configuration. For detailed steps, see [authn-jwt:*](../../configuration/configuration-manual.html#authn-jwt:hmac-based). -->

