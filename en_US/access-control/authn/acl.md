# Access Control List

::: tip
This document discusses the Access Control List (ACL) rules specific to each client, included in the authentication response. This approach is concise and performant, and should meet most requirements. For For more comprehensive but generic authorization methods, refer to [Authorization](../authz/authz.md).
:::

The Access Control List (ACL) is an optional extension of authentication result to control the permissions of the client after login. For example a private Claim `acl` defined in JWT, or a `acl` JSON property returned as a part of the HTTP authentication response.

::: tip

ACL rules returned by authentication are checked before all Authorizers. For details, see [Authorization Check Priority](../authz/authz.md#authorization-check-priority).
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
      "permission": "allow",
      "action": "subscribe",
      // The 'eq' prefix means the rule matches 't/1/#', but not 't/1/x' or 't/1/y'
      "topic": "eq t/1/#",
      // Matches QoS 1, but not QoS 0 or 2
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

The permission list defines `pub`, `sub`, and `all` as three optional fields, specifying the whitelist of topics for publishing, subscribing, or both. Topics may include topic wildcards and placeholders (currently supports `${clientid}` and `${username}`). To address potential conflicts between topic content and placeholder syntax, the `eq` syntax is provided to bypass placeholder interpolation. Example:

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
