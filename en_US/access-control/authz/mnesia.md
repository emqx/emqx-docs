# Use Built-in Database

EMQX provides users with a low-cost, out-of-the-box authorization rule storage method through the built-in database. You can use the built-in database (Mnesia) as a data source by setting it up through the Dashboard or configuration files, and add relevant authorization check rules through the Dashboard or HTTP API.

::: tip Prerequisite

Knowledge about [basic EMQX authorization concepts](./authz.md)

:::

## Create Built-in Database Authorizer via Dashboard

1. On the [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), navigate to **Access Control** > **Authorization** in the left-hand menu to open the **Authorization** page.

2. Click **Create** in the top-right corner, select **Built-in Database** as the **Backend**, then click **Next** to continue.

   ![authz-mnesia_ee](./assets/authz-mnesia_ee.png)

3. In the **Configuration** step, set the value for **Max Rules** (default: `100`), which defines the maximum number of authorization rules allowed per client or user.

   ::: tip Note

   Setting a high number of rules may impact system performance.

   :::

4. Click **Create** to complete the setup.

## Create Built-in Database Authorizer via Configuration File

The built-in database authorizer is identified by type `built_in_database`.

Sample configuration:

```bash
{
    type = built_in_database
    enable = true
}
```

-  `type`: The data source type of the authorization checker; fill in `built_in_database` here.

- `enable`: Whether to activate this checker; optional values: `true`, `false`.

<!--For detailed parameter list, see [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia).-->

## Create Authorization Rules

You can create authorization rules through the Dashboard or API.

### Create Authorization Rules via Dashboard

You can define authorization rules directly from the EMQX Dashboard by using the **Permissions** page of the **Built-in Database** backend.

#### Access the Permissions Page

1. In the Dashboard, go to the **Authorization** page.
2. In the **Actions** column of the **Built-in Database** backend, click **Permissions**.

![authz-mnesia-rule](./assets/authz-mnesia-rule.png)

#### Authorization Rule Scope

Authorization rules can be configured at three scopes:

- **Client ID**: Apply rules to a specific client ID.
- **Username**: Apply rules to a specific username.
- **All Users**: Apply rules to all clients/users, optionally filtered by patterns or IP ranges.

#### Common Rule Fields

These fields are available for all rule types:

| Field                | Description                                                  |
| -------------------- | ------------------------------------------------------------ |
| **Action**           | The operation type the rule applies to. Options: `Publish`, `Subscribe`, `Publish & Subscribe`. |
| **Permission**       | Whether to allow or deny the operation. Options: `Allow`, `Deny`. |
| **Topic**            | The MQTT topic this rule applies to. Wildcards (`+`, `#`) are supported. |
| **QoS**              | Allowed QoS levels. Multiple values can be selected: `0`, `1`, `2`. |
| **Retain**           | Whether the rule applies to retained messages. Options: `true`, `false`, `All`. |
| **IP Address Range** | Specifies the client IP range this rule applies to. Supports CIDR notation (e.g., `192.168.1.0/24`) or exact IPs. |
| **Listener**         | The listener this rule applies to. Use `{type}:{name}` format, e.g., `tcp:default`, `ws:default`. |
| **Zone**             | The Zone in which the rule takes effect. Applicable in multi-Zone deployments. |

#### Fields by Rule Scope

| Rule Scope    | Fields                                                       |
| ------------- | ------------------------------------------------------------ |
| **Client ID** | **Client ID**: (Required) Exact client ID to which this rule applies.<br />**Username Pattern**: (Optional) A regular expression to match usernames for which this rule is valid. |
| **Username**  | **Username**: (Required) Exact username to which this rule applies.<br />**Client ID Pattern**: (Optional) A regular expression to match client IDs for which this rule is valid. |
| **All Users** | **Client ID Pattern**: (Optional) A regular expression to match usernames for which this rule is valid.<br />**Username Pattern**: (Optional) A regular expression to match usernames for which this rule is valid. |

**Example patterns:**

- `^device-user-.*`: Matches usernames starting with `device-user-`.
- `^sensor-.*`: Matches client IDs starting with `sensor-`.

#### Add a Rule

1. On the **Permissions** page, select the target tab: **Client ID**, **Username**, or **All Users**.
2. Click **Add**.
3. Fill in the [common fields](#common-rule-fields) and any [scope-specific fields](#fields-by-rule-scope).
4. (Optional) Click **Add Permission** to add multiple rules. Use the **Up** and **Down** buttons to adjust rule execution order.
5. Click **Add** to save the rule.

#### Manage Multiple Rules (All Users Only)

For **All Users** rules, you can change rule order from the **More** menu in the **Actions** column:

- Move Up
- Move Down
- Move to Top
- Move to Bottom

Rules are evaluated from top to bottom, so order determines priority.

#### Edit and Manage Rules

On the **Permissions** page, you can edit or delete existing rules:

- In the **Actions** column of the corresponding rule, click the **Edit** button to modify rule fields, matching patterns, or IP range settings.
- Click the **Delete** button to remove a rule.

### Create Authorization Rules via REST API

Rules are also managed through `/api/v5/authorization/sources/built_in_database` API endpoints.

Each rule can be applied to:
* A specific client by client ID
  *  `/api/v5/authorization/sources/built_in_database/clientid`
* A specific client by username
  * `/api/v5/authorization/sources/built_in_database/username` 

* all clients
  *  `/api/v5/authorization/sources/built_in_database/all` 

#### Using Regular Expressions and IP Ranges

In addition to exact matches for client ID or username, you can define rules for groups of clients using regular expressions or IP address ranges. This allows you to apply the same ACLs to multiple clients that share the same naming pattern or network location, avoiding the need to create duplicate rules multiple times.

Two new fields are available for this purpose:

| Field           | Description                                                  |
| --------------- | ------------------------------------------------------------ |
| **scope**       | The match type for the rule. Options: `all` (default), `clientid_re` (client ID regex), `username_re` (username regex), `ipaddress` (IP or CIDR). |
| **scope_value** | The value to match against, based on the `scope` type. For regex, provide a regular expression (e.g., `^emqx-.*`). For `ipaddress`, use exact IP or CIDR notation (e.g., `192.168.1.0/24`). |

Below is an example of how to create rules for a client (`client1`):

```bash
curl -X 'POST' \
  'http://localhost:18083/api/v5/authorization/sources/built_in_database/clientid' \
  -H 'accept: */*' \
  -H 'Content-Type: application/json' \
  -d '[
  {
    "clientid": "client1",
    "scope": "username_re",
    "scope_value": "^emqx-.*",
    "rules": [
      {
        "permission": "allow",
        "action": "publish",
        "topic": "v1/devices/#"
      },
      {
        "permission": "deny",
        "action": "all",
        "topic": "v1/#"
      },
      {
        "permission": "allow",
        "action": "subscribe",
        "topic": "v1/devices/+/robot_state"
      }
    ]
  }
]'
```

Each rule contains:

- `clientid`: Exact Client ID to which this rule applies.
- `scope`: The additional matching filter. `scope = "username_re"` with `scope_value = "^emqx-.*"` means these rules only apply to usernames starting with `emqx-` *for this client*.

* `permission`: Whether to allow or deny a certain type of operation request from the current client/user; optional values: `allow` or `deny`;
* `action`: Configure the operation corresponding to this rule; optional values: `publish`, `subscribe`, or `all`;
* `topic`: Configure the corresponding topic to this rule, supporting [topic placeholders](./authz.md#topic-placeholders).
* `qos`: (Optional) A number array used to specify the QoS levels that the rule applies to, e.g., `[0, 1]`, `[1, 2]`. The default is all QoS levels.
* `retain`: (Optional) Used to specify whether the current rule supports retained messages. Value options are `true`, `false`. Default is to allow retained messages.

