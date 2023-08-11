# Integrate with LDAP

EMQX supports integrating with LDAP for password authentication.

::: tip

- Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Data Schema and Query Statement

LDAP authenticator supports almost all LDAP schema. You can determine how to store credentials and access them as your business needs.

Users must provide some necessary attribute names, the distinguished name of the base object and a filter for LDAP query.

Here is an example schema for OpenLDAP:

```sql

attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.1.4 NAME 'isSuperuser'
	EQUALITY booleanMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.7
	SINGLE-VALUE
	USAGE userApplications )

objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
	AUXILIARY
	MAY ( isSuperuser )
    MUST ( userPassword ) )

```

This schema defines an attribute named `isSuperuser` to indicate whether a user is a superuser, then defines an auxiliary class namely `mqttUser` which is used to represent the user and must include the `userPassword` attribute.

## Configure with Dashboard

You can use EMQX Dashboard to configure how to use LDAP for password authentication.

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **Password-Based** as **Mechanism**, and **LDAP** as **Backend**, this will lead us to the **Configuration** tab, as shown below.

Follow the instruction below on how to configure:

**Connect**: Fill in the information needed to connect to LDAP.

- **Server**: Specify the server address that EMQX is to connect (`host:port`).
- **Username** Specify the LDAP root user name.
- **Password** Specify the LDAP root user password.

**TLS Configuration**: Turn on the toggle switch if you want to enable TLS. For more information on enabling TLS, see [Network and TLS](../../network/overview.md).

**Connection Configuration**: Set the concurrent connections and waiting time before a connection is timed out.

- **Pool size** (optional): Input an integer value to define the number of concurrent connections from an EMQX node to LDAP. Default: **8**.
- **Query Timeout** (optional): Specify the waiting period before EMQX assumes the query is timed out. Units supported include milliseconds, second, minute, and hour.

**Authentication configuration**: Fill in the authentication-related settings:
- **base_dn**: The name of the base object entry (or possibly the root) relative to which the `Search` is to be performed. For more information, see [RFC 4511 Search Request](#https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1), the placeholders is supported.
- **filter**: The `filter` that defines the conditions that must be fulfilled in order for the `Search` to match a given entry.
The syntax of the filter follows [RFC 4515](#https://www.rfc-editor.org/rfc/rfc4515) and also supports placeholders.
- **password_attribute**: Indicates which attribute is used to represent the user's password.The value of this attribute should follow [RFC 3112](#https://datatracker.ietf.org/doc/html/rfc3112), the supported algorithm is `md5` `sha` `sha256` `sha384` `sha512` and `ssha`.
- **is_superuser_attribute**: Indicates which attribute is used to represent whether the user is a superuser.The value of this attribute should be in boolean, if absent is equal to `false`.

Now we can click **Create** to finish the settings.

## Configure with Configuration Items

You can configure the EMQX LDAP authenticator with EMQX configuration items.<!--插入超链接-->

LDAP authentication is identified with `mechanism = password_based` and `backend = ldap`.

Sample configuration:

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  server = "127.0.0.1:389"
  password_attribute = "userPassword"
  is_superuser_attribute = "isSuperuser"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
}
```
