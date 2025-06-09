# Integrate with LDAP

[Lightweight Directory Access Protocol (LDAP)](https://ldap.com/) is a protocol used to access and manage directory information. EMQX supports integrating with an LDAP server for password authentication. This integration enables users to use their LDAP credentials for authentication in EMQX.

::: tip Prerequisite

Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## Password Authentication Methods

EMQX's LDAP integration includes two distinct authentication methods:
- **Local Password Comparison**

   EMQX queries LDAP to retrieve the client's password and then compares the retrieved password with the password information stored locally in EMQX. This method allows EMQX to have greater flexibility and capability when performing LDAP user authentication, supporting more complex validation logic and security strategies. However, this approach requires users to have the necessary permissions to configure schemas and data on the LDAP server.

- **LDAP Bind Authentication**

  EMQX directly uses LDAP binding to authenticate usernames and passwords. This method provides basic authentication solely through the LDAP `BIND` operation, involving only the use of existing usernames and passwords, without engaging in complex queries or data processing. Therefore, this approach is suitable for situations where users already have account data on the LDAP server or lack the permissions to add or modify data.

In both strategies, EMQX can retrieve the user's `isSuperuser` flag as well as additional ACL rules from the LDAP entry addressed by the `base_dn` and `filter` configuration options.

## LDAP Data Schema and Query

::: tip

This section applies to the "Local Password Comparison" authentication method. If you are using the "LDAP Bind Authentication" method, you can bypass this section.

:::

This section describes how to configure LDAP schema, create LDAP credentials, and store the credentials for password authentication.

An LDAP schema defines the structure and rules for organizing and storing authentication data within an LDAP directory. The LDAP authenticator supports almost all LDAP schema. Here is an example schema for OpenLDAP:

```sql

attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.1.4 NAME 'isSuperuser'
	EQUALITY booleanMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.7
	SINGLE-VALUE
	USAGE userApplications )


objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
	SUP top
	STRUCTURAL
	MAY ( isSuperuser )
    MUST ( uid $ userPassword ) )

```
The given schema example defines an attribute named `isSuperuser` to indicate whether a user is a superuser. It also defines an object class named `mqttUser` which is used to represent the user and the object class must include the `userPassword` attribute.

To create LDAP credentials, users need to define some necessary attribute names, the distinguished name (dn) of the base object, and a filter for the LDAP query.

Below are some sample LDAP credentials specified in [LDAP Data Interchange Format (LDIF)](https://ldap.com/ldif-the-ldap-data-interchange-format/) based on the given schema for OpenLDAP:

```sql

## create organization: emqx.io
dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

## create organization unit: testdevice.emqx.io
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

## create user=mqttuser0001,
#         password=mqttuser0001,
#         passhash={SHA}mlb3fat40MKBTXUVZwCKmL73R/0=
#         base64passhash=e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9
dn:uid=mqttuser0001,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0001
userPassword:: e1NIQX1tbGIzZmF0NDBNS0JUWFVWWndDS21MNzNSLzA9

## create user=mqttuser0002
#         password=mqttuser0002,
#         passhash={SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
#         base64passhash=e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0002
userPassword:: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=

## create a superuser mqttuser0003
#         password=mqttuser0003,
#         passhash={MD5}ybsPGoaK3nDyiQvveiCOIw==
#         base64passhash=e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=
dn:uid=mqttuser0003,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
uid: mqttuser0003
isSuperuser: TRUE
userPassword:: e01ENX15YnNQR29hSzNuRHlpUXZ2ZWlDT0l3PT0=

```

Edit the LDAP configuration file `slapd.conf` to include the schema and LDIF file so that they will be loaded when the LDAP server is started. Below is an example `slapd.conf` file:

::: tip

You can determine how to store LDAP credentials and access them based on your business needs.

:::

```sh
include         /usr/local/etc/openldap/schema/core.schema
include         /usr/local/etc/openldap/schema/cosine.schema
include         /usr/local/etc/openldap/schema/inetorgperson.schema
include         /usr/local/etc/openldap/schema/emqx.schema

TLSCACertificateFile  /usr/local/etc/openldap/cacert.pem
TLSCertificateFile    /usr/local/etc/openldap/cert.pem
TLSCertificateKeyFile /usr/local/etc/openldap/key.pem

database mdb
suffix "dc=emqx,dc=io"
rootdn "cn=root,dc=emqx,dc=io"
rootpw {SSHA}eoF7NhNrejVYYyGHqnt+MdKNBh4r1w3W

directory       /usr/local/etc/openldap/data
```

## Configure LDAP Authentication via Dashboard

You can configure how to use LDAP for password authentication in the EMQX Dashboard.

1. In the EMQX Dashboard, click **Access Control** -> **Authentication** from the left navigation menu.
2. On the **Authentication** page, click **Create** in the top right corner.
3. Click to select **Password-Based** as **Mechanism**, and **LDAP** as **Backend** to go to the **Configuration** tab, as shown below.

<img src="./assets/authn-ldap.png" alt="authn-ldap"  />

4. Follow the instructions below for the configuration:

   - Enter the information needed to connect to the LDAP server.

     - **Server**: Specify the server address that EMQX is to connect (`host:port`).
     - **Username**: Specify the LDAP root user name.
     - **Password**: Specify the LDAP root user password.

   - **Authentication configuration**: Fill in the authentication-related settings.

     - **Password Authentication Method**: Select the authentication method: `LDAP Bind Authentication` (default) or `Local Password Comparison`.

     - **Bind Password**: Specifies the password that EMQX uses to authenticate itself to the LDAP server before it can perform any operations or queries. It is referenced through a placeholder `${password}` that will be resolved at runtime with the actual password defined in the configuration option **Password**.

     - **Base DN**: The name of the base object entry (or possibly the root) relative to which the search is to be performed. For more information, see [RFC 4511 Search Request](https://datatracker.ietf.org/doc/html/rfc4511#section-4.5.1), the placeholders are supported.

       ::: tip

       DN refers to Distinguished Name. This is a unique identifier of each object entry and it also describes the location of the entry within the information tree.

       :::

     - **Password Hash Attribute**: Specifies the attribute representing the user's password, applicable when `Local Password Comparison` is selected as the authentication method. The value of this attribute should follow [RFC 3112](#https://datatracker.ietf.org/doc/html/rfc3112), the supported algorithm is `md5` `sha` `sha256` `sha384` `sha512` and `ssha`.

     - **Is Superuser Attribute**: Identifies the attribute that indicates whether a user is a superuser, applicable when `Local Password Comparison` is selected as the authentication method.  The value of this attribute should be in boolean, if absent is equal to `false`.

- **Precondition**: A [Variform expression](../../configuration/configuration.md#variform-expressions) used to control whether this LDAP authenticator should be applied to a client connection. The expression is evaluated against attributes from the client (such as `username`, `clientid`, `listener`, etc.). The authenticator will only be invoked if the expression evaluates to the string `"true"`. Otherwise, it will be skipped. For more information about the precondition, see [Authentication Preconditions](./authn.md#authentication-preconditions).
- **Enable TLS**: Turn on the toggle switch if you want to enable TLS. For more information on enabling TLS, see [Network and TLS](../../network/overview.md).
- **Filter**: Defines the criteria for the LDAP query. The filter sets conditions that an entry must meet to be considered a match.
  The syntax of the filter follows [RFC 4515](#https://www.rfc-editor.org/rfc/rfc4515) and also supports placeholders.
- **Advanced Settings**: Set the concurrent connections and waiting time before a connection is timed out.
  - **Connection Pool size** (optional): Input an integer value to define the number of concurrent connections from an EMQX node to LDAP. Default: `8`.
  - **Query Timeout** (optional): Specify the waiting period before EMQX assumes the query is timed out. Units supported include milliseconds, second, minute, and hour.

After you finish the settings, click **Create**.

## Configure LDAP Authentication via Configuration Items

You can configure the EMQX LDAP authenticator with EMQX configuration items.<!--插入超链接-->

LDAP authentication is identified with `mechanism = password_based` and `backend = ldap`.

Below is a sample configuration for the **Local Password Comparison** method:

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  method {
    type = hash
    password_attribute = "userPassword"
    is_superuser_attribute = "isSuperuser"
  }
  server = "127.0.0.1:389"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
}
```

Below is a sample configuration for the **LDAP Bind Authentication** method:

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  method {
    type = bind
    bind_password = "${password}"
  }
  server = "127.0.0.1:389"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
}
```

## Configure ACL Rules

EMQX can retrieve the user's ACL rules from the LDAP entry addressed by the `base_dn` and `filter` configuration options during the authentication process. These ACL rules are cached in the client's session and may be used to perform authorization checks without additional LDAP queries.

To fetch ACL rules from LDAP, you need to define any the following attributes in the LDAP schema:

- Attribute for puplish topic whitelist: (`mqttPublishTopic` below).
- Attribute for subscribe topic whitelist: (`mqttSubscriptionTopic`).
- Attribute for pubsub topic whitelist: (`mqttPubSubTopic`).
- Attribute for ACL rules: (`mqttAclRule`).
- Attribute for ACL TTL: (`mqttAclTtl`).

The names of these attributes may be arbitrary and may be configured in the LDAP authenticator configuration. The meaning of the attributes is the same as in [LDAP authorizer](../authz/ldap.md), except for the `mqttAclTtl` attribute. This attribute is used to specify the TTL of the ACL rules and is supported only in the LDAP authenticator.
Its value is the number of seconds for which the ACL rules are valid. After the TTL expires, the client will be treated according EMQX authorization settings.

The `mqttAclRule` supports time units common for EMQX configuration, e.g. `1s`, `15m`, `1h`, `1d`.

The example schema below defines the attributes for ACL rules:

```
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.1.4 NAME 'isSuperuser'
	EQUALITY booleanMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.7
	SINGLE-VALUE
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.1 NAME ( 'mqttPublishTopic' 'mpt' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.2 NAME ( 'mqttSubscriptionTopic' 'mst' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.3 NAME ( 'mqttPubSubTopic' 'mpst' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.4 NAME ( 'mqttAclRule' 'mar' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
attributetype ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4.5 NAME ( 'mqttAclTtl' 'mat' )
	EQUALITY caseExactMatch
	SUBSTR caseExactSubstringsMatch
	SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
	USAGE userApplications )
objectclass ( 1.3.6.1.4.1.11.2.53.2.2.3.1.2.3.4 NAME 'mqttUser'
	SUP top
	STRUCTURAL
	MAY ( isSuperuser $ mqttPublishTopic $ mqttSubscriptionTopic $ mqttPubSubTopic $ mqttAclRule $ mqttAclTtl )
  MUST ( uid $ userPassword ))
```

Below is an example of LDAP authentication data specified in [LDAP Data Interchange Format (LDIF)](https://ldap.com/ldif-the-ldap-data-interchange-format/) based on the given schema for OpenLDAP:

```sql
dn:dc=emqx,dc=io
objectclass: top
objectclass: dcobject
objectclass: organization
dc:emqx
o:emqx,Inc.

# create testdevice.emqx.io
dn:ou=testdevice,dc=emqx,dc=io
objectClass: top
objectclass:organizationalUnit
ou:testdevice

## create user=mqttuser0002
#         password=mqttuser0002,
#         passhash={SSHA}n9XdtoG4Q/TQ3TQF4Y+khJbMBH4qXj4M
#         base64passhash=e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
dn:uid=mqttuser0002,ou=testdevice,dc=emqx,dc=io
objectClass: top
objectClass: mqttUser
objectClass: mqttDevice
objectClass: mqttSecurity
uid: mqttuser0002
isEnabled: TRUE
mqttAccountName: user2
mqttPublishTopic: mqttuser0002/pub/1
mqttPublishTopic: mqttuser0002/pub/+
mqttPublishTopic: mqttuser0002/pub/#
mqttSubscriptionTopic: mqttuser0002/sub/1
mqttSubscriptionTopic: mqttuser0002/sub/+
mqttSubscriptionTopic: mqttuser0002/sub/#
mqttPubSubTopic: mqttuser0002/pubsub/1
mqttPubSubTopic: mqttuser0002/pubsub/+
mqttPubSubTopic: mqttuser0002/pubsub/#
mqttAclRule: [{"permission": "allow", "action": "pub", "topic": "mqttuser0002/complexrule1/1"}]
mqttAclRule: {"permission": "allow", "action": "pub", "topic": "mqttuser0002/complexrule2/#"}
mqttAclTtl: 1s
userPassword:: e1NTSEF9bjlYZHRvRzRRL1RRM1RRRjRZK2toSmJNQkg0cVhqNE0=
```

To enable ACL rule caching, you need to **explicitly** configure the attribute names in the LDAP authenticator configuration:

```bash
{
  backend = "ldap"
  mechanism = "password_based"
  method {
    type = hash
    password_attribute = "userPassword"
    is_superuser_attribute = "isSuperuser"
  }
  server = "127.0.0.1:389"
  query_timeout = "5s"
  username = "root"
  password = "root password"
  pool_size = 8
  base_dn = "uid=${username},ou=testdevice,dc=emqx,dc=io"
  filter = "(objectClass=mqttUser)"
  publish_attribute = "mqttPublishTopic"
  subscribe_attribute = "mqttSubscriptionTopic"
  all_attribute = "mqttPubSubTopic"
  acl_attribute = "mqttAclRule"
  acl_ttl_attribute = "mqttAclTtl"
}
```





