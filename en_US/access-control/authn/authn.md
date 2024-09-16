# Authentication

Authentication is the process of verifying the identity of a client. It is an essential part of most applications and can help to protect our services from illegal client connections. 

EMQX supports several authentication mechanisms and also supports [TLS X.509](https://en.wikipedia.org/wiki/X.509) certificate authentication and [TLS-PSK](https://www.rfc-editor.org/rfc/rfc4279) authentication, which offers an option for the authentication request between the client and the server side. 

This section covers the basic concepts of identity authentication and the settings. 

::: tip

By default, EMQX does not enable the authentication feature, meaning it allows all clients to connect. If you are using it in a production environment, configure at least one authentication method in advance.

:::

## Authentication Mechanism

The authentication mechanisms supported in EMQX include:

- X.509 Certificate Authentication
- JWT authentication
- Username/password authentication
- Enhanced authentication of MQTT 5.0
- PSK Authentication

### X.509 Certificate Authentication

EMQX supports [X.509 certificate authentication](./x509.md) for client authentication. Using X.509 certificate authentication in EMQX, clients and servers can establish trusted connections through TLS/SSL, ensuring the authenticity of communication parties and the integrity of the data transmitted. EMQX allows for both one-way and two-way authentication: one-way authentication where only the server is authenticated by the client, and two-way authentication where both client and server mutually verify each other's certificates. This flexibility caters to various levels of security requirements and deployment scenarios.

### JWT Authentication

[JSON Web Token (JWT)](https://jwt.io/) is a token-based authentication mechanism that does not rely on the server to retain client authentication or session information.

The client carries the JWT in the connection request, and EMQX uses the pre-configured secret or public key to verify the JWT signature. If the user configures a JWKS endpoint, the JWT authenticator verifies the JWT signature using the list of public keys queried from the JWKS endpoint.

### Password Authentication

EMQX supports the simplest and most popular password authentication, which requires the client to provide credentials indicating identities, such as username, client ID, and the corresponding password. In some cases, users may choose to use some fields in the TLS certificate (such as the certificate's Common Name) as the client's identity credentials. Either way, these credentials are stored in advance in a database, where passwords are usually stored in salted and hashed forms.

This is how password authentication in EMQX works: The client will carry the identity credentials when initiating a connect request, EMQX will query the database for the hashed password corresponding to the identity credentials provided by the client, and will only accept the connection after the match is successful.
![emqx-authn-flow](./assets/emqx-authn-flow.png)

Besides the built-in database, EMQX also supports integration with various backend databases for password authentication, including MySQL, PostgreSQL, MongoDB, and Redis.

EMQX can also be configured to delegate authentication work to external services, such as an HTTP server developed by our users.

### MQTT 5.0 Enhanced Authentication

[MQTT 5.0 enhanced authentication](https://www.emqx.com/en/blog/mqtt5-enhanced-authentication) extends the basic authentication to include challenge/response style authentication. The implementation of enhanced authentication allows the use of various more secure authentication mechanisms, such as Salted Challenge Response Authentication Mechanism (SCRAM) authentication, Kerberos authentication, etc. The concrete EMQX implementation of the enhanced authentication supports SCRAM user management through our built-in database and external HTTP services.

### PSK Authentication

[PSK authentication](../../network/psk-authentication.md) in EMQX provides a simpler yet secure alternative to certificate-based TLS. It relies on a shared secret key known both to the client and the server, bypassing the need for digital certificates. This mechanism is particularly useful in resource-constrained environments, where the overhead of handling certificates can be significant.

## EMQX Authenticator

EMQX supports below authentication methods (referred to as authenticator hereafter) based on the authentication mechanism and backend database used: 

| Mechanism      | Database          | Description                                                  |
| -------------- | ----------------- | ------------------------------------------------------------ |
| Password-Based | Built-in Database | [Authentication with Mnesia database as credential storage](./mnesia.md) |
| Password-Based | MySQL             | [Authentication with MySQL database as credential storage](mysql.md) |
| Password-Based | PostgreSQL        | [Authentication with PostgreSQL database as credential storage](postgresql.md) |
| Password-Based | MongoDB           | [Authentication with MongoDB database as credential storage](./mongodb.md) |
| Password-Based | Redis             | [Authentication with Redis database as credential storage](./redis.md) |
| Password-Based | LDAP              | [Authentication with LDAP server as credential storage](./ldap.md) |
| Password-Based | HTTP Server       | [Authentication using external HTTP API for credential verification](./http.md) |
| JWT            |                   | [Authentication using JWT](./jwt.md)                         |
| SCRAM          | Built-in Database | [Authentication using SCRAM](./scram.md)                     |
| SCRAM          | HTTP Server       | [Authentication using RESP API-Based SCRAM](./scram_restapi.md) |
| GSSAPI         | Kerberos          | [Authentication using GSSAPI with Kerberos](./kerberos.md)   |
| Rule-Based     |                   | [Authentication using Client-info](./cinfo.md)                         |

## Authentication Chain

EMQX allows the creation of authentication chain using multiple authenticators and follows the authenticator's position in the chain to perform the authentication. The authenticators for creating the authentication chain should be of different types.

::: tip

Currently, EMQX only supports creating an authentication chain for MQTT clients. For gateways, it is recommended to use its own authenticator, and the authentication chain is also not supported. 

:::

When the X.509 certificate authentication is applied, it will be executed before performing the authentication chain.

### Workflow

With authentication chain configured, EMQX first tries to retrieve the matching authentication information from the first authenticator, if fails, it switches to the next authenticator to continue the process. 

Taking the password-based authentication as an example, EMQX tries to retrieve the possible authentication information from the configured authenticators:

1. If authentication credentials exist, and:
   - the authentication information matches (e.g. password is correct, JWT is valid), the client will be allowed to connect.
   - the authentication information does not match, and the client will be denied to connect.
2. When multiple authenticators are configured, EMQX will look for credentials in order. Once the match is successful it will allow the client to connect.
   If no credentials are found in the current authenticator, it will:
   - continue to retrieve the information from other authenticators.
   - refuse the connection if this is already the last authenticator.

::: tip 

The current authenticator will also be skipped when the authenticator is in a disabled state or there are errors in the process of authentication, for example, the database is not available.

:::

![](./assets/authn-chain.png)

### Use Case

Users have a large number of clients and a high connection rate, thus users can create an authentication chain with the Redis authenticator and the MySQL or PostgreSQL authenticator. With Redis as a caching layer, the query performance can be greatly improved.

## Super User

Usually, authentication only verifies the client's identity credentials, and whether the client has the right to publish and subscribe to certain topics is determined by the authorization system. But EMQX also provides a super user role and a permission preset feature to facilitate the follow-up publish/subscribe authorization steps. 

::: tip

Permission preset is supported in JWT and HTTP authentication. An [Access Control List (ACL)](./acl.md) of publish/subscribe permissions owned by the current client will be carried via the JWT Payload and HTTP response body. Permissions will be preset to the client after passing the authentication.

:::

You can check if a user is a superuser with the  `is_superuser` field in a database query, HTTP response, or JWT claims.

## Password Hashing

Storing a password in plain text would mean that anyone who looked through the database would be able to just read the user’s passwords. Therefore it is recommended to use password hashing algorithms to store the password as the generated hash. EMQX supports a variety of password hashing algorithms to meet various security requirements.

Besides, EMQX also supports adding salt to hashing, the unique hash produced by adding the salt (password_hash) can protect us against different attacks. 

### Workflow

The workflow of password hashing is as follows:

1. EMQX authenticator uses the configured query statement to query qualified identity credentials from the database, including hashed passwords and salt values;
2. When a client tries to connect, EMQX authenticator hashes the password provided by the client with the configured hash algorithm and the queried salt value;
3. EMQX authenticator compares the hash password queried from the database in step 1 with the hash value calculated in step 2. If they match, it allows the permission request.

Below is the hashing algorithms EMQX supports: 

```
# simple algorithms
password_hash_algorithm {
  name = sha256             # plain, md5, sha, sha512
  salt_position = suffix    # prefix, disable
}

# bcrypt
password_hash_algorithm {
  name = bcrypt
}

# pbkdf2
password_hash_algorithm {
  name = pbkdf2
  mac_fun = sha256          # md4, md5, ripemd160, sha, sha224, sha384, sha512
  iterations = 4096
  dk_length = 32           # optional, Unit: Byte
}
```

Note that there can be large performance differences between different hashing algorithms, so use your discretion. For reference, here are the average runtimes achieved after running each hashing algorithm 100 times on a 4-core 8GB machine:

![](./assets/hash-compare.png)

## Authentication Placeholders

EMQX supports using placeholders in the query statements and HTTP requests. During the authentication step, these placeholders will be replaced with actual client information to construct a query or HTTP request that matches the current client.

For example, in EMQX MySQL authenticator, The default query SQL uses the placeholder `${username}`: 

```
SELECT password_hash, salt FROM mqtt_user where username = ${username} LIMIT 1
```

So, when a client (name: `emqx_u`) initiates a connect request, the constructed query statement is like: 

```
SELECT password_hash, salt FROM mqtt_user where username = 'emqx_u' LIMIT 1
```

EMQX currently supports the following placeholders:

- `${clientid}`:  It will be replaced by the client ID at runtime. The client ID is normally explicitly specified by the client in the `CONNECT` packet. If `use_username_as_clientid` or `peer_cert_as_clientid` is enabled, this field will be overridden by the username, fields in the certificate, or the content of the certificate.
- `${username}`: It will be replaced with the username at runtime. The username comes from the `Username` field in the `CONNECT` packet. If `peer_cert_as_username` is enabled, it will be overridden by the fields or the content of the certificate.
- `${password}`: It will be replaced with the password at runtime. The password comes from the `Password` field in the `CONNECT` packet.
- `${peerhost}`: It will be replaced with the client's IP address at runtime. EMQX supports [Proxy Protocol](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt), that is, even if EMQX is deployed behind some TCP proxy or load balancer, users can still use this placeholder to get the real IP address.
- `${peername}`: It will be replaced with the client's IP address and port in runtime, and the format is `IP: PORT`.
- `${cert_subject}`: It will be replaced by the subject of the client's TLS certificate at runtime. If the load balancer sends client certificate information to the TCP listener, ensure that Proxy Protocol v2 is in use.
- `${cert_common_name}`: It will be replaced by the Common Name of the client's TLS certificate at runtime. If the load balancer sends client certificate information to the TCP listener, ensure that Proxy Protocol v2 is in use.
- `${client_attrs.NAME}`: A client attribute. `NAME` will be replaced by an attribute name set based on predefined configurations at runtime. For details about the client attributes, see [MQTT Client Attributes](../../client-attributes/client-attributes.md).

## Configure Authenticators

EMQX provides 3 ways to use authentication, namely: Dashboard, Configuration file and HTTP API.

### Configure with Dashboard

EMQX Dashboard is an intuitive way to configure EMQX authenticators, where you can check their status or customize the settings. For example, as shown in the screenshot below, you have configured 2 authenticators: password authentication based on built-in database and JWT authentication. 

![](./assets/authn-dashboard-2.png)

### Configure with Configuration File

You can also configure EMQX authenticators with our configuration file. 

For example, as shown in the `authentication` field below, we have created an authentication chain with multiple authenticators, these authenticators will run in the order as they are in the configuration file. 

```
# emqx.conf

# Specific global authentication chain for all MQTT listeners
authentication = [
  ...
]

listeners.tcp.default {
  ...
  # Specific authentication chain for the specified MQTT listener
  authentication = [
    ...
  ]
}

gateway.stomp {
  ...
  # Specific global authenticator for all STOMP listeners
  authentication = {
    ...
  }

}
```

Different types of authenticators have different configuration item requirements. For more information, you may refer to the Configuration chapter.<!--后续插入到对应章节的超链接-->

### Configure with HTTP API

Compared with the configuration file, the HTTP API is more convenient to use and supports runtime updates, which can automatically synchronize configuration changes to the entire cluster.

You can manage EMQX authentication chains and authenticators via EMQX authentication API, for example, to create a global authenticator, or to update the configuration of a specific authenticator.

- `/api/v5/authentication`: API endpoint for managing global MQTT authentications;
- `/api/v5/gateway/{protocol}/authentication`: API endpoint for managing global authentication for other access protocols;
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication`: API endpoint for managing authentication of listeners for other access protocols;

#### Authenticator ID

To operate on a specific authenticator, you need to append an authenticator ID to the above endpoints, such as `/api/v5/authentication/{id}`. To facilitate maintenance, the ID here is not automatically generated by EMQX and returned by the API, but follows a set of predefined specifications:

```
<mechanism>:<backend>
```

or:

```
<mechanism>
```

For example, 

1. `password_based:built_in_database`
2. `jwt`
3. `scram:built_in_database`

We have a similar set of conventions for the listener ID:

```bash
<transport_protocol>:<name>
```

The format of the gateway listener ID is to add the protocol name in front:

```bash
<protocol>:<transport_protocol>:<name>
```

Note that both authenticator IDs and listener IDs need to follow URL encoding conventions when they are used in URLs, for example, we need to replace `:` with `%3A`:

```bash
PUT /api/v5/authentication/password_based%3Abuilt_in_database
```

#### Data Operation API

For authentication using [built-in database](./mnesia.md) and [MQTT 5.0 enhanced authentication](./scram.md), EMQX provides HTTP API to manage authentication data, including the operations such as creating, updating, deleting, and listing data. For more information, see [Manage authentication data with HTTP API](./user_management.md).

For more detailed API requests and parameters, see [HTTP API](../../admin/api.md).

