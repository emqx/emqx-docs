# Introduction

Authentication is an important part of most applications. MQTT protocol supports username/password authentication. Enabling authentication can effectively prevent illegal client connections.

Authentication in EMQX Broker means that when a client connects to EMQX Broker, the server configuration  is used to control the client's permission to connect to the server.

EMQX Broker's authentication support includes two levels:

- The MQTT protocol specifies the user name and password in the CONNECT packet by itself. EMQX Broker supports multiple forms of authentication based on Username, ClientID, HTTP, JWT, LDAP, and various databases such as MongoDB, MySQL, PostgreSQL, Redis through plugins.
- At the transport layer, TLS guarantees client-to-server authentication using client certificates and ensures that the server verifies the server certificate to the client. PSK-based TLS/DTLS authentication is also supported.

This authentication methods supported by EMQX and the configuration methods of the corresponding plugins are introduced in this article.

## Authentication method

EMQX supports the use of built-in data sources (files, built-in databases), JWT, external mainstream databases, and custom HTTP APIs as authentication data sources.

The data source connection and authentication logic are implemented through plugins. Each plugin corresponds to an authentication method, and the corresponding plugin needs to be enabled before use.

When the client connects, the plugin implements the identity authentication of the client by checking whether its username/clientid and password are consistent with the information of the specified data source.

Authentication methods supported by EMQX:

**Built-in data source**


{% emqxee %}

* [Built-in Auth](../modules/mnesia_authentication.md)

{% endemqxee %}

{% emqxce %}

* [Mnesia (username/clientid) authentication](./auth-mnesia.md)

{% endemqxce %}


The configuration file and the built-in database of EMQX are used to provide an authenticated data source, which is managed through the HTTP API and is simple and lightweight.



**External Database**


{% emqxee %}

* [MySQL authentication](../modules/mysql_authentication.md)
* [PostgreSQL authentication](../modules/pgsql_authentication.md)
* [Redis authentication](../modules/redis_authentication.md)
* [MongoDB authentication](../modules/mongo_authentication.md)
* [LDAP authentication](../modules/ldap_authentication.md)

{% endemqxee %}

{% emqxce %}

* [LDAP authentication](./auth-ldap.md)
* [MySQL authentication](./auth-mysql.md)
* [PostgreSQL authentication](./auth-postgresql.md)
* [Redis authentication](./auth-redis.md)
* [MongoDB authentication](./auth-mongodb.md)

{% endemqxce %}


The external database can store a large amount of data, while facilitating integration with external device management systems.



Others

{% emqxee %}

* [HTTP authentication](../modules/http_authentication.md)
* [JWT authentication](../modules/jwt_authentication.md)

{% endemqxee %}

{% emqxce %}

* [HTTP authentication](./auth-http.md)
* [JWT authentication](./auth-jwt.md)

{% endemqxce %}


JWT authentication can issue authentication information in batches, and HTTP authentication can implement complex authentication logic.



::: tip

After changing the plugin configuration, you need to restart the plugin to take effect. Some authentication plugins include [ACL function](./acl.md).

:::


## Authentication results

Any authentication method will eventually return a result:

- Authentication succeeded: the client authentication succeeded after comparison
- Authentication failed: the client authentication fails after comparison, which is because the password in the data source does not match the current password
- Ignore: The authentication data is not found with the current authentication method, and the result cannot be determined explicitly. The next method of authentication chain or anonymous authentication is used to determine the result.

## Anonymous Login

By default, EMQX allows clients to connect without authentication.

```bash
# etc/emqx.conf

## Value: true | false | false_quick_deny
allow_anonymous = true
```

::: tip

Disable anonymous authentication in production environments.

:::

When set to `true` (to 'allow') or `false` (to 'deny'),
it takes effect when any of the following conditions is met:

- When there is no authentication plug-in enabled.
- All authentication plug-ins returned 'ignore' (i.e. not explicitly 'allow' or 'deny').

This means, even when set to `false`,
the anonymous-check is done after going through the authentication plug-ins.

This is due to the fact that some authentication backends may have the precedence to decide if
this client is anonymous or not.

The drawback of this behaviour is, it may cause unnecessary overhead if anonymous is not allowed after all.
As an enhancement, `false_quick_deny` option was added in v4.3.17 (Enterprise e4.3.12).
This new config can be used to quickly deny all anonymous clients
(having no username provided in connection request) without involving any of the authentication plug-ins.

## Password salting rules and hash methods

The hash method can be enabled in most EMQX authentication plugins. Only the password cipher text is saved in the data source to ensure data security.

When the hash method is enabled, the user can specify a salt for each client and configure a salting rule. The password stored in the database is the cipher text processed according to the salting rule and hash method.

Taking MySQL authentication as an example：

**Salting rules and hash method configuration:**

```bash
# etc/plugins/emqx_auth_mysql.conf

## only hash is used without salt
auth.mysql.password_hash = sha256

## salt prefix: use sha256 to encrypt salt + password
auth.mysql.password_hash = salt,sha256

## salt suffix: encrypted password using sha256 + salt
auth.mysql.password_hash = sha256,salt

## pbkdf2 with macfun iterations dklen
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
## auth.mysql.password_hash = pbkdf2,sha256,1000,20
```
<!-- TODO 翻译最后一句 -->

### How to generate authentication information

1. Assign user name, Client ID, password, and salt for each client
2. Use the same salting rules and hash method as MySQL authentication to process client information to get cipher text
3. Write the client information to the database. The client password should be cipher text information.

### EMQX authentication process

1. The authentication data such as password (ciphertext) and salt are queried according to the configured authentication SQL combined with the information passed in by the client. If there is no query result, the authentication will terminate and the ignore result will be returned
2. The cipher text is calculated according to the configured salting rule and hash method. If no hash method is enabled,  this step is skipped.
3. Compare the cipher text stored in the database with the cipher text calculated by the current client. If the comparison is successful, the authentication succeeds. Otherwise, the authentication fails.

MySQL authentication function logic diagram:

![image-20200217154254202](./assets/image-20200217154254202.png)

::: tip
The authentication can be performed normally when the salting rules and hash method of the written data are consistent with the configuration of the corresponding plugin. It will invalidate existing authentication data when changing the hashing method.
:::



## Authentication chain

When multiple authentication methods are enabled at the same time, EMQX will perform chain authentication in the order in which the plugins are opened:
- Once authentication succeeds, terminate the authentication chain and allow clients to access
- Once authentication fails, terminate the authentication chain and prohibit client access
- If Failing to pass until the last authentication method,  it is determined according to  **anonymous authentication** configuration
  - Allow client access when anonymous authentication is enabled
  - Deny client access when anonymous authentication is disabled



![_images/guide_2.png](./assets/guide_2.png)

<!-- replace -->

::: tip

It can improve client authentication efficiency when enabling only one authentication plugin at the same time

:::


## TLS authentication
The default port for MQTT TLS is 8883:

```bash
listener.ssl.external = 8883
```

Configure certificates and CAs:

```bash
listener.ssl.external.keyfile = etc/certs/key.pem
listener.ssl.external.certfile = etc/certs/cert.pem
listener.ssl.external.cacertfile = etc/certs/cacert.pem
```

Note that the `key.pem`,` cert.pem` and `cacert.pem` under the default directory of ` etc/certs` are self-signed certificates generated by EMQX Broker. Therefore, when testing with a client that supports TLS, you need to configure the above CA certificate `etc/certs/cacert.pem` to the client.

The cipher list supported by the server needs to be specified explicitly. The default list is consistent with Mozilla's server cipher list:

```bash
listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA
```

## PSK authentication 
If you want to use PSK authentication, you need to comment out `listener.ssl.external.ciphers` in [TLS Authentication](#auth-tls), and then configure ` listener.ssl.external.psk_ciphers`:

```bash
#listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,...
listener.ssl.external.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

```

Then enable the emqx_psk_file plugin:

```bash
$ emqx_ctl plugins load emqx_psk_file
```

The configuration file for PSK is `etc/psk.txt`. A colon`: ` is used to separate the PSK ID and PSK:

```bash
client1:1234
client2:abcd
```

**Notice**:

If the `RSA-PSK` cipher suites are used, the `RSA` certificate is still required, see [RFC4279](https://www.rfc-editor.org/rfc/rfc4279#section-4) for details.