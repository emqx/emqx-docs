# MQTT 5.0 Enhanced Authentication - Kerberos

Kerberos is a network authentication protocol that works on the basis of "tickets" to allow nodes communicating over a non-secure network to prove their identity to one another in a secure manner. It is designed to provide strong authentication for client/server applications by using secret-key cryptography.

EMQX integrates Kerberos authentication following the SASL/GSSAPI mechanism from RFC 4422.

::: tip
MQTT enhanced authentication is not supported before protocol version 5.
There is no mechanism negotiation, so the client must explicitly specify the mechanism as `GSSAPI-KERBEROS`.
:::

## Prerequisites

### Install Kerberos Library

Before configuring the Kerberos authenticator, you need to install the MIT Kerberos library on the EMQX node.

On Debian/Ubuntu, the required package are `libsasl2-2` and `libsasl2-modules-gssapi-mit`.

On Redhat, the required package are `krb5-libs` and `cyrus-sasl-gssapi`.

### Configure Kerberos Library

The Kerberos library configuration file is `/etc/krb5.conf`. The file contains the configuration information for the Kerberos library, including the realms and KDCs. The Kerberos library uses this file to locate the KDCs and realms.

Here is an example of a `krb5.conf` file:

```ini
[libdefaults]
    default_realm = EXAMPLE.COM
    default_keytab_name = /var/lib/emqx/emqx.keytab

[realms]
   EXAMPLE.COM = {
      kdc = kdc.example.com
      admin_server = kdc.example.com
   }
```

### Keytab Files

To test the Kerberos authenticator, you need to have a KDC (Key Distribution Center) server running and a valid keytab files for the server and clients.

EMQX can only support keytab file at the default location. You may try to configure system default value using environment variable `KRB5_KTNAME` or set `default_keytab_name` in `/etc/krb5.conf`.

## Configure with Dashboard

On [EMQX Dashboard](http://127.0.0.1:18083/#/authentication), click **Access Control** -> **Authentication** on the left navigation tree to enter the **Authentication** page. Click **Create** at the top right corner, then click to select **GSSAPI** as **Mechanism**, and **Kerberos** as **Backend**, this will lead us to the **Configuration** tab.

Set Kerberos principal for the server. For example, `mqtt/cluster1.example.com@EXAMPLE.COM`.

NOTE: The realm in use has to be configured in `/etc/krb5.conf` in EMQX nodes.

## Configure with Configuration Items

Sample configuration:

```hcl
  {
    mechanism = gssapi
    backend = kerberos
    principal = "mqtt/cluster1.example.com@EXAMPLE.COM"
  }
```

where `principal` is the server principal which must be found in the system default keytab file.

## Common Issues

- Error message `Keytab contains no suitable keys for mqtt/cluster1.example.com@EXAMPLE.COM`: The keytab file does not contain the required keys for the principal. Check if the default keytab file is correctly configured. Inspect the keytab file using `klist -k` command. e.g. `klist -kte /etc/krb5.keytab`. Please note that currently EMQX can only support keytab file at the default location. When this error happenss, the current system default keytab file path is also returned in the error message. You may try to configure system default value using environment variable `KRB5_KTNAME` or set `default_keytab_name` in `/etc/krb5.conf`.

- Error message `invalid_server_principal_string`: The Kerberos principal string must be of the form `service/SERVER-FQDN@REALM.NAME`.

- Error message `Cannot find KDC for realm "EXAMPLE.COM"`: The `EXAMPLE.COM` realm is missing in the `/etc/krb5.conf` file's `realms` section.

- Error message `Cannot contact any KDC for realm 'EXAMPLE.COM'`: The KDC service is not running or not reachable.

- Error message `Resource temporarily unavailable`: The kdc service configured in `/etc/krb5.conf` is not running or not reachable.

- Error message `Preauthentication failed`: The server ticket is not valid. Check if the keytab file is outdated.

## Authtiocation Flow

- Client to Server CONNECT Authentication Method="GSSAPI-KERBEROS", Authentication Data=ClientInitialToken
- Server to Client AUTH rc=0x18 Authentication Method="GSSAPI-KERBEROS", Authentication Data=ServerInitialToken
- One or more rounds of SASL challenge-response between client and server, all with Authentication Method="GSSAPI-KERBEROS"
- Server to Client CONNACK rc=0 Authentication Method="GSSAPI-KERBEROS"
