# NATS Protocol Gateway

Starting from EMQX 5.10.0, EMQX introduces the NATS protocol gateway based on the [NATS Protocol](https://docs.nats.io/reference/reference-protocols/nats-protocol). It enables EMQX to accept connections from NATS clients and perform message interoperability with MQTT. This document describes its capabilities and guides you through the process of enabling and configuring the NATS gateway.

## Feature Overview

The NATS protocol gateway currently supports the following core features:

### Protocol Support

- **Full support for NATS protocol message types**:
  - Connection and session management: `INFO`, `CONNECT`
  - Message publish/subscribe: `PUB`, `HPUB`, `SUB`, `UNSUB`
  - Message delivery and response: `MSG`, `HMSG`
  - Heartbeats and status: `PING`, `PONG`, `+OK`, `-ERR`
- **Verbose mode support**: Enables response acknowledgment when clients connect with `CONNECT verbose=true`.

### Interoperability with MQTT

- **Bidirectional message interoperability with MQTT**:
  - Messages published by NATS clients are translated into MQTT publishes.
  - MQTT messages are forwarded to NATS clients subscribed to the corresponding topics.
- **Support for NATS wildcard subscriptions**, automatically converted to MQTT-compatible topic formats.
- **Support for Queue Group shared subscriptions**: NATS Queue Group subscriptions are converted to the MQTT shared subscription format.
- **Support for Request/Reply mode**, including:
  - Requests from NATS clients are translated into MQTT requests.
  - If no MQTT subscriber is found for the target topic, EMQX returns an error response quickly.

### Networking and Connectivity

- **Multiple transport protocols supported**: TCP, TLS, WebSocket (WS), and WebSocket over TLS (WSS).

## Cross-Protocol Messaging Between NATS and MQTT

The NATS protocol is fully compatible with the publish/subscribe messaging model and interoperates with MQTT messaging through the NATS gateway. The conversion rules are as follows:

- **PUB and HPUB messages are treated as publish operations**:
  - The topic is derived from the `subject` field in the PUB message. For example, `t.a` will be converted to MQTT topic `t/a`.
  - The message payload is taken directly from the PUB message body.
  - If the client connects with `CONNECT verbose=1`, the translated MQTT message uses QoS 1; otherwise, QoS is set to 0.
- **SUB messages are treated as subscription requests**:
  - The topic is derived from the `subject` field in the SUB message. For example, `t.a` will be converted to MQTT topic `t/a`.
  - QoS follows the same rule: `verbose=1` results in QoS 1; otherwise, QoS 0.
  - Wildcards are supported. For example, `*.b.>` is converted to `+/b/#`.
  - Queue Groups are supported. The Queue Group value in the SUB message is converted into the group name for MQTT shared subscriptions.
- **UNSUB messages are treated as unsubscription requests**, and the subscription ID (sid) is used to identify the subscription to be removed.

::: tip

The NATS gateway does not implement its own access control for publish/subscribe operations. Topic permissions must be managed using the unified [authorization configuration](../access-control/authz/authz.md).

:::

## Enable the NATS Gateway

Starting from EMQX 5.10.0, the NATS gateway can be enabled in three ways:

- Through the Dashboard
- Using the REST API
- By editing the `emqx.conf` configuration file

::: tip

In cluster mode, configurations made via the Dashboard or REST API are automatically applied across all nodes. To apply settings to a specific node only, use the `emqx.conf` configuration file on that node.

:::

### Enable via Dashboard

To quickly enable the NATS gateway from the EMQX Dashboard:

1. Navigate to **Management** -> **Gateways** in the left-hand menu.
2. On the **Gateways** page, locate **NATS** and click the **Setup** button in the **Actions** column to enter the **Initialize NATS** setup wizard.
3. Follow the steps in the wizard:
   - On the **Basic Configuration** step, accept the default values and click **Next**.
   - On the **Listeners** step, either configure a listener or skip and click **Next**.
      (For detailed listener configuration, see [Add a Listener](#add-a-listener).)
   - Click **Enable** to activate the NATS gateway.

Once activation is complete, you will be redirected to the **Gateways** page, where the status of the NATS gateway will show as **Enabled**.

### Enable via REST API

You can use the following example to enable the NATS gateway via the REST API:

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/nats' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "nats",
  "enable": true,
  "mountpoint": "nats/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "4222",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'

```
### Enable via Configuration File

You can use the following configuration example to enable the NATS gateway via `emqx.conf`:

```properties
gateway.nats {

  mountpoint = "nats/"

  listeners.tcp.default {
    bind = 4222
    acceptors = 16
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```
The NATS gateway supports TCP, SSL, WS, and WSS type listeners. For the complete list of configurable parameters, refer to the gateway configuration - listeners section in the [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

## Customize Your NATS Gateway

In addition to the default settings, EMQX provides various configuration options to better suit your specific business needs. This section provides a detailed overview of the available options on the **Gateways** page.

### Basic Settings

1. On the **Gateways** page, locate **NATS** and click the **Settings** button in the **Actions** column.

2. In the **Settings** tab, you can configure the gateway's connection parameters, mountpoint prefix, and client identity overrides.

   - **Server Name**: A unique identifier for the gateway, used for internal reference. Default: `emq_nats_gateway`.

   - **Mountpoint**: A string prefix automatically added to all topics passing through the gateway. This helps isolate topics between protocols. For example, using `nats/` enables cross-protocol routing without requiring clients to manually include the prefix.

   - **Default Heartbeat Interval**: The interval at which the server sends `PING` packets to check if the client is still alive. Default: `60` seconds.

   - **Heartbeat Timeout Threshold**: If the client fails to respond within this time frame, it is considered disconnected.

   - **Maximum Payload Size**: The maximum size (in bytes) of a single `PUB` or `HPUB` message payload. Default: `1048576` bytes.

   - **Idle Timeout**: The period (in seconds) after which an inactive client connection is considered stale and will be closed. Default: `30` seconds.

   - **Enable Statistics**: Whether to enable statistics collection and reporting for this gateway. Default: enabled.

   - **Client Info Override**: Defines how to extract authentication information from the `CONNECT` packet.

     ::: tip

     When authentication is enabled, make sure to map the correct fields for `username` and `password` to ensure proper credential processing.

     :::

     - **Username**: Maps to the `user` field in the `CONNECT` packet.
     - **Password**: Maps to the `pass` field in the `CONNECT` packet.
     - **Client ID**: Can be set to `${generated}` for auto-generation, or customized using specific logic.

3. Click **Update** to apply your changes.

### Add a Listener

You can further customize your gateway by editing, deleting, or adding new listeners in the **Listeners** tab.

1. In the **Listeners** tab, click **+ Add Listener**.

2. In the **Add Listener** dialog, configure the following options:

   **Basic Settings**

   - **Name**: A unique name to identify the listener.
   - **Type**: Select the listener type. Supported options for NATS are `tcp`, `ssl`, `ws`, and `wss`.
   - **Bind**: The port on which the listener will accept incoming connections.

   **Listener Settings**

   - **Max Connections**: The maximum number of concurrent connections allowed. Default: `1024000`.
   - **Max Connection Rate (Listener)**: Maximum number of new connections accepted per second. Default: `1000`.
   - **Proxy Protocol**: Whether to enable Proxy Protocol v1/v2. Default: `false`.
   - **Proxy Protocol Timeout**: Timeout for receiving the Proxy Protocol header. If no header is received within the specified time, the connection is closed. Default: `3` seconds.

   **Very Peer Settings** (only applicable for SSL and WSS listeners)

   Mutual TLS is enabled by default. You must configure the TLS certificate, private key, and CA certificate. These can be uploaded or directly pasted into the form fields. For more information, see [Enable SSL/TLS Connections](../network/emqx-mqtt-tls.md).

   - **TLS Cert**: The TLS certificate file path or its contents.
   - **TLS Key**: The TLS private key file path or its contents.
   - **CA Cert**: The CA certificate file path or its contents.
   - **Force Verify Peer Certificate**: Whether to require client certificate verification. Default: `true`.

3. Click **Add** to complete listener creation.

### Configure Authentication

The NATS protocol supports various authentication methods, including username/password and token-based authentication. The NATS gateway supports the following authentication backends:

- [Built-in Database Authentication](../access-control/authn/mnesia.md)
- [MySQL Authentication](../access-control/authn/mysql.md)
- [MongoDB Authentication](../access-control/authn/mongodb.md)
- [PostgreSQL Authentication](../access-control/authn/postgresql.md)
- [Redis Authentication](../access-control/authn/redis.md)
- [HTTP Server Authentication](../access-control/authn/http.md)
- [JWT Authentication](../access-control/authn/jwt.md)
- [LDAP Authentication](../access-control/authn/ldap.md)

Unlike the MQTT protocol, the gateway supports only a single authenticator, not a list (or chain) of authenticators. If no authenticator is enabled, all NATS clients are allowed to connect without authentication.

The NATS gateway extracts authentication credentials from the `CONNECT` packet:

- **Client ID**: Auto-generated by default.
- **Username**: Value of the `user` field.
- **Password**: Value of the `pass` field.

#### Configure via Dashboard

The example below demonstrates configuring password-based authentication using an HTTP server:

1. In the NATS gateway settings, go to the **Authentication** tab.
2. Click **+ Create Authentication**, select **Password-Based** as the mechanism, and choose **HTTP Server** as the data source. Click **Next**.
3. Fill in the configuration parameters. Refer to [HTTP Password Authentication](../access-control/authn/http.md) for details on each option.
4. Click **Create**, then review and confirm the settings by clicking **Update**.

#### Configure via REST API

The following example show how to configure built-in database authentication by using REST API:

```
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/nats/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "backend": "built_in_database",
  "mechanism": "password_based",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  },
  "user_id_type": "username"
}'
```

#### Configure via Configuration File

The following examples show how to configure built-in database authentication in the configuration file:

```
gateway.nats {

  authentication {
    backend = built_in_database
    mechanism = password_based
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
    user_id_type = username
  }
}
```

For other authentication types, refer to the documentation on [EMQX Authenticators](../access-control/authn/authn.md#emqx-authenticators).

### Configure User-Level Interfaces

- For complete configuration reference, see: [NATS Gateway Configuration](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)
- For REST API details, see: [Gateway REST API Documentation](https://docs.emqx.com/zh/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)