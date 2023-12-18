# OCPP Gateway

EMQX OCPP Gateway is a messaging protocol translator that bridges the gap between [OCPP](https://www.openchargealliance.org/) and MQTT protocols, allowing clients using these protocols to communicate with each other.

This OCPP Gateway provides a lightweight and simple messaging solution for clients and servers, enabling message exchange in a variety of messaging environments. With its support for Websocket and Websocket over TLS listeners, the OCPP gateway is a flexible and versatile tool for building messaging systems.

::: tip

The OCPP gateway is based on [OCPP v1.6](https://www.openchargealliance.org/protocols/ocpp-16/)

:::

## Enable OCPP Gateway

In EMQX 5.0, OCPP gateway can be configured and enabled through the Dashboard, HTTP API, and configuration file `emqx.conf`. This section takes the configuration via Dashboard as an example to illustrate the operating steps.

On EMQX Dashboard, click **Management** -> **Gateways** on the left navigation menu. On the **Gateways** page, all supported gateways are listed. Locate **OCPP** and click **Setup** in the **Actions** column. Then, you will be directed to the **Initialize OCPP** page.

::: tip

If you are running EMQX in a cluster, the settings you made through the Dashboard or HTTP API will affect the whole cluster. If you only want to change the settings with one node, configure with [`emqx.conf`](../configuration/configuration.md).

:::

To simplify the configuration process, EMQX offers default values for all required fields on the **Gateways** page. If you don't need extensive customization, you can enable the OCPP Gateway in just 3 clicks:

1. Click **Next** in the **Basic Configuration** tab to accept all the default settings.
2. Then you will be directed to the **Listeners** tab, where EMQX has pre-configured a Websocket listener on port `33033`. Click **Next** again to confirm the setting.
3. Then click the **Enable** button to activate the OCPP Gateway.

Upon completing the gateway activation process, you can return to the **Gateways** page and observe that the OCPP Gateway now displays an **Enabled** status.

<img src="./assets/ocpp-enabled.png" alt="OCPP gateway enabled" style="zoom:50%;" />

In EMQX 5.0, OCPP gateways can be configured and enabled through the Dashboard.

The above configuration can also be configured with HTTP API:

**Example Code:**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/ocpp' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "ocpp",
  "enable": true,
  "mountpoint": "ocpp/",
  "listeners": [
    {
      "type": "ws",
      "name": "default",
      "bind": "33033",
      "websocket": {
        "path": "/ocpp"
      }
    }
  ]
}'
```

## Work with OCPP Clients

After establishing the OCPP gateway, you can use the OCPP client tools to test the connections and ensure everything works as expected.

Using [ocpp-go](https://github.com/lorenzodonini/ocpp-go) as an example, we will demonstrate in this section how to access it to the OCPP Gateway.

1. We need to prepare an MQTT client to interact with the OCPP Gateway. Using [MQTTX](https://mqttx.app/downloads) as an example, we will connect it to EMQX and subscribe to the topic `ocpp/#`

![Create MQTT Connection](./assets/ocpp-mqttx-create-conn.png)

2. Run the ocpp-go client and connect to the OCPP Gateway:

Note: You need to replace `<host>` in the following command with the address of the machine where EMQX running.

```shell
docker run -e CLIENT_ID=chargePointSim -e CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp -it --rm --name charge-point ldonini/ocpp1.6-charge- point:latest
```

After a successful connected to EMQX, a log similar to the following is printed:
```
INFO[2023-12-01T03:08:39Z] connecting to server logger=websocket
INFO[2023-12-01T03:08:39Z] connected to server as chargePointSim logger=websocket
INFO[2023-12-01T03:08:39Z] connected to central system at ws://172.31.1.103:33033/ocpp
INFO[2023-12-01T03:08:39Z] dispatched request 1200012677 to server logger=ocppj
```

3. At this point observe that MQTTX receives a message in the following format:
```json
Topic: ocpp/cp/chargePointSim
{
  "UniqueId": "1200012677",
  "Payload": {
    "chargePointVendor": "vendor1", "chargePointModel".
    "chargePointModel": "model1"
  },
  "Action": "BootNotification"
}
```
This message indicates that the ocpp-go client has connected to the OCPP Gateway and sent a request for a `BootNotification`.


4. fill in the topic of the MQTTX `ocpp/cs/chargePointSim` message body with the following and send it.

Note: The `UniqueId` needs to be filled in as the `UniqueId` received in the previous step.

```json
{
  "MessageTypeId": 3,
  "UniqueId": "***",
  "Payload": {
    "currentTime": "2023-12-01T14:20:39+00:00",
    "interval": 300,
    "status": "Accepted"
  }
}
```

5. After that, you can see that MQTTX immediately receives a `StatusNotification` status report. This means that the OCPP client has successfully established a connection with the OCPP Gateway.

```json
Topic: ocpp/cp/chargePointSim
Payload:
{
  "UniqueId": "3062609974",
  "Payload": {
    "status": "Available",
    "errorCode": "NoError",
    "connectorId": 0
  },
  "MessageTypeId": 2,
  "Action": "StatusNotification"
}
```

## Customize Your OCPP Gateway

In addition to the default settings, EMQX provides a variety of configuration options to better accommodate your specific business requirements. This section offers an in-depth overview of the various fields available on the **Gateways** page.

### Basic Configuration

In the **Basic Configuration** tab, you can configure the following fields:

![ocpp-basic-conf](./assets/ocpp-basic-conf.png)

1. **MountPoint**: Set a string that is prefixed to all topics when publishing or subscribing, providing a way to implement message routing isolation between different protocols, for example, *ocpp/*.

2. **Default Heartbeat Interval**: The default Heartbeat time interval, default: `60s`.

3. **Heartbeat Checking Times Backoff**: The backoff for heartbeat checking times, default: `1`.

4. **Message Format Checking**: Whether to enable message format legality checking. EMQX checks the message format of the upload stream and download stream against the format defined in json-schema. When the check fails, emqx will reply with a corresponding answer message. The checking strategy can be one of the following values:
    - `all`: check all messages
    - `upstream_only`: check upload stream messages only
    - `dnstream_only`: check download stream messages only
    - `disable`: don't check any messages

5. **Json Schema Directory**: JSON Schema directory for OCPP message definitions, default: `${application}/priv/schemas`.

6. **Json Schema Id Prefix**: The ID prefix for the OCPP message schemas, default: `urn:OCPP:1.6:2019:12:`.

7. **Idle Timeout**: Set the maximum amount of time in seconds that the gateway will wait for a OCPP frame before closing the connection due to inactivity.

8. **Upstream**:
  - **Topic**: The topic for Upload stream Call Request messages, default: `cp/${cid}`.
  - **Reply Topic**: The topic for Upload stream Reply messages, default: `cp/${cid}/Reply`.
  - **Error Topic**: The topic for Upload stream Error messages, default: `cp/${cid}/Reply`.
  - **Topic Override Mapping**: Upload stream topic override mapping by Action in message payload.

9. **Dnstream**:
    - **Topic**: Download stream topic to receive request/control messages from EMQX. This value is a wildcard topic name that subscribed by every connected Charge Point.
      default: `cs/${cid}`.
    - **max_mqueue_len**: The maximum message queue length for download stream message delivery. default: `100`.

### Add Listeners

One Websocket listener with the name of **default** is already configured on port `33033`, which allows a maximum of 16 acceptors in the pool, and support up to 1,024,000 concurrent connections. You can click **Settings** for more customized settings, click **Delete** to delete the listener, or click **+ Add Listener** to add a new listener.

::: tip

The OCPP gateway only supports Websocket and Websocket over TLS types of listeners.

:::

Click **Add Listener** to open **Add Listener** page, where you can continue with the following configuration fields:

**Basic settings**

- **Name**: Set a unique identifier for the listener.
- **Type**: Select the protocol type, for OCPP, this can be either **ws** or **wss**.
- **Bind**: Set the port number on which the listener accepts incoming connections.
- **MountPoint** (optional): Set a string that is prefixed to all topics when publishing or subscribing, providing a way to implement message routing isolation between different protocols.

**Listener Settings**

- **Path**: Sets the path prefix for the connection address. The client must carry this entire address for the connection, default value `/ocpp`.
- **Acceptor**: Set the size of the acceptor pool, default **16**.
- **Max Connections**: Set the maximum number of concurrent connections that the listener can handle, default: **1024000**.
- **Max Connection Rate**: Set the maximum rate of new connections the listener can accept per second, default: **1000**.
- **Proxy Protocol**: Set to enable protocol V1/2 if EMQX is configured behind the [load balancer](../deploy/cluster/lb.md).
- **Proxy Protocol Timeout**: Set the maximum amount of time in seconds that the gateway will wait for the proxy protocol package before closing the connection due to inactivity, default: **3s**.

**TCP Settings**

- **ActiveN**: Set the `{active, N}` option for the socket, that is, the number of incoming packets the socket can actively process. For details, see [Erlang Documentation -  setopts/2](https://erlang.org/doc/man/inet.html#setopts-2).
- **Buffer**: Set the size of the buffer used to store incoming and outgoing packets, unit: KB.
- **TCP_NODELAY**: Set whether to enable the `TCP_NODELAY` flat for the connection, that is, whether the client needs to wait for the acknowledgment of the previous data before sending additional data; default: **false**, optional values: **true**, **false**.
- **SO_REUSEADDR**: Set whether to allow local reuse of port numbers. <!--not quite sure what this means-->
- **Send Timeout**: Set the maximum amount of time in seconds that the gateway will wait for the proxy protocol package before closing the connection due to inactivity, default: **15s**.
- **Send Timeout**: Set whether to close the connection if the send timeout.

**SSL Settings **(for SSL listeners only)

You can set whether to enable the TLS Verify by setting the toggle switch. But before that, you need to configure the related **TLS Cert**, **TLS Key**, and **CA Cert** information, either by entering the content of the file or uploading with the **Select File** button. For details, see [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md).

Then you can continue to set:

- **SSL Versions**: Set the SSL versions supported, default, **tlsv1.3** **tlsv1.2**, **tlsv1.1**, and **tlsv1**.
- **Fail If No Peer Cert**: Set whether EMQX will reject the connection if the client sends an empty certificate, default: **false**, optional values: **true**, **false**.
- **Intermediate Certificate Depth**: Set the maximum number of non-self-issued intermediate certificates that can be included in a valid certification path following the peer certificate, default, **10**.
- **Key Password**: Set the user's password, used only when the private key is password-protected.

## Configure Authentication

As the concept of username and password is already defined in the connection message of the OCPP protocol, the OCPP supports a variety of authenticator types, such as:

- [Built-in Database Authentication](../access-control/authn/mnesia.md)
- [MySQL Authentication](../access-control/authn/mysql.md)
- [MongoDB Authentication](../access-control/authn/mongodb.md)
- [PostgreSQL Authentication](../access-control/authn/postgresql.md)
- [Redis Authentication](../access-control/authn/redis.md)
- [HTTP Server Authentication](../access-control/authn/http.md)
- [JWT Authentication](../access-control/authn/jwt.md)

OCPP gateway uses the information in the Basic Authentication of the Websocket handshake message to generate the authentication fields for the client:

- Client ID: Valu of the part of the connection address after the fixed path prefix.
- Username: Value of the Username in the Basic Authentication.
- Password: Value of the Password in the Basic Authentication.

You can also use HTTP API to create a built-in database authentication for a OCPP gateway:

**Example Code:**

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/ocpp/authentication' \
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

::: tip

Unlike the MQTT protocol, **the gateway only supports the creation of an authenticator, not a list of authenticators (or an authentication chain)**.

When no authenticator is enabled, all OCPP clients are allowed to log in.

:::
