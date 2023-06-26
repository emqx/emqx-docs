# ExProto Gateway

## Introduction

<!--**Add an introductory section**: Begin the page briefly introducing the relevant protocol and the Gateway. This will provide context for users who are new to the concept.-->

The ExProto (Extension Protocol) is a custom protocol parsing gateway implemented based on gRPC
communication. It allows users to write gRPC services in their familiar programming languages,
such as Java, Python, Go, etc., to parse the network protocols of devices and complete functions
such as device connecting, authentication, and message transmission.

This guide will teach you how to configure and use the ExProto Gateway in EMQX.

<!--a brief introduction of the architecture-->

## Enable the ExProto Gateway

In EMQX 5.0, ExProto gateway can be configured and enabled through the Dashboard, HTTP API, and
configuration file `emqx.conf`.
This section takes the configuration via Dashboard as an example to illustrate the operating steps.

On EMQX Dashboard, click **Management** -> **Gateways** on the left navigation menu.
On the **Gateways** page, all supported gateways are listed. Locate **ExProto** and click **Setup**
in the **Actions** column. Then, you will be directed to the **Initialize ExProto** page.

::: tip

If you are running EMQX in a cluster, the settings you made through the Dashboard or HTTP API will
affect the whole cluster. If you only want to change the settings with one node,
please configure with [`emqx.conf`](../configuration/configuration.md)

:::

To simplify the configuration process, EMQX offers default values for all required fields on
the **Gateways** page. If you don't need extensive customization, you can enable the ExProto
Gateway in just 3 clicks:

1. Click **Next** in the **Basic Configuration** tab to accept all the default settings.
2. Then you will be directed to the **Listeners** tab, where EMQX has pre-configured a TCP listener
   on port 7993. Click **Next** again to confirm the setting.
3. Then click the **Enable** button to activate the ExProto Gateway.

Upon completing the gateway activation process, you can return to the **Gateways** page and
observe that the ExProto Gateway now displays an **Enabled** status.

<img src="./assets/exproto-enabled.png" alt="Enabled ExProto gateway" style="zoom:50%;" />

The above configuration can also be configured with HTTP API:

**Example Code:**

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/exproto' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "exproto",
  "enable": true,
  "mountpoint": "exproto/",
  "server": {
    "bind": "0.0.0.0:9100"
  }
  "handler": {
    "address": "http://127.0.0.1:9001"
    "ssl_options": {"enable": false}
  }
  "listeners": [
    {
      "type": "tcp",
      "bind": "7993",
      "name": "default",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

For a detailed HTTP API description, see [HTTP API - Gateway](../admin/api.md)

If you have some customization needs, want to add more listeners, or add authentication rules,
you can continue to read the [Customize Your ExProto Gateway section](#customize-your-exproto-gateway).

### Start the example gRPC service to test it

In this section, we can try to start a example gRPC service to learn how ExProto Gateway and the
gRPC service works together.

We provide various language example programs in
[emqx-extension-examples](https://github.com/emqx/emqx-extension-examples).In this repository,
`exproto-svr-python` is an Echo program that implements the ConnectionUnaryHandler of ExProto
Gateway using Python. It simply sends back any data received from a TCP client.

Let's take it as an example:

1. Make sure you are running EMQX v5.1.0 or above and start the ExProto Gateway with above configurations.

2. On the same machine as EMQX, clone the sample code and enter the directory of exproto-svr-python:
```
git clone https://github.com/emqx/emqx-extension-examples
cd exproto-svr-python
```

3. Make sure you have installed Python 3.7 or above, and install the following dependencies:
```
python -m pip install grpcio
python -m pip install grpcio-tools
```

4. Start the gRPC Server by the following command:
```
python exproto_server.py
```

5. Once successfully started, it will print output similar to the following:
```
ConnectionUnaryHandler started successfully, listening on 9001

Tips: If the Listener of EMQX ExProto gateway listen on 7993:
      You can use the telnet to test the server, for example:

      telnet 127.0.0.1 7993

Waiting for client connections...
```

6. Use `telenet` to access the 7993 port that Exproto Gateway is listening on, and enter
   `Hi, this is tcp client!` to test if the gRPC Server is working properly. For example:

```
$ telnet 127.0.0.1 7993
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
Hi, this is tcp client!
Hi, this is tcp client!
```

7. A simple test has already been completed. You can also open the EMQX Dashboard and enter
   the management page of the ExProto gateway to see the client you connected to using telnet. For example:

<img src="./assets/connected-exproto-client.png" alt="Connected ExProto Client" style="zoom:50%;" />

In this test, we can simply summarize the roles of these three programs involved in the interaction:

- **Telnet**
    - simulate a tcp client to send or receive messages.
    - in actual environment, it may be a device that implements custom private protocols. It is
      connected to the listener port (7993) of ExProto Gateway and sends messages to EMQX or
      receives messages from EMQX through this connection.

- **ExProto Gateway**
    - receives device connections by listening 7993 and transparently passes the byte data of the
      devices to the gRPC service that implements ConnectionUnaryHandler defined in
      exproto.proto (in this case, it's exproto-svr-python).
    - listens on port 9100 and provides a ConnectionAdapter service defined in exproto.proto.
      This service provides function for exproto-svr-python to initiate subscriptions, publish
      messages, start timers and close connections functions for managing connections.

- **exproto-svr-python**
    - provides a gRPC service for ConnectionUnaryHandler defined in exproto.proto on port 9001.
    - in the actual environment, it performs the serialization and deserialization of client bytes,
      publishes these upstream messages to EMQX, or subscribes to topics to receive messages from
      EMQX and delivers them to client connections.


## The `exproto.proto` File

`exproto.proto` defines the interface between ExProto Gateway and user gRPC service.
It mainly the `ConnectionAdapter` service that needs to be implemented by ExProto Gateway,
and the `ConnectionUnaryHandler` service that needs to be implemented by user's gRPC service.

### ConnectionUnaryHandler

The ConnectionUnaryHandler service is defined in `exproto.proto`. The user's gRPC Server should
implement this service to handle the connection of client sockets and byte parsing.

This service includes the following methods:

| Method             | Description  |
| -------------------| --------------------------------------------------------------------------------------- |
| OnSocketCreated    | Whenever a new Socket connects to the ExProto Gateway, this callback will be triggered. |
| OnSocketClosed     | Whenever a Socket is closed, this callback will be triggered. |
| OnReceivedBytes    | Whenever data is received from the client's socket, this callback will be triggered. |
| OnTimerTimeout     | Whenever a timer times out, this callback will be triggered. |
| OnReceivedMessages | Whenever a message is received for the subscribed topic, this callback will be triggered. |

When ExProto Gateway calls these methods, it will pass a unique identifier `conn` in the
parameters to mark which socket sent out this event.

For example, in the `OnSocketCreated` function parameters:

```
message SocketCreatedRequest {
  string conn = 1;
  ConnInfo conninfo = 2;
}

```

::: tip

Since ExProto Gateway cannot recognize the start and end of private protocol message frames,
if there is TCP packet sticking or splitting, it needs to be handled in the OnReceivedBytes callback.

:::


### ConnectionAdapter

The `ConnectionAdapter` defined in `exproto.proto` is implemented by the ExProto Gateway to provide
interfaces to the gRPC Server:

| Method       | Description  |
| ------------ | ------------ |
| Send         | Used to send bytes to the specified connection |
| Close        | Close the specified connection |
| Authenticate | Register a client with ExProto Gateway and complete the authentication |
| StartTimer   | To start a timer for the specified connection, typically used for liveness detecting |
| Publish      | Publish messages to emqx from the specified connection |
| Subscribe    | Create a subscription for the specified connection |
| Unsubscribe  | Delete a subscription for the specified connection |
| RawPublish   | Publish a message to EMQX |


## Customize Your ExProto Gateway

In addition to the default settings, EMQX provides a variety of configuration options to better
accommodate your specific business requirements. This section offers an in-depth overview of the
various fields available on the **Gateways** page.

### Basic Configuration

In the **Basic Configuration** tab, you can customize your ConnectionUnaryHandler service address,
ConnectionAdapter listening port, and set the MountPoint string for this gateway. See the texts
below the screenshot for a comprehensive explanation of each field.

![Basic Configuration](./assets/exproto-basic-config.png)

- **Idle Timeout**: Set the duration (in seconds) of inactivity after which a connected client will be considered disconnected. Default: **30 s**.

- **Enable Statistics**: Set whether to allow the Gateway to collect and report statistics; default: **true**, optional values: **true**, **false**.

- **MountPoint**: Set a string that is prefixed to all topics when publishing or subscribing, providing a way to implement message routing isolation between different protocols, for example, `mqttsn/`.

  **Note**: This topic prefix is managed by the gateway. clients do not need to add this prefix explicitly when publishing and subscribing.

- **gRPC ConnectionAdapter**: ExProto gateway provides the ConnectionAdapter service configuration.
    - **Bind**: Listening Address. Default: **0.0.0.0:9100**.

- **gRPC ConnectionHandler**: The callback server configuration that implemented ConnectionUnaryHandler
    - **Server**: The callback server address.


### Add Listeners

By default, one TCP listener with the name of **default** is already configured on port `7993`, which allows a maximum of 1,000 connections per second, and support up to 1,024,000 concurrent connections. You can click **Settings** for more customized settings for **Delete** to delete the listener. Or click **Add Listener** to add a new listener.

<img src="./assets/exproto-listerner.png" alt="ExProto listener" style="zoom:50%;" />

Click **Add Listener** to open **Add Listener** page, where you can continue with the following configuration fields:

**Basic settings**

- **Name**: Set a unique identifier for the listener.
- **Type**: Select the protocol type, for MQTT-SN, this can be either **udp** or **dtls**.
- **Bind**: Set the port number on which the listener accepts incoming connections.
- **MountPoint** (optional): Set a string that is prefixed to all topics when publishing or subscribing, providing a way to implement message routing isolation between different protocols

**Listener Settings**

- **Acceptor**: Set the size of the acceptor pool, default: **16**.
- **Max Connections**: Set the maximum number of concurrent connections that the listener can handle, default: **1024000**.
- **Max Connection Rate**: Set the maximum rate of new connections the listener can accept per second, default: **1000**.

**TCP Settings**

- **ActiveN**: Set the `{active, N}` option for the socket, that is, the number of incoming packets the socket can actively process. For details, see [Erlang Documentation -  setopts/2](https://erlang.org/doc/man/inet.html#setopts-2).
- **Buffer**: Set the size of the buffer used to store incoming and outgoing packets, unit: KB.
- **Receive Buffer**: Set the size of the receive buffer,  unit: KB.
- **Send Buffer**: Set the size of the send buffer,  unit: KB.
- **SO_REUSEADDR**: Set whether to allow local reuse of port numbers. <!--not quite sure what this means-->

**TLS Settings** (for ssl listeners only)

You can set whether to enable the TLS Verify by setting the toggle switch. But before that, you need to configure the related **TLS Cert**, **TLS Key**, and **CA Cert** information, either by entering the content of the file or uploading with the **Select File** button. For details, see [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md).

Then you can continue to set:

- **SSL Versions**: Set the DTLS versions supported, default, **dtlsv1.2** and **dtlsv1**.
- **SSL Fail If No Peer Cert**: Set whether EMQX will reject the connection if the client sends an empty certificate, default: **false**, optional values: **true**, **false**.
- **CACert Depth**: Set the maximum number of non-self-issued intermediate certificates that can be included in a valid certification path following the peer certificate, default, **10**.
- **Key File Passphrase**: Set the user's password, used only when the private key is password-protected.


### Configure Authentication

ExProto Gateway supports various types of authenticators, such as:

- [Built-in Database Authentication](../access-control/authn/mnesia.md)
- [MySQL Authentication](../access-control/authn/mysql.md)
- [MongoDB Authentication](../access-control/authn/mongodb.md)
- [PostgreSQL Authentication](../access-control/authn/postgresql.md)
- [Redis Authentication](../access-control/authn/redis.md)
- [HTTP Server Authentication](../access-control/authn/http.md)
- [JWT Authentication](../access-control/authn/jwt.md)

The Client ID, Username, and Password of the client information are all derived from the parameters
passed in the Authenticate method of the ConnectionAdapter.


This part takes the Dashboard as an example to illustrate how to do the authentication configuration.

On the **Gateways** page, locate **ExProto** and click **Setup** in the **Actions** column and click **Authentication** to enter the **Authentication** tab.

Click **Create Authentication**, choose **Password-Based** as the **Mechanism**, and select **HTTP Server** as the **Backend**. Then in the **Configuration** tab, you can set the authentication rules.

![mqttsn authentication](./assets/exproto-authn-config.png)

For a detailed explanation of each field on the page, you can refer to [HTTP Server Authentication](../access-control/authn/http.md).

The above configuration can also be performed via HTTP API.

**Example Code**

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/exproto/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "method": "post",
  "url": "http://127.0.0.1:8080",
  "headers": {
    "content-type": "application/json"
  },
  "body": {
    "username": "${username}",
    "password": "${password}"
  },
  "pool_size": 8,
  "connect_timeout": "5s",
  "request_timeout": "5s",
  "enable_pipelining": 100,
  "ssl": {
    "enable": false,
    "verify": "verify_none"
  },
  "backend": "http",
  "mechanism": "password_based",
  "enable": true
}'
```
