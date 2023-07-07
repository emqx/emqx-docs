# ExProto 协议网关

Extension Protocol (ExProto) 协议是一个基于 gRPC 通信实现的自定义协议解析网关。它允许用户使用其熟悉的编程语言（如 Java、Python、Go 等）开发 gRPC 服务，用于解析设备的网络协议，并完成设备连接、身份验证和消息传输等功能。

本也介绍了如何在 EMQX 中配置和使用 ExProto 网关。

<!--a brief introduction of the architecture-->

## ExProto 网关和 gPRC 服务的工作原理

当 ExProto 网关在 EMQX 中启用 ，它将在特定端口（例如7993）上监听设备连接。当它接收到客户端设备的连接时，它将传递客户端设备生成的字节数据和事件给用户的 gRPC 服务。这需要 ExProto网 关中的 gRPC 客户端调用用户 gRPC 服务器中使用的`ConnectionUnaryHandler` 服务中的方法。

用户的 gRPC 服务器中的 gRPC 服务接收来自 ExProto 网关的字节数据和事件，解析客户端的网络协议，并将字节数据和事件转换为Pub/Sub 请求，并将它们发送回  ExProto网关。ExProto网关中的 `ConnectionAdapter` 服务提供了与用户的 gRPC 服务器进行交互的接口。最后通过 ExProto 网关，客户端设备可以向 EMQX 发布消息、订阅主题和管理客户端连接。

下图展示了 ExProto 网关和 gRPC 服务工作的架构。

<img src="./assets/exproto-gateway-architecture.png" alt="exproto-gateway-architecture" style="zoom:50%;" />

### `exproto.proto` 文件

`exproto.proto` 文件定义了 ExProto 网关和用户 gRPC  服务之间的接口。该文件指定了以下两个服务：

- `ConnectionAdapter` 服务：由 ExProto 网关实现，为 gPRC 服务器提供接口。
- `ConnectionUnaryHandler` 服务：由用户的 gRPC 服务器实现，用于定义处理客户端 socket 连接和字节解析的方法。

### `ConnectionUnaryHandler` 服务

`ConnectionUnaryHandler` 服务由用户的 gRPC 服务器实现，用于处理客户端 socket 连接和字节解析。

该服务包括以下方法：

| 方法               | 描述                                                    |
| ------------------ | ------------------------------------------------------- |
| OnSocketCreated    | 每当有新的 socket 连接到 ExProto 网关时，将触发此回调。 |
| OnSocketClosed     | 每当 socket 关闭时，将触发此回调。                      |
| OnReceivedBytes    | 每当从客户端 socket 接收到数据时，将触发此回调。        |
| OnTimerTimeout     | 每当定时器超时时，将触发此回调。                        |
| OnReceivedMessages | 每当接收到订阅主题的消息时，将触发此回调。              |

当  ExProto 网关调用这些方法时，它将在参数中传递一个唯一标识符 `conn`，以标记是哪个套接字发送了该事件。例如，在 `OnSocketCreated` 函数的参数中：

```
javaCopy code
message SocketCreatedRequest {
  string conn = 1;
  ConnInfo conninfo = 2;
}
```

::: tip

由于 ExProto 网关无法识别私有协议消息帧的起始和结束位置，如果存在 TCP 数据包粘连或分割，需要在 `OnReceivedBytes` 回调中处理。

:::

### `ConnectionAdapter` 服务

`ConnectionAdapter` 服务由 ExProto 网关实现，为 gRPC 服务提供订阅初始化、消息发布、定时器启动和关闭连接等管理连接的功能。它包括以下方法：

| 方法         | 描述                                             |
| ------------ | ------------------------------------------------ |
| Send         | 向指定连接发送字节。                             |
| Close        | 关闭指定连接。                                   |
| Authenticate | 在 ExProto 网关中注册客户端并完成身份验证。      |
| StartTimer   | 为指定连接启动定时器，通常用于检测连接是否存活。 |
| Publish      | 从指定连接向 EMQX 发布消息。                     |
| Subscribe    | 为指定连接创建订阅。                             |
| Unsubscribe  | 删除指定连接的订阅。                             |
| RawPublish   | 发布消息到 EMQX。                                |

## 启用 ExProto 网关

从 EMQX 5.0 开始，您可以通过 Dashboard，HTTP API 或配置文件 `emqx.conf` 启用和配置 ExProto 网关。本节演示了如何通过 Dashboard 启用 ExProto 网关。

在 EMQX Dashboard 左侧导航目录中点击**管理** -> **网关**。**网关**页面上列出了所有支持的网关。找到 **ExProto** 并点击**操作**列中的**配置**按钮，您将进入**初始化 ExProto** 页面。

::: tip

如果 EMQX 在集群中运行，通过 Dashboard 或 HTTP API 的配置将在整个集群中生效。如果您只希望针对单个节点进行设置，可以使用配置文件 `emqx.conf` 配置网关。

:::

为了简化配置过程，EMQX 为**网关**页面上所有必填的字段提供了默认值。如果不需要自定义配置，您只需以下 3 步就可启用 ExProto 网关:

1. Click **Next** in the **Basic Configuration** tab to accept all the default settings.
2. Then you will be directed to the **Listeners** tab, where EMQX has pre-configured a TCP listener on port 7993. Click **Next** again to confirm the setting.
3. Click the **Enable** button to activate the ExProto Gateway.

Upon completing the gateway activation process, you can return to the **Gateways** page and observe that the ExProto Gateway now displays an **Enabled** status.

<img src="/Users/emqx/Documents/GitHub/emqx-docs/en_US/gateway/assets/exproto-enabled.png" alt="Enabled ExProto gateway" style="zoom:50%;" />

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

For a detailed HTTP API description, see [HTTP API - Gateway](../admin/api.md).

If you have some customization needs, want to add more listeners, or add authentication rules, you can continue to read the [Customize Your ExProto Gateway](#customize-your-exproto-gateway).

## Start gRPC Service Example for Testing

In this section, you can start a gRPC service example to learn how ExProto Gateway and the gRPC service work together.

You can find example programs for various languages in [emqx-extension-examples](https://github.com/emqx/emqx-extension-examples). In this repository, `exproto-svr-python` is an Echo program that implements the ConnectionUnaryHandler of ExProto Gateway using Python. It simply sends back any data received from a TCP client. The following steps take `exproto-svr-python` as an example:

::: tip Prerequisites:

Before you start, make sure you have completed the following:

- Run EMQX 5.1.0 or above and enable the ExProto Gateway with default configurations.

- Install Python 3.7 or above, and install the following dependencies:

  ```
  python -m pip install grpcio
  python -m pip install grpcio-tools
  ```

:::

1. On the same machine with EMQX running, clone the sample code and enter the directory `exproto-svr-python`:

   ```bash
   git clone https://github.com/emqx/emqx-extension-examples
   cd exproto-svr-python
   ```

2. Start the gRPC Server using the following command:

   ```
   python exproto_server.py
   ```

   After a successful start, an output similar to the following will be printed:

   ```
   ConnectionUnaryHandler started successfully, listening on 9001
   
   Tips: If the Listener of EMQX ExProto gateway listen on 7993:
         You can use the telnet to test the server, for example:
   
         telnet 127.0.0.1 7993
   
   Waiting for client connections...
   ```

3. Use `telenet` to access the 7993 port that Exproto Gateway is listening on, and enter `Hi, this is tcp client!` to test if the gRPC Server is working properly. For example:

   ```
   $ telnet 127.0.0.1 7993
   Trying 127.0.0.1...
   Connected to 127.0.0.1.
   Escape character is '^]'.
   Hi, this is tcp client!
   Hi, this is tcp client!
   ```

4. Go to EMQX Dashboard and click **Management** -> **Gateways** from the left navigation menu. Click **Clients** of ExProto. On the ExProto page, you can see the client you connect to using telnet. 

   <img src="/Users/emqx/Documents/GitHub/emqx-docs/en_US/gateway/assets/connected-exproto-client.png" alt="Connected ExProto Client" style="zoom:50%;" />




## Customize Your ExProto Gateway

In addition to the default settings, EMQX provides a variety of configuration options to better accommodate your specific business requirements. This section offers an in-depth overview of the configuration options available on the **Gateways** page.

### Basic Configuration

In the **Basic Configuration** tab, you can customize your ConnectionUnaryHandler service address, ConnectionAdapter listening port, and set the MountPoint string for this gateway. 

![Basic Configuration](/Users/emqx/Documents/GitHub/emqx-docs/en_US/gateway/assets/exproto-basic-config.png)

- **Idle Timeout**: Set the duration (in seconds) of inactivity after which a connected client will be considered disconnected. Default: `30 s`.

- **Enable Statistics**: Set whether to allow the Gateway to collect and report statistics; default: `true`, optional values: `true`, `false`.

- **MountPoint**: Set a string that is prefixed to all topics when publishing or subscribing, providing a way to implement message routing isolation between different protocols, for example, `mqttsn/`.

  **Note**: This topic prefix is managed by the gateway. Clients do not need to add this prefix explicitly when publishing and subscribing.

- **gRPC ConnectionAdapter**: ExProto gateway provides the ConnectionAdapter service configuration.

  - **Bind**: Listening address. Default: **0.0.0.0:9100**.

- **gRPC ConnectionHandler**: The callback server configuration that implemented ConnectionUnaryHandler.

  - **Server**: The callback server address.


### Add Listeners

By default, one TCP listener with the name of **default** is already configured on port `7993`, which allows a maximum of 1,000 connections per second, and support up to 1,024,000 concurrent connections. You can click **Settings** for more customized settings, click **Delete** to delete the listener, or click **Add Listener** to add a new listener.

<img src="/Users/emqx/Documents/GitHub/emqx-docs/en_US/gateway/assets/exproto-listener.png" alt="exproto-listener" style="zoom:50%;" />

Click **Add Listener** to open **Add Listener** page, where you can continue with the following configuration fields:

**Basic settings**

- **Name**: Set a unique identifier for the listener.
- **Type**: Select the protocol type, for MQTT-SN, this can be either `udp` or `dtls`.
- **Bind**: Set the port number on which the listener accepts incoming connections.
- **MountPoint** (optional): Set a string that is prefixed to all topics when publishing or subscribing, providing a way to implement message routing isolation between different protocols.

**Listener Settings**

- **Acceptor**: Set the size of the acceptor pool, default: `16`.
- **Max Connections**: Set the maximum number of concurrent connections that the listener can handle, default: `1024000`.
- **Max Connection Rate**: Set the maximum rate of new connections the listener can accept per second, default: `1000`.

**TCP Settings**

- **ActiveN**: Set the `{active, N}` option for the socket, that is, the number of incoming packets the socket can actively process. For details, see [Erlang Documentation -  setopts/2](https://erlang.org/doc/man/inet.html#setopts-2).
- **Buffer**: Set the size of the buffer used to store incoming and outgoing packets, unit: KB.
- **Receive Buffer**: Set the size of the receive buffer, unit: KB.
- **Send Buffer**: Set the size of the send buffer, unit: KB.
- **SO_REUSEADDR**: Set whether to allow local reuse of port numbers. <!--not quite sure what this means-->

**TLS Settings** (for ssl listeners only)

You can set whether to enable the TLS Verify by setting the toggle switch. But before that, you need to configure the related **TLS Cert**, **TLS Key**, and **CA Cert** information, either by entering the content of the file or uploading with the **Select File** button. For details, see [Enable SSL/TLS Connection](../network/emqx-mqtt-tls.md).

Then you can continue to set:

- **SSL Versions**: Set the DTLS versions supported, default: `dtlsv1.2` and `dtlsv1`.
- **SSL Fail If No Peer Cert**: Set whether EMQX will reject the connection if the client sends an empty certificate, default: `false`, optional values: `true`, `false`.
- **CACert Depth**: Set the maximum number of non-self-issued intermediate certificates that can be included in a valid certification path following the peer certificate, default, `10`.
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

The Client ID, Username, and Password of the client information are all derived from the parameters passed in the Authenticate method of the ConnectionAdapter.


This part takes the Dashboard as an example to illustrate how to do the authentication configuration.

On the **Gateways** page, locate **ExProto** and click **Settings** in the **Actions** column. On the Exproto page, click the **Authentication** tab.

Click **+ Create Authentication**, choose **Password-Based** as the **Mechanism**, and select **HTTP Server** as the **Backend**. Then in the **Configuration** tab, you can set the authentication rules.

![mqttsn authentication](/Users/emqx/Documents/GitHub/emqx-docs/en_US/gateway/assets/exproto-authn-config.png)

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

