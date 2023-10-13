# OCPP Protocol Gateway

## Protocol Introduction

[OCPP (Open Charge Point Protocol)](https://www.openchargealliance.org/) is a communication protocol used between Charge Points and a Central System to enable the monitoring and control of EV (Electric Vehicle) charging sessions.

Since e4.4.18, EMQX has implemented the `emqx_ocpp` plugin based on the [OCPP-J 1.6](https://www.openchargealliance.org/protocols/ocpp-16/) standard, providing the ability to connect to Charge Point devices. Its primary function is to handle formatting conversion and forwarding of upstream and downstream messages.

However, it's important to note that while the OCPP Gateway handles formatting conversion and forwarding of upstream/downstream messages, any business-related implementation such as charging initiation and billing must be handled by third-party systems. Additionally, for this version, the OCPP Protocol Gateway functions as a plugin rather than a module.

## Quick Start

All configurations of the OCPP plugin are located in the `plugins/emqx_ocpp.conf` file under the EMQX `etc` directory.

Execute the following command to start the OCPP plugin with the default configuration file:
```bash
emqx_ctl plugins load emqx_ocpp
```

### Connect to OCPP Gateway

After the plugin is successfully loaded, you can use [ocpp-go](https://github.com/lorenzodonini/ocpp-go) to simulate a Charge Point for connection testing.

Please follow the instructions provided in the ocpp-go documentation to complete the compilation
first, and then execute the following command to connect to EMQX's OCPP gateway.

Note: Please replace`<host>` with the address of your EMQX node.
```bash
CLIENT_ID=chargePointSim CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp go run example/1.6/cp/*.go
```

## Client and Connection

EMQX OCPP gateway only supports client connections via WebSocket, as defined in the OCPP-J 1.6 protocol.

Once the connection is established, EMQX treats the OCPP client as a standard client, which means that you can manage this client through Client ID on Dashboard/HTTP-API/CLI, where, the Client ID is the unique identifier of the Charge Point.

### Authentication

EMQX OCPP gateway extracts the user name and password from the HTTP Basic authentication used by Charge Points (as specified in OCPP-J 1.6), and uses EMQX's authentication system to manage the client and determine access permission.

You can refer to [Authentication](../advanced/auth.md) for detailed configuration, and you can get the Username and Password field from HTTP Basic authentication provided by the Charge Point during connection.

## Messaging Flow

The flowchart represents the communication between a Charge Point and a third-party service via EMQX OCPP gateway.

```
                                +--------------+  upstream publish    +--------------+
+--------------+   Req/Resp     | OCPP Gateway | -------------------> | 3rd-party    |
| Charge Point | <------------> | over         |     over Topic       | Service      |
+--------------+   over ws/wss  | EMQX         | <------------------- |              |
                                +--------------+  downstream publish  +--------------+
```

As shown in the figure:
- The Charge Point communicates with EMQX OCPP Gateway using WebSockets or secure WebSockets (ws/wss) protocol.
- The OCPP gateway converts Charge Point messages into a standard MQTT Publish message. The topic and message connect are defined by the OCPP gateway.
- Third-party systems receive messages from Charge Points by subscribing to upstream topics;
  they also push control messages to Charge Points by sending messages to downstream topics.

### Messaging from OCPP Gateway to Third-Party Services (Upstream)

The OCPP gateway facilitates the transmission of messages and events originating from the Charge Point, utilizing EMQX. This data flow, known as the **Upstream** process, ensures seamless communication between the charging infrastructure and the management system.

For instance, by setting a default upstream topic using the following configuration, the EMQX OCPP gateway will publish all Charge Point messages to this topic. During the messaging process, the placeholders `${cid}` (Charge Point ID) and `${action}` (Message Name) will be replaced with their respective actual values.

```hcl
## plugins/emqx_ocpp.conf
## OCPP gateway will publish all Charge Point messages to this topic.
ocpp.upstream.topic = ocpp/cp/${cid}/${action}

## Supports overriding the default topic by message name
ocpp.upstream.topic.BootNotification = ocpp/cp/${cid}/Notify/${action}
```

The message content (Payload) is a JSON string with a fixed pattern, which includes the following fields:

| Field              | Type        | Required | Description |
| ------------------ | ----------- | -------- | ---- |
| `MessageTypeId`    | MessageType | Y       | Define the type of Message, optional values: <br><br/>`2` for Call<br>`3` for CallRequest<br>`4` for CallError |
| `UniqueId`         | String      | Y       | Must be the same ID  as carried in the `call` request so the receiver can match the request and result |
| `Action`           | String      | N       | Name of OCPP message, for example, authorize |
| `ErrorCode`        | ErrorType   | N       | Optional, but required for `CallError` messages |
| `ErrorDescription` | String      | N       | Error details |
| `Payload`          | Object      | N       | Payload field with the serialized data of the OCPP message in protobuf format |

For example, the message format of `BootNotification` on upstream is
```json
Topic: ocpp/cp/CP001/Notify/BootNotifiaction
Payload:
  {"MessageTypeId": 2,
   "UniqueId": "1",
   "Payload": {"chargePointVendor":"vendor1","chargePointModel":"model1"}
  }
```

Similarly, for response messages and error notifications sent by Charge Point to third-party services,
their topic formats can also be customized as follows:

```hcl
## plugins/emqx_ocpp.conf

ocpp.upstream.reply_topic = ocpp/cp/Reply/${cid}
ocpp.upstream.error_topic = ocpp/cp/Error/${cid}
```

### Messaging from Third-Party Services to OCPP Gateway (Downstream)

Third-party services can issue control messages for the Charge Point by utilizing the topics configured in the OCPP gateway. This **Downstream** data flow facilitates seamless interaction and improved management of charging stations through external service integration.

You can define a downstream topic for receiving the control messages. The OCPP gateway auto-subscribes to this topic for each connected Charge Point. During the messaging process, the placeholders `${cid}` (Charge Point ID) will be replaced with the actual values.

```
## plugins/emqx_ocpp.conf
## Topic to receive downstream control commands.

ocpp.dnstream.topic = ocpp/cs/${cid}/+/+
```

Note: The use of the wildcard `+` in this example provides flexibility in the topic structure. However, it's not necessary and can be adjusted according to your requirements.

The payload of a downstream message is a JSON string with a fixed pattern, similar to upstream messaging, for example, below is a downstream send to Charge Point CP001.

```
Topic: ocpp/cs/CP001/Reply/BootNotification
Payload:
  {"MessageTypeId": 3,
   "UniqueId": "1",
   "Payload": {"currentTime": "2022-06-21T14:20:39+00:00", "interval": 300, "status": "Accepted"}
  }
```
