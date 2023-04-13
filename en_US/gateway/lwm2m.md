# LwM2M Gateway

## Introduction

[LwM2M (Lightweight Machine-to-Machine)](https://lwm2m.openmobilealliance.org/)
is a protocol designed for IoT devices and machine-to-machine communication.
It is a lightweight protocol that supports devices with limited processing power and memory.

The **LwM2M Gateway** in EMQX can accept LwM2M clients and translate theirevents
and messages into MQTT Publish messages.

In the current implementation, it has the following limitations:
- Based UDP/DTLS transport.
- Only supports v1.0.2. The v1.1.x and v1.2.x is not supported yet.
- Not included LwM2M Bootstrap services.


## Quick Start

In EMQX 5.0, LwM2M gateways can be configured and enabled through the Dashboard.

It can also be enabled via the HTTP API, and emqx.conf e.g:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/lwm2m' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "lwm2m"
  "xml_dir": "etc/lwm2m_xml/",
  "qmode_time_window": "22s",
  "lifetime_min": "1s",
  "lifetime_max": "86400s",
  "auto_observe": true,
  "enable_stats": true,
  "update_msg_publish_condition": "contains_object_list",
  "mountpoint": "lwm2m/${endpoint_name}/",
  "translators": {
    "command": {"topic": "dn/#", "qos": 0},
    "response": {"topic": "up/resp", "qos": 0},
    "notify": {"topic": "up/notify", "qos": 0},
    "register": {"topic": "up/resp", "qos": 0},
    "update": {"topic": "up/update", "qos": 0}
  },
  "listeners": [
    {
      "type": "udp",
      "name": "default",
      "bind": "5783",
      "max_conn_rate": 1000,
      "max_connections": 1024000,
    }
  ],
}'
```
:::

::: tab Configuration

```properties
gateway.lwm2m {
  xml_dir = "etc/lwm2m_xml/"
  auto_observe = true
  enable_stats = true
  idle_timeout = "30s"
  lifetime_max = "86400s"
  lifetime_min = "1s"
  mountpoint = "lwm2m/${endpoint_namea}/"
  qmode_time_window = "22s"
  update_msg_publish_condition = "contains_object_list"
  translators {
    command {qos = 0, topic = "dn/#"}
    notify {qos = 0, topic = "up/notify"}
    register {qos = 0, topic = "up/resp"}
    response {qos = 0, topic = "up/resp"}
    update {qos = 0, topic = "up/update"}
  }
  listeners {
    udp {
      default {
        bind = "5783"
        max_conn_rate = 1000
        max_connections = 1024000
      }
    }
  }
}
```
:::

::::


::: tip
Configuring the gateway via emqx.conf requires changes on a per-node basis, but configuring it via Dashboard or the HTTP API will take effect across the cluster.
:::

The LwM2M gateway only supports UDP and DTLS type listeners, for a complete list of configurable parameters refer to: [Gateway Configuration - Listeners](../configuration/configuration-manual.md)


## Authentication

Since the LwM2M protocol only given the Endpoint Name of Client, there is no Username and Password.
Therefore, the LwM2M gateway only supports [HTTP Server Authentication](../access-control/authn/http.md).

For example, to create an HTTP authentication for LwM2M gateway via HTTP API or emqx.conf:

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/lwm2m/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "method": "post",
  "url": "http://127.0.0.1:8080",
  "headers": {
    "content-type": "application/json"
  },
  "body": {
    "clientid": "${clientid}"
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
:::

::: tab Configuration

```properties
gateway.lwm2m {
  authentication {
    enable = true
    backend = "http"
    mechanism = "password_based"
    method = "post"
    connect_timeout = "5s"
    enable_pipelining = 100
    url = "http://127.0.0.1:8080"
    headers {
      "content-type" = "application/json"
    }
    body {
      clientid = "${clientid}"
    }
    pool_size = 8
    request_timeout = "5s"
    ssl.enable = false
  }
}
```
:::

::::


## Message Format

The Messaging model of LwM2M protocol is based on [Resources Model and Operations](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html). This is completely different
from the Publish/Subscribe model of the MQTT protocol. So, in the LwM2M Gateway,
we need to make a Message Format to compatible these message models.

### Client Registration Interface

#### Register

the **Register** message is sent by the LwM2M client to the LwM2M server to
register itself with the server. It contains information about the client and
its capabilities like endpoint name, lifetime, LwM2M version, objects, object 
instances, etc.

The Register message is the first message sent by the client to initiate
communication with the server.

The **Register** message will be converted to following MQTT message by LwM2M
Gateway.

The **Topic** format is:
```
{?mountpoint}{?translators.register.topic}
```

Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.register.topic}` is the value of `translators.register.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.register.topic` is `up/register`, then the topic for the
response message would be `lwm2m/<your-real-client-endpoint-name>/up/register`.


The **Payload** format is:
```json
{
  "msgType": "register",
  "data": {
    "ep": {?EndpointName},
    "lwm2m": {?Version},
    "lt": {?LifetTime},
    "b": {?Binding},
    "objectList": {?ObjectList}
  }
}
```

Variables:
- `{?EndpointName}`: String, the endpoint name of LwM2M client.
- `{?Version}`: String, the protocol version of LwM2M client.
- `{?LifeTime}`: Number, the life time of LwM2M client requested.
- `{?Binding}`: Enum, this parameter specifies the type of binding that the
  client supports for communication with the server. It can be:
  * `"U"`: UDP
  * `"UQ"`: UDP with data queuing
- `{?ObjectList}`: Array, the list of Objects supported and Object Instances
  available on the LwM2M Client.

For example, a full MQTT Payload for Register message can be:
```json
{
  "msgType": "register",
  "data": {
    "objectList": ["/1/0", "/2/0", "/3/0", "/4/0", "/5/0", "/6/0", "/7/0"],
    "lwm2m": "1.0",
    "lt": 300,
    "ep": "testlwm2mclient",
    "b": "U"
  }
}
```

#### Update

The **Update** message is sent by the LwM2M client to the LwM2M server to update
the registration information. It is similar to the Register message but it is
sent after an initial registration has taken place.
The Update message contains information about any changes to the client's
capabilities or status, such as a change in IP address or an update to the data
modeled by LwM2M objects.
The Update message extends the client's registration period, so it is a way
for the client to let the server know it is still available and active.

The frequency of the Update message is determined by the lifetime value
specified in the Register message.

The **Update** message will be converted to following MQTT message by LwM2M Gateway.

The **Topic** format is:
```
{?mountpoint}{?translators.update.topic}
```
Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.update.topic}` is the value of `translators.update.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.update.topic` is `up/update`, then the topic for the
message would be `lwm2m/<your-real-client-endpoint-name>/up/update`.


The **Payload** format is:
```json
{
  "msgType": "update",
  "data": {
    "ep": {?EndpointName},
    "lwm2m": {?Version},
    "lt": {?LifetTime},
    "b": {?Binding},
    "objectList": {?ObjectList}
  }
}
```

Variables that are the same as the Register message.

For example, a full MQTT Payload for Update message can be:
```json
{
  "msgType": "update",
  "data": {
    "objectList": ["/7/0"],
    "lwm2m": "1.0",
    "lt": 300,
    "ep": "testlwm2mclient",
    "b": "U"
  }
}
```

### LwM2M Device Management & Service Enablement Interface

This interface is used by LwM2M Server to access Object Instances and Resources
available from a registered LwM2M Client.

The interface provides this access through the use of "Create", "Read", "Write",
"Delete", "Execute", "Write-Attributes", or "Discover" operations.

The operations that Resource supports are defined in the Object definition using
the Object Template files.

To send commands to the LwM2M client, you need to send MQTT messages to EMQX in
a fixed format. These messages will be converted by the LwM2M Gateway into the
correct LwM2M message and sent to the client.


The **Topic** of command request is:
```
{?mountpoint}{?translators.command.topic}
```
Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.command.topic}` is the value of `translators.command.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.command.topic` is `dn/cmd`, then the topic for the
message would be `lwm2m/<your-real-client-endpoint-name>/dn/cmd`.


The **Payload** format of command request is:
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
Variables:
- `{?ReqID}`: Integer, request-id, used for matching the response to the request
- `{?MsgType}`: String, can be one of the following:
  - `"read"`: LwM2M Read
  - `"discover"`: LwM2M Discover
  - `"write"`: LwM2M Write
  - `"write-attr"`: LwM2M Write Attributes
  - `"execute"`: LwM2M Execute
  - `"create"`: LwM2M Create
  - `"delete"`: LwM2M Delete
- `{?RequestData}`: JSON Object, its value depends on the {?MsgType} and will be
  explained in next sections

The **Topic** of command response is:
```
{?mountpoint}{?translators.response.topic}
```
Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.response.topic}` is the value of `translators.response.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.response.topic` is `up/resp`, then the topic for the
message would be `lwm2m/<your-real-client-endpoint-name>/up/resp`.


The **Payload** format of the command response is:
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
Variables:
- `{?ReqID}`: Integer, request-id, used for matching the request
- `{?MsgType}`: String, the MsgType for the requested command, same as the Request command
- `{?ResponseData}`: JSON Object, the command response content.


#### Read

The "Read" operation is used to access the value of a Resource, an array of Resource
Instances, an Object Instance or all the Object Instance of an Object.

In request command, when the **MsgType** is `"read"`, the structure of the 
**RequestData** should be as follows:

```json
{
  "path": {?ResourcePath}
}
```
Variables:
- `{?ResourcePath}`: String, the requested resource path, it has three possible
  scenarios:
  * Only `ObjectID`, e.g. `/3`, it means reading the values of all instances and resources
    under that Object.
  * `ObjectID/InstanceID`, e.g. `/3/0`, it means reading the values of all resources
    under that Object Instance.
  * Full path, which is `{ObjectID}/{InstanceID}/{ResourceID}`, e.g. `/3/0/1`, it
    means reading the value of a specific resource.

For example, a full MQTT Payload for Read command can be:
```json
{
  "reqID": 1,
  "msgType": "read",
  "data": {
    "path": "/3/0/1"
  }
}
```

In the response, the structure of the **ResponseData** should be as follows:
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?ReadResponseData}
}
```
Variables:
- `{?ResourcePath}`, String, equals to the `path` field of that Request.
- `{?ResponseCode}`, String, LwM2M status code, e.g. "2.01", "4.00", etc.
- `{?ResponseMsg}`, String, LwM2M response message, e.g. "content", "bad_request".
- `{?ReadResponseData}`, JSON Object, the value results for that Request. It's
  a array of resources values.

For example, a fully MQTT Payload for Read Response can be:
```json
{
  "reqID": 1,
  "msgType": "read",
  "data": {
    "reqPath": "/3/0/1",
    "code": "2.05",
    "codeMsg": "content",
    "content": [
      {
        "value": "Lightweight M2M Client",
        "path": "/3/0/1"
      }
    ]
  }
}
```

#### Discover

The "Discover" operation is used to discover LwM2M Attributes attached to
an Object, Object Instances, and Resources.
This operation can be used to discover which Resources are instantiated in a
given Object Instance.
The returned payload is a list of application/link-format CoRE Links [RFC6690]
for each targeted Object, Object Instance, or Resource.

In request command, when the **MsgType** is `"discover"`, the structure of the 
**RequestData** should be as follows:

```json
{
  "path": {?ResourcePath}
}
```

It has the same format as the **Read** message:
* Only `ObjectID`, e.g. `/3`, it means discovering all instances, resources and attributes
  under that Object.
* `ObjectID/InstanceID`, e.g. `/3/0`, it means discovering all resources and attributes
  under that Object Instance.
* Full path, which is `{ObjectID}/{InstanceID}/{ResourceID}`, e.g. `/3/0/1`, it
  means discovering all attributes of a specific resource.

For example, a full MQTT Payload for Discover command can be:
```json
{
  "reqID": 2,
  "msgType": "discover",
  "data": {
    "path": "/3/0"
  }
}
```


In the response, the structure of the **ResponseData** should be as follows:
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?DiscoverResponseData}
}
```
It has same variables as the **Read** response, except the `content` field.
The `{?DiscoverResponseData}` is a array of resources and attributes.

For example, a fully MQTT Payload for Discover Response can be:
```json
{
  "reqID": 123,
  "msgType": "discover",
  "data": {
    "reqPath": "/3/0",
    "code": "2.05",
    "codeMsg": "content",
    "content": [
      "</3/0>;pmin=10",
      "</3/0/0>", "</3/0/1>", "</3/0/2>", "</3/0/3>", "</3/0/4>", "</3/0/5>",
      "</3/0/6>", "</3/0/7>", "</3/0/8>", "</3/0/9>", "</3/0/10>", "</3/0/11>",
      "</3/0/12>", "</3/0/13>", "</3/0/14>", "</3/0/15>", "</3/0/16>"
    ]
  }
}
```

#### Write

The "Write" operation is used to change the value of a Resource, the values of an array of Resources Instances or the values of multiple Resources from an Object Instance.

In request command, when the **MsgType** is `"write"`, the **RequestData** has
two possible structure.

For writing a value to a single Resource:
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
- `{?ResourcePath}`: String, the requested fully resource path, i.e: `31024/11/1`.
- `{?ValueType}`: String, can be: "Time", "String", "Integer", "Float", "Boolean", "Opaque", "Objlnk"
- `{?Value}`: Value of the resource, depends on "type".

For example, a full MQTT Payload for Write command can be:
```json
{
  "reqID": 3,
  "msgType": "write",
  "data": {
    "path": "/31024/11/1",
    "type": "String",
    "value": "write_an_example_value"
  }
}
```

For writing multiple Resources:
```json
{
  "basePath": {?BasePath},
  "content": [
    {
      "path": {?ResourcePath},
      "type": {?ValueType},
      "value": {?Value}
    }
  ]
}
```
The full path is concatenation of `{?BasePath}` and `"{ResourcePath}`.

For example, a full MQTT Payload for Write command can be:
```json
{
  "reqID": 3,
  "msgType": "write",
  "data": {
    "basePath": "/31024/11/",
    "content": [
      {
        "path": "1",
        "type": "String",
        "value": "write_the_1st_value"
      },
      {
        "path": "2",
        "type": "String",
        "value": "write_the_2nd_value"
      }
    ]
  }
}
```

#### Write-Attributes

In LwM2M 1.0, only Attributes from the `<NOTIFICATION>` class MAY be changed in using the "Write-Attributes" operation.

The operation permits multiple Attributes to be modified within the same operation.

In request command, when the **MsgType** is `"write-attr"`, the structure of the 
**RequestData** should be as follows:

```json
{
  "path": {?ResourcePath},
  "pmin": {?PeriodMin},
  "pmax": {?PeriodMax},
  "gt": {?GreaterThan},
  "lt": {?LessThan},
  "st": {?Step}
}
```
Variables:
- `{?PeriodMin}`: Number, Minimum period for notifications.
- `{?PeriodMax}`: Number, Maximum period for notifications.
- `{?GreaterThan}`: Number, Notification triggered when resource value is greater than this value.
- `{?LessThan}`: Number, Notification triggered when resource value is less than this value.
- `{?Step}`: Number, Notification triggered when the change in resource value exceeds this value.

#### Execute

The "Execute" operation is used by the LwM2M Server to initiate some action,
and can only be performed on individual Resources.

In the request command, when the **MsgType** is `"execute"`, the structure of the 
**RequestData** should be as follows:
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
Variables:
- {?Arguments}: String, LwM2M Execute Arguments.


#### Create

The "Create" operation is used by the LwM2M Server to create Object Instance(s)
within the LwM2M Client. The "Create" operation MUST target an Object.

In request command, when the **MsgType** is `"create"`, the structure of the 
**RequestData** should be as follows:

```json
{
  "basePath": "/{?ObjectID}",
  "content": [
    {
      "path": {?ResourcePath},
      "type": {?ValueType},
      "value": {?Value}
    }
  ]
}
```
Variables:
- `{?ObjectID}`: Integer, LwM2M Object ID

#### Delete

The "Delete" operation is used for LwM2M Server to delete an Object Instance
within the LwM2M Client.

In request command, when the **MsgType** is `"create"`, the structure of the 
**RequestData** should be as follows:
```json
{
  "path": "{?ObjectID}/{?InstanceID}"
}
```
Variables:
- `{?InstanceID}`: Integer, LwM2M Object Instance ID

### Information Reporting Interface

This interface is used by a LwM2M Server to observe any changes in a Resource
on a registered LwM2M Client, receiving notifications when new values are available.
This observation relationship is initiated by sending  an "Observe" operation to
the L2M2M Client for an Object, an Object Instance or a Resource.
An observation ends when a "Cancel Observation" operation is performed.

#### Observe and Cancel Observation

The **Topic** of observe and cancel observe request is:
```
{?mountpoint}{?translators.command.topic}
```
Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.command.topic}` is the value of `translators.command.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.command.topic` is `dn/cmd`, then the topic for the
message would be `lwm2m/<your-real-client-endpoint-name>/dn/cmd`.


The **Payload** format of observe and cancel observe request is:
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data":
    {
      "path": {?ResourcePath}
    }
}
```
Variables:
- `{?ReqID}`: Integer, request-id, is the {?ReqID} in the request
- `{?MsgType}`: String, can be:
  * `"observe"`: LwM2M Observe
  * `"cancel-observe"`: LwM2M Cancel Observe
- {?ResourcePath}: String, the LwM2M resource to be observed/cancel-observed.
  It only supports fully resource path, e.g. `/3/0/1`.

For example, a full MQTT Payload for Observe command can be:
```json
{
  "reqID": 10,
  "msgType": "observe",
  "data": {
    "path": "/31024/0/1"
  }
}
```

The **Topic** of the Observe response is:
```
{?mountpoint}{?translators.response.topic}
```
Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.response.topic}` is the value of `translators.response.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.response.topic` is `up/resp`, then the topic for the
message would be `lwm2m/<your-real-client-endpoint-name>/up/resp`.


The **Payload** format of the Observe response is:
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {
    "reqPath": {?RequestPath},
    "code": {?ResponseCode},
    "codeMsg": {?ResponseMsg},
    "content": [
      {
        "path": {?ResourcePath},
        "value": {?Value}
      }
    ]
  }
}
```
Variables:
- `{?ReqID}`: Integer, request-id, used for matching the request
- `{?MsgType}`: String, the MsgType for the requested command, same as the Request command
- `{?RequestPath}`, String, equals to the `path` field of that Request.
- `{?ResponseCode}`, String, LwM2M status code, e.g. "2.01", "4.00", etc.
- `{?ResponseMsg}`, String, LwM2M response message, e.g. "content", "bad_request".
- `{?ResourcePath}`: String, the requested fully resource path, i.e: `31024/11/1`.
- `{?Value}`: the current value of the resource being observed.

#### Notify

The "Notify" operation is sent from the LwM2M Client to the LwM2M Server during
a valid observation on an Object Instance or Resource.
This operation includes the new value of the Object Instance or Resource.

The notifications from LwM2M clients will be converted to MQTT message.

The **Topic** of the Notification message is:
```json
{?mountpoint}{?translators.notify.topic}
```
Variables:
- `{?mountpoint}` is the value of `mountpoint` option in LwM2M Gateway configurations.
- `{?translators.notify.topic}` is the value of `translators.notify.topic` option
  in LwM2M Gateway configurations.

For example, if the `mountpoint` is configured as `lwm2m/${endpoint_name}/`
and the `translators.notify.topic` is `up/notify`, then the topic for the
message would be `lwm2m/<your-real-client-endpoint-name>/up/notify`.


The **Payload** format of the Notification message is:

```json
{
  "reqID": {?ReqID},
  "msgType": "notify",
  "seqNum": {?ObserveSeqNum},
  "data": {
    "code": {?ResponseCode},
    "codeMsg": {?ResponseMsg},
    "reqPath": {?RequestPath},
    "content": [
      {
        "path": {?ResourcePath},
        "value": {?Value}
      }
    ]
  }
}
```
Variables:
- `{?ReqID}`: Integer, request-id, used for matching the request
- `{?ObserveSeqNum}`: Number, value of "Observe" option in CoAP message
- `{?ResponseCode}`, String, LwM2M status code, e.g. "2.01", "4.00", etc.
- `{?ResponseMsg}`, String, LwM2M response message, e.g. "content", "bad_request".
- `{?RequestPath}`, String, equals to the `path` field of that Request.
- `{?ResourcePath}`: String, the requested fully resource path, i.e: `31024/11/1`.
- `{?Value}`: the latest value of the resource.

## User Interfaces

- Detailed configuration options: [Configuration - LwM2M Gateway](../configuration/configuration-manual.md)
- Detailed HTTP APIs Description: [HTTP API - Gateway](../admin/api.md)

## Client libraries

- [wakaama](https://github.com/eclipse/wakaama)
