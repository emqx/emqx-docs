# LwM2M Protocol Gateway

## Protocol Introduction

LwM2M (Lightweight Machine-To-Machine) is a protocol from the Open Mobile Alliance (OMA) for machine to machine (M2M) or Internet of things device (IoT), it provides device management and communication functions, especially suitable for terminal devices with limited resources.

For more information, see [OMA SpecWorks](http://openmobilealliance.org/iot/lightweight-m2m-lwm2m.)

LwM2M is built on [Constrained Application Protocol](https://www.rfc-editor.org/rfc/rfc7252) (CoAP), carried over UDP or SMS, based on REST architecture,
so the message structure is simple and compact to worker better in environments where network resources are limited and the device is not always online.

There are two roles in the LwM2M protocol:

- LwM2M Server

  LwM2M has two kind services:

  1. LwM2M bootstrap server. The `emqx-lwm2m` plugin does not implement this server.
  2. LwM2M server. The `emqx-lwm2m` plugin implement it over UDP/DTLS (but not over TCP/TLS, nor over SMS).

- LwM2M Client

    An LwM2M client is deployed on an LwM2M device which connects to the server for machine to machine communication.

Between LwM2M Server and LwM2M Client, the LwM2M protocol defines 4 interfaces.

1. Bootstrap interface

    This interface is to provide the client with necessary server information (such as server access point) for it to register to the server. The client also makes use of this interface to provide information (such as supported resources) to the server.

2. Client Registration Interface

     The client registers itself to the server, and the server stores the information about the connection for future use. Communication between client and server is only possible after registration is completed.

3. Device management and service implementation interface

    Allows the server to access and modify LwM2M client object instances and resources.

4. Information reporting interface

    Allows the server to subscribe resource information to the client, and the client reports its resource changes to the server in agreed mode.

LwM2M abstracts the services on the device as `Object`s and `Resource`s, and defines the attributes and functionalities of various objects in XML files. See
[Here](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html) for details.


The LwM2M protocol predefines 8 kinds of objects, namely:

  - Security object
  - Server object
  - Access Control access control object
  - Device object
  - Connectivity Monitoring Connectivity monitoring object
  - Firmware firmware object
  - Location object
  - Connectivity Statistics Connectivity statistics object

## Quick Start

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules), click the `Modules` tab on the left, and choose to add:

![image-20200927213049265](./assets/modules.png)

Select the `LwM2M Gateway`

![image-20200927213049265](./assets/proto_lwm2m1.png)

Configure basic parameters:

![image-20200927213049265](./assets/proto_lwm2m2.png)

Add a listener:

![image-20200927213049265](./assets/proto_lwm2m3.png)

Configure listener parameters:

![image-20200927213049265](./assets/proto_lwm2m4.png)

Click `confirm` to enter the configuration parameter page:

![image-20200927213049265](./assets/proto_lwm2m5.png)

After clicking `Add`, the module is added:
![image-20200927213049265](./assets/proto_lwm2m6.png)

EMQX-LwM2M is a gateway module of EMQX server, it implements most of the functionalities of LwM2M. MQTT client can access LwM2M-enabled devices through EMQX-LWM2M and devices can also report notification to EMQX-LwM2M for data collection and integration over MQTT protocol.

### Configuration parameters

| Configuration     | Description                                                  |
| ----------------- | ------------------------------------------------------------ |
| Minimum Lifetime  | Minimum lifetime allowed to be set for registration/update, in seconds |
| Maximum Lifetime  | Maximum lifetime allowed to be set for registration/update, in seconds |
| QMode Time Window | QMode time window, indicating how long the downstream command sent to the client will be cached, in seconds |
| Auto Observe      | After successful registration, whether to automatically observe the objectlist |
| Mountpoint        | topic Prefix                                                 |
| Command Topic     | Downlink command topic, %e means endpoint name              |
| Response Topic    | Uplink response topic, %e means endpoint name                |
| Register Topic    | Register message topic, %e means endpoint name                 |
| Notify Topic      | Uplink notification topic, %e means endpoint name              |
| Update Topic      | Update message topic, %e means endpoint name                   |
| XML Directory     | The directory of the LwM2M schema files which define LwM2M objects' schema |

## MQTT and LwM2M conversion

With `emqx_lwm2m`, user is able to send LwM2M commands(READ/WRITE/EXECUTE/...) and get LwM2M response in MQTT way. `emqx_lwm2m` transforms data between MQTT and LwM2M protocol.

emqx_lwm2m needs object definitions to parse data from lwm2m devices. Object definitions are declared by organizations in XML format, you could find those XMLs from [LwM2MRegistry](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html), download and put them into the directory specified by `lwm2m.xml_dir`. If no associated object definition is found, response from device will be discarded and report an error message in log.

### Register/Update (LwM2M Client Registration Interface)

- **LwM2M Register and Update message will be converted to following MQTT message:**

  - **Method:** PUBLISH
  - **Topic:** `lwm2m/{?EndpointName}/up/resp` (configurable, `Mountpoint` + `Register/Update` Topic. The same rules apply to other conversion topics)
  - **Payload**:
    - MsgType **register** and **update**:
      ```json
      {
          "msgType": {?MsgType},
          "data": {
              "ep": {?EndpointName},
              "lt": {?LifeTime},
              "sms": {?MSISDN},
              "lwm2m": {?Lwm2mVersion},
              "b": {?Binding},
              "alternatePath": {?AlternatePath},
              "objectList": {?ObjectList}
          }
      }
      ```
      - {?EndpointName}: String, the endpoint name of the LwM2M client
      - {?MsgType}: String, could be:
        - "register": LwM2M Register
        - "update": LwM2M Update
      - "data" contains the query options and the object-list of the register message
      - The *update* message is only published if the object-list changed.

### Downlink Command and Uplink Response (LwM2M Device Management & Service Enablement Interface)

- **To send a downlink command to device, publish following MQTT message:**
  - **Method:** PUBLISH
  - **Topic:** `lwm2m/{?EndpointName}/dn`
  - **Request Payload**:
    ```json
    {
        "reqID": {?ReqID},
        "msgType": {?MsgType},
        "data": {?Data}
    }
    ```
    - {?ReqID}: Integer, request-id, used for matching the response to the request
    - {?MsgType}: String, can be one of the following:
      - "read": LwM2M Read
      - "discover": LwM2M Discover
      - "write": LwM2M Write
      - "write-attr": LwM2M Write Attributes
      - "execute": LwM2M Execute
      - "create": LwM2M Create
      - "delete": LwM2M Delete
    - {?Data}: Json Object, its value depends on the {?MsgType}:
      - **If {?MsgType} = "read" or "discover"**:
        ```json
        {
            "path": {?ResourcePath}
        }
        ```
        - {?ResourcePath}: String, LwM2M full resource path. e.g. "3/0", "/3/0/0", "/3/0/6/0"
      - **If {?MsgType} = "write" (single write)**:
        ```json
        {
            "path": {?ResourcePath},
            "type": {?ValueType},
            "value": {?Value}
        }
        ```
        - {?ValueType}: String, can be: "Time", "String", "Integer", "Float", "Boolean", "Opaque", "Objlnk"
        - {?Value}: Value of the resource, depends on "type".
      - **If {?MsgType} = "write" (batch write)**:
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
        - The full path is concatenation of "basePath" and "path".
      - **If {?MsgType} = "write-attr"**:
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
        - {?PeriodMin}: Number, LwM2M Notification Class Attribute - Minimum Period.
        - {?PeriodMax}: Number, LwM2M Notification Class Attribute - Maximum Period.
        - {?GreaterThan}: Number, LwM2M Notification Class Attribute - Greater Than.
        - {?LessThan}: Number, LwM2M Notification Class Attribute - Less Than.
        - {?Step}: Number, LwM2M Notification Class Attribute - Step.

      - **If {?MsgType} = "execute"**:
        ```json
        {
            "path": {?ResourcePath},
            "args": {?Arguments}
        }
        ```
        - {?Arguments}: String, LwM2M Execute Arguments.

      - **If {?MsgType} = "create"**:
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
        - {?ObjectID}: Integer, LwM2M Object ID

      - **If {?MsgType} = "delete"**:
        ```json
        {
            "path": "{?ObjectID}/{?ObjectInstanceID}"
        }
        ```
        - {?ObjectInstanceID}: Integer, LwM2M Object Instance ID

- **The response of LwM2M will be converted to following MQTT message:**
  - **Method:** PUBLISH
  - **Topic:** `"lwm2m/{?EndpointName}/up/resp"`
  - **Response Payload:**

  ```json
  {
      "reqID": {?ReqID},
      "imei": {?IMEI},
      "imsi": {?IMSI},
      "msgType": {?MsgType},
      "data": {?Data}
  }
  ```

  - {?MsgType}: String, can be:
    - "read": LwM2M Read
    - "discover": LwM2M Discover
    - "write": LwM2M Write
    - "write-attr": LwM2M Write Attributes
    - "execute": LwM2M Execute
    - "create": LwM2M Create
    - "delete": LwM2M Delete
    - **"ack"**: [CoAP Empty ACK](https://tools.ietf.org/html/rfc7252#section-5.2.2)
  - {?Data}: Json Object, its value depends on {?MsgType}:
    - **If {?MsgType} = "write", "write-attr", "execute", "create", "delete", or "read"(when response without content)**:
      ```json
      {
            "code": {?StatusCode},
            "codeMsg": {?CodeMsg},
            "reqPath": {?RequestPath}
      }
      ```
      - {?StatusCode}: String, LwM2M status code, e.g. "2.01", "4.00", etc.
      - {?CodeMsg}: String, LwM2M response message, e.g. "content", "bad_request"
      - {?RequestPath}: String, the requested "path" or "basePath"

    - **If {?MsgType} = "discover"**:
      ```json
      {
          "code": {?StatusCode},
          "codeMsg": {?CodeMsg},
          "reqPath": {?RequestPath},
          "content": [
              {?Link},
              ...
          ]
      }
      ```
      - {?Link}: String(LwM2M link format) e.g. `"</3>"`, `"<3/0/1>;dim=8"`

    - **If {?MsgType} = "read"(when response with content)**:
      ```json
      {
          "code": {?StatusCode},
          "codeMsg": {?CodeMsg},
          "content": {?Content}
      }
      ```
      - {?Content}
        ```json
        [
            {
                "path": {?ResourcePath},
                "value": {?Value}
            }
        ]
        ```

    - **If {?MsgType} = "ack", "data" does not exists**

### Observe (Information Reporting Interface - Observe/Cancel-Observe)

- **To observe/cancel-observe LwM2M client, send following MQTT PUBLISH:**
  - **Method:** PUBLISH
  - **Topic:** `lwm2m/{?EndpointName}/dn`
  - **Request Payload**:
    ```json
    {
        "reqID": {?ReqID},
        "msgType": {?MsgType},
        "data": {
            "path": {?ResourcePath}
        }
    }
    ```
    - {?ResourcePath}: String, the LwM2M resource to be observed/cancel-observed.
    - {?MsgType}: String, can be:
      - "observe": LwM2M Observe
      - "cancel-observe": LwM2M Cancel Observe
    - {?ReqID}: Integer, request-id, is the {?ReqID} in the request

- **Responses will be converted to following MQTT message:**
  - **Method:** PUBLISH
  - **Topic:** `lwm2m/{?EndpointName}/up/resp`
  - **Response Payload**:
    ```json
    {
        "reqID": {?ReqID},
        "msgType": {?MsgType},
        "data": {
            "code": {?StatusCode},
            "codeMsg": {?CodeMsg},
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
    - {?MsgType}: String, can be:
      - "observe": LwM2M Observe
      - "cancel-observe": LwM2M Cancel Observe
      - **"ack"**: [CoAP Empty ACK](https://tools.ietf.org/html/rfc7252#section-5.2.2)

### Notification (Information Reporting Interface - Notify)

- **The notifications from LwM2M clients will be converted to MQTT PUBLISH:**
  - **Method:** PUBLISH
  - **Topic:** `lwm2m/{?EndpiontName}/up/notify`
  - **Notification Payload**:
    ```json
    {
        "reqID": {?ReqID},
        "msgType": {?MsgType},
        "seqNum": {?ObserveSeqNum},
        "data": {
            "code": {?StatusCode},
            "codeMsg": {?CodeMsg},
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
    - {?MsgType}: String, must be "notify"
    - {?ObserveSeqNum}: Number, value of "Observe" option in CoAP message
    - "content": same to the "content" field contains in the response of "read" command
