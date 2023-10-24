---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: 
---

# Multi-Protocol Gateway Plugin

The multi-protocol gateway plugins are crucial components of EMQX, enabling EMQX to support a multitude of IoT communication protocols. Through the protocol conversion, message routing, and processing functionalities of the gateway plugins, devices and applications using various communication protocols can establish connections with EMQX, thereby achieving cross-protocol interoperability. This page provides an overview of the connecting protocols supported by EMQX and offers guidance on the utilization and configuration of the protocol gateway plugin.

## MQTT-SN Protocol

The MQTT-SN protocol, closely related to MQTT, uses UDP for communication on port 1884. MQTT-SN is primarily designed to accommodate resource-constrained devices and networks, such as sensors with limited memory and CPU resources. For these devices, TCP is considered resource-intensive. Additionally, some networks, like ZIGBEE, have message length restrictions of less than 300 bytes, making MQTT-SN's smaller packets more suitable.

Official MQTT-SN standard download link: http://mqtt.org/new/wp-content/uploads/2009/06/MQTT-SN_spec_v1.2.pdf

### Differences Between MQTT-SN and MQTT

Most of MQTT-SN's signaling and MQTT's are similar, including features like "Will," "Connect," "Subscribe," and "Publish" commands. However, MQTT-SN's significant difference lies in its use of "TopicId" instead of regular "Topic." TopicId is a 16-bit number, and each number corresponds to a specific Topic. Devices and cloud systems must use the "REGISTER" command to map the relationship between TopicId and Topic.

MQTT-SN allows real-time changes to "Will" content and even cancellation, while MQTT only allows "Will" content to be set during connection and prohibits subsequent changes.

MQTT-SN networks include gateway devices that convert MQTT-SN to MQTT, facilitating communication between MQTT-SN and MQTT brokers in the cloud. MQTT-SN's protocol supports the automatic discovery of these gateways.

MQTT-SN also supports device sleep mode. When a device enters sleep mode and cannot receive UDP data, the gateway caches downstream "PUBLISH" messages until the device wakes up, ensuring message delivery upon awakening.

### EMQX-SN Gateway Plugin

EMQX-SN is a gateway plugin for EMQX that implements most MQTT-SN features. It acts as a cloud-based MQTT-SN gateway, directly connecting to the EMQX Broker.

#### Configuration Parameters

```bash
# File: etc/plugins/emqx_sn.conf:

mqtt.sn.port = 1884

mqtt.sn.advertise_duration = 900

mqtt.sn.gateway_id = 1

mqtt.sn.username = mqtt_sn_user

mqtt.sn.password = abc
```

| Configuration Item         | Description                                                  |
| -------------------------- | ------------------------------------------------------------ |
| mqtt.sn.port               | Specifies the port number for MQTT-SN listening              |
| mqtt.sn.advertise_duration | ADVERTISE message transmission interval (seconds)            |
| mqtt.sn.gateway_id         | Gateway ID                                                   |
| mqtt.sn.username           | Optional parameter specifying the username for all MQTT-SN connections for authentication |
| mqtt.sn.password           | This is also an optional parameter, used in conjunction with the username |

#### Start emqx-sn

```bash
./bin/emqx_ctl plugins load emqx_sn
```

### MQTT-SN Client Libraries

1. https://github.com/eclipse/paho.mqtt-sn.embedded-c/
2. https://github.com/ty4tw/MQTT-SN
3. https://github.com/njh/mqtt-sn-tools
4. https://github.com/arobenko/mqtt-sn

## LWM2M Protocol

LwM2M stands for Lightweight Machine-To-Machine and is a lightweight protocol designed for the Internet of Things (IoT). It provides device management and communication functions, particularly suitable for resource-constrained IoT devices. The protocol is available for download [here](http://www.openmobilealliance.org/wp/).

LwM2M is based on the REST architecture and uses CoAP as its underlying transport protocol, running over UDP or SMS. This results in a simple and compact message structure, making it suitable for environments with limited network resources and devices that are not always online.

LwM2M has two main entities: the LwM2M Server and the LwM2M Client.

The LwM2M Server, deployed at M2M service providers or network service providers, defines two types of servers:

- LwM2M BOOTSTRAP SERVER, not implemented by the emqx-lwm2m plugin.
- LwM2M SERVER, implemented by emqx-lwm2m for UDP, but not for SMS.

The LwM2M Client, as the client, is deployed at each LwM2M device.

LwM2M defines four interfaces between the LwM2M Server and the LwM2M Client:

1. **Bootstrap Interface**: Provides essential registration information to LwM2M clients, such as server access and client-supported resource information.
2. **Client Registration Interface**: Establishes connections between LwM2M clients and the LwM2M server, storing the relevant information on the server. Communication and management between the LwM2M client and server occur only after successful registration.
3. **Device Management and Service Implementation Interface**: Controlled by the LwM2M server, this interface sends commands to the client, with the client responding and sending the response message to the server.
4. **Information Reporting Interface**: Allows the LwM2M server to subscribe to resource information from the client, with the client reporting changes in its resources according to the agreed-upon pattern.

LwM2M abstracts device services into Objects and Resources. Various Objects' properties and functions are defined in XML files, which can be found [here](http://www.openmobilealliance.org/wp/OMNA/LwM2M/LwM2MRegistry.html).

LwM2M predefines eight Objects to meet basic requirements:

- Security Object
- Server Object
- Access Control Object
- Device Object
- Connectivity Monitoring Object
- Firmware Object
- Location Object
- Connectivity Statistics Object

### EMQX-LWM2M Plugin

EMQX-LWM2M is a gateway plugin for the EMQX server, implementing most LwM2M features. MQTT clients can access devices that support LwM2M through EMQX-LWM2M. Devices can also report notifications to EMQX-LWM2M, collecting data for EMQX backend services.

### MQTT to LwM2M Conversion

Commands from MQTT clients can be sent to LwM2M devices. MQTT to LwM2M commands use the following topic format:

```bash
"lwm2m/{?device_end_point_name}/command"
```

The MQTT payload is a JSON-formatted string that specifies the command to be sent. For more details, refer to the emqx-lwm2m documentation.

Replies from LwM2M devices are sent using the following topic format:

```bash
"lwm2m/{?device_end_point_name}/response"
```

The MQTT payload is also a JSON-formatted string. Refer to emqx-lwm2m documentation for further details.

#### Configuration Parameters

```bash
# File: etc/emqx_lwm2m.conf:

lwm2m.port = 5683

lwm2m.certfile = etc/certs/cert.pem

lwm2m.keyfile = etc/certs/key.pem

lwm2m.xml_dir = etc/lwm2m_xml
```

| Configuration Item | Description                                                  |
| ------------------ | ------------------------------------------------------------ |
| lwm2m.port         | Specifies the port number for LwM2M listening (use non-standard port 5783 to avoid conflicts with emqx-coap) |
| lwm2m.certfile     | Certificate used for DTLS                                    |
| lwm2m.keyfile      | Key used for DTLS                                            |
| lwm2m.xml_dir      | Directory storing XML files defining LwM2M Objects           |

#### Start emqx-lwm2m

```bash
./bin/emqx_ctl plugins load emqx_lwm2m
```

### LwM2M Client Libraries

- https://github.com/eclipse/wakaama
- https://github.com/OpenMobileAlliance/OMA-LWM2M-DevKit
- https://github.com/AVSystem/Anjay
- http://www.eclipse.org/leshan/

## CoAP Protocol

CoAP (Constrained Application Protocol) is a lightweight network transport protocol designed for the IoT environment. It operates over UDP, supports client/server communication, and ensures reliable transmission through confirmable/non-confirmable messages and resource observation. CoAP messages are small, making them suitable for environments with wireless constraints and low battery requirements.

CoAP's lightweight and efficient characteristics make it an excellent fit for IoT environments, either as a standalone protocol or combined with others to fulfill various application scenarios.

EMQX provides CoAP support through plugins. For usage, refer to [EMQX CoAP](https://github.com/emqx/emqx/blob/main-v4.4/apps/emqx_coap/README.md).

## STOMP Protocol

STOMP (Simple Text Oriented Messaging Protocol) is a text-based messaging middleware protocol. It uses straightforward text formatting for communication, supporting both publish-subscribe and point-to-point messaging modes. STOMP is easy to implement and extend and can be used across multiple programming languages. It's commonly used for asynchronous messaging between clients and message brokers.

STOMP's simplicity and lightweight text communication make it an efficient choice for supporting messaging middleware and asynchronous communication in distributed systems. Its ease of use has made it a popular protocol for text-based machine-to-machine communication.

EMQX provides STOMP support through plugins. For usage, refer to [EMQX STOMP](https://github.com/emqx/emqx/blob/main-v4.4/apps/emqx_stomp/README.md).
