# MQTT Specific

## Overview

[MQTT](https://mqtt.org/) is a standard messaging protocol for the Internet of Things (IoT). It is designed as an extremely lightweight publish/subscribe messaging transport that is ideal for connecting remote devices with a small code footprint and minimal network bandwidth.

MQTT today is used in a wide variety of industries, such as IoT, mobile web, intelligent hardware, Internet of Cars, smart city, telemedicine, electricity, oil and gas, etc.

MQTT 3.1.1: [http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html)
MQTT 5.0: [http://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html](http://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html)

## Advantages

1. Open messaging protocol, easy to implement
2. Publish/subscribe model, support one-to-many message publishing
3. Based on TCP/IP network connection
4. Compact message structure with 1 byte of fixed header and 2 bytes of heartbeat message
5. Message QoS supported, reliable message transmission
6. Flexible messaging capability, with no dependency on payload data format
7. Persistent session awareness, with timely information on whether the device is online

## Core feature

Below is a list of MQTT protocols and the EMQX extended concepts:

- [Publish/Subscribe Model](./mqtt-publish-and-subscribe.md)
- [Session and Message Expiration](./mqtt-session-and-message-expiry.md)
- [QoS](./mqtt-qos.md)
- [Retained Messages](./mqtt-retained-messages.md)
- [Will Message](./mqtt-last-will-and-testament.md)
- [Shared Subscription](./mqtt-shared-subscription.md)
- [Exclusive Subscription](./mqtt-exclusive-subscription.md)
- [Delayed Publish](./mqtt-delayed-publish.md)
- [Auto Subscription](./mqtt-auto-subscription.md)
- [Topic Rewrite](./mqtt-topic-rewrite.md)
- [System Topic](./mqtt-system-topics.md)

## Comparison with other protocols

Typical IoT use cases are very complex, with many devices, networks, applications, and services involved, therefore a variety of IoT protocols have emerged. You may refer to the link below for a comparison between MQTT and other protocols:

[*MQTT, CoAP, or LwM2M? Which IoT protocol to choose?*](https://www.emqx.com/en/blog/iot-protocols-mqtt-coap-lwm2m)
