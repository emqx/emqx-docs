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
ref: undefined
---

# Device management

With the help of client management and topic subscription, message publishing management interface and plug-ins such as authentication, ACL, WebHook provided by EMQX [Http API](../advanced/http-api.md), we can quickly build a device and message management system to build a network access platform.



## Device authentication

Use the [authentication](../advanced/auth.md)  function to implement device connection authentication.

The management system can directly read and write external authentication databases or change the internal authentication data of EMQX through APIs to realize dynamic management of device connection authentication.

## Online status and connection history management

When the device is connected or disconnected, EMQX can send the online and offline information to the management system HTTP API through the Webhook plug-in and [rule engine](../rule/rule-engine.md) to realize the device online status rewriting and device connection/disconnection history recording and other functions.

Use the disconnecting device API to achieve online device kick out and session clean operations.


## Publish-Subscribe/ACL

Use the  [publish and subscribe ACL](../advanced/acl.md) function to realize the permission management of device publishing and subscription.

The management system can directly read and write external authentication databases to realize dynamic management of device publishing and subscription.


## Proxy subscription

The management system can subscribe/unsubscribe specified topics for online devices through the proxy subscription function, and there is no need to reset the device program after the business Topic is changed, which is very flexible.


## HTTP message publishing

The message publishing API management system can publish messages to any Topic without using additional clients, and implements HTTP-MQTT message conversion.

HTTP message publishing decouples communication between users and devices, and between management systems and devices. Indirect communication are used to reduce system complexity and further improve security.


