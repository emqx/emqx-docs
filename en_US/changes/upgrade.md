---
# 标题
title: 升级指南
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

# Upgrade guide

## Upgrade to version 4.0

The following provides a set of guidelines for migrating from the EMQ X 3.x version to the latest EMQ X 4.0 version. Although we tried to reduce some major changes, we have modified in several places in order to balance performance and simplify usage.

**How long does it take to migrate from EMQ X 3.x to EMQ X 4.0?** 

EMQ X always guarantees the standardization and continuous update of the access protocol. When the version is migrated, the client does not need any adjustments, which means that you do not need to stop the device function and reprogram the device firmware. You only need to pay attention to the changes of plug-ins, configuration items, command line and REST API.

The time required depends on the size of your project and the scope of changes. Small and medium-sized projects can be completed within a day.

### Core

#### Change client\_id  to clientid

We have made major changes in variable names here. All client\_id characters in EMQ X have been changed to clientid, including:

  - URL of REST API, field name in request and corresponding data
  - Naming conventions in the source code
  - Command line CLI

### REST API

#### Change v3 to v4

REST API is changed from `http(s)://host:8081/api/v3/` to
`http(s)://host:8081/api/v4/`。

#### Change connection to clients

Change the concept of connections to clients, which involves APIs related to nodes and clusters:

  - Get the list of cluster connections:`GET /connections` -\> `/clients`
  - Get the specified connection information of the cluster:`GET /connections/:clientid` -\> `GET
    /connections/:clientid`
  - Get the node connection list:`GET /nodes/:node/connections` -\> `GET
    /nodes/:node/clients`
  - Get the specified connection information of the node:`GET /nodes/:node/connections/:clientid` -\> `GET
    /nodes/:node/clients/:clientid`
  - Field names of client\_id in request and corresponding data  are all changed to clientid.

At the same time, there is a big change in the content returned by the API. See the 4.0 document for details.

#### Remove session related API

The concept of Channel was introduced in 4.0, which combines session and client. The following API has been **removed** in version 4.0:

  - Get the list of cluster sessions:`GET /sessions`
  - Get the session information of the specified client of the cluster:`GET /sessions/:clientid`
  - Get the list of node sessions: `GET /nodes/:node/sessions`
  - **Get** the session information of the specified client of the node:`GET /nodes/:node/sessions/:clientid`

If you need to obtain session-related information after 4.0 version, please use the client-related API.

#### Remove plugin configuration acquisition and change API

The plugin configuration may contain sensitive information. At the same time, the plug-in configuration does not support persistence, which brings great doubts to users. Taking security issues and practical issues into account, we **removed** the plugin acquisition and change API.

  - Get plug-in configuration information:`GET /nodes/:node/plugins/:plugin_name`
  - Update the plugin configuration:`PUT /nodes/:node/plugins/:plugin_name`

We plan to solve the above problems through security specifications and local storage of configuration items in the Enterprise version, and provide APIs related to hot configuration of plug-ins again. *Currently, the enterprise version supports hot configuration operations for critical configurations.*

### Dashboard

#### Change connection to clients

The concept of **connections** in Dashboard is changed to **clients**. The original connection information can be viewed on the current **clients** page.

#### Remove sessions management page

The **session** management page was removed from the Dashboard, and related information was integrated into the **clients** page.

#### Rule engine

The SQL syntax of the rule engine has been changed, and the **event** drop-down selection box is no longer provided in the Dashboard when the rule is created. For the detailed change of the SQL grammar, see the **Rules Engine** section of this article.

### Rule engine

#### SQL syntax changes

In the 4.0 version, the SQL syntax of the rule engine is easier to use. In the 3.x version, event name needs to be specified after the all **FROM** clauses. After 4.0, we introduce the concept of **event topic**. By default, the **message Publishing** event no longer requires specifying the event name:

```bash 
## 3.x version
## Need to specify event name
SELECT * FROM "message.publish" WHERE topic =~ 't/#'

## 4.0 and later version
##The message.publish event is processed by default, and MQTT topics are filtered directly after FROM
## The above SQL statement is equivalent to:
SELECT * FROM 't/#'

## Other events are filtered by event topic
SELECT * FROM "$events/message_acked" where topic =~ 't/#'
SELECT * FROM "$events/client_connected"
```

Dashboard provides old SQL syntax conversion function to complete SQL upgrade migration.

#### Event name change

In version 4.0, Subscription/Unsubscription principal is changed to **session**. When converting *event* to **event topic,** the following changes need to be noted:

  - **Terminal subscription** is changed to **Session subscription**：`client.subscribe` -\>
    `$events/session_subscribe`
  - **Terminal unsubscription** is changed to **Session unsubscription**：`client.unsubscribe` -\>
    `$events/session_unsubscribe`



## Upgrade to version 3.1

<div class="note">

<div class="admonition-title">

Note

</div>

For Version 3.1, the project architecture, configuration method and plug-in management method are completely redesigned. Upgrade of 2.x and 1.x versions requires reconfiguration and deployment.

</div>

Upgrade process:

1.  Download and unzip version 3.1 to the new installation directory, such as /opt/emqx-3.1/;
2. Refer to the old version etc/vm.args, etc/emqttd.config or etc/emq.conf to configure version 3.1
     etc/emqx.conf;
3. Reconfigure the plugin etc/ plugins/${your-plugin}.conf;
4. Edit the plugin loading file data/loaded_plugins;
5. Stop the old version and start the new version.

## Upgrade 2.0 version to 2.0.3 version

Upgrade process:

1.  Download and unzip the 2.0.3 version to the new installation directory, such as /opt/emqttd-2.0.3/;
2. The  'etc /' configuration file and the 'data /' data file of old version are overwritten to the new version directory;
3. Stop the old version and start the new version.

## Upgrade to version 2.0

<div class="note">

<div class="admonition-title">

Note

</div>

For Version 2.0, the project architecture, configuration method and plugin management method are completely redesigned. Upgrade of 1.x versions requires reconfiguration and deployment.

</div>

Upgrade process:

1.  Download and unzip version 2.0 to the new installation directory, such as /opt/emqx-2.0/;
2.  Refer to the old version etc/vm.args, etc/emqttd.config or etc/emq.conf to configure version 2.0
    etc/emqx.conf;
3.  Reconfigure the plugin etc/ plugins/${your-plugin}.conf;
4.  Edit the plugin loading file data/loaded_plugins;
5.  Stop the old version and start the new version.

## Upgrade to version 1.1.2

<div class="note">

<div class="admonition-title">

Note

</div>

Versions after 1.0 can be smoothly upgraded to 1.1.2

</div>

Upgrade process:

1.  Download and unzip version 1.1.2 to the new installation directory, such as /opt/emqttd_112;
2. The 'etc /' configuration file and the 'data /' data file of the old version are overwritten to the new version directory;
3. If there are plugins loaded, overwrite the old plugin configuration file to the new version;
4. Stop the old version and start the new version.

