---
# 编写日期
date: 2020-02-20 12:44:32
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

# Use Guide
## How to use EMQX?


EMQX Broker is free and it can be download at [https://www.emqx.com/en/try?product=broker](https://www.emqx.com/en/try?product=broker).

EMQX Enterprise can be downloaded and evaluated for free.  You can download it from [https://www.emqx.com/en/try?product=enterprise](https://www.emqx.com/en/try?product=enterprise), and then apply trial license at [https://www.emqx.com/en/apply-licenses/emqx](https://www.emqx.com/en/apply-licenses/emqx).

Also you can use the EMQX enterprise version through public cloud service.

- [AWS](https://aws.amazon.com/marketplace/pp/B07N2ZFVLX?qid=1552872864456&)




## How to update EMQX license?

**Tags:** [*Update*](tags.md#update)


You need to two steps:

1. The license file will be sent by email, find the attached zip file and unzip it.

2. Extract the license file (emqx.lic) from the zip file to a directory readable by the EMQX user.

After the extraction is complete, the license needs to be reloaded from the command line to complete the update:

```
emqx_ctl license reload [license file path]
```

The update commands for different installation modes:

```
## zip packages
./bin/emqx_ctl license reload path/to/emqx.lic

## DEB/RPM
emqx_ctl license reload path/to/emqx.lic

## Docker
docker exec -it emqx-ee emqx_ctl license reload path/to/emqx.lic
```

::: tip
On a multi-node cluster, the `emqx_ctl license reload` command needs to be executed only on one of the nodes, as the license will be replicated and applied to all members.  Each one will contain a copy of the new license under the configured data directory for EMQX, as well as a backup of the old license, if any.

Note that this command only takes effect _on the local node_ executing the command for EMQX versions prior to e4.3.10, so this command will require being executed on each node of the cluster for those older versions.
:::


## Can EMQX support customized protocols? How to implement?

**Tags:** [*Protocol*](tags.md#protocol)  [*Extends*](tags.md#extends)


TODO...




## Can I capture device online and offline events? How to use it?

**Tags:** [*WebHook*](tags.md#webhook)  [*System Topic*](tags.md#system-topic)


EMQX supports to capture device online and offline events through below 3 approaches,

- Web Hook

- Subscribe related $SYS topics

  - $SYS/brokers/${node}/clients/${clientid}/connected
  - $SYS/brokers/${node}/clients/${clientid}/disconnected

- Directly save events into database

  The final approach is only supported in enterprise version, and supported database includes Redis, MySQL, PostgreSQL, MongoDB and Cassandra. User can configure database, client.connected and client.disconnected events in the configuration file. When a device is online or offline, the information will be saved into database.




## I want to control topics can be used for specific clients, how to configure it in EMQX?

**Tags:** [*ACL*](tags.md#acl)  [*Pub/Sub*](tags.md#pub-sub)


EMQX can constrain clients used topics to realize device access controls. To use this feature, ACL (Access Control List) should be enabled, disable anonymous access and set `acl_nomatch` to 'deny' (For the convenience of debugging, the last 2 options are enabled by default, and please close them).

ACL can be configured in configuration file, or backend databases. Below is one of sample line for ACL control file, the meaning is user 'dashboard' can subscribe '$SYS/#' topic. ACL configuration in backend databases is similar, refer to EMQX document for more detailed configurations.

```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
```




## Can EMQX support traffic control?

**Tags:** [*Rate Limit*](tags.md#rate-limit)


Yes. Currently EMQX supports to control connection rate and message publish rate. Refer to below for sample configuration.

```
## Value: Number
listener.tcp.external.max_conn_rate = 1000

## Value: rate,burst
listener.tcp.external.rate_limit = 1024,4096
```




## How does the EMQX achieve high concurrency and high availability?

**Tags:** [*Performance*](tags.md#performance)


High concurrency and availability are design goals of EMQX. To achieve these goals, several technologies are applied:

- Making maximum use of the soft-realtime, high concurrent and fault-tolerant Erlang/OTP platform;
- Full asynchronous architecture;
- Layered design of connection, session, route and cluster;
- Separated messaging and control panel;

With the well design and implementation, a single EMQX cluster can handle million level connections.

EMQX supports clustering. The EMQX performance can be scale-out with the increased number of nodes in cluster, and the MQTT service will not be interrupted when a single node is down.




## Can EMQX store messages to database?

**Tags:** [*Persistence*](tags.md#persistence)


The EMQX Enterprise supports data persistence. Supported databases are:

- Redis
- MongoDB
- MySQL
- PostgreSQL
- Cassandra




## Can I disconnect an MQTT connection from EMQX server?

**Tags:** [*REAT API*](tags.md#reat-api)  [*Dashboard*](tags.md#dashboard)


Yes. You can do it by invoking REST API provided by EMQX, but the implementation is different in EMQX 2.x and 3.x:

- EMQX customized protocol in 2.x versions.
- Follow the process defined in MQTT 5.0 protocol after version 3.0.

Refer to below for API invocation:

```html
HTTP Method: DELETE
URL：api/[v2|v3]/clients/{clientid}
<!--Please notice the 2nd section in URL, and use the correct version number according to your EMQX version. -->

Returned response:
{
    "code": 0,
    "result": []
}
```




## Can EMQX forward messages to Kafka?

**Tags:** [*Kafka*](tags.md#kafka)  [*Bridge*](tags.md#bridge)  [*Persistence*](tags.md#persistence)


The EMQX Enterprise integrates a Kafka bridge, it can bridge data to Kafka.




## I use Kafka bridge in EMQX enterprise, when will the MQTT Ack packet sent back to client?  Is the time when message arriving EMQX or after getting Ack message from Kafka?

**Tags:** [*Kafka*](tags.md#kafka)


It's up to Kafka bridge configuration, the configuration file is at `/etc/emqx/plugins/emqx_bridge_kafka.conf`

```bash
## Pick a partition producer and sync/async.
bridge.kafka.produce = sync
```

- Sync: MQTT Ack packet will be sent back to client after receiving Ack from Kafka.
- Async: MQTT Ack packet will be sent back to client right after EMQX receiving the message, and EMQX will not wait the Ack returned from Kafka.

If the backend Kafka server is not available, then the message will be accumulated in EMQX broker.

- The message will be cached in memory before EMQX 2.4.3 version, if the memory is exhausted, then the EMQX server will be down.
- The message will be cached in disk after EMQX 2.4.3 version, message will probably lost if the disk is full.

So we suggest you to closely monitor Kafka server, and recover Kafka service as soon as possible when it has any questions.




## Does EMQX support cluster auto discovery? What clustering methods are supported?

**Tags:** [*Cluster*](tags.md#cluster)


EMQX supports cluster auto discovery. EMQX clustering can be done manually or automatically.

Currently supported clustering methods:

- Manual clustering
- Static clustering
- Auto clustering using IP multi-cast
- Auto clustering using DNS
- Auto clustering using ETCD
- Auto clustering using K8S




## Can I forward MQTT messages EMQX to other MQTT broker, like RabbitMQ?

**Tags:** [*RabbitMQ*](tags.md#rabbitmq)  [*Bridge*](tags.md#bridge)  [*Persistence*](tags.md#persistence)


EMQX support forward messages to other MQTT broker. Using MQTT bridge, EMQX can forward messages of interested topics to other broker.




## Can I forward messages from EMQX to MQTT services hosted on public cloud?

**Tags:** [*Bridge*](tags.md#bridge)  [*Cloud*](tags.md#cloud)


EMQX can forward messages to IoT Hub hosted on public cloud, this is a feature of EMQX bridge.




## Can other MQTT broker (for example Mosquitto) forward messages to EMQX?

**Tags:** [*Mosquitto*](tags.md#mosquitto)  [*Bridge*](tags.md#bridge)


EMQX can receive messages from other broker, but it depends also on the implementation of other brokers, Mosquitto can forward messages to EMQX, please refer to [TODO](https://www.emqx.io)。




## What should I do if I want trace the subscription and publish of some particular message?

**Tags:** [*Trace*](tags.md#trace)


EMQX support the tracing of messages from particular client or under particular topic. You can use the command line tool `emqx_ctl` for tracing. The example below shows how to trace messages under 'topic' and save the result in 'trace_topic.log'. For more details, please refer to EMQX document.

```
./bin/emqx_ctl trace topic "topic" "trace_topic.log"
```




## Does EMQX support encrypted connection? What is the recommended deployment?

**Tags:** [*TLS*](tags.md#tls)


EMQX Support SSL/TLS. In production, we recommend to terminate the TLS connection by Load Balancer. By this way, the connection between device and server(load balancer) use secured connection, and connection between load balancer and EMQX nodes use general TCP connection.




## How to troubleshoot if EMQX can't start after installation?

**Tags:** [*Debug*](tags.md#debug)


Execute `$ emqx console` to view the output.

- `logger` command is missing

  ```
  $ emqx console
  Exec: /usr/lib/emqx/erts-10.3.5.1/bin/erlexec -boot /usr/lib/emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /usr/lib/emqx/erts-10.3.5.1/../lib -mnesia dir "/var/lib/emqx/mnesia/emqx@127.0.0.1" -config /var/lib/emqx/configs/app.2019.07.23.03.07.32.config -args_file /var/lib/emqx/configs/vm.2019.07.23.03.07.32.args -vm_args /var/lib/emqx/configs/vm.2019.07.23.03.07.32.args -- console
  Root: /usr/lib/emqx
  /usr/lib/emqx
  /usr/bin/emqx: line 510: logger: command not found
  ```

  **Solution:**

  - `Centos/Redhat`

    ```
    $ yum install rsyslog
    ```

  - `Ubuntu/Debian`

    ```
    $ apt-get install bsdutils
    ```

- `Missssl` is missing

```
    $ emqx console
    Exec: /emqx/erts-10.3/bin/erlexec -boot /emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /emqx/erts-10.3/../lib -mnesia dir "/emqx/data/mnesia/emqx@127.0.0.1" -config /emqx/data/configs/app.2019.07.23.03.34.43.config -args_file /emqx/data/configs/vm.2019.07.23.03.34.43.args -vm_args /emqx/data/configs/vm.2019.07.23.03.34.43.args -- console
    Root: /emqx
    /emqx
    Erlang/OTP 21 [erts-10.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:32] [hipe]

    {"Kernel pid terminated",application_controller,"{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}},{kernel,start,[normal,[]]}}}"}
    Kernel pid terminated (application_controller) ({application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}},{kernel,start,[normal,[]]}}})

    Crash dump is being written to: log/crash.dump...done
```

**Solution:** Install openssl above version 1.1.1

- `License` file is missing

```
  $ emqx console
  Exec: /usr/lib/emqx/erts-10.3.5.1/bin/erlexec -boot /usr/lib/emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /usr/lib/emqx/erts-10.3.5.1/../lib -mnesia dir "/var/lib/emqx/mnesia/emqx@127.0.0.1" -config /var/lib/emqx/configs/app.2019.07.23.05.52.46.config -args_file /var/lib/emqx/configs/vm.2019.07.23.05.52.46.args -vm_args /var/lib/emqx/configs/vm.2019.07.23.05.52.46.args -- console
  Root: /usr/lib/emqx
  /usr/lib/emqx
  Erlang/OTP 21 [erts-10.3.5.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:32] [hipe]

  Starting emqx on node emqx@127.0.0.1
  Start http:management listener on 8080 successfully.
  Start http:dashboard listener on 18083 successfully.
  Start mqtt:tcp listener on 127.0.0.1:11883 successfully.
  Start mqtt:tcp listener on 0.0.0.0:1883 successfully.
  Start mqtt:ws listener on 0.0.0.0:8083 successfully.
  Start mqtt:ssl listener on 0.0.0.0:8883 successfully.
  Start mqtt:wss listener on 0.0.0.0:8084 successfully.
  EMQX Broker 3.2.1 is running now!
  "The license certificate is expired!"
  2019-07-23 05:52:51.355 [critical] The license certificate is expired!
  2019-07-23 05:52:51.355 [critical] The license certificate is expired! System shutdown!
  Stop mqtt:tcp listener on 127.0.0.1:11883 successfully.
  Stop mqtt:tcp listener on 0.0.0.0:1883 successfully.
  Stop mqtt:ws listener on 0.0.0.0:8083 successfully.
  Stop mqtt:ssl listener on 0.0.0.0:8883 successfully.
  Stop mqtt:wss listener on 0.0.0.0:8084 successfully.
  [os_mon] memory supervisor port (memsup): Erlang has closed
  [os_mon] cpu supervisor port (cpu_sup): Erlang has closed
```

  **Solution:** Go to [emqx.io](https://emqx.io) to apply for a license or install the open source version of EMQX Broker




## Use of ssl resumption session in EMQX

**Tags:** [*Performance*](tags.md#performance)


Modify the reuse_sessions = on in the emqx.conf configuration and take effect. If the client and the server are successfully connected through SSL, when the client connection is encountered for the second time, the SSL handshake phase is skipped, the connection is directly established to save the connection time and increase the client connection speed.




## MQTT client disconnect statistics

**Tags:** [*Metrics*](tags.md#metrics)


Execute `emqx_ctl listeners` to view the `shutdown_count` statistics under the corresponding port.

Client disconnect link error code list:

- `keepalive_timeout`：MQTT keepalive time out

- `closed`： TCP client disconnected (the FIN sent by the client did not receive the MQTT DISCONNECT)

- `normal`： MQTT client is normally disconnected

- `einval`：EMQX wants to send a message to the client, but the Socket has been disconnected

- `function_clause`：MQTT packet format error

- `etimedout`：TCP Send timeout (no TCP ACK response received)

- `proto_unexpected_c`：Repeatedly received an MQTT connection request when there is already an MQTT connection

- `idle_timeout`： After the TCP connection is established for 15s, the connect packet has not been received yet.

## Can EMQX guarantee the original order when forwarding the messages to subscribers?

**Tags:** [*Pub/Sub*](tags.md#pub-sub)

EMQX will ensure that messages with the same topic from the same client are forwarded in the order of their arrival, regardless of the QoS level of the messages. The QoS level does not affect the order of message forwarding. Whether messages are lost or duplicated, they will not cause message disorder. This is also the requirement from MQTT.

However, EMQX does not guarantee the forwarding order of messages from different topics. We can consider them as entering different channels. For example, messages from topic A arrive at EMQX before messages from topic B, it is possible that messages from topic B will be forwarded earlier.

## What should I do when I encounter problems related to client connection, publishing and subscribing?

**Tags:** [*Debug*](tags.md#debug)

EMQX's debug logs already record all the behaviors and phenomena. By reading the debug logs, we can know when the client initiated the connection, which parameters were specified during the connection, whether the connection was successful, and the reasons for connection rejection, etc. However, due to the excessive information recorded in debug logs, it may incur additional resource consumption and make it difficult to analyze individual clients or topics.

So EMQX provides a [Log Trace](../getting-started/log.md) feature. We can specify the clients or topics we want to trace, and EMQX will output all the debug logs related to those clients or topics to the designated log file. This makes it convenient for both self-analysis and seeking help from the community.

Just so you know, if the client cannot connect to EMQX for network reasons, the log tracing feature will not be helpful, because EMQX has not received any messages in such cases. This situation often occurs due to network configuration issues such as firewalls or security groups, which result in the server port not being open. This is particularly common when deploying EMQX on cloud instances. So, in addition to log tracing, we can troubleshoot network-related issues by checking port occupation, listening status, and network configurations.

## Why are there client IDs like "CENSYS" or other unfamiliar clients?

**Tags:** [*Security*](tags.md#security)

CENSYS is an internet scanning and reconnaissance tool that periodically scans the IPv4 address space to discover default ports for protocols such as HTTP, SSH, MQTT, etc. So if you find MQTT clients with a client ID of "CENSYS" or other unknown clients accessing your MQTT broker, it indicates that you are currently at a relatively lower level of security protection. The following measures can effectively help you avoid this issue:

1. Avoid using default configurations, such as the AppID and AppSecret used for verifying HTTP API access permissions in EMQX.
2. Enable authentication, which can be password-based authentication or JWT authentication, to prevent situations where only knowledge of an IP address is required for login.
3. Enable TLS mutual authentication, allowing only clients with valid certificates to access the system.
4. Enable authorization to prevent unauthorized devices from accessing sensitive data after logging in.
5. Configure your firewall to close unnecessary ports as much as possible.

## Can I shared subscribe to the system topic?

**Tags:** [*Shared Subscription*](tags.md#shared-subscription)

Yes, some system messages, such as client online/offline events, may be published frequently, so using a shared subscription is essential for clients. Example subscription: `$share/group1/$SYS/brokers/+/clients/+/connected`.

## Why can't I receive retained messages when using shared subscriptions?

**Tags:** [*Shared Subscription*](tags.md#shared-subscription)

The MQTT protocol stipulates that when the client uses a shared subscription, the server cannot send retained messages to it.

## Why are messages occasionally lost when using shared subscriptions?

**Tags:** [*Shared Subscription*](tags.md#shared-subscription)

When the connection of the shared subscriber is disconnected but the session still exists, the server will continue delivering the messages to the shared subscriber, and these messages will be temporarily stored in the corresponding session. This will cause other online shared subscribers to appear to have not consumed all the messages. In addition, if the shared subscriber chooses to create a new session when reconnecting, the messages cached in the old session will be permanently lost.

If we confirm that the above situation does not exist, but the problem of message loss still exists, then we can use the client tracking function of EMQX for further investigation.

## What should I do if EMQX prompts that the port is occupied (eaddrinuse) when starting?

**Tags:** [*Failed to Start*](tags.md#failed-to-start)

By default, EMQX will occupy 7 ports when it starts. They are:

1. 1883, used for MQTT over TCP listener, can be modified by configuration
2. 8883, used for MQTT over SSL/TLS listener, can be modified by configuration
3. 8083, used for MQTT over WebSocket listener, can be modified by configuration
4. 8084, for MQTT over WSS (WebSocket over SSL) listener, can be modified by configuration
5. 18083, the default listening port of the HTTP API service. And the dashboard also depends on this port, which can be modified by configuration
6. 4370, used for remote function calls of EMQX distributed cluster, Mnesia data synchronization, etc. This port will be occupied by default even if no cluster is formed. The listening port should be `BasePort (4370) + Offset`, 4370 is fixed and cannot be modified, and Offset is determined by the numeric suffix of the Name part in the node name (`Name@Host`). If there is no numeric suffix, it defaults to 0. For example, the Offset of `emqx@127.0.0.1` is 0, and the Offset of `emqx1@127.0.0.1` is 1.
7. 5370, the cluster RPC port used to share the pressure of the last port, mainly used for forwarding MQTT messages between nodes. Similar to port 4370, even if no cluster is formed, this port will be occupied by default, and it should actually be `BasePort (5370) + Offset`, 5370 is fixed and cannot be modified, and Offset is determined by the Name part of the node name (`Name@Host`). If there is no number suffix, it defaults to 0.

## Why does EMQX output the log "WARNING: Default (insecure) Erlang cookie is in use." when it starts?

**Tags:** [*Log*](tags.md#log)

The complete WARNING log is as follows:

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

Only EMQX nodes using the same cookie can form a cluster. While a cookie does not secure cluster communication, it prevents a node from connecting to a cluster it did not intend to communicate with. EMQX nodes uniformly use `emqxsecretcookie` as cookie by default, so we recommend users change the cookie value when building a cluster.

The second warning log indicates two ways to modify the cookie: `node.cookie` in the `emqx.conf` configuration file and the environment variable `EMQX_NODE__COOKIE`.

## When using Docker to deploy EMQX, why does the restart of the container cause data loss, such as configured rules and resources?

**Tags:** [*Persistence*](tags.md#persistence)

The runtime data of EMQX are all stored in the `/opt/emqx/data` directory, such as the configuration of rules and resources, retained messages, etc. We need to mount the `/opt/emqx/data` directory to a local host directory or a data volume to ensure that these data will not be lost due to container restarts.

But we may find that even if the `/opt/emqx/data` directory has been mounted, the data may still be lost after the container restarts. This is because the runtime data of EMQX is stored in the `/opt/emqx/data/mnesia/${Node Name}` directory. So the data loss is actually that the node name of EMQX has changed after the container is restarted, which in turn caused EMQX to create a new storage directory.

EMQX node name consists of Name and Host, where Host comes from the container's IP address by default. Under the default network configuration, the container's IP may change after restarting, so we have to keep the container's IP fixed.

EMQX provides an environment variable `EMQX_HOST`, which allows us to set the Host part of the node name. Of course, the premise is that this Host must be connectable to other nodes, so we also need to use it with the network alias:

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.0.24
```

## Reasons for common logs

**Tags:** [*Log*](tags.md#log)

### Keyword: “reason: {shutdown, takeovered}”

In EMQX, each MQTT client connection is maintained by an independent process. Whenever an MQTT client connects, EMQX will create a new process. If the client wants to restore communication from the existing session state, EMQX needs to migrate the session state from the old process to the new one. We call this process: Take over, and the old process will be closed after the takeover is completed, and the reason for closing is `{shutdown, takeovered}`. If the original client is still connected, EMQX will send a DISCONNECT packet with Reason Code 0x8E (Session taken over), and then close the old connection.

### Keyword: “reason: {shutdown, discarded}”

Contrary to session takeover, if the client indicates that it wants to start a new session when connecting, then EMQX needs to discard the old session state, and EMQX will close the old process with the reason `{shutdown, discarded}`. If the original client is still connected, EMQX will send a DISCONNECT packet with Reason Code 0x8E (Session taken over), and then close the old connection.

### Keyword: “reason: {shutdown, kicked}”

Indicates that the client connection was manually kicked out (click the Kick Out button on the Dashboard or call the `DELETE clients/{clientid}` API). If the original client is still connected, EMQX will send a DISCONNECT packet with Reason Code 0x98 (Administrative action), and then close the old connection.

### Keyword: “reason: {shutdown, tcp_closed}”

This log indicates that the client directly closed the network connection without sending a DISCONNECT packet. The corresponding reason would be `{shutdown, normal}`, which means that the client first sent a DISCONNECT packet with a Reason Code of 0 and then closed the network connection.

Therefore, when you encounter that `tcp_closed` appears in the EMQX logs, and this disconnection behavior is not what you expected, it is recommended to investigate the client's implementation for any potential issues.

### Keyword: “maximum heap size reached”

The appearance of this log indicates that the heap occupied by the current client process has exceeded the preset maximum value, and the client process will be forcibly killed to ensure the availability of EMQX, and avoid the unlimited growth of the memory usage of the client process and eventually lead to EMQX OOM. 

Usually, message accumulation is the main reason for the increase in client process heap occupation, and message accumulation is generally because the client's consumption capacity does not match the peer's message production capacity. The recommended solution is to optimize the client processing code or use shared subscriptions to disperse the load.

The related configuration item is `force_shutdown_policy`, and its format is `<Maximum Message Queue Length>|<Maximum Heap Size>`, for example, `10000|64MB`.  `<Maximum Heap Size>` refers to the maximum heap memory that each client process can occupy.

### Keyword: “Parse failed for function_clause”

This log indicates that the parsing of the packet failed. Maybe because it is not an MQTT packet, we have encountered many situations where HTTP requests are sent to the MQTT port, or because the packet contains non-UTF-8 characters. We can search the `Frame data` keyword in this "Parse failed..." log to view the complete message and help us analyze the possible reasons for the parsing failure.

### Keyword: “Dropped msg due to mqueue is full”

EMQX can send multiple unconfirmed QoS 1 and QoS 2 messages at the same time, but limited by the performance of the client, we usually limit the maximum number of messages allowed to be sent at the same time. After reaching this limit, subsequent arriving messages will be cached by EMQX in the message queue of each client process.

To prevent excessive message buffering, EMQX also sets a maximum length limit for the message queue. If the message queue is full when a message arrives, the latest message will still be enqueued, but the oldest message in the queue will be dequeued and dropped, and EMQX will generate the log message: "Dropped msg due to mqueue is full."

If this log only appears during traffic peaks, you can modify the configuration item `max_mqueue_len` only to increase the maximum length of the message queue. However, if it continues to appear, it indicates that the current client consumption capability is poor. In such cases, it is recommended to optimize the client's code or use shared subscriptions to distribute the load.