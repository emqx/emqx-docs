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


EMQX Broker is free and it can be download at [https://www.emqx.com/en/downloads?product=broker](https://www.emqx.com/en/downloads?product=broker).

EMQX Enterprise can be downloaded and evaluated for free.  You can download it from [https://www.emqx.com/en/downloads?product=enterprise](https://www.emqx.com/en/downloads?product=enterprise), and then apply trial license at [https://www.emqx.com/en/licenses#trial](https://www.emqx.com/en/licenses#trial).

Also you can use the EMQX enterprise version through public cloud service.

- [AWS](https://aws.amazon.com/marketplace/pp/B07N2ZFVLX?qid=1552872864456&)




## How to update EMQX license?

**Tags:** [*Update*](tags.md#update)


You need to two steps：

1. After clicking "Download License", browse to the "license.zip" file that you downloaded.

2. Copy the two files(emqx.lic, emqx.key) in the zip file to the EMQX license directory.
  - If your installation package is a zip file, the licenses are under "emqx/etc/"; 
  - For DEB/RPM package, the licenses are under "/etc/emqx/";
  - For Docker image, the licenses are under "/opt/emqx/etc/".

After the copy is completed, the license needs to be reloaded from the command line to complete the update：

```
emqx_ctl license reload [license file path]
```

The update commands for different installation modes：

```
## zip packages
./bin/emqx_ctl license reload etc/emqx.lic

## DEB/RPM
emqx_ctl license reload /etc/emqx/emqx.lic

## Docker
docker exec -it emqx-ee emqx_ctl license reload /opt/emqx/etc/emqx.lic
```




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


The EMQX Enterprise edition supports data persistence. Supported databases are:

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


The EMQX Enterprise edition integrates a Kafka bridge, it can bridge data to Kafka.




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