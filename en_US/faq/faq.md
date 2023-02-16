# FAQ

## Which products do we offer?

EMQX has [3 products](https://www.emqx.com/en/products/emqx) in total. The different products support different number of connections, features, services, etc.

- EMQX Broker: EMQX open source version. It supports the popular IoT protocols MQTT, CoAP and LwM2M.
- EMQX Enterprise: EMQX enterprise version. It is based on the open source version, and adds data persistence (support Redis, MySQL, MongoDB or PostgreSQL), data bridge to Kafka, LoRaWAN support, EMQX monitoring, Kubernetes deployment etc. It supports 1 million concurrent MQTT connections.
- EMQX Cloud: [EMQX Cloud](https://www.emqx.com/cloud) is an MQTT middleware for the IoT from EMQ. As the world's first fully managed MQTT 5.0 public cloud service, EMQX Cloud provides a one-stop O&M colocation and a unique isolated environment for MQTT services. In the era of Internet of Everything, EMQX Cloud can help you quickly build industry applications and easily realize the collection, transmission, computation and persistence of IoT data.

## What are EMQX's authentication options?

When a client connects to an EMQX server, the EMQX server can authenticate it in different ways. EMQX supports the following 3 approaches:

- Username and password: A client connection can be established only when passing the correct user name and password (which can be configured at server).

- ClientID: Every MQTT client will have a unique ClientID. A list of acceptable ClientIDs can be configured for server. Only ClientIDs in this list can be authenticated successfully.

- Anonymous: Allows anonymous access.

Besides using the configuration file (to configure authentication), EMQX can also use database and integration with external applications, such as MySQL, PostgreSQL, Redis, MongoDB, HTTP and LDAP.

## What's a mqueue? How to configure mqueues in EMQX?

A mqueue is a message queue that store messages for a session. If the clean session flag is set to false in the MQTT connect packet, then EMQX would maintain the session for the client even when the client has been disconnected from EMQX. Then the session would receive messages from the subscribed topic and store these messages into the sessions mqueue. And when the client is online again, these messages would be delivered to the client instantly. Because of low priority of QOS 0 message in mqtt protocol, EMQX do not save QOS 0 message in the mqueue. However, this behavior can be overridden by setting `zone.$name.mqueue_store_qos0 = true` in `emqx.conf`. With the `zone.$name.mqueue_store_qos0 = true`, even a QOS 0 message would been saved in the mqueue. The maximum size of the mqueue can be configured with the setting `zone.external.max_mqueue_len`. Notice that the mqueue is stored in memory, so please do not set the mqueue length to 0 (0 means there is no limit for mqueue), otherwise the system would risk running out of memory.

## What's a WebSocket? When to use a Websocket to connect to EMQX?

WebSocket is a full-duplex communication protocol with an API supported by modern web browsers. A user can use the WebSocket API to create a dual direction communication channel between a web browser and a server. Through a WebSocket, the server can push messages to the web browser. EMQX provides support for WebSocket. This means that users can publish to MQTT topics and subscribe to MQTT topics from browsers.

## What's a shared subscription, and when is it useful?

Shared subscription is an MQTT feature that was introduced in MQTT 5.0 specification. Before the feature was introduced in MQTT 5.0 specification, EMQ 2.x already supported the feature as a non-standard MQTT protocol. In general, all of subscribers will receive ALL messages for the subscribed topics. However, clients that share a subscription to a topic will receive the messages in a round-robin way, so only one of the clients that share a subscription will receive each message. This feature can thus be used for load-balancing.

Shared subscription is very useful in data collection and centralized data analysis applications. In such cases, the number of data producers is much larger than consumers, and one message ONLY need to be consumed once.

## What is off-line message?

Usually an MQTT client receives messages only when it is connected to an EMQX broker, and it will not receive messages if it is off-line. But if a client has a fixed ClientID, and it connects to the broker with clean_session = false, the broker will store particular messages for it when it is off-line. If the Pub/Sub is done at certain QoS level (broker configuration), these messages will be delivered when this client is reconnected.

Off-line messages are useful when the connection is not stable, or the application has special requirements on QoS.

## What is Subscription by Broker? And its use scenario?

Usually an MQTT client has to subscribe to the topics explicitly by itself, if it wants to receive the messages under these topics. Subscription by Broker means that the broker can subscribe to particular topics for a client without client's interaction. The relation of such clients and the topics they should be subscribed to is stored at broker side.

Usage of Subscription by Broker can ease the management of massive clients, and save computational resources and bandwidth for devices.

## What is the usage of system topics? What system topics are available?

The system topics have a prefix of `$SYS/`. Periodically, EMQX publishes system messages to system topics, these messages include system status, statistics, client's online/offline status and so on.

Here are some examples of system topics (for a complete list of system topic please refer to EMQX documentation):

- $SYS/brokers: List of nodes in cluster
- $SYS/brokers/${node}/clients/${clientid}/connected: this message is published when a client connects
- $SYS/broker/${node}/stats/connections/count: Number of connections on a node
- $SYS/broker/${node}/stats/sessions/count: Number of sessions on a node

## Incompatible openssl version

For better security, starting from version 4.3, EMQX runs on openssl-1.1.
This may cause some troulbes for users running EMQX on some old linux distributions,

### Error

If starting EMQX with command `./bin/emqx console` result in below error messages:

```bash
FATAL: Unable to start Erlang.
Please make sure openssl-1.1.1 (libcrypto) and libncurses are installed.
```

Or for emqx version earlier to v4.3.10 and emqx-enterprise version earlier than e4.3.5

```bash
\{application_start_failure,kernel,\{\{shutdown,\{failed_to_start_child,kernel_safe_sup,\{on_load_function_failed,crypto\}\}\}, ..\}
```

It indicates that the "crypto" application in Erlang/OTP that EMQX depends on failed to start because the required openssl dynamic lib (.so) is not found.

### Solution

#### CentOS (install from epel-relese, using centos7 as example)

Extra Packages for Enterprise Linux (or EPEL) is a Fedora Special Interest Group that creates, maintains, and manages a high quality set of additional packages for Enterprise Linux.

1. To install the RPM repos, execute `yum install epel-release`
1. If failed to install, follow the instructions here: https://docs.fedoraproject.org/en-US/epel/#_el7 to ensure the yum repos are added, and try step 1 again
1. Execute `yum install openssl11` to install openssl-1.1

#### Linux (compile openssl-1.1 from source code)

Go to the installation directory of EMQX (If you use the package management tool to install EMQX, you should enter the same level directory as the `lib` of EMQX)

```bash
  ## Package installation
$ cd emqx

  ## Package manager installation, such as yum. Its lib directory should be in /lib/emqx
$ cd /lib/emqx
```

Query the list of `.so` dynamic libraries that `crypto` depends on and its location in memory:

```bash
$ ldd lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.6/priv/lib/crypto.so: /lib64/libcrypto.so.10: version `OPENSSL_1.1.1' not found (required by lib/crypto-4.6/priv/lib/crypto.so)
          linux-vdso.so.1 =>  (0x00007fff67bfc000)
          libcrypto.so.10 => /lib64/libcrypto.so.10 (0x00007fee749ca000)
          libc.so.6 => /lib64/libc.so.6 (0x00007fee74609000)
          libdl.so.2 => /lib64/libdl.so.2 (0x00007fee74404000)
          libz.so.1 => /lib64/libz.so.1 (0x00007fee741ee000)
          /lib64/ld-linux-x86-64.so.2 (0x00007fee74fe5000)

```

Among them, `OPENSSL_1.1.1' not found` indicates that the `.so` library of specified OPENSSL version is not installed correctly.

Compile and install OPENSSL 1.1.1 from source code, and place its so file to a path recognized by the system:

```bash
## Download the latest version 1.1.1
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## Upload to ct-test-ha
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## Unzip, compile and install
$ tar zxf   openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test   		# Perform test; continue if PASS is output
$ make install

## Ensure library references
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

After the completion, execute `ldd lib/crypto-*/priv/lib/crypto.so` in the lib-level directory of EMQX to check whether it can be correctly identified. If there is no `.so` library in `not found`, you can start EMQX normally.

#### macOS

Go to the installation directory of EMQX:

```bash
  ## package installation
$ cd emqx

  ## brew installation
$ cd /usr/local/Cellar/emqx/<version>/
```

Query the list of `.so` dynamic libraries that `crypto` depends on:

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
  /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

It shows that OPENSSL has been successfully installed to the specified directory by checking:

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

If the file does not exist, you need to install the version of OPENSSL corresponding with what printed by `otool`. For example, it shown here as `openssl@1.1`:

```bash
$ brew install openssl@1.1
```

After the installation is complete, you can start EMQX normally.

{% emqxce %}

## MSVCR120.dll is missing from Windows

### Phenomenon

When Windows executes `./bin/emqx console`, an error window pops up:

```bash
This program cannot be started because MSVCR120.dll is missing from the computer. Please try to reinstall the program to resolve this issue.
```

### Solution

Install [Microsoft Visual C++ RedistributablePackage](https://www.microsoft.com/en-us/download/search.aspx?q=redistributable+package.)

{% endemqxce %}

## SSL Connection Error

### Phenomenon

The client cannot establish an SSL connection with EMQX.

### Solution

You can use the keywords in the EMQX log to perform simple troubleshooting. For the EMQX log related content, please refer to: [Log and Trace](../observability/tracer.md).

1. certificate_expired

   The `certificate_expired` keyword appears in the log, indicating that the certificate has expired, please renew it in time.

2. no_suitable_cipher

   The `no_suitable_cipher` keyword appears in the log, indicating that a suitable cipher suite was not found during the handshake process. The possible reasons are that the certificate type does not match the cipher suite, the cipher suite supported by both the server and the client was not found, and so on.

3. handshake_failure

   The `handshake_failure` keyword appears in the log. There are many reasons, which may be analyzed in conjunction with the error reported by the client. For example, the client may find that the connected server address does not match the domain name in the server certificate.

4. unknown_ca

   The `unknown_ca` keyword appears in the log, which means that the certificate verification fails. Common reasons are that the intermediate CA certificate is omitted, the Root CA certificate is not specified, or the wrong Root CA certificate is specified. In the two-way authentication, we can judge whether the certificate configuration of the server or the client is wrong according to other information in the log. If there is a problem with the server certificate, the error log is usually:

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify received CLIENT ALERT: Fatal - Unknown CA\n"}}}
   ```

   When you see `CLIENT ALERT`, you can know that this is a warning message from the client, and the server certificate fails the client's check.

   If there is a problem with the client certificate, the error log is usually:

   ```
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify at ssl_handshake.erl:1887 generated SERVER ALERT: Fatal - Unknown CA\n"}}}
   ```

   When you see `SERVER ALERT`, you can know that the server finds that the certificate cannot pass the authentication when checking the client certificate, and the client will receive a warning message from the server.

5. protocol_version

   The `protocol_version` keyword appears in the log, indicating a mismatch between the TLS protocol versions supported by the client and server.

## How to use EMQX?

EMQX Broker is free and it can be download at [https://www.emqx.com/en/try?product=broker](https://www.emqx.com/en/try?product=broker).

EMQX Enterprise can be downloaded and evaluated for free. You can download it from [https://www.emqx.com/en/try?product=enterprise](https://www.emqx.com/en/try?product=enterprise), and then apply trial license at [https://www.emqx.com/en/apply-licenses/emqx](https://www.emqx.com/en/apply-licenses/emqx).

Also you can use the EMQX enterprise version through public cloud service.

- [AWS](https://aws.amazon.com/marketplace/pp/B07N2ZFVLX?qid=1552872864456&)

## How to update EMQX license?

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
On a multi-node cluster, the `emqx_ctl license reload` command needs to be executed only on one of the nodes, as the license will be replicated and applied to all members. Each one will contain a copy of the new license under the configured data directory for EMQX, as well as a backup of the old license, if any.

Note that this command only takes effect _on the local node_ executing the command for EMQX versions prior to e4.3.10, so this command will require being executed on each node of the cluster for those older versions.
:::

## Can EMQX support customized protocols? How to implement?

TODO...

## Can I capture device online and offline events? How to use it?

EMQX supports to capture device online and offline events through below 3 approaches,

- Web Hook

- Subscribe related $SYS topics

  - $SYS/brokers/${node}/clients/${clientid}/connected
  - $SYS/brokers/${node}/clients/${clientid}/disconnected

- Directly save events into database

  The final approach is only supported in enterprise version, and supported database includes Redis, MySQL, PostgreSQL, MongoDB and Cassandra. User can configure database, client.connected and client.disconnected events in the configuration file. When a device is online or offline, the information will be saved into database.

## I want to control topics can be used for specific clients, how to configure it in EMQX?

EMQX can constrain clients used topics to realize device access controls. To use this feature, ACL (Access Control List) should be enabled, disable anonymous access and set `acl_nomatch` to 'deny' (For the convenience of debugging, the last 2 options are enabled by default, and please close them).

ACL can be configured in configuration file, or backend databases. Below is one of sample line for ACL control file, the meaning is user 'dashboard' can subscribe '$SYS/#' topic. ACL configuration in backend databases is similar, refer to EMQX document for more detailed configurations.

```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
```

## Can EMQX support traffic control?

Yes. Currently EMQX supports to control connection rate and message publish rate. Refer to below for sample configuration.

```
## Value: Number
listener.tcp.external.max_conn_rate = 1000

## Value: rate,burst
listener.tcp.external.rate_limit = 1024,4096
```

## How does the EMQX achieve high concurrency and high availability?

High concurrency and availability are design goals of EMQX. To achieve these goals, several technologies are applied:

- Making maximum use of the soft-realtime, high concurrent and fault-tolerant Erlang/OTP platform;
- Full asynchronous architecture;
- Layered design of connection, session, route and cluster;
- Separated messaging and control panel;

With the well design and implementation, a single EMQX cluster can handle million level connections.

EMQX supports clustering. The EMQX performance can be scale-out with the increased number of nodes in cluster, and the MQTT service will not be interrupted when a single node is down.

## Can EMQX store messages to database?

The EMQX Enterprise supports data persistence. Supported databases are:

- Redis
- MongoDB
- MySQL
- PostgreSQL
- Cassandra

## Can I disconnect an MQTT connection from EMQX server?

Yes. You can do it by invoking REST API provided by EMQX, but the implementation is different in EMQX 2.x and 3.x:

- EMQX customized protocol in 2.x versions.
- Follow the process defined in MQTT 5.0 protocol after version 3.0.

Refer to below for API invocation:

```html
HTTP Method: DELETE URL：api/[v2|v3]/clients/{clientid}
<!--Please notice the 2nd section in URL, and use the correct version number according to your EMQX version. -->

Returned response: { "code": 0, "result": [] }
```

## Can EMQX forward messages to Kafka?

The EMQX Enterprise integrates a Kafka bridge, it can bridge data to Kafka.

## I use Kafka bridge in EMQX enterprise, when will the MQTT Ack packet sent back to client? Is the time when message arriving EMQX or after getting Ack message from Kafka?

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

EMQX supports cluster auto discovery. EMQX clustering can be done manually or automatically.

Currently supported clustering methods:

- Manual clustering
- Static clustering
- Auto clustering using IP multi-cast
- Auto clustering using DNS
- Auto clustering using ETCD
- Auto clustering using K8S

## Can I forward MQTT messages EMQX to other MQTT broker, like RabbitMQ?

EMQX support forward messages to other MQTT broker. Using MQTT bridge, EMQX can forward messages of interested topics to other broker.

## Can I forward messages from EMQX to MQTT services hosted on public cloud?

EMQX can forward messages to IoT Hub hosted on public cloud, this is a feature of EMQX bridge.

## Can other MQTT broker (for example Mosquitto) forward messages to EMQX?

EMQX can receive messages from other broker, but it depends also on the implementation of other brokers, Mosquitto can forward messages to EMQX, please refer to [TODO](https://www.emqx.io)。

## What should I do if I want trace the subscription and publish of some particular message?

EMQX support the tracing of messages from particular client or under particular topic. You can use the command line tool `emqx_ctl` for tracing. The example below shows how to trace messages under 'topic' and save the result in 'trace_topic.log'. For more details, please refer to EMQX document.

```
./bin/emqx_ctl trace topic "topic" "trace_topic.log"
```

## Does EMQX support encrypted connection? What is the recommended deployment?

EMQX Support SSL/TLS. In production, we recommend to terminate the TLS connection by Load Balancer. By this way, the connection between device and server(load balancer) use secured connection, and connection between load balancer and EMQX nodes use general TCP connection.

## How to troubleshoot if EMQX can't start after installation?

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

Modify the reuse_sessions = on in the emqx.conf configuration and take effect. If the client and the server are successfully connected through SSL, when the client connection is encountered for the second time, the SSL handshake phase is skipped, the connection is directly established to save the connection time and increase the client connection speed.

## MQTT client disconnect statistics

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

## What's EMQX suggested OS?

EMQX supports deployment on Linux, MacOS, ARM system, however it is recommended to deploy the product on one of the supported Linux distributions, such as CentOS, Ubuntu and Debian.

Only EMQX Broker supports Windows and we don't suggestion deployment on Windows.
## How to estimate resource usage of EMQX?

The following factors will have an impact on EMQX resource consumption, mainly on CPU and memory usage.

- Number of connections: EMQX creates 2 Erlang process for each MQTT connection, and every Erlang process consumes some resource. The more connections, the more resources are required.

- Average throughput: Throughput means (pub message number + sub message number) processed by EMQX per second. With higher throughput value, more resource will be used for handling route and message delivery in EMQX.

- Payload size: With bigger size of payload, more memory and CPU are required for message cache and processing.

- Number of topics: With more topics, the route table in EMQX will increase, and more resource is required.

- QoS: With higher message QoS level, more resource will be used for message handling.

If client devices connect to EMQX through TLS, more CPU resource is required for encryption and decryption. Our suggested solution is to add a load balancer in front of EMQX nodes, the TLS is offloaded at load balance node, connections between load balancer and backend EMQX nodes use plain TCP connections.

You can use our online calculation tool [https://www.emqx.com/en/server-estimate](https://www.emqx.com/en/server-estimate) to estimate the resource consumption.

## When I was executing stress test, the connection number and throughput are lower than expected. How can I tune the system to make full use of it?

When executing a stress test, besides ensuring the necessary hardware resource, it is also necessary to tune the OS and the Erlang VM to make the maximum use of the resource. The most common tuning is to modify the global limitation of file handles, the user limitation of file handles, the TCP backlog and buffer, the limitation of process number of Erlang VM and so on. You will also need to tune the client machine to ensure it has the ability and resource to handle all the subs and pubs.

Different use cases require different tuning. In the EMQX document there is a chapter about tuning the system for general purpose. [TODO](https://www.emqx.io)

## My connections number is small, do I still need to deploy multiple nodes in production?

Even when the connection number is low, or message rate is low, it still makes sense to deploy a cluster with multiple nodes in production. Clustering improves the availability of system: when a single node goes down, the rest of the nodes in the cluster ensure that the service is not interrupted.
