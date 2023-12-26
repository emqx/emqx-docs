# Version 4 


## 4.4.23

*Release Date: 2023-11-24*

### Enhancements

- Improved the performance of message sending between EMQX nodes.

  `gen_rpc` is the RPC channel used internally by EMQX for sending MQTT messages between nodes. In this improvement, we optimized the `gen_rpc`'s ability to handle backlogged messages in the channel, allowing the system to recover more quickly from traffic peaks.

### Bug Fixes

- Fixed the issue that the rule engine could not connect to [upstash](https://upstash.com/) Redis.

  Before the fix, after establishing a TCP connection with the Redis service, the Redis driver of emqx used [Inline Commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) to send AUTH and SELECT commands. However, the upstash Redis service did not support Inline Commands, which caused the rule engine to fail to connect to the upstash Redis service.
  After the fix, the redis driver of emqx uses RESP (REdis Serialization Protocol) to send AUTH and SELECT commands.

- Added validity check for some parameters of the "Offline Msg to Redis" action and Redis resource.

  * Checked the "Redis Key TTL" parameter of the "Offline Msg to Redis" action.
  * Checked the "Redis Database" parameter of the Redis resource.

## 4.4.22

*Release Date: 2023-11-01*

### Enhancements

- Added the Audit Log feature to track important operation changes.

  - To enable the Audit Log, click **General** -> **Audit Log** -> **Enable** on Dashboard, modify the parameters, and click **Add**.
  - Once enabled, all HTTP requests except `GET` and CLI executions will be recorded.
  - The most recent 5,000 audit log entries are visible by default in the Dashboard, while the complete log file is stored in the `data/audit` directory.

- Added support for Role-Based Access Control (RBAC) roles in the dashboard. With this new functionality, users can be assigned one of two roles: "Administrator" or "Viewer" when logging into the Dashboard, each with distinct permissions.

  - Administrator Role: Administrators enjoy unrestricted access, granting them full control over all aspects of the dashboard's functionality. 
  - Viewer Role: Viewers are limited to read-only access. They can view dashboard information but are unable to make any modifications.

  RBAC ensures that the right users have the appropriate level of access, simplifying user management and access control and enhancing security and data integrity.

- LwM2M gateway supported sending downlink data using Block Wise Transfer.

- Added the new SQL functions: map_keys(), map_values(), map_to_entries(), join_to_string(), join_to_string(), join_to_sql_values_string(), is_null_var(), is_not_null_var().

  For more information on the functions and their usage, refer to the documentation.

- Added `Forward QoS` configuration option for the action of "Data bridge to MQTT Broker" to specify the QoS level of messages to be forwarded through the MQTT bridge.

- Added support for specifying the expiration time of MQTT messages via configuration file.

  See the description of the `mqtt.message_expiry_interval` configuration in the `emqx.conf` file for more details.

- Updated Erlang/OTP version to OTP-24.3.4.2-4.

- Added schema validations for better configuring OCSP Stapling and CRL Check consistency.

### Bug Fixes

- Resolved the issue causing the Kafka client (wolff) producer to crash.

  This problem occurred when a Kafka resource was inadvertently deleted during the initialization of certain rules, leading to a failure in the dependent rules. This error then propagated, triggering an error escalation mechanism, leading to the crash of all rules. The resolution prevents this propagation, ensuring the system stability.
  
- Fixed the issue that GBT32960 gateway module could not parse the `retry_interval` parameter.

- Fixed the issue that GBT32960 client was unable to fetch through the HTTP API.

- Fixed the issue that exception logs appeared when the OCPP client failed in authentication.

- Fixed the issue that OCPP gateway did not validate an empty ClientID.

- Upgraded RabbitMQ driver and fixed some security vulnerabilities.

- Fixed the issue with the GCP PubSub action in the rule engine, where the statistics counter did not increase in asynchronous sending mode.

- Fixed the issue that only the resources of the current node would reconnect when manually reconnecting resources.

- Fixed the issue that the statistics counter of actions was not reset after deleting and reimporting rules.

- Fixed the issue in cluster mode where restarting rules would result in an action resource leak.

  Before the fix, when stopping and starting rules, the action resource (some processes associated with the actions) leaked if the action creation failed on certain nodes.

- Fixed the issue of reduced performance in some data integration actions with batch mode in multi-CPU scenarios compared to versions before 4.4.5.

  In version 4.4.5, the number of workers in the batch process pool was modified to `number of CPU cores * 4`. When running on machines with a higher number of CPU cores, this resulted in an excessive number of worker processes, causing each process to accumulate relatively few messages within the specified batch time. This, in turn, led to a decrease in the performance of batch data sending.

  The fix no longer hardcodes the number of workers in the batch process pool. Instead, it introduces a new configuration option called `batch_pool_size`, with a default value of 8.

  The data integration actions affected are: data_to_cassa, data_to_clickhouse, data_to_influxdb, data_to_iotdb, data_to_lindorm, data_to_mysql, data_to_oracle, data_to_pgsql, data_to_sqlserver, data_to_tablestore, data_to_tdengine, data_to_gcp_pubsub.

- Fixed an issue in the MQTT bridge that sending QoS2 messages failed when using the MQTT 5.0 protocol.

- Fixed the issue that hot configuration updates failed when the configuration for a listener in the configuration file was missing.

- Fixed the issue of LwM2M gateway plugin startup failure.

  Before the fix, if the LwM2M module was first shut down and then the LwM2M plugin was started, it would result in a plugin startup failure. The log message was as follows:

  ```
  {emqx_lwm2m,{bad_return,{{emqx_lwm2m_app,start,[normal,[]]},{'EXIT',{{already_started,<0.3895.177>},[...]}}}}}
  ```

- Fixed the issue that the shared subscription topic prefix on the Dashboard was not displayed correctly.

  Before the fix, topics like `$share/g//t` would be displayed as `/t` on the client details page of the Dashboard, causing the shared subscription prefix to be lost. After the fix, it will be displayed correctly as `$share/g//t`.

- Added a `none` option for `peer_cert_as_username` and `peer_cert_as_clientid` in the configuration file. These two options are used to use (client) certificate content as the username/ClientID.

- Fixed the issue of occasional listener restarts when enabling hot configuration feature.

- Fixed the issue that errors occurred when stopping actively running rules.

  Before the fix, manually stopping actively running rules occasionally resulted in error logs like the following, indicating that the action was not properly initialized or had been cleared:
  ```
  foo@x.x.x.x:54663 Rule: <<"rule:ba48182b">>; Action: data_to_kafka; Resource: <<"resource:7bacacdc">>. Continue next action, reason: {error,{badmatch,not_found}, ...
  ```
  After the fix, such error logs are no longer generated, and error logs for uninitiated actions in other cases have been optimized.

- Fixed the issue of DTLS PSK handshake failure in the LwM2M gateway.

- Added checks for illegal fields in the retainer module's configuration.

  Added checks for the `Max Retained Messages` and `Max Retained Payload Size` fields to ensure they are non-negative values.

- Fixed the issue of failing to send messages to TDEngine after hot update.

- Fixed the issue of RabbitMQ resources becoming unavailable after hot update.

- Canceled the HTTP refresh timer of the OCSP when disabling OCSP stapling or the TLS listener.

- Canceled the CRL refresh timer when disabling CRL check or the TLS listener.

## 4.4.21

*Release Date: 2023-10-16*

### Enhancements

- Added support for Confluent data bridge.

- Now the MQTT topic field in the Kafka consumer group supports templates with placeholders.

  For example, if the key of the message consumed by Kafka is "a", and the configured MQTT topic is "topic/${key}", then the MQTT topic will be replaced with "topic/a" when the message is forwarded.

- Now the "Message Republish" action supports two new fields: "MQTT Properties" and "User Properties". Both of the fields are in the format of key-value pairs, and both the key and value support placeholders.


### Bug Fixes

- Fixed the issue that the Kafka action cannot send numeric values as Kafka Headers.

  Prior to this fix, when the "Kafka Headers value encode mode" was set to "NONE", if the "Kafka Headers" field contains a JSON object with numeric types (such as `{"a": 1, "b": "str"}`), the numeric values (`"a":1`) would be ignored and not sent to Kafka. After the fix, the numeric types in JSON will be converted to strings before being sent to Kafka.

## 4.4.20

*Release Date: 2023-08-01*

### Enhancements

- Improved the performance of sending data to Kafka and HStreamDB.

  This enhancement added an Erlang message buffer ahead of the driver process, reducing the frequency of internal message passing within EMQX. This optimization comes at the expense of increased message latency, but it significantly enhances the throughput capacity when sending data to Kafka and HStreamDB.

  Now, messages sent from EMQX to Kafka or HStreamDB drivers will first enter the buffer. When the number of cached messages reaches `message_accumulation_size` or the time interval reaches `message_accumulation_interval`, the buffered messages will be batched and sent to the Kafka or HStreamDB driver. The driver will then handle the forwarding to Kafka or HStreamDB services. Setting `message_accumulation_size = 0` (default value) will disable this message buffering feature.

- Added the `auto_reconnect` option for SQL Server resources.

  Before this improvement, when the connection between EMQX and the SQL Server database was disrupted, EMQX was unable to reconnect automatically. With this new enhancement, EMQX can reconnect automatically. You can still opt to set `auto_reconnect = false` to turn off the automatic reconnection feature.

- Added TLS connection support to RabbitMQ resource.

- Added support for defining attributes and ordering key for GCP PubSub actions.

### Bug Fixes

- Fixed the issue that the `mongo_date()` function of the rule engine cannot be tested on the Dashboard.

  Before the fix, `mongo_date()` can be used normally, but an error will occur when testing on the SQL test page of the Dashboard.

- Fixed the issue where the rule engine failed to send messages through RabbitMQ actions after a hot upgrade to version 4.4.19.

## 4.4.19

*Release Date: 2023-06-27*

### Enhancements

- Added support for TCP keep-alive in MQTT/TCP and MQTT/SSL listeners [#10854](https://github.com/emqx/emqx/pull/10854).

  A new configuration option has been added: `zone.<zone-name>.tcp_keepalive = Idle,Interval,Probes`. Users can enable the TCP layer's Keep Alive feature and specify time parameters using this configuration. This configuration is only effective on Linux and MacOS systems.

- Improved error logs related to Proxy Protocol [emqx/esockd#177](https://github.com/emqx/esockd/pull/177).

  The sample logs before this improvement:
  ```
  2023-04-20T14:56:51.671735+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {invalid_proxy_info,<<"f\n">>}, offender: [{pid,<0.3192.0>},{name,connection},{mfargs,{...}}]

  2023-04-20T14:57:01.348275+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {proxy_proto_timeout,5000}, offender: [{pid,<0.3194.0>},{name,connection},{mfargs,{...}}]
  ```
  After the improvement:
  ```
  2023-04-20T18:07:06.180134+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but received invalid proxy_protocol header, raw_bytes=<<"f\n">>

  2023-04-20T18:10:17.205436+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but timed out while waiting for proxy_protocol header
  ```

- Added a new feature to enable partial certificate chain validation for TLS listeners [#10553](https://github.com/emqx/emqx/pull/10553).

  For details, please check out the `listener.ssl.external.partial_chain` in the `listeners.conf` config file.

- Added a new feature to enable client certificate extended key usage validation for TLS listeners [#10669](https://github.com/emqx/emqx/pull/10669).

  For details, please check out the `listener.ssl.external.verify_peer_ext_key_usage` in the `listeners.conf` config file.

- Added the `live_connections` field in the HTTP API `/api/v4/nodes` response [#10859](https://github.com/emqx/emqx/pull/10859).

  Previously, this interface had a `connections` field, which represented the number of active connections on the current node that had not expired. This means that even if the MQTT connection has been disconnected, as long as the client has a persistent session, it would still be counted in the `connections` field. The newly added `live_connections` field specifically counts the number of clients with MQTT connections that have not been disconnected.

- Added 3 random SQL functions to the rule engine [#11113](https://github.com/emqx/emqx/pull/11113).

  - random(): Generates a random number between 0 and 1 (0.0 =< X < 1.0).
  - uuid_v4(): Generates a random UUID (version 4) string.
  - uuid_v4_no_hyphen(): Generates a random UUID (version 4) string without hyphens.

- Added numerical range validation (23-65535) for the `mqtt.max_clientid_len` configuration parameter [#11096](https://github.com/emqx/emqx/pull/11096).

- Added a plugin `emqx_gcp_device`.

  It simplifies migration from Google IoT Core:
  * It allows import of Google IoT Core device configuration and authentication data.
  * Implements Google IoT Core compatible MQTT authentication.
  * Provides API endpoints for managing device configuration and authentication data.

- Added support for creating RabbitMQ actions with dynamic Routing Key.

  The "RabbitMQ Routing Key" parameter of RabbitMQ actions can now use dynamic variables in the `${key}` format.

- Added default ports for DynamoDB resources.

  Previously, the "DynamoDB Server" parameter of DynamoDB resources required a URL with a specified port number, otherwise the resource creation would fail.
  Now, if the URL does not include a port number, the default value will be 80 (HTTP) or 443 (HTTPS).

### Bug Fixes

- Fixed an issue where the rule engine was unable to access variables exported by `FOREACH` in the `DO` clause [#10620](https://github.com/emqx/emqx/pull/10620).

  Given a payload: `{"date": "2023-05-06", "array": ["a"]}`, as well as the following SQL statement:
  ```
  FOREACH payload.date as date, payload.array as elem
  DO date, elem
  FROM "t/#"
  ```
  Prior to the fix, the `date` variable exported by `FOREACH` could not be accessed in the `DO` clause of the above SQL, resulting in the following output for the SQL statement:
  `[{"elem": "a","date": "undefined"}]`.
  After the fix, the output of the SQL statement is: `[{"elem": "a","date": "2023-05-06"}]`

- Fixed the issue where the cache of rules failed to update in certain cases [#11072](https://github.com/emqx/emqx/pull/11072).

  Prior to the fix, after manually updating the rules, there could be instances where the cache update did not synchronize to certain nodes. This would result in inconsistent rule execution states across different nodes.

- Fixed an issue where the WebHook plugin failed to execute the `on_client_connack` hook [#10710](https://github.com/emqx/emqx/pull/10710).

  See https://github.com/emqx/emqx/issues/10628 for more details.

- Fixed an issue related to reconnection of the authentication module.

  When starting EMQX, if the connection between the authentication module and the database is disconnected, the authentication module will periodically initiate reconnection.
  Prior to the fix, even if the module was manually disabled, EMQX would still periodically reconnect to the database. After the fix, reconnection attempts are made only when the module is enabled.

- Fixed an issue where the PgSQL authentication module lost Prepared Statements after reconnection.

  Prior to the fix, if the connection between the PgSQL authentication module and the database was disconnected and reconnected, authentication would fail due to the loss of Prepared Statements, and the following error log would be printed:
  ```
  2023-03-30T20:50:48.088416+08:00 [error] abc@124.79.220.151:58561 [Postgres] query '"auth_query"' failed: {error,error,<<"26000">>,invalid_sql_statement_name,<<"prepared statement \"auth_query\" does not exist">>,[...]}
  ```

- Fixed an issue where connection to Kafka failed after importing resources from version 4.4.9.

  Prior to the fix, when importing data from version 4.4.9 to 4.4.18, if the Kafka resource was not configured with a username and password (i.e. authentication mode was NONE), EMQX might incorrectly use PLAIN authentication mode to connect to Kafka after importing, resulting in authentication failure.

- Fixed the issue of EMQX docker container unable to integrate with Kafka using Kerberos authentication.

  Prior to the fix, the EMQX docker (alpine) image was missing two software packages, libsasl and cyrus-sasl-gssapiv2, which caused the Kerberos functionality to not work properly. The error log was as follows:
  ```
  2023-06-15T05:30:31.148811+00:00 [warning] ...,{connect_kafka_server_fail,[{<<"kafka-a:9092">>,{{not_loaded,[{module,sasl_auth},{line,212},{on_load_error_info,{error,{load_failed,"Failed to load NIF library: 'Error loading shared library libsasl2.so.3: No such file or directory (needed by /opt/emqx/lib/sasl_auth-2.0.1/priv/sasl_auth.so)'"}
  ```

- Fixed the data distribution logic of the RocketMQ action in the rule engine.

  Prior to the fix, in the scenario where EMQX sends data to a RocketMQ cluster in master-slave mode, if the RocketMQ cluster has multiple master nodes, regardless of whether the `roundrobin` or `key_dispatch` strategy is used, the messages will always be distributed to the first RocketMQ master node.

- Fixed the issue of module order changing after restarting or joining a cluster.

  Prior to the fix, after a node restarting or joining a cluster, the order of modules could change, which would cause the authentication chain order to change if multiple authentication modules were enabled.

- Fixed the issue of failing to import listener configurations from 4.4.7.

  Prior to the fix, if the JSON file contained configurations for "wss" or "wss" listeners, the import could fail due to an incompatible type of the `fail_if_no_subprotocol` configuration item, but without any error messages or logs.

- Fixed the issue of hot configurations not taking effect after a new node joins the cluster.

  Prior to the fix, when a node joined a cluster with hot configurations enabled, it could successfully replicate the hot configurations from the cluster, but the configurations did not take effect at runtime.

- Fix the issue that the WebSocket downlink message type of the OCPP gateway is incorrect.

  Prior to the fix, the WebSocket downlink message type of the OCPP gateway was `binary`, but it should be `text`.

- Fix issue when MQTT clients could not connect over TLS if the listener was configured to use TLS v1.3 only.

  The problem was that TLS connection was trying to use options incompatible with TLS v1.3.

- Fixed the issue of retainer module throwing errors after hot upgrade.

  After upgrading from old versions (e4.4.0 ~ e4.4.16) to e4.4.17 or e4.4.18, the retainer module might throw errors, causing retain messages to be unable to be sent properly. The error log is as follows:
  ```
  2023-05-17T01:48:44.515012+00:00 [error] mqtt_conti@62.93.210.184:54851 [Hooks] Failed to execute {fun emqx_retainer:on_session_subscribed/3,[]}: {error,badarg,[...]}
  ```

- Fixed the issue of error log appearing when testing the connectivity of RabbitMQ.

  Prior to the fix, when clicking the test button for RabbitMQ resources, the following error log would be printed (only the error log appeared, and the functionality was not affected):
  ```
  2023-06-02T05:59:16.025229+00:00 [error] Destroy Resource bridge_rabbit failed, ResId: <<"_probe_:6edc3a76">>, not_found
  ```

- Fixed the issue of creating multiple duplicate hot configuration modules when continuously clicking the **Enable** button on the Dashboard settings page.

## 4.4.18

*Release Date: 2023-04-28*

### Enhancements

-   Added plugin `emqx_ocpp` to support the OCPP 1.6-J protocol.

    OCPP (Open Charge Point Protocol) is a protocol used for
    communication between electric vehicle charging stations and central
    management systems. This plugin serves as an OCPP gateway for EMQX,
    enabling seamless integration between OCPP and MQTT protocols. It
    facilitates the smooth connection of charging stations to EMQX
    through OCPP over WebSocket.".

    To start the plugin, you can use the
    `emqx_ctl plugins load emqx_ocpp` command or EMQX Dashboard.
    Additionally, tools such as
    [ocpp-go](https://github.com/lorenzodonini/ocpp-go) can be used to
    simulate charging points for message exchange testing.

-   Improved the placeholder syntax of rule engine.

    The parameters of actions support using placeholder syntax to
    dynamically fill in the content of strings. The format of the
    placeholder syntax is `${key}`.\
    Before this improvement, the `key` in `${key}` could only contain
    letters, numbers, and underscores. Now the `key` supports any UTF8
    characters.

### Bug Fixes

-   Fixed the issue where required plugins were missing in
    `data/load_plugins`.

    Before this fix, if the `data/load_plugins` file was manually
    deleted and EMQX was restarted, three required plugins
    (`emqx_schema_registry`, `emqx_eviction_agent`,
    `emqx_node_rebalance`) would not be automatically enabled and would
    not be recorded in the newly generated `data/load_plugins` file.

## 4.4.17

*Release Date: 2023-04-13*

### Enhancements

- When the listener enabled with `Proxy Protocol` receives a TCP port probe, no error logs will be printed anymore [emqx/esockd#172](https://github.com/emqx/esockd/pull/172).

  Before the fix, if the listener had enabled the proxy protocol (`listener.tcp.external.proxy_protocol=on`), but the connection was disconnected after the TCP handshake was completed and before the proxy information was received, the following error log would be printed:

  ```
  [error] supervisor: 'esockd_connection_sup - <0.3265.0>', errorContext: connection_shutdown, reason: {recv_proxy_info_error,tcp_closed}, offender:
  ```
  After the fix, no logs will be printed, but you can still view the error reason statistics through the `emqx_ctl listeners` command.

- Improved the error logs of the listener for file descriptor exhaustion [emqx/esockd#173](https://github.com/emqx/esockd/pull/173).

  Before the improvement, the log was:
  ```
  [error] Accept error on 0.0.0.0:1883: emfile
  ```
  After the improvement, the log became:
  ```
  [error] Accept error on 0.0.0.0:1883: EMFILE (Too many open files)
  ```

- Improved the performance of the rule engine when there are many rules [#10283](https://github.com/emqx/emqx/pull/10283)

  Before the improvement, when there were many rules, the rule engine would consume a lot of CPU time on rule queries and matching, becoming a performance bottleneck.
  In this optimization, by simply adding a cache to the rule list, the rule execution efficiency in this scenario was greatly improved.
  In our test, we created 700 rules that did not perform any actions (bound to the "do_nothing" debugging action) on a 32-core 32G virtual machine, and sent MQTT messages to EMQX at a rate of 1000 messages per second (that is, the rule trigger frequency was 700 * 1000 times per second).
  In the above scenario, the CPU usage of the optimized rule engine dropped to 55% ~ 60% of the previous level.

- Improve the alarm logs when importing data from old versions (4.2 or earlier).

  Before this change, if data was imported from versions 4.2 or earlier to version 4.4, the built-in authentication part of the data would be discarded due to the lack of authentication type, and the log description of the failure reason was not clear enough.
  After this change, the importing will fail, and the EMQX log will prompt the user to use the command line tool for data import and specify the authentication type:

  ```
  $ emqx_ctl data import <filename> --env '{"auth.mnesia.as":"username"}'
  ```

### Bug Fixes

- Fixed the issue where `Erlang distribution` could not use TLS [#9981](https://github.com/emqx/emqx/pull/9981).

  For more information on `Erlang distribution`, see [here](https://www.emqx.io/docs/en/v4.4/advanced/cluster.html).

- Fixed the issue where MQTT bridging could not verify TLS certificates with wildcard domains on the peer side [#10094](https://github.com/emqx/emqx/pull/10094).

- Fixed the issue where EMQX could not timely clear the information of disconnected MQTT connections when there were too many messages backlogged in the retainer. [#10189](https://github.com/emqx/emqx/pull/10189).

  Before the fix, the `emqx_retainer` plugin and the EMQX connection information cleanup task shared a process pool. Therefore, if the process pool was blocked by a large number of retain message distribution tasks, many disconnected MQTT connection information would not be cleared in time. See [#9409](https://github.com/emqx/emqx/issues/9409) for details.
  After the fix, the `emqx_retainer` plugin uses a separate process pool to avoid this problem.

- Fixed the issue where the path of the template file `service-monitor.yaml` in the Helm Chart was incorrect. [#10229](https://github.com/emqx/emqx/pull/10229)

- When upgrading from EMQX 4.3 to 4.4, EMQX will migrate the ACL table in the "built-in authentication" module upon restart.

  Before the fix, if data was migrated from version 4.3 to 4.4 by copying the `data/mnesia/<node-name>` directory, after the migration was completed,
  when viewing the "built-in authentication" module through the Dashboard, a 500 error would occur because the ACL table was not migrated to the new format.
  Note: This issue only occurs when the module is disabled, and users can manually enable the module to resolve it.
  After the fix, EMQX will attempt to migrate the ACL table upon restart after upgrading, thus avoiding this issue.

- Fix the issue of incorrect counting statistics for the IoTDB action.

  Before the fix, if all measurements were null, IoTDB would ignore them, not insert any data but return 200 OK, causing the increment of the successful sending count to be incorrect.
  After the fix, when all measurements are null, the IoTDB action will discard the request and count it as a sending failure.

- Fix the issue of rule creation failure when TDEngine SQL statements contain line breaks.

  Before the fix, TDEngine SQL statements could not contain line breaks. For example, when using the following statement as the `SQL template` parameter for the TDEngine action, rule creation would fail:
  ```
  INSERT INTO ${devid}
  USING
    tsdb.profit
  TAGS
    ('${custid}', '${devid}')
  VALUES (${ts}, ${value})
  ```

- Fix the issue of incorrect encoding of error messages returned by the HTTP API `/load_rebalance/:node/start`.

- Fix the process leak issue of the RocketMQ client in EMQX [rocketmq-client-erl#24](https://github.com/emqx/rocketmq-client-erl/pull/24).

  EMQX's RocketMQ client periodically obtains node information from RocketMQ, checks whether the node information has been updated, and updates or adds producer processes based on the returned results.
  Before the fix, due to problems with the method of comparing node information, process leaks could occur in certain situations.

## 4.4.16

*Release Date: 2023-03-10*

This version update includes 4 enhancements and 7 fixes.

### Enhancements

- Improve the logs for IoTDB resource.
  Before this change, if the user configured a different `iotdb_version` from the installed IoTDB
  version, sending messages to IoTDB will fail but it is hard to know the reason just from the log
  messages.
  After this change, we will print more readable logs to prompt the user that he may have
  configured a wrong `iotdb_version`.

- Don't print error logs when the offline-msg actions receive QoS0 messages.

- Change "EMQX" to "EMQX" from the outputs of CLIs and names of plugins.

### Bug Fixes

- Start the `emqx_schema_registry` plugin automatically when release hot upgrade.
  The `emqx_schema_registry` is a necessary plugin when using rules to decode serialized binary data
  (e.g. Protobuf or Avro), we should ensure this plugin started in the EMQX enterprise.

- Fix the issue that the `message_key` parameter of the RocketMQ action does not work.

- Fix the rule failed when processing the decoded protobuf messages.
  Before this fix, if the protobuf schema contains `oneof` definitions, the rule may failed when trying to parse the decoded message to JSON string.

- Fix the issue that send JSON Object as Kafka Headers failed.

- Delete the temporary directories generated by resources and `emqx-modules`.
  Before this fix, sometimes the sub-directories in `data/rules` and `data/modules` cannot be cleaned even after the resources or `emqx-module` are deleted.

- Fix some problems in the descriptions of HStreamDB resource fields.

- Avoid changing the payload of MQTT messages when printing debug logs [#10091](https://github.com/emqx/emqx/pull/10091).
  Before this fix, if EMQX receives a message with Payload "e\ne\nc\nc\n2\n\n\n", the log message will be as follows:
  ```
  2023-03-08T13:28:04.320622+08:00 [debug] mqttx_e34bd582@127.0.0.1:54020 [MQTT] RECV PUBLISH(Q1, R0, D0, Topic=t/1, PacketId=39467, Payload=e, e, c, c, 2, , , )
  ```
  This is the corresponding log message now:
  ```
  2023-03-08T14:26:50.935575+08:00 [debug] mqttx_e34bd582@127.0.0.1:54020 [MQTT] RECV PUBLISH(Q1, R0, D0, Topic=t/1, PacketId=39467, Payload=<<"e\ne\nc\nc\n2\n\n\n">>)
  ```

## 4.4.15

*Release Date: 2023-03-03*

This version update includes 16 enhancements and 20 fixes.
Among the enhancements, there are new exciting new features worth highlighting:

- Upgrade the MongoDB client library of EMQX to support MongoDB 5.1 and above.
- Dashboard supports the proxy protocol of HAProxy.
- Release the Ubuntu 22.04 installation package.
- Support Kafka headers in rule engine.
- Support storing data to IoTDB using rule-engine.

### Enhancements

- Support Kafka headers in rule engine.

- Support storing data to IoTDB using rule-engine.

- The JT/T 808 compatible non-standard location reporting messages. When a user uses a reserved ID to report a location, EMQX will pass it through in Base64 format instead of disconnecting the client.

- Only create EMQX modules locally when the emqx_modules application is started.
  Before this change, we RPC to all the nodes to create/recreate modules when emqx_modules application
  get started, so finally we created modules N^2 times on all the nodes (N times on each node).

- Improve the log message when the DynamoDB action cannot find the `hash_key` or `range_key`.

- HStreamDB driver update to support HStreamDB ~> 0.12.0.

- The plugin `schema_registry` will be enabled by default as an optional feature of `rule_engine`.

- Add TLS connections support for HStreamDB action.

- The MongoDB library has been upgraded to support MongoDB version 5.1 and greater.

- Support proxy protocol of HAProxy for dashboard API [9803](https://github.com/emqx/emqx/pull/9803).

- Added Ubuntu 22.04 package release [#9831](https://github.com/emqx/emqx/pull/9831).

- Improve the integration of the `banned` and the `delayed` feature [#9790](https://github.com/emqx/emqx/pull/9790).
  Now when publishing a delayed message will check first if its source client is banned, if true, this publish will be ignored.

- Security enhancement for retained messages [#9790](https://github.com/emqx/emqx/pull/9790).
  The retained messages will not be published if the publisher client is banned.

- Now the corresponding session will be kicked when client is banned by `clientid` [#9904](https://github.com/emqx/emqx/pull/9904).

- Add more debug logs for authentication and ACL [#9943](https://github.com/emqx/emqx/pull/9943).

- Expose the stats `live_connections.count` and `live_connections.max` to Prometheus [#9929](https://github.com/emqx/emqx/pull/9929).

### Bug Fixes

- Fixed `tlsv1.3` is missing from Module(Stomp Gateway, GB/T 32960 Gateway, JT/T808 Gateway, Extension Protocol, TCP Gateway, MQTT Subscriber) `tls_versions` tab.

- Fix the problem of sending offline messages to clients in reverse order when using Redis offline message feature.

- Fix the emqx-modules are disabled if their initialization failed after EMQX is restarted.

- Fix some issues in descriptions of the actions and resources.

- Fix the issue that Oracle resources cannot get connected automatically after release hot upgrade.

- Fix the issue that produce messages to RocketMQ cluster using rule-engine failed.

- Returning a failure when creating an existing listener in a cluster using the API.

- Delete the files directory when `resources/modules/schema_registry` were deleted to avoid files leaking.

- Fixed an error when forward MQTT messages with User-Property using the `republish` action [#9942](https://github.com/emqx/emqx/pull/9942).

- Fix some issues in descriptions of the actions, resources and emqx-modules [#9931](https://github.com/emqx/emqx/pull/9931).

- Fix there's no error logs when query the JWKS server failed [#9931](https://github.com/emqx/emqx/pull/9931).

- The returned client lists of HTTP query `GET /api/v4/clients?_page=2&_limit=20` to different nodes might be inconsistent [#9926](https://github.com/emqx/emqx/pull/9926).

- Fix the problem that new MQTT TLS connections failed to establish after release hot upgrade [#9810](https://github.com/emqx/emqx/pull/9810).
  For more detailed information please see: [emqx/esockd#170](https://github.com/emqx/esockd/pull/170).

- Fix a problem in the log message format of MQTT packets [#9858](https://github.com/emqx/emqx/pull/9858).
  Before this fix, a comma was missing between the flags (DUP) of the fixed header
  and the fields (ClientId) of the variable header:
  ```
  2023-01-29T13:40:36.567692+08:00 [debug] 127.0.0.1:50393 [MQTT] RECV CONNECT(Q0, R0, D0ClientId=test_client, ... Password=undefined)
  ```

- Avoid crash logs in CoAP gateway when receiving liveness checking packets from Load Balancer [#9869](https://github.com/emqx/emqx/pull/9869).

- Fix the exclusive topics aren't removed when the session has already been cleaned [#9868](https://github.com/emqx/emqx/pull/9868).

- Fix the EMQX reports `{case_clause,{error,closed}}` error log message when WebSocket connections interrupted [emqx/cowboy#8](https://github.com/emqx/cowboy/pull/8).

- Fix sometimes the rules cannot be enabled automatically after EMQX is restarted [#9911](https://github.com/emqx/emqx/pull/9911).

- Fix the `{badarg,[{ets,lookup,[gproc,{shared, ...` error logs during shutdown [#9919](https://github.com/emqx/emqx/pull/9919).

- Fix crash when updating a client's `keepalive` via the HTTP API if it connects with `keepalive` disabled [#9933](https://github.com/emqx/emqx/pull/9933).

## 4.4.14

*Release Date: 2023-01-06*

### Enhancements

- Add a password complexity requirement when adding or modifying Dashboard users via the API. Now passwords must contain at least 2 of alphabetic, numeric and special characters, and must be 8 to 64 characters long.

### Bug Fixes

- Fix the problem that adding or importing Dashboard users via the API fails to add complex passwords due to incorrect checksum of the passwords.

- Fix load bootstrap_app_file's apps is not sync when reboot.

## 4.4.13

*Release Date: 2023-01-03*

### Bug Fixes

- Fix an issue where testing the GCP PubSub could leak memory, and an issue where its JWT token would fail to refresh a second time. [#9640](https://github.com/emqx/emqx/pull/9640)

## 4.4.12

*Release Date: 2022-12-29*

This version comes with an exciting new feature: cluster load rebalance.
The newly introduced CLI command emqx_ctl rebalance provides support of the below two common scenarios:
- Newly joined or restarted nodes may stay under-loaded for along time if the clients are mostly long-lived connections
- For maintenance, shutting down a node will cause all connected connections to reconnect around the same time, increasing the chance to overload the cluster. Also the non-clean sessions in this node will be lost.

Now it’s possible to issue the rebalance command to move some of the connections the under-loaded nodes. With the --evacuation option, we can also move all the connected MQTT clients off the node before stopping the service.
For more information about this feature, please refer to [Cluster Rebalancing](../advanced/rebalancing.md)

### Enhancements

- Added topic validation for `emqx_mod_rewrite`. The dest topics contains wildcards are not allowed to publish.

- TDEngine resource support HTTP response formats of both TDEngine 2.x and 3.x [emqx/tdengine-client-erl#7](https://github.com/emqx/tdengine-client-erl/pull/7).
  The HTTP response of TDEngine 2.x uses the `status` field to represent the success or failure,
  while TDEngine 3.x uses the `code` field instead.

- Support batch sending messages to [TDEngine SubTables](https://docs.tdengine.com/2.6/concept/#subtable).

- The offline message clickhouse action prints an info level log: `Destroyed .. Successfully` when enabling a rule.

- Now the rules can be created even though the corresponding resources are not ready.
  Before this change, one cannot create rules without getting the resources connected. We made it
  possible in this change, but the newly created rule will be in `disabled` state.

- Avoid delete offline message twice.
  EMQX while delete offline message in external database when subscriber send a PUBACK or PUBREC packet.
  But a message with `retain = true` will be stored twice (in retainer and external database) in case retain message and offline message are used in same time.
  The reduplicated PUBACK and PUBREC will trigger deleted action twice. And the action-metrics will also increase caused by Rule-SQL execution succeed.
  In most cases this does not generate any exceptions or errors, and only a few databases will report that the message to be deleted does not exist on the second delete.
  This change will avoid redundant offline message deletion operations.

- Users can define the `externalTrafficPolicy` of service in EMQX Enterprise Helm Chart.

- When dashboard creates a new user, the password format is `^[A-Za-z0-9]+[A-Za-z0-9-_]*$`.

### Bug Fixes

- After a reconnect, the unacknowledged QoS1/QoS2 messages in non-clean session were not retransmitted periodically as before the reconnect.
  The configuration `zone.<zone-name>.retry_interval` specifies the retransmission interval of
  unacknowledged QoS1/QoS2 messages (defaults to 30s).
  Prior to this fix, unacknowledged messages buffered in the session are re-sent only once after session take-over, but not retried at configured interval.

- The expired 'awaiting_rel' queue is not cleared after persistent session MQTT client disconnected.
  Before this change, if the 'awaiting_rel' queue is full when the MQTT client reconnect
  to the broker and publish a QoS2 message, the client will get disconnected by the broker
  with reason code RC_RECEIVE_MAXIMUM_EXCEEDED(0x93), even if the packet IDs in the 'awaiting_rel'
  queue have already expired.

- Authentication for RocketMQ resource not working.
  In this change we moved the configuration fields `access_key`, `secret_key`
  and `security_token` from the `data_to_rocket` action to the `bridge_rocket`
  resource. And we also added a new field `namespace` for RocketMQ services in
  Aliyun cloud.

- Added validation for Kafka action parameters, Segment Bytes should not be greater than Max Bytes.

- Added validation for Pulsar action parameters, Segment Bytes should not be greater than Max Bytes.

- Fix the "ORA-01000: maximum open cursors exceeded" problem when sending data via the emqx oracle resource.

- Fixed EMQX Enterprise Helm Chart deployment error.
  - Fixed the `Discovery error: no such service` error occurred during helm chart deployment, resulting in an abnormal discovery of cluster nodes.
  - Fixed EMQX Enterprise Helm Chart can not set JSON type value for EMQX Enterprise configuration items.

- Fixed an issue where the configuration would not be reloaded on all nodes in a cluster after importing a backup configuration.

- Fixed an issue where the HTTP API would fail to download a backup configuration file when downloading it from a node where it does not reside in.

- Add the `SNI` field for SSL connection configuration of Kafka resource.

- Fixed the issue that the MongoDB resource connection process was slow when authentication was enabled.

- Fixed the issue that after the release hot upgrade, EMQX occasionally alarms resources down, and the alarms could not be automatically cleared.

- Fix connection statistics in the dashboard: mark evacuated clients as disconnected before they can reconnect.
## 4.4.11

*Release Date: 2022-11-26*

This release included 23 enhancements and 21 bug fixes.
Among the enhancements, there are new exciting new features worth highlighting.

- Google PubSub integration as a Rule-Engine data bridge.
- OCSP (Online Certificate Status Protocol) Stapling.
- CRL (Certificate Revocation List) cache.
- Pulsar data bridge supports data buffering.
- OTP upgrade from 24.1.5-3 to 24.3.4.2-1.
- Customizable client aliases to make it easier to when creating customized authentication and authorization.

It is possible to hot-upgrade from the older version e4.4 to this version.
Please note though, in order to start making use of the new features such as OCSP Stapling, and CRL cache,
a node restart (and configuration change) is required.

### Enhancements

- Upgraded Pulsar client to 0.7.0.
  Now it's possible to buffer messages to be
  produced to Pulsar while EMQX has no connection to the Pulsar
  broker.  Such messages will be retained for a configurable amount of
  time.

  Also, credentials such as basic auth and JWT tokens used for Pulsar
  authentication will be censored out when a crash happens, thus
  avoiding secret leakage to logs.

  **Note**: if an older EMQX version is upgraded to a version
  containing this update, *and then* downgraded back to the previous
  old version, there's a risk that some messages that were sent in a
  sync manner might be sent but regarded as a timeout error, which in
  turn causes such clients to be disconnected.

- Added hot-configuration support for OCSP stapling and CRL checking/caching.

- Added a new rule engine bridge and corresponding rule action for GCP PubSub.

- Support to use placeholders like `${var}` in the `Collection` field of Rule-Engine's MongoDB actions

- Add a format check to the `host` field of the InfluxDB resource in Rule-Engine.
  The host field should be an ip/domain without scheme and port.

- OTP upgrade from 24.1.5-3 to 24.3.4.2-1 [#9265](https://github.com/emqx/emqx/pull/9265).
  Change highlights:
    - Erlang/OTP [SSL library vulnerability fix](https://nvd.nist.gov/vuln/detail/CVE-2022-37026)
    - Added support for OCSP (Online Certificate Status Protocol) Stapling
    - Added CRL (Certificate Revocation List) cache auto refresh

- Added support for OCSP stapling and CRL
  caching [#9297](https://github.com/emqx/emqx/pull/9297).

- Added support for specifying custom modules for adding clientid and common name
  aliases [#9297](https://github.com/emqx/emqx/pull/9297).
  Now you can implement a simple callback to enrich clients with aliases, and then use the aliases
  in the authentication and authorization (ACL) rules' place holders (`%cida` for clientid alias
  and `%cna` for username alias).

- Added support for specifying custom modules for custom authentication [#9297](https://github.com/emqx/emqx/pull/9297).
  To support simple authentication rules, it is no longer necessary to implement a full-blown plugin.

- Added a JWT management for Rule-Engine, for creating and refreshing JWT tokens in rule engine actions [#9241](https://github.com/emqx/emqx/pull/9241).
  This feature is so far only used in EMQX Enterprise Google PubSub integration.
  Can be used as webhook integration's JWT authentication against the webhook service endpoint.

- Make sure listener's `tls_versions` config value is one or more of `tlsv1`, `tlsv1.1`, `tlsv1.2`, `tlsv1.3` [#9260](https://github.com/emqx/emqx/pull/9260).

- Remove useless information from the dashboard listener failure log [#9260](https://github.com/emqx/emqx/pull/9260).

- We now trigger the `'message.acked'` hook after the CoAP gateway sends a message to the device and receives the ACK from the device [#9264](https://github.com/emqx/emqx/pull/9264).
  With this change, the CoAP gateway can be combined with the offline message caching function (in the
  emqx enterprise), so that CoAP devices are able to read the missed messages from the database when
  it is online again.

- Support to use placeholders like `${var}` in the HTTP `Headers` of Rule-Engine's Webhook actions [#9239](https://github.com/emqx/emqx/pull/9239).

- Asynchronously refresh the resources and rules during emqx boot-up [#9199](https://github.com/emqx/emqx/pull/9199).
  This is to avoid slowing down the boot if some resources spend long time establishing the connection.

- Add a warning log if the ACL check failed for subscription [#9124](https://github.com/emqx/emqx/pull/9124).
  This is to make the ACL deny logging for subscription behave the same as for publish.

- JWT ACL claim supports `all` action to imply the rules applie to both `pub` and `sub` [#9044](https://github.com/emqx/emqx/pull/9044).

- Added a log censor to avoid logging sensitive data [#9189](https://github.com/emqx/emqx/pull/9189).
  If the data to be logged is a map or key-value list which contains sensitive key words such as `password`, the value is obfuscated as `******`.

- Enhanced log security in ACL modules, sensitive data will be obscured [#9242](https://github.com/emqx/emqx/pull/9242).

- Add `management.bootstrap_apps_file` configuration to bulk import default app/secret when EMQX initializes the database [#9273](https://github.com/emqx/emqx/pull/9273).

- Added two new configs for deterministic order of authentication and ACL checks [#9283](https://github.com/emqx/emqx/pull/9283).
  The two new global config names are `auth_order` and `acl_order`.
  When multiple ACL or auth plugins (or modules) are enabled, without this config, the order (in which each backend is queried)
  is determined by the start/restart order of the plugin (or module).
  Meaning, if a plugin (or module) is restarted after initial boot, it may get ordered to the end of the list.
  With this config, you may set the order with a comma-speapated ACL or auth plugin names (or aliases).
  For example: `acl_order = jwt,http`, this will make sure `jwt` is always checked before `http`,
  meaning if JWT is not found (or no `acl` cliam) for a client, then the ACL check will fallback to use the HTTP backend.

- Added configurations to enable more `client.disconnected` events (and counter bumps) [#9267](https://github.com/emqx/emqx/pull/9267).
  Prior to this change, the `client.disconnected` event (and counter bump) is triggered when a client
  performs a 'normal' disconnect, or is 'kicked' by system admin, but NOT triggered when a
  stale connection had to be 'discarded' (for clean session) or 'takeovered' (for non-clean session) by new connection.
  Now it is possible to set configs `broker.client_disconnect_discarded` and `broker.client_disconnect_takeovered` to `on` to enable the event in these scenarios.

- For Rule-Engine resource creation failure, delay before the first retry [#9313](https://github.com/emqx/emqx/pull/9313).
  Prior to this change, the retry delay was added *after* the retry failure.

### Bug Fixes

- Fix the default authentication mechanism of Kafka resource changed to `NONE` from `PLAIN`
  when upgrading emqx from e4.4.5 and older versions.

- Fix an upgrade issue for JWT authentication plugin.
  When upgrading from e4.4.3 or earlier, an EMQX internal resource which holds the keys will have to be restarted,
  during the restart, clients may fail to be authenticated.

- Fixed the option to choose the `reset_by_subscriber` offset reset
  policy in Kafka Consumer.

- Added the missing `tlsv1.3` option to `tls_versions` in hot-config.

- Made Rule-Engine able to connect SQL server when its listening port is not the default (`1433`).

- Make sure Schema-Registry API supports Percent-encoding `name` in HTTP request URI.
  Note that the `name` in `POST /api/v4/schemas` request body should not be percent-encoded as it's a JSON field value.

- Fix an upgrade issue for JWT authentication plugin.
  When upgrading from e4.3.9 or earlier, an EMQX internal resource which holds the keys will have to be restarted,
  during the restart, clients may fail to be authenticated.

- Fix get trace list crash when trace not initialize. [#9156](https://github.com/emqx/emqx/pull/9156)

- Fix create trace sometime failed by end_at time has already passed. [#9156](https://github.com/emqx/emqx/pull/9156)

- Fix that after uploading a backup file with an non-ASCII filename, HTTP API `GET /data/export` fails with status code 500 [#9224](https://github.com/emqx/emqx/pull/9224).

- Improve the display of rule's 'Maximum Speed' counter to only reserve 2 decimal places [#9185](https://github.com/emqx/emqx/pull/9185).
  This is to avoid displaying floats like `0.30000000000000004` on the dashboard.

- Fix the issue that emqx prints too many error logs when connecting to mongodb but auth failed [#9184](https://github.com/emqx/emqx/pull/9184).

- Fix that after receiving publish in `idle mode` the emqx-sn gateway may panic [#9024](https://github.com/emqx/emqx/pull/9024).

- "Pause due to rate limit" log level demoted from warning to notice [#9134](https://github.com/emqx/emqx/pull/9134).

- Restore old `emqx_auth_jwt` module API, so the hook callback functions registered in older version will not be invalidated after hot-upgrade [#9144](https://github.com/emqx/emqx/pull/9144).

- Fixed the response status code for the `/status` endpoint [#9210](https://github.com/emqx/emqx/pull/9210).
  Before the fix, it always returned `200` even if the EMQX application was not running.  Now it returns `503` in that case.

- Fix message delivery related event encoding [#9226](https://github.com/emqx/emqx/pull/9226)
  For Rule-Engine's input events like `$events/message_delivered`, and `$events/message_dropped`,
  if the message was delivered to a shared-subscription, the encoding (to JSON) of the event will fail.
  Affected versions: `v4.3.21`, `v4.4.10`, `e4.3.16` and `e4.4.10`.

- Make sure Rule-Engine API supports Percent-encoding `rule_id` and `resource_id` in HTTP request path [#9190](https://github.com/emqx/emqx/pull/9190).
  Note that the `id` in `POST /api/v4/rules` should be literals (not encoded) when creating a `rule` or `resource`.
  See docs [Create Rule](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-rules) [Create Resource](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-resources).

- Calling 'DELETE /alarms/deactivated' now deletes deactived alarms on all nodes, including remote nodes, not just the local node [#9280](https://github.com/emqx/emqx/pull/9280).

- When republishing messages or bridge messages to other brokers, check the validity of the topic and make sure it does not have topic wildcards [#9291](https://github.com/emqx/emqx/pull/9291).

- Disable authorization for `api/v4/emqx_prometheus` endpoint on management api listener (default 8081) [#9294](https://github.com/emqx/emqx/pull/9294).

## 4.4.10

*Release Date: 2022-10-14*

### Enhancements

- Added more Kafka action parameter checks
  - TCP send buffer size and max batch size parameters are not allowed to be left blank from the configration UI.
  - The combination of "Produce Strategy" set to 'key_dispatch' and the "Key" set to "none" is now not allowed,
    the dashboard will get an error with text like: "with strategy set to 'key_dispatch', key is not allowed to be 'none'"

- TLS listener memory usage optimization [#9005](https://github.com/emqx/emqx/pull/9005).
  New config `listener.ssl.$NAME.hibernate_after` to hibernate TLS connection process after idling.
  Hibernation can reduce RAM usage significantly, but may cost more CPU.
  This configuration is by default disabled.
  Our preliminary test shows a 50% of RAM usage decline when configured to '5s'.

- TLS listener default buffer size to 4KB [#9007](https://github.com/emqx/emqx/pull/9007).
  Eliminate uncertainty that the buffer size is set by OS default.

- Disable authorization for `api/v4/emqx_prometheus` endpoint [#8955](https://github.com/emqx/emqx/pull/8955).

- Added a test to prevent a last will testament message to be
  published when a client is denied connection [#8894](https://github.com/emqx/emqx/pull/8894).

- More rigorous checking of flapping to improve stability of the system [#9045](https://github.com/emqx/emqx/pull/9045).
  Previsouly only normal disconnects are counted, now the connection rejections (e.g. authentication failure) is also included.
  Find more about flapping detection in [EMQX document](https://www.emqx.io/docs/en/v4.3/configuration/configuration.html#flapping-detect-policy)

- QoS1 and QoS2 messages in session's buffer are re-dispatched to other members in the group
  when the session terminates [#9094](https://github.com/emqx/emqx/pull/9094).
  to prevent sessions from buffering messages, however this acknowledgement costs extra resources.

- Fix delayed publish timing inaccuracy caused by OS time change [#8908](https://github.com/emqx/emqx/pull/8908).

### Bug Fixes

- Fix `load_modules` reset after new node joins the cluster.
  Prior to this fix, if `load_modules` for a cluster has been changed, adding a new node to the cluster with default modules
  would cause the other nodes to reset to default too.
  In this fix, the node which is going to join the cluster will copy the `loaded_modules` from the oldest node in the cluster.

- Fix getting subscriptions from backends successfully with QoS values out of range [0, 2].
  Before this change, when we add subscriptions for clients from backends like Redis or MySQL, we won't validate the QoS.
  For example if the QoS is an integer -1, the topic was still subscribed successfully with QoS -1,
  if we send a message to this topic, then an error will occur and the MQTT connection will crash.
  After this change QoS will be clamped into range [0, 2].

- Fix Rule-Engine increased 'success' counter when get subscriptions from Redis failed (due to query Redis timeout).

- Fix Rule-Engine increased 'success' counter when saving offline messages with QoS = 0.
  We don't allow saving offline messages to backends with QoS = 0, so we need to increase the 'failed' counter instead of the 'success' counter in this case.

- Fix the `verify` field is missing from the SSL settings of redis-cluster and redis-sentinel resources.

- Fixed Redis resource liveness problem issue. Prior to this fix, the resource is considered alive when connection can be established.
  The fix is to perform a PING query to make sure the service is alive.

- Fix the redis-cluster resource prints too many error logs when redis servers are not avaliable.

- Fixed an internal Redis resource ID clashing. This clashing may cause resources in use getting deleted when deleting another resource.

- Mask secret/password in the resource/module creation UI.

- Fix HTTP client library to handle SSL socket passive signal [#9145](https://github.com/emqx/emqx/pull/9145).

- Hide redis password in error logs [#9071](https://github.com/emqx/emqx/pull/9071).
  More changes in redis client included in this release:
  - Improve redis connection error logging [eredis#19](https://github.com/emqx/eredis/pull/19).
    Also added support for eredis to accept an anonymous function as password instead of
    passing around plaintext args which may get dumpped to crash logs (hard to predict where).
    This change also added `format_status` callback for `gen_server` states which hold plaintext
    password so the process termination log and `sys:get_status` will print '******' instead of
    the password to console.
  - Avoid pool name clashing [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22).
    Same `format_status` callback is added here too for `gen_server`s which hold password in
    their state.

- Fix shared subscription message re-dispatches [#9094](https://github.com/emqx/emqx/pull/9094).
  - When discarding QoS 2 inflight messages, there were excessive logs
  - For wildcard deliveries, the re-dispatch used the wrong topic (the publishing topic,
    but not the subscribing topic), caused messages to be lost when dispatching.

- Fix shared subscription group member unsubscribe issue when 'sticky' strategy is used.
  Prior to this fix, if a previously picked member unsubscribes from the group (without reconnect)
  the message is still dispatched to it.
  This issue only occurs when unsubscribe with the session kept.
  Fixed in [#9119](https://github.com/emqx/emqx/pull/9119)

- Fix shared subscription 'sticky' strategy when there is no local subscriptions at all.
  Prior to this change, it may take a few rounds to randomly pick group members until a local subscriber
  is hit (and then start sticking to it).
  After this fix, it will start sticking to whichever randomly picked member even when it is a
  subscriber from another node in the cluster.
  Fixed in [#9122](https://github.com/emqx/emqx/pull/9122)

- Fix rule engine fallback actions metrics reset [#9125](https://github.com/emqx/emqx/pull/9125).

## 4.4.9

*Release Date: 2022-09-17*

### Enhancements

- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps

### Bug Fixes

- Fix rule engine update behaviour which may initialize actions for disabled rules
- Fix inaccurate delayed publish due to OS time changes
- Fix the issue that the IP address bound to the Dashboard listener did not take effect
- Fix the issue that shared subscriptions might get stuck in an infinite loop when `shared_dispatch_ack_enabled` is set to true
- Fix the issue that the rule engine SQL crashes when subject matching null values

## 4.4.8

*Release Date: 2022-08-29*

### Enhancements

- Add `GET /trace/:name/detail` API to view log trace file information
- Improve the log when LwM2M packet parsing fails
- Improve the rule engine error log, the log will contain the rule ID when the action execution fails
- Improve log when `loaded_modules` and `loaded_plugins` files do not exist
- Add a guide for changing the default password on Dashboard
- Improved import performance for Protobuf Schema files

### Bug Fixes

- Fix `client.disconnected` event not trigger in some cases
- Fix the issue that the JWK authentication module could not be started later when the JWKS service was not ready in time
- Fix the issue that setting the listener port via an environment variable would prevent either listener from being stopped
- Fix the issue that the built-in database authentication did not distinguish the pagination statistics of the authentication data of the client ID and username
- Fix the issue that the module status would be reset after EMQX restarts after hot upgrade
- Fix Redis driver process leak problem
- Fix rule engine MQTT bridge to AWS IOT connection timeout issue
- Fix `GET /listener` request crashing when listener is not ready
- Fix the issue that the comparison between any variable and null value in the rule engine SQL always returns false after e4.4.1
- Fix the issue that when the execution priority of ExHook is higher than that of the rule engine, the topic filtered by the ExHook Message Hook will not trigger the rule engine
- Fix the issue that the write request of TDEngine may fail because the peer end closes the network connection
- Fix the issue that the configuration of the MQTT-SN module other than the listener would not take effect
- Fix the issue that the ExHook management process was forcibly killed due to the supervisor shutdown timeout
- Fix the issue that the Client ID parameter in ExProto `client.connect` hook is not defined
- Fix ExProto not triggering disconnect event when client is kicked

## 4.4.7

*Release Date: 2022-08-11*

### Important Changes

- As of version 4.4.7, we will no longer provide packages for macOS 10

### Enhancements

- Allows the connection process to be configured to be garbage collected after the TLS handshake is complete to reduce memory footprint, which can reduce memory consumption by about 35% per SSL connection, but increases CPU consumption accordingly
- Allows configuring the log level of the TLS handshake log to view the detailed handshake process

### Bug Fixes

- Fix the issue that EMQX could not be started when deployed through Helm Chart after unmounting the `loaded_modules` file in ConfigMap

## 4.4.6

*Release Date: 2022-07-29*

### Enhancement

- Rules engine supports RocketMQ with ACL enabled
- Supports searching and paging of rules in rule engine
- Kafka in Rules Engine now supports SASL/SCRAM authentication and SASL/GSSAPI authentication. Note that the `cyrus-sasl-gssapi` dependency needs to be installed before using SASL/GSSAPI authentication
- Provides CLI `./bin/emqx check_conf` to actively check if the configuration is correct
- Optimize the write performance of TDEngine in the rule engine
- Support for clearing historical alarms on Dashboard
- Optimizing Shared Subscription Performance
- Add `db_name` field to the action of rule engine writing data to TDEngine to improve support for super table

### Bug Fixes

- Fix the issue that the action count is wrong when the rule engine writes to TDEngine
- Fix the issue that the process pool size setting does not take effect when the rule engine writes to HStreamDB
- Fix the issue that an error was reported when querying the subscription list after the GB/T 32960 plugin was enabled
- Fix the issue that incompatible configuration items when restoring a 4.2 backup to 4.4 with hot configuration enabled
- Fix the issue that once the old version of EMQX is uninstalled after hot upgrade, EMQX will not be able to start again
- Fix the issue that the keep-alive check for UDP clients in the Multilingual Protocol Extension was incorrect, causing clients not to expire
- Fix the issue that the client information in the Multilingual Protocol Extension was not updated in time
- Fix the issue that the license update does not take effect when running after hot upgrade to e4.4.4 and later versions
- Fix the issue that when the client specified Clean Session as false to reconnect, the shared subscription message in the flight window would be re-dispatched to the old session process
- Fix the issue that the new node did not use the cluster license after joining the cluster
- Fix the issue that the `emqx_lua_hook` plugin cannot cancel the message publishing

## 4.4.5

*Release Date: 2022-06-30*

### Enhancement

- Rule engine supports persisting data to HStreamDB
- QoS and Retain flag in rule engine's message republish actions can now use placeholders
- Supports exclusive subscriptions, that is, only one subscriber is allowed for a topic
- Support one-click update of cluster license through CLI
- Dashboard and management API's HTTPS listeners can now use password-protected private key files, providing `key_password` configuration item
- Support for placeholders `%u` and `%c` in topic rewrite rules
- Support setting MQTT 5.0 properties in the API request for message publishing, such as message expiry interval, response topic, etc.
- Optimize the UI when creating rule engine resources, such as folding some uncommon options, etc.
- Opened 4 TCP-related configuration items: KeepAlive, TCP_NODELAY, SO_RCVBUF and SO_SNDBUF for the underlying gRPC connection of ExHook

### Bug Fixes

- Fix the issue of inaccurate memory calculation in Linux OS, and calculate the memory usage of the current OS instead of the memory usage of EMQX
- Fix the issue that the old disconnect event of ExHook would be triggered later than the new connect event when the client reconnects
- Improve the JWT authentication module's judgment logic for startup status in a cluster environment
- Extend the timeout of DynamoDB resource status query of rule engine to avoid the problem that some overseas resources may not be available
- Fix the issue that the timestamp is not updated when the PostgreSQL data storage plugin persists messages and updates message consumption
- Fix the issue that the rules engine's Tablestore, Lindorm and InfluxDB resources did not check connection status when creating
- Fix rule engine not checking connection status when creating Tablestore, Lindorm and InfluxDB resources
- Fix rule engine not updating metrics correctly when writing to TDEngine resource fails
- Fix the issue that the Cassandra message store plugin persisted retained messages to multiple tables, causing duplicate messages
- Fix the issue that Kafka resources exported from versions prior to 4.3.0 (only when the Produce policy is set to `first_key_dispatch`) could not be imported into 4.3.0 and later
- Fix the issue that the execution order of topic rewriting and delayed publish is not fixed, now it is fixed to execute topic rewriting first
- Improve the null value handling when rule engine persists data to InfluxDB and Tablestore, now null values will not be written
- Fix the issue that rule engine could not encode MQTT 5.0 user properties
- Fix the issue that the count of `connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access
- Fix the issue that the UDP listeners of LwM2M and CoAP gateways could not bind to the specified network interface
- Fix Dashboard not starting after removing the default Dashboard user in the configuration file
- Fix `client.subscribe` hook not being able to reject subscriptions
- If the placeholder in the ACL rule is not replaced, the client's publish or subscribe operation will be rejected
- Fix the issue that TLS was enabled but no TLS connection was actually established with Pulsar

## 4.4.4

*Release Date: 2022-06-01*

### Enhancement

- Add more time transformation functions to the SQL of rule engine
- Add the `float2str/2` function to the SQL of rule engine to support specifying the output precision of floating point numbers
- Rule engine supports message persistence to Alibaba TableStore
- Rule engine supports connecting to Pulsar using Basic and JWT authentication
- Add `service_name` option to Oracle resource of rule engine to support Oracle Database RAC
- Support for using JWT for authorization, now MQTT clients can authorize using specific claims that include a pub-sub whitelist
- Improved authentication related metrics to make it easier to understand, now `client.authenticate = client.auth.success + client.auth.failure`
- Support binding the listener of the REST API to a specified network interface
- Upload license will be automatically synchronized to the entire cluster, no need for each node to upload separately, provide HTTP API
- Support multi-condition query and fuzzy query for user data in authentication and authorization using built-in database as data source
- Supports querying clients using the length of the message queue and the number of dropped messages as conditions
- Support to configure the log time format to be compatible with the time format in older versions
- When `use_username_as_clientid` is configured to `true` and the client connects without specifying a `username`, the connection is now rejected with a reason code `0x85`
- Full randomisation of app secrets (previously partially randomised)
- When using CLI for backup and recovery, it is no longer required that the backup file must be located in the `backup` folder of the EMQX data directory
- Hot upgrades between incompatible versions will now be rejected
- Allow white spaces in EMQX's installation path
- Boot script fail fast on invalid node name (improve error message readability)

### Bug Fixes

- Fix the issue that the client could not get the message after going online when using the PostgreSQL offline message plugin
- Fix the issue that the rules engine could not successfully establish a TLS connection with Pulsar in some cases
- Fix the issue that rule engine's SQL function `hexstr_to_bin/1` could not handle half-byte
- Fix the issue that the alarm was not cleared when the rule engine resource was deleted
- Fix Dashboard HTTPS listener's `verify` option not taking effect
- Fix the issue that messages were lost when the peer session was terminated during the delivery of QoS 1 messages through shared subscriptions
- Fix the issue that when the log tracer encounters large packets, the heap size grows too fast and triggers the policy of forcibly closeing the connection process
- Fix the issue that the relevant hooks were not properly uninstalled when the module was disabled, resulting in abnormal functions
- Fix the issue that the MQTT-SN client would be disconnected when retransmitting QoS 2 messages
- Fix the issue that modules that were turned off in the backup file would be automatically enabled after restoring the backup
- Fix the issue that the subscriber's connection was disconnected due to the wrong user properties type in the message publishing API `api/v4/mqtt/publish`
- Fix DynamoDB driver not adapting to OTP 24, causing it to be unavailable
- Fix the issue that some authentication algorithms were unavailable due to the PostgreSQL driver not adapting to OTP 24
- Fix the issue that the returned results did not match the query conditions when querying subscriptions with multiple conditions
- Fix rule engine resource connection test not working
- Fix multiple Dashboard display issues

## 4.4.3

*Release Date: 2022-04-18*

### Enhancement

- Schema registry now supports decoding arbitrary binary payloads to JSON data using gRPC services
- Support for connecting to Pulsar using TLS
- Add `mongo_date` function for SQL in rule engine, which supports saving timestamps as MongoDB Date objects
- Rule engine supports resetting metrics of the specified rule
- Add connection confirmation and authorization completion events to the rule engine
- Rule engine supports copying rule for fast reuse
- SQL in rule engine supports zip, gzip and other compression and decompression functions
- Improve the error message when rule engine fails to parse payload
- Improve the connection test for some resources in rule engine
- Support setting execution priority for ExHook
- ExHook callback interface adds a Protobuf field `RequestMeta meta` to return the EMQX cluster name
- Support `local` policy for shared subscriptions, which will preferentially send messages to shared subscribers under the node where messages flow in. In some scenarios, the efficiency of shared message scheduling will be improved, especially when the MQTT bridge is configured as a shared subscription
- `RSA-PSK-AES256-GCM-SHA384`, `RSA-PSK-AES256-CBC-SHA384`, `RSA-PSK-AES128-GCM-SHA256` and `RSA-PSK-AES128-CBC- SHA256` four new TLS PSK cipher suites are supported, removing two insecure cipher suites `PSK-3DES-EDE-CBC-SHA` and `PSK-RC4-SHA` from the default configuration
- Diagnostic logging for `wait_for_table` of mnesia
  - Prints check points of mnesia internal stats
  - Prints check points of per table loading stats, help to locate the problem of long table loading time.
- Subscribing to an empty topic is prohibited in strict mode
- Generate default files when `loaded_modules` and `loaded_plugins` files do not exist

### Bug Fixes

- Fix the issue that the TLS configuration item `server_name_indication` is set to disable and does not take effect
- Fix potential process leak issue in MongoDB driver
- Fix the issue that the password of the default Dashboard user modified via the CLI command would be reset after the node leaves the cluster
- Silence grep and sed warnings in `docker-entrypoint.sh`
- Fix the backup file cannot be deleted and downloaded when the API path contains ISO8859-1 escape characters
- Fix the issue that the Redis driver would crash when DNS resolution failed, etc
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- When creating a rule, if a rule with the same ID already exists, the rules engine will now report an error instead of replacing the existing rule
- Fix the issue that the HTTP driver process pool may not be deleted
- Fix the issue that the module parameters could not be updated again after failing to update
- Fix the incorrect type of some fields in the GB/T 32960 access gateway module in Dashboard
- Fix the issue that the configuration of Bridge resources such as Kafka and Pulsar could not be updated
- Fix the issue that JT/T 808 client authentication fails when anonymous authentication is enabled

## 4.4.2

*Release Date: 2022-04-01*

### Important changes

- For Docker images, the configuration directory `/opt/emqx/etc` has been removed from the VOLUME list, making it easier for users to rebuild images with changed configurations.
- CentOS 7 Erlang runtime rebuilt on OpenSSL-1.1.1n (previously 1.0), prior to v4.3.13, EMQX will fail to handshake and trigger `malformed_handshake_data` exception when clients use certain cipher suites.
- CentOS 8 Erlang runtime system rebuilt on RockyLinux 8. `centos8` will remain in the package name for backward compatibility.

### Enhancement

- Add Pulsar proxy support for rule engine bridging data to Pulsar.
- Add OOM protection for Kafka producers.
- Add command line interface `emqx_ctl pem_cache clean` to allow forcibly clear x509 certificate cache to reload immediately after certificate file update.
- Refactored ExProto so that anonymous clients can also be displayed on Dashboard.
- Topic configuration items in bridges can now use `${node}` placeholders.
- Add validation of UTF-8 strings in MQTT packets in strict mode. When set to `true`, invalid UTF-8 strings will cause the client to disconnect.
- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the writing precision of rule engine floating point data from 10 decimal places to 17 decimal places.
- EMQX will prompt how to modify the initial password of Dashboard at startup.

### Bug Fixes

- Fix `MQTT Subscriber` module not being able to use two-way SSL connection.
- Fix the issue that `PSKFile` module failed to start.
- Fix the issue that `Kafka Consumer Group` module could not process binary data.
- Fix the issue that `Log Trace` could not be stopped.
- Fix the issue that the alternate action could not be triggered when the action of the rule engine persisting data to Oracle and Lindorm (only synchronous operation) failed to execute.
- Fix an issue where rule engine data persistence to Oracle failed but the success count still increased.
- Fix the issue that some zone configurations could not be cleared.
- Fix an issue where changes to some monitoring and alarm configurations were invalid after restarting.
- Fix the issue that `Schema Registry` is not available in the cluster environment.
- Fix the issue that the LwM2M client list query API returned incorrect data in a cluster environment, which resulted in the inability to access the LwM2M gateway module management page.
- Fix the issue that the JT/T 808 location report frame was parsed incorrectly.
- Fix the issue that the el8 installation package cannot be started on Amazon Linux 2022, the error content is `errno=13 Permission denied`.
- Fix an issue where the client could not reconnect if the connection process was blocked in some cases. Now waiting for more than 15 seconds without a response will force the old connection process to be closed.
- Fix the issue of query resource request timeout when rule engine resource is unavailable.
- Fix the issue of `{error, eexist}` error when re-run after hot upgrade failed.
- Fix an issue where publishing to a non-existing topic alias would crash the connection.
- Fix 500 error when querying lwm2m client list on another node via HTTP API.
- Fix HTTP API for subscribing topics crashes when invalid QoS are passed in.
- Fix the issue that the connection count was not updated because the related resources were not released when the connection process accessed through the ExProto exited abnormally.
- Fix an issue where the value of `server_keepalive` configuration item would be incorrectly applied to MQTT v3.1.1 clients.
- Fix Stomp client not firing `$event/client_connection` event messages.
- Fix the issue that the system memory alarm was incorrectly activated when EMQX was started.
- Fixed an issue where messages that failed to be delivered due to unregistered topics were not retransmitted when topics were successfully registered with the MQTT-SN client.
- Fix EMQX startup output error log when duplicate plugins are configured in `loaded_plugins` file.
- Fix MongoDB related features outputting excessive error logs when configured incorrectly.
- Add format check for Dashboard User and AppID, special characters such as `/` are not allowed.
- Corrected the reason code in the DISCONNECT packet returned when kicking the client to `0x98`.
- Auto subscriptions will ignore empty topics.

## 4.4.1

*Release Date: 2022-02-18*

NOTE: 4.4.1 is in sync with: 4.3.7.
The compare base of this change set is 4.4.0.

### Important changes

- A cluster-wide total connections calculation bug was fixed in in Enterprise edition 4.4.1. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation.
- The slow subscription feature is improved. It supports counting the time spent in the process of message transmission, and recording and displaying time-consuming clients and topics.
**Users planning to upgrade should be aware of the possibility that this change may cause clients to reach the license limit and not be able to connect.**
- Rules engine supports Lindorm database
- Support client-level message drop metrics
- Optimize online Trace log display on Dashboard, support syntax highlighting

### Minor changes

- Support alarm about the usage rate of license connections. By default, the number of connections reaches 80% of the allowed number of licenses, and the alarm is raised. When it is less than 75%, the alarm is cleared. User can also customize in `emqx.conf`: `license.connection_high_watermark_alarm` , `license.connection_low_watermark_alarm`
- Support alarm about license expiration, when the validity period is less than 30 days, the alarm will be raised
- Rule engine supports the configuration of rules and actions for the event of abnormal loss of client messages to enhance the user's custom processing capabilities in this scenario
- Improve the relevant metrics during the execution of the rule engine SQL matching
- Fuzzy search on client supports special characters such as `*`, `(`, `)`
- Improve ACL-related metrics to solve the issue that the count does not increase due to hitting the ACL cache
- Added `connected_at` field to webhook event notifications
- Log client state before terminating client due to holding the lock too long

### Bug Fixes

- Fix the issue that data import and export were not available in some cases
- The module update mechanism is improved to solve the issue that the module is unavailable after the update fails
- Fix the issue that the rule engine did not perform type checking when executing the size comparison statement
- Fix the issue that the related counts are cleared after updating the rule engine action
- Fixed the issue that the metrics interface does not return authentication metrics such as `client.acl.deny` by default
- Fixed the issue that the subscription query interface did not return paginated data
- Fix the issue of parsing failure when STOMP handles TCP sticky packets
- Fix the issue where the session creation time option was not available when filtering clients
- Fix the issue where memory alarms might not be triggered after restarting
- Fix the crash of import data when user data exists in `emqx_auth_mnesia` plugin

## 4.4.0

*Release Date: 2021-12-21*

EMQX Enterprise 4.4.0 mainly includes the following changes:

### Important changes

- Starting from 4.4, EMQX releases are named with Erlang/OTP release in the package name. e.g. `emqx-ee-4.4.0-otp24.1.5-3-centos7-arm64.rpm`

- **For Debian/Ubuntu users**, Debian/Ubuntu package (deb) installed EMQX now now run on systemd. This is to use systemd's supervision functionality to ensure that EMQX service restarts after a crash. The package installation service upgrade from init.d to systemd has been verified, but it is still recommended that you verify and confirm again before deploying to the production environment, at least to ensure that systemd is available in your system

- Rule engine InfluxDB integration adds support for InfluxDB v2 API, rule engine supports InfluxDB 2.0 and InfluxDB Cloud now

- Rule engine adds support for SAP Event Mesh

- Rule engine adds support for MatrixDB

- MongoDB integration supports DNS SRV and TXT Records resolution, which can seamlessly connect with MongoDB Altas

- Supports trace online, users can complete the tracking operation of the client and topic on the Dashboard, and view or download the trace log

- Supports slow subscription statistics, which can be used to find abnormal situations such as message blockage in the production environment in time

- Support dynamic modification of MQTT Keep Alive to adapt to different energy consumption strategies

- Support 4.3 to 4.4 rolling upgrade of clustered nodes. See upgrade document for more dtails.

- TLS for cluster backplane (RPC) connections. See [clustering document](../advanced/cluster.md#using-tls-for-backplane-connections) for details.

### Minor changes

- Dashboard supports viewing the number of active client connections

- Dashboard supports relative paths and custom access paths

- Dashboard remove tab navigation

- Support configuring whether to write integer data to InfluxDB as floating point type

- Supports configuring whether to forward retained messages with empty payload to suit users who are still using MQTT v3.1. The relevant configurable item is `retainer.stop_publish_clear_msg`

- Multi-language hook extension (ExHook) supports dynamic cancellation of subsequent forwarding of client messages

- Rule engine SQL supports the use of single quotes in FROM clause, for example: `SELECT * FROM't/#'`

- Optimize the use and interaction of the built-in access control file module

- Change the default value of the `max_topic_levels` configurable item to 128. Previously, it had no limit (configured to 0), which may be a potential DoS threat

- Improve the error log content when the Proxy Protocol message is received but the `proxy_protocol` configuration is not turned on

- Add additional message attributes to the message reported by the gateway. Messages from gateways such as CoAP, LwM2M, Stomp, ExProto, etc., when converted to EMQX messages, add fields such as protocol name, protocol version, user name, client IP, etc., which can be used for multi-language hook extension (ExHook)

- HTTP client performance improvement

- Add openssl-1.1 to RPM dependency

### Bug Fixes

- Fix the issue that the client process becomes unresponsive due to the blockage of RPC calls between nodes

- Fix the issue that the lock management process `ekka_locker` crashes after killing the suspended lock owner

- Fix the issue of garbled data when writing data to RocketMQ asynchronously

- Fix the issue of inaccurate statistics of RocketMQ

- Fix the display error of the Dashboard monitoring page when the number of cluster nodes exceeds seven

- Fix the issue that the rule engine may have a higher failure rate when saving data to MySQL

- Fix the issue that the Clickhouse-based offline messaging feature of the rule engine is unavailable

- Fix the issue that the Max Returned Count option in the MongoDB-based offline message feature of the rule engine cannot be used

- Fix the issue that the Path parameter of WebHook action in rule engine cannot use the rule engine variable

- Fix MongoDB authentication module cannot use Replica Set mode and other issues

- Fix the issue of out-of-sequence message forwarding between clusters. The relevant configurable item is `rpc.tcp_client_num`

- Fix the issue of incorrect calculation of memory usage

- Fix MQTT bridge malfunction when remote host is unreachable (hangs the connection)

- Fix the issue that HTTP headers may be duplicated

## 4.3.19

*Release Date: 2023-03-03*

### Enhancements

- Add TCP keepalive configuration for Kafka client.

- Improve error messages in the dashboard when adding users to the internal auth database.

- The plugin `schema_registry` will be enabled by default as an optional feature of `rule_engine`.

### Bug fixes

- Fix the problem that new MQTT TLS connections failed to establish after release hot upgrade.
  For more detailed information please see: [emqx/esockd#170](https://github.com/emqx/esockd/pull/170).

- fix the issue that produce messages to RocketMQ cluster using rule-engine failed.

- fix some issues in descriptions of the actions, resources amd emqx-modules.

- fix there's no error logs when query the JWKS server failed.

- Fixed an error when forward MQTT messages with User-Property using the `republish` action.

- Fix the problem of sending offline messages to clients in reverse order when using Redis offline message feature.

- Fix the problem that the same request sent to different EMQX nodes returns inconsistent results when sending the HTTP API to get the client list in paging mode.
  Before this change, different lists of clients will be returned if one sends
  'GET http://localhost:8081/api/v4/clients?_page=1&_limit=1000' to different
  EMQX nodes in the cluster.

- When uploading a license, now EMQX will always reload the license, to avoid the case where a user replaces the current license file with new contents.

- Only create EMQX modules locally when the emqx_modules application is started.
  Before this change, we RPC to all the nodes to create/recreate modules when emqx_modules application
  get started, so finally we created modules N^2 times on all the nodes (N times on each node).

- Password format for new dashboard users is no longer limited to ^[A-Za-z0-9]+[A-Za-z0-9-_]*$.

- Returning a failure when creating an existing listener in a cluster using the API.

- Delete the files directory when `resources/modules/schema_registry` were deleted to avoid files leaking.

## 4.3.18

*Release Date: 2022-12-29*

### Enhancements

- Fix the "ORA-01000: maximum open cursors exceeded" problem when sending data via the emqx oracle resource [#1560](https://github.com/emqx/emqx-enterprise/pull/1560).

- Add more PSK ciphers support [#1619](https://github.com/emqx/emqx-enterprise/pull/1619).

- Upgrade Erlang/OTP from 23.3.4.9-3 to 23.3.4.18-1 [#1660](https://github.com/emqx/emqx-enterprise/pull/1660).

### Bug fixes

- Fixed an issue where the configuration would not be reloaded on all nodes in a cluster after importing a backup configuration. [#1486](https://github.com/emqx/emqx-enterprise/pull/1486)

- Fixed an issue where the HTTP API would fail to download a backup configuration file when downloading it from a node where it does not reside in. [#1486](https://github.com/emqx/emqx-enterprise/pull/1486)

- Add validations for the config fields of Kafka resource [#1511](https://github.com/emqx/emqx-enterprise/pull/1511).
  Before this change, when creating Kafka resources, some of the config fields had no validity check, such
  as duration and byte size. Even if arbitrary strings were passed to these fields, the resource
  could be created successfully, then later leads to runtime error (until resource is updated).

- Avoid delete offline message twice [#1522](https://github.com/emqx/emqx-enterprise/pull/1522).
  EMQX while delete offline message in external database when subscriber send a PUBACK or PUBREC packet.
  But a message with `retain = true` will be stored twice (in retainer and external database) in case retain message and offline message are used in same time.
  The reduplicated PUBACK and PUBREC will trigger deleted action twice. And the action-metrics will also increase caused by Rule-SQL execution suceeeed.
  In most cases this does not generate any exceptions or errors, and only a few databases will report that the message to be deleted does not exist on the second delete.
  This change will avoid redundant offline message deletion operations.

- Upgrade http client library `ehttpc` from `0.2.1` to `0.4.2` [#1587](https://github.com/emqx/emqx-enterprise/pull/1587).

- Added topic validation for `emqx_mod_rewrite`. The dest topics contains wildcards are not allowed to publish [#1589](https://github.com/emqx/emqx-enterprise/pull/1589).

- The offline message clickhouse action prints an info level log: `Destroyed .. Successfully` when enabling a rule [#1594](https://github.com/emqx/emqx-enterprise/pull/1594).

- Now the rules can be created even though the corresponding resources are not ready [#1620](https://github.com/emqx/emqx-enterprise/pull/1620).
  Before this change, one cannot create rules without getting the resources connected. We made it
  possible in this change, but the newly created rule will be in `disabled` state.

- Fixed `cluster/invite_node` crash if node name is not provided [#1531](https://github.com/emqx/emqx-enterprise/pull/1531).

- Fixed broken error message for bad rpc in `/load_rebalance/{node}/evacuation/start` [#1572](https://github.com/emqx/emqx-enterprise/pull/1572).

- Fixed load bootstrap file when no bootstrap user in `mqtt_app` [#1600](https://github.com/emqx/emqx-enterprise/pull/1600).

- The expired 'awaiting_rel' queue is not cleared after persistent session MQTT client disconnected [#1574](https://github.com/emqx/emqx-enterprise/pull/1574).
  Before this change, if the 'awaiting_rel' queue is full when the MQTT client reconnect to the broker and publish a QoS2 message, the client will get
  disconnected by the broker with reason code RC_RECEIVE_MAXIMUM_EXCEEDED(0x93),
  even if the packet IDs in the 'awaiting_rel' queue have already expired.

- Trigger `message.dropped` hook when QoS2 message is resend by client with a same packet id, or 'awaiting_rel' queue is full [#1605](https://github.com/emqx/emqx-enterprise/pull/1605).

- After a reconnect, the unacknowledged QoS1/QoS2 messages in non-clean session were not retransmitted periodically as before the reconnect [#1617](https://github.com/emqx/emqx-enterprise/pull/1617).
  The configuration `zone.<zone-name>.retry_interval` specifies the retransmission interval of
  unacknowledged QoS1/QoS2 messages (defaults to 30s).
  Prior to this fix, unacknowledged messages buffered in the session are re-sent only once after session take-over, but not retried at configured interval.

- Fix Rule-Engine action `Data to InfluxDB` exection failed since hot-upgrade from `e4.3.0..e4.3.10` to `e4.3.11..e4.3.17` [#1601](https://github.com/emqx/emqx-enterprise/pull/1601).

- Added validation for Kafka action parameters, Segment Bytes should not be greater than Max Bytes [#1608](https://github.com/emqx/emqx-enterprise/pull/1608).

- Added validation for the duration and bytesize parameters of Pulsar actions [#1631](https://github.com/emqx/emqx-enterprise/pull/1631).

- Authentication for RocketMQ resource not working [#1561](https://github.com/emqx/emqx-enterprise/pull/1561).
  In this change we moved the configuration fields `access_key`, `secret_key`
  and `security_token` from the `data_to_rocket` action to the `bridge_rocket`
  resource. And we also added a new field `namespace` for RocketMQ services in
  Aliyun cloud.

- When dashboard creates a new user, the password length must match 3-32 and the format is `^[A-Za-z0-9]+[A-Za-z0-9-_]*$` [#1599](https://github.com/emqx/emqx-enterprise/pull/1599).

- When resource creation is too slow, there may be some temporary probing connections left [#1641](https://github.com/emqx/emqx-enterprise/pull/1641).

- Add the `SNI` field for SSL connection configuration of Kafka resource [#1647](https://github.com/emqx/emqx-enterprise/pull/1647).

- Fixed the issue that the MongoDB resource connection process was slow when authentication was enabled [#1669](https://github.com/emqx/emqx-enterprise/pull/1669).

- Fixed the issue that after the release hot upgrade, EMQX occasionally alarms resources down, and the alarms could not be automatically cleared [#1668](https://github.com/emqx/emqx-enterprise/pull/1668).

## 4.3.17

*Release Date: 2022-11-26*

### Enhancements

- Support to use placeholders like `${var}` in the `Collection` field of Rule-Engine's MongoDB actions [#1503](https://github.com/emqx/emqx-enterprise/pull/1503).

- Add a format check to the `host` field of the InfluxDB resource in Rule-Engine [#1426](https://github.com/emqx/emqx-enterprise/pull/1426).
  The host field should be an ip/domain without scheme and port.

- Make sure listener's `tls_versions` config value is one or more of `tlsv1`, `tlsv1.1`, `tlsv1.2`, `tlsv1.3` [#9260](https://github.com/emqx/emqx/pull/9260).

- Remove useless information from the dashboard listener failure log [#9260](https://github.com/emqx/emqx/pull/9260).

- We now trigger the `'message.acked'` hook after the CoAP gateway sends a message to the device and receives the ACK from the device [#9264](https://github.com/emqx/emqx/pull/9264).
  With this change, the CoAP gateway can be combined with the offline message caching function (in the
  emqx enterprise), so that CoAP devices are able to read the missed messages from the database when
  it is online again.

- Support to use placeholders like `${var}` in the HTTP `Headers` of Rule-Engine's Webhook actions [#9239](https://github.com/emqx/emqx/pull/9239).

- Asynchronously refresh the resources and rules during emqx boot-up [#9199](https://github.com/emqx/emqx/pull/9199).
  This is to avoid slowing down the boot if some resources spend long time establishing the connection.

- Add a warning log if the ACL check failed for subscription [#9124](https://github.com/emqx/emqx/pull/9124).
  This is to make the ACL deny logging for subscription behave the same as for publish.

- JWT ACL claim supports `all` action to imply the rules applie to both `pub` and `sub` [#9044](https://github.com/emqx/emqx/pull/9044).

- Added a log censor to avoid logging sensitive data [#9189](https://github.com/emqx/emqx/pull/9189).
  If the data to be logged is a map or key-value list which contains sensitive key words such as `password`, the value is obfuscated as `******`.

- Enhanced log security in ACL modules, sensitive data will be obscured [#9242](https://github.com/emqx/emqx/pull/9242).

- Add `management.bootstrap_apps_file` configuration to bulk import default app/secret when EMQX initializes the database [#9273](https://github.com/emqx/emqx/pull/9273).

- Added two new configs for deterministic order of authentication and ACL checks [#9283](https://github.com/emqx/emqx/pull/9283).
  The two new global config names are `auth_order` and `acl_order`.
  When multiple ACL or auth plugins (or modules) are enabled, without this config, the order (in which each backend is queried)
  is determined by the start/restart order of the plugin (or module).
  Meaning, if a plugin (or module) is restarted after initial boot, it may get ordered to the end of the list.
  With this config, you may set the order with a comma-speapated ACL or auth plugin names (or aliases).
  For example: `acl_order = jwt,http`, this will make sure `jwt` is always checked before `http`,
  meaning if JWT is not found (or no `acl` cliam) for a client, then the ACL check will fallback to use the HTTP backend.

- Added configurations to enable more `client.disconnected` events (and counter bumps) [#9267](https://github.com/emqx/emqx/pull/9267).
  Prior to this change, the `client.disconnected` event (and counter bump) is triggered when a client
  performs a 'normal' disconnect, or is 'kicked' by system admin, but NOT triggered when a
  stale connection had to be 'discarded' (for clean session) or 'takeovered' (for non-clean session) by new connection.
  Now it is possible to set configs `broker.client_disconnect_discarded` and `broker.client_disconnect_takeovered` to `on` to enable the event in these scenarios.

- For Rule-Engine resource creation failure, delay before the first retry [#9313](https://github.com/emqx/emqx/pull/9313).
  Prior to this change, the retry delay was added *after* the retry failure.

### Bug fixes

- Fixed the option to choose the `reset_by_subscriber` offset reset
  policy in Kafka Consumer [#1463](https://github.com/emqx/emqx-enterprise/pull/1463).

- Added the missing `tlsv1.3` option to `tls_versions` in hot-config [#1532](https://github.com/emqx/emqx-enterprise/pull/1532).

- Made Rule-Engine able to connect SQL server when its listening port is not the default (`1433`) [#1464](https://github.com/emqx/emqx-enterprise/pull/1464).

- Make sure Schema-Registry API supports Percent-encoding `name` in HTTP request URI [#1497](https://github.com/emqx/emqx-enterprise/issues/1497).
  Note that the `name` in `POST /api/v4/schemas` request body should not be percent-encoded as it's a JSON field value.

- Fix an upgrade issue for JWT authentication plugin [#1554](https://github.com/emqx/emqx-enterprise/pull/1554).
  When upgrading from e4.3.9 or earlier, an EMQX internal resource which holds the keys will have to be restarted,
  during the restart, clients may fail to be authenticated.

- Fix that after uploading a backup file with an non-ASCII filename, HTTP API `GET /data/export` fails with status code 500 [#9224](https://github.com/emqx/emqx/pull/9224).

- Improve the display of rule's 'Maximum Speed' counter to only reserve 2 decimal places [#9185](https://github.com/emqx/emqx/pull/9185).
  This is to avoid displaying floats like `0.30000000000000004` on the dashboard.

- Fix the issue that emqx prints too many error logs when connecting to mongodb but auth failed [#9184](https://github.com/emqx/emqx/pull/9184).

- Fix that after receiving publish in `idle mode` the emqx-sn gateway may panic [#9024](https://github.com/emqx/emqx/pull/9024).

- "Pause due to rate limit" log level demoted from warning to notice [#9134](https://github.com/emqx/emqx/pull/9134).

- Restore old `emqx_auth_jwt` module API, so the hook callback functions registered in older version will not be invalidated after hot-upgrade [#9144](https://github.com/emqx/emqx/pull/9144).

- Fixed the response status code for the `/status` endpoint [#9210](https://github.com/emqx/emqx/pull/9210).
  Before the fix, it always returned `200` even if the EMQX application was not running.  Now it returns `503` in that case.

- Fix message delivery related event encoding [#9226](https://github.com/emqx/emqx/pull/9226)
  For Rule-Engine's input events like `$events/message_delivered`, and `$events/message_dropped`,
  if the message was delivered to a shared-subscription, the encoding (to JSON) of the event will fail.
  Affected versions: `v4.3.21`, `v4.4.10`, `e4.3.16` and `e4.4.10`.

- Make sure Rule-Engine API supports Percent-encoding `rule_id` and `resource_id` in HTTP request path [#9190](https://github.com/emqx/emqx/pull/9190).
  Note that the `id` in `POST /api/v4/rules` should be literals (not encoded) when creating a `rule` or `resource`.
  See docs [Create Rule](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-rules) [Create Resource](https://docs.emqx.com/en/enterprise/v4.4/advanced/http-api.html#post-api-v4-resources).

- Calling 'DELETE /alarms/deactivated' now deletes deactived alarms on all nodes, including remote nodes, not just the local node [#9280](https://github.com/emqx/emqx/pull/9280).

- When republishing messages or bridge messages to other brokers, check the validity of the topic and make sure it does not have topic wildcards [#9291](https://github.com/emqx/emqx/pull/9291).

- Disable authorization for `api/v4/emqx_prometheus` endpoint on management api listener (default 8081) [#9294](https://github.com/emqx/emqx/pull/9294).

## 4.3.16

*Release Date: 2022-10-14*

### Enhancements

- Added more Kafka action parameter checks
  - TCP send buffer size and max batch size parameters are not allowed to be left blank from the configration UI.
  - The combination of "Produce Strategy" set to 'key_dispatch' and the "Key" set to "none" is now not allowed,
    the dashboard will get an error with text like: "with strategy set to 'key_dispatch', key is not allowed to be 'none'"

- TLS listener memory usage optimization [#9005](https://github.com/emqx/emqx/pull/9005).
  New config `listener.ssl.$NAME.hibernate_after` to hibernate TLS connection process after idling.
  Hibernation can reduce RAM usage significantly, but may cost more CPU.
  This configuration is by default disabled.
  Our preliminary test shows a 50% of RAM usage decline when configured to '5s'.

- TLS listener default buffer size to 4KB [#9007](https://github.com/emqx/emqx/pull/9007).
  Eliminate uncertainty that the buffer size is set by OS default.

- Disable authorization for `api/v4/emqx_prometheus` endpoint [#8955](https://github.com/emqx/emqx/pull/8955).

- Added a test to prevent a last will testament message to be
  published when a client is denied connection [#8894](https://github.com/emqx/emqx/pull/8894).

- More rigorous checking of flapping to improve stability of the system [#9045](https://github.com/emqx/emqx/pull/9045).

- QoS1 and QoS2 messages in session's buffer are re-dispatched to other members in the group
  when the session terminates [#9094](https://github.com/emqx/emqx/pull/9094).
  Prior to this enhancement, one would have to set `broker.shared_dispatch_ack_enabled` to `true`
  to prevent sessions from buffering messages, however this acknowledgement costs extra resources.

- Fix delayed publish timing inaccuracy caused by OS time change [#8908](https://github.com/emqx/emqx/pull/8908).

### Bug fixes

- Fix `load_modules` reset after new node joins the cluster.
  Prior to this fix, if `load_modules` for a cluster has been changed, adding a new node to the cluster with default modules
  would cause the other nodes to reset to default too.
  In this fix, the node which is going to join the cluster will copy the `loaded_modules` from the oldest node in the cluster.

- Fix getting subscriptions from backends successfully with QoS values out of range [0, 2].
  Before this change, when we add subscriptions for clients from backends like Redis or MySQL, we won't validate the QoS.
  For example if the QoS is an integer -1, the topic was still subscribed successfully with QoS -1,
  if we send a message to this topic, then an error will occur and the MQTT connection will crash.
  After this change QoS will be clamped into range [0, 2].

- Fix Rule-Engine increased 'success' counter when get subscriptions from Redis failed (due to query Redis timeout).

- Fix Rule-Engine increased 'success' counter when saving offline messages with QoS = 0.
  We don't allow saving offline messages to backends with QoS = 0, so we need to increase the 'failed' counter instead of the 'success' counter in this case.

- Fix the `verify` field is missing from the SSL settings of redis-cluster and redis-sentinel resources.

- Fixed Redis resource liveness problem issue. Prior to this fix, the resource is considered alive when connection can be established.
  The fix is to perform a PING query to make sure the service is alive.

- Fix the redis-cluster resource prints too many error logs when Redis servers are not avaliable.

- Fixed an internal Redis resource ID clashing. This clashing may cause resources in use getting deleted when deleting another resource.

- Mask secret/password in the resource/module creation UI.

- Fix HTTP client library to handle SSL socket passive signal [#9145](https://github.com/emqx/emqx/pull/9145).

- Hide Redis password in error logs [#9071](https://github.com/emqx/emqx/pull/9071).
  More changes in Redis client included in this release:
  - Improve Redis connection error logging [eredis#19](https://github.com/emqx/eredis/pull/19).
    Also added support for eredis to accept an anonymous function as password instead of
    passing around plaintext args which may get dumpped to crash logs (hard to predict where).
    This change also added `format_status` callback for `gen_server` states which hold plaintext
    password so the process termination log and `sys:get_status` will print '******' instead of
    the password to console.
  - Avoid pool name clashing [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22.
    Same `format_status` callback is added here too for `gen_server`s which hold password in
    their state.

- Fix shared subscription message re-dispatches [#9094](https://github.com/emqx/emqx/pull/9094).
  - When discarding QoS 2 inflight messages, there were excessive logs
  - For wildcard deliveries, the re-dispatch used the wrong topic (the publishing topic,
    but not the subscribing topic), caused messages to be lost when dispatching.

- Fix shared subscription group member unsubscribe issue when 'sticky' strategy is used.
  Prior to this fix, if a previously picked member unsubscribes from the group (without reconnect)
  the message is still dispatched to it.
  This issue only occurs when unsubscribe with the session kept.
  Fixed in [#9119](https://github.com/emqx/emqx/pull/9119)

- Fix shared subscription 'sticky' strategy when there is no local subscriptions at all.
  Prior to this change, it may take a few rounds to randomly pick group members until a local subscriber
  is hit (and then start sticking to it).
  After this fix, it will start sticking to whichever randomly picked member even when it is a
  subscriber from another node in the cluster.
  Fixed in [#9122](https://github.com/emqx/emqx/pull/9122)

- Fix rule engine fallback actions metrics reset [#9125](https://github.com/emqx/emqx/pull/9125).

## 4.3.15

*Release Date: 2022-09-17*

### Enhancements

- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps

### Bug fixes

- Fix rule engine update behaviour which may initialize actions for disabled rules
- Fix inaccurate delayed publish due to OS time changes
- Fix the issue that the IP address bound to the Dashboard listener did not take effect
- Fix the issue that shared subscriptions might get stuck in an infinite loop when `shared_dispatch_ack_enabled` is set to true
- Fix the issue that the rule engine SQL crashes when subject matching null values

## 4.3.14

*Release Date: 2022-08-29*

### Enhancements

- Improve the log when LwM2M packet parsing fails
- Improve the rule engine error log, the log will contain the rule ID when the action execution fails
- Improve log when `loaded_modules` and `loaded_plugins` files do not exist
- Add a guide for changing the default password on Dashboard
- Improved import performance for Protobuf Schema files

### Bug fixes

- Fix `client.disconnected` event not trigger in some cases
- Fix the issue that the JWK authentication module could not be started later when the JWKS service was not ready in time
- Fix the issue that setting the listener port via an environment variable would prevent either listener from being stopped
- Fix the issue that the built-in database authentication did not distinguish the pagination statistics of the authentication data of the client ID and username
- Fix the issue that the module status would be reset after EMQX restarts after hot upgrade
- Fix Redis driver process leak problem
- Fix rule engine MQTT bridge to AWS IOT connection timeout issue
- Fix `GET /listener` request crashing when listener is not ready
- Fix the issue that the comparison between any variable and null value in the rule engine SQL always returns false after e4.3.7
- Fix the issue that when the execution priority of ExHook is higher than that of the rule engine, the topic filtered by the ExHook Message Hook will not trigger the rule engine
- Fix the issue that the write request of TDEngine may fail because the peer end closes the network connection
- Fix the issue that the configuration of the MQTT-SN module other than the listener would not take effect
- Fix the issue that the ExHook management process was forcibly killed due to the supervisor shutdown timeout
- Fix the issue that the Client ID parameter in ExProto `client.connect` hook is not defined
- Fix ExProto not triggering disconnect event when client is kicked

## 4.3.13

*Release Date: 2022-08-11*

### Important Changes

- Upgraded the OTP version used to solve the low probability of random process unresponsiveness caused by OTP bugs. Users who are still using 4.3 are recommended to upgrade to this version
- From the next release, we will stop supporting macOS 10 and provide an installation package for macOS 11

### Enhancements

- Allows the connection process to be configured to be garbage collected after the TLS handshake is complete to reduce memory footprint, which can reduce memory consumption by about 35% per SSL connection, but increases CPU consumption accordingly
- Allows configuring the log level of the TLS handshake log to view the detailed handshake process

### Bug fixes

- Fix the issue that EMQX could not be started when deployed through Helm Chart after unmounting the `loaded_modules` file in ConfigMap

## 4.3.12

*Release Date: 2022-07-29*

### Enhancement

- Rules engine supports RocketMQ with ACL enabled
- Supports searching and paging of rules in rule engine
- Provides CLI `./bin/emqx check_conf` to actively check if the configuration is correct
- Optimize the write performance of TDEngine in the rule engine
- Optimizing Shared Subscription Performance
- Add `db_name` field to the action of rule engine writing data to TDEngine to improve support for super table

### Bug fixes

- Fix the issue that the action count is wrong when the rule engine writes to TDEngine
- Fix the issue that the process pool size setting does not take effect when the rule engine writes to HStreamDB
- Fix the issue that an error was reported when querying the subscription list after the GB/T 32960 plugin was enabled
- Fix the issue that incompatible configuration items when restoring a 4.2 backup to 4.3 with hot configuration enabled
- Fix the issue that once the old version of EMQX is uninstalled after hot upgrade, EMQX will not be able to start again
- Fix the issue that the keep-alive check for UDP clients in the Multilingual Protocol Extension was incorrect, causing clients not to expire
- Fix the issue that the client information in the Multilingual Protocol Extension was not updated in time
- Fix the issue that the license update does not take effect when running after hot upgrade to e4.3.10 and later versions
- Fix the issue that when the client specified Clean Session as false to reconnect, the shared subscription message in the flight window would be re-dispatched to the old session process
- Fix the issue that the new node did not use the cluster license after joining the cluster
- Fix the issue that the `emqx_lua_hook` plugin cannot cancel the message publishing

## 4.3.11

*Release Date: 2022-06-30*

### Enhancement

- Rule engine supports persisting data to HStreamDB
- QoS and Retain flag in rule engine's message republish actions can now use placeholders
- Supports exclusive subscriptions, that is, only one subscriber is allowed for a topic
- Dashboard and management API's HTTPS listeners can now use password-protected private key files, providing `key_password` configuration item
- Support for placeholders `%u` and `%c` in topic rewrite rules
- Support setting MQTT 5.0 properties in the API request for message publishing, such as message expiry interval, response topic, etc.
- Optimize the UI when creating rule engine resources, such as folding some uncommon options, etc.
- Opened 4 TCP-related configuration items: KeepAlive, TCP_NODELAY, SO_RCVBUF and SO_SNDBUF for the underlying gRPC connection of ExHook

### Bug fixes

- Fix the issue of inaccurate memory calculation in Linux OS, and calculate the memory usage of the current OS instead of the memory usage of EMQX
- Fix the issue that the old disconnect event of ExHook would be triggered later than the new connect event when the client reconnects
- Improve the JWT authentication module's judgment logic for startup status in a cluster environment
- Extend the timeout of DynamoDB resource status query of rule engine to avoid the problem that some overseas resources may not be available
- Fix the issue that the timestamp is not updated when the PostgreSQL data storage plugin persists messages and updates message consumption
- Fix the issue that the rules engine's Tablestore, Lindorm and InfluxDB resources did not check connection status when creating
- Fix rule engine not checking connection status when creating Tablestore, Lindorm and InfluxDB resources
- Fix rule engine not updating metrics correctly when writing to TDEngine resource fails
- Fix the issue that the Cassandra message store plugin persisted retained messages to multiple tables, causing duplicate messages
- Fix the issue that Kafka resources exported from versions prior to 4.3.0 (only when the Produce policy is set to `first_key_dispatch`) could not be imported into 4.3.0 and later
- Fix the issue that the execution order of topic rewriting and delayed publish is not fixed, now it is fixed to execute topic rewriting first
- Improve the null value handling when rule engine persists data to InfluxDB and Tablestore, now null values will not be written
- Fix the issue that rule engine could not encode MQTT 5.0 user properties
- Fix the issue that the count of `connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access
- Fix the issue that the UDP listeners of LwM2M and CoAP gateways could not bind to the specified network interface
- Fix Dashboard not starting after removing the default Dashboard user in the configuration file
- Fix `client.subscribe` hook not being able to reject subscriptions
- If the placeholder in the ACL rule is not replaced, the client's publish or subscribe operation will be rejected
- Fix the issue that TLS was enabled but no TLS connection was actually established with Pulsar

## 4.3.10

*Release Date: 2022-06-01*

### Enhancement

- Add more time transformation functions to the SQL of rule engine
- Add the `float2str/2` function to the SQL of rule engine to support specifying the output precision of floating point numbers
- Rule engine supports connecting to Pulsar using Basic and JWT authentication
- Add `service_name` option to Oracle resource of rule engine to support Oracle Database RAC
- Support for using JWT for authorization, now MQTT clients can authorize using specific claims that include a pub-sub whitelist
- Improved authentication related metrics to make it easier to understand, now `client.authenticate = client.auth.success + client.auth.failure`
- Support binding the listener of the REST API to a specified network interface
- Upload license will be automatically synchronized to the entire cluster, no need for each node to upload separately, provide HTTP API
- Support multi-condition query and fuzzy query for user data in authentication and authorization using built-in database as data source
- Supports querying clients using the length of the message queue and the number of dropped messages as conditions
- Support to configure the log time format to be compatible with the time format in older versions
- When `use_username_as_clientid` is configured to `true` and the client connects without specifying a `username`, the connection is now rejected with a reason code `0x85`
- Full randomisation of app secrets (previously partially randomised)
- Hot upgrades between incompatible versions will now be rejected
- Allow white spaces in EMQX's installation path
- Boot script fail fast on invalid node name (improve error message readability)

### Bug fixes

- Fix the issue that the client could not get the message after going online when using the PostgreSQL offline message plugin
- Fix the issue that the rules engine could not successfully establish a TLS connection with Pulsar in some cases
- Fix the issue that rule engine's SQL function `hexstr_to_bin/1` could not handle half-byte
- Fix the issue that the alarm was not cleared when the rule engine resource was deleted
- Fix Dashboard HTTPS listener's `verify` option not taking effect
- Fix the issue that messages were lost when the peer session was terminated during the delivery of QoS 1 messages through shared subscriptions
- Fix the issue that when the log tracer encounters large packets, the heap size grows too fast and triggers the policy of forcibly closeing the connection process
- Fix the issue that the relevant hooks were not properly uninstalled when the module was disabled, resulting in abnormal functions
- Fix the issue that the MQTT-SN client would be disconnected when retransmitting QoS 2 messages
- Fix the issue that modules that were turned off in the backup file would be automatically enabled after restoring the backup
- Fix the issue that the returned results did not match the query conditions when querying subscriptions with multiple conditions
- Fix rule engine resource connection test not working
- Fix multiple Dashboard display issues

## 4.3.9

*Release Date: 2022-04-18*

### Enhancement

- Schema registry now supports decoding arbitrary binary payloads to JSON data using gRPC services
- Support for connecting to Pulsar using TLS
- Add `mongo_date` function for SQL in rule engine, which supports saving timestamps as MongoDB Date objects
- Rule engine supports copying rule for fast reuse
- SQL in rule engine supports zip, gzip and other compression and decompression functions
- Improve the error message when rule engine fails to parse payload
- Improve the connection test for some resources in rule engine
- Support setting execution priority for ExHook
- ExHook callback interface adds a Protobuf field `RequestMeta meta` to return the EMQX cluster name
- Support `local` policy for shared subscriptions, which will preferentially send messages to shared subscribers under the node where messages flow in. In some scenarios, the efficiency of shared message scheduling will be improved, especially when the MQTT bridge is configured as a shared subscription
- `RSA-PSK-AES256-GCM-SHA384`, `RSA-PSK-AES256-CBC-SHA384`, `RSA-PSK-AES128-GCM-SHA256` and `RSA-PSK-AES128-CBC- SHA256` four new TLS PSK cipher suites are supported, removing two insecure cipher suites `PSK-3DES-EDE-CBC-SHA` and `PSK-RC4-SHA` from the default configuration
- Diagnostic logging for `wait_for_table` of mnesia
  - Prints check points of mnesia internal stats
  - Prints check points of per table loading stats, help to locate the problem of long table loading time.
- Subscribing to an empty topic is prohibited in strict mode
- Generate default files when `loaded_modules` and `loaded_plugins` files do not exist

### Bug fixes

- Fix the issue that the TLS configuration item `server_name_indication` is set to disable and does not take effect
- Fix potential process leak issue in MongoDB driver
- Fix the issue that the password of the default Dashboard user modified via the CLI command would be reset after the node leaves the cluster
- Silence grep and sed warnings in `docker-entrypoint.sh`
- Fix the backup file cannot be deleted and downloaded when the API path contains ISO8859-1 escape characters
- Fix the issue that the Redis driver would crash when DNS resolution failed, etc
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- When creating a rule, if a rule with the same ID already exists, the rules engine will now report an error instead of replacing the existing rule
- Fix the issue that the HTTP driver process pool may not be deleted
- Fix the issue that the module parameters could not be updated again after failing to update
- Fix the incorrect type of some fields in the GB/T 32960 access gateway module in Dashboard
- Fix the issue that the configuration of Bridge resources such as Kafka and Pulsar could not be updated
- Fix the issue that JT/T 808 client authentication fails when anonymous authentication is enabled

## 4.3.8

*Release Date: 2022-04-01*

### Important changes

- For Docker images, the configuration directory `/opt/emqx/etc` has been removed from the VOLUME list, making it easier for users to rebuild images with changed configurations.
- CentOS 7 Erlang runtime rebuilt on OpenSSL-1.1.1n (previously 1.0), prior to v4.3.13, EMQX will fail to handshake and trigger `malformed_handshake_data` exception when clients use certain cipher suites.
- CentOS 8 Erlang runtime system rebuilt on RockyLinux 8. `centos8` will remain in the package name for backward compatibility.

### Enhancement

- Add Pulsar proxy support for rule engine bridging data to Pulsar.
- Add OOM protection for Kafka producers.
- Add command line interface `emqx_ctl pem_cache clean` to allow forcibly clear x509 certificate cache to reload immediately after certificate file update.
- Refactored ExProto so that anonymous clients can also be displayed on Dashboard.
- Topic configuration items in bridges can now use `${node}` placeholders.
- Add validation of UTF-8 strings in MQTT packets in strict mode. When set to `true`, invalid UTF-8 strings will cause the client to disconnect.
- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the writing precision of rule engine floating point data from 10 decimal places to 17 decimal places.
- EMQX will prompt how to modify the initial password of Dashboard at startup.

### Bug fixes

- Fix `MQTT Subscriber` module not being able to use two-way SSL connection.
- Fix the issue that `PSKFile` module failed to start.
- Fix the issue that `Kafka Consumer Group` module could not process binary data.
- Fix the issue that `Log Trace` could not be stopped.
- Fix the issue that the alternate action could not be triggered when the action of the rule engine persisting data to Oracle and Lindorm (only synchronous operation) failed to execute.
- Fix an issue where rule engine data persistence to Oracle failed but the success count still increased.
- Fix the issue that some zone configurations could not be cleared.
- Fix an issue where changes to some monitoring and alarm configurations were invalid after restarting.
- Fix the issue that `Schema Registry` is not available in the cluster environment.
- Fix the issue that the LwM2M client list query API returned incorrect data in a cluster environment, which resulted in the inability to access the LwM2M gateway module management page.
- Fix the issue that the JT/T 808 location report frame was parsed incorrectly.
- Fix the issue that the el8 installation package cannot be started on Amazon Linux 2022, the error content is `errno=13 Permission denied`.
- Fix an issue where the client could not reconnect if the connection process was blocked in some cases. Now waiting for more than 15 seconds without a response will force the old connection process to be closed.
- Fix the issue of query resource request timeout when rule engine resource is unavailable.
- Fix the issue of `{error, eexist}` error when re-run after hot upgrade failed.
- Fix an issue where publishing to a non-existing topic alias would crash the connection.
- Fix 500 error when querying lwm2m client list on another node via HTTP API.
- Fix HTTP API for subscribing topics crashes when invalid QoS are passed in.
- Fix the issue that the connection count was not updated because the related resources were not released when the connection process accessed through the ExProto exited abnormally.
- Fix an issue where the value of `server_keepalive` configuration item would be incorrectly applied to MQTT v3.1.1 clients.
- Fix Stomp client not firing `$event/client_connection` event messages.
- Fix the issue that the system memory alarm was incorrectly activated when EMQX was started.
- Fixed an issue where messages that failed to be delivered due to unregistered topics were not retransmitted when topics were successfully registered with the MQTT-SN client.
- Fix EMQX startup output error log when duplicate plugins are configured in `loaded_plugins` file.
- Fix MongoDB related features outputting excessive error logs when configured incorrectly.
- Add format check for Dashboard User and AppID, special characters such as `/` are not allowed.
- Corrected the reason code in the DISCONNECT packet returned when kicking the client to `0x98`.
- Auto subscriptions will ignore empty topics.

## 4.3.7

*Release Date: 2022-02-11*

### Important

A cluster-wide total connections calculation bug was fixed in in EMQX Enterprise 4.3.7. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation.

Users planning to upgrade should be aware of the possibility that this change may cause clients to reach the license limit and not be able to connect.

### Enhancement

- Support alarm about the usage rate of license connections. By default, the number of connections reaches 80% of the allowed number of licenses, and the alarm is raised. When it is less than 75%, the alarm is cleared. User can also customize in `emqx.conf`: `license.connection_high_watermark_alarm` , `license.connection_low_watermark_alarm`
- Support alarm about license expiration, when the validity period is less than 30 days, the alarm will be raised
- Rule engine supports the configuration of rules and actions for the event of abnormal loss of client messages to enhance the user's custom processing capabilities in this scenario
- Improve the relevant metrics during the execution of the rule engine SQL matching
- Fuzzy search on client supports special characters such as `*`, `(`, `)`
- Improve ACL-related metrics to solve the issue that the count does not increase due to hitting the ACL cache
- Added `connected_at` field to webhook event notifications
- Log client state before terminating client due to holding the lock too long

### Bug fixes

- Fix the issue that data import and export were not available in some cases
- The module update mechanism is improved to solve the issue that the module is unavailable after the update fails
- Fix the issue that the rule engine did not perform type checking when executing the size comparison statement
- Fix the issue that the related counts are cleared after updating the rule engine action
- Fixed the issue that the metrics interface does not return authentication metrics such as `client.acl.deny` by default
- Fixed the issue that the subscription query interface did not return paginated data
- Fix the issue of parsing failure when STOMP handles TCP sticky packets
- Fix the issue where the session creation time option was not available when filtering clients
- Fix the issue where memory alarms might not be triggered after restarting
- Fix the crash of import data when user data exists in `emqx_auth_mnesia` plugin

## 4.3.6

*Release Date: 2021-12-17*

### Enhancement

- Rule engine supports Ali Lindorm database now
- Support the configuration of whether to continue to deliver empty retained messages to suit users who are still using the MQTT v3.1 protocol
- Optimize the use and interaction of the built-in access control file module

### Bug fixes

- Fix the issue of incorrect calculation of memory usage
- Fix the issue that the Path option of Webhook Action in rule engine doesn't support the use of ${Variable}
- Fix the issue of garbled data when writing data to RocketMQ asynchronously
- Fix the issue of inaccurate statistics of RocketMQ
- Fix the issue that the connection failure log will continue to be printed when updating or deleting MQTT Bridge and MQTT Subscribe resources in some cases
- Fix the issue that the rule engine may have a higher failure rate when saving data to MySQL
- Fix the issue that the Clickhouse-based offline messaging feature of the rule engine is unavailable
- Fix the issue that the Max Returned Count option in the MongoDB-based offline message feature of the rule engine cannot be used
- Fix the issue of partial hot configuration failure

## 4.3.5

*Release Date: 2021-11-05*

### Enhancement

- Improve client kick (forced step-down)
- Add support for new cipher suites for LwM2M gateway
- Introduced interleaving for priority queues (to avoid low priority queue stavation)
- HTTP authentication plugin disable superuser requests by default
- Improve InfluxDB write performance
- InfluxDB Tag Name and Field Name support the use of placeholders

### Important fixes

- Fix the issue that calls between clusters may cause the client process to lose response
- WebHook's HTTP client SSL configuration parse
- MongoDB resources allow host names
- Performance improvement for built-in database ACL (emqx_auth_mnesia)
- Fix the issue that the authentication based on the built-in database incorrectly transcodes the HTTP request parameters
- Fix the issue that the MySQL authentication module may fail to start
- Fix some issues of STOMP gateway

### Minor fixes

- Fix the wrong status of Kafka and MongoDB resources of the rule engine
- Fixed the issue that the Client ID containing "\" characters could not be searched in a fuzzy manner
- Fix the issue that variable byte integers may be larger than 4 bytes
- Fix the issue that duplicate modules may be added
- Fix the issue that the Listener cannot be restarted on the Dashboard

## 4.3.4

*Release Date: 2021-09-18*

### Enhancement

- Rule engine Data bridge to pursar supports data compression
- Extend the timeout interval for creating rules

### Bug fixes

- Fix rule engine data saved to InfluxDB performance issues
- Fixes an issue where WebHook inability to configure sni caused HTTPS to not be available in some cases
- Fix an issue where resources cannot be released after the rule is shut down by the rule engine
- Fixes an issue where the rule engine offline messages cannot be deleted after receiving them in some cases


## 4.3.3

*Release Date: 2021-08-16*

### Enhancement

- Save offline messages to Redis to support clearing residual data
- ExHook to add automatic reconnection mechanism, timeout parameters and alternative actions

### Bug fixes

- Fix the rule engine can't connect to InfluxDB using HTTPS
- Fix the rule engine data saving to InfluxDB action cannot use placeholder
- Fix the rule engine data bridging to WebServer can not use Path
- Repair grpc-client timeout processing logic.
- Fix ExProto bug, add retry logic, reduce some unnecessary printing


## 4.3.2

*Release Date: 2021-07-17*

### Enhancement

- The client adds more fields to the on/offline message

### Bug fixes

- Fix an issue where the LwM2M Gateway Management page could not be opened
- Fix an issue where custom fields cannot be resolved in a resolution location escalation for the JT/T808 gateway
- Fix an acl.conf file format error that invalidated the ACL rule
- Fix an issue where creating a auth_ldap authentication module failed
- Fix an issue that cannot be stopped in the case of a multilingual protocol resolution exception
- Fix an issue where the Rule Engine was unable to create Oracle resources
- Fix an issue where the Rule Engine failed to synchronize bulk writes to SQL Server

## 4.3.1

*Release Date: 2021-06-05*

### Enhancement

- New Rule Engine Description of the TimeRange field in Offline Message Save

### Bug fixes

- Rule engine Problems where data cannot be written in the case of An OpenTSDB exception
- An issue with the wrong display in the hotly upgraded version
- MQTT-SN protocol 'cleansession'false' client loses the topicid when recovering the session
- There is a problem with the preset module modifying the configuration after the restart
- Dashboard rule engine editing shows an error issue
- Dashboard navigation breadcrumbs show problems


## 4.3.0

*Release Date: 2021-05-19*

### Enhancement

- Rule engine supports Kafka to add partitions
- Rule engine supports offline message and auto-subscription using ClickHouse Storage
- The batch and async mode is enabled by default for the actions of the rule engine, if the actions support batch and asnyc
- Refactoring and improving the performance of data-to-InfluxDB
- Using Kafka to send MQTTmessage to support the set payload format

### Bug fixes

- The rule engine will make mistakes when editing actions
- Fix i18n translate of module in Dashboard
- The rule engine supports writing `null` to the database

## 4.2.13 Release

*Release Date: 2022-08-10*

### Enhancement

- Hot upgrades between incompatible versions will now be rejected
- Support binding the listener of the HTTP API to a specified network interface
- Boot script fail fast on invalid node name, improve error message readability

### Bug fixes

- Fix the issue that hot configuration items did not take effect after restoring from backup
- Fix the issue that the rule engine data is not replicated to disk in the cluster, resulting in the problem that the rule engine data will be lost after restarting the cluster after the only node that writes data to disk leaves the cluster
- Fix the issue that the rule engine would get a 500 error when performing a connection test on an unavailable resource
- Fix connection test always passing when creating MongoDB resource in rules engine
- Fix rule engine not updating metrics correctly when writing to TDEngine resource fails
- Extend the timeout of DynamoDB resource status query of rule engine to avoid the problem that some overseas resources may not be available
- If the placeholder in the ACL rule is not replaced, the client's publish or subscribe operation will be rejected
- Fix the issue that the execution order of topic rewriting and delayed publish is not fixed, now it is fixed to execute topic rewriting first
- Fix the issue that modules that were turned off in the backup file would be automatically enabled after restoring the backup
- Fix the issue that the MQTT-SN client would be disconnected when retransmitting QoS 2 messages
- Fix Dashboard HTTPS listener's `verify` option not taking effect
- Fix multiple Dashboard display issues

## 4.2.12 Release

*Release Date: 2022-07-11*

### Bug fixes

- Fix the issue that the hot configuration function cannot continue to be used after hot upgrade

## 4.2.11 Release

*Release Date: 2022-04-26*

### Important changes

- A cluster-wide total connections calculation bug was fixed in in Enterprise edition 4.2.11. Previously only the individual node's local number of connections were checked against the max number of connections allowed by the license. After this fix, the total number of connections is aggregated cluster-wide every 5 seconds. An extra 10% overrun is allowed to compensate the delays in aggregation. **Users planning to upgrade should be aware of the possibility that this change may cause clients to reach the license limit and not be able to connect.**

> Note: The WebSocket listener will be restarted when e4.2.0 and e4.2.1 are hot-upgraded to a higher version, so WebSocket connections will be disconnected for a short time, please pay attention when upgrading.

### Enhancement

- MQTT-SN gateway supports initiative to synchronize registered topics after session resumed.
- Improve the relevant metrics during the execution of the rule engine SQL matching
- Improve the error message when rule engine fails to parse payload

### Bug fixes

#### Rule Engine

- Fix the issue that rule engine data persistence to Oracle failed but the success count still increased
- Fix the issue that the alternate action could not be triggered when the action of the rule engine persisting data to Oracle (only synchronous operation) failed to execute
- Fix the issue that enabling system messages would cause rule engine's Kakfa action to crash
- Fix the issue of query resource request timeout when rule engine resource is unavailable
- If a rule with the same ID already exists when creating a rule, , rule engine will now report an error instead of replacing the existing rule

#### Protocol

- Fix the issue that the configuration item `server_keepalive` would be incorrectly applied to MQTT v3.1.1 clients
- Fix the issue that the JT/T 808 location report frame was parsed incorrectly
- Fix the issue that messages that failed to be delivered due to unregistered topics were not retransmitted when topics were successfully registered with the MQTT-SN client

#### REST API & CLI

- Fix the issue that incorrect query results were returned when querying subscriptions using multiple condition
- Fix the issue that the subscription query interface did not return paginated data
- Add the format check for Dashboard User and AppID to avoid User and AppID containing some special characters cannot be deleted
- Fix the issue that the metrics interface does not return authentication metrics such as client.acl.deny by default
- Fix the issue that the LwM2M client list query API returned incorrect data in a cluster environment, which resulted in the inability to access the LwM2M gateway module management page

#### Dashboard

- Fix the issue that the session creation time option was not available when filtering clients
- Fix multiple UI display issues

#### Other

- Fix various issues of hot config, such as the configuration cannot be cleared, the updated configuration is invalid after restarting, etc
- Fix the issue that the MQTT Bridge plugin cannot be started when only the subscription topic is configured but QoS is not configured
- Fix an issue with plugin default startup list, now duplicate plugin startup items in `loaded_plugins` file will be ignored
- Fix the issue that auto subscriptions might subscribe to an empty topic
- Fix the issue that Message ID displayed garbled characters in some logs

## 4.2.10 Version

*Release date: 2022-01-13*

### Enhancement

- The action's metrics in rule engine will no longer be cleared when updating
- Supports configuring whether to forward retained messages with empty payload to suit users who are still using MQTT v3.1. The relevant configurable item is `retainer.stop_publish_clear_msg`
- Optimize the use and interaction of the built-in access control file module
- Change the default value of the `max_topic_levels` configurable item to 128. Previously, it had no limit (configured to 0), which may be a potential DoS threat
- Improve the error log content when the Proxy Protocol message is received but the `proxy_protocol` configuration is not turned on

### Important fixes

- Fix the issue that the rule engine may have a higher failure rate when saving data to MySQL
- Fix the issue of garbled data when writing data to RocketMQ asynchronously
- Fix the issue of inaccurate metrics of RocketMQ
- Fix the issue that the Max Returned Count option in the MongoDB-based offline message feature of the rule engine cannot be used
- Fixed an issue that health checks on resources could block the creation process

### Minor fixes

- Fixed the issue that the Retain Handling subscription option in the proxy subscription module could not be configured to 2
- Fix the issue that the client list obtained by filtering by session creation time is inaccurate
- Fix Erlang VM memory calculation error in Dashboard node details page
- Removed run-time configurable items that have expired, support more run-time configurable items

## 4.2.9 Version

*Release date: 2021-11-17*

### Enhancement

- Improve client kick (forced step-down)

### Important fixes

- Fix the issue that calls between clusters may cause the client process to lose response
- Fix the issue that modules reporting errors after multiple startups and shutdowns
- Fix the issue that lock release may cause the client process to crash in some cases

### Minor fixes

- MongoDB resources allow host names
- Fix some issues of MongoDB authentication module
- Fixed the issue that the Client ID containing "\\" characters could not be searched in a fuzzy manner
- Fix the issue that variable byte integers may be larger than 4 bytes
- Fix the issue that the same module may be added repeatedly
- Fixed the issue that the new configuration actually took effect after modifying the Action-related configuration in Dashboard, but the Dashboard did not refresh the display

## 4.2.8 version

*Release date: 2021-09-29*

- Fix an issue that the rule engine failed to synchronize batch write to SQL Server
- Fix an issue that the rule engine cannot create Oracle resources
- Fix an issue that multi-language protocol analysis cannot be stopped under abnormal conditions
- Fix an issue of failure to create LDAP Auth authentication module
- Fix an issue where custom fields could not be parsed in the JT/T808 gateway parsing location report
- Fix an issue that rule engine offline messages cannot be deleted after being received in some cases
- Fix an issue that resources cannot be released after the rule engine is closed
- Enhanced saving of offline messages to Redis to support clearing residual data
- Fix an issue that the error code returned by the backend is not clear when the wrong data format is entered when searching on the client
- Fix an issue that the client's protocol name is incorrectly displayed after the MQTT-SN client is connected
- Fix an issue that the client process may be stuck, causing some clients to fail to connect
- Fix an issue that the client cannot access after proxy-protocol is turned on.
- Fix an issue where the client page displayed incorrect Socket type after proxy-protocol was turned on
- Fix an issue of "Connection process is not alive" when calling exproto's ConnectionAdapter method across nodes in the cluster
- Fix a bug that caused a zombie on the Kafka client due to network fluctuations
- Webhook supports switching http-pipelining , which is disabled by default
- Added support for ipaddrs in acl.conf
- Optimize an issue of printing a large number of useless logs when the exproto client is disconnected

## 4.2.7 version

*Release date: 2021-06-17*

- Fix an issue where rule engine data is saved to an openTSDB exception that cannot be written  
- Fix A hot configuration issue cannot be performed on dashboard in a special case 
- Fix the problem that the client of the MQTT-SN protocol cleansession-false lost topicid when restoring the session  
- Fix MQTT-SN the client is stuck in an abnormal situation
- Fix an issue where rule engine data is forwarded to a WebServer SSL configuration that does not take effect  
- Fix an issue where module Kafka consumer group SSL configurations do not work  
- Fix rule engine The problem with editing a resource that prevents the list of resources from appearing  
- Enhanced exception handling of failed import lice

## 4.2.6 version

*Release date: 2021-04-28*

- Fix the problem that the module cannot be started after stopping under special circumstances
- Fix the problem of the time format of the alarm list
- Fix the problem that the MQTT-SN client goes offline abnormally and the Will message is not sent
- Fix the problem that PUBLISH and REGACK are out of order when the MQTT-SN client reconnects and cleansession=false
- Fix the problem of partial display errors in Dashboard
- Update Log default output in File

## 4.2.5 version

*Release date: 2021-03-10*

- Fix an issue with the Pulsar consumer group resolving bulk message errors
- Fix an issue that cannot be resolved in the event of an MQTT protocol exception
- Fix an issue where an error was displayed in the exception of the Dashboard subscription list
- Fix a bulk message performance issue that the rule engine handles for a single process

## 4.2.4 version

*Release date: 2021-02-18*

- New rule engine update resource logic
- Added new rule engine, data bridge to kafka supports configuration of cache size
- Fix the situation where the AUTH_HTTP long connection is disconnected when the Keepalive timeout period or the maximum number of requests is reached, causing the request to be lost
- Fix the issue of WebHook SSL certificate configuration
- Fix the problem of AuthRedis reconnection failure
- Fix the issue of checking MQTT Topic format when creating Kafka consumer group
- Optimize the theme statistics page moved to the module management page

## 4.2.3 version

*Release date: 2020-12-25*

- New GT/T32960 protocol access
- New Rule Engine SQL statements support binary data operation functions
- Adjust the rule engine/module interface parameters uniformly
- Optimize the LWM2M access process
- Optimize webHook plug-in performance
- Fix The rule engine redis sentinel mode failed to create a resource

## 4.2.2 version

*Release date: 2020-12-10*

- Optimize AuthHttp performance issues
- Add new rule engine data save to Oracle
- Added rule engine data save to DolphinDB
- Added rule engine data saving to MS SQL server
- Enhanced rule engine data saving support synchronous and asynchronous
- Fix the problem of inaccurate counting in the asynchronous mode of the rule engine
- Added SSL support to configure the depth of the CA certificate
- Fix the abnormal problem in the hot upgrade

## 4.2.1 version

*Release date: 2020-11-16*

- Added Dashboard module page to support management mqtt enhanced authentication
- Added Dashboard module page to support management of lwm2m client
- Added redis resources to support configuring SSL parameters
- Added auth_jwt to support JWKs
- Added alert message when subscriber TCP is busy
- New rule engine, data bridge to kafka, support ACK policy configuration
- Optimize Dashboard monitoring page
- Optimize emqx_exporto performance
- Optimize emqx_exhook performance
- Fix the wrong action type when editing actions in dashboard
- Fix rule engine-resource typo
- Fix the failure of import and export recovery in cluster
- Fix the problem that the rule engine MySQL resource cannot use the domain name
- Fix the problem that the message is too large when the data is bridged to Kafka

## 4.2.0 version

*Release date: 2020-10-13*

- Rule engine Mysql/MongoDB/Cassandra/PGsql resource supports IPV6 and SSL connection
- The rule engine "resources" supports uploading certificates
- Rule engine "action" group
- Fix InfluxDB not supporting underscore characters
- Support dynamic creation and configuration of functional modules
- Support more parameter hot configuration
- Support hot upgrade between minor version numbers
- Remove emqx_auth_username and emqx_auth_clientid plugins
- Refactor emqx_auth_mnesia, compatible with the data import of the old version emqx_auth_username and emqx_auth_clientid
- The emqx main configuration file is split and supports include configuration files

## 4.1.5

*Release Date: 2020-08-30*

EMQ X 4.1.5 is released now, it fixes a bug in MQTT message parser.

## 4.1.4

*Release Date: 2020-08-29*

EMQ X 4.1.4 is released now, it mainly includes the following changes:

### emqx

**Bug fixes:**

- Fix the issue of abnormal memory growth caused by the topic metrics feature

  Github PR: [emqx#3680](https://github.com/emqx/emqx/pull/3680)

### emqx-bridge-mqtt

**Enhancements:**

- The clientid configuration item supports `${node}` placeholders to optimize the user experience under the cluster

  Github PR: [emqx-bridge-mqtt#99](https://github.com/emqx/emqx-bridge-mqtt/pull/99)

### emqx-management

**Bug fixes:**

- Fix the issue that the data migration function is not available under Windows

  Github PR: [emqx-management#262](https://github.com/emqx/emqx-management/pull/262)

### emqx-lua-hook

**Bug fixes:**

- Fix the issue that the Username field cannot be obtained

  Github PR: [emqx-lua-hook#115](https://github.com/emqx/emqx-lua-hook/pull/115)

## 4.1.3

*Release Date: 2020-07-24*

EMQ X 4.1.3 is released now, it mainly includes the following changes:

### emqx-management

**Bug fixes:**

- Add type checking for the payload field in PUBLISH API

  Github PR: [emqx/emqx-management#250](https://github.com/emqx/emqx-management/pull/250)

### emqx-retainer

**Bug fixes:**

- Fix the issue that the retained message will not be sent when the subscription topic contains both '+' and '#'

  Github PR: [emqx/emqx-retainer#146](https://github.com/emqx/emqx-retainer/pull/146)

## 4.1.2

*Release Date: 2020-08-08*

- Fixes some known issues

## 4.1.1

*Release Date: 2020-08-07*

1. rule_engine adds Pulsar consumer group resources
2. rule_engine add Kafka consumer group resources
3. rule_engine Add data and save to TDengine database
4. rule_engine Add offline message save to MySQL action
5. rule_engine Add offline message save to PostgreSQL action
6. rule_engine Add offline message and save to Cassandra action
7. rule_engine Add offline message and save to MongoDB action
8. rule_engine Add to get subscription relationship from MySQL
9. rule_engine Add to get subscription relationship from PostgreSQL
10. rule_engine Add to get subscription relationship from Cassandra
11. rule_engine Add to get subscription relationship from MongoDB
12. rule_engine Save data to MongoDB Action support message template
13. Fix the bug that the HTTP Publish API cannot support the json format of the payload

## 4.1.0

*Release Date: 2020-07-18*

1. Built-in preview version license, you can start emqx directly without registering on the official website to get the license
2. Modify the license expiration policy, the emqx service will not stop, but the new connection cannot log in
3. Rule engine add MQTT subscription resources
4. Rule engine MQTT message bridge support pool
5. Rule engine MQTT message bridge fixes the cluster cannot use the bug
6. Rule engine Add data and save to ClickHouse database
7. InfluxDB supports http/https connection
8. Enterprise edition multi-language development supports northbound message processing
9. Rule engine Add offline message and save to redis action
10. Rule engine add to get subscription relationship from redis

## 4.0.5

*Release date: 2020-03-17*

EMQX 4.0.5 is now released, which mainly fixed some bugs.

emqx
----

**Bugs fixed:**

- Fix GC strategy

  Github PR: [emqx/emqx#3317](https://github.com/emqx/emqx/pull/3317)
  
- Fixed the issue where the value of the `Maximum-QoS` attribute was set incorrectly

  Github issue: [emqx/emqx#3304](https://github.com/emqx/emqx/issues/3304), [emqx/emqx#3315](https://github.com/emqx/emqx/issues/3315)
  Github PR: [emqx/emqx#3321](https://github.com/emqx/emqx/pull/3321)
  
- Fixed the issue where the CPU usage rate increased abnormally every 15 seconds when EMQX was running in a Docker environment

 Github issue: [emqx/emqx#3274](https://github.com/emqx/emqx/pull/3274)
  Github PR: [emqx/emqx-rel#462](https://github.com/emqx/emqx-rel/pull/462)

- Fix the issue that the node.* configuration item in the configuration file does not take effect

  Github issue: [emqx/emqx#3302](https://github.com/emqx/emqx/pull/3302)
  Github PR: [emqx/emqx-rel#463](https://github.com/emqx/emqx-rel/pull/463)

emqx-rule-engine (plugin)
------------------------

**Bugs fixed:**

- Fix the issue that the rule engine does not support Payload as UTF-8 string

  Github issue: [emqx/emqx#3287](https://github.com/emqx/emqx/issues/3287)
  Github PR: [emqx/emqx#3299](https://github.com/emqx/emqx/pull/3299)

emqx-sn (plugin)
----------------

**Bugs fixed:**

- Fix the issue of missing MQTT-SN subscription

  Github issue: [emqx/emqx#3275](https://github.com/emqx/emqx/issues/3275)
  Github PR: [emqx/emqx-sn#156](https://github.com/emqx/emqx-sn/pull/156)


## 4.0.4 Version

*Release Date: 2019-03-06*

EMQX 4.0.4 is now released, which mainly fixed some bugs.

### emqx

**Bugs fixed:**

  - Fix the issue that the `acl_deny_action` configuration item does not take effect
    
    Github issue:
    [emqx/emqx\#3266](https://github.com/emqx/emqx/issues/3266)
    
    Github PR: [emqx/emqx\#3286](https://github.com/emqx/emqx/pull/3286)

  - Fix wrong type of `mountpoint` configuration item
    
    Github issue:
    [emqx/emqx\#3271](https://github.com/emqx/emqx/issues/3271)
    
    Github PR: [emqx/emqx\#3272](https://github.com/emqx/emqx/pull/3272)

  - Fix the issue that the `peer_cert_as_username` configuration item does not take effect
    
    Github issue:
    [emqx/emqx\#3281](https://github.com/emqx/emqx/issues/3281)
    
    Github PR: [emqx/emqx\#3291](https://github.com/emqx/emqx/pull/3291)

  - Fix the problem that the error log is still printed even if the connection is closed normally
    
    Github PR: [emqx/emqx\#3290](https://github.com/emqx/emqx/pull/3290)

### emqx-dashboard (plugin)

**Bugs fixed:**

  - Fix the problem that the Dashboard node displays a blank in the drop-down list
    
    Github issue:
    [emqx/emqx\#3278](https://github.com/emqx/emqx/issues/3278)
    
    Github PR:
    [emqx/emqx-dashboard\#206](https://github.com/emqx/emqx-dashboard/pull/206)

### emqx-retainer (plugin)

**Bugs fixed:**

  - 保留消息达到最大存储数量后的行为由无法存储任何保留消息更正为可以替换已存在主题的保留消息
    
    Github PR:
    [emqx/emqx-retainer\#136](https://github.com/emqx/emqx-retainer/pull/136)

## 4.0.3

*Release Date: 2019-02-21*

EMQX 4.0.3 现已发布。此版本主要进行了错误修复。

### emqx

**功能增强:**

  - 添加允许客户端绕过认证插件登录的选项
    
    Github PR: [emqx/emqx\#3253](https://github.com/emqx/emqx/pull/3253)

**Bugs fixed:**

  - 修复某些竞争条件下会打印不必要的错误日志的问题
    
    Github PR: [emqx/emqx\#3246](https://github.com/emqx/emqx/pull/3253)

### emqx-management (plugin)

**Bugs fixed:**

  - 移除不再使用的字段和函数以及修复字段值异常的问题
    
    Github PR:
    [emqx/emqx-management\#176](https://github.com/emqx/emqx-management/pull/176)

  - 修复集群环境下无法获取客户端列表的问题
    
    Github PR:
    [emqx/emqx-management\#173](https://github.com/emqx/emqx-management/pull/173)

  - 修复 HTTPS 监听选项
    
    Github PR:
    [emqx/emqx-management\#172](https://github.com/emqx/emqx-management/pull/172)

  - 修复应用列表的返回格式
    
    Github PR:
    [emqx/emqx-management\#169](https://github.com/emqx/emqx-management/pull/169)

## 4.0.2

*Release Date: 2019-02-07*

EMQX 4.0.2 现已发布。此版本主要进行了错误修复和性能优化。

### emqx

**功能增强:**

  - 提升 Json 编解码性能
    
    Github PR:
    [emqx/emqx\#3213](https://github.com/emqx/emqx/pull/3213),
    [emqx/emqx\#3230](https://github.com/emqx/emqx/pull/3230),
    [emqx/emqx\#3235](https://github.com/emqx/emqx/pull/3235)

  - 压缩生成的项目大小
    
    Github PR: [emqx/emqx\#3214](https://github.com/emqx/emqx/pull/3214)

**Bugs fixed:**

  - 修复某些情况下没有发送 DISCONNECT 报文的问题
    
    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

  - 修复收到相同 PacketID 的 PUBLISH 报文时会断开连接的问题
    
    Github PR: [emqx/emqx\#3233](https://github.com/emqx/emqx/pull/3233)

### emqx-stomp (plugin)

**Bugs fixed:**

  - 修复最大连接数限制不生效的问题
    
    Github PR:
    [emqx/emqx-stomp\#93](https://github.com/emqx/emqx-stomp/pull/93)

### emqx-auth-redis (plugin)

**Bugs fixed:**

  - 修复内部模块启动失败的问题
    
    Github PR:
    [emqx/emqx-auth-redis\#151](https://github.com/emqx/emqx-auth-redis/pull/151)

### cowboy (dependency)

**Bugs fixed:**

  - 修复 Websocket 连接某些情况下不会发送遗嘱消息的问题
    
    Github Commit:
    [emqx/cowboy\#3b6bda](https://github.com/emqx/cowboy/commit/3b6bdaf4f2e3c5b793a0c3cada2c3b74c3d5e885)

## 4.0.1

*Release Date: 2019-01-17*

EMQX 4.0.1 现已发布。此版本主要进行了错误修复和性能优化。

### emqx

**功能增强:**

  - force\_shutdown\_policy 默认关闭
    
    Github PR: [emqx/emqx\#3184](https://github.com/emqx/emqx/pull/3184)

  - 支持定时全局 GC 并提供配置项
    
    Github PR: [emqx/emqx\#3190](https://github.com/emqx/emqx/pull/3190)

  - 优化 `force_gc_policy` 的默认配置
    
    Github PR:
    [emqx/emqx\#3192](https://github.com/emqx/emqx/pull/3192),
    [emqx/emqx\#3201](https://github.com/emqx/emqx/pull/3201)

  - 优化 Erlang VM 参数配置
    
    Github PR:
    [emqx/emqx\#3195](https://github.com/emqx/emqx/pull/3195),
    [emqx/emqx\#3197](https://github.com/emqx/emqx/pull/3197)

**Bugs fixed:**

  - 修复使用错误的单位导致黑名单功能异常的问题
    
    Github PR: [emqx/emqx\#3188](https://github.com/emqx/emqx/pull/3188)

  - 修复对 `Retain As Publish` 标志位的处理并且在桥接模式下保持 `Retain` 标识位的值
    
    Github PR: [emqx/emqx\#3189](https://github.com/emqx/emqx/pull/3189)

  - 修复无法使用多个 Websocket 监听端口的问题
    
    Github PR: [emqx/emqx\#3196](https://github.com/emqx/emqx/pull/3196)

  - 修复会话 takeover 时 EMQX 可能不发送 DISCONNECT 报文的问题
    
    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

### emqx-rule-engine

**功能增强:**

  - 提供更多操作数组的 SQL 函数
    
    Github PR:
    [emqx/emqx-rule-engine\#136](https://github.com/emqx/emqx-rule-engine/pull/136)

  - 减少未配置任何规则时对性能的影响
    
    Github PR:
    [emqx/emqx-rule-engine\#138](https://github.com/emqx/emqx-rule-engine/pull/138)

### emqx-web-hook

**Bugs fixed:**

  - 修复参数不匹配导致的崩溃问题
    
    Github PR:
    [emqx/emqx-web-hook\#167](https://github.com/emqx/emqx-web-hook/pull/167)

## 4.0.0

*Release Date: 2019-01-10*

EMQX 4.0.0 正式版现已发布。在这个版本中，我们通过重构 channel 和 session
显著地改进了吞吐性能，通过添加更多的钩子和统计指标增强了可扩展性，重新设计了规则引擎的
SQL，并优化 Edge 版本的性能表现。

#### 常规

**功能增强:**

  - 架构优化，大幅提高消息吞吐性能，降低了 CPU 与内存占用
  - 改进 MQTT 5.0 报文处理流程
  - 规则引擎支持全新的 SQL 语句
  - 调整 metrics 命名并增加更多的 metrics
  - 调整钩子参数并增加更多的钩子
  - emqtt 提供发布与订阅的命令行接口

**Bugs fixed:**

  - 修复了 SSL 握手失败导致崩溃的问题
  - 修复 `max_subscriptions` 配置不生效的问题
  - 修复跨集群转发消息失序的问题
  - 修复命令行接口无法获取单个主题的多条路由信息的问题

#### REST API

**功能增强:**

  - 支持 IPv6
  - REST API 默认监听端口由 8080 改为 8081，减少被其他应用占用的情况
  - 移除所有 sessions 相关的接口
  - connections 调整为 clients，并提供原先 sessions 的功能
  - 支持订阅查询接口返回共享订阅的真实主题
  - 支持配置默认的 AppID 与 AppSecret
  - 发布消息的 REST API 支持使用 base64 编码的 payload

**Bugs fixed:**

  - 修复转码后的 URI 没有被正确处理的问题

#### 认证

**功能增强:**

  - HTTP 认证插件支持用户配置自定义的 HTTP 请求头部
  - clientid 与 username 认证插件重新支持用户通过配置文件配置默认的 clientid 与 username

## 4.0-rc.4

*Release Date: 2019-12-31*

EMQX 4.0-rc.4 版本现已发布，其中包括以下更改:

### emqx

**功能增强:**

  - 增加了更多的钩子
    
    Github PR: [emqx/emqx\#3138](https://github.com/emqx/emqx/pull/3138)

  - 增加了更多的 metrics
    
    Github PR:
    [emqx/emqx\#3139](https://github.com/emqx/emqx/pull/3139),
    [emqx/emqx\#3141](https://github.com/emqx/emqx/pull/3141)

**Bugs fixed:**

  - 修复定时器超时消息可能匹配失败的问题
    
    Github PR: [emqx/emqx\#3145](https://github.com/emqx/emqx/pull/3145)

### emqx-bridge-mqtt

**Bugs fixed:**

  - 修复 keepalive 配置项使用了错误的单位的问题
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#43](https://github.com/emqx/emqx-bridge-mqtt/pull/43)

### emqx-management

**功能增强:**

  - 支持配置默认的 AppID 与 AppSecret
    
    Github PR:
    [emqx/emqx-management\#153](https://github.com/emqx/emqx-management/pull/153)

  - 发布消息的 HTTP API 现以支持 base64 编码后的 payload
    
    Github PR:
    [emqx/emqx-management\#154](https://github.com/emqx/emqx-management/pull/154)

### emqx-auth-http

**功能增强:**

  - 支持用户自行配置 HTTP 请求头
    
    Github PR:
    [emqx/emqx-auth-http\#170](https://github.com/emqx/emqx-auth-http/pull/170)

## 4.0-rc.3

*Release Date: 2019-12-21*

EMQX 4.0-rc.3 版本现已发布，其中包括以下更改:

### emqx

**功能增强:**

  - 添加更多的 Metrics; 并删除 `channel.gc`, `messages.qos2.expired`,
    `messages.qos2.dropped`, `auth.mqtt.anonymous` 等
    
    Github PR: [emqx/emqx\#3128](https://github.com/emqx/emqx/pull/3128)

  - 日志格式支持配置行号
    
    Github PR: [emqx/emqx\#3117](https://github.com/emqx/emqx/pull/3117)

  - 为 emqx\_connection 增加更多的测试用例
    
    Github PR: [emqx/emqx\#3116](https://github.com/emqx/emqx/pull/3116)

  - 修复 MQTT/WS 消息乱序的 BUG
    
    Github PR: [emqx/emqx\#3115](https://github.com/emqx/emqx/pull/3115)

### emqx-dashboard (plugin)

**进行了以下更改:**

  - 优化 SQL 编辑器使用体验:
    
    Github PR:
    [emqx/emqx-dashboard\#176](https://github.com/emqx/emqx-dashboard/pull/176),
    [emqx/emqx-dashboard\#177](https://github.com/emqx/emqx-dashboard/pull/177)

  - 优化 Overview 页面显示
    
    Github PR:
    [emqx/emqx-dashboard\#179](https://github.com/emqx/emqx-dashboard/pull/179)

### emqx-management (plugin)

**进行了以下更改:**

  - 支持返回共享订阅的真实主题
    
    Github PR:
    [emqx/emqx-management\#151](https://github.com/emqx/emqx-management/pull/151)

**Bugs fixed:**

  - 修复无法获取单个主题的多条路由信息的问题
    
    Github PR:
    [emqx/emqx-management\#150](https://github.com/emqx/emqx-management/pull/150)

### emqx-coap (plugin)

**Bugs fixed:**

  - 修复停止插件后，无法正常启动的问题
    
    Github PR:
    [emqx/emqx-coap\#151](https://github.com/emqx/emqx-coap/pull/151)

### emqx-delayed-publish (plugin)

**进行了以下更改:**

  - 新增 `messages.delayed` Metrics 计数
    
    Github PR:
    [emqx/emqx-delayed-publish\#55](https://github.com/emqx/emqx-delayed-publish/pull/55)

### emqx-statsd (plugin)

**进行了以下更改:**

  - 对新的 Metrics 进行适配
    
    Github PR:
    [emqx/emqx-statsd\#43](https://github.com/emqx/emqx-statsd/pull/43)

### emqx-bridge-mqtt (plugin)

**进行了以下修复:**

  - 修正 Keepalive 单位为秒
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#43](https://github.com/emqx/emqx-bridge-mqtt/pull/43)

### emqx-auth-http (plugin)

**进行了以下更改:**

  - 支持 '%p' 占位符，以获取客户端所连接的端口
    
    Github PR:
    [emqx/emqx-auth-http\#167](https://github.com/emqx/emqx-auth-http/pull/167)

### All of Authentication Plugins

**进行了以下更改:**

  - 重命名认证成功或失败的计数 Metrics 的前缀为 `client.auth.`; 重命名 ACL 检查成功或失败的 Metrics
    前缀为 `client.acl.`
    
    Github PR:
    [emqx/emqx-auth-username\#132](https://github.com/emqx/emqx-auth-username/pull/132),
    [emqx/emqx-auth-clientid\#127](https://github.com/emqx/emqx-auth-clientid/pull/127),
    [emqx/emqx-auth-http\#168](https://github.com/emqx/emqx-auth-http/pull/168),
    [emqx/emqx-auth-jwt\#107](https://github.com/emqx/emqx-auth-jwt/pull/107),
    [emqx/emqx-auth-ldap\#96](https://github.com/emqx/emqx-auth-ldap/pull/96),
    [emqx/emqx-auth-mongo\#197](https://github.com/emqx/emqx-auth-mongo/pull/197),
    [emqx/emqx-auth-mysql\#193](https://github.com/emqx/emqx-auth-mysql/pull/193),
    [emqx/emqx-auth-pgsql\#174](https://github.com/emqx/emqx-auth-pgsql/pull/174),
    [emqx/emqx-auth-redis\#144](https://github.com/emqx/emqx-auth-redis/pull/144)

## 4.0-rc.2

*Release Date: 2019-12-16*

EMQX 4.0-rc.2 版本现已发布，其中包括以下更改:

### emqx

**功能增强:**

  - 为更多模块增加测试用例，提升原有测试用例的测试覆盖率
    
    Github PR:
    [emqx/emqx\#3091](https://github.com/emqx/emqx/pull/3091),
    [emqx/emqx\#3095](https://github.com/emqx/emqx/pull/3095),
    [emqx/emqx\#3096](https://github.com/emqx/emqx/pull/3096),
    [emqx/emqx\#3100](https://github.com/emqx/emqx/pull/3100),
    [emqx/emqx\#3106](https://github.com/emqx/emqx/pull/3106),
    [emqx/emqx\#3107](https://github.com/emqx/emqx/pull/3107)

  - Get the timestamp uniformly by `erlang:system_time`
    
    Github PR:
    [emqx/emqx\#3088](https://github.com/emqx/emqx/pull/3088),
    [emqx/emqx\#3089](https://github.com/emqx/emqx/pull/3089)

  - 移除 `sessions.persistent.count` 与 `sessions.persistent.max` 计数
    
    Github PR: [emqx/emqx\#3111](https://github.com/emqx/emqx/pull/3111)

  - WebSocket 支持会话机制
    
    Github PR:
    [emqx/emqx\#3106](https://github.com/emqx/emqx/pull/3106),
    [emqx/cowboy\#1](https://github.com/emqx/cowboy/pull/1),
    [emqx/cowboy\#3](https://github.com/emqx/cowboy/pull/3)

### emqx-retainer (plugin)

**Bugs fixed:**

  - 存在大量保留消息时 EMQX 不能及时向客户端回复 SUBACK
    
    Github PR:
    [emqx/emqx-retainer\#126](https://github.com/emqx/emqx-retainer/pull/126)

### emqx-dashboard (plugin)

**进行了以下更改:**

  - 客户端列表增加 IP 字段，不需要进入详情才能查看
    
    Github PR:
    [emqx/emqx-dashboard\#172](https://github.com/emqx/emqx-dashboard/pull/172)

## 4.0-rc.1

*Release Date: 2019-12-07*

EMQX 4.0-rc.1 版本发布。此版本主要优化了内部模块和 MQTT 报文处理流程。

### emqx

功能增强:

  - 优化 MQTT 报文优化逻辑
    
    Github PR:
    [emqx/emqx\#3079](https://github.com/emqx/emqx/pull/3079),
    [emqx/emqx\#3082](https://github.com/emqx/emqx/pull/3082),
    [emqx/emqx\#3083](https://github.com/emqx/emqx/pull/3083)

### emqx-auth-username (plugin)

功能增强:

  - 重新支持通过配置文件配置默认的 `username`
    
    Github PR:
    [emqx/emqx-auth-username\#126](https://github.com/emqx/emqx-auth-username/pull/126)

### emqx-auth-clientid (plugin)

功能增强:

  - 重新支持通过配置文件配置默认的 `clientid`
    
    Github PR:
    [emqx/emqx-auth-clientid\#122](https://github.com/emqx/emqx-auth-clientid/pull/122)

### emqx-management (plugin)

功能增强:

  - HTTP API 服务器默认监听端口由 8080 改为 8081
    
    Github PR:
    [emqx/emqx-management\#144](https://github.com/emqx/emqx-management/pull/144)

## 3.2.7

*Release Date: 2019-12-03*

EMQX 3.2.7 版本发布。此版本主要重新支持了通过配置文件配置默认的 `username` 和 `clientid`。

### emqx-auth-username (plugin)

功能增强:

  - 重新支持了通过配置文件配置默认的 `username`
    
    Github PR:
    [emqx/emqx-auth-username\#127](https://github.com/emqx/emqx-auth-username/pull/127)

### emqx-auth-clientid (plugin)

功能增强:

  - 重新支持了通过配置文件配置默认的 `clientid`
    
    Github PR:
    [emqx/emqx-auth-clientid\#123](https://github.com/emqx/emqx-auth-clientid/pull/123)

## 3.2.6

*Release Date: 2019-11-23*

EMQX 3.2.6 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

错误修复:

  - 修复通过 `gen_rpc` 向远程节点转发消息时可能失序的问题
    
    Github PR: [emqx/emqx\#3049](https://github.com/emqx/emqx/pull/3049)

  - 修复认证插件崩溃会导致 `emqx` 崩溃的问题
    
    Github PR: [emqx/emqx\#3048](https://github.com/emqx/emqx/pull/3048)

## 4.0-beta.4

*Release Date: 2019-11-18*

EMQX 4.0-beta.4 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

功能增强:

  - 被检测到 flapping 的客户端会被 banned
    
    Github PR: [emqx/emqx\#3033](https://github.com/emqx/emqx/pull/3033)

  - 优化 emqx\_vm 模块并更新测试用例
    
    Github PR: [emqx/emqx\#3034](https://github.com/emqx/emqx/pull/3034)

### emqx-management (plugin)

功能增强:

  - 更新 banned API
    
    Github PR:
    [emqx/emqx-management\#141](https://github.com/emqx/emqx-management/pull/141)

错误修复:

  - 修复一些错误的返回值
    
    Github PR:
    [emqx/emqx-management\#142](https://github.com/emqx/emqx-management/pull/142)

### minirest (plugin)

错误修复:

  - 添加错误处理并增加日志
    
    Github PR:
    [emqx/minirest\#20](https://github.com/emqx/minirest/pull/20)

### esockd (dependency)

功能增强:

  - 调整部分接口并增加测试用例
    
    Github PR:
    [emqx/esockd\#124](https://github.com/emqx/esockd/pull/124)

### ekka (dependency)

功能增强:

  - 调整部分接口并增加测试用例
    
    Github PR: [emqx/ekka\#67](https://github.com/emqx/ekka/pull/67)

## 3.2.5

*Release Date: 2019-11-15*

EMQX 3.2.5 版本发布。此版本主要进行了错误修复。

### emqx-rule-engine (plugin)

错误修复:

  - 支持 SQL 关键字: FOREACH/DO/INCASE
    
    Github Commit:
    [emqx/emqx-rule-engine\#a962e3](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

  - 支持 SQL 关键字: CASE/WHEN
    
    Github Commit:
    [emqx/emqx-rule-engine\#40e68e](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

  - 支持在 SQL 的 WHERE 子句中比较原子与二进制
    
    Github Commit:
    [emqx/emqx-rule-engine\#b240cc](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

  - 修复 select 和 foreach 中的列验证失败
    
    Github Commit:
    [emqx/emqx-rule-engine\#6a1267](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

  - 修复重建规则时出现竞争的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#af8967](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

  - 修复重发消息时没有确证设置标志的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#60e45c](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)

### minirest (plugin)

错误修复:

  - 修复日志没有记录错误数据的问题
    
    Github PR:
    [emqx/minirest\#20](https://github.com/emqx/minirest/pull/20)

### emqx-web-hook (plugin)

错误修复:

  - 修复错误的匹配
    
    Github Commit:
    [emqx/emqx-web-hook\#3dd041](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)

## 4.0-beta.3

*Release Date: 2019-11-01*

EMQX 4.0-beta.3 版本发布。此版本主要针对错误修复以及测试覆盖率提升。

错误修复:

  - 修复跨集群转发时消息失序的问题
    
    Github PR: [emqx/emqx\#3000](https://github.com/emqx/emqx/pull/3000)

### emqx-management (plugin)

功能增强:

  - REST API 支持 IPv6
    
    Github PR:
    [emqx/emqx-management\#135](https://github.com/emqx/emqx-management/pull/135)

错误修复:

  - 修复转码后的 URI 没有被正确处理的问题
    
    Github PR:
    [emqx/emqx-management\#137](https://github.com/emqx/emqx-management/pull/137)

### emqx-dashboard (plugin)

功能增强:

  - 支持使用 IPv6 访问 Dashbaord
    
    Github PR:
    [emqx/emqx-dashboard\#162](https://github.com/emqx/emqx-dashboard/pull/162)

### emqx-delayed-publish (plugin)

错误修复:

  - 修复插件在集群环境下只能在一个节点中开启的问题
    
    Github PR:
    [emqx/emqx-delay-publish\#50](https://github.com/emqx/emqx-delay-publish/pull/50)

  - 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献
    
    Github PR:
    [emqx/emqx-delay-publish\#49](https://github.com/emqx/emqx-delay-publish/pull/49)
    
    Github Issue:
    [emqx/emqx-delay-publish\#15](https://github.com/emqx/emqx-delay-publish/issues/15)

## 3.2.4

*Release Date: 2019-10-28*

EMQX 3.2.4 版本发布。此版本主要为 Dashbaord 和 REST API 添加了 IPv6 支持，并修复了一些错误。

错误修复:

  - 修复 max\_subscriptions 配置不生效的问题
    
    Github PR: [emqx/emqx\#2922](https://github.com/emqx/emqx/pull/2922)
    
    Github Issue:
    [emqx/emqx\#2908](https://github.com/emqx/emqx/issues/2908)

### emqx-auth-mysql (plugin)

错误修复:

  - 使用占位符时更安全地取值
    
    Github PR:
    [emqx/emqx-auth-mysql\#180](https://github.com/emqx/emqx-auth-mysql/pull/180)
    
    Github Issue:
    [emqx/emqx\#2937](https://github.com/emqx/emqx/issues/2937)

### emqx-dashboard (plugin)

功能增强:

  - 支持使用 IPv6 访问 Dashbaord
    
    Github PR:
    [emqx/emqx-dashboard\#161](https://github.com/emqx/emqx-dashboard/pull/161)

### emqx-management (plugin)

功能增强:

  - REST API 支持 IPv6
    
    Github PR:
    [emqx/emqx-management\#134](https://github.com/emqx/emqx-management/pull/134)

### emqx-delay-publish (plugin)

错误修复:

  - 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献
    
    Github PR:
    [emqx/emqx-delay-publish\#48](https://github.com/emqx/emqx-delay-publish/pull/48)
    
    Github Issue:
    [emqx/emqx-delay-publish\#15](https://github.com/emqx/emqx-delay-publish/issues/15)

### emqx-rule-engine (plugin)

功能增强:

  - 优化规则引擎中 JSON Payload 解析语句
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

## 4.0-beta.2

*Release Date: 2019-10-12*

<div id="release_4.0-beta.1">

EMQX 4.0-beta.2 版本发布。此版本主要针对错误修复以及继续优化内部模块设计。

</div>

错误修复:

  - 修复 SSL 握手失败导致崩溃的问题
    
    Github PR: [emqx/emqx\#2963](https://github.com/emqx/emqx/pull/2963)

  - 检查 PUBLISH 报文的主题层级
    
    Github PR: [emqx/emqx\#2964](https://github.com/emqx/emqx/pull/2964)

### emqtt (plugin)

功能增强:

  - 提供命令行接口
    
    Github PR: [emqx/emqtt\#91](https://github.com/emqx/emqtt/pull/91)

### emqx-sn (plugin)

错误修复:

  - 适配 MQTT-SN 插件到 4.0 版本
    
    Github PR:
    [emqx/emqx-sn\#145](https://github.com/emqx/emqx-sn/pull/145)

### emqx-coap (plugin)

错误修复:

  - 适配 CoAP 插件到 4.0 版本
    
    Github Commit:
    [emqx/emqx-coap\#c7c175](https://github.com/emqx/emqx-coap/commit/c7c17540c1248dcdd402b41323c23a211e8292fc),
    [emqx/emqx-coap\#9b8ede](https://github.com/emqx/emqx-coap/commit/9b8ede093cfc3b7211663520e496c579c11611f6)

## 4.0-beta.1

*Release Date: 2019-09-30*

EMQX 4.0-beta.1 版本发布。此版本主要针对内部模块进行重新设计，实现吞吐大幅度提升。

## 3.2.3

*Release Date: 2019-09-16*

EMQX 3.2.3 版本改动主要为错误修复。

错误修复:

  - 修复 emqx 容器运行时 CPU 占用率告警异常的问题
    
    GitHub Commit:
    [emqx/emqx\#9cdaa7](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

  - 修复消息过期机制不生效的问题
    
    Github Commit:
    [emqx/emqx\#31671f](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

  - 修复占位符在 mountpoint 中不生效的问题
    
    Github Commit:
    [emqx/emqx\#58ba22](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)

### emqx-dashboard (plugin)

错误修复:

  - 修复 SSL 无法使用的问题
    
    Github Commit:
    [emqx/emqx-dashboard\#272a42](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)

## 3.2.2

*Release Date: 2019-08-03*

EMQX 3.2.2 版本改动主要为错误修复。

功能增强:

  - 扩展 `gen_rpc` 配置
    
    Github PR: [emqx/emqx\#2732](https://github.com/emqx/emqx/pull/2732)

### emqx-rule-engine (plugin)

错误修复:

  - 修复测试 URL 连通性的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#88](https://github.com/emqx/emqx-rule-engine/pull/88)

### emqx-dashboard (plugin)

功能增强:

  - 增加帮助页面

### ekka (dependency)

错误修复:

  - 修复释放锁可能导致崩溃的问题
    
    Github PR: [emqx/ekka\#60](https://github.com/emqx/ekka/pull/60)

## 3.2.1

*Release Date: 2019-07-20*

EMQX 3.2.1 版本改动主要包括错误修复与性能增强。

功能增强:

  - 优化 `gen_rpc` 的调用
    
    Github PR: [emqx/emqx\#2694](https://github.com/emqx/emqx/pull/2694)

  - 支持使用 hostname 自动发现 k8s 集群
    
    Github PR: [emqx/emqx\#2699](https://github.com/emqx/emqx/pull/2699)

  - 将默认 uptime 心跳时间改为 30s
    
    Github PR: [emqx/emqx\#2696](https://github.com/emqx/emqx/pull/2696)

错误修复:

  - 修复 WebSocket 非正常下线时出现 crash 的问题
    
    Github PR: [emqx/emqx\#2697](https://github.com/emqx/emqx/pull/2697)

  - 修复 Session 异常关闭时，ws\_channel 仍然在线的问题
    
    Github PR: [emqx/emqx\#2704](https://github.com/emqx/emqx/pull/2704)

### emqx-rule-engine (plugin)

功能增强:

  - 增强 republish 动作参数
    
    Github PR:
    [emqx/emqx-rule-engine\#81](https://github.com/emqx/emqx-rule-engine/pull/81)

错误修复:

  - 修复使用 '.' 筛选 payload 字段失败的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#83](https://github.com/emqx/emqx-rule-engine/pull/83)

### emqx-dashboard (plugin)

错误修复:

  - 修复 Dashboard 资源列表在 Safari 下渲染错误的问题
    
    Github PR:
    [emqx/emqx-dashboard\#124](https://github.com/emqx/emqx-dashboard/pull/124),
    [emqx/emqx-dashboard\#125](https://github.com/emqx/emqx-dashboard/pull/125),
    [emqx/emqx-dashboard\#126](https://github.com/emqx/emqx-dashboard/pull/126)

### emqx-lwm2m (plugin)

功能增强:

  - 兼容 LwM2M 1.1 版本客户端登录
    
    Github Commit:
    [emqx/emqx-lwm2m\#1c03bf](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)

### emqx-rel (build project)

功能增强:

  - 内置 rebar3 脚本
    
    Github PR:
    [emqx/emqx-rel\#394](https://github.com/emqx/emqx-rel/pull/394)

  - EMQX Windows 服务延迟启动
    
    Github PR:
    [emqx/emqx-rel\#395](https://github.com/emqx/emqx-rel/pull/395)

## 3.2.0

*Release Date: 2019-07-12*

EMQX 3.2.0 版本主要优化和改进了规则引擎。

### 规则引擎

改进规则引擎功能和规则管理界面(Dashboard)，支持更多动作。

### 项目构建

改用 rebar3 构建项目。

### MQTT Broker 桥接

将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件，并提升了 RPC bridge 的性能。

### HTTP 插件

支持 HTTPs。

### 集群 (ekka)

改善集群稳定性。

### 其他插件和依赖

修复 Windows 服务注册问题。

## 3.2-rc.3

*Release Date: 2019-07-06*

EMQX 3.2-rc.3 版本改动主要包括功能增强与错误修复。

错误修复:

  - 修复 [emqx/emqx:
    issue\#2635](https://github.com/emqx/emqx/issues/2635)
    
    Github PR: [emqx/emqx\#2663](https://github.com/emqx/emqx/pull/2663)

### emqx-web-hook (plugin)

错误修复:

  - 修复 `actions.failure` 无计数的问题
    
    Github PR:
    [emqx/emqx-web-hook\#137](https://github.com/emqx/emqx-web-hook/pull/137)

### emqx-bridge-mqtt (plugin)

功能增强:

  - 增加桥接模式选项
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#6](https://github.com/emqx/emqx-bridge-mqtt/pull/6)

  - 优化 RPC 消息的应答机制

  - 支持规则引擎下的 MQTT/RPC Bridge 缓存消息到本地磁盘队列

  - 修复规则引擎下的 RPC Bridge 无法桥接远程 EMQX 节点的问题
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#7](https://github.com/emqx/emqx-bridge-mqtt/pull/7)

### emqx-rule-engine (plugin)

功能增强:

  - Rule 与 Resource 的 API 支持集群
    
    Github PR:
    [emqx/emqx-rule-engine\#75](https://github.com/emqx/emqx-rule-engine/pull/75)

  - 新增返回触发事件的可用字段的 API
    
    Github PR:
    [emqx/emqx-rule-engine\#74](https://github.com/emqx/emqx-rule-engine/pull/74),
    [emqx/emqx-rule-engine\#77](https://github.com/emqx/emqx-rule-engine/pull/77)

错误修复:

  - 修复获取资源状态超时引起的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#76](https://github.com/emqx/emqx-rule-engine/pull/76)

### emqx-dashboard (plugin)

功能增强:

  - 规则引擎各项指标细分到节点
    
    Github PR:
    [emqx/emqx-dashboard\#114](https://github.com/emqx/emqx-dashboard/pull/114)

错误修复:

  - 修复资源创建的 BUG
    
    Github PR:
    [emqx/emqx-dashboard\#114](https://github.com/emqx/emqx-dashboard/pull/114)

## 3.2-rc.2

*Release Date: 2019-06-29*

EMQX 3.2-rc.2 版本改动主要包括错误修复。

功能增强:

  - 把默认日志级别改为 **warning**
    
    Github PR: [emqx/emqx\#2657](https://github.com/emqx/emqx/pull/2657)

  - 增加获取历史告警的接口
    
    Github PRs:
    [emqx/emqx\#2660](https://github.com/emqx/emqx/pull/2660)
    [emqx/emqx-management\#98](https://github.com/emqx/emqx-management/pull/98)

错误修复:

  - 删除残留的 Session 记录
    
    Github PR: [emqx/emqx\#2655](https://github.com/emqx/emqx/pull/2655)

  - 解决批量发送时，消息失序的问题
    
    Github PR: [emqx/emqx\#2650](https://github.com/emqx/emqx/pull/2650)
    
    感谢 [tradingtrace](https://github.com/tradingtrace) 的贡献\!

### emqx-rule-engine (plugin)

功能增强:

  - 新增一个动作 "do nothing"
    
    Github PR:
    [emqx/emqx-rule-engine\#70](https://github.com/emqx/emqx-rule-engine/pull/70)

  - 将 `retain` flag 的数据类型改为 integer
    
    Github RP:
    [emqx/emqx-rule-engine\#72](https://github.com/emqx/emqx-rule-engine/pull/72)

错误修复:

  - 修复 SQL 中无法使用 `timestamp` 关键字作为字段的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#71](https://github.com/emqx/emqx-rule-engine/pull/71)

### emq-bridge-mqtt (plugin)

功能增强:

  - 将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件
    
    Github PR:
    [emqx/emqx-bridge-mqtt\#2](https://github.com/emqx/emqx-bridge-mqtt/pull/2)

### emqx-rel (build project)

错误修复:

  - 解决 windows 服务注册的问题
    
    Github PR:
    [emqx/emqx-rel\#381](https://github.com/emqx/emqx-rel/pull/381)

## 3.2-rc.1

*Release Date: 2019-06-22*

EMQX 3.2-rc.1 版本发布。此版本改动主要包括功能增强与错误修复。

功能增强:

  - 支持设置日志前缀
    
    Github PR: [emqx/emqx\#2627](https://github.com/emqx/emqx/pull/2627)

  - 提升 connect/disconnect 系统消息中客户端上下线的时间戳精度
    
    Github PR: [emqx/emqx\#2641](https://github.com/emqx/emqx/pull/2641)

  - 优化开发流程，支持 `make run`
    
    Github PR: [emqx/emqx\#2644](https://github.com/emqx/emqx/pull/2644)

错误修复:

  - 修复 flapping 模块无法正确读取配置的问题
    
    Github PR: [emqx/emqx\#2628](https://github.com/emqx/emqx/pull/2628)

  - 修复 `cpu_sup:util/0` 在 Windows 环境不可用导致崩溃的问题
    
    Github PR: [emqx/emqx\#2629](https://github.com/emqx/emqx/pull/2629)

  - 修复 [emqx/emqx:
    issue\#2619](https://github.com/emqx/emqx/issues/2619)
    
    Github PR: [emqx/emqx\#2646](https://github.com/emqx/emqx/pull/2646)

### emqx-rule-engine (plugin)

功能增强:

  - 支持定期获取资源状态并设置告警
    
    Github PR:
    [emqx/emqx-rule-engine\#67](https://github.com/emqx/emqx-rule-engine/pull/67)

### emqx-sn (plugin)

错误修复:

  - 修复误判 `keepalive_timeout` 的问题
    
    Github PR:
    [emqx/emqx-sn\#127](https://github.com/emqx/emqx-sn/pull/127)

  - 修复没有正确获取 `idle_timeout` 的问题
    
    Github PR:
    [emqx/emqx-sn\#128](https://github.com/emqx/emqx-sn/pull/128)

  - 修复测试用例
    
    Github PR:
    [emqx/emqx-sn\#130](https://github.com/emqx/emqx-sn/pull/130)

### emqx-auth-jwt (plugin)

错误修复:

  - 正确读取 pubkey
    
    Github PR:
    [emqx/emqx-auth-jwt\#88](https://github.com/emqx/emqx-auth-jwt/pull/88)

### emqx-rel (build-project)

Enhancements:

  - 使项目构建更加智能和健壮
    
    GitHub PR:
    [emqx/emqx-rel\#375](https://github.com/emqx/emqx-rel/pull/375),
    [emqx/emqx-rel\#376](https://github.com/emqx/emqx-rel/pull/376)

## 3.2-beta.3

*Release Date: 2019-06-14*

EMQX 3.2-beta.3 版本发布。此版本改动主要包括增强规则引擎和错误修复。

错误修复:

  - 修复没有检查 `Will Retain` 的问题
    
    Github PR: [emqx/emqx\#2607](https://github.com/emqx/emqx/pull/2607)

  - 修复 [emqx/emqx:
    issue\#2591](https://github.com/emqx/emqx/issues/2591)
    
    Github PR: [emqx/emqx\#2615](https://github.com/emqx/emqx/pull/2615)

  - 默认情况下删除日志记录的字符限制
    
    Github PR: [emqx/emqx\#2617](https://github.com/emqx/emqx/pull/2617)

  - 修复无法处理分裂的 TCP 报文的问题
    
    Github PR: [emqx/emqx\#2611](https://github.com/emqx/emqx/pull/2611)

### emqx-rule-engine (plugin)

功能增强:

  - 支持规则命中次数等 Metrics 统计
    
    Github PR:
    [emqx/emqx-rule-engine\#63](https://github.com/emqx/emqx-rule-engine/pull/63)

### emqx-management (plugin)

错误修复:

  - 修复 CLI 无法踢掉 websocket 连接的问题
    
    Github PR:
    [emqx/emqx-management\#93](https://github.com/emqx/emqx-management/pull/93)

## 3.2-beta.2

*Release Date: 2019-06-06*

EMQX 3.2-beta.2 版本发布。此版本改动主要包括增强规则引擎和错误修复。

错误修复:

  - 修复 [emqx/emqx:
    issue\#2553](https://github.com/emqx/emqx/issues/2553)
    
    Github PR: [emqx/emqx\#2596](https://github.com/emqx/emqx/pull/2596)

### emqx-rule-engine (plugin)

功能增强:

  - 支持在 Dashboard 中测试 SQL 语句
    
    Github Commit:
    [emqx/emqx-rule-engine\#3e7c4c](https://github.com/emqx/emqx-rule-engine/commit/3e7c4cbe275d8f120ad8efb83fd23ee571d465db)

  - 预处理 PreparedStatement 以获得更好的性能
    
    Github Commit:
    [emqx/emqx-rule-engine\#fa3720](https://github.com/emqx/emqx-rule-engine/commit/fa37205850c6efe9af5f8ca2f230e17c7de2adb4),
    [emqx/emqx-rule-engine\#b00fad](https://github.com/emqx/emqx-rule-engine/commit/b00fad45c283fa2ec3aa57353bbe161960547461)

  - 规则引擎适配集群
    
    Github Commit:
    [emqx/emqx-rule-engine\#3da7fe](https://github.com/emqx/emqx-rule-engine/commit/3da7fed60d92c9a994c2aed5f34509c0d0d4eff4),
    [emqx/emqx-rule-engine\#4963b0](https://github.com/emqx/emqx-rule-engine/commit/4963b0ee3a6114ebe74b48876d25723137df14ad)

  - Dashboard 可以显示 Resource 状态
    
    Github Commit:
    [emqx/emqx-rule-engine\#dd9a8d](https://github.com/emqx/emqx-rule-engine/commit/dd9a8d4801f650c1ac888f7420f5497f7d0d6c73),
    [emqx/emqx-rule-engine\#d16224](https://github.com/emqx/emqx-rule-engine/commit/d162246c0b630e059c21f7b36e50154f3d7832e3),
    [emqx/emqx-rule-engine\#e4574c](https://github.com/emqx/emqx-rule-engine/commit/e4574c9554d7e7d79a8ce55a6c9e4089ee00db79)

  - 支持通过 Dashboard 重启 Resource
    
    Github Commit:
    [emqx/emqx-rule-engine\#ccbffd](https://github.com/emqx/emqx-rule-engine/commit/ccbffd7d5db514adf6cd20e8d139e73f80bc1c96)

  - 支持检查 HTTP 是否可连通
    
    Github Commit:
    [emqx/emqx-rule-engine\#3feffc](https://github.com/emqx/emqx-rule-engine/commit/3feffcd5a3f0da78725f1208594cea1b3273ec0b)

错误修复:

  - 修复删除 Resource 前检查依赖发生错误的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#3265ff](https://github.com/emqx/emqx-rule-engine/commit/3265ffe10584f0edccc084e6f78ae035ba310c07)

  - 修复 Resource 无法被销毁的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#58a1ce](https://github.com/emqx/emqx-rule-engine/commit/58a1ce45e1cf96cf05481d8ed076febef0d41976)

  - 修复 SQL 无法嵌套插入的问题
    
    Github Commit:
    [emqx/emqx-rule-engine\#64776a](https://github.com/emqx/emqx-rule-engine/commit/64776aebde1fe48c1038fba3b61f457590ab4408)

### emqx-auth-http (plugin)

功能增强:

  - 支持 HTTPs
    
    Github PR:
    [emqx/emqx-auth-http\#133](https://github.com/emqx/emqx-auth-http/pull/133)

### emqx-docker

错误修复:

  - 修复 [emqx/emqx-docker:
    issue\#115](https://github.com/emqx/emqx-docker/issues/115)
    
    Github Commit:
    [emqx/emqx-docker\#f3c219](https://github.com/emqx/emqx-docker/commit/f3c21978f5ffefd5d419bc78a1caf1ad71de9c91)

### emqx-management (plugin)

错误修复:

  - 修复重新加载插件失败的问题
    
    Github PR:
    [emqx/emqx-management\#91](https://github.com/emqx/emqx-management/pull/91)

### ekka (deps)

错误修复:

  - 修复导致 emqx\_sm\_locker 崩溃的问题
    
    Github Commit:
    [emqx/ekka\#2d5bf2](https://github.com/emqx/ekka/commit/2d5bf2a1f10d84408e4b35d3e274a49f395056c3)

## 3.2-beta.1

*Release Date: 2019-05-27*

EMQX 3.2.beta-1 版本发布。此版本改动主要包括支持 rebar3 构建和增强规则引擎。

功能增强:

  - 支持通过 rabar3 构建项目
    
    Github PR:
    [emqx/emqx\#2475](https://github.com/emqx/emqx/pull/2475),
    [emqx/emqx\#2510](https://github.com/emqx/emqx/pull/2510),
    [emqx/emqx\#2518](https://github.com/emqx/emqx/pull/2518),
    [emqx/emqx\#2521](https://github.com/emqx/emqx/pull/2521)

  - SSL 连接支持 {active, N} 选项
    
    Github PR: [emqx/emqx\#2531](https://github.com/emqx/emqx/pull/2531)

  - 更正匿名访问行为表现
    
    Github PR: [emqx/emqx\#2355](https://github.com/emqx/emqx/pull/2355)

  - 提升 zone 的访问速度
    
    Github PR: [emqx/emqx\#2548](https://github.com/emqx/emqx/pull/2548)

错误修复:

  - 修复 emqx\_sm 中的致命错误
    
    Github PR: [emqx/emqx\#2559](https://github.com/emqx/emqx/pull/2559)

  - 修复发布 MQTT-SN、CoAP 消息时的错误
    
    Github PR: [emqx/emqx\#2556](https://github.com/emqx/emqx/pull/2556)

### emqx-rule-engine (plugin)

功能增强:

  - 更好的规则引擎
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

### emqx-web-hook (plugin)

功能增强:

  - 增加一个用于编码 payload 字段的选项
    
    Github PR:
    [emqx/emqx-web-hook\#119](https://github.com/emqx/emqx-web-hook/pull/119)

### emqx-auth-http (plugin)

功能增强:

  - HTTP 请求支持更多选项
    
    Github PR:
    [emqx/emqx-auth-http\#128](https://github.com/emqx/emqx-auth-http/pull/128)

### emqx-sn (plugin)

错误修复:

  - 修复错误的函数调用
    
    Github PR:
    [emqx/emqx-sn\#118](https://github.com/emqx/emqx-sn/pull/118)

## 3.1.2

*Release Date: 2019-06-06*

EMQX 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

### EMQX Core

Bug fixes:

  - 修复 [emqx/emqx: issue
    \#2595](https://github.com/emqx/emqx/issues/2595)
    
    Github PR: [emqx/emqx\#2601](https://github.com/emqx/emqx/pull/2601)

  - 修复无法设置日志等级的问题
    
    Github PR: [emqx/emqx\#2600](https://github.com/emqx/emqx/pull/2600)

  - 修复返回值不匹配的问题
    
    Github PR: [emqx/emqx\#2560](https://github.com/emqx/emqx/pull/2560)

  - 热修复 `emqx_sn` 与 `emqx_coap` 插件
    
    Github PR: [emqx/emqx\#2556](https://github.com/emqx/emqx/pull/2556)

### emqx-coap (plugin)

错误修复:

  - 修复无法发布消息的问题
    
    Github PR:
    [emqx/emqx-coap\#120](https://github.com/emqx/emqx-coap/pull/120)

### ekka (deps)

错误修复:

  - 修复导致 `emqx_sm_locker` 崩溃的问题
    
    Github PR: [emqx/ekka\#54](https://github.com/emqx/ekka/pull/54)

  - 修复 k8s 无法使用 dns 集群的问题
    
    Github PR: [emqx/ekka\#53](https://github.com/emqx/ekka/pull/53)

  - 修复 etcd 集群不可用的问题
    
    Github PR: [emqx/ekka\#52](https://github.com/emqx/ekka/pull/52)

## 3.1.1

*Release Date: 2019-05-10*

EMQX 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

功能增强:

  - 增大单条日志可打印的最大字符数量
    
    Github PR: [emqx/emqx\#2509](https://github.com/emqx/emqx/pull/2509)

  - `force_shutdown_policy` 将根据系统位数使用不同的默认值
    
    Github PR: [emqx/emqx\#2515](https://github.com/emqx/emqx/pull/2515)

错误修复:

  - 正确地配置和使用 `long_gc` 与 `long_schedule`
    
    Github PR:
    [emqx/emqx\#2504](https://github.com/emqx/emqx/pull/2504),
    [emqx/emqx\#2513](https://github.com/emqx/emqx/pull/2513)

  - 修复没有更新 `suboptions/count` 的问题
    
    Github PR: [emqx/emqx\#2507](https://github.com/emqx/emqx/pull/2507)

### emqx-lwm2m (plugin)

错误修复:

  - 修复 mountpoint 没有生效的问题
    
    Github PR:
    [emqx/emqx-lwm2m\#34](https://github.com/emqx/emqx-lwm2m/pull/34)

  - 修复消息无法被 `emqx-web-hook` 转发的问题
    
    Github PR:
    [emqx/emqx-lwm2m\#35](https://github.com/emqx/emqx-lwm2m/pull/35)

## 3.1.0

*Release Date: 2019-04-26*

EMQX 3.1.0 版本发布。此版本改动主要包括全面支持规则引擎、引入 storm 模块以支持 edge storm、 重构
flapping 代码。

功能改进:

  - 添加 emqx\_ct\_helpers 依赖，并重构测试用例
    
    Github PR: [emqx/emqx\#2480](https://github.com/emqx/emqx/pull/2480)

  - 重构 flapping 代码
    
    Github PR: [emqx/emqx\#2476](https://github.com/emqx/emqx/pull/2476)

### emqx-management (plugin)

问题修复:

  - 修复 listeners acceptors 的值没有正确获取的问题
    
    Github PR:
    [emqx/emqx-management\#76](https://github.com/emqx/emqx-management/pull/76)

### emqx-rule-engine (plugin)

功能改进:

  - 支持规则动作参数的验证
    
    Github PR:
    [emqx/emqx-rule-engine\#b28318](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

  - 删除资源时检查是否存在依赖
    
    Github PR:
    [emqx/emqx-rule-engine\#fa75b9](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

  - 从 republish 动作中移除 `from` 参数
    
    Github PR:
    [emqx/emqx-rule-engine\#8721eb](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

  - 修复了 SQL where 子句不能处理整数的问题
    
    Github PR:
    [emqx/emqx-rule-engine\#c9c761](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)

### emqx-storm (plugin)

功能改进:

  - 支持 edge storm
    
    Github Repository:
    [emqx/emqx-storm](https://github.com/emqx/emqx-storm)

## 3.1-rc.3

*Release Date: 2019-04-19*

EMQX 3.1-rc.3 版本发布。此版本改动主要包括规则引擎增强、错误修复。 注意: 从此版本开始，新增 OpenSUSE
安装包，并且不再提供 Debian 7 安装包。

功能改进:

  - 支持对客户端进行 flapping 检测，以及禁止异常的客户端
    
    Github PR: [emqx/emqx\#2438](https://github.com/emqx/emqx/pull/2438)

  - 支持配置日志输出长度
    
    Github PR: [emqx/emqx\#2461](https://github.com/emqx/emqx/pull/2461)

问题修复:

  - 修复 `emqx_client` 没有正确设置 CONNECT 报文 Keep Alive 字段的问题
    
    Github PR: [emqx/emqx\#2443](https://github.com/emqx/emqx/pull/2443)

### emqx-auth-mysql (plugin)

功能改进:

  - 支持 proxysql
    
    Github PR:
    [emqx/emqx-auth-mysql\#134](https://github.com/emqx/emqx-auth-mysql/pull/134)

### emqx-statsd (plugin)

问题修复:

  - 修复 Windows 兼容性引起的问题
    
    Github PR:
    [emqx/emqx-statsd\#24](https://github.com/emqx/emqx-statsd/pull/24)

### emqx-web-hook (plugin)

功能改进:

  - 支持事件 actions
    
    Github Commit:
    [emqx/emqx-web-hook\#8367e0](https://github.com/emqx/emqx-web-hook/commit/8367e02f5ccafc7df9600c258348461a67c171bd)

  - 优化 webhook 资源的 specs
    
    Github Commit:
    [emqx/emqx-web-hook\#5a1345](https://github.com/emqx/emqx-web-hook/commit/5a13457d4f823fa80df1c7eab9a8e945ae6a0701)

  - 支持通过 hook 类型搜索 actions
    
    Github Commit:
    [emqx/emqx-web-hook\#fb3b1b](https://github.com/emqx/emqx-web-hook/commit/fb3b1ba98ca3f2557a51be98a06537781119132c)

### emqx-rule-engine (plugin)

功能改进:

  - 支持通过资源类型搜索 actions
    
    Github PR:
    [emqx/emqx-rule-engine\#25](https://github.com/emqx/emqx-rule-engine/pull/25)

  - 注册资源提供者更改为加载资源提供者
    
    Github PR:
    [emqx/emqx-rule-engine\#26](https://github.com/emqx/emqx-rule-engine/pull/26)

  - 优化 actions 的输入数据
    
    Github PR:
    [emqx/emqx-rule-engine\#27](https://github.com/emqx/emqx-rule-engine/pull/27)

### emqx-rel

问题修复:

  - 修复修改 log.rotation.size 后启动失败的问题
    
    Github PR:
    [emqx/emqx-rel\#336](https://github.com/emqx/emqx-rel/pull/336)

## 3.1-rc.2

*Release Date: 2019-04-13*

EMQX 3.1-rc.2 版本发布。此版本改动主要包括规则引擎增强、错误修复。

功能改进:

  - 重新设计 emqx\_bridge 的 ensure\_start 与 ensure\_stop API
    
    Github PR: [emqx/emqx\#2423](https://github.com/emqx/emqx/pull/2423)

  - 提供 Handler 以扩展 emqx\_bridge
    
    Github PR: [emqx/emqx\#2414](https://github.com/emqx/emqx/pull/2414)

问题修复:

  - 修复 metrics 在某些情况下没有正确更新的问题
    
    Github PR: [emqx/emqx\#2416](https://github.com/emqx/emqx/pull/2416)

  - 修复 trace log level 无法生效时没有提示的问题
    
    Github PR: [emqx/emqx\#2408](https://github.com/emqx/emqx/pull/2408)

### emqx-auth-http (plugin)

功能增强:

  - 支持用户的 WebServer 回传 Mountpoint
    
    Github PR:
    [emqx/emqx-auth-http\#116](https://github.com/emqx/emqx-auth-http/pull/116)

### emqx-auth-username (plugin)

功能增强:

  - 移除在配置文件中配置默认 username 的功能
    
    Github PR:
    [emqx/emqx-auth-username\#96](https://github.com/emqx/emqx-auth-username/pull/96)

### emqx-auth-clientid (plugin)

功能增强:

  - 移除在配置文件中配置默认 clientid 的功能
    
    Github PR:
    [emqx/emqx-auth-clientid\#81](https://github.com/emqx/emqx-auth-clientid/pull/81)

### emqx-rule-engine (plugin)

功能增强:

  - 支持标准 POSIX CLI 格式
    
    Github PR:
    [emqx/emqx-rule-engine\#23](https://github.com/emqx/emqx-rule-engine/pull/23)

问题修复:

  - 修复 HTTP APIs 中的错误
    
    Github PR:
    [emqx/emqx-rule-engine\#21](https://github.com/emqx/emqx-rule-engine/pull/21)

### emqx-packages (plugin)

问题修复:

  - 修复 EMQX 在 CentOS 中开机启动失败的问题
    
    Github Commit:
    [emqx/emqx-packages\#64760523ea29ca0ad1d85b763f0e8a8e6954db9c](https://github.com/emqx/emqx-packages/commit/64760523ea29ca0ad1d85b763f0e8a8e6954db9c)

### emqx-dashboard (plugin)

功能增强:

  - 新增 Rule-Engine 前端页面
    
    Github PR:
    [emqx/emqx-dashboard\#50](https://github.com/emqx/emqx-dashboard/pull/50)

  - 支持在集群中统一管理 Dashboard 用户
    
    Github PR:
    [emqx/emqx-dashboard\#48](https://github.com/emqx/emqx-dashboard/pull/48)

## 3.1-rc.1

*Release Date: 2019-04-04*

EMQX 3.1-rc.1 版本发布。此版本改动主要包括规则引擎增强、错误修复、稳定性增强等。

功能改进:

  - 支持压缩 WebSocket 消息
    
    Github PR: [emqx/emqx\#2356](https://github.com/emqx/emqx/pull/2356)

  - etcd 集群支持 SSL 连接
    
    Github PR: [emqx/emqx\#2367](https://github.com/emqx/emqx/pull/2367)

  - 支持 Websocket 的 proxy protocol
    
    Github PR: [emqx/emqx\#2372](https://github.com/emqx/emqx/pull/2372)

问题修复:

  - 修复 monitor 模块中的错误逻辑
    
    Github PR: [emqx/emqx\#2353](https://github.com/emqx/emqx/pull/2353)

  - 修复 allow\_anonymous 功能不符合预期的问题
    
    Github PR: [emqx/emqx\#2355](https://github.com/emqx/emqx/pull/2355)

  - 修复 session 进程中无法一次性接收多个消息的问题
    
    Github PR: [emqx/emqx\#2373](https://github.com/emqx/emqx/pull/2373)

  - 修复 message.dropped 的 hook 在某些情况下不会被触发的问题
    
    Github PR: [emqx/emqx\#2399](https://github.com/emqx/emqx/pull/2399)

### emqx-auth-http (plugin)

功能增强:

  - 支持从 SSL 双向连接中取出 Subject Name 与 Common Name 用于认证
    
    Github PR:
    [emqx/emqx-auth-http\#113](https://github.com/emqx/emqx-auth-http/pull/113)

### emqx-auth-clientid (plugin)

功能增强:

  - 支持通过 REST API 操作 ClientId
    
    Github PR:
    [emqx/emqx-auth-clientid\#78](https://github.com/emqx/emqx-auth-clientid/pull/78)

### emqx-auth-jwt (plugin)

功能增强:

  - 支持验证指定的 claims 字段
    
    Github PR:
    [emqx/emqx-auth-jwt\#69](https://github.com/emqx/emqx-auth-jwt/pull/69)

### emqx-rule-engine (plugin)

功能增强:

  - 增强规则引擎
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

### emqx-rel

错误修复:

  - 修复 Windows 环境下 EMQX 需要启动两次的问题
    
    Github Commit:
    [emqx/emqx-rel\#75de3441db9bf03d489609dcbb340a74de263508](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

  - 修复 Windows 环境下 EMQX 安装路径含有中文或空格时无法启动的问题
    
    Github Commit:
    [emqx/emqx-rel\#75de3441db9bf03d489609dcbb340a74de263508](https://github.com/emqx/emqx-rel/commit/75de3441db9bf03d489609dcbb340a74de263508)

## 3.1-beta.3

*Release Date: 2019-03-22*

EMQX 3.1-beta.3 版本发布。此版本改动主要包括引入规则引擎，增强插件发现机制，和修复一些问题等。

功能改进:

  - 增强插件发现机制
    
    Github PR: [emqx/emqx\#2339](https://github.com/emqx/emqx/pull/2339)

问题修复:

  - 修复重复清除告警的错误
    
    Github PR: [emqx/emqx\#2332](https://github.com/emqx/emqx/pull/2332)

  - 修复粘包解析失败的问题
    
    Github PR: [emqx/emqx\#2333](https://github.com/emqx/emqx/pull/2333)

  - 正确设置 PUBLISH 文件中的 DUP 标识
    
    Github PR: [emqx/emqx\#2337](https://github.com/emqx/emqx/pull/2337)

### emqx-rule-engine (plugin)

功能增强:

  - 实现规则引擎原型
    
    Github Repository:
    [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

### emqx-lua-hook (plugin)

功能增强:

  - 增加认证与 ACL 的 hook
    
    Github PR:
    [emqx/emqx-lua-hook\#63](https://github.com/emqx/emqx-lua-hook/pull/63)

### emqx-auth-mysql (plugin)

问题修复:

  - 修复 ACL 功能无法使用的问题
    
    Github PR:
    [emqx/emqx-auth-mysql\#130](https://github.com/emqx/emqx-auth-mysql/pull/130)

## 3.1-beta.2

*Release Date: 2019-03-16*

EMQX 3.1-beta.2 版本发布。此版本改动主要包括重新设计 hooks, 支持 TLS/PSK 和修复 gen\_rpc
的一些问题等。

功能改进:

  - 优化 emqx hooks
    
    Github PR: [emqx/emqx\#2309](https://github.com/emqx/emqx/pull/2309)

  - 支持 TLS/DTLS PSK
    
    Github PR: [emqx/emqx\#2297](https://github.com/emqx/emqx/pull/2297)

  - 将 Request/Response 从 emqx client 分离
    
    Github PR: [emqx/emqx\#2293](https://github.com/emqx/emqx/pull/2293)

错误修复:

  - 修复某些情况下集群转发消息时 Broker 可能崩溃的问题
    
    Github issues:
    [emqx/emqx\#2290](https://github.com/emqx/emqx/issues/2290)
    
    Github PR: [emqx/emqx\#2320](https://github.com/emqx/emqx/pull/2320)

  - 在 Broker 卸载插件并退出前卸载 emqx\_alarm\_handler
    
    Github PR: [emqx/emqx\#2316](https://github.com/emqx/emqx/pull/2316)

  - 修复一个与 emqx bridge 相关的错误
    
    Github issues:
    [emqx/emqx\#2312](https://github.com/emqx/emqx/issues/2312)
    
    Github PR: [emqx/emqx\#2313](https://github.com/emqx/emqx/pull/2313)

  - 终结 inflight full error
    
    Github PR: [emqx/emqx\#2281](https://github.com/emqx/emqx/pull/2281)

### emqx-management (plugin)

功能增强:

  - 增加默认的 secret 配置
    
    Github PR:
    [emqx/emqx-management\#58](https://github.com/emqx/emqx-management/pull/58)

  - 修复插件尚未启动时无法 reload 的问题
    
    Github PR:
    [emqx/emqx-management\#59](https://github.com/emqx/emqx-management/pull/59)

  - 插件相关的 HTTP API 由插件各自实现
    
    Github PR:
    [emqx/emqx-management\#57](https://github.com/emqx/emqx-management/pull/57)

  - 修复查询 io/max\_fds 返回 undefined 的问题
    
    Github issues:
    [emqx/emqx-management\#2222](https://github.com/emqx/emqx-management/issues/2222)
    
    Github PR:
    [emqx/emqx-management\#54](https://github.com/emqx/emqx-management/pull/54)

### emqx-auth-jwt (plugin)

功能增强:

  - 优化 JWT 认证插件
    
    Github PR:
    [emqx/emqx-auth-jwt\#63](https://github.com/emqx/emqx-auth-jwt/pull/63)

### emqx-auth-usernmae (plugin)

功能增强:

  - 增加 CURD HTTP API 以管理用户名密码
    
    Github PR:
    [emqx/emqx-auth-username\#82](https://github.com/emqx/emqx-auth-username/pull/82)

### emqx-web-hook (plugin)

错误修复:

  - 修复格式化消息时的错误
    
    Github issues:
    [emqx/emqx-web-hook\#93](https://github.com/emqx/emqx-web-hook/issues/93)
    
    Github PR:
    [emqx/emqx-web-hook\#96](https://github.com/emqx/emqx-web-hook/pull/96)

### minirest (deps)

错误修复:

  - 过滤未启动插件的 HTTP API
    
    Github PR:
    [emqx/minirest\#12](https://github.com/emqx/minirest/pull/12)

### gen\_rpc (deps)

错误修复:

  - 修复 'gen\_rpc' 的 raw socket flags
    
    Github PR:
    [emqx/gen\_rpc\#5](https://github.com/emqx/gen_rpc/pull/5)

## 3.1-beta.1

*Release Date: 2019-02-28*

EMQX 3.1-beta.1 版本发布。此版本主要针对功能改进，包括引入全新的 Bridge，支持消息批量发送，支持 redis 集群等。

功能改进:

  - 引入新的 Bridge 实现，支持 EMQ Broker 节点间桥接和 MQTT 协议间桥接
    
    Github PR: [emqx/emqx\#2199](https://github.com/emqx/emqx/pull/2199)

  - 支持消息批量发送
    
    Github PR: [emqx/emqx\#2253](https://github.com/emqx/emqx/pull/2253)

  - 使用 gen\_statem behaviour 改进 emqx\_connection 模块
    
    Github PR: [emqx/emqx\#2235](https://github.com/emqx/emqx/pull/2235)

  - 新增资源监控，优化告警处理
    
    Github PR: [emqx/emqx\#2266](https://github.com/emqx/emqx/pull/2266)

### emqx-auth-redis

功能改进:

  - 支持 redis 集群
    
    Github PR:
    [emqx/emqx-auth-redis\#93](https://github.com/emqx/emqx-auth-redis/pull/93)

### emqx-dashboard

功能改进:

  - 为 emqx\_dashboard\_cli 模块增加测试用例
    
    Github PR:
    [emqx/emqx-dashboard\#34](https://github.com/emqx/emqx-dashboard/pull/34)

### emqx-auth-username

功能改进:

  - 增加新的 CLI 以更新 username
    
    Github PR:
    [emqx/emqx-auth-username\#74](https://github.com/emqx/emqx-auth-username/pull/74)

### emqx-auth-clientid

功能改进:

  - 增加新的 CLI 以更新 clientid
    
    Github PR:
    [emqx/emqx-auth-clientid\#59](https://github.com/emqx/emqx-auth-clientid/pull/59)

## 3.0.1

*Release Date: 2019-01-25*

EMQX 3.0.1 版本发布。此版本主要包含功能改进和错误修复。

功能改进:

  - 为 emqx edge 增加 +L 虚拟机参数以减少内存
    
    Github PR: [emqx/emqx\#2110](https://github.com/emqx/emqx/pull/2110)

  - 简化修改日志输出等级的命令
    
    Github PR: [emqx/emqx\#2115](https://github.com/emqx/emqx/pull/2115)

  - 重构 bridge 代码; 支持 bridge 消息持久化
    
    Github PR:
    [emqx/emqx\#2160](https://github.com/emqx/emqx/pull/2160),
    [emqx/emqx\#2117](https://github.com/emqx/emqx/pull/2117),
    [emqx/emqx\#2113](https://github.com/emqx/emqx/pull/2113),
    [emqx/emqx\#2108](https://github.com/emqx/emqx/pull/2108),
    [emqx/emqx\#2053](https://github.com/emqx/emqx/pull/2053)

  - 优化路由匹配
    
    Github PR: [emqx/emqx\#2124](https://github.com/emqx/emqx/pull/2124)

  - 改进 'emqx\_client' 模块设计
    
    Github PR: [emqx/emqx\#2137](https://github.com/emqx/emqx/pull/2137)

  - 改进 'emqx\_pool' 模块的设计
    
    Github PR: [emqx/emqx\#2138](https://github.com/emqx/emqx/pull/2138)

  - 改进共享订阅调度实现
    
    Github PR: [emqx/emqx\#2144](https://github.com/emqx/emqx/pull/2144)

  - 支持重启 emqx 时重新生成配置
    
    Github PR: [emqx/emqx\#2175](https://github.com/emqx/emqx/pull/2175)

问题修复:

  - 修复对端关闭连接时崩溃的问题
    
    Github PR: [emqx/emqx\#2074](https://github.com/emqx/emqx/pull/2074)

  - 修复客户端正常断开连接时依旧发送遗嘱消息的问题
    
    Github PR: [emqx/emqx\#2156](https://github.com/emqx/emqx/pull/2156)

### emqx-lwm2m

问题修复:

  - 移除认证功能
    
    GitHub PR:
    [emqx/emqx-lwm2m\#14](https://github.com/emqx/emqx-lwm2m/pull/14)

### emqx-auth-username

问题修复:

  - 支持可选的加密模式
    
    GitHub PR:
    [emqx/emqx-auth-usernmae\#64](https://github.com/emqx/emqx-auth-username/pull/64)

### emqx-auth-clientid

功能改进:

  - 支持可选的加密模式
    
    GitHub PR:
    [emqx/emqx-auth-clientid\#52](https://github.com/emqx/emqx-auth-username/pull/52)

### emqx-management

功能改进:

  - 增加 'plugins reload <Name\>' CLI 命令，支持重载插件时重新生成配置
    
    Github PR:
    [emqx/emqx-management\#30](https://github.com/emqx/emqx-management/pull/30)

## 3.0.0

*Release Date: 2018-12-22*

EMQX 3.0.0版本，重新设计了订阅的 ETS 表，通过重构模块和调节 erlang 虚拟机参数提升了 EMQ 性能

功能改进:

  - 将虚拟机参数移动到单独的 vm.args 文件
    
    Github PR:
    [emqx/emqx\#2033](https://github.com/emqx/emqx/pull/2033),
    [emqx/emqx\#2057](https://github.com/emqx/emqx/pull/2057),
    [emqx/emqx\#2070](https://github.com/emqx/emqx/pull/2070)

  - 为遗嘱消息主题增加格式校验和 ACL 检查
    
    Github PR: [emqx/emqx\#2075](https://github.com/emqx/emqx/pull/2075)

  - 增加 ACL 检查返回拒绝时是否断开客户端连接的配置选项
    
    Github PR: [emqx/emqx\#2059](https://github.com/emqx/emqx/pull/2059)

  - 重构 session 监控树
    
    Github PR: [emqx/emqx\#2077](https://github.com/emqx/emqx/pull/2077)

  - 增加 'active\_n' 选项以优化 emqx\_connection 的 CPU 占用率
    
    Github PR: [emqx/emqx\#2060](https://github.com/emqx/emqx/pull/2060)

  - 支持客户端批量下线
    
    Github PR: [emqx/emqx\#2060](https://github.com/emqx/emqx/pull/2060)

  - 增加订阅表分片机制
    
    Github PR: [emqx/emqx\#2044](https://github.com/emqx/emqx/pull/2044)

  - 重构 'emqx\_gc' 模块
    
    Github PR: [emqx/emqx\#2090](https://github.com/emqx/emqx/pull/2090)

问题修复:

  - 修复 Topic Alias Maximum 的错误实现
    
    Github PR: [emqx/emqx\#2074](https://github.com/emqx/emqx/pull/2074)

  - 修复部分情况下不会发送遗嘱消息的错误
    
    Github PR: [emqx/emqx\#2068](https://github.com/emqx/emqx/pull/2068)

### emqx-auth-ldap

功能改进:

  - 更好的设计
    
    GitHub PR:
    [emqx/emqx-auth-ldap\#46](https://github.com/emqx/emqx-auth-ldap/pull/46)

### emqx-lua-hook

问题修复:

  - 修复测试用例
    
    GitHub PR:
    [emqx/emqx-lua-hook\#45](https://github.com/emqx/emqx-lua-hook/pull/45)

### emqx-management

功能改进:

  - 为 REST API 增加测试用例，并规范返回的响应格式
    
    Github PR:
    [emqx/emqx-management\#21](https://github.com/emqx/emqx-management/pull/21)

## 3.0-rc.5

*Release Date: 2018-11-30*

EMQX 3.0-rc.5版本发布，该版本支持 metrics 的批量提交和修复错误:

功能改进:

  - 减小依赖大小
    
    Github PR: [emqx/emqx\#1981](https://github.com/emqx/emqx/pull/1981)

  - 支持 metrics 的批量提交
    
    Github PR: [emqx/emqx\#2001](https://github.com/emqx/emqx/pull/2001)

  - 优化 mnesia/ets 的并行读写性能
    
    Github PR: [emqx/emqx\#2006](https://github.com/emqx/emqx/pull/2006)

问题修复:

  - 修复 emqx\_router 中的 'function\_clause' 错误
    
    Github PR: [emqx/emqx\#1998](https://github.com/emqx/emqx/pull/1998)

  - 启动过程中移除 simple 日志句柄
    
    Github PR: [emqx/emqx\#2000](https://github.com/emqx/emqx/pull/2000)

  - 修复 emqx\_reason\_codes 模块可能出现参数异常的问题
    
    Github PR: [emqx/emqx\#2008](https://github.com/emqx/emqx/pull/2008)

### emqx-passwd

功能改进:

  - 支持 Rebar3
    
    GitHub PR:
    [emqx/emqx-passwd\#6](https://github.com/emqx/emqx-passwd/pull/6)

### emqx-web-hook

功能改进:

  - 支持 Rebar3
    
    GitHub PR:
    [emqx/emqx-web-hook\#77](https://github.com/emqx/emqx-web-hook/pull/77)

问题修复:

  - 修复 emqx-web-hook 发送 HTTP 请求时未携带 username 和 clientid 的错误
    
    GitHub PR:
    [emqx/emqx-web-hook\#77](https://github.com/emqx/emqx-web-hook/pull/77)

### emqx-dashboard

问题修复:

  - 修复火狐浏览器无法拷贝应用信息的问题
    
    Github PR:
    [emqx/emqx-dashboard\#12](https://github.com/emqx/emqx-dashboard/pull/12)

### emqx-management

问题修复:

  - 修复 clients 的 CLI 错误
    
    Github PR:
    [emqx/emqx-management\#16](https://github.com/emqx/emqx-management/pull/16)

## 3.0-rc.4

*Release Date: 2018-11-24*

EMQX 3.0-rc.4版本发布，该版本改进日志功能，部分项目支持 Rebar3 构建:

功能改进:

  - 为使用 MQTT v3.1.1的客户端提供避免 loop delivery 的功能
    
    Github PR: [emqx/emqx\#1964](https://github.com/emqx/emqx/pull/1964)

  - 支持使用 username 代替 client\_id，默认不开启
    
    Github PR: [emqx/emqx\#1961](https://github.com/emqx/emqx/pull/1961)

  - 默认日志类型为 both
    
    Github PR: [emqx/emqx\#1979](https://github.com/emqx/emqx/pull/1979)

  - 添加控制日志等级的命令行接口
    
    Github PR: [emqx/emqx\#1977](https://github.com/emqx/emqx/pull/1977)

  - 改进 log tracer 的命令行接口
    
    Github PR: [emqx/emqx\#1973](https://github.com/emqx/emqx/pull/1973)

  - 优化日志性能
    
    Github PR: [emqx/emqx\#1960](https://github.com/emqx/emqx/pull/1960)

问题修复:

  - 修复用户属性的类型验证
    
    Github PR: [emqx/emqx\#1969](https://github.com/emqx/emqx/pull/1969)

  - 修复 max\_topic\_alias 配置项的错误描述
    
    Github PR: [emqx/emqx\#1962](https://github.com/emqx/emqx/pull/1962)

  - 当 client\_id 为空时，将 proc meta-data 设置为服务端生成的 client\_id
    
    Github PR: [emqx/emqx\#1980](https://github.com/emqx/emqx/pull/1980)

### emqx-coap

功能改进:

  - 支持 Rebar3
    
    GitHub PR:
    [emqx/emqx-coap\#89](https://github.com/emqx/emqx-coap/pull/89)

问题修复:

  - 修复 sendfun 参数错误的问题
    
    Github PR:
    [emqx/emqx-coap\#89](https://github.com/emqx/emqx-coap/pull/89)

### emqx-management

问题修复:

  - 修复集群模式下通过 REST API 查找连接不稳定的问题
    
    Github PR:
    [emqx/emqx-management\#11](https://github.com/emqx/emqx-management/pull/11)

### ekka

问题修复:

  - 修复分布式锁的错误判断
    
    Github PR: [emqx/ekka\#39](https://github.com/emqx/ekka/pull/39)

### minirest

功能改进:

  - 支持Rebar3
    
    Github PR:
    [emqx/minirest\#6](https://github.com/emqx/minirest/pull/6)

### cuttlefish

问题修复:

  - 将 cuttlefish 的日志输出到 std\_error
    
    Github PR:
    [emqx/cuttlefish\#4](https://github.com/emqx/cuttlefish/pull/4)

### emqx-rel

功能改进:

  - 构建时更新 cuttlefish
    
    Github PR:
    [emqx/emqx-rel\#253](https://github.com/emqx/emqx-rel/pull/253)

  - 默认不启用 delay\_publish 插件
    
    Github PR:
    [emqx/emqx-rel\#251](https://github.com/emqx/emqx-rel/pull/251)

## 3.0-rc.3

*Release Date: 2018-11-10*

EMQX 3.0-rc.3版本发布，该版本重构 emqx\_mqueue 代码，支持 MQTT-SN, CoAP 与 STOMP 协议:

功能改进:

  - 将 QOS$i 替换为 QOS\_$i
    
    Github PR: [emqx/emqx\#1948](https://github.com/emqx/emqx/pull/1948)

  - 更新配置文件中 ACL cache 的描述信息
    
    Github PR: [emqx/emqx\#1950](https://github.com/emqx/emqx/pull/1950)

  - 重构 emqx\_mqueue 代码
    
    Github PR: [emqx/emqx\#1926](https://github.com/emqx/emqx/pull/1926)

  - lager 替换为 OTP logger
    
    Github PR: [emqx/emqx\#1898](https://github.com/emqx/emqx/pull/1898)

问题修复:

  - 修复重复订阅时的 'badarg' 错误
    
    Github PR: [emqx/emqx\#1943](https://github.com/emqx/emqx/pull/1943)

  - 修复 emqx\_message:format 函数 'badarg' 错误
    
    Github PR: [emqx/emqx\#1954](https://github.com/emqx/emqx/pull/1954)

  - 修复 MQTT bridge 无法使用 TLS 连接的问题
    
    Github PR: [emqx/emqx\#1949](https://github.com/emqx/emqx/pull/1949)

### emqx-stomp

功能改进:

  - 增强 receipt 报文支持，增加测试用例
    
    GitHub PR:
    [emqx/emqx-stomp\#53](https://github.com/emqx/emqx-stomp/pull/53)

### emqx-sn

功能改进:

  - 增强对 MQTT-SN 协议的支持
    
    GitHub PR:
    [emqx/emqx-sn\#90](https://github.com/emqx/emqx-sn/pull/90)

### emqx-lua-hook

问题修复:

  - 修复 emqx-lua-hook 无法正常使用的问题
    
    Github PR:
    [emqx/emqx-lua-hook\#41](https://github.com/emqx/emqx-lua-hook/pull/41)

### emqx-statsd

功能改进:

  - 增加统计指标
    
    Github PR:
    [emqx/emqx-statsd\#4](https://github.com/emqx/emqx-statsd/pull/4)

### emqx-dashboard

功能改进:

  - 增加 qos2/forward 指标
    
    Github PR:
    [emqx/emqx-dashboard\#7](https://github.com/emqx/emqx-dashboard/pull/7)

### emqx-auth-pgsql

问题修复:

  - 修复并发量大时 emqx-auth-pgsql 出错的问题
    
    Github PR:
    [emqx/emqx-auth-pgsql\#94](https://github.com/emqx/emqx-auth-pgsql/pull/94)

## 3.0-rc.2

*Release Date: 2018-10-27*

EMQX 3.0-rc.2版本发布，该版本改进 Will Message 发布机制，新增支持使用 ssl 证书作为 MQTT 用户名:

功能改进:

  - 改进 Will Message 发布机制，增加取消发布处理
    
    Github PR: [emqx/emqx\#1889](https://github.com/emqx/emqx/pull/1889)

  - 新增支持使用 ssl 证书作为 MQTT 用户名
    
    Github PR: [emqx/emqx\#1913](https://github.com/emqx/emqx/pull/1913)

  - 提升代码测试覆盖率
    
    Github PR: [emqx/emqx\#1921](https://github.com/emqx/emqx/pull/1921)

问题修复:

  - 修复 emqx\_broker:subscribed 函数 'bad argument' 错误
    
    Github PR: [emqx/emqx\#1921](https://github.com/emqx/emqx/pull/1921)

## 3.0-rc.1

*Release Date: 2018-10-20*

EMQX 3.0-rc.1版本发布，该版本新增 request & response 以及 LwM2M 插件，修复 PUBLISH 验证问题:

功能改进:

  - 为 CONNECT & CONNACK 报文添加 request & response 支持
    
    Github PR: [emqx/emqx\#1819](https://github.com/emqx/emqx/pull/1819)

  - 为未认证的订阅添加警告信息
    
    Github PR:
    
    [emqx/emqx\#1878](https://github.com/emqx/emqx/pull/1878)

  - 增加 emqx\_hooks 的测试覆盖率, 为 emqx\_mod\_sup 模块增加测试用例
    
    Github PR:
    
    [emqx/emqx\#1892](https://github.com/emqx/emqx/pull/1892)

问题修复:

  - 更新 ACL 文档链接
    
    Github PR: [emqx/emqx\#1899](https://github.com/emqx/emqx/pull/1899)

  - 修复验证 PUBLISH 报文时的匹配问题
    
    Github PR: [emqx/emqx\#1888](https://github.com/emqx/emqx/pull/1888)

  - 修复某些情况下不返回 Reason Code 给 client 的 BUG
    
    Github PR: [emqx/emqx\#1819](https://github.com/emqx/emqx/pull/1819)

  - 修复 emqx\_client 模块中的兼容性问题
    
    Github PR: [emqx/emqx\#1819](https://github.com/emqx/emqx/pull/1819)

### emqx-lwm2m

  - 更新 LwM2M 插件以适配 EMQX 3.0
    
    Github PR:
    [emqx/emqx-lwm2m\#3](https://github.com/emqx/emqx-lwm2m/pull/3)

## 3.0-Beta.4

*Release Date: 2018-09-29*

EMQX 3.0-beta.4 版本发布，该版本改进连接 Shutdown 策略，改进共享订阅 sticky 策略，修复 Delayed
Publish 问题：

功能改进:

  - 为进程自定义 max\_heap\_size
    
    GitHub issues:
    [emqx/emqx\#1855](https://github.com/emqx/emqx/pull/1855)

  - 改进 Topic 别名 Maximum、连接 Receive Maximum
    
    GitHub issues:
    [emqx/emqx\#1873](https://github.com/emqx/emqx/pull/1873)

  - 修复共享订阅 sticky 策略 pick ID 方式
    
    GitHub issues:
    [emqx/emqx\#1871](https://github.com/emqx/emqx/pull/1871)

  - 为 Zone 新增 Mountpoint 配置
    
    GitHub issues:
    [emqx/emqx\#1869](https://github.com/emqx/emqx/pull/1869)

  - 修复make app.config 错误
    
    GitHub issues:
    [emqx/emqx\#1868](https://github.com/emqx/emqx/pull/1868),

  - 修复 Hooks 回调参数错误
    
    GitHub issues:
    [emqx/emqx\#1866](https://github.com/emqx/emqx/pull/1866)

  - 改进 travis 构建支持 rebar3 xref
    
    GitHub issues:
    [emqx/emqx\#1861](https://github.com/emqx/emqx/pull/1861)

  - 升级依赖库 esockd 至 v5.4.2
    
    GitHub issues:
    [emqx/emqx\#1875](https://github.com/emqx/emqx/pull/1875)

  - 升级依赖库 erlang-bcrypt 至0.5.1
    
    GitHub issues:
    [emqx/emqx-passwd\#3](https://github.com/emqx/emqx-passwd/pull/3)

### emqx-delayed-publish

  - 修复消息延时发布
    
    GitHub issues:
    [emqx/emqx-delayed-publish\#5](https://github.com/emqx/emqx-delayed-publish/pull/5)

### emqx-passwd

  - 改进 check\_pass 方式，供各类认证插件调用
    
    GitHub issues:
    [emqx/emqx-passwd\#3](https://github.com/emqx/emqx-passwd/pull/3)

### bcrypt

  - 改进 bcrypt 验证方式
    
    GitHub issues:
    [emqx/erlang-bcrypt\#1](https://github.com/emqx/erlang-bcrypt/pull/1)

### esockd

  - 新增 DTLS PSK 样例
    
    GitHub issues:
    [emqx/esockd\#88](https://github.com/emqx/esockd/pull/88)

  - 修复 DTLS 启动失败
    
    GitHub issues:
    [emqx/esockd\#89](https://github.com/emqx/esockd/pull/89)

  - 改进 SSL 启动方式
    
    GitHub issues:
    [emqx/esockd\#90](https://github.com/emqx/esockd/pull/90)

## 3.0-Beta.3

*Release Date: 2018-09-22*

EMQX 3.0-beta.3版本发布，该版本新增共享订阅派发策略功能，改进GC策略、桥接设计:

功能改进:

  - 修复 travis 构建
    
    GitHub issues:
    [emqx/emqx\#1818](https://github.com/emqx/emqx/pull/1818)

  - 更新模块emqx\_mqueue.erl文档说明
    
    GitHub issues:
    [emqx/emqx\#1815](https://github.com/emqx/emqx/pull/1815)

  - 新增共享订阅派发策略
    
    GitHub issues:
    [emqx/emqx\#1823](https://github.com/emqx/emqx/pull/1823)

  - 修复emqx\_pool模块参数错误
    
    GitHub issues:
    [emqx/emqx\#1827](https://github.com/emqx/emqx/pull/1827)

  - 新增强制shutdown策略
    
    GitHub issues:
    [emqx/emqx\#1836](https://github.com/emqx/emqx/pull/1836)

  - 改进KeepAlive检测算法
    
    GitHub issues:
    [emqx/emqx\#1839](https://github.com/emqx/emqx/pull/1839)

  - 修复跨节点消息投递
    
    GitHub issues:
    [emqx/emqx\#1846](https://github.com/emqx/emqx/pull/1846)

  - 改进Bridge设计
    
    GitHub issues:
    [emqx/emqx\#1849](https://github.com/emqx/emqx/pull/1849)

  - 改进force\_gc\_policy配置
    
    GitHub issues:
    [emqx/emqx\#1851](https://github.com/emqx/emqx/pull/1851)

  - 修复Maximum-QoS选项值
    
    GitHub issues:
    [emqx/emqx\#1852](https://github.com/emqx/emqx/pull/1852)

  - 升级依赖库esockd至v5.4.1
    
    GitHub issues:
    [emqx/emqx\#1858](https://github.com/emqx/emqx/pull/1858)

问题修复:

  - 订阅API，主题属性支持通配符
    
    GitHub issues:
    [emqx/emqx\#1706](https://github.com/emqx/emqx/issues/1706)

  - WebSocket 连接新增Path配置
    
    GitHub issues:
    [emqx/emqx\#1809](https://github.com/emqx/emqx/issues/1809)

  - 修复报文尺寸过大导致block问题
    
    GitHub issues:
    [emqx/emqx\#1811](https://github.com/emqx/emqx/issues/1811)

  - 新增函数check\_expiry
    
    GitHub issues:
    [emqx/emqx\#1813](https://github.com/emqx/emqx/issues/1813)

  - 修复DISCONNECT报文Session Expiry Interval不起作用
    
    GitHub issues:
    [emqx/emqx\#1833](https://github.com/emqx/emqx/issues/1833)

  - 修复DISCONNECT报文Max Session Expiry Interval不起作用
    
    GitHub issues:
    [emqx/emqx\#1834](https://github.com/emqx/emqx/issues/1834)

### emqx-management

  - 改进Bridge CTL命令

  - 修复函数调用emqx\_mgmt\_cli:print() crash问题

  - 修复emqx\_mgmt:subscribe函数'function\_clause'错误
    
    GitHub issues:
    [emqx/emqx-management\#1815](https://github.com/emqx/emqx-management/pull/7)

### emqx-web-hook

修复加载emqx\_web\_hook错误

### emqx-dashboard

  - 修复 Dashboard -\> OverView 中disconnect统计数据显示

  - 在 Dashboard -\> Websocket 新增WebSocket Path参数
    
    GitHub issues:
    [emqx/emqx-dashboard\#5](https://github.com/emqx/emqx-dashboard/pull/5)

### emqx-retainer

  - Retained 消息新增TTL
    
    GitHub issues:
    [emqx/emqx-retainer\#52](https://github.com/emqx/emqx-retainer/issues/52)

### emqx-coap

  - 新增emqx\_coap插件
    
    GitHub issues:
    [emqx/emqx-coap\#5](https://github.com/emqx/emqx-coap/pull/86)
    [emqx/gen-coap\#5](https://github.com/emqx/gen_coap/pull/8)

### emqx-docker

  - 优化Dockerfile
    
    GitHub issues:
    [emqx/emqx-docker\#5](https://github.com/emqx/emqx-docker/pull/71)

### esockd

  - 改进esockd\_connection\_sup模块设计
    
    GitHub issues:
    [emqx/esockd\#86](https://github.com/emqx/esockd/pull/86)

## 3.0-Beta.2

*Release Date: 2018-09-10*

EMQX 3.0-Beta.2 版本主要包含了对 MQTT 5.0 新特性的改进，以及问题修复。

### EMQX Core

功能改进:

  - 支持 MQTT 5.0 'subscription options'
    
    GitHub issues:
    [emqx/emqx\#1788](https://github.com/emqx/emqx/pull/1788),
    [emqx/emqx-retainer\#58](https://github.com/emqx/emqx-retainer/pull/58),
    [emqx/emqx\#1803](https://github.com/emqx/emqx/pull/1803)

  - 增加对 MQTT 5.0 'Topic-Alias' 的校验
    
    GitHub issues:
    [emqx/emqx\#1789](https://github.com/emqx/emqx/pull/1789),
    [emqx/emqx\#1802](https://github.com/emqx/emqx/pull/1802)

  - 改进 hooks 的设计
    
    GitHub issue:
    [emqx/emqx\#1790](https://github.com/emqx/emqx/pull/1790)

  - 将模块 'emqx\_mqtt\_properties' 重命名为 'emqx\_mqtt\_props'
    
    GitHub issue:
    [emqx/emqx\#1791](https://github.com/emqx/emqx/pull/1791)

  - 改进 emqx\_zone
    
    GitHub issue:
    [emqx/emqx\#1795](https://github.com/emqx/emqx/pull/1795)

问题修复:

  - 修复了 'Will Delay Interval' 属性处理错误
    
    GitHub issues:
    [emqx/emqx\#1800](https://github.com/emqx/emqx/pull/1800),
    [emqx/emqx-delayed-publish\#3](https://github.com/emqx/emqx-delayed-publish/pull/3)

  - 修复了 'Reserved' 标志位的处理错误
    
    GitHub issue:
    [emqx/emqx\#1783](https://github.com/emqx/emqx/pull/1783)

  - 为单元测试生成配置文件
    
    GitHub issue:
    [emqx/emqx\#1794](https://github.com/emqx/emqx/pull/1794)

### emqx-management (插件)

功能改进:

  - 增加 'banned' 功能的 restful API
    
    GitHub issue:
    [emqx/emqx-management\#6](https://github.com/emqx/emqx-management/pull/6)

### emqx-delayed-publish (插件)

功��改进:

  - 重构代码
    
    GitHub issue:
    [emqx/emqx-delayed-publish\#4](https://github.com/emqx/emqx-delayed-publish/pull/4)

### minirest (依赖工程)

功能改进:

  - 回调函数里，同时传递 query 参数和 body 参数
    
    GitHub issue:
    [emqx/minirest\#4](https://github.com/emqx/minirest/pull/4)

### emqx-rel (编译工程)

功能改进:

  - 编译时检查 OTP 版本
    
    GitHub issue:
    [emqx/emqx-rel\#217](https://github.com/emqx/emqx-rel/pull/217)

## 3.0-Beta.1

*Release Date: 2018-09-02* 版本别名: The Promise of Tomorrow

3.0-beta.1 版本正式发布。兼容 MQTT-3.1.1 协议的同时， 完整支持 MQTT-5.0 协议。
此外还增加了很多实用的功能特性，重构了核心组件，提升了系统的伸缩性和扩展能力。

### 全面支持 MQTT-5.0

EMQX 3.0 版本实现了大多数的 MQTT-5.0 特性，主要的 MQTT-5.0 新特性一览:

  - 增加了新的 MQTT 控制报文类型: AUTH
    
    MQTT-5.0 里新增加了一个 AUTH 类型的报文，用来实现相对复杂的认证交互流程。

  - Session 过期机制
    
    之前版本的 "Clean session flag" 现在拆分成了两个字段: "Clean Start Flag"，"Session
    Expiry Interval"。

  - Message 过期机制
    
    MQTT-5.0 里，在发布消息时允许设置一个消息过期时间。

  - 所有的 ACK 都可携带 Reason Code
    
    MQTT-5.0 里，所有的回复报文都包含 Reason Code 字段。现在终端可以知道一个请求失败的原因了。

  - 所有的 ACK 都可携带 Reason String
    
    除了 Reason Code 之外，所有的回复报文都可包含一个 Reason String。

  - Server 端主动断开
    
    MQTT-5.0 里，Server 端可以主动断开一个连接了。

  - Payload format and content type
    
    MQTT-5.0 里发消息的时候，可以指定 Payload 类型和一个 MIME 风格的 content type。

  - Request/Response 模式
    
    增加了几个 property，来规范使用 MQTT 协议做 Request/Response 模式的交互。

  - 共享订阅
    
    EMQX 2.x 支持单节点的共享订阅。 现在 EMQX 3.0 支持了整个集群范围内的共享订阅。

  - 订阅 ID
    
    有了这个订阅 ID，终端可以获知某条消息是由哪个订阅来的。

  - Topic 别名
    
    Topic 现在可以有一个整型的别名，这可以降低 MQTT 由于长 Topic 导致的网络交互损耗。

  - 用户自定义的 User properties
    
    MQTT-5.0 里，多数报文都可以携带 User properties。

  - 报文大小限制
    
    EMQX 2.x 里可以配置 Broker 端的最大报文限制，过大的报文会被丢弃，然后 Broker 会将连接断开。MQTT-5.0
    里，通过 CONNECT/CONNECT ACK 报文，客户端和 Broker 端都可以指定最大报文限制。

  - 可选的 Server 端能力通知 (TODO)
    
    Broker 端可以定义不支持的特性，并将其通知给终端。

  - 订阅选项
    
    MQTT-5.0 提供了一些订阅选项，主要是为了桥接的应用。 比如 nolocal，和 retained 消息处理相关的选项。

  - Will delay
    
    MQTT-5.0 允许指定一个时延，定义从连接断开到遗嘱消息被发送出去之前的延时。这样可以避免在短暂的网络断开和波动时发出遗嘱消息。

  - Broker 端的保活设置
    
    MQTT-5.0 里，Broker 端可以指定一个期望终端使用的保活时间。

  - Assigned ClientID
    
    MQTT-5.0 里，如果 ClientID 是 Broker 分配的，服务端需要返回这个 ClientID 给终端。

  - Server reference
    
    MQTT-5.0 里，Broker 可以指定一个另外一个 Broker 让终端使用。可以用来做连接重定向。

### 集群架构演进

EQMX 3.0 引入了伸缩性较强的 RPC 机制，现在单集群可以支持千万级别的并发连接:

    --------               --------

>  EMQX EMQX |
> 
> | Ekka Ekka | | Mnesia Mnesia | | Kernel Kernel | -------- --------

  - 引入 Ekka 以实现集群的自动建立和自动恢复。目前支持以下几种集群建立方式:
    
      - manual: 手动加入集群;
      - static: 使用预置的节点列表自动组建集群;
      - mcast: 使用广播自动建立集群;
      - dns: 使用 DNS A 记录自动建立集群;
      - etcd: 使用 etcd 自动建立集群;
      - k8s: 使用 k8s 自动建立集群。

### 消息速率限制

3.0 引入了消息速率限制，以增加 Broker 的可靠性。在 MQTT TCP 或 SSL 监听器配置里，可以配置:

  - 并发连接数量: max\_clients
  - 连接速率限制: max\_conn\_rate
  - 连接流量限制: rate\_limit
  - 发布速率限制: max\_publish\_rate

### Feature improvements and Bug Fixes

  - 更新了 esockd;
  - 改用 cowboy 以提升 HTTP 连接的性能;
  - 重构了 ACL 缓存机制;
  - 增加本地和远程 MQTT 桥接功能;
  - 配置文件引入 "zone" 的概念，不同的 "zone" 可以使用不同的配置;
  - 重构了 session 模块，减少了节点间的内存拷贝，提升了节点间通信效率;
  - 改进了 OpenLDAP 的 Access Control;
  - 增加了延时发布功能;
  - 增加了支持 Prometheus 的新的监控和统计功能;
  - 改进了 hook 机制。

## 2.3.11

*Release Date: 2018-07-23*

### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

### emq-auth-jwt

Fix token verification with jwerl 1.0.0

### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo\#123)

Support both clientid and username variables in all queries.
(emq-auth-mongo\#123)

## 2.3.10

*Release Date: 2018-06-27*

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9

*Release Date: 2018-05-20*

### Bugfix and Enhancements

Bugfix: check params for REST publish API (\#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (\#78)

## 2.3.8

*Release Date: 2018-05-11*

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq\_auth\_username (\#1588)

Bugfix: Should be an info level when change CleanSession (\#1590)

Bugfix: emqttd\_ctl crashed when emq\_auth\_usename doesn't exist
(\#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (\#116)

## 2.3.7

*Release Date: 2018-04-22*

### Bugfix and Enhancements

Bugfix: fixed spec of function setstats/3 (\#1575)

Bugfix: clean dead persistent session on connect (\#1575)

Bugfix: dup flag not set when re-deliver (\#1575)

Bugfix: Upgrade the lager\_console\_backend config (\#1575)

Improve: Support set k8s namespace (\#1575)

Upgrade the ekka library to v0.2.3 (\#1575)

Improve: move PIPE\_DIR dir from /tmp/${WHOAMI}\_erl\_pipes/$NAME/ to
/$RUNNER\_DATA\_DIR/${WHOAMI}\_erl\_pipes/$NAME/ (emq-relx\#188)

### emq-auth-http

Improve: Retry 3 times when httpc:request occurred
socket\_closed\_remotely error (emq-auth-http\#70)

## 2.3.6

*Release Date: 2018-03-25*

### Bugfix and Enhancements

Security: LWT message checking the ACL (\#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions
(\#1529)

### emq-auth-jwt

Validate JWT token using a expired field (\#29)

## 2.3.5

*Release Date: 2018-03-03*

### Bugfix and Enhancements

Feature: Add etc/ssl\_dist.conf file for erlang SSL distribution
(emq-relx\#178)

Feature: Add node.ssl\_dist\_optfile option and etc/ssl\_dist.conf file
(\#1512)

Feature: Support Erlang Distribution over TLS (\#1512)

Improve: Tune off the 'tune\_buffer' option for external MQTT
connections (\#1512)

### emq-sn

Clean registered topics if mqtt-sn client send a 2nd CONNECT in
connected state (\#76)

Upgrade the esockd library to v5.2.1 (\#76)

### emq-auth-http

Remove 'password' param from ACL and superuser requests (\#66)

## 2.3.4

*Release Date: 2018-01-29*

### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket
(\#1335)

Feature: EMQ node.name with link local ipv6 address not responding to
ping (\#1460)

Feature: Add PROTO\_DIST\_ARG flag to support clustering via IPv6
address. (\#1460)

Bugfix: retain bit is not set when publishing to clients (when it should
be set). (\#1461)

Bugfix: Can't search topic on web dashboard (\#1473)

### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn\#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn\#66)

## 2.3.3

*Release Date: 2018-01-08*

### Bugfix and Enhancements

Add a full documentation for emq.conf and plugins.

Repair a dead link in README - missing emq-lwm2m. (\#1430)

Subscriber with wildcard topic does not receive retained messages with
sub topic has $ sign (\#1398)

Web Interface with NGINX Reverse Proxy not working. (\#953)

### emq-dashboard

Add dashboard.default\_user.login, dashboard.default\_user.password
options to support configuring default admin.

### emq-modules

The emq-modules rewrite config is not right. (\#35)

### emq-docker

Upgrade alpine to 3.7 (\#31)

### emq-packages

Support ARM Platform (\#12)

## 2.3.2

*Release Date: 2017-12-26*

### Bugfix and Enhancements

Support X.509 certificate based authentication (\#1388)

Add proxy\_protocol, proxy\_protocol\_timeout options for ws/wss
listener.

Cluster discovery etcd nodes key must be created manually. (\#1402)

Will read an incorrect password at the last line of
emq\_auth\_username.conf (\#1372)

How can I use SSL/TLS certificate based client authentication? (\#794)

Upgrade the esockd library to v5.2.

### esockd

Improve the parser of proxy protocol v2.

Add 'send\_timeout', 'send\_timeout\_close' options.

Rename esockd\_transport:port\_command/2 function to async\_send/2.

Add test case for esockd\_transport:async\_send/2 function.

Add esockd\_transport:peer\_cert\_subject/1, peer\_cert\_common\_name/1
functions.

### emq-auth-mysql

Update depends on emqtt/mysql-otp.

Fixed the issue that Cannot connect to MySQL 5.7 (\#67).

### emq-relx

Fix mergeconf/3 appending line break error. (\#152)

### emq-sn

Fix crash in emq\_sn\_gateway:transform() function which handles SUBACK.
(\#57)

Define macro SN\_RC\_MQTT\_FAILURE. (\#59)

### emq-web-hook

Filter auth\_failure client for disconnected hook. (\#30)

## 2.3.1

*Release Date: 2017-12-03*

### Bugfix and Enhancements

Remove the unnecessary transactions to optimize session management.

Should not exit arbitrarily when clientid conflicts in mnesia.

Change the default value of 'mqtt.session.enable\_stats' to 'on'.

The DUP flag should be set to 0 for all QoS0 messages. (emqttd\#1319)

Fix the 'no function clause' exception. (emqttd\#1293)

The retained flags should be propagated for bridge. (emqttd\#1293)

The management API should listen on 0.0.0.0:8080. (emqttd\#1353)

Fast close the invalid websocket in init/1 function.

erlang:demonitor/1 the reference when erasing a monitor. (emqttd\#1340)

### emq-retainer

Don't clean the retain flag after the retained message is stored.

Add three CLIs for the retainer plugin. (emq-retainer\#38)

### emq-dashboard

Refactor(priv/www): improve the routing page. (emq-dashboard\#185)

### emq-modules

Turn off the subscription module by default. (emq-modules\#26)

### emq-sn

Add an integration test case for sleeping device.

Do not send will topic if client is kicked out.

Prevent crash information in log when emq\_sn\_gateway getting timeout,
since it is a possible procedure.

### emq-relx

Support node cookie value with = characters. (emq-relx\#146)

### mochiweb

Improve Req:get(peername) funciton to support x-forwarded-for and
x-remote-port. (emqtt/mochiweb\#9)

## 2.3.0 "Passenger's Log"

*Release Date: 2017-11-20*

EMQ 2.3.0 版本正式发布，改进了 PubSub 设计与消息路由性能，更新 EMQ 自带的自签名 SSL 证书，改进 Dashboard
界面与 API 设计。

### Bugfix and Enhancements

Fix the issue that Retained message is not sent for Subscribe to
existing topic. (emqttd\#1314)

Fix the issue that The DUP flag MUST be set to 0 for all QoS0
messages.(emqttd\#1319)

Improve the pubsub design and fix the race-condition issue.
(emqttd\#PR1342)

Crash on macOS High Sierra (emqttd\#1297)

### emq-dashboard Plugin (emq-dashboard\#PR174)

Upgraded the 'subscriptions' RESTful API.

Improvement of the auth failure log. (emq-dashboard\#59)

### emq-coap Plugin (emq-coap\#PR61)

Replaced coap\_client with er\_coap\_client.

Fix: correct the output format of coap\_discover() to enable
".well-known/core".

Refactor the coap\_discover method.

### emq-relx

Upgraded the bin/nodetool script to fix the rpcterms command.

### emq-web-hook Plugin

Fix the emq\_web\_hook plugin getting username from client.connected
hook. (emq-web-hook\#19)

### emq-auth-jwt Plugin(emq-auth-jwt\#PR15)

Added test cases for emq\_auth\_jwt.

Fix jwt:decode/2 functions's return type.

### emq-auth-mongo Plugin(emq-auth-mongo\#PR92)

Updated the default MongoDB server configuration.

## 2.3-rc.2

*Release Date: 2017-10-22*

### Bugfix

Change the default logging level of trace CLI. (emqttd\#1306)

### emq-dashboard Plugin (emq-dashboard\#164)

Fix the 'Status' filters of plugins's management.

Fix the URL Redirection when deleting an user.

Compatible with IE,Safari,360 Browsers.

## 2.3-rc.1

*Release Date: 2017-10-12*

### Bugfix

Fix the issue that invalid clients can publish will message.
(emqttd\#1230)

Fix Dashboard showing no stats data (emqttd\#1263)

Fix a rare occurred building failure (emqttd\#1284)

Support Persistence Logs for longer time (emqttd\#1275)

Fix for users APIs (emqttd\#1289)

Changed passwd\_hash/2 function's return type (emqttd\#1289)

### emq-dashboard Plugin (emq-dashboard\#154)

Improved the Dashboard Interface of Monitoring/Management/Tools.

Allow switching dashboard themes.

Supoort both EN and CN languages.

## 2.3-beta.4

*Release Date: 2017-09-13*

### Highlights

Released a new sexy dashboard.

Add more RESTful APIs for manangement and monitoring.

Configuring the broker through CLI or API without having to restart.

### Bugfix

Job for emqttd.service failed because the control process exited with
error code. (emqttd\#1238)

Travis-CI Build Failing (emqttd\#1221)

Https listener of Dashboard plugin won't work (emqttd\#1220)

Service not starting on Debian 8 Jessie (emqttd\#1228)

### emq-dashboard

1.  Support switching to other clustered node.
2.  Configure and reboot the plugins on the dashboard.
3.  A login page to replace the basic authentication popup window.

### emq-coap

1.Try to clarify the relationship between coap and mqtt in EMQ.
(emq-coap\#54).

2.Fix crashes in coap concurrent test(gen-coap\#3).

## 2.3-beta.3

*Release Date: 2017-08-21*

## 2.3-beta.3

*Release Date: 2017-08-21*

### Enhancements

Add HTTP API for hot configuration.

### Bugfix

1.  Parse 'auth.mysql.password\_hash' error when hot configuration
    reload (emq-auth-mysql\#68)
2.  Set 'auth.pgsql.server' error when hot configuration reload
    (emq-auth-pgsql\#67)
3.  Set 'auth.redis.server' and 'auth.redis.password\_hash' error when
    hot configuration reload (emq-auth-redis\#47)
4.  Fix the issue that when deleting retained message subscribed clients
    are not notified (emqttd\#1207)
5.  Support more parameters for hot configuration reload:

<!-- end list -->

  - mqtt.websocket\_protocol\_header = on
  - mqtt.mqueue.low\_watermark = 20%
  - mqtt.mqueue.high\_watermark = 60%
  - mqtt.client.idle\_timeout = 30s
  - mqtt.client.enable\_stats = off

## 2.3-beta.2

*Release Date: 2017-08-12*

EMQ R2.3-beta.2 版本发布！该版本新增 HTTP 管理 API，支持配置 Keepalive 检测周期，支持配置参数热更新。

目前支持配置热更新的插件有:

  - emq-stomp
  - emq-coap
  - emq-sn
  - emq-lwm2m
  - emq-retainer
  - emq-recon
  - emq-web-hook
  - emq-auth-jwt
  - emq-auth-http
  - emq-auth-mongo
  - emq-auth-mysql
  - emq-auth-pgsql
  - emq-auth-redis

::: tip Tip
为支持命令行更新配置参数，部分认证插件参数值采用','替代了空格分隔符。
:::


### Enhancements

1.  Introduce new HTTP management API.
2.  Add ClientId parameter for HTTP Publish API.
3.  Allow configuring keepalive backoff.
4.  Remove the fullsweep\_after option to lower CPU usage.
5.  Authorize HTTP Publish API with clientId.

### emq-sn Plugin (emq-sn\#49)

1.  Support CONNECT message in
    connected/wait\_for\_will\_topic/wait\_for\_will\_msg states.
2.  Clean registered topic for a restarted client.
3.  Bug fix of not clearing buffered PUBLISH messages received during
    asleep state as those messages are sent to client when client wakes
    up.

### emq-auth-ldap Plugin (emq-auth-ldap\#21)

Improve the design LDAP authentication.

### emq-coap Plugin (emq-coap\#51)

Support CoAP PubSub Specification
(<https://www.ietf.org/id/draft-ietf-core-coap-pubsub-02.txt>)

## 2.3-beta.1

*Release Date: 2017-07-24*

EMQ R2.3-beta.1版本发布！该版本正式支持集群节点自动发现与集群脑裂自动愈合，支持基于IP
Multicast、Etcd、Kubernetes等多种策略自动构建集群。

### 节点发现与自动集群

EMQ R2.3 版本支持多种策略的节点自动发现与集群:

| 策略     | 说明               |
| ------ | ---------------- |
| static | 静态节点列表自动集群       |
| mcast  | UDP组播方式自动集群      |
| dns    | DNS A记录自动集群      |
| etcd   | 通过etcd自动集群       |
| k8s    | Kubernetes服务自动集群 |

### 集群脑裂与自动愈合

EMQ R2.3版本正式支持集群脑裂自动愈合(Network Partition Autoheal):

```properties
cluster.autoheal = on
```

集群脑裂自动恢复流程:

1.  节点收到Mnesia库的'inconsistent\_database'事件3秒后进行集群脑裂确认；
2.  节点确认集群脑裂发生后，向Leader节点(集群中最早启动节点)上报脑裂消息；
3.  Leader节点延迟一段时间后，在全部节点在线状态下创建脑裂视图(SplitView)；
4.  Leader节点在多数派(Majority)分区选择集群自愈的Coordinator节点；
5.  Coordinator节点重启少数派(minority)分区节点恢复集群。

### 节点宕机与自动清除

EMQ R2.3版本支持从集群自动删除宕机节点(Autoclean):

```properties
cluster.autoclean = 5m
```

### LWM2M协议支持

EMQ R2.3
版本正式支持LWM2M协议网关，实现了LWM2M协议的大部分功能。MQTT客户端可以通过EMQ-LWM2M访问支持LWM2M的设备。设备也可以往EMQ-LWM2M上报notification，为EMQ后端的服务采集数据。

LWM2M是由Open Mobile
Alliance(OMA)定义的一套适用于物联网的协议，它提供了设备管理和通讯的功能。LWM2M使用CoAP作为底层的传输协议，承载在UDP或者SMS上

### JSON Web Token认证支持

EMQ R2.3 版本支持基于JWT(JSON Web Token)的MQTT客户端认证。

### Retainer插件

Retainer插件支持'disc\_only'模式存储retained消息。

### Debian 9 安装包

EMQ R2.3 支持Debian 9系统安装包。

### Erlang/OTP R20

EMQ R2.3 版本兼容Erlang/OTP R20，全部程序包基于Erlang/OTP R20构建。

## 2.2 正式版 "Nostalgia"

*Release Date: 2017-07-08*

*版本别名: Nostalgia*

EMQ-2.2.0版本正式发布！EMQ R2.2版本完整支持CoAP(RFC 7252)、MQTT-SN协议，支持Web Hook、Lua
Hook、Proxy Protocol V2，支持Elixir语言插件开发。

Feature: Add 'listeners restart/stop' CLI command (emqttd\#1135)

Bugfix: Exit Code from emqttd\_ctl (emqttd\#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd\#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard\#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed
(emq-coap\#43)

## 2.2-rc.2

*Release Date: 2017-07-03*

::: warning Warning
2.2-rc.2版本源码编译需要Erlang/OTP R19.3+
:::

### 问题与改进

Compatible with Erlang/OTP R20 (emq-relx\#77)

CoAP gateway plugin supports coap-style publish & subscribe pattern.
(emq\_coap\#33)

MQTT-SN gateway plugin supports sleeping device (emq\_sn\#32)

Upgrade esockd and mochiweb libraries to support restarting a listener

## 2.2-rc.1

*Release Date: 2017-06-14*

### 问题与改进

Add a new listener for HTTP REST API (emqttd\#1094)

Fix the race condition issue caused by unregister\_session/1
(emqttd\#1096)

Fix the issue that we cannot remove a down node from the cluster
(emqttd\#1100)

Passed org.eclipse.paho.mqtt\_sn.testing/interoperability tests
(emq\_sn\#29)

Fix the issue that send http request and return non-200 status code, but
AUTH/ACL result is denied (emq-auth-http\#33)

Fix the issue that fail to stop listener (emq\_stomp\#24)

Support using systemctl to manage emqttd service on CentOS

## 2.2-beta.3

*Release Date: 2017-05-27*

### 问题与改进

Call emit\_stats when force GC (emqttd\#1071)

Update the default value of 'mqtt.mqueue.max\_length' to 1000
(emqttd\#1074)

Update emq-auth-mongo READEME (emq-auth-mongo\#66)

Update default password field (emq-auth-mongo\#67)

Upgrade the mongodb library to v3.0.3

Remove ‘check password===undefined && userName\!== undefined’
(emq-dashboard\#120)

### emq\_auth\_redis插件

认证支持HGET查询

### emq\_auth\_mongo插件

支持mongodb集群、Replica Set

### 文档更新

更新Windows源码编译安装

## 2.2-beta.2

*Release Date: 2017-05-20*

### 问题与改进

Add a 'websocket\_protocol\_header' option to handle WebSocket
connection from WeChat (emqttd\#1060)

Assign username and password to MQTT-SN's CONNECT message (emqttd\#1041)

Allow for Content-Type:application/json in HTTP Publish API
(emqttd\#1045)

emqttd\_http.erl:data conversion (emqttd\#1059)

Seperate emq\_sn from emqttd (emq-sn\#24)

Check St0's type, making it easier to debug crash problems
(emq-lua-hook\#6)

Fix error: load xxx.lua (emq-lua-hook\#8)

Leave luerl alone as a rebar project (emq-lue-hook\#9)

Display websocket data in reverse order (emq-dashboard\#118)

priv/www/assets/js/dashboard.js:Fixed a typo (emq-dashboard\#118)

### Update README

Update README of emq-auth-pgsql: add the 'ssl\_opts' configuration
(emq-auth-pgsql\#56)

Update README of emq-auth-mysql: fix the 'passwd\_hash' typo
(emq-auth-mysql\#54)

Update README of emq-auth-mongo: change 'aclquery' to 'acl\_query'
(emq-auth-mongo\#63)

### Elixir Plugin

Add a new plugin
[emq-elixir-plugin](https://github.com/emqtt/emq-elixir-plugin) to
support Elixir language.

## 2.2-beta.1

*Release Date: 2017-05-05*

*EMQ* 2.2-beta.1版本正式发布！EMQ2.2 版本发布主要新功能包括:

1.  支持MQTT协议多监听器配置，支持HAProxy的Proxy Protocol V1/V2
2.  新增Web Hook插件(emq-web-hook)、Lua Hook插件(emq-lua-hook)

### MQTT协议监听器配置

一个EMQ节点可配置多个MQTT协议监听端口，例如下述配置external, internal监听器，分别用于设备连接与内部通信:

    -------

>   - \-- Ex，支持Web Hook、Lua Hook、ernal TCP 1883 --\> | |
>    
>     EMQ | -- Internal TCP 2883 --\> Service
> 
>   - \-- External SSL 8883--\> | |
>    
>    -----

EMQ 2.2 版本etc/emq.conf监听器配置方式:

    listener.tcp.${name}= 127.0.0.1:2883
    
    listener.tcp.${name}.acceptors = 16
    
    listener.tcp.${name}.max_clients = 102400

### Proxy Protocol V1/2支持

EMQ 集群通常部署在负载均衡器(LB)后面，典型架构:

    -----
    |   |
    | L | --TCP 1883--> EMQ

>   - \--SSL 8883--\> | | |
>    
>     B | --TCP 1883--\> EMQ  
>       |
>    
>    -----

HAProxy、NGINX等常用的负载均衡器(LB)，一般通过Proxy Protocol协议传递TCP连接源地址、源端口给EMQ。

EMQ 2.2 版本的监听器开启Proxy Protocol支持:

    ## Proxy Protocol V1/2
    ## listener.tcp.${name}.proxy_protocol = on
    ## listener.tcp.${name}.proxy_protocol_timeout = 3s

### Web Hook插件

新增WebHook插件: [emq-web-hook](https://github.com/emqtt/emq-web-hook)
，支持在MQTT客户端上下线、消息发布订阅时触发WebHook回调。

### Lua Hook插件

新增Lua Hook插件: [emq-lua-hook](https://github.com/emqtt/emq-lua-hook)
，支持Lua脚本注册EMQ扩展钩子来开发插件。

### 改进认证链设计

EMQ 2.2 版本改进认证链设计，当前认证模块返回ignore(例如用户名不存在等情况下)，认证请求将继续转发后面认证模块:

    -------------           ------------           -------------

>   - Client --\> | Redis认证 | -ignore-\> | HTTP认证 | -ignore-\> | MySQL认证
>     |
>    
>       - \------------- ------------ ------------- | | |  
>           |/ |/ |/
>    
>     allow | deny allow | deny allow | deny

### 支持bcrypt密码Hash

EMQ 2.2 版本支持bcrypt密码Hash方式，例如Redis认证插件配置:

    auth.redis.password_hash = bcrypt

### etc/emq.conf配置变更

'mqtt.queue.*' 配置变更为 'mqtt.mqueue.*'

### emq-dashboard

WebSocket页面支持Unsubscribe

## 2.1.2

*Release Date: 2017-04-21*

Fix emqttd\_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd\#1000)

Fix crash caused by duplicated PUBREC packet (emqttd\#1004)

Unload the 'session.created' and 'session.teminated' hooks
(emq-plugin-template)

## 2.1.1

*Release Date: 2017-04-14*

Localhost:8083/status returns 404 when AWS LB check the health of EMQ
(emqttd\#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard\#105)

Fix mqtt-sn Gateway not working (emq-sn\#12)

Upgrade emq-sn Plugin (emq-sn\#11)

Upgrade emq-coap Plugin (emq-coap\#21)

## 2.1.0

*Release Date: 2017-04-07*

The stable release of 2.1 version.

Trouble with auth.mysql.acl\_query (emq-auth-mysql\#38)

Filter the empty fields in ACL table (emq-auth-mysql\#39)

## 2.1.0-rc.2

*Release Date: 2017-03-31*

Support pbkdf2 hash (emq-auth-mongo\#46)

Kickout the conflict WebSocket connection (emqttd\#963)

Correct licence in app.src (emqttd\#958)

SSL options to connect to pgsql (emq-auth-pgsql\#41)

## 2.1.0-rc.1

*Release Date: 2017-03-24*

EMQ fails to start if run under a different linux user than that which
first ran it (emqttd\#842)

Depend on emqtt/pbkdf2 to fix the building errors of Travis CI
(emqttd\#957)

Depend on goldrush and emqtt/pbkdf2 to resolve the building errors
(emqttd\#956)

Fix 'rebar command not found' (emq-relx\#33)

Compile error in v2.1.0-beta.2 (emq-relx\#32)

Support salt with passwords (emq-auth-mongo\#11)

Change the default storage\_type to 'ram' (emq-retainer\#13)

## 2.1.0-beta.2

*Release Date: 2017-03-13*

Cannot find AwaitingAck (emqttd\#597)

EMQ V2.1 crash when public with QoS = 2 (emqttd\#919)

Support pbkdf2 hash (emqttd\#940)

Add src/emqttd.app.src to be compatible with rebar3 (emqttd\#920)

Add more test cases (emqttd\#944)

CRASH REPORT Process <0.1498.0\> with 0 neighbours crashed with reason:
{ssl\_error,{tls\_alert,"certificate unknown"}} in
esockd\_connection:upgrade (emqttd\#915)

'auth.redis.password\_hash = plain' by default (emq-auth-redis\#20)

## 2.1.0-beta.1

*Release Date: 2017-02-24*

*EMQ* 2.1.0-beta.1版本发布。

::: warning Warning
2.1.x版本源码编译需要Erlang/OTP R19+
:::


EMQ正式采用 [Semantic Versioning 2.0.0](http://semver.org)
规范创建发布版本号，按'Tick-Tock'方式按月发布迭代版本。奇数版本问题修复与性能改进，偶数版本架构改进和新功能布。

### GC优化

1.  WebSocket、Client、Session进程空置一段时间后自动Hibernate与GC。
2.  新增'mqtt.conn.force\_gc\_count'配置，Client、Session进程处理一定数量消息后强制GC。
3.  大幅降低WebSocket、Client、Session进程fullsweep\_after设置，强制进程深度GC。

### API改进

Hooks API支持注册带Tag的回调函数，解决相同模块函数多次Hook注册问题。

### 问题修复

emqttd\#916: Add 'mqtt\_msg\_from()' type

emq-auth-http\#15: ACL endpoint isnt called

## 2.1-beta

*Release Date: 2017-02-18*

EMQ v2.1-beta版本正式发布，改进Session/Inflight窗口设计，一个定时器负责全部Inflight
QoS1/2消息重传，大幅降低高消息吞吐情况下的CPU占用。

### Client, Session统计信息

支持对单个Client、Session进程进行统计，etc/emq.conf配置文件中设置'enable\_stats'开启:

    mqtt.client.enable_stats = 60s
    
    mqtt.session.enable_stats = 60s

### 新增missed统计指标

EMQ收到客户端PUBACK、PUBREC、PUBREL、PUBCOMP报文，但在Inflight窗口无法找到对应消息时，计入missed统计指标:

    packets/puback/missed
    
    packets/pubrec/missed
    
    packets/pubrel/missed
    
    packets/pubcomp/missed

### Syslog日志集成

支持输出EMQ日志到Syslog，etc/emq.config配置项:

    ## Syslog. Enum: on, off
    log.syslog = on
    
    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

### Tune QoS支持

支持订阅端升级QoS，etc/emq.conf配置项:

    mqtt.session.upgrade_qos = on

### 'acl reload'管理命令

Reload acl.conf without restarting emqttd service (\#885)

### 配置项变更

1.  变更 mqtt.client\_idle\_timeout 为 mqtt.client.idle\_timeout
2.  新增 mqtt.client.enable\_stats 配置项
3.  新增 mqtt.session.upgrade\_qos 配置项
4.  删除 mqtt.session.collect\_interval 配置项
5.  新增 mqtt.session.enable\_stats 配置项
6.  变更 mqtt.session.expired\_after 为 mqtt.session.expiry\_interval

### 合并扩展模块到emq\_modules项目

合并emq\_mod\_presence, emq\_mod\_subscription,
emq\_mod\_rewrite到emq\_modules项目

变更emq\_mod\_retainer为emq\_retainer项目

### Dashboard插件

Overview页面增加missed相关统计指标。 Client页面增加SendMsg、RecvMsg统计指标。
Session页面增加DeliverMsg、EnqueueMsg指标。

### recon插件

变更recon.gc\_interval配置项类型为duration

### reloader插件

变更reloader.interval配置项类型为duration

## 2.0.7

*Release Date: 2017-01-20*

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB
Packages.

emq-auth-http\#9: Update the priv/emq\_auth\_http.schema,
cuttlefish:unset() if no super\_req/acl\_req config exists

emq-auth-mongo\#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard\#91: Fix the exception caused by binary payload

emq-relx\#21: Improve the bin\\emqttd.cmd batch script for windows

emqttd\#873: Documentation: installing-from-source

emqttd\#870: Documentation: The word in Documents is wrong

emqttd\#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd\#856: Support variables in etc/emq.conf: {{ runner\_etc\_dir }},
{{ runner\_etc\_dir }}, {{ runner\_data\_dir }}

## 2.0.6

*Release Date: 2017-01-08*

Upgrade the [esockd](https://github.com/emqtt/esockd) library to v4.1.1

esockd\#41: Fast close the TCP socket if ssl:ssl\_accept failed

emq-relx\#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo\#31: Mongodb ACL Cannot work?

## 2.0.5

*Release Date: 2016-12-24*

emq-auth-http\#9: Disable ACL support

emq-auth-mongo\#29: Disable ACL support

emq-auth-mongo\#30: {datatype, flag}

## 2.0.4

*Release Date: 2016-12-16*

emqttd\#822: Test cases for SSL connections

emqttd\#818: trap\_exit to link WebSocket process

emqttd\#799: Can't publish via HTTPS

## 2.0.3

*Release Date: 2016-12-12*

emqttd\#796: Unable to forbidden tcp lisener

emqttd\#814: Cannot remove a 'DOWN' node from the cluster

emqttd\#813: Change parameters order

emqttd\#795: Fix metrics of websocket connections

emq-dashboard\#88: Rename the default topic from “/World” to “world”

emq-dashboard\#86: Lookup all online clients

emq-dashboard\#85: Comment the default listener port

emq-mod-retainer\#3: Retained messages get lost after EMQTT broker
restart.

## 2.0.2

*Release Date: 2016-12-05*

emqttd\#787: Stop plugins before the broker stopped, clean routes when a
node down

emqttd\#790: Unable to start emqttd service if username/password
contains special characters

emq-auth-clientid\#4: Improve the configuration of
emq\_auth\_clientid.conf to resolve emqttd\#790

emq-auth-username\#4: Improve the configuration of
emq\_auth\_username.conf to resolve emqttd\#790

## 2.0.1

*Release Date: 2016-11-30*

emqttd\#781: 更新项目README到2.0版本

emq\_dashboard\#84: 显示节点集群状态

emq\_dashboard\#79: 集群节点采用disc\_copies存储mqtt\_admin表

emq\_auth\_clientid: 集群节点采用disc\_copies存储mqtt\_auth\_clientid表

emq\_auth\_username: 集群节点采用disc\_copies存储mqtt\_auth\_username表

emq\_mod\_subscription\#3:
删除emq\_mod\_subscription表与module.subscription.backend配置

emq\_plugin\_template\#5: 插件停止时注销认证/ACL模块

## 2.0 正式版 "西湖以西"

*Release Date: 2016-11-24*

*版本别名: 西湖以西(West of West Lake)*

EMQ-2.0版本正式发布！EMQ-1.0版本产品环境下已支持900K并发连接，EMQ-2.0版本重构了整个项目架构并正式支持共享订阅功能:

1.  支持共享订阅(Shared Subscription)与本地订阅(Local
    Subscription)，解决MQTT协议负载平衡消费问题；
2.  支持CoAP(RFC 7252)、MQTT-SN协议和网关，支持CoAP、MQTT-SN客户端与MQTT客户端互通；
3.  重构配置文件格式与加载方式，支持用户友好的'K = V'文件格式，支持操作系统环境变量；
4.  增加了扩展钩子和大量的认证插件，支持与大部分数据库或NoSQL的认证集成；
5.  支持全平台编译部署，Linux/Unix/Windows以及ARM平台网关，支持Docker镜像制作。

### 共享订阅(Shared Subscription)

共享订阅(Shared Subscription)支持在多订阅者间采用分组负载平衡方式派发消息:

    ---------
    |       | --Msg1--> Subscriber1

>   - Publisher--Msg1,Msg2,Msg3--\>| EMQ | --Msg2--\> Subscriber2
>    
>           | --Msg3--\> Subscriber3
>    
>    -----

使用方式: 订阅者在主题(Topic)前增加'$queue'或'$share/<group\>/'前缀。

### 本地订阅(Local Subscription)

本地订阅(Local Subscription)只在本节点创建订阅与路由表，不会在集群节点间广播全局路由，非常适合物联网数据采集应用。

使用方式: 订阅者在主题(Topic)前增加'$local/'前缀。

### erlang.mk与relx

2.0版本分离 [emqttd](https://github.com/emqtt/emqttd) 主项目和发布项目
[emq-relx](https://github.com/emqtt/emq-relx), 采用
[erlang.mk](https://erlang.mk) 和 [relx](https://github.com/erlware/relx)
编译发布工具替换1.x版本使用的rebar，项目可以跨平台在Linux/Unix/Windows系统下编译。

### CoAP协议支持

2.0版本支持CoAP协议(RFC7252)，支持CoAP网关与MQTT客户端互通。

CoAP插件: <https://github.com/emqtt/emq_coap>

### MQTT-SN协议支持

2.0版本支持MQTT-SN协议，支持MQTT-SN网关与MQTT客户端互通。

MQTT-SN插件: <https://github.com/emqtt/emq_sn>

### 'K = V'格式配置文件

2.0版本支持用户友好的'K = V'格式配置文件etc/emq.conf:

    node.name = emqttd@127.0.0.1
    
    ...
    
    mqtt.listener.tcp = 1883
    
    ...

### 操作系统环境变量

2.0版本支持操作系统环境变量。启动时通过环境变量设置EMQ节点名称、安全Cookie以及TCP端口号:

    EMQ_NODE_NAME=emqttd@127.0.0.1
    EMQ_NODE_COOKIE=emq_dist_cookie
    EMQ_MAX_PORTS=65536
    EMQ_TCP_PORT=1883
    EMQ_SSL_PORT=8883
    EMQ_HTTP_PORT=8083
    EMQ_HTTPS_PORT=8084

### Docker镜像支持

EMQ-2.0版本支持Docker镜像制作，Dockerfile开源在:
<https://github.com/emqtt/emq_docker>

### Windows平台支持

2.0版本完整支持Windows平台的编译、发布与运行，支持Windows平台下的'emqttd\_ctl'控制命令，支持在Windows节点间的集群。

### 问题与改进

\#764: add mqtt.cache\_acl option

\#667: Configuring emqttd from environment variables

\#722: mqtt/superuser calls two times emqtt\_auth\_http

\#754: "-heart" option for EMQ 2.0

\#741: emq\_auth\_redis cannot use hostname as server
address

### 扩展插件

2.0版本发布的认证与扩展插件列表:

| 插件                                                                      | 说明                    |
| ----------------------------------------------------------------------- | --------------------- |
| [emq\_dashboard](https://github.com/emqtt/emqttd_dashboard)             | Web控制台插件(默认加载)        |
| [emq\_auth\_clientid](https://github.com/emqtt/emq_auth_clientid)       | ClientId认证插件          |
| [emq\_auth\_username](https://github.com/emqtt/emq_auth_username)       | 用户名、密码认证插件            |
| [emq\_auth\_ldap](https://github.com/emqtt/emq_auth_ldap)               | LDAP认证/访问控制           |
| [emq\_auth\_http](https://github.com/emqtt/emq_auth_http)               | HTTP认证/访问控制           |
| [emq\_auth\_mysql](https://github.com/emqtt/emq_auth_mysql)             | MySQL认证/访问控制          |
| [emq\_auth\_pgsql](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL认证/访问控制     |
| [emq\_auth\_redis](https://github.com/emqtt/emq_auth_redis)             | Redis认证/访问控制          |
| [emq\_auth\_mongo](https://github.com/emqtt/emq_auth_mongo)             | MongoDB认证/访问控制        |
| [emq\_mod\_rewrite](https://github.com/emqtt/emq_mod_rewrite)           | 重写主题(Topic)插件         |
| [emq\_mod\_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retain消息存储模块          |
| [emq\_mod\_presence](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布          |
| [emq\_mod\_subscription](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅           |
| [emq\_coap](https://github.com/emqtt/emq_coap)                          | CoAP协议支持              |
| [emq\_sn](https://github.com/emqtt/emq_sn)                              | MQTT-SN协议支持           |
| [emq\_stomp](https://github.com/emqtt/emq_stomp)                        | Stomp协议支持             |
| [emq\_sockjs](https://github.com/emqtt/emq_sockjs)                      | Stomp over SockJS协议支持 |
| [emq\_recon](https://github.com/emqtt/emq_recon)                        | Recon性能调试             |
| [emq\_reloader](https://github.com/emqtt/emq_reloader)                  | Reloader代码热加载插件       |
| [emq\_plugin\_template](https://github.com/emqtt/emq_plugin_template)   | 插件开发模版                |

## 2.0-rc.3

## 2.0-rc.3

*Release Date:
2016-11-01*

1.  将Presence、Retainer、Subscription三个扩展模块改为独立插件:

|                                                                         |              |
| ----------------------------------------------------------------------- | ------------ |
| [emq\_mod\_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retain消息存储模块 |
| [emq\_mod\_presence](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布 |
| [emq\_mod\_subscription](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅  |

2.  更新EMQ自带的自签名SSL证书，修复SSL双向认证配置文件错误
3.  Bugfix: Fixed a typo (\#716)
4.  Bugfix: emqttd\_http can not use emq\_auth\_http? \#739
5.  Bugfix: emq\_auth\_redis cannot use hostname as server address
    (\#741)
6.  升级Redis, MySQL, Postgre, MongoDB插件，支持主机名或域名配置

## 2.0-rc.2

*Release Date: 2016-10-19*

1.  集成cuttlefish库，支持'K = V'通用配置文件格式，重构EMQ与全部插件配置文件:
    
        node.name = emqttd@127.0.0.1
        
        ...
        
        mqtt.listener.tcp = 1883
        
        ...

2.  支持操作系统环境变量。启动时通过环境变量设置EMQ节点名称、Cookie以及TCP端口号:
    
        EMQ_NODE_NAME
        EMQ_NODE_COOKIE
        EMQ_MAX_PORTS
        EMQ_TCP_PORT
        EMQ_SSL_PORT
        EMQ_HTTP_PORT
        EMQ_HTTPS_PORT

3.  重构认证模块、ACL模块与扩展模块，更新全部插件项目名称以及配置文件。

TODO: issues closed.

## 2.0-rc.1

*Release Date: 2016-10-03*

1.  超级用户认证成功后，发布订阅时不进行ACL鉴权 (\#696)

2.  MQTT客户端认证失败后，EMQ服务器主动关闭TCP连接 (\#707)

3.  改进插件管理设计，新增插件无需修改rel/sys.config配置

4.  改进全部插件Makefile的emqttd依赖:
    
        BUILD_DEPS = emqttd
        dep_emqttd = git https://github.com/emqtt/emqttd emq20

5.  重新设计Redis插件的ACL鉴权模块

## 2.0-beta.3

*Release Date: 2016-09-18*

### 共享订阅(Shared Subscription)

Shared Suscriptions (\#639, \#416):

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

### 本地订阅(Local Subscription)

Local Subscriptions that will not create global routes:

    mosquitto_sub -t '$local/topic'

### 问题修复

Error on Loading emqttd\_auth\_http (\#691)

Remove 'emqttd' application from dependencies (emqttd\_coap PR\#3)

## 2.0-beta.2

*Release Date: 2016-09-10*

### CoAP协议支持

CoAP协议支持插件(Beta): <https://github.com/emqtt/emqttd_coap>

### API Breaking Changes

'$u', '$c' variables in emqttd.conf and modules/acl.conf changed to
'%u', '%c'

Improve the design of mqtt retained message, replace emqttd\_retainer
with emqttd\_mod\_retainer.

Add 'session.subscribed', 'session.unsubscribed' hooks, remove
'client.subscribe.after' hook

Tab 'retained\_message' -\> 'mqtt\_retained'

### Bugfix

\[2.0 beta1\] FORMAT ERROR: "~s PUBLISH to ~s: ~p" (PR \#671)

Fixing issues in cluster mode. (PR \#681)

Fixing issues with unsubscribe hook (PR \#673)

## 2.0-beta.1

*Release Date: 2016-08-30*

*版本别名: 西湖以西(West of West Lake)*

EMQ 2.0-beta1预览版本(Preview Release)发布。EMQ
2.0版本改进了项目结构、发布方式、Git分支结构以及配置文件格式，以奠定EMQ消息服务器项目长期演进基础。

::: tip Tip
1.x版本产品部署用户请勿升级到该版本，2.0正式版本发布前会有API变更。
:::


### 项目简称 - EMQ

项目简称变更为EMQ(Erlang/Enterprise/Elastic MQTT
Broker)，E含义Erlang/OTP平台、企业(Enterprise)、弹性(Elastic)。

### 项目发布方式

2.0 版本后采用预览版(Preview Release) + 候选版本(Release
Candidate)版本方式迭代发布，2.0版本将陆续发布beta1, beta2,
beta3, rc1, rc2等迭代，直到2.0正式版本发布。

### 应用与发布

2.0 版本后 [emqttd](https://github.com/emqtt/emqttd)
项目只包括消息服务器应用源码，分离发布(rel)为独立项目:
[emqttd\_relx](https://github.com/emqtt/emqttd-relx)
，以解决1.0版本的插件(plugins)与emqttd应用编译依赖问题。

源码编译请clone [emqttd\_relx](https://github.com/emqtt/emqttd-relx):

    git clone https://github.com/emqtt/emqttd-relx.git
    
    cd emqttd-relx && make
    
    cd _rel/emqttd && ./bin/emqttd console

### erlang.mk与relx

2.0 版本发布项目 [emqttd\_relx](https://github.com/emqtt/emqttd-relx) 采用
[erlang.mk](https://erlang.mk) 和 [relx](https://github.com/erlware/relx)
编译发布工具替换1.x版本使用的rebar。原因: <https://erlang.mk/guide/why.html>

### Git分支结构

|             |            |
| ----------- | ---------- |
| stable      | 1.x 稳定版本分支 |
| master      | 2.x 主版本分支  |
| emq10       | 1.x 版本开发分支 |
| emq20       | 2.x 版本开发分支 |
| emq30       | 3.x 版本开发分支 |
| issue\#{id} | Issue修复分支  |

etc/emqttd.conf配置文件 ---------=-------------

2.0 版本改进项目配置文件格式，采用rebar.config、relx.config类似格式，提高配置文件的可读性和可编辑性。

etc/emqttd.conf配置示例:

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.
    
    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.
    
    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

### MQTT-SN协议支持

2.0-beta1版本正式发布 [emqttd\_sn](http://github.com/emqtt/emqttd_sn)
项目支持MQTT-SN协议，插件加载方式启用emqttd\_sn项目，MQTT-SN默认UDP端口: 1884:

    ./bin/emqttd_ctl plugins load emqttd_sn

### 改进插件架构

2.0
版本从emqttd项目删除plugins/目录，插件作为一个普通的Erlang应用，直接依赖(deps)方式在编译到lib目录，插件配置文件统一放置在etc/plugins/目录中:

    ▾ emqttd-relx/
      ▾ etc/
        ▸ modules/
        ▾ plugins/
            emqtt_coap.conf
            emqttd.conf
            emqttd_auth_http.conf
            emqttd_auth_mongo.conf
            emqttd_auth_mysql.conf
            emqttd_auth_pgsql.conf
            emqttd_auth_redis.conf
            emqttd_coap.conf
            emqttd_dashboard.conf
            emqttd_plugin_template.conf
            emqttd_recon.conf
            emqttd_reloader.conf
            emqttd_sn.conf
            emqttd_stomp.conf

### 2.0 版本项目文档

2.0 版本中文文档: <http://emqtt.com/docs/v2/index.html> 或
<http://docs.emqtt.cn/zh_CN/emq20>

2.0 版本英文文档: <https://developer.emqx.io/docs/emq/v2/en/index.html> 或
<http://docs.emqtt.com/>

### 发布订阅流程

<!-- ![image](./_static/images/publish.png) -->

## 1.1.3

*Release Date: 2016-08-19*

Support './bin/emqttd\_ctl users list' CLI (\#621)

Cannot publish payloads with a size of the order 64K using WebSockets
(\#643)

Optimize the procedures that retrieve the Broker version and Borker
description in the tick timer (PR\#627)

Fix SSL certfile, keyfile config (\#651)

## 1.1.2

*Release Date: 2016-06-30*

Upgrade mysql-otp driver to 1.2.0 (\#564, \#523, \#586, \#596)

Fix WebSocket Client Leak (PR \#612)

java.io.EOFException using paho java client (\#551)

Send message from paho java client to javascript client (\#552)

Compatible with the Qos0 PUBREL packet (\#575)

Empty clientId with non-clean session accepted (\#599)

Update docs to fix typos (\#601, \#607)

## 1.1.1

*Release Date: 2016-06-04*

Compatible with the Qos0 PUBREL packet (\#575)

phpMqtt Client Compatibility (\#572)

java.io.EOFException using paho java client (\#551)

## 1.1

*Release Date:
2016-06-01*

1.1版本升级eSockd库到4.0，支持IPv6与监听特定IP地址。新增MongoDB认证插件、HTTP认证插件与Reloader插件。升级MySQL、PostgreSQL、Redis认证插件，采用参数化查询避免SQL注入，并支持超级用户(superuser)认证。

### 问题与改进

Allow human-friendly IP addresses (PR\#395)

File operation error: emfile (\#445)

emqttd\_plugin\_mongo not found in emqttd (\#489)

emqttd\_plugin\_mongo Error While Loading in emqttd (\#505)

Feature request: HTTP Authentication (\#541)

Compatible with the Qos0 PUBREL packet (\#575)

Bugfix: function\_clause exception occurs when registering a duplicated
authentication module (\#542)

Bugfix: ./emqttd\_top msg\_q result: {"init terminating in
do\_boot",{undef,\[{etop,start,\[\],\[\]},{init,start\_it,1,\[\]},{init,start\_em,1,\[\]}\]}}
(\#557)

### Dashboard插件

WebSocket连接页面支持Clean Session, Qos, Retained参数设置 (emqttd\_dashboard\#52)

升级eSockd库到4.0版本，Overview页面显示OTP版本 (emqttd\_dashboard\#61)

Changing dashboard credentials for username authentication
(emqttd\_dashboard\#56)

新增'./bin/emqttd\_ctl admins'管理命令，支持通过命令行重新设置admin密码

### HTTP认证插件

支持通过HTTP API认证/鉴权MQTT客户端: <https://github.com/emqtt/emqttd_auth_http>

### MongoDB认证插件

升级Erlang Mongodb驱动到v1.0.0 (emqttd\_plugin\_mongo\#1)

支持超级用户认证

支持基于MongoDB的ACL (emqttd\_plugin\_mongo\#3)

### MySQL认证插件

支持超级用户认证

采用参数化查询避免SQL注入

### Postgre认证插件

支持超级用户认证

采用参数化查询避免SQL注入

### Redis认证插件

支持超级用户认证

支持ClientId认证/ACL (emqttd\_plugin\_redis\#4)

### Reloader插件

开发调试代码热升级插件: <https://github.com/emqtt/emqttd_reloader>

## 1.0.2

*Release Date: 2016-05-04*

Issue\#534 - './bin/emqttd\_ctl vm' - add 'port/count', 'port/limit'
statistics

Issue\#535 - emqttd\_client should be terminated properly even if
exception happened when sending data

PR\#519 - The erlang '-name' requires the fully qualified host name

emqttd\_reloader plugin - help reload modified modules during
development.

## 1.0.1

*Release Date: 2016-04-16*

PR\#515 - Fix '$queue' pubsub, add 'pubsub\_queue' test and update docs

## 1.0 (七英里)

*Release Date: 2016-04-13*

*版本别名: 七英里(The Seven Mile Journey)*

经过两年开发，五十个版本迭代，我们正式发布1.0(七英里)版本，和完整的中英文项目文档。

1.0版本基本实现了设计目标: 稳定承载来自移动互联网或物联网终端的大量并发MQTT连接，并实现在大数量的终端间快速低延时的MQTT消息路由。

1.  完整支持MQTT V3.1.1协议，扩展支持WebSocket、Stomp或私有TCP等多协议。
2.  稳定承载大规模的并发MQTT客户端连接，单服务器节点支持50万到100万连接。
3.  分布式节点集群或桥接，快速低延时的消息路由，单集群支持1000万规模的路由。
4.  支持消息服务器内扩展，支持定制多种认证方式，插件方式存储消息到后端数据库。

### 问题与改进

1.0版本主要发布完整项目文档，相比0.17.1版本很少代码变更:

Possible race condition using emqttd\_cm (\#486)

Improve the design of retained message expiration (\#503)

Should not expire the retained messages from $SYS/\# topics (\#500)

### 项目文档

1.0 版本中文文档: <http://emqtt.com/docs/> 或 <http://docs.emqtt.cn>

1.0 版本英文文档: <https://developer.emqx.io/docs/emq/v1/en/index.html> 或
<http://docs.emqtt.com/>

### 官方站点

中文站点: <http://emqtt.com>

英文站点: <https://www.emqx.io/>

### 致谢

爱立信与Erlang/OTP语言平台团队(<http://www.erlang.org/>)\!

贡献者(GitHub帐户): @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng
@Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

公司: 开源中国，鲁能电力，太极计算机，电信天翼云直播，研色科技，杭州华思

乐队: 七英里(The Seven Mile Journey)，腰乐队，万能青年旅店

## 0.17.1-beta

*Release Date: 2016-03-22*

### Enhancements

Time unit of session 'expired\_after' changed to minute. (\#479)

### Dashboard

Code Review and improve the design of Dashboard.

## 0.17.0-beta

*Release Date: 2016-03-15*

### Highlights

Installation and Configuration Guide released on <http://docs.emqtt.com>

Improve and Consolidate the design of Hook, Server, PubSub and Router

Upgrade the \[Web
Dashboard\](<https://github.com/emqtt/emqttd_dashboard>) to support
pagination

Bridge emqttd broker to another emqttd broker & emqttd to mosquitto
bridge (\#438)

### Enhancements

emqttd\_ctl: better error message (\#450)

./bin/emqttd\_ctl: add 'routes' command

`` ` routes list # List all routes routes show <Topic> # Show a
route``\`

Add 'backend\_subscription' table and support static subscriptions
(emqttd\_backend)

Add 'retained\_message' table and refactor emqttd\_retainer module
(emqttd\_backend)

A New Hook and Callback Design (emqttd\_hook)

Add PubSub, Hooks APIs to emqttd module (emqttd)

Move start\_listeners/0, stop\_listeners/0 APIs to emqttd\_app module
(emqttd\_app)

### Tests

Add 100+ common test cases.

### Plugins

Upgrade Dashboard, Redis, Stomp and Template Plugins

## 0.16.0-beta

*Release Date: 2016-02-16*

### Highlights

Licensed under the Apache License, Version 2.0 Now.

Improve the design of cluster, support to join or leave the cluster
(\#449):

`` ` $ ./bin/emqttd_ctl cluster cluster join <Node> #Join the cluster
cluster leave #Leave the cluster cluster remove <Node> #Remove the node
from cluster cluster status #Cluster status``\`

Improve the design of Trie and Route, only the wildcard topics stored in
Trie.

Common Test to replace EUnit.

### Enhancements

mqtt\_message record: add 'sender' field (\#440)

refactor the emqttd, emqttd\_time, emqttd\_opts, emqttd\_node modules.

### Bugfix

noproc error when call to gen\_server2:call(false,
{add\_route,Topic,<0.685.0\>}, infinity) (\#446)

\#\#\#\# Plugins

Changed the license of all plugins.

## 0.15.0-beta

*Release Date: 2016-01-31*

### Highlights

Optimize for Push Application, 500K+ Subscribers to a Topic.

Optimization for Route ETS insertion (\#427)

Priority Message Queue for Persistent Session (\#432)

Add Redis, MongoDB Plugins (\#417)

### Enhancements

Username/Password Authentication: Support to configure default users
(\#428)

Improve CLI Commands: pubsub, bridges, trace (\#429)

emqttd\_mod\_subscription: fix client\_connected/3

emqttd\_auth\_mod: add passwd\_hash/2 function

priority\_queue: add plen/2, out/2 functions

### Bugfix

Fix dequeue/1 of emqttd\_bridge...

Add emqttd:seed\_now/0 function

### Plugins

emqttd\_plubin\_mysql: Changed mysql driver to mysql-otp

emqttd\_plugin\_pgsql: Integrate with ecpool

emqttd\_plugin\_redis: First release

emqttd\_plugin\_mongo: First release

## 0.14.1-beta

*Release Date: 2015-12-28*

Bugfix: emqttd\_ws\_client.erl: Unexpected Info:
{'EXIT',<0.27792.18\>,{shutdown,destroy}} (\#413)

Improve: fix spec errors found by dialyzer

## 0.14.0-beta

*Release Date: 2015-12-18*

### Highlights

Scaling to 1.3 Million Concurrent MQTT Connections on a 12 Core, 32G
CentOS server.

New PubSub, Router Design (\#402). Prepare for scaling to 10 millions on
one cluster.

### Enhancements

Improve the gproc\_pool usage with a general emqttd\_pool\_sup

Improve the design of emqttd\_pubsub, add a new emqttd\_router module

Improve the design of the whole supervisor tree

Route aging mechanism to remove the topics that have no subscriptions

Improve the dashboard, mysql, pgsql, stomp, sockjs plugins

Add 'topics', 'subscriptions' admin commands

Avoid using mnesia table index and mnesia:index\_read API to lower CPU
usage

Subscribe timeout exception (\#366)

Long Delay on Multiple Topic Subscription (\#365)

Subscriptions persistence (\#344)

emqttd\_ctl: 'subscriptions' command to force clients to subscribe some
topics (\#361)

### Bugfix

emqttd\_sm: spec of lookup\_session/1 is not right BUG (\#411)

Observer application should be removed from reltool.config for 'wx' app
is not available (\#410)

### Benchmark

1.3 million concurrent MQTT connections on a 12 Core, 32G CentOS Server,
consume about 15G Memory and 200% CPU.

## 0.13.1-beta

*Release Date: 2015-11-28*

Bugfix: Plugin pathes error under windows (\#387)

Improve: Too many error logs "\[error\] Session ..... Unexpected EXIT:
client\_pid=<0.14137.35\>, exit\_pid=<0.30829.22\>, reason=nop..."
(\#383)

Improve: Define QOS0/1/2, Pooler Error (PR\#382)

Improve: High CPU load when 400K unstable mobile connections (\#377)

BugFix: emqttd\_plugin\_pgsql - error using same query with latest
update plugin (pgsql\#5)

## 0.13.0-beta

*Release Date: 2015-11-08*

### Highlights

Rate Limiting based on \[Token
Bucket\](<https://en.wikipedia.org/wiki/Token_bucket>) and \[Leaky
Bucket\](<https://en.wikipedia.org/wiki/Leaky_bucket#The_Leaky_Bucket_Algorithm_as_a_Meter>)
Algorithm

Upgrade eSockd and MochiWeb libraries to support Parameterized
Connection Module

Improve emqttd\_client to support fully asynchronous socket networking

### Enhancements

Protocol Compliant - Session Present Flag (\#163)

Compilation fails if repo is cloned with a different name (\#348)

emqttd\_client: replace gen\_tcp:send with port\_command (\#358)

TCP sndbuf, recbuf, buffer tuning (\#359)

emqttd\_client.erl to handle 'inet\_async', 'inet\_reply' properly
(\#360)

Refator the \[client/session management
design\](<https://github.com/emqtt/emqttd/blob/master/doc/design/ClientSession.md>)

### Bugfix

Cannot kick transient client out when clientId collision (\#357)

Fix the order of emqttd\_app:start\_server/1 (\#367)

emqttd\_<session:subscribe/2> will crash (\#374)

### Benchmark

\[benchmark for 0.13.0
release\](<https://github.com/emqtt/emqttd/wiki/benchmark-for-0.13.0-release>)

3.1G memory and 50+ CPU/core:

```bash
Connections: 250K
Subscribers: 250K
Topics:      50K
Qos1 Messages/Sec In:  4K
Qos1 Messages/Sec Out: 20K
Traffic In(bps):  12M+
Traffic Out(bps): 56M+
```

## 0.12.3-beta

*Release Date: 2015-10-22*

Bugfix: emqttd\_sysmon crasher for 'undefined' process\_info (\#350)

Bugfix: emqttd\_client: catch parser exception (\#353)

## 0.12.2-beta

*Release Date: 2015-10-16*

Bugfix: Retained messages should not be expired if
'broker.retained.expired\_after = 0' (\#346)

## 0.12.1-beta

*Release Date: 2015-10-15*

Highlight: Release for Bugfix and Code Refactor.

Feature: Retained message expiration (\#182)

Improve: '$SYS/\#' publish will not match '\#' or '+/\#' (\#68)

Improve: Add more metrics and ignore '$SYS/\#' publish (\#266)

Improve: emqttd\_sm should be optimized for clustered nodes may be
crashed (\#282)

Improve: Refactor emqttd\_sysmon and suppress 'monitor' messages (\#328)

Task: benchmark for 0.12.0 release (\#225)

Benchmark: About 900K concurrent connections established on a 20Core,
32G CentOS server.

## 0.12.0-beta

*Release Date: 2015-10-08*

### Highlights

Enhance the **emqttd\_ctl** module to allow plugins to register new
commands (\#256)

Add \[emqttd\_recon plugin\](<https://github.com/emqtt/emqttd_recon>) to
debug/optimize the broker (\#235)

Add **'./bin/emqttd\_ctl broker pubsub'** command to check the status of
core pubsub processes

Add **'./bin/emqttd\_top'** command(like etop) to show the top 'msg\_q',
'reductions', 'memory' or 'runtime' processes

'rel/files/emqttd.config.production' for production deployment(default)

'rel/files/emqttd.config.development' for development deployment

### Enhancements

Qos1/2 messages will not be dropped under unstable mobile network
(\#264)

**emqttd\_<session:subscribe/2>, emqttd\_<session:unsubscribe/2>** APIs
should be asynchronous (\#292)

**etc/emqttd.config**: 'idle\_timeout' option to close the idle
client(socket connected but no 'CONNECT' frame received)

**etc/emqttd.config**: 'unack\_retry\_interval' option for redelivering
Qos1/2 messages

How to monitor large 'message\_queue\_len' (\#283)

### Bugfix

Behaviour emqttd\_auth\_mod is missing init callback (\#318)

### Benchmark

Write a new \[benchmark
tool\](<https://github.com/emqtt/emqtt_benchmark>) to benchmark this
release

Hw requirements - 5K users, 25-50 msgs/sec, QoS=1 (\#209)

Supported Number of Connections Greatly Reduced When Clients are
Subscribing (\#324)

## 0.11.0-beta

*Release Date: 2015-09-25*

Highlight: Rebar to manage plugin dependencies.

Highlight: \[Stomp\](<https://github.com/emqtt/emqttd_stomp>) and
\[SockJS\](<https://github.com/emqtt/emqttd_sockjs>) Plugins\!

Improve: add rel/files/emqttd.config.development|production.

Improve: rel/reltool.config.script to release deps of plugin.

Improve: persist mnesia schema on slave nodes.

Improve: use timer:seconds/1 api.

Improve: The binary release will be compiled with R18.1 now.

Bugfix: issue\#306 - emqttd\_cm should unregister the duplicated client

Bugfix: issue\#310 - usage of emqttd\_ctl error: 'session list' should
be 'sessions list'

Bugfix: issue\#311 - './bin/emqttd\_ctl sessions list' error

Bugfix: issue\#312 - unsubcribe will lead to crash if
emqttd\_plugin\_template plugin loaded

## 0.10.4-beta

*Release Date: 2015-09-18*

Optimize session management and upgrade eSockd library to 2.7.1

\[Benchmark for 0.10.4
release\](<https://github.com/emqtt/emqttd/wiki/benchmark-for-0.10.4-release>)

Improve: issue\#294 - \[error\] failed to start connection on
0.0.0.0:1883 - enotconn

Improve: issue\#297 - How do I allow user with some pattern to access
topic with some pattern?

Bugfix: issue\#291 - "./bin/emqttd attach ..." cannot work

Bugfix: issue\#284 - Should not use erlang:list\_to\_atom/1 in
emqttd\_vm.erl

## 0.10.3-beta

*Release Date: 2015-08-30*

Bugfix: issue\#271 - add emqttd\_ws\_client:subscribe/2 function

Bugfix: issue\#269 - bin/emqttd Syntax error on ubuntu

Improve: issue\#265 - client under unstable mobile network generate a
lot of logs

## 0.10.2-beta

*Release Date: 2015-08-26*

Improve: issue\#257 - After the node name changed, the broker cannot
restart for mnesia schema error.

## 0.10.1-beta

*Release Date: 2015-08-25*

Bugfix: issue\#259 - when clustered the emqttd\_dashboard port is close,
and the 'emqttd' application cannot stop normally.

Feature: issue\#262 - Add '<http://host:8083/mqtt/status>' Page for
health check

## 0.10.0-beta

*Release Date: 2015-08-20*

\[Web Dashboard\](<https://github.com/emqtt/emqttd_dashboard>) and
\[MySQL\](<https://github.com/emqtt/emqttd_plugin_mysql>),
\[PostgreSQL\](<https://github.com/emqtt/emqttd_plugin_pgsql>)
Authentication/ACL Plugins\!

Highlight: Web Dashboard to monitor Statistics, Metrics, Clients,
Sessions and Topics of the broker.

Highlight: JSON/HTTP API to query all clients connected to broker.

Highlight: A new \[Plugin
Design\](<https://github.com/emqtt/emqttd/wiki/Plugin%20Design>) and a
\[Template project\](<https://github.com/emqtt/emqttd_plugin_template>)
for plugin development.

Highlight: Authentication/ACL with MySQL, PostreSQl databases (\#194,
\#172)

Feature: Session Statistics including inflight\_queue, message\_queue,
message\_dropped, awaiting\_rel, awaiting\_ack, awaiting\_comp (\#213)

Feature: Cookie based authentication for MQTT over websocket connections
(\#231)

Feature: Get all clients connected to the broker (\#228, \#230, \#148,
\#129)

Feature: "./bin/emqttd\_ctl clients show ClientId" to query client
status (\#226)

Feature: "./bin/emqttd\_ctl clients kick ClientId" to kick out a client

Feature: "./bin/emqttd\_ctl sessions list" to show all sessions

Feature: "./bin/emqttd\_ctl sessions show ClientId" to show a session

Feature: Erlang VM metrics monitor with Web Dashboard (\#59)

Improve: Too many "inflight queue is full\!" log when session is
overloaded (\#247)

Improve: There are two many "MQueue(~s) drop ~s" logs if the message
queue of session is small (\#244)

Improve: gen\_server2(from RabbitMQ) to improve emqttd\_session,
emqttd\_pubsub

Improve: Makefile to build plugins

Bugfix: emqttd\_broker:unhook/2 cannot work (\#238)

Bugfix: emqttd plugin cannot include\_lib("emqttd/include/emqttd.hrl")
(\#233)

Bugfix: Too many 'Session ~s cannot find PUBACK' logs (\#212)

Bugfix: emqttd\_pooler cannot work

## 0.9.3-alpha

*Release Date: 2015-07-25*

Wiki: \[Bridge\](<https://github.com/emqtt/emqttd/wiki/Bridge>)

Improve: emqttd\_protocol.hrl to define 'QOS\_I'

Improve: emqttd\_pubsub to add subscribe/2 API

Improve: ./bin/emqttd\_ctl to support new bridges command

Bugfix: issue \#206 - Cannot bridge two nodes

## 0.9.2-alpha

*Release Date: 2015-07-18*

Improve: issue \#196 - Add New Hook 'client.subscribe.after'

## 0.9.1-alpha

*Release Date: 2015-07-10*

Bugfix: issue \#189 - MQTT over WebSocket(SSL) cannot work?

Bugfix: issue \#193 - 'client.ack' hook should be renamed to
'message.acked', and called by emqttd\_broker:foreach\_hooks

## 0.9.0-alpha

*Release Date: 2015-07-09*

\[Session, Queue, Inflight Window, Hooks, Global MessageId and More
Protocol
Compliant\](<https://github.com/emqtt/emqttd/releases/tag/0.9.0-alpha>)
Now\!

Feature: Session/Queue/Inflight Window Design (\#145).

Feature: Support to resume a persistent session on other clustered node.

Feature: Support alarm management.

Feature: emqttd\_guid to generate global unique message id.

Feature: Hooks for message pub/ack.

Feature: Protocol compliant - message ordering, timeout and retry.

Improve: Every client will start\_link a session process, whether or not
the client is persistent.

Improve: etc/emqttd.config to support more session, queue configuration.

Improve: issue \#179 - Max offline message queue {max\_queue, 100}
meaning.

Improve: issue \#180 - Should change project structure for other
projects maybe depend on 'emqttd'. Merge emqtt, emqttd apps.

Improve: issue \#185 - PacketId and MessageId: the broker should
generate global unique message id.

Improve: issue \#187 - etc/emqttd.config to support https listener

Improve: issue \#186 - emqttd\_cm to store client details

Improve: issue \#174 - add 'from' field to mqtt\_message record.

Improve: issue \#170 - $SYS Topics should support alarms.

Improve: issue \#169 - Add More
\[Hooks\](<https://github.com/emqtt/emqttd/wiki/Hooks-Design>)

Improve: issue \#167 - Inflight window to assure message ordering.

Improve: issue \#166 - Message delivery timeout and retry.

Improve: issue \#143 - Qos1, Qos2 PubSub message timeout.

Improve: issue \#122 - Labeling message with unique id. emqttd\_guid
module to generate global unique msgid.

Improve: emqttd\_bridge to support pending message queue, and fix the
wrong Qos design.

Improve: mqtt\_message record to add 'msgid', 'from' and 'sys' fields.

Change: Add emqttd\_mqueue, emqttd\_guid, emqttd\_alarm modules.

Bugfix: issue \#184 - emqttd\_stats:setstats is not right.

Bugfix: Closed issues \#181, \#119.

Tests: fix the parser, acl test cases.

## 0.8.6-beta

*Release Date: 2015-06-17*

Bugfix: issue \#175 - publish Will message when websocket is closed
without 'DISCONNECT' packet

## 0.8.5-beta

*Release Date: 2015-06-10*

Bugfix: issue \#53 - client will receive duplicate messages when
overlapping subscription

## 0.8.4-beta

*Release Date: 2015-06-08*

Bugfix: issue \#165 - duplicated message when publish 'retained' message
to persistent client

## 0.8.3-beta

*Release Date: 2015-06-05*

Bugfix: issue \#158 - should queue:in new message after old one dropped

Bugfix: issue \#155 - emqtt\_parser.erl: parse\_topics/3 should reverse
topics

Bugfix: issue \#149 - Forget to merge plugins/emqttd\_auth\_mysql from
'dev' branch to 'master' in 0.8.x release

## 0.8.2-alpha

*Release Date: 2015-06-01*

Bugfix: issue \#147 - WebSocket client cannot subscribe queue
'$Q/queue/${clientId}'

Bugfix: issue \#146 - emqttd\_auth\_ldap: fill(Username, UserDn) is not
right

## 0.8.1-alpha

*Release Date: 2015-05-28*

Client \[Presence\](<https://github.com/emqtt/emqttd/wiki/Presence>)
Support and \[$SYS
Topics\](<https://github.com/emqtt/emqttd/wiki/$SYS-Topics>)
Redesigned\!

Bugfix: issue \#138 - when client disconnected normally, broker will not
publish disconnected $SYS message

Bugfix: fix websocket url in emqttd/priv/www/websocket.html

Improve: etc/emqttd.config to allow websocket connections from any hosts

Improve: rel/reltool.config to exclude unnecessary apps.

## 0.8.0-alpha

*Release Date: 2015-05-25*

\[Hooks\](<https://github.com/emqtt/emqttd/wiki/Hooks%20Design>),
Modules and
\[Plugins\](<https://github.com/emqtt/emqttd/wiki/Plugin%20Design>) to
extend the broker Now\!

Plugin: emqttd\_auth\_mysql - MySQL authentication plugin (issues \#116,
\#120)

Plugin: emqttd\_auth\_ldap - LDAP authentication plugin

Feature: emqttd\_broker to support Hooks API

Feature: issue \#111 - Support 'Forced Subscriptions' by
emqttd\_mod\_autosub module

Feature: issue \#126 - Support 'Rewrite rules' by emqttd\_mod\_rewrite
module

Improve: Support hooks, modules to extend the broker

Improve: issue \#76 - dialyzer check

Improve: 'Get Started', 'User Guide', 'Developer Guide' Wiki

Improve: emqtt\_topic to add join/1, feed\_var/3, is\_queue/1

Improve: emqttd\_pooler to execute common tasks

Improve: add emqttd\_sm\_sup module, and use 'hash' gproc\_pool to
manage sessions

Tests: add more test cases for 'emqttd' app

## 0.7.1-alpha

*Release Date: 2015-05-04*

Add doc/design/\* and merge doc/\* to github Wiki

Bugfix: issue \#121 - emqttd cluster issuse

Bugfix: issue \#123 - emqttd:unload\_all\_plugins/0 cannot unload any
plugin

Bugfix: fix errors found by dialyzer

## 0.7.0-alpha

*Release Date: 2015-05-02*

\[MQTT over
WebSocket(SSL)\](<https://github.com/emqtt/emqttd/wiki/MQTT-Over-WebSocket>)
Now\!

\[Plugin
Achitecture\](<https://github.com/emqtt/emqttd/wiki/Plugin%20Design>)
based on OTP application

\[Trace MQTT Packets or
Messages\](<https://github.com/emqtt/emqttd/wiki/Trace%20Design>) to log
files

Feature: issue \#40, \#115 - WebSocket/SSL Support

Feature: issue \#49, \#105 - Plugin Architecture Support

Feature: issue \#93 - Trace API Design

Improve: issue \#109 - emqttd\_broker should add subscribe, notify API

Improve: update README.md to add 'Goals', 'Contributors' chapters

Change: rename etc/app.config to etc/emqttd.config

Change: etc/emqttd.config changed

Bugfix: critical issue \#54 - error when resume session\!

Bugfix: issue \#118 - error report when UNSUBSCRIBE with no topics

Bugfix: issue \#117 - sys\_interval = 0 config cannot work

Bugfix: issue \#112 - Makefile to support build plugins

Bugfix: issue \#96 - "make clean" cannot work

## 0.6.2-alpha

*Release Date: 2015-04-24*

Bugfix: critical issue \#54, \#104, \#106 - error when resume session

Improve: add emqttd\_cm\_sup module, and use 'hash' gproc\_pool to
register/unregister client ids

Improve: kick old client out when session is duplicated.

Improve: move mnesia dir config from etc/app.config to etc/vm.args

## 0.6.1-alpha

*Release Date: 2015-04-20*

Integrate with \[gproc library\](<https://github.com/uwiger/gproc>) to
support pool

Feature: issues\#91 - should use worker\_pool to handle some async work?

Feature: issues\#95 - Topic filters in ACL rule should support 'eq' tag

Improve: issues\#84 - emqttd\_pubsub is redesigned again to protect
mnesia transaction

Improve: issues\#74 - ACL Support and update \[ACL Design
Wiki\](<https://github.com/emqtt/emqttd/wiki/ACL-Design>)

## 0.6.0-alpha

*Release Date: 2015-04-17*

ACL Support Now: \[ACL-Design
Wiki\](<https://github.com/emqtt/emqttd/wiki/ACL-Design>)

Authentication with username, clientid Now: \[Authentication
Wiki\](<https://github.com/emqtt/emqttd/wiki/Authentication>)

Seperate common MQTT library to 'emqtt' application

Redesign message pubsub, route and retain modules

Redesign mnesia database cluster

Feature: issues\#47 - authentication, authorization support

Feature: issues\#92 - merge emqttd\_acl and emqttd\_auth to
emqttd\_access\_control

Feature: emqttd\_acl\_mod, emqttd\_auth\_mod behaviour to extend ACL,
authentication

Feature: issues\#85 - lager:info to log subscribe, unsubscribe actions

Feature: issues\#77 - authentication with clientid, ipaddress

Improve: issues\#90 - fix lager\_file\_backend log format, and rotate 10
log files

Improve: issues\#88 - use '-mneisa\_create', '-mnesia\_replicate'
attributes to init mneisa

Improve: issues\#87 - record mqtt\_user and mqtt\_client is duplicated

Improve: issues\#81 - redesign nodes cluster to support disc\_copies
mnesia tables

Improve: issues\#80 - redesign emqttd\_cm to handle more concurrent
connections

Improve: issues\#70 - how to handle connection flood? Now could support
2K+ CONNECT/sec

Change: redesign mnesia tables: message, topic, subscriber, trie,
trie\_node

Bugfix: issues\#83 - emqttd\_broker stats cannot work

Bugfix: issues\#75 - careless about function name when emqttd\_pubsub
handle getstats message

## 0.5.5-beta

*Release Date: 2015-04-09*

Bugfix: issue \#75 - careless about function name when emqttd\_pubsub
handle getstats message.

Bugfix: issue \#79 - cannot find topic\_subscriber table after cluster
with other nodes.

## 0.5.4-alpha

*Release Date: 2015-03-22*

Benchmark this release on a ubuntu/14.04 server with 8 cores, 32G memory
from QingCloud.com: :

    200K Connections,
    30K Messages/Sec,
    20Mbps In/Out Traffic,
    200K Topics,
    200K Subscribers,
    Consumed 7G memory, 40% CPU/core

Benchmark code: <https://github.com/emqtt/emqttd_benchmark>

Change: rewrite emqttd\_pubsub to handle more concurrent subscribe
requests.

Change: ./bin/emqttd\_ctl add 'stats', 'metrics' commands.

Bugfix: issue \#71, \#72

## 0.5.3-alpha

*Release Date: 2015-03-19*

Bugfix: issues\#72 - emqttd\_cm, emqtt\_sm ets:match\_delete/2 with
wrong pattern

## 0.5.2-alpha

*Release Date: 2015-03-18*

Change: upgrade esockd to 2.1.0-alpha, do not tune socket buffer for
mqtt connection.

## 0.5.1-alpha

*Release Date: 2015-03-13*

Change: upgrade esockd to v1.2.0-beta, rename 'acceptor\_pool' to
'acceptors'

## 0.5.0-alpha

*Release Date: 2015-03-12*

RENAME 'emqtt' to 'emqttd'\!

Support \[Broker
Bridge\](<https://github.com/emqtt/emqttd/wiki/Bridge-Design>) Now\!

Change: rename project from 'emqtt' to 'emqttd'

Change: lager:debug to dump RECV/SENT packets

Feature: emqttd\_bridge, emqttd\_bridge\_sup to support broker bridge

Feature: emqtt\_event to publish client connected/disconnected message
to $SYS topics

Feature: ./bin/emqttd\_ctl add more commands: listeners, broker,
bridges, start\_bridge, stop\_bridge...

Feature: issue\#57 - support to configure max packet size

Feature: issue\#68 - if sys\_interval = 0, emqttd\_broker will not
publish messages to $SYS/brokers/\#

Bugfix: issue\#67 - subscribe '\#' to receive all messages

Bugfix: issue\#64 - emqtt\_app start/2: should wait\_for\_databases

Test: emqttd\_topic\_tests add more '\_match\_test'

## 0.4.0-alpha

*Release Date: 2015-03-10*

Support \[$SYS Topics of
Broker\](<https://github.com/emqtt/emqttd/wiki/$SYS-Topics-of-Broker>)
Now\!

Feature: emqtt\_broker to publish version, uptime, datetime to
$SYS/brokers/\# topics

Feature: emqtt\_broker to publish count of clients, sessions, suscribers
to $SYS/brokers/\# topics

Feature: emqtt\_metrics to publish bytes, packets, messages metrics to
$SYS/brokers/\# topics

Feature: add include/emqtt\_systop.hrl

Change: emqtt\_cm to count current clients

Change: emqtt\_sm to count current sessions

Change: emqtt\_pubsub to count current topics and suscribers

Change: emqtt\_pubsub to add create/1 API

Change: emqtt\_pubsub dispatch/2 to return number of subscribers

Change: emqtt\_pubsub to count 'dropped' messages

Change: emqtt\_opts to add merge/2 function

Test: add emqtt\_serialiser\_tests.erl

## 0.3.4-beta

*Release Date: 2015-03-08*

Bugfix: emqtt\_serialiser.erl cannot serialise UNSUBACK packets

## 0.3.3-beta

*Release Date: 2015-03-07*

Bugfix: emqtt\_serialiser.erl cannot serialise PINGRESP issue\#60

## 0.3.2-beta

*Release Date: 2015-03-05*

Improve: merge emqttc serialiser, parser, packet

Add: emqtt\_opts to merge socket options

## 0.3.1-beta

*Release Date: 2015-03-02*

Feature: SSL Socket Support

Feature: issue\#44 HTTP API should add Qos parameter

Bugfix: issue\#52 emqtt\_session crash

Bugfix: issue\#53 sslsocket keepalive error

Upgrade: esockd to v0.2.0

Upgrade: mochiweb to v3.0.0

## 0.3.0-beta

*Release Date: 2015-01-19*

Feature: HTTP POST API to support 'qos', 'retain' parameters

Feature: $SYS system topics support

Change: Rewrite emqtt\_topic.erl, use '', '\#', '+' to replace
<<""\>\>, <<"\#"\>\>, <<"+"\>\>

Change: fix emqtt\_pubsub.erl to match '\#', '+'

Tests: emqtt\_topic\_tests.erl add more test cases

## 0.3.0-alpha

*Release Date: 2015-01-08*

NOTICE: Full MQTT 3.1.1 support now\!

Feature: Passed org.eclipse.paho.mqtt.testing/interoperability tests

Feature: Qos0, Qos1 and Qos2 publish and suscribe

Feature: session(clean\_sess=false) management and offline messages

Feature: redeliver awaiting puback/pubrec messages(doc: Chapter 4.4)

Feature: retain messages, add emqtt\_server module

Feature: MQTT 3.1.1 null client\_id support

Bugfix: keepalive timeout to send will message

Improve: overlapping subscription support

Improve: add emqtt\_packet:dump to dump packets

Test: passed org.eclipse.paho.mqtt.testing/interoperability

Test: simple cluster test

Closed Issues: \#22, \#24, \#27, \#28, \#29, \#30, \#31, \#32, \#33,
\#34, \#36, \#37, \#38, \#39, \#41, \#42, \#43

## 0.2.1-beta

*Release Date: 2015-01-08*

pull request 26: Use binaries for topic paths and fix wildcard topics

emqtt\_pubsub.erl: fix wildcard topic match bug caused by binary topic
in 0.2.0

Makefile: deps -\> get-deps

rebar.config: fix mochiweb git url

tag emqtt release accoding to \[Semantic
Versioning\](<http://semver.org/>)

max clientId length is 1024 now.

## 0.2.0

*Release Date: 2014-12-07*

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5

*Release Date: 2013-01-05*

Bugfix: remove QOS\_1 match when handle PUBREL request

Bugfix: reverse word in emqtt\_topic:words/1 function

## 0.1.4

*Release Date: 2013-01-04*

Bugfix: fix "mosquitto\_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3

*Release Date: 2013-01-04*

Feature: support QOS2 PUBREC, PUBREL,PUBCOMP messages

Bugfix: fix emqtt\_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2

*Release Date: 2012-12-27*

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp\_listener.erl

## 0.1.1

*Release Date: 2012-09-24*

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0

*Release Date: 2012-09-21*

The first public release.
