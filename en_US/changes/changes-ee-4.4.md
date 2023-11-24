# Releases

## e4.4.23

*Release Date: 2023-11-24*

### Bug Fixes

- Fix the issue that the rule engine cannot connect to `upstash` Redis.

  Before the fix, after establishing a TCP connection with the Redis service, the redis driver of emqx used [Inline Commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) to send AUTH and SELECT commands. However, the `upstash` Redis service does not support Inline Commands, which causes the rule engine to fail to connect to the `upstash` Redis service.
  After the fix, the redis driver of emqx uses RESP (REdis Serialization Protocol) to send AUTH and SELECT commands.

- Add validity check for some parameters of the "Offline Msg to Redis" action and Redis resource.

  * Check the "Redis Key TTL" parameter of the "Offline Msg to Redis" action.
  * Check the "Redis Database" parameter of the Redis resource.

## e4.4.22

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

## e4.4.21

*Release Date: 2023-10-16*

### Enhancements

- Added support for Confluent data bridge.

- Now the MQTT topic field in the Kafka consumer group supports templates with placeholders.

  For example, if the key of the message consumed by Kafka is "a", and the configured MQTT topic is "topic/${key}", then the MQTT topic will be replaced with "topic/a" when the message is forwarded.

- Now the "Message Republish" action supports two new fields: "MQTT Properties" and "User Properties". Both of the fields are in the format of key-value pairs, and both the key and value support placeholders.


### Bug Fixes

- Fixed the issue that the Kafka action cannot send numeric values as Kafka Headers.

  Prior to this fix, when the "Kafka Headers value encode mode" was set to "NONE", if the "Kafka Headers" field contains a JSON object with numeric types (such as `{"a": 1, "b": "str"}`), the numeric values (`"a":1`) would be ignored and not sent to Kafka. After the fix, the numeric types in JSON will be converted to strings before being sent to Kafka.

## e4.4.20

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

## e4.4.19

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

## e4.4.18

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

## e4.4.17

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

## e4.4.16

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

- Change "EMQ X" to "EMQX" from the outputs of CLIs and names of plugins.

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

## e4.4.15

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

## e4.4.14

*Release Date: 2023-01-06*

### Enhancements

- Add a password complexity requirement when adding or modifying Dashboard users via the API. Now passwords must contain at least 2 of alphabetic, numeric and special characters, and must be 8 to 64 characters long.

### Bug Fixes

- Fix the problem that adding or importing Dashboard users via the API fails to add complex passwords due to incorrect checksum of the passwords.

- Fix load bootstrap_app_file's apps is not sync when reboot.

## e4.4.13

*Release Date: 2023-01-03*

### Bug Fixes

- Fix an issue where testing the GCP PubSub could leak memory, and an issue where its JWT token would fail to refresh a second time. [#9640](https://github.com/emqx/emqx/pull/9640)

## e4.4.12

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
## e4.4.11

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

## e4.4.10

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

## e4.4.9

*Release Date: 2022-09-17*

### Enhancements

- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps

### Bug Fixes

- Fix rule engine update behaviour which may initialize actions for disabled rules
- Fix inaccurate delayed publish due to OS time changes
- Fix the issue that the IP address bound to the Dashboard listener did not take effect
- Fix the issue that shared subscriptions might get stuck in an infinite loop when `shared_dispatch_ack_enabled` is set to true
- Fix the issue that the rule engine SQL crashes when subject matching null values

## e4.4.8

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

## e4.4.7

*Release Date: 2022-08-11*

### Important Changes

- As of version 4.4.7, we will no longer provide packages for macOS 10

### Enhancements

- Allows the connection process to be configured to be garbage collected after the TLS handshake is complete to reduce memory footprint, which can reduce memory consumption by about 35% per SSL connection, but increases CPU consumption accordingly
- Allows configuring the log level of the TLS handshake log to view the detailed handshake process

### Bug Fixes

- Fix the issue that EMQX could not be started when deployed through Helm Chart after unmounting the `loaded_modules` file in ConfigMap

## e4.4.6

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

## e4.4.5

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

## e4.4.4

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

## e4.4.3

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

## e4.4.2

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

## e4.4.1

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

## e4.4.0

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
