# Known Issues in EMQX 5.8

## e5.8.1

- **Node Crash if Linux monotonic clock steps backward (since 5.0)**

  In certain virtual Linux environments, the operating system is unable to keep the clocks monotonic,
  which may cause Erlang VM to exit with message `OS monotonic time stepped backwards!`.
  For such environments, one may set the `+c` flag to `false` in `etc/vm.args`.

- **Node Cannot Start if a New Node Joined Cluster While It was Stopped (since 5.0)**

  In a cluster of 2 or more nodes, if a new node joins the cluster while some nodes are down, the nodes which were down will fail to restart and will emit logs like below.
  `2024-10-03T17:13:45.063985+00:00 [error] Mnesia('emqx@172.17.0.5'): ** ERROR ** (core dumped to file: "/opt/emqx/MnesiaCore.emqx@172.17.0.5_1727_975625_63176"), ** FATAL ** Failed to merge schema: {aborted,function_clause}`

  > **Workaround:**
  > Delete the `data/mnesia` directory and restart the node.

  <!-- https://emqx.atlassian.net/browse/EMQX-12290 -->

- **Kafka Disk Buffer Directory Name (since 5.8.0)**

  The introduction of a dynamic topic template for Kafka (Azure EventHubs, Confluent Platform) producer integration imposed an incompatible change for the on-disk buffer directory name.
  If `disk` mode buffer is used, please wait for the 5.8.2 release to avoid buffered messages getting lost after upgrade from an older version.
  If `hybrid` mode buffer is used, you will need to manually clean up the old directories after upgrading from an older version.

  <!-- https://emqx.atlassian.net/browse/EMQX-13248 -->

- **Kafka Disk Buffer Resume (since 5.8.0)**

  If `disk` mode buffer is used, Kafka (Azure EventHubs, Confluent Platform) producers will not automatically start sending data from disk to Kafka after node restart. The sending will be triggered only after there is a new message to trigger the dynamic add of a topic producer.
  This will be fixed in 5.8.2.

  <!-- https://emqx.atlassian.net/browse/EMQX-13242 -->

- **Limitation in SAML-Based SSO (since 5.3)**

  EMQX Dashboard supports Single Sign-On based on the Security Assertion Markup Language (SAML) 2.0 standard and integrates with Okta and OneLogin as identity providers. However, the SAML-based SSO currently does not support a certificate signature verification mechanism and is incompatible with Azure Entra ID due to its complexity.

- **Performance degradation viewing Audit events (since 5.4.0)**

  When Audit log is enabled and specific Audit events are recently logged, in rare cases an attempt to view Audit events in the dashboard may cause a severe performance degradation, or even a crash of the EMQX node in exceptional situations, e.g. when the node is memory-constrained. Events that are known to cause this issue are Backup and Restore API requests, and commands evaluated in the EMQX remote console manipulating particularly large data structures. Nodes may also take longer to start and become responsive in these situations.
  This will be fixed in 5.8.2.

  > **Workaround:**
  > Change the _Max Dashboard Record Size_ setting either through the Dashboard or by setting the `log.audit.max_filter_size` to a particularly low number, eventually the offending events should be cleared from the Audit log once enough new events are logged.

## e5.8.0

- **Node Crash Race Condition (since 5.0, fixed in 5.8.1)**
  If a node shuts down while RPC channels are being established, it may cause the peer node to crash.

- **500 Error Occurs When Deleting an Action with the Same Name as Source (since 5.5.0)**

  In the Dashboard data integration, if the name of an Action item in the Action list matches the Name of a Source item in the Source list, deleting this Action item will return an error code 500.

  Example of error keyword: `{name_clash_action_source, mqtt, <<"test">>}`, where the `mqtt` and `test` are the type and name of the Source item with the same name.

  >**Workaround:** You can execute the following command to delete the Action item, for example:
  >
  >```
  >./bin/emqx eval 'emqx_bridge_v2:remove(mqtt, <<"test">>)'
  >```
  >Then, you need to check the Rule list and delete any rules associated with this action.
