# Known Issues in EMQX 5.8

## e5.8.1

- **Node Cannot Start if a New Node Joined Cluster While It was Stopped (since 5.0)**

  In a cluster of 2 or more nodes, if a new node joins the cluster while some nodes are down, the down nodes will fail to restart and emit logs like below.
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

## e5.8.0

- **Node Crash Race Condition (since 5.0, fixed in 5.8.1)**
  If a node shutsdown while RPC channels are being established, it may cause the peer node to crash.
