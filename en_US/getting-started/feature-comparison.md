# Feature Comparison

This page lists features supported across different deployment types in detail.

## Core / Enterprise Features

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Features</th>
    <th colspan="2">Self-Hosted</th>
    <th colspan="2">MQTT as a Service</th>
    <th rowspan="2">Notes and Links</th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 5.0 Broker</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Complete MQTT 5.0 protocol implementation</td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Globally Poineering Support<br>In development for Cloud</td>
  </tr>
  <tr>
    <td><b>MQTT Add-ons</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-shared-subscription.html">Shared subscription</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-exclusive-subscription.html">Exclusive subscription</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-delayed-publish.html">Delayed publish</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-auto-subscription.html">Auto-subscription</a><br><a href="https://docs.emqx.com/en/emqx/latest/messaging/mqtt-topic-rewrite.html">Topic rewrite</a><br>More customization options</td>
  </tr>
  <tr>
    <td><b>Multi-protocol Gateways</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> More industry device access</td>
  </tr>
  <tr>
    <td><b>Multi-tenancy</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Higer system flexibility and utilization<br>(Coming soon)</td>
  </tr>
  <tr>
    <td><b>Cluster Linking</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Seamless connection of devices and application data<br>(Coming soon)</td>
  </tr>
  <tr>
    <td><b>Event History</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Client fault diagnosis and behavioral audit</td>
  </tr>
  <tr>
    <td><b>Message Queuing</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Unified architecture for data transmission and analysis<br>(Coming soon)</td>
  </tr>
  <tr>
    <td><b>Stream Processing</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Higher reliability and disaster recovery capabilities<br>(Coming soon)</td>
  </tr>
  <tr>
    <td><b>Data Persistence</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> Built-in RocksDB backend or external databases</td>
    <td>N/A</td>
    <td>N/A</td>
    <td> <a href="https://docs.emqx.com/en/emqx/latest/durability/durability_introduction.html">Improved stability and reliability</a></td>
  </tr>
  <tr>
    <td><b>Schema Registry</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/schema-registry.html">Schema Registry</a> ensures data consistency and compatibility</td>
  </tr>
  <tr>
    <td><b>Message Codec</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>Flexible message format conversion for:<br>JSON<br>Avro<br>Protobuf<br>Custom codec (HTTP/gRPC)</td>
  </tr>
<tr>
    <td><b>Schema Validation</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td> Ensuring integrity and legality of messages</td>
  </tr>
  <tr>
    <td><b>Rule Engine</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="href="https://docs.emqx.com/en/emqx/latest/data-integration/rules.html">SQL-based built-in Rule Engine and real-time data processing</a></td>
  </tr>
<tr>
    <td><b>Flow Designer</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/flow-designer/introduction.html">Easy orchestration of data integration</a></td>
  </tr>
  <tr>
    <td><b>File Transfer</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td>Unified platform data transmission</td>
  </tr>
  <tr>
    <td><b>Kafka Integration</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/data-bridge-kafka.html">Stream MQTT Data into Apache Kafka</a></td>
  </tr>
  <tr>
    <td><b>Enterprise Data Integrations</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 40+</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" />40+</td>
    <td><a href="https://www.emqx.com/en/integrations">Accelerate business development and delivery speed</a></td>
  </tr>
  <tr>
    <td><b>Troubleshooting</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/observability/tracer.html">Log Trace</a><br><a href="https://docs.emqx.com/en/emqx/latest/observability/slow-subscribers-statistics.html">Slow Subscriptions</a></td>
  </tr>
  <tr>
    <td><b>Cloud-Native &amp; K8s</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/en/deployments">Reduce system deployment and management costs</a></td>
  </tr>
  <tr>
    <td><b>Edge Computing</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Reduce data transmission latency and costs<br><a href="https://www.emqx.com/en/products/neuronex">Neuron</a><br><a href="https://www.emqx.com/en/products/nanomq">NanoMQ</a></td>
  </tr>
</tbody>
</table>
</div>




## Scalability and Performance

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Scalability/Performance</th>
    <th colspan="2">Self-Hosted</th>
    <th colspan="2">MQTT as a Service</th>
    <th rowspan="2">Notes and Links</th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Scalability</b></td>
    <td><span style="font-weight:normal">Up to 3 nodes cluster<br>Up to 100,000 connections in production</span></td>
    <td><span style="font-weight:normal">Up to 100 nodes cluster<br>Up to 100 million MQTT connections per cluster</span></td>
    <td><span style="font-weight:normal">1000 auto scale</span></td>
    <td><span style="font-weight:normal">1000 - unlimited</span></td>
    <td><a href="https://www.emqx.com/en/blog/reaching-100m-mqtt-connections-with-emqx-5-0">Reaching 100M MQTT connections with EMQX 5.0</a></td>
  </tr>
  <tr>
    <td><b>Availability</b></td>
    <td><span style="font-weight:normal">Masterless cluster</span></td>
    <td><span style="font-weight:normal">Core-Replica cluster</span></td>
    <td><span style="font-weight:normal">Masterless cluster</span></td>
    <td><span style="font-weight:normal">Masterless cluster</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>Reliability</b></td>
    <td><span style="font-weight:normal">Session persistence</span></td>
    <td><span style="font-weight:normal">Data persistence in RocksDB with highly available replication</span></td>
    <td><span style="font-weight:normal">Session persistence</span></td>
    <td><span style="font-weight:normal">Session persistence</span></td>
    <td><a href="https://www.emqx.com/en/blog/mqtt-persistence-based-on-rocksdb">Highly Reliable MQTT Data Persistence Based on RocksDB</a></td>
  </tr>
  <tr>
    <td><b>Performance</b></td>
    <td><span style="font-weight:normal">100,000 MQTT messages per second</span></td>
    <td><span style="font-weight:normal">5M+ MQTT messages per second</span></td>
    <td><span style="font-weight:normal">1000 MQTT messages per second</span></td>
    <td><span style="font-weight:normal">5M+ MQTT messages per second</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>Latency</b></td>
    <td><span style="font-weight:normal">1~5 millisecond</span></td>
    <td><span style="font-weight:normal">1~5 millisecond</span></td>
    <td><span style="font-weight:normal">1~5 millisecond</span></td>
    <td><span style="font-weight:normal">1~5 millisecond</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>SLA</b></td>
    <td><span style="font-weight:normal">N/A</span></td>
    <td><span style="font-weight:normal">N/A</span></td>
    <td><span style="font-weight:normal">99.9% uptime</span></td>
    <td><span style="font-weight:normal">Up to 99.99%</span><br><span style="font-weight:normal">uptime</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
</tbody>
</table>
</div>


## Clustering Architecture

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Clustering Architecture<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Nodes of Cluster</b></td>
    <td>No more than 3</td>
    <td>100+</td>
    <td>confidential</td>
    <td>confidential</td>
    <td> Large-scale clusters</td>
  </tr>
  <tr>
    <td><b>Elastic and resilient scaling at runtime</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Higer system stability and resource utilization</td>
  </tr>
  <tr>
    <td><b>Autoscaling</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Consistency</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>Transaction</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Ensuring the atomicity and reliability of data operations</td>
  </tr>
  <tr>
    <td><b>Network Split Recovery</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Cluster failure automatic repair</td>
  </tr>
  <tr>
    <td><b>Node Evacuation &amp; Cluster Rebalance</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> Non-stop cluster maintenance</td>
  </tr>
  <tr>
    <td><b>Autocluster Discoveries</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td>static: Discovery based on a static node list<br>mcast: Discovery with UDP multicast mode<br>dns: Discovery based on DNS records<br>etcd: Discovery via etcd<br>k8s: Discovery via Kubernetes service</td>
  </tr>
  <tr>
    <td><b>Zero Downtime/Hot Upgrades</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> Immediate repair of system vulnerabilities</td>
  </tr>
  <tr>
    <td><b>Hot Patch</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> Ensuring stable system operation</td>
  </tr>
  <tr>
    <td><b>Overload Protection</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> Improving system management efficiency</td>
  </tr>
  <tr>
    <td><b>Multi-cluster Management</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> Enhancing system stability</td>
  </tr>
  <tr>
    <td><b>Cluster Metrics</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>



## MQTT and Connectivity

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTT &amp; Connectivity<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 3.x</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT 5.0</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT Retainer</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TLS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over WebSocket</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX is now the only MQTT broker in the world that supports QUIC transport.</td>
  </tr>
  <tr>
    <td><b>LB (Proxy Protocol)</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>Proxy protocol v1, v2</td>
  </tr>
  <tr>
    <td><b>LB (Custom)</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>GmSSL<br>Smooth connection migration</td>
  </tr>
  <tr>
    <td><b>IPv6 Support</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Multi-protocol Gateway</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT-SN</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>STOMP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CoAP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>LwM2M</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ExProto</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>OCPP</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JT/808</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>GBT32960</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>


## Security

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Security<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>TLS/SSL</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Protect data transmission security: TLS 1.1, 1.2, 1.3</td>
  </tr>
   <tr>
    <td><b>QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Enhance efficiency of weak network and mobile network data transmission</td>
  </tr>
  <tr>
    <td><b>OCSP Stapling</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Provide more flexible security practices</td>
  </tr>
  <tr>
    <td><b>Flapping Detect</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td> Detect and intercept frequent online and offline connections</td>
  </tr>
  <tr>
    <td><b>Audit Logs</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Support audit tracing for important operations</td>
  </tr>
  <tr>
    <td><b>Dashboard SSO</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Secure and simplified authentication processes</td>
  </tr>
  <tr>
    <td><b>Dashboard/REST API RBAC</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Minimize permissions to ensure system security</td>
  </tr>
  <tr>
    <td><b>Black Duck Analysis</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> Secure and simplified authentication processes</td>
  </tr>
</tbody>
</table>
</div>



## Authentication and Authorization

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Authentication/Authorization<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Username/Password</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/access-control/authn/pwoverview.html">Password-Based Authentication</a></td>
  </tr>
  <tr>
    <td><b>JWT</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.html">JWT Authentication</a></td>
  </tr>
  <tr>
    <td><b>MQTT 5.0 Enhanced Authentication</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/access-control/authn/scram.html">MQTT 5.0 Enhanced Authentication</a></td>
  </tr>
   <tr>
    <td><b>LDAP Authentication</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>PSK Authentication</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/network/psk-authentication.html#enable-psk-authentication">Enable PSK Authentication</a></td>
  </tr>
  <tr>
    <td><b>X.509 Certificates</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> Managed by EMQX Cloud</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Fine-grained Access Control</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Authentication Database Backends</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ACL Database Backends</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>




## Data Integration

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Data Integration<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT Bridge</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Webhook/HTTP Server</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Kafka/Confluent</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache IoTDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Pulsar</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS Kinesis</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS S3</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Azure Event Hubs</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Cassandra</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>ClickHouse</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>DynamoDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Elasticsearch</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GCP PubSub</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GreptimeDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>HStreamDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>InfluxDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Microsoft SQL Server</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MongoDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MySQL</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>OpenTSDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Oracle Database</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>PostgreSQL</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>RabbitMQ</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Redis</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>RocketMQ</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Syskeeper</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>TDengine</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>TimeScaleDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
</tbody>
</table>
</div>



## Rule Engine

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Rule Engine<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Schema Registry</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Ensure data format consistency</td>
  </tr>
  <tr>
    <td><b>JSON Codec</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Avro Codec</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Protobuf Codec</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Sparkplug B Codec</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JSON Schema Validation</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Avro Validation</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ProtoBuf Validation</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Built-in Functions</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/rule-sql-builtin-functions.html">Functions available in SQL statements, rich built-in libraries, support custom extensions</a></td>
  </tr>
  <tr>
    <td><b>jq Functions</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> Efficient JSON data processing</td>
  </tr>
  <tr>
    <td><b>Event Trigger</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/data-integration/rule-sql-events-and-fields.html#mqtt-events">Client events</a>, event-driven business development</td>
  </tr>
</tbody>
</table>
</div>




## Extensibility

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Extensibility<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Hooks</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/extensions/hooks.html#hooks">Hooks</a></td>
  </tr>
  <tr>
    <td><b>Plugins</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/en/emqx/latest/extensions/plugins.html#plugins">Plugins</a></td>
  </tr>
  <tr>
    <td><b>Plugin Hot Loading</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Plugin Hot Configuration</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Gateways</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ExHooks/gRPC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td></td>
  </tr>
</tbody>
</table>
</div>



## Operability

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Operability<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Dashboard</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX dashboard is feature-rich.<br>Configs can be hot udpated through dashboard.</td>
  </tr>
  <tr>
    <td><b>Configuration</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> HOCON</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> HOCON</td>
    <td>N/A</td>
    <td>N/A</td>
    <td>The HOCON format is simple and concise.</td>
  </tr>
  <tr>
    <td><b>HTTP API</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CLI</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Config Hot Updates</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Operational Auditing</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>

## Observability

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Observability<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Dashboard</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>Monitor clusters in real time with an elegant dashboard</td>
  </tr>
  <tr>
    <td><b>Metrics</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>Node metrics</td>
  </tr>
  <tr>
    <td><b>Grafana</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Prometheus</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Datadog</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td></td>
  </tr>
  <tr>
    <td><b>OpenTelemetry</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td></td>
  </tr>
  <tr>
    <td><b>Cluster Metrics</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Alarm Alerts</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Slow Subscription Monitoring</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Topic Monitoring</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> In product roadmap</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Client Monitoring</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Log Trace</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>


## Cloud Native and K8S

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Cloud Native &amp; K8s<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Docker</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://hub.docker.com/_/emqx">emqx - Official Image | Docker Hub</a><a href="https://hub.docker.com/r/emqx/emqx">Docker</a></td>
  </tr>
  <tr>
    <td><b>Kubernetes Operator</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/en/emqx-kubernetes-operator">EMQX Kubernetes Operator</a></td>
  </tr>
  <tr>
    <td><b>Terraform</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/en/emqx-terraform">EMQX Terraform</a></td>
  </tr>
</tbody>
</table>
</div>


## Cloud Platform Availability

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Cloud Platform<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>AWS Marketplace</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX is available on AWS Marketplace.<br><a href="https://aws.amazon.com/marketplace/pp/prodview-cwa2e6xbrwtzi">AWS Marketplace: EMQX Enterprise on Ubuntu 20.04</a> </td>
  </tr>
  <tr>
    <td><b>Azure Marketplace</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GCP Marketplace</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>AWS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>Azure</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>GCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
</tbody>
</table>
</div>


## MQTT Tools and SDKs

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTT Tools &amp; SDKs<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT Desktop Client</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>MQTT X - The best tool for learning MQTT.<br><a href="https://mqttx.app/">MQTTX: Your All-in-one MQTT Client Toolbox</a></td>
  </tr>
  <tr>
    <td><b>MQTT CLI</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://mqttx.app/cli">MQTTX CLI: A Powerful and Easy-to-use MQTT CLI Tool</a></td>
  </tr>
  <tr>
    <td><b>MQTT Web Tool</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>Feature-rich and Easy-to-use.<br><a href="https://mqttx.app/web">MQTTX Web: Easy-to-use MQTT Websocket Client Tool</a></td>
  </tr>
  <tr>
    <td><b>MQTT Benchmark</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/emqtt-bench">GitHub - emqx/emqtt-bench: Lightweight MQTT benchmark tool written in Erlang</a></td>
  </tr>
  <tr>
    <td><b>MQTT Load Testing</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> XMeter</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>XMeter - The world’s #1 MQTT load testing tool</td>
  </tr>
  <tr>
    <td><b>MQTT &amp; JMeter</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> JMeter Plugin</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> XMeter</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/mqtt-jmeter">GitHub - emqx/mqtt-jmeter: MQTT JMeter Plugin</a></td>
  </tr>
  <tr>
    <td><b>MQTT SDK for C</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> NanoSDK</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> NanoSDK</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/nanomq/NanoSDK">GitHub - nanomq/NanoSDK: NanoSDK - MQTT 5.0-compliant SDK with QUIC support in NNG flavor</a></td>
  </tr>
  <tr>
    <td><b>MQTT Erlang SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/emqtt">GitHub - emqx/emqtt: Erlang MQTT 5.0 Client</a></td>
  </tr>
  <tr>
    <td><b>MQTT iOS SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/CocoaMQTT">GitHub - emqx/CocoaMQTT: MQTT 5.0 client library for iOS and macOS written in Swift</a></td>
  </tr>
  <tr>
    <td><b>MQTT QUIC Client</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/quic">GitHub - emqx/quic: QUIC protocol for Erlang &amp; Elixir</a></td>
  </tr>
</tbody>
</table>
</div>


## Support Services

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">Support Services<br></th>
    <th colspan="2">Self-Hosted<br></th>
    <th colspan="2">MQTT as a Service<br></th>
    <th rowspan="2">Notes and Links<br></th>
  </tr>
  <tr>
    <td>EMQX Open Source</td>
    <td>EMQX Enterprise</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Dedicated Cloud</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Technical Support</b></td>
    <td>Community &amp; Forum</td>
    <td>5*8, 7*24 Global Support</td>
    <td>5*8 Global Support</td>
    <td>5*8, 7*24 Global Support</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Architecture Consulting</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Project Integration</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Custom Development</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>