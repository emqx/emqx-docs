# Perform Rolling Update of EMQX Cluster

## Objective

Perform a graceful rolling update of the EMQX cluster.

## Background

EMQX Operator performs rolling updates when fields in the EMQX Pod template change, such as the image, image pull policy, resource requests, or node templates.

During a rolling update, Core nodes are updated in place through a single StatefulSet, one Pod at a time. Replicant nodes use a Deployment-style rollout controlled by `maxUnavailable` and `maxSurge`. Node evacuation is used by default to drain MQTT connections and sessions before Pods are removed. It can be disabled with `.spec.updateStrategy.evacuationStrategy.type: Disabled`.

## Solution

When an EMQX CR is updated, EMQX Operator compares the desired Pod template with the running workloads and rolls the cluster forward until every managed Pod matches the new template.

For Core nodes, the Operator updates the StatefulSet template, drains the selected Core Pod if evacuation is enabled, recreates that Pod with the new template, and waits until it is ready before moving to the next Core Pod. For Replicant nodes, the Operator creates updated Replicant Pods up to the `maxSurge` limit and drains old Replicant Pods up to the `maxUnavailable` limit. This controls how quickly the update proceeds while keeping the number of serving nodes within the configured bounds.

In Core-Replicant clusters, at least one updated Core node must be ready before the Replicant rollout starts, and at least one old Core node is kept until Replicant Pods have migrated away from the old revision.

The update process is roughly divided into the following steps:

1. Detect a change in the EMQX Pod template.
2. Update Core nodes one StatefulSet Pod at a time until at least one updated Core node is ready.
3. Roll out Replicant nodes by creating updated Replicant Pods according to `maxSurge` and draining old Replicant Pods according to `maxUnavailable`.
4. Keep at least one old Core node while old Replicant Pods are still migrating.
5. Use node evacuation, unless disabled, to migrate MQTT connections and sessions at a controlled rate.
6. Complete the update when all desired Pods are ready and old Replicant Pods have been removed.

## Procedure

### Configure the Update Strategy

1. Create an `apps.emqx.io/v3beta1` EMQX CR and configure the update strategy.

  ```yaml
  apiVersion: apps.emqx.io/v3beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx:@EE_VERSION@
    config:
      data: |
        license {
          key = "..."
        }
    updateStrategy:
      type: RollingUpdate
      evacuationStrategy:
        # MQTT client evacuation rate, connections per second:
        connectionEvictionRate: 1000
        # MQTT Session evacuation rate, sessions per second:
        sessionEvictionRate: 1000
        # Time to wait before deleting a Pod:
        waitTakeover: 10
      replicants:
        maxUnavailable: 1
        maxSurge: 1
    coreTemplate:
      spec:
        replicas: 2
    replicantTemplate:
      spec:
        replicas: 3
  ```

2. Save the above content as `emqx-update.yaml` and deploy it using `kubectl apply`:

  ```bash
  $ kubectl apply -f emqx-update.yaml
  emqx.apps.emqx.io/emqx created
  ```

3. Check the status of the EMQX cluster.

  Make sure that `STATUS` is `Ready`. This may take a while.

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

### Connect to EMQX Cluster

[MQTTX](https://mqttx.app/cli) is an open-source MQTT 5.0 compatible command line client tool that supports automatic reconnection, designed to help in development and debugging of MQTT services and applications.

Use MQTTX to connect to the EMQX cluster:

```bash
mqttx bench conn -h ${IP} -p ${PORT} -c 3000
[10:05:21 AM] › ℹ  Start the connect benchmarking, connections: 3000, req interval: 10ms
✔  success   [3000/3000] - Connected
[10:06:13 AM] › ℹ  Done, total time: 31.113s
```

### Trigger the Update

1. Any modifications made to the Pod template will trigger the upgrade strategy of EMQX Operator.

  In this example, we trigger the upgrade by modifying the Pod's `ImagePullPolicy`.

  ```bash
  $ kubectl patch emqx emqx --type=merge -p '{"spec": {"imagePullPolicy": "Never"}}'
  emqx.apps.emqx.io/emqx patched
  ```

2. Check the status of the update process.

  ```bash
  $ kubectl get emqx emqx -o json | jq ".status.nodeEvacuations"
  [
    {
      "nodeName": "emqx@10.244.4.56",
      "initialConnections": 33,
      "initialSessions": 0,
      "connectionEvictionRate": 200,
      "sessionEvictionRate": 200,
      "state": "waiting_takeover",
      "sessionRecipients": [
        "emqx@10.244.4.57",
        "emqx@10.244.4.58"
      ]
    }
  ]
  ```

  | Field                   | Description                                                          |
  |-------------------------|----------------------------------------------------------------------|
  | `nodeName`              | The node currently being evacuated.                                  |
  | `state`                 | Node evacuation phase.                                               |
  | `sessionRecipients`     | MQTT session recipients.                                             |
  | `sessionEvictionRate`   | MQTT session eviction rate on this node (sessions per second).       |
  | `connectionEvictionRate`| MQTT connection eviction rate on this node (connections per second). |
  | `initialSessions`       | Initial number of sessions on this node.                             |
  | `initialConnections`    | Initial number of connections on this node.                          |

  Progress of a node evacuation can be estimated by looking at `connections` and `sessions` counters in the respective [EMQX node status](../reference/v3beta1-reference.md#status).

3. Wait for the update to complete.

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

  Make sure that the `STATUS` is `Ready`. Depending on the number of MQTT clients and sessions, the update process may take a while.

  After the update is completed, you can verify that all Pods are running the desired template using `kubectl get pods`.

## Grafana Monitoring

The following monitoring graph shows the number of connections during the update process, using 10,000 connections as an example.

<svg viewBox="0 0 920 360" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Connection counts during a Replicant rolling update">
  <rect width="920" height="360" fill="#111827"/>
  <rect x="58" y="34" width="680" height="266" fill="#121a24" stroke="#263241"/>
  <g stroke="#263241" stroke-width="1">
    <path d="M58 60H738M58 108H738M58 156H738M58 204H738M58 252H738M58 300H738"/>
    <path d="M80 34V300M160 34V300M260 34V300M360 34V300M460 34V300M560 34V300M660 34V300M738"/>
  </g>
  <g fill="#9ca3af" font-family="sans-serif" font-size="12">
    <text x="25" y="304">0</text>
    <text x="20" y="256">2K</text>
    <text x="20" y="208">4K</text>
    <text x="20" y="160">6K</text>
    <text x="20" y="112">8K</text>
    <text x="14" y="64">10K</text>
    <text x="62" y="322">14:08</text>
    <text x="242" y="322">14:11</text>
    <text x="442" y="322">14:14</text>
    <text x="642" y="322">14:17</text>
  </g>
  <g fill="none" stroke-linecap="round" stroke-linejoin="round">
    <path d="M80 300 C105 300 135 180 160 62 C210 58 270 61 330 60 C390 61 445 59 505 60 C565 62 630 59 700 60" stroke="#73bf69" stroke-width="2"/>
    <path d="M160 217 L300 216 C330 218 355 254 382 300" stroke="#e24d42" stroke-width="1.2"/>
    <path d="M160 221 L400 222 C430 222 455 257 482 300" stroke="#8f7ee7" stroke-width="1.2"/>
    <path d="M160 225 L500 224 C530 225 565 259 600 300" stroke="#5794f2" stroke-width="1.2"/>
    <path d="M280 300 C315 300 350 246 382 218 L400 218 C430 218 455 205 482 200 L500 200 C535 200 570 190 600 180 L700 180" stroke="#f2cc0c" stroke-width="1.2"/>
    <path d="M380 300 C415 300 450 260 482 248 L500 248 C535 248 570 225 600 221 L700 221" stroke="#ff9830" stroke-width="1.2"/>
    <path d="M500 300 C535 300 570 283 600 264 L700 264" stroke="#56a64b" stroke-width="1.2"/>
  </g>
  <g font-family="sans-serif" font-size="9">
    <rect x="758" y="46" width="144" height="166" rx="4" fill="#101923" stroke="#263241"/>
    <circle cx="768" cy="66" r="3" fill="#73bf69"/><text x="776" y="69" fill="#d1d5db">Total 10K</text>
    <circle cx="768" cy="88" r="3" fill="#e24d42"/><text x="776" y="91" fill="#d1d5db">emqx-replicant-86f864f9-0</text>
    <circle cx="768" cy="110" r="3" fill="#8f7ee7"/><text x="776" y="113" fill="#d1d5db">emqx-replicant-86f864f9-1</text>
    <circle cx="768" cy="132" r="3" fill="#5794f2"/><text x="776" y="135" fill="#d1d5db">emqx-replicant-86f864f9-2</text>
    <circle cx="768" cy="154" r="3" fill="#f2cc0c"/><text x="776" y="157" fill="#d1d5db">emqx-replicant-648c45c7-0</text>
    <circle cx="768" cy="176" r="3" fill="#ff9830"/><text x="776" y="179" fill="#d1d5db">emqx-replicant-648c45c7-1</text>
    <circle cx="768" cy="198" r="3" fill="#56a64b"/><text x="776" y="201" fill="#d1d5db">emqx-replicant-648c45c7-2</text>
  </g>
  <g fill="#d1d5db" font-family="sans-serif">
    <text x="58" y="24" font-size="14">Replicant rolling update, maxSurge = 1, maxUnavailable = 1</text>
  </g>
</svg>

| Label/Prefix         | Description                                         |
|----------------------|-----------------------------------------------------|
| Total                | Total number of connections; shown as the top line in the graph. |
| `emqx-replicant-86f864f9`    | Name prefix for the set of old Replicant Pods. |
| `emqx-replicant-648c45c7`    | Name prefix for the set of updated Replicant Pods. |

This timeline illustrates how EMQX Operator performs a smooth rolling update. Throughout the process, the total number of connections remained stable (subject to factors such as migration rate, server capacity, and client reconnection strategy). This approach reduces disruption, prevents server overload, and improves overall service stability.
