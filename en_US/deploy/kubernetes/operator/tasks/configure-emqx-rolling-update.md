# Perform a Rolling Update of an EMQX Cluster

Use EMQX Operator 3.0 to perform a graceful rolling update of an EMQX cluster.

## Before You Begin

- [Install EMQX Operator 3.0](../getting-started.md) and configure `kubectl` to access the Kubernetes cluster.
- Make sure that the Kubernetes environment can provision an external address for a `LoadBalancer` Service.
- Prepare a valid EMQX Enterprise license that supports at least 3,000 concurrent connections. Replace the license placeholder in this example with your license key. For more information, see [Manage License](./configure-emqx-license.md).
- Install `jq` to inspect the node evacuation status.
- Install [MQTTX CLI](https://mqttx.app/cli) to generate test connections.
- For a Core-Replicant cluster, configure at least two Core replicas. EMQX Operator rejects a Core-Replicant configuration with fewer than two Core replicas.

## How Rolling Updates Work

EMQX Operator performs a rolling update when a change to an EMQX custom resource modifies a Pod template, such as the image, image pull policy, resource requests, or Core and Replicant templates.

Node evacuation is enabled by default to drain MQTT connections and sessions before Pods are removed. To disable node evacuation, set `.spec.updateStrategy.evacuationStrategy.type` to `Disabled`.

For Core nodes, the Operator updates Pods one at a time within the same StatefulSet. The Operator drains the selected Core Pod if node evacuation is enabled, recreates the Pod with the desired template, and waits until it is ready before updating the next Core Pod.

For Replicant nodes, the Operator uses a Deployment-style rollout. It creates updated Replicant Pods up to the `maxSurge` limit and drains outdated Replicant Pods up to the `maxUnavailable` limit. These settings control how quickly the update proceeds while keeping the number of serving nodes within the configured bounds.

In Core-Replicant clusters, at least one updated Core node must be ready before the Replicant rollout starts, and at least one old Core node is kept until Replicant Pods have migrated away from the old revision.

The following procedure changes annotations in the Core and Replicant Pod templates to demonstrate the rolling update mechanism without changing the EMQX version. To upgrade EMQX, update `.spec.image` to a supported target version. The Operator uses the same rolling update mechanism for the image change.

## Configure the Update Strategy

1. Create an `apps.emqx.io/v3beta1` EMQX CR and configure the update strategy.

  ```yaml
  apiVersion: apps.emqx.io/v3beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx:@EE_VERSION@
    config:
      roots:
        license:
          key: "..."
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
    listenersServiceTemplate:
      spec:
        type: LoadBalancer
  ```

  `maxUnavailable` and `maxSurge` cannot both be `0`. If `maxUnavailable` is `100%`, `maxSurge` must be greater than `0`. For field definitions, defaults, and validation rules, see [ReplicantsUpdateStrategy](../reference/v3beta1-reference.md#replicantsupdatestrategy).

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

## Generate Test Connections

Use MQTTX CLI to generate MQTT connections for observing connection evacuation during the rolling update. MQTTX CLI supports automatic reconnection.

Get the external address of the `emqx-listeners` Service. The command supports load balancers that publish either an IP address or a hostname.

```bash
export EMQX_HOST="$(kubectl get service emqx-listeners -o jsonpath='{.status.loadBalancer.ingress[0].ip}{.status.loadBalancer.ingress[0].hostname}')"
```

After `EMQX_HOST` contains an address, use MQTTX to open 3,000 connections to the default TCP listener on port `1883`:

```bash
mqttx bench conn -h "${EMQX_HOST}" -p 1883 -c 3000
[10:05:21 AM] › ℹ  Start the connect benchmarking, connections: 3000, req interval: 10ms
✔  success   [3000/3000] - Connected
[10:06:13 AM] › ℹ  Done, total time: 31.113s
```

## Trigger the Update

1. Update an annotation in the Core and Replicant Pod templates to trigger a rolling update. The command combines a timestamp with a random suffix to reduce the chance of reusing the same annotation value.

  ```bash
  ROLLOUT_ID="$(date +%s)-${RANDOM}"

  kubectl patch emqx emqx --type=merge -p \
    "{\"spec\":{\"coreTemplate\":{\"metadata\":{\"annotations\":{\"docs.emqx.com/rollout-id\":\"${ROLLOUT_ID}\"}}},\"replicantTemplate\":{\"metadata\":{\"annotations\":{\"docs.emqx.com/rollout-id\":\"${ROLLOUT_ID}\"}}}}}"
  ```

  Expected output:

  ```text
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

  Estimate node evacuation progress by checking the `connections` and `sessions` counters in the corresponding [EMQX node status](../reference/v3beta1-reference.md#emqxnode).

3. Wait for the update to complete.

  ```bash
  $ kubectl get emqx
  NAME      STATUS   AGE
  emqx      Ready    8m33s
  ```

  Make sure that the `STATUS` is `Ready`. Depending on the number of MQTT clients and sessions, the update process may take a while.

  After the update is completed, you can verify that all Pods are running the desired template using `kubectl get pods`.

## Rolling Update Illustration

The following diagram illustrates a possible connection distribution during a Replicant rolling update with 3,000 connections. It is not based on measured Grafana data.

<svg viewBox="0 0 920 360" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Illustrative connection distribution during a Replicant rolling update">
  <rect width="920" height="360" fill="#111827"/>
  <rect x="58" y="34" width="680" height="266" fill="#121a24" stroke="#263241"/>
  <g stroke="#263241" stroke-width="1">
    <path d="M58 60H738M58 108H738M58 156H738M58 204H738M58 252H738M58 300H738"/>
    <path d="M80 34V300M160 34V300M260 34V300M360 34V300M460 34V300M560 34V300M660 34V300M738 34V300"/>
  </g>
  <g fill="#9ca3af" font-family="sans-serif" font-size="12">
    <text x="25" y="304">0</text>
    <text x="16" y="256">600</text>
    <text x="10" y="208">1,200</text>
    <text x="10" y="160">1,800</text>
    <text x="10" y="112">2,400</text>
    <text x="10" y="64">3,000</text>
    <text x="350" y="322">Update progress</text>
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
    <circle cx="768" cy="66" r="3" fill="#73bf69"/><text x="776" y="69" fill="#d1d5db">Total 3,000</text>
    <circle cx="768" cy="88" r="3" fill="#e24d42"/><text x="776" y="91" fill="#d1d5db">emqx-replicant-86f864f9-0</text>
    <circle cx="768" cy="110" r="3" fill="#8f7ee7"/><text x="776" y="113" fill="#d1d5db">emqx-replicant-86f864f9-1</text>
    <circle cx="768" cy="132" r="3" fill="#5794f2"/><text x="776" y="135" fill="#d1d5db">emqx-replicant-86f864f9-2</text>
    <circle cx="768" cy="154" r="3" fill="#f2cc0c"/><text x="776" y="157" fill="#d1d5db">emqx-replicant-648c45c7-0</text>
    <circle cx="768" cy="176" r="3" fill="#ff9830"/><text x="776" y="179" fill="#d1d5db">emqx-replicant-648c45c7-1</text>
    <circle cx="768" cy="198" r="3" fill="#56a64b"/><text x="776" y="201" fill="#d1d5db">emqx-replicant-648c45c7-2</text>
  </g>
  <g fill="#d1d5db" font-family="sans-serif">
    <text x="58" y="24" font-size="14">Illustrative Replicant rolling update, maxSurge = 1, maxUnavailable = 1</text>
  </g>
</svg>

| Label/Prefix         | Description                                         |
|----------------------|-----------------------------------------------------|
| Total                | Example total connection count, shown as the top line in the diagram. |
| `emqx-replicant-86f864f9`    | Example name prefix for the set of outdated Replicant Pods. |
| `emqx-replicant-648c45c7`    | Example name prefix for the set of updated Replicant Pods. |

During a Replicant rollout, clients disconnected from an evacuated Pod may reconnect to other serving Pods, including updated Pods. The lines illustrate this redistribution only; they are not measured values. Actual connection counts can fluctuate based on the node evacuation rate, available cluster capacity, and client reconnection behavior.
