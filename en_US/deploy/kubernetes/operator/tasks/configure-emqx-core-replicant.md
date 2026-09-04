# Enable Core + Replicant Cluster

## Objective

- Configure EMQX cluster Core nodes through the `coreTemplate` field.
- Configure EMQX cluster Replicant nodes through the `replicantTemplate` field.

## Core and Replicant Nodes

Nodes in the EMQX cluster can have one of two roles: Core node and Replicant node.

* Core nodes are responsible for data persistence in the cluster.
  
    They serve as the authoritative source for shared cluster state such as routing tables, MQTT client channels, retained messages, cluster configuration, alarms, Dashboard user credentials, etc.

* Replicant nodes are designed to be stateless and do not participate in database operations.

    Adding or deleting Replicant nodes will not affect the redundancy of the cluster data.

Communication between Core and Replicant nodes in a typical EMQX cluster is illustrated in the following diagram:

  <div style="text-align:center">
  <img src="./assets/configure-core-replicant/mria-core-replicant.png" style="zoom:30%;" />
  </div>

For more information about the EMQX Core-Replicant architecture, refer to the [Cluster Architecture](../../../cluster/mria-introduction.md) documentation.

:::tip
* There must be at least one Core node in the EMQX cluster.
* When Replicant nodes are enabled, EMQX Operator 3.0 requires at least two Core nodes for rolling updates.
* For high availability, running at least three Core nodes is recommended.
:::

## Configure EMQX Cluster

EMQX CRD `apps.emqx.io/v3beta1` supports configuring Core nodes of the EMQX cluster through the `.spec.coreTemplate` field, and configuring Replicant nodes of the EMQX cluster through the `.spec.replicantTemplate` field.

1. Save the following content as a YAML file and deploy using `kubectl apply`.

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
     coreTemplate:
       spec:
         replicas: 2
         resources:
           requests:
             cpu: 250m
             memory: 512Mi
     replicantTemplate:
       spec:
         replicas: 3
         resources:
           requests:
             cpu: 250m
             memory: 1Gi
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   In the example above, the EMQX CR defines an EMQX cluster consisting of two Core nodes and three Replicant nodes.

   Core nodes require a minimum of 512Mi of memory, and Replicant nodes require a minimum of 1Gi of memory. You can adjust these constraints according to the actual business load. Typically, Replicant nodes accept all client requests, so the resources required by Replicant nodes may be higher to accommodate many concurrent connections.

   EMQX Operator exposes the Replicant replica count through the Kubernetes `scale` subresource, which allows HorizontalPodAutoscaler to manage scaling of Replicant set in Core-Replicant mode.

2. Wait for the EMQX cluster to become ready. Check the status of the EMQX cluster with `kubectl get`, ensuring that `STATUS` is `Ready`. This may take some time.

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## Verify EMQX Cluster

You can view information about all nodes in the cluster by checking the `.status` field of the EMQX CR.

```bash
$ kubectl get emqx emqx -o json | jq .status.coreNodes
[
  {
    "name": "emqx@emqx-core-0.emqx-headless.default.svc.cluster.local",
    "podName": "emqx-core-0",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "core",
    "version": "@EE_VERSION@",
    "sessions": 0,
    "connections": 0
  },
  {
    "name": "emqx@emqx-core-1.emqx-headless.default.svc.cluster.local",
    "podName": "emqx-core-1",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "core",
    "version": "@EE_VERSION@",
    "sessions": 0,
    "connections": 0
  }
]
```


```bash
$ kubectl get emqx emqx -o json | jq .status.replicantNodes
[
  {
    "name": "emqx@10.244.4.56",
    "podName": "emqx-replicant-adcdef012-0",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "replicant",
    "version": "@EE_VERSION@",
    "sessions": 42,
    "connections": 42
  },
  {
    "name": "emqx@10.244.4.57",
    "podName": "emqx-replicant-adcdef012-1",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "replicant",
    "version": "@EE_VERSION@",
    "sessions": 11,
    "connections": 11
  },
  {
    "name": "emqx@10.244.4.58",
    "podName": "emqx-replicant-adcdef012-2",
    "status": "running",
    "otpRelease": "27.3.4.2-6/15.2.7.1",
    "role": "replicant",
    "version": "@EE_VERSION@",
    "sessions": 13,
    "connections": 13
  }
]
```
