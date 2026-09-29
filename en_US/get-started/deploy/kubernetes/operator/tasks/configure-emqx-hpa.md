# Autoscale Replicant Nodes with HPA

Use Kubernetes HorizontalPodAutoscaler (HPA) to automatically scale EMQX Replicant nodes. EMQX Operator 3.0 exposes the Kubernetes `scale` subresource for `apps.emqx.io/v3beta1` EMQX resources. This subresource targets `.spec.replicantTemplate.spec.replicas`, so HPA scales Replicant nodes in a Core-Replicant cluster.

## Before You Begin

Ensure that the Kubernetes resource metrics API is available. The CPU utilization example on this page requires [Metrics Server](https://kubernetes-sigs.github.io/metrics-server/) or another metrics adapter that provides `metrics.k8s.io` data.

Run the following command to verify that Pod metrics are available:

```bash
kubectl top pods
```

If the command does not return CPU and memory usage, install and configure a metrics provider before creating the HPA.

When Replicant nodes are enabled, configure at least two Core replicas. To let HPA analyze utilization and make scaling decisions, pod resource requests also need to be configured.

## Deploy EMQX

1. Save the following content as `emqx.yaml`:

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
     replicantTemplate:
       spec:
         replicas: 4
         resources:
           requests:
             cpu: 500m
             memory: 1Gi
   ```

2. Deploy the EMQX cluster:

   ```bash
   kubectl apply -f emqx.yaml
   ```

3. Wait until the EMQX cluster is ready:

   ```bash
   kubectl wait --for=condition=Ready emqx/emqx
   ```

## Create an HPA

1. Save the following content as `emqx-hpa.yaml`. This example uses CPU utilization to scale Replicant nodes between 2 and 10 replicas:

   ```yaml
   apiVersion: autoscaling/v2
   kind: HorizontalPodAutoscaler
   metadata:
     name: emqx-replicants
   spec:
     scaleTargetRef:
       apiVersion: apps.emqx.io/v3beta1
       kind: EMQX
       name: emqx
     minReplicas: 2
     maxReplicas: 10
     metrics:
       - type: Resource
         resource:
           name: cpu
           target:
             type: Utilization
             averageUtilization: 70
     behavior:
       scaleDown:
         stabilizationWindowSeconds: 300
   ```

2. Apply the HPA:

   ```bash
   kubectl apply -f emqx-hpa.yaml
   ```

## Verify Autoscaling

1. Check the HPA and EMQX status:

   ```bash
   kubectl get hpa emqx-replicants
   kubectl get emqx emqx -o custom-columns='DESIRED:.spec.replicantTemplate.spec.replicas,CURRENT:.status.replicantReplicas'
   ```

2. With no MQTT client workload, monitor the HPA. If the observed CPU utilization is low enough for the HPA to calculate a desired replica count of two, verify that it updates `.spec.replicantTemplate.spec.replicas` to two. The configured scale-down stabilization window can delay this update. EMQX Operator then reconciles the Replicant set to that replica count.

Scaling down Replicant nodes can evacuate MQTT connections and sessions and therefore adds load to the cluster. Use HPA stabilization windows and conservative scaling policies to avoid frequent replica-count changes under a fluctuating workload.
