# Autoscale Replicant Nodes with HPA

## Objective

Use Kubernetes HorizontalPodAutoscaler (HPA) to scale EMQX Replicant nodes.

## Background

EMQX Operator 3.0.0 exposes the Kubernetes `scale` subresource for `apps.emqx.io/v3beta1` EMQX resources. The scale subresource targets `.spec.replicantTemplate.spec.replicas`, so HPA scales Replicant nodes in a Core-Replicant cluster.

## Deploy EMQX

When Replicant nodes are enabled, configure at least two Core replicas. To let HPA analyze utilization and make scaling decisions, pod resource requests also need to be configured.

Save the following content as `emqx.yaml` and deploy it with `kubectl apply`:

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

Wait until the EMQX cluster is ready:

```bash
kubectl wait --for=condition=Ready emqx/emqx
```

## Create an HPA

The following example uses CPU utilization to scale Replicant nodes between 2 and 10 replicas:

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
```

Save the content as `emqx-hpa.yaml` and apply it:

```bash
kubectl apply -f emqx-hpa.yaml
```

## Verify Autoscaling

Check the HPA and EMQX status:

```bash
kubectl get hpa emqx-replicants
kubectl get emqx emqx -o jsonpath='{.spec.replicantTemplate.spec.replicas}{"\n"}'
kubectl get emqx emqx -o jsonpath='{.status.replicantReplicas}{"\n"}'
```

With no MQTT client workload present, the HPA should eventually update `.spec.replicantTemplate.spec.replicas` to the allowed minimum of 2 replicas, and let EMQX Operator reconcile Replicant set to the desired replica count.
