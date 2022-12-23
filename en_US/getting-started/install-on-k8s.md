# Use EMQX Operator to deploy EMQX cluster

The corresponding relationship between EMQX Operator and the applicable EMQX version is as follows:

|         EMQX Version   |                  EMQX Operator Version        |  APIVersion |
|:----------------------:|:---------------------------------------------:|:-----------:|
|   4.4.6<= EMQX < 4.4.8 |   1.2.5                                       |  v1beta3    |
|   4.4.8 <= EMQX        | 1.2.6，1.2.7，1.2.8，2.0.0，2.0.1，2.0.2（推荐） |  v1beta3    |


For the latest information about EMQX Operator, please visit [EMQX Operator](https://github.com/emqx/emqx-operator/blob/main/docs/en_US/getting-started/getting-started.md). **Note**: EMQX Operator requires Kubernetes v1.20.11 or above.

## Deploy EMQX Operator

### Prepare

We use [cert-manager](https://github.com/cert-manager/cert-manager) to provide certificates to EMQX Operator webhook service. For the installation tutorial of cert-manager, please refer to the document: [cert-manager document](https://cert-manager.io/docs/installation/).

### Install 

1. Install EMQX Operator

- Add EMQX helm chart repository

```bash
helm repo add emqx https://repos.emqx.io/charts
helm repo update emqx
```

- Install EMQX Operator

```bash
helm install emqx-operator emqx/emqx-operator --namespace emqx-operator-system --create-namespace --version=2.0.2
```

**NOTE**: `--version` is used to install the specified version of EMQX Operator, if no version is specified, the latest version will be installed by default. Please install the appropriate EMQX Operator version according to the corresponding relationship table between EMQX Operator and applicable EMQX version.

2. Check whether EMQX Operator is ready

```bash
kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system
```

The output is similar to:

```
NAME                                                READY   STATUS    RESTARTS   AGE
emqx-operator-controller-manager-68b866c8bf-kd4g6   1/1     Running   0          15s
```

**NOTE**: The `NAME` field is the name of the EMQX Operator Pod, and the `READY` field indicates the number of ready Containers in the Pod. The `STATUS` field is the status of the Pod.

{% emqxce %}

## Deploy EMQX Broker

1. Deploy EMQX custom resources

```bash
cat << "EOF" | kubectl apply -f -
   apiVersion: apps.emqx.io/v1beta3
   kind: EmqxBroker
   metadata:
     name: emqx
   spec:
     emqxTemplate:
       image: emqx/emqx:4.4.9
EOF
```

2. Check whether the EMQX cluster is ready

```bash
kubectl get emqxbroker emqx -o json | jq ".status.conditions"
```

The output is similar to:

```bash
[
   {
     "lastTransitionTime": "2022-12-21T11:59:47Z",
     "lastUpdateTime": "2022-12-21T12:00:28Z",
     "message": "All resources are ready",
     "reason": "ClusterReady",
     "status": "True",
     "type": "Running"
   },
   {
     "lastTransitionTime": "2022-12-21T11:59:34Z",
     "lastUpdateTime": "2022-12-21T11:59:34Z",
     "message": "All default plugins initialized",
     "reason": "PluginInitializeSuccessfully",
     "status": "True",
     "type": "PluginInitialized"
   }
]
```
**NOTE**: From the output, we can see that the EMQX cluster is ready.

{% endemqxce %}

{% emqxee %}

## Deploy EMQX Enterprise

1. Deploy EMQX custom resources

```bash
cat << "EOF" | kubectl apply -f -
   apiVersion: apps.emqx.io/v1beta3
   kind: EmqxEnterprise
   metadata:
     name: emqx-ee
   spec:
     emqxTemplate:
       image: emqx/emqx-ee:4.4.9
EOF
```

2. Check whether the EMQX cluster is ready

```bash
kubectl get emqxenterprise emqx-ee -o json | jq ".status.conditions"
```

The output is similar to:

```bash
[
   {
     "lastTransitionTime": "2022-12-21T12:03:24Z",
     "lastUpdateTime": "2022-12-21T12:03:24Z",
     "message": "All resources are ready",
     "reason": "ClusterReady",
     "status": "True",
     "type": "Running"
   },
   {
     "lastTransitionTime": "2022-12-21T12:03:08Z",
     "lastUpdateTime": "2022-12-21T12:03:08Z",
     "message": "All default plugins initialized",
     "reason": "PluginInitializeSuccessfully",
     "status": "True",
     "type": "PluginInitialized"
   }
]
```

**NOTE**: From the output, we can see that the EMQX cluster is ready.

{% endemqxee %}