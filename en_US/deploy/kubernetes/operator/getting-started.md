# Install Operator and Deploy EMQX

This section guides you through preparing the environment for EMQX Operator, installing the Operator itself, and using it to deploy EMQX. By following the steps provided, you can install and manage EMQX efficiently and reliably with the Operator.

## Prepare the Environment

Before deploying EMQX Operator, ensure that the following components are ready:

- A [Kubernetes](https://kubernetes.io/docs/concepts/overview/) environment running Kubernetes version 1.24 or higher.

- A [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl) tool that can access the Kubernetes cluster. You can check the status of the Kubernetes cluster using `kubectl cluster-info` command.

## Install EMQX Operator

1. Install the EMQX Operator with the command below:

   ```bash
   $ kubectl apply --server-side=true -f https://github.com/emqx/emqx-operator/releases/latest/download/install.yaml
   ```

   This command will download latest 2.3.x release, install cluster-wide EMQX CRDs and deploy controller services into a separate `emqx-operator-system` namespace.

2. Wait till EMQX Operator is ready:

   ```bash
   $ kubectl wait --for=condition=Ready pods --namespace emqx-operator-system -l "control-plane=controller-manager"
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Once the Operator is running, you can proceed to deploy EMQX.

## Deploy EMQX

1. Save the following content as a YAML file and deploy it with the `kubectl apply`.

   ```yaml
   apiVersion: apps.emqx.io/v2
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
   ```

   For more details about the EMQX CRD, check out the [reference documentation](./reference/v2beta1-reference.md).

2. Wait until the EMQX cluster is ready.

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   Make sure the `STATUS` is `Ready`. It may take some time for the EMQX cluster to become ready.

## Troubleshooting

EMQX Operator exposes limited number of events to the Kubernetes API.
```sh
kubectl get events --sort-by=.lastTimestamp
```

Alternatively, if EMQX resources fail to reach `Ready` status condition, consult the controller manager logs for more details:
```sh
kubectl logs -l "control-plane=controller-manager" --tail=-1 --namespace emqx-operator-system
```

## Deploy on Public Cloud

Use the following guides to deploy EMQX on managed Kubernetes services using the EMQX Operator:

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
