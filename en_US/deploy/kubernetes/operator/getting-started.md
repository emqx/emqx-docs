# Install Operator and Deploy EMQX

This section guides you through preparing the environment for EMQX Operator, installing the Operator itself, and using it to deploy EMQX. By following the steps provided, you can install and manage EMQX efficiently and reliably with the Operator.

## Prepare the Environment

Before deploying EMQX Operator, ensure that the following components are ready:

- A [Kubernetes](https://kubernetes.io/docs/concepts/overview/) environment running Kubernetes version 1.24 or higher.

- A [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl) tool that can access the Kubernetes cluster. You can check the status of the Kubernetes cluster using `kubectl cluster-info` command.

- [Helm](https://helm.sh) 3 or higher

## Install EMQX Operator

1. Install and start `cert-manager`.

   ::: tip
   `cert-manager` version `1.1.6` or higher is required. Skip this step if the `cert-manager` is already installed and started.
   :::

   You can use Helm to install `cert-manager`.

   ```bash
   $ helm repo add jetstack https://charts.jetstack.io
   $ helm repo update
   $ helm upgrade --install cert-manager jetstack/cert-manager \
     --namespace cert-manager \
     --create-namespace \
     --set crds.enabled=true
   ```

   Alternatively, follow the official [cert-manager installation guide](https://cert-manager.io/docs/installation/).

2. Install the EMQX Operator with the command below:

   ```bash
   $ helm repo add emqx https://repos.emqx.io/charts
   $ helm repo update
   $ helm upgrade --install emqx-operator emqx/emqx-operator \
     --namespace emqx-operator-system \
     --create-namespace
   ```

3. Wait till EMQX Operator is ready:

   ```bash
   $ kubectl wait --for=condition=Ready pods -l "control-plane=controller-manager" -n emqx-operator-system
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Once the Operator is running, you can proceed to deploy EMQX.

## Deploy EMQX

:::: tabs type:card

::: tab EMQX Enterprise 5

1. Save the following content as a YAML file and deploy it with the `kubectl apply`.

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx-ee
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
   emqx-ee   Ready     2m55s
   ```

   Make sure the `STATUS` is `Ready`. It may take some time for the EMQX cluster to become ready.

:::

::: tab EMQX Open Source 5

1. Save the following content as a YAML file and deploy it with the `kubectl apply`.

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx:@CE_VERSION@
   ```

   For more details about the EMQX CRD, check out the [reference documentation](./reference/v2beta1-reference.md).

2. Wait until the EMQX cluster is ready.

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   Make sure the `STATUS` is `Ready`, it may take some time for the EMQX cluster to become ready. A lot of things happen behind the scenes.

:::

::::

## Deploy on Public Cloud

Use the following guides to deploy EMQX on managed Kubernetes services using the EMQX Operator:

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
