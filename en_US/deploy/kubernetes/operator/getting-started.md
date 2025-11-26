# Install Operator and Deploy EMQX

In this section, we will walk you through the steps required to efficiently set up the environment for EMQX Operator, install it, and then use it to deploy EMQX. By following the guidelines outlined in this section, you will be able to install and manage EMQX effectively using the Operator.

## Prepare the Environment

Before deploying EMQX Operator, please confirm that the following components have been ready:

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

   Or you can follow the [cert-manager installation guide](https://cert-manager.io/docs/installation/) to install it.

   ::: warning
   If you install cert-manager on Google Kubernetes Engine (GKE) with default configuration may cause bootstrapping issues. Therefore, by adding the configuration of `--set global.leaderElection.namespace=cert-manager`, configure to use a different namespace in leader election. Please check [cert-manager compatibility](https://cert-manager.io/docs/installation/compatibility/)
   :::


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

In the following section, you will learn how to use the EMQX Operator to deploy EMQX.

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

Check out the following guides to deploy EMQX on public cloud platforms using the EMQX Operator:

- [Amazon Elastic Kubernetes Service (EKS)](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service (AKS)](./azure-aks.md)
