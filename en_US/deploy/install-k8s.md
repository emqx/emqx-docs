# K8s

EMQX Provide [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator), could automate the creation, configuration, and management of EMQX cluster.

EMQX Kubernetes Operator is a new way to create and manage cloud-native EMQX instances based on Kubernetes architectures. It simplifies the process and required knowledge of the deployment and management.

::: tip
EMQX Operator Controller requires Kubernetes v1.20.0 and up.
:::

## Running the Operator

We use a [cert-manager (opens new window)](https://github.com/jetstack/cert-manager)for provisioning the certificates for the webhook server. You can follow [the cert-manager documentation (opens new window)](https://cert-manager.io/docs/installation/)to install it.

1. Install by helm

  ```bash
  helm repo add emqx https://repos.emqx.io/charts
  helm repo update
  helm install emqx-operator emqx/emqx-operator --namespace emqx-operator-system --create-namespace
  ```

2. Wait EMQX Operator Controller running

  ```bash
  $ kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system
  NAME                                                READY   STATUS    RESTARTS   AGE
  emqx-operator-controller-manager-68b866c8bf-kd4g6   1/1     Running   0          15s
  ```

## Deploy the EMQX

1. Deploy EMQX Custom Resource

  ```bash
  cat << "EOF" | kubectl apply -f -
    apiVersion: apps.emqx.io/v2alpha1
    kind: EMQX
    metadata:
      name: emqx
    spec:
      image: emqx/emqx:5.0.13
  EOF
  ```

​	Full example please check [`emqx-full.yaml` (opens new window)](https://github.com/emqx/emqx-operator/blob/main/config/samples/emqx/v2alpha1/emqx-full.yaml). Detailed explanation of each field please check [v2alpha1-reference](https://docs.emqx.com/en/emqx-operator/latest/reference/v2alpha1-reference.html).

2. Check EMQX Custom Resource status

  ```bash
  kubectl get pods
  kubectl get emqx emqx -o json | jq ".status.emqxNodes"
  ```

You have now completed the deployment of EMQX on Operator. To configure EMQX, please refer to{%emqxce%}[EMQX Operator - Config](https://docs.emqx.com/en/emqx-operator/latest/config/v1beta3/EmqxBroker.html){%emqxce%}{%emqxee%}[EMQX Operator - Config](https://docs.emqx.com/en/emqx-operator/latest/config/v1beta3/EmqxEnterprise.html){%endemqxee%}。

## Deployment on Public Cloud

The following will guide you deploying an EMQX cluster on a public cloud K8s platform.

- [AWS EKS](https://docs.emqx.com/en/emqx-operator/latest/deployment/aws-eks-deployment.html)
- [Azure AKS](https://docs.emqx.com/en/emqx-operator/latest/deployment/azure-deployment.html)



## Next steps

To connect to EMQX using a client for sending and receiving messages, please refer to  [Publish and Subscribe](../messaging/mqtt-publish-and-subscribe.md).

The following will guide you through the common configuration operations of an EMQX cluster via Operator:

- [Configure EMQX logs collection](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-logs-collection.html)
- [Configure EMQX Core and Replicant nodes](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-core-replicant.html)
- [Configure EMQX TLS certificate](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-tls.html)
- [Configure EMQX License](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-license.html)
- [Configure EMQX persistence](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-persistence.html)
