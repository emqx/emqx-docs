# Deploy with K8s

EMQX provides the [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator) to automate the creation, configuration, and management of EMQX clusters on Kubernetes (K8s).

EMQX Kubernetes Operator is a new way to create and manage cloud-native EMQX instances based on Kubernetes architectures. It simplifies the process and required knowledge of the deployment and management.

::: tip
Kubernetes v1.20.0 or higher version is needed.
:::

## Deploy EMQX Kubernetes Operator

We will use [cert-manager](https://github.com/jetstack/cert-manager) to provision the certificates for the Webhook server. You can refer to [the cert-manager documentation](https://cert-manager.io/docs/installation/) on how to install it. 

1. To install by Helm, run: 

  ```bash
  helm repo add emqx https://repos.emqx.io/charts
  helm repo update
  helm install emqx-operator emqx/emqx-operator --namespace emqx-operator-system --create-namespace
  ```

2. Wait till EMQX Operator Controller starts running：

  ```bash
  $ kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system
  NAME                                                READY   STATUS    RESTARTS   AGE
  emqx-operator-controller-manager-68b866c8bf-kd4g6   1/1     Running   0          15s
  ```

## Deploy EMQX

1. To deploy EMQX Custom Resource, run:

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

​	You can see a complete code example on the [GitHub page of EMQX operator](https://github.com/emqx/emqx-operator/blob/main/config/samples/emqx/v2alpha1/emqx-full.yaml). For detailed explanation of each field, see [EMQX Operator - API Reference](https://docs.emqx.com/en/emqx-operator/latest/reference/v2alpha1-reference.html).

2. To check the status of EMQX Custom Resource, run:

  ```bash
  kubectl get pods
  kubectl get emqx emqx -o json | jq ".status.emqxNodes"
  ```

Now we have completed the deployment of EMQX with EMQX Kubernetes Operator. On how to configure, see{%emqxce%}[EMQX Operator - Config](https://docs.emqx.com/en/emqx-operator/latest/config/v1beta3/EmqxBroker.html){%emqxce%}{%emqxee%}[EMQX Operator - Config](https://docs.emqx.com/en/emqx-operator/latest/config/v1beta3/EmqxEnterprise.html){%endemqxee%}. 

## Deploy on Public Cloud

You can also use EMQX Kubernetes Operator to deploy EMQX cluster on Kubernetes in the Public Cloud, click the link below on how to configure:

- [Deploy on AWS EKS](https://docs.emqx.com/en/emqx-operator/latest/deployment/aws-eks-deployment.html)
- [Deploy on Azure AKS](https://docs.emqx.com/en/emqx-operator/latest/deployment/azure-deployment.html)

## Next steps

Use an MQTT client to connect EMQX for message publish/subscribe. For more information, see [Publish and Subscribe](../messaging/mqtt-publish-and-subscribe.md). 

Or you may click the links below to learn how to use EMQX Kubernetes Operator for common EMQX cluster configurations:

- [Configure EMQX logs collection](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-logs-collection.html)
- [Configure EMQX Core and Replicant nodes](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-core-replicant.html)
- [Configure EMQX TLS certificate](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-tls.html)
- [Configure EMQX License](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-license.html)
- [Configure EMQX persistence](https://docs.emqx.com/en/emqx-operator/latest/tasks/configure-emqx-persistence.html)
