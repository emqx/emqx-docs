# Deploy EMQX on Azure Kubernetes Service

EMQX Operator supports deploying EMQX on Azure Kubernetes Service (AKS). AKS simplifies deploying a managed Kubernetes cluster in Azure by offloading the operational overhead to Azure. As a hosted Kubernetes service, Azure handles critical tasks, like health monitoring and maintenance. When an AKS cluster is created, Azure automatically provisions and manages the Kubernetes control plane at no additional cost.

## Before You Begin

Before deploying EMQX on AKS, ensure the following prerequisites are met:

- An AKS cluster in your Azure subscription
  * Refer to the [Azure Kubernetes Service documentation](https://learn.microsoft.com/en-us/azure/aks/) for guidance on creating and configuring an AKS cluster.

- A working `kubectl` configuration for connecting to the AKS cluster
  - To connect using the locally installed `kubectl`, follow the instructions in [Connect to an AKS cluster](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-cli).
  - To connect using Azure Cloud Shell, see [Manage an AKS cluster in Azure CloudShell](https://learn.microsoft.com/en-us/azure/aks/learn/quick-kubernetes-deploy-portal?tabs=azure-cli).

- EMQX Operator installed on the cluster
  - Refer to [Install EMQX Operator](./getting-started.md) for installation details.
  

## Deploy EMQX Cluster Quickly

The following example shows a basic configuration for an EMQX Custom Resource (CR).

1. Save it as a YAML file and deploy with `kubectl apply`.

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
     coreTemplate:
       spec:
         volumeClaimTemplates:
           ## more information about storage classes: https://learn.microsoft.com/en-us/azure/aks/concepts-storage#storage-classes
           storageClassName: default
           resources:
             requests:
               storage: 10Gi
           accessModes:
           - ReadWriteOnce
     dashboardServiceTemplate:
       spec:
         ## more information about load balancer: https://learn.microsoft.com/en-us/azure/aks/load-balancer-standard
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         ## more information about load balancer: https://learn.microsoft.com/en-us/azure/aks/load-balancer-standard
         type: LoadBalancer
   ```

2. Wait for the EMQX cluster to become ready.

   Check the cluster status using `kubectl get` and verify that the `STATUS` is `Ready`. A startup may take some time.

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m5s
   ```

3. Retrieve the external IP of the EMQX Dashboard and access it.

   The EMQX Operator automatically creates a Service based on the `dashboardServiceTemplate` configuration.

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   20.245.230.91
   ```

4. Open the Dashboard at `http://20.245.230.91:18083`.

    Log in with the default credentials:

     - **Username:** `admin`
     - **Password:** `public`

## Use MQTTX to Subscribe and Publish

This walkthrough uses [MQTTX CLI](https://mqttx.app/cli), an open-source MQTT 5.0 command-line client tool that helps developers quickly test the MQTT services and applications.

1. Obtain the external IP of the EMQX TCP listener.

   The EMQX Operator automatically creates a Service resource for each configured listener.

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. Subscribe to a topic.

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  Connecting...
   [10:00:25] › ✔  Connected
   [10:00:25] › …  Subscribing to hello...
   [10:00:25] › ✔  Subscribed to hello
   ```

3. In another terminal, connect to the EMQX cluster and publish a message.

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   [10:00:58] › …  Connecting...
   [10:00:58] › ✔  Connected
   [10:00:58] › …  Message Publishing...
   [10:00:58] › ✔  Message published
   ```

4. Observe the subscriber receiving the message.

   ```shell
   [10:00:58] › payload: hello world
   ```

## Notes on TLS Offloading with LoadBalancer

As an L3/L4 load balancer, Azure LoadBalancer does not support TLS termination. Please refer to this [discussion](https://github.com/emqx/emqx-operator/discussions/312) to understand possible workarounds.
