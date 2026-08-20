# Deploy EMQX on Google Kubernetes Engine

The EMQX Operator allows for the deployment of EMQX on Google Kubernetes Engine (GKE), which simplifies the process of deploying a managed Kubernetes cluster in GCP. With GKE, you can offload the operational overhead to GCP. By deploying EMQX on GKE, you can take advantage of the scalability and flexibility of Kubernetes, while benefiting from the simplicity and convenience of a managed service. With EMQX Operator on GKE, you can easily deploy and manage your MQTT broker in the cloud and focus on your business goals.

## Before You Begin

Before deploying EMQX on GKE, ensure the following prerequisites are met:

- A GKE cluster on Google Cloud Platform
  - You must enable the GKE API in your project. Refer to the [Google Kubernetes Engine documentation](https://cloud.google.com/kubernetes-engine/) for setup instructions.

- A working `kubectl` configuration to connect to the GKE cluster
  - To connect using a local `kubectl` installation, see [Connect to a GKE cluster](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-access-for-kubectl).
  
    To connect using Cloud Shell directly from the GCP Console, refer to [Manage a GKE cluster with Cloud Shell](https://cloud.google.com/code/docs/shell/create-configure-gke-cluster).
  
- EMQX Operator installed on the cluster
  - Refer to [Install EMQX Operator](./getting-started.md) for further details.

  ::: warning Note
  
  Installing cert-manager on GKE with default settings may cause bootstrapping issues. Add the configuration `--set global.leaderElection.namespace=cert-manager` to use a different namespace in leader election. For details, see the [cert-manager compatibility documentation](https://cert-manager.io/docs/installation/compatibility/).
  
  :::

## Deploy EMQX Cluster Quickly

The following example shows the basic EMQX Custom Resource (CR) configuration.

1. Save the following document as a YAML file and deploy it with `kubectl apply`.

    ::: warning Note

    If you specify CPU and memory limits, ensure a minimum of 250m CPU and 512Mi memory. See [Resource requests in Autopilot](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-resource-requests) for details.

    :::

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
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
         ## more information about storage classes: https://cloud.google.com/kubernetes-engine/docs/concepts/persistent-volumes#storageclasses
           storageClassName: standard
           resources:
             requests:
               storage: 10Gi
           accessModes:
           - ReadWriteOnce
     dashboardServiceTemplate:
       spec:
         ## more information about load balancer: https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balancing
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         ## more information about load balancer: https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balancing
         type: LoadBalancer
   ```

2. Wait for the EMQX cluster to become ready.

   Check the status of the EMQX cluster using `kubectl get`, make sure that the `STATUS` is `Ready`. This may take some time.

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m2s
   ```

3. Retrieve the external IP of the EMQX Dashboard.

   EMQX Operator will create a Service resource for the EMQX Dashboard according to the `dashboardServiceTemplate` configuration.

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   34.122.174.166
   ```

4. Open the Dashboard at `http://34.122.174.166:18083`.

   Log in with the default credentials:
   
    - **Username:** `admin`
    - **Password:** `public`

## Subscribe and Publish

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

3. In a separate terminal, connect to the EMQX cluster and publish a message.

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

At the time of writing, Google LoadBalancer does not support termination of TLS-to-plain-TCP traffic. Refer to this [discussion](https://github.com/emqx/emqx-operator/discussions/312) to understand possible workarounds.
