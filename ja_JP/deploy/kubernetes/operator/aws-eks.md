# Deploy EMQX on Amazon Elastic Kubernetes Service

EMQX Operator supports running on Amazon Container Service EKS (Elastic Kubernetes Service). Amazon EKS is a managed Kubernetes service that simplifies the deployment, management, and scaling of containerized applications. EKS provides the Kubernetes control plane and node groups, automatically handling node replacements, upgrades, and patching. It supports AWS services such as Load Balancers, RDS, and IAM, and integrates seamlessly with other Kubernetes ecosystem tools.

For an in-depth introduction, refer to [What is Amazon EKS](https://docs.aws.amazon.com/eks/latest/userguide/what-is-eks.html).

## Before You Begin

Before deploying EMQX on EKS, ensure you have completed the following prerequisites:

- Create an EKS cluster.<br/>See [Create an Amazon EKS cluster](https://docs.aws.amazon.com/eks/latest/userguide/getting-started.html) for more details.

- Configure kubectl to connect to your EKS cluster.<br/>See [Using kubectl to connect to the cluster](https://docs.aws.amazon.com/eks/latest/userguide/getting-started-console.html#eks-configure-kubectl) for more details.

- Deploy an AWS Load Balancer Controller on a cluster.<br/>See [Create a Network Load Balancer](https://docs.aws.amazon.com/eks/latest/userguide/network-load-balancing.html) for more details.

- Install the Amazon EBS CSI driver on the cluster.<br/>See [Amazon EBS CSI driver](https://docs.aws.amazon.com/eks/latest/userguide/ebs-csi.html) for further details.

- Install EMQX Operator.<br/>Please refer to [Install EMQX Operator](./getting-started.md) for further details.

## Deploy EMQX Cluster Quickly

The following example demonstrates the relevant EMQX Custom Resource (CR) configuration for deployment on EKS.

1. Save the following content as a YAML file and deploy it with `kubectl apply`.

   ```yaml
   # Configure EBS StorageClass with WaitForFirstConsumer binding mode
   # This ensures volumes are created in the same AZ as the pods that will use them
   apiVersion: storage.k8s.io/v1
   kind: StorageClass
   metadata:
     name: ebs-sc
   provisioner: ebs.csi.aws.com
   volumeBindingMode: WaitForFirstConsumer
   ---
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
         ## EMQX custom resources do not support updating this field at runtime
         persistentVolumeClaimSpec:
           storageClassName: ebs-sc
           resources:
             requests:
               storage: 10Gi
           accessModes:
             - ReadWriteOnce
     dashboardServiceTemplate:
       metadata:
         ## More content: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/annotations/
         annotations:
           ## Specifies whether the NLB is Internet-facing or internal. If not specified, defaults to internal.
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## More content: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
     listenersServiceTemplate:
       metadata:
         ## More content: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/annotations/
         annotations:
           ## Specifies whether the NLB is Internet-facing or internal. If not specified, defaults to internal.
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## More content: https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
   ```

2. Wait for the EMQX cluster to become ready.

   Use the following command to check the status. The `STATUS` field must show `Ready`, which may take several minutes:

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     55s
   ```

3. Obtain the external IP of the EMQX Dashboard and access it.

   The EMQX Operator creates a Service for the EMQX Dashboard based on your `dashboardServiceTemplate` configuration.

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   192.168.1.200
   ```

4. Open the Dashboard at: `http://192.168.1.200:18083`.

   Log in with the default credentials:

     - **Username:** `admin`
     - **Password:** `public`

## Subscribe and Publish

This walkthrough uses [MQTTX CLI](https://mqttx.app/cli), an open-source MQTT 5.0 command-line client tool that helps developers quickly test the MQTT services and applications.

1. Retrieve the external IP of the EMQX TCP listener.

   The EMQX Operator automatically creates a Service resource for each configured listener.

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. Subscribe to a topic.

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   
   [10:00:25] › … Connecting...
   [10:00:25] › ✔ Connected
   [10:00:25] › … Subscribing to hello...
   [10:00:25] › ✔ Subscribed to hello
   ```

3. In another terminal, connect to the EMQX cluster and publish a message.

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```

4. Observe the subscriber receiving the message.

   ```shell
   [10:00:58] › payload: hello world
   ```

## Terminate TLS Encryption with LoadBalancer

You can use an AWS Network Load Balancer (NLB) to terminate TLS traffic for EMQX. Follow the steps below:

1. Import relevant certificates in [AWS Console](https://us-east-2.console.aws.amazon.com/acm/home). Open the certificate details page by clicking the certificate ID. Record the certificate ARN.

    ::: tip
For certificate/key import formats, see [Importing certificates](https://docs.aws.amazon.com/acm/latest/userguide/import-certificate-format.html).
    :::

2. Add annotations to the EMQX Service metadata, for example:

    ```yaml
    ## Specifies the ARN of one or more certificates managed by the AWS Certificate Manager.
    service.beta.kubernetes.io/aws-load-balancer-ssl-cert: arn:aws:acm:us-west-2:xxxxx:certificate/xxxxxxx
    ## Specifies whether to use TLS for the backend traffic between the load balancer and the kubernetes pods.
    service.beta.kubernetes.io/aws-load-balancer-backend-protocol: tcp
    ## Specifies a frontend port with a TLS listener. This means that accessing port 1883 through AWS NLB service requires TLS authentication,
    ## but direct access to K8S service port does not require TLS authentication
    service.beta.kubernetes.io/aws-load-balancer-ssl-ports: "1883"
    ```

    ::: tip
    The value of `service.beta.kubernetes.io/aws-load-balancer-ssl-cert` should match the ARN recorded in step 1.
    :::
