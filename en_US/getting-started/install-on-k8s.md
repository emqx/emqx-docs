# Install EMQX Cluster with EMQX Operator

1. Use [cert-manager](https://github.com/cert-manager/cert-manager) for provisioning the certificates for the webhook server. Please follow the [cert-manager documentation](https://cert-manager.io/docs/installation/) to install it。

2. Install Operator Controller Manager service in one of the following ways：

    + Install with default static files

    ```shell
    curl -f -L "https://github.com/emqx/emqx-operator/releases/download/1.2.1/emqx-operator-controller.yaml" | kubectl apply -f -
    ```

    + Install with Helm
      + Add the EMQX Helm repository

    ```shell
    helm repo add emqx https://repos.emqx.io/charts 
    helm repo update
    ```

    + Install EMQX Operator Controller by Helm

    ```shell
    $ helm install emqx-operator emqx/emqx-operator \ 
    --set installCRDs=true \ 
    --namespace emqx-operator-system \ 
    --create-namespace
    ```

3. Check EMQX Operator Controller Status

    ```shell
    $ kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system 
    NAME READY STATUS RESTARTS AGE 
    emqx-operator-controller-manager-68b866c8bf-kd4g6 1/1 Running 0 15s
    ```

{% emqxce %}
4. Deploy the EMQX Broker

+ Deploy EMQX Custom Resource

    ```shell
    $ cat << "EOF" | kubectl apply -f -
    apiVersion: apps.emqx.io/v1beta3
    kind: EmqxBroker
    metadata:
      name: emqx
    spec:
      image: emqx/emqx:4.4.5
    EOF
    ```

+ Check EMQX status

   ```shell
   $ kubectl get pods 
    NAME READY STATUS RESTARTS AGE 
    emqx-0 1/1 Running 0 22s 
    emqx-1 1/1 Running 0 22s 
    emqx-2 1/1 Running 0 22s 
    $ kubectl exec -it emqx-0 -- emqx_ctl status 
    Node 'emqx@emqx-0.emqx-headless.default.svc.cluster.local' 4.4.5 is started
    $ kubectl exec -it emqx-0 -- emqx_ctl cluster status 
    Cluster status: #{running_nodes =>
                      ['emqx@emqx-0.emqx-headless.default.svc.cluster.local',
                       'emqx@emqx-1.emqx-headless.default.svc.cluster.local',
                       'emqx@emqx-2.emqx-headless.default.svc.cluster.local'],
                  stopped_nodes => []}
    ```

{% endemqxce %}

{% emqxee %}
4. Deploy EMQX Enterprise

+ Deploy EMQX Enterprise Custom Resource

    ```shell
    $ cat << "EOF" | kubectl apply -f -
    apiVersion: apps.emqx.io/v1beta3
    kind: EmqxEnterprise
    metadata:
      name: emqx-ee
    spec:
      image: emqx/emqx-ee:4.4.5
    EOF
    ```

+ Check EMQX Enterprise status

   ```shell
   $ kubectl get pods 
   NAME READY STATUS RESTARTS AGE 
   emqx-ee-0 1/1 Running 0 22s 
   emqx-ee-1 1/1 Running 0 22s 
   emqx-ee-2 1/1 Running 0 22s
   $ kubectl exec -it emqx-ee-0 -- emqx_ctl status 
   Node 'emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local' 4.4.5 is started
   $ kubectl exec -it emqx-ee-0 -- emqx_ctl cluster status 
   Cluster status: #{running_nodes =>
                      ['emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local',
                       'emqx-ee@emqx-ee-1.emqx-ee-headless.default.svc.cluster.local',
                       'emqx-ee@emqx-ee-2.emqx-ee-headless.default.svc.cluster.local'],
                  stopped_nodes => []}
    ```

{% endemqxee %}
