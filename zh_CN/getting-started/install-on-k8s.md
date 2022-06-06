# 利用 EMQX Operator 部署 EMQX 集群

1. 使用 [cert-manager](https://github.com/cert-manager/cert-manager) 给 webhook 服务提供证书，可以参考 [cert-manager 文档](https://cert-manager.io/docs/installation/) 安装

2. 可以通过如下两种方式之一来安装 Operator Controller：

    + 使用静态文件安装

    ```shell
    curl -f -L "https://github.com/emqx/emqx-operator/releases/download/1.1.7/emqx-operator-controller.yaml" | kubectl apply -f -
    ```

    + 使用 Helm 安装
      + 添加 EMQX Helm 仓库

      ```shell
      helm repo add emqx https://repos.emqx.io/charts 
      helm repo update
      ```

      + 用 Helm 安装 EMQX Operator 控制器

      ```shell
      $ helm install emqx-operator emqx/emqx-operator \ 
        --set installCRDs=true \ 
        --namespace emqx-operator-system \ 
        --create-namespace
      ```

3. 检查 EMQX Operator 控制器状态

    ```shell
    $ kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system 
    NAME READY STATUS RESTARTS AGE 
    emqx-operator-controller-manager-68b866c8bf-kd4g6 1/1 Running 0 15s
    ```

{% emqxce %}
4. 部署 EMQX Broker

+ 部署 EMQX Custom Resource

    ```shell
    $ cat << "EOF" | kubectl apply -f -
    apiVersion: apps.emqx.io/v1beta2
    kind: EmqxBroker
    metadata:
      name: emqx
    spec:
      image: emqx/emqx:4.3.11
    EOF
    ```

+ 检查 EMQX 状态

   ```shell
   $ kubectl get pods 
    NAME READY STATUS RESTARTS AGE 
    emqx-0 1/1 Running 0 22s 
    emqx-1 1/1 Running 0 22s 
    emqx-2 1/1 Running 0 22s 
    $ kubectl exec -it emqx-0 -- emqx_ctl status 
    Node 'emqx@emqx-0.emqx-headless.default.svc.cluster.local' 4.3.11 is started
    $ kubectl exec -it emqx-0 -- emqx_ctl cluster status 
    Cluster status: #{running_nodes =>
                      ['emqx@emqx-0.emqx-headless.default.svc.cluster.local',
                       'emqx@emqx-1.emqx-headless.default.svc.cluster.local',
                       'emqx@emqx-2.emqx-headless.default.svc.cluster.local'],
                  stopped_nodes => []}
    ```

{% endemqxce %}

{% emqxee %}
4. 部署 EMQX Enterprise

+ 部署 EMQX Custom Resource

    ```shell
    cat << "EOF" | kubectl apply -f -
    apiVersion: apps.emqx.io/v1beta2
    kind: EmqxEnterprise
    metadata:
      name: emqx-ee
    spec:
      image: emqx/emqx-ee:4.3.6
    EOF
    ```

+ 检查 EMQX 状态

   ```shell
   $ kubectl get pods 
   NAME READY STATUS RESTARTS AGE 
   emqx-ee-0 1/1 Running 0 22s 
   emqx-ee-1 1/1 Running 0 22s 
   emqx-ee-2 1/1 Running 0 22s
   $ kubectl exec -it emqx-ee-0 -- emqx_ctl status 
   Node 'emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local' 4.3.6 is started
   $ kubectl exec -it emqx-ee-0 -- emqx_ctl cluster status 
   Cluster status: #{running_nodes =>
                      ['emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local',
                       'emqx-ee@emqx-ee-1.emqx-ee-headless.default.svc.cluster.local',
                       'emqx-ee@emqx-ee-2.emqx-ee-headless.default.svc.cluster.local'],
                  stopped_nodes => []}
    ```

{% endemqxee %}