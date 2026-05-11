# License Configuration (EMQX Enterprise)

## Task Target

- Configure EMQX Enterprise License.
- Update EMQX Enterprise License.

## Configure License

EMQX Enterprise License can be applied for free on EMQ official website: [Apply for EMQX Enterprise License](https://www.emqx.com/en/apply-licenses/emqx).

## Configure EMQX Cluster

`apps.emqx.io/v2beta1 EMQX` supports configuring EMQX cluster license through `.spec.config.data`. For config.data configuration, please refer to the document: [Configuration Manual](../../../../configuration/configuration.md). This field is only allowed to be configured when creating an EMQX cluster, and does not support updating.

  > After the EMQX cluster is created, if the license needs to be updated, please update it through the EMQX Dashboard.

+ Save the following content as a YAML file and deploy it via the `kubectl apply` command

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    config:
      data: |
        license {
          key = "..."
        }
    image: emqx/emqx-enterprise:@EE_VERSION@
    dashboardServiceTemplate:
      spec:
        type: LoadBalancer
  ```

  > The `license.key` in the `config.data` field represents the License content. In this example, the License content is omitted, please fill it in by the user.

+ Wait for the EMQX cluster to be ready, you can check the status of the EMQX cluster through `kubectl get` command, please make sure `STATUS` is `Running`, this may take some time

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ Obtain the Dashboard External IP of EMQX cluster and access EMQX console

  EMQX Operator will create two EMQX Service resources, one is emqx-dashboard and the other is emqx-listeners, corresponding to EMQX console and EMQX listening port respectively.

  ```bash
  $ kubectl get svc emqx-ee-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'
  
  192.168.1.200
  ```

  Access `http://192.168.1.200:18083` through a browser, and use the default username and password `admin/public` to login EMQX console.

## Update License

+ View License information
  ```bash
  $ pod_name="$(kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/db-role=core' -o json | jq --raw-output '.items[0].metadata.name')"
  $ kubectl exec -it ${pod_name} -c emqx -- emqx_ctl license info
  ```

  The following output can be obtained. From the output, we can see the basic information of the license we applied for, including applicant's information, maximum connection supported by the license, and expiration time of the license.
  ```bash
  customer        : Evaluation
  email           : contact@emqx.io
  deployment      : default
  max_connections : 100
  start_at        : 2023-01-09
  expiry_at       : 2028-01-08
  type            : trial
  customer_type   : 10
  expiry          : false
  ```

+ Modify EMQX custom resources to update the License.
  ```bash
  $ kubectl edit emqx emqx
  ...
  spec:
    image: emqx/emqx-enterprise:@EE_VERSION@
    config:
      data: |
        license {
          key = "${new_license_key}"
        }
  ...
  ```

  + Check if the EMQX cluster license has been updated.
  ```bash
  $ pod_name="$(kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/db-role=core' -o json | jq --raw-output '.items[0].metadata.name')"
  $ kubectl exec -it ${pod_name} -c emqx -- emqx_ctl license info
  ```

  It can be seen from the "max_connections" field that the content of the License has been updated, indicating that the EMQX Enterprise Edition License update is successful. If the certificate information is not updated, you can wait for a while as there may be some delay in updating the License.
  ```bash
  customer        : Evaluation
  email           : contact@emqx.io
  deployment      : default
  max_connections : 100000
  start_at        : 2023-01-09
  expiry_at       : 2028-01-08
  type            : trial
  customer_type   : 10
  expiry          : false
  ```
