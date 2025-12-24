# Manage License

## Objective

- Configure the EMQX Enterprise license.
- Update EMQX Enterprise license.

## Configure License

You can apply for an EMQX Enterprise license for free on the EMQX official website: [Apply for EMQX Enterprise License](https://www.emqx.com/en/apply-licenses/emqx).

## Configure EMQX Cluster

EMQX CRD `apps.emqx.io/v2beta1` supports configuring the EMQX cluster license through the `.spec.config.data` field. Refer to the [Configuration Manual](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/) for a complete configuration reference.

1. Save the following as a YAML file and deploy it using `kubectl apply`.

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
     name: emqx-ee
   spec:
     config:
       data: |
         license {
           key = "..."
         }
     image: emqx/emqx:@EE_VERSION@
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip

   The `license.key` in the `.spec.config.data` field represents the license content. In this example, the license content is omitted. Please fill it in with your own license key.

   :::

2. Wait for the EMQX cluster to become ready. 

   Check the status of the EMQX cluster with `kubectl get` and ensure that `STATUS` is `Ready`. This may take some time.

   ```bash
   $ kubectl get emqx emqx-ee
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## Update License

1. View the license information.

   ```bash
   $ kubectl exec -it service/emqx-ee-headless -c emqx -- emqx ctl license info
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

   The output shows basic license information, including the applicant's information, the maximum number of connections supported by the license, and the expiration time.

2. Modify the EMQX CR to update the license.

   ```bash
   $ kubectl edit emqx emqx-ee
   ...
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       data: |
         license {
           key = "${new_license_key}"
         }
   ...
   ```

3. Verify that the license has been updated.

   ```bash
   $ kubectl exec -it service/emqx-ee-headless -c emqx -- emqx ctl license info
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

   The updated `max_connections` field clearly indicates that the EMQX Enterprise license has been updated successfully. Keep in mind that the license update may take time, so you may need to retry the command.

