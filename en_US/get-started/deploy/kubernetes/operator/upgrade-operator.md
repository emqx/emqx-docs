# Upgrade EMQX Operator

This page provides instructions on how to upgrade EMQX Operator to the latest version, 2.3.0.

## Upgrade from EMQX Operator 2.2.x to 2.3.0

1. Before starting the upgrade process, make sure that all EMQX custom resources are using the `v2beta1` API version. EMQX Operator 2.3.0 does not support API versions earlier than `v2beta1`.

   If your resources are still using `v2alpha1` or `v1beta4` API versions, update them to `v2beta1`. In most cases, this can be done by patching the `apiVersion` field:

   ```sh
   kubectl patch emqx emqx --type=merge -p '{"apiVersion":"apps.emqx.io/v2beta1"}'
   ```

2. Patch the existing EMQX CRDs to explicitly remove the conversion webhook.

   ```sh
   kubectl patch crd emqxes.apps.emqx.io     --type=json -p='[{"op":"replace", "path":"/spec/conversion", "value":{"strategy":"None"}}]'
   kubectl patch crd rebalances.apps.emqx.io --type=json -p='[{"op":"replace", "path":"/spec/conversion", "value":{"strategy":"None"}}]'
   ```

3. After patching the EMQX CRDs, delete the existing controller manager deployment and other related resources.

   ```sh
   kubectl delete --ignore-not-found clusterrole emqx-operator-manager-role
   kubectl delete --ignore-not-found clusterrolebinding emqx-operator-manager-rolebinding
   kubectl delete --ignore-not-found mutatingwebhookconfiguration emqx-operator-mutating-webhook-configuration
   kubectl delete --ignore-not-found validatingwebhookconfiguration emqx-operator-validating-webhook-configuration
   kubectl delete --ignore-not-found namespace emqx-operator-system
   ```

4. Optionally, delete legacy CRDs.

   ```sh
   kubectl delete --ignore-not-found crd emqxbrokers.apps.emqx.io emqxenterprises.apps.emqx.io emqxplugins.apps.emqx.io
   ```

5. Deploy the new EMQX Operator by following the [installation steps](./getting-started.md).

