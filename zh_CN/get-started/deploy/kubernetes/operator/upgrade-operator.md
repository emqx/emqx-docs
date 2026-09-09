# 升级 EMQX Operator

本页面提供将 EMQX Operator 升级到最新版本 2.3.0 的操作说明。

## 从 EMQX Operator 2.2.x 升级到 2.3.0

1. 在开始升级流程之前，请确保所有 EMQX 自定义资源均使用 `v2beta1` API 版本。EMQX Operator 2.3.0 不支持早于 `v2beta1` 的 API 版本。

   如果您的资源仍在使用 `v2alpha1` 或 `v1beta4` API 版本，需要将其更新为 `v2beta1`。在大多数情况下，可以通过修改 `apiVersion` 字段来完成：

   ```sh
   kubectl patch emqx emqx --type=merge -p '{"apiVersion":"apps.emqx.io/v2beta1"}'
   ```

2. 对现有的 EMQX CRD 进行打补丁，显式移除 conversion webhook。

   ```sh
   kubectl patch crd emqxes.apps.emqx.io     --type=json -p='[{"op":"replace", "path":"/spec/conversion", "value":{"strategy":"None"}}]'
   kubectl patch crd rebalances.apps.emqx.io --type=json -p='[{"op":"replace", "path":"/spec/conversion", "value":{"strategy":"None"}}]'
   ```

3. 在完成 EMQX CRD 的修改后，删除现有的 controller manager 部署以及其他相关资源。

   ```sh
   kubectl delete --ignore-not-found clusterrole emqx-operator-manager-role
   kubectl delete --ignore-not-found clusterrolebinding emqx-operator-manager-rolebinding
   kubectl delete --ignore-not-found mutatingwebhookconfiguration emqx-operator-mutating-webhook-configuration
   kubectl delete --ignore-not-found validatingwebhookconfiguration emqx-operator-validating-webhook-configuration
   kubectl delete --ignore-not-found namespace emqx-operator-system
   ```

4. （可选）删除旧版 CRD。

   ```sh
   kubectl delete --ignore-not-found crd emqxbrokers.apps.emqx.io emqxenterprises.apps.emqx.io emqxplugins.apps.emqx.io
   ```

5. 按照[安装步骤](./getting-started.md)部署新的 EMQX Operator。