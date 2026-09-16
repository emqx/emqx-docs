# EMQX Operator のアップグレード

このページでは、EMQX Operator を最新バージョン 2.3.0 にアップグレードする手順を説明します。

## EMQX Operator 2.2.x から 2.3.0 へのアップグレード

1. アップグレードを開始する前に、すべての EMQX カスタムリソースが `v2beta1` API バージョンを使用していることを確認してください。EMQX Operator 2.3.0 は `v2beta1` より前の API バージョンをサポートしていません。

   リソースがまだ `v2alpha1` または `v1beta4` API バージョンを使用している場合は、`v2beta1` に更新してください。ほとんどの場合、`apiVersion` フィールドをパッチ適用することで対応可能です。

   ```sh
   kubectl patch emqx emqx --type=merge -p '{"apiVersion":"apps.emqx.io/v2beta1"}'
   ```

2. 既存の EMQX CRD に対して、変換用の webhook を明示的に削除するパッチを適用します。

   ```sh
   kubectl patch crd emqxes.apps.emqx.io     --type=json -p='[{"op":"replace", "path":"/spec/conversion", "value":{"strategy":"None"}}]'
   kubectl patch crd rebalances.apps.emqx.io --type=json -p='[{"op":"replace", "path":"/spec/conversion", "value":{"strategy":"None"}}]'
   ```

3. EMQX CRD のパッチ適用後、既存のコントローラーマネージャーのデプロイメントおよび関連リソースを削除します。

   ```sh
   kubectl delete --ignore-not-found clusterrole emqx-operator-manager-role
   kubectl delete --ignore-not-found clusterrolebinding emqx-operator-manager-rolebinding
   kubectl delete --ignore-not-found mutatingwebhookconfiguration emqx-operator-mutating-webhook-configuration
   kubectl delete --ignore-not-found validatingwebhookconfiguration emqx-operator-validating-webhook-configuration
   kubectl delete --ignore-not-found namespace emqx-operator-system
   ```

4. 必要に応じて、レガシー CRD を削除します。

   ```sh
   kubectl delete --ignore-not-found crd emqxbrokers.apps.emqx.io emqxenterprises.apps.emqx.io emqxplugins.apps.emqx.io
   ```

5. [インストール手順](./getting-started.md) に従って、新しい EMQX Operator をデプロイします。
