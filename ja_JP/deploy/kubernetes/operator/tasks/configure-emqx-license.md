# ライセンス管理

## 目的

- EMQX Enterpriseライセンスの設定
- EMQX Enterpriseライセンスの更新

## ライセンスの設定

EMQX Enterpriseライセンスは、EMQX公式サイトから無料で申請できます：[EMQX Enterpriseライセンスの申請](https://www.emqx.com/en/apply-licenses/emqx)。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2` は、`.spec.config.data` フィールドを通じてEMQXクラスターのライセンス設定をサポートしています。完全な設定リファレンスについては、[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)を参照してください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v2
   kind: EMQX
   metadata:
     name: emqx
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

   `.spec.config.data` フィールド内の `license.key` はライセンスの内容を表します。この例ではライセンス内容は省略しています。ご自身のライセンスキーを入力してください。

   :::

2. EMQXクラスターが準備完了になるまで待ちます。

   `kubectl get` コマンドでEMQXクラスターの状態を確認し、`STATUS` が `Ready` になっていることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## ライセンスの更新

1. ライセンス情報を確認します。

   ```bash
   $ kubectl exec -it service/emqx-headless -c emqx -- emqx ctl license info
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

   出力には、申請者情報、ライセンスでサポートされる最大接続数、有効期限などの基本的なライセンス情報が表示されます。

2. EMQX CRを編集してライセンスを更新します。

   ```bash
   $ kubectl edit emqx emqx
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

3. ライセンスが更新されたことを確認します。

   ```bash
   $ kubectl exec -it service/emqx-headless -c emqx -- emqx ctl license info
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

   更新された `max_connections` の値から、EMQX Enterpriseライセンスが正常に更新されたことが確認できます。ライセンスの更新には時間がかかる場合があるため、コマンドの再実行が必要な場合があります。
