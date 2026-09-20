# ライセンスの管理

## 目的

- EMQX Enterprise ライセンスの設定
- EMQX Enterprise ライセンスの更新

## ライセンスの設定

EMQX Enterprise ライセンスは、EMQX公式サイトから無料で申請できます：[EMQX Enterprise ライセンス申請](https://www.emqx.com/en/apply-licenses/emqx)。

## EMQX クラスターの設定

EMQX CRD `apps.emqx.io/v2beta1` は、`.spec.config.data` フィールドを通じて EMQX クラスターのライセンス設定をサポートしています。詳細な設定リファレンスは[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)をご参照ください。

1. 以下の内容を YAML ファイルとして保存し、`kubectl apply` でデプロイします。

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

   `.spec.config.data` フィールド内の `license.key` はライセンスの内容を表します。この例ではライセンス内容を省略していますので、ご自身のライセンスキーを入力してください。

   :::

2. EMQX クラスターが準備完了になるまで待ちます。

   `kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Ready` になっていることを確認してください。準備完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx-ee
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## ライセンスの更新

1. ライセンス情報を確認します。

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

   出力には、申請者情報、ライセンスでサポートされる最大接続数、有効期限などの基本的なライセンス情報が表示されます。

2. EMQX CR を編集してライセンスを更新します。

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

3. ライセンスが更新されたことを確認します。

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

   更新された `max_connections` フィールドにより、EMQX Enterprise ライセンスが正常に更新されたことが明確にわかります。ライセンスの更新には時間がかかる場合があるため、コマンドを再試行する必要があるかもしれません。
