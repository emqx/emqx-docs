# ライセンス管理

## 目的

- EMQX Enterpriseライセンスの設定
- EMQX Enterpriseライセンスの更新

## ライセンスの設定

<<<<<<< HEAD
EMQX Enterpriseライセンスは、EMQX公式サイトで無料で申請できます：[EMQX Enterpriseライセンス申請](https://www.emqx.com/en/apply-licenses/emqx)。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2` は、`.spec.config.data` フィールドを通じてEMQXクラスターのライセンス設定をサポートしています。完全な設定リファレンスは[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)を参照してください。
=======
EMQX公式サイトでEMQX Enterpriseライセンスを無料で申請できます：[EMQX Enterpriseライセンス申請](https://www.emqx.com/en/apply-licenses/emqx)。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2beta1` は、`.spec.config.data` フィールドを通じてEMQXクラスターのライセンス設定をサポートしています。詳細な設定リファレンスは[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)をご参照ください。
>>>>>>> origin/release-6.1

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

   ```yaml
<<<<<<< HEAD
   apiVersion: apps.emqx.io/v2
   kind: EMQX
   metadata:
     name: emqx
=======
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
     name: emqx-ee
>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
   `.spec.config.data` フィールド内の `license.key` はライセンスの内容を表します。この例ではライセンス内容は省略されていますので、ご自身のライセンスキーを入力してください。

   :::

2. EMQXクラスターが準備完了になるまで待ちます。

   `kubectl get` コマンドでEMQXクラスターの状態を確認し、`STATUS` が `Ready` になっていることを確認してください。準備完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
=======
   `.spec.config.data` フィールド内の `license.key` はライセンスの内容を示します。この例ではライセンス内容を省略していますので、ご自身のライセンスキーを入力してください。

   :::

2. EMQXクラスターがReady状態になるまで待ちます。

   `kubectl get` コマンドでEMQXクラスターのステータスを確認し、`STATUS`が`Ready`となっていることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx-ee
>>>>>>> origin/release-6.1
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## ライセンスの更新

1. ライセンス情報を確認します。

   ```bash
<<<<<<< HEAD
   $ kubectl exec -it service/emqx-headless -c emqx -- emqx ctl license info
=======
   $ kubectl exec -it service/emqx-ee-headless -c emqx -- emqx ctl license info
>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
   出力には申請者情報、ライセンスでサポートされる最大接続数、期限などの基本的なライセンス情報が表示されます。
=======
   出力には申請者情報、ライセンスでサポートされる最大接続数、有効期限などの基本的なライセンス情報が表示されます。
>>>>>>> origin/release-6.1

2. EMQX CRを編集してライセンスを更新します。

   ```bash
<<<<<<< HEAD
   $ kubectl edit emqx emqx
=======
   $ kubectl edit emqx emqx-ee
>>>>>>> origin/release-6.1
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
<<<<<<< HEAD
   $ kubectl exec -it service/emqx-headless -c emqx -- emqx ctl license info
=======
   $ kubectl exec -it service/emqx-ee-headless -c emqx -- emqx ctl license info
>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
   更新された `max_connections` フィールドにより、EMQX Enterpriseライセンスが正常に更新されたことが明確にわかります。ライセンスの更新には時間がかかる場合があるため、コマンドを再試行する必要があるかもしれません。
=======
   更新された `max_connections` フィールドにより、EMQX Enterpriseライセンスが正常に更新されたことが明確に確認できます。ライセンスの更新には時間がかかる場合があるため、コマンドを再試行する必要があるかもしれません。
>>>>>>> origin/release-6.1
