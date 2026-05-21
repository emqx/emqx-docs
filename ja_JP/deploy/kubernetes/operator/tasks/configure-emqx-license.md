<<<<<<< HEAD
# ライセンス管理

## 目的

- EMQX Enterpriseライセンスの設定
- EMQX Enterpriseライセンスの更新

## ライセンスの設定

EMQX公式サイトでEMQX Enterpriseライセンスを無料で申請できます：[EMQX Enterpriseライセンス申請](https://www.emqx.com/en/apply-licenses/emqx)。

## EMQXクラスターの設定

EMQX CRD `apps.emqx.io/v2beta1` は、`.spec.config.data` フィールドを通じてEMQXクラスターのライセンス設定をサポートしています。詳細な設定リファレンスは[設定マニュアル](https://docs.emqx.com/en/enterprise/v6.0.0/hocon/)をご参照ください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`でデプロイします。

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
=======
# ライセンス設定（EMQX Enterprise）

## 対象タスク

- EMQX Enterprise ライセンスの設定
- EMQX Enterprise ライセンスの更新

## ライセンスの設定

EMQX Enterprise ライセンスは、EMQ公式サイトから無料で申請できます：[EMQX Enterprise ライセンス申請](https://www.emqx.com/en/apply-licenses/emqx)。

## EMQX クラスターの設定

`apps.emqx.io/v2beta1 EMQX` では、`.spec.config.data` を通じて EMQX クラスターのライセンスを設定できます。`config.data` の設定方法については、以下のドキュメントを参照してください：[設定マニュアル](../../../../configuration/configuration.md)。このフィールドは EMQX クラスター作成時のみ設定可能で、更新はサポートされていません。

> EMQX クラスター作成後にライセンスを更新する必要がある場合は、EMQX ダッシュボードから更新してください。

+ 以下の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイします。
>>>>>>> origin/release-5.10

   ::: tip

<<<<<<< HEAD
   `.spec.config.data` フィールド内の `license.key` はライセンスの内容を示します。この例ではライセンス内容を省略していますので、ご自身のライセンスキーを入力してください。

   :::
=======
  > `config.data` フィールド内の `license.key` はライセンスの内容を表します。本例ではライセンス内容を省略していますので、ユーザーが適宜入力してください。

+ EMQX クラスターの準備が整うまで待機します。`kubectl get` コマンドでクラスターの状態を確認し、`STATUS` が `Running` であることを確認してください。準備には時間がかかる場合があります。
>>>>>>> origin/release-5.10

2. EMQXクラスターがReady状態になるまで待ちます。

<<<<<<< HEAD
   `kubectl get` コマンドでEMQXクラスターのステータスを確認し、`STATUS`が`Ready`となっていることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx-ee
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```
=======
+ EMQX クラスターのダッシュボード外部IPを取得し、EMQX コンソールにアクセスします。

  EMQX Operator は、EMQX コンソール用の `emqx-dashboard` と EMQX リスニングポート用の `emqx-listeners` の2つの Service リソースを作成します。
>>>>>>> origin/release-5.10

## ライセンスの更新

<<<<<<< HEAD
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

   出力には申請者情報、ライセンスでサポートされる最大接続数、有効期限などの基本的なライセンス情報が表示されます。

2. EMQX CRを編集してライセンスを更新します。

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

   更新された `max_connections` フィールドにより、EMQX Enterpriseライセンスが正常に更新されたことが明確に確認できます。ライセンスの更新には時間がかかる場合があるため、コマンドを再試行する必要があるかもしれません。
=======
  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` で EMQX コンソールにログインしてください。

## ライセンスの更新

+ ライセンス情報の確認

  ```bash
  $ pod_name="$(kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/db-role=core' -o json | jq --raw-output '.items[0].metadata.name')"
  $ kubectl exec -it ${pod_name} -c emqx -- emqx_ctl license info
  ```

  以下のような出力が得られます。出力から、申請したライセンスの基本情報（申請者情報、ライセンスでサポートされる最大接続数、ライセンスの有効期限など）を確認できます。

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

+ EMQX カスタムリソースを編集してライセンスを更新します。

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

+ EMQX クラスターのライセンスが更新されたか確認します。

  ```bash
  $ pod_name="$(kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/db-role=core' -o json | jq --raw-output '.items[0].metadata.name')"
  $ kubectl exec -it ${pod_name} -c emqx -- emqx_ctl license info
  ```

  `max_connections` フィールドの値が変わっていれば、ライセンスの内容が更新されていることを示し、EMQX Enterprise エディションのライセンス更新が成功したことを意味します。証明書情報がすぐに更新されない場合は、ライセンス更新に遅延がある可能性があるため、しばらく待ってから再度確認してください。

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
>>>>>>> origin/release-5.10
