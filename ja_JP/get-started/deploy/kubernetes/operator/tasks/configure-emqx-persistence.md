# EMQX クラスターでのパーシステンス有効化

## 対象タスク

`volumeClaimTemplates` フィールドを使用して、EMQX 5.x クラスターのコアノードのパーシステンスを設定します。

## EMQX クラスターのパーシステンス設定

以下は EMQX カスタムリソースの関連設定例です。デプロイしたい EMQX のバージョンに応じて、対応する APIVersion を選択してください。具体的な対応関係については [EMQX Operator Compatibility](../operator.md) を参照してください。

`apps.emqx.io/v2beta1 EMQX` では、`.spec.coreTemplate.spec.volumeClaimTemplates` フィールドを通じて EMQX クラスターのコアノードのパーシステンスを設定できます。`.spec.coreTemplate.spec.volumeClaimTemplates` の意味と設定は Kubernetes の `PersistentVolumeClaimSpec` と一致しており、設定方法は以下のドキュメントを参照してください：[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.25/#persistentvolumeclaimspec-v1-core)。

ユーザーが `.spec.coreTemplate.spec.volumeClaimTemplates` フィールドを設定すると、EMQX Operator は EMQX コンテナ内の `/opt/emqx/data` ディレクトリを、作成された PV と PVC の [StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/) にマウントします。EMQX Pod が削除されても PV と PVC は削除されないため、EMQX のランタイムデータの保存が可能になります。PV と PVC の詳細は [Persistent Volumes](https://kubernetes.io/docs/concepts/storage/persistent-volumes/) をご参照ください。

+ 以下の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイします。

  ```yaml
  apiVersion: apps.emqx.io/v2beta1
  kind: EMQX
  metadata:
    name: emqx
  spec:
    image: emqx/emqx-enterprise:@EE_VERSION@
    config:
      data: |
        license {
          key = "..."
        }
    coreTemplate:
      spec:
        volumeClaimTemplates:
          storageClassName: standard
          resources:
            requests:
              storage: 20Mi
          accessModes:
            - ReadWriteOnce
        replicas: 3
    listenersServiceTemplate:
      spec:
        type: LoadBalancer
    dashboardServiceTemplate:
      spec:
        type: LoadBalancer
  ```

  > `storageClassName` フィールドは StorageClass の名前を示します。`kubectl get storageclass` コマンドで Kubernetes クラスター内に存在する StorageClass を確認するか、必要に応じて独自に StorageClass を作成してください。

+ EMQX クラスターの準備完了を待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Running` であることを確認してください。準備完了までに時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ EMQX クラスターのダッシュボード外部 IP を取得し、EMQX コンソールにアクセスします。

  EMQX Operator は EMQX の Service リソースを2つ作成します。1つは emqx-dashboard、もう1つは emqx-listeners で、それぞれ EMQX コンソールと EMQX のリスニングポートに対応しています。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` で EMQX コンソールにログインしてください。

## EMQX クラスターのパーシステンス検証

検証手順：1) 旧 EMQX ダッシュボードでテスト用ルールを作成する。2) 旧クラスターを削除する。3) EMQX クラスターを再作成し、ダッシュボードで以前作成したルールが存在するか確認する。

+ ブラウザで EMQX ダッシュボードにアクセスし、テスト用ルールを作成します。

  ```bash
  external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
  ```

  `http://${external_ip}:18083` にアクセスして EMQX ダッシュボードにログインし、Data Integration → Rules をクリックしてルール作成画面に入ります。まず「Add a response action for this rule」ボタンをクリックし、続けて「Create」をクリックしてルールを生成します。以下の図のようになります。

  ![](./assets/configure-emqx-persistent/emqx-core-action.png)

  ルールが正常に作成されると、ページにルール ID: emqx-persistent-test のルールレコードが表示されます。以下の図をご参照ください。

  ![](./assets/configure-emqx-persistent/emqx-core-rule-old.png)

+ 旧 EMQX クラスターを削除します。

  以下のコマンドを実行して EMQX クラスターを削除します。

  ```bash
  $ kubectl delete -f emqx.yaml

  emqx.apps.emqx.io "emqx" deleted
  # emqxenterprise.apps.emqx.io "emqx" deleted
  ```

  > emqx-persistent.yaml は本記事で最初に EMQX クラスターをデプロイする際に使用した YAML ファイルであり、変更は不要です。

+ EMQX クラスターを再作成します。

  以下のコマンドを実行して EMQX クラスターを再作成します。

  ```bash
  $ kubectl apply -f emqx.yaml

  emqx.apps.emqx.io/emqx created
  # emqxenterprise.apps.emqx.io/emqx created
  ```

  EMQX クラスターの準備完了を待ち、ブラウザで EMQX ダッシュボードにアクセスして、以前作成したルールが存在するか確認します。以下の図のように表示されます。

  ![](./assets/configure-emqx-persistent/emqx-core-rule-new.png)

  図から、旧クラスターで作成したルール emqx-persistent-test が新クラスターでも存在していることが確認でき、設定したパーシステンスが有効であることがわかります。
