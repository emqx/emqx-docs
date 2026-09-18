# EMQX クラスターでのパーシステンス有効化

## 対象タスク

`volumeClaimTemplates` フィールドを使用して、EMQX 5.x クラスターの Core ノードのパーシステンスを構成します。

## EMQX クラスターのパーシステンス構成

以下は EMQX カスタムリソースの関連構成です。デプロイしたい EMQX のバージョンに応じて対応する APIVersion を選択してください。具体的な互換性については [EMQX Operator Compatibility](../operator.md) を参照してください。

`apps.emqx.io/v2beta1 EMQX` は `.spec.coreTemplate.spec.volumeClaimTemplates` フィールドを通じて EMQX クラスター Core ノードのパーシステンスを構成できます。`.spec.coreTemplate.spec.volumeClaimTemplates` フィールドの意味と構成は Kubernetes の `PersistentVolumeClaimSpec` と一致しており、構成は以下のドキュメントを参照できます：[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.25/#persistentvolumeclaimspec-v1-core)。

ユーザーが `.spec.coreTemplate.spec.volumeClaimTemplates` フィールドを構成すると、EMQX Operator は EMQX コンテナ内の `/opt/emqx/data` ディレクトリを PV と PVC に作成された [StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/) にマウントします。EMQX Pod が削除されても PV と PVC は削除されないため、EMQX のランタイムデータを保存する目的を達成します。PV と PVC の詳細については、[Persistent Volumes](https://kubernetes.io/docs/concepts/storage/persistent-volumes/) を参照してください。

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

+ EMQX クラスターの準備が整うまで待ちます。`kubectl get` コマンドで EMQX クラスターの状態を確認し、`STATUS` が `Running` であることを確認してください。準備には時間がかかる場合があります。

  ```bash
  $ kubectl get emqx emqx
  NAME   IMAGE                              STATUS    AGE
  emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
  ```

+ EMQX クラスターのダッシュボード外部IPを取得し、EMQX コンソールにアクセスします。

  EMQX Operator は 2 つの EMQX Service リソースを作成します。1 つは emqx-dashboard、もう 1 つは emqx-listeners で、それぞれ EMQX コンソールと EMQX のリスニングポートに対応しています。

  ```bash
  $ kubectl get svc emqx-dashboard -o json | jq '.status.loadBalancer.ingress[0].ip'

  192.168.1.200
  ```

  ブラウザで `http://192.168.1.200:18083` にアクセスし、デフォルトのユーザー名とパスワード `admin/public` で EMQX コンソールにログインします。

## EMQX クラスターのパーシステンス検証

検証手順：1) 旧 EMQX ダッシュボードでテストルールを作成する。2) 旧クラスターを削除する。3) EMQX クラスターを再作成し、ダッシュボードで以前作成したルールが存在するか確認する。

+ ブラウザで EMQX ダッシュボードにアクセスし、テストルールを作成します。

  ```bash
  external_ip=$(kubectl get svc emqx-listeners -o json | jq '.status.loadBalancer.ingress[0].ip')
  ```

  `http://${external_ip}:18083` にアクセスして EMQX ダッシュボードにログインし、Data Integration → Rules をクリックしてルール作成ページに入ります。まず「Add a response action for this rule」ボタンをクリックしてアクションを追加し、「Create」をクリックしてルールを生成します。以下の図のようになります：

  ![](./assets/configure-emqx-persistent/emqx-core-action.png)

  ルールが正常に作成されると、ルールID: emqx-persistent-test のルールレコードがページに表示されます。以下の図をご覧ください：

  ![](./assets/configure-emqx-persistent/emqx-core-rule-old.png)

+ 旧 EMQX クラスターを削除します。

  以下のコマンドを実行して EMQX クラスターを削除します。

  ```bash
  $ kubectl delete -f emqx.yaml

  emqx.apps.emqx.io "emqx" deleted
  # emqxenterprise.apps.emqx.io "emqx" deleted
  ```

  > emqx-persistent.yaml は本記事で最初に EMQX クラスターをデプロイするために使用した YAML ファイルであり、このファイルは変更する必要はありません。

+ EMQX クラスターを再作成します。

  以下のコマンドを実行して EMQX クラスターを再作成します。

  ```bash
  $ kubectl apply -f emqx.yaml

  emqx.apps.emqx.io/emqx created
  # emqxenterprise.apps.emqx.io/emqx created
  ```

  EMQX クラスターの準備が整うまで待ち、ブラウザで EMQX ダッシュボードにアクセスして以前作成したルールが存在するか確認します。以下の図のようになります：

  ![](./assets/configure-emqx-persistent/emqx-core-rule-new.png)

  図から、旧クラスターで作成したルール emqx-persistent-test が新クラスターにも存在していることがわかります。これは構成したパーシステンスが有効であることを示しています。
