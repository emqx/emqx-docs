# EMQXクラスターでのパーシステンス有効化

## 目的

`persistentVolumeClaimSpec` フィールドを使用して、EMQXクラスターのCoreノードに対して永続ストレージを構成します。

## EMQXクラスターのパーシステンス設定

`apps.emqx.io/v3beta1` EMQX CRDの `.spec.coreTemplate.spec.persistentVolumeClaimSpec` フィールドは、各Coreノードの永続ストレージを構成します。このフィールドはKubernetesの `PersistentVolumeClaimSpec` スキーマおよびセマンティクスを使用します。

EMQX Operator 3.0は、単一のStatefulSetでCoreノードを管理します。各Core Podはイメージの更新やローリングアップデートを通じて安定したIDと安定したPVCを持ちます。このフィールドを設定すると、EMQX OperatorはEMQXコンテナの `/opt/emqx/data` ボリュームをPersistent Volume Claim（PVC）でバックアップされるように構成します。PVCは指定された[StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/)を使用してPersistent Volume（PV）をプロビジョニングします。

## PVCのライフサイクル

CoreノードのPVCはStatefulSetのPodオーディナルに紐づいています。例えば、`emqx-core-0` のPVCはイメージ更新やローリングアップデート中も `emqx-core-0` にアタッチされたままであり、ノードは同じデータボリュームを使い続けます。

EMQX Operatorは不要になったCoreノードのPVCをKubernetesに削除させるように設定しています。

- Coreノードをスケールダウンすると、削除されたPodオーディナルに対応するPVCが削除されます。

    例えば、Coreレプリカ数を5から3にスケールダウンすると、オーディナル3と4のPVCが削除されます。StatefulSetのスケールダウン前に、EMQX OperatorはDurable Storageのデータを該当Coreノードからリバランスし、データと耐久性を保護します。

- EMQXカスタムリソースを削除すると、KubernetesはCore StatefulSetとそれに関連付けられたPVCを削除します。

- ローリングアップデート中は、StatefulSetの名前とPodオーディナルが変わらないため、PVCは保持されます。

この自動クリーンアップはKubernetesの `StatefulSetAutoDeletePVC` フィーチャーゲートに依存しており、Kubernetes 1.27以降ではデフォルトで有効です。クラスター管理者がこのフィーチャーゲートを無効化している場合、Kubernetesは削除ポリシーを無視し、未使用のPVCを手動でクリーンアップする必要があります。

PVおよびPVCの詳細については、[Persistent Volumes](https://kubernetes.io/docs/concepts/storage/persistent-volumes/)のドキュメントを参照してください。

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply` でデプロイします。

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "..."
     coreTemplate:
       spec:
         persistentVolumeClaimSpec:
           storageClassName: standard
           resources:
             requests:
               storage: 1Gi
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

   ::: tip

   `storageClassName` フィールドを使って、EMQXデータに適した[StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/)を選択してください。`kubectl get storageclass` を実行すると、Kubernetesクラスターに既存のStorageClass一覧が表示されます。必要に応じてStorageClassを作成してください。

   :::

2. EMQXクラスターが準備完了になるまで待ちます。

   `kubectl get` でEMQXクラスターのステータスを確認し、`STATUS` が `Ready` になることを確認してください。準備完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## パーシステンスの検証

Core Podが置き換えられた際にKubernetesが同じPVCを再アタッチすることを検証します。このテストではEMQXリソースを削除しないでください。EMQX OperatorはStatefulSetが削除されると関連するPVCも削除するように構成しています。

1. 最初のCore PodにアタッチされているPVCのUIDを記録します。

   ```bash
   pvc_name=emqx-core-data-emqx-core-0
   pvc_uid_before=$(kubectl get pvc "${pvc_name}" -o jsonpath='{.metadata.uid}')
   kubectl get pvc "${pvc_name}"
   ```

2. Podを削除し、StatefulSetが再作成するまで待ちます。

   ```bash
   kubectl delete pod emqx-core-0
   kubectl wait --for=condition=Ready pod/emqx-core-0 --timeout=10m
   ```

3. Podが準備完了後のPVCのUIDを比較します。

   ```bash
   pvc_uid_after=$(kubectl get pvc "${pvc_name}" -o jsonpath='{.metadata.uid}')
   test "${pvc_uid_before}" = "${pvc_uid_after}" && echo "Core Podは同じPVCを再利用しました。"
   ```

UIDが一致すれば、Kubernetesが元のPVCを保持しており、置き換えられたPodが同じ永続ボリュームを引き続き使用していることが確認できます。
