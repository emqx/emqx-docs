# EMQX Operator 2.3 から 3.0 への移行

Operator 3.0 は `apps.emqx.io/v3beta1` API を使用します。以前の EMQX カスタムリソースを変換したり、Operator 2.3 によって作成されたワークロードを引き継ぐことはできません。そのため、この移行では既存のクラスターをその場で更新するのではなく、新しい EMQX クラスターをデプロイします。古いクラスターと新しいクラスターは結合されません。

::: warning

この手順では MQTT セッションは移行されません。メンテナンスウィンドウを計画し、クライアントが新しいクラスターに再接続できることを確認してください。ライブ接続、セッション状態、オフラインキュー、インフライトメッセージ、および Durable Storage のデータは EMQX のデータバックアップに含まれません。

:::

この手順では、Operator 3.0 をインストールし、別名の EMQX クラスターを検証する間、古いワークロードはロールバック用に残します。検証後、クライアントトラフィックを新しいクラスターに切り替え、古いワークロードを廃止します。

以下の例では次の名前を使用しています。ご自身のデプロイメントの名前とネームスペースに置き換えてください：

```bash
export EMQX_NAMESPACE=default
export OLD_EMQX=my-emqx
export NEW_EMQX=my-emqx-v3
```

## 移行の準備

開始前に以下を行ってください：

- 本番環境に近い環境で手順をリハーサルしてください。
- EMQX イメージとバージョンは変更しないでください。新しいクラスターが Operator 3.0 で稼働した後にのみ EMQX をアップグレードしてください。
- Kubernetes クラスターに古い EMQX ワークロードと新しい EMQX ワークロードを同時に実行できる十分な容量があることを確認してください。
- ロードバランサー、イングレス、DNS 変更などを通じて、古いリスナー Service から新しいリスナー Service へクライアントを誘導する方法を計画してください。
- 最終バックアップを取得する前に設定やアプリケーションデータの変更を停止してください。以降に行われた変更は新しいクラスターにコピーされません。
- Durable Storage を使用していないことを確認してください。使用している場合は、この手順では移行されないため、データ損失が許容できる場合のみ続行してください。

EMQX CRD はクラスター全体にスコープされています。Operator 2.3 の CRD を削除する前に、すべての EMQX リソースをリストアップしてください：

```bash
kubectl get emqx.apps.emqx.io --all-namespaces
```

リストアップされたすべてのリソースは同じメンテナンスイベントに含める必要があります。Operator 2.3 のクラスターがまだ依存している間は CRD を削除しないでください。

## 既存クラスターのバックアップ

既存のカスタムリソースとそのワークロードの一覧を保存します。これらのファイルは Kubernetes クラスター外に保管し、ロールバックに使用できるようにしてください：

```bash
kubectl get emqx.apps.emqx.io "$OLD_EMQX" \
  -n "$EMQX_NAMESPACE" -o yaml > emqx-v2.yaml

kubectl get statefulset,replicaset,pod,service,pvc \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX" -o yaml \
  > emqx-v2-workloads.yaml
```

稼働中の Core Pod を選択し、その `/tmp` ディレクトリにグローバルな EMQX データバックアップを作成します：

```bash
OLD_EMQX_CORE_POD="$(kubectl get pod \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX,apps.emqx.io/db-role=core" \
  --field-selector=status.phase=Running \
  -o jsonpath='{.items[0].metadata.name}')"

kubectl exec -n "$EMQX_NAMESPACE" "$OLD_EMQX_CORE_POD" -c emqx -- \
  emqx ctl data export --dir /tmp
```

エクスポートコマンドは生成されたアーカイブのパスを出力します。`EMQX_BACKUP_FILE` にそのファイル名を設定し、Pod からアーカイブをコピーして検証してください：

```bash
export EMQX_BACKUP_FILE='<exported-file-name>.tar.gz'

kubectl cp -c emqx \
  "$EMQX_NAMESPACE/$OLD_EMQX_CORE_POD:/tmp/$EMQX_BACKUP_FILE" \
  "./$EMQX_BACKUP_FILE"

chmod 600 "./$EMQX_BACKUP_FILE"
test -s "./$EMQX_BACKUP_FILE"
tar -tzf "./$EMQX_BACKUP_FILE" >/dev/null
```

あるいは、ダッシュボードの **System** -> **Backup & Restore** からグローバルバックアップを作成してダウンロードすることも可能です。詳細は [Backup and Restore](../../../../../guides/backup-restore.md) を参照してください。Operator が生成した API キーは使用しないでください。API キー認証されたエクスポートではダッシュボードユーザーや API キーが省略されます。

バックアップにはサポートされている設定、EMQX データディレクトリのファイル、認証レコード、API キー、保持メッセージなどの組み込みデータベースデータが含まれます。上記の警告にあるライブ MQTT 状態は含まれません。続行前にテスト環境で復元してアーカイブを検証することを推奨します。

## EMQX マニフェストの変換

`emqx-v3.yaml` という名前で新しいマニフェストを作成します。Operator 3.0 が古い Operator 2.3 のワークロードを自身のリソースと誤認しないように、`metadata.name` は `$NEW_EMQX` など異なる名前にしてください。

マニフェストに以下の変更を適用してください：

| Operator 2.3 の設定 | Operator 3.0 での変更 | 必要な対応 |
| --- | --- | --- |
| `apiVersion: apps.emqx.io/v2` | `apps.emqx.io/v3beta1` に置き換え | マニフェストの `apiVersion` を変更する。 |
| `.spec.config.data` | `.spec.config.roots` に置き換え | リテラルの HOCON を構造化された YAML に変換する。 |
| `.spec.coreTemplate.spec.volumeClaimTemplates` | `.spec.coreTemplate.spec.persistentVolumeClaimSpec` に名称変更 | フィールド名を変更し、ストレージ設定を保持する。 |
| `.spec.coreTemplate.spec.replicas` | デフォルトが `2` から `1` に変更 | 意図するレプリカ数を明示的に設定する。Replicant が有効な場合は Core レプリカを少なくとも 2 つ設定する。 |
| `.spec.replicantTemplate.spec.replicas` | デフォルトが `2` から `0` に変更 | 意図するレプリカ数を明示的に設定する。 |
| `.spec.bootstrapAPIKeys` | 削除 | 既存の API キーはグローバルバックアップから復元し、その後 EMQX で管理する。 |
| `.spec.updateStrategy.initialDelaySeconds` | 削除（直接の置換なし） | フィールドを削除し、ロールアウトのタイミングを見直す。意味が異なるため `minReadySeconds` にマッピングしない。 |
| `.spec.updateStrategy.evacuationStrategy.connEvictRate` | `.spec.updateStrategy.evacuationStrategy.connectionEvictionRate` に名称変更 | フィールド名を変更し、値を保持する。 |
| `.spec.updateStrategy.evacuationStrategy.sessEvictRate` | `.spec.updateStrategy.evacuationStrategy.sessionEvictionRate` に名称変更 | フィールド名を変更し、値を保持する。 |
| `.spec.coreTemplate.spec.minAvailable`<br/>`.spec.coreTemplate.spec.maxUnavailable`<br/>`.spec.replicantTemplate.spec.minAvailable`<br/>`.spec.replicantTemplate.spec.maxUnavailable` | 削除 | 移行後に別途 PDB を作成する。詳細は [Configure Pod Disruption Budgets](../tasks/configure-disruption-budgets.md) を参照。 |
| `Rebalance` リソース | 削除 | Operator 2.3 CRD を削除する前に既存の `Rebalance` リソースを削除する。 |

例えば、Operator 2.3 の設定例：

```yaml
spec:
  config:
    mode: Merge
    data: |
      log.console.level = warning
      dashboard.listeners.http.bind = 18083
```

は Operator 3.0 では以下のように変換します：

```yaml
spec:
  config:
    roots:
      log:
        console:
          level: warning
      dashboard:
        listeners:
          http:
            bind: 18083
```

設定の詳細は [Configure EMQX](../tasks/configure-emqx-config.md) を参照してください。

カスタムの init コンテナを使用している場合、root 権限が必要なものは `initContainers` エントリで `securityContext.runAsNonRoot` を `false`、`securityContext.runAsUser` を `0` に明示的に設定してください。

新しいクラスターをデプロイする前に、古い Operator のブートストラップ API キー Secret を新しい EMQX リソースが期待する名前にコピーしてください：

```bash
kubectl get secret "$OLD_EMQX-bootstrap-api-key" \
  -n "$EMQX_NAMESPACE" -o jsonpath='{.data.bootstrap_api_key}' \
  | base64 --decode \
  | kubectl create secret generic "$NEW_EMQX-bootstrap-api-key" \
      -n "$EMQX_NAMESPACE" \
      --from-file=bootstrap_api_key=/dev/stdin
```

グローバルバックアップには Operator コントローラーの API キー情報が含まれています。ブートストラップ Secret を再利用することで、復元時に認証情報の整合性が保たれます。古い node-cookie Secret をコピーしたり、`node.cookie` を設定したりしないでください。Operator 3.0 はこの手順で使用する独立したクラスター用に新しい cookie を作成します。

## Operator 2.3 の停止とワークロードの保持

Operator 2.3 のコントローラーをレプリカ数ゼロにスケールダウンします。カスタムインストールを使用している場合はネームスペースや Deployment 名を調整してください：

```bash
kubectl scale deployment emqx-operator-controller-manager \
  -n emqx-operator-system --replicas=0
```

オーファンプロパゲーションを指定して、Operator 2.3 の EMQX リソースを削除します。これによりカスタムリソースは削除されますが、StatefulSet、ReplicaSet、Pod、Service、PVC は残ります：

```bash
kubectl delete emqx.apps.emqx.io "$OLD_EMQX" \
  -n "$EMQX_NAMESPACE" --cascade=orphan --wait=true
```

古いリソースとクライアントエンドポイントが利用可能なままであることを確認してください：

```bash
kubectl get statefulset,replicaset,pod,service,pvc \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX"
```

Operator 2.3 をアンインストールし、CRD を削除します。インストール時と同じ方法を使用してください：

```bash
kubectl delete --ignore-not-found crd \
  emqxes.apps.emqx.io rebalances.apps.emqx.io
```

Operator 3.0 を [Install Operator and Deploy EMQX](../getting-started.md) に従ってインストールしますが、そのページのサンプル EMQX リソースはデプロイしないでください。

## 新しいクラスターのデプロイと復元

変換済みマニフェストを検証し、適用します：

```bash
kubectl apply --dry-run=server -f emqx-v3.yaml
kubectl apply -f emqx-v3.yaml
```

ワークロードと設定が準備完了になるまで待機します：

```bash
kubectl wait emqx.apps.emqx.io/"$NEW_EMQX" \
  -n "$EMQX_NAMESPACE" --for=condition=Ready --timeout=15m

kubectl wait emqx.apps.emqx.io/"$NEW_EMQX" \
  -n "$EMQX_NAMESPACE" --for=condition=ConfigApplied --timeout=15m
```

稼働中の Core Pod を選択し、バックアップをコピーしてデータを復元します：

```bash
NEW_EMQX_CORE_POD="$(kubectl get pod \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$NEW_EMQX,apps.emqx.io/db-role=core" \
  --field-selector=status.phase=Running \
  -o jsonpath='{.items[0].metadata.name}')"

kubectl cp -c emqx \
  "./$EMQX_BACKUP_FILE" \
  "$EMQX_NAMESPACE/$NEW_EMQX_CORE_POD:/tmp/$EMQX_BACKUP_FILE"

kubectl exec -n "$EMQX_NAMESPACE" "$NEW_EMQX_CORE_POD" -c emqx -- \
  emqx ctl data import "/tmp/$EMQX_BACKUP_FILE"

kubectl exec -n "$EMQX_NAMESPACE" "$NEW_EMQX_CORE_POD" -c emqx -- \
  rm -f "/tmp/$EMQX_BACKUP_FILE"
```

変換済みの `.spec.config.roots` を設定の真実の情報源として保持してください。再度準備完了チェックを実行し、インポートが正常に完了したことを確認してからクライアントトラフィックを切り替えてください。

## クライアントトラフィックの切り替え

新しいリスナー Service に対して代表的な接続、認証、パブリッシュ、サブスクライブ、保持メッセージ、ルール、統合のテストを実施してください。その後、ロードバランサー、イングレス、DNS レコードを更新し、新しいクラスターの `<new-emqx-name>-listeners` に新規クライアント接続を送るようにしてください。

クライアントが固定の NodePort 経由で接続している場合は、古い Service からポートを解放し、`.spec.listenersServiceTemplate` と `.spec.dashboardServiceTemplate` を通じて新しい Operator 管理の Service に割り当ててください。この切り替えは一時的にトラフィックを中断する可能性があります。

トラフィックの切り替えは新規接続に影響します。既存の接続は古いクラスターに残ります。

この切り替えをセッション境界として扱ってください。クライアントは切断され、新しいクラスターに再接続する必要があります。クライアントの再接続動作を検証し、認証失敗、再接続ループ、メッセージフローを監視してから続行してください。

## 移行の完了

受け入れ期間の後に以下を実施してください：

1. 再接続の急増を避けるために古いクライアントを制御された速度で切断し、新しいクラスターへの再接続が成功することを確認する。
2. オーファンとなった Operator 2.3 の StatefulSet と ReplicaSet をリストアップし、それぞれをレプリカ数ゼロにスケールダウンする。
3. Operator 3.0 クラスターから新しい EMQX データバックアップを取得する。
4. ロールバック期間が終了した後にのみ、オーファンとなった Operator 2.3 のワークロードを削除する。

必要に応じて、コピーしたブートストラップ API キー Secret に新しい EMQX リソースのオーナーリファレンスを追加してください。

新しいクラスターを検証し、両方のバックアップを Kubernetes 外部に保存するまでは、古い PVC や node-cookie Secret を削除しないでください。これらのリソースの削除は元に戻せません。

## ロールバック

古い StatefulSet、ReplicaSet、PVC、設定リソース、Service、Secret がまだ存在することを確認してください。

古いワークロードがまだ稼働している場合は、古いリスナー Service をテストし、クライアントトラフィックを元に戻してください。

すでに古いワークロードをレプリカ数ゼロにスケールダウンしている場合は、以下の手順でブルーグリーンアップデートにより Operator 2.3 に戻します：

1. 移行前に稼働していた StatefulSet と ReplicaSet を、`emqx-v2-workloads.yaml` を参照して以前のレプリカ数に戻してスケールアップする。

2. 古い Core Pod で EMQX が起動するまで待機する：

   ```bash
   kubectl exec -n "$EMQX_NAMESPACE" <old-core-pod> -c emqx -- \
     emqx ctl status
   ```

   再作成された Pod は Operator 2.3 が管理するレディネスゲートのために未準備状態のままでも構いません。まだクライアントトラフィックを切り替えないでください。

3. インストール時と同じ方法で Operator 3.0 をアンインストールします。これにより新しい EMQX リソース、その管理ワークロード、および EMQX CRD が削除されます。CRD を別途インストールしていた場合は続行前に削除してください。

4. 古いクラスターを管理していた同じ Operator 2.3 バージョンを再インストールし、保存しておいたリソースを再適用します：

   ```bash
   kubectl apply -f emqx-v2.yaml
   ```

5. 復元したリソースが準備完了になるまで待機し、古いリスナー Service をテストしてからトラフィックを切り替えます：

   ```bash
   kubectl wait emqx.apps.emqx.io/"$OLD_EMQX" \
     -n "$EMQX_NAMESPACE" --for=condition=Ready --timeout=15m
   ```

Operator 2.3 は古いワークロードを厳密に引き継ぐわけではありません。稼働中のワークロードをブルーグリーンアップデートの開始リビジョンとして使用し、完全なデータ移行を実行します。
