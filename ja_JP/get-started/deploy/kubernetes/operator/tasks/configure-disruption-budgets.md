# Pod Disruption Budget の設定

EMQX Operator 3.0 は EMQX クラスターに対して Kubernetes の PodDisruptionBudget（PDB）を作成しません。このページでは、ノードのドレイン、クラスターオートスケーラーのスケールダウン、デスケジューラーによる追い出しなどの操作による任意の中断を制限するために、Core Pod と Replicant Pod に対して個別に PDB を作成する方法を説明します。

PDB は Kubernetes Eviction API を通じて要求される追い出しを制限します。強制的な障害、直接的な Pod 削除、または EMQX Operator がローリングアップデート中に管理する Pod の置き換えは防止しません。

## はじめに

この例では、現在の `kubectl` のネームスペースに `emqx` という名前の EMQX リソースが存在すると仮定しています。リソース名が異なる場合は、`apps.emqx.io/instance` セレクターや EMQX リソースコマンド内の `emqx` を `metadata.name` に置き換えてください。各 PDB は対応する EMQX リソースと同じネームスペースに作成してください。PDB リソース名の `emqx-core` と `emqx-replicant` は例です。変更した場合は、PDB の確認コマンドでも新しい名前を使用してください。

EMQX Operator は EMQX のステータスに現在の Pod セレクターを公開しています：

- `status.coreSelector`
- `status.replicantSelector`

`emqx` という名前の EMQX クラスターの場合、これらのセレクターは以下の安定したラベルを使用します：

```yaml
apps.emqx.io/instance: emqx # 対応する EMQX CR の metadata.name を使用してください。
apps.emqx.io/managed-by: emqx-operator
apps.emqx.io/db-role: core # Replicant Pod には replicant を使用します。
```

各ロールの Pod 数と可用性要件に基づいて、`minAvailable` または `maxUnavailable` を選択してください。例えば、Pod が 1 つだけのロールに対して `maxUnavailable: 1` を設定すると可用性は維持されません。任意の中断でその Pod を削除してはならない場合は `minAvailable: 1` を使用してください。

## PodDisruptionBudget の作成

1. 以下のリソースを `emqx-pdb.yaml` として保存します。

   この例では、Core Pod と Replicant Pod を別々の可用性プールとして保護するために 2 つの PDB を定義しています。1 つの PDB が両方のロールにマッチすると、片方のロールでの追い出しがもう片方のロールの中断容量を消費してしまいます。クラスターで Replicant Pod を使用していない場合は、`emqx-replicant` PDB を省略してください。

   ```yaml
   apiVersion: policy/v1
   kind: PodDisruptionBudget
   metadata:
     name: emqx-core
   spec:
     maxUnavailable: 1
     selector:
       matchLabels:
         apps.emqx.io/instance: emqx
         apps.emqx.io/managed-by: emqx-operator
         apps.emqx.io/db-role: core
   ---
   apiVersion: policy/v1
   kind: PodDisruptionBudget
   metadata:
     name: emqx-replicant
   spec:
     maxUnavailable: 1
     selector:
       matchLabels:
         apps.emqx.io/instance: emqx
         apps.emqx.io/managed-by: emqx-operator
         apps.emqx.io/db-role: replicant
   ```

2. PDB を適用します：

   ```bash
   kubectl apply -f emqx-pdb.yaml
   ```

## PodDisruptionBudget の確認

1. EMQX Operator が報告するセレクターを確認します：

   ```bash
   kubectl get emqx emqx -o jsonpath='{.status.coreSelector}{"\n"}{.status.replicantSelector}{"\n"}'
   ```

2. 各セレクターが期待する Pod にマッチしているか確認します。クラスターで Replicant Pod を使用していない場合は、2 番目のコマンドは省略してください。

   ```bash
   kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/managed-by=emqx-operator,apps.emqx.io/db-role=core'
   kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/managed-by=emqx-operator,apps.emqx.io/db-role=replicant'
   ```

3. 各 PDB を確認し、許可されている中断数をレビューします。クラスターで Replicant Pod を使用していない場合は、`emqx-replicant` とその `describe` コマンドは省略してください。

   ```bash
   kubectl get pdb emqx-core emqx-replicant
   kubectl describe pdb emqx-core
   kubectl describe pdb emqx-replicant
   ```

   制限の厳しい PDB はノードのドレインを無期限にブロックする可能性があります。各 PDB の `status.disruptionsAllowed` の値を確認し、メンテナンスを開始する前に十分な数のマッチする Pod が準備完了していることを確認してください。
