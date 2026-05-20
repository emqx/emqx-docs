# API リファレンス (v2)

## パッケージ
- [apps.emqx.io/v2](#appsemqxiov2)


## apps.emqx.io/v2

package v2 は apps v2 API グループの API スキーマ定義を含みます。

### リソースタイプ
- [EMQX](#emqx)



#### BootstrapAPIKey







_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |
| `secret` _string_ |  |  | 最大長: 128 <br />最小長: 3 <br /> |
| `secretRef` _[SecretRef](#secretref)_ | EMQX API キーを含む Secret エントリへの参照。 |  |  |


#### Config







_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `mode` _string_ | 設定更新の適用方法を決定します。<br />* `Merge`: 新しい設定を既存の設定にマージします。<br />* `Replace`: 設定全体を置き換えます。 | Merge | 列挙型: [Merge Replace] <br /> |
| `data` _string_ | HOCON 形式の EMQX 設定。<br />この設定はコンテナに `base.hocon` として提供されます。詳細は<br />[ドキュメント](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#base-configuration-file)を参照してください。 |  |  |


#### DSDBReplicationStatus







_出現箇所:_
- [DSReplicationStatus](#dsreplicationstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `name` _string_ | データベース名 |  |  |
| `numShards` _integer_ | データベースのシャード数 |  |  |
| `numShardReplicas` _integer_ | シャードレプリカの総数 |  |  |
| `lostShardReplicas` _integer_ | 失われたサイトに属するシャードレプリカの総数 |  |  |
| `numTransitions` _integer_ | 現在のシャード所有権の遷移数 |  |  |
| `minReplicas` _integer_ | データベースシャードの最小レプリケーション係数 |  |  |
| `maxReplicas` _integer_ | データベースシャードの最大レプリケーション係数 |  |  |


#### DSReplicationStatus



データベースごとの DS レプリケーション状態の概要。



_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `dbs` _[DSDBReplicationStatus](#dsdbreplicationstatus) 配列_ |  |  |  |


#### EMQX



EMQX クラスターを表すカスタムリソース。





| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | EMQX クラスターの望ましい状態の仕様。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | EMQX クラスターの現在の状態。 |  |  |


#### EMQXCoreTemplate







_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | コアノードの望ましい状態の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXCoreTemplateSpec







_出現箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Pod がノードに適合するために満たす必要があるセレクター。<br />Pod がそのノードにスケジュールされるには、ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | この Pod を特定のノードにスケジュールするリクエスト。<br />空でなければ、スケジューラーはリソース要件を満たすと仮定して単純にこのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Pod 割り当てのためのアフィニティ。<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) 配列_ | Pod のトレランス。<br />指定された場合、Pod は <key,value,effect> の組み合わせにマッチする任意のテイントをトレランスします。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) 配列_ | 指定されたトポロジー間でマッチする Pod をどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | インスタンスの望ましい数。<br />コアノードの場合、各インスタンスは一貫した識別子を持ちます。 | 2 | 最小値: 0 <br /> |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector" で選択された Pod のうち、少なくとも "minAvailable" がエビクション後も利用可能である場合にエビクションを許可します。<br />例: "100%" を指定するとすべての任意のエビクションを防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector" で選択された Pod のうち、エビクション後に最大 "maxUnavailable" までが利用不可である場合にエビクションを許可します。<br />例: 0 を指定するとすべての任意のエビクションを防止できます。これは "minAvailable" と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string 配列_ | エントリポイントの配列。シェル内で実行されません。<br />指定されない場合はコンテナイメージの ENTRYPOINT が使用されます。<br />環境変数を使った変数参照 `$(VAR_NAME)` はコンテナの環境で展開されます。解決できない場合は文字列は変更されません。`$$` は `$` に変換され、`$(VAR_NAME)` のエスケープが可能です。エスケープされた参照は展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string 配列_ | エントリポイントへの引数。<br />指定されない場合はコンテナイメージの CMD が使用されます。<br />変数参照は `command` と同様に展開されます。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) 配列_ | コンテナから公開するポートのリスト。<br />ここでポートを公開することはネットワーク接続に関する追加情報をシステムに提供しますが、主に情報的なものです。ここでポートを指定しなくても、そのポートが公開されることは妨げられません。コンテナ内で `0.0.0.0` アドレスでリッスンしているポートはネットワークからアクセス可能です。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) 配列_ | コンテナに設定する環境変数のリスト。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) 配列_ | コンテナの環境変数を設定するためのソースのリスト。<br />ソース内のキーは C_IDENTIFIER である必要があります。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合は最後のソースの値が優先されます。<br />Env で定義された重複キーの値が優先されます。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Pod レベルのセキュリティ属性および共通コンテナ設定。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | コンテナの実行に使用するセキュリティオプション。<br />設定されている場合、SecurityContext のフィールドは PodSecurityContext の同等のフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | Pod に属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。いずれかの初期化コンテナが失敗すると Pod は失敗とみなされ、restartPolicy に従って処理されます。初期化コンテナと通常コンテナの名前は全て一意である必要があります。<br />初期化コンテナは Lifecycle アクション、Readiness プローブ、Liveness プローブ、Startup プローブを持てません。<br />初期化コンテナの resourceRequirements はスケジューリング時に考慮され、各リソースタイプの最大要求/制限を通常コンテナの合計と比較して最大値が使用されます。制限も同様に適用されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | メインコンテナと並行して実行する追加コンテナ。 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) 配列_ | Pod に提供する追加ボリューム。 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) 配列_ | 追加ボリュームをメインコンテナにマウントする方法を指定。 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナの生存確認を定期的に行うプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナのサービス準備完了を定期的に確認するプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Pod が正常に初期化されたことを示すプローブ。<br />指定された場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、`livenessProbe` が失敗した場合と同様に Pod は再起動されます。<br />Pod のライフサイクル開始時にデータのロードやキャッシュのウォームアップに時間がかかる場合に、通常時とは異なるパラメータを設定可能です。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに応じて管理システムが実行すべきアクション。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#persistentvolumeclaimspec-v1-core)_ | コアノードのデータストレージ用 PVC の仕様。<br />注意: このフィールド名は一貫しておらず、実際には単なる `PersistentVolumeClaimSpec` です。 |  |  |


#### EMQXNode







_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `name` _string_ | ノード名 |  |  |
| `podName` _string_ | 対応する Pod 名 |  |  |
| `status` _string_ | ノードの状態 |  |  |
| `otpRelease` _string_ | ノードが動作している Erlang/OTP バージョン |  |  |
| `version` _string_ | EMQX バージョン |  |  |
| `role` _string_ | ノードの役割。 "core" または "replicant" |  |  |
| `sessions` _integer_ | MQTT セッション数 |  |  |
| `connections` _integer_ | 接続中の MQTT クライアント数 |  |  |


#### EMQXNodesStatus







_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `replicas` _integer_ | レプリカの合計数。 |  |  |
| `readyReplicas` _integer_ | 準備完了のレプリカ数。 |  |  |
| `currentRevision` _string_ | 対応するコアまたはレプリカントセットの現在のリビジョン。 |  |  |
| `currentReplicas` _integer_ | 現在のリビジョンを実行しているレプリカ数。 |  |  |
| `updateRevision` _string_ | 対応するコアまたはレプリカントセットの更新リビジョン。<br />現在のリビジョンと異なる場合、セットは更新中です。 |  |  |
| `updateReplicas` _integer_ | 更新リビジョンを実行しているレプリカ数。 |  |  |
| `collisionCount` _integer_ |  |  |  |


#### EMQXReplicantTemplate







_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | レプリカントノードの望ましい状態の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXReplicantTemplateSpec







_出現箇所:_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Pod がノードに適合するために満たす必要があるセレクター。<br />Pod がそのノードにスケジュールされるには、ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | この Pod を特定のノードにスケジュールするリクエスト。<br />空でなければ、スケジューラーはリソース要件を満たすと仮定して単純にこのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Pod 割り当てのためのアフィニティ。<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) 配列_ | Pod のトレランス。<br />指定された場合、Pod は <key,value,effect> の組み合わせにマッチする任意のテイントをトレランスします。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) 配列_ | 指定されたトポロジー間でマッチする Pod をどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | インスタンスの望ましい数。<br />コアノードの場合、各インスタンスは一貫した識別子を持ちます。 | 2 | 最小値: 0 <br /> |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector" で選択された Pod のうち、少なくとも "minAvailable" がエビクション後も利用可能である場合にエビクションを許可します。<br />例: "100%" を指定するとすべての任意のエビクションを防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector" で選択された Pod のうち、エビクション後に最大 "maxUnavailable" までが利用不可である場合にエビクションを許可します。<br />例: 0 を指定するとすべての任意のエビクションを防止できます。これは "minAvailable" と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string 配列_ | エントリポイントの配列。シェル内で実行されません。<br />指定されない場合はコンテナイメージの ENTRYPOINT が使用されます。<br />環境変数を使った変数参照 `$(VAR_NAME)` はコンテナの環境で展開されます。解決できない場合は文字列は変更されません。`$$` は `$` に変換され、`$(VAR_NAME)` のエスケープが可能です。エスケープされた参照は展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string 配列_ | エントリポイントへの引数。<br />指定されない場合はコンテナイメージの CMD が使用されます。<br />変数参照は `command` と同様に展開されます。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) 配列_ | コンテナから公開するポートのリスト。<br />ここでポートを公開することはネットワーク接続に関する追加情報をシステムに提供しますが、主に情報的なものです。ここでポートを指定しなくても、そのポートが公開されることは妨げられません。コンテナ内で `0.0.0.0` アドレスでリッスンしているポートはネットワークからアクセス可能です。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) 配列_ | コンテナに設定する環境変数のリスト。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) 配列_ | コンテナの環境変数を設定するためのソースのリスト。<br />ソース内のキーは C_IDENTIFIER である必要があります。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合は最後のソースの値が優先されます。<br />Env で定義された重複キーの値が優先されます。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Pod レベルのセキュリティ属性および共通コンテナ設定。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | コンテナの実行に使用するセキュリティオプション。<br />設定されている場合、SecurityContext のフィールドは PodSecurityContext の同等のフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | Pod に属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。いずれかの初期化コンテナが失敗すると Pod は失敗とみなされ、restartPolicy に従って処理されます。初期化コンテナと通常コンテナの名前は全て一意である必要があります。<br />初期化コンテナは Lifecycle アクション、Readiness プローブ、Liveness プローブ、Startup プローブを持てません。<br />初期化コンテナの resourceRequirements はスケジューリング時に考慮され、各リソースタイプの最大要求/制限を通常コンテナの合計と比較して最大値が使用されます。制限も同様に適用されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | メインコンテナと並行して実行する追加コンテナ。 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) 配列_ | Pod に提供する追加ボリューム。 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) 配列_ | 追加ボリュームをメインコンテナにマウントする方法を指定。 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナの生存確認を定期的に行うプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナのサービス準備完了を定期的に確認するプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Pod が正常に初期化されたことを示すプローブ。<br />指定された場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、`livenessProbe` が失敗した場合と同様に Pod は再起動されます。<br />Pod のライフサイクル開始時にデータのロードやキャッシュのウォームアップに時間がかかる場合に、通常時とは異なるパラメータを設定可能です。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに応じて管理システムが実行すべきアクション。 |  |  |


#### EMQXSpec



EMQXSpec は EMQX の望ましい状態を定義します。



_出現箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `image` _string_ | EMQX コンテナイメージ。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#pullpolicy-v1-core)_ | コンテナイメージのプルポリシー。<br />`Always`、`Never`、`IfNotPresent` のいずれか。<br />`:latest` タグが指定されている場合はデフォルトで `Always`、それ以外は `IfNotPresent`。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#localobjectreference-v1-core) 配列_ | この PodSpec で使用されるイメージをプルするために同じネームスペース内の Secret への参照の任意リスト。<br />指定された場合、これらの Secret は個々のプラー実装に渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | ServiceAccount 名。<br />管理された ReplicaSets および StatefulSets は認証のために指定された ServiceAccount に関連付けられます。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) 配列_ | EMQX API にアクセスするためのブートストラップ API キー。<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQX 設定。 |  |  |
| `clusterDomain` _string_ | Kubernetes クラスターのドメイン。 | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古い ReplicaSets、古い StatefulSets、古い PersistentVolumeClaims の数。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | クラスターアップグレード戦略の設定。 | \{ type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | EMQX コアノードを実行する Pod のテンプレート。 | \{ spec:map[replicas:2] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | EMQX レプリカントノードを実行する Pod のテンプレート。 |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | EMQX ダッシュボードを公開する Service のテンプレート。<br />ダッシュボード Service は常に EMQX コアノードのセットを指します。 |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | 有効な EMQX リスナーを公開する Service のテンプレート。<br />リスナー Service は有効かつ存在する場合は EMQX レプリカントノードのセットを指します。<br />そうでなければ、EMQX コアノードのセットを指します。 |  |  |


#### EMQXStatus



EMQXStatus は EMQX の観測された状態を定義します。



_出現箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#condition-v1-meta) 配列_ | EMQX カスタムリソースの現在の状態を表す条件。 |  |  |
| `coreNodes` _[EMQXNode](#emqxnode) 配列_ | クラスター内の各コアノードの状態。 |  |  |
| `coreNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ | コアノードセットの概要状態。 |  |  |
| `replicantNodes` _[EMQXNode](#emqxnode) 配列_ | クラスター内の各レプリカントノードの状態。 |  |  |
| `replicantNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ | レプリカントノードセットの概要状態。 |  |  |
| `nodeEvacuationsStatus` _[NodeEvacuationStatus](#nodeevacuationstatus) 配列_ | クラスター内のアクティブなノード避難の状態。 |  |  |
| `dsReplication` _[DSReplicationStatus](#dsreplicationstatus)_ | EMQX Durable Storage レプリケーションの状態。 |  |  |


#### EvacuationStrategy







_出現箇所:_
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `connEvictRate` _integer_ | クライアント切断率（秒あたりの数）。<br />[EMQX ノード避難](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation) の `conn-evict-rate` と同じ。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | セッション避難率（秒あたりの数）。<br />[EMQX ノード避難](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation) の `sess-evict-rate` と同じ。 | 1000 | 最小値: 1 <br /> |
| `waitTakeover` _integer_ | セッション避難開始までの待機時間（秒）。<br />[EMQX ノード避難](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation) の `wait-takeover` と同じ。 | 10 | 最小値: 0 <br /> |
| `waitHealthCheck` _integer_ | ノードがロードバランサーのアクティブなバックエンドノードリストから削除されるのを待つ時間（秒）。<br />[EMQX ノード避難](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation) の `wait-health-check` と同じ。 | 60 | 最小値: 0 <br /> |


#### KeyRef







_出現箇所:_
- [SecretRef](#secretref)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `secretName` _string_ | Secret オブジェクトの名前。 |  |  |
| `secretKey` _string_ | Secret データ内のエントリ。 |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |


#### NodeEvacuationStatus







_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeName` _string_ | 避難中のノード名 |  |  |
| `state` _string_ | 避難状態 |  |  |
| `sessionRecipients` _string 配列_ | セッション受信者 |  |  |
| `sessionEvictionRate` _integer_ | セッション避難率（秒あたりのセッション数）。 |  |  |
| `connectionEvictionRate` _integer_ | 接続避難率（秒あたりの接続数）。 |  |  |
| `initialSessions` _integer_ | このノード上の初期セッション数 |  |  |
| `initialConnections` _integer_ | このノードへの初期接続数 |  |  |


#### SecretRef







_出現箇所:_
- [BootstrapAPIKey](#bootstrapapikey)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _[KeyRef](#keyref)_ | EMQX API キーを含む Secret エントリへの参照。 |  |  |
| `secret` _[KeyRef](#keyref)_ | EMQX API キーのシークレットを含む Secret エントリへの参照。 |  |  |


#### ServiceTemplate







_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `enabled` _boolean_ | Service を作成するかどうかを指定します。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#servicespec-v1-core)_ | Service の望ましい状態の仕様。<br />https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### UpdateStrategy







_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _string_ | クラスターアップグレードの実行方法を決定します。<br />* `Recreate`: ブルーグリーンアップグレードを実行。 | Recreate | 列挙型: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | 接続避難開始までの秒数。 | 10 | 最小値: 0 <br /> |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 避難戦略の設定。 |  |  |
