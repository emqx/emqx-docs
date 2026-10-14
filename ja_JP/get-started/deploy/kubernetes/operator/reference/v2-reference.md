# APIリファレンス (v2)

## パッケージ
- [apps.emqx.io/v2](#appsemqxiov2)


## apps.emqx.io/v2

package v2 は apps v2 APIグループのAPIスキーマ定義を含みます。

### リソースタイプ
- [EMQX](#emqx)


#### BootstrapAPIKey







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |
| `secret` _string_ |  |  | 最大長: 128 <br />最小長: 3 <br /> |
| `secretRef` _[SecretRef](#secretref)_ | EMQX APIキーを含むSecretエントリへの参照。 |  |  |


#### Config







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `mode` _string_ | 設定更新の適用方法を決定します。<br />* `Merge`: 新しい設定を既存の設定にマージします。<br />* `Replace`: 設定全体を置き換えます。 | Merge | 列挙型: [Merge Replace] <br /> |
| `data` _string_ | HOCON形式のEMQX設定。<br />この設定はコンテナに `base.hocon` として提供されます。詳細は<br />[ドキュメント](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#base-configuration-file)を参照してください。 |  |  |


#### DSDBReplicationStatus







_登場箇所:_
- [DSReplicationStatus](#dsreplicationstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `name` _string_ | データベース名 |  |  |
| `numShards` _integer_ | データベースのシャード数 |  |  |
| `numShardReplicas` _integer_ | シャードレプリカの総数 |  |  |
| `lostShardReplicas` _integer_ | 失われたサイトに属するシャードレプリカの総数 |  |  |
| `numTransitions` _integer_ | 現在のシャード所有権の遷移数 |  |  |
| `minReplicas` _integer_ | データベースシャード間の最小レプリケーション係数 |  |  |
| `maxReplicas` _integer_ | データベースシャード間の最大レプリケーション係数 |  |  |


#### DSReplicationStatus



データベースごとのDSレプリケーション状況の概要。



_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `dbs` _[DSDBReplicationStatus](#dsdbreplicationstatus) 配列_ |  |  |  |


#### EMQX



EMQXクラスターを表すカスタムリソース。





| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | EMQXクラスターの望ましい状態の仕様。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | EMQXクラスターの現在の状態。 |  |  |


#### EMQXCoreTemplate







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | コアノードの望ましい状態の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXCoreTemplateSpec







_登場箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Podがノードに適合するために満たす必要があるセレクター。<br />Podがそのノードにスケジュールされるには、ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | このPodを特定のノードにスケジュールするリクエスト。<br />空でなければ、スケジューラーはリソース要件を満たすと仮定して単純にこのPodをそのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ。<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) 配列_ | Podのトレランス。<br />指定された場合、Podはマッチングオペレーターを使ってトリプル <key,value,effect> に一致する任意のテイントを許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) 配列_ | 指定されたトポロジー間でマッチするPodをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | 望ましいインスタンス数。<br />コアノードの場合、各インスタンスは一貫した識別子を持ちます。 | 2 | 最小値: 0 <br /> |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、少なくとも"minAvailable"が退避後も利用可能であれば退避を許可します。<br />例えば、"100%"を指定するとすべての任意退避を防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、退避後に最大で"maxUnavailable"が利用不可であれば退避を許可します。<br />例えば0を指定するとすべての任意退避を防止できます。"minAvailable"と相互排他的です。 |  | XIntOrString: \{\} <br /> |
| `command` _string 配列_ | エントリポイント配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照 `$(VAR_NAME)` はコンテナの環境変数で展開されます。解決できない場合は入力文字列の参照は変更されません。`$$` は `$` に変換され、`$(VAR_NAME)` のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string 配列_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照 `$(VAR_NAME)` はコンテナの環境変数で展開されます。解決できない場合は入力文字列の参照は変更されません。`$$` は `$` に変換され、`$(VAR_NAME)` のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) 配列_ | コンテナから公開するポートのリスト。<br />ここでポートを公開すると、システムにネットワーク接続情報が追加されますが、主に情報提供用です。ここでポートを指定しなくても、そのポートの公開は妨げられません。<br />コンテナ内でデフォルトの `0.0.0.0` アドレスでリッスンしている任意のポートはネットワークからアクセス可能です。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) 配列_ | コンテナ内に設定する環境変数のリスト。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) 配列_ | コンテナ内の環境変数を設定するためのソースリスト。<br />ソース内のキーはC_IDENTIFIERである必要があります。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />Envで定義された重複キーの値が優先されます。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Podレベルのセキュリティ属性および共通コンテナ設定。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | コンテナの実行に使用するセキュリティオプション。<br />設定されている場合、SecurityContextのフィールドはPodSecurityContextの対応するフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。いずれかの初期化コンテナが失敗するとPodは失敗とみなされ、restartPolicyに従い処理されます。初期化コンテナおよび通常コンテナの名前はすべてのコンテナ間で一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />初期化コンテナのresourceRequirementsはスケジューリング時に考慮され、各リソースタイプの最大要求/制限値が通常コンテナの合計またはその最大値として使用されます。制限も同様に適用されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | メインコンテナと共に実行する追加コンテナ。 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) 配列_ | Podに提供する追加ボリューム。 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) 配列_ | 追加ボリュームをメインコンテナにマウントする方法を指定。 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナの生存確認のための定期的なプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナのサービス準備完了確認のための定期的なプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Podが正常に初期化されたことを示すStartupProbe。<br />指定された場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、`livenessProbe`の失敗時と同様にPodは再起動されます。<br />Podのライフサイクル開始時にデータのロードやキャッシュのウォームアップに時間がかかる場合に、通常時とは異なるプローブパラメータを設定できます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに応じて管理システムが実行すべきアクション。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#persistentvolumeclaimspec-v1-core)_ | コアノードのデータストレージ用PVC仕様。<br />注意: このフィールド名は一貫しておらず、実際には単なる `PersistentVolumeClaimSpec` です。 |  |  |


#### EMQXNode







_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `name` _string_ | ノード名 |  |  |
| `podName` _string_ | 対応するPod名 |  |  |
| `status` _string_ | ノードの状態 |  |  |
| `otpRelease` _string_ | ノードが稼働しているErlang/OTPのバージョン |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | ノードの役割。 "core" または "replicant" |  |  |
| `sessions` _integer_ | MQTTセッション数 |  |  |
| `connections` _integer_ | 接続中のMQTTクライアント数 |  |  |


#### EMQXNodesStatus







_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `replicas` _integer_ | レプリカの総数。 |  |  |
| `readyReplicas` _integer_ | 準備完了のレプリカ数。 |  |  |
| `currentRevision` _string_ | 対応するコアまたはレプリカントセットの現在のリビジョン。 |  |  |
| `currentReplicas` _integer_ | 現在のリビジョンで稼働しているレプリカ数。 |  |  |
| `updateRevision` _string_ | 対応するコアまたはレプリカントセットの更新リビジョン。<br />現在のリビジョンと異なる場合、セットは更新中です。 |  |  |
| `updateReplicas` _integer_ | 更新リビジョンで稼働しているレプリカ数。 |  |  |
| `collisionCount` _integer_ |  |  |  |


#### EMQXReplicantTemplate







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | レプリカントノードの望ましい状態の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXReplicantTemplateSpec







_登場箇所:_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Podがノードに適合するために満たす必要があるセレクター。<br />Podがそのノードにスケジュールされるには、ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | このPodを特定のノードにスケジュールするリクエスト。<br />空でなければ、スケジューラーはリソース要件を満たすと仮定して単純にこのPodをそのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ。<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) 配列_ | Podのトレランス。<br />指定された場合、Podはマッチングオペレーターを使ってトリプル <key,value,effect> に一致する任意のテイントを許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) 配列_ | 指定されたトポロジー間でマッチするPodをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | 望ましいインスタンス数。<br />コアノードの場合、各インスタンスは一貫した識別子を持ちます。 | 2 | 最小値: 0 <br /> |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、少なくとも"minAvailable"が退避後も利用可能であれば退避を許可します。<br />例えば、"100%"を指定するとすべての任意退避を防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、退避後に最大で"maxUnavailable"が利用不可であれば退避を許可します。<br />例えば0を指定するとすべての任意退避を防止できます。"minAvailable"と相互排他的です。 |  | XIntOrString: \{\} <br /> |
| `command` _string 配列_ | エントリポイント配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照 `$(VAR_NAME)` はコンテナの環境変数で展開されます。解決できない場合は入力文字列の参照は変更されません。`$$` は `$` に変換され、`$(VAR_NAME)` のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string 配列_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照 `$(VAR_NAME)` はコンテナの環境変数で展開されます。解決できない場合は入力文字列の参照は変更されません。`$$` は `$` に変換され、`$(VAR_NAME)` のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) 配列_ | コンテナから公開するポートのリスト。<br />ここでポートを公開すると、システムにネットワーク接続情報が追加されますが、主に情報提供用です。ここでポートを指定しなくても、そのポートの公開は妨げられません。<br />コンテナ内でデフォルトの `0.0.0.0` アドレスでリッスンしている任意のポートはネットワークからアクセス可能です。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) 配列_ | コンテナ内に設定する環境変数のリスト。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) 配列_ | コンテナ内の環境変数を設定するためのソースリスト。<br />ソース内のキーはC_IDENTIFIERである必要があります。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />Envで定義された重複キーの値が優先されます。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Podレベルのセキュリティ属性および共通コンテナ設定。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | コンテナの実行に使用するセキュリティオプション。<br />設定されている場合、SecurityContextのフィールドはPodSecurityContextの対応するフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。いずれかの初期化コンテナが失敗するとPodは失敗とみなされ、restartPolicyに従い処理されます。初期化コンテナおよび通常コンテナの名前はすべてのコンテナ間で一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />初期化コンテナのresourceRequirementsはスケジューリング時に考慮され、各リソースタイプの最大要求/制限値が通常コンテナの合計またはその最大値として使用されます。制限も同様に適用されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) 配列_ | メインコンテナと共に実行する追加コンテナ。 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) 配列_ | Podに提供する追加ボリューム。 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) 配列_ | 追加ボリュームをメインコンテナにマウントする方法を指定。 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナの生存確認のための定期的なプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | コンテナのサービス準備完了確認のための定期的なプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Podが正常に初期化されたことを示すStartupProbe。<br />指定された場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、`livenessProbe`の失敗時と同様にPodは再起動されます。<br />Podのライフサイクル開始時にデータのロードやキャッシュのウォームアップに時間がかかる場合に、通常時とは異なるプローブパラメータを設定できます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに応じて管理システムが実行すべきアクション。 |  |  |


#### EMQXSpec



EMQXSpecはEMQXの望ましい状態を定義します。



_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `image` _string_ | EMQXコンテナイメージ。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#pullpolicy-v1-core)_ | コンテナイメージのプルポリシー。<br />`Always`、`Never`、`IfNotPresent` のいずれか。<br />`:latest` タグが指定されている場合はデフォルトで `Always`、それ以外は `IfNotPresent`。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#localobjectreference-v1-core) 配列_ | このPodSpecで使用するイメージのプルに使う同一ネームスペース内のSecretへの参照の任意リスト。<br />指定された場合、これらのSecretは個別のプラー実装に渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | ServiceAccount名。<br />管理されたReplicaSetsおよびStatefulSetsは認証目的で指定されたServiceAccountに関連付けられます。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) 配列_ | EMQX APIにアクセスするためのBootstrap APIキー。<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQX設定。 |  |  |
| `clusterDomain` _string_ | Kubernetesクラスターのドメイン。 | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古いReplicaSets、古いStatefulSets、古いPersistentVolumeClaimsの数。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | クラスターアップグレード戦略の設定。 | \{ type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | EMQXコアノードを実行するPodのテンプレート。 | \{ spec:map[replicas:2] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | EMQXレプリカントノードを実行するPodのテンプレート。 |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | EMQXダッシュボードを公開するServiceのテンプレート。<br />ダッシュボードServiceは常にEMQXコアノードのセットを指します。 |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | 有効なEMQXリスナーを公開するServiceのテンプレート。<br />リスナーServiceは有効かつ存在する場合はEMQXレプリカントノードのセットを指します。<br />そうでなければEMQXコアノードのセットを指します。 |  |  |


#### EMQXStatus



EMQXStatusはEMQXの観測された状態を定義します。



_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#condition-v1-meta) 配列_ | EMQXカスタムリソースの現在の状態を表す条件。 |  |  |
| `coreNodes` _[EMQXNode](#emqxnode) 配列_ | クラスター内の各コアノードの状態。 |  |  |
| `coreNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ | コアノードセットの概要状態。 |  |  |
| `replicantNodes` _[EMQXNode](#emqxnode) 配列_ | クラスター内の各レプリカントノードの状態。 |  |  |
| `replicantNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ | レプリカントノードセットの概要状態。 |  |  |
| `nodeEvacuationsStatus` _[NodeEvacuationStatus](#nodeevacuationstatus) 配列_ | クラスター内のアクティブなノード退避の状態。 |  |  |
| `dsReplication` _[DSReplicationStatus](#dsreplicationstatus)_ | EMQX Durable Storageのレプリケーション状態。 |  |  |


#### EvacuationStrategy







_登場箇所:_
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `connEvictRate` _integer_ | クライアント切断率（秒あたりの数）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation) の `conn-evict-rate` と同じ。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | セッション退避率（秒あたりの数）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation) の `sess-evict-rate` と同じ。 | 1000 | 最小値: 1 <br /> |
| `waitTakeover` _integer_ | セッション退避開始までの待機時間（秒）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation) の `wait-takeover` と同じ。 | 10 | 最小値: 0 <br /> |
| `waitHealthCheck` _integer_ | ノードがロードバランサーからアクティブなバックエンドノードリストから除外されるまでの待機時間（秒）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation) の `wait-health-check` と同じ。 | 60 | 最小値: 0 <br /> |


#### KeyRef







_登場箇所:_
- [SecretRef](#secretref)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `secretName` _string_ | Secretオブジェクトの名前。 |  |  |
| `secretKey` _string_ | Secretデータ内のエントリ。 |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |


#### NodeEvacuationStatus







_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeName` _string_ | 退避中のノード名 |  |  |
| `state` _string_ | 退避状態 |  |  |
| `sessionRecipients` _string 配列_ | セッション受信先 |  |  |
| `sessionEvictionRate` _integer_ | セッション退避率（秒あたりのセッション数）。 |  |  |
| `connectionEvictionRate` _integer_ | 接続退避率（秒あたりの接続数）。 |  |  |
| `initialSessions` _integer_ | このノード上の初期セッション数 |  |  |
| `initialConnections` _integer_ | このノードへの初期接続数 |  |  |


#### SecretRef







_登場箇所:_
- [BootstrapAPIKey](#bootstrapapikey)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _[KeyRef](#keyref)_ | EMQX APIキーを含むSecretエントリへの参照。 |  |  |
| `secret` _[KeyRef](#keyref)_ | EMQX APIキーのシークレットを含むSecretエントリへの参照。 |  |  |


#### ServiceTemplate







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `enabled` _boolean_ | Serviceを作成するかどうかを指定します。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#servicespec-v1-core)_ | Serviceの望ましい状態の仕様。<br />https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### UpdateStrategy







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _string_ | クラスターアップグレードの実行方法を決定します。<br />* `Recreate`: ブルーグリーンアップグレードを実行。 | Recreate | 列挙型: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | 接続退避開始までの秒数。 | 10 | 最小値: 0 <br /> |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 退避戦略の設定。 |  |  |
