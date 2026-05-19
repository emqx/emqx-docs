# APIリファレンス (v2beta1)

## パッケージ
- [apps.emqx.io/v2beta1](#appsemqxiov2beta1)


## apps.emqx.io/v2beta1

Package v2beta1 は apps v2beta1 APIグループのAPIスキーマ定義を含みます

### リソースタイプ
- [EMQX](#emqx)
- [EMQXList](#emqxlist)
- [Rebalance](#rebalance)
- [RebalanceList](#rebalancelist)


#### BootstrapAPIKey

_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |
| `secret` _string_ |  |  | 最大長: 128 <br />最小長: 3 <br /> |
| `secretRef` _[SecretRef](#secretref)_ |  |  |  |


#### Config

_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `mode` _string_ |  | Merge | 列挙: [Merge Replace] <br /> |
| `data` _string_ | EMQX設定。HOCON形式で、etc/emqx.confファイルのような形式 |  |  |


#### EMQX

EMQXはemqxes APIのスキーマです

_出現箇所:_
- [EMQXList](#emqxlist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | Specはこのセット内のEMQXノードの望ましい状態を定義します。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | StatusはEMQXノードの現在の状態です。このデータは一定の遅延を含む場合があります。 |  |  |


#### EMQXCoreTemplate

_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | EMQXコアノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXCoreTemplateSpec

_出現箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorは、Podがノードに割り当てられるために満たす必要がある条件です。ノードのラベルとマッチする必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | NodeNameは特定のノードにPodをスケジューリングするリクエストです。空でなければ、スケジューラーはリソース要件を満たすと仮定してそのノードにPodを割り当てます。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする汚染を許容します。<br />TODO: 将来的には`tolerations`を使うべきであり、このフィールドは旧バージョンとの互換性のために存在し、将来削除されます。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする汚染を許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | TopologySpreadConstraintは、指定されたトポロジー内でのPodの分散方法を指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスですが、各レプリカは一貫した識別性を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "minAvailable"で指定された数のPodが、退避後も利用可能である場合にのみ退避を許可します。<br />例えば、"100%"を指定するとすべての任意の退避を防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "maxUnavailable"で指定された数以下のPodが退避後に利用不可であれば退避を許可します。<br />例えば0を指定するとすべての任意の退避を防止できます。これは"minAvailable"と相互排他的です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。$$は$に変換され、$(VAR_NAME)のエスケープが可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。$$は$に変換され、$(VAR_NAME)のエスケープが可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを指定することは主に情報提供目的であり、指定しなくてもポートは公開されます。<br />コンテナ内の"0.0.0.0"でリッスンしているポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内で環境変数を設定するためのソースリスト。<br />ソース内のキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーがある場合、最後のソースの値が優先されます。<br />Envで定義された重複キーの値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナが必要とする計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | Podレベルのセキュリティ属性および共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | コンテナの実行に使用するセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナは通常のコンテナ起動前に順に実行されます。失敗するとPodは失敗とみなされ、restartPolicyに従って処理されます。名前は全コンテナで一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />リソース要求はスケジューリング時に考慮され、通常コンテナの合計か最大値のいずれか大きい方が使用されます。<br />現在、追加・削除はできません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | Podに追加する追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存確認のための定期的なプローブ。<br />失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備完了確認のための定期的なプローブ。<br />失敗するとサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | Podの初期化成功を示すプローブ。<br />指定されると他のプローブは成功まで実行されません。<br />失敗するとlivenessProbe失敗時と同様にPodは再起動されます。<br />Podのライフサイクル初期段階での長時間処理に対応可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | 名前はVolumeClaimTemplatesですが、実際はPersistentVolumeClaimSpecです。申し訳ありません。<br />PersistentVolumeClaimSpecはストレージデバイスの共通属性を記述し、プロバイダー固有の属性のソースを許可します。<br />EMQXReplicantTemplateSpecよりも詳細です。 |  |  |


#### EMQXList

EMQXListはEMQXのリストを含みます

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQXList` | | |
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `items` _[EMQX](#emqx) array_ |  |  |  |


#### EMQXNode

_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `controllerUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `podUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `node` _string_ | EMQXノード名。例: emqx@127.0.0.1 |  |  |
| `node_status` _string_ | EMQXノードの状態。例: Running |  |  |
| `otp_release` _string_ | EMQXで使用されているErlang/OTPのバージョン。例: 24.2/12.2 |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | EMQXクラスターのノード役割。列挙: "core" "replicant" |  |  |
| `edition` _string_ | EMQXクラスターのノードエディション。列挙: "Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`connections`フィールドはMQTTセッション数を意味します。 |  |  |
| `live_connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`live_connections`フィールドは接続中のMQTTクライアント数を意味します。<br />`live_connections`はEMQX 5.1以降で動作します。 |  |  |


#### EMQXNodesStatus

_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `replicas` _integer_ |  |  |  |
| `readyReplicas` _integer_ |  |  |  |
| `currentRevision` _string_ |  |  |  |
| `currentReplicas` _integer_ |  |  |  |
| `updateRevision` _string_ |  |  |  |
| `updateReplicas` _integer_ |  |  |  |
| `collisionCount` _integer_ |  |  |  |


#### EMQXReplicantTemplate

_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQXレプリカントノードの望ましい動作の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller toolsは複雑なバリデーション(oneOf/anyOf/allOfなど)をサポートしないため、バリデーションルールを使用してください。https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |


#### EMQXReplicantTemplateSpec

_出現箇所:_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorは、Podがノードに割り当てられるために満たす必要がある条件です。ノードのラベルとマッチする必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | NodeNameは特定のノードにPodをスケジューリングするリクエストです。空でなければ、スケジューラーはリソース要件を満たすと仮定してそのノードにPodを割り当てます。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする汚染を許容します。<br />TODO: 将来的には`tolerations`を使うべきであり、このフィールドは旧バージョンとの互換性のために存在し、将来削除されます。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする汚染を許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | TopologySpreadConstraintは、指定されたトポロジー内でのPodの分散方法を指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスですが、各レプリカは一貫した識別性を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "minAvailable"で指定された数のPodが、退避後も利用可能である場合にのみ退避を許可します。<br />例えば、"100%"を指定するとすべての任意の退避を防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "maxUnavailable"で指定された数以下のPodが退避後に利用不可であれば退避を許可します。<br />例えば0を指定するとすべての任意の退避を防止できます。これは"minAvailable"と相互排他的です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。$$は$に変換され、$(VAR_NAME)のエスケープが可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。$$は$に変換され、$(VAR_NAME)のエスケープが可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを指定することは主に情報提供目的であり、指定しなくてもポートは公開されます。<br />コンテナ内の"0.0.0.0"でリッスンしているポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内で環境変数を設定するためのソースリスト。<br />ソース内のキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーがある場合、最後のソースの値が優先されます。<br />Envで定義された重複キーの値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナが必要とする計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | Podレベルのセキュリティ属性および共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | コンテナの実行に使用するセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナは通常のコンテナ起動前に順に実行されます。失敗するとPodは失敗とみなされ、restartPolicyに従って処理されます。名前は全コンテナで一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />リソース要求はスケジューリング時に考慮され、通常コンテナの合計か最大値のいずれか大きい方が使用されます。<br />現在、追加・削除はできません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | Podに追加する追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存確認のための定期的なプローブ。<br />失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備完了確認のための定期的なプローブ。<br />失敗するとサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | Podの初期化成功を示すプローブ。<br />指定されると他のプローブは成功まで実行されません。<br />失敗するとlivenessProbe失敗時と同様にPodは再起動されます。<br />Podのライフサイクル初期段階での長時間処理に対応可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |


#### EMQXSpec

EMQXSpecはEMQXの望ましい状態を定義します

_出現箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `image` _string_ | EMQXのイメージ名。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#pullpolicy-v1-core)_ | イメージプルポリシー。<br />Always, Never, IfNotPresentのいずれか。<br />:latestタグ指定時はデフォルトでAlways、それ以外はIfNotPresent。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#localobjectreference-v1-core) array_ | ImagePullSecretsは、このPodSpecで使用されるイメージをプルするための同一ネームスペース内のシークレット参照のオプションリスト。<br />指定された場合、これらのシークレットは個別のプラーに渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | サービスアカウント名<br />ReplicaSetやStatefulSetを指定されたサービスアカウントに関連付けます。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) array_ | EMQXのブートストラップユーザー<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQXの設定 |  |  |
| `clusterDomain` _string_ |  | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古いReplicaSet、StatefulSet、PersistentVolumeClaimの数。<br />明示的なゼロと未指定を区別するためのポインタ。<br />デフォルトは3。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | UpdateStrategyはEMQXのブルーグリーンアップデート戦略を記述するオブジェクト | \{ evacuationStrategy:map[connEvictRate:1000 sessEvictRate:1000 waitTakeover:10] initialDelaySeconds:10 type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | CoreTemplateは作成されるEMQXコアノードを記述するオブジェクト | \{ spec:map[replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | ReplicantTemplateは作成されるEMQXレプリカントノードを記述するオブジェクト |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | DashboardServiceTemplateは作成されるEMQXダッシュボードサービスを記述するオブジェクト<br />このサービスは常にEMQXコアノードをセレクトします |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | ListenersServiceTemplateは作成されるEMQXリスナーサービスを記述するオブジェクト<br />EMQXレプリカントノードが存在する場合、このサービスはレプリカントノードをセレクトします。<br />存在しない場合はコアノードをセレクトします。 |  |  |


#### EMQXStatus

EMQXStatusはEMQXの観測された状態を定義します

_出現箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#condition-v1-meta) array_ | EMQXカスタムリソースの最新の状態観測を表します。 |  |  |
| `coreNodes` _[EMQXNode](#emqxnode) array_ |  |  |  |
| `coreNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ |  |  |  |
| `replicantNodes` _[EMQXNode](#emqxnode) array_ |  |  |  |
| `replicantNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ |  |  |  |
| `nodeEvacuationsStatus` _[NodeEvacuationStatus](#nodeevacuationstatus) array_ |  |  |  |


#### EvacuationStrategy

_出現箇所:_
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `waitTakeover` _integer_ |  |  | 最小値: 0 <br /> |
| `connEvictRate` _integer_ | EMQX Enterpriseでのみ動作します。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | EMQX Enterpriseでのみ動作します。 | 1000 | 最小値: 1 <br /> |


#### KeyRef

_出現箇所:_
- [SecretRef](#secretref)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `secretName` _string_ |  |  |  |
| `secretKey` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |


#### NodeEvacuationStats

_出現箇所:_
- [NodeEvacuationStatus](#nodeevacuationstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `initial_sessions` _integer_ |  |  |  |
| `initial_connected` _integer_ |  |  |  |
| `current_sessions` _integer_ |  |  |  |
| `current_connected` _integer_ |  |  |  |


#### NodeEvacuationStatus

_出現箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `node` _string_ |  |  |  |
| `stats` _[NodeEvacuationStats](#nodeevacuationstats)_ |  |  |  |
| `state` _string_ |  |  |  |
| `session_recipients` _string array_ |  |  |  |
| `session_goal` _integer_ |  |  |  |
| `session_eviction_rate` _integer_ |  |  |  |
| `connection_goal` _integer_ |  |  |  |
| `connection_eviction_rate` _integer_ |  |  |  |


#### Rebalance

Rebalanceはrebalances APIのスキーマです

_出現箇所:_
- [RebalanceList](#rebalancelist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `Rebalance` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[RebalanceSpec](#rebalancespec)_ |  |  |  |
| `status` _[RebalanceStatus](#rebalancestatus)_ |  |  |  |


#### RebalanceCondition

RebalanceConditionはEMQXのリバランスジョブの現在の状態を表します。

_出現箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | リバランス条件タイプの状態。Processing, Complete, Failedのいずれか。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件の状態。True, False, Unknownのいずれか。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | この条件が最後に更新された時刻。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件が別の状態に遷移した最後の時刻。 |  |  |
| `reason` _string_ | 条件の最後の遷移理由。 |  |  |
| `message` _string_ | 遷移の詳細を示す人間が読めるメッセージ。 |  |  |


#### RebalanceConditionType

_基底型:_ _string_

_出現箇所:_
- [RebalanceCondition](#rebalancecondition)

| フィールド | 説明 |
| --- | --- |
| `Processing` |  |
| `Completed` |  |
| `Failed` |  |


#### RebalanceList

RebalanceListはRebalanceのリストを含みます

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `RebalanceList` | | |
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `items` _[Rebalance](#rebalance) array_ |  |  |  |


#### RebalancePhase

_基底型:_ _string_

_出現箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 |
| --- | --- |
| `Processing` |  |
| `Completed` |  |
| `Failed` |  |


#### RebalanceSpec

RebalanceSpecはRebalanceの望ましい状態を定義します

_出現箇所:_
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `instanceKind` _string_ | InstanceKindはEMQXとEMQXEnterpriseを区別するために使用されます。<br />"EMQX"の場合はEMQX CRがv2beta1であることを意味し、<br />"EmqxEnterprise"の場合はEmqxEnterprise CRがv1beta4であることを意味します。 | EMQX |  |
| `instanceName` _string_ | InstanceNameはEMQX CRの名前を表し、EMQX Enterpriseでのみ有効です。 |  | 必須: \{\} <br /> |
| `rebalanceStrategy` _[RebalanceStrategy](#rebalancestrategy)_ | RebalanceStrategyはEMQXのリバランス戦略を表します。<br />詳細: https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing |  | 必須: \{\} <br /> |


#### RebalanceState

RebalanceはEMQXの観測されたリバランス状態を定義します

_出現箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `state` _string_ | EMQXクラスターのリバランス状態を表します。 |  |  |
| `session_eviction_rate` _integer_ | ノードのセッション退避率（秒あたり）を表します。 |  |  |
| `recipients` _string array_ | リバランスのターゲットノードを表します。 |  |  |
| `node` _string_ | リバランスのスケジューリングノードを表します。 |  |  |
| `donors` _string array_ | リバランスのソースノードを表します。 |  |  |
| `coordinator_node` _string_ | 現在リバランス中のノードを表します。 |  |  |
| `connection_eviction_rate` _integer_ | ノードの接続退避率（秒あたり）を表します。 |  |  |


#### RebalanceStatus

RebalanceStatusはRebalanceの現在の状態を表します

_出現箇所:_
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | オブジェクトの最新の状態観測。<br />リバランスが失敗した場合、条件は"type"が"Failed"で"status"がfalseになります。<br />処理中は"type"が"Processing"で"status"がtrueになります。<br />完了時は"type"が"Complete"で"status"がtrueになります。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | リバランスのフェーズを表します。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | リバランスジョブ開始時刻を表します。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | リバランスジョブ完了時刻を表します。 |  |  |


#### RebalanceStrategy

RebalanceStrategyはEMQXのリバランス戦略を表します

_出現箇所:_
- [RebalanceSpec](#rebalancespec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `connEvictRate` _integer_ | ソースノードのクライアント切断率（秒あたり）。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のconn-evict-rateと同じ。<br />値は0より大きくなければなりません。 |  | 最小値: 1 <br />必須: \{\} <br /> |
| `sessEvictRate` _integer_ | ソースノードのセッション退避率（秒あたり）。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のsess-evict-rateと同じ。<br />値は0より大きくなければなりません。<br />デフォルトは500。 | 500 |  |
| `waitTakeover` _integer_ | すべての接続が切断された後、クライアントがセッションを引き継ぐために再接続を待つ秒数。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-takeoverと同じ。<br />値は0より大きくなければなりません。<br />デフォルトは60秒。 | 60 |  |
| `waitHealthCheck` _integer_ | LBがソースノードをアクティブなバックエンドノードリストから削除するまでの待機時間（秒）。<br />指定時間経過後にリバランスタスクが開始されます。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-health-checkと同じ。<br />値は0より大きくなければなりません。<br />デフォルトは60秒。 | 60 |  |
| `absConnThreshold` _integer_ | 接続バランスチェックの絶対閾値。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-conn-thresholdと同じ。<br />値は0より大きくなければなりません。<br />デフォルトは1000。 | 1000 |  |
| `relConnThreshold` _string_ | 接続バランスチェックの相対閾値。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-conn-thresholdと同じ。<br />言語によって浮動小数点のサポートが異なるため、string型で定義されています。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"。 | 1.1 |  |
| `absSessThreshold` _integer_ | セッション接続バランスチェックの絶対閾値。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-sess-thresholdと同じ。<br />値は0より大きくなければなりません。<br />デフォルトは1000。 | 1000 |  |
| `relSessThreshold` _string_ | セッション接続バランスチェックの相対閾値。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-sess-thresholdと同じ。<br />言語によって浮動小数点のサポートが異なるため、string型で定義されています。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"。 | 1.1 |  |


#### SecretRef

_出現箇所:_
- [BootstrapAPIKey](#bootstrapapikey)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _[KeyRef](#keyref)_ |  |  |  |
| `secret` _[KeyRef](#keyref)_ |  |  |  |


#### ServiceTemplate

_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `enabled` _boolean_ | EMQX OperatorはEMQXノード用のサービスを作成します。<br />これは`false`と未指定を区別するためのポインタです。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#servicespec-v1-core)_ | サービスの動作を定義します。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### UpdateStrategy

_出現箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _string_ |  | Recreate | 列挙: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | 退避接続開始までの秒数。 |  |  |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 退避接続タイムアウトまでの秒数。 |  |  |
