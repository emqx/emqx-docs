<<<<<<< HEAD:zh_CN/deploy/kubernetes/operator/reference/v2beta1-reference.md
# API Reference (v2beta1)
=======
# APIリファレンス
>>>>>>> origin/release-5.10:ja_JP/deploy/kubernetes/operator/api-reference.md

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







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |
| `secret` _string_ |  |  | 最大長: 128 <br />最小長: 3 <br /> |
| `secretRef` _[SecretRef](#secretref)_ |  |  |  |


#### Config







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `mode` _string_ |  | Merge | 列挙: [Merge Replace] <br /> |
| `data` _string_ | EMQX設定、HOCON形式。etc/emqx.confファイルのような形式 |  |  |


#### EMQX



EMQXはemqxes APIのスキーマです



_登場箇所:_
- [EMQXList](#emqxlist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | Specはこのセット内のEMQXノードの望ましい識別情報を定義します。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | StatusはEMQXノードの現在の状態です。このデータは一定の遅延を伴う場合があります。 |  |  |


#### EMQXCoreTemplate







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | EMQXコアノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXCoreTemplateSpec







_登場箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD:zh_CN/deploy/kubernetes/operator/reference/v2beta1-reference.md
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelector is a selector which must be true for the pod to fit on a node. Selector which must match a node's labels for the pod to be scheduled on that node.<br />More info: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeName is a request to schedule this pod onto a specific node. If it is non-empty, the scheduler simply schedules this pod onto that node, assuming that it fits resource requirements. |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Affinity for pod assignment<br />ref: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | If specified, the pod's tolerations.<br />The pod this Toleration is attached to tolerates any taint that matches the triple <key,value,effect> using the matching operator .<br />TODO: should use `tolerations` instead, this field just for compatible with old version, will delete in future. |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | If specified, the pod's tolerations.<br />The pod this Toleration is attached to tolerates any taint that matches the triple <key,value,effect> using the matching operator . |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraint specifies how to spread matching pods among the given topology. |  |  |
| `replicas` _integer_ | Replicas is the desired number of replicas of the given Template.<br />These are replicas in the sense that they are instantiations of the<br />same Template, but individual replicas also have a consistent identity.<br />Defaults to 2. | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | An eviction is allowed if at least "minAvailable" pods selected by<br />"selector" will still be available after the eviction, i.e. even in the<br />absence of the evicted pod.  So for example you can prevent all voluntary<br />evictions by specifying "100%". |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | An eviction is allowed if at most "maxUnavailable" pods selected by<br />"selector" are unavailable after the eviction, i.e. even in absence of<br />the evicted pod. For example, one can prevent all voluntary evictions<br />by specifying 0. This is a mutually exclusive setting with "minAvailable". |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | Entrypoint array. Not executed within a shell.<br />The container image's ENTRYPOINT is used if this is not provided.<br />Variable references $(VAR_NAME) are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double $$ are reduced<br />to a single $, which allows for escaping the $(VAR_NAME) syntax: i.e. "$$(VAR_NAME)" will<br />produce the string literal "$(VAR_NAME)". Escaped references will never be expanded, regardless<br />of whether the variable exists or not. Cannot be updated.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | Arguments to the entrypoint.<br />The container image's CMD is used if this is not provided.<br />Variable references $(VAR_NAME) are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double $$ are reduced<br />to a single $, which allows for escaping the $(VAR_NAME) syntax: i.e. "$$(VAR_NAME)" will<br />produce the string literal "$(VAR_NAME)". Escaped references will never be expanded, regardless<br />of whether the variable exists or not. Cannot be updated.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | List of ports to expose from the container. Exposing a port here gives<br />the system additional information about the network connections a<br />container uses, but is primarily informational. Not specifying a port here<br />DOES NOT prevent that port from being exposed. Any port which is<br />listening on the default "0.0.0.0" address inside a container will be<br />accessible from the network.<br />Cannot be updated. |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | List of environment variables to set in the container.<br />Cannot be updated. |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | List of sources to populate environment variables in the container.<br />The keys defined within a source must be a C_IDENTIFIER. All invalid keys<br />will be reported as an event when the container is starting. When a key exists in multiple<br />sources, the value associated with the last source will take precedence.<br />Values defined by an Env with a duplicate key will take precedence.<br />Cannot be updated. |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | Compute Resources required by this container.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContext holds pod-level security attributes and common container settings. | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContext defines the security options the container should be run with.<br />If set, the fields of SecurityContext override the equivalent fields of PodSecurityContext.<br />More info: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | List of initialization containers belonging to the pod.<br />Init containers are executed in order prior to containers being started. If any<br />init container fails, the pod is considered to have failed and is handled according<br />to its restartPolicy. The name for an init container or normal container must be<br />unique among all containers.<br />Init containers may not have Lifecycle actions, Readiness probes, Liveness probes, or Startup probes.<br />The resourceRequirements of an init container are taken into account during scheduling<br />by finding the highest request/limit for each resource type, and then using the max of<br />of that value or the sum of the normal containers. Limits are applied to init containers<br />in a similar fashion.<br />Init containers cannot currently be added or removed.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainers represents extra containers to be added to the pod.<br />See https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | See https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | See https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | Periodic probe of container liveness.<br />Container will be restarted if the probe fails.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | Periodic probe of container service readiness.<br />Container will be removed from service endpoints if the probe fails.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbe indicates that the Pod has successfully initialized.<br />If specified, no other probes are executed until this completes successfully.<br />If this probe fails, the Pod will be restarted, just as if the livenessProbe failed.<br />This can be used to provide different probe parameters at the beginning of a Pod's lifecycle,<br />when it might take a long time to load data or warm a cache, than during steady-state operation.<br />This cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | Actions that the management system should take in response to container lifecycle events.<br />Cannot be updated. |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | This field is named VolumeClaimTemplates but actually it is PersistentVolumeClaimSpec. I'm sorry for the bad naming.<br />PersistentVolumeClaimSpec describes the common attributes of storage devices<br />and allows a Source for provider-specific attributes<br />More than EMQXReplicantTemplateSpec |  |  |
=======
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorはポッドがノードに適合するために真でなければならないセレクターです。ポッドがノードにスケジュールされるためにノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/concepts/config/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeNameはこのポッドを特定のノードにスケジュールするリクエストです。空でなければ、スケジューラーはリソース要件に合うと仮定して単純にこのポッドをそのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | ポッド割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/concepts/config/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定されている場合、ポッドのトレランス。<br />このTolerationが付与されたポッドは、<key,value,effect>の組にマッチする任意のテイントをマッチングオペレーターを使って許容します。<br />TODO: 将来的には`tolerations`を使用すべきで、このフィールドは旧バージョンとの互換性のためだけに存在し、将来削除予定です。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定されている場合、ポッドのトレランス。<br />このTolerationが付与されたポッドは、<key,value,effect>の組にマッチする任意のテイントをマッチングオペレーターを使って許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraintは指定されたトポロジー間でマッチするポッドをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスとしてのレプリカですが、個々のレプリカは一貫した識別を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたポッドのうち、少なくとも"minAvailable"がエビクション後も利用可能であればエビクションが許可されます。<br />例えば、"100%"を指定するとすべての任意のエビクションを防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたポッドのうち、最大で"maxUnavailable"がエビクション後に利用不可であればエビクションが許可されます。<br />例えば、0を指定するとすべての任意のエビクションを防止できます。これは"minAvailable"と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定されなければコンテナイメージのENTRYPOINTが使われます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は変数の有無にかかわらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定されなければコンテナイメージのCMDが使われます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は変数の有無にかかわらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開することは、システムにコンテナのネットワーク接続情報を追加で提供しますが、主に情報目的です。ここでポートを指定しなくても、そのポートが公開されることを妨げません。コンテナ内のデフォルトの"0.0.0.0"アドレスでリッスンしているポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内で定義されたキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />重複キーのEnvで定義された値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナが必要とする計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/config/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContextはポッドレベルのセキュリティ属性と共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContextはコンテナのセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContextの同等のフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ポッドに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。初期化コンテナが失敗するとポッドは失敗とみなされ、restartPolicyに従って処理されます。初期化コンテナと通常コンテナの名前はすべてのコンテナ間で一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />スケジューリング時に初期化コンテナのリソース要求は、各リソースタイプの最大要求/制限を見つけ、それと通常コンテナの合計の最大値を使用して考慮されます。制限も同様に適用されます。<br />初期化コンテナは現在追加や削除ができません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainersはポッドに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存確認の定期的なプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備完了の定期的なプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbeはポッドが正常に初期化されたことを示します。<br />指定されている場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、livenessProbeが失敗した場合と同様にポッドは再起動されます。<br />ポッドのライフサイクル開始時にデータのロードやキャッシュのウォームアップに時間がかかる場合に、通常の状態とは異なるプローブパラメータを提供するために使えます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | このフィールド名はVolumeClaimTemplatesですが、実際はPersistentVolumeClaimSpecです。命名が悪く申し訳ありません。<br />PersistentVolumeClaimSpecはストレージデバイスの共通属性を記述し、プロバイダー固有の属性のソースを許容します。<br />EMQXReplicantTemplateSpecより多いです。 |  |  |
>>>>>>> origin/release-5.10:ja_JP/deploy/kubernetes/operator/api-reference.md


#### EMQXList



EMQXListはEMQXのリストを含みます





| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQXList` | | |
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `items` _[EMQX](#emqx) array_ |  |  |  |


#### EMQXNode







_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `controllerUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `podUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `node` _string_ | EMQXノード名、例: emqx@127.0.0.1 |  |  |
| `node_status` _string_ | EMQXノードの状態、例: Running |  |  |
| `otp_release` _string_ | EMQXで使用されているErlang/OTPのバージョン、例: 24.2/12.2 |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | EMQXクラスターのノードロール、列挙: "core" "replicant" |  |  |
| `edition` _string_ | EMQXクラスターのノードエディション、列挙: "Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`connections`フィールドはMQTTセッション数を意味します。 |  |  |
| `live_connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`live_connections`フィールドは接続中のMQTTクライアント数を意味します。<br />`live_connections`はEMQX 5.1以降でのみ有効です。 |  |  |


#### EMQXNodesStatus







_登場箇所:_
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







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQXレプリカントノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller toolsは複雑なバリデーション(oneOf/anyOf/allOfなど)をサポートしていないため、バリデーションルールを使用してください。https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |


#### EMQXReplicantTemplateSpec







_登場箇所:_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD:zh_CN/deploy/kubernetes/operator/reference/v2beta1-reference.md
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelector is a selector which must be true for the pod to fit on a node. Selector which must match a node's labels for the pod to be scheduled on that node.<br />More info: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeName is a request to schedule this pod onto a specific node. If it is non-empty, the scheduler simply schedules this pod onto that node, assuming that it fits resource requirements. |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Affinity for pod assignment<br />ref: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | If specified, the pod's tolerations.<br />The pod this Toleration is attached to tolerates any taint that matches the triple <key,value,effect> using the matching operator .<br />TODO: should use `tolerations` instead, this field just for compatible with old version, will delete in future. |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | If specified, the pod's tolerations.<br />The pod this Toleration is attached to tolerates any taint that matches the triple <key,value,effect> using the matching operator . |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraint specifies how to spread matching pods among the given topology. |  |  |
| `replicas` _integer_ | Replicas is the desired number of replicas of the given Template.<br />These are replicas in the sense that they are instantiations of the<br />same Template, but individual replicas also have a consistent identity.<br />Defaults to 2. | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | An eviction is allowed if at least "minAvailable" pods selected by<br />"selector" will still be available after the eviction, i.e. even in the<br />absence of the evicted pod.  So for example you can prevent all voluntary<br />evictions by specifying "100%". |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | An eviction is allowed if at most "maxUnavailable" pods selected by<br />"selector" are unavailable after the eviction, i.e. even in absence of<br />the evicted pod. For example, one can prevent all voluntary evictions<br />by specifying 0. This is a mutually exclusive setting with "minAvailable". |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | Entrypoint array. Not executed within a shell.<br />The container image's ENTRYPOINT is used if this is not provided.<br />Variable references $(VAR_NAME) are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double $$ are reduced<br />to a single $, which allows for escaping the $(VAR_NAME) syntax: i.e. "$$(VAR_NAME)" will<br />produce the string literal "$(VAR_NAME)". Escaped references will never be expanded, regardless<br />of whether the variable exists or not. Cannot be updated.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | Arguments to the entrypoint.<br />The container image's CMD is used if this is not provided.<br />Variable references $(VAR_NAME) are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double $$ are reduced<br />to a single $, which allows for escaping the $(VAR_NAME) syntax: i.e. "$$(VAR_NAME)" will<br />produce the string literal "$(VAR_NAME)". Escaped references will never be expanded, regardless<br />of whether the variable exists or not. Cannot be updated.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | List of ports to expose from the container. Exposing a port here gives<br />the system additional information about the network connections a<br />container uses, but is primarily informational. Not specifying a port here<br />DOES NOT prevent that port from being exposed. Any port which is<br />listening on the default "0.0.0.0" address inside a container will be<br />accessible from the network.<br />Cannot be updated. |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | List of environment variables to set in the container.<br />Cannot be updated. |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | List of sources to populate environment variables in the container.<br />The keys defined within a source must be a C_IDENTIFIER. All invalid keys<br />will be reported as an event when the container is starting. When a key exists in multiple<br />sources, the value associated with the last source will take precedence.<br />Values defined by an Env with a duplicate key will take precedence.<br />Cannot be updated. |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | Compute Resources required by this container.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContext holds pod-level security attributes and common container settings. | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContext defines the security options the container should be run with.<br />If set, the fields of SecurityContext override the equivalent fields of PodSecurityContext.<br />More info: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | List of initialization containers belonging to the pod.<br />Init containers are executed in order prior to containers being started. If any<br />init container fails, the pod is considered to have failed and is handled according<br />to its restartPolicy. The name for an init container or normal container must be<br />unique among all containers.<br />Init containers may not have Lifecycle actions, Readiness probes, Liveness probes, or Startup probes.<br />The resourceRequirements of an init container are taken into account during scheduling<br />by finding the highest request/limit for each resource type, and then using the max of<br />of that value or the sum of the normal containers. Limits are applied to init containers<br />in a similar fashion.<br />Init containers cannot currently be added or removed.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainers represents extra containers to be added to the pod.<br />See https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | See https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | See https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | Periodic probe of container liveness.<br />Container will be restarted if the probe fails.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | Periodic probe of container service readiness.<br />Container will be removed from service endpoints if the probe fails.<br />Cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbe indicates that the Pod has successfully initialized.<br />If specified, no other probes are executed until this completes successfully.<br />If this probe fails, the Pod will be restarted, just as if the livenessProbe failed.<br />This can be used to provide different probe parameters at the beginning of a Pod's lifecycle,<br />when it might take a long time to load data or warm a cache, than during steady-state operation.<br />This cannot be updated.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | Actions that the management system should take in response to container lifecycle events.<br />Cannot be updated. |  |  |
=======
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorはポッドがノードに適合するために真でなければならないセレクターです。ポッドがノードにスケジュールされるためにノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/concepts/config/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeNameはこのポッドを特定のノードにスケジュールするリクエストです。空でなければ、スケジューラーはリソース要件に合うと仮定して単純にこのポッドをそのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | ポッド割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/concepts/config/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定されている場合、ポッドのトレランス。<br />このTolerationが付与されたポッドは、<key,value,effect>の組にマッチする任意のテイントをマッチングオペレーターを使って許容します。<br />TODO: 将来的には`tolerations`を使用すべきで、このフィールドは旧バージョンとの互換性のためだけに存在し、将来削除予定です。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定されている場合、ポッドのトレランス。<br />このTolerationが付与されたポッドは、<key,value,effect>の組にマッチする任意のテイントをマッチングオペレーターを使って許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraintは指定されたトポロジー間でマッチするポッドをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスとしてのレプリカですが、個々のレプリカは一貫した識別を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたポッドのうち、少なくとも"minAvailable"がエビクション後も利用可能であればエビクションが許可されます。<br />例えば、"100%"を指定するとすべての任意のエビクションを防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたポッドのうち、最大で"maxUnavailable"がエビクション後に利用不可であればエビクションが許可されます。<br />例えば、0を指定するとすべての任意のエビクションを防止できます。これは"minAvailable"と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定されなければコンテナイメージのENTRYPOINTが使われます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は変数の有無にかかわらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定されなければコンテナイメージのCMDが使われます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は変数の有無にかかわらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開することは、システムにコンテナのネットワーク接続情報を追加で提供しますが、主に情報目的です。ここでポートを指定しなくても、そのポートが公開されることを妨げません。コンテナ内のデフォルトの"0.0.0.0"アドレスでリッスンしているポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内で定義されたキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />重複キーのEnvで定義された値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナが必要とする計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/config/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContextはポッドレベルのセキュリティ属性と共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContextはコンテナのセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContextの同等のフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ポッドに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。初期化コンテナが失敗するとポッドは失敗とみなされ、restartPolicyに従って処理されます。初期化コンテナと通常コンテナの名前はすべてのコンテナ間で一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />スケジューリング時に初期化コンテナのリソース要求は、各リソースタイプの最大要求/制限を見つけ、それと通常コンテナの合計の最大値を使用して考慮されます。制限も同様に適用されます。<br />初期化コンテナは現在追加や削除ができません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainersはポッドに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存確認の定期的なプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備完了の定期的なプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbeはポッドが正常に初期化されたことを示します。<br />指定されている場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、livenessProbeが失敗した場合と同様にポッドは再起動されます。<br />ポッドのライフサイクル開始時にデータのロードやキャッシュのウォームアップに時間がかかる場合に、通常の状態とは異なるプローブパラメータを提供するために使えます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |
>>>>>>> origin/release-5.10:ja_JP/deploy/kubernetes/operator/api-reference.md


#### EMQXSpec



EMQXSpecはEMQXの望ましい状態を定義します



_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `image` _string_ | EMQXのイメージ名。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#pullpolicy-v1-core)_ | イメージのプルポリシー。<br />Always、Never、IfNotPresentのいずれか。<br />:latestタグが指定されている場合はデフォルトでAlways、それ以外はIfNotPresent。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#localobjectreference-v1-core) array_ | ImagePullSecretsはこのPodSpecで使用されるイメージをプルするために同じネームスペース内のシークレットへの参照のオプションリストです。<br />指定されている場合、これらのシークレットは個々のプラー実装に渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | サービスアカウント名<br />ReplicaSetまたはStatefulSetを指定されたサービスアカウントと関連付けて認証に使用します。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) array_ | EMQXのブートストラップユーザー<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQXの設定 |  |  |
| `clusterDomain` _string_ |  | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古いReplicaSet、古いStatefulSet、古いPersistentVolumeClaimの数。<br />明示的なゼロと未指定を区別するためのポインター。<br />デフォルトは3。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | UpdateStrategyはEMQXのブルーグリーンアップデート戦略を記述するオブジェクトです | \{ evacuationStrategy:map[connEvictRate:1000 sessEvictRate:1000 waitTakeover:10] initialDelaySeconds:10 type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | CoreTemplateは作成されるEMQXコアノードを記述するオブジェクトです | \{ spec:map[replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | ReplicantTemplateは作成されるEMQXレプリカントノードを記述するオブジェクトです |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | DashboardServiceTemplateは作成されるEMQXダッシュボードサービスを記述するオブジェクトです<br />このサービスは常にEMQXコアノードをセレクトします |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | ListenersServiceTemplateは作成されるEMQXリスナーサービスを記述するオブジェクトです<br />EMQXレプリカントノードが存在する場合、このサービスはEMQXレプリカントノードをセレクトします<br />存在しない場合はEMQXコアノードをセレクトします |  |  |


#### EMQXStatus



EMQXStatusはEMQXの観測された状態を定義します



_登場箇所:_
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







_登場箇所:_
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `waitTakeover` _integer_ |  |  | 最小値: 0 <br /> |
| `connEvictRate` _integer_ | EMQX Enterpriseでのみ有効です。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | EMQX Enterpriseでのみ有効です。 | 1000 | 最小値: 1 <br /> |


#### KeyRef







_登場箇所:_
- [SecretRef](#secretref)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `secretName` _string_ |  |  |  |
| `secretKey` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |


#### NodeEvacuationStats







_登場箇所:_
- [NodeEvacuationStatus](#nodeevacuationstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `initial_sessions` _integer_ |  |  |  |
| `initial_connected` _integer_ |  |  |  |
| `current_sessions` _integer_ |  |  |  |
| `current_connected` _integer_ |  |  |  |


#### NodeEvacuationStatus







_登場箇所:_
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



_登場箇所:_
- [RebalanceList](#rebalancelist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `Rebalance` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[RebalanceSpec](#rebalancespec)_ |  |  |  |
| `status` _[RebalanceStatus](#rebalancestatus)_ |  |  |  |


#### RebalanceCondition



RebalanceConditionはEMQXリバランスジョブの現在の状態を表します。



_登場箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | リバランス条件タイプの状態。Processing、Complete、Failedのいずれか。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件の状態。True、False、Unknownのいずれか。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | この条件が最後に更新された時間。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件がある状態から別の状態に最後に遷移した時間。 |  |  |
| `reason` _string_ | 条件の最後の遷移の理由。 |  |  |
| `message` _string_ | 遷移の詳細を示す人間が読めるメッセージ。 |  |  |


#### RebalanceConditionType

_基底型:_ _string_





_登場箇所:_
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





_登場箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 |
| --- | --- |
| `Processing` |  |
| `Completed` |  |
| `Failed` |  |


#### RebalanceSpec



RebalanceSpecはRebalanceの望ましい状態を定義します



_登場箇所:_
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `instanceKind` _string_ | InstanceKindはEMQXとEMQXEnterpriseを区別するために使われます。<br />"EMQX"に設定されている場合はEMQX CRがv2beta1であることを意味し、<br />"EmqxEnterprise"に設定されている場合はEmqxEnterprise CRがv1beta4であることを意味します。 | EMQX |  |
| `instanceName` _string_ | InstanceNameはEMQX CRの名前を表し、EMQX Enterpriseでのみ有効です。 |  | 必須: \{\} <br /> |
| `rebalanceStrategy` _[RebalanceStrategy](#rebalancestrategy)_ | RebalanceStrategyはEMQXリバランスの戦略を表します。<br />詳細: https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing |  | 必須: \{\} <br /> |


#### RebalanceState



RebalanceStateはEMQXの観測されたリバランス状態を定義します



_登場箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `state` _string_ | EMQXクラスターリバランスの状態を表します。 |  |  |
| `session_eviction_rate` _integer_ | ノードのセッションエバキュエーション率（秒あたり）を表します。 |  |  |
| `recipients` _string array_ | リバランスのターゲットノードを表します。 |  |  |
| `node` _string_ | リバランスのスケジューリングノードを表します。 |  |  |
| `donors` _string array_ | リバランスのソースノードを表します。 |  |  |
| `coordinator_node` _string_ | 現在リバランス中のノードを表します。 |  |  |
| `connection_eviction_rate` _integer_ | ノードの接続エバキュエーション率（秒あたり）を表します。 |  |  |


#### RebalanceStatus



RebalanceStatusはRebalanceの現在の状態を表します



_登場箇所:_
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | オブジェクトの最新の状態観測。<br />Rebalanceが失敗した場合、条件は"type"が"Failed"で"status"がfalseになります。<br />Rebalanceが処理中の場合、条件は"type"が"Processing"で"status"がtrueになります。<br />Rebalanceが完了した場合、条件は"type"が"Complete"で"status"がtrueになります。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | Rebalanceのフェーズを表します。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | リバランスジョブ開始時刻を表します。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | リバランスジョブ完了時刻を表します。 |  |  |


#### RebalanceStrategy



RebalanceStrategyはEMQXリバランスの戦略を表します



_登場箇所:_
- [RebalanceSpec](#rebalancespec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `connEvictRate` _integer_ | ConnEvictRateはソースノードのクライアント切断率（秒あたり）を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のconn-evict-rateと同じです。<br />値は0より大きくなければなりません。 |  | 最小値: 1 <br />必須: \{\} <br /> |
| `sessEvictRate` _integer_ | SessEvictRateはソースノードのセッションエバキュエーション率（秒あたり）を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のsess-evict-rateと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは500です。 | 500 |  |
| `waitTakeover` _integer_ | WaitTakeoverはすべての接続が切断された後、クライアントが再接続してセッションを引き継ぐまでの待機時間（秒）を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-takeoverと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `waitHealthCheck` _integer_ | WaitHealthCheckはロードバランサーがソースノードをアクティブなバックエンドノードのリストから削除するまでの待機時間（秒）を表します。<br />指定された待機時間を超えるとリバランスタスクが開始されます。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-health-checkと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `absConnThreshold` _integer_ | AbsConnThresholdは接続バランスの絶対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-conn-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relConnThreshold` _string_ | RelConnThresholdは接続バランスの相対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-conn-thresholdと同じです。<br />浮動小数点の使用は言語間でのサポート差異が大きいため強く推奨されません。<br />そのためRelConnThresholdフィールドは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
| `absSessThreshold` _integer_ | AbsSessThresholdはセッション接続バランスの絶対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-sess-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relSessThreshold` _string_ | RelSessThresholdはセッション接続バランスの相対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-sess-thresholdと同じです。<br />浮動小数点の使用は言語間でのサポート差異が大きいため強く推奨されません。<br />そのためRelSessThresholdフィールドは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |


#### SecretRef







_登場箇所:_
- [BootstrapAPIKey](#bootstrapapikey)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _[KeyRef](#keyref)_ |  |  |  |
| `secret` _[KeyRef](#keyref)_ |  |  |  |


#### ServiceTemplate







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `enabled` _boolean_ | EMQX OperatorはEMQXノード用のサービスを作成します。<br />これは`false`と未指定を区別するためのポインターです。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#servicespec-v1-core)_ | Specはサービスの動作を定義します。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### UpdateStrategy







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD:zh_CN/deploy/kubernetes/operator/reference/v2beta1-reference.md
| `type` _string_ |  | Recreate | Enum: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | Number of seconds before evacuation connection start. |  |  |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | Number of seconds before evacuation connection timeout. |  |  |

=======
| `type` _string_ |  | Recreate | 列挙: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | エバキュエーション接続開始までの秒数。 |  |  |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | エバキュエーション接続タイムアウトまでの秒数。 |  |  |
>>>>>>> origin/release-5.10:ja_JP/deploy/kubernetes/operator/api-reference.md
