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







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |
| `secret` _string_ |  |  | 最大長: 128 <br />最小長: 3 <br /> |
| `secretRef` _[SecretRef](#secretref)_ |  |  |  |


#### Config







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `mode` _string_ |  | Merge | 列挙: [Merge Replace] <br /> |
<<<<<<< HEAD
| `data` _string_ | EMQX設定、HOCONフォーマット、etc/emqx.confファイルのような形式 |  |  |
=======
| `data` _string_ | EMQX設定、HOCON形式、etc/emqx.confファイルのような形式 |  |  |
>>>>>>> origin/release-6.1


#### EMQX



EMQXはemqxes APIのスキーマです



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXList](#emqxlist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | Specはこのセット内のEMQXノードの望ましい状態を定義します。 |  |  |
<<<<<<< HEAD
| `status` _[EMQXStatus](#emqxstatus)_ | StatusはEMQXノードの現在の状態です。このデータは一定の遅延がある可能性があります。 |  |  |
=======
| `status` _[EMQXStatus](#emqxstatus)_ | StatusはEMQXノードの現在の状態です。このデータは一定の遅延を伴う場合があります。 |  |  |
>>>>>>> origin/release-6.1


#### EMQXCoreTemplate







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | EMQXコアノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXCoreTemplateSpec







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorはPodがノードに適合するために真でなければならないセレクターです。ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | NodeNameはこのPodを特定のノードにスケジューリングするリクエストです。空でなければ、スケジューラーはリソース要件に合うと仮定してこのノードにPodを割り当てます。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする任意のテイントを許容します。<br />TODO: 将来的には`tolerations`を使用すべきで、このフィールドは旧バージョンとの互換性のために存在し、将来削除予定です。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする任意のテイントを許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraintは指定されたトポロジー間でマッチするPodをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスですが、個々のレプリカは一貫した識別を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、少なくとも"minAvailable"が退避後も利用可能であれば退避を許可します。<br />例えば、すべての任意退避を防ぐには"100%"を指定します。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、退避後に最大で"maxUnavailable"が利用不可であれば退避を許可します。<br />例えば、すべての任意退避を防ぐには0を指定します。これは"minAvailable"と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定がなければコンテナイメージのENTRYPOINTが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は参照はそのままです。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定がなければコンテナイメージのCMDが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は参照はそのままです。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開することは、システムにコンテナのネットワーク接続情報を追加で提供しますが、主に情報的なものです。ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内のデフォルトの"0.0.0.0"アドレスでリッスンしているポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内のキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />Envで定義された重複キーの値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContextはPodレベルのセキュリティ属性および共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContextはコンテナ実行時のセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。失敗するとPodは失敗とみなされ、restartPolicyに従って処理されます。初期化コンテナと通常コンテナの名前はすべて一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />リソース要件はスケジューリング時に考慮され、各リソースタイプの最大要求/制限値が使用されます。<br />初期化コンテナの追加や削除は現在できません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainersはPodに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存確認のための定期的なプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備状態の定期的なプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbeはPodの初期化成功を示します。<br />指定されている場合、これが成功するまで他のプローブは実行されません。<br />失敗するとlivenessProbe失敗時と同様にPodは再起動されます。<br />Podのライフサイクル開始時に異なるプローブパラメータを設定可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | このフィールドはVolumeClaimTemplatesと名付けられていますが、実際はPersistentVolumeClaimSpecです。名前が不適切で申し訳ありません。<br />PersistentVolumeClaimSpecはストレージデバイスの共通属性を記述し、プロバイダー固有の属性のためのソースを許容します。<br />EMQXReplicantTemplateSpecより詳細です。 |  |  |
=======
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorはポッドがノードに適合するために真でなければならないセレクターです。ポッドがそのノードにスケジュールされるためにノードのラベルと一致しなければなりません。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | NodeNameはこのポッドを特定のノードにスケジュールするリクエストです。空でなければ、スケジューラーはリソース要件に適合すると仮定して単純にこのポッドをそのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | ポッド割り当てのためのAffinity<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランスです。<br />このTolerationが付与されたポッドは、マッチング演算子を使用してトリプル<key,value,effect>に一致する任意の汚染を許容します。<br />TODO: 将来的には`tolerations`を使用すべきで、このフィールドは旧バージョンとの互換性のためだけに存在し、将来削除されます。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランスです。<br />このTolerationが付与されたポッドは、マッチング演算子を使用してトリプル<key,value,effect>に一致する任意の汚染を許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraintは指定されたトポロジー間でマッチするポッドをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同一テンプレートのインスタンスであり、個々のレプリカは一貫した識別子を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択された少なくとも"minAvailable"個のポッドが、退避後も利用可能である場合に退避が許可されます。つまり、退避されたポッドが存在しなくても利用可能である必要があります。<br />例えば、"100%"を指定するとすべての任意退避を防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択された最大"maxUnavailable"個のポッドが退避後に利用不可である場合に退避が許可されます。つまり、退避されたポッドが存在しなくても利用不可である必要があります。<br />例えば、0を指定するとすべての任意退避を防止できます。これは"minAvailable"と相互排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイント配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。ダブル$$は単一の$に変換され、$(VAR_NAME)構文のエスケープを可能にします。例えば"$$(VAR_NAME)"は文字列リテラル"$(VAR_NAME)"になります。エスケープされた参照は変数の存在に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。ダブル$$は単一の$に変換され、$(VAR_NAME)構文のエスケープを可能にします。例えば"$$(VAR_NAME)"は文字列リテラル"$(VAR_NAME)"になります。エスケープされた参照は変数の存在に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開すると、システムにコンテナが使用するネットワーク接続に関する追加情報を提供しますが、主に情報提供目的です。ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内のデフォルトの"0.0.0.0"アドレスでリッスンしている任意のポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内で定義されたキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />重複キーのEnvで定義された値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContextはポッドレベルのセキュリティ属性および共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContextはコンテナを実行する際のセキュリティオプションを定義します。<br />設定されている場合、SecurityContextのフィールドはPodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ポッドに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順に実行されます。いずれかの初期化コンテナが失敗するとポッドは失敗と見なされ、restartPolicyに従って処理されます。初期化コンテナまたは通常コンテナの名前はすべてのコンテナ間で一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />初期化コンテナのresourceRequirementsはスケジューリング時に考慮され、各リソースタイプの最大要求/制限値を見つけ、それと通常コンテナの合計の最大値を使用します。制限も同様に適用されます。<br />初期化コンテナは現在追加・削除できません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainersはポッドに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存性を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備完了状態を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbeはポッドが正常に初期化されたことを示します。<br />指定された場合、これが成功するまでは他のプローブは実行されません。<br />このプローブが失敗すると、livenessProbeが失敗した場合と同様にポッドは再起動されます。<br />これはポッドのライフサイクル開始時にデータロードやキャッシュウォームアップに時間がかかる場合に、通常運用時とは異なるプローブパラメータを提供するために使用できます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに応じて管理システムが実行すべきアクション。<br />更新不可。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | このフィールドはVolumeClaimTemplatesという名前ですが、実際にはPersistentVolumeClaimSpecです。名前が悪くて申し訳ありません。<br />PersistentVolumeClaimSpecはストレージデバイスの共通属性を記述し、プロバイダー固有の属性のためのソースを許可します。<br />EMQXReplicantTemplateSpecより詳細です。 |  |  |
>>>>>>> origin/release-6.1


#### EMQXList



EMQXListはEMQXのリストを含みます





| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQXList` | | |
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `items` _[EMQX](#emqx) array_ |  |  |  |


#### EMQXNode







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `controllerUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `podUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `node` _string_ | EMQXノード名、例: emqx@127.0.0.1 |  |  |
| `node_status` _string_ | EMQXノードの状態、例: Running |  |  |
<<<<<<< HEAD
| `otp_release` _string_ | EMQXで使用されているErlang/OTPのバージョン、例: 24.2/12.2 |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | EMQXクラスターのノード役割、列挙: "core" "replicant" |  |  |
| `edition` _string_ | EMQXクラスターのノードエディション、列挙: "Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQXのAPI `/api/v5/nodes`における`connections`はMQTTセッション数を意味します。 |  |  |
| `live_connections` _integer_ | EMQXのAPI `/api/v5/nodes`における`live_connections`は接続中のMQTTクライアント数を意味します。<br />`live_connections`はEMQX 5.1以降で有効です。 |  |  |
=======
| `otp_release` _string_ | EMQXが使用するErlang/OTPのバージョン、例: 24.2/12.2 |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | EMQXクラスターのノードロール、列挙: "core" "replicant" |  |  |
| `edition` _string_ | EMQXクラスターのノードエディション、列挙: "Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`connections`フィールドはMQTTセッション数を意味します。 |  |  |
| `live_connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`live_connections`フィールドは接続中のMQTTクライアント数を意味します。<br />`live_connections`はEMQX 5.1以降でのみ有効です。 |  |  |
>>>>>>> origin/release-6.1


#### EMQXNodesStatus







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
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







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
<<<<<<< HEAD
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQXレプリカントノードの望ましい動作の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller toolsは複雑なバリデーション(oneOf/anyOf/allOfなど)をサポートしていないため、バリデーションルールを使用してください。https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |
=======
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQXレプリカントノードの望ましい動作の仕様。<br />詳細: https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller toolsは複雑なバリデーション(oneOf/anyOf/allOfなど)をサポートしていないため、代わりにバリデーションルールを使用してください。 https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |
>>>>>>> origin/release-6.1


#### EMQXReplicantTemplateSpec







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorはPodがノードに適合するために真でなければならないセレクターです。ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | NodeNameはこのPodを特定のノードにスケジューリングするリクエストです。空でなければ、スケジューラーはリソース要件に合うと仮定してこのノードにPodを割り当てます。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする任意のテイントを許容します。<br />TODO: 将来的には`tolerations`を使用すべきで、このフィールドは旧バージョンとの互換性のために存在し、将来削除予定です。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、Podのトレランスです。<br />このTolerationが付与されたPodは、<key,value,effect>の組み合わせにマッチする任意のテイントを許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraintは指定されたトポロジー間でマッチするPodをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスですが、個々のレプリカは一貫した識別を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、少なくとも"minAvailable"が退避後も利用可能であれば退避を許可します。<br />例えば、すべての任意退避を防ぐには"100%"を指定します。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択されたPodのうち、退避後に最大で"maxUnavailable"が利用不可であれば退避を許可します。<br />例えば、すべての任意退避を防ぐには0を指定します。これは"minAvailable"と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定がなければコンテナイメージのENTRYPOINTが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は参照はそのままです。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定がなければコンテナイメージのCMDが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない場合は参照はそのままです。$$は$に変換され、$(VAR_NAME)構文のエスケープが可能です。<br />エスケープされた参照は展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開することは、システムにコンテナのネットワーク接続情報を追加で提供しますが、主に情報的なものです。ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内のデフォルトの"0.0.0.0"アドレスでリッスンしているポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内のキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />Envで定義された重複キーの値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContextはPodレベルのセキュリティ属性および共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContextはコンテナ実行時のセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順番に実行されます。失敗するとPodは失敗とみなされ、restartPolicyに従って処理されます。初期化コンテナと通常コンテナの名前はすべて一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />リソース要件はスケジューリング時に考慮され、各リソースタイプの最大要求/制限値が使用されます。<br />初期化コンテナの追加や削除は現在できません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainersはPodに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存確認のための定期的なプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備状態の定期的なプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbeはPodの初期化成功を示します。<br />指定されている場合、これが成功するまで他のプローブは実行されません。<br />失敗するとlivenessProbe失敗時と同様にPodは再起動されます。<br />Podのライフサイクル開始時に異なるプローブパラメータを設定可能です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |
=======
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelectorはポッドがノードに適合するために真でなければならないセレクターです。ポッドがそのノードにスケジュールされるためにノードのラベルと一致しなければなりません。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes/ |  |  |
| `nodeName` _string_ | NodeNameはこのポッドを特定のノードにスケジュールするリクエストです。空でなければ、スケジューラーはリソース要件に適合すると仮定して単純にこのポッドをそのノードにスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | ポッド割り当てのためのAffinity<br />参照: https://kubernetes.io/docs/tasks/configure-pod-container/assign-pods-nodes-using-node-affinity/ |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランスです。<br />このTolerationが付与されたポッドは、マッチング演算子を使用してトリプル<key,value,effect>に一致する任意の汚染を許容します。<br />TODO: 将来的には`tolerations`を使用すべきで、このフィールドは旧バージョンとの互換性のためだけに存在し、将来削除されます。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランスです。<br />このTolerationが付与されたポッドは、マッチング演算子を使用してトリプル<key,value,effect>に一致する任意の汚染を許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | // TopologySpreadConstraintは指定されたトポロジー間でマッチするポッドをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicasは指定されたテンプレートの望ましいレプリカ数です。<br />これらは同一テンプレートのインスタンスであり、個々のレプリカは一貫した識別子を持ちます。<br />デフォルトは2です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択された少なくとも"minAvailable"個のポッドが、退避後も利用可能である場合に退避が許可されます。つまり、退避されたポッドが存在しなくても利用可能である必要があります。<br />例えば、"100%"を指定するとすべての任意退避を防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "selector"で選択された最大"maxUnavailable"個のポッドが退避後に利用不可である場合に退避が許可されます。つまり、退避されたポッドが存在しなくても利用不可である必要があります。<br />例えば、0を指定するとすべての任意退避を防止できます。これは"minAvailable"と相互排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイント配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。ダブル$$は単一の$に変換され、$(VAR_NAME)構文のエスケープを可能にします。例えば"$$(VAR_NAME)"は文字列リテラル"$(VAR_NAME)"になります。エスケープされた参照は変数の存在に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照$(VAR_NAME)はコンテナの環境変数で展開されます。解決できない変数はそのまま残ります。ダブル$$は単一の$に変換され、$(VAR_NAME)構文のエスケープを可能にします。例えば"$$(VAR_NAME)"は文字列リテラル"$(VAR_NAME)"になります。エスケープされた参照は変数の存在に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開すると、システムにコンテナが使用するネットワーク接続に関する追加情報を提供しますが、主に情報提供目的です。ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内のデフォルトの"0.0.0.0"アドレスでリッスンしている任意のポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内で設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内で定義されたキーはC_IDENTIFIERでなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />重複キーのEnvで定義された値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContextはポッドレベルのセキュリティ属性および共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContextはコンテナを実行する際のセキュリティオプションを定義します。<br />設定されている場合、SecurityContextのフィールドはPodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ポッドに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順に実行されます。いずれかの初期化コンテナが失敗するとポッドは失敗と見なされ、restartPolicyに従って処理されます。初期化コンテナまたは通常コンテナの名前はすべてのコンテナ間で一意でなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />初期化コンテナのresourceRequirementsはスケジューリング時に考慮され、各リソースタイプの最大要求/制限値を見つけ、それと通常コンテナの合計の最大値を使用します。制限も同様に適用されます。<br />初期化コンテナは現在追加・削除できません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainersはポッドに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存性を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備完了状態を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbeはポッドが正常に初期化されたことを示します。<br />指定された場合、これが成功するまでは他のプローブは実行されません。<br />このプローブが失敗すると、livenessProbeが失敗した場合と同様にポッドは再起動されます。<br />これはポッドのライフサイクル開始時にデータロードやキャッシュウォームアップに時間がかかる場合に、通常運用時とは異なるプローブパラメータを提供するために使用できます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに応じて管理システムが実行すべきアクション。<br />更新不可。 |  |  |
>>>>>>> origin/release-6.1


#### EMQXSpec



EMQXSpecはEMQXの望ましい状態を定義します



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `image` _string_ | EMQXイメージ名。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#pullpolicy-v1-core)_ | イメージプルポリシー。<br />Always, Never, IfNotPresentのいずれか。<br />:latestタグ指定時はデフォルトでAlways、それ以外はIfNotPresent。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#localobjectreference-v1-core) array_ | ImagePullSecretsはこのPodSpecで使用されるイメージのプルに使用する同一ネームスペース内のシークレット参照のオプションリストです。<br />指定すると、これらのシークレットは個別のプラー実装に渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | サービスアカウント名<br />ReplicaSetまたはStatefulSetを指定されたサービスアカウントに関連付けて認証に使用します。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) array_ | EMQXブートストラップユーザー<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQX設定 |  |  |
| `clusterDomain` _string_ |  | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古いReplicaSet、StatefulSet、PersistentVolumeClaimの数。<br />明示的なゼロと未指定を区別するためのポインタ。<br />デフォルトは3。 | 3 |  |
=======
| `image` _string_ | EMQXのイメージ名。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#pullpolicy-v1-core)_ | イメージプルポリシー。<br />Always、Never、IfNotPresentのいずれか。<br />:latestタグが指定されている場合はデフォルトでAlways、それ以外はIfNotPresent。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#localobjectreference-v1-core) array_ | ImagePullSecretsはこのPodSpecで使用されるイメージをプルするために同じネームスペース内のシークレットへの参照のオプションリストです。<br />指定された場合、これらのシークレットは個別のプラー実装に渡されて使用されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | サービスアカウント名<br />ReplicaSetまたはStatefulSetを指定されたサービスアカウントに関連付けて認証目的で使用します。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) array_ | EMQXのブートストラップユーザー<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQX設定 |  |  |
| `clusterDomain` _string_ |  | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古いReplicaSet、古いStatefulSet、古いPersistentVolumeClaimの数。<br />明示的なゼロと未指定を区別するためのポインター。<br />デフォルトは3。 | 3 |  |
>>>>>>> origin/release-6.1
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | UpdateStrategyはEMQXのブルーグリーンアップデート戦略を記述するオブジェクトです | \{ evacuationStrategy:map[connEvictRate:1000 sessEvictRate:1000 waitTakeover:10] initialDelaySeconds:10 type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | CoreTemplateは作成されるEMQXコアノードを記述するオブジェクトです | \{ spec:map[replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | ReplicantTemplateは作成されるEMQXレプリカントノードを記述するオブジェクトです |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | DashboardServiceTemplateは作成されるEMQXダッシュボードサービスを記述するオブジェクトです<br />このサービスは常にEMQXコアノードをセレクトします |  |  |
<<<<<<< HEAD
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | ListenersServiceTemplateは作成されるEMQXリスナーサービスを記述するオブジェクトです<br />EMQXレプリカントノードが存在する場合、このサービスはレプリカントノードをセレクトします<br />存在しない場合はコアノードをセレクトします |  |  |
=======
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | ListenersServiceTemplateは作成されるEMQXリスナーサービスを記述するオブジェクトです<br />EMQXレプリカントノードが存在する場合、このサービスはEMQXレプリカントノードをセレクトします<br />存在しない場合はEMQXコアノードをセレクトします |  |  |
>>>>>>> origin/release-6.1


#### EMQXStatus



EMQXStatusはEMQXの観測された状態を定義します



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#condition-v1-meta) array_ | EMQXカスタムリソースの現在の状態に関する最新の観測結果を表します。 |  |  |
| `coreNodes` _[EMQXNode](#emqxnode) array_ |  |  |  |
| `coreNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ |  |  |  |
| `replicantNodes` _[EMQXNode](#emqxnode) array_ |  |  |  |
| `replicantNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ |  |  |  |
| `nodeEvacuationsStatus` _[NodeEvacuationStatus](#nodeevacuationstatus) array_ |  |  |  |


#### EvacuationStrategy







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `waitTakeover` _integer_ |  |  | 最小値: 0 <br /> |
| `connEvictRate` _integer_ | EMQX Enterpriseでのみ有効です。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | EMQX Enterpriseでのみ有効です。 | 1000 | 最小値: 1 <br /> |


#### KeyRef







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [SecretRef](#secretref)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `secretName` _string_ |  |  |  |
| `secretKey` _string_ |  |  | パターン: `^[a-zA-Z\d-_]+$` <br /> |


#### NodeEvacuationStats







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [NodeEvacuationStatus](#nodeevacuationstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `initial_sessions` _integer_ |  |  |  |
| `initial_connected` _integer_ |  |  |  |
| `current_sessions` _integer_ |  |  |  |
| `current_connected` _integer_ |  |  |  |


#### NodeEvacuationStatus







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
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



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [RebalanceList](#rebalancelist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `Rebalance` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[RebalanceSpec](#rebalancespec)_ |  |  |  |
| `status` _[RebalanceStatus](#rebalancestatus)_ |  |  |  |


#### RebalanceCondition



<<<<<<< HEAD
RebalanceConditionはEMQXのリバランスジョブの現在の状態を表します。



_登場箇所:_
=======
RebalanceConditionはEMQXのリバランシングジョブの現在の状態を表します。



_出現箇所:_
>>>>>>> origin/release-6.1
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | リバランス条件タイプの状態。Processing, Complete, Failedのいずれか。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件の状態。True, False, Unknownのいずれか。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | この条件が最後に更新された時刻。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件がある状態から別の状態に遷移した最後の時刻。 |  |  |
| `reason` _string_ | 条件の最後の遷移理由。 |  |  |
| `message` _string_ | 遷移の詳細を示す人間が読めるメッセージ。 |  |  |
=======
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | リバランス条件タイプの状態。Processing、Complete、Failedのいずれか。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件の状態。True、False、Unknownのいずれか。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | この条件が最後に更新された時刻。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件がある状態から別の状態に遷移した最後の時刻。 |  |  |
| `reason` _string_ | 条件の最後の遷移理由。 |  |  |
| `message` _string_ | 遷移の詳細を示す人間が読みやすいメッセージ。 |  |  |
>>>>>>> origin/release-6.1


#### RebalanceConditionType

<<<<<<< HEAD
_基礎型:_ _string_
=======
_基底型:_ _string_
>>>>>>> origin/release-6.1





<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
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

<<<<<<< HEAD
_基礎型:_ _string_
=======
_基底型:_ _string_
>>>>>>> origin/release-6.1





<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 |
| --- | --- |
| `Processing` |  |
| `Completed` |  |
| `Failed` |  |


#### RebalanceSpec



RebalanceSpecはRebalanceの望ましい状態を定義します



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `instanceKind` _string_ | InstanceKindはEMQXとEMQXEnterpriseを区別するために使用されます。<br />"EMQX"に設定されている場合はEMQX CRがv2beta1であることを意味し、<br />"EmqxEnterprise"に設定されている場合はEmqxEnterprise CRがv1beta4であることを意味します。 | EMQX |  |
| `instanceName` _string_ | InstanceNameはEMQX CRの名前を表し、EMQX Enterpriseでのみ有効です。 |  | 必須: \{\} <br /> |
| `rebalanceStrategy` _[RebalanceStrategy](#rebalancestrategy)_ | RebalanceStrategyはEMQXのリバランス戦略を表します。<br />詳細: https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing |  | 必須: \{\} <br /> |


#### RebalanceState



<<<<<<< HEAD
RebalanceStateはEMQXのリバランスの観測された状態を定義します



_登場箇所:_
=======
RebalanceStateはEMQXの観測されたリバランス状態を定義します



_出現箇所:_
>>>>>>> origin/release-6.1
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `state` _string_ | Stateはemqxクラスターのリバランス状態を表します。 |  |  |
| `session_eviction_rate` _integer_ | SessionEvictionRateはノードのセッション退避率（秒あたり）を表します。 |  |  |
| `recipients` _string array_ | Recipientsはリバランスの対象ノードを表します。 |  |  |
| `node` _string_ | Nodeはリバランスのスケジューリングノードを表します。 |  |  |
| `donors` _string array_ | Donorsはリバランスのソースノードを表します。 |  |  |
| `coordinator_node` _string_ | CoordinatorNodeは現在リバランス中のノードを表します。 |  |  |
| `connection_eviction_rate` _integer_ | ConnectionEvictionRateはノードの接続退避率（秒あたり）を表します。 |  |  |


#### RebalanceStatus



RebalanceStatusはRebalanceの現在の状態を表します



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | オブジェクトの現在の状態に関する最新の観測結果。<br />Rebalanceが失敗した場合、条件は"type"が"Failed"で"status"がfalseになります。<br />Rebalanceが処理中の場合、条件は"type"が"Processing"で"status"がtrueになります。<br />Rebalanceが完了した場合、条件は"type"が"Complete"で"status"がtrueになります。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | PhaseはRebalanceのフェーズを表します。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | StartedTimeはリバランスジョブ開始時刻を表します。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | CompletedTimeはリバランスジョブ完了時刻を表します。 |  |  |
=======
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | オブジェクトの現在の状態に関する最新の観測結果。<br />Rebalanceが失敗した場合、条件は"type"が"Failed"でstatusがfalseになります。<br />Rebalanceが処理中の場合、条件は"type"が"Processing"でstatusがtrueになります。<br />Rebalanceが完了した場合、条件は"type"が"Complete"でstatusがtrueになります。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | PhaseはRebalanceのフェーズを表します。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | StartedTimeはリバランスジョブの開始時刻を表します。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | CompletedTimeはリバランスジョブの完了時刻を表します。 |  |  |
>>>>>>> origin/release-6.1


#### RebalanceStrategy



RebalanceStrategyはEMQXのリバランス戦略を表します



<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [RebalanceSpec](#rebalancespec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `connEvictRate` _integer_ | ConnEvictRateはソースノードのクライアント切断率（秒あたり）を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のconn-evict-rateと同じです。<br />値は0より大きくなければなりません。 |  | 最小値: 1 <br />必須: \{\} <br /> |
| `sessEvictRate` _integer_ | SessEvictRateはソースノードのセッション退避率（秒あたり）を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のsess-evict-rateと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは500です。 | 500 |  |
<<<<<<< HEAD
| `waitTakeover` _integer_ | WaitTakeoverはすべての接続が切断された後にクライアントが再接続してセッションを引き継ぐまでの待機時間（秒）を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-takeoverと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `waitHealthCheck` _integer_ | WaitHealthCheckはロードバランサーがソースノードをアクティブなバックエンドノードリストから除外するまでの待機時間（秒）を表します。<br />指定された待機時間を超えるとリバランスタスクが開始されます。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-health-checkと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `absConnThreshold` _integer_ | AbsConnThresholdは接続バランスをチェックするための絶対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-conn-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relConnThreshold` _string_ | RelConnThresholdは接続バランスをチェックするための相対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-conn-thresholdと同じです。<br />浮動小数点の使用は言語間のサポート差異があるため推奨されません。<br />そのためRelConnThresholdは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
| `absSessThreshold` _integer_ | AbsSessThresholdはセッション接続バランスをチェックするための絶対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-sess-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relSessThreshold` _string_ | RelSessThresholdはセッション接続バランスをチェックするための相対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-sess-thresholdと同じです。<br />浮動小数点の使用は言語間のサポート差異があるため推奨されません。<br />そのためRelSessThresholdは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
=======
| `waitTakeover` _integer_ | WaitTakeoverはすべての接続が切断された後、クライアントがセッションを引き継ぐために再接続を待つ秒数を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-takeoverと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `waitHealthCheck` _integer_ | WaitHealthCheckはロードバランサーがソースノードをアクティブなバックエンドノードのリストから除外するまでの待機時間（秒）を表します。<br />指定された待機時間を超えるとリバランスタスクが開始されます。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-health-checkと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `absConnThreshold` _integer_ | AbsConnThresholdは接続バランスチェックの絶対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-conn-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relConnThreshold` _string_ | RelConnThresholdは接続バランスチェックの相対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-conn-thresholdと同じです。<br />浮動小数点の使用は言語間のサポート差異が大きいため推奨されません。<br />そのためRelConnThresholdは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
| `absSessThreshold` _integer_ | AbsSessThresholdはセッション接続バランスチェックの絶対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-sess-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relSessThreshold` _string_ | RelSessThresholdはセッション接続バランスチェックの相対閾値を表します。<br />[EMQXリバランシング](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-sess-thresholdと同じです。<br />浮動小数点の使用は言語間のサポート差異が大きいため推奨されません。<br />そのためRelSessThresholdは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
>>>>>>> origin/release-6.1


#### SecretRef







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [BootstrapAPIKey](#bootstrapapikey)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `key` _[KeyRef](#keyref)_ |  |  |  |
| `secret` _[KeyRef](#keyref)_ |  |  |  |


#### ServiceTemplate







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `enabled` _boolean_ | EMQX OperatorはEMQXノードのためのサービスを作成します。<br />これは`false`と未指定を区別するためのポインタです。 | true |  |
=======
| `enabled` _boolean_ | EMQX OperatorはEMQXノード用のサービスを作成します。<br />これは`false`と未指定を区別するためのポインターです。 | true |  |
>>>>>>> origin/release-6.1
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#servicespec-v1-core)_ | Specはサービスの動作を定義します。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### UpdateStrategy







<<<<<<< HEAD
_登場箇所:_
=======
_出現箇所:_
>>>>>>> origin/release-6.1
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _string_ |  | Recreate | 列挙: [Recreate] <br /> |
<<<<<<< HEAD
| `initialDelaySeconds` _integer_ | 退避接続開始までの待機秒数。 |  |  |
=======
| `initialDelaySeconds` _integer_ | 退避接続開始までの秒数。 |  |  |
>>>>>>> origin/release-6.1
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 退避接続タイムアウトまでの秒数。 |  |  |
