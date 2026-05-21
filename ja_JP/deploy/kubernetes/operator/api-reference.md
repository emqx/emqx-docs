<<<<<<< HEAD
# API リファレンス
=======
# APIリファレンス
>>>>>>> origin/release-5.9

## パッケージ
- [apps.emqx.io/v2beta1](#appsemqxiov2beta1)


## apps.emqx.io/v2beta1

<<<<<<< HEAD
Package v2beta1 は apps v2beta1 API グループの API スキーマ定義を含みます。
=======
Package v2beta1 は apps v2beta1 APIグループのAPIスキーマ定義を含みます
>>>>>>> origin/release-5.9

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
<<<<<<< HEAD
| `data` _string_ | EMQX 設定、HOCON 形式、etc/emqx.conf ファイルのような形式 |  |  |
=======
| `data` _string_ | EMQX設定、HOCON形式。etc/emqx.confファイルのような形式 |  |  |
>>>>>>> origin/release-5.9


#### EMQX



<<<<<<< HEAD
EMQX は emqxes API のスキーマです。
=======
EMQXはemqxes APIのスキーマです
>>>>>>> origin/release-5.9



_登場箇所:_
- [EMQXList](#emqxlist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQX` | | |
<<<<<<< HEAD
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | Spec はこのセット内の EMQX ノードの望ましい状態を定義します。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | Status は EMQX ノードの現在の状態を示します。このデータは一定の遅延を伴う可能性があります。 |  |  |
=======
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | Specはこのセット内のEMQXノードの望ましい識別情報を定義します。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | StatusはEMQXノードの現在の状態です。このデータは一定の遅延を伴う場合があります。 |  |  |
>>>>>>> origin/release-5.9


#### EMQXCoreTemplate







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | EMQX コアノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |
=======
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | EMQXコアノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |
>>>>>>> origin/release-5.9


#### EMQXCoreTemplateSpec







_登場箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelector はポッドがノードに適合するために真でなければならないセレクターです。ポッドがノードにスケジュールされるためにノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/concepts/config/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeName はこのポッドを特定のノードにスケジュールする要求です。空でなければ、スケジューラーはリソース要件に適合すると仮定してこのノードにポッドをスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | ポッド割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/concepts/config/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランス。<br />このトレランスが付与されたポッドは、<key,value,effect> のトリプルにマッチする任意のテイントをマッチングオペレーターを使って許容します。<br />TODO: 将来的に `tolerations` を使うべきであり、このフィールドは旧バージョンとの互換性のためだけに存在し、将来的に削除されます。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランス。<br />このトレランスが付与されたポッドは、<key,value,effect> のトリプルにマッチする任意のテイントをマッチングオペレーターを使って許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | トポロジースプレッド制約は、指定されたトポロジー間でマッチするポッドをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicas は指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスですが、個々のレプリカは一貫した識別子を持ちます。<br />デフォルトは 2 です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "minAvailable" ポッドが選択された後も少なくとも利用可能であれば、エビクションが許可されます。<br />例えば "100%" を指定すると、すべての任意のエビクションを防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "maxUnavailable" ポッドが選択された後に利用不可能であっても許容される最大数です。<br />例えば 0 を指定すると、すべての任意のエビクションを防止できます。<br />"minAvailable" と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージの ENTRYPOINT が使用されます。<br />変数参照 $(VAR_NAME) はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。ダブル $$ は単一の $ に変換され、$(VAR_NAME) 構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージの CMD が使用されます。<br />変数参照 $(VAR_NAME) はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。ダブル $$ は単一の $ に変換され、$(VAR_NAME) 構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開することは、システムにネットワーク接続の情報を提供しますが、主に情報提供目的です。ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内で "0.0.0.0" アドレスでリッスンしている任意のポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内に設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内のキーは C_IDENTIFIER でなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />同じキーを持つ Env による値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/config/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContext はポッドレベルのセキュリティ属性と共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContext はコンテナの実行に使用されるセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContext の同等のフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ポッドに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順に実行されます。いずれかの初期化コンテナが失敗すると、ポッドは失敗とみなされ restartPolicy に従い処理されます。初期化コンテナと通常コンテナの名前はすべて一意でなければなりません。<br />初期化コンテナは Lifecycle アクション、Readiness プローブ、Liveness プローブ、Startup プローブを持てません。<br />スケジューリング時には、各リソースタイプの最大要求/制限を考慮し、通常コンテナの合計と比較して最大値を使用します。制限も同様に適用されます。<br />初期化コンテナは現在追加・削除できません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainers はポッドに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存性を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備状態を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbe はポッドが正常に初期化されたことを示します。<br />指定されている場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、livenessProbe が失敗した場合と同様にポッドは再起動されます。<br />ポッドのライフサイクル開始時に異なるパラメータを使用でき、データのロードやキャッシュのウォームアップに時間がかかる場合に有用です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | このフィールド名は VolumeClaimTemplates ですが、実際は PersistentVolumeClaimSpec です。命名が悪く申し訳ありません。<br />PersistentVolumeClaimSpec はストレージデバイスの共通属性を記述し、プロバイダー固有の属性のソースを許可します。<br />EMQXReplicantTemplateSpec よりも詳細です。 |  |  |
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
>>>>>>> origin/release-5.9


#### EMQXList



<<<<<<< HEAD
EMQXList は EMQX のリストを含みます。
=======
EMQXListはEMQXのリストを含みます
>>>>>>> origin/release-5.9





| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQXList` | | |
<<<<<<< HEAD
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
=======
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
>>>>>>> origin/release-5.9
| `items` _[EMQX](#emqx) array_ |  |  |  |


#### EMQXNode







_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `controllerUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `podUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
<<<<<<< HEAD
| `node` _string_ | EMQX ノード名、例: emqx@127.0.0.1 |  |  |
| `node_status` _string_ | EMQX ノードの状態、例: Running |  |  |
| `otp_release` _string_ | EMQX が使用する Erlang/OTP バージョン、例: 24.2/12.2 |  |  |
| `version` _string_ | EMQX バージョン |  |  |
| `role` _string_ | EMQX クラスターのノード役割、列挙: "core" "replicant" |  |  |
| `edition` _string_ | EMQX クラスターのノードエディション、列挙: "Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQX の `/api/v5/nodes` API における `connections` フィールドは MQTT セッション数を意味します。 |  |  |
| `live_connections` _integer_ | EMQX の `/api/v5/nodes` API における `live_connections` フィールドは接続中の MQTT クライアント数を意味します。<br />`live_connections` は EMQX 5.1 以降でのみ有効です。 |  |  |
=======
| `node` _string_ | EMQXノード名、例: emqx@127.0.0.1 |  |  |
| `node_status` _string_ | EMQXノードの状態、例: Running |  |  |
| `otp_release` _string_ | EMQXで使用されているErlang/OTPのバージョン、例: 24.2/12.2 |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | EMQXクラスターのノードロール、列挙: "core" "replicant" |  |  |
| `edition` _string_ | EMQXクラスターのノードエディション、列挙: "Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`connections`フィールドはMQTTセッション数を意味します。 |  |  |
| `live_connections` _integer_ | EMQXの`/api/v5/nodes` APIにおける`live_connections`フィールドは接続中のMQTTクライアント数を意味します。<br />`live_connections`はEMQX 5.1以降でのみ有効です。 |  |  |
>>>>>>> origin/release-5.9


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
<<<<<<< HEAD
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQX レプリカントノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller tools は複雑なバリデーション（oneOf/anyOf/allOf など）をサポートしていないため、バリデーションルールを使用してください。https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |
=======
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQXレプリカントノードの望ましい動作の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller toolsは複雑なバリデーション(oneOf/anyOf/allOfなど)をサポートしていないため、バリデーションルールを使用してください。https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |
>>>>>>> origin/release-5.9


#### EMQXReplicantTemplateSpec







_登場箇所:_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelector はポッドがノードに適合するために真でなければならないセレクターです。ポッドがノードにスケジュールされるためにノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/concepts/config/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeName はこのポッドを特定のノードにスケジュールする要求です。空でなければ、スケジューラーはリソース要件に適合すると仮定してこのノードにポッドをスケジュールします。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | ポッド割り当てのためのアフィニティ<br />参照: https://kubernetes.io/docs/concepts/config/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランス。<br />このトレランスが付与されたポッドは、<key,value,effect> のトリプルにマッチする任意のテイントをマッチングオペレーターを使って許容します。<br />TODO: 将来的に `tolerations` を使うべきであり、このフィールドは旧バージョンとの互換性のためだけに存在し、将来的に削除されます。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 指定された場合、ポッドのトレランス。<br />このトレランスが付与されたポッドは、<key,value,effect> のトリプルにマッチする任意のテイントをマッチングオペレーターを使って許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | トポロジースプレッド制約は、指定されたトポロジー間でマッチするポッドをどのように分散させるかを指定します。 |  |  |
| `replicas` _integer_ | Replicas は指定されたテンプレートの望ましいレプリカ数です。<br />これらは同じテンプレートのインスタンスですが、個々のレプリカは一貫した識別子を持ちます。<br />デフォルトは 2 です。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "minAvailable" ポッドが選択された後も少なくとも利用可能であれば、エビクションが許可されます。<br />例えば "100%" を指定すると、すべての任意のエビクションを防止できます。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | "maxUnavailable" ポッドが選択された後に利用不可能であっても許容される最大数です。<br />例えば 0 を指定すると、すべての任意のエビクションを防止できます。<br />"minAvailable" と排他的な設定です。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | エントリポイントの配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージの ENTRYPOINT が使用されます。<br />変数参照 $(VAR_NAME) はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。ダブル $$ は単一の $ に変換され、$(VAR_NAME) 構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージの CMD が使用されます。<br />変数参照 $(VAR_NAME) はコンテナの環境変数で展開されます。解決できない場合は文字列は変更されません。ダブル $$ は単一の $ に変換され、$(VAR_NAME) 構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。ここでポートを公開することは、システムにネットワーク接続の情報を提供しますが、主に情報提供目的です。ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内で "0.0.0.0" アドレスでリッスンしている任意のポートはネットワークからアクセス可能です。<br />更新不可。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | コンテナ内に設定する環境変数のリスト。<br />更新不可。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内のキーは C_IDENTIFIER でなければなりません。無効なキーはコンテナ起動時にイベントとして報告されます。複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />同じキーを持つ Env による値が優先されます。<br />更新不可。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | このコンテナに必要な計算リソース。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/config/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContext はポッドレベルのセキュリティ属性と共通のコンテナ設定を保持します。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContext はコンテナの実行に使用されるセキュリティオプションを定義します。<br />設定されている場合、PodSecurityContext の同等のフィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ポッドに属する初期化コンテナのリスト。<br />初期化コンテナはコンテナ起動前に順に実行されます。いずれかの初期化コンテナが失敗すると、ポッドは失敗とみなされ restartPolicy に従い処理されます。初期化コンテナと通常コンテナの名前はすべて一意でなければなりません。<br />初期化コンテナは Lifecycle アクション、Readiness プローブ、Liveness プローブ、Startup プローブを持てません。<br />スケジューリング時には、各リソースタイプの最大要求/制限を考慮し、通常コンテナの合計と比較して最大値を使用します。制限も同様に適用されます。<br />初期化コンテナは現在追加・削除できません。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainers はポッドに追加される追加コンテナを表します。<br />詳細: https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 詳細: https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナの生存性を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナは再起動されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | コンテナのサービス準備状態を定期的にチェックするプローブ。<br />プローブが失敗するとコンテナはサービスエンドポイントから除外されます。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbe はポッドが正常に初期化されたことを示します。<br />指定されている場合、これが成功するまで他のプローブは実行されません。<br />このプローブが失敗すると、livenessProbe が失敗した場合と同様にポッドは再起動されます。<br />ポッドのライフサイクル開始時に異なるパラメータを使用でき、データのロードやキャッシュのウォームアップに時間がかかる場合に有用です。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
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
>>>>>>> origin/release-5.9
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対して管理システムが取るべきアクション。<br />更新不可。 |  |  |


#### EMQXSpec



<<<<<<< HEAD
EMQXSpec は EMQX の望ましい状態を定義します。
=======
EMQXSpecはEMQXの望ましい状態を定義します
>>>>>>> origin/release-5.9



_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `image` _string_ | EMQX イメージ名。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#pullpolicy-v1-core)_ | イメージプルポリシー。<br />Always, Never, IfNotPresent のいずれか。<br />:latest タグが指定されている場合はデフォルトで Always、そうでなければ IfNotPresent。<br />更新不可。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#localobjectreference-v1-core) array_ | ImagePullSecrets はこの PodSpec で使用されるイメージをプルするために同じネームスペース内のシークレットへの参照のオプションリストです。<br />指定された場合、これらのシークレットは個々のプラー実装に渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | サービスアカウント名<br />ReplicaSet または StatefulSet を指定されたサービスアカウントと関連付けて認証に使用します。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) array_ | EMQX ブートストラップユーザー<br />更新不可。 |  |  |
| `config` _[Config](#config)_ | EMQX 設定 |  |  |
| `clusterDomain` _string_ |  | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古い ReplicaSet、古い StatefulSet、古い PersistentVolumeClaim の数。<br />明示的なゼロと未指定を区別するためのポインター。<br />デフォルトは 3。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | UpdateStrategy は EMQX のブルーグリーンアップデート戦略を記述するオブジェクトです。 | \{ evacuationStrategy:map[connEvictRate:1000 sessEvictRate:1000 waitTakeover:10] initialDelaySeconds:10 type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | CoreTemplate は作成される EMQX コアノードを記述するオブジェクトです。 | \{ spec:map[replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | ReplicantTemplate は作成される EMQX レプリカントノードを記述するオブジェクトです。 |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | DashboardServiceTemplate は作成される EMQX ダッシュボードサービスを記述するオブジェクトです。<br />このサービスは常に EMQX コアノードをセレクトします。 |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | ListenersServiceTemplate は作成される EMQX リスナーサービスを記述するオブジェクトです。<br />EMQX レプリカントノードが存在する場合、このサービスはレプリカントノードをセレクトします。<br />存在しない場合はコアノードをセレクトします。 |  |  |
=======
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
>>>>>>> origin/release-5.9


#### EMQXStatus



<<<<<<< HEAD
EMQXStatus は EMQX の観測された状態を定義します。
=======
EMQXStatusはEMQXの観測された状態を定義します
>>>>>>> origin/release-5.9



_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#condition-v1-meta) array_ | EMQX カスタムリソースの現在の状態に関する最新の観測結果を表します。 |  |  |
=======
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#condition-v1-meta) array_ | EMQXカスタムリソースの最新の状態観測を表します。 |  |  |
>>>>>>> origin/release-5.9
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
<<<<<<< HEAD
| `connEvictRate` _integer_ | EMQX Enterprise でのみ有効です。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | EMQX Enterprise でのみ有効です。 | 1000 | 最小値: 1 <br /> |
=======
| `connEvictRate` _integer_ | EMQX Enterpriseでのみ有効です。 | 1000 | 最小値: 1 <br /> |
| `sessEvictRate` _integer_ | EMQX Enterpriseでのみ有効です。 | 1000 | 最小値: 1 <br /> |
>>>>>>> origin/release-5.9


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



<<<<<<< HEAD
Rebalance は rebalances API のスキーマです。
=======
Rebalanceはrebalances APIのスキーマです
>>>>>>> origin/release-5.9



_登場箇所:_
- [RebalanceList](#rebalancelist)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `Rebalance` | | |
<<<<<<< HEAD
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
=======
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
>>>>>>> origin/release-5.9
| `spec` _[RebalanceSpec](#rebalancespec)_ |  |  |  |
| `status` _[RebalanceStatus](#rebalancestatus)_ |  |  |  |


#### RebalanceCondition



<<<<<<< HEAD
RebalanceCondition は EMQX のリバランスジョブの現在の状態を表します。
=======
RebalanceConditionはEMQXリバランスジョブの現在の状態を表します。
>>>>>>> origin/release-5.9



_登場箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | リバランス条件のタイプの状態。Processing、Complete、Failed のいずれか。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件の状態。True、False、Unknown のいずれか。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | この条件が最後に更新された時刻。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件がある状態から別の状態に遷移した最後の時刻。 |  |  |
=======
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | リバランス条件タイプの状態。Processing、Complete、Failedのいずれか。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件の状態。True、False、Unknownのいずれか。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | この条件が最後に更新された時間。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件がある状態から別の状態に最後に遷移した時間。 |  |  |
>>>>>>> origin/release-5.9
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



<<<<<<< HEAD
RebalanceList は Rebalance のリストを含みます。
=======
RebalanceListはRebalanceのリストを含みます
>>>>>>> origin/release-5.9





| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `RebalanceList` | | |
<<<<<<< HEAD
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
=======
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
>>>>>>> origin/release-5.9
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



<<<<<<< HEAD
RebalanceSpec は Rebalance の望ましい状態を定義します。
=======
RebalanceSpecはRebalanceの望ましい状態を定義します
>>>>>>> origin/release-5.9



_登場箇所:_
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `instanceKind` _string_ | InstanceKind は EMQX と EMQXEnterprise を区別するために使われます。<br />"EMQX" に設定されている場合は EMQX CR が v2beta1 であることを意味し、<br />"EmqxEnterprise" に設定されている場合は EmqxEnterprise CR が v1beta4 であることを意味します。 | EMQX |  |
| `instanceName` _string_ | InstanceName は EMQX CR の名前を表します。EMQX Enterprise でのみ有効です。 |  | 必須: \{\} <br /> |
| `rebalanceStrategy` _[RebalanceStrategy](#rebalancestrategy)_ | RebalanceStrategy は EMQX のリバランス戦略を表します。<br />詳細: https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing |  | 必須: \{\} <br /> |
=======
| `instanceKind` _string_ | InstanceKindはEMQXとEMQXEnterpriseを区別するために使われます。<br />"EMQX"に設定されている場合はEMQX CRがv2beta1であることを意味し、<br />"EmqxEnterprise"に設定されている場合はEmqxEnterprise CRがv1beta4であることを意味します。 | EMQX |  |
| `instanceName` _string_ | InstanceNameはEMQX CRの名前を表し、EMQX Enterpriseでのみ有効です。 |  | 必須: \{\} <br /> |
| `rebalanceStrategy` _[RebalanceStrategy](#rebalancestrategy)_ | RebalanceStrategyはEMQXリバランスの戦略を表します。<br />詳細: https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing |  | 必須: \{\} <br /> |
>>>>>>> origin/release-5.9


#### RebalanceState



<<<<<<< HEAD
RebalanceState は EMQX のリバランスの観測された状態を定義します。
=======
RebalanceStateはEMQXの観測されたリバランス状態を定義します
>>>>>>> origin/release-5.9



_登場箇所:_
- [RebalanceStatus](#rebalancestatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `state` _string_ | State は emqx クラスターのリバランス状態を表します。 |  |  |
| `session_eviction_rate` _integer_ | SessionEvictionRate はノードのセッションエバキュエーション率（秒あたり）を表します。 |  |  |
| `recipients` _string array_ | Recipients はリバランスの対象ノードを表します。 |  |  |
| `node` _string_ | Node はリバランスをスケジュールするノードを表します。 |  |  |
| `donors` _string array_ | Donors はリバランスのソースノードを表します。 |  |  |
| `coordinator_node` _string_ | CoordinatorNode は現在リバランス中のノードを表します。 |  |  |
| `connection_eviction_rate` _integer_ | ConnectionEvictionRate はノードの接続エバキュエーション率（秒あたり）を表します。 |  |  |
=======
| `state` _string_ | EMQXクラスターリバランスの状態を表します。 |  |  |
| `session_eviction_rate` _integer_ | ノードのセッションエバキュエーション率（秒あたり）を表します。 |  |  |
| `recipients` _string array_ | リバランスのターゲットノードを表します。 |  |  |
| `node` _string_ | リバランスのスケジューリングノードを表します。 |  |  |
| `donors` _string array_ | リバランスのソースノードを表します。 |  |  |
| `coordinator_node` _string_ | 現在リバランス中のノードを表します。 |  |  |
| `connection_eviction_rate` _integer_ | ノードの接続エバキュエーション率（秒あたり）を表します。 |  |  |
>>>>>>> origin/release-5.9


#### RebalanceStatus



<<<<<<< HEAD
RebalanceStatus は Rebalance の現在の状態を表します。
=======
RebalanceStatusはRebalanceの現在の状態を表します
>>>>>>> origin/release-5.9



_登場箇所:_
- [Rebalance](#rebalance)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | オブジェクトの現在の状態に関する最新の観測結果。<br />Rebalance が失敗した場合、条件はタイプ "Failed" かつステータス false になります。<br />処理中の場合、条件はタイプ "Processing" かつステータス true になります。<br />完了した場合、条件はタイプ "Complete" かつステータス true になります。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | Phase は Rebalance のフェーズを表します。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | StartedTime はリバランスジョブが開始された時刻を表します。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | CompletedTime はリバランスジョブが完了した時刻を表します。 |  |  |
=======
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | オブジェクトの最新の状態観測。<br />Rebalanceが失敗した場合、条件は"type"が"Failed"で"status"がfalseになります。<br />Rebalanceが処理中の場合、条件は"type"が"Processing"で"status"がtrueになります。<br />Rebalanceが完了した場合、条件は"type"が"Complete"で"status"がtrueになります。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | Rebalanceのフェーズを表します。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | リバランスジョブ開始時刻を表します。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | リバランスジョブ完了時刻を表します。 |  |  |
>>>>>>> origin/release-5.9


#### RebalanceStrategy



<<<<<<< HEAD
RebalanceStrategy は EMQX のリバランス戦略を表します。
=======
RebalanceStrategyはEMQXリバランスの戦略を表します
>>>>>>> origin/release-5.9



_登場箇所:_
- [RebalanceSpec](#rebalancespec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
<<<<<<< HEAD
| `connEvictRate` _integer_ | ConnEvictRate はソースノードのクライアント切断率（秒あたり）を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の conn-evict-rate と同じです。<br />値は 0 より大きくなければなりません。 |  | 最小値: 1 <br />必須: \{\} <br /> |
| `sessEvictRate` _integer_ | SessEvictRate はソースノードのセッションエバキュエーション率（秒あたり）を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の sess-evict-rate と同じです。<br />値は 0 より大きくなければなりません。<br />デフォルトは 500 です。 | 500 |  |
| `waitTakeover` _integer_ | WaitTakeover はすべての接続が切断された後にクライアントがセッションを引き継ぐために再接続を待つ秒数を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の wait-takeover と同じです。<br />値は 0 より大きくなければなりません。<br />デフォルトは 60 秒です。 | 60 |  |
| `waitHealthCheck` _integer_ | WaitHealthCheck はロードバランサーがソースノードをアクティブなバックエンドノードリストから除外するまでの待機時間（秒）を表します。<br />指定された待機時間を超えるとリバランスタスクが開始されます。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の wait-health-check と同じです。<br />値は 0 より大きくなければなりません。<br />デフォルトは 60 秒です。 | 60 |  |
| `absConnThreshold` _integer_ | AbsConnThreshold は接続バランスの絶対閾値を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の abs-conn-threshold と同じです。<br />値は 0 より大きくなければなりません。<br />デフォルトは 1000 です。 | 1000 |  |
| `relConnThreshold` _string_ | RelConnThreshold は接続バランスの相対閾値を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の rel-conn-threshold と同じです。<br />浮動小数点の使用は言語間でのサポート差異が大きいため推奨されません。<br />そのため RelConnThreshold は文字列型として定義されており、浮動小数点型ではありません。<br />値は "1.0" より大きくなければなりません。<br />デフォルトは "1.1" です。 | 1.1 |  |
| `absSessThreshold` _integer_ | AbsSessThreshold はセッション接続バランスの絶対閾値を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の abs-sess-threshold と同じです。<br />値は 0 より大きくなければなりません。<br />デフォルトは 1000 です。 | 1000 |  |
| `relSessThreshold` _string_ | RelSessThreshold はセッション接続バランスの相対閾値を表します。<br />[EMQX リバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) の rel-sess-threshold と同じです。<br />浮動小数点の使用は言語間でのサポート差異が大きいため推奨されません。<br />そのため RelSessThreshold は文字列型として定義されており、浮動小数点型ではありません。<br />値は "1.0" より大きくなければなりません。<br />デフォルトは "1.1" です。 | 1.1 |  |
=======
| `connEvictRate` _integer_ | ConnEvictRateはソースノードのクライアント切断率（秒あたり）を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のconn-evict-rateと同じです。<br />値は0より大きくなければなりません。 |  | 最小値: 1 <br />必須: \{\} <br /> |
| `sessEvictRate` _integer_ | SessEvictRateはソースノードのセッションエバキュエーション率（秒あたり）を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のsess-evict-rateと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは500です。 | 500 |  |
| `waitTakeover` _integer_ | WaitTakeoverはすべての接続が切断された後、クライアントが再接続してセッションを引き継ぐまでの待機時間（秒）を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-takeoverと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `waitHealthCheck` _integer_ | WaitHealthCheckはロードバランサーがソースノードをアクティブなバックエンドノードのリストから削除するまでの待機時間（秒）を表します。<br />指定された待機時間を超えるとリバランスタスクが開始されます。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のwait-health-checkと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは60秒です。 | 60 |  |
| `absConnThreshold` _integer_ | AbsConnThresholdは接続バランスの絶対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-conn-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relConnThreshold` _string_ | RelConnThresholdは接続バランスの相対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-conn-thresholdと同じです。<br />浮動小数点の使用は言語間でのサポート差異が大きいため強く推奨されません。<br />そのためRelConnThresholdフィールドは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
| `absSessThreshold` _integer_ | AbsSessThresholdはセッション接続バランスの絶対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のabs-sess-thresholdと同じです。<br />値は0より大きくなければなりません。<br />デフォルトは1000です。 | 1000 |  |
| `relSessThreshold` _string_ | RelSessThresholdはセッション接続バランスの相対閾値を表します。<br />[EMQXリバランス](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing)のrel-sess-thresholdと同じです。<br />浮動小数点の使用は言語間でのサポート差異が大きいため強く推奨されません。<br />そのためRelSessThresholdフィールドは文字列型で定義されており、浮動小数点型ではありません。<br />値は"1.0"より大きくなければなりません。<br />デフォルトは"1.1"です。 | 1.1 |  |
>>>>>>> origin/release-5.9


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
<<<<<<< HEAD
| `enabled` _boolean_ | EMQX Operator は EMQX ノード用のサービスを作成します。<br />これは `false` と未指定を区別するためのポインターです。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata` のフィールドについては Kubernetes API ドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#servicespec-v1-core)_ | Spec はサービスの動作を定義します。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |
=======
| `enabled` _boolean_ | EMQX OperatorはEMQXノード用のサービスを作成します。<br />これは`false`と未指定を区別するためのポインターです。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | `metadata`のフィールドについてはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#servicespec-v1-core)_ | Specはサービスの動作を定義します。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |
>>>>>>> origin/release-5.9


#### UpdateStrategy







_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _string_ |  | Recreate | 列挙: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | エバキュエーション接続開始までの秒数。 |  |  |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | エバキュエーション接続タイムアウトまでの秒数。 |  |  |
