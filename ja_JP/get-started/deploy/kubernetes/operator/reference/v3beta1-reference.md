# APIリファレンス (v3beta1)

## パッケージ
- [apps.emqx.io/v3beta1](#appsemqxiov3beta1)


## apps.emqx.io/v3beta1

Package v3beta1 は apps.emqx.io v3beta1 APIグループのAPIスキーマ定義を含みます。

### リソースタイプ
- [EMQX](#emqx)


#### Config

_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `roots` _[ConfigRoots](#configroots)_ | トップレベルのEMQX設定ルート。値はJSON互換である必要があります。Operatorは<br />ランタイム適用可能なルートを`base.hocon`にシリアライズし、EMQX起動時に有効となる設定を`emqx.conf`にシリアライズします。<br />includes、substitutions、重複宣言などのHOCON専用構文はサポートされません。<br />ルートを削除するとOperatorの所有権は放棄されますが、EMQXによって永続化された値は削除されません。<br />`node.cookie`パスはOperator専用であり、ここで指定してはいけません。 |  |  |


#### ConfigRoots

_基底型:_ _[JSON](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#json-v1-apiextensions-k8s-io)_

_登場箇所:_
- [Config](#config)


#### ConfigStatus

ConfigStatusはコントローラーが所有する調整チェックポイントを含みます。  
これらのフィールドは情報提供のための実装詳細であり、変更される可能性があります。

_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `desiredRevision` _string_ | spec.config.rootsにある完全な望ましい設定のリビジョン。 |  |  |
| `runtimeRevision` _string_ | EMQX APIによって最後に受け入れられたランタイム設定のリビジョン。<br />初回起動前はクラスターのブートストラップ用にステージされたリビジョンです。 |  |  |
| `desiredStartupRevision` _string_ | EMQX起動時に有効となる望ましい設定のリビジョン。 |  |  |
| `activeStartupRevisions` _string array_ | 起動構成のリビジョンで、準備完了状態のPodが使用しています。<br />複数のリビジョンがある場合、準備完了Podが異なるバージョンの設定で起動されたことを示します。 |  |  |


#### CoreNodesStatus

CoreNodesStatusは単一のStatefulSetで管理されるコアノードの概要ステータスです。

_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `readyReplicas` _integer_ | 準備完了レプリカ数。 |  |  |
| `updatedReplicas` _integer_ | 望ましいPodテンプレートに既に更新されたレプリカ数。 |  |  |
| `currentReplicas` _integer_ | まだ前のPodテンプレートで稼働しているレプリカ数。 |  |  |


#### DSDBReplicationStatus

_登場箇所:_
- [DSReplicationStatus](#dsreplicationstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `name` _string_ | データベース名 |  |  |
| `numShards` _integer_ | データベースのシャード数 |  |  |
| `numShardReplicas` _integer_ | シャードレプリカの総数 |  |  |
| `lostShardReplicas` _integer_ | ロストサイトに属するシャードレプリカの総数 |  |  |
| `numTransitions` _integer_ | 現在のシャード所有権の遷移数 |  |  |
| `minReplicas` _integer_ | データベースシャードの最小レプリケーション係数 |  |  |
| `maxReplicas` _integer_ | データベースシャードの最大レプリケーション係数 |  |  |


#### DSReplicationStatus

データベースごとのDSレプリケーションステータスの概要。

_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `dbs` _[DSDBReplicationStatus](#dsdbreplicationstatus) array_ |  |  |  |


#### EMQX

EMQXクラスターを表すカスタムリソース。

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v3beta1` |  |  |
| `kind` _string_ | `EMQX` |  |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | `metadata`のフィールドはKubernetes APIドキュメントを参照してください。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | EMQXクラスターの望ましい状態の仕様。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | EMQXクラスターの現在の状態。 |  |  |


#### EMQXCoreTemplate

_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[TemplateObjectMeta](#templateobjectmeta)_ | このテンプレートから生成されるオブジェクトに適用されるメタデータ。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | コアノードの望ましい状態の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status | \{  \} |  |


#### EMQXCoreTemplateSpec

_登場箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Podがノードに割り当てられるために満たす必要のあるセレクター。<br />Podがノードにスケジュールされるには、ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/ |  |  |
| `nodeName` _string_ | 特定のノードにこのPodをスケジュールするリクエスト。<br />空でなければ、スケジューラーはリソース要件を満たすと仮定してこのノードにPodを割り当てます。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ。<br />参照: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) array_ | Podのトレランス。<br />指定された場合、Podは<key,value,effect>のトリプルにマッチする任意のtaintを許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) array_ | 指定されたトポロジー間でマッチするPodをどのように分散させるかを指定します。 |  |  |
| `dnsConfig` _[PodDNSConfig](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#poddnsconfig-v1-core)_ | PodのDNSパラメータを指定します。<br />ここで指定されたパラメータはDNSPolicy（常にClusterFirst）に基づく生成されたDNS設定にマージされます。<br />詳細: https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-dns-config |  |  |
| `minReadySeconds` _integer_ | Podが利用可能とみなされるためにReady状態である必要がある最小時間（秒）。<br />コアノードにはStatefulSetに適用されます（apps/v1 StatefulSetSpec.minReadySecondsに準拠）。<br />レプリカントにはReplicaSetに適用されます（apps/v1 ReplicaSetSpec.minReadySecondsに準拠）。<br />省略または0はapps/v1のデフォルト（0）と同じです。 |  | 最小値: 0 <br /> |
| `command` _string array_ | エントリポイント配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照`$(VAR_NAME)`はコンテナの環境変数を使って展開されます。解決できない場合は文字列は変更されません。`$$`は単一の`$`に変換され、`$(VAR_NAME)`構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  | オプション: \{\} <br /> |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照`$(VAR_NAME)`はコンテナの環境変数を使って展開されます。解決できない場合は文字列は変更されません。`$$`は単一の`$`に変換され、`$(VAR_NAME)`構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。<br />ここでポートを公開することはネットワーク接続に関する追加情報をシステムに提供しますが、主に情報的なものです。<br />ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内で`0.0.0.0`アドレスでリッスンしているポートはネットワークからアクセス可能です。<br />ポート名`dashboard`および`dashboard-https`はOperatorによって予約されており、テンプレートで指定できません。<br />これらの名前付きポートは`spec.config.roots.dashboard.listeners`からOperatorが派生し、プローブ、Service、Pod単位のAPIリクエストに使用されます。<br />コンテナポートを変更する場合は対応するリスナーのバインドを変更してください。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) array_ | コンテナ内に設定する環境変数のリスト。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内で定義されるキーはC_IDENTIFIERである必要があります。無効なキーはコンテナ起動時にイベントとして報告されます。<br />複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />Envで重複キーが定義されている場合はそちらが優先されます。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/)のリンク_ | このコンテナが必要とする計算リソース。 |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Podレベルのセキュリティ属性および共通のコンテナ設定。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | コンテナ実行時のセキュリティオプション。<br />設定されている場合、PodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ |  |  |
| `initContainers` _[Container](https://kubernetes.io/docs/concepts/workloads/pods/init-containers/)のリンク_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナは通常のコンテナ起動前に順番に実行されます。失敗するとPodは失敗とみなされ、restartPolicyに従って処理されます。<br />初期化コンテナおよび通常コンテナの名前はすべてユニークでなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />スケジューリング時には、各リソースタイプの最大要求/制限値が考慮されます。制限も同様に適用されます。 |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) array_ | メインコンテナと共に実行される追加コンテナ。 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) array_ | Podに提供される追加ボリューム。 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) array_ | 追加ボリュームがメインコンテナにマウントされる方法を指定。 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes)_ | コンテナの生存確認を定期的に行うプローブ。<br />失敗するとコンテナは再起動されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes)_ | コンテナのサービス準備完了状態を定期的に確認するプローブ。<br />失敗するとコンテナはサービスエンドポイントから除外されます。<br />Operatorによるノード退避やスケーリング、ローリングアップデートを考慮して、現在のデフォルト値を維持することを強く推奨します。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:1 httpGet:map[path:/api/v5/load_rebalance/availability_check port:dashboard] initialDelaySeconds:10 periodSeconds:5 timeoutSeconds:3 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes)_ | Podの初期化成功を示すプローブ。<br />指定されている場合、成功するまで他のプローブは実行されません。<br />失敗すると`livenessProbe`失敗時と同様にPodは再起動されます。<br />Podのライフサイクル初期段階でデータロードやキャッシュウォームアップに時間がかかる場合に、通常運用時とは異なるパラメータを設定可能です。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対する管理システムのアクション。 |  |  |
| `replicas` _integer_ | 望ましいコアノード数。各インスタンスは一貫したIDを持ちます。 | 1 | 最小値: 0 <br /> |
| `persistentVolumeClaimSpec` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#persistentvolumeclaimspec-v1-core)_ | コアノードのデータストレージ用PVC仕様。 |  |  |


#### EMQXNode

_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `name` _string_ | ノード名 |  |  |
| `podName` _string_ | 対応するPod名 |  |  |
| `status` _string_ | ノードの状態 |  |  |
| `otpRelease` _string_ | ノードが動作しているErlang/OTPのバージョン |  |  |
| `version` _string_ | EMQXのバージョン |  |  |
| `role` _string_ | ノードの役割。`core`または`replicant`。不明な場合は省略。 |  |  |
| `sessions` _integer_ | MQTTセッション数 |  |  |
| `connections` _integer_ | 接続中のMQTTクライアント数 |  |  |


#### EMQXReplicantTemplate

_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `metadata` _[TemplateObjectMeta](#templateobjectmeta)_ | このテンプレートから生成されるオブジェクトに適用されるメタデータ。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | レプリカントノードの望ましい状態の仕様。<br />詳細: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status | \{ replicas:0 \} |  |


#### EMQXReplicantTemplateSpec

_登場箇所:_
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Podがノードに割り当てられるために満たす必要のあるセレクター。<br />Podがノードにスケジュールされるには、ノードのラベルと一致する必要があります。<br />詳細: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/ |  |  |
| `nodeName` _string_ | 特定のノードにこのPodをスケジュールするリクエスト。<br />空でなければ、スケジューラーはリソース要件を満たすと仮定してこのノードにPodを割り当てます。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Pod割り当てのためのアフィニティ。<br />参照: https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) array_ | Podのトレランス。<br />指定された場合、Podは<key,value,effect>のトリプルにマッチする任意のtaintを許容します。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) array_ | 指定されたトポロジー間でマッチするPodをどのように分散させるかを指定します。 |  |  |
| `dnsConfig` _[PodDNSConfig](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#poddnsconfig-v1-core)_ | PodのDNSパラメータを指定します。<br />ここで指定されたパラメータはDNSPolicy（常にClusterFirst）に基づく生成されたDNS設定にマージされます。<br />詳細: https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-dns-config |  |  |
| `minReadySeconds` _integer_ | Podが利用可能とみなされるためにReady状態である必要がある最小時間（秒）。<br />コアノードにはStatefulSetに適用されます（apps/v1 StatefulSetSpec.minReadySecondsに準拠）。<br />レプリカントにはReplicaSetに適用されます（apps/v1 ReplicaSetSpec.minReadySecondsに準拠）。<br />省略または0はapps/v1のデフォルト（0）と同じです。 |  | 最小値: 0 <br /> |
| `command` _string array_ | エントリポイント配列。シェル内で実行されません。<br />指定しない場合はコンテナイメージのENTRYPOINTが使用されます。<br />変数参照`$(VAR_NAME)`はコンテナの環境変数を使って展開されます。解決できない場合は文字列は変更されません。`$$`は単一の`$`に変換され、`$(VAR_NAME)`構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。更新不可。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  | オプション: \{\} <br /> |
| `args` _string array_ | エントリポイントへの引数。<br />指定しない場合はコンテナイメージのCMDが使用されます。<br />変数参照`$(VAR_NAME)`はコンテナの環境変数を使って展開されます。解決できない場合は文字列は変更されません。`$$`は単一の`$`に変換され、`$(VAR_NAME)`構文のエスケープが可能です。<br />エスケープされた参照は変数の有無に関わらず展開されません。<br />詳細: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) array_ | コンテナから公開するポートのリスト。<br />ここでポートを公開することはネットワーク接続に関する追加情報をシステムに提供しますが、主に情報的なものです。<br />ここでポートを指定しなくても、そのポートの公開は妨げられません。コンテナ内で`0.0.0.0`アドレスでリッスンしているポートはネットワークからアクセス可能です。<br />ポート名`dashboard`および`dashboard-https`はOperatorによって予約されており、テンプレートで指定できません。<br />これらの名前付きポートは`spec.config.roots.dashboard.listeners`からOperatorが派生し、プローブ、Service、Pod単位のAPIリクエストに使用されます。<br />コンテナポートを変更する場合は対応するリスナーのバインドを変更してください。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) array_ | コンテナ内に設定する環境変数のリスト。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) array_ | コンテナ内の環境変数を設定するためのソースのリスト。<br />ソース内で定義されるキーはC_IDENTIFIERである必要があります。無効なキーはコンテナ起動時にイベントとして報告されます。<br />複数のソースに同じキーが存在する場合、最後のソースの値が優先されます。<br />Envで重複キーが定義されている場合はそちらが優先されます。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/)のリンク_ | このコンテナが必要とする計算リソース。 |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Podレベルのセキュリティ属性および共通のコンテナ設定。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | コンテナ実行時のセキュリティオプション。<br />設定されている場合、PodSecurityContextの同等フィールドを上書きします。<br />詳細: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ |  |  |
| `initContainers` _[Container](https://kubernetes.io/docs/concepts/workloads/pods/init-containers/)のリンク_ | Podに属する初期化コンテナのリスト。<br />初期化コンテナは通常のコンテナ起動前に順番に実行されます。失敗するとPodは失敗とみなされ、restartPolicyに従って処理されます。<br />初期化コンテナおよび通常コンテナの名前はすべてユニークでなければなりません。<br />初期化コンテナはLifecycleアクション、Readinessプローブ、Livenessプローブ、Startupプローブを持てません。<br />スケジューリング時には、各リソースタイプの最大要求/制限値が考慮されます。制限も同様に適用されます。 |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) array_ | メインコンテナと共に実行される追加コンテナ。 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) array_ | Podに提供される追加ボリューム。 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) array_ | 追加ボリュームがメインコンテナにマウントされる方法を指定。 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes)_ | コンテナの生存確認を定期的に行うプローブ。<br />失敗するとコンテナは再起動されます。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes)_ | コンテナのサービス準備完了状態を定期的に確認するプローブ。<br />失敗するとコンテナはサービスエンドポイントから除外されます。<br />Operatorによるノード退避やスケーリング、ローリングアップデートを考慮して、現在のデフォルト値を維持することを強く推奨します。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:1 httpGet:map[path:/api/v5/load_rebalance/availability_check port:dashboard] initialDelaySeconds:10 periodSeconds:5 timeoutSeconds:3 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes)_ | Podの初期化成功を示すプローブ。<br />指定されている場合、成功するまで他のプローブは実行されません。<br />失敗すると`livenessProbe`失敗時と同様にPodは再起動されます。<br />Podのライフサイクル初期段階でデータロードやキャッシュウォームアップに時間がかかる場合に、通常運用時とは異なるパラメータを設定可能です。<br />詳細: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | コンテナのライフサイクルイベントに対する管理システムのアクション。 |  |  |
| `replicas` _integer_ | 望ましいレプリカントノード数。 | 0 | 最小値: 0 <br /> |


#### EMQXSpec

EMQXSpecはEMQXの望ましい状態を定義します。

_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `image` _string_ | EMQXコンテナイメージ。<br />詳細: https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#pullpolicy-v1-core)_ | コンテナイメージのプルポリシー。<br />`Always`、`Never`、`IfNotPresent`のいずれか。<br />`:latest`タグ指定時はデフォルトで`Always`、それ以外は`IfNotPresent`。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#localobjectreference-v1-core) array_ | このPodSpecで使用されるイメージのプルに使う同一ネームスペース内のシークレットへの参照リスト（オプション）。<br />指定すると、これらのシークレットが個別のプラー実装に渡されます。<br />詳細: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | ServiceAccount名。<br />管理されるReplicaSetおよびStatefulSetは認証のため指定されたServiceAccountに関連付けられます。<br />詳細: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `config` _[Config](#config)_ | EMQX設定。 |  |  |
| `clusterDomain` _string_ | Kubernetesクラスターのドメイン。 | cluster.local |  |
| `revisionHistoryLimit` _integer_ | ロールバックを可能にするために保持する古いReplicaSetの数。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | クラスターアップグレード戦略の設定。 | \{ type:RollingUpdate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | EMQXコアノードを実行するPodのテンプレート。 | \{ spec:map[persistentVolumeClaimSpec:map[accessModes:[ReadWriteOnce] resources:map[requests:map[storage:500Mi]]] replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | EMQXレプリカントノードを実行するPodのテンプレート。 | \{ spec:map[replicas:0] \} |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | EMQXダッシュボードを公開するServiceのテンプレート。<br />ダッシュボードServiceは常にEMQXコアノードのセットを指します。<br />テンプレート内の`dashboard`または`dashboard-https`という名前のポートは対応する<br />生成されたServiceポートを上書きします。その`port`は異なるServiceポートでリスナーを公開できますが、<br />`targetPort`は対応するダッシュボードリスナーに解決される必要があります。リスナーのバインド変更に追従するため、予約済みの名前付きターゲットポートを使うことを推奨します。 |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | 有効なEMQXリスナーを公開するServiceのテンプレート。<br />リスナーServiceは有効かつ存在する場合、EMQXレプリカントノードのセットを指します。<br />そうでなければEMQXコアノードのセットを指します。 |  |  |


#### EMQXStatus

EMQXStatusはEMQXの観測された状態を定義します。

_登場箇所:_
- [EMQX](#emqx)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#condition-v1-meta) array_ | EMQXカスタムリソースの現在の状態を表す条件。 |  |  |
| `coreReplicas` _integer_ | コアセットの観測されたレプリカ数。 |  |  |
| `coreSelector` _string_ | コアPodにマッチするシリアライズされたラベルセレクター。 |  |  |
| `replicantReplicas` _integer_ | レプリカントセットPodの観測されたレプリカ数。<br />スケールサブリソースで使用。 |  |  |
| `replicantSelector` _string_ | レプリカントPodにマッチするシリアライズされたラベルセレクター。<br />HPAのPod検出にスケールサブリソースで使用。 |  |  |
| `coreNodesStatus` _[CoreNodesStatus](#corenodesstatus)_ | コアノードセットの概要ステータス。 |  |  |
| `replicantNodesStatus` _[ReplicantNodesStatus](#replicantnodesstatus)_ | レプリカントノードセットの概要ステータス。 |  |  |
| `clusterNodes` _[EMQXNode](#emqxnode) array_ | 役割が不明なノードを含む観測されたクラスター内ノード。 |  |  |
| `nodeEvacuations` _[NodeEvacuationStatus](#nodeevacuationstatus) array_ | クラスター内のアクティブなノード退避の状態。 |  |  |
| `dsReplication` _[DSReplicationStatus](#dsreplicationstatus)_ | EMQX Durable Storageのレプリケーション状態。 |  |  |
| `config` _[ConfigStatus](#configstatus)_ | 宣言的EMQX設定の調整状態。<br />フィールドは情報提供のための実装詳細です。設定のライフサイクル状態を判断するには`ConfigApplied`条件を推奨します。 |  |  |


#### EvacuationStrategy

_登場箇所:_
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _[EvacuationStrategyType](#evacuationstrategytype)_ | 退避ポリシーのタイプ。 | NodeEvacuation | Enum: [NodeEvacuation Disabled] <br /> |
| `connectionEvictionRate` _integer_ | クライアント切断率（秒あたりの数）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation)の`conn-evict-rate`と同じ。 | 1000 | 最小値: 1 <br /> |
| `sessionEvictionRate` _integer_ | セッション退避率（秒あたりの数）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation)の`sess-evict-rate`と同じ。 | 1000 | 最小値: 1 <br /> |
| `waitTakeover` _integer_ | セッション退避開始までの待機時間（秒）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation)の`wait-takeover`と同じ。 | 10 | 最小値: 0 <br /> |
| `waitHealthCheck` _integer_ | ノードがロードバランサーのアクティブバックエンドリストから除外されるのを待つ時間（秒）。<br />[EMQXノード退避](https://docs.emqx.com/en/emqx/v5.10/guides/cluster/rebalancing.html#node-evacuation)の`wait-health-check`と同じ。 | 60 | 最小値: 0 <br /> |


#### EvacuationStrategyType

_基底型:_ _string_

_登場箇所:_
- [EvacuationStrategy](#evacuationstrategy)

| フィールド | 説明 |
| --- | --- |
| `NodeEvacuation` |  |
| `Disabled` |  |


#### NodeEvacuationStatus

_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `nodeName` _string_ | 退避中のノード名 |  |  |
| `state` _string_ | 退避状態 |  |  |
| `sessionRecipients` _string array_ | セッション受け取り先 |  |  |
| `sessionEvictionRate` _integer_ | セッション退避率（秒あたりのセッション数） |  |  |
| `connectionEvictionRate` _integer_ | 接続退避率（秒あたりの接続数） |  |  |
| `initialSessions` _integer_ | このノードの初期セッション数 |  |  |
| `initialConnections` _integer_ | このノードへの初期接続数 |  |  |


#### ReplicantNodesStatus

ReplicantNodesStatusはレプリカントノードセットの概要ステータスです。  
マルチReplicaSetパターンではCRレベルでのリビジョントラッキングが必要です。

_登場箇所:_
- [EMQXStatus](#emqxstatus)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `readyReplicas` _integer_ | 準備完了レプリカ数。 |  |  |
| `currentRevision` _string_ | レプリカントセットの現在のリビジョン。 |  |  |
| `currentReplicas` _integer_ | 現在のリビジョンで稼働中のレプリカ数。 |  |  |
| `updateRevision` _string_ | レプリカントセットの更新リビジョン。<br />現在のリビジョンと異なる場合、セットは更新中。 |  |  |
| `updateReplicas` _integer_ | 更新リビジョンで稼働中のレプリカ数。 |  |  |
| `collisionCount` _integer_ |  |  |  |


#### ReplicantsUpdateStrategy

ReplicantsUpdateStrategyはレプリカントReplicaSetのローリングアップデートの進行速度を制御します。  
意味論は`apps/v1 Deployment.spec.strategy.rollingUpdate`に準じます。

_登場箇所:_
- [UpdateStrategy](#updatestrategy)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | MaxUnavailableはレプリカントReplicaSetのローリングアップデート中に一度にドレイン（退避、終了、削除予定）可能な古いPodの最大数。<br />整数は絶対数、文字列は望ましいレプリカ数の割合（例: "25%"）。<br />デフォルトは1（シリアルドレイン）。 |  | XIntOrString: \{\} <br /> |
| `maxSurge` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | MaxSurgeはテンプレートローリングアップデート中に新しいReplicaSetで許可される望ましいレプリカ数を超えた追加Pod数。整数は絶対数、文字列は望ましいレプリカ数の割合。<br />デフォルトは0。 |  | XIntOrString: \{\} <br /> |


#### ServiceTemplate

_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `enabled` _boolean_ | Serviceを作成するかどうかを指定します。 | true |  |
| `metadata` _[TemplateObjectMeta](#templateobjectmeta)_ | このテンプレートから生成されるオブジェクトに適用されるメタデータ。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#servicespec-v1-core)_ | Serviceの望ましい状態の仕様。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### TemplateObjectMeta

TemplateObjectMetaはテンプレートから作成されたオブジェクトに伝播されるメタデータを含みます。

_登場箇所:_
- [EMQXCoreTemplate](#emqxcoretemplate)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)
- [ServiceTemplate](#servicetemplate)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `labels` _object (keys:string, values:string)_ | 生成されたオブジェクトに適用されるラベル。 |  |  |
| `annotations` _object (keys:string, values:string)_ | 生成されたオブジェクトに適用されるアノテーション。 |  |  |


#### UpdateStrategy

_登場箇所:_
- [EMQXSpec](#emqxspec)

| フィールド | 説明 | デフォルト | バリデーション |
| --- | --- | --- | --- |
| `type` _string_ | クラスターアップグレードの実行方法を決定します。<br />* `RollingUpdate`: ローリングアップグレードを実行し、Podを段階的に更新します。コアPodは常に1つずつ更新され、レプリカントの更新は`replicants`戦略で制御されます。 | RollingUpdate | Enum: [RollingUpdate] <br /> |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 退避戦略の設定。 | \{ type:NodeEvacuation \} |  |
| `replicants` _[ReplicantsUpdateStrategy](#replicantsupdatestrategy)_ | レプリカントReplicaSetのローリングアップデートのパラメータ。 |  |  |
