# API 参考 (v2beta1)

## 包
- [apps.emqx.io/v2beta1](#appsemqxiov2beta1)


## apps.emqx.io/v2beta1

Package v2beta1 包含 apps v2beta1 API 组的 API Schema 定义

### 资源类型
- [EMQX](#emqx)
- [EMQXList](#emqxlist)
- [Rebalance](#rebalance)
- [RebalanceList](#rebalancelist)



#### BootstrapAPIKey



_出现于:_
- [EMQXSpec](#emqxspec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `key` _string_ |  |  | Pattern: `^[a-zA-Z\d-_]+$` <br /> |
| `secret` _string_ |  |  | MaxLength: 128 <br />MinLength: 3 <br /> |
| `secretRef` _[SecretRef](#secretref)_ |  |  |  |


#### Config



_出现于:_
- [EMQXSpec](#emqxspec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `mode` _string_ |  | Merge | Enum: [Merge Replace] <br /> |
| `data` _string_ | EMQX 配置，HOCON 格式，类似 etc/emqx.conf 文件 |  |  |


#### EMQX



EMQX 是 emqxes API 的 Schema



_出现于:_
- [EMQXList](#emqxlist)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | Spec 定义该集合中 EMQX 节点的期望标识。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | Status 是 EMQX 节点的当前状态，该数据可能存在一定延迟。 |  |  |


#### EMQXCoreTemplate



_出现于:_
- [EMQXSpec](#emqxspec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | EMQX Core 节点期望行为的规格说明。<br />更多信息：https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXCoreTemplateSpec



_出现于:_
- [EMQXCoreTemplate](#emqxcoretemplate)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelector 是 Pod 必须满足的节点选择器，Pod 只会被调度到标签匹配的节点上。<br />更多信息：https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeName 是将 Pod 调度到指定节点的请求。若非空，调度器会在满足资源需求的前提下直接将 Pod 调度到该节点。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Pod 分配的亲和性配置。<br />参考：https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 若指定，则为 Pod 的容忍设置。附加此 Toleration 的 Pod 可以容忍匹配 <key,value,effect> 三元组的任意污点。<br />TODO: 将来应使用 `tolerations`，此字段仅为兼容旧版本而保留，未来将删除。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 若指定，则为 Pod 的容忍设置。附加此 Toleration 的 Pod 可以容忍匹配 <key,value,effect> 三元组的任意污点。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | TopologySpreadConstraint 指定如何在给定拓扑中分布匹配的 Pod。 |  |  |
| `replicas` _integer_ | Replicas 是指定模板的期望副本数。这些副本是同一模板的实例化，但每个副本也具有一致的标识。默认值为 2。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | 若驱逐后 "selector" 选中的 Pod 中至少还有 "minAvailable" 个可用，则允许该驱逐。例如，指定 "100%" 可防止所有自愿驱逐。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | 若驱逐后 "selector" 选中的 Pod 中不可用数不超过 "maxUnavailable"，则允许该驱逐。例如，指定 0 可防止所有自愿驱逐。与 "minAvailable" 互斥。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | 入口点数组，不在 shell 中执行。若未提供则使用容器镜像的 ENTRYPOINT。支持变量引用 $(VAR_NAME)，无法解析的变量保持原样。$$ 转义为 $。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | 入口点的参数。若未提供则使用容器镜像的 CMD。支持变量引用 $(VAR_NAME)，无法解析的变量保持原样。$$ 转义为 $。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | 容器暴露的端口列表。此处声明端口主要用于提供信息，不声明不代表该端口无法访问。容器内监听默认 "0.0.0.0" 地址的端口均可从网络访问。更新后不可修改。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | 容器中设置的环境变量列表。更新后不可修改。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | 用于填充容器环境变量的来源列表。来源中定义的键名必须是 C_IDENTIFIER。无效的键在容器启动时以事件形式上报。多个来源存在相同键时，以最后一个来源的值为准。与 Env 中重复键的值以 Env 为准。更新后不可修改。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | 容器所需的计算资源。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContext 保存 Pod 级别的安全属性和通用容器设置。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContext 定义容器运行时的安全选项。若设置，将覆盖 PodSecurityContext 中的同等字段。<br />更多信息：https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | 属于 Pod 的初始化容器列表。初始化容器在普通容器启动前按顺序执行。若任一初始化容器失败，Pod 将被视为失败并按 restartPolicy 处理。初始化容器和普通容器的名称在所有容器中必须唯一。初始化容器不支持 Lifecycle 动作、Readiness 探针、Liveness 探针或 Startup 探针。调度时，初始化容器的资源需求按各资源类型取最大请求/限制值，再与普通容器的合计值取较大者。目前初始化容器不支持增删。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainers 表示要添加到 Pod 中的额外容器。<br />参见 https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 参见 https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 参见 https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | 容器存活性的定期探测。探测失败时容器将被重启。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | 容器服务就绪状态的定期探测。探测失败时容器将从服务端点列表中移除。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbe 表示 Pod 已成功初始化。若指定，在此探测成功之前不会执行其他探测。若探测失败，Pod 将像 livenessProbe 失败一样被重启。适用于 Pod 生命周期初始阶段需要较长时间加载数据或预热缓存的场景。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | 管理系统在容器生命周期事件发生时应采取的动作。更新后不可修改。 |  |  |
| `volumeClaimTemplates` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#persistentvolumeclaimspec-v1-core)_ | 该字段名为 VolumeClaimTemplates，但实际类型为 PersistentVolumeClaimSpec，命名不佳敬请谅解。PersistentVolumeClaimSpec 描述存储设备的通用属性，并允许指定特定存储提供商的属性。比 EMQXReplicantTemplateSpec 具有更多字段。 |  |  |


#### EMQXList



EMQXList 包含 EMQX 的列表




| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `EMQXList` | | |
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `items` _[EMQX](#emqx) array_ |  |  |  |


#### EMQXNode



_出现于:_
- [EMQXStatus](#emqxstatus)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `controllerUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `podUID` _[UID](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#uid-types-pkg)_ |  |  |  |
| `node` _string_ | EMQX 节点名称，例如：`emqx@127.0.0.1` |  |  |
| `node_status` _string_ | EMQX 节点状态，例如：Running |  |  |
| `otp_release` _string_ | EMQX 使用的 Erlang/OTP 版本，例如：24.2/12.2 |  |  |
| `version` _string_ | EMQX 版本 |  |  |
| `role` _string_ | EMQX 集群节点角色，枚举值："core" "replicant" |  |  |
| `edition` _string_ | EMQX 集群节点版本，枚举值："Opensource" "Enterprise" |  |  |
| `connections` _integer_ | EMQX `/api/v5/nodes` API 中的 `connections` 字段表示 MQTT 会话数。 |  |  |
| `live_connections` _integer_ | EMQX `/api/v5/nodes` API 中的 `live_connections` 字段表示当前连接的 MQTT 客户端数。`live_connections` 仅在 EMQX 5.1 及以上版本中生效。 |  |  |


#### EMQXNodesStatus



_出现于:_
- [EMQXStatus](#emqxstatus)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `replicas` _integer_ |  |  |  |
| `readyReplicas` _integer_ |  |  |  |
| `currentRevision` _string_ |  |  |  |
| `currentReplicas` _integer_ |  |  |  |
| `updateRevision` _string_ |  |  |  |
| `updateReplicas` _integer_ |  |  |  |
| `collisionCount` _integer_ |  |  |  |


#### EMQXReplicantTemplate



_出现于:_
- [EMQXSpec](#emqxspec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | EMQX Replicant 节点期望行为的规格说明。<br />更多信息：https://github.com/kubernetes/community/blob/master/contributors/devel/sig-architecture/api-conventions.md#spec-and-status<br />Controller tools 不支持更复杂的校验（oneOf/anyOf/allOf 等），请改用校验规则。https://github.com/kubernetes-sigs/controller-tools/issues/461#issuecomment-1982741599 |  |  |


#### EMQXReplicantTemplateSpec



_出现于:_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | NodeSelector 是 Pod 必须满足的节点选择器，Pod 只会被调度到标签匹配的节点上。<br />更多信息：https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/ |  |  |
| `nodeName` _string_ | NodeName 是将 Pod 调度到指定节点的请求。若非空，调度器会在满足资源需求的前提下直接将 Pod 调度到该节点。 |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#affinity-v1-core)_ | Pod 分配的亲和性配置。<br />参考：https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `toleRations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 若指定，则为 Pod 的容忍设置。附加此 Toleration 的 Pod 可以容忍匹配 <key,value,effect> 三元组的任意污点。<br />TODO: 将来应使用 `tolerations`，此字段仅为兼容旧版本而保留，未来将删除。 |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#toleration-v1-core) array_ | 若指定，则为 Pod 的容忍设置。附加此 Toleration 的 Pod 可以容忍匹配 <key,value,effect> 三元组的任意污点。 |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#topologyspreadconstraint-v1-core) array_ | TopologySpreadConstraint 指定如何在给定拓扑中分布匹配的 Pod。 |  |  |
| `replicas` _integer_ | Replicas 是指定模板的期望副本数。这些副本是同一模板的实例化，但每个副本也具有一致的标识。默认值为 2。 | 2 |  |
| `minAvailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | 若驱逐后 "selector" 选中的 Pod 中至少还有 "minAvailable" 个可用，则允许该驱逐。例如，指定 "100%" 可防止所有自愿驱逐。 |  | XIntOrString: \{\} <br /> |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#intorstring-intstr-util)_ | 若驱逐后 "selector" 选中的 Pod 中不可用数不超过 "maxUnavailable"，则允许该驱逐。例如，指定 0 可防止所有自愿驱逐。与 "minAvailable" 互斥。 |  | XIntOrString: \{\} <br /> |
| `command` _string array_ | 入口点数组，不在 shell 中执行。若未提供则使用容器镜像的 ENTRYPOINT。支持变量引用 $(VAR_NAME)，无法解析的变量保持原样。$$ 转义为 $。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `args` _string array_ | 入口点的参数。若未提供则使用容器镜像的 CMD。支持变量引用 $(VAR_NAME)，无法解析的变量保持原样。$$ 转义为 $。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#containerport-v1-core) array_ | 容器暴露的端口列表。此处声明端口主要用于提供信息，不声明不代表该端口无法访问。容器内监听默认 "0.0.0.0" 地址的端口均可从网络访问。更新后不可修改。 |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envvar-v1-core) array_ | 容器中设置的环境变量列表。更新后不可修改。 |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#envfromsource-v1-core) array_ | 用于填充容器环境变量的来源列表。来源中定义的键名必须是 C_IDENTIFIER。无效的键在容器启动时以事件形式上报。多个来源存在相同键时，以最后一个来源的值为准。与 Env 中重复键的值以 Env 为准。更新后不可修改。 |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#resourcerequirements-v1-core)_ | 容器所需的计算资源。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#podsecuritycontext-v1-core)_ | SecurityContext 保存 Pod 级别的安全属性和通用容器设置。 | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsUser:1000 supplementalGroups:[1000] \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#securitycontext-v1-core)_ | SecurityContext 定义容器运行时的安全选项。若设置，将覆盖 PodSecurityContext 中的同等字段。<br />更多信息：https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ | \{ runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | 属于 Pod 的初始化容器列表。初始化容器在普通容器启动前按顺序执行。若任一初始化容器失败，Pod 将被视为失败并按 restartPolicy 处理。初始化容器和普通容器的名称在所有容器中必须唯一。初始化容器不支持 Lifecycle 动作、Readiness 探针、Liveness 探针或 Startup 探针。调度时，初始化容器的资源需求按各资源类型取最大请求/限制值，再与普通容器的合计值取较大者。目前初始化容器不支持增删。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#container-v1-core) array_ | ExtraContainers 表示要添加到 Pod 中的额外容器。<br />参见 https://github.com/emqx/emqx-operator/issues/252 |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volume-v1-core) array_ | 参见 https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#volumemount-v1-core) array_ | 参见 https://github.com/emqx/emqx-operator/pull/72 |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | 容器存活性的定期探测。探测失败时容器将被重启。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | 容器服务就绪状态的定期探测。探测失败时容器将从服务端点列表中移除。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:12 httpGet:map[path:/status port:dashboard] initialDelaySeconds:10 periodSeconds:5 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#probe-v1-core)_ | StartupProbe 表示 Pod 已成功初始化。若指定，在此探测成功之前不会执行其他探测。若探测失败，Pod 将像 livenessProbe 失败一样被重启。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#lifecycle-v1-core)_ | 管理系统在容器生命周期事件发生时应采取的动作。更新后不可修改。 |  |  |


#### EMQXSpec



EMQXSpec 定义 EMQX 的期望状态



_出现于:_
- [EMQX](#emqx)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `image` _string_ | EMQX 镜像名称。<br />更多信息：https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#pullpolicy-v1-core)_ | 镜像拉取策略，可选 Always、Never、IfNotPresent。指定 :latest 标签时默认为 Always，否则默认为 IfNotPresent。更新后不可修改。<br />更多信息：https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#localobjectreference-v1-core) array_ | ImagePullSecrets 是同命名空间下用于拉取镜像的 Secret 引用列表（可选）。若指定，这些 Secret 将传递给各拉取器实现。<br />更多信息：https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | Service Account 名称，用于将 ReplicaSet 或 StatefulSet 与指定的 Service Account 关联以进行身份验证。<br />更多信息：https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `bootstrapAPIKeys` _[BootstrapAPIKey](#bootstrapapikey) array_ | EMQX bootstrap 用户，更新后不可修改。 |  |  |
| `config` _[Config](#config)_ | EMQX 配置 |  |  |
| `clusterDomain` _string_ |  | cluster.local |  |
| `revisionHistoryLimit` _integer_ | 为支持回滚而保留的旧 ReplicaSet、旧 StatefulSet 和旧 PersistentVolumeClaim 的数量。使用指针以区分显式零值和未指定。默认值为 3。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | UpdateStrategy 描述 EMQX 蓝绿升级策略的对象 | \{ evacuationStrategy:map[connEvictRate:1000 sessEvictRate:1000 waitTakeover:10] initialDelaySeconds:10 type:Recreate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | CoreTemplate 描述将要创建的 EMQX Core 节点 | \{ spec:map[replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | ReplicantTemplate 描述将要创建的 EMQX Replicant 节点 |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | DashboardServiceTemplate 描述将要创建的 EMQX Dashboard 服务，该服务始终选择 EMQX Core 节点 |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | ListenersServiceTemplate 描述将要创建的 EMQX 监听器服务。若 EMQX Replicant 节点存在，该服务将选择 Replicant 节点；否则选择 Core 节点 |  |  |


#### EMQXStatus



EMQXStatus 定义 EMQX 的观测状态



_出现于:_
- [EMQX](#emqx)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#condition-v1-meta) array_ | 表示 EMQX 自定义资源当前状态的最新可用观测信息。 |  |  |
| `coreNodes` _[EMQXNode](#emqxnode) array_ |  |  |  |
| `coreNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ |  |  |  |
| `replicantNodes` _[EMQXNode](#emqxnode) array_ |  |  |  |
| `replicantNodesStatus` _[EMQXNodesStatus](#emqxnodesstatus)_ |  |  |  |
| `nodeEvacuationsStatus` _[NodeEvacuationStatus](#nodeevacuationstatus) array_ |  |  |  |


#### EvacuationStrategy



_出现于:_
- [UpdateStrategy](#updatestrategy)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `waitTakeover` _integer_ |  |  | Minimum: 0 <br /> |
| `connEvictRate` _integer_ | 仅在 EMQX Enterprise 中生效。 | 1000 | Minimum: 1 <br /> |
| `sessEvictRate` _integer_ | 仅在 EMQX Enterprise 中生效。 | 1000 | Minimum: 1 <br /> |


#### KeyRef



_出现于:_
- [SecretRef](#secretref)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `secretName` _string_ |  |  |  |
| `secretKey` _string_ |  |  | Pattern: `^[a-zA-Z\d-_]+$` <br /> |


#### NodeEvacuationStats



_出现于:_
- [NodeEvacuationStatus](#nodeevacuationstatus)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `initial_sessions` _integer_ |  |  |  |
| `initial_connected` _integer_ |  |  |  |
| `current_sessions` _integer_ |  |  |  |
| `current_connected` _integer_ |  |  |  |


#### NodeEvacuationStatus



_出现于:_
- [EMQXStatus](#emqxstatus)

| 字段 | 描述 | 默认值 | 校验规则 |
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



Rebalance 是 rebalances API 的 Schema



_出现于:_
- [RebalanceList](#rebalancelist)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `Rebalance` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `spec` _[RebalanceSpec](#rebalancespec)_ |  |  |  |
| `status` _[RebalanceStatus](#rebalancestatus)_ |  |  |  |


#### RebalanceCondition



RebalanceCondition 描述 EMQX 重平衡任务的当前状态。



_出现于:_
- [RebalanceStatus](#rebalancestatus)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `type` _[RebalanceConditionType](#rebalanceconditiontype)_ | 重平衡条件类型的状态，可选 Processing、Complete、Failed。 |  |  |
| `status` _[ConditionStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#conditionstatus-v1-core)_ | 条件状态，可选 True、False、Unknown。 |  |  |
| `lastUpdateTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 该条件最后一次更新的时间。 |  |  |
| `lastTransitionTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 条件最后一次从一个状态转换到另一个状态的时间。 |  |  |
| `reason` _string_ | 条件最后一次转换的原因。 |  |  |
| `message` _string_ | 描述转换详情的人类可读消息。 |  |  |


#### RebalanceConditionType

_底层类型:_ _string_



_出现于:_
- [RebalanceCondition](#rebalancecondition)

| 字段 | 描述 |
| --- | --- |
| `Processing` |  |
| `Completed` |  |
| `Failed` |  |


#### RebalanceList



RebalanceList 包含 Rebalance 的列表




| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v2beta1` | | |
| `kind` _string_ | `RebalanceList` | | |
| `metadata` _[ListMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#listmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `items` _[Rebalance](#rebalance) array_ |  |  |  |


#### RebalancePhase

_底层类型:_ _string_



_出现于:_
- [RebalanceStatus](#rebalancestatus)

| 字段 | 描述 |
| --- | --- |
| `Processing` |  |
| `Completed` |  |
| `Failed` |  |


#### RebalanceSpec



RebalanceSpec 定义 Rebalance 的期望状态



_出现于:_
- [Rebalance](#rebalance)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `instanceKind` _string_ | InstanceKind 用于区分 EMQX 和 EMQXEnterprise。设为 "EMQX" 表示 EMQX CR 为 v2beta1；设为 "EmqxEnterprise" 表示 EmqxEnterprise CR 为 v1beta4。 | EMQX |  |
| `instanceName` _string_ | InstanceName 表示 EMQX CR 的名称，仅对 EMQX Enterprise 生效。 |  | Required: \{\} <br /> |
| `rebalanceStrategy` _[RebalanceStrategy](#rebalancestrategy)_ | RebalanceStrategy 表示 EMQX 重平衡的策略。<br />更多信息：https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing |  | Required: \{\} <br /> |


#### RebalanceState



RebalanceState 定义 EMQX 观测到的重平衡状态



_出现于:_
- [RebalanceStatus](#rebalancestatus)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `state` _string_ | 表示 EMQX 集群重平衡的状态。 |  |  |
| `session_eviction_rate` _integer_ | 表示节点会话迁移速率（每秒）。 |  |  |
| `recipients` _string array_ | 表示重平衡的目标节点。 |  |  |
| `node` _string_ | 表示重平衡的调度节点。 |  |  |
| `donors` _string array_ | 表示重平衡的源节点。 |  |  |
| `coordinator_node` _string_ | 表示当前正在执行重平衡的节点。 |  |  |
| `connection_eviction_rate` _integer_ | 表示节点连接迁移速率（每秒）。 |  |  |


#### RebalanceStatus



RebalanceStatus 表示 Rebalance 的当前状态



_出现于:_
- [Rebalance](#rebalance)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `conditions` _[RebalanceCondition](#rebalancecondition) array_ | 对象当前状态的最新可用观测信息。Rebalance 失败时条件 type 为 "Failed"、status 为 false；处理中时 type 为 "Processing"、status 为 true；完成时 type 为 "Complete"、status 为 true。 |  |  |
| `phase` _[RebalancePhase](#rebalancephase)_ | 表示 Rebalance 的阶段。 |  |  |
| `rebalanceStates` _[RebalanceState](#rebalancestate) array_ |  |  |  |
| `startedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 表示重平衡任务开始的时间。 |  |  |
| `completedTime` _[Time](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#time-v1-meta)_ | 表示重平衡任务完成的时间。 |  |  |


#### RebalanceStrategy



RebalanceStrategy 表示 EMQX 重平衡的策略



_出现于:_
- [RebalanceSpec](#rebalancespec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `connEvictRate` _integer_ | ConnEvictRate 表示源节点客户端断开速率（每秒），对应 [EMQX Rebalancing](https://docs.emqx.com/en/enterprise/v4.4/advanced/rebalancing.html#rebalancing) 中的 conn-evict-rate，值必须大于 0。 |  | Minimum: 1 <br />Required: \{\} <br /> |
| `sessEvictRate` _integer_ | SessEvictRate 表示源节点会话迁移速率（每秒），对应 conn-evict-rate，值必须大于 0，默认值为 500。 | 500 |  |
| `waitTakeover` _integer_ | WaitTakeover 表示所有连接断开后等待客户端重连并接管会话的时间（秒），对应 wait-takeover，值必须大于 0，默认值为 60 秒。 | 60 |  |
| `waitHealthCheck` _integer_ | WaitHealthCheck 表示等待负载均衡器将源节点从活跃后端列表中移除的时间（秒），超过等待时间后重平衡任务启动，对应 wait-health-check，值必须大于 0，默认值为 60 秒。 | 60 |  |
| `absConnThreshold` _integer_ | AbsConnThreshold 表示检查连接均衡的绝对阈值，对应 abs-conn-threshold，值必须大于 0，默认值为 1000。 | 1000 |  |
| `relConnThreshold` _string_ | RelConnThreshold 表示检查连接均衡的相对阈值，对应 rel-conn-threshold。由于浮点类型在各语言中支持程度不一，该字段定义为字符串类型，值必须大于 "1.0"，默认值为 "1.1"。 | 1.1 |  |
| `absSessThreshold` _integer_ | AbsSessThreshold 表示检查会话连接均衡的绝对阈值，对应 abs-sess-threshold，值必须大于 0，默认值为 1000。 | 1000 |  |
| `relSessThreshold` _string_ | RelSessThreshold 表示检查会话连接均衡的相对阈值，对应 rel-sess-threshold。由于浮点类型在各语言中支持程度不一，该字段定义为字符串类型，值必须大于 "1.0"，默认值为 "1.1"。 | 1.1 |  |


#### SecretRef



_出现于:_
- [BootstrapAPIKey](#bootstrapapikey)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `key` _[KeyRef](#keyref)_ |  |  |  |
| `secret` _[KeyRef](#keyref)_ |  |  |  |


#### ServiceTemplate



_出现于:_
- [EMQXSpec](#emqxspec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `enabled` _boolean_ | EMQX Operator 将为 EMQX 节点创建服务。使用指针以区分 `false` 和未指定。 | true |  |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#objectmeta-v1-meta)_ | 请参阅 Kubernetes API 文档了解 `metadata` 的字段说明。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.23/#servicespec-v1-core)_ | Spec 定义服务的行为。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### UpdateStrategy



_出现于:_
- [EMQXSpec](#emqxspec)

| 字段 | 描述 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `type` _string_ |  | Recreate | Enum: [Recreate] <br /> |
| `initialDelaySeconds` _integer_ | 启动连接迁移前的等待秒数。 |  |  |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 连接迁移超时前的等待秒数。 |  |  |
