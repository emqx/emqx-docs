# API 参考（v3beta1）

## 包
- [apps.emqx.io/v3beta1](#appsemqxiov3beta1)


## apps.emqx.io/v3beta1

v3beta1 包包含 apps.emqx.io v3beta1 API 组的 API Schema 定义。

### 资源类型
- [EMQX](#emqx)



#### Config







_出现于：_
- [EMQXSpec](#emqxspec)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `roots` _[ConfigRoots](#configroots)_ | EMQX 的顶层配置根项。值必须与 JSON 兼容。Operator 会将可在运行时应用的根项序列化到 `base.hocon`，<br />并将 EMQX 启动时生效的设置序列化到 `emqx.conf`。<br />不支持 include、替换和重复声明等仅适用于 HOCON 的语法。<br />删除根项表示 Operator 不再管理该配置，但不会删除 EMQX 已持久化的值。<br />`node.cookie` 路径由 Operator 保留，不得在此处指定。 |  |  |


#### ConfigRoots

_底层类型：_ _[JSON](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#json-v1-apiextensions-k8s-io)_





_出现于：_
- [Config](#config)



#### ConfigStatus



ConfigStatus 包含由控制器管理的协调检查点。
这些字段仅用于提供实现信息，将来可能会发生变化。



_出现于：_
- [EMQXStatus](#emqxstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `desiredRevision` _string_ | `spec.config.roots` 中完整预期配置的修订版本。 |  |  |
| `runtimeRevision` _string_ | EMQX API 最近接受的运行时配置修订版本。<br />首次启动前，该值表示为集群引导暂存的修订版本。 |  |  |
| `desiredStartupRevision` _string_ | EMQX 启动时生效的预期设置修订版本。 |  |  |
| `activeStartupRevisions` _string array_ | Ready Pod 所使用的启动配置修订版本。<br />存在多个修订版本表示 Ready Pod 使用了不同版本的启动设置。 |  |  |


#### CoreNodesStatus



CoreNodesStatus 是单个 StatefulSet 所管理的 Core 节点的汇总状态。



_出现于：_
- [EMQXStatus](#emqxstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `readyReplicas` _integer_ | Ready 副本数。 |  |  |
| `updatedReplicas` _integer_ | 已更新为预期 Pod 模板的副本数。 |  |  |
| `currentReplicas` _integer_ | 仍在运行旧 Pod 模板的副本数。 |  |  |


#### DSDBReplicationStatus







_出现于：_
- [DSReplicationStatus](#dsreplicationstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `name` _string_ | 数据库名称 |  |  |
| `numShards` _integer_ | 数据库的分片数 |  |  |
| `numShardReplicas` _integer_ | 分片副本总数 |  |  |
| `lostShardReplicas` _integer_ | 属于已丢失站点的分片副本总数 |  |  |
| `numTransitions` _integer_ | 当前正在转移所有权的分片数 |  |  |
| `minReplicas` _integer_ | 数据库各分片的最小复制因子 |  |  |
| `maxReplicas` _integer_ | 数据库各分片的最大复制因子 |  |  |


#### DSReplicationStatus



各数据库的 Durable Storage 复制状态摘要。



_出现于：_
- [EMQXStatus](#emqxstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `dbs` _[DSDBReplicationStatus](#dsdbreplicationstatus) array_ |  |  |  |


#### EMQX



表示 EMQX 集群的自定义资源。





| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `apiVersion` _string_ | `apps.emqx.io/v3beta1` | | |
| `kind` _string_ | `EMQX` | | |
| `metadata` _[ObjectMeta](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#objectmeta-v1-meta)_ | 有关 `metadata` 的字段，请参阅 Kubernetes API 文档。 |  |  |
| `spec` _[EMQXSpec](#emqxspec)_ | EMQX 集群的预期状态规格。 |  |  |
| `status` _[EMQXStatus](#emqxstatus)_ | EMQX 集群的当前状态。 |  |  |


#### EMQXCoreTemplate







_出现于：_
- [EMQXSpec](#emqxspec)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `metadata` _[TemplateObjectMeta](#templateobjectmeta)_ | 应用于此模板所生成对象的元数据。 |  |  |
| `spec` _[EMQXCoreTemplateSpec](#emqxcoretemplatespec)_ | Core 节点的预期状态规格。<br />更多信息：https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status | \{  \} |  |


#### EMQXCoreTemplateSpec







_出现于：_
- [EMQXCoreTemplate](#emqxcoretemplate)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Selector which must be true for the pod to fit on a node.<br />Must match a node's labels for the pod to be scheduled on that node.<br />More info: https://kubernetes.io/docs/concepts/config/assign-pod-node/ |  |  |
| `nodeName` _string_ | Request to schedule this pod onto a specific node.<br />If it is non-empty, the scheduler simply schedules this pod onto that node, assuming that it fits resource requirements. |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Affinity for pod assignment<br />ref: https://kubernetes.io/docs/concepts/config/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) array_ | Pod tolerations.<br />If specified, Pod tolerates any taint that matches the triple <key,value,effect> using the matching operator. |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) array_ | Specifies how to spread matching pods among the given topology. |  |  |
| `dnsConfig` _[PodDNSConfig](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#poddnsconfig-v1-core)_ | Specifies the DNS parameters of a pod.<br />Parameters specified here will be merged to the generated DNS<br />configuration based on DNSPolicy (always ClusterFirst).<br />More info: https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-dns-config |  |  |
| `replicas` _integer_ | 预期实例数。<br />对于 Core 节点，每个实例都具有稳定的标识。 | 1 | Minimum: 0 <br /> |
| `minReadySeconds` _integer_ | MinReadySeconds is the minimum time (seconds) a pod must be Ready before it counts as available.<br />For core nodes this is applied to the StatefulSet (mirrors apps/v1 StatefulSetSpec.minReadySeconds);<br />for replicants, to the ReplicaSet (mirrors apps/v1 ReplicaSetSpec.minReadySeconds).<br />Omitted or zero matches the apps/v1 default (0). |  | Minimum: 0 <br /> |
| `command` _string array_ | Entrypoint array. Not executed within a shell.<br />The container image's ENTRYPOINT is used if this is not provided.<br />Variable references `$(VAR_NAME)` are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double `$$` are reduced<br />to a single `$`, which allows for escaping the `$(VAR_NAME)` syntax: i.e. `$$(VAR_NAME)` will<br />produce the string literal `$(VAR_NAME)`. Escaped references will never be expanded, regardless<br />of whether the variable exists or not. Cannot be updated.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  | Optional: \{\} <br /> |
| `args` _string array_ | Arguments to the entrypoint.<br />The container image's CMD is used if this is not provided.<br />Variable references `$(VAR_NAME)` are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double `$$` are reduced<br />to a single `$`, which allows for escaping the `$(VAR_NAME)` syntax: i.e. `$$(VAR_NAME)` will<br />produce the string literal `$(VAR_NAME)`. Escaped references will never be expanded, regardless<br />of whether the variable exists or not.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) array_ | List of ports to expose from the container.<br />Exposing a port here gives the system additional information about the network connections a<br />container uses, but is primarily informational. Not specifying a port here DOES NOT prevent that<br />port from being exposed. Any port which is listening on the default `0.0.0.0` address inside a<br />container will be accessible from the network.<br />Port names `dashboard` and `dashboard-https` are reserved by the Operator and cannot be supplied<br />in the template. The Operator derives these named ports from<br />`spec.config.roots.dashboard.listeners` for probes, Services, and per-Pod API requests.<br />Change their container port by changing the corresponding listener bind instead. |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) array_ | List of environment variables to set in the container. |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) array_ | List of sources to populate environment variables from in the container.<br />The keys defined within a source must be a C_IDENTIFIER. All invalid keys<br />will be reported as an event when the container is starting. When a key exists in multiple<br />sources, the value associated with the last source will take precedence.<br />Values defined by an Env with a duplicate key will take precedence. |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcerequirements-v1-core)_ | Compute Resources required by this container.<br />More info: https://kubernetes.io/docs/concepts/config/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Pod-level security attributes and common container settings. | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | Security options the container should be run with.<br />If set, the fields of SecurityContext override the equivalent fields of PodSecurityContext.<br />More info: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ |  |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) array_ | List of initialization containers belonging to the pod.<br />Init containers are executed in order prior to containers being started. If any<br />init container fails, the pod is considered to have failed and is handled according<br />to its restartPolicy. The name for an init container or normal container must be<br />unique among all containers.<br />Init containers may not have Lifecycle actions, Readiness probes, Liveness probes, or Startup probes.<br />The resourceRequirements of an init container are taken into account during scheduling<br />by finding the highest request/limit for each resource type, and then using the max of<br />of that value or the sum of the normal containers. Limits are applied to init containers<br />in a similar fashion.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) array_ | Additional containers to run alongside the main container. |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) array_ | Additional volumes to provide to a Pod. |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) array_ | Specifies how additional volumes are mounted into the main container. |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Periodic probe of container liveness.<br />Container will be restarted if the probe fails.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Periodic probe of container service readiness.<br />Container will be removed from service endpoints if the probe fails.<br />Strongly advised to keep the current default: it takes into account ongoing node evacuations managed<br />by the Operator as part of scaling operations and rolling updates.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:1 httpGet:map[path:/api/v5/load_rebalance/availability_check port:dashboard] initialDelaySeconds:10 periodSeconds:5 timeoutSeconds:3 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | StartupProbe indicates that the Pod has successfully initialized.<br />If specified, no other probes are executed until this completes successfully.<br />If this probe fails, the Pod will be restarted, just as if the `livenessProbe` failed.<br />This can be used to provide different probe parameters at the beginning of a Pod's lifecycle,<br />when it might take a long time to load data or warm a cache, than during steady-state operation.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | Actions that the management system should take in response to container lifecycle events. |  |  |
| `persistentVolumeClaimSpec` _[PersistentVolumeClaimSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#persistentvolumeclaimspec-v1-core)_ | Core 节点数据存储的 PVC 规格。 |  |  |


#### EMQXNode







_出现于：_
- [EMQXStatus](#emqxstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `name` _string_ | 节点名称 |  |  |
| `podName` _string_ | 对应的 Pod 名称 |  |  |
| `status` _string_ | 节点状态 |  |  |
| `otpRelease` _string_ | 节点所运行的 Erlang/OTP 版本 |  |  |
| `version` _string_ | EMQX 版本 |  |  |
| `role` _string_ | 节点角色，取值为 `core` 或 `replicant` |  |  |
| `sessions` _integer_ | MQTT 会话数 |  |  |
| `connections` _integer_ | 已连接的 MQTT 客户端数 |  |  |


#### EMQXReplicantTemplate







_出现于：_
- [EMQXSpec](#emqxspec)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `metadata` _[TemplateObjectMeta](#templateobjectmeta)_ | 应用于此模板所生成对象的元数据。 |  |  |
| `spec` _[EMQXReplicantTemplateSpec](#emqxreplicanttemplatespec)_ | Replicant 节点的预期状态规格。<br />更多信息：https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### EMQXReplicantTemplateSpec







_出现于：_
- [EMQXCoreTemplateSpec](#emqxcoretemplatespec)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `nodeSelector` _object (keys:string, values:string)_ | Selector which must be true for the pod to fit on a node.<br />Must match a node's labels for the pod to be scheduled on that node.<br />More info: https://kubernetes.io/docs/concepts/config/assign-pod-node/ |  |  |
| `nodeName` _string_ | Request to schedule this pod onto a specific node.<br />If it is non-empty, the scheduler simply schedules this pod onto that node, assuming that it fits resource requirements. |  |  |
| `affinity` _[Affinity](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#affinity-v1-core)_ | Affinity for pod assignment<br />ref: https://kubernetes.io/docs/concepts/config/assign-pod-node/#affinity-and-anti-affinity |  |  |
| `tolerations` _[Toleration](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#toleration-v1-core) array_ | Pod tolerations.<br />If specified, Pod tolerates any taint that matches the triple <key,value,effect> using the matching operator. |  |  |
| `topologySpreadConstraints` _[TopologySpreadConstraint](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#topologyspreadconstraint-v1-core) array_ | Specifies how to spread matching pods among the given topology. |  |  |
| `dnsConfig` _[PodDNSConfig](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#poddnsconfig-v1-core)_ | Specifies the DNS parameters of a pod.<br />Parameters specified here will be merged to the generated DNS<br />configuration based on DNSPolicy (always ClusterFirst).<br />More info: https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-dns-config |  |  |
| `replicas` _integer_ | 预期实例数。<br />对于 Core 节点，每个实例都具有稳定的标识。 | 1 | Minimum: 0 <br /> |
| `minReadySeconds` _integer_ | MinReadySeconds is the minimum time (seconds) a pod must be Ready before it counts as available.<br />For core nodes this is applied to the StatefulSet (mirrors apps/v1 StatefulSetSpec.minReadySeconds);<br />for replicants, to the ReplicaSet (mirrors apps/v1 ReplicaSetSpec.minReadySeconds).<br />Omitted or zero matches the apps/v1 default (0). |  | Minimum: 0 <br /> |
| `command` _string array_ | Entrypoint array. Not executed within a shell.<br />The container image's ENTRYPOINT is used if this is not provided.<br />Variable references `$(VAR_NAME)` are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double `$$` are reduced<br />to a single `$`, which allows for escaping the `$(VAR_NAME)` syntax: i.e. `$$(VAR_NAME)` will<br />produce the string literal `$(VAR_NAME)`. Escaped references will never be expanded, regardless<br />of whether the variable exists or not. Cannot be updated.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  | Optional: \{\} <br /> |
| `args` _string array_ | Arguments to the entrypoint.<br />The container image's CMD is used if this is not provided.<br />Variable references `$(VAR_NAME)` are expanded using the container's environment. If a variable<br />cannot be resolved, the reference in the input string will be unchanged. Double `$$` are reduced<br />to a single `$`, which allows for escaping the `$(VAR_NAME)` syntax: i.e. `$$(VAR_NAME)` will<br />produce the string literal `$(VAR_NAME)`. Escaped references will never be expanded, regardless<br />of whether the variable exists or not.<br />More info: https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/#running-a-command-in-a-shell |  |  |
| `ports` _[ContainerPort](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#containerport-v1-core) array_ | List of ports to expose from the container.<br />Exposing a port here gives the system additional information about the network connections a<br />container uses, but is primarily informational. Not specifying a port here DOES NOT prevent that<br />port from being exposed. Any port which is listening on the default `0.0.0.0` address inside a<br />container will be accessible from the network.<br />Port names `dashboard` and `dashboard-https` are reserved by the Operator and cannot be supplied<br />in the template. The Operator derives these named ports from<br />`spec.config.roots.dashboard.listeners` for probes, Services, and per-Pod API requests.<br />Change their container port by changing the corresponding listener bind instead. |  |  |
| `env` _[EnvVar](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envvar-v1-core) array_ | List of environment variables to set in the container. |  |  |
| `envFrom` _[EnvFromSource](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#envfromsource-v1-core) array_ | List of sources to populate environment variables from in the container.<br />The keys defined within a source must be a C_IDENTIFIER. All invalid keys<br />will be reported as an event when the container is starting. When a key exists in multiple<br />sources, the value associated with the last source will take precedence.<br />Values defined by an Env with a duplicate key will take precedence. |  |  |
| `resources` _[ResourceRequirements](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcerequirements-v1-core)_ | Compute Resources required by this container.<br />More info: https://kubernetes.io/docs/concepts/config/manage-resources-containers/ |  |  |
| `podSecurityContext` _[PodSecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#podsecuritycontext-v1-core)_ | Pod-level security attributes and common container settings. | \{ fsGroup:1000 fsGroupChangePolicy:Always runAsGroup:1000 runAsNonRoot:true runAsUser:1000 \} |  |
| `containerSecurityContext` _[SecurityContext](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#securitycontext-v1-core)_ | Security options the container should be run with.<br />If set, the fields of SecurityContext override the equivalent fields of PodSecurityContext.<br />More info: https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ |  |  |
| `initContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) array_ | List of initialization containers belonging to the pod.<br />Init containers are executed in order prior to containers being started. If any<br />init container fails, the pod is considered to have failed and is handled according<br />to its restartPolicy. The name for an init container or normal container must be<br />unique among all containers.<br />Init containers may not have Lifecycle actions, Readiness probes, Liveness probes, or Startup probes.<br />The resourceRequirements of an init container are taken into account during scheduling<br />by finding the highest request/limit for each resource type, and then using the max of<br />of that value or the sum of the normal containers. Limits are applied to init containers<br />in a similar fashion.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/init-containers/ |  |  |
| `extraContainers` _[Container](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#container-v1-core) array_ | Additional containers to run alongside the main container. |  |  |
| `extraVolumes` _[Volume](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volume-v1-core) array_ | Additional volumes to provide to a Pod. |  |  |
| `extraVolumeMounts` _[VolumeMount](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#volumemount-v1-core) array_ | Specifies how additional volumes are mounted into the main container. |  |  |
| `livenessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Periodic probe of container liveness.<br />Container will be restarted if the probe fails.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:3 httpGet:map[path:/status port:dashboard] initialDelaySeconds:60 periodSeconds:30 \} |  |
| `readinessProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | Periodic probe of container service readiness.<br />Container will be removed from service endpoints if the probe fails.<br />Strongly advised to keep the current default: it takes into account ongoing node evacuations managed<br />by the Operator as part of scaling operations and rolling updates.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes | \{ failureThreshold:1 httpGet:map[path:/api/v5/load_rebalance/availability_check port:dashboard] initialDelaySeconds:10 periodSeconds:5 timeoutSeconds:3 \} |  |
| `startupProbe` _[Probe](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#probe-v1-core)_ | StartupProbe indicates that the Pod has successfully initialized.<br />If specified, no other probes are executed until this completes successfully.<br />If this probe fails, the Pod will be restarted, just as if the `livenessProbe` failed.<br />This can be used to provide different probe parameters at the beginning of a Pod's lifecycle,<br />when it might take a long time to load data or warm a cache, than during steady-state operation.<br />More info: https://kubernetes.io/docs/concepts/workloads/pods/pod-lifecycle#container-probes |  |  |
| `lifecycle` _[Lifecycle](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#lifecycle-v1-core)_ | Actions that the management system should take in response to container lifecycle events. |  |  |


#### EMQXSpec



EMQXSpec 定义 EMQX 的预期状态。



_出现于：_
- [EMQX](#emqx)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `image` _string_ | EMQX 容器镜像。<br />更多信息：https://kubernetes.io/docs/concepts/containers/images |  |  |
| `imagePullPolicy` _[PullPolicy](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#pullpolicy-v1-core)_ | 容器镜像拉取策略。<br />可取值为 `Always`、`Never` 或 `IfNotPresent`。<br />指定 `:latest` 标签时默认为 `Always`，否则默认为 `IfNotPresent`。<br />更多信息：https://kubernetes.io/docs/concepts/containers/images#updating-images |  |  |
| `imagePullSecrets` _[LocalObjectReference](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#localobjectreference-v1-core) array_ | ImagePullSecrets is an optional list of references to secrets in the same namespace to use for pulling any of the images used by this PodSpec.<br />If specified, these secrets will be passed to individual puller implementations for them to use.<br />More info: https://kubernetes.io/docs/concepts/containers/images#specifying-imagepullsecrets-on-a-pod |  |  |
| `serviceAccountName` _string_ | ServiceAccount name.<br />Managed ReplicaSets and StatefulSets are associated with the specified ServiceAccount for authentication purposes.<br />More info: https://kubernetes.io/docs/concepts/security/service-accounts |  |  |
| `config` _[Config](#config)_ | EMQX 配置。 |  |  |
| `clusterDomain` _string_ | Kubernetes 集群域名。 | cluster.local |  |
| `revisionHistoryLimit` _integer_ | 为支持回滚而保留的旧 ReplicaSet 数量。 | 3 |  |
| `updateStrategy` _[UpdateStrategy](#updatestrategy)_ | 集群升级策略设置。 | \{ type:RollingUpdate \} |  |
| `coreTemplate` _[EMQXCoreTemplate](#emqxcoretemplate)_ | 运行 EMQX Core 节点的 Pod 模板。 | \{ spec:map[persistentVolumeClaimSpec:map[accessModes:[ReadWriteOnce] resources:map[requests:map[storage:500Mi]]] replicas:1] \} |  |
| `replicantTemplate` _[EMQXReplicantTemplate](#emqxreplicanttemplate)_ | 运行 EMQX Replicant 节点的 Pod 模板。 |  |  |
| `dashboardServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | 用于公开 EMQX Dashboard 的 Service 模板。<br />Dashboard Service 始终指向 EMQX Core 节点集。<br />模板中名为 `dashboard` 或 `dashboard-https` 的端口会覆盖对应的自动生成 Service 端口。<br />可通过 `port` 在其他 Service 端口上公开监听器，但 `targetPort` 必须解析到对应的 Dashboard 监听器。<br />建议使用保留的命名目标端口，使其能够跟随监听器绑定地址的变化。 |  |  |
| `listenersServiceTemplate` _[ServiceTemplate](#servicetemplate)_ | 用于公开已启用 EMQX 监听器的 Service 模板。<br />如果已启用且存在 Replicant 节点，Listeners Service 将指向 EMQX Replicant 节点集；<br />否则指向 EMQX Core 节点集。 |  |  |


#### EMQXStatus



EMQXStatus 定义观测到的 EMQX 状态。



_出现于：_
- [EMQX](#emqx)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `conditions` _[Condition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#condition-v1-meta) array_ | 表示 EMQX 自定义资源当前状态的条件。 |  |  |
| `coreReplicas` _integer_ | 观测到的 Core 节点集副本数。 |  |  |
| `coreSelector` _string_ | 与 Core Pod 匹配的序列化标签选择器。 |  |  |
| `replicantReplicas` _integer_ | 观测到的 Replicant 节点集 Pod 副本数。<br />供 scale 子资源使用。 |  |  |
| `replicantSelector` _string_ | 与 Replicant Pod 匹配的序列化标签选择器。<br />供 scale 子资源发现 HPA Pod。 |  |  |
| `coreNodes` _[EMQXNode](#emqxnode) array_ | 集群中各 Core 节点的状态。 |  |  |
| `coreNodesStatus` _[CoreNodesStatus](#corenodesstatus)_ | Core 节点集的汇总状态。 |  |  |
| `replicantNodes` _[EMQXNode](#emqxnode) array_ | 集群中各 Replicant 节点的状态。 |  |  |
| `replicantNodesStatus` _[ReplicantNodesStatus](#replicantnodesstatus)_ | Replicant 节点集的汇总状态。 |  |  |
| `nodeEvacuations` _[NodeEvacuationStatus](#nodeevacuationstatus) array_ | 集群中正在进行的节点疏散状态。 |  |  |
| `dsReplication` _[DSReplicationStatus](#dsreplicationstatus)_ | EMQX Durable Storage 复制状态。 |  |  |
| `config` _[ConfigStatus](#configstatus)_ | 声明式 EMQX 配置的协调状态。<br />这些字段仅提供实现信息。请优先使用 `ConfigApplied` 条件<br />判断配置的生命周期状态。 |  |  |


#### EvacuationStrategy







_出现于：_
- [UpdateStrategy](#updatestrategy)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `type` _[EvacuationStrategyType](#evacuationstrategytype)_ | 节点疏散策略的类型。 | NodeEvacuation | Enum: [NodeEvacuation Disabled] <br /> |
| `connectionEvictionRate` _integer_ | 客户端断开连接速率（每秒数量）。<br />与 [EMQX 节点疏散](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation)中的 `conn-evict-rate` 相同。 | 1000 | Minimum: 1 <br /> |
| `sessionEvictionRate` _integer_ | 会话疏散速率（每秒数量）。<br />与 [EMQX 节点疏散](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation)中的 `sess-evict-rate` 相同。 | 1000 | Minimum: 1 <br /> |
| `waitTakeover` _integer_ | 开始疏散会话前的等待时间（秒）。<br />与 [EMQX 节点疏散](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation)中的 `wait-takeover` 相同。 | 10 | Minimum: 0 <br /> |
| `waitHealthCheck` _integer_ | 节点等待负载均衡器将其从活动后端节点列表中移除的时间（秒）。<br />与 [EMQX 节点疏散](https://docs.emqx.com/en/emqx/v5.10/deploy/cluster/rebalancing.html#node-evacuation)中的 `wait-health-check` 相同。 | 60 | Minimum: 0 <br /> |


#### EvacuationStrategyType

_底层类型：_ _string_





_出现于：_
- [EvacuationStrategy](#evacuationstrategy)

| 字段 | 说明 |
| --- | --- |
| `NodeEvacuation` |  |
| `Disabled` |  |


#### NodeEvacuationStatus







_出现于：_
- [EMQXStatus](#emqxstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `nodeName` _string_ | 被疏散的节点名称 |  |  |
| `state` _string_ | 疏散状态 |  |  |
| `sessionRecipients` _string array_ | 会话接收节点 |  |  |
| `sessionEvictionRate` _integer_ | 会话疏散速率，单位为会话数/秒。 |  |  |
| `connectionEvictionRate` _integer_ | 连接疏散速率，单位为连接数/秒。 |  |  |
| `initialSessions` _integer_ | 此节点的初始会话数 |  |  |
| `initialConnections` _integer_ | 此节点的初始连接数 |  |  |


#### ReplicantNodesStatus



ReplicantNodesStatus 是 Replicant 节点集的汇总状态。
多 ReplicaSet 模式要求在 CR 级别跟踪修订版本。



_出现于：_
- [EMQXStatus](#emqxstatus)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `readyReplicas` _integer_ | Ready 副本数。 |  |  |
| `currentRevision` _string_ | Replicant 节点集的当前修订版本。 |  |  |
| `currentReplicas` _integer_ | 运行当前修订版本的副本数。 |  |  |
| `updateRevision` _string_ | Replicant 节点集的更新修订版本。<br />如果与当前修订版本不同，表示正在更新该节点集。 |  |  |
| `updateReplicas` _integer_ | 运行更新修订版本的副本数。 |  |  |
| `collisionCount` _integer_ |  |  |  |


#### ReplicantsUpdateStrategy



ReplicantsUpdateStrategy 控制 Replicant ReplicaSet 的发布速度。
其语义与 `apps/v1 Deployment.spec.strategy.rollingUpdate` 一致。



_出现于：_
- [UpdateStrategy](#updatestrategy)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `maxUnavailable` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | Replicant ReplicaSet 发布期间，允许同时排空（正在疏散、终止或标记为删除）的旧 Replicant Pod 最大数量。<br />整数表示绝对数量；字符串表示预期 Replicant 副本数的百分比，例如 `"25%"`。<br />默认值为 1，即逐个排空。 |  | XIntOrString: \{\} <br /> |
| `maxSurge` _[IntOrString](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#intorstring-intstr-util)_ | 模板发布期间，新 ReplicaSet 中允许超出预期副本数的额外 Replicant Pod 数量。<br />整数表示绝对数量；字符串表示预期副本数的百分比。默认值为 0。 |  | XIntOrString: \{\} <br /> |


#### ServiceTemplate







_出现于：_
- [EMQXSpec](#emqxspec)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `enabled` _boolean_ | 是否创建此 Service。 | true |  |
| `metadata` _[TemplateObjectMeta](#templateobjectmeta)_ | 应用于此模板所生成对象的元数据。 |  |  |
| `spec` _[ServiceSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#servicespec-v1-core)_ | Service 的预期状态规格。<br />https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status |  |  |


#### TemplateObjectMeta



TemplateObjectMeta 包含要传播到模板所创建对象的元数据。



_出现于：_
- [EMQXCoreTemplate](#emqxcoretemplate)
- [EMQXReplicantTemplate](#emqxreplicanttemplate)
- [ServiceTemplate](#servicetemplate)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `labels` _object (keys:string, values:string)_ | 应用于所生成对象的标签。 |  |  |
| `annotations` _object (keys:string, values:string)_ | 应用于所生成对象的注解。 |  |  |


#### UpdateStrategy







_出现于：_
- [EMQXSpec](#emqxspec)

| 字段 | 说明 | 默认值 | 校验规则 |
| --- | --- | --- | --- |
| `type` _string_ | 确定集群升级的执行方式。<br />* `RollingUpdate`：通过逐步更新 Pod 来执行滚动升级；Core Pod 始终逐个更新，<br />   Replicant 的更新由 `replicants` 策略控制。 | RollingUpdate | Enum: [RollingUpdate] <br /> |
| `evacuationStrategy` _[EvacuationStrategy](#evacuationstrategy)_ | 节点疏散策略设置。 | \{ type:NodeEvacuation \} |  |
| `replicants` _[ReplicantsUpdateStrategy](#replicantsupdatestrategy)_ | Replicant ReplicaSet 滚动更新的参数。 |  |  |
