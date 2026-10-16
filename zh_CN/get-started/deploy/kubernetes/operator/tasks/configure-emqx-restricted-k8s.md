# 在受限 Kubernetes 环境中部署 EMQX 集群

本页假定 Kubernetes 集群无法访问互联网，并且用户没有创建或使用 `ClusterRole` 的权限。

+ `emqx-operator` 和 `emqx` 安装在同一命名空间中。
+ `emqx-operator` 配置为使用私有 Docker 镜像仓库，`emqx` 配置为使用自定义 `securityContext`。

## 任务目标

- 将所需镜像推送到私有 Docker 镜像仓库。
- 手动安装 EMQX Operator CRD。
- 覆盖 `emqx-operator` 的默认参数，以使用私有镜像仓库、单一命名空间和自定义 `securityContext`。
- 为 EMQX 使用自定义 `securityContext`。

## 将所需 Docker 镜像推送到私有镜像仓库

```bash
export EMQX_OPERATOR_VERSION='3.0.0'
export EMQX_VERSION='5.10.0'
export REGISTRY='my.private.registry'

pull_retag_push() {
    local source=$1
    local target=$2
    docker pull "$source"
    docker tag "$source" "$target"
    docker push "$target"
}

pull_retag_push "emqx/emqx-enterprise:$EMQX_VERSION" "$REGISTRY/emqx/emqx-enterprise:$EMQX_VERSION"
pull_retag_push "ghcr.io/emqx/emqx-operator:$EMQX_OPERATOR_VERSION" "$REGISTRY/emqx/emqx-operator:$EMQX_OPERATOR_VERSION"
```

## 部署 EMQX Operator

### 使用 Release 资产手动部署 CRD

```bash
kubectl -n emqx apply -f https://github.com/emqx/emqx-operator/releases/download/$EMQX_OPERATOR_VERSION/crds.yaml
```

### 部署 EMQX Operator

本示例中的 `podSecurityContext` 和 `containerSecurityContext` 使用默认值，请根据需要覆盖这些值。

```bash
helm repo add emqx https://repos.emqx.io/charts
helm repo update
helm upgrade --install emqx-operator emqx/emqx-operator \
  --namespace emqx \
  --create-namespace \
  --set-json='watchNamespaces=["emqx"]' \
  --set skipCRDs=true \
  --set-json='podSecurityContext={"runAsNonRoot":true}' \
  --set-json='containerSecurityContext={"allowPrivilegeEscalation":false}' \
  --set image.repository=$REGISTRY/emqx/emqx-operator \
  --set image.tag=$EMQX_OPERATOR_VERSION
```

确认 EMQX Operator 已启动并正常运行：

```bash
kubectl -n emqx wait --for=condition=Ready pods -l "control-plane=controller-manager"
```

## 配置 EMQX 集群

1. 将以下内容保存为 `emqx.yaml`：

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
     namespace: emqx
   spec:
     image: ${REGISTRY}/emqx/emqx-enterprise:${EMQX_VERSION}
     config:
       roots:
         license:
           key: "..."
   ```

2. 使用 `envsubst` 替换之前导出的环境变量，并部署渲染后的清单：

   ```bash
   envsubst < emqx.yaml | kubectl apply -f -
   ```

3. 等待 EMQX 集群就绪。可以使用 `kubectl get` 检查 EMQX 集群状态。确保 `STATUS` 为 `Ready`。此过程可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```
