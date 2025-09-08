# 在限制访问环境下在 Kubernetes 中部署 EMQX 集群

本文档适用于以下场景：

- Kubernetes 集群无法访问外网；
- 用户无权限创建或使用 `ClusterRole`；
- `emqx-operator` 和 `emqx` 部署在同一个命名空间；
- cert-manager 可用，但仅在集群级或 `emqx-operator` 命名空间中；
- `emqx-operator` 配置为使用私有镜像仓库；
- `emqx` 配置了自定义的 `securityContext`。

## 部署目标

- 将所需镜像推送到私有镜像仓库；
- 覆盖 cert-manager 默认参数以使用私有仓库；
- 手动安装 EMQX Operator 的 CRD；
- 配置 emqx-operator 使用私有仓库、单命名空间、自定义 `securityContext`，并禁用 Webhook；
- 配置 EMQX 使用自定义 `securityContext`。

## 将所需 Docker 镜像推送到私有镜像仓库

```bash
export CERT_MANAGER_VERSION='v1.16.2'
export EMQX_OPERATOR_VERSION='2.2.26'
export EMQX_VERSION='5.10.0'
export REGISTRY='my.private.registry'

CERT_MANAGER_IMAGES=(
    "cert-manager-controller"
    "cert-manager-cainjector"
    "cert-manager-webhook"
    "cert-manager-acmesolver"
    "cert-manager-startupapicheck"
)

pull_retag_push() {
    local source=$1
    local target=$2
    docker pull "$source"
    docker tag "$source" "$target"
    docker push "$target"
}

for img in "${CERT_MANAGER_IMAGES[@]}"; do
    pull_retag_push "quay.io/jetstack/$img:$CERT_MANAGER_VERSION" "$REGISTRY/jetstack/$img:$CERT_MANAGER_VERSION"
done

pull_retag_push "emqx/emqx-enterprise:$EMQX_VERSION" "$REGISTRY/emqx/emqx-enterprise:$EMQX_VERSION"
pull_retag_push "emqx/emqx-operator-controller:$EMQX_OPERATOR_VERSION" "$REGISTRY/emqx/emqx-operator-controller:$EMQX_OPERATOR_VERSION"
```

## 部署 cert-manager

如果集群中已安装 cert-manager，可跳过此步骤。

如有需要，请修改命名空间名称。

```bash
helm repo add jetstack https://charts.jetstack.io
helm repo update
helm upgrade --install cert-manager jetstack/cert-manager \
   --namespace emqx \
   --create-namespace \
   --set crds.enabled=true \
   --set image.repository=$REGISTRY/jetstack/cert-manager-controller \
   --set image.tag=$CERT_MANAGER_VERSION \
   --set webhook.image.repository=$REGISTRY/jetstack/cert-manager-webhook \
   --set webhook.image.tag=$CERT_MANAGER_VERSION \
   --set cainjector.image.repository=$REGISTRY/jetstack/cert-manager-cainjector \
   --set cainjector.image.tag=$CERT_MANAGER_VERSION \
   --set acmesolver.image.repository=$REGISTRY/jetstack/cert-manager-acmesolver \
   --set acmesolver.image.tag=$CERT_MANAGER_VERSION \
   --set startupapicheck.image.repository=$REGISTRY/jetstack/cert-manager-startupapicheck \
   --set startupapicheck.image.tag=$CERT_MANAGER_VERSION
```

## 部署 EMQX Operator

### 手动部署 CRD

```bash
kubectl -n emqx apply -f https://github.com/emqx/emqx-operator/releases/download/$EMQX_OPERATOR_VERSION/crds.yaml
```

### 安装 emqx-operator

> 如果集群中已安装 cert-manager，可通过 `--set cert-manager.enable=false` 禁用自动安装。

以下示例中 `podSecurityContext` 和 `containerSecurityContext` 使用默认值，请根据实际需要进行调整：

```bash
helm repo add emqx https://repos.emqx.io/charts
helm repo update
helm upgrade --install emqx-operator emqx/emqx-operator \
  --namespace emqx \
  --create-namespace \
  --set singleNamespace=true \
  --set webhook.enabled=false \
  --set crds.enabled=false \
  --set-json='podSecurityContext={"runAsNonRoot":true}' \
  --set-json='containerSecurityContext={"allowPrivilegeEscalation":false}' \
  --set image.repository=$REGISTRY/emqx/emqx-operator-controller \
  --set image.tag=$EMQX_OPERATOR_VERSION
```

确认 `emqx-operator` 是否已成功运行：

```bash
kubectl -n emqx wait --for=condition=Ready pods -l "control-plane=controller-manager"
```

## 配置 EMQX 集群

1. 将以下内容保存为 YAML 文件，并通过 `kubectl apply` 命令部署：

```yaml
apiVersion: apps.emqx.io/v2beta1
kind: EMQX
metadata:
  name: emqx
  namespace: emqx
spec:
  image: ${REGISTRY}/emqx/emqx-enterprise:${EMQX_VERSION}
  config:
    data: |
      license {
        key = "..."
      }
```

2. 等待 EMQX 集群就绪。可通过以下命令检查状态，请确保 `STATUS` 为 `Running`（可能需要一些时间）：

```bash
$ kubectl get emqx emqx
NAME   IMAGE                                             STATUS    AGE
emqx   my.private.registry/emqx/emqx-enterprise:5.10.0   Running   10m
```
