# Deploy EMQX Cluster in k8s with Restricted Access

Here we are assuming k8s cluster does not have access to the internet, and the user does not have permissions to create and/or use `ClusterRole`.

+ Both `emqx-operator` and `emqx` are installed in the same namespace
+ The `emqx-operator` is configured to use a private docker registry, and the `emqx` is configured to use a custom `securityContext`

## Task Target

- Push necessary images to a private docker registry
- Manually install EMQX Operator CRDs
- Override default parameters of `emqx-operator` to use private registry, single namespace, and custom `securityContext`
- Use custom `securityContext` for EMQX

## Push Necessary Docker Images to a Private Docker Registry

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

## Deploy EMQX Operator

### Deploy CRDs Manually from Release Assets

```bash
kubectl -n emqx apply -f https://github.com/emqx/emqx-operator/releases/download/$EMQX_OPERATOR_VERSION/crds.yaml
```

### Deploy Emqx-Operator

In this example `podSecurityContext` and `containerSecurityContext` contain default values, override as necessary.

```bash
helm repo add emqx https://repos.emqx.io/charts
helm repo update
helm upgrade --install emqx-operator emqx/emqx-operator \
  --namespace emqx \
  --create-namespace \
  --set singleNamespace=true \
  --set crds.enabled=false \
  --set-json='podSecurityContext={"runAsNonRoot":true}' \
  --set-json='containerSecurityContext={"allowPrivilegeEscalation":false}' \
  --set image.repository=$REGISTRY/emqx/emqx-operator \
  --set image.tag=$EMQX_OPERATOR_VERSION
```

Ensure emqx-operator is up and running:

```bash
kubectl -n emqx wait --for=condition=Ready pods -l "control-plane=controller-manager"
```

## Configure EMQX Cluster

1. Save the following content as a YAML file and deploy it with the `kubectl apply` command:

   ```bash
   apiVersion: apps.emqx.io/v3beta1
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

2. Wait for the EMQX cluster to be ready. You can check the status of the EMQX cluster through `kubectl get` command. Make sure `STATUS` is `Ready`. This may take some time.

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```
