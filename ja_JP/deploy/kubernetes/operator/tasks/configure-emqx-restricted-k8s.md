# 制限付きアクセス環境での k8s 上の EMQX クラスターのデプロイ

ここでは、k8s クラスターがインターネットにアクセスできず、ユーザーが `ClusterRole` の作成および使用権限を持っていないことを前提とします。

+ `emqx-operator` と `emqx` は同じネームスペースにインストールされている
+ Cert manager はクラスター全体または `emqx-operator` と同じネームスペースに存在する可能性がある
+ `emqx-operator` はプライベートな Docker レジストリを使用するように設定されており、`emqx` はカスタムの `securityContext` を使用するように設定されている

## タスクの目標

- 必要なイメージをプライベート Docker レジストリにプッシュする
- `cert-manager` のデフォルトパラメータをオーバーライドしてプライベートレジストリを使用する
- EMQX Operator の CRD を手動でインストールする
- `emqx-operator` のデフォルトパラメータをオーバーライドしてプライベートレジストリ、単一ネームスペース、カスタム `securityContext`、および webhook 無効化を設定する
- EMQX にカスタム `securityContext` を使用する

## 必要な Docker イメージをプライベート Docker レジストリにプッシュする

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

## Cert-Manager のデプロイ

クラスターに cert-manager がインストール済みの場合はこのステップをスキップしてください。

必要に応じてネームスペース名を更新してください。

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

## EMQX Operator のデプロイ

### リリースアセットから CRD を手動でデプロイ

```bash
kubectl -n emqx apply -f https://github.com/emqx/emqx-operator/releases/download/$EMQX_OPERATOR_VERSION/crds.yaml
```

### Emqx-Operator のデプロイ

cert-manager がクラスター全体にインストール済みの場合は `--set cert-manager.enable=false` を追加してください。

この例では `podSecurityContext` と `containerSecurityContext` にデフォルト値を設定しています。必要に応じてオーバーライドしてください。

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

`emqx-operator` が起動して稼働していることを確認してください。

```bash
kubectl -n emqx wait --for=condition=Ready pods -l "control-plane=controller-manager"
```

## EMQX クラスターの設定

1. 以下の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイします。

   ```bash
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

2. EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターのステータスを確認できます。`STATUS` が `Running` であることを確認してください。準備完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   IMAGE                                             STATUS    AGE
   emqx   my.private.registry/emqx/emqx-enterprise:5.10.0   Running   10m
   ```
