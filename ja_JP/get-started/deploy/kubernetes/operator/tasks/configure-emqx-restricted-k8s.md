# k8sでアクセス制限付きEMQXクラスターをデプロイする

ここでは、k8sクラスターがインターネットにアクセスできず、ユーザーが`ClusterRole`の作成および使用権限を持っていないことを前提としています。

+ `emqx-operator`と`emqx`は同じネームスペースにインストールされます
+ `emqx-operator`はプライベートなDockerレジストリを使用するように設定されており、`emqx`はカスタムの`securityContext`を使用するように設定されています

## タスクの目的

- 必要なイメージをプライベートDockerレジストリにプッシュする
- EMQX OperatorのCRDを手動でインストールする
- `emqx-operator`のデフォルトパラメータを上書きし、プライベートレジストリ、単一ネームスペース、およびカスタム`securityContext`を使用するようにする
- EMQXにカスタム`securityContext`を使用する

## 必要なDockerイメージをプライベートDockerレジストリにプッシュする

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

## EMQX Operatorのデプロイ

### リリースアセットからCRDを手動でデプロイ

```bash
kubectl -n emqx apply -f https://github.com/emqx/emqx-operator/releases/download/$EMQX_OPERATOR_VERSION/crds.yaml
```

### Emqx-Operatorのデプロイ

この例では`podSecurityContext`と`containerSecurityContext`にデフォルト値を設定しています。必要に応じて上書きしてください。

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

emqx-operatorが起動して稼働していることを確認します：

```bash
kubectl -n emqx wait --for=condition=Ready pods -l "control-plane=controller-manager"
```

## EMQXクラスターの設定

1. 以下の内容を`emqx.yaml`として保存します：

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

2. 先にエクスポートした環境変数を`envsubst`で展開し、レンダリングされたマニフェストをデプロイします：

   ```bash
   envsubst < emqx.yaml | kubectl apply -f -
   ```

3. EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでEMQXクラスターの状態を確認できます。`STATUS`が`Ready`であることを確認してください。完了までに時間がかかる場合があります。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```
