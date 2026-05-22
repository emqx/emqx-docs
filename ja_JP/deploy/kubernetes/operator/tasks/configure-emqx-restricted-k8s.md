<<<<<<< HEAD
# 制限付きアクセス環境での k8s 上への EMQX クラスターのデプロイ

ここでは、k8s クラスターがインターネットにアクセスできず、ユーザーに `ClusterRole` の作成および使用権限がないことを前提としています。

+ `emqx-operator` と `emqx` は同じネームスペースにインストールされます
+ Cert manager はクラスター全体または `emqx-operator` と同じネームスペースに存在する可能性があります
+ `emqx-operator` はプライベートな Docker レジストリを使用するように設定されており、`emqx` はカスタムの `securityContext` を使用するように設定されています

## タスクの目的

- 必要なイメージをプライベート Docker レジストリにプッシュする
- `cert-manager` のデフォルトパラメータをプライベートレジストリ使用に上書きする
- EMQX Operator の CRD を手動でインストールする
- `emqx-operator` のデフォルトパラメータをプライベートレジストリ、単一ネームスペース、カスタム `securityContext`、および webhook 無効化に上書きする
- EMQX にカスタム `securityContext` を使用する

## 必要な Docker イメージをプライベート Docker レジストリにプッシュする
=======
# k8sでアクセス制限付きEMQXクラスターをデプロイする

ここでは、k8sクラスターがインターネットにアクセスできず、ユーザーが`ClusterRole`の作成および使用権限を持っていないことを前提としています。

+ `emqx-operator`と`emqx`は同じネームスペースにインストールされます
+ Cert managerはクラスター全体、または`emqx-operator`と同じネームスペースに存在する可能性があります
+ `emqx-operator`はプライベートなDockerレジストリを使用するように設定され、`emqx`はカスタムの`securityContext`を使用するように設定されています

## タスクの対象

- 必要なイメージをプライベートDockerレジストリにプッシュする
- `cert-manager`のデフォルトパラメータをプライベートレジストリを使用するように上書きする
- EMQX OperatorのCRDを手動でインストールする
- `emqx-operator`のデフォルトパラメータをプライベートレジストリ、単一ネームスペース、カスタム`securityContext`、およびWebhook無効化で上書きする
- EMQXにカスタム`securityContext`を使用する

## 必要なDockerイメージをプライベートDockerレジストリにプッシュする
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
## Cert-Manager のデプロイ

cert-manager がクラスターにインストール済みの場合はこのステップをスキップしてください。
=======
## Cert-Managerのデプロイ

クラスターにcert-managerがインストールされている場合はこのステップをスキップしてください。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
## EMQX Operator のデプロイ

### リリースアセットから CRD を手動でデプロイ
=======
## EMQX Operatorのデプロイ

### リリースアセットからCRDを手動でデプロイ
>>>>>>> origin/release-6.1

```bash
kubectl -n emqx apply -f https://github.com/emqx/emqx-operator/releases/download/$EMQX_OPERATOR_VERSION/crds.yaml
```

<<<<<<< HEAD
### Emqx-Operator のデプロイ

cert-manager がクラスター全体にインストール済みの場合は `--set cert-manager.enable=false` を追加してください。

この例では `podSecurityContext` と `containerSecurityContext` にデフォルト値が含まれています。必要に応じて上書きしてください。
=======
### Emqx-Operatorのデプロイ

cert-managerがすでにクラスター全体にインストールされている場合は、`--set cert-manager.enable=false`を追加してください。

この例では`podSecurityContext`と`containerSecurityContext`はデフォルト値を含みます。必要に応じて上書きしてください。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
emqx-operator が起動して稼働していることを確認してください：
=======
emqx-operatorが起動して正常に稼働していることを確認してください：
>>>>>>> origin/release-6.1

```bash
kubectl -n emqx wait --for=condition=Ready pods -l "control-plane=controller-manager"
```

<<<<<<< HEAD
## EMQX クラスターの設定

1. 以下の内容を YAML ファイルとして保存し、`kubectl apply` コマンドでデプロイします：
=======
## EMQXクラスターの設定

1. 以下の内容をYAMLファイルとして保存し、`kubectl apply`コマンドでデプロイします：
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
2. EMQX クラスターが準備完了になるまで待ちます。`kubectl get` コマンドで EMQX クラスターのステータスを確認し、`STATUS` が `Running` であることを確認してください。準備完了までに時間がかかる場合があります。
=======
2. EMQXクラスターが準備完了になるまで待ちます。`kubectl get`コマンドでEMQXクラスターのステータスを確認できます。`STATUS`が`Running`になっていることを確認してください。完了までに時間がかかる場合があります。
>>>>>>> origin/release-6.1

   ```bash
   $ kubectl get emqx emqx
   NAME   IMAGE                                             STATUS    AGE
   emqx   my.private.registry/emqx/emqx-enterprise:5.10.0   Running   10m
   ```
