# Kubernetes上でのHelmチャートを使ったEMQXのデプロイ

このページでは、公式Helmチャートを使用してKubernetesクラスター上にEMQXをデプロイする手順を段階的に説明します。

公式のEMQX Helmチャートは、StatefulSet、Service、ConfigMap、Ingressルールなどの必要なEMQXコンポーネントをすべてパッケージ化し、単一の設定可能なHelmチャートとして提供することで、Kubernetesベースのデプロイを簡素化します。

## 前提条件

開始する前に、以下がインストールおよび設定されていることを確認してください：

+ 稼働中のKubernetesクラスター（バージョン1.6以上）
+ [Helm](https://github.com/helm/helm/releases)

## EMQX Helmチャートのインストール

EMQX Helmチャートは、EMQXのGitHubリポジトリまたは公式Helmチャートリポジトリのいずれかからインストールできます。

### GitHubからのインストール

リリース名を`my-emqx`としてGitHubからチャートをインストールするには、以下を実行します：

```bash
$ git clone https://github.com/emqx/emqx.git
$ cd emqx/deploy/charts/emqx-enterprise
$ helm install my-emqx .
```

### Helmリポジトリからのインストール

リリース名を`my-emqx`として公式Helmチャートリポジトリからインストールするには、以下を実行します：

```bash
helm repo add emqx https://repos.emqx.io/charts
helm install my-emqx emqx/emqx-enterprise
```
> 不安定なバージョンをインストールしたい場合は、`--devel`フラグを追加してください：
>
> ```bash
> helm install my-emqx emqx/emqx-enterprise --devel
> ```

## チャートのアンインストール

`my-emqx`という名前のEMQXリリースを削除し、関連するすべてのKubernetesリソースを削除するには：

**Helm v3以降の場合**

```bash
$ helm uninstall my-emqx
```

**Helm v2（旧バージョン）の場合**

```bash
$ helm del my-emqx
```

## 設定パラメータ

EMQX Helmチャートは、`values.yaml`ファイルを通じて幅広い設定パラメータを提供しています。以下の表は主なパラメータとデフォルト値を示しています。

| パラメータ                            | 説明                                                  | デフォルト値                                           |
| ------------------------------------ | ----------------------------------------------------- | ------------------------------------------------------- |
| `replicaCount`                       | ネットワーク分断時の自動回復のため、奇数のノード数を推奨します。 | 3                                                       |
| `image.repository`                   | EMQXイメージ名                                        | emqx/emqx-enterprise                                    |
| `image.pullPolicy`                   | イメージのプルポリシー                                | IfNotPresent                                            |
| `image.pullSecrets`                  | イメージプルシークレット                              | `[]`（デプロイされたPodにプルシークレットは追加されません） |
| `serviceAccount.create`              | `true`の場合、新しいサービスアカウントを作成します。 | `true`                                                  |
| `serviceAccount.name`                | 使用するサービスアカウント。未設定かつ`serviceAccount.create`が`true`の場合、フルネームテンプレートで名前が生成されます。 |                                                         |
| `serviceAccount.annotations`         | サービスアカウントに追加するアノテーション            |                                                         |
| `envFromSecret`                      | 同じKubernetesネームスペース内のシークレット名。環境変数として値が追加されます。 | nil                                                     |
| `recreatePods`                       | アップグレード時にPodの再作成を強制します。最新設定を常に適用したい場合に有用です。 | false                                                   |
| `podAnnotations`                    | Podに付与するアノテーション                            | `{}`                                                    |
| `podManagementPolicy`                | 既存のPVCを持つチャートを再デプロイする場合、デッドロックを避けるため`Parallel`に設定する必要があります。 | `Parallel`                                              |
| `persistence.enabled`                | PVCを使用したEMQXのパーシステンスを有効にします。     | false                                                   |
| `persistence.storageClass`           | バックエンドPVCのストレージクラス                      | `nil`（alphaストレージクラスアノテーションを使用）       |
| `persistence.existingClaim`          | EMQXデータ用の既存Persistent Volumeクレーム名。テンプレートとして評価されます。 | ""                                                      |
| `persistence.accessMode`             | EMQXボリュームのPVCアクセスモード                      | ReadWriteOnce                                           |
| `persistence.size`                   | EMQXボリュームのPVCストレージ要求サイズ                | 20Mi                                                    |
| `initContainers`                     | EMQXコンテナ作成前に実行されるコンテナ。ユーティリティやセットアップスクリプトを含めることができます。 | `{}`                                                    |
| `resources`                          | CPU/メモリのリソース要求・制限                         | {}                                                      |
| `extraVolumeMounts`                  | デフォルトバックエンドコンテナへの追加のvolumeMounts  | []                                                      |
| `extraVolumes`                       | デフォルトバックエンドPodへの追加ボリューム            | []                                                      |
| `nodeSelector`                       | Pod割り当て用のノードラベル                            | `{}`                                                    |
| `tolerations`                        | Pod割り当て用のトレランスラベル                        | `[]`                                                    |
| `affinity`                           | ノード/Podのアフィニティマップ                          | `{}`                                                    |
| `service.type`                       | Kubernetes Serviceタイプ                                | ClusterIP                                               |
| `service.mqtt`                       | MQTT用ポート                                           | 1883                                                    |
| `service.mqttssl`                    | MQTT(SSL)用ポート                                      | 8883                                                    |
| `service.ws`                         | WebSocket/HTTP用ポート                                 | 8083                                                    |
| `service.wss`                        | WSS/HTTPS用ポート                                     | 8084                                                    |
| `service.dashboard`                  | ダッシュボードおよびAPI用ポート                        | 18083                                                   |
| `service.customPorts`                | Serviceで公開するカスタムポート                        | {}                                                      |
| `service.nodePorts.mqtt`             | MQTT用Kubernetesノードポート                           | nil                                                     |
| `service.nodePorts.mqttssl`          | MQTT(SSL)用Kubernetesノードポート                      | nil                                                     |
| `service.nodePorts.ws`               | WebSocket/HTTP用Kubernetesノードポート                 | nil                                                     |
| `service.nodePorts.wss`              | WSS/HTTPS用Kubernetesノードポート                      | nil                                                     |
| `service.nodePorts.dashboard`        | ダッシュボード用Kubernetesノードポート                 | nil                                                     |
| `service.customNodePorts`            | カスタムポート用Kubernetesノードポート                 | {}                                                      |
| `service.loadBalancerClass`          | このServiceが属するロードバランサークラス               |                                                         |
| `service.loadBalancerIP`             | ServiceのloadBalancerIP                                | nil                                                     |
| `service.loadBalancerSourceRanges`   | LoadBalancerサービスで許可されるアドレス                | []                                                      |
| `service.externalIPs`                | ServiceのExternalIPs                                   | []                                                      |
| `service.externalTrafficPolicy`      | Serviceの外部トラフィックポリシー                       | `Cluster`                                               |
| `service.annotations`                | Service/ServiceMonitorのアノテーション                  | {}（テンプレートとして評価）                             |
| `service.labels`                     | Service/ServiceMonitorのラベル                           | {}（テンプレートとして評価）                             |
| `ingress.dashboard.enabled`          | EMQXダッシュボード用Ingressを有効化                    | false                                                   |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定                 |                                                         |
| `ingress.dashboard.path`             | EMQXダッシュボード用Ingressパス                         | /                                                       |
| `ingress.dashboard.pathType`         | EMQXダッシュボード用Ingress pathType                   | `ImplementationSpecific`                                |
| `ingress.dashboard.hosts`            | EMQXダッシュボード用Ingressホスト                       | dashboard.emqx.local                                    |
| `ingress.dashboard.tls`              | EMQXダッシュボード用Ingress TLS設定                     | []                                                      |
| `ingress.dashboard.annotations`      | EMQXダッシュボード用Ingressアノテーション               | {}                                                      |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定                 |                                                         |
| `ingress.mqtt.enabled`               | MQTT用Ingressを有効化                                   | false                                                   |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                               |                                                         |
| `ingress.mqtt.path`                  | MQTT用Ingressパス                                       | /                                                       |
| `ingress.mqtt.pathType`              | MQTT用Ingress pathType                                  | `ImplementationSpecific`                                |
| `ingress.mqtt.hosts`                 | MQTT用Ingressホスト                                    | mqtt.emqx.local                                         |
| `ingress.mqtt.tls`                   | MQTT用Ingress TLS設定                                  | []                                                      |
| `ingress.mqtt.annotations`           | MQTT用Ingressアノテーション                              | {}                                                      |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                               |                                                         |
| `metrics.enable`                     | `true`に設定すると、[prometheus-operator](https://github.com/prometheus-operator/prometheus-operator)のインストールとemqx_prometheusの有効化が必要です。 | false                                                   |
| `metrics.type`                       | 現在サポートされているのは"prometheus"のみです。          | "prometheus"                                            |
| `ssl.enabled`                        | SSLサポートを有効化                                    | false                                                   |
| `ssl.useExisting`                    | 既存の証明書を使用するか、cert-managerに生成させるかを指定 | false                                                   |
| `ssl.existingName`                   | 既存証明書の名前                                      | emqx-tls                                                |
| `ssl.dnsnames`                       | 生成する証明書のDNS名                                  | {}                                                      |
| `ssl.commonName`                     | 生成する証明書の共通名                                  |                                                         |
| `ssl.issuer.name`                    | 証明書生成用のIssuer名                                 | letsencrypt-dns                                         |
| `ssl.issuer.kind`                    | 証明書生成用のIssuer種別                               | ClusterIssuer                                           |

### EMQX固有のパラメータ

以下の表は、チャートのEMQX固有の設定可能なパラメータとそのデフォルト値を示しています。

| パラメータ                                                                                                                                                              | 説明                                                                                  | デフォルト値 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------|---------------|
| `emqxConfig`                                                                                                                                                           | [環境変数](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#environment-variables)（`EMQX_`プレフィックスは任意）またはEMQX設定ファイルで使用される名前空間付きドット表記を用いて定義された[設定](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html)項目のマップ。 | `nil`         |
| `emqxLicenseSecretName`                                                                                                                                                | ライセンス情報を保持するシークレット名（非推奨）                                    | `nil`         |
| `emqxLicenseSecretRef.name`                                                                                                                                         | ライセンス情報を保持するシークレット名                                               | `""`         |
| `emqxLicenseSecretRef.key`                                                                                                                                          | ライセンス情報を保持するシークレットのキー                                           | `""`         |

## SSL設定

`cert-manager`を使用する場合、TLS証明書はKubernetesシークレットに標準キー`tls.crt`および`tls.key`で保存されます。EMQX Helmチャートはこれらの証明書ファイルをコンテナ内の以下のディレクトリに自動的にマウントします：

```
/tmp/ssl/
```

EMQXでSSLサポートを有効にするには、EMQX設定内でファイルパスを明示的に設定する必要があります。これはEMQX設定ファイルを修正するか、以下の環境変数を渡すことで可能です：

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip

既存のTLS証明書（`cert-manager`で生成されたものではない）を使用する場合は、ファイルパスが実際にマウントされている場所と一致していることを必ず確認してください。

:::

## Proxy Protocolサポート（HAProxy、Nginx）

HAProxyやNginxなどのProxy Protocolをサポートするリバースプロキシの背後にEMQXをデプロイする場合、以下の環境変数を設定して有効化してください：

```yaml
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

HAProxy Ingress Controllerの場合は、以下のアノテーションを追加します：

```yaml
haproxy-ingress.github.io/proxy-protocol: "v2"
```

これにより、プロキシを通じて転送される元のクライアントIPアドレスが保持されます。
