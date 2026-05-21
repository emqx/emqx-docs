# Helmチャートを使ったKubernetes上へのEMQXデプロイ

本ページでは、公式Helmチャートを使用してKubernetesクラスター上にEMQXをデプロイする手順を段階的に説明します。

公式のEMQX Helmチャートは、StatefulSet、Service、ConfigMap、Ingressルールなど、EMQXに必要なすべてのコンポーネントを単一の設定可能なHelmチャートにまとめることで、Kubernetesベースのデプロイを簡素化します。

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

リリース名を`my-emqx`として公式Helmチャートリポジトリからチャートをインストールするには、以下を実行します：

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

**Helm v2（レガシー）の場合**

```bash
$ helm del my-emqx
```

## 設定パラメータ

EMQX Helmチャートは、`values.yaml`ファイルを通じて幅広い設定パラメータを提供しています。以下の表は主要なパラメータとデフォルト値を示しています。

| パラメータ                            | 説明                                                  | デフォルト値                                           |
| ------------------------------------ | ----------------------------------------------------- | ----------------------------------------------------- |
| `replicaCount`                       | ネットワーク分断時の自動回復のため、ノード数は奇数を推奨します。 | 3                                                     |
| `image.repository`                   | EMQXイメージ名                                        | emqx/emqx-enterprise                                  |
| `image.pullPolicy`                   | イメージのプルポリシー                                | IfNotPresent                                          |
| `image.pullSecrets `                 | イメージプルシークレット                              | `[]`（デプロイされたポッドにイメージプルシークレットは追加されません） |
| `serviceAccount.create`              | `true`の場合、新しいサービスアカウントを作成します。 | `true`                                                |
| `serviceAccount.name`                | 使用するサービスアカウント。未設定かつ`serviceAccount.create`が`true`の場合、フルネームテンプレートで名前が生成されます。 |                                                       |
| `serviceAccount.annotations`         | サービスアカウントに追加するアノテーション            |                                                       |
| `envFromSecret`                      | 同じKubernetesネームスペース内のシークレット名で、環境変数に追加される値を含みます。 | nil                                                   |
| `recreatePods`                       | アップグレード時にポッドの再作成を強制し、最新設定を常に適用するのに役立ちます。 | false                                                 |
| `podAnnotations `                    | ポッドのアノテーション                                | `{}`                                                  |
| `podManagementPolicy`                | 既存のPVCを持つチャートを再デプロイする場合、デッドロック回避のために`Parallel`に設定する必要があります。 | `Parallel`                                            |
| `persistence.enabled`                | PVCを使用したEMQXのパーシステンスを有効にします。    | false                                                 |
| `persistence.storageClass`           | バックエンドPVCのストレージクラス                      | `nil`（alphaストレージクラスアノテーションを使用）     |
| `persistence.existingClaim`          | EMQXデータ用の既存Persistent Volumeクレーム名（テンプレートとして評価されます） | ""                                                    |
| `persistence.accessMode`             | EMQXボリューム用PVCのアクセスモード                    | ReadWriteOnce                                         |
| `persistence.size`                   | EMQXボリューム用PVCのストレージ要求サイズ              | 20Mi                                                  |
| `initContainers`                     | EMQXコンテナ作成前に実行されるコンテナ。ユーティリティやセットアップスクリプトを含めることができます。 | `{}`                                                  |
| `resources`                          | CPU/メモリのリソース要求/制限                          | {}                                                    |
| `extraVolumeMounts`                  | デフォルトのバックエンドコンテナへの追加のvolumeMounts | []                                                    |
| `extraVolumes`                       | デフォルトのバックエンドポッドへの追加ボリューム       | []                                                    |
| `nodeSelector`                       | ポッド割り当て用のノードラベル                          | `{}`                                                  |
| `tolerations`                        | ポッド割り当て用のトレランスラベル                      | `[]`                                                  |
| `affinity`                           | ノード/ポッドのアフィニティマップ                        | `{}`                                                  |
| `service.type`                       | Kubernetes Serviceのタイプ                              | ClusterIP                                             |
| `service.mqtt`                       | MQTT用ポート                                           | 1883                                                  |
| `service.mqttssl`                    | MQTT(SSL)用ポート                                      | 8883                                                  |
| `service.ws`                         | WebSocket/HTTP用ポート                                 | 8083                                                  |
| `service.wss`                        | WSS/HTTPS用ポート                                     | 8084                                                  |
| `service.dashboard`                  | ダッシュボードおよびAPI用ポート                        | 18083                                                 |
| `service.customPorts`                | Serviceで公開するカスタムポート                        | {}                                                    |
| `service.nodePorts.mqtt`             | MQTT用Kubernetesノードポート                           | nil                                                   |
| `service.nodePorts.mqttssl`          | MQTT(SSL)用Kubernetesノードポート                      | nil                                                   |
| `service.nodePorts.ws`               | WebSocket/HTTP用Kubernetesノードポート                 | nil                                                   |
| `service.nodePorts.wss`              | WSS/HTTPS用Kubernetesノードポート                      | nil                                                   |
| `service.nodePorts.dashboard`        | ダッシュボード用Kubernetesノードポート                 | nil                                                   |
| `service.customNodePorts`            | カスタムポート用Kubernetesノードポート                 | {}                                                    |
| `service.loadBalancerClass`          | このServiceが属するロードバランサーの実装クラス          |                                                       |
| `service.loadBalancerIP`             | Service用のloadBalancerIP                              | nil                                                   |
| `service.loadBalancerSourceRanges`   | LoadBalancerサービスで許可されるアドレス               | []                                                    |
| `service.externalIPs`                | Service用のExternalIPs                                 | []                                                    |
| `service.externalTrafficPolicy`      | Serviceの外部トラフィックポリシー                       | `Cluster`                                             |
| `service.annotations`                | Service/ServiceMonitorのアノテーション                  | {}（テンプレートとして評価されます）                   |
| `service.labels`                     | Service/ServiceMonitorのラベル                           | {}（テンプレートとして評価されます）                   |
| `ingress.dashboard.enabled`          | EMQXダッシュボード用Ingressを有効化                    | false                                                 |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定                |                                                       |
| `ingress.dashboard.path`             | EMQXダッシュボード用Ingressパス                         | /                                                     |
| `ingress.dashboard.pathType`         | EMQXダッシュボード用IngressのpathType                   | `ImplementationSpecific`                              |
| `ingress.dashboard.hosts`            | EMQXダッシュボード用Ingressホスト                        | dashboard.emqx.local                                  |
| `ingress.dashboard.tls`              | EMQXダッシュボード用IngressのTLS設定                     | []                                                    |
| `ingress.dashboard.annotations`      | EMQXダッシュボード用Ingressアノテーション                | {}                                                    |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定                |                                                       |
| `ingress.mqtt.enabled`               | MQTT用Ingressを有効化                                   | false                                                 |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                               |                                                       |
| `ingress.mqtt.path`                  | MQTT用Ingressパス                                      | /                                                     |
| `ingress.mqtt.pathType`              | MQTT用IngressのpathType                                | `ImplementationSpecific`                              |
| `ingress.mqtt.hosts`                 | MQTT用Ingressホスト                                    | mqtt.emqx.local                                       |
| `ingress.mqtt.tls`                   | MQTT用IngressのTLS設定                                 | []                                                    |
| `ingress.mqtt.annotations`           | MQTT用Ingressアノテーション                            | {}                                                    |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                               |                                                       |
| `metrics.enable`                     | `true`の場合、[prometheus-operator](https://github.com/prometheus-operator/prometheus-operator)のインストールとemqx_prometheusの有効化が必要です。 | false                                                 |
| `metrics.type`                       | 現在サポートしているのは「prometheus」のみです。       | "prometheus"                                          |
| `ssl.enabled`                        | SSLサポートを有効化                                    | false                                                 |
| `ssl.useExisting`                    | 既存の証明書を使用するか、cert-managerに生成させるか。  | false                                                 |
| `ssl.existingName`                   | 既存証明書の名前                                      | emqx-tls                                              |
| `ssl.dnsnames`                       | 生成する証明書のDNS名                                  | {}                                                    |
| `ssl.commonName`                     | 生成する証明書の共通名                                 |                                                       |
| `ssl.issuer.name`                    | 証明書生成用のIssuer名                                | letsencrypt-dns                                       |
| `ssl.issuer.kind`                    | 証明書生成用のIssuer種別                              | ClusterIssuer                                         |

### EMQX固有のパラメータ

以下の表は、チャートの設定可能なEMQX固有パラメータとそのデフォルト値を示します。

| パラメータ                                                                                                                                                              | 説明                                                                   | デフォルト値 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------|-------------|
| `emqxConfig`                                                                                                                                                           | [環境変数](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#environment-variables)（`EMQX_`プレフィックスは任意）またはEMQX設定ファイルで使用される名前空間付きドット表記を使って定義された[設定](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html)項目のマップ。 | `nil`       |
| `emqxLicenseSecretName`                                                                                                                                                | ライセンス情報を保持するシークレットの名前（非推奨）                   | `nil`       |
| `emqxLicenseSecretRef.name`                                                                                                                                         | ライセンス情報を保持するシークレットの名前                             | `""`       |
| `emqxLicenseSecretRef.key`                                                                                                                                          | ライセンス情報を保持するシークレットのキー                             | `""`       |

## SSL設定

`cert-manager`を使用する場合、TLS証明書はKubernetesシークレットに標準キーの`tls.crt`および`tls.key`で保存されます。EMQX Helmチャートはこれらの証明書ファイルを自動的にコンテナ内の以下のディレクトリにマウントします：

```
/tmp/ssl/
```

EMQXでSSLサポートを有効にするには、EMQX設定内でファイルパスを明示的に設定する必要があります。これはEMQX設定ファイルを修正するか、以下の環境変数を渡すことで行えます：

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip

既存のTLS証明書（`cert-manager`で生成されたものではない）を使用する場合は、ファイルパスがデプロイ時に実際にマウントされているファイルの場所と一致していることを確認してください。

:::

## Proxy Protocolサポート（HAProxy、Nginx）

Proxy Protocolをサポートするリバースプロキシ（例：HAProxyやNginx）の背後にEMQXをデプロイする場合、以下の環境変数を設定して有効化してください：

```yaml
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

HAProxy Ingress Controllerを使用する場合は、以下のアノテーションを追加してください：

```yaml
haproxy-ingress.github.io/proxy-protocol: "v2"
```

これにより、プロキシを経由して転送される元のクライアントIPアドレスが保持されます。
