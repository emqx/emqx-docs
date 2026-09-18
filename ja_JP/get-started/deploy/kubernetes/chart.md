# Kubernetes上でのEMQXのHelmチャートによるデプロイ

このページでは、公式Helmチャートを使用してKubernetesクラスター上にEMQXをデプロイする手順を段階的に説明します。

公式のEMQX Helmチャートは、StatefulSet、Service、ConfigMap、Ingressルールなど、必要なEMQXコンポーネントをすべてパッケージ化し、単一の設定可能なHelmチャートとして提供することで、Kubernetesベースのデプロイを簡素化します。

## 前提条件

開始する前に、以下がインストールおよび設定されていることを確認してください：

+ 稼働中のKubernetesクラスター（バージョン1.6以上）
+ [Helm](https://github.com/helm/helm/releases)

## EMQX Helmチャートのインストール

EMQX Helmチャートは、EMQXのGitHubリポジトリまたは公式Helmチャートリポジトリからインストールできます。

### GitHubからのインストール

GitHubからリリース名 `my-emqx` でチャートをインストールするには、以下を実行します：

```bash
$ git clone https://github.com/emqx/emqx.git
$ cd emqx/deploy/charts/emqx-enterprise
$ helm install my-emqx .
```

### Helmリポジトリからのインストール

公式Helmチャートリポジトリからリリース名 `my-emqx` でチャートをインストールするには、以下を実行します：

```bash
helm repo add emqx https://repos.emqx.io/charts
helm install my-emqx emqx/emqx-enterprise
```
> 不安定なバージョンをインストールしたい場合は、`--devel` フラグを追加してください：
>
> ```bash
> helm install my-emqx emqx/emqx-enterprise --devel
> ```

## チャートのアンインストール

リリース名 `my-emqx` のEMQXを削除し、関連するすべてのKubernetesリソースを削除するには以下を実行します：

**Helm v3以降の場合**

```bash
$ helm uninstall my-emqx
```

**Helm v2（レガシー）の場合**

```bash
$ helm del my-emqx
```

## 設定パラメータ

EMQX Helmチャートは、`values.yaml` ファイルを通じて幅広い設定パラメータを提供しています。以下の表は主要なパラメータとデフォルト値を示しています。

| パラメータ                            | 説明                                                  | デフォルト値                                           |
| ------------------------------------ | ----------------------------------------------------- | ----------------------------------------------------- |
| `replicaCount`                       | ネットワーク分断時の自動回復のため、ノード数は奇数を推奨 | 3                                                     |
| `image.repository`                   | EMQXイメージ名                                        | emqx/emqx-enterprise                                  |
| `image.pullPolicy`                   | イメージのプルポリシー                                | IfNotPresent                                          |
| `image.pullSecrets`                  | イメージプルシークレット                              | `[]`（デプロイされたポッドにプルシークレットは追加されません） |
| `serviceAccount.create`              | `true`の場合、新しいサービスアカウントを作成         | `true`                                                |
| `serviceAccount.name`                | 使用するサービスアカウント名。未設定かつ`serviceAccount.create`が`true`の場合はフルネームテンプレートで生成されます |                                                       |
| `serviceAccount.annotations`         | サービスアカウントに追加するアノテーション             |                                                       |
| `envFromSecret`                      | 同じKubernetesネームスペース内のシークレット名。環境変数として値を追加 | nil                                                   |
| `recreatePods`                       | アップグレード時にポッドを再作成し、最新設定を常に適用 | false                                                 |
| `podAnnotations`                    | ポッドのアノテーション                                | `{}`                                                  |
| `podManagementPolicy`                | 既存のPVCを持つチャートを再デプロイする場合、デッドロック回避のため`Parallel`に設定 | `Parallel`                                            |
| `persistence.enabled`                | PVCを使用したEMQXのパーシステンスを有効化             | false                                                 |
| `persistence.storageClass`           | バックエンドPVCのストレージクラス                      | `nil`（alphaストレージクラスアノテーションを使用）     |
| `persistence.existingClaim`          | EMQXデータ用Persistent Volumeの既存クレーム名（テンプレート評価） | ""                                                    |
| `persistence.accessMode`             | EMQXボリュームのPVCアクセスモード                      | ReadWriteOnce                                         |
| `persistence.size`                   | EMQXボリュームのPVCストレージ要求                      | 20Mi                                                  |
| `initContainers`                     | EMQXコンテナ作成前に実行されるコンテナ。ユーティリティやセットアップスクリプトを含むことが可能 | `{}`                                                  |
| `resources`                          | CPU/メモリのリソース要求/制限                          | {}                                                    |
| `extraVolumeMounts`                  | デフォルトバックエンドコンテナへの追加のvolumeMounts  | []                                                    |
| `extraVolumes`                       | デフォルトバックエンドポッドへの追加ボリューム         | []                                                    |
| `nodeSelector`                       | ポッド割り当て用のノードラベル                         | `{}`                                                  |
| `tolerations`                        | ポッド割り当て用のトレランスラベル                     | `[]`                                                  |
| `affinity`                           | ノード/ポッドのアフィニティマップ                       | `{}`                                                  |
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
| `service.loadBalancerClass`          | このServiceが属するロードバランサー実装                 |                                                       |
| `service.loadBalancerIP`             | ServiceのloadBalancerIP                                | nil                                                   |
| `service.loadBalancerSourceRanges`   | LoadBalancerサービスで許可されるアドレス               | []                                                    |
| `service.externalIPs`                | ServiceのExternalIPs                                   | []                                                    |
| `service.externalTrafficPolicy`      | Serviceの外部トラフィックポリシー                       | `Cluster`                                             |
| `service.annotations`                | Service/ServiceMonitorのアノテーション                   | {}（テンプレート評価）                                 |
| `service.labels`                     | Service/ServiceMonitorのラベル                          | {}（テンプレート評価）                                 |
| `ingress.dashboard.enabled`          | EMQXダッシュボード用Ingressの有効化                     | false                                                 |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスの設定                 |                                                       |
| `ingress.dashboard.path`             | EMQXダッシュボード用Ingressパス                         | /                                                     |
| `ingress.dashboard.pathType`         | EMQXダッシュボード用Ingress pathType                   | `ImplementationSpecific`                              |
| `ingress.dashboard.hosts`            | EMQXダッシュボード用Ingressホスト                       | dashboard.emqx.local                                  |
| `ingress.dashboard.tls`              | EMQXダッシュボード用Ingress TLS設定                     | []                                                    |
| `ingress.dashboard.annotations`      | EMQXダッシュボード用Ingressアノテーション               | {}                                                    |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスの設定                 |                                                       |
| `ingress.mqtt.enabled`               | MQTT用Ingressの有効化                                   | false                                                 |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスの設定                               |                                                       |
| `ingress.mqtt.path`                  | MQTT用Ingressパス                                      | /                                                     |
| `ingress.mqtt.pathType`              | MQTT用Ingress pathType                                  | `ImplementationSpecific`                              |
| `ingress.mqtt.hosts`                 | MQTT用Ingressホスト                                    | mqtt.emqx.local                                       |
| `ingress.mqtt.tls`                   | MQTT用Ingress TLS設定                                  | []                                                    |
| `ingress.mqtt.annotations`           | MQTT用Ingressアノテーション                             | {}                                                    |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスの設定                               |                                                       |
| `metrics.enable`                     | `true`に設定すると、[prometheus-operator](https://github.com/prometheus-operator/prometheus-operator)のインストールとemqx_prometheusの有効化が必要 | false                                                 |
| `metrics.type`                       | 現在サポートされているのは "prometheus" のみ            | "prometheus"                                          |
| `ssl.enabled`                        | SSLサポートの有効化                                    | false                                                 |
| `ssl.useExisting`                    | 既存の証明書を使用するか、cert-managerに生成させるか      | false                                                 |
| `ssl.existingName`                   | 既存証明書の名前                                      | emqx-tls                                              |
| `ssl.dnsnames`                       | 生成される証明書のDNS名                                | {}                                                    |
| `ssl.commonName`                     | 生成される証明書の共通名                               |                                                       |
| `ssl.issuer.name`                    | 証明書生成用のIssuer名                                 | letsencrypt-dns                                       |
| `ssl.issuer.kind`                    | 証明書生成用のIssuer種別                               | ClusterIssuer                                         |

### EMQX固有のパラメータ

以下の表は、チャートのEMQX固有の設定可能なパラメータとそのデフォルト値を示します。

| パラメータ                                                                                                                                                              | 説明                                                                   | デフォルト値 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|-------------|
| `emqxConfig`                                                                                                                                                           | [設定](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html)項目のマップ。環境変数（`EMQX_`プレフィックスは任意）またはEMQX設定ファイルで使われる名前空間付きドット表記のいずれかで定義可能。 | `nil`       |
| `emqxLicenseSecretName`                                                                                                                                                | ライセンス情報を保持するシークレット名（非推奨）                      | `nil`       |
| `emqxLicenseSecretRef.name`                                                                                                                                         | ライセンス情報を保持するシークレット名                                | `""`        |
| `emqxLicenseSecretRef.key`                                                                                                                                          | ライセンス情報を保持するシークレットのキー                            | `""`        |

## SSL設定

`cert-manager`を使用する場合、TLS証明書はKubernetesシークレットに標準キー `tls.crt` と `tls.key` で保存されます。EMQX Helmチャートはこれらの証明書ファイルをコンテナ内の以下のディレクトリに自動的にマウントします：

```
/tmp/ssl/
```

EMQXでSSLサポートを有効にするには、EMQX設定ファイルを修正するか、以下の環境変数を渡してファイルパスを明示的に設定する必要があります：

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip

既存のTLS証明書を使用する場合（`cert-manager`で生成されたものではない場合）、ファイルパスが実際にマウントされている場所と一致していることを確認してください。

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

これにより、プロキシを通じて転送される元のクライアントIPアドレスが保持されます。
