<<<<<<< HEAD
# Helmチャートを使用したKubernetes上へのEMQXデプロイ

このページでは、公式Helmチャートを使用してKubernetesクラスターにEMQXをデプロイする手順を段階的に説明します。

公式のEMQX Helmチャートは、StatefulSet、Service、ConfigMap、Ingressルールなどの必要なEMQXコンポーネントをすべてパッケージ化し、単一の設定可能なHelmチャートとしてKubernetesベースのデプロイを簡素化します。

## 前提条件

開始する前に、以下がインストールおよび設定されていることを確認してください：
=======
# Helmチャートを使ったKubernetes上へのEMQXデプロイ

このページでは、公式Helmチャートを使用してKubernetesクラスター上にEMQXをデプロイする手順を段階的に説明します。

公式のEMQX Helmチャートは、StatefulSet、Service、ConfigMap、Ingressルールなど、必要なEMQXコンポーネントをすべてパッケージ化し、単一の設定可能なHelmチャートとして提供することで、Kubernetesベースのデプロイを簡素化します。

## 前提条件

開始する前に、以下がインストールおよび設定されていることを確認してください。
>>>>>>> origin/release-5.9

+ 稼働中のKubernetesクラスター（バージョン1.6以上）
+ [Helm](https://github.com/helm/helm/releases)

## EMQX Helmチャートのインストール

<<<<<<< HEAD
EMQX Helmチャートは、EMQXのGitHubリポジトリから、または公式Helmチャートリポジトリからインストールできます。

### GitHubからのインストール

リリース名を`my-emqx`としてGitHubからチャートをインストールするには、以下を実行します：
=======
EMQX Helmチャートは、EMQXのGitHubリポジトリまたは公式Helmチャートリポジトリからインストールできます。

### GitHubからのインストール

リリース名を `my-emqx` としてGitHubからチャートをインストールするには、以下を実行します。
>>>>>>> origin/release-5.9

```bash
$ git clone https://github.com/emqx/emqx.git
$ cd emqx/deploy/charts/emqx-enterprise
$ helm install my-emqx .
```

### Helmリポジトリからのインストール

<<<<<<< HEAD
リリース名を`my-emqx`として公式Helmチャートリポジトリからチャートをインストールするには、以下を実行します：
=======
リリース名を `my-emqx` として公式Helmチャートリポジトリからチャートをインストールするには、以下を実行します。
>>>>>>> origin/release-5.9

```bash
helm repo add emqx https://repos.emqx.io/charts
helm install my-emqx emqx/emqx-enterprise
```
<<<<<<< HEAD
> 不安定なバージョンをインストールしたい場合は、`--devel`フラグを追加してください：
=======
> 不安定なバージョンをインストールしたい場合は、`--devel` フラグを追加してください：
>>>>>>> origin/release-5.9
>
> ```bash
> helm install my-emqx emqx/emqx-enterprise --devel
> ```

## チャートのアンインストール

<<<<<<< HEAD
`my-emqx`という名前のEMQXリリースを削除し、関連するすべてのKubernetesリソースを削除するには：
=======
`my-emqx` という名前のEMQXリリースを削除し、関連するすべてのKubernetesリソースを削除するには以下を実行します。
>>>>>>> origin/release-5.9

**Helm v3以降の場合**

```bash
$ helm uninstall my-emqx
```

**Helm v2（レガシー）の場合**

```bash
$ helm del my-emqx
```

## 設定パラメータ

<<<<<<< HEAD
EMQX Helmチャートは、`values.yaml`ファイルを通じて幅広い設定パラメータを提供しています。以下の表は主要なパラメータとデフォルト値を示しています。
=======
EMQX Helmチャートは、`values.yaml` ファイルを通じて幅広い設定パラメータを提供しています。以下の表は主要なパラメータとデフォルト値を示しています。
>>>>>>> origin/release-5.9

| パラメータ                            | 説明                                                  | デフォルト値                                           |
| ------------------------------------ | ----------------------------------------------------- | ----------------------------------------------------- |
| `replicaCount`                       | ネットワーク分断時の自動復旧のため、ノード数は奇数を推奨します。 | 3                                                     |
| `image.repository`                   | EMQXイメージ名                                        | emqx/emqx-enterprise                                  |
<<<<<<< HEAD
| `image.pullPolicy`                   | イメージのプルポリシー                                | IfNotPresent                                          |
| `image.pullSecrets`                  | イメージプルシークレット                              | `[]`（デプロイされたポッドにイメージプルシークレットは追加されません） |
| `serviceAccount.create`              | `true`の場合、新しいサービスアカウントを作成します。 | `true`                                                |
| `serviceAccount.name`                | 使用するサービスアカウント名。設定されておらず、かつ`serviceAccount.create`が`true`の場合は、フルネームテンプレートで名前が生成されます。 |                                                       |
| `serviceAccount.annotations`         | サービスアカウントに追加するアノテーション            |                                                       |
| `envFromSecret`                      | 同じKubernetesネームスペース内のシークレット名。環境変数に値を追加します。 | nil                                                   |
| `recreatePods`                       | アップグレード時にポッドの再作成を強制します。最新設定を常に適用したい場合に有用です。 | false                                                 |
| `podAnnotations`                    | ポッドに付与するアノテーション                         | `{}`                                                  |
| `podManagementPolicy`                | 既存のPVCを持つチャートを再デプロイする場合、デッドロックを避けるために`Parallel`に設定する必要があります。 | `Parallel`                                            |
| `persistence.enabled`                | PVCを使用したEMQXのパーシステンスを有効にします。     | false                                                 |
| `persistence.storageClass`           | バックエンドPVCのストレージクラス                      | `nil`（alphaストレージクラスアノテーションを使用）    |
| `persistence.existingClaim`          | EMQXデータ用の既存Persistent Volumeクレーム名。テンプレートとして評価されます。 | ""                                                    |
| `persistence.accessMode`             | EMQXボリューム用PVCのアクセスモード                    | ReadWriteOnce                                        |
| `persistence.size`                   | EMQXボリューム用PVCのストレージ要求サイズ              | 20Mi                                                  |
| `initContainers`                     | EMQXコンテナ作成前に実行されるコンテナ。ユーティリティやセットアップスクリプトを含めることができます。 | `{}`                                                  |
| `resources`                          | CPU/メモリのリソース要求/制限                           | {}                                                    |
| `extraVolumeMounts`                  | デフォルトバックエンドコンテナへの追加のvolumeMounts   | []                                                    |
| `extraVolumes`                       | デフォルトバックエンドポッドへの追加のボリューム       | []                                                    |
| `nodeSelector`                       | ポッド割り当て用のノードラベル                          | `{}`                                                  |
| `tolerations`                        | ポッド割り当て用のトレランスラベル                      | `[]`                                                  |
| `affinity`                           | ノード/ポッドのアフィニティマップ                        | `{}`                                                  |
| `service.type`                       | Kubernetes Serviceのタイプ                               | ClusterIP                                             |
| `service.mqtt`                       | MQTT用ポート                                            | 1883                                                  |
| `service.mqttssl`                    | MQTT(SSL)用ポート                                       | 8883                                                  |
| `service.ws`                         | WebSocket/HTTP用ポート                                  | 8083                                                  |
| `service.wss`                        | WSS/HTTPS用ポート                                      | 8084                                                  |
| `service.dashboard`                  | ダッシュボードおよびAPI用ポート                          | 18083                                                 |
| `service.customPorts`                | Serviceで公開するカスタムポート                          | {}                                                    |
| `service.nodePorts.mqtt`             | MQTT用のKubernetesノードポート                           | nil                                                   |
| `service.nodePorts.mqttssl`          | MQTT(SSL)用のKubernetesノードポート                      | nil                                                   |
| `service.nodePorts.ws`               | WebSocket/HTTP用のKubernetesノードポート                 | nil                                                   |
| `service.nodePorts.wss`              | WSS/HTTPS用のKubernetesノードポート                      | nil                                                   |
| `service.nodePorts.dashboard`        | ダッシュボード用のKubernetesノードポート                 | nil                                                   |
| `service.customNodePorts`            | カスタムポート用のKubernetesノードポート                 | {}                                                    |
| `service.loadBalancerClass`          | このServiceが属するロードバランサーの実装クラス           |                                                       |
| `service.loadBalancerIP`             | Service用のloadBalancerIP                               | nil                                                   |
| `service.loadBalancerSourceRanges`   | LoadBalancerサービスで許可されるアドレス                  | []                                                    |
| `service.externalIPs`                | Service用のExternalIPs                                  | []                                                    |
| `service.externalTrafficPolicy`      | Serviceの外部トラフィックポリシー                         | `Cluster`                                             |
| `service.annotations`                | Service/ServiceMonitorのアノテーション                    | {}（テンプレートとして評価）                           |
| `service.labels`                     | Service/ServiceMonitorのラベル                            | {}（テンプレートとして評価）                           |
| `ingress.dashboard.enabled`          | EMQXダッシュボード用のIngressを有効にする                | false                                                 |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用のIngressクラスを設定                |                                                       |
| `ingress.dashboard.path`             | EMQXダッシュボード用のIngressパス                         | /                                                     |
| `ingress.dashboard.pathType`         | EMQXダッシュボード用のIngress pathType                   | `ImplementationSpecific`                              |
| `ingress.dashboard.hosts`            | EMQXダッシュボード用のIngressホスト                       | dashboard.emqx.local                                  |
| `ingress.dashboard.tls`              | EMQXダッシュボード用のIngress TLS                         | []                                                    |
| `ingress.dashboard.annotations`      | EMQXダッシュボード用のIngressアノテーション               | {}                                                    |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用のIngressクラスを設定                |                                                       |
| `ingress.mqtt.enabled`               | MQTT用のIngressを有効にする                               | false                                                 |
| `ingress.mqtt.ingressClassName`      | MQTT用のIngressクラスを設定                               |                                                       |
| `ingress.mqtt.path`                  | MQTT用のIngressパス                                       | /                                                     |
| `ingress.mqtt.pathType`              | MQTT用のIngress pathType                                  | `ImplementationSpecific`                              |
| `ingress.mqtt.hosts`                 | MQTT用のIngressホスト                                    | mqtt.emqx.local                                       |
| `ingress.mqtt.tls`                   | MQTT用のIngress TLS                                      | []                                                    |
| `ingress.mqtt.annotations`           | MQTT用のIngressアノテーション                             | {}                                                    |
| `ingress.mqtt.ingressClassName`      | MQTT用のIngressクラスを設定                               |                                                       |
| `metrics.enable`                     | `true`に設定すると、[prometheus-operator](https://github.com/prometheus-operator/prometheus-operator)のインストールとemqx_prometheusの有効化が必要です。 | false                                                 |
| `metrics.type`                       | 現在サポートされているのは"prometheus"のみです。           | "prometheus"                                          |
| `ssl.enabled`                        | SSLサポートを有効にする                                   | false                                                 |
| `ssl.useExisting`                    | 既存の証明書を使用するか、cert-managerに生成させるかを指定 | false                                                 |
| `ssl.existingName`                   | 既存証明書の名前                                        | emqx-tls                                              |
| `ssl.dnsnames`                       | 生成される証明書のDNS名                                  | {}                                                    |
| `ssl.commonName`                     | 生成される証明書の共通名                                 |                                                       |
| `ssl.issuer.name`                    | 証明書生成用のIssuer名                                   | letsencrypt-dns                                       |
| `ssl.issuer.kind`                    | 証明書生成用のIssuer種別                                 | ClusterIssuer                                         |
=======
| `image.pullPolicy`                   | イメージプルポリシー                                  | IfNotPresent                                          |
| `image.pullSecrets`                  | イメージプルシークレット                              | `[]`（デプロイされたポッドにイメージプルシークレットを追加しません） |
| `serviceAccount.create`              | `true`の場合、新しいサービスアカウントを作成します。 | `true`                                                |
| `serviceAccount.name`                | 使用するサービスアカウント。設定されておらず、かつ `serviceAccount.create` が `true` の場合、フルネームテンプレートで名前が生成されます。 |                                                       |
| `serviceAccount.annotations`         | サービスアカウントに追加するアノテーション            |                                                       |
| `envFromSecret`                      | 同じKubernetesネームスペース内のシークレット名。環境変数に追加される値を含みます。 | nil                                                   |
| `recreatePods`                       | アップグレード時にポッドの再作成を強制します。最新の設定を常に適用するのに有用です。 | false                                                 |
| `podAnnotations`                    | ポッドに付与するアノテーション                         | `{}`                                                  |
| `podManagementPolicy`                | 既存のPVCを持つチャートを再デプロイする場合、デッドロック回避のため `Parallel` に設定する必要があります。 | `Parallel`                                            |
| `persistence.enabled`                | PVCを使用したEMQXのパーシステンスを有効にします。     | false                                                 |
| `persistence.storageClass`           | バックエンドPVCのストレージクラス                      | `nil`（アルファストレージクラスアノテーションを使用） |
| `persistence.existingClaim`          | EMQXデータ用の既存Persistent Volumeクレーム名。テンプレートとして評価されます。 | ""                                                    |
| `persistence.accessMode`             | EMQXボリュームのPVCアクセスモード                      | ReadWriteOnce                                         |
| `persistence.size`                   | EMQXボリュームのPVCストレージ要求                      | 20Mi                                                  |
| `initContainers`                     | EMQXコンテナ作成前に実行されるコンテナ。ユーティリティやセットアップスクリプトを含めることができます。 | `{}`                                                  |
| `resources`                          | CPU/メモリのリソース要求/制限                          | {}                                                    |
| `extraVolumeMounts`                  | デフォルトのバックエンドコンテナに追加するvolumeMounts | []                                                    |
| `extraVolumes`                       | デフォルトのバックエンドポッドに追加するボリューム    | []                                                    |
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
| `service.loadBalancerClass`          | このServiceが属するロードバランサーの実装クラス         |                                                       |
| `service.loadBalancerIP`             | ServiceのloadBalancerIP                                | nil                                                   |
| `service.loadBalancerSourceRanges`   | LoadBalancerサービスで許可されるアドレス               | []                                                    |
| `service.externalIPs`                | ServiceのExternalIPs                                   | []                                                    |
| `service.externalTrafficPolicy`      | Serviceの外部トラフィックポリシー                       | `Cluster`                                             |
| `service.annotations`                | Service/ServiceMonitorのアノテーション                  | {}（テンプレートとして評価）                           |
| `service.labels`                     | Service/ServiceMonitorのラベル                           | {}（テンプレートとして評価）                           |
| `ingress.dashboard.enabled`          | EMQXダッシュボード用Ingressを有効化                    | false                                                 |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定                |                                                       |
| `ingress.dashboard.path`             | EMQXダッシュボード用Ingressパス                         | /                                                     |
| `ingress.dashboard.pathType`         | EMQXダッシュボード用IngressのpathType                   | `ImplementationSpecific`                              |
| `ingress.dashboard.hosts`            | EMQXダッシュボード用Ingressホスト                       | dashboard.emqx.local                                  |
| `ingress.dashboard.tls`              | EMQXダッシュボード用IngressのTLS設定                    | []                                                    |
| `ingress.dashboard.annotations`      | EMQXダッシュボード用Ingressアノテーション               | {}                                                    |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定                |                                                       |
| `ingress.mqtt.enabled`               | MQTT用Ingressを有効化                                   | false                                                 |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                               |                                                       |
| `ingress.mqtt.path`                  | MQTT用Ingressパス                                      | /                                                     |
| `ingress.mqtt.pathType`              | MQTT用IngressのpathType                                 | `ImplementationSpecific`                              |
| `ingress.mqtt.hosts`                 | MQTT用Ingressホスト                                    | mqtt.emqx.local                                       |
| `ingress.mqtt.tls`                   | MQTT用IngressのTLS設定                                 | []                                                    |
| `ingress.mqtt.annotations`           | MQTT用Ingressアノテーション                             | {}                                                    |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                               |                                                       |
| `metrics.enable`                     | `true`に設定すると、[prometheus-operator](https://github.com/prometheus-operator/prometheus-operator)のインストールとemqx_prometheusの有効化が必要です。 | false                                                 |
| `metrics.type`                       | 現状サポートされているのは "prometheus" のみです。      | "prometheus"                                          |
| `ssl.enabled`                        | SSLサポートを有効化                                    | false                                                 |
| `ssl.useExisting`                    | 既存の証明書を使用するか、cert-managerに生成させるか。   | false                                                 |
| `ssl.existingName`                   | 既存証明書の名前                                     | emqx-tls                                              |
| `ssl.dnsnames`                       | 生成される証明書のDNS名                                | {}                                                    |
| `ssl.commonName`                     | 生成される証明書の共通名                               |                                                       |
| `ssl.issuer.name`                    | 証明書生成用のIssuer名                                | letsencrypt-dns                                       |
| `ssl.issuer.kind`                    | 証明書生成用のIssuer種別                              | ClusterIssuer                                         |
>>>>>>> origin/release-5.9

### EMQX固有のパラメータ

以下の表は、チャートの設定可能なEMQX固有パラメータとそのデフォルト値を示します。

<<<<<<< HEAD
| パラメータ                                                                                                                                                              | 説明                                                                                   | デフォルト値 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------|-------------|
| `emqxConfig`                                                                                                                                                           | [環境変数](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#environment-variables)（`EMQX_`プレフィックスは任意）またはEMQX設定ファイルで使用される名前空間付きドット表記を用いて定義された[設定](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html)項目のマップ。 | `nil`       |
| `emqxLicenseSecretName`                                                                                                                                                | ライセンス情報を保持するシークレット名（非推奨）                                      | `nil`       |
| `emqxLicenseSecretRef.name`                                                                                                                                         | ライセンス情報を保持するシークレット名                                                | `""`       |
| `emqxLicenseSecretRef.key`                                                                                                                                          | ライセンス情報を保持するシークレットのキー                                            | `""`       |

## SSL設定

`cert-manager`を使用する場合、TLS証明書はKubernetesシークレットに標準キー`tls.crt`と`tls.key`で保存されます。EMQX Helmチャートはこれらの証明書ファイルをコンテナ内の以下のディレクトリに自動的にマウントします：
=======
| パラメータ                                                                                                                                                              | 説明                                                                                      | デフォルト値 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|-------------|
| `emqxConfig`                                                                                                                                                           | [設定](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html)項目のマップ。環境変数（`EMQX_`プレフィックスは任意）またはEMQX設定ファイルで使用される名前空間付きドット表記のいずれかで定義します。 | `nil`       |
| `emqxLicenseSecretName`                                                                                                                                                | ライセンス情報を保持するシークレット名（非推奨）                                         | `nil`       |
| `emqxLicenseSecretRef.name`                                                                                                                                         | ライセンス情報を保持するシークレット名                                                   | `""`        |
| `emqxLicenseSecretRef.key`                                                                                                                                          | ライセンス情報を保持するシークレットのキー                                               | `""`        |

## SSL設定

`cert-manager`を使用する場合、TLS証明書はKubernetesシークレットに標準のキー `tls.crt` と `tls.key` で保存されます。EMQX Helmチャートはこれらの証明書ファイルをコンテナ内の以下のディレクトリに自動的にマウントします。
>>>>>>> origin/release-5.9

```
/tmp/ssl/
```

<<<<<<< HEAD
EMQXでSSLサポートを有効にするには、EMQX設定内でファイルパスを明示的に設定する必要があります。これはEMQX設定ファイルを修正するか、以下の環境変数を渡すことで行えます：
=======
EMQXでSSLサポートを有効にするには、EMQX設定内でファイルパスを明示的に設定する必要があります。これは、EMQX設定ファイルを修正するか、以下の環境変数を渡すことで行えます。
>>>>>>> origin/release-5.9

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip

<<<<<<< HEAD
既存のTLS証明書（`cert-manager`で生成されたものではない）を使用する場合は、ファイルパスが実際にマウントされている場所と一致していることを確認してください。
=======
既存のTLS証明書（`cert-manager`による生成ではないもの）を使用する場合は、ファイルパスが実際にマウントされているファイルの場所と一致していることを確認してください。
>>>>>>> origin/release-5.9

:::

## Proxy Protocolサポート（HAProxy、Nginx）

<<<<<<< HEAD
Proxy Protocolをサポートするリバースプロキシ（例：HAProxyやNginx）の背後にEMQXをデプロイする場合、以下の環境変数を設定して有効にします：
=======
Proxy Protocolをサポートするリバースプロキシ（例：HAProxyやNginx）の背後にEMQXをデプロイする場合、以下の環境変数を設定して有効化してください。
>>>>>>> origin/release-5.9

```yaml
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

<<<<<<< HEAD
HAProxy Ingress Controllerを使用する場合は、以下のアノテーションを追加してください：
=======
HAProxy Ingress Controllerを使用する場合は、以下のアノテーションを追加します。
>>>>>>> origin/release-5.9

```yaml
haproxy-ingress.github.io/proxy-protocol: "v2"
```

これにより、プロキシ経由で転送される元のクライアントIPアドレスが保持されます。
