# Kubernetes上でのHelmチャートを使用したEMQXのデプロイ

このページでは、公式Helmチャートを使用してKubernetesクラスター上にEMQXをデプロイする手順を段階的に説明します。

公式のEMQX Helmチャートは、StatefulSet、Service、ConfigMap、Ingressルール、Gateway APIルートなど、必要なEMQXコンポーネントをすべてパッケージ化し、単一の設定可能なHelmチャートとして提供することで、Kubernetesベースのデプロイを簡素化します。

## 前提条件

開始する前に、以下がインストールおよび設定されていることを確認してください：

+ 稼働中のKubernetesクラスター（バージョン1.6以上）
+ [Helm](https://github.com/helm/helm/releases)

## EMQX Helmチャートのインストール

EMQX Helmチャートは、EMQXのGitHubリポジトリまたは公式Helmチャートリポジトリのいずれかからインストールできます。

### GitHubからのインストール

GitHubからリリース名 `my-emqx` でチャートをインストールするには：

```bash
$ git clone https://github.com/emqx/emqx.git
$ cd emqx/deploy/charts/emqx-enterprise
$ helm install my-emqx .
```

### Helmリポジトリからのインストール

公式Helmチャートリポジトリからリリース名 `my-emqx` でチャートをインストールするには：

```bash
helm repo add emqx https://repos.emqx.io/charts
helm install my-emqx emqx/emqx-enterprise
```
> 安定版以外のバージョンをインストールしたい場合は、`--devel` フラグを追加してください：
>
> ```bash
> helm install my-emqx emqx/emqx-enterprise --devel
> ```

## チャートのアンインストール

`my-emqx` という名前のEMQXリリースを削除し、関連するすべてのKubernetesリソースを削除するには：

**Helm v3以降の場合**

```bash
$ helm uninstall my-emqx
```

**Helm v2（レガシー）の場合**

```bash
$ helm del my-emqx
```

## 設定パラメータ

EMQX Helmチャートは、`values.yaml` ファイルを通じて幅広い設定パラメータを提供します。以下の表は主なパラメータとデフォルト値を示しています。

| パラメータ                            | 説明                                                  | デフォルト値                                           |
| ------------------------------------ | ----------------------------------------------------- | ----------------------------------------------------- |
| `replicaCount`                       | ネットワーク分断時の自動復旧のため、ノード数は奇数を推奨します。 | 3                                                     |
| `image.repository`                   | EMQXイメージ名                                        | emqx/emqx-enterprise                                  |
| `image.pullPolicy`                   | イメージのプルポリシー                                | IfNotPresent                                          |
| `image.pullSecrets`                  | イメージプルシークレット                              | `[]`（デプロイされたポッドにイメージプルシークレットを追加しません） |
| `serviceAccount.create`              | `true` の場合、新しいサービスアカウントを作成します。 | `true`                                                |
| `serviceAccount.name`                | 使用するサービスアカウント。未設定かつ `serviceAccount.create` が `true` の場合、フルネームテンプレートで名前が生成されます。 |                                                       |
| `serviceAccount.annotations`         | サービスアカウントに追加するアノテーション            |                                                       |
| `envFromSecret`                      | 同じKubernetesネームスペース内のシークレット名。環境変数として値が追加されます。 | nil                                                   |
| `recreatePods`                       | アップグレード時にポッドの再作成を強制し、常に最新設定を適用するのに役立ちます。 | false                                                 |
| `podAnnotations`                    | ポッドに付与するアノテーション                         | `{}`                                                  |
| `podManagementPolicy`                | 既存のPVCを持つチャートを再デプロイする場合は、デッドロックを避けるために `Parallel` に設定してください。 | `Parallel`                                            |
| `persistence.enabled`                | PVCを使用したEMQXのパーシステンスを有効にします。     | false                                                 |
| `persistence.storageClass`           | バックエンドPVCのストレージクラス                      | `nil`（alphaストレージクラスアノテーションを使用）     |
| `persistence.existingClaim`          | EMQXデータ用の既存Persistent Volumeクレーム名。テンプレートとして評価されます。 | ""                                                    |
| `persistence.accessMode`             | EMQXボリューム用PVCのアクセスモード                   | ReadWriteOnce                                         |
| `persistence.size`                   | EMQXボリューム用PVCのストレージ要求サイズ             | 20Mi                                                  |
| `initContainers`                     | EMQXコンテナ作成前に実行されるコンテナ。ユーティリティやセットアップスクリプトを含めることができます。 | `{}`                                                  |
| `resources`                          | CPU/メモリのリソース要求/制限                          | {}                                                    |
| `extraVolumeMounts`                  | デフォルトのバックエンドコンテナに追加するvolumeMounts | []                                                    |
| `extraVolumes`                       | デフォルトのバックエンドポッドに追加するボリューム     | []                                                    |
| `nodeSelector`                       | ポッド割り当て用のノードラベル                         | `{}`                                                  |
| `tolerations`                        | ポッド割り当て用のトレランス                           | `[]`                                                  |
| `affinity`                           | ノード/ポッドのアフィニティマップ                       | `{}`                                                  |
| `service.type`                       | Kubernetes Serviceのタイプ                             | ClusterIP                                             |
| `service.mqtt`                       | MQTT用ポート                                          | 1883                                                  |
| `service.mqttssl`                    | MQTT(SSL)用ポート                                     | 8883                                                  |
| `service.ws`                         | WebSocket/HTTP用ポート                                | 8083                                                  |
| `service.wss`                        | WSS/HTTPS用ポート                                    | 8084                                                  |
| `service.wsEnabled`                  | ServiceでWebSocketおよびWSSポートを公開します。`httpRoute.ws.enabled` または `tlsRoute.wss.enabled` を有効にする場合は必ず `true` にしてください。 | true                                                  |
| `service.dashboard`                  | ダッシュボードおよびAPI用ポート                       | 18083                                                 |
| `service.customPorts`                | Serviceで公開するカスタムポート                       | {}                                                    |
| `service.nodePorts.mqtt`             | MQTT用Kubernetesノードポート                          | nil                                                   |
| `service.nodePorts.mqttssl`          | MQTT(SSL)用Kubernetesノードポート                     | nil                                                   |
| `service.nodePorts.ws`               | WebSocket/HTTP用Kubernetesノードポート                | nil                                                   |
| `service.nodePorts.wss`              | WSS/HTTPS用Kubernetesノードポート                     | nil                                                   |
| `service.nodePorts.dashboard`        | ダッシュボード用Kubernetesノードポート                | nil                                                   |
| `service.customNodePorts`            | カスタムポート用Kubernetesノードポート                | {}                                                    |
| `service.loadBalancerClass`          | このServiceが属するロードバランサー実装               |                                                       |
| `service.loadBalancerIP`             | ServiceのloadBalancerIP                                | nil                                                   |
| `service.loadBalancerSourceRanges`   | LoadBalancerサービスで許可されるアドレス              | []                                                    |
| `service.externalIPs`                | ServiceのExternalIPs                                   | []                                                    |
| `service.externalTrafficPolicy`      | ServiceのExternal Traffic Policy                       | `Cluster`                                             |
| `service.annotations`                | Service/ServiceMonitorのアノテーション                 | {}（テンプレートとして評価）                           |
| `service.labels`                     | Service/ServiceMonitorのラベル                          | {}（テンプレートとして評価）                           |
| `ingress.dashboard.enabled`          | EMQXダッシュボード用のIngressを有効化                  | false                                                 |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定               |                                                       |
| `ingress.dashboard.path`             | EMQXダッシュボード用Ingressパス                        | /                                                     |
| `ingress.dashboard.pathType`         | EMQXダッシュボード用IngressのpathType                  | `ImplementationSpecific`                              |
| `ingress.dashboard.hosts`            | EMQXダッシュボード用Ingressホスト                      | dashboard.emqx.local                                  |
| `ingress.dashboard.tls`              | EMQXダッシュボード用Ingress TLS                         | []                                                    |
| `ingress.dashboard.annotations`      | EMQXダッシュボード用Ingressアノテーション               | {}                                                    |
| `ingress.dashboard.ingressClassName` | EMQXダッシュボード用Ingressクラスを設定               |                                                       |
| `ingress.mqtt.enabled`               | MQTT用Ingressを有効化                                  | false                                                 |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                              |                                                       |
| `ingress.mqtt.path`                  | MQTT用Ingressパス                                     | /                                                     |
| `ingress.mqtt.pathType`              | MQTT用IngressのpathType                               | `ImplementationSpecific`                              |
| `ingress.mqtt.hosts`                 | MQTT用Ingressホスト                                   | mqtt.emqx.local                                       |
| `ingress.mqtt.tls`                   | MQTT用Ingress TLS                                     | []                                                    |
| `ingress.mqtt.annotations`           | MQTT用Ingressアノテーション                            | {}                                                    |
| `ingress.mqtt.ingressClassName`      | MQTT用Ingressクラスを設定                              |                                                       |
| `metrics.enable`                     | `true`の場合、[prometheus-operator](https://github.com/prometheus-operator/prometheus-operator) のインストールとemqx_prometheusの有効化が必要です。 | false                                                 |
| `metrics.type`                       | 現在サポートされているのは "prometheus" のみです。    | "prometheus"                                          |
| `ssl.enabled`                        | SSLサポートを有効化                                   | false                                                 |
| `ssl.useExisting`                    | 既存の証明書を使用するか、cert-managerに生成させるか    | false                                                 |
| `ssl.existingName`                   | 既存証明書の名前                                     | emqx-tls                                              |
| `ssl.dnsnames`                       | 生成される証明書のDNS名                               | {}                                                    |
| `ssl.commonName`                     | 生成される証明書の共通名                              |                                                       |
| `ssl.issuer.name`                    | 証明書生成用のIssuer名                                | letsencrypt-dns                                       |
| `ssl.issuer.kind`                    | 証明書生成用のIssuer種別                              | ClusterIssuer                                         |

### EMQX固有のパラメータ

以下の表は、チャートの設定可能なEMQX固有パラメータとそのデフォルト値を示しています。

| パラメータ                                                                                                                                                              | 説明                                                                   | デフォルト値 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------|--------------|
| `emqxConfig`                                                                                                                                                           | [環境変数](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#environment-variables)（`EMQX_`プレフィックスは任意）またはEMQX設定ファイルで使用される名前空間付きドット表記を用いて定義された[設定](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html)項目のマップ。 | `nil`        |
| `emqxLicenseSecretName`                                                                                                                                                | ライセンス情報を保持するシークレットの名前（非推奨）                   | `nil`        |
| `emqxLicenseSecretRef.name`                                                                                                                                         | ライセンス情報を保持するシークレットの名前                             | `""`         |
| `emqxLicenseSecretRef.key`                                                                                                                                          | ライセンス情報を保持するシークレットのキー                             | `""`         |

## フィーチャーゲートの設定

EMQX 6.3.0以降、`EMQX_FEATURES` を設定して起動時に利用可能なオプション機能を制御できます。例：

```yaml
emqxConfig:
  EMQX_FEATURES: "dashboard,metrics,plugins"
```

フィーチャーゲートはEMQX起動時にのみ解決されます。この値を変更した場合は、EMQXポッドを再作成または再起動してください。全フィーチャーリストと依存関係の詳細は[フィーチャーゲート](../feature-gates.md)を参照してください。

## Gateway APIルートの設定

EMQX 6.3以降、EMQX Enterprise HelmチャートはIngressリソースの代替としてKubernetes Gateway APIルートを作成できます。`HTTPRoute` はEMQXダッシュボードとWebSocket経由のMQTTを公開し、`TLSRoute` はTLSパススルーによるMQTTSおよびWSSを公開します。

ルートを有効化する前に、以下の前提条件を満たしてください：

- [Gateway APIコントローラーおよびそのカスタムリソース定義（CRD）](https://gateway-api.sigs.k8s.io/guides/getting-started/introduction/)をインストールする。
- `tlsRoute.mqtts` または `tlsRoute.wss` を有効にするには、Kubernetes 1.31以降およびGateway API standard-channel CRDバージョン1.5.0以降が必要です。Gateway APIコントローラーはパススルーモードの `TLSRoute` をサポートしている必要があります。
- `httpRoute.ws` を有効にするには、`ServicePort.appProtocol` を必要としないHTTPRoute WebSocketトラフィックをサポートするGateway APIコントローラーを使用してください。EMQX HelmチャートはWebSocket用Serviceポートに `appProtocol` を設定しません。
- ルートに対応するリスナーを持つGatewayを作成する。
- `tlsRoute.mqtts` と `tlsRoute.wss` で使用されるGateway TLSリスナーを `tls.mode: Passthrough` に設定する。EMQXがTLS接続を終了します。

すべてのルートはデフォルトで無効です。有効にするルートごとに `parentRefs` を設定し、ルートをGatewayにアタッチしてください。以下はすべてのサポートされるルートを有効にする `values.yaml` の例です：

```yaml
service:
  wsEnabled: true

httpRoute:
  dashboard:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: https
    hostnames:
      - dashboard.emqx.local
    path: /
    pathType: PathPrefix
  ws:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: https
    hostnames:
      - ws.emqx.local
    path: /mqtt
    pathType: PathPrefix

tlsRoute:
  mqtts:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: mqtts
    hostnames:
      - mqtt.emqx.local
  wss:
    enabled: true
    parentRefs:
      - name: emqx-gateway
        namespace: default
        sectionName: wss
    hostnames:
      - wss.emqx.local
```

Gateway名、ネームスペース、リスナーセクション名、およびホスト名は環境に合わせて置き換えてください。GatewayとHelmリリースが異なるネームスペースにある場合は、参照される各Gatewayリスナーで `allowedRoutes` を設定し、Helmリリースのネームスペースからのルートを許可してください。チャートは以下のルートを作成します：

| ルート | バックエンドサービスポート | デフォルトパス |
| --- | --- | --- |
| `httpRoute.dashboard` | ダッシュボードおよびAPI（ポート `18083`） | `/` |
| `httpRoute.ws` | WebSocket経由のMQTT（ポート `8083`） | `/mqtt` |
| `tlsRoute.mqtts` | MQTTS（ポート `8883`） | 該当なし |
| `tlsRoute.wss` | WSS（ポート `8084`） | 該当なし |

::: warning 重要なお知らせ

`httpRoute.ws` または `tlsRoute.wss` を有効にする場合は、必ず `service.wsEnabled` を `true` に設定してください。そうしないと、Helmは `httpRoute.ws.enabled requires service.wsEnabled=true` または `tlsRoute.wss.enabled requires service.wsEnabled=true` のエラーでチャートのレンダリングを停止します。

:::

リリースのインストールまたはアップグレード後、ルートのステータスを確認してください：

```bash
kubectl get httproute,tlsroute -o yaml
```

アタッチされた各ルートについて、`status.parents` 内の `Accepted` および `ResolvedRefs` 条件が `True` であることを確認してください。コントローラーが `Programmed` 条件を報告している場合は、それも `True` であることを確認してください。

ルート条件はエンドツーエンドのトラフィックを検証しません。設定したダッシュボードまたはWebSocketのホスト名にリクエストを送信するか、Gateway経由でMQTTSまたはWSS接続を確立し、ルートがEMQXへのトラフィック転送を正しく行えることを確認してください。

Gateway APIルートのパラメータは以下の通りです：

| パラメータ | 説明 | デフォルト値 |
| --- | --- | --- |
| `httpRoute.<route>.enabled` | `dashboard` または `ws` のHTTPRouteを作成します。 | `false` |
| `httpRoute.<route>.annotations` | HTTPRouteに追加するアノテーション。 | `{}` |
| `httpRoute.<route>.labels` | HTTPRouteに追加するラベル。 | `{}` |
| `httpRoute.<route>.parentRefs` | 親Gatewayおよびリスナーへの参照。 | `[]` |
| `httpRoute.<route>.hostnames` | ルートがマッチするホスト名。 | `dashboard.emqx.local`（dashboard）、`ws.emqx.local`（ws） |
| `httpRoute.<route>.path` | ルートがマッチするパス。 | `/`（dashboard）、`/mqtt`（ws） |
| `httpRoute.<route>.pathType` | パスマッチのタイプ。 | `PathPrefix` |
| `tlsRoute.<route>.enabled` | `mqtts` または `wss` のTLSRouteを作成します。 | `false` |
| `tlsRoute.<route>.annotations` | TLSRouteに追加するアノテーション。 | `{}` |
| `tlsRoute.<route>.labels` | TLSRouteに追加するラベル。 | `{}` |
| `tlsRoute.<route>.parentRefs` | 親GatewayおよびTLSリスナーへの参照。 | `[]` |
| `tlsRoute.<route>.hostnames` | ルートがマッチする必須のSNIホスト名。少なくとも1つの有効なFQDNを含む必要があります。 | `mqtt.emqx.local`（mqtts）、`wss.emqx.local`（wss） |

## SSL設定

`cert-manager` を使用する場合、TLS証明書はKubernetesシークレットに標準キー `tls.crt` と `tls.key` で格納されます。EMQX Helmチャートはこれらの証明書ファイルをコンテナ内の以下のディレクトリに自動的にマウントします：

```
/tmp/ssl/
```

EMQXでSSLサポートを有効にするには、EMQX設定内でファイルパスを明示的に設定する必要があります。これはEMQX設定ファイルを修正するか、以下の環境変数を渡すことで行えます：

```yaml
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CERTFILE: /tmp/ssl/tls.crt
EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__KEYFILE: /tmp/ssl/tls.key
```

::: tip

既存のTLS証明書を使用する場合（`cert-manager`による生成ではない場合）、ファイルパスが実際のマウント先と一致していることを確認してください。

:::

## Proxy Protocolサポート（HAProxy、Nginx）

Proxy Protocolをサポートするリバースプロキシ（例：HAProxyやNginx）の背後にEMQXをデプロイする場合、以下の環境変数を設定して有効にしてください：

```yaml
EMQX_LISTENERS__TCP__DEFAULT__PROXY_PROTOCOL: "true"
```

HAProxy Ingress Controllerを使用する場合は、以下のアノテーションを追加してください：

```yaml
haproxy-ingress.github.io/proxy-protocol: "v2"
```

これにより、プロキシを通過する元のクライアントIPアドレスが保持されます。
