# Prometheusとの統合

EMQXは、SoundCloudが開発したオープンソースの監視ソリューションである[Prometheus](https://prometheus.io/)などのサードパーティ監視システムとの統合をサポートしています。Prometheusは、多次元データモデル、柔軟なクエリ言語（PromQL）、強力なアラート機能を提供します。

サードパーティ監視システムを利用することで、以下のような利点があります。

- EMQXの監視データを他のシステムのデータと統合した完全な監視システムを構築可能です。例えば、サーバーホストの監視情報も取得できます。
- [Grafanaダッシュボード](#use-grafana-to-visualize-EMQX-metrics)を使ってEMQXのメトリクスを可視化するなど、より直感的な監視レポート（図表）を作成できます。
- Prometheus Alertmanagerを利用したアラームルールや通知方法の設定など、多様なアラーム通知オプションを利用できます。

EMQXはPrometheusメトリクス監視の統合に対して、以下の2つの方法をサポートしています。

- **プルモード**：PrometheusがEMQXのREST APIを通じて直接メトリクスを収集します。
- **プッシュモード**：EMQXがPushgatewayサービスにメトリクスをプッシュし、Prometheusがそこからメトリクスを収集します。

Prometheus統合の設定手順は以下の通りです。

1. EMQXダッシュボードの **Management** -> **Monitoring** に移動します。
2. **Integration** タブに切り替えます。
3. 監視プラットフォームとして **Prometheus** を選択します。

選択したモードに応じて、一部の設定オプションはプルモードにのみ適用され、その他は両モードに影響します。詳細な設定手順はダッシュボードの **Help** ボタンをクリックしてご確認ください。

以下のエンドポイントで公開されるメトリクスシリーズ（アラートに適したものを含む）のリファレンスは、[Broker Health Indicators](./broker-health-indicators.md)をご参照ください。

<img src="./assets/enable-push-gateway.png" alt="Pushgatewayを有効化" style="zoom:40%;" />

## Prometheus設定オプション

このセクションでは、ダッシュボードで **Prometheus** を選択した際に利用可能なすべての設定オプションについて説明します。

### 一般オプション（プルモード・プッシュモード両方に影響）

#### レイテンシーバケット

レイテンシー関連メトリクスのヒストグラムバケットの境界値を指定します。

**フォーマット**

カンマ区切りの期間値リスト：

```
10ms, 100ms, 1s, 5s, 30s
```

**説明**

これらの値は、Prometheusでレイテンシーメトリクスをヒストグラムバケットに分類する際の境界を定義します。小さいバケット間隔はより細かい粒度を提供しますが、メトリクスのカーディナリティやストレージ使用量が増加する可能性があります。

この設定はレイテンシーヒストグラムメトリクスの内部生成に影響し、以下に適用されます。

- プルモードメトリクス
- プッシュモードメトリクス（Pushgateway経由）

### プルモード設定

以下のオプションは、PrometheusがREST API経由でEMQXメトリクスをスクレイプするプルモード時にのみ適用されます。

#### Basic Authを有効化

PrometheusのスクレイプAPIに対するHTTP Basic認証を有効または無効にします。

デフォルトでは、PrometheusプルモードAPIは認証不要です。このオプションを有効にすると：

- Prometheusは以下のAPIにアクセスする際にHTTP Basic認証を使用する必要があります：
  - `/api/v5/prometheus/stats`
  - `/api/v5/prometheus/auth`
  - `/api/v5/prometheus/data_integration`
- EMQXで[APIキー](../admin/api.md#authentication)を作成する必要があります。
- `prometheus.yaml`の`basic_auth`セクションを設定します。

このオプションはプルモードにのみ適用され、Pushgateway統合には影響しません。詳細は[プルモード統合の設定](#configure-pull-mode-integration)をご参照ください。

#### ネームスペースデータスクレイピングのレート制限

ネームスペース関連メトリクスのスクレイプリクエストの最大レートを制限します。

ネームスペースレベルのメトリクスはマルチテナント環境でサポートされ、ネームスペース単位で公開または集約可能です。詳細は[Prometheusメトリクスの分離](../multi-tenancy/namespace-overview.md#multi-tenancy-capability-support)をご覧ください。

**フォーマット**：`<リクエスト数>/<期間>`

**例**：`1/5s` は5秒あたり最大1リクエストを許可し、それを超えるリクエストは拒否されます。

**挙動**：

- ネームスペースレベルのメトリクススクレイプリクエストにのみ適用されます。
- 特定のネームスペースを対象としたリクエストは制限されません。
- プルモードにのみ適用されます。

大規模またはマルチネームスペース環境での過負荷防止に役立ちます。

### プッシュモード設定

プッシュモードでは、EMQXがPushgatewayインスタンスにメトリクスを送信します。デフォルトではプッシュモードは無効です。

#### Pushgatewayを有効化

Pushgatewayへのメトリクスプッシュを有効または無効にします。有効にした場合、以下の項目を設定してください。

#### プッシュ間隔

EMQXがPushgatewayにメトリクスをプッシュする間隔を指定します。デフォルトは`15`秒です。

#### Pushgatewayサーバー

PushgatewayサーバーのURLを指定します。デフォルトは`http://127.0.0.1:9091`です。

#### ジョブ名

Pushgatewayにメトリクスをプッシュする際に使用するジョブラベルを指定します。

EMQXのノード名やホスト名から派生した変数を使ってジョブラベルを構築可能です。デフォルト値は`${name}/instance/${name}~${host}`です。

**変数**：

- `${name}`：EMQXノード名（例：`emqx`）
- `${host}`：ホストIPアドレス（例：`127.0.0.1`）

例えば、ノード名が`emqx@127.0.0.1`の場合：

- `${name}` = `emqx`
- `${host}` = `127.0.0.1`

#### ヘッダー

Pushgatewayにメトリクスをプッシュする際に送信する任意のHTTPヘッダーです。

値の型は文字列で、以下のようにキーと値のペアで設定できます。

```
Authorization = "some-auth-token"
```

追加のヘッダーは **Add** ボタンで挿入可能です。

## プルモード統合の設定

プルモードでは、PrometheusがREST APIを通じてEMQXからメトリクスをスクレイプします。

EMQXは以下のエンドポイントを提供しています。

- `/api/v5/prometheus/stats`：EMQXの基本メトリクスとカウンター。
- `/api/v5/prometheus/auth`：認証・認可を含むアクセス制御に関する主要メトリクスとカウンター。
- `/api/v5/prometheus/data_integration`：ルールエンジン、コネクター、アクション、Sink/Source、エンコード/デコードに関連するメトリクスとカウンター。

### メトリクス収集モード

上記APIを呼び出す際にURLクエリパラメータ`mode`を指定することで、異なるタイプのメトリクスデータを取得できます。各パラメータの意味は以下の通りです。

:::: tabs type: card

::: tab シングルノードモード

```
mode=node
```

デフォルトモードで、リクエストを受けた現在のノードのメトリクスを返します。特に指定しない場合、このモードが適用されます。

:::

::: tab クラスター集約モード

```
mode=all_nodes_aggregated
```

クラスター内のすべての稼働ノードのメトリクスを集約し、算術和または論理和を返します。

- 「オン状態」や「稼働状態」などのメトリクスは論理和で返されます。すべてのノードがオンまたは稼働中なら1、それ以外は0を返します。
- CPUやメモリ使用率のようにノードごとに独立したメトリクスは集約値を返さず、ノード名をラベルに付与して区別します。例：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター内のどのノードでも値が一貫しているメトリクスは、APIリクエストを受けたノードの値を直接返します。これらは集約されず、ノード名ラベルも付きません。例：

  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスは算術和を返します。すなわち、すべてのノードのメトリクスの合計値です。

:::

::: tab クラスター非集約モード

```
mode=all_nodes_unaggregated
```

クラスター内のすべての稼働ノードの個別メトリクスを返します。

- ノード名をラベルに付与して、異なるノードのメトリクスを区別します。例：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- クラスター内のどのノードでも値が一貫しているメトリクス（例：ブラックリスト数、保持メッセージ数など）は、APIリクエストを受けたノードの値を直接返し、ノード名ラベルは付きません。例：

  ```bash
  emqx_retained_count 3
  ```

:::

::::

Prometheusプルエンドポイントの詳細は、[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご参照ください。

### 認証（任意）

デフォルトでは、PrometheusプルモードAPIは認証不要です。

EMQXダッシュボードで **Basic Authを有効化** をオンにした場合、PrometheusはHTTP Basic認証で認証する必要があります。

この場合の手順は以下の通りです。

1. EMQXで[APIキー](../admin/api.md#authentication)を作成します。
2. Prometheus設定で作成したAPIキーとシークレットキーを使用します。

Prometheus設定例：

```yaml
basic_auth:
  username: '<API_KEY>'
  password: '<SECRET_KEY>'
```

- `username` はAPIキー
- `password` は対応するシークレットキー

Prometheusはこれらの認証情報を使用してEMQXメトリクスをスクレイプします。

### Prometheusサーバー設定例

PrometheusがEMQXメトリクスをスクレイプできるように、Prometheusサーバーの設定ファイルに以下を追加し、サービスを再起動してください。

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # デフォルトのスクレイプ間隔は10秒
  evaluation_interval: 10s # デフォルトの評価間隔は10秒
  # このマシン上のすべての時系列がデフォルトでエクスポートされます
  external_labels:
    monitor: 'emqx-monitor'
scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''
```

## プッシュモード統合の設定

プッシュモードでは、EMQXからPushgatewayへメトリクスを送信します。

ダッシュボードで **Pushgatewayを有効化** をオンにし、必要な項目を設定したら、**Save Changes** をクリックしてください。

プッシュモードは現時点で、`/api/v5/prometheus/stats` エンドポイントの基本メトリクスとカウンターのみを含みます。包括的な監視には通常、プルモードの利用が推奨されます。

### 設定ファイル例

設定ファイルに以下を追加してPushgatewayを有効化・設定することも可能です。設定項目の詳細は[Configuration - Prometheus](../configuration/prometheus.md)をご参照ください。

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

## Grafanaを使ってEMQXメトリクスを可視化する

GrafanaとPrometheusを組み合わせてEMQXメトリクスを可視化することも可能です。GrafanaにEMQXのテンプレートファイルをインポートすることで実現できます。テンプレートのダウンロードは、[EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) をクリックするか、**Monitoring** ページの **Integration** タブ下部の **Help** ボタンをクリックしてください。

::: tip

詳細な操作手順は、[Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana) をご参照ください。

:::
