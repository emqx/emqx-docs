# Prometheusとの統合

EMQXは、SoundCloudで開発されたオープンソースの監視ソリューションである[Prometheus](https://prometheus.io/)などのサードパーティ監視システムとの統合をサポートしています。Prometheusは多次元データモデル、柔軟なクエリ言語（PromQL）、強力なアラート機能を提供します。

サードパーティ監視システムを利用することで、以下の利点があります。

- EMQXの監視データを他のシステムの監視データと統合した完全な監視システムを構築できます。例えば、サーバーホストの監視情報も取得可能です。
- [Grafanaダッシュボード](#use-grafana-to-visualize-EMQX-metrics)を使ったEMQXメトリクスの可視化など、より直感的な監視レポート（図表）を作成できます。
- Prometheus Alertmanagerを利用したアラームルールや通知方法の設定など、多様なアラーム通知オプションを利用できます。

EMQXはPrometheusメトリクス監視の統合に対して、以下の2つの方法をサポートしています。

- **Pullモード**：PrometheusがEMQXのREST APIを通じて直接メトリクスを収集します。
- **Pushモード**：EMQXがPushgatewayサービスにメトリクスをプッシュし、Prometheusがそこからメトリクスを収集します。

Prometheus統合の設定手順は以下の通りです。

1. EMQXダッシュボードの **Management** -> **Monitoring** に移動します。
2. **Integration** タブに切り替えます。
3. 監視プラットフォームとして **Prometheus** を選択します。

選択したモードによって、一部の設定オプションはPullモードにのみ適用されるものや、両モードに影響するものがあります。詳細な設定手順はダッシュボードページの **Help** ボタンをクリックしてご確認ください。

以下のエンドポイントで公開されているメトリクスシリーズ（アラートに適したものを含む）の厳選リファレンスは、[Broker Health Indicators](./broker-health-indicators.md)をご参照ください。

<img src="./assets/enable-push-gateway.png" alt="Pushgatewayを有効化" style="zoom:40%;" />

## Prometheus設定オプション

このセクションでは、ダッシュボードで**Prometheus**を選択した際に利用可能なすべての設定オプションについて説明します。

### 一般オプション（Pullモード・Pushモード両方に影響）

#### レイテンシーバケット

レイテンシー関連メトリクスのヒストグラムバケットの境界値を指定します。

**フォーマット**

カンマ区切りの期間値リスト：

```
10ms, 100ms, 1s, 5s, 30s
```

**説明**

これらの値はPrometheusでレイテンシーメトリクスをヒストグラムバケットに分類する際の境界を定義します。小さいバケット間隔はより細かい粒度を提供しますが、メトリクスのカーディナリティやストレージ使用量が増加する可能性があります。

この設定はレイテンシーヒストグラムメトリクスの内部生成に影響し、以下に適用されます。

- Pullモードのメトリクス
- Pushモードのメトリクス（Pushgateway経由）

### Pullモード設定

以下のオプションは、PrometheusがREST API経由でEMQXメトリクスをスクレイプする場合にのみ適用されます。

#### ベーシック認証を有効化

PrometheusのスクレイプAPIに対するHTTP Basic認証の有効・無効を設定します。

デフォルトでは、Prometheus PullモードAPIは認証を必要としません。このオプションを有効にすると：

- Prometheusは以下のエンドポイントにアクセスする際にHTTP Basic認証を使用する必要があります：
  - `/api/v5/prometheus/stats`
  - `/api/v5/prometheus/auth`
  - `/api/v5/prometheus/data_integration`
- EMQXで[APIキー](../admin/api.md#authentication)を作成する必要があります。
- `prometheus.yaml`の`basic_auth`セクションを設定します。

このオプションはPullモードのみに適用され、Pushgateway統合には影響しません。詳細は[Pullモード統合の設定](#configure-pull-mode-integration)をご参照ください。

#### ネームスペースデータスクレイピングのレート制限

ネームスペース関連メトリクスのスクレイピングリクエストの最大レートを制限します。

ネームスペースレベルのメトリクスはマルチテナント環境でサポートされ、ネームスペース単位で公開または集約可能です。詳細は[Prometheusメトリクスの分離](../multi-tenancy/namespace-overview.md#multi-tenancy-capability-support)をご覧ください。

**フォーマット**：`<リクエスト数>/<期間>`

**例**：`1/5s` は5秒あたり最大1リクエストを許容し、それ以上のリクエストは拒否されます。

**動作**：

- ネームスペースレベルのメトリクススクレイピングリクエストにのみ適用されます。
- 特定のネームスペースを対象としたリクエストは制限されません。
- Pullモードにのみ適用されます。

大規模またはマルチネームスペース環境での過負荷防止に役立ちます。

### Pushモード設定

PushモードではEMQXがPushgatewayインスタンスにメトリクスを送信します。デフォルトではPushモードは無効です。

#### Pushgatewayを有効化

Pushgatewayへのメトリクスプッシュを有効または無効にします。有効にした場合、以下の項目を設定します。

#### インターバル

EMQXがPushgatewayにメトリクスをプッシュする間隔を指定します。デフォルトは`15`秒です。

#### Pushgatewayサーバー

PushgatewayサーバーのURLを指定します。デフォルトは`http://127.0.0.1:9091`です。

#### ジョブ名

Pushgatewayにメトリクスをプッシュする際に使用するジョブラベルを指定します。

EMQXノード名やホスト名に基づく変数を用いてジョブラベルを構成できます。デフォルト値は`${name}/instance/${name}~${host}`です。

**変数**：

- `${name}`：EMQXノード名（例：`emqx`）
- `${host}`：ホストIPアドレス（例：`127.0.0.1`）

例えば、ノード名が`emqx@127.0.0.1`の場合：

- `${name}` = `emqx`
- `${host}` = `127.0.0.1`

#### ヘッダー

Pushgatewayにメトリクスをプッシュする際に送信する任意のHTTPヘッダーを指定できます。

値の型は文字列です。以下のようにキーと値のペアで設定可能です。

```
Authorization = "some-auth-token"
```

追加のヘッダーは **Add** ボタンで挿入できます。

## Pullモード統合の設定

Pullモードでは、PrometheusがREST API経由でEMQXからメトリクスをスクレイプします。

EMQXは以下のエンドポイントを提供しています。

- `/api/v5/prometheus/stats`：EMQXの基本メトリクスとカウンター。
- `/api/v5/prometheus/auth`：認証・認可を含むアクセス制御に関する主要メトリクスとカウンター。
- `/api/v5/prometheus/data_integration`：ルールエンジン、コネクター、アクション、Sink/Source、エンコード/デコードに関連するメトリクスとカウンター。

### メトリクス収集モード

上記APIを呼び出す際、URLクエリパラメータ`mode`を指定することで異なる種類のメトリクスデータを取得できます。各パラメータの意味は以下の通りです。

:::: tabs type: card

::: tab シングルノードモード

```
mode=node
```

デフォルトモードで、リクエストしたノードのメトリクスを返します。特に指定しない場合はこのモードが適用されます。

:::

::: tab クラスター集約モード

```
mode=all_nodes_aggregated
```

クラスター内の全稼働ノードのメトリクスを集約し、*算術和*または*論理和*を返します。

- 「オン状態」や「稼働状態」などのメトリクスは論理和で返されます。全ノードがオンまたは稼働中なら1、それ以外は0を返します。
- CPUやメモリ使用率などノードごとに独立したメトリクスは集約されず、ノード名をラベルとして区別します。例：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター内で値が一貫しているべきメトリクスは、APIリクエストを受け付けたノードの値を直接返します。これらは集約されず、ノード名ラベルも付きません。例：

  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスは算術和を返します。つまり、全ノードのメトリクスの合計値です。

:::

::: tab クラスター非集約モード

```
mode=all_nodes_unaggregated
```

クラスター内の全稼働ノードの個別メトリクスを返します。

- ノード名をラベルとして付与し、異なるノードのメトリクスを区別します。例：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- クラスター内で値が一貫しているべきメトリクスは、APIリクエストを受け付けたノードの値を直接返します。ノード名ラベルは付きません。例：

  ```bash
  emqx_retained_count 3
  ```

:::

::::

PrometheusのPullエンドポイントの詳細は、[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご参照ください。

### 認証（任意）

デフォルトでは、Prometheus PullモードAPIは認証を要求しません。

EMQXダッシュボードで**ベーシック認証を有効化**を設定した場合、PrometheusはHTTP Basic認証で認証を行う必要があります。

その場合は以下の手順を行います。

1. EMQXで[APIキー](../admin/api.md#authentication)を作成します。
2. Prometheus設定で生成されたAPIキーとシークレットキーを使用します。

Prometheus設定例：

```yaml
basic_auth:
  username: '<API_KEY>'
  password: '<SECRET_KEY>'
```

- `username` はAPIキー
- `password` は対応するシークレットキー

Prometheusはこれらの認証情報を用いてEMQXメトリクスをスクレイプします。

### Prometheusサーバー設定例

PrometheusがEMQXメトリクスをスクレイプできるように、Prometheusサーバーを設定します。

Prometheus設定ファイルに以下を追加し、Prometheusサービスを再起動してください。

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

## Pushモード統合の設定

PushモードではEMQXからPushgatewayへメトリクスを送信します。

ダッシュボードで**Pushgatewayを有効化**をオンにし、必要な項目を設定したら、**変更を保存**をクリックしてください。

Pushモードでは現状、EMQXの基本メトリクスとカウンター（`/api/v5/prometheus/stats`エンドポイントのみ）が含まれます。包括的な監視にはPullモードの利用が推奨されます。

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

## GrafanaでEMQXメトリクスを可視化する

PrometheusとGrafanaを組み合わせてEMQXメトリクスを可視化することも可能です。GrafanaにEMQXのテンプレートファイルをインポートすることで実現できます。テンプレートのダウンロードは[EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/)から、または**Monitoring**ページの**Integration**タブ下部の**Help**ボタンからアクセスできます。

::: tip

詳細な操作手順は[Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana)をご参照ください。

:::
