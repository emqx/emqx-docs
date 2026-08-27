# Prometheusとの統合

EMQXは、SoundCloudが開発したオープンソースの監視ソリューションである[Prometheus](https://prometheus.io/)などのサードパーティ監視システムとの統合をサポートしています。Prometheusは多次元データモデル、柔軟なクエリ言語（PromQL）、強力なアラート機能を提供します。

サードパーティ監視システムを利用することで、以下のような利点があります。

- EMQXの監視データが他のシステムの監視データと統合される完全な監視システムを構築可能。例えば、サーバーホストの監視情報も取得できます。
- [Grafanaダッシュボード](#use-grafana-to-visualize-EMQX-metrics)を使ったEMQXメトリクスの可視化など、より直感的な監視レポート（図表）を作成可能。
- Prometheus Alertmanagerを使ったアラームルールや通知方法の設定など、多様なアラーム通知オプション。

EMQXはPrometheusメトリクス監視の統合方法として、以下の2つのモードをサポートしています。

- **Pullモード**：PrometheusがEMQXのREST APIを通じて直接メトリクスを収集します。
- **Pushモード**：EMQXがPushgatewayサービスにメトリクスをプッシュし、PrometheusがPushgatewayからメトリクスを収集します。

::: tip
EMQX 6.3.0以降、Prometheusメトリクスは`metrics`機能ゲートで制御されます。`EMQX_FEATURES`を手動で設定する場合、`metrics`を有効にすると、依存する`dashboard`および`auth`も自動的に有効になります。詳細は[機能ゲート](../deploy/feature-gates.md)をご参照ください。
:::

Prometheus統合の設定手順は以下の通りです。

1. EMQXダッシュボードの **Management** -> **Monitoring** に移動します。
2. **Integration** タブに切り替えます。
3. 監視プラットフォームとして **Prometheus** を選択します。

選択したモードにより、一部の設定オプションはPullモードにのみ適用され、他は両モードに影響します。ダッシュボードの **Help** ボタンをクリックすると、各モードの詳細な設定手順を確認できます。

以下のエンドポイントで公開されるメトリクスシリーズ（アラート対象のものを含む）については、[ブローカーのヘルス指標](./broker-health-indicators.md)を参照してください。

<img src="./assets/enable-push-gateway.png" alt="Pushgatewayの有効化" style="zoom:40%;" />

## Prometheus設定オプション

このセクションでは、ダッシュボードで**Prometheus**を選択した際に利用可能なすべての設定オプションについて説明します。

### 一般オプション（Pullモード・Pushモード共通）

#### レイテンシーバケット

レイテンシー関連メトリクスのヒストグラムバケット境界を指定します。

**フォーマット**

カンマ区切りの期間値リスト：

```
10ms, 100ms, 1s, 5s, 30s
```

**説明**

これらの値はPrometheusでレイテンシーメトリクスをヒストグラムバケットに分類する際の境界を定義します。小さいバケット間隔はより細かい粒度を提供しますが、メトリクスのカーディナリティやストレージ使用量が増加する可能性があります。

この設定は内部的にレイテンシーヒストグラムメトリクスの生成方法に影響し、以下に適用されます。

- Pullモードのメトリクス
- Pushモードのメトリクス（Pushgateway経由）

### Pullモード設定

以下のオプションは、PrometheusがREST APIを通じてEMQXメトリクスをスクレイプするPullモード時にのみ適用されます。

#### Basic Authの有効化

PrometheusスクレイプAPIに対するHTTP Basic認証を有効または無効にします。

デフォルトでは、PrometheusのPullモードAPIは認証不要です。このオプションを有効にすると：

- Prometheusは以下のAPIにアクセスする際にHTTP Basic認証を使用する必要があります：
  - `/api/v5/prometheus/stats`
  - `/api/v5/prometheus/auth`
  - `/api/v5/prometheus/data_integration`
- EMQXで[APIキー](../admin/api.md#authentication)を作成する必要があります。
- `prometheus.yaml`の`basic_auth`セクションを設定します。

このオプションはPullモードのみに適用され、Pushgateway統合には影響しません。詳細は[Pullモード統合の設定](#configure-pull-mode-integration)をご覧ください。

#### ネームスペースデータスクレイプのレート制限

ネームスペース関連メトリクスのスクレイプ時の最大リクエストレートを制限します。

ネームスペースレベルのメトリクスはマルチテナント環境でサポートされ、ネームスペース単位で公開または集約可能です。詳細は[Prometheusメトリクスの分離](../multi-tenancy/namespace-overview.md#multi-tenancy-capability-support)を参照してください。

**フォーマット**：`<requests>/<duration>`

**例**：`1/5s` は5秒あたり最大1リクエストを許可し、それ以降のリクエストは拒否されます。

**挙動**：

- ネームスペースレベルのメトリクススクレイプリクエストにのみ適用されます。
- 特定のネームスペースを対象としたリクエストは制限されません。
- Pullモードのみに適用されます。

このオプションは大規模またはマルチネームスペース環境での過負荷防止に役立ちます。

### Pushモード設定

Pushモードでは、EMQXがメトリクスをPushgatewayインスタンスに送信します。デフォルトではPushモードは無効です。

#### Pushgatewayの有効化

Pushgatewayへのメトリクスプッシュを有効または無効にします。有効にした場合、以下の項目を設定してください。

#### インターバル

EMQXがPushgatewayにメトリクスをプッシュする間隔を指定します。デフォルトは`15`秒です。

#### Pushgatewayサーバー

PushgatewayサーバーのURLを指定します。デフォルトは`http://127.0.0.1:9091`です。

#### ジョブ名

Pushgatewayにメトリクスをプッシュする際のジョブラベルを指定します。

EMQXノード名やホスト名に基づく変数を使ってジョブラベルを構築できます。デフォルト値は`${name}/instance/${name}~${host}`です。

**変数**：

- `${name}`：EMQXノード名（例：`emqx`）
- `${host}`：ホストのIPアドレス（例：`127.0.0.1`）

例として、ノード名が`emqx@127.0.0.1`の場合：

- `${name}` = `emqx`
- `${host}` = `127.0.0.1`

#### ヘッダー

Pushgatewayにメトリクスをプッシュする際に送信する任意のHTTPヘッダーを指定します。

値の型は文字列です。キーと値のペアで設定可能です。例：

```
Authorization = "some-auth-token"
```

追加のヘッダーは**Add**ボタンで挿入できます。

## Pullモード統合の設定

Pullモードでは、PrometheusがREST APIを通じてEMQXからメトリクスをスクレイプします。

EMQXは以下のエンドポイントを提供しています。

- `/api/v5/prometheus/stats`：EMQXの基本的なメトリクスとカウンター。
- `/api/v5/prometheus/auth`：認証・認可を含むアクセス制御に関する主要メトリクスとカウンター。
- `/api/v5/prometheus/data_integration`：ルールエンジン、コネクター、アクション、Sink/Source、エンコード/デコードに関連するメトリクスとカウンター。

### メトリクス収集モード

上記APIを呼び出す際、URLクエリパラメータ`mode`を指定して異なる種類のメトリクスを取得できます。各パラメータの意味は以下の通りです。

:::: tabs type: card

::: tab シングルノードモード

```
mode=node
```

デフォルトモードで、リクエストされたノードのメトリクスを返します。特に指定しない場合はこのモードが適用されます。

:::

::: tab クラスター集約モード

```
mode=all_nodes_aggregated
```

クラスター内のすべての稼働ノードのメトリクスを集約し、算術和または論理和を返します。

- 「オン状態」や「稼働状態」などのメトリクスは論理和を返します。すべてのノードがオンまたは稼働中なら1、それ以外は0を返します。
- ノードごとに独立したメトリクス（CPU使用率やメモリ使用率など）は集約値を返さず、ノード名をラベルに付与して区別します。例：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター内で値が一貫しているべきメトリクスは、APIリクエストを受けたノードの値を直接返します。これらは合計されず、ノード名ラベルも付きません。例：

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

- ノード名をラベルに付与して区別します。例：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- クラスター内で値が一貫しているべきメトリクス（例：「ブラックリスト数」や「保持メッセージ数」など）は、APIリクエストを受けたノードの値を直接返し、ノード名ラベルは付きません。例：

  ```bash
  emqx_retained_count 3
  ```

:::

::::

PrometheusのPullエンドポイントの詳細は、[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご参照ください。

### 認証（任意）

デフォルトでは、PrometheusのPullモードAPIは認証不要です。

EMQXダッシュボードで**Basic Authの有効化**をオンにした場合、PrometheusはHTTP Basic認証を用いて認証する必要があります。

この場合の手順：

1. EMQXで[APIキー](../admin/api.md#authentication)を作成します。
2. 生成されたAPIキーとシークレットキーをPrometheus設定に使用します。

Prometheus設定例：

```yaml
basic_auth:
  username: '<API_KEY>'
  password: '<SECRET_KEY>'
```

- `username` はAPIキー
- `password` は対応するシークレットキー

Prometheusはこれらの認証情報を使ってEMQXメトリクスをスクレイプします。

### Prometheusサーバー設定例

PrometheusがEMQXメトリクスをスクレイプできるように、Prometheusサーバーの設定ファイルに以下を追加し、サービスを再起動してください。

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # デフォルトのスクレイプ間隔は10秒
  evaluation_interval: 10s # デフォルトの評価間隔は10秒
  # このマシン上のすべての時系列はデフォルトでエクスポートされます
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

PushモードはEMQXからPushgatewayにメトリクスを送信します。

ダッシュボードで**Enable Pushgateway**を有効化し、必要な項目を設定したら、**Save Changes**をクリックしてください。

Pushモードは現状、`/api/v5/prometheus/stats`エンドポイントの基本的なメトリクスとカウンターのみを含みます。包括的な監視には一般的にPullモードの利用が推奨されます。

### 設定ファイル例

設定ファイルに以下を追加してPushgatewayを有効化・設定することも可能です。設定項目の詳細は[設定 - Prometheus](../configuration/prometheus.md)を参照してください。

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

## Grafanaを使ったEMQXメトリクスの可視化

GrafanaとPrometheusを組み合わせてEMQXメトリクスを可視化することも可能です。GrafanaにEMQXのテンプレートファイルをインポートすることで実現できます。テンプレートのダウンロードは[EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/)から、または**Monitoring**ページの**Integration**タブ下部の**Help**ボタンから行えます。

::: tip

詳細な操作手順は[PrometheusとGrafanaによるMQTTブローカーの監視](https://www.emqx.com/en/blog/emqx-prometheus-grafana)をご覧ください。

:::
