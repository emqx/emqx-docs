# PrometheusでEMQXを監視する

EMQXは、Prometheusがクエリ、アラート、可視化のために収集できるランタイムメトリクスを公開しています。これらのメトリクスは、以下のいずれかの方法で収集できます。

- **プルモード（推奨）**：PrometheusがREST APIエンドポイントから直接EMQXのメトリクスをスクレイプします。このモードでは利用可能なメトリクスの完全なセットを収集できます。
- **プッシュモード**：EMQXが基本的なメトリクスをPushgatewayに送信し、PrometheusがPushgatewayからそれらをスクレイプします。PrometheusがEMQXに直接接続できない場合にこのモードを使用します。

その後、Grafanaを使って[収集したEMQXメトリクスを可視化](#visualize-emqx-metrics-in-grafana)できます。

::: tip
EMQX 6.3.0以降、Prometheusメトリクスは`metrics`機能ゲートで制御されます。`EMQX_FEATURES`を手動で設定する場合、`metrics`を有効にすると、それに依存する`dashboard`と`auth`も有効になります。詳細は[機能ゲート](../deploy/feature-gates.md)を参照してください。
:::

## EMQXでのメトリクス収集の設定

DashboardのPrometheus統合ページで、認証、Pushgatewayへの送信、レイテンシーバケット、ネームスペースのリクエスト制限を制御できます。このセクションでは各設定項目の内容を説明します。続くセクションではプルモードとプッシュモードの設定手順を解説します。

Prometheus統合設定を開くには：

1. EMQX Dashboardで**Management** -> **Monitoring**に移動します。
2. **Integration**タブを選択します。
3. **Prometheus**を選択します。

<img src="./assets/enable-push-gateway.png" alt="Prometheus統合設定" style="zoom: 67%;" />

以下のエンドポイントで公開されているメトリクスシリーズ（アラートに適したものを含む）については、[ブローカーのヘルス指標](./broker-health-indicators.md)を参照してください。

### スクレイプリクエストの認証を要求する

**Enable Basic Auth**は、`/api/v5/prometheus/*`以下のすべてのPrometheusスクレイプAPIに対する認証を制御します。Dashboardのラベルとは異なり、この設定はHTTP Basic認証とBearer認証の両方を制御します。

EMQX 6.3.0以降、認証はデフォルトで有効です。認証情報なしのリクエストは`401`を返します。PrometheusはAPIキーとシークレットキーを使ったHTTP Basic認証を利用できます。EMQXはDashboardのログイントークンをBearerトークンとしても受け入れますが、トークンは期限切れとなり、継続的なスクレイプには適しません。

長期間稼働するPrometheusサーバーには、`monitoring`スコープの専用APIキーを作成し、[Prometheusのスクレイプリクエスト認証を設定](#authenticate-prometheus-scrape-requests)してください。

認証なしのスクレイプを許可するには、**Enable Basic Auth**をオフにするか、`prometheus.enable_basic_auth = false`を設定します。この設定はプルモードにのみ影響し、Pushgatewayへの送信には影響しません。

::: warning 重要なお知らせ
認証を無効にすると、Dashboardリスナーに到達可能な任意のクライアントがEMQXメトリクスをスクレイプ可能になります。アップグレード後、明示的に`prometheus.enable_basic_auth = false`を設定した構成やレガシーフォーマットのPrometheus構成は認証なしスクレイプを継続します。アップグレード後はDashboardの**Enable Basic Auth**を必ず確認してください。
:::

### Pushgatewayへのメトリクス送信を設定する

**Enable Pushgateway**をオンにすると、EMQXがPushgatewayインスタンスにメトリクスを送信します。プッシュ送信はデフォルトで無効です。以下の項目を設定してください。

| 項目 | 説明 |
| --- | --- |
| **Interval** | EMQXがメトリクスをプッシュする間隔。デフォルトは`15`秒です。 |
| **Pushgateway Server** | PushgatewayのURL。デフォルトは`http://127.0.0.1:9091`です。 |
| **Job Name** | プッシュされるメトリクスのジョブラベル。デフォルトは`${name}/instance/${name}~${host}`で、`${name}`は`@`の前のノード名、`${host}`は`@`の後のホスト名です。例えば`emqx@127.0.0.1`の場合、`emqx`と`127.0.0.1`になります。 |
| **Headers** | Pushgatewayに送信する任意のHTTPヘッダー。各ヘッダーはキーと値のペアで追加します。例：`Authorization = "some-auth-token"`。 |

完全な手順は[Pushgatewayへのメトリクス送信の設定](#configure-push-mode-integration)を参照してください。

### レイテンシーヒストグラムのバケットを定義する

**Latency Buckets**にカンマ区切りの期間値を入力します。例：

```text
10ms, 100ms, 1s, 5s, 30s
```

これらの値はプルモードとプッシュモードの両方でレイテンシーヒストグラムのバケット境界を定義します。バケット数が多いほど粒度は細かくなりますが、メトリクスのカーディナリティとストレージ使用量が増加する可能性があります。

### すべてのネームスペースをスクレイプするリクエストの制限

**Namespace Data Scraping Rate Limit**は、すべてのネームスペースのメトリクスをスクレイプするリクエストの最大レートを設定します。特定のネームスペースに対するリクエストは制限されません。`<requests>/<duration>`形式で値を入力します。デフォルトの`1/5s`は5秒に1回のリクエストを許可し、それ以外のリクエストは拒否されます。

## PrometheusでEMQXメトリクスをスクレイプする設定

プルモードでは、PrometheusがEMQX Dashboardリスナーに接続し、1つ以上のREST APIエンドポイントをスクレイプします。

### スクレイプするメトリクスエンドポイントを選択する

収集したいメトリクスカテゴリごとにPrometheusのスクレイプジョブを追加します。

| エンドポイント | メトリクス内容 |
| --- | --- |
| `/api/v5/prometheus/stats` | 基本的なEMQXメトリクスとカウンター |
| `/api/v5/prometheus/namespaced_stats` | ネームスペース別に集約されたメトリクス |
| `/api/v5/prometheus/auth` | 認証、認可、禁止クライアントのメトリクス |
| `/api/v5/prometheus/data_integration` | ルール、コネクター、アクション、Sink/Source、エンコード/デコードのメトリクス |
| `/api/v5/prometheus/schema_validation` | スキーマ検証のメトリクス |
| `/api/v5/prometheus/message_transformation` | メッセージ変換のメトリクス |
| `/api/v5/prometheus/topic_metrics` | トピックメトリクス収集カウンター |

完全なAPIリファレンスは[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)を参照してください。

### ネームスペース別のデータ統合メトリクスをスクレイプする

EMQX 6.3.0以降、`GET /api/v5/prometheus/data_integration`は認証ユーザーのネームスペースに応じてルール、アクション、コネクターメトリクスを制限します。

- ネームスペースユーザーは割り当てられたネームスペースのメトリクスのみ取得できます。`ns=<namespace>`で他のネームスペースを指定すると`403`が返されます。
- グローバル管理者はデフォルトで全ネームスペースのメトリクスを取得します。`ns=<namespace>`で特定のネームスペースをスクレイプするか、`ns`なしで`only_global=true`を指定するとグローバルネームスペースのみをスクレイプします。
- 認証が無効な場合、グローバル管理者と同様の可視性が適用され、デフォルトで全ネームスペースのメトリクスが返されます。

グローバルネームスペース以外のルール、アクション、コネクターのリソース単位メトリクスには`namespace`ラベルが付きます。グローバルネームスペースのリソース単位メトリクスにはこのラベルは付きません。`emqx_schema_registrys_count`メトリクスはスキーマレジストリリソースがネームスペースでスコープされないためクラスター全体のままです。

すべてのネームスペースをスクレイプするリクエストは[ネームスペースデータスクレイプリクエスト制限](#limit-requests-that-scrape-all-namespaces)の対象となります。

### メトリクス収集モードを選択する

対応するエンドポイントでは、`mode`クエリパラメータで現在のノードかクラスターのメトリクスを返すかを制御できます。

:::: tabs type: card

::: tab 現在のノード

```text
mode=node
```

リクエストを受けたノードのメトリクスを返します。デフォルトモードです。

:::

::: tab 集約されたクラスター

```text
mode=all_nodes_aggregated
```

稼働中のすべてのノードのメトリクスを以下の集約ルールで返します。

- 状態メトリクスは論理集約されます。例えば、すべてのノードで状態が有効または実行中の場合に`1`、それ以外は`0`になります。
- CPUやメモリ使用率などノード固有のメトリクスは集約されず、`node`ラベルを保持します。

  ```text
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター全体で一貫するメトリクスはリクエストを受けたノードの値を返し、合計はせず`node`ラベルも付きません。

  ```text
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスはすべての稼働ノードの算術和を返します。

:::

::: tab 非集約クラスター

```text
mode=all_nodes_unaggregated
```

すべての稼働ノードの個別メトリクスを返します。ノード固有の値には`node`ラベルが付きます。

```text
emqx_connections_count{node="emqx@127.0.0.1"} 0
```

クラスター全体で一貫するメトリクスはリクエストを受けたノードの値を返し、`node`ラベルは付きません。

```text
emqx_retained_count 3
```

:::

::::

<a id="authentication"></a>

### Prometheusスクレイプリクエストの認証設定

EMQX 6.3.0以降、PrometheusスクレイプAPIはデフォルトで認証が必要です。継続的なスクレイプには専用APIキーを使ったHTTP Basic認証を推奨します。

1. EMQXで`monitoring`スコープの[APIキー](../admin/api.md#authentication)を作成します。
2. `prometheus.yaml`の各EMQXスクレイプジョブにAPIキーとシークレットキーを追加します。

   ```yaml
   basic_auth:
     username: '<API_KEY>'
     password: '<SECRET_KEY>'
   ```

EMQXは`POST /api/v5/login`で取得したBearerトークンも受け入れますが、Dashboardログイントークンは期限切れになるため、長期間稼働するPrometheusスクレイパーにはAPIキーを使用してください。

### PrometheusにEMQXスクレイプジョブを追加する

以下の`prometheus.yaml`例は、3つの一般的なメトリクスカテゴリを収集します。ターゲットアドレスと認証情報を置き換え、必要に応じて他の[メトリクスエンドポイント](#select-metrics-endpoints-to-scrape)のジョブを追加してください。ファイル変更後はPrometheusを再起動してください。

```yaml
global:
  scrape_interval: 10s
  evaluation_interval: 10s
  external_labels:
    monitor: 'emqx-monitor'

scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'
```

<a id="configure-push-mode-integration"></a>

## EMQXをPushgatewayにメトリクス送信するよう設定する

プッシュモードでは、`/api/v5/prometheus/stats`で利用可能な基本的なメトリクスとカウンターのみが送信されます。他のエンドポイントのメトリクスが必要な場合はプルモードを使用してください。

### DashboardでPushgateway送信を有効にする

1. DashboardのPrometheus統合設定を開きます。
2. **Enable Pushgateway**をオンにします。
3. Pushgatewayサーバー、プッシュ間隔、ジョブ名、および必要なHTTPヘッダーを入力します。
4. **Save Changes**をクリックします。

PrometheusもPushgatewayインスタンスをスクレイプするよう設定する必要があります。

### 設定ファイルでPushgateway送信を有効にする

代わりに、`etc/base.hocon`に以下の推奨ネスト構成を追加します。

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
    interval = 15s
    headers {}
    job_name = "${name}/instance/${name}~${host}"
  }
}
```

## GrafanaでEMQXメトリクスを可視化する

PrometheusがEMQXメトリクスの収集を開始したら、[EMQX Grafanaダッシュボード](https://grafana.com/grafana/dashboards/17446-emqx/)をインポートして可視化できます。このテンプレートはDashboardのPrometheus統合の**Help**ページからも入手可能です。

完全な例は[PrometheusとGrafanaによるMQTTブローカーの監視](https://www.emqx.com/en/blog/emqx-prometheus-grafana)を参照してください。
