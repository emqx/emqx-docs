# Prometheusとの統合

EMQXは、[Prometheus](https://prometheus.io/)などのサードパーティ監視システムとの統合をサポートしています。PrometheusはSoundCloudによってオープンソース化された監視ソリューションで、多次元データモデルのサポート、柔軟なクエリ言語、強力なアラーム管理など、多彩な機能を提供します。

サードパーティ監視システムを利用することで、以下のような利点があります。

- EMQXの監視データを他のシステムの監視データと統合した、包括的な監視システムを構築可能です。例えば、サーバーホストの監視情報も取得できます。
- [Grafanaダッシュボード](#use-grafana-to-visualize-EMQX-metrics)などを用いて、図表による直感的な監視レポートを作成できます。
- Prometheus Alertmanagerを利用したアラームルールや通知方法の設定など、多様なアラーム通知オプションが利用可能です。

EMQXはPrometheusのメトリクス監視統合に対して、以下の2つの方法をサポートしています。

- **プルモード**：PrometheusがEMQXのREST APIを通じて直接メトリクスを収集します。
- **プッシュモード**：EMQXがPushgatewayサービスへメトリクスをプッシュし、Prometheusはそこからメトリクスを収集します。

本ページでは両モードの設定手順を紹介します。以下のエンドポイントで公開されるメトリクスシリーズの厳選リファレンス（アラートに適したものを含む）は、[Broker Health Indicators](./broker-health-indicators.md)をご参照ください。EMQXダッシュボードの左ナビゲーションメニューから**Management** -> **Monitoring**をクリックし、**Integration**タブで**Prometheus**を選択すると設定が可能です。また、各モードの具体的な設定手順はページの**Help**ボタンからも確認できます。

## プルモード統合の設定

EMQXはPrometheusがシステムメトリクスを収集するために、以下のREST APIを提供しています。

- `/api/v5/prometheus/stats`：EMQXの基本的なメトリクスとカウンター。
- `/api/v5/prometheus/auth`：認証・認可を含むアクセス制御に関する主要なメトリクスとカウンター。
- `/api/v5/prometheus/data_integration`：ルールエンジン、コネクター、アクション、Sink/Source、エンコード/デコードに関連するメトリクスとカウンター。

上記APIを呼び出してメトリクスを取得する際、URLクエリパラメータの`mode`を指定することで異なる種類のメトリクスデータを取得できます。各パラメータの意味は以下の通りです。

:::: tabs type: card

::: tab シングルノードモード

```
mode=node
```

デフォルトモードで、現在リクエストを受けたノードのメトリクスを返します。特に指定がない場合はこのモードが適用されます。

:::

::: tab クラスター集約モード

```
mode=all_nodes_aggregated
```

クラスター内の全稼働ノードのメトリクスを集約し、*算術和*または*論理和*を返します。

- 「オン状態」や「稼働状態」などのメトリクスは論理和で返されます。すべてのノードがオンまたは稼働中なら1、それ以外は0を返します。

- CPUやメモリ使用率のようにノードごとに独立したメトリクスは集約値を返さず、ノード名をラベルとして付加して区別します。例：

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123

  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- クラスター内のどのノードでも値が一貫しているべきメトリクス（クラスター一貫性メトリクス）は、APIリクエストを受けたノードの値をそのまま返し、集約やノード名ラベルは付加しません。例：

  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- その他のメトリクスは算術和を返します。すなわち、全ノードのメトリクスの合計値です。

:::

::: tab クラスター非集約モード

```
mode=all_nodes_unaggregated
```

クラスター内の全稼働ノードの個別メトリクスを返すモードです。

- ノード名をラベルとして付加し、異なるノードのメトリクスを区別します。例：

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- 「ブラックリスト数」や「保持メッセージ数」など、クラスター内のどのノードでも値が一貫しているべきメトリクスは、APIリクエストを受けたノードの値をそのまま返し、ノード名ラベルは付加しません。例：

  ```bash
  emqx_retained_count 3
  ```

:::

::::

Prometheusのプルエンドポイントの詳細は、[EMQX Enterprise APIドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご参照ください。

::: tip

プルモードAPIはデフォルトで認証不要です。ページ上の**Enable Basic Auth**スイッチを有効にすると、インターフェースにベーシック認証を設定できます。有効化後は、EMQXで[APIキー](../api.md#authentication)を作成し、Prometheus設定に適用してメトリクスを取得してください。

:::

### Prometheus設定例

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # デフォルトのスクレイプ間隔は10秒ごとです。
  evaluation_interval: 10s # デフォルトの評価間隔は10秒ごとです。
  # このマシン上のすべての時系列がデフォルトでエクスポートされます。
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

EMQXはPushgatewayにメトリクスをプッシュし、Prometheusがそこからメトリクスを収集することをサポートしています。Pushgatewayへのプッシュはデフォルトで無効化されています。Pushgatewayサービスを有効にするには、ダッシュボードのPrometheus設定ページで**Enable Pushgateway**トグルスイッチをクリックしてください。

<img src="./assets/enable-push-gateway.png" alt="Pushgatewayを有効にする" style="zoom:40%;" />

ビジネス要件に応じて以下の項目を設定し、**Save Changes**をクリックします。

- **Interval**：Pushgatewayへ監視メトリクスデータを報告する時間間隔を指定します。デフォルトは`15`秒です。
- **Pushgateway Server**：PrometheusサーバーのURLを入力します。デフォルトは`http://127.0.0.1:9091`です。
- **Job Name**：EMQXクラスター名、ノード名、ホスト名を含む変数を指定します。デフォルトは`${name}/instance/${name}~${host}`です。例えば、EMQXノード名が`emqx@127.0.0.1`の場合、`name`変数は`emqx`、`host`変数は`127.0.0.1`の値を取ります。
- **Headers**：Pushgatewayにプッシュする監視メトリクスのHTTPヘッダーのキーと値を入力します。**Add**ボタンをクリックして複数のヘッダーを追加可能です。型は文字列で、例：{ Authorization = "some-authz-tokens"}。

同時に、**Help**ボタンをクリックし、**Use Pushgateway**タブの手順を参照して設定できます。

::: tip

プッシュモードは現状、`/api/v5/prometheus/stats`エンドポイントの基本的なメトリクスとカウンターのみを含むため、プルモードの利用を推奨します。

:::

Pushgatewayの有効化と設定は、設定ファイルに以下を追加することでも可能です。設定項目の詳細は[Configuration - Prometheus](../configuration/prometheus.md)をご参照ください。

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

## Grafanaを使ってEMQXメトリクスを可視化する

GrafanaとPrometheusを組み合わせてEMQXメトリクスを可視化することも可能です。GrafanaにEMQXのテンプレートファイルをインポートすることで実現できます。テンプレートのダウンロードは、[EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/)をクリックするか、**Monitoring**ページの**Integration**タブ下部にある**Help**ボタンをクリックしてください。

::: tip

詳細な操作手順は、[Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana)をご参照ください。

:::
