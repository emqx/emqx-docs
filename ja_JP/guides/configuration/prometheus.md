# Prometheus 設定

[Prometheus](https://prometheus.io/) は、SoundCloud がオープンソースとして公開したモニタリングソリューションで、多次元データモデルのサポート、柔軟なクエリ言語、強力なアラーム管理機能を備えています。EMQX は、サードパーティのモニタリングシステムとして Prometheus との統合をサポートしています。この機能の詳細については、[Prometheus との統合](../observability/prometheus.md) をご参照ください。

`base.hocon` 設定ファイルを通じて Pushgateway を有効化および設定できます。例は以下の通りです。

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

ここで、

- `push_gateway_server` は Prometheus のプッシュゲートウェイサーバーの URL を設定します。これはメトリクスを Prometheus にプッシュするために使用され、デフォルトは `http://127.0.0.1:9091` です。
- `interval` はメトリクスを収集し Prometheus にエクスポートする間隔を設定します。デフォルトは `15s` です。
- `headers` は Prometheus プッシュゲートウェイサーバーへの HTTP リクエストに含める追加のヘッダーを設定します。
- `job_name` はメトリクスをエクスポートする Prometheus 上のジョブ名を設定します。デフォルトは `"${name}/instance/${name}~${host}"` です。

::: tip

左側のナビゲーションメニューから **Management** -> **Monitoring** -> **Integration** をクリックすると、ダッシュボードからも Prometheus とのプッシュモード統合を設定できます。

ダッシュボードで統合を設定すると、その設定は設定ファイル内の同じ項目を上書きします。
設定ファイルから Prometheus を設定する場合は、`emqx.conf` ではなく `base.hocon` を使用することを推奨します。
これは、`emqx.conf` に設定がある場合、ダッシュボードでの変更は一時的なものとなり、EMQX 再起動時に失われるためです。

:::

::: tip

EMQX では、よりカスタマイズされたニーズに対応するために多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
