# Prometheus 設定

[Prometheus](https://prometheus.io/) は SoundCloud がオープンソース化した監視ソリューションで、多次元データモデルのサポート、柔軟なクエリ言語、強力なアラーム管理機能を備えています。EMQX はサードパーティの監視システムとして Prometheus との連携をサポートしています。この機能の詳細については、[Prometheus との連携](../observability/prometheus.md) をご参照ください。

`base.hocon` 設定ファイルを通じて Pushgateway の有効化および設定が可能です。例：

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

ここで、

- `push_gateway_server` は Prometheus の Pushgateway サーバーの URL を設定します。メトリクスを Prometheus にプッシュするために使用され、デフォルトは `http://127.0.0.1:9091` です。
- `interval` はメトリクスを収集し Prometheus にエクスポートする間隔を設定します。デフォルトは `15s` です。
- `headers` は Prometheus Pushgateway サーバーへの HTTP リクエストに含める追加のヘッダーを設定します。
- `job_name` はメトリクスをエクスポートする Prometheus 上のジョブ名を設定します。デフォルトは `"${name}/instance/${name}~${host}"` です。

::: tip

左側のナビゲーションメニューから **Management** -> **Monitoring** -> **Integration** をクリックすることで、ダッシュボードからも Prometheus とのプッシュモード連携を設定できます。

ダッシュボードで連携を設定すると、その設定が設定ファイル内の同じ項目を上書きします。
設定ファイルから Prometheus を設定する場合は、`emqx.conf` ではなく `base.hocon` の使用を推奨します。
これは、`emqx.conf` に設定があると、ダッシュボードでの変更が一時的なものとなり、EMQX の再起動時に失われるためです。

:::

::: tip

EMQX はよりカスタマイズされたニーズに対応するために、さらに多くの設定項目を提供しています。詳細は [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

:::
