# EMQX 5.8 における互換性のない変更点

## e5.8.9

- [#16062](https://github.com/emqx/emqx/pull/16062) RocketMQアクションが指定されたペイロードテンプレートを無視し、ルール出力全体をレンダリングしてしまう問題を修正しました。

- [#16491](https://github.com/emqx/emqx/pull/16491) macOS 13（Ventura）向けパッケージのリリースを停止しました。

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) このバージョン以降、REST APIまたはダッシュボード経由でのプラグインインストールには明示的な権限が必要になりました。インストール前に以下のCLIコマンドで権限を取得する必要があります。

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  この変更により、不正なプラグインインストールを防止しセキュリティが強化されます。APIやダッシュボード経由でプラグインを管理しているユーザーはワークフローの調整が必要です。

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) `force_shutdown.max_heap_size` の最大許容値を `128GB` に変更しました。以前に128GBを超える値に設定していた場合、アップグレード後の設定リロードや更新時に問題が発生する可能性があります。

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) PrometheusメトリクスのJSON形式リクエスト時に、トップレベルの `client` キーが単一のJSONオブジェクトではなく常にJSONオブジェクトの配列となります。この変更により監視ツールのデータ処理に影響が出る可能性があります。

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDBデータ統合の設定変更：

  - セルフディスクリビングテンプレートが廃止されました。EMQXは設定されたデータテンプレートのみを使用し、メッセージペイロードからテンプレートを抽出しなくなりました。

  - 各MQTTメッセージは単一のペイロードのみを持つことが可能で、ペイロードの配列はサポートされません。これにより、MQTTメッセージは単一またはバッチ挿入の単一原子操作としてIoTDBに処理されます。1つのMQTTメッセージから複数のIoTDB操作を生成することはできなくなりました。

  - `data type` はテンプレート値ではなく単純な値として扱われます。

  - REST APIドライバーはIoTDB 1.3.x以降のみをサポートします。

  - Thriftドライバーは「バッチ」モードをサポートするようになりました。

    **重要**：バッチモードでタイムスタンプの重複を防ぐため、MQTTメッセージのタイムスタンプ（`${timestamp}`）を使用するか、ペイロード内に時間フィールド（例：`${payload.time}`）を含めることを推奨します。

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) 認証におけるハッシュアルゴリズム `MD4`、`MD5`、`RIPEMD-160` のサポートを廃止しました。これらは[NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard)に準拠していません。

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) クラスターリンクにおいて、`topics` 設定内の重複するトピックフィルターが原因でクロスクラスターのメッセージルーティングが不整合かつ不完全になる問題を修正しました。各トピックフィルターは個別に処理されるため、`topics` 設定内で冗長なトピックフィルター（例：`t/1` と `t/+`）は無効とみなされます。そのような設定が検出された場合、リンクは起動に失敗します。

- [#14015](https://github.com/emqx/emqx/pull/14015) 動的トピック（プレースホルダーを含むトピック）を持つKafka/Confluent/Azure Event Hubのパブリッシャーはディスクバッファリングをサポートしなくなりました。現在はメモリモードとハイブリッドモードのみがサポートされています。

- [#14106](https://github.com/emqx/emqx/pull/14106) 単一のKafkaコンシューマーコネクター内で同じKafkaトピックを持つソースの重複を禁止するバリデーションを追加しました。トピックを重複させたい場合は、新しいコネクターとソースを作成してください。

## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) `until` 値なしで作成された禁止アイテムのデフォルト有効期限を無期限（以前は最大1年）に変更しました。

- [#13742](https://github.com/emqx/emqx/pull/13742) クライアントが `#` または `+` トピックをサブスクライブした際に、`$` で始まるトピックの保持メッセージを受信してしまう問題を修正しました。

  この修正は [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246) の要件を満たしています。

## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) `mqtt.retry_interval` 設定のデフォルト値を30秒から無限大（`infinity`）に変更しました。

  以前はEMQXがメッセージ配信を30秒ごとに自動的に再試行していましたが、新しいデフォルトでは自動再試行を行いません。この変更はMQTT仕様に準拠したもので、セッション内でのメッセージ再試行は一般的に推奨されていません。

  再試行機能を利用しているユーザーのために、特定の再試行間隔を設定するオプションは引き続き利用可能です。

- [#13190](https://github.com/emqx/emqx/pull/13190) CentOS 7およびUbuntu 18向けリリースのサポートを終了しました。これらのOSはサポート終了のため、EMQXはビルドを提供しません。

- [#13248](https://github.com/emqx/emqx/pull/13248) `builtin` 永続化ストレージバックエンドを廃止し、柔軟性とスケーラビリティを向上させるために2つの新しいバックエンドに置き換えました：

  - **`builtin_local`**：レプリケーションをサポートしない永続化ストレージバックエンドで、シングルノード展開に適しています。オープンソース版およびエンタープライズ版で利用可能ですが、マルチノードクラスターには対応していません。
  - **`builtin_raft`**：Raftコンセンサスアルゴリズムを利用し、複数ノード間でデータレプリケーションを行う永続化ストレージバックエンドです。エンタープライズ版専用で、データの耐久性とフォールトトレランスが向上します。

  さらに、以下のPrometheusメトリクス名が機能をより正確に反映するように変更されました：

  - `emqx_ds_egress_batches` → `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` → `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` → `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` → `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` → `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` → `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) オープンソース版からCore-replicant機能を削除しました。5.8以降、オープンソース版の全ノードはCoreロールで動作します。この変更はエンタープライズ版には影響ありません。不要となった `cluster.core_nodes` 設定パラメータも削除されました。

- [#13372](https://github.com/emqx/emqx/pull/13372) ゲートウェイが受け入れる接続数はライセンス条件により制御されるようになり、許可された接続数の制限を順守します。
